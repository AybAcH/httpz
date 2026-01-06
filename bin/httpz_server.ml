(* httpz_server.ml - Async static file server using httpz with zero-copy bigstring I/O *)

open Core
open Async

(* Response buffer size - 64KB for headers *)
let response_buffer_size = 65536

(* Connection state *)
type conn_state = {
  reader : Reader.t;
  writer : Writer.t;
  read_buf : Httpz.buffer;
  write_buf : Httpz.buffer;
  mutable read_len : int;
  mutable keep_alive : bool;
}

(* Create connection state *)
let create_conn reader writer = {
  reader;
  writer;
  read_buf = Httpz.create_buffer ();
  write_buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout response_buffer_size;
  read_len = 0;
  keep_alive = true;
}

(* Basic MIME type detection *)
let mime_type_of_path path =
  match Filename.split_extension path with
  | _, Some "html" | _, Some "htm" -> "text/html"
  | _, Some "css" -> "text/css"
  | _, Some "js" -> "application/javascript"
  | _, Some "json" -> "application/json"
  | _, Some "txt" -> "text/plain"
  | _, Some "md" -> "text/markdown"
  | _, Some "xml" -> "application/xml"
  | _, Some "png" -> "image/png"
  | _, Some "jpg" | _, Some "jpeg" -> "image/jpeg"
  | _, Some "gif" -> "image/gif"
  | _, Some "svg" -> "image/svg+xml"
  | _, Some "ico" -> "image/x-icon"
  | _, Some "pdf" -> "application/pdf"
  | _, Some "woff" -> "font/woff"
  | _, Some "woff2" -> "font/woff2"
  | _, Some "ttf" -> "font/ttf"
  | _, Some "ml" | _, Some "mli" -> "text/x-ocaml"
  | _, Some "c" | _, Some "h" -> "text/x-c"
  | _, Some "py" -> "text/x-python"
  | _, Some "sh" -> "text/x-shellscript"
  | _, Some "yaml" | _, Some "yml" -> "text/yaml"
  | _, Some "toml" -> "text/toml"
  | _ -> "application/octet-stream"

(* Write response headers directly to bigstring *)
let write_headers conn status content_type content_length version =
  let buf = conn.write_buf in
  let off = 0 in
  let off = Httpz.write_status_line_bigstring buf ~off status version in
  let off = Httpz.write_header_bigstring buf ~off "Content-Type" content_type in
  let off = Httpz.write_content_length_bigstring buf ~off content_length in
  let off = Httpz.write_header_bigstring buf ~off "Server" "httpz/0.1" in
  let off = Httpz.write_connection_bigstring buf ~off conn.keep_alive in
  let off = Httpz.write_crlf_bigstring buf ~off in
  Writer.write_bigstring conn.writer buf ~pos:0 ~len:off

(* Send error response *)
let send_error conn status message version =
  write_headers conn status "text/plain" (String.length message) version;
  Writer.write conn.writer message;
  Writer.flushed conn.writer

(* Normalize path - remove .. and resolve to absolute within root *)
let normalize_path ~root request_path =
  (* Decode URL-encoded characters (basic) *)
  let decoded = request_path in
  (* Split into components and filter out . and empty, handle .. *)
  let parts = String.split decoded ~on:'/' in
  let rec resolve acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> resolve acc rest
    | ".." :: rest ->
      (match acc with
       | [] -> resolve [] rest  (* Can't go above root *)
       | _ :: acc' -> resolve acc' rest)
    | part :: rest -> resolve (part :: acc) rest
  in
  let normalized = resolve [] parts in
  let relative = String.concat ~sep:"/" normalized in
  Filename.concat root relative

(* Serve a file *)
let serve_file conn ~root target_span version =
  let target = Httpz.span_to_string conn.read_buf target_span in
  (* Extract path before query string *)
  let path = match String.lsplit2 target ~on:'?' with
    | Some (p, _) -> p
    | None -> target
  in
  (* Normalize and resolve path *)
  let file_path = normalize_path ~root path in
  (* Check if path is within root (security check) *)
  let root_abs = Filename_unix.realpath root in
  Sys.file_exists file_path
  >>= function
  | `No | `Unknown ->
    send_error conn Httpz.S404_Not_Found "Not Found" version
  | `Yes ->
    Sys.is_directory file_path
    >>= function
    | `Yes ->
      (* Try index.html for directories *)
      let index_path = Filename.concat file_path "index.html" in
      Sys.file_exists index_path
      >>= (function
        | `Yes ->
          Reader.file_contents index_path
          >>= fun contents ->
          write_headers conn Httpz.S200_OK "text/html" (String.length contents) version;
          Writer.write conn.writer contents;
          Writer.flushed conn.writer
        | `No | `Unknown ->
          send_error conn Httpz.S404_Not_Found "Not Found" version)
    | `No | `Unknown ->
      (* Check file is within root *)
      Monitor.try_with (fun () ->
        let file_abs = Filename_unix.realpath file_path in
        if String.is_prefix file_abs ~prefix:root_abs then
          Reader.file_contents file_path
          >>| fun contents -> Some contents
        else
          return None)
      >>= function
      | Error _ -> send_error conn Httpz.S404_Not_Found "Not Found" version
      | Ok None -> send_error conn Httpz.S403_Forbidden "Forbidden" version
      | Ok (Some contents) ->
        let content_type = mime_type_of_path file_path in
        write_headers conn Httpz.S200_OK content_type (String.length contents) version;
        Writer.write conn.writer contents;
        Writer.flushed conn.writer

(* Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size then
    return `Buffer_full
  else begin
    let available = Httpz.buffer_size - conn.read_len in
    let bss = Bigsubstring.create conn.read_buf ~pos:conn.read_len ~len:available in
    Reader.read_bigsubstring conn.reader bss
    >>| function
    | `Eof -> `Eof
    | `Ok n ->
      conn.read_len <- conn.read_len + n;
      `Ok n
  end

(* Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len then begin
    for i = 0 to conn.read_len - consumed - 1 do
      Bigarray.Array1.set conn.read_buf i
        (Bigarray.Array1.get conn.read_buf (consumed + i))
    done;
    conn.read_len <- conn.read_len - consumed
  end else if consumed >= conn.read_len then
    conn.read_len <- 0

(* Handle one request on connection *)
let handle_request conn ~root =
  let buf = conn.read_buf in
  let len = conn.read_len in
  let #(status, req, _headers) = Httpz.parse buf ~len in
  let body_off = req.#body_off in
  let version = req.#version in
  let target = req.#target in
  match status with
  | Httpz.Ok ->
    let body_span = Httpz.body_span ~len req in
    let body_span_len = body_span.#len in
    let body_span_off = body_span.#off in
    if body_span_len = -1 then
      return `Need_more
    else begin
      conn.keep_alive <- req.#keep_alive;
      serve_file conn ~root target version
      >>| fun () ->
      let consumed = if body_span_len > 0 then
        body_span_off + body_span_len
      else
        body_off
      in
      shift_buffer conn consumed;
      if conn.keep_alive then `Continue else `Close
    end
  | Httpz.Partial ->
    return `Need_more
  | Httpz.Headers_too_large ->
    conn.keep_alive <- false;
    send_error conn Httpz.S413_Payload_Too_Large "Payload Too Large" Httpz.HTTP_1_1
    >>| fun () -> `Close
  | _ ->
    conn.keep_alive <- false;
    send_error conn Httpz.S400_Bad_Request "Bad Request" Httpz.HTTP_1_1
    >>| fun () -> `Close

(* Handle connection loop *)
let handle_connection conn ~root =
  let rec loop () =
    if conn.read_len = 0 then begin
      read_more conn
      >>= function
      | `Eof -> return ()
      | `Buffer_full ->
        conn.keep_alive <- false;
        send_error conn Httpz.S413_Payload_Too_Large "Payload Too Large" Httpz.HTTP_1_1
      | `Ok _ -> loop ()
    end else begin
      handle_request conn ~root
      >>= function
      | `Continue -> loop ()
      | `Close -> return ()
      | `Need_more ->
        read_more conn
        >>= function
        | `Eof -> return ()
        | `Buffer_full ->
          conn.keep_alive <- false;
          send_error conn Httpz.S413_Payload_Too_Large "Payload Too Large" Httpz.HTTP_1_1
        | `Ok _ -> loop ()
    end
  in
  loop ()

(* Handle a single client connection *)
let handle_client ~root addr reader writer =
  let addr_str = match addr with
    | `Inet (host, port) ->
      sprintf "%s:%d" (Unix.Inet_addr.to_string host) port
    | `Unix path -> path
  in
  let conn = create_conn reader writer in
  Monitor.try_with (fun () -> handle_connection conn ~root)
  >>| fun result ->
  match result with
  | Ok () -> ()
  | Error exn ->
    printf "[%s] Error: %s\n%!" addr_str (Exn.to_string exn)

(* Run server *)
let run ~port ~root () =
  let where_to_listen = Tcp.Where_to_listen.of_port port in
  printf "httpz serving %s on http://localhost:%d/\n%!" root port;
  Tcp.Server.create
    ~on_handler_error:`Raise
    ~backlog:128
    ~max_connections:10000
    where_to_listen
    (fun addr reader writer -> handle_client ~root addr reader writer)
  >>= fun _server ->
  Deferred.never ()

(* Command-line interface *)
let command =
  Command.async
    ~summary:"Static file server using httpz"
    (Command.Param.map2
       (Command.Param.flag "-p" (Command.Param.optional_with_default 8080 Command.Param.int)
          ~doc:"PORT Port to listen on (default: 8080)")
       (Command.Param.flag "-d" (Command.Param.optional_with_default "." Command.Param.string)
          ~doc:"DIR Directory to serve (default: .)")
       ~f:(fun port root () -> run ~port ~root ()))

let () = Command_unix.run command
