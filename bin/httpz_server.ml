(* httpz_server.ml - HTTP server using httpz and Core_unix *)

open Core

(* Response buffer size - 64KB for headers + small bodies *)
let response_buffer_size = 65536

(* Connection state *)
type conn_state = {
  fd : Core_unix.File_descr.t;
  read_buf : Httpz.buffer;
  write_buf : Bytes.t;
  mutable read_len : int;
  mutable keep_alive : bool;
}

(* Simple response type *)
type response = {
  status : Httpz.response_status;
  content_type : string;
  body : string;
}

(* Handler type: takes buffer, method, target (span), headers (local), body span -> response *)
(* All parsing data stays local/stack-allocated; handler extracts what it needs *)
type handler =
  Httpz.buffer ->
  Httpz.method_ ->
  Httpz.span ->                   (* target span *)
  Httpz.header list @ local ->    (* headers - local *)
  Httpz.span ->                   (* body span *)
  response
[@@warning "-34"]

(* Default 404 response *)
let not_found_response = {
  status = Httpz.S404_Not_Found;
  content_type = "text/plain";
  body = "Not Found";
}

(* Default 400 response *)
let bad_request_response = {
  status = Httpz.S400_Bad_Request;
  content_type = "text/plain";
  body = "Bad Request";
}

(* Default 413 response *)
let payload_too_large_response = {
  status = Httpz.S413_Payload_Too_Large;
  content_type = "text/plain";
  body = "Payload Too Large";
}

(* Default 500 response - reserved for future use *)
let _internal_error_response = {
  status = Httpz.S500_Internal_Server_Error;
  content_type = "text/plain";
  body = "Internal Server Error";
}

(* Create connection state *)
let create_conn fd = {
  fd;
  read_buf = Httpz.create_buffer ();
  write_buf = Bytes.create response_buffer_size;
  read_len = 0;
  keep_alive = true;
}

(* Write response to connection *)
let write_response conn (resp : response) version =
  let buf = conn.write_buf in
  let off = 0 in
  (* Status line *)
  let off = Httpz.write_status_line buf ~off resp.status version in
  (* Headers *)
  let off = Httpz.write_header buf ~off "Content-Type" resp.content_type in
  let off = Httpz.write_content_length buf ~off (String.length resp.body) in
  let off = Httpz.write_header buf ~off "Server" "httpz/0.1" in
  let off = Httpz.write_connection buf ~off conn.keep_alive in
  (* End of headers *)
  let off = Httpz.write_crlf buf ~off in
  (* Write headers *)
  let header_bytes = Bytes.To_string.sub buf ~pos:0 ~len:off in
  let written = Core_unix.single_write_substring conn.fd ~buf:header_bytes ~pos:0 ~len:off in
  if written < off then
    failwith "Short write on headers";
  (* Write body *)
  if String.length resp.body > 0 then begin
    let body_written = Core_unix.single_write_substring conn.fd ~buf:resp.body ~pos:0 ~len:(String.length resp.body) in
    if body_written < String.length resp.body then
      failwith "Short write on body"
  end

(* Read more data into buffer *)
let read_more conn =
  if conn.read_len >= Httpz.buffer_size then
    `Buffer_full
  else begin
    let available = Httpz.buffer_size - conn.read_len in
    (* Convert bigarray to bytes for reading, then copy back *)
    let tmp = Bytes.create available in
    let n = Core_unix.read conn.fd ~buf:tmp ~pos:0 ~len:available in
    if n = 0 then
      `Eof
    else begin
      (* Copy to bigarray *)
      for i = 0 to n - 1 do
        Bigarray.Array1.set conn.read_buf (conn.read_len + i) (Bytes.get tmp i)
      done;
      conn.read_len <- conn.read_len + n;
      `Ok n
    end
  end

(* Shift buffer contents to remove processed data *)
let shift_buffer conn consumed =
  if consumed > 0 && consumed < conn.read_len then begin
    (* Move remaining data to start of buffer *)
    for i = 0 to conn.read_len - consumed - 1 do
      Bigarray.Array1.set conn.read_buf i
        (Bigarray.Array1.get conn.read_buf (consumed + i))
    done;
    conn.read_len <- conn.read_len - consumed
  end else if consumed >= conn.read_len then
    conn.read_len <- 0

(* Route request to handler *)
let route_request ~routes buf meth (target_span : Httpz.span) (headers @ local) (body_span : Httpz.span) =
  (* Extract path (before query string) *)
  let target = Httpz.span_to_string buf target_span in
  let path = match String.lsplit2 target ~on:'?' with
    | Some (p, _) -> p
    | None -> target
  in
  (* Find matching handler *)
  match List.Assoc.find routes ~equal:String.equal path with
  | Some handler -> handler buf meth target_span headers body_span
  | None -> not_found_response

(* Handle one request on connection *)
let handle_request conn ~routes =
  let buf = conn.read_buf in
  let len = conn.read_len in
  (* Parse request - uses stack-allocated return *)
  let #(status, req, headers) = Httpz.parse buf ~len in
  (* Extract fields from unboxed record to use as global values *)
  let body_off = req.#body_off in
  let version = req.#version in
  let meth = req.#meth in
  let target = req.#target in
  match status with
  | Httpz.Ok ->
    (* Check if body is complete *)
    let body_span = Httpz.body_span buf ~len ~body_off headers in
    let body_span_len = body_span.#len in
    let body_span_off = body_span.#off in
    if body_span_len = -1 then begin
      (* Need more data for body *)
      `Need_more
    end else begin
      (* Determine keep-alive *)
      conn.keep_alive <- Httpz.is_keep_alive buf headers version;
      (* Create body span - use extracted int values which are global *)
      let body_span_rec : Httpz.span = #{ off = body_span_off; len = body_span_len } in
      (* Route and handle - all data stays local until handler extracts what it needs *)
      let response = route_request ~routes buf meth target headers body_span_rec in
      write_response conn response version;
      (* Calculate bytes consumed *)
      let consumed = if body_span_len > 0 then
        body_span_off + body_span_len
      else
        body_off
      in
      shift_buffer conn consumed;
      if conn.keep_alive then `Continue else `Close
    end
  | Httpz.Partial ->
    `Need_more
  | Httpz.Headers_too_large ->
    conn.keep_alive <- false;
    write_response conn payload_too_large_response Httpz.HTTP_1_1;
    `Close
  | _ ->
    conn.keep_alive <- false;
    write_response conn bad_request_response Httpz.HTTP_1_1;
    `Close

(* Handle connection loop *)
let handle_connection conn ~routes =
  let rec loop () =
    (* Read data first if buffer is empty *)
    if conn.read_len = 0 then begin
      match read_more conn with
      | `Eof -> ()
      | `Buffer_full ->
        conn.keep_alive <- false;
        write_response conn payload_too_large_response Httpz.HTTP_1_1
      | `Ok _ -> loop ()
    end else begin
      (* Try to handle buffered data *)
      match handle_request conn ~routes with
      | `Continue -> loop ()
      | `Close -> ()
      | `Need_more ->
        (* Read more data *)
        match read_more conn with
        | `Eof -> ()
        | `Buffer_full ->
          conn.keep_alive <- false;
          write_response conn payload_too_large_response Httpz.HTTP_1_1
        | `Ok _ -> loop ()
    end
  in
  loop ()

(* Server configuration *)
type config = {
  port : int;
  backlog : int;
}

let default_config = {
  port = 8080;
  backlog = 128;
}

(* Create listening socket *)
let create_listen_socket config =
  let sock = Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Core_unix.setsockopt sock SO_REUSEADDR true;
  let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.bind_any, config.port) in
  Core_unix.bind sock ~addr;
  Core_unix.listen sock ~backlog:config.backlog;
  sock

(* Run server *)
let run ?(config = default_config) ~routes () =
  let sock = create_listen_socket config in
  printf "httpz server listening on port %d\n%!" config.port;
  (* Accept loop *)
  while true do
    let client_fd, client_addr = Core_unix.accept sock in
    (* Log connection *)
    let addr_str = match client_addr with
      | Core_unix.ADDR_INET (addr, port) ->
        sprintf "%s:%d" (Core_unix.Inet_addr.to_string addr) port
      | Core_unix.ADDR_UNIX path -> path
    in
    printf "[%s] Connected\n%!" addr_str;
    (* Handle connection *)
    let conn = create_conn client_fd in
    begin
      try
        handle_connection conn ~routes
      with
      | exn ->
        printf "[%s] Error: %s\n%!" addr_str (Exn.to_string exn)
    end;
    Core_unix.close client_fd;
    printf "[%s] Disconnected\n%!" addr_str
  done

(* Example handlers *)
let hello_handler _buf _meth _target (_headers @ local) _body =
  { status = Httpz.S200_OK;
    content_type = "text/plain";
    body = "Hello from httpz!" }

let echo_handler buf _meth (target : Httpz.span) (headers @ local) (body : Httpz.span) =
  let host = match Httpz.find_header headers Httpz.H_host with
    | Some h -> Httpz.span_to_string buf h.value
    | None -> "unknown"
  in
  let target_str = Httpz.span_to_string buf target in
  let body_str = if body.#len > 0 then Httpz.span_to_string buf body else "" in
  { status = Httpz.S200_OK;
    content_type = "text/plain";
    body = sprintf "Host: %s\nTarget: %s\nBody length: %d\nBody: %s" host target_str body.#len body_str }

let json_handler _buf _meth _target (_headers @ local) _body =
  { status = Httpz.S200_OK;
    content_type = "application/json";
    body = {|{"message": "Hello, JSON!", "server": "httpz"}|} }

(* Main *)
let () =
  let routes = [
    "/", hello_handler;
    "/hello", hello_handler;
    "/echo", echo_handler;
    "/json", json_handler;
  ] in
  run ~routes ()
