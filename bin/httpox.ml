(* httpox - HTTP server using httpl with Core libraries

   A clean reimplementation using Jane Street's Core for:
   - String operations
   - Option/Result handling
   - Unix operations via Core_unix
   - Command-line parsing via Core.Command
*)

open Core

(* Configuration - will be set from command line *)
let host = ref "0.0.0.0"
let port = ref 8080
let root = ref "."
let verbose = ref false

(* Constants *)
let read_size = 4096
let response_buf_size = 8192
let file_buf_size = 65536

(* Status code reason phrases *)
let reason_of_status = function
  | 200 -> "OK"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 500 -> "Internal Server Error"
  | code -> sprintf "Status %d" code

(* Check if path is safe (no directory traversal) *)
let is_safe_path path =
  let parts = String.split path ~on:'/' in
  let rec check depth = function
    | [] -> depth >= 0
    | "" :: rest -> check depth rest
    | "." :: rest -> check depth rest
    | ".." :: rest -> check (depth - 1) rest
    | _ :: rest -> check (depth + 1) rest
  in
  check 0 parts

(* Decode URL-encoded path *)
let decode_path path =
  let len = String.length path in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else match String.unsafe_get path i with
      | '%' when i + 2 < len ->
        let hex = String.sub path ~pos:(i + 1) ~len:2 in
        (try
           let code = Int.of_string ("0x" ^ hex) in
           Buffer.add_char buf (Char.of_int_exn code);
           loop (i + 3)
         with _ ->
           Buffer.add_char buf '%';
           loop (i + 1))
      | '+' ->
        Buffer.add_char buf ' ';
        loop (i + 1)
      | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  in
  loop 0

(* Logging helpers *)
let log fmt =
  if !verbose then eprintf fmt
  else Printf.ifprintf stderr fmt

let log_always fmt = eprintf fmt

(* Write all bytes to a file descriptor *)
let rec write_all fd buf ~pos ~len =
  if len > 0 then begin
    let n = Core_unix.write fd ~buf ~pos ~len in
    write_all fd buf ~pos:(pos + n) ~len:(len - n)
  end

(* Write a string to a file descriptor *)
let write_string fd s =
  write_all fd (Bytes.of_string s) ~pos:0 ~len:(String.length s)

(* Write a simple HTTP response *)
let write_response response_buf fd ~status ~headers ~body =
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers
  in
  write_all fd response_buf ~pos:0 ~len;
  Option.iter body ~f:(write_string fd)

(* Write response headers then stream file *)
let write_file_response response_buf fd ~status ~headers ~file_fd ~file_size =
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers
  in
  write_all fd response_buf ~pos:0 ~len;
  (* Stream the file *)
  let buf = Bytes.create file_buf_size in
  let mutable remaining = file_size in
  while remaining > 0 do
    let to_read = min file_buf_size remaining in
    let n = Core_unix.read file_fd ~buf ~pos:0 ~len:to_read in
    if n = 0 then remaining <- 0
    else begin
      write_all fd buf ~pos:0 ~len:n;
      remaining <- remaining - n
    end
  done

(* Get file stats, returns None if not found *)
let stat_file path =
  match Core_unix.stat path with
  | stats -> Some stats
  | exception Core_unix.Unix_error (ENOENT, _, _) -> None

(* Serve a static file *)
let serve_file response_buf fd full_path =
  match stat_file full_path with
  | None ->
    write_response response_buf fd ~status:404
      ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
      ~body:(Some "404 Not Found");
    true
  | Some stats ->
    let size = Int64.to_int_exn stats.st_size in
    let mime = Magic_mime.lookup (Filename.basename full_path) in
    let headers = [
      (Httpl.str "Content-Type", Httpl.str mime);
      (Httpl.str "Content-Length", Httpl.str (Int.to_string size));
      (Httpl.str "Connection", Httpl.str "keep-alive");
    ] in
    let file_fd = Core_unix.openfile full_path ~mode:[O_RDONLY] in
    Exn.protect
      ~f:(fun () ->
        write_file_response response_buf fd ~status:200 ~headers ~file_fd ~file_size:size;
        true)
      ~finally:(fun () -> Core_unix.close file_fd)

(* Handle a single request *)
let handle_request response_buf fd buf (req : Httpl.request) =
  let target = Httpl.span_to_string buf (Httpl.request_target req) in

  (* Only handle GET and HEAD *)
  let is_get = match req.meth with
    | Httpl.GET | Httpl.HEAD -> true
    | _ -> false
  in
  if not is_get then begin
    write_response response_buf fd ~status:405
      ~headers:[(Httpl.str "Content-Length", Httpl.str "22")]
      ~body:(Some "405 Method Not Allowed");
    false
  end else begin
    (* Parse path (strip query string) *)
    let path = match String.lsplit2 target ~on:'?' with
      | Some (p, _) -> p
      | None -> target
    in
    let path = decode_path path in

    (* Security check *)
    if not (is_safe_path path) then begin
      write_response response_buf fd ~status:403
        ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
        ~body:(Some "403 Forbidden");
      Httpl.is_keep_alive buf req
    end else begin
      let rel_path =
        if String.equal path "/" then ""
        else if String.length path > 0 && Char.equal (String.unsafe_get path 0) '/' then
          String.sub path ~pos:1 ~len:(String.length path - 1)
        else path
      in
      let full_path =
        if String.is_empty rel_path then !root
        else Filename.concat !root rel_path
      in

      match stat_file full_path with
      | None ->
        write_response response_buf fd ~status:404
          ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
          ~body:(Some "404 Not Found");
        Httpl.is_keep_alive buf req
      | Some stats ->
        (match stats.st_kind with
        | S_REG ->
          ignore (serve_file response_buf fd full_path : bool);
          Httpl.is_keep_alive buf req
        | S_DIR ->
          (* Try index.html *)
          let index_path = Filename.concat full_path "index.html" in
          (match stat_file index_path with
          | Some index_stats when Poly.equal index_stats.st_kind S_REG ->
            ignore (serve_file response_buf fd index_path : bool);
            Httpl.is_keep_alive buf req
          | _ ->
            write_response response_buf fd ~status:403
              ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
              ~body:(Some "403 Forbidden");
            Httpl.is_keep_alive buf req)
        | _ ->
          write_response response_buf fd ~status:404
            ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
            ~body:(Some "404 Not Found");
          Httpl.is_keep_alive buf req)
    end
  end

(* Format client address *)
let string_of_sockaddr = function
  | Core_unix.ADDR_INET (addr, port) ->
    sprintf "%s:%d" (Core_unix.Inet_addr.to_string addr) port
  | Core_unix.ADDR_UNIX path -> path

(* Log header details *)
let log_headers client_str buf (req : Httpl.request) =
  let target = Httpl.request_target req in
  log "[%s]   target: span{start=%d, len=%d} = %S\n"
    client_str target.#start target.#len (Httpl.span_to_string buf target);
  log "[%s]   headers (%d):\n" client_str (Httpl.Headers.count req.headers);
  let i = ref 0 in
  Httpl.Headers.iter (fun name name_span value ->
    let name_str = Httpl.header_name_to_string buf name in
    let value_str = Httpl.span_to_string buf value in
    log "[%s]     [%d] %S: %S\n" client_str !i name_str value_str;
    log "[%s]         name_span{start=%d, len=%d} value_span{start=%d, len=%d}\n"
      client_str name_span.#start name_span.#len value.#start value.#len;
    Int.incr i
  ) req.headers

(* Handle a connection *)
let handle_connection fd client_addr =
  let client_str = string_of_sockaddr client_addr in
  log "[%s] Connection opened\n" client_str;

  (* Per-connection buffers *)
  let response_buf = Bytes.create response_buf_size in
  let eof = ref false in
  let bytes_read = ref 0 in
  let fragment_count = ref 0 in

  (* Refill returns bool: true if data added, false for EOF *)
  let refill httpl_buf =
    if !eof then false
    else
      try
        (* Allocate fresh buffer for each read - zbuf keeps references (zero-copy) *)
        let data = Bytes.create read_size in
        let n = Core_unix.read fd ~buf:data ~pos:0 ~len:read_size in
        log "[%s] Read %d bytes\n" client_str n;
        if n = 0 then begin
          eof := true;
          false
        end else begin
          Int.incr fragment_count;
          bytes_read := !bytes_read + n;
          log "[%s] Zbuf push: fragment #%d, offset=0, len=%d, total_bytes=%d\n"
            client_str !fragment_count n !bytes_read;
          (* Show first 80 chars of data for debugging *)
          let preview_len = min n 80 in
          let preview = Bytes.To_string.sub data ~pos:0 ~len:preview_len in
          log "[%s]   data preview: %S%s\n"
            client_str preview (if n > 80 then "..." else "");
          Httpl.push httpl_buf data ~off:0 ~len:n;
          true
        end
      with
      | Core_unix.Unix_error (ECONNRESET, _, _) ->
        log "[%s] Connection reset\n" client_str;
        eof := true;
        false
      | Core_unix.Unix_error (err, _, _) ->
        log "[%s] Read error: %s\n" client_str (Core_unix.Error.message err);
        eof := true;
        false
  in
  let buf = Httpl.create () in

  (* Use effect-based refill handler *)
  Httpl.with_refill buf refill (fun () ->
    let rec loop () =
      match Httpl.try_parse_request buf with
      | Error Httpl.Partial when !eof ->
        log "[%s] Connection closed (EOF after %d bytes in %d fragments)\n"
          client_str !bytes_read !fragment_count
      | Error Httpl.Partial ->
        if not (Httpl.try_refill buf 1) then
          log "[%s] Connection closed during parse\n" client_str
        else loop ()
      | Error err ->
        log "[%s] Parse error: %s\n" client_str (Httpl.error_to_string err);
        write_response response_buf fd ~status:400
          ~headers:[(Httpl.str "Content-Length", Httpl.str "15")]
          ~body:(Some (Httpl.error_to_string err))
      | Ok req ->
        let target = Httpl.span_to_string buf (Httpl.request_target req) in
        let meth = match req.Httpl.meth with
          | Httpl.GET -> "GET" | Httpl.POST -> "POST" | Httpl.HEAD -> "HEAD"
          | Httpl.PUT -> "PUT" | Httpl.DELETE -> "DELETE" | _ -> "OTHER"
        in
        log "[%s] Parsed request: %s %s\n" client_str meth target;
        log_headers client_str buf req;
        let keep_alive =
          try handle_request response_buf fd buf req
          with exn ->
            log "[%s] Error handling request: %s\n" client_str (Exn.to_string exn);
            write_response response_buf fd ~status:500
              ~headers:[(Httpl.str "Content-Length", Httpl.str "25")]
              ~body:(Some "500 Internal Server Error");
            false
        in
        if keep_alive then loop ()
    in
    loop ()
  )

(* Create server socket *)
let create_server_socket ~host ~port =
  let sock = Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Core_unix.setsockopt sock SO_REUSEADDR true;
  let addr = Core_unix.Inet_addr.of_string host in
  Core_unix.bind sock ~addr:(ADDR_INET (addr, port));
  Core_unix.listen sock ~backlog:128;
  sock

(* Handle connection in a thread *)
let connection_handler (client_fd, client_addr) =
  Exn.protect
    ~f:(fun () -> handle_connection client_fd client_addr)
    ~finally:(fun () ->
      try Core_unix.close client_fd with _ -> ());
  ()

(* Main server loop *)
let run_server () =
  log_always "httpox starting on %s:%d, serving files from %s\n" !host !port !root;

  let server_sock = create_server_socket ~host:!host ~port:!port in

  (* Threaded server - spawn a thread per connection *)
  while true do
    let client_fd, client_addr = Core_unix.accept server_sock in
    let (_ : Caml_threads.Thread.t) = Caml_threads.Thread.create connection_handler (client_fd, client_addr) in
    ()
  done

(* Command-line interface using Core.Command *)
let command =
  Command.basic
    ~summary:"httpox - OxCaml HTTP server using Core"
    ~readme:(fun () ->
      "A static file HTTP server built with httpl and Jane Street Core libraries.\n\
       Uses zero-copy parsing with OxCaml unboxed types and effects.")
    (let%map_open.Command
       host_arg = flag "-h" (optional_with_default "0.0.0.0" string)
         ~doc:"HOST Host to bind to (default: 0.0.0.0)"
     and port_arg = flag "-p" (optional_with_default 8080 int)
         ~doc:"PORT Port to listen on (default: 8080)"
     and root_arg = flag "-r" (optional_with_default "." string)
         ~doc:"PATH Root directory to serve (default: .)"
     and verbose_arg = flag "-v" no_arg
         ~doc:" Enable verbose logging"
     in
     fun () ->
       host := host_arg;
       port := port_arg;
       root := root_arg;
       verbose := verbose_arg;
       run_server ())

let () = Command_unix.run command
