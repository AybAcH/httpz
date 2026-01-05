(* eewwu - Simple static file HTTP server using httpl, Unix and threads *)

(* Hardcoded config *)
let host = "0.0.0.0"
let port = 8080
let root = "."
let read_size = 4096  (* Read buffer size per fragment *)
let buffer_size = 65536

(* Status code reason phrases *)
let reason_of_status = function
  | 200 -> "OK"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 500 -> "Internal Server Error"
  | code -> Printf.sprintf "Status %d" code

(* Check if path is safe (no directory traversal) *)
let is_safe_path path =
  let parts = String.split_on_char '/' path in
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
    else match path.[i] with
      | '%' when i + 2 < len ->
        let hex = String.sub path (i + 1) 2 in
        (try
           let code = int_of_string ("0x" ^ hex) in
           Buffer.add_char buf (Char.chr code);
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

(* Write all bytes to a file descriptor *)
let rec write_all fd buf off len =
  if len > 0 then
    let n = Unix.write fd buf off len in
    write_all fd buf (off + n) (len - n)

(* Write a string to a file descriptor *)
let write_string fd s =
  write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)

(* Write a simple HTTP response *)
let write_response response_buf fd ~status ~headers ~body =
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers
  in
  write_all fd response_buf 0 len;
  match body with
  | Some content -> write_string fd content
  | None -> ()

(* Write response headers then stream file *)
let write_file_response response_buf fd ~status ~headers ~file_fd ~file_size =
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers
  in
  write_all fd response_buf 0 len;
  (* Stream the file *)
  let buf = Bytes.create buffer_size in
  let remaining = ref file_size in
  while !remaining > 0 do
    let to_read = min buffer_size !remaining in
    let n = Unix.read file_fd buf 0 to_read in
    if n = 0 then remaining := 0
    else begin
      write_all fd buf 0 n;
      remaining := !remaining - n
    end
  done

(* Get file stats, returns None if not found *)
let stat_file path =
  try Some (Unix.stat path)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

(* Serve a static file *)
let serve_file response_buf fd full_path =
  match stat_file full_path with
  | None ->
    write_response response_buf fd ~status:404
      ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
      ~body:(Some "404 Not Found");
    true
  | Some stat ->
    let size = stat.Unix.st_size in
    let mime = Magic_mime.lookup (Filename.basename full_path) in
    let headers = [
      (Httpl.str "Content-Type", Httpl.str mime);
      (Httpl.str "Content-Length", Httpl.str (string_of_int size));
      (Httpl.str "Connection", Httpl.str "keep-alive");
    ] in
    let file_fd = Unix.openfile full_path [Unix.O_RDONLY] 0 in
    (try
       write_file_response response_buf fd ~status:200 ~headers ~file_fd ~file_size:size;
       Unix.close file_fd;
       true
     with exn ->
       Unix.close file_fd;
       raise exn)

(* Handle a single request - req is already parsed *)
let handle_request response_buf fd buf (req : Httpl.request) =
  let target = Httpl.span_to_string buf req.target in

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
    let path = match String.index_opt target '?' with
      | Some i -> String.sub target 0 i
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
        if path = "/" then ""
        else if String.length path > 0 && path.[0] = '/' then
          String.sub path 1 (String.length path - 1)
        else path
      in
      let full_path =
        if rel_path = "" then root
        else Filename.concat root rel_path
      in

      match stat_file full_path with
      | None ->
        write_response response_buf fd ~status:404
          ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
          ~body:(Some "404 Not Found");
        Httpl.is_keep_alive buf req
      | Some stat ->
        if stat.Unix.st_kind = Unix.S_REG then begin
          ignore (serve_file response_buf fd full_path);
          Httpl.is_keep_alive buf req
        end else if stat.Unix.st_kind = Unix.S_DIR then begin
          (* Try index.html *)
          let index_path = Filename.concat full_path "index.html" in
          match stat_file index_path with
          | Some index_stat when index_stat.Unix.st_kind = Unix.S_REG ->
            ignore (serve_file response_buf fd index_path);
            Httpl.is_keep_alive buf req
          | _ ->
            write_response response_buf fd ~status:403
              ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
              ~body:(Some "403 Forbidden");
            Httpl.is_keep_alive buf req
        end else begin
          write_response response_buf fd ~status:404
            ~headers:[(Httpl.str "Content-Length", Httpl.str "13")]
            ~body:(Some "404 Not Found");
          Httpl.is_keep_alive buf req
        end
    end
  end

(* Format client address *)
let string_of_sockaddr = function
  | Unix.ADDR_INET (addr, port) ->
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port
  | Unix.ADDR_UNIX path -> path

(* Log header details *)
let log_headers client_str buf (req : Httpl.request) =
  Printf.eprintf "[%s]   target: span{start=%d, len=%d} = %S\n%!"
    client_str req.target.start req.target.len (Httpl.span_to_string buf req.target);
  Printf.eprintf "[%s]   headers (%d):\n%!" client_str (Httpl.Headers.count req.headers);
  let i = ref 0 in
  Httpl.Headers.iter (fun name name_span value ->
    let name_str = Httpl.header_name_to_string buf name in
    let value_str = Httpl.span_to_string buf value in
    Printf.eprintf "[%s]     [%d] %S: %S\n%!" client_str !i name_str value_str;
    Printf.eprintf "[%s]         name_span{start=%d, len=%d} value_span{start=%d, len=%d}\n%!"
      client_str name_span.start name_span.len value.start value.len;
    incr i
  ) req.headers

(* Handle a connection *)
let handle_connection fd client_addr =
  let client_str = string_of_sockaddr client_addr in
  Printf.eprintf "[%s] Connection opened\n%!" client_str;
  (* Per-thread buffers *)
  let response_buf = Bytes.create 8192 in
  let eof = ref false in
  let bytes_read = ref 0 in
  let fragment_count = ref 0 in
  let refill httpl_buf =
    if !eof then
      Httpl.push httpl_buf Bytes.empty ~off:0 ~len:0  (* EOF marker *)
    else
      try
        (* Allocate fresh buffer for each read - zbuf keeps references (zero-copy) *)
        let data = Bytes.create read_size in
        let n = Unix.read fd data 0 read_size in
        Printf.eprintf "[%s] Read %d bytes\n%!" client_str n;
        if n = 0 then begin
          eof := true;
          Httpl.push httpl_buf Bytes.empty ~off:0 ~len:0  (* EOF marker *)
        end else begin
          incr fragment_count;
          bytes_read := !bytes_read + n;
          (* Log the zbuf push *)
          Printf.eprintf "[%s] Zbuf push: fragment #%d, offset=0, len=%d, total_bytes=%d\n%!"
            client_str !fragment_count n !bytes_read;
          (* Show first 80 chars of data for debugging *)
          let preview_len = min n 80 in
          let preview = Bytes.sub_string data 0 preview_len in
          Printf.eprintf "[%s]   data preview: %S%s\n%!"
            client_str preview (if n > 80 then "..." else "");
          Httpl.push httpl_buf data ~off:0 ~len:n
        end
      with
      | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
        Printf.eprintf "[%s] Connection reset\n%!" client_str;
        eof := true;
        Httpl.push httpl_buf Bytes.empty ~off:0 ~len:0
      | Unix.Unix_error (err, _, _) ->
        Printf.eprintf "[%s] Read error: %s\n%!" client_str (Unix.error_message err);
        eof := true;
        Httpl.push httpl_buf Bytes.empty ~off:0 ~len:0
  in
  let buf = Httpl.create () in

  (* Use effect-based refill handler *)
  Httpl.with_refill buf refill (fun () ->
    let rec loop () =
      match Httpl.try_parse_request buf with
      | Error Httpl.Partial when !eof ->
        (* Connection closed cleanly *)
        Printf.eprintf "[%s] Connection closed (EOF after %d bytes in %d fragments)\n%!"
          client_str !bytes_read !fragment_count
      | Error Httpl.Partial ->
        (* Need more data - effect will trigger refill *)
        if not (Httpl.try_refill buf 1) then
          Printf.eprintf "[%s] Connection closed during parse\n%!" client_str
        else loop ()
      | Error err ->
        Printf.eprintf "[%s] Parse error: %s\n%!" client_str (Httpl.error_to_string err);
        write_response response_buf fd ~status:400
          ~headers:[(Httpl.str "Content-Length", Httpl.str "15")]
          ~body:(Some (Httpl.error_to_string err));
        ()
      | Ok req ->
        let target = Httpl.span_to_string buf req.Httpl.target in
        let meth = match req.Httpl.meth with
          | Httpl.GET -> "GET" | Httpl.POST -> "POST" | Httpl.HEAD -> "HEAD"
          | Httpl.PUT -> "PUT" | Httpl.DELETE -> "DELETE" | _ -> "OTHER"
        in
        Printf.eprintf "[%s] Parsed request: %s %s\n%!" client_str meth target;
        log_headers client_str buf req;
        let keep_alive =
          try handle_request response_buf fd buf req
          with exn ->
            Printf.eprintf "[%s] Error handling request: %s\n%!" client_str (Printexc.to_string exn);
            write_response response_buf fd ~status:500
              ~headers:[(Httpl.str "Content-Length", Httpl.str "25")]
              ~body:(Some "500 Internal Server Error");
            false
        in
        if keep_alive then loop ()
    in
    loop ()
  )

(* Set up server socket *)
let create_server_socket () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let addr = Unix.inet_addr_of_string host in
  Unix.bind sock (Unix.ADDR_INET (addr, port));
  Unix.listen sock 128;
  sock

(* Handle connection in a thread *)
let connection_handler (client_fd, client_addr) =
  (try
     handle_connection client_fd client_addr
   with exn ->
     Printf.eprintf "Connection error: %s\n%!" (Printexc.to_string exn));
  (try Unix.close client_fd with _ -> ())

(* Main server loop *)
let () =
  Printf.printf "eewwu starting on %s:%d, serving files from %s (threaded)\n%!" host port root;

  let server_sock = create_server_socket () in

  (* Threaded server - spawn a thread per connection *)
  while true do
    let client_fd, client_addr = Unix.accept server_sock in
    let _thread = Thread.create connection_handler (client_fd, client_addr) in
    ()
  done
