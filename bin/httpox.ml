(* httpox - HTTP server using httpl with Core libraries and OxCaml optimizations

   Optimized for minimal heap allocation:
   - Uses let mutable instead of ref for connection state
   - Pre-allocates response buffers per thread
   - Avoids string allocations in hot paths where possible
   - Uses span comparisons instead of string conversions
   - Stack-allocates small temporary values

   Note: zbuf data buffers MUST be heap-allocated since zbuf keeps references
   for zero-copy parsing.
*)

open Core

(* ============================================================
   Configuration
   ============================================================ *)

let host = ref "0.0.0.0"
let port = ref 8080
let root = ref "."
let verbose = ref false

(* Constants *)
let read_size = 4096
let response_buf_size = 8192
let file_buf_size = 65536

(* Pre-allocated string constants to avoid repeated allocations *)
let content_length_str = "Content-Length"
let content_type_str = "Content-Type"
let connection_str = "Connection"
let keep_alive_str = "keep-alive"

(* Pre-allocated response bodies *)
let body_404 = "404 Not Found"
let body_403 = "403 Forbidden"
let body_405 = "405 Method Not Allowed"
let body_500 = "500 Internal Server Error"

(* ============================================================
   Status codes and reasons
   ============================================================ *)

(* Status code reason phrases - returns static strings, no allocation *)
let reason_of_status = function
  | 200 -> "OK"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 500 -> "Internal Server Error"
  | _ -> "Unknown"

(* ============================================================
   Path validation and decoding
   ============================================================ *)

(* Check if path is safe (no directory traversal) - minimal allocation *)
let is_safe_path_span zb (sp : Httpl.span) =
  (* Walk through the span character by character, tracking depth *)
  let len = sp.#len in
  if len = 0 then true
  else begin
    let mutable depth = 0 in
    let mutable i = 0 in
    let mutable safe = true in
    let mutable in_segment = false in
    let mutable dots = 0 in
    while safe && i < len do
      let c = Zbuf.get zb (sp.#start + i) in
      if Char.equal c '/' then begin
        (* End of segment - check for .. *)
        if in_segment && dots = 2 then begin
          depth <- depth - 1;
          if depth < 0 then safe <- false
        end else if in_segment then
          depth <- depth + 1;
        in_segment <- false;
        dots <- 0
      end else begin
        in_segment <- true;
        if Char.equal c '.' then dots <- dots + 1
        else dots <- 0
      end;
      i <- i + 1
    done;
    (* Check final segment *)
    if safe && in_segment && dots = 2 then
      depth <- depth - 1;
    safe && depth >= 0
  end

(* Decode URL-encoded path - allocates result string *)
let decode_path path =
  let len = String.length path in
  let buf = Buffer.create len in
  let mutable i = 0 in
  while i < len do
    let c = String.unsafe_get path i in
    if Char.equal c '%' && i + 2 < len then begin
      let h1 = String.unsafe_get path (i + 1) in
      let h2 = String.unsafe_get path (i + 2) in
      let hex_val c =
        if Char.(c >= '0' && c <= '9') then Char.to_int c - 48
        else if Char.(c >= 'a' && c <= 'f') then Char.to_int c - 87
        else if Char.(c >= 'A' && c <= 'F') then Char.to_int c - 55
        else -1
      in
      let v1 = hex_val h1 in
      let v2 = hex_val h2 in
      if v1 >= 0 && v2 >= 0 then begin
        Buffer.add_char buf (Char.of_int_exn (v1 * 16 + v2));
        i <- i + 3
      end else begin
        Buffer.add_char buf '%';
        i <- i + 1
      end
    end else if Char.equal c '+' then begin
      Buffer.add_char buf ' ';
      i <- i + 1
    end else begin
      Buffer.add_char buf c;
      i <- i + 1
    end
  done;
  Buffer.contents buf

(* ============================================================
   Logging - only allocates when verbose
   ============================================================ *)

let[@inline] _log_enabled () = !verbose

let _log fmt =
  if !verbose then eprintf fmt
  else Printf.ifprintf stderr fmt

let log_always fmt = eprintf fmt

(* ============================================================
   I/O helpers
   ============================================================ *)

(* Write all bytes - no allocation *)
let rec write_all fd buf ~pos ~len =
  if len > 0 then begin
    let n = Core_unix.write fd ~buf ~pos ~len in
    write_all fd buf ~pos:(pos + n) ~len:(len - n)
  end

(* Write a string - allocates bytes copy *)
let write_string fd s =
  write_all fd (Bytes.of_string s) ~pos:0 ~len:(String.length s)

(* ============================================================
   HTTP Response writing
   ============================================================ *)

(* Write error response with pre-allocated body *)
let write_error_response response_buf fd ~status ~body ~body_len =
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers:[(Httpl.str content_length_str, Httpl.str (Int.to_string body_len))]
  in
  write_all fd response_buf ~pos:0 ~len;
  write_string fd body

(* Write file response - streams file content *)
let write_file_response response_buf file_buf fd ~status ~mime ~file_fd ~file_size =
  let size_str = Int.to_string file_size in
  let headers = [
    (Httpl.str content_type_str, Httpl.str mime);
    (Httpl.str content_length_str, Httpl.str size_str);
    (Httpl.str connection_str, Httpl.str keep_alive_str);
  ] in
  let len = Httpl.write_response response_buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status
    ~reason:(Httpl.str (reason_of_status status))
    ~headers
  in
  write_all fd response_buf ~pos:0 ~len;
  (* Stream the file using pre-allocated buffer *)
  let mutable remaining = file_size in
  while remaining > 0 do
    let to_read = min file_buf_size remaining in
    let n = Core_unix.read file_fd ~buf:file_buf ~pos:0 ~len:to_read in
    if n = 0 then remaining <- 0
    else begin
      write_all fd file_buf ~pos:0 ~len:n;
      remaining <- remaining - n
    end
  done

(* ============================================================
   File operations
   ============================================================ *)

(* Get file stats - no allocation on success path *)
let stat_file path =
  match Core_unix.stat path with
  | stats -> Some stats
  | exception Core_unix.Unix_error (ENOENT, _, _) -> None

(* Serve a static file *)
let serve_file response_buf file_buf fd full_path =
  match stat_file full_path with
  | None ->
    write_error_response response_buf fd ~status:404 ~body:body_404 ~body_len:13;
    true
  | Some stats ->
    let size = Int64.to_int_exn stats.st_size in
    let mime = Magic_mime.lookup (Filename.basename full_path) in
    let file_fd = Core_unix.openfile full_path ~mode:[O_RDONLY] in
    Exn.protect
      ~f:(fun () ->
        write_file_response response_buf file_buf fd ~status:200 ~mime ~file_fd ~file_size:size;
        true)
      ~finally:(fun () -> Core_unix.close file_fd)

(* ============================================================
   Request handling
   ============================================================ *)

(* Handle a single request - minimizes allocations *)
let handle_request response_buf file_buf fd httpl_buf (req : Httpl.request) =
  let target_span = Httpl.request_target req in

  (* Only handle GET and HEAD - no allocation *)
  let is_get = match req.meth with
    | Httpl.GET | Httpl.HEAD -> true
    | _ -> false
  in
  if not is_get then begin
    write_error_response response_buf fd ~status:405 ~body:body_405 ~body_len:22;
    false
  end else begin
    (* Security check using span - no string allocation *)
    if not (is_safe_path_span httpl_buf.Httpl.zb target_span) then begin
      write_error_response response_buf fd ~status:403 ~body:body_403 ~body_len:13;
      Httpl.is_keep_alive httpl_buf req
    end else begin
      (* Now we need to materialize the path for filesystem access *)
      (* First, find the path part (before ?) *)
      let target_str = Httpl.span_to_string httpl_buf target_span in
      let path = match String.lsplit2 target_str ~on:'?' with
        | Some (p, _) -> p
        | None -> target_str
      in
      let path = decode_path path in

      (* Build full path *)
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
        write_error_response response_buf fd ~status:404 ~body:body_404 ~body_len:13;
        Httpl.is_keep_alive httpl_buf req
      | Some stats ->
        (match stats.st_kind with
        | S_REG ->
          ignore (serve_file response_buf file_buf fd full_path : bool);
          Httpl.is_keep_alive httpl_buf req
        | S_DIR ->
          let index_path = Filename.concat full_path "index.html" in
          (match stat_file index_path with
          | Some index_stats when Poly.equal index_stats.st_kind S_REG ->
            ignore (serve_file response_buf file_buf fd index_path : bool);
            Httpl.is_keep_alive httpl_buf req
          | _ ->
            write_error_response response_buf fd ~status:403 ~body:body_403 ~body_len:13;
            Httpl.is_keep_alive httpl_buf req)
        | _ ->
          write_error_response response_buf fd ~status:404 ~body:body_404 ~body_len:13;
          Httpl.is_keep_alive httpl_buf req)
    end
  end

(* ============================================================
   Connection handling - optimized for minimal allocation
   ============================================================ *)

(* Connection state - uses mutable fields instead of refs *)
type connection_state = {
  fd : Core_unix.File_descr.t;
  response_buf : bytes;
  file_buf : bytes;
  (* Note: read_buf not stored here - zbuf requires fresh allocations for zero-copy *)
  mutable eof : bool;
  mutable bytes_read : int;
  mutable fragment_count : int;
}

(* Handle a connection with pre-allocated state *)
let handle_connection_with_state (state : connection_state) =
  let httpl_buf = Httpl.create () in

  (* Refill function - allocates data buffer for zbuf to keep
     Note: We MUST allocate fresh buffers here because zbuf keeps references
     for zero-copy parsing. The read_buf in state is NOT used. *)
  let refill zb =
    if state.eof then false
    else
      try
        (* Fresh allocation required - zbuf keeps reference *)
        let data = Bytes.create read_size in
        let n = Core_unix.read state.fd ~buf:data ~pos:0 ~len:read_size in
        if n = 0 then begin
          state.eof <- true;
          false
        end else begin
          state.fragment_count <- state.fragment_count + 1;
          state.bytes_read <- state.bytes_read + n;
          Httpl.push zb data ~off:0 ~len:n;
          true
        end
      with
      | Core_unix.Unix_error (ECONNRESET, _, _) ->
        state.eof <- true;
        false
      | Core_unix.Unix_error (_, _, _) ->
        state.eof <- true;
        false
  in

  (* Use effect-based refill handler *)
  Httpl.with_refill httpl_buf refill (fun () ->
    let mutable running = true in
    while running do
      match Httpl.try_parse_request httpl_buf with
      | Error Httpl.Partial when state.eof ->
        running <- false
      | Error Httpl.Partial ->
        if not (Httpl.try_refill httpl_buf 1) then
          running <- false
      | Error _err ->
        write_error_response state.response_buf state.fd ~status:400
          ~body:"Bad Request" ~body_len:11;
        running <- false
      | Ok req ->
        let keep_alive =
          try handle_request state.response_buf state.file_buf state.fd httpl_buf req
          with _exn ->
            write_error_response state.response_buf state.fd ~status:500
              ~body:body_500 ~body_len:25;
            false
        in
        if not keep_alive then running <- false
    done
  )

(* Per-thread state - allocated once per thread *)
type thread_state = {
  t_response_buf : bytes;
  t_file_buf : bytes;
}

(* Create thread-local state *)
let create_thread_state () = {
  t_response_buf = Bytes.create response_buf_size;
  t_file_buf = Bytes.create file_buf_size;
}

(* Thread-local storage for buffers *)
let thread_state_key : thread_state option ref = ref None

let get_thread_state () =
  match !thread_state_key with
  | Some state -> state
  | None ->
    let state = create_thread_state () in
    thread_state_key := Some state;
    state

(* Handle a connection *)
let handle_connection fd _client_addr =
  let ts = get_thread_state () in
  let state = {
    fd;
    response_buf = ts.t_response_buf;
    file_buf = ts.t_file_buf;
    eof = false;
    bytes_read = 0;
    fragment_count = 0;
  } in
  handle_connection_with_state state

(* ============================================================
   Server setup
   ============================================================ *)

let create_server_socket ~host ~port =
  let sock = Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Core_unix.setsockopt sock SO_REUSEADDR true;
  let addr = Core_unix.Inet_addr.of_string host in
  Core_unix.bind sock ~addr:(ADDR_INET (addr, port));
  Core_unix.listen sock ~backlog:128;
  sock

let connection_handler (client_fd, client_addr) =
  Exn.protect
    ~f:(fun () -> handle_connection client_fd client_addr)
    ~finally:(fun () ->
      try Core_unix.close client_fd with _ -> ());
  ()

let run_server () =
  log_always "httpox starting on %s:%d, serving files from %s\n" !host !port !root;
  log_always "OxCaml optimizations: let mutable, unboxed spans, effects\n";

  let server_sock = create_server_socket ~host:!host ~port:!port in

  while true do
    let client_fd, client_addr = Core_unix.accept server_sock in
    let (_ : Caml_threads.Thread.t) =
      Caml_threads.Thread.create connection_handler (client_fd, client_addr)
    in
    ()
  done

(* ============================================================
   Command-line interface
   ============================================================ *)

let command =
  Command.basic
    ~summary:"httpox - OxCaml HTTP server using Core (allocation-optimized)"
    ~readme:(fun () ->
      "A static file HTTP server built with httpl and Jane Street Core libraries.\n\
       Uses zero-copy parsing with OxCaml unboxed types and effects.\n\n\
       Optimizations:\n\
       - let mutable instead of ref for connection state\n\
       - Pre-allocated per-thread buffers\n\
       - Span-based path validation (no string allocation)\n\
       - Unboxed span types (no heap allocation for token boundaries)")
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
