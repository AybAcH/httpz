(* test_httpl.ml - Tests for zero-copy HTTP/1.1 parser and serializer *)

open Httpl

(* ============================================================
   Test Utilities
   ============================================================ *)

(** Create a buffer and refill function from a string *)
let buffer_and_refill_of_string s =
  let refilled = ref false in
  let buf = create () in
  let refill buf =
    if !refilled then
      push buf Bytes.empty ~off:0 ~len:0  (* EOF marker *)
    else begin
      refilled := true;
      push buf (Bytes.of_string s) ~off:0 ~len:(String.length s)
    end
  in
  (buf, refill)

(** Create a buffer and refill function from chunks *)
let buffer_and_refill_of_chunks chunks =
  let remaining = ref chunks in
  let buf = create () in
  let refill buf =
    match !remaining with
    | [] -> push buf Bytes.empty ~off:0 ~len:0  (* EOF marker *)
    | chunk :: rest ->
      remaining := rest;
      push buf (Bytes.of_string chunk) ~off:0 ~len:(String.length chunk)
  in
  (buf, refill)

(** Run a test function with a string buffer *)
let with_string_buffer s f =
  let buf, refill = buffer_and_refill_of_string s in
  with_refill buf refill (fun () -> f buf)

(** Run a test function with a chunked buffer *)
let with_chunked_buffer chunks f =
  let buf, refill = buffer_and_refill_of_chunks chunks in
  with_refill buf refill (fun () -> f buf)

(** Assert helper *)
let assert_equal ~msg expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL: %s\n  expected: %S (len=%d)\n  actual:   %S (len=%d)\n"
      msg expected (String.length expected) actual (String.length actual);
    assert false
  end

let assert_equal_int ~msg expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL: %s\n  expected: %d\n  actual: %d\n"
      msg expected actual;
    assert false
  end

let assert_true ~msg cond =
  if not cond then begin
    Printf.printf "FAIL: %s\n" msg;
    assert false
  end

let pass msg = Printf.printf "PASS: %s\n" msg

(* ============================================================
   Buffer Tests
   ============================================================ *)

let test_buffer_simple () =
  with_string_buffer "hello" (fun buf ->
    (* peek auto-refills *)
    assert_true ~msg:"peek 0" (peek buf 0 = 'h');
    assert_true ~msg:"peek 4" (peek buf 4 = 'o');
    pass "buffer_simple")

let test_buffer_refill () =
  with_chunked_buffer ["hel"; "lo w"; "orld"] (fun buf ->
    (* peek auto-refills across chunks *)
    assert_true ~msg:"peek 0" (peek buf 0 = 'h');
    assert_true ~msg:"peek 5" (peek buf 5 = ' ');
    assert_true ~msg:"peek 10" (peek buf 10 = 'd');
    pass "buffer_refill")

let test_buffer_eof () =
  with_string_buffer "abc" (fun buf ->
    (* Can peek available data *)
    assert_true ~msg:"peek 0" (peek buf 0 = 'a');
    assert_true ~msg:"peek 2" (peek buf 2 = 'c');
    (* Peeking beyond EOF raises End_of_file *)
    (try
      ignore (peek buf 5);
      assert false  (* Should have raised *)
    with End_of_file -> ());
    pass "buffer_eof")

(* ============================================================
   Span Tests
   ============================================================ *)

let test_span_to_string () =
  with_string_buffer "hello world" (fun buf ->
    (* peek triggers refill *)
    ignore (peek buf 10);
    let sp = { start = 0; len = 5 } in
    assert_equal ~msg:"span_to_string" "hello" (span_to_string buf sp);
    let sp2 = { start = 6; len = 5 } in
    assert_equal ~msg:"span_to_string 2" "world" (span_to_string buf sp2);
    pass "span_to_string")

let test_span_equal () =
  with_string_buffer "GET /path HTTP/1.1" (fun buf ->
    (* peek triggers refill *)
    ignore (peek buf 17);
    let sp = { start = 0; len = 3 } in
    assert_true ~msg:"span_equal GET" (span_equal buf sp "GET");
    assert_true ~msg:"span_equal not POST" (not (span_equal buf sp "POST"));
    pass "span_equal")

let test_span_equal_caseless () =
  with_string_buffer "Content-Type: text/html" (fun buf ->
    (* peek triggers refill *)
    ignore (peek buf 22);
    let name_span = { start = 0; len = 12 } in
    assert_true ~msg:"caseless match" (span_equal_caseless buf name_span "content-type");
    assert_true ~msg:"caseless match 2" (span_equal_caseless buf name_span "CONTENT-TYPE");
    pass "span_equal_caseless")

(* ============================================================
   Request Parsing Tests
   ============================================================ *)

let test_parse_simple_request () =
  with_string_buffer "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"method is GET" (req.meth = GET);
    assert_equal ~msg:"target" "/index.html" (span_to_string buf req.target);
    assert_true ~msg:"version is 1.1" (req.version = HTTP_1_1);
    assert_equal_int ~msg:"header count" 1 (Headers.count req.headers);
    assert_equal ~msg:"header value" "example.com"
      (match get_header buf req "Host" with Some v -> v | None -> "");
    pass "parse_simple_request")

let test_parse_request_with_body_headers () =
  with_string_buffer
    "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     Content-Length: 42\r\n\
     \r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"method is POST" (req.meth = POST);
    assert_equal ~msg:"target" "/api/data" (span_to_string buf req.target);
    assert_equal_int ~msg:"header count" 3 (Headers.count req.headers);
    (* Test header lookup *)
    (match get_header buf req "Content-Type" with
     | Some v -> assert_equal ~msg:"Content-Type header" "application/json" v
     | None -> assert false);
    (match get_header buf req "content-length" with  (* case insensitive *)
     | Some v -> assert_equal ~msg:"Content-Length header" "42" v
     | None -> assert false);
    (match content_length buf req with
     | Some v -> assert_true ~msg:"content_length function" (v = 42L)
     | None -> assert false);
    pass "parse_request_with_body_headers")

let test_parse_all_methods () =
  let methods = [
    ("GET", GET); ("POST", POST); ("HEAD", HEAD);
    ("PUT", PUT); ("DELETE", DELETE); ("PATCH", PATCH);
    ("OPTIONS", OPTIONS); ("TRACE", TRACE); ("CONNECT", CONNECT);
  ] in
  List.iter (fun (s, expected) ->
    with_string_buffer (s ^ " / HTTP/1.1\r\n\r\n") (fun buf ->
      let req = parse_request buf in
      assert_true ~msg:("method " ^ s) (req.meth = expected))
  ) methods;
  pass "parse_all_methods"

let test_parse_other_method () =
  with_string_buffer "CUSTOM / HTTP/1.1\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    match req.meth with
    | Other sp ->
      assert_equal ~msg:"custom method" "CUSTOM" (span_to_string buf sp);
      pass "parse_other_method"
    | _ -> assert false)

let test_parse_http10 () =
  with_string_buffer "GET / HTTP/1.0\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"version is 1.0" (req.version = HTTP_1_0);
    pass "parse_http10")

let test_parse_chunked_in_chunks () =
  (* Parse a request that arrives in multiple chunks *)
  with_chunked_buffer [
    "GET /pa";
    "th HTTP/1.";
    "1\r\nHo";
    "st: exam";
    "ple.com\r\n\r\n"
  ] (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"method is GET" (req.meth = GET);
    assert_equal ~msg:"target" "/path" (span_to_string buf req.target);
    assert_equal_int ~msg:"header count" 1 (Headers.count req.headers);
    pass "parse_chunked_in_chunks")

let test_parse_multiple_headers () =
  with_string_buffer
    "GET / HTTP/1.1\r\n\
     Accept: text/html\r\n\
     Accept: application/json\r\n\
     Host: example.com\r\n\
     \r\n" (fun buf ->
    let req = parse_request buf in
    assert_equal_int ~msg:"header count" 3 (Headers.count req.headers);
    let accepts = get_headers_by_name buf req H_accept in
    assert_equal_int ~msg:"accept count" 2 (List.length accepts);
    assert_true ~msg:"first accept"
      (List.mem "text/html" accepts);
    assert_true ~msg:"second accept"
      (List.mem "application/json" accepts);
    pass "parse_multiple_headers")

let test_parse_ows_trimming () =
  with_string_buffer "GET / HTTP/1.1\r\nHost:   example.com  \r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_equal ~msg:"trimmed value" "example.com"
      (match get_header buf req "Host" with Some v -> v | None -> "");
    pass "parse_ows_trimming")

(* ============================================================
   Response Parsing Tests
   ============================================================ *)

let test_parse_simple_response () =
  with_string_buffer "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello" (fun buf ->
    let resp = parse_response buf in
    assert_true ~msg:"version is 1.1" (resp.version = HTTP_1_1);
    assert_equal_int ~msg:"status" 200 resp.status;
    assert_equal ~msg:"reason" "OK" (span_to_string buf resp.reason);
    assert_equal_int ~msg:"header count" 1 (Headers.count resp.headers);
    pass "parse_simple_response")

let test_parse_response_various_status () =
  let test_status code reason =
    let line = Printf.sprintf "HTTP/1.1 %d %s\r\n\r\n" code reason in
    with_string_buffer line (fun buf ->
      let resp = parse_response buf in
      assert_equal_int ~msg:("status " ^ string_of_int code) code resp.status;
      assert_equal ~msg:("reason " ^ reason) reason (span_to_string buf resp.reason))
  in
  test_status 200 "OK";
  test_status 201 "Created";
  test_status 301 "Moved Permanently";
  test_status 400 "Bad Request";
  test_status 404 "Not Found";
  test_status 500 "Internal Server Error";
  pass "parse_response_various_status"

let test_parse_chunked_response () =
  with_string_buffer
    "HTTP/1.1 200 OK\r\n\
     Transfer-Encoding: chunked\r\n\
     \r\n" (fun buf ->
    let resp = parse_response buf in
    assert_true ~msg:"is_chunked" (is_chunked_resp buf resp);
    pass "parse_chunked_response")

(* ============================================================
   Materialization Tests
   ============================================================ *)

let test_method_to_string () =
  with_string_buffer "CUSTOM / HTTP/1.1\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_equal ~msg:"GET string" "GET" (method_to_string buf GET);
    assert_equal ~msg:"POST string" "POST" (method_to_string buf POST);
    assert_equal ~msg:"CUSTOM string" "CUSTOM" (method_to_string buf req.meth);
    pass "method_to_string")

let test_version_to_string () =
  assert_equal ~msg:"HTTP/1.1" "HTTP/1.1" (version_to_string HTTP_1_1);
  assert_equal ~msg:"HTTP/1.0" "HTTP/1.0" (version_to_string HTTP_1_0);
  pass "version_to_string"

let test_keep_alive () =
  (* HTTP/1.1 defaults to keep-alive *)
  with_string_buffer "GET / HTTP/1.1\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"1.1 default keep-alive" (is_keep_alive buf req));

  (* HTTP/1.0 defaults to close *)
  with_string_buffer "GET / HTTP/1.0\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"1.0 default close" (not (is_keep_alive buf req)));

  (* Explicit Connection: close *)
  with_string_buffer "GET / HTTP/1.1\r\nConnection: close\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"explicit close" (not (is_keep_alive buf req)));

  (* Explicit Connection: keep-alive *)
  with_string_buffer "GET / HTTP/1.0\r\nConnection: keep-alive\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    assert_true ~msg:"explicit keep-alive" (is_keep_alive buf req));

  pass "keep_alive"

(* ============================================================
   Serializer Tests
   ============================================================ *)

let test_write_request () =
  let dst = Bytes.create 256 in
  let len = write_request dst ~pos:0
    ~method_:(str "GET")
    ~target:(str "/index.html")
    ~version:HTTP_1_1
    ~headers:[(str "Host", str "example.com")] in
  let result = Bytes.sub_string dst 0 len in
  assert_equal ~msg:"request line"
    "GET /index.html HTTP/1.1\r\nHost: example.com\r\n\r\n"
    result;
  pass "write_request"

let test_write_response () =
  let dst = Bytes.create 256 in
  let len = write_response dst ~pos:0
    ~version:HTTP_1_1
    ~status:200
    ~reason:(str "OK")
    ~headers:[(str "Content-Length", str "5")] in
  let result = Bytes.sub_string dst 0 len in
  assert_equal ~msg:"response line"
    "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
    result;
  pass "write_response"

let test_write_with_spans () =
  (* Parse a request then serialize using spans - zero-copy proxy style *)
  with_string_buffer "GET /original HTTP/1.1\r\nHost: origin.com\r\n\r\n" (fun buf ->
    let req = parse_request buf in

    let dst = Bytes.create 256 in
    (* Create output using spans from parsed request *)
    let target_out = out_span buf req.target in
    (* Iterate over headers to get name and value spans *)
    let host_header = ref None in
    Headers.iter (fun _name name_span value ->
      if !host_header = None then
        host_header := Some (out_span buf name_span, out_span buf value)
    ) req.headers;
    let host_name_out, host_value_out = match !host_header with
      | Some (n, v) -> (n, v)
      | None -> assert false
    in

    let len = write_request dst ~pos:0
      ~method_:(str "GET")  (* Method as string *)
      ~target:target_out    (* Target as span - zero copy *)
      ~version:req.version
      ~headers:[(host_name_out, host_value_out)] in

    let result = Bytes.sub_string dst 0 len in
    assert_equal ~msg:"proxied request"
      "GET /original HTTP/1.1\r\nHost: origin.com\r\n\r\n"
      result;
    pass "write_with_spans")

let test_write_chunks () =
  let dst = Bytes.create 256 in
  let pos = 0 in
  let pos = pos + write_chunk_header dst ~pos 5 in
  Bytes.blit_string "hello" 0 dst pos 5;
  let pos = pos + 5 in
  let pos = pos + write_chunk_trailer dst ~pos in
  let pos = pos + write_final_chunk dst ~pos in
  let result = Bytes.sub_string dst 0 pos in
  assert_equal ~msg:"chunked body"
    "5\r\nhello\r\n0\r\n\r\n"
    result;
  pass "write_chunks"

(* ============================================================
   Body Reading Tests
   ============================================================ *)

let test_body_fixed () =
  with_string_buffer
    "POST /api HTTP/1.1\r\nContent-Length: 13\r\n\r\nHello, World!" (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    (match reader with
     | Fixed_body _ -> ()
     | _ -> assert false);
    let body = body_read_string buf reader in
    assert_equal ~msg:"fixed body content" "Hello, World!" body;
    assert_true ~msg:"body is done" (body_is_done reader);
    pass "body_fixed")

let test_body_fixed_span () =
  with_string_buffer
    "POST /api HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello" (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    (match body_read_span buf reader with
     | Some sp ->
       assert_equal ~msg:"span body" "hello" (span_to_string buf sp)
     | None -> assert false);
    pass "body_fixed_span")

let test_body_chunked () =
  with_string_buffer
    ("POST /api HTTP/1.1\r\n" ^
     "Transfer-Encoding: chunked\r\n" ^
     "\r\n" ^
     "5\r\n" ^
     "hello\r\n" ^
     "6\r\n" ^
     " world\r\n" ^
     "0\r\n" ^
     "\r\n") (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    (match reader with
     | Chunked_body _ -> ()
     | _ -> assert false);
    let body = body_read_string buf reader in
    assert_equal ~msg:"chunked body content" "hello world" body;
    assert_true ~msg:"body is done" (body_is_done reader);
    pass "body_chunked")

let test_body_chunked_with_trailers () =
  with_string_buffer
    ("POST /api HTTP/1.1\r\n" ^
     "Transfer-Encoding: chunked\r\n" ^
     "\r\n" ^
     "5\r\nhello\r\n" ^
     "0\r\n" ^
     "X-Checksum: abc123\r\n" ^
     "\r\n") (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    let body = body_read_string buf reader in
    assert_equal ~msg:"chunked body" "hello" body;
    (match body_trailers reader with
     | Some trailers ->
       assert_equal_int ~msg:"trailer count" 1 (Headers.count trailers);
       (* Look up the trailer by string name *)
       (match Headers.find_by_string buf.zb trailers "X-Checksum" with
        | Some sp -> assert_equal ~msg:"trailer value" "abc123" (span_to_string buf sp)
        | None -> assert false)
     | None -> assert false);
    pass "body_chunked_with_trailers")

let test_body_chunked_extensions () =
  (* Chunk extensions should be ignored *)
  with_string_buffer
    ("POST /api HTTP/1.1\r\n" ^
     "Transfer-Encoding: chunked\r\n" ^
     "\r\n" ^
     "5;name=value\r\n" ^
     "hello\r\n" ^
     "0\r\n" ^
     "\r\n") (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    let body = body_read_string buf reader in
    assert_equal ~msg:"chunked body with extensions" "hello" body;
    pass "body_chunked_extensions")

let test_body_empty () =
  with_string_buffer "GET / HTTP/1.1\r\n\r\n" (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    (match reader with
     | Empty_body -> ()
     | _ -> assert false);
    let body = body_read_string buf reader in
    assert_equal ~msg:"empty body" "" body;
    pass "body_empty")

let test_body_incremental_read () =
  with_string_buffer
    "POST /api HTTP/1.1\r\nContent-Length: 10\r\n\r\n0123456789" (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    let dst = Bytes.create 4 in
    (* Read in chunks *)
    let n1 = body_read buf reader dst ~off:0 ~len:4 in
    assert_equal_int ~msg:"first read" 4 n1;
    assert_equal ~msg:"first chunk" "0123" (Bytes.sub_string dst 0 4);
    let n2 = body_read buf reader dst ~off:0 ~len:4 in
    assert_equal_int ~msg:"second read" 4 n2;
    assert_equal ~msg:"second chunk" "4567" (Bytes.sub_string dst 0 4);
    let n3 = body_read buf reader dst ~off:0 ~len:4 in
    assert_equal_int ~msg:"third read" 2 n3;
    assert_equal ~msg:"third chunk" "89" (Bytes.sub_string dst 0 2);
    let n4 = body_read buf reader dst ~off:0 ~len:4 in
    assert_equal_int ~msg:"eof read" 0 n4;
    pass "body_incremental_read")

let test_body_response_no_body () =
  (* 204 No Content has no body *)
  with_string_buffer "HTTP/1.1 204 No Content\r\n\r\n" (fun buf ->
    let resp = parse_response buf in
    let reader = body_reader_of_response buf resp in
    (match reader with
     | Empty_body -> ()
     | _ -> assert false);
    pass "body_response_no_body")

let test_body_drain () =
  with_string_buffer
    ("POST /api HTTP/1.1\r\nContent-Length: 1000\r\n\r\n" ^
     String.make 1000 'x') (fun buf ->
    let req = parse_request buf in
    let reader = body_reader_of_request buf req in
    body_drain buf reader;
    assert_true ~msg:"body drained" (body_is_done reader);
    pass "body_drain")

(* ============================================================
   Error Handling Tests
   ============================================================ *)

let test_parse_error_invalid_method () =
  with_string_buffer " GET / HTTP/1.1\r\n\r\n" (fun buf ->  (* Leading space *)
    match try_parse_request buf with
    | Error Partial -> pass "parse_error_invalid_method"
    | Error _ -> pass "parse_error_invalid_method"  (* Any error is acceptable *)
    | Ok _ -> assert false)

let test_parse_error_invalid_version () =
  with_string_buffer "GET / HTTP/2.0\r\n\r\n" (fun buf ->
    match try_parse_request buf with
    | Error Invalid_version -> pass "parse_error_invalid_version"
    | Error _ -> pass "parse_error_invalid_version"
    | Ok _ -> assert false)

let test_parse_error_partial () =
  with_string_buffer "GET /pa" (fun buf ->  (* Incomplete *)
    match try_parse_request buf with
    | Error Partial -> pass "parse_error_partial"
    | Error _ -> pass "parse_error_partial"
    | Ok _ -> assert false)

(* ============================================================
   Multi-fragment Tests (new for zbuf)
   ============================================================ *)

let test_span_across_fragments () =
  (* Parse a request where a single span (like the target) crosses fragment boundaries *)
  with_chunked_buffer [
    "GET /very-long";  (* target starts here *)
    "-path-that-";     (* continues *)
    "spans-frags HTTP/1.1\r\n\r\n"  (* ends here *)
  ] (fun buf ->
    let req = parse_request buf in
    assert_equal ~msg:"target across fragments"
      "/very-long-path-that-spans-frags"
      (span_to_string buf req.target);
    pass "span_across_fragments")

let test_header_value_across_fragments () =
  with_chunked_buffer [
    "GET / HTTP/1.1\r\nX-Long-Header: this-is-a-very";
    "-long-header-value";
    "-that-spans\r\n\r\n"
  ] (fun buf ->
    let req = parse_request buf in
    assert_equal ~msg:"header value across fragments"
      "this-is-a-very-long-header-value-that-spans"
      (match get_header buf req "X-Long-Header" with Some v -> v | None -> "");
    pass "header_value_across_fragments")

let test_realistic_browser_request_fragmented () =
  (* Simulate a realistic browser request split into small fragments of varying sizes *)
  let full_request =
    "GET /dune-project HTTP/1.1\r\n" ^
    "Host: localhost:8080\r\n" ^
    "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15\r\n" ^
    "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n" ^
    "Accept-Language: en-US,en;q=0.9\r\n" ^
    "Accept-Encoding: gzip, deflate\r\n" ^
    "Connection: keep-alive\r\n" ^
    "Upgrade-Insecure-Requests: 1\r\n" ^
    "\r\n"
  in
  (* Split into fragments of varying sizes: 17, 23, 31, 19, 29, 13, 41, 37, ... *)
  let fragment_sizes = [17; 23; 31; 19; 29; 13; 41; 37; 11; 47; 7; 53; 3; 61; 100; 100; 100] in
  let rec split_into_fragments s sizes acc =
    if String.length s = 0 then List.rev acc
    else match sizes with
      | [] -> List.rev (s :: acc)  (* remainder *)
      | size :: rest ->
        let len = min size (String.length s) in
        let chunk = String.sub s 0 len in
        let remaining = String.sub s len (String.length s - len) in
        split_into_fragments remaining rest (chunk :: acc)
  in
  let fragments = split_into_fragments full_request fragment_sizes [] in
  Printf.printf "    (split into %d fragments)\n%!" (List.length fragments);

  with_chunked_buffer fragments (fun buf ->
    let req = parse_request buf in

    (* Verify parsing is correct *)
    assert_true ~msg:"method is GET" (req.meth = GET);
    assert_equal ~msg:"target" "/dune-project" (span_to_string buf req.target);
    assert_true ~msg:"version is 1.1" (req.version = HTTP_1_1);
    assert_equal_int ~msg:"header count" 7 (Headers.count req.headers);

    (* Verify each header *)
    assert_equal ~msg:"Host header" "localhost:8080"
      (match get_header buf req "Host" with Some v -> v | None -> "");
    assert_true ~msg:"User-Agent starts with Mozilla"
      (match get_header buf req "User-Agent" with
       | Some v -> String.length v > 10 && String.sub v 0 10 = "Mozilla/5."
       | None -> false);
    assert_equal ~msg:"Accept-Language" "en-US,en;q=0.9"
      (match get_header buf req "Accept-Language" with Some v -> v | None -> "");
    assert_equal ~msg:"Accept-Encoding" "gzip, deflate"
      (match get_header buf req "Accept-Encoding" with Some v -> v | None -> "");
    assert_equal ~msg:"Connection" "keep-alive"
      (match get_header buf req "Connection" with Some v -> v | None -> "");

    (* Verify header name variants are correctly identified via lookup *)
    assert_true ~msg:"Host header found via H_host"
      (Headers.find_known req.headers H_host <> None);
    assert_true ~msg:"User-Agent header found via H_user_agent"
      (Headers.find_known req.headers H_user_agent <> None);
    assert_true ~msg:"Connection header found via H_connection"
      (Headers.find_known req.headers H_connection <> None);

    pass "realistic_browser_request_fragmented")

let test_tiny_fragments () =
  (* Extreme case: every byte is a separate fragment *)
  let request = "GET / HTTP/1.1\r\nHost: x\r\n\r\n" in
  let fragments = List.init (String.length request) (fun i ->
    String.make 1 request.[i]
  ) in
  Printf.printf "    (split into %d single-byte fragments)\n%!" (List.length fragments);

  with_chunked_buffer fragments (fun buf ->
    let req = parse_request buf in

    assert_true ~msg:"method is GET" (req.meth = GET);
    assert_equal ~msg:"target" "/" (span_to_string buf req.target);
    assert_equal_int ~msg:"header count" 1 (Headers.count req.headers);
    assert_equal ~msg:"Host value" "x"
      (match get_header buf req "Host" with Some v -> v | None -> "");

    pass "tiny_fragments")

(* ============================================================
   Run All Tests
   ============================================================ *)

let () =
  Printf.printf "\n=== httpl Tests ===\n\n";

  Printf.printf "--- Buffer Tests ---\n";
  test_buffer_simple ();
  test_buffer_refill ();
  test_buffer_eof ();

  Printf.printf "\n--- Span Tests ---\n";
  test_span_to_string ();
  test_span_equal ();
  test_span_equal_caseless ();

  Printf.printf "\n--- Request Parsing Tests ---\n";
  test_parse_simple_request ();
  test_parse_request_with_body_headers ();
  test_parse_all_methods ();
  test_parse_other_method ();
  test_parse_http10 ();
  test_parse_chunked_in_chunks ();
  test_parse_multiple_headers ();
  test_parse_ows_trimming ();

  Printf.printf "\n--- Response Parsing Tests ---\n";
  test_parse_simple_response ();
  test_parse_response_various_status ();
  test_parse_chunked_response ();

  Printf.printf "\n--- Materialization Tests ---\n";
  test_method_to_string ();
  test_version_to_string ();
  test_keep_alive ();

  Printf.printf "\n--- Serializer Tests ---\n";
  test_write_request ();
  test_write_response ();
  test_write_with_spans ();
  test_write_chunks ();

  Printf.printf "\n--- Body Reading Tests ---\n";
  test_body_fixed ();
  test_body_fixed_span ();
  test_body_chunked ();
  test_body_chunked_with_trailers ();
  test_body_chunked_extensions ();
  test_body_empty ();
  test_body_incremental_read ();
  test_body_response_no_body ();
  test_body_drain ();

  Printf.printf "\n--- Error Handling Tests ---\n";
  test_parse_error_invalid_method ();
  test_parse_error_invalid_version ();
  test_parse_error_partial ();

  Printf.printf "\n--- Multi-fragment Tests ---\n";
  test_span_across_fragments ();
  test_header_value_across_fragments ();
  test_realistic_browser_request_fragmented ();
  test_tiny_fragments ();

  Printf.printf "\n=== All tests passed! ===\n"
