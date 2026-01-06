(* test_httpz.ml - Tests for the Httpz parser *)
open Base

let copy_to_buffer buf s =
  let len = String.length s in
  for i = 0 to len - 1 do
    Bigarray.Array1.set buf i (String.get s i)
  done;
  len
;;

(* Helper to parse a request and assert success.
   Returns the unboxed triple directly - caller must destructure immediately *)
let parse_ok buf request = exclave_
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len in
  if Poly.( <> ) status Httpz.Ok
  then failwith (Printf.sprintf "Expected Ok, got %s" (Httpz.status_to_string status));
  #(len, req, headers)
;;

let test_simple_get () =
  let buf = Httpz.create_buffer () in
  let request =
    "GET /index.html HTTP/1.1\r\nHost: example.com\r\nContent-Length: 0\r\n\r\n"
  in
  let #(_len, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#meth Httpz.GET);
  assert (Httpz.span_equal buf req.#target "/index.html");
  assert (Poly.( = ) req.#version Httpz.HTTP_1_1);
  (* Content-Length is now cached in request struct and excluded from headers *)
  assert (Int64.( = ) req.#content_length 0L);
  assert (List.length headers = 1);
  (match headers with
   | [ hdr0 ] ->
     assert (Poly.( = ) hdr0.name Httpz.H_host);
     assert (Httpz.span_equal buf hdr0.value "example.com")
   | _ -> assert false);
  Stdio.printf "test_simple_get: PASSED\n"
;;

let test_post_with_body () =
  let buf = Httpz.create_buffer () in
  let request =
    "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     Content-Length: 13\r\n\
     \r\n\
     {\"key\":\"val\"}"
  in
  let #(len, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#meth Httpz.POST);
  assert (Httpz.span_equal buf req.#target "/api/data");
  assert (Poly.( = ) req.#version Httpz.HTTP_1_1);
  (* Content-Length excluded from headers, only Host and Content-Type remain *)
  assert (List.length headers = 2);
  assert (req.#body_off = len - 13);
  (* Content-Length is now in the request struct *)
  assert (Int64.( = ) req.#content_length 13L);
  Stdio.printf "test_post_with_body: PASSED\n"
;;

let test_unknown_method () =
  let buf = Httpz.create_buffer () in
  let request = "PURGE /cache HTTP/1.1\r\nHost: cdn.example.com\r\n\r\n" in
  let #(_len, req, _headers) = parse_ok buf request in
  (match req.#meth with
   | Httpz.Other sp -> assert (Httpz.span_equal buf sp "PURGE")
   | _ -> assert false);
  Stdio.printf "test_unknown_method: PASSED\n"
;;

let test_unknown_header () =
  let buf = Httpz.create_buffer () in
  let request =
    "GET / HTTP/1.1\r\nHost: example.com\r\nX-Custom-Header: custom-value\r\n\r\n"
  in
  let #(_len, _req, headers) = parse_ok buf request in
  assert (List.length headers = 2);
  (* Headers are returned in reverse order: X-Custom-Header is first, Host is second *)
  (match headers with
   | [ hdr0; _ ] ->
     (match hdr0.name with
      | Httpz.H_other ->
        assert (Httpz.span_equal_caseless buf hdr0.name_span "x-custom-header")
      | _ -> assert false);
     assert (Httpz.span_equal buf hdr0.value "custom-value")
   | _ -> assert false);
  Stdio.printf "test_unknown_header: PASSED\n"
;;

let test_partial () =
  let buf = Httpz.create_buffer () in
  let request = "GET /index.html HTTP/1.1\r\nHost: exam" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len in
  assert (Poly.( = ) status Httpz.Partial);
  Stdio.printf "test_partial: PASSED\n"
;;

let test_http10 () =
  let buf = Httpz.create_buffer () in
  let request = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len, req, headers) = parse_ok buf request in
  assert (Poly.( = ) req.#version Httpz.HTTP_1_0);
  assert (List.length headers = 0);
  Stdio.printf "test_http10: PASSED\n"
;;

let test_keep_alive () =
  let buf = Httpz.create_buffer () in
  (* HTTP/1.1 default is keep-alive *)
  let request1 = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let #(_len1, req1, _headers1) = parse_ok buf request1 in
  (* Use cached keep_alive from request struct *)
  assert req1.#keep_alive;
  (* HTTP/1.0 default is close *)
  let request2 = "GET / HTTP/1.0\r\n\r\n" in
  let #(_len2, req2, _headers2) = parse_ok buf request2 in
  assert (not req2.#keep_alive);
  Stdio.printf "test_keep_alive: PASSED\n"
;;

let test_chunked () =
  let buf = Httpz.create_buffer () in
  let request =
    "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r\n\r\n"
  in
  let #(_len, req, headers) = parse_ok buf request in
  (* Transfer-Encoding is now cached in request struct and excluded from headers *)
  assert req.#is_chunked;
  (* Only Host header remains *)
  assert (List.length headers = 1);
  Stdio.printf "test_chunked: PASSED\n"
;;

let test_find_header () =
  let buf = Httpz.create_buffer () in
  let request = "GET / HTTP/1.1\r\nHost: example.com\r\nAccept: text/html\r\n\r\n" in
  let #(_len, _req, headers) = parse_ok buf request in
  (match Httpz.find_header headers Httpz.H_host with
   | Some hdr -> assert (Httpz.span_equal buf hdr.value "example.com")
   | None -> assert false);
  (match Httpz.find_header headers Httpz.H_content_length with
   | Some _ -> assert false
   | None -> ());
  Stdio.printf "test_find_header: PASSED\n"
;;

let () =
  test_simple_get ();
  test_post_with_body ();
  test_unknown_method ();
  test_unknown_header ();
  test_partial ();
  test_http10 ();
  test_keep_alive ();
  test_chunked ();
  test_find_header ();
  Stdio.printf "\nAll tests passed!\n"
;;
