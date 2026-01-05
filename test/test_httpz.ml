(* test_httpz.ml - Tests for the Httpz parser *)
open Base

let copy_to_buffer buf s =
  let len = String.length s in
  for i = 0 to len - 1 do
    Bigarray.Array1.set buf i (String.get s i)
  done;
  len

let test_simple_get () =
  let buf = Httpz.create_buffer () in
  let request = "GET /index.html HTTP/1.1\r\nHost: example.com\r\nContent-Length: 0\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  assert (Poly.(=) req.#meth Httpz.GET);
  assert (Httpz.span_equal buf req.#target "/index.html");
  assert (Poly.(=) req.#version Httpz.HTTP_1_1);
  assert (List.length headers = 2);
  (match headers with
   | [hdr0; hdr1] ->
     assert (Poly.(=) hdr0.name Httpz.H_host);
     assert (Httpz.span_equal buf hdr0.value "example.com");
     assert (Poly.(=) hdr1.name Httpz.H_content_length);
     assert (Httpz.span_equal buf hdr1.value "0")
   | _ -> assert false);
  Stdio.printf "test_simple_get: PASSED\n"

let test_post_with_body () =
  let buf = Httpz.create_buffer () in
  let request = "POST /api/data HTTP/1.1\r\nHost: api.example.com\r\nContent-Type: application/json\r\nContent-Length: 13\r\n\r\n{\"key\":\"val\"}" in
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  assert (Poly.(=) req.#meth Httpz.POST);
  assert (Httpz.span_equal buf req.#target "/api/data");
  assert (Poly.(=) req.#version Httpz.HTTP_1_1);
  assert (List.length headers = 3);
  assert (req.#body_off = len - 13);
  let cl = Httpz.content_length buf headers in
  assert (Int64.(=) cl 13L);
  Stdio.printf "test_post_with_body: PASSED\n"

let test_unknown_method () =
  let buf = Httpz.create_buffer () in
  let request = "PURGE /cache HTTP/1.1\r\nHost: cdn.example.com\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, req, _headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  (match req.#meth with
   | Httpz.Other sp -> assert (Httpz.span_equal buf sp "PURGE")
   | _ -> assert false);
  Stdio.printf "test_unknown_method: PASSED\n"

let test_unknown_header () =
  let buf = Httpz.create_buffer () in
  let request = "GET / HTTP/1.1\r\nHost: example.com\r\nX-Custom-Header: custom-value\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  assert (List.length headers = 2);
  (match headers with
   | [_; hdr1] ->
     (match hdr1.name with
      | Httpz.H_other sp -> assert (Httpz.span_equal_caseless buf sp "x-custom-header")
      | _ -> assert false);
     assert (Httpz.span_equal buf hdr1.value "custom-value")
   | _ -> assert false);
  Stdio.printf "test_unknown_header: PASSED\n"

let test_partial () =
  let buf = Httpz.create_buffer () in
  let request = "GET /index.html HTTP/1.1\r\nHost: exam" in
  let len = copy_to_buffer buf request in
  let #(status, _req, _headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Partial);
  Stdio.printf "test_partial: PASSED\n"

let test_http10 () =
  let buf = Httpz.create_buffer () in
  let request = "GET / HTTP/1.0\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  assert (Poly.(=) req.#version Httpz.HTTP_1_0);
  assert (List.length headers = 0);
  Stdio.printf "test_http10: PASSED\n"

let test_keep_alive () =
  let buf = Httpz.create_buffer () in
  (* HTTP/1.1 default is keep-alive *)
  let request1 = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let len1 = copy_to_buffer buf request1 in
  let #(status1, req1, headers1) = Httpz.parse buf ~len:len1 in
  assert (Poly.(=) status1 Httpz.Ok);
  assert (Httpz.is_keep_alive buf headers1 req1.#version);

  (* HTTP/1.0 default is close *)
  let request2 = "GET / HTTP/1.0\r\n\r\n" in
  let len2 = copy_to_buffer buf request2 in
  let #(status2, req2, headers2) = Httpz.parse buf ~len:len2 in
  assert (Poly.(=) status2 Httpz.Ok);
  assert (not (Httpz.is_keep_alive buf headers2 req2.#version));

  Stdio.printf "test_keep_alive: PASSED\n"

let test_chunked () =
  let buf = Httpz.create_buffer () in
  let request = "POST /upload HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  assert (Httpz.is_chunked buf headers);
  Stdio.printf "test_chunked: PASSED\n"

let test_find_header () =
  let buf = Httpz.create_buffer () in
  let request = "GET / HTTP/1.1\r\nHost: example.com\r\nAccept: text/html\r\n\r\n" in
  let len = copy_to_buffer buf request in
  let #(status, _req, headers) = Httpz.parse buf ~len in
  assert (Poly.(=) status Httpz.Ok);
  (match Httpz.find_header headers Httpz.H_host with
   | Some hdr -> assert (Httpz.span_equal buf hdr.value "example.com")
   | None -> assert false);
  (match Httpz.find_header headers Httpz.H_content_length with
   | Some _ -> assert false
   | None -> ());
  Stdio.printf "test_find_header: PASSED\n"

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
