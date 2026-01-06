(* bench_httpl.ml - core_bench benchmarks for httpl HTTP parser
   Compares OxCaml version (httpl/zbuf) vs standard OCaml (httplo/zbufo) *)

open Core
open Core_bench

(* Sample HTTP requests of varying complexity *)

let minimal_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

let simple_request = {|GET /path/to/resource HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0
Accept: text/html
Connection: keep-alive

|}
|> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"

let make_request_with_headers n_headers =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "GET /api/v1/users/12345 HTTP/1.1\r\n";
  Buffer.add_string buf "Host: api.example.com\r\n";
  for i = 1 to n_headers do
    Buffer.add_string buf (Printf.sprintf "X-Custom-Header-%d: value-%d\r\n" i i)
  done;
  Buffer.add_string buf "\r\n";
  Buffer.contents buf

let request_5_headers = make_request_with_headers 5
let request_10_headers = make_request_with_headers 10
let request_20_headers = make_request_with_headers 20
let request_50_headers = make_request_with_headers 50

let make_request_with_body body_size =
  let body = String.make body_size 'x' in
  Printf.sprintf "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: %d\r\nContent-Type: application/octet-stream\r\n\r\n%s"
    body_size body

let request_body_100 = make_request_with_body 100
let request_body_1k = make_request_with_body 1024
let request_body_10k = make_request_with_body 10240
let request_body_100k = make_request_with_body 102400

let chunked_request =
  {|POST /stream HTTP/1.1
Host: example.com
Transfer-Encoding: chunked

a
0123456789
14
twenty bytes here...
0

|}
|> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"

let browser_request =
  {|GET /index.html HTTP/1.1
Host: www.example.com
User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8
Accept-Language: en-US,en;q=0.9
Accept-Encoding: gzip, deflate, br
Connection: keep-alive
Cache-Control: max-age=0
Cookie: session=abc123; tracking=xyz789; preferences=dark-mode
If-None-Match: "abc123"
If-Modified-Since: Mon, 01 Jan 2024 00:00:00 GMT

|}
|> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"

(* Sample HTTP responses *)

let simple_response = {|HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 13

Hello, World!|}
|> String.substr_replace_all ~pattern:"\n" ~with_:"\r\n"

let make_response_with_body body_size =
  let body = String.make body_size 'y' in
  Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: %d\r\n\r\n%s"
    body_size body

let response_body_1k = make_response_with_body 1024
let response_body_10k = make_response_with_body 10240

(* Benchmark helpers *)

let parse_request_once data =
  let buf = Httpl.create () in
  let bytes = Bytes.of_string data in
  Httpl.push buf bytes ~off:0 ~len:(Bytes.length bytes);
  Httpl.with_refill buf (fun _ -> false) (fun () ->
    match Httpl.try_parse_request buf with
    | Ok req -> req
    | Error _ -> failwith "parse failed"
  )

let parse_request_and_body data =
  let buf = Httpl.create () in
  let bytes = Bytes.of_string data in
  Httpl.push buf bytes ~off:0 ~len:(Bytes.length bytes);
  Httpl.with_refill buf (fun _ -> false) (fun () ->
    match Httpl.try_parse_request buf with
    | Ok req ->
      let reader = Httpl.body_reader_of_request buf req in
      let body = Httpl.body_read_string buf reader in
      (req, body)
    | Error _ -> failwith "parse failed"
  )

let parse_response_once data =
  let buf = Httpl.create () in
  let bytes = Bytes.of_string data in
  Httpl.push buf bytes ~off:0 ~len:(Bytes.length bytes);
  Httpl.with_refill buf (fun _ -> false) (fun () ->
    match Httpl.try_parse_response buf with
    | Ok resp -> resp
    | Error _ -> failwith "parse failed"
  )

let parse_fragmented data fragment_size =
  let buf = Httpl.create () in
  let bytes = Bytes.of_string data in
  let len = Bytes.length bytes in
  let offset = ref 0 in
  let refill () =
    if !offset >= len then false
    else begin
      let chunk_size = min fragment_size (len - !offset) in
      Httpl.push buf bytes ~off:!offset ~len:chunk_size;
      offset := !offset + chunk_size;
      true
    end
  in
  (* Push initial fragment *)
  ignore (refill ());
  Httpl.with_refill buf (fun _ -> refill ()) (fun () ->
    match Httpl.try_parse_request buf with
    | Ok req -> req
    | Error _ -> failwith "parse failed"
  )

let serialize_request () =
  let buf = Bytes.create 4096 in
  Httpl.write_request buf ~pos:0
    ~method_:(Httpl.str "GET")
    ~target:(Httpl.str "/api/v1/users")
    ~version:Httpl.HTTP_1_1
    ~headers:[
      (Httpl.str "Host", Httpl.str "api.example.com");
      (Httpl.str "User-Agent", Httpl.str "Benchmark/1.0");
      (Httpl.str "Accept", Httpl.str "application/json");
      (Httpl.str "Connection", Httpl.str "keep-alive");
    ]

let serialize_response () =
  let buf = Bytes.create 4096 in
  Httpl.write_response buf ~pos:0
    ~version:Httpl.HTTP_1_1
    ~status:200
    ~reason:(Httpl.str "OK")
    ~headers:[
      (Httpl.str "Content-Type", Httpl.str "application/json");
      (Httpl.str "Content-Length", Httpl.str "1024");
      (Httpl.str "Cache-Control", Httpl.str "max-age=3600");
      (Httpl.str "Server", Httpl.str "httpl/1.0");
    ]

(* Benchmarks *)

let request_parsing_benchmarks = [
  Bench.Test.create ~name:"parse_minimal_request" (fun () ->
    ignore (parse_request_once minimal_request));

  Bench.Test.create ~name:"parse_simple_request" (fun () ->
    ignore (parse_request_once simple_request));

  Bench.Test.create ~name:"parse_browser_request" (fun () ->
    ignore (parse_request_once browser_request));

  Bench.Test.create ~name:"parse_5_headers" (fun () ->
    ignore (parse_request_once request_5_headers));

  Bench.Test.create ~name:"parse_10_headers" (fun () ->
    ignore (parse_request_once request_10_headers));

  Bench.Test.create ~name:"parse_20_headers" (fun () ->
    ignore (parse_request_once request_20_headers));

  Bench.Test.create ~name:"parse_50_headers" (fun () ->
    ignore (parse_request_once request_50_headers));
]

let body_parsing_benchmarks = [
  Bench.Test.create ~name:"parse_body_100B" (fun () ->
    ignore (parse_request_and_body request_body_100));

  Bench.Test.create ~name:"parse_body_1KB" (fun () ->
    ignore (parse_request_and_body request_body_1k));

  Bench.Test.create ~name:"parse_body_10KB" (fun () ->
    ignore (parse_request_and_body request_body_10k));

  Bench.Test.create ~name:"parse_body_100KB" (fun () ->
    ignore (parse_request_and_body request_body_100k));

  Bench.Test.create ~name:"parse_chunked_body" (fun () ->
    ignore (parse_request_and_body chunked_request));
]

let fragmented_benchmarks = [
  Bench.Test.create ~name:"parse_browser_frag_64B" (fun () ->
    ignore (parse_fragmented browser_request 64));

  Bench.Test.create ~name:"parse_browser_frag_128B" (fun () ->
    ignore (parse_fragmented browser_request 128));

  Bench.Test.create ~name:"parse_browser_frag_256B" (fun () ->
    ignore (parse_fragmented browser_request 256));

  Bench.Test.create ~name:"parse_browser_frag_1byte" (fun () ->
    ignore (parse_fragmented browser_request 1));
]

let response_benchmarks = [
  Bench.Test.create ~name:"parse_simple_response" (fun () ->
    ignore (parse_response_once simple_response));

  Bench.Test.create ~name:"parse_response_1KB" (fun () ->
    ignore (parse_response_once response_body_1k));

  Bench.Test.create ~name:"parse_response_10KB" (fun () ->
    ignore (parse_response_once response_body_10k));
]

let serialization_benchmarks = [
  Bench.Test.create ~name:"serialize_request" (fun () ->
    ignore (serialize_request ()));

  Bench.Test.create ~name:"serialize_response" (fun () ->
    ignore (serialize_response ()));
]

(* Zbuf span operation benchmarks *)

let zbuf_benchmarks =
  let make_zbuf data =
    let zb = Zbuf.create () in
    Zbuf.push zb (Bytes.of_string data) ~off:0 ~len:(String.length data);
    zb
  in
  let sample_data = "Content-Type: application/json; charset=utf-8" in
  let zb = make_zbuf sample_data in
  let sp = Zbuf.span ~start:0 ~len:(String.length sample_data) in
  [
    Bench.Test.create ~name:"span_to_string_45B" (fun () ->
      ignore (Zbuf.span_to_string zb sp));

    Bench.Test.create ~name:"span_equal_45B_match" (fun () ->
      ignore (Zbuf.span_equal zb sp sample_data));

    Bench.Test.create ~name:"span_equal_45B_mismatch" (fun () ->
      ignore (Zbuf.span_equal zb sp "Content-Type: text/html"));

    Bench.Test.create ~name:"span_equal_caseless_45B" (fun () ->
      ignore (Zbuf.span_equal_caseless zb sp "content-type: application/json; charset=utf-8"));

    Bench.Test.create ~name:"span_blit_45B" (fun () ->
      let dst = Bytes.create 64 in
      Zbuf.span_blit zb sp dst ~dst_off:0);
  ]

(* Header lookup benchmarks *)

let header_lookup_benchmarks =
  let req = parse_request_once browser_request in
  let buf = Httpl.create () in
  Httpl.push buf (Bytes.of_string browser_request) ~off:0 ~len:(String.length browser_request);
  [
    Bench.Test.create ~name:"is_keep_alive" (fun () ->
      ignore (Httpl.is_keep_alive buf req));

    Bench.Test.create ~name:"is_chunked" (fun () ->
      ignore (Httpl.is_chunked buf req));

    Bench.Test.create ~name:"get_header_host" (fun () ->
      ignore (Httpl.get_header buf req "Host"));

    Bench.Test.create ~name:"get_header_user_agent" (fun () ->
      ignore (Httpl.get_header buf req "User-Agent"));

    Bench.Test.create ~name:"get_header_by_name_host" (fun () ->
      ignore (Httpl.get_header_by_name buf req Httpl.H_host));

    Bench.Test.create ~name:"get_header_by_name_content_type" (fun () ->
      ignore (Httpl.get_header_by_name buf req Httpl.H_content_type));
  ]

(* Throughput benchmarks - measure bytes/second *)

let throughput_benchmarks =
  let iterations = 1000 in
  [
    Bench.Test.create ~name:"throughput_simple_request" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once simple_request)
      done);

    Bench.Test.create ~name:"throughput_browser_request" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once browser_request)
      done);

    Bench.Test.create ~name:"throughput_50_headers" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once request_50_headers)
      done);
  ]

(* Standard OCaml version (httplo/zbufo) benchmarks for comparison *)

let parse_request_once_std data =
  let buf = Httplo.create () in
  let bytes = Bytes.of_string data in
  Httplo.push buf bytes ~off:0 ~len:(Bytes.length bytes);
  Httplo.with_refill buf (fun _ -> false) (fun () ->
    match Httplo.try_parse_request buf with
    | Ok req -> req
    | Error _ -> failwith "parse failed"
  )

let parse_response_once_std data =
  let buf = Httplo.create () in
  let bytes = Bytes.of_string data in
  Httplo.push buf bytes ~off:0 ~len:(Bytes.length bytes);
  Httplo.with_refill buf (fun _ -> false) (fun () ->
    match Httplo.try_parse_response buf with
    | Ok resp -> resp
    | Error _ -> failwith "parse failed"
  )

let serialize_request_std () =
  let buf = Bytes.create 4096 in
  Httplo.write_request buf ~pos:0
    ~method_:(Httplo.str "GET")
    ~target:(Httplo.str "/api/v1/users")
    ~version:Httplo.HTTP_1_1
    ~headers:[
      (Httplo.str "Host", Httplo.str "api.example.com");
      (Httplo.str "User-Agent", Httplo.str "Benchmark/1.0");
      (Httplo.str "Accept", Httplo.str "application/json");
      (Httplo.str "Connection", Httplo.str "keep-alive");
    ]

let serialize_response_std () =
  let buf = Bytes.create 4096 in
  Httplo.write_response buf ~pos:0
    ~version:Httplo.HTTP_1_1
    ~status:200
    ~reason:(Httplo.str "OK")
    ~headers:[
      (Httplo.str "Content-Type", Httplo.str "application/json");
      (Httplo.str "Content-Length", Httplo.str "1024");
      (Httplo.str "Cache-Control", Httplo.str "max-age=3600");
      (Httplo.str "Server", Httplo.str "httpl/1.0");
    ]

(* Head-to-head comparison benchmarks *)

let comparison_parse_benchmarks = [
  Bench.Test.create ~name:"oxcaml_minimal" (fun () ->
    ignore (parse_request_once minimal_request));
  Bench.Test.create ~name:"stdlib_minimal" (fun () ->
    ignore (parse_request_once_std minimal_request));

  Bench.Test.create ~name:"oxcaml_browser" (fun () ->
    ignore (parse_request_once browser_request));
  Bench.Test.create ~name:"stdlib_browser" (fun () ->
    ignore (parse_request_once_std browser_request));

  Bench.Test.create ~name:"oxcaml_50headers" (fun () ->
    ignore (parse_request_once request_50_headers));
  Bench.Test.create ~name:"stdlib_50headers" (fun () ->
    ignore (parse_request_once_std request_50_headers));
]

let comparison_response_benchmarks = [
  Bench.Test.create ~name:"oxcaml_response_simple" (fun () ->
    ignore (parse_response_once simple_response));
  Bench.Test.create ~name:"stdlib_response_simple" (fun () ->
    ignore (parse_response_once_std simple_response));

  Bench.Test.create ~name:"oxcaml_response_1k" (fun () ->
    ignore (parse_response_once response_body_1k));
  Bench.Test.create ~name:"stdlib_response_1k" (fun () ->
    ignore (parse_response_once_std response_body_1k));
]

let comparison_serialize_benchmarks = [
  Bench.Test.create ~name:"oxcaml_serialize_req" (fun () ->
    ignore (serialize_request ()));
  Bench.Test.create ~name:"stdlib_serialize_req" (fun () ->
    ignore (serialize_request_std ()));

  Bench.Test.create ~name:"oxcaml_serialize_resp" (fun () ->
    ignore (serialize_response ()));
  Bench.Test.create ~name:"stdlib_serialize_resp" (fun () ->
    ignore (serialize_response_std ()));
]

let comparison_zbuf_benchmarks =
  let make_zbuf data =
    let zb = Zbuf.create () in
    Zbuf.push zb (Bytes.of_string data) ~off:0 ~len:(String.length data);
    zb
  in
  let make_zbufo data =
    let zb = Zbufo.create () in
    Zbufo.push zb (Bytes.of_string data) ~off:0 ~len:(String.length data);
    zb
  in
  let sample_data = "Content-Type: application/json; charset=utf-8" in
  let zb = make_zbuf sample_data in
  let sp = Zbuf.span ~start:0 ~len:(String.length sample_data) in
  let zbo = make_zbufo sample_data in
  let spo = Zbufo.span ~start:0 ~len:(String.length sample_data) in
  [
    Bench.Test.create ~name:"oxcaml_span_to_string" (fun () ->
      ignore (Zbuf.span_to_string zb sp));
    Bench.Test.create ~name:"stdlib_span_to_string" (fun () ->
      ignore (Zbufo.span_to_string zbo spo));

    Bench.Test.create ~name:"oxcaml_span_equal" (fun () ->
      ignore (Zbuf.span_equal zb sp sample_data));
    Bench.Test.create ~name:"stdlib_span_equal" (fun () ->
      ignore (Zbufo.span_equal zbo spo sample_data));

    Bench.Test.create ~name:"oxcaml_span_equal_caseless" (fun () ->
      ignore (Zbuf.span_equal_caseless zb sp "content-type: application/json; charset=utf-8"));
    Bench.Test.create ~name:"stdlib_span_equal_caseless" (fun () ->
      ignore (Zbufo.span_equal_caseless zbo spo "content-type: application/json; charset=utf-8"));
  ]

let comparison_header_benchmarks =
  let req = parse_request_once browser_request in
  let buf = Httpl.create () in
  Httpl.push buf (Bytes.of_string browser_request) ~off:0 ~len:(String.length browser_request);
  let req_std = parse_request_once_std browser_request in
  let buf_std = Httplo.create () in
  Httplo.push buf_std (Bytes.of_string browser_request) ~off:0 ~len:(String.length browser_request);
  [
    Bench.Test.create ~name:"oxcaml_is_keep_alive" (fun () ->
      ignore (Httpl.is_keep_alive buf req));
    Bench.Test.create ~name:"stdlib_is_keep_alive" (fun () ->
      ignore (Httplo.is_keep_alive buf_std req_std));

    Bench.Test.create ~name:"oxcaml_get_header_host" (fun () ->
      ignore (Httpl.get_header buf req "Host"));
    Bench.Test.create ~name:"stdlib_get_header_host" (fun () ->
      ignore (Httplo.get_header buf_std req_std "Host"));
  ]

let comparison_throughput_benchmarks =
  let iterations = 1000 in
  [
    Bench.Test.create ~name:"oxcaml_1k_simple" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once simple_request)
      done);
    Bench.Test.create ~name:"stdlib_1k_simple" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once_std simple_request)
      done);

    Bench.Test.create ~name:"oxcaml_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once browser_request)
      done);
    Bench.Test.create ~name:"stdlib_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once_std browser_request)
      done);
  ]

(* ============================================================
   Httpz (zero-allocation bigarray parser) benchmarks
   ============================================================ *)

(* Helper to copy string into httpz bigarray buffer *)
let copy_to_httpz_buffer buf data =
  let len = String.length data in
  for i = 0 to len - 1 do
    Bigarray.Array1.set buf i (String.get data i)
  done;
  len

(* Parse request using httpz - note: result is stack-allocated (local) *)
let parse_request_httpz buf data =
  let len = copy_to_httpz_buffer buf data in
  let #(status, req, headers) = Httpz.parse buf ~len in
  (* Extract values to prevent them from being optimized away *)
  let _ = req.#body_off in
  let _ = headers in
  status

(* Variant to test if List.length causes allocation *)
let parse_request_httpz_with_length buf data =
  let len = copy_to_httpz_buffer buf data in
  let #(status, req, headers) = Httpz.parse buf ~len in
  let _ = req.#body_off in
  let _ = List.length headers in
  status

(* Test just the copy - no parsing *)
let just_copy_httpz buf data =
  let len = copy_to_httpz_buffer buf data in
  len

(* Test parse only - assume data already in buffer *)
let just_parse_httpz buf len =
  let #(status, req, headers) = Httpz.parse buf ~len in
  let _ = req.#body_off in
  let _ = headers in
  status

(* Httpz parsing benchmarks *)
let httpz_buf = Httpz.create_buffer ()
let minimal_len = String.length minimal_request
let _ = String.length browser_request (* silence warning *)

(* Pre-populate buffer for parse-only test *)
let () =
  for i = 0 to minimal_len - 1 do
    Bigarray.Array1.set httpz_buf i (String.get minimal_request i)
  done

let httpz_parsing_benchmarks = [
  Bench.Test.create ~name:"httpz_noop" (fun () ->
    ());

  Bench.Test.create ~name:"httpz_just_copy" (fun () ->
    ignore (just_copy_httpz httpz_buf minimal_request));

  Bench.Test.create ~name:"httpz_just_parse" (fun () ->
    ignore (just_parse_httpz httpz_buf minimal_len));

  Bench.Test.create ~name:"httpz_minimal" (fun () ->
    ignore (parse_request_httpz httpz_buf minimal_request));

  Bench.Test.create ~name:"httpz_minimal_len" (fun () ->
    ignore (parse_request_httpz_with_length httpz_buf minimal_request));

  Bench.Test.create ~name:"httpz_simple" (fun () ->
    ignore (parse_request_httpz httpz_buf simple_request));

  Bench.Test.create ~name:"httpz_browser" (fun () ->
    ignore (parse_request_httpz httpz_buf browser_request));

  Bench.Test.create ~name:"httpz_browser_len" (fun () ->
    ignore (parse_request_httpz_with_length httpz_buf browser_request));

  Bench.Test.create ~name:"httpz_5_headers" (fun () ->
    ignore (parse_request_httpz httpz_buf request_5_headers));

  Bench.Test.create ~name:"httpz_10_headers" (fun () ->
    ignore (parse_request_httpz httpz_buf request_10_headers));

  Bench.Test.create ~name:"httpz_20_headers" (fun () ->
    ignore (parse_request_httpz httpz_buf request_20_headers));

  Bench.Test.create ~name:"httpz_50_headers" (fun () ->
    ignore (parse_request_httpz httpz_buf request_50_headers));
]

(* Httpz header lookup benchmarks *)
let httpz_header_benchmarks =
  let buf = Httpz.create_buffer () in
  let len = copy_to_httpz_buffer buf browser_request in
  [
    Bench.Test.create ~name:"httpz_parse_and_find_host" (fun () ->
      let #(_status, _req, headers) = Httpz.parse buf ~len in
      ignore (Httpz.find_header headers Httpz.H_host));

    Bench.Test.create ~name:"httpz_parse_and_is_keepalive" (fun () ->
      let #(_status, req, headers) = Httpz.parse buf ~len in
      ignore (Httpz.is_keep_alive buf headers req.#version));

    Bench.Test.create ~name:"httpz_parse_and_content_length" (fun () ->
      let #(_status, _req, headers) = Httpz.parse buf ~len in
      ignore (Httpz.content_length buf headers));
  ]

(* Httpz body handling benchmarks *)
let httpz_body_benchmarks =
  let buf = Httpz.create_buffer () in
  [
    Bench.Test.create ~name:"httpz_body_100B" (fun () ->
      let len = copy_to_httpz_buffer buf request_body_100 in
      let #(_status, req, headers) = Httpz.parse buf ~len in
      let body = Httpz.body_span buf ~len ~body_off:req.#body_off headers in
      ignore body.#len);

    Bench.Test.create ~name:"httpz_body_1KB" (fun () ->
      let len = copy_to_httpz_buffer buf request_body_1k in
      let #(_status, req, headers) = Httpz.parse buf ~len in
      let body = Httpz.body_span buf ~len ~body_off:req.#body_off headers in
      ignore body.#len);

    Bench.Test.create ~name:"httpz_body_10KB" (fun () ->
      let len = copy_to_httpz_buffer buf request_body_10k in
      let #(_status, req, headers) = Httpz.parse buf ~len in
      let body = Httpz.body_span buf ~len ~body_off:req.#body_off headers in
      ignore body.#len);
  ]

(* Httpz response serialization benchmarks *)
let httpz_serialize_benchmarks =
  let response_buf = Bytes.create 4096 in
  [
    Bench.Test.create ~name:"httpz_write_status_line" (fun () ->
      ignore (Httpz.write_status_line response_buf ~off:0 Httpz.S200_OK Httpz.HTTP_1_1));

    Bench.Test.create ~name:"httpz_write_response_headers" (fun () ->
      let off = Httpz.write_status_line response_buf ~off:0 Httpz.S200_OK Httpz.HTTP_1_1 in
      let off = Httpz.write_header response_buf ~off "Content-Type" "application/json" in
      let off = Httpz.write_content_length response_buf ~off 1024 in
      let off = Httpz.write_connection response_buf ~off true in
      let off = Httpz.write_crlf response_buf ~off in
      ignore off);
  ]

(* Httpz throughput benchmarks *)
let httpz_throughput_benchmarks =
  let buf = Httpz.create_buffer () in
  let iterations = 1000 in
  [
    Bench.Test.create ~name:"httpz_1k_simple" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf simple_request)
      done);

    Bench.Test.create ~name:"httpz_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf browser_request)
      done);

    Bench.Test.create ~name:"httpz_1k_50headers" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz buf request_50_headers)
      done);
  ]

(* ============================================================
   Three-way comparison benchmarks: httpl vs httplo vs httpz
   ============================================================ *)

let three_way_minimal_benchmarks = [
  Bench.Test.create ~name:"httpl_minimal" (fun () ->
    ignore (parse_request_once minimal_request));
  Bench.Test.create ~name:"httplo_minimal" (fun () ->
    ignore (parse_request_once_std minimal_request));
  Bench.Test.create ~name:"httpz_minimal" (fun () ->
    ignore (parse_request_httpz httpz_buf minimal_request));
]

let three_way_browser_benchmarks = [
  Bench.Test.create ~name:"httpl_browser" (fun () ->
    ignore (parse_request_once browser_request));
  Bench.Test.create ~name:"httplo_browser" (fun () ->
    ignore (parse_request_once_std browser_request));
  Bench.Test.create ~name:"httpz_browser" (fun () ->
    ignore (parse_request_httpz httpz_buf browser_request));
]

let three_way_50headers_benchmarks = [
  Bench.Test.create ~name:"httpl_50headers" (fun () ->
    ignore (parse_request_once request_50_headers));
  Bench.Test.create ~name:"httplo_50headers" (fun () ->
    ignore (parse_request_once_std request_50_headers));
  Bench.Test.create ~name:"httpz_50headers" (fun () ->
    ignore (parse_request_httpz httpz_buf request_50_headers));
]

let three_way_throughput_benchmarks =
  let iterations = 1000 in
  [
    Bench.Test.create ~name:"httpl_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once browser_request)
      done);
    Bench.Test.create ~name:"httplo_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_once_std browser_request)
      done);
    Bench.Test.create ~name:"httpz_1k_browser" (fun () ->
      for _ = 1 to iterations do
        ignore (parse_request_httpz httpz_buf browser_request)
      done);
  ]

let command =
  Bench.make_command [
    Bench.Test.create_group ~name:"request_parsing" request_parsing_benchmarks;
    Bench.Test.create_group ~name:"body_parsing" body_parsing_benchmarks;
    Bench.Test.create_group ~name:"fragmented" fragmented_benchmarks;
    Bench.Test.create_group ~name:"response_parsing" response_benchmarks;
    Bench.Test.create_group ~name:"serialization" serialization_benchmarks;
    Bench.Test.create_group ~name:"zbuf_span" zbuf_benchmarks;
    Bench.Test.create_group ~name:"header_lookup" header_lookup_benchmarks;
    Bench.Test.create_group ~name:"throughput" throughput_benchmarks;
    (* Comparison benchmarks: OxCaml vs standard OCaml *)
    Bench.Test.create_group ~name:"compare_parse" comparison_parse_benchmarks;
    Bench.Test.create_group ~name:"compare_response" comparison_response_benchmarks;
    Bench.Test.create_group ~name:"compare_serialize" comparison_serialize_benchmarks;
    Bench.Test.create_group ~name:"compare_zbuf" comparison_zbuf_benchmarks;
    Bench.Test.create_group ~name:"compare_header" comparison_header_benchmarks;
    Bench.Test.create_group ~name:"compare_throughput" comparison_throughput_benchmarks;
    (* Httpz (zero-alloc bigarray parser) benchmarks *)
    Bench.Test.create_group ~name:"httpz_parsing" httpz_parsing_benchmarks;
    Bench.Test.create_group ~name:"httpz_headers" httpz_header_benchmarks;
    Bench.Test.create_group ~name:"httpz_body" httpz_body_benchmarks;
    Bench.Test.create_group ~name:"httpz_serialize" httpz_serialize_benchmarks;
    Bench.Test.create_group ~name:"httpz_throughput" httpz_throughput_benchmarks;
    (* Three-way comparison: httpl vs httplo vs httpz *)
    Bench.Test.create_group ~name:"3way_minimal" three_way_minimal_benchmarks;
    Bench.Test.create_group ~name:"3way_browser" three_way_browser_benchmarks;
    Bench.Test.create_group ~name:"3way_50headers" three_way_50headers_benchmarks;
    Bench.Test.create_group ~name:"3way_throughput" three_way_throughput_benchmarks;
  ]

let () = Command_unix.run command
