# httpz - Zero-Allocation HTTP/1.1 Parser for OxCaml

A high-performance HTTP/1.1 parser and serializer that achieves near-zero heap allocations using OxCaml's unboxed types and local allocations.

## Features

- **Zero heap allocations**: Parser results are stack-allocated using OxCaml unboxed records and local lists
- **2-3x faster** than traditional Eio-based parsers
- **Direct bigstring I/O**: Read and write directly to/from bigarray buffers
- **Complete HTTP/1.1 support**: Methods, headers, chunked transfer encoding, keep-alive
- **Async file server included**: Production-ready static file server

## Performance

Benchmarks comparing httpz (OxCaml) vs httpe (Eio-based parser):

| Request Size | httpz (ns/op) | httpe (ns/op) | Speedup | Allocation Reduction |
|--------------|---------------|---------------|---------|---------------------|
| Small (35B)  | 69            | 218           | **3.14x** | 94x fewer words   |
| Medium (439B)| 792           | 1,690         | **2.13x** | 399x fewer words  |
| Large (1155B)| 1,771         | 4,017         | **2.27x** | 829x fewer words  |

**Throughput**: 14.6M requests/sec (vs 4.6M for httpe)

### Detailed Timings

| Operation | Time | Heap Allocations |
|-----------|------|------------------|
| Parse minimal request | 209ns | 3 words |
| Parse browser request (10 headers) | 2.8μs | 3 words |
| Parse 50 headers | 7.7μs | 3 words |
| Write status line | 21ns | 3 words |
| Write full response headers | 62ns | 3 words |

## Installation

Requires OxCaml compiler with unboxed types support.

```bash
opam pin add httpz .
```

## Usage

### Parsing HTTP Requests

```ocaml
let buf = Httpz.create_buffer () in
(* Read data into buf from socket... *)
let len = (* bytes read *) in

let #(status, req, headers) = Httpz.parse buf ~len in
match status with
| Httpz.Ok ->
  (* Access request fields - all stack allocated *)
  let meth = req.#meth in
  let target = req.#target in
  let keep_alive = req.#keep_alive in
  let content_length = req.#content_length in

  (* Find headers *)
  (match Httpz.find_header headers Httpz.H_host with
   | Some h -> Httpz.span_to_string buf h.value
   | None -> "unknown")

| Httpz.Partial -> (* need more data *)
| _ -> (* error *)
```

### Writing HTTP Responses

```ocaml
let response_buf = Bytes.create 4096 in
let off = 0 in
let off = Httpz.write_status_line response_buf ~off Httpz.S200_OK Httpz.HTTP_1_1 in
let off = Httpz.write_header response_buf ~off "Content-Type" "application/json" in
let off = Httpz.write_content_length response_buf ~off body_len in
let off = Httpz.write_connection response_buf ~off true in
let off = Httpz.write_crlf response_buf ~off in
(* Write response_buf[0..off] to socket, then write body *)
```

### Bigstring Variants (Zero-Copy with Async)

```ocaml
let buf = Httpz.create_buffer () in  (* This is a bigstring *)
let off = Httpz.write_status_line_bigstring buf ~off status version in
let off = Httpz.write_header_bigstring buf ~off "Content-Type" content_type in
(* ... *)
Writer.write_bigstring writer buf ~pos:0 ~len:off  (* Zero-copy write *)
```

## Static File Server

A production-ready Async-based static file server is included:

```bash
# Serve current directory on port 8080
dune exec bin/httpz_server.exe

# Serve specific directory on custom port
dune exec bin/httpz_server.exe -- -d /var/www -p 3000

# Get help
dune exec bin/httpz_server.exe -- -help
```

Features:
- Async concurrent connection handling (up to 10,000 connections)
- Zero-copy bigstring I/O
- MIME type detection
- Directory traversal protection
- Automatic `index.html` for directories

## Running Benchmarks

```bash
# Comparative benchmark (httpz vs httpe)
dune exec bench/bench_compare.exe

# Detailed httpz benchmarks with core_bench
dune exec bench/bench_httpz.exe -- -quota 2
```

## Architecture

httpz achieves zero-allocation parsing through:

1. **Unboxed records** (`#{...}`): Request and span types are stack-allocated
2. **Local lists** (`@ local`): Header list grows on the stack, not heap
3. **Span-based parsing**: Strings are referenced by offset+length into the input buffer
4. **Pre-allocated buffers**: 32KB read buffer reused across requests

## License

ISC
