# httpz - Zero-Allocation HTTP/1.1 Parser for OxCaml

A high-performance HTTP/1.1 parser and serializer that aims for zero heap
allocations using OxCaml's unboxed types and local allocations.

Will soon have io_uring on Linux.

## Features

- **Zero heap allocations**: Parser results are stack-allocated using OxCaml unboxed records and local lists
- **Direct bigstring I/O**: Read and write directly to/from bigarray buffers
- **HTTP/1.1 support**: Methods, headers, chunked transfer encoding, keep-alive
- **Async file server included**: Production-ready static file server. Soon to be parallel.

## Architecture

httpz achieves zero-allocation parsing through:

1. **Unboxed records** (`#{...}`): Request and span types are stack-allocated
2. **Local lists** (`@ local`): Header list grows on the stack, not heap
3. **Span-based parsing**: Strings are referenced by offset+length into the input buffer
4. **Pre-allocated buffers**: 32KB read buffer reused across requests

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

Requires OxCaml compiler from https://oxcaml.org/

## Static File Server

An Async-based static file server is included:

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

## License

ISC
