(* httpz.ml - Stack-allocated HTTP/1.1 parser for OxCaml
   Using Base_bigstring for optimized operations *)

open Base

let buffer_size = 32768
let max_headers = 32

type span = #{ off : int; len : int }

type method_ =
  | GET | HEAD | POST | PUT | DELETE
  | CONNECT | OPTIONS | TRACE | PATCH
  | Other of span

type version = HTTP_1_0 | HTTP_1_1

type header_name =
  | H_cache_control | H_connection | H_date | H_transfer_encoding
  | H_upgrade | H_via
  | H_accept | H_accept_charset | H_accept_encoding | H_accept_language
  | H_authorization | H_cookie | H_expect | H_host
  | H_if_match | H_if_modified_since | H_if_none_match | H_if_unmodified_since
  | H_range | H_referer | H_user_agent
  | H_age | H_etag | H_location | H_retry_after | H_server
  | H_set_cookie | H_www_authenticate
  | H_allow | H_content_disposition | H_content_encoding | H_content_language
  | H_content_length | H_content_location | H_content_range | H_content_type
  | H_expires | H_last_modified
  | H_x_forwarded_for | H_x_forwarded_proto | H_x_forwarded_host
  | H_x_request_id | H_x_correlation_id
  | H_other  (* No payload - name_span in header record holds the span *)

(* Boxed header for local list storage.
   name_span is only valid when name = H_other *)
type header = {
  name : header_name;
  name_span : span;
  value : span;
}

type request = #{
  meth : method_;
  target : span;
  version : version;
  body_off : int;
  content_length : int64;  (** -1L if not present *)
  is_chunked : bool;
  keep_alive : bool;
}

type status =
  | Ok
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed

type buffer = Base_bigstring.t

let create_buffer () =
  Base_bigstring.create buffer_size

let[@inline always] peek buf pos =
  Base_bigstring.unsafe_get buf pos

let[@inline always] ( =. ) (a : char) (b : char) = Char.equal a b
let[@inline always] ( <>. ) (a : char) (b : char) = Poly.(<>) a b

(* Character predicates *)
let[@inline always] is_token_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let[@inline always] is_space = function ' ' | '\t' -> true | _ -> false

let[@inline always] to_lower c =
  if Char.(>=) c 'A' && Char.(<=) c 'Z' then Char.unsafe_of_int (Char.to_int c + 32) else c

(* Span operations - use Base_bigstring.memcmp_string for speed *)
let[@inline] span_equal buf (sp : span) s =
  let slen = String.length s in
  if sp.#len <> slen then false
  else Base_bigstring.memcmp_string buf ~pos1:sp.#off s ~pos2:0 ~len:slen = 0

let[@inline] span_equal_caseless buf (sp : span) s =
  let slen = String.length s in
  if sp.#len <> slen then false
  else begin
    let mutable i = 0 in
    let mutable equal = true in
    let off = sp.#off in
    while equal && i < slen do
      if to_lower (peek buf (off + i)) <>. to_lower (String.unsafe_get s i) then
        equal <- false
      else i <- i + 1
    done;
    equal
  end

let[@inline] parse_int64 buf (sp : span) =
  if sp.#len = 0 then -1L
  else begin
    let mutable acc = 0L in
    let mutable i = 0 in
    let mutable valid = true in
    let off = sp.#off in
    let len = sp.#len in
    while valid && i < len do
      let c = peek buf (off + i) in
      if Char.(>=) c '0' && Char.(<=) c '9' then begin
        let digit = Int64.of_int (Char.to_int c - 48) in
        acc <- Int64.(+) (Int64.( * ) acc 10L) digit;
        i <- i + 1
      end else
        valid <- false
    done;
    if i = 0 then -1L else acc
  end

let span_to_string buf (sp : span) =
  Base_bigstring.To_string.sub buf ~pos:sp.#off ~len:sp.#len

let span_to_bytes buf (sp : span) =
  Base_bigstring.To_bytes.sub buf ~pos:sp.#off ~len:sp.#len

(* Parse header name from span. TODO avsm replace with a DFA *)
let parse_header_name buf (sp : span) : header_name =
  match sp.#len with
  | 3 ->
    if span_equal_caseless buf sp "age" then H_age
    else if span_equal_caseless buf sp "via" then H_via
    else H_other
  | 4 ->
    if span_equal_caseless buf sp "date" then H_date
    else if span_equal_caseless buf sp "etag" then H_etag
    else if span_equal_caseless buf sp "host" then H_host
    else H_other
  | 5 ->
    if span_equal_caseless buf sp "allow" then H_allow
    else if span_equal_caseless buf sp "range" then H_range
    else H_other
  | 6 ->
    if span_equal_caseless buf sp "accept" then H_accept
    else if span_equal_caseless buf sp "cookie" then H_cookie
    else if span_equal_caseless buf sp "expect" then H_expect
    else if span_equal_caseless buf sp "server" then H_server
    else H_other
  | 7 ->
    if span_equal_caseless buf sp "expires" then H_expires
    else if span_equal_caseless buf sp "referer" then H_referer
    else if span_equal_caseless buf sp "upgrade" then H_upgrade
    else H_other
  | 8 ->
    if span_equal_caseless buf sp "if-match" then H_if_match
    else if span_equal_caseless buf sp "location" then H_location
    else H_other
  | 10 ->
    if span_equal_caseless buf sp "connection" then H_connection
    else if span_equal_caseless buf sp "set-cookie" then H_set_cookie
    else if span_equal_caseless buf sp "user-agent" then H_user_agent
    else H_other
  | 11 ->
    if span_equal_caseless buf sp "retry-after" then H_retry_after
    else H_other
  | 12 ->
    if span_equal_caseless buf sp "content-type" then H_content_type
    else if span_equal_caseless buf sp "x-request-id" then H_x_request_id
    else H_other
  | 13 ->
    if span_equal_caseless buf sp "authorization" then H_authorization
    else if span_equal_caseless buf sp "cache-control" then H_cache_control
    else if span_equal_caseless buf sp "content-range" then H_content_range
    else if span_equal_caseless buf sp "if-none-match" then H_if_none_match
    else if span_equal_caseless buf sp "last-modified" then H_last_modified
    else H_other
  | 14 ->
    if span_equal_caseless buf sp "accept-charset" then H_accept_charset
    else if span_equal_caseless buf sp "content-length" then H_content_length
    else H_other
  | 15 ->
    if span_equal_caseless buf sp "accept-encoding" then H_accept_encoding
    else if span_equal_caseless buf sp "accept-language" then H_accept_language
    else if span_equal_caseless buf sp "x-forwarded-for" then H_x_forwarded_for
    else if span_equal_caseless buf sp "x-correlation-id" then H_x_correlation_id
    else H_other
  | 16 ->
    if span_equal_caseless buf sp "content-encoding" then H_content_encoding
    else if span_equal_caseless buf sp "content-language" then H_content_language
    else if span_equal_caseless buf sp "content-location" then H_content_location
    else if span_equal_caseless buf sp "www-authenticate" then H_www_authenticate
    else if span_equal_caseless buf sp "x-forwarded-host" then H_x_forwarded_host
    else H_other
  | 17 ->
    if span_equal_caseless buf sp "if-modified-since" then H_if_modified_since
    else if span_equal_caseless buf sp "transfer-encoding" then H_transfer_encoding
    else if span_equal_caseless buf sp "x-forwarded-proto" then H_x_forwarded_proto
    else H_other
  | 19 ->
    if span_equal_caseless buf sp "content-disposition" then H_content_disposition
    else if span_equal_caseless buf sp "if-unmodified-since" then H_if_unmodified_since
    else H_other
  | _ -> H_other

let[@inline] parse_method buf ~pos ~len : #(status * method_ * int) =
  let mutable p = pos in
  while p < len && is_token_char (peek buf p) do
    p <- p + 1
  done;
  let meth_len = p - pos in
  if meth_len = 0 then #(Invalid_method, GET, 0)
  else
    let sp = #{ off = pos; len = meth_len } in
    let meth = match meth_len with
      | 3 ->
        if span_equal buf sp "GET" then GET
        else if span_equal buf sp "PUT" then PUT
        else Other sp
      | 4 ->
        if span_equal buf sp "POST" then POST
        else if span_equal buf sp "HEAD" then HEAD
        else Other sp
      | 5 ->
        if span_equal buf sp "PATCH" then PATCH
        else if span_equal buf sp "TRACE" then TRACE
        else Other sp
      | 6 ->
        if span_equal buf sp "DELETE" then DELETE
        else Other sp
      | 7 ->
        if span_equal buf sp "OPTIONS" then OPTIONS
        else if span_equal buf sp "CONNECT" then CONNECT
        else Other sp
      | _ -> Other sp
    in
    #(Ok, meth, p)

let[@inline] parse_target buf ~pos ~len : #(status * int * int * int) =
  let mutable p = pos in
  while p < len && (let c = peek buf p in c <>. ' ' && c <>. '\r') do
    p <- p + 1
  done;
  let target_len = p - pos in
  if target_len = 0 then #(Invalid_target, 0, 0, 0)
  else #(Ok, pos, target_len, p)

let[@inline] parse_version buf ~pos ~len : #(status * version * int) =
  if pos + 8 > len then #(Partial, HTTP_1_1, 0)
  else if peek buf pos =. 'H' && peek buf (pos+1) =. 'T' &&
          peek buf (pos+2) =. 'T' && peek buf (pos+3) =. 'P' &&
          peek buf (pos+4) =. '/' && peek buf (pos+5) =. '1' &&
          peek buf (pos+6) =. '.' then
    let minor = peek buf (pos+7) in
    if minor =. '1' then #(Ok, HTTP_1_1, pos + 8)
    else if minor =. '0' then #(Ok, HTTP_1_0, pos + 8)
    else #(Invalid_version, HTTP_1_1, 0)
  else #(Invalid_version, HTTP_1_1, 0)

(* Find CRLF from position - returns position or -1 if not found (no allocation) *)
let find_crlf buf ~pos ~len =
  if len - pos < 2 then -1
  else begin
    let mutable p = pos in
    let mutable found = false in
    while not found && p + 1 < len do
      (* Use unsafe_find to jump to next '\r' *)
      let search_pos = p in
      let search_len = len - p in
      let cr_pos = Base_bigstring.unsafe_find buf '\r' ~pos:search_pos ~len:search_len in
      if cr_pos < 0 || cr_pos >= len - 1 then begin
        (* No '\r' found (returns -1), or found at very end with no room for '\n' *)
        p <- len;
      end else if Base_bigstring.unsafe_get buf (cr_pos + 1) =. '\n' then begin
        p <- cr_pos;
        found <- true
      end else begin
        (* '\r' not followed by '\n', continue searching after it *)
        p <- cr_pos + 1
      end
    done;
    if found then p else -1
  end

let[@inline] parse_header buf ~pos ~len : #(status * header_name * int * int * int * int * int) =
  (* Find colon *)
  let mutable colon_pos = pos in
  while colon_pos < len && is_token_char (peek buf colon_pos) do
    colon_pos <- colon_pos + 1
  done;
  let name_len = colon_pos - pos in
  if name_len = 0 || colon_pos >= len || peek buf colon_pos <>. ':' then
    #(Invalid_header, H_host, 0, 0, 0, 0, 0)
  else begin
    let name_span = #{ off = pos; len = name_len } in
    let name = parse_header_name buf name_span in
    (* Skip colon and OWS *)
    let mutable p = colon_pos + 1 in
    while p < len && is_space (peek buf p) do
      p <- p + 1
    done;
    let value_start = p in
    (* Find end of value (CRLF) - returns -1 if not found *)
    let crlf_pos = find_crlf buf ~pos:p ~len in
    if crlf_pos < 0 then #(Partial, H_host, 0, 0, 0, 0, 0)
    else begin
      (* Trim trailing whitespace *)
      let mutable value_end = crlf_pos in
      while value_end > value_start && is_space (peek buf (value_end - 1)) do
        value_end <- value_end - 1
      done;
      #(Ok, name, pos, name_len, value_start, value_end - value_start, crlf_pos + 2)
    end
  end

(* Parse headers recursively - builds local list on stack
   Tracks content headers (Content-Length, Transfer-Encoding, Connection) separately
   and excludes them from the returned header list.
   conn values: 0=not seen, 1=close, 2=keep-alive *)
let rec parse_headers_loop buf ~pos ~len ~count ~acc ~content_len ~chunked ~conn = exclave_
  if pos + 1 >= len then
    #(Partial, pos, 0, acc, content_len, chunked, conn)
  else if peek buf pos =. '\r' && peek buf (pos+1) =. '\n' then
    (* End of headers *)
    #(Ok, pos + 2, count, acc, content_len, chunked, conn)
  else if count >= max_headers then
    #(Headers_too_large, pos, count, acc, content_len, chunked, conn)
  else
    let #(s, name, noff, nlen, voff, vlen, new_pos) = parse_header buf ~pos ~len in
    if Poly.(<>) s Ok then
      #(s, pos, count, acc, content_len, chunked, conn)
    else
      let value_span = #{ off = voff; len = vlen } in
      (* Check for content headers and handle specially *)
      match name with
      | H_content_length ->
        let cl = parse_int64 buf value_span in
        parse_headers_loop buf ~pos:new_pos ~len ~count:(count + 1) ~acc
          ~content_len:cl ~chunked ~conn
      | H_transfer_encoding ->
        let is_chunked = span_equal_caseless buf value_span "chunked" in
        parse_headers_loop buf ~pos:new_pos ~len ~count:(count + 1) ~acc
          ~content_len ~chunked:is_chunked ~conn
      | H_connection ->
        let new_conn =
          if span_equal_caseless buf value_span "close" then 1
          else if span_equal_caseless buf value_span "keep-alive" then 2
          else conn
        in
        parse_headers_loop buf ~pos:new_pos ~len ~count:(count + 1) ~acc
          ~content_len ~chunked ~conn:new_conn
      | _ ->
        (* Regular header - add to list *)
        let hdr = { name; name_span = #{ off = noff; len = nlen }; value = value_span } in
        parse_headers_loop buf ~pos:new_pos ~len ~count:(count + 1) ~acc:(hdr :: acc)
          ~content_len ~chunked ~conn

(* Main parse function - uses recursive local list building *)
let parse buf ~len = exclave_
  let error_result status = exclave_
    #(status, #{ meth = GET; target = #{ off = 0; len = 0 }; version = HTTP_1_1; body_off = 0;
                 content_length = -1L; is_chunked = false; keep_alive = true }, ([] : header list))
  in
  if len > buffer_size then error_result Headers_too_large
  else
    (* Parse method *)
    let #(s, meth, pos) = parse_method buf ~pos:0 ~len in
    if Poly.(<>) s Ok then error_result s
    else if pos >= len then error_result Partial
    else if peek buf pos <>. ' ' then error_result Invalid_method
    else
      let pos = pos + 1 in
      (* Parse target *)
      let #(s, target_off, target_len, pos) = parse_target buf ~pos ~len in
      if Poly.(<>) s Ok then error_result s
      else if pos >= len then error_result Partial
      else if peek buf pos <>. ' ' then error_result Invalid_target
      else
        let pos = pos + 1 in
        (* Parse version *)
        let #(s, version, pos) = parse_version buf ~pos ~len in
        if Poly.(<>) s Ok then error_result s
        else if pos + 1 >= len then error_result Partial
        else if peek buf pos <>. '\r' || peek buf (pos+1) <>. '\n' then error_result Malformed
        else
          let pos = pos + 2 in
          (* Parse headers recursively - local list (reversed order)
             Content headers are extracted and excluded from the list *)
          let #(s, body_off, _count, headers, content_length, is_chunked, conn) =
            parse_headers_loop buf ~pos ~len ~count:0 ~acc:[]
              ~content_len:(-1L) ~chunked:false ~conn:0
          in
          match s with
          | Ok ->
            let target = #{ off = target_off; len = target_len } in
            (* Compute keep_alive: conn=0 uses version default, 1=close, 2=keep-alive *)
            let keep_alive = match conn with
              | 1 -> false
              | 2 -> true
              | _ -> Poly.(=) version HTTP_1_1
            in
            let req = #{ meth; target; version; body_off; content_length; is_chunked; keep_alive } in
            #(Ok, req, headers)
          | err -> error_result err

let rec find_header (headers : header list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches = match name, hdr.name with
      | H_other, _ | _, H_other -> false  (* Use find_header_string for unknown headers *)
      | n1, n2 -> Poly.(=) n1 n2
    in
    if matches then Some hdr else find_header rest name

(* Canonical lowercase name for known headers *)
let header_name_lowercase = function
  | H_cache_control -> "cache-control"
  | H_connection -> "connection"
  | H_date -> "date"
  | H_transfer_encoding -> "transfer-encoding"
  | H_upgrade -> "upgrade"
  | H_via -> "via"
  | H_accept -> "accept"
  | H_accept_charset -> "accept-charset"
  | H_accept_encoding -> "accept-encoding"
  | H_accept_language -> "accept-language"
  | H_authorization -> "authorization"
  | H_cookie -> "cookie"
  | H_expect -> "expect"
  | H_host -> "host"
  | H_if_match -> "if-match"
  | H_if_modified_since -> "if-modified-since"
  | H_if_none_match -> "if-none-match"
  | H_if_unmodified_since -> "if-unmodified-since"
  | H_range -> "range"
  | H_referer -> "referer"
  | H_user_agent -> "user-agent"
  | H_age -> "age"
  | H_etag -> "etag"
  | H_location -> "location"
  | H_retry_after -> "retry-after"
  | H_server -> "server"
  | H_set_cookie -> "set-cookie"
  | H_www_authenticate -> "www-authenticate"
  | H_allow -> "allow"
  | H_content_disposition -> "content-disposition"
  | H_content_encoding -> "content-encoding"
  | H_content_language -> "content-language"
  | H_content_length -> "content-length"
  | H_content_location -> "content-location"
  | H_content_range -> "content-range"
  | H_content_type -> "content-type"
  | H_expires -> "expires"
  | H_last_modified -> "last-modified"
  | H_x_forwarded_for -> "x-forwarded-for"
  | H_x_forwarded_proto -> "x-forwarded-proto"
  | H_x_forwarded_host -> "x-forwarded-host"
  | H_x_request_id -> "x-request-id"
  | H_x_correlation_id -> "x-correlation-id"
  | H_other -> ""

let rec find_header_string buf (headers : header list @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches = match hdr.name with
      | H_other -> span_equal_caseless buf hdr.name_span name
      | known ->
        let canonical = header_name_lowercase known in
        String.(=) (String.lowercase name) canonical
    in
    if matches then Some hdr else find_header_string buf rest name

(* Debug functions *)
let status_to_string = function
  | Ok -> "Ok"
  | Partial -> "Partial"
  | Invalid_method -> "Invalid_method"
  | Invalid_target -> "Invalid_target"
  | Invalid_version -> "Invalid_version"
  | Invalid_header -> "Invalid_header"
  | Headers_too_large -> "Headers_too_large"
  | Malformed -> "Malformed"

let method_to_string buf = function
  | GET -> "GET"
  | HEAD -> "HEAD"
  | POST -> "POST"
  | PUT -> "PUT"
  | DELETE -> "DELETE"
  | CONNECT -> "CONNECT"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | PATCH -> "PATCH"
  | Other sp -> span_to_string buf sp

let version_to_string = function
  | HTTP_1_0 -> "HTTP/1.0"
  | HTTP_1_1 -> "HTTP/1.1"

let header_name_to_string _buf = function
  | H_cache_control -> "Cache-Control"
  | H_connection -> "Connection"
  | H_date -> "Date"
  | H_transfer_encoding -> "Transfer-Encoding"
  | H_upgrade -> "Upgrade"
  | H_via -> "Via"
  | H_accept -> "Accept"
  | H_accept_charset -> "Accept-Charset"
  | H_accept_encoding -> "Accept-Encoding"
  | H_accept_language -> "Accept-Language"
  | H_authorization -> "Authorization"
  | H_cookie -> "Cookie"
  | H_expect -> "Expect"
  | H_host -> "Host"
  | H_if_match -> "If-Match"
  | H_if_modified_since -> "If-Modified-Since"
  | H_if_none_match -> "If-None-Match"
  | H_if_unmodified_since -> "If-Unmodified-Since"
  | H_range -> "Range"
  | H_referer -> "Referer"
  | H_user_agent -> "User-Agent"
  | H_age -> "Age"
  | H_etag -> "ETag"
  | H_location -> "Location"
  | H_retry_after -> "Retry-After"
  | H_server -> "Server"
  | H_set_cookie -> "Set-Cookie"
  | H_www_authenticate -> "WWW-Authenticate"
  | H_allow -> "Allow"
  | H_content_disposition -> "Content-Disposition"
  | H_content_encoding -> "Content-Encoding"
  | H_content_language -> "Content-Language"
  | H_content_length -> "Content-Length"
  | H_content_location -> "Content-Location"
  | H_content_range -> "Content-Range"
  | H_content_type -> "Content-Type"
  | H_expires -> "Expires"
  | H_last_modified -> "Last-Modified"
  | H_x_forwarded_for -> "X-Forwarded-For"
  | H_x_forwarded_proto -> "X-Forwarded-Proto"
  | H_x_forwarded_host -> "X-Forwarded-Host"
  | H_x_request_id -> "X-Request-Id"
  | H_x_correlation_id -> "X-Correlation-Id"
  | H_other -> "(unknown)"

(* Body handling - use cached values from request struct *)
let body_in_buffer ~len (req : request @ local) =
  if req.#is_chunked then false
  else
    let cl = req.#content_length in
    if Int64.(<=) cl 0L then true
    else
      let body_end = req.#body_off + Int64.to_int_exn cl in
      body_end <= len

let body_span ~len (req : request @ local) =
  if req.#is_chunked then #{ off = 0; len = -1 }
  else
    let cl = req.#content_length in
    if Int64.(<=) cl 0L then #{ off = req.#body_off; len = 0 }
    else
      let body_len = Int64.to_int_exn cl in
      let body_end = req.#body_off + body_len in
      if body_end <= len then #{ off = req.#body_off; len = body_len }
      else #{ off = 0; len = -1 }

let body_bytes_needed ~len (req : request @ local) =
  if req.#is_chunked then -1
  else
    let cl = req.#content_length in
    if Int64.(<=) cl 0L then 0
    else
      let body_len = Int64.to_int_exn cl in
      let body_end = req.#body_off + body_len in
      if body_end <= len then 0
      else body_end - len

(* Chunked transfer encoding *)
type chunk_status =
  | Chunk_ok
  | Chunk_partial
  | Chunk_done
  | Chunk_error

type chunk = #{
  data_off : int;
  data_len : int;
  next_off : int;
}

let parse_chunk buf ~off ~len =
  let empty_chunk = #{ data_off = 0; data_len = 0; next_off = 0 } in
  if off >= len then #(Chunk_partial, empty_chunk)
  else begin
    (* Parse hex size *)
    let mutable p = off in
    let mutable size = 0 in
    let mutable valid = true in
    while valid && p < len do
      let c = peek buf p in
      if Char.(>=) c '0' && Char.(<=) c '9' then begin
        size <- size * 16 + (Char.to_int c - 48);
        p <- p + 1
      end else if Char.(>=) c 'a' && Char.(<=) c 'f' then begin
        size <- size * 16 + (Char.to_int c - 87);
        p <- p + 1
      end else if Char.(>=) c 'A' && Char.(<=) c 'F' then begin
        size <- size * 16 + (Char.to_int c - 55);
        p <- p + 1
      end else
        valid <- false
    done;
    if p = off then #(Chunk_error, empty_chunk)
    else begin
      (* Skip optional chunk extension and CRLF *)
      while p < len && peek buf p <>. '\r' do
        p <- p + 1
      done;
      if p + 1 >= len then #(Chunk_partial, empty_chunk)
      else if peek buf (p+1) <>. '\n' then #(Chunk_error, empty_chunk)
      else begin
        let data_off = p + 2 in
        if size = 0 then begin
          (* Final chunk - skip trailing CRLF *)
          if data_off + 1 >= len then #(Chunk_partial, empty_chunk)
          else if peek buf data_off =. '\r' && peek buf (data_off+1) =. '\n' then
            #(Chunk_done, #{ data_off; data_len = 0; next_off = data_off + 2 })
          else
            #(Chunk_done, #{ data_off; data_len = 0; next_off = data_off })
        end else begin
          let data_end = data_off + size in
          if data_end + 1 >= len then #(Chunk_partial, empty_chunk)
          else if peek buf data_end <>. '\r' || peek buf (data_end+1) <>. '\n' then
            #(Chunk_error, empty_chunk)
          else
            #(Chunk_ok, #{ data_off; data_len = size; next_off = data_end + 2 })
        end
      end
    end
  end

(* Response types and writing *)
type response_status =
  | S200_OK | S201_Created | S204_No_Content
  | S301_Moved_Permanently | S302_Found | S304_Not_Modified
  | S400_Bad_Request | S401_Unauthorized | S403_Forbidden | S404_Not_Found
  | S405_Method_Not_Allowed | S411_Length_Required | S413_Payload_Too_Large
  | S500_Internal_Server_Error | S502_Bad_Gateway | S503_Service_Unavailable

let response_status_code = function
  | S200_OK -> 200
  | S201_Created -> 201
  | S204_No_Content -> 204
  | S301_Moved_Permanently -> 301
  | S302_Found -> 302
  | S304_Not_Modified -> 304
  | S400_Bad_Request -> 400
  | S401_Unauthorized -> 401
  | S403_Forbidden -> 403
  | S404_Not_Found -> 404
  | S405_Method_Not_Allowed -> 405
  | S411_Length_Required -> 411
  | S413_Payload_Too_Large -> 413
  | S500_Internal_Server_Error -> 500
  | S502_Bad_Gateway -> 502
  | S503_Service_Unavailable -> 503

let response_status_reason = function
  | S200_OK -> "OK"
  | S201_Created -> "Created"
  | S204_No_Content -> "No Content"
  | S301_Moved_Permanently -> "Moved Permanently"
  | S302_Found -> "Found"
  | S304_Not_Modified -> "Not Modified"
  | S400_Bad_Request -> "Bad Request"
  | S401_Unauthorized -> "Unauthorized"
  | S403_Forbidden -> "Forbidden"
  | S404_Not_Found -> "Not Found"
  | S405_Method_Not_Allowed -> "Method Not Allowed"
  | S411_Length_Required -> "Length Required"
  | S413_Payload_Too_Large -> "Payload Too Large"
  | S500_Internal_Server_Error -> "Internal Server Error"
  | S502_Bad_Gateway -> "Bad Gateway"
  | S503_Service_Unavailable -> "Service Unavailable"

(* Write string to bytes at offset, return new offset *)
let[@inline] write_string_at dst off s =
  let len = String.length s in
  Bytes.From_string.blit ~src:s ~src_pos:0 ~dst ~dst_pos:off ~len;
  off + len

(* Write integer to bytes at offset, return new offset *)
let write_int_at dst off n =
  if n = 0 then begin
    Bytes.unsafe_set dst off '0';
    off + 1
  end else begin
    (* Count digits *)
    let mutable temp = n in
    let mutable digits = 0 in
    while temp > 0 do
      digits <- digits + 1;
      temp <- temp / 10
    done;
    (* Write digits right-to-left *)
    let mutable p = off + digits - 1 in
    let mutable remaining = n in
    while remaining > 0 do
      Bytes.unsafe_set dst p (Char.of_int_exn (48 + Int.rem remaining 10));
      remaining <- remaining / 10;
      p <- p - 1
    done;
    off + digits
  end

let write_status_line dst ~off status version =
  (* HTTP/1.x *)
  let off = write_string_at dst off (version_to_string version) in
  Bytes.unsafe_set dst off ' ';
  let off = off + 1 in
  (* Status code *)
  let off = write_int_at dst off (response_status_code status) in
  Bytes.unsafe_set dst off ' ';
  let off = off + 1 in
  (* Reason phrase *)
  let off = write_string_at dst off (response_status_reason status) in
  (* CRLF *)
  Bytes.unsafe_set dst off '\r';
  Bytes.unsafe_set dst (off+1) '\n';
  off + 2

let write_header dst ~off name value =
  let off = write_string_at dst off name in
  Bytes.unsafe_set dst off ':';
  Bytes.unsafe_set dst (off+1) ' ';
  let off = off + 2 in
  let off = write_string_at dst off value in
  Bytes.unsafe_set dst off '\r';
  Bytes.unsafe_set dst (off+1) '\n';
  off + 2

let write_header_int dst ~off name value =
  let off = write_string_at dst off name in
  Bytes.unsafe_set dst off ':';
  Bytes.unsafe_set dst (off+1) ' ';
  let off = off + 2 in
  let off = write_int_at dst off value in
  Bytes.unsafe_set dst off '\r';
  Bytes.unsafe_set dst (off+1) '\n';
  off + 2

let write_crlf dst ~off =
  Bytes.unsafe_set dst off '\r';
  Bytes.unsafe_set dst (off+1) '\n';
  off + 2

let write_content_length dst ~off len =
  write_header_int dst ~off "Content-Length" len

let write_connection dst ~off keep_alive =
  if keep_alive then write_header dst ~off "Connection" "keep-alive"
  else write_header dst ~off "Connection" "close"
