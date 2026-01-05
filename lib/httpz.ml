(* httpz.ml - Stack-allocated HTTP/1.1 parser for OxCaml *)

open Base

let buffer_size = 32768
let max_headers = 128

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
  | H_other of span

type header = {
  name : header_name;
  value : span;
}

type request = #{
  meth : method_;
  target : span;
  version : version;
  body_off : int;
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

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let create_buffer () =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout buffer_size

(* Buffer access *)
let[@inline always] peek buf pos =
  Bigarray.Array1.unsafe_get buf pos

(* Character operations - use Poly for simple char comparisons *)
let[@inline always] ( =. ) (a : char) (b : char) = Poly.(=) a b
let[@inline always] ( <>. ) (a : char) (b : char) = Poly.(<>) a b

(* Character predicates *)
let[@inline always] is_token_char = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let[@inline always] is_space = function ' ' | '\t' -> true | _ -> false

let[@inline always] to_lower c =
  if Char.(>=) c 'A' && Char.(<=) c 'Z' then Char.of_int_exn (Char.to_int c + 32) else c

(* Span operations *)
let span_equal buf (sp : span) s =
  let slen = String.length s in
  if sp.#len <> slen then false
  else begin
    let mutable i = 0 in
    let mutable equal = true in
    while equal && i < slen do
      if peek buf (sp.#off + i) <>. String.get s i then equal <- false
      else i <- i + 1
    done;
    equal
  end

let span_equal_caseless buf (sp : span) s =
  let slen = String.length s in
  if sp.#len <> slen then false
  else begin
    let mutable i = 0 in
    let mutable equal = true in
    while equal && i < slen do
      if to_lower (peek buf (sp.#off + i)) <>. to_lower (String.get s i) then
        equal <- false
      else i <- i + 1
    done;
    equal
  end

let parse_int64 buf (sp : span) =
  if sp.#len = 0 then -1L
  else begin
    let mutable acc = 0L in
    let mutable i = 0 in
    let mutable valid = true in
    while valid && i < sp.#len do
      let c = peek buf (sp.#off + i) in
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
  let s = Bytes.create sp.#len in
  for i = 0 to sp.#len - 1 do
    Bytes.unsafe_set s i (peek buf (sp.#off + i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s

let span_to_bytes buf (sp : span) =
  let s = Bytes.create sp.#len in
  for i = 0 to sp.#len - 1 do
    Bytes.unsafe_set s i (peek buf (sp.#off + i))
  done;
  s

(* Parse header name from span *)
let parse_header_name buf (sp : span) : header_name =
  match sp.#len with
  | 3 ->
    if span_equal_caseless buf sp "age" then H_age
    else if span_equal_caseless buf sp "via" then H_via
    else H_other sp
  | 4 ->
    if span_equal_caseless buf sp "date" then H_date
    else if span_equal_caseless buf sp "etag" then H_etag
    else if span_equal_caseless buf sp "host" then H_host
    else H_other sp
  | 5 ->
    if span_equal_caseless buf sp "allow" then H_allow
    else if span_equal_caseless buf sp "range" then H_range
    else H_other sp
  | 6 ->
    if span_equal_caseless buf sp "accept" then H_accept
    else if span_equal_caseless buf sp "cookie" then H_cookie
    else if span_equal_caseless buf sp "expect" then H_expect
    else if span_equal_caseless buf sp "server" then H_server
    else H_other sp
  | 7 ->
    if span_equal_caseless buf sp "expires" then H_expires
    else if span_equal_caseless buf sp "referer" then H_referer
    else if span_equal_caseless buf sp "upgrade" then H_upgrade
    else H_other sp
  | 8 ->
    if span_equal_caseless buf sp "if-match" then H_if_match
    else if span_equal_caseless buf sp "location" then H_location
    else H_other sp
  | 10 ->
    if span_equal_caseless buf sp "connection" then H_connection
    else if span_equal_caseless buf sp "set-cookie" then H_set_cookie
    else if span_equal_caseless buf sp "user-agent" then H_user_agent
    else H_other sp
  | 11 ->
    if span_equal_caseless buf sp "retry-after" then H_retry_after
    else H_other sp
  | 12 ->
    if span_equal_caseless buf sp "content-type" then H_content_type
    else if span_equal_caseless buf sp "x-request-id" then H_x_request_id
    else H_other sp
  | 13 ->
    if span_equal_caseless buf sp "authorization" then H_authorization
    else if span_equal_caseless buf sp "cache-control" then H_cache_control
    else if span_equal_caseless buf sp "content-range" then H_content_range
    else if span_equal_caseless buf sp "if-none-match" then H_if_none_match
    else if span_equal_caseless buf sp "last-modified" then H_last_modified
    else H_other sp
  | 14 ->
    if span_equal_caseless buf sp "accept-charset" then H_accept_charset
    else if span_equal_caseless buf sp "content-length" then H_content_length
    else H_other sp
  | 15 ->
    if span_equal_caseless buf sp "accept-encoding" then H_accept_encoding
    else if span_equal_caseless buf sp "accept-language" then H_accept_language
    else if span_equal_caseless buf sp "x-forwarded-for" then H_x_forwarded_for
    else if span_equal_caseless buf sp "x-correlation-id" then H_x_correlation_id
    else H_other sp
  | 16 ->
    if span_equal_caseless buf sp "content-encoding" then H_content_encoding
    else if span_equal_caseless buf sp "content-language" then H_content_language
    else if span_equal_caseless buf sp "content-location" then H_content_location
    else if span_equal_caseless buf sp "www-authenticate" then H_www_authenticate
    else if span_equal_caseless buf sp "x-forwarded-host" then H_x_forwarded_host
    else H_other sp
  | 17 ->
    if span_equal_caseless buf sp "if-modified-since" then H_if_modified_since
    else if span_equal_caseless buf sp "transfer-encoding" then H_transfer_encoding
    else if span_equal_caseless buf sp "x-forwarded-proto" then H_x_forwarded_proto
    else H_other sp
  | 19 ->
    if span_equal_caseless buf sp "content-disposition" then H_content_disposition
    else if span_equal_caseless buf sp "if-unmodified-since" then H_if_unmodified_since
    else H_other sp
  | _ -> H_other sp

(* Boxed result types for internal parsing *)
type method_result = { mr_meth : method_; mr_pos : int }
type target_result = { tr_off : int; tr_len : int; tr_pos : int }
type version_result = { vr_version : version; vr_pos : int }
type header_result = { hr_hdr : header; hr_pos : int }

(* Parse method *)
let parse_method buf ~pos ~len : (method_result, status) result =
  let mutable p = pos in
  while p < len && is_token_char (peek buf p) do
    p <- p + 1
  done;
  let meth_len = p - pos in
  if meth_len = 0 then Error Invalid_method
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
    Result.Ok { mr_meth = meth; mr_pos = p }

(* Parse request target *)
let parse_target buf ~pos ~len : (target_result, status) result =
  let mutable p = pos in
  while p < len && (let c = peek buf p in c <>. ' ' && c <>. '\r') do
    p <- p + 1
  done;
  let target_len = p - pos in
  if target_len = 0 then Error Invalid_target
  else Result.Ok { tr_off = pos; tr_len = target_len; tr_pos = p }

(* Parse HTTP version *)
let parse_version buf ~pos ~len : (version_result, status) result =
  if pos + 8 > len then Error Partial
  else if peek buf pos =. 'H' && peek buf (pos+1) =. 'T' &&
          peek buf (pos+2) =. 'T' && peek buf (pos+3) =. 'P' &&
          peek buf (pos+4) =. '/' && peek buf (pos+5) =. '1' &&
          peek buf (pos+6) =. '.' then
    let minor = peek buf (pos+7) in
    if minor =. '1' then Result.Ok { vr_version = HTTP_1_1; vr_pos = pos + 8 }
    else if minor =. '0' then Result.Ok { vr_version = HTTP_1_0; vr_pos = pos + 8 }
    else Error Invalid_version
  else Error Invalid_version

(* Find CRLF from position *)
let find_crlf buf ~pos ~len =
  let mutable p = pos in
  let mutable found = false in
  while not found && p + 1 < len do
    if peek buf p =. '\r' && peek buf (p+1) =. '\n' then found <- true
    else p <- p + 1
  done;
  if found then Some p else None

(* Parse a single header *)
let parse_header buf ~pos ~len : (header_result, status) result =
  (* Find colon *)
  let mutable colon_pos = pos in
  while colon_pos < len && is_token_char (peek buf colon_pos) do
    colon_pos <- colon_pos + 1
  done;
  let name_len = colon_pos - pos in
  if name_len = 0 || colon_pos >= len || peek buf colon_pos <>. ':' then
    Error Invalid_header
  else begin
    let name_span = #{ off = pos; len = name_len } in
    let name = parse_header_name buf name_span in
    (* Skip colon and OWS *)
    let mutable p = colon_pos + 1 in
    while p < len && is_space (peek buf p) do
      p <- p + 1
    done;
    let value_start = p in
    (* Find end of value (CRLF) *)
    match find_crlf buf ~pos:p ~len with
    | None -> Error Partial
    | Some crlf_pos ->
      (* Trim trailing whitespace *)
      let mutable value_end = crlf_pos in
      while value_end > value_start && is_space (peek buf (value_end - 1)) do
        value_end <- value_end - 1
      done;
      let value = #{ off = value_start; len = value_end - value_start } in
      let hdr = { name; value } in
      Result.Ok { hr_hdr = hdr; hr_pos = crlf_pos + 2 }
  end

(* Main parse function *)
let parse buf ~len = exclave_
  if len > buffer_size then
    #(Headers_too_large, #{ meth = GET; target = #{ off = 0; len = 0 }; version = HTTP_1_1; body_off = 0 }, [])
  else
    let mutable pos = 0 in
    let mutable status = Ok in
    let mutable meth = GET in
    let mutable target_off = 0 in
    let mutable target_len = 0 in
    let mutable version = HTTP_1_1 in
    let mutable headers = [] in
    let mutable header_count = 0 in
    let mutable body_off = 0 in
    let mutable done_ = false in

    (* Parse method *)
    if not done_ then begin
      match parse_method buf ~pos ~len with
      | Error e -> status <- e; done_ <- true
      | Result.Ok r ->
        meth <- r.mr_meth;
        pos <- r.mr_pos;
        (* Expect space *)
        if pos >= len then begin status <- Partial; done_ <- true end
        else if peek buf pos <>. ' ' then begin status <- Invalid_method; done_ <- true end
        else pos <- pos + 1
    end;

    (* Parse target *)
    if not done_ then begin
      match parse_target buf ~pos ~len with
      | Error e -> status <- e; done_ <- true
      | Result.Ok r ->
        target_off <- r.tr_off;
        target_len <- r.tr_len;
        pos <- r.tr_pos;
        (* Expect space *)
        if pos >= len then begin status <- Partial; done_ <- true end
        else if peek buf pos <>. ' ' then begin status <- Invalid_target; done_ <- true end
        else pos <- pos + 1
    end;

    (* Parse version *)
    if not done_ then begin
      match parse_version buf ~pos ~len with
      | Error e -> status <- e; done_ <- true
      | Result.Ok r ->
        version <- r.vr_version;
        pos <- r.vr_pos;
        (* Expect CRLF *)
        if pos + 1 >= len then begin status <- Partial; done_ <- true end
        else if peek buf pos <>. '\r' || peek buf (pos+1) <>. '\n' then begin
          status <- Malformed; done_ <- true
        end
        else pos <- pos + 2
    end;

    (* Parse headers *)
    while not done_ do
      (* Check for end of headers (empty line) *)
      if pos + 1 >= len then begin
        status <- Partial; done_ <- true
      end else if peek buf pos =. '\r' && peek buf (pos+1) =. '\n' then begin
        (* End of headers *)
        body_off <- pos + 2;
        done_ <- true
      end else if header_count >= max_headers then begin
        status <- Headers_too_large; done_ <- true
      end else begin
        match parse_header buf ~pos ~len with
        | Error e -> status <- e; done_ <- true
        | Result.Ok r ->
          headers <- r.hr_hdr :: headers;
          header_count <- header_count + 1;
          pos <- r.hr_pos
      end
    done;

    (* Reverse headers to get correct order *)
    let headers = List.rev headers in

    let target = #{ off = target_off; len = target_len } in
    let req = #{ meth; target; version; body_off } in
    #(status, req, headers)

(* Header utilities *)
let rec find_header (headers @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let matches = match name, hdr.name with
      | H_other sp1, H_other sp2 -> sp1.#off = sp2.#off && sp1.#len = sp2.#len
      | H_other _, _ | _, H_other _ -> false
      | n1, n2 -> Poly.(=) n1 n2
    in
    if matches then Some hdr else find_header rest name

let rec find_header_string buf (headers @ local) name = exclave_
  match headers with
  | [] -> None
  | hdr :: rest ->
    let name_span = match hdr.name with
      | H_other sp -> sp
      | _ ->
        (* Get canonical name and compare *)
        let canonical = match hdr.name with
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
          | H_other _ -> ""
        in
        if String.(=) (String.lowercase name) canonical then
          #{ off = 0; len = -1 } (* Signal match *)
        else
          #{ off = 0; len = 0 } (* No match, continue *)
    in
    if name_span.#len = -1 then Some hdr
    else if name_span.#len > 0 && span_equal_caseless buf name_span name then Some hdr
    else find_header_string buf rest name

let content_length buf (headers @ local) =
  match find_header headers H_content_length with
  | None -> -1L
  | Some hdr -> parse_int64 buf hdr.value

let is_chunked buf (headers @ local) =
  match find_header headers H_transfer_encoding with
  | None -> false
  | Some hdr -> span_equal_caseless buf hdr.value "chunked"

let is_keep_alive buf (headers @ local) version =
  match find_header headers H_connection with
  | None -> Poly.(=) version HTTP_1_1
  | Some hdr ->
    if span_equal_caseless buf hdr.value "close" then false
    else if span_equal_caseless buf hdr.value "keep-alive" then true
    else Poly.(=) version HTTP_1_1

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

let header_name_to_string buf = function
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
  | H_other sp -> span_to_string buf sp
