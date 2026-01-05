(* httpl.ml - Zero-copy HTTP/1.1 parser and serializer using Zbuf

   Design principles:
   - No string allocation during parsing; store spans into multi-buffer zbuf
   - Explicit materialization: strings only created on request
   - Multi-buffer support via Zbuf for fragmented reads
   - Concurrent parsing via independent cursors
*)

(* ============================================================
   Core Types
   ============================================================ *)

(** Re-export span from Zbuf *)
type span = Zbuf.span = { start: Zbuf.pos; len: int }

(** Parser state combining zbuf and cursor *)
type buffer = {
  zb: Zbuf.t;
  cur: Zbuf.cursor;
}

(** Refill function: push data to buffer. Push zero-length fragment for EOF. *)
type refill = buffer -> unit

(** HTTP methods - Other uses span to avoid allocation *)
type method_ =
  | GET | POST | HEAD | PUT | DELETE | PATCH
  | OPTIONS | TRACE | CONNECT
  | Other of span

(** HTTP version *)
type version = HTTP_1_0 | HTTP_1_1

(** Common HTTP header names as variants for efficient matching *)
type header_name =
  (* General headers *)
  | H_cache_control
  | H_connection
  | H_date
  | H_transfer_encoding
  | H_upgrade
  | H_via
  (* Request headers *)
  | H_accept
  | H_accept_charset
  | H_accept_encoding
  | H_accept_language
  | H_authorization
  | H_cookie
  | H_expect
  | H_host
  | H_if_match
  | H_if_modified_since
  | H_if_none_match
  | H_if_unmodified_since
  | H_range
  | H_referer
  | H_user_agent
  (* Response headers *)
  | H_age
  | H_etag
  | H_location
  | H_retry_after
  | H_server
  | H_set_cookie
  | H_www_authenticate
  (* Entity headers *)
  | H_allow
  | H_content_disposition
  | H_content_encoding
  | H_content_language
  | H_content_length
  | H_content_location
  | H_content_range
  | H_content_type
  | H_expires
  | H_last_modified
  (* Common extension headers *)
  | H_x_forwarded_for
  | H_x_forwarded_proto
  | H_x_forwarded_host
  | H_x_request_id
  | H_x_correlation_id
  (* Unknown header - stores span for zero-copy access *)
  | H_other of span

(** A header entry with name span and value span *)
type header_entry = { name_span: span; value: span }

(* ============================================================
   Headers Module - Map-based O(log n) lookup
   ============================================================ *)

module Headers = struct
  module IntMap = Map.Make(Int)

  (** Map int key for known header names. Returns -1 for H_other. *)
  let key_of_name = function
    | H_cache_control -> 0 | H_connection -> 1 | H_date -> 2
    | H_transfer_encoding -> 3 | H_upgrade -> 4 | H_via -> 5
    | H_accept -> 6 | H_accept_charset -> 7 | H_accept_encoding -> 8
    | H_accept_language -> 9 | H_authorization -> 10 | H_cookie -> 11
    | H_expect -> 12 | H_host -> 13 | H_if_match -> 14
    | H_if_modified_since -> 15 | H_if_none_match -> 16
    | H_if_unmodified_since -> 17 | H_range -> 18 | H_referer -> 19
    | H_user_agent -> 20 | H_age -> 21 | H_etag -> 22 | H_location -> 23
    | H_retry_after -> 24 | H_server -> 25 | H_set_cookie -> 26
    | H_www_authenticate -> 27 | H_allow -> 28 | H_content_disposition -> 29
    | H_content_encoding -> 30 | H_content_language -> 31
    | H_content_length -> 32 | H_content_location -> 33
    | H_content_range -> 34 | H_content_type -> 35 | H_expires -> 36
    | H_last_modified -> 37 | H_x_forwarded_for -> 38
    | H_x_forwarded_proto -> 39 | H_x_forwarded_host -> 40
    | H_x_request_id -> 41 | H_x_correlation_id -> 42
    | H_other _ -> -1

  (** Header storage: known headers in IntMap, unknown in list *)
  type t = {
    known: (header_name * header_entry list) IntMap.t;  (** key -> (canonical_name, values) *)
    unknown: (span * header_entry) list;  (** (name_span, entry) for H_other *)
    count: int;
  }

  let empty = { known = IntMap.empty; unknown = []; count = 0 }

  let count t = t.count

  (** Add a header. For known headers, appends to value list. *)
  let add t name ~name_span ~value =
    let entry = { name_span; value } in
    let new_count = t.count + 1 in
    let key = key_of_name name in
    if key >= 0 then begin
      let entries = match IntMap.find_opt key t.known with
        | None -> (name, [entry])
        | Some (n, es) -> (n, entry :: es)
      in
      { known = IntMap.add key entries t.known; unknown = t.unknown; count = new_count }
    end else begin
      (* H_other - store with name span for later comparison *)
      let name_sp = match name with H_other sp -> sp | _ -> assert false in
      { known = t.known; unknown = (name_sp, entry) :: t.unknown; count = new_count }
    end

  (** Find first matching header by known name. O(log n) *)
  let find_known t name =
    let key = key_of_name name in
    if key < 0 then None
    else match IntMap.find_opt key t.known with
      | None -> None
      | Some (_, entries) ->
        (* Return most recent (entries are in reverse order) *)
        match entries with [] -> None | e :: _ -> Some e.value

  (** Find all values for a known header. Returns in order added. *)
  let find_all_known t name =
    let key = key_of_name name in
    if key < 0 then []
    else match IntMap.find_opt key t.known with
      | None -> []
      | Some (_, entries) -> List.rev_map (fun e -> e.value) entries

  (** Find header by name span (for unknown headers). Needs zbuf for comparison. *)
  let find_unknown zb t name_span =
    let rec loop = function
      | [] -> None
      | (sp, entry) :: rest ->
        if Zbuf.span_equal_caseless zb sp (Zbuf.span_to_string zb name_span) then
          Some entry.value
        else loop rest
    in
    loop t.unknown

  (** Find header by string name. Checks known first, then unknown. *)
  let find_by_string zb t name =
    (* Try to match against known header names by length *)
    let len = String.length name in
    let try_known () =
      (* Check common headers by length - same logic as parse_header_name *)
      let check_caseless s h =
        if String.lowercase_ascii name = s then find_known t h else None
      in
      match len with
      | 3 ->
        (match check_caseless "age" H_age with Some v -> Some v
         | None -> check_caseless "via" H_via)
      | 4 ->
        (match check_caseless "date" H_date with Some v -> Some v
         | None -> match check_caseless "etag" H_etag with Some v -> Some v
         | None -> check_caseless "host" H_host)
      | 6 ->
        (match check_caseless "accept" H_accept with Some v -> Some v
         | None -> match check_caseless "cookie" H_cookie with Some v -> Some v
         | None -> match check_caseless "expect" H_expect with Some v -> Some v
         | None -> check_caseless "server" H_server)
      | 10 ->
        (match check_caseless "connection" H_connection with Some v -> Some v
         | None -> match check_caseless "set-cookie" H_set_cookie with Some v -> Some v
         | None -> check_caseless "user-agent" H_user_agent)
      | 12 ->
        (match check_caseless "content-type" H_content_type with Some v -> Some v
         | None -> check_caseless "x-request-id" H_x_request_id)
      | 13 ->
        (match check_caseless "authorization" H_authorization with Some v -> Some v
         | None -> match check_caseless "cache-control" H_cache_control with Some v -> Some v
         | None -> match check_caseless "content-range" H_content_range with Some v -> Some v
         | None -> match check_caseless "if-none-match" H_if_none_match with Some v -> Some v
         | None -> check_caseless "last-modified" H_last_modified)
      | 14 ->
        (match check_caseless "accept-charset" H_accept_charset with Some v -> Some v
         | None -> check_caseless "content-length" H_content_length)
      | 15 ->
        (match check_caseless "accept-encoding" H_accept_encoding with Some v -> Some v
         | None -> match check_caseless "accept-language" H_accept_language with Some v -> Some v
         | None -> match check_caseless "x-forwarded-for" H_x_forwarded_for with Some v -> Some v
         | None -> check_caseless "x-correlation-id" H_x_correlation_id)
      | 16 ->
        (match check_caseless "content-encoding" H_content_encoding with Some v -> Some v
         | None -> match check_caseless "content-language" H_content_language with Some v -> Some v
         | None -> match check_caseless "content-location" H_content_location with Some v -> Some v
         | None -> match check_caseless "www-authenticate" H_www_authenticate with Some v -> Some v
         | None -> check_caseless "x-forwarded-host" H_x_forwarded_host)
      | 17 ->
        (match check_caseless "if-modified-since" H_if_modified_since with Some v -> Some v
         | None -> match check_caseless "transfer-encoding" H_transfer_encoding with Some v -> Some v
         | None -> check_caseless "x-forwarded-proto" H_x_forwarded_proto)
      | 19 ->
        (match check_caseless "content-disposition" H_content_disposition with Some v -> Some v
         | None -> check_caseless "if-unmodified-since" H_if_unmodified_since)
      | _ -> None
    in
    match try_known () with
    | Some v -> Some v
    | None ->
      (* Search unknown headers *)
      let name_lower = String.lowercase_ascii name in
      let rec loop = function
        | [] -> None
        | (sp, entry) :: rest ->
          if Zbuf.span_equal_caseless zb sp name_lower then Some entry.value
          else loop rest
      in
      loop t.unknown

  (** Iterate over all headers in order (known first, then unknown) *)
  let iter f t =
    IntMap.iter (fun _ (name, entries) ->
      List.iter (fun e -> f name e.name_span e.value) (List.rev entries)
    ) t.known;
    List.iter (fun (name_sp, e) ->
      f (H_other name_sp) e.name_span e.value
    ) (List.rev t.unknown)

  (** Fold over all headers *)
  let fold f t init =
    let acc = IntMap.fold (fun _ (name, entries) acc ->
      List.fold_left (fun acc e -> f acc name e.name_span e.value) acc (List.rev entries)
    ) t.known init in
    List.fold_left (fun acc (name_sp, e) ->
      f acc (H_other name_sp) e.name_span e.value
    ) acc (List.rev t.unknown)
end

(** Legacy header type for compatibility *)
type header = { name: header_name; name_span: span; value: span }

(** Parsed request - all strings are spans *)
type request = {
  meth: method_;
  target: span;
  version: version;
  headers: Headers.t;
}

(** Parsed response *)
type response = {
  version: version;
  status: int;
  reason: span;
  headers: Headers.t;
}

(** Parse errors *)
type error =
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_status
  | Invalid_header
  | Line_too_long
  | Too_many_headers

exception Parse_error of error

let error_to_string = function
  | Partial -> "Partial: need more data"
  | Invalid_method -> "Invalid HTTP method"
  | Invalid_target -> "Invalid request target"
  | Invalid_version -> "Invalid HTTP version"
  | Invalid_status -> "Invalid status code"
  | Invalid_header -> "Invalid header"
  | Line_too_long -> "Line too long"
  | Too_many_headers -> "Too many headers"

(* ============================================================
   Buffer Management
   ============================================================ *)

let default_size = 8192

(** Create a buffer (zbuf + cursor) *)
let create () =
  let zb = Zbuf.create () in
  let cur = Zbuf.cursor zb ~pos:0 in
  { zb; cur }

(** Push data to buffer *)
let push buf data ~off ~len =
  Zbuf.push buf.zb data ~off ~len

let with_refill buf refill f =
  let zbuf_refill _zb = refill buf in
  Zbuf.with_refill buf.zb buf.cur zbuf_refill f

(** Mark the start of a parse operation *)
let mark buf =
  Zbuf.cursor_mark buf.cur

(** Peek at byte at current position + offset.
    Auto-refills via effects, raises End_of_file on EOF. *)
let[@inline] peek buf off =
  Zbuf.peek buf.zb buf.cur off

(** Try to ensure n bytes available. Returns true if ok, false if EOF.
    Used for body reading where EOF is not an error. *)
let try_refill buf n =
  Zbuf.ensure buf.zb buf.cur n

(** Consume n bytes *)
let[@inline] consume buf n =
  Zbuf.advance buf.cur n

(* ============================================================
   Character Predicates (inlined for performance)
   ============================================================ *)

(** Token characters per RFC 7230 *)
let[@inline] is_tchar = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' -> true
  | '^' | '_' | '`' | '|' | '~' -> true
  | _ -> false

let[@inline] is_digit = function '0'..'9' -> true | _ -> false

let[@inline] is_space = function ' ' | '\t' -> true | _ -> false

let[@inline] is_vchar c = c > '\x20' && c < '\x7f'

(* ============================================================
   Span Operations (zero-copy)
   ============================================================ *)

(** Extract string from span - explicit materialization *)
let span_to_string (buf : buffer) (sp : span) : string =
  Zbuf.span_to_string buf.zb sp

(** Compare span to string without allocation *)
let span_equal (buf : buffer) (sp : span) (s : string) : bool =
  Zbuf.span_equal buf.zb sp s

(** Case-insensitive span comparison *)
let span_equal_caseless (buf : buffer) (sp : span) (s : string) : bool =
  Zbuf.span_equal_caseless buf.zb sp s

(** Iterate over span bytes *)
let span_iter f (buf : buffer) (sp : span) =
  Zbuf.span_iter f buf.zb sp

(** Fold over span bytes *)
let span_fold f (buf : buffer) (sp : span) init =
  Zbuf.span_fold f buf.zb sp init

(* ============================================================
   Header Name Parsing
   ============================================================ *)

(** Parse header name span into typed variant (case-insensitive) *)
let parse_header_name (buf : buffer) (sp : span) : header_name =
  (* Use length-based dispatch for efficiency *)
  match sp.len with
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
    else if span_equal_caseless buf sp "if-range" then H_if_match  (* Note: if-range is 8 chars *)
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

(** Convert header name to string *)
let header_name_to_string (buf : buffer) = function
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

(** Check if header name matches a variant *)
let header_name_equal name1 name2 =
  match name1, name2 with
  | H_other _, _ | _, H_other _ -> false  (* Other headers need span comparison *)
  | _ -> name1 = name2

(* ============================================================
   Low-level Parsing Helpers
   ============================================================ *)

(** Take bytes while predicate holds, return span. At least 1 byte required.
    Uses effect-based refilling via Zbuf.scan_while. *)
let take_while1 pred buf =
  Zbuf.cursor_mark buf.cur;
  ignore (Zbuf.scan_while pred buf.zb buf.cur);
  let sp = Zbuf.span_since_mark buf.cur in
  if sp.len = 0 then raise (Parse_error Partial);
  sp

(** Take bytes while predicate holds, return span. Zero bytes allowed. *)
let take_while pred buf =
  Zbuf.cursor_mark buf.cur;
  ignore (Zbuf.scan_while pred buf.zb buf.cur);
  Zbuf.span_since_mark buf.cur

(** Skip optional whitespace (OWS = *( SP / HTAB )) *)
let skip_ows buf =
  ignore (Zbuf.scan_while is_space buf.zb buf.cur)

(** Expect and consume CRLF *)
let expect_crlf buf =
  if peek buf 0 = '\r' && peek buf 1 = '\n' then
    consume buf 2
  else
    raise (Parse_error Invalid_header)

(** Find CRLF from current position, returns offset from cursor or raises *)
let find_crlf buf =
  let rec scan off =
    if peek buf off = '\r' && peek buf (off + 1) = '\n' then
      off
    else
      scan (off + 1)
  in
  scan 0

(* ============================================================
   Request Parsing
   ============================================================ *)

(** Parse HTTP method *)
let parse_method buf =
  let sp = take_while1 is_tchar buf in
  (* Match common methods without allocation *)
  if sp.len = 3 then begin
    if span_equal buf sp "GET" then GET
    else if span_equal buf sp "PUT" then PUT
    else Other sp
  end else if sp.len = 4 then begin
    if span_equal buf sp "POST" then POST
    else if span_equal buf sp "HEAD" then HEAD
    else Other sp
  end else if sp.len = 5 then begin
    if span_equal buf sp "PATCH" then PATCH
    else if span_equal buf sp "TRACE" then TRACE
    else Other sp
  end else if sp.len = 6 then begin
    if span_equal buf sp "DELETE" then DELETE
    else Other sp
  end else if sp.len = 7 then begin
    if span_equal buf sp "OPTIONS" then OPTIONS
    else if span_equal buf sp "CONNECT" then CONNECT
    else Other sp
  end else
    Other sp

(** Parse request target (until SP) *)
let parse_target buf =
  Zbuf.cursor_mark buf.cur;
  let rec scan () =
    let c = peek buf 0 in
    if c = ' ' || c = '\r' then ()
    else begin
      consume buf 1;
      scan ()
    end
  in
  scan ();
  let sp = Zbuf.span_since_mark buf.cur in
  if sp.len = 0 then raise (Parse_error Invalid_target);
  sp

(** Parse HTTP version (HTTP/1.0 or HTTP/1.1) *)
let parse_version buf =
  if peek buf 0 = 'H' && peek buf 1 = 'T' && peek buf 2 = 'T' &&
     peek buf 3 = 'P' && peek buf 4 = '/' && peek buf 5 = '1' &&
     peek buf 6 = '.' then
    let v = peek buf 7 in
    consume buf 8;
    if v = '1' then HTTP_1_1
    else if v = '0' then HTTP_1_0
    else raise (Parse_error Invalid_version)
  else
    raise (Parse_error Invalid_version)

(** Parse request line: METHOD SP request-target SP HTTP-version CRLF *)
let parse_request_line buf =
  let meth = parse_method buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_method);
  consume buf 1;
  let target = parse_target buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_target);
  consume buf 1;
  let version = parse_version buf in
  expect_crlf buf;
  (meth, target, version)

(* ============================================================
   Header Parsing
   ============================================================ *)

(** Parse a single header, returns None at end of headers *)
let parse_header buf =
  (* Check for empty line (end of headers) *)
  if peek buf 0 = '\r' && peek buf 1 = '\n' then begin
    consume buf 2;
    None
  end else begin
    (* Parse header name (token) *)
    let name_span = take_while1 is_tchar buf in
    let name = parse_header_name buf name_span in
    (* Expect colon *)
    if peek buf 0 <> ':' then raise (Parse_error Invalid_header);
    consume buf 1;
    (* Skip OWS *)
    skip_ows buf;
    (* Find end of value (CRLF) and mark start *)
    let value_start = Zbuf.cursor_pos buf.cur in
    let crlf_off = find_crlf buf in
    (* Trim trailing OWS from value - data already in buffer from find_crlf *)
    let rec trim_end off =
      if off <= 0 then 0
      else if is_space (peek buf (off - 1)) then trim_end (off - 1)
      else off
    in
    let value_len = trim_end crlf_off in
    let value = { start = value_start; len = value_len } in
    consume buf (crlf_off + 2);  (* Skip value + CRLF *)
    Some { name; name_span; value }
  end

(** Empty header for array initialization *)
let empty_header = { name = H_other { start = 0; len = 0 }; name_span = { start = 0; len = 0 }; value = { start = 0; len = 0 } }

(** Parse all headers into Headers.t map *)
let parse_headers ?(max_headers = 100) buf =
  let rec loop hdrs count =
    if count >= max_headers then raise (Parse_error Too_many_headers)
    else match parse_header buf with
      | None -> hdrs
      | Some h ->
        let hdrs = Headers.add hdrs h.name ~name_span:h.name_span ~value:h.value in
        loop hdrs (count + 1)
  in
  loop Headers.empty 0

(** Parse complete HTTP request *)
let parse_request ?(max_headers = 100) buf =
  let meth, target, version = parse_request_line buf in
  let headers = parse_headers ~max_headers buf in
  { meth; target; version; headers }

(* ============================================================
   Response Parsing
   ============================================================ *)

(** Parse status code (3 digits) *)
let parse_status buf =
  let d1 = Char.code (peek buf 0) - 48 in
  let d2 = Char.code (peek buf 1) - 48 in
  let d3 = Char.code (peek buf 2) - 48 in
  if d1 < 1 || d1 > 9 || d2 < 0 || d2 > 9 || d3 < 0 || d3 > 9 then
    raise (Parse_error Invalid_status);
  consume buf 3;
  d1 * 100 + d2 * 10 + d3

(** Parse reason phrase (until CRLF) *)
let parse_reason buf =
  let start = Zbuf.cursor_pos buf.cur in
  let crlf_off = find_crlf buf in
  let sp = { start; len = crlf_off } in
  consume buf (crlf_off + 2);
  sp

(** Parse status line: HTTP-version SP status-code SP reason-phrase CRLF *)
let parse_status_line buf =
  let version = parse_version buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_version);
  consume buf 1;
  let status = parse_status buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_status);
  consume buf 1;
  let reason = parse_reason buf in
  (version, status, reason)

(** Parse complete HTTP response *)
let parse_response ?(max_headers = 100) buf =
  let version, status, reason = parse_status_line buf in
  let headers = parse_headers ~max_headers buf in
  { version; status; reason; headers }

(* ============================================================
   Materialization Functions
   ============================================================ *)

(** Get method as string *)
let method_to_string buf = function
  | GET -> "GET"
  | POST -> "POST"
  | HEAD -> "HEAD"
  | PUT -> "PUT"
  | DELETE -> "DELETE"
  | PATCH -> "PATCH"
  | OPTIONS -> "OPTIONS"
  | TRACE -> "TRACE"
  | CONNECT -> "CONNECT"
  | Other sp -> span_to_string buf sp

(** Get version as string *)
let version_to_string = function
  | HTTP_1_0 -> "HTTP/1.0"
  | HTTP_1_1 -> "HTTP/1.1"

(** Find header by header_name variant, return value span. O(log n) for known headers. *)
let find_header (_buf : buffer) (req : request) (target : header_name) : span option =
  Headers.find_known req.headers target

(** Find header by string name (case-insensitive), return value span *)
let find_header_span (buf : buffer) (req : request) (name : string) : span option =
  Headers.find_by_string buf.zb req.headers name

(** Find header by variant and materialize value *)
let get_header_by_name (buf : buffer) (req : request) (target : header_name) : string option =
  Option.map (span_to_string buf) (find_header buf req target)

(** Find header by string and materialize value *)
let get_header (buf : buffer) (req : request) (name : string) : string option =
  Option.map (span_to_string buf) (find_header_span buf req name)

(** Get all headers matching variant name *)
let get_headers_by_name (buf : buffer) (req : request) (target : header_name) : string list =
  List.map (span_to_string buf) (Headers.find_all_known req.headers target)

(** Get all headers matching string name *)
let get_headers (buf : buffer) (req : request) (name : string) : string list =
  (* For now, use find_header_span for first value, then check if it's a multi-value header *)
  match find_header_span buf req name with
  | None -> []
  | Some sp -> [span_to_string buf sp]
  (* TODO: implement proper multi-value lookup for string names *)

(** Get content-length if present (uses variant for efficiency) *)
let content_length (buf : buffer) (req : request) : int64 option =
  Option.bind (find_header buf req H_content_length)
    (fun sp -> Int64.of_string_opt (span_to_string buf sp))

(** Check if transfer-encoding is chunked (uses variant for efficiency) *)
let is_chunked (buf : buffer) (req : request) : bool =
  find_header buf req H_transfer_encoding
  |> Option.map (fun sp -> span_equal_caseless buf sp "chunked")
  |> Option.value ~default:false

(** Check connection header (uses variant for efficiency) *)
let is_keep_alive (buf : buffer) (req : request) : bool =
  match find_header buf req H_connection with
  | None -> req.version = HTTP_1_1  (* HTTP/1.1 defaults to keep-alive *)
  | Some sp ->
    if span_equal_caseless buf sp "close" then false
    else if span_equal_caseless buf sp "keep-alive" then true
    else req.version = HTTP_1_1

(* Response helpers *)

(** Find response header by variant *)
let find_header_resp (_buf : buffer) (resp : response) (target : header_name) : span option =
  Headers.find_known resp.headers target

(** Find response header by string name *)
let find_header_span_resp buf resp name =
  Headers.find_by_string buf.zb resp.headers name

let get_header_resp buf resp name =
  Option.map (span_to_string buf) (find_header_span_resp buf resp name)

let content_length_resp buf resp =
  Option.bind (find_header_resp buf resp H_content_length)
    (fun sp -> Int64.of_string_opt (span_to_string buf sp))

let is_chunked_resp buf resp =
  find_header_resp buf resp H_transfer_encoding
  |> Option.map (fun sp -> span_equal_caseless buf sp "chunked")
  |> Option.value ~default:false

(* ============================================================
   Serialization
   ============================================================ *)

(** Output abstraction - either a span from source buffer or a string *)
type out =
  | Span of buffer * span
  | Str of string

(** Convenience constructors *)
let str s = Str s
let out_span buf sp = Span (buf, sp)

(** Write out value to bytes, returns number of bytes written *)
let write_out dst ~pos = function
  | Str s ->
    let len = String.length s in
    Bytes.blit_string s 0 dst pos len;
    len
  | Span (buf, sp) ->
    Zbuf.span_blit buf.zb sp dst ~dst_off:pos;
    sp.len

(** Write CRLF *)
let write_crlf dst ~pos =
  Bytes.set dst pos '\r';
  Bytes.set dst (pos + 1) '\n';
  2

(** Write SP *)
let write_sp dst ~pos =
  Bytes.set dst pos ' ';
  1

(** Write ": " *)
let write_colon_sp dst ~pos =
  Bytes.set dst pos ':';
  Bytes.set dst (pos + 1) ' ';
  2

(** Write method *)
let write_method dst ~pos = function
  | GET -> Bytes.blit_string "GET" 0 dst pos 3; 3
  | POST -> Bytes.blit_string "POST" 0 dst pos 4; 4
  | HEAD -> Bytes.blit_string "HEAD" 0 dst pos 4; 4
  | PUT -> Bytes.blit_string "PUT" 0 dst pos 3; 3
  | DELETE -> Bytes.blit_string "DELETE" 0 dst pos 6; 6
  | PATCH -> Bytes.blit_string "PATCH" 0 dst pos 5; 5
  | OPTIONS -> Bytes.blit_string "OPTIONS" 0 dst pos 7; 7
  | TRACE -> Bytes.blit_string "TRACE" 0 dst pos 5; 5
  | CONNECT -> Bytes.blit_string "CONNECT" 0 dst pos 7; 7
  | Other _span -> failwith "Cannot write Other method without buffer"

(** Write method from parsed request (can handle Other) *)
let write_method_from ~src dst ~pos = function
  | Other sp ->
    Zbuf.span_blit src.zb sp dst ~dst_off:pos;
    sp.len
  | m -> write_method dst ~pos m

(** Write version *)
let write_version dst ~pos = function
  | HTTP_1_0 -> Bytes.blit_string "HTTP/1.0" 0 dst pos 8; 8
  | HTTP_1_1 -> Bytes.blit_string "HTTP/1.1" 0 dst pos 8; 8

(** Write status code *)
let write_status_code dst ~pos status =
  Bytes.set dst pos (Char.chr (48 + status / 100));
  Bytes.set dst (pos + 1) (Char.chr (48 + (status / 10) mod 10));
  Bytes.set dst (pos + 2) (Char.chr (48 + status mod 10));
  3

(** Write request line *)
let write_request_line dst ~pos ~method_ ~target ~version =
  let p = pos in
  let p = p + write_out dst ~pos:p method_ in
  let p = p + write_sp dst ~pos:p in
  let p = p + write_out dst ~pos:p target in
  let p = p + write_sp dst ~pos:p in
  let p = p + write_version dst ~pos:p version in
  let p = p + write_crlf dst ~pos:p in
  p - pos

(** Write status line *)
let write_status_line dst ~pos ~version ~status ~reason =
  let p = pos in
  let p = p + write_version dst ~pos:p version in
  let p = p + write_sp dst ~pos:p in
  let p = p + write_status_code dst ~pos:p status in
  let p = p + write_sp dst ~pos:p in
  let p = p + write_out dst ~pos:p reason in
  let p = p + write_crlf dst ~pos:p in
  p - pos

(** Write a single header *)
let write_header dst ~pos ~name ~value =
  let p = pos in
  let p = p + write_out dst ~pos:p name in
  let p = p + write_colon_sp dst ~pos:p in
  let p = p + write_out dst ~pos:p value in
  let p = p + write_crlf dst ~pos:p in
  p - pos

(** Write headers from list, plus final CRLF *)
let write_headers dst ~pos headers =
  let p = List.fold_left (fun p (name, value) ->
      p + write_header dst ~pos:p ~name ~value
    ) pos headers
  in
  let p = p + write_crlf dst ~pos:p in
  p - pos

(** Write complete request *)
let write_request dst ~pos ~method_ ~target ~version ~headers =
  let p = pos in
  let p = p + write_request_line dst ~pos:p ~method_ ~target ~version in
  let p = p + write_headers dst ~pos:p headers in
  p - pos

(** Write complete response *)
let write_response dst ~pos ~version ~status ~reason ~headers =
  let p = pos in
  let p = p + write_status_line dst ~pos:p ~version ~status ~reason in
  let p = p + write_headers dst ~pos:p headers in
  p - pos

(** Write chunk header (size in hex + CRLF) *)
let write_chunk_header dst ~pos size =
  let hex = Printf.sprintf "%x" size in
  let len = String.length hex in
  Bytes.blit_string hex 0 dst pos len;
  let p = pos + len in
  let p = p + write_crlf dst ~pos:p in
  p - pos

(** Write chunk trailer (CRLF after chunk data) *)
let write_chunk_trailer dst ~pos =
  write_crlf dst ~pos

(** Write final chunk (0\r\n\r\n) *)
let write_final_chunk dst ~pos =
  Bytes.blit_string "0\r\n\r\n" 0 dst pos 5;
  5

(* ============================================================
   Result-based API wrappers
   ============================================================ *)

let try_parse_request ?max_headers buf =
  try Ok (parse_request ?max_headers buf)
  with
  | Parse_error e -> Error e
  | End_of_file -> Error Partial

let try_parse_response ?max_headers buf =
  try Ok (parse_response ?max_headers buf)
  with
  | Parse_error e -> Error e
  | End_of_file -> Error Partial

(* ============================================================
   Body Reading
   ============================================================ *)

(** Transfer encoding detection *)
type transfer =
  | Fixed of int64
  | Chunked
  | Close_delimited
  | No_body

(** Determine transfer encoding from request *)
let transfer_of_request (buf : buffer) (req : request) : transfer =
  if is_chunked buf req then Chunked
  else match content_length buf req with
    | Some 0L -> No_body
    | Some len -> Fixed len
    | None -> No_body  (* Requests without Content-Length have no body *)

(** Determine transfer encoding from response *)
let transfer_of_response (buf : buffer) (resp : response) : transfer =
  (* 1xx, 204, 304 have no body *)
  if resp.status < 200 || resp.status = 204 || resp.status = 304 then No_body
  else if is_chunked_resp buf resp then Chunked
  else match content_length_resp buf resp with
    | Some 0L -> No_body
    | Some len -> Fixed len
    | None -> Close_delimited  (* Responses may use close-delimited *)

(** Body reader state *)
type body_reader =
  | Fixed_body of { mutable remaining: int64 }
  | Chunked_body of {
      mutable chunk_remaining: int64;
      mutable done_: bool;
      mutable trailers: Headers.t option;
    }
  | Close_body
  | Empty_body

(** Create a body reader from transfer encoding *)
let body_reader_of_transfer = function
  | Fixed len -> Fixed_body { remaining = len }
  | Chunked -> Chunked_body { chunk_remaining = 0L; done_ = false; trailers = None }
  | Close_delimited -> Close_body
  | No_body -> Empty_body

(** Create body reader for request *)
let body_reader_of_request buf req =
  body_reader_of_transfer (transfer_of_request buf req)

(** Create body reader for response *)
let body_reader_of_response buf resp =
  body_reader_of_transfer (transfer_of_response buf resp)

(** Parse hex chunk size *)
let parse_chunk_size buf =
  let rec parse_hex acc =
    let c = peek buf 0 in
    let digit = match c with
      | '0'..'9' -> Some (Char.code c - 48)
      | 'a'..'f' -> Some (Char.code c - 87)
      | 'A'..'F' -> Some (Char.code c - 55)
      | _ -> None
    in
    match digit with
    | Some d ->
      consume buf 1;
      parse_hex (Int64.add (Int64.shift_left acc 4) (Int64.of_int d))
    | None -> acc
  in
  let size = parse_hex 0L in
  (* Skip chunk extensions (;name=value) until CRLF *)
  let rec skip_to_crlf () =
    match peek buf 0 with
    | '\r' ->
      if peek buf 1 = '\n' then
        consume buf 2
      else begin
        consume buf 1;
        skip_to_crlf ()
      end
    | _ ->
      consume buf 1;
      skip_to_crlf ()
  in
  skip_to_crlf ();
  size

(** Read body data into destination buffer.
    Returns number of bytes read, or 0 for EOF.
    This copies data from the internal buffer to dst. *)
let rec body_read (buf : buffer) (reader : body_reader) (dst : bytes) ~(off : int) ~(len : int) : int =
  match reader with
  | Empty_body -> 0

  | Fixed_body state ->
    if state.remaining = 0L then 0
    else begin
      let want = min len (Int64.to_int (min state.remaining (Int64.of_int max_int))) in
      let available = Zbuf.cursor_remaining buf.zb buf.cur in
      if available = 0 then begin
        if try_refill buf 1 then body_read buf reader dst ~off ~len
        else 0
      end else begin
        let to_read = min want available in
        (* Read bytes from zbuf into dst *)
        let src_pos = Zbuf.cursor_pos buf.cur in
        let sp = { start = src_pos; len = to_read } in
        Zbuf.span_blit buf.zb sp dst ~dst_off:off;
        consume buf to_read;
        state.remaining <- Int64.sub state.remaining (Int64.of_int to_read);
        to_read
      end
    end

  | Close_body ->
    let available = Zbuf.cursor_remaining buf.zb buf.cur in
    if available = 0 then begin
      if try_refill buf 1 then body_read buf reader dst ~off ~len
      else 0
    end else begin
      let to_read = min len available in
      let src_pos = Zbuf.cursor_pos buf.cur in
      let sp = { start = src_pos; len = to_read } in
      Zbuf.span_blit buf.zb sp dst ~dst_off:off;
      consume buf to_read;
      to_read
    end

  | Chunked_body state ->
    if state.done_ && state.chunk_remaining = 0L then 0
    else begin
      (* Need to read next chunk header? *)
      if state.chunk_remaining = 0L && not state.done_ then begin
        let size = parse_chunk_size buf in
        if size = 0L then begin
          (* Final chunk - parse trailers *)
          state.done_ <- true;
          let trailers = parse_headers buf in
          state.trailers <- Some trailers;
          0
        end else begin
          state.chunk_remaining <- size;
          body_read buf reader dst ~off ~len
        end
      end else begin
        let want = min len (Int64.to_int (min state.chunk_remaining (Int64.of_int max_int))) in
        let available = Zbuf.cursor_remaining buf.zb buf.cur in
        if available = 0 then begin
          if try_refill buf 1 then body_read buf reader dst ~off ~len
          else 0
        end else begin
          let to_read = min want available in
          let src_pos = Zbuf.cursor_pos buf.cur in
          let sp = { start = src_pos; len = to_read } in
          Zbuf.span_blit buf.zb sp dst ~dst_off:off;
          consume buf to_read;
          state.chunk_remaining <- Int64.sub state.chunk_remaining (Int64.of_int to_read);
          (* If chunk is done, consume trailing CRLF *)
          if state.chunk_remaining = 0L && not state.done_ then begin
            if try_refill buf 2 && peek buf 0 = '\r' && peek buf 1 = '\n' then
              consume buf 2
          end;
          to_read
        end
      end
    end

(** Read body as a span into the buffer (zero-copy for small bodies).
    Only works for Fixed-length bodies that fit in the buffer.
    Returns None if body is too large or not fixed-length. *)
let body_read_span (buf : buffer) (reader : body_reader) : span option =
  match reader with
  | Empty_body -> Some { start = Zbuf.cursor_pos buf.cur; len = 0 }
  | Fixed_body state ->
    let len = Int64.to_int state.remaining in
    if len > 1024 * 1024 then None  (* Too large for single span *)
    else if not (try_refill buf len) then None
    else begin
      let start = Zbuf.cursor_pos buf.cur in
      let sp = { start; len } in
      consume buf len;
      state.remaining <- 0L;
      Some sp
    end
  | Chunked_body _ -> None  (* Can't do zero-copy for chunked *)
  | Close_body -> None  (* Can't know size upfront *)

(** Read entire body as string *)
let body_read_string (buf : buffer) (reader : body_reader) : string =
  match reader with
  | Empty_body -> ""
  | Fixed_body state when state.remaining <= 65536L ->
    (* Small fixed body - use span *)
    (match body_read_span buf reader with
     | Some sp -> span_to_string buf sp
     | None -> "")
  | _ ->
    (* Large or streaming body - accumulate *)
    let result = Buffer.create 4096 in
    let chunk = Bytes.create 4096 in
    let rec loop () =
      let n = body_read buf reader chunk ~off:0 ~len:4096 in
      if n > 0 then begin
        Buffer.add_subbytes result chunk 0 n;
        loop ()
      end
    in
    loop ();
    Buffer.contents result

(** Drain body without reading content *)
let body_drain (buf : buffer) (reader : body_reader) : unit =
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let n = body_read buf reader chunk ~off:0 ~len:4096 in
    if n > 0 then loop ()
  in
  loop ()

(** Check if body is fully read *)
let body_is_done (reader : body_reader) : bool =
  match reader with
  | Empty_body -> true
  | Fixed_body state -> state.remaining = 0L
  | Chunked_body state -> state.done_ && state.chunk_remaining = 0L
  | Close_body -> false  (* Can't know until EOF *)

(** Get trailers from chunked body (only available after body is fully read) *)
let body_trailers (reader : body_reader) : Headers.t option =
  match reader with
  | Chunked_body { done_ = true; trailers = Some t; _ } -> Some t
  | _ -> None
