(* httpl.ml - Zero-copy HTTP/1.1 parser and serializer using Zbuf

   OxCaml-optimized design:
   - Unboxed span type (no allocation for token boundaries)
   - Local cursor support where appropriate
   - let mutable for efficient loops
*)

(* ============================================================
   Core Types
   ============================================================ *)

(** Re-export unboxed span from Zbuf - used in hot paths, zero allocation *)
type span = Zbuf.span = #{ start : Zbuf.pos; len : int }

(** Boxed span for use in option/list containers (which require layout value) *)
type boxed_span = { bs_start : Zbuf.pos; bs_len : int }

let box_span (sp : span) : boxed_span = { bs_start = sp.#start; bs_len = sp.#len }
let unbox_span (bs : boxed_span) : span = #{ start = bs.bs_start; len = bs.bs_len }

(** Parser state combining zbuf and cursor.
    Uses cursor_global since buffer is stored in a record. *)
type buffer = {
  zb : Zbuf.t;
  cur : Zbuf.cursor;
}

(** Refill function: push data to buffer, return true if data added, false for EOF. *)
type refill = buffer -> bool

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

(** A header entry with name span and value span.
    Boxed record with unboxed span fields (mixed block). *)
type header_entry = {
  name_span_start : Zbuf.pos;
  name_span_len : int;
  value_start : Zbuf.pos;
  value_len : int;
}

(** Construct header_entry from spans *)
let make_header_entry ~name_span ~value =
  { name_span_start = name_span.#start;
    name_span_len = name_span.#len;
    value_start = value.#start;
    value_len = value.#len }

(** Get name span from header entry *)
let header_entry_name_span e : span = #{ start = e.name_span_start; len = e.name_span_len }

(** Get value span from header entry *)
let header_entry_value e : span = #{ start = e.value_start; len = e.value_len }

(** Unknown header entry: name span (as boxed record) + header_entry *)
type unknown_header = {
  unk_name_start : Zbuf.pos;
  unk_name_len : int;
  unk_entry : header_entry;
}

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
    known : (header_name * header_entry list) IntMap.t;
    unknown : unknown_header list;
    count : int;
  }

  let empty = { known = IntMap.empty; unknown = []; count = 0 }

  let count t = t.count

  (** Add a header. For known headers, appends to value list. *)
  let add t name ~name_span ~value =
    let entry = make_header_entry ~name_span ~value in
    let new_count = t.count + 1 in
    let key = key_of_name name in
    if key >= 0 then begin
      let entries = match IntMap.find_opt key t.known with
        | None -> (name, [entry])
        | Some (n, es) -> (n, entry :: es)
      in
      { known = IntMap.add key entries t.known; unknown = t.unknown; count = new_count }
    end else begin
      let name_sp = match name with H_other sp -> sp | _ -> assert false in
      let unk = { unk_name_start = name_sp.#start;
                  unk_name_len = name_sp.#len;
                  unk_entry = entry } in
      { known = t.known; unknown = unk :: t.unknown; count = new_count }
    end

  (** Find first matching header by known name. O(log n).
      Returns boxed span since option requires layout value. *)
  let find_known t name : boxed_span option =
    let key = key_of_name name in
    if key < 0 then None
    else match IntMap.find_opt key t.known with
      | None -> None
      | Some (_, entries) ->
        match entries with [] -> None | e :: _ -> Some (box_span (header_entry_value e))

  (** Find all values for a known header. Returns in order added.
      Returns boxed spans since list requires layout value. *)
  let find_all_known t name : boxed_span list =
    let key = key_of_name name in
    if key < 0 then []
    else match IntMap.find_opt key t.known with
      | None -> []
      | Some (_, entries) -> List.rev_map (fun e -> box_span (header_entry_value e)) entries

  (** Find header by name span (for unknown headers). Needs zbuf for comparison.
      Returns boxed span since option requires layout value. *)
  let find_unknown zb t name_span : boxed_span option =
    let rec loop = function
      | [] -> None
      | unk :: rest ->
        let sp : span = #{ start = unk.unk_name_start; len = unk.unk_name_len } in
        if Zbuf.span_equal_caseless zb sp (Zbuf.span_to_string zb name_span) then
          Some (box_span (header_entry_value unk.unk_entry))
        else loop rest
    in
    loop t.unknown

  (** Find header by string name. Checks known first, then unknown. *)
  let find_by_string zb t name =
    let len = String.length name in
    let try_known () =
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
      let name_lower = String.lowercase_ascii name in
      let rec loop = function
        | [] -> None
        | unk :: rest ->
          let sp : span = #{ start = unk.unk_name_start; len = unk.unk_name_len } in
          if Zbuf.span_equal_caseless zb sp name_lower then
            Some (box_span (header_entry_value unk.unk_entry))
          else loop rest
      in
      loop t.unknown

  (** Iterate over all headers in order (known first, then unknown) *)
  let iter f t =
    IntMap.iter (fun _ (name, entries) ->
      List.iter (fun e ->
        f name (header_entry_name_span e) (header_entry_value e)
      ) (List.rev entries)
    ) t.known;
    List.iter (fun unk ->
      let name_sp : span = #{ start = unk.unk_name_start; len = unk.unk_name_len } in
      let e = unk.unk_entry in
      f (H_other name_sp) (header_entry_name_span e) (header_entry_value e)
    ) (List.rev t.unknown)

  (** Fold over all headers *)
  let fold f t init =
    let acc = IntMap.fold (fun _ (name, entries) acc ->
      List.fold_left (fun acc e ->
        f acc name (header_entry_name_span e) (header_entry_value e)
      ) acc (List.rev entries)
    ) t.known init in
    List.fold_left (fun acc unk ->
      let name_sp : span = #{ start = unk.unk_name_start; len = unk.unk_name_len } in
      let e = unk.unk_entry in
      f acc (H_other name_sp) (header_entry_name_span e) (header_entry_value e)
    ) acc (List.rev t.unknown)
end

(** Legacy header type for compatibility *)
type header = { name : header_name; name_span : span; value : span }

(** Parsed request - uses boxed span storage for layout compatibility *)
type request = {
  meth : method_;
  target_start : Zbuf.pos;
  target_len : int;
  version : version;
  headers : Headers.t;
}

(** Get target span from request *)
let request_target r : span = #{ start = r.target_start; len = r.target_len }

(** Parsed response - uses boxed span storage for layout compatibility *)
type response = {
  version : version;
  status : int;
  reason_start : Zbuf.pos;
  reason_len : int;
  headers : Headers.t;
}

(** Get reason span from response *)
let response_reason r : span = #{ start = r.reason_start; len = r.reason_len }

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

(** Create a buffer (zbuf + cursor).
    Uses cursor_global since buffer is stored in a record. *)
let create () =
  let zb = Zbuf.create () in
  let cur = Zbuf.cursor_global zb ~pos:0 in
  { zb; cur }

(** Push data to buffer *)
let push buf data ~off ~len =
  Zbuf.push buf.zb data ~off ~len

(** Run with refill handler.
    Adapts buffer-based refill to zbuf's t-based refill. *)
let with_refill buf refill f =
  let zbuf_refill zb =
    (* Create a temporary buffer wrapper for the refill callback *)
    ignore zb;  (* We use buf.zb which is the same *)
    refill buf
  in
  Zbuf.with_refill buf.zb zbuf_refill f

(** Mark the start of a parse operation *)
let mark buf =
  Zbuf.cursor_mark buf.cur

(** Peek at byte at current position + offset.
    Auto-refills via effects, raises End_of_file on EOF. *)
let[@inline] peek buf off =
  Zbuf.peek buf.zb buf.cur off

(** Try to ensure n bytes available. Returns true if ok, false if EOF. *)
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
    else if span_equal_caseless buf sp "if-range" then H_if_match
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
  | H_other _, _ | _, H_other _ -> false
  | _ -> name1 = name2

(* ============================================================
   Low-level Parsing Helpers
   ============================================================ *)

(** Take bytes while predicate holds, return span. At least 1 byte required. *)
let take_while1 pred buf =
  Zbuf.cursor_mark buf.cur;
  ignore (Zbuf.scan_while pred buf.zb buf.cur);
  let sp = Zbuf.span_since_mark buf.cur in
  if sp.#len = 0 then raise (Parse_error Partial);
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
  let mutable off = 0 in
  let mutable found = false in
  while not found do
    if peek buf off = '\r' && peek buf (off + 1) = '\n' then
      found <- true
    else
      off <- off + 1
  done;
  off

(* ============================================================
   Request Parsing
   ============================================================ *)

(** Parse HTTP method *)
let parse_method buf =
  let sp = take_while1 is_tchar buf in
  match sp.#len with
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

(** Parse request target (until SP) *)
let parse_target buf =
  Zbuf.cursor_mark buf.cur;
  let mutable scanning = true in
  while scanning do
    let c = peek buf 0 in
    if c = ' ' || c = '\r' then scanning <- false
    else consume buf 1
  done;
  let sp = Zbuf.span_since_mark buf.cur in
  if sp.#len = 0 then raise (Parse_error Invalid_target);
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

(** Result of parsing a request line - record to avoid tuple layout constraints *)
type request_line = {
  rl_meth : method_;
  rl_target_start : Zbuf.pos;
  rl_target_len : int;
  rl_version : version;
}

(** Parse request line: METHOD SP request-target SP HTTP-version CRLF *)
let parse_request_line buf : request_line =
  let meth = parse_method buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_method);
  consume buf 1;
  let target = parse_target buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_target);
  consume buf 1;
  let version = parse_version buf in
  expect_crlf buf;
  { rl_meth = meth;
    rl_target_start = target.#start;
    rl_target_len = target.#len;
    rl_version = version }

(* ============================================================
   Header Parsing
   ============================================================ *)

(** Parse a single header, returns None at end of headers *)
let parse_header buf =
  if peek buf 0 = '\r' && peek buf 1 = '\n' then begin
    consume buf 2;
    None
  end else begin
    let name_span = take_while1 is_tchar buf in
    let name = parse_header_name buf name_span in
    if peek buf 0 <> ':' then raise (Parse_error Invalid_header);
    consume buf 1;
    skip_ows buf;
    let value_start = Zbuf.cursor_pos buf.cur in
    let crlf_off = find_crlf buf in
    (* Trim trailing OWS from value using let mutable *)
    let mutable end_off = crlf_off in
    while end_off > 0 && is_space (peek buf (end_off - 1)) do
      end_off <- end_off - 1
    done;
    let value = #{ start = value_start; len = end_off } in
    consume buf (crlf_off + 2);
    Some { name; name_span; value }
  end

(** Empty header for array initialization *)
let empty_header = {
  name = H_other #{ start = 0; len = 0 };
  name_span = #{ start = 0; len = 0 };
  value = #{ start = 0; len = 0 }
}

(** Parse all headers into Headers.t map *)
let parse_headers ?(max_headers = 100) buf =
  let mutable hdrs = Headers.empty in
  let mutable count = 0 in
  let mutable done_ = false in
  while not done_ do
    if count >= max_headers then raise (Parse_error Too_many_headers);
    match parse_header buf with
    | None -> done_ <- true
    | Some h ->
      hdrs <- Headers.add hdrs h.name ~name_span:h.name_span ~value:h.value;
      count <- count + 1
  done;
  hdrs

(** Parse complete HTTP request *)
let parse_request ?(max_headers = 100) buf =
  let rl = parse_request_line buf in
  let headers = parse_headers ~max_headers buf in
  { meth = rl.rl_meth;
    target_start = rl.rl_target_start;
    target_len = rl.rl_target_len;
    version = rl.rl_version;
    headers }

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
  let sp = #{ start; len = crlf_off } in
  consume buf (crlf_off + 2);
  sp

(** Result of parsing a status line - record to avoid tuple layout constraints *)
type status_line = {
  sl_version : version;
  sl_status : int;
  sl_reason_start : Zbuf.pos;
  sl_reason_len : int;
}

(** Parse status line: HTTP-version SP status-code SP reason-phrase CRLF *)
let parse_status_line buf : status_line =
  let version = parse_version buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_version);
  consume buf 1;
  let status = parse_status buf in
  if peek buf 0 <> ' ' then raise (Parse_error Invalid_status);
  consume buf 1;
  let reason = parse_reason buf in
  { sl_version = version;
    sl_status = status;
    sl_reason_start = reason.#start;
    sl_reason_len = reason.#len }

(** Parse complete HTTP response *)
let parse_response ?(max_headers = 100) buf =
  let sl = parse_status_line buf in
  let headers = parse_headers ~max_headers buf in
  { version = sl.sl_version;
    status = sl.sl_status;
    reason_start = sl.sl_reason_start;
    reason_len = sl.sl_reason_len;
    headers }

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

(** Find header by header_name variant, return boxed value span. O(log n) for known headers. *)
let find_header (_buf : buffer) (req : request) (target : header_name) : boxed_span option =
  Headers.find_known req.headers target

(** Find header by string name (case-insensitive), return boxed value span *)
let find_header_span (buf : buffer) (req : request) (name : string) : boxed_span option =
  Headers.find_by_string buf.zb req.headers name

(** Materialize a boxed span to string *)
let boxed_span_to_string (buf : buffer) (bs : boxed_span) : string =
  Zbuf.span_to_string buf.zb (unbox_span bs)

(** Find header by variant and materialize value *)
let get_header_by_name (buf : buffer) (req : request) (target : header_name) : string option =
  Option.map (boxed_span_to_string buf) (find_header buf req target)

(** Find header by string and materialize value *)
let get_header (buf : buffer) (req : request) (name : string) : string option =
  Option.map (boxed_span_to_string buf) (find_header_span buf req name)

(** Get all headers matching variant name *)
let get_headers_by_name (buf : buffer) (req : request) (target : header_name) : string list =
  List.map (boxed_span_to_string buf) (Headers.find_all_known req.headers target)

(** Get all headers matching string name *)
let get_headers (buf : buffer) (req : request) (name : string) : string list =
  match find_header_span buf req name with
  | None -> []
  | Some bs -> [boxed_span_to_string buf bs]

(** Get content-length if present *)
let content_length (buf : buffer) (req : request) : int64 option =
  Option.bind (find_header buf req H_content_length)
    (fun bs -> Int64.of_string_opt (boxed_span_to_string buf bs))

(** Check if transfer-encoding is chunked *)
let is_chunked (buf : buffer) (req : request) : bool =
  find_header buf req H_transfer_encoding
  |> Option.map (fun bs -> span_equal_caseless buf (unbox_span bs) "chunked")
  |> Option.value ~default:false

(** Check connection header *)
let is_keep_alive (buf : buffer) (req : request) : bool =
  match find_header buf req H_connection with
  | None -> req.version = HTTP_1_1
  | Some bs ->
    if span_equal_caseless buf (unbox_span bs) "close" then false
    else if span_equal_caseless buf (unbox_span bs) "keep-alive" then true
    else req.version = HTTP_1_1

(* Response helpers *)

let find_header_resp (_buf : buffer) (resp : response) (target : header_name) : boxed_span option =
  Headers.find_known resp.headers target

let find_header_span_resp buf resp name : boxed_span option =
  Headers.find_by_string buf.zb resp.headers name

let get_header_resp buf resp name =
  Option.map (boxed_span_to_string buf) (find_header_span_resp buf resp name)

let content_length_resp buf resp =
  Option.bind (find_header_resp buf resp H_content_length)
    (fun bs -> Int64.of_string_opt (boxed_span_to_string buf bs))

let is_chunked_resp buf resp =
  find_header_resp buf resp H_transfer_encoding
  |> Option.map (fun bs -> span_equal_caseless buf (unbox_span bs) "chunked")
  |> Option.value ~default:false

(* ============================================================
   Serialization
   ============================================================ *)

(** Output abstraction - either a span from source buffer or a string *)
type out =
  | Span of buffer * span
  | Str of string

let str s = Str s
let out_span buf sp = Span (buf, sp)

let write_out dst ~pos = function
  | Str s ->
    let len = String.length s in
    Bytes.blit_string s 0 dst pos len;
    len
  | Span (buf, sp) ->
    Zbuf.span_blit buf.zb sp dst ~dst_off:pos;
    sp.#len

let write_crlf dst ~pos =
  Bytes.set dst pos '\r';
  Bytes.set dst (pos + 1) '\n';
  2

let write_sp dst ~pos =
  Bytes.set dst pos ' ';
  1

let write_colon_sp dst ~pos =
  Bytes.set dst pos ':';
  Bytes.set dst (pos + 1) ' ';
  2

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

let write_method_from ~src dst ~pos = function
  | Other sp ->
    Zbuf.span_blit src.zb sp dst ~dst_off:pos;
    sp.#len
  | m -> write_method dst ~pos m

let write_version dst ~pos = function
  | HTTP_1_0 -> Bytes.blit_string "HTTP/1.0" 0 dst pos 8; 8
  | HTTP_1_1 -> Bytes.blit_string "HTTP/1.1" 0 dst pos 8; 8

let write_status_code dst ~pos status =
  Bytes.set dst pos (Char.chr (48 + status / 100));
  Bytes.set dst (pos + 1) (Char.chr (48 + (status / 10) mod 10));
  Bytes.set dst (pos + 2) (Char.chr (48 + status mod 10));
  3

let write_request_line dst ~pos ~method_ ~target ~version =
  let mutable p = pos in
  p <- p + write_out dst ~pos:p method_;
  p <- p + write_sp dst ~pos:p;
  p <- p + write_out dst ~pos:p target;
  p <- p + write_sp dst ~pos:p;
  p <- p + write_version dst ~pos:p version;
  p <- p + write_crlf dst ~pos:p;
  p - pos

let write_status_line dst ~pos ~version ~status ~reason =
  let mutable p = pos in
  p <- p + write_version dst ~pos:p version;
  p <- p + write_sp dst ~pos:p;
  p <- p + write_status_code dst ~pos:p status;
  p <- p + write_sp dst ~pos:p;
  p <- p + write_out dst ~pos:p reason;
  p <- p + write_crlf dst ~pos:p;
  p - pos

let write_header dst ~pos ~name ~value =
  let mutable p = pos in
  p <- p + write_out dst ~pos:p name;
  p <- p + write_colon_sp dst ~pos:p;
  p <- p + write_out dst ~pos:p value;
  p <- p + write_crlf dst ~pos:p;
  p - pos

let write_headers dst ~pos headers =
  let p = List.fold_left (fun p (name, value) ->
    p + write_header dst ~pos:p ~name ~value
  ) pos headers in
  let p = p + write_crlf dst ~pos:p in
  p - pos

let write_request dst ~pos ~method_ ~target ~version ~headers =
  let mutable p = pos in
  p <- p + write_request_line dst ~pos:p ~method_ ~target ~version;
  p <- p + write_headers dst ~pos:p headers;
  p - pos

let write_response dst ~pos ~version ~status ~reason ~headers =
  let mutable p = pos in
  p <- p + write_status_line dst ~pos:p ~version ~status ~reason;
  p <- p + write_headers dst ~pos:p headers;
  p - pos

let write_chunk_header dst ~pos size =
  let hex = Printf.sprintf "%x" size in
  let len = String.length hex in
  Bytes.blit_string hex 0 dst pos len;
  let mutable p = pos + len in
  p <- p + write_crlf dst ~pos:p;
  p - pos

let write_chunk_trailer dst ~pos =
  write_crlf dst ~pos

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

type transfer =
  | Fixed of int64
  | Chunked
  | Close_delimited
  | No_body

let transfer_of_request (buf : buffer) (req : request) : transfer =
  if is_chunked buf req then Chunked
  else match content_length buf req with
    | Some 0L -> No_body
    | Some len -> Fixed len
    | None -> No_body

let transfer_of_response (buf : buffer) (resp : response) : transfer =
  if resp.status < 200 || resp.status = 204 || resp.status = 304 then No_body
  else if is_chunked_resp buf resp then Chunked
  else match content_length_resp buf resp with
    | Some 0L -> No_body
    | Some len -> Fixed len
    | None -> Close_delimited

type body_reader =
  | Fixed_body of { mutable remaining : int64 }
  | Chunked_body of {
      mutable chunk_remaining : int64;
      mutable done_ : bool;
      mutable trailers : Headers.t option;
    }
  | Close_body
  | Empty_body

let body_reader_of_transfer = function
  | Fixed len -> Fixed_body { remaining = len }
  | Chunked -> Chunked_body { chunk_remaining = 0L; done_ = false; trailers = None }
  | Close_delimited -> Close_body
  | No_body -> Empty_body

let body_reader_of_request buf req =
  body_reader_of_transfer (transfer_of_request buf req)

let body_reader_of_response buf resp =
  body_reader_of_transfer (transfer_of_response buf resp)

let parse_chunk_size buf =
  let mutable acc = 0L in
  let mutable parsing = true in
  while parsing do
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
      acc <- Int64.add (Int64.shift_left acc 4) (Int64.of_int d)
    | None -> parsing <- false
  done;
  (* Skip chunk extensions until CRLF *)
  let mutable skipping = true in
  while skipping do
    match peek buf 0 with
    | '\r' ->
      if peek buf 1 = '\n' then begin
        consume buf 2;
        skipping <- false
      end else
        consume buf 1
    | _ -> consume buf 1
  done;
  acc

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
        let src_pos = Zbuf.cursor_pos buf.cur in
        let sp = #{ start = src_pos; len = to_read } in
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
      let sp = #{ start = src_pos; len = to_read } in
      Zbuf.span_blit buf.zb sp dst ~dst_off:off;
      consume buf to_read;
      to_read
    end

  | Chunked_body state ->
    if state.done_ && state.chunk_remaining = 0L then 0
    else begin
      if state.chunk_remaining = 0L && not state.done_ then begin
        let size = parse_chunk_size buf in
        if size = 0L then begin
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
          let sp = #{ start = src_pos; len = to_read } in
          Zbuf.span_blit buf.zb sp dst ~dst_off:off;
          consume buf to_read;
          state.chunk_remaining <- Int64.sub state.chunk_remaining (Int64.of_int to_read);
          if state.chunk_remaining = 0L && not state.done_ then begin
            if try_refill buf 2 && peek buf 0 = '\r' && peek buf 1 = '\n' then
              consume buf 2
          end;
          to_read
        end
      end
    end

let body_read_span (buf : buffer) (reader : body_reader) : boxed_span option =
  match reader with
  | Empty_body -> Some (box_span #{ start = Zbuf.cursor_pos buf.cur; len = 0 })
  | Fixed_body state ->
    let len = Int64.to_int state.remaining in
    if len > 1024 * 1024 then None
    else if not (try_refill buf len) then None
    else begin
      let start = Zbuf.cursor_pos buf.cur in
      let sp = #{ start; len } in
      consume buf len;
      state.remaining <- 0L;
      Some (box_span sp)
    end
  | Chunked_body _ -> None
  | Close_body -> None

let body_read_string (buf : buffer) (reader : body_reader) : string =
  match reader with
  | Empty_body -> ""
  | Fixed_body state when state.remaining <= 65536L ->
    (match body_read_span buf reader with
     | Some bs -> Zbuf.span_to_string buf.zb (unbox_span bs)
     | None -> "")
  | _ ->
    let result = Buffer.create 4096 in
    let chunk = Bytes.create 4096 in
    let mutable reading = true in
    while reading do
      let n = body_read buf reader chunk ~off:0 ~len:4096 in
      if n > 0 then Buffer.add_subbytes result chunk 0 n
      else reading <- false
    done;
    Buffer.contents result

let body_drain (buf : buffer) (reader : body_reader) : unit =
  let chunk = Bytes.create 4096 in
  let mutable draining = true in
  while draining do
    let n = body_read buf reader chunk ~off:0 ~len:4096 in
    if n = 0 then draining <- false
  done

let body_is_done (reader : body_reader) : bool =
  match reader with
  | Empty_body -> true
  | Fixed_body state -> state.remaining = 0L
  | Chunked_body state -> state.done_ && state.chunk_remaining = 0L
  | Close_body -> false

let body_trailers (reader : body_reader) : Headers.t option =
  match reader with
  | Chunked_body { done_ = true; trailers = Some t; _ } -> Some t
  | _ -> None
