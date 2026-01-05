(** Httpz - Stack-allocated HTTP/1.1 request parser for OxCaml.

    Parses HTTP/1.1 requests from a 32KB bigarray buffer, returning
    results entirely on the caller's stack. No heap allocation during
    parsing, no mutable state.

    {2 Usage}

    {[
      let buf = Httpz.create_buffer () in
      let len = read_from_socket buf in
      let #(status, req, headers) = Httpz.parse buf ~len in
      match status with
      | Ok ->
        let target = req.#target in
        List.iter (fun hdr ->
          match hdr.name with
          | H_content_length ->
            let n = Httpz.parse_int64 buf hdr.value in ...
          | H_host ->
            let host = hdr.value in ...
          | H_other name_span ->
            if Httpz.span_equal_caseless buf name_span "x-custom" then ...
          | _ -> ()
        ) headers
      | Partial -> need_more_data ()
      | Headers_too_large -> send_413 ()
      | _ -> send_400 ()
    ]} *)

(** {1 Constants} *)

val buffer_size : int
(** Maximum buffer size: 32KB. *)

val max_headers : int
(** Maximum headers per request. *)

(** {1 Core Types} *)

type span = #{ off : int; len : int }
(** Unboxed span into the buffer. *)

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
(** Parsed header. Stack-allocated when returned in list. *)

type request = #{
  meth : method_;
  target : span;
  version : version;
  body_off : int;
}
(** Unboxed request record. *)

type status =
  | Ok
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed

(** {1 Buffer} *)

type buffer =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create_buffer : unit -> buffer
(** Create a new 32KB buffer. *)

(** {1 Parsing} *)

val parse : buffer -> len:int -> #(status * request * header list) @ local
(** Parse HTTP/1.1 request. Returns unboxed tuple on caller's stack.
    Use Base.List functions (which support local) to process headers. *)

(** {1 Span Operations} *)

val span_equal : buffer -> span -> string -> bool
(** Case-sensitive comparison. *)

val span_equal_caseless : buffer -> span -> string -> bool
(** Case-insensitive comparison. *)

val parse_int64 : buffer -> span -> int64
(** Parse decimal integer. Returns [-1L] on error. *)

val span_to_string : buffer -> span -> string
(** Copy span to string. Allocates. *)

val span_to_bytes : buffer -> span -> bytes
(** Copy span to bytes. Allocates. *)

(** {1 Header Utilities} *)

val find_header : header list @ local -> header_name -> header option @ local
(** Find first header by name. *)

val find_header_string : buffer -> header list @ local -> string -> header option @ local
(** Find header by string name (case-insensitive). *)

val content_length : buffer -> header list @ local -> int64
(** Get Content-Length or [-1L]. *)

val is_chunked : buffer -> header list @ local -> bool
(** Check Transfer-Encoding: chunked. *)

val is_keep_alive : buffer -> header list @ local -> version -> bool
(** Check Connection keep-alive. *)

(** {1 Debug} *)

val status_to_string : status -> string
val method_to_string : buffer -> method_ -> string
val version_to_string : version -> string
val header_name_to_string : buffer -> header_name -> string
