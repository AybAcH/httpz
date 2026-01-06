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
  | H_other
(** Header name. [H_other] indicates an unknown header; the actual name
    is stored in the header's [name_span] field. *)

type header = {
  name : header_name;
  name_span : span;
  value : span;
}
(** Parsed header. Stored in local list - stack allocated, no heap allocation.
    [name_span] is only meaningful when [name = H_other]. *)

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
    Headers stored in local list - stack allocated, grows as needed. *)

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
(** Find first header by name.
    Only matches known headers; use [find_header_string] for [H_other]. *)

val find_header_string : buffer -> header list @ local -> string -> header option @ local
(** Find header by string name (case-insensitive). *)

val content_length : buffer -> header list @ local -> int64
(** Get Content-Length or [-1L]. *)

val is_chunked : buffer -> header list @ local -> bool
(** Check Transfer-Encoding: chunked. *)

val is_keep_alive : buffer -> header list @ local -> version -> bool
(** Check Connection keep-alive. *)

(** {1 Body Handling} *)

val body_in_buffer : buffer -> len:int -> body_off:int -> header list @ local -> bool
(** Check if the complete body is available in the buffer.
    Returns [true] if body_off + content_length <= len, or if there's no body. *)

val body_span : buffer -> len:int -> body_off:int -> header list @ local -> span
(** Get span of body if fully in buffer. Returns span with [len = -1] if body
    incomplete or chunked encoding (use [parse_chunk] for chunked). *)

val body_bytes_needed : buffer -> len:int -> body_off:int -> header list @ local -> int
(** Returns additional bytes needed for complete body, or [0] if complete.
    Returns [-1] for chunked encoding (unknown length). *)

(** {2 Chunked Transfer Encoding} *)

type chunk_status =
  | Chunk_ok       (** Chunk parsed successfully *)
  | Chunk_partial  (** Need more data *)
  | Chunk_done     (** Final chunk (zero-length) *)
  | Chunk_error    (** Malformed chunk *)

type chunk = #{
  data_off : int;  (** Offset of chunk data in buffer *)
  data_len : int;  (** Length of chunk data *)
  next_off : int;  (** Offset where next chunk starts *)
}
(** Unboxed chunk record. *)

val parse_chunk : buffer -> off:int -> len:int -> #(chunk_status * chunk)
(** Parse a single chunk starting at [off]. Buffer contains [len] bytes total.
    Returns chunk info and status. For [Chunk_ok], use [next_off] to parse
    the next chunk. For [Chunk_done], parsing is complete. *)

(** {1 Response Writing} *)

type response_status =
  | S200_OK | S201_Created | S204_No_Content
  | S301_Moved_Permanently | S302_Found | S304_Not_Modified
  | S400_Bad_Request | S401_Unauthorized | S403_Forbidden | S404_Not_Found
  | S405_Method_Not_Allowed | S411_Length_Required | S413_Payload_Too_Large
  | S500_Internal_Server_Error | S502_Bad_Gateway | S503_Service_Unavailable

val response_status_code : response_status -> int
(** Get numeric status code. *)

val response_status_reason : response_status -> string
(** Get reason phrase. *)

val write_status_line : bytes -> off:int -> response_status -> version -> int
(** Write "HTTP/1.x CODE Reason\\r\\n" to bytes at offset.
    Returns new offset (bytes written = new_off - off). *)

val write_header : bytes -> off:int -> string -> string -> int
(** Write "Name: Value\\r\\n" to bytes at offset. Returns new offset. *)

val write_header_int : bytes -> off:int -> string -> int -> int
(** Write header with integer value. Returns new offset. *)

val write_crlf : bytes -> off:int -> int
(** Write "\\r\\n" to bytes. Returns new offset. *)

val write_content_length : bytes -> off:int -> int -> int
(** Write "Content-Length: N\\r\\n". Returns new offset. *)

val write_connection : bytes -> off:int -> bool -> int
(** Write "Connection: keep-alive\\r\\n" or "Connection: close\\r\\n".
    [true] for keep-alive. Returns new offset. *)

(** {1 Debug} *)

val status_to_string : status -> string
val method_to_string : buffer -> method_ -> string
val version_to_string : version -> string
val header_name_to_string : buffer -> header_name -> string
val header_name_lowercase : header_name -> string
(** Lowercase canonical name for known headers. Returns [""] for [H_other]. *)
