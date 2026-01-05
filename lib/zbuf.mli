(** Zbuf - Zero-copy multi-buffer spans with effect-based refilling

    Manages spans across multiple buffer fragments without consolidation
    copying. Uses OCaml 5 effects to request more data when needed.

    {2 Design Principles}

    - {b Zero-copy}: Spans reference positions across fragments without copying
    - {b Effect-based}: Performs [Need_data] effect when more bytes are needed
    - {b Concurrent parsing}: Multiple cursors can parse the same buffer set
    - {b Sequential optimization}: Cursor caches fragment for O(1) sequential access

    {2 Usage Pattern}

    {[
      let zb = Zbuf.create () in
      let cur = Zbuf.cursor zb ~pos:0 in

      (* Wrap parsing in effect handler *)
      Zbuf.with_refill zb cur
        (fun () ->
          match read_from_socket () with
          | 0 -> None  (* EOF *)
          | n -> Some (buf, 0, n))
        (fun () ->
          (* Parsing code - effects handled automatically *)
          Zbuf.cursor_mark cur;
          ignore (Zbuf.scan_while is_token zb cur);
          let token = Zbuf.span_since_mark cur in
          Zbuf.span_to_string zb token)
    ]}
*)

(** {1 Types} *)

(** Logical stream position. *)
type pos = int

(** Span referencing a region across potentially multiple fragments. *)
type span = {
  start : pos;
  len : int;
}

(** A single buffer fragment. *)
type fragment = {
  data : bytes;
  off : int;
  len : int;
  base : pos;
}

(** Multi-buffer collection. *)
type t = {
  fragments : fragment Dynarray.t;  (** Resizable fragment array *)
  mutable total_len : int;          (** Total bytes across all fragments *)
}

(** Parsing cursor. *)

type cursor = {
  mutable pos : pos;
  mutable mark : pos;
  mutable frag_idx : int;
}

(** {1 Effects} *)

(** Effect performed when more data is needed.

    The effect carries the zbuf and cursor. The handler should:
    1. Call the refill function to get more data
    2. If data available: call [push], continue with [true]
    3. If EOF: continue with [false]

    Use {!with_refill} for a convenient handler. *)
type _ Effect.t += Need_data : (t * cursor) -> bool Effect.t

(** Refill function: push data to [t]. Push a zero-length fragment for EOF.
    Zero-allocation API. *)
type refill = t -> unit

(** Run a function with automatic refill handling.

    [with_refill zb cur refill f] runs [f ()] and handles [Need_data] effects
    by calling [refill], pushing any returned data, and continuing. *)
val with_refill : t -> cursor -> refill -> (unit -> 'a) -> 'a

(** {1 Buffer Management} *)

(** Create empty buffer collection. *)
val create : unit -> t

(** Push a fragment.

    @raise Invalid_argument if [off] or [len] are out of bounds. *)
val push : t -> bytes -> off:int -> len:int -> unit

(** Total logical length across all fragments. *)
val length : t -> int

(** End position (equal to [length t]). *)
val limit : t -> pos

(** Number of fragments currently held. *)
val fragment_count : t -> int

(** {1 Cursor Management} *)

(** Create cursor at given position.

    @raise Invalid_argument if [pos < 0] or [pos > limit t]. *)
val cursor : t -> pos:pos -> cursor

(** Current cursor position. *)
val cursor_pos : cursor -> pos

(** Set cursor position and update fragment cache.

    @raise Invalid_argument if position is out of bounds. *)
val cursor_set : t -> cursor -> pos -> unit

(** Mark current position for later span creation. *)
val cursor_mark : cursor -> unit

(** Get marked position. *)
val cursor_marked : cursor -> pos

(** Bytes available from cursor position to end of buffer. *)
val cursor_remaining : t -> cursor -> int

(** {1 Cursor-based Access} *)

(** Peek byte at cursor position plus offset.

    Auto-refills via [Need_data] effect if data not available.
    @raise End_of_file if EOF is reached before data available.
    @raise Invalid_argument if offset is negative. *)
val peek : t -> cursor -> int -> char

(** Advance cursor by [n] bytes. *)
val advance : cursor -> int -> unit

(** Scan forward while predicate holds, requesting more data as needed.

    [scan_while pred t cur] advances past all bytes satisfying [pred].
    When data is exhausted, performs [Need_data] effect to request more.
    Returns total bytes consumed. Stops on predicate failure or EOF. *)
val scan_while : (char -> bool) -> t -> cursor -> int

(** Ensure at least [n] bytes are available from cursor position.

    Performs [Need_data] effects as needed to refill.
    Returns [true] if [n] bytes available, [false] if EOF before [n] bytes. *)
val ensure : t -> cursor -> int -> bool

(** Consume [n] bytes, returning span from mark. *)
val take : cursor -> int -> span

(** {1 Direct Position Access} *)

(** Get byte at absolute position.

    @raise Invalid_argument if position is out of bounds. *)
val get : t -> pos -> char

(** {1 Span Construction} *)

(** Create span from start position and length. *)
val span : start:pos -> len:int -> span

(** Create span from cursor's mark to current position. *)
val span_since_mark : cursor -> span

(** Empty span at given position. *)
val span_empty : pos -> span

(** Span length accessor. *)
val span_len : span -> int

(** {1 Span Operations} *)

(** Materialize span to string.

    @raise Invalid_argument if span is out of bounds. *)
val span_to_string : t -> span -> string

(** Compare span to string without allocation.

    @raise Invalid_argument if span is out of bounds. *)
val span_equal : t -> span -> string -> bool

(** Case-insensitive span comparison without allocation.

    @raise Invalid_argument if span is out of bounds. *)
val span_equal_caseless : t -> span -> string -> bool

(** Iterate over bytes in span.

    @raise Invalid_argument if span is out of bounds. *)
val span_iter : (char -> unit) -> t -> span -> unit

(** Fold over bytes in span.

    @raise Invalid_argument if span is out of bounds. *)
val span_fold : ('a -> char -> 'a) -> t -> span -> 'a -> 'a

(** Copy span bytes to destination buffer.

    @raise Invalid_argument if span or destination is out of bounds. *)
val span_blit : t -> span -> bytes -> dst_off:int -> unit
