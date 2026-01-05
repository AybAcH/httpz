(** Zbuf - Zero-copy multi-buffer spans with OxCaml optimizations

    Manages spans across multiple buffer fragments without consolidation
    copying. Uses OCaml 5 effects to request more data when needed.

    {2 OxCaml Optimizations}

    This module is optimized for minimal allocation using OxCaml features:

    - {b Unboxed spans}: The {!span} type is an unboxed record, eliminating
      heap allocation for token boundaries. Access fields with [.#start]
      and [.#len].

    - {b Local cursors}: Cursors are stack-allocated by default via {!cursor}.
      Use {!cursor_global} only when storing cursors in data structures.

    - {b Zero-alloc hot paths}: Core operations like {!advance}, {!span},
      and {!span_since_mark} are annotated [@zero_alloc].

    - {b Simplified effects}: The {!Need_data} effect carries no payload,
      enabling local cursor usage within effect handlers.

    {2 Usage Pattern}

    {[
      let zb = Zbuf.create () in

      (* Refill function pushes data, returns true; or returns false for EOF *)
      let refill zb =
        match read_from_socket () with
        | 0 -> false  (* EOF *)
        | n -> Zbuf.push zb buf ~off:0 ~len:n; true
      in

      Zbuf.with_refill zb refill (fun () ->
        (* Create stack-allocated cursor *)
        let local_ cur = Zbuf.cursor zb ~pos:0 in
        Zbuf.cursor_mark cur;
        ignore (Zbuf.scan_while is_token zb cur);
        let token = Zbuf.span_since_mark cur in  (* no allocation *)
        Zbuf.span_to_string zb token)            (* string allocated here *)
    ]}
*)

(** {1 Types} *)

(** Logical stream position. *)
type pos = int

(** Unboxed span referencing a region across potentially multiple fragments.

    This is an unboxed record - creating, returning, and passing spans
    never allocates. Access fields with [sp.#start] and [sp.#len]. *)
type span = #{ start : pos; len : int }

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

(** Parsing cursor.

    Cursors track position within a buffer collection. They are designed
    for stack allocation - use {!cursor} for the common case, and
    {!cursor_global} only when the cursor must be stored in a data structure. *)
type cursor = {
  mutable pos : pos;
  mutable mark : pos;
  mutable frag_idx : int;
}

(** {1 Effects} *)

(** Effect performed when more data is needed.

    Unlike traditional designs, this effect carries no payload. The handler
    accesses the buffer collection via closure, enabling stack-allocated
    cursors in parsing code.

    The handler should:
    1. Attempt to read more data
    2. If data available: call {!push}, continue with [true]
    3. If EOF: continue with [false]

    Use {!with_refill} for convenient handling. *)
type _ Effect.t += Need_data : bool Effect.t

(** Refill function: attempt to push data to [t].

    Returns [true] if data was pushed, [false] for EOF.
    This is a change from the previous API which used unit return. *)
type refill = t -> bool

(** Run a function with automatic refill handling.

    [with_refill zb refill f] runs [f ()] and handles {!Need_data} effects
    by calling [refill zb] and continuing with the result.

    The cursor is not passed to this function - it remains local to [f]. *)
val with_refill : t -> refill -> (unit -> 'a) -> 'a

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

(** Create cursor at given position, stack-allocated.

    The returned cursor is allocated on the caller's stack frame and
    must not escape the calling scope. This is the recommended way
    to create cursors for parsing operations.

    @raise Invalid_argument if [pos < 0] or [pos > limit t]. *)
val cursor : t -> pos:pos -> cursor @ local

(** Create cursor at given position, heap-allocated.

    Use this only when the cursor must be stored in a data structure
    or otherwise escape the current scope.

    @raise Invalid_argument if [pos < 0] or [pos > limit t]. *)
val cursor_global : t -> pos:pos -> cursor

(** Current cursor position. *)
val cursor_pos : cursor @ local -> pos

(** Set cursor position and update fragment cache.

    @raise Invalid_argument if position is out of bounds. *)
val cursor_set : t -> cursor @ local -> pos -> unit

(** Mark current position for later span creation. *)
val cursor_mark : cursor @ local -> unit

(** Get marked position. *)
val cursor_marked : cursor @ local -> pos

(** Bytes available from cursor position to end of buffer. *)
val cursor_remaining : t -> cursor @ local -> int

(** {1 Cursor-based Access} *)

(** Peek byte at cursor position plus offset.

    Auto-refills via {!Need_data} effect if data not available.
    @raise End_of_file if EOF is reached before data available.
    @raise Invalid_argument if offset is negative. *)
val peek : t -> cursor @ local -> int -> char

(** Advance cursor by [n] bytes. *)
val advance : cursor @ local -> int -> unit

(** Scan forward while predicate holds, requesting more data as needed.

    [scan_while pred t cur] advances past all bytes satisfying [pred].
    When data is exhausted, performs {!Need_data} effect to request more.
    Returns total bytes consumed. Stops on predicate failure or EOF. *)
val scan_while : (char -> bool) -> t -> cursor @ local -> int

(** Ensure at least [n] bytes are available from cursor position.

    Performs {!Need_data} effects as needed to refill.
    Returns [true] if [n] bytes available, [false] if EOF before [n] bytes. *)
val ensure : t -> cursor @ local -> int -> bool

(** Consume [n] bytes, returning span from mark.

    Updates mark to new position. Returns unboxed span (no allocation). *)
val take : cursor @ local -> int -> span

(** {1 Direct Position Access} *)

(** Get byte at absolute position.

    @raise Invalid_argument if position is out of bounds. *)
val get : t -> pos -> char

(** {1 Span Construction} *)

(** Create span from start position and length.

    Returns unboxed span - no heap allocation. *)
val span : start:pos -> len:int -> span

(** Create span from cursor's mark to current position.

    Returns unboxed span - no heap allocation. *)
val span_since_mark : cursor @ local -> span

(** Empty span at given position.

    Returns unboxed span - no heap allocation. *)
val span_empty : pos -> span

(** Span length accessor. *)
val span_len : span -> int

(** {1 Span Operations} *)

(** Materialize span to string.

    This allocates a new string.
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
