(* zbufo.mli - Zero-copy multi-buffer spans, standard OCaml version (no OxCaml) *)

type pos = int
type span = { start : pos; len : int }
type t
type cursor
type refill = t -> bool

val create : unit -> t
val push : t -> bytes -> off:int -> len:int -> unit
val length : t -> int
val limit : t -> int
val fragment_count : t -> int

val cursor : t -> pos:int -> cursor
val cursor_global : t -> pos:int -> cursor
val cursor_pos : cursor -> pos
val cursor_set : t -> cursor -> int -> unit
val cursor_mark : cursor -> unit
val cursor_marked : cursor -> pos
val cursor_remaining : t -> cursor -> int

val with_refill : t -> refill -> (unit -> 'a) -> 'a
val request_data : unit -> bool

val peek : t -> cursor -> int -> char
val advance : cursor -> int -> unit
val scan_while : (char -> bool) -> t -> cursor -> int
val ensure : t -> cursor -> int -> bool
val take : cursor -> int -> span

val get : t -> pos -> char

val span : start:pos -> len:int -> span
val span_since_mark : cursor -> span
val span_empty : pos -> span
val span_len : span -> int

val span_to_string : t -> span -> string
val span_equal : t -> span -> string -> bool
val span_equal_caseless : t -> span -> string -> bool
val span_iter : (char -> unit) -> t -> span -> unit
val span_fold : ('a -> char -> 'a) -> t -> span -> 'a -> 'a
val span_blit : t -> span -> bytes -> dst_off:int -> unit
