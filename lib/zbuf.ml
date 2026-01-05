(* zbuf.ml - Zero-copy multi-buffer spans with OxCaml optimizations *)

type pos = int

type span = #{ start : pos; len : int }

type fragment = { data : bytes; off : int; len : int; base : pos }

type t = { fragments : fragment Dynarray.t; mutable total_len : int }

type cursor = { mutable pos : pos; mutable mark : pos; mutable frag_idx : int }

type _ Effect.t += Need_data : bool Effect.t

type refill = t -> bool

let create () = { fragments = Dynarray.create (); total_len = 0 }

let push t data ~off ~len =
  if off < 0 || len < 0 || off + len > Bytes.length data then
    invalid_arg "Zbuf.push: invalid offset or length";
  let frag = { data; off; len; base = t.total_len } in
  Dynarray.add_last t.fragments frag;
  if len > 0 then t.total_len <- t.total_len + len

let[@inline] length t = t.total_len
let[@inline] limit t = t.total_len
let[@inline] fragment_count t = Dynarray.length t.fragments

let find_frag_idx_from t pos start_idx =
  let count = Dynarray.length t.fragments in
  if count = 0 then 0
  else begin
    let mutable idx = start_idx in
    let mutable found = false in
    while not found && idx < count do
      let frag = Dynarray.get t.fragments idx in
      if pos < frag.base + frag.len then found <- true
      else idx <- idx + 1
    done;
    if found then idx else count - 1
  end

let cursor t ~pos = exclave_
  if pos < 0 || pos > t.total_len then
    invalid_arg "Zbuf.cursor: position out of bounds";
  stack_ { pos; mark = pos; frag_idx = find_frag_idx_from t pos 0 }

let cursor_global t ~pos =
  if pos < 0 || pos > t.total_len then
    invalid_arg "Zbuf.cursor_global: position out of bounds";
  { pos; mark = pos; frag_idx = find_frag_idx_from t pos 0 }

let[@inline] cursor_pos cur = cur.pos

let cursor_set t cur new_pos =
  if new_pos < 0 || new_pos > t.total_len then
    invalid_arg "Zbuf.cursor_set: position out of bounds";
  cur.pos <- new_pos;
  let count = Dynarray.length t.fragments in
  if count > 0 && cur.frag_idx < count then begin
    let frag = Dynarray.get t.fragments cur.frag_idx in
    if new_pos < frag.base then cur.frag_idx <- 0
  end;
  let mutable done_ = false in
  while not done_ do
    if cur.frag_idx >= count - 1 then done_ <- true
    else begin
      let frag = Dynarray.get t.fragments cur.frag_idx in
      if new_pos >= frag.base + frag.len then cur.frag_idx <- cur.frag_idx + 1
      else done_ <- true
    end
  done

let[@inline] cursor_mark cur = cur.mark <- cur.pos
let[@inline] cursor_marked cur = cur.mark
let[@inline] cursor_remaining t cur = t.total_len - cur.pos

let[@inline] request_data () = Effect.perform Need_data

let with_refill (type a) zb (refill : refill) (f : unit -> a) : a =
  let do_refill () : bool = refill zb in
  let effc : type b. b Effect.t -> ((b, a) Effect.Deep.continuation -> a) option =
    function
    | Need_data -> Some (fun k -> Effect.Deep.continue k (do_refill () : b))
    | _ -> None
  in
  Effect.Deep.match_with f () { retc = Fun.id; exnc = raise; effc }

let rec peek t cur off =
  let pos = cur.pos + off in
  if pos < 0 then invalid_arg "Zbuf.peek: negative position";
  if pos >= t.total_len then begin
    if request_data () then peek t cur off
    else raise End_of_file
  end else begin
    let count = Dynarray.length t.fragments in
    if cur.frag_idx < count then begin
      let frag = Dynarray.get t.fragments cur.frag_idx in
      if pos >= frag.base && pos < frag.base + frag.len then
        Bytes.unsafe_get frag.data (frag.off + pos - frag.base)
      else begin
        let mutable idx = cur.frag_idx + 1 in
        let mutable result = '\x00' in
        let mutable found = false in
        while not found do
          let f = Dynarray.get t.fragments idx in
          if pos < f.base + f.len then begin
            result <- Bytes.unsafe_get f.data (f.off + pos - f.base);
            found <- true
          end else idx <- idx + 1
        done;
        result
      end
    end else invalid_arg "Zbuf.peek: inconsistent state"
  end

let[@inline always] advance cur n = cur.pos <- cur.pos + n

let scan_while pred t cur =
  let start_pos = cur.pos in
  let mutable scanning = true in
  while scanning do
    let count = Dynarray.length t.fragments in
    if cur.frag_idx >= count then begin
      if not (request_data ()) then scanning <- false
    end else begin
      let frag = Dynarray.get t.fragments cur.frag_idx in
      let frag_end = frag.base + frag.len in
      if cur.pos >= frag_end then cur.frag_idx <- cur.frag_idx + 1
      else begin
        let pos_in_frag = cur.pos - frag.base in
        let frag_remaining = frag.len - pos_in_frag in
        let mutable i = 0 in
        let mutable matched = true in
        while matched && i < frag_remaining do
          let c = Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i) in
          if pred c then i <- i + 1 else matched <- false
        done;
        cur.pos <- cur.pos + i;
        if i = frag_remaining then begin
          cur.frag_idx <- cur.frag_idx + 1;
          if cur.frag_idx >= count then
            if not (request_data ()) then scanning <- false
        end else scanning <- false
      end
    end
  done;
  cur.pos - start_pos

let ensure t cur n =
  let mutable filling = true in
  let mutable result = false in
  while filling do
    let available = t.total_len - cur.pos in
    if available >= n then begin result <- true; filling <- false end
    else if request_data () then ()
    else begin result <- available > 0; filling <- false end
  done;
  result

let[@inline] take cur n =
  let sp = #{ start = cur.mark; len = cur.pos + n - cur.mark } in
  cur.pos <- cur.pos + n;
  cur.mark <- cur.pos;
  sp

let get t pos =
  if pos < 0 || pos >= t.total_len then invalid_arg "Zbuf.get: position out of bounds";
  let mutable idx = 0 in
  let mutable result = '\x00' in
  let mutable found = false in
  while not found do
    let frag = Dynarray.get t.fragments idx in
    if pos < frag.base + frag.len then begin
      result <- Bytes.unsafe_get frag.data (frag.off + pos - frag.base);
      found <- true
    end else idx <- idx + 1
  done;
  result

let[@inline] span ~start ~len = #{ start; len }
let[@inline] span_since_mark cur = #{ start = cur.mark; len = cur.pos - cur.mark }
let[@inline] span_empty pos = #{ start = pos; len = 0 }
let[@inline] span_len (sp : span) = sp.#len

let find_frag_idx t pos =
  let mutable idx = 0 in
  let mutable found = false in
  while not found do
    let frag = Dynarray.get t.fragments idx in
    if pos < frag.base + frag.len then found <- true else idx <- idx + 1
  done;
  idx

let span_to_string t (sp : span) =
  if sp.#len = 0 then ""
  else begin
    if sp.#start < 0 || sp.#start + sp.#len > t.total_len then
      invalid_arg "Zbuf.span_to_string: span out of bounds";
    let result = Bytes.create sp.#len in
    let start_idx = find_frag_idx t sp.#start in
    let mutable dst_off = 0 in
    let mutable idx = start_idx in
    let mutable pos = sp.#start in
    let mutable remaining = sp.#len in
    while remaining > 0 do
      let frag = Dynarray.get t.fragments idx in
      let pos_in_frag = pos - frag.base in
      let avail = frag.len - pos_in_frag in
      let to_copy = min avail remaining in
      Bytes.blit frag.data (frag.off + pos_in_frag) result dst_off to_copy;
      dst_off <- dst_off + to_copy;
      idx <- idx + 1;
      pos <- pos + to_copy;
      remaining <- remaining - to_copy
    done;
    Bytes.unsafe_to_string result
  end

let span_equal_with ~char_equal ~err_msg t (sp : span) s =
  if sp.#len <> String.length s then false
  else if sp.#len = 0 then true
  else begin
    if sp.#start < 0 || sp.#start + sp.#len > t.total_len then invalid_arg err_msg;
    let start_idx = find_frag_idx t sp.#start in
    let mutable str_off = 0 in
    let mutable idx = start_idx in
    let mutable pos = sp.#start in
    let mutable remaining = sp.#len in
    let mutable equal = true in
    while equal && remaining > 0 do
      let frag = Dynarray.get t.fragments idx in
      let pos_in_frag = pos - frag.base in
      let avail = frag.len - pos_in_frag in
      let to_check = min avail remaining in
      let mutable i = 0 in
      while equal && i < to_check do
        let c1 = Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i) in
        let c2 = String.unsafe_get s (str_off + i) in
        if not (char_equal c1 c2) then equal <- false else i <- i + 1
      done;
      str_off <- str_off + to_check;
      idx <- idx + 1;
      pos <- pos + to_check;
      remaining <- remaining - to_check
    done;
    equal
  end

let span_equal t sp s =
  span_equal_with ~char_equal:Char.equal ~err_msg:"Zbuf.span_equal: span out of bounds" t sp s

let span_equal_caseless t sp s =
  span_equal_with
    ~char_equal:(fun c1 c2 -> Char.lowercase_ascii c1 = Char.lowercase_ascii c2)
    ~err_msg:"Zbuf.span_equal_caseless: span out of bounds" t sp s

let span_iter f t (sp : span) =
  if sp.#len > 0 then begin
    if sp.#start < 0 || sp.#start + sp.#len > t.total_len then
      invalid_arg "Zbuf.span_iter: span out of bounds";
    let start_idx = find_frag_idx t sp.#start in
    let mutable idx = start_idx in
    let mutable pos = sp.#start in
    let mutable remaining = sp.#len in
    while remaining > 0 do
      let frag = Dynarray.get t.fragments idx in
      let pos_in_frag = pos - frag.base in
      let avail = frag.len - pos_in_frag in
      let to_iter = min avail remaining in
      for i = 0 to to_iter - 1 do
        f (Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i))
      done;
      idx <- idx + 1;
      pos <- pos + to_iter;
      remaining <- remaining - to_iter
    done
  end

let span_fold f t (sp : span) init =
  if sp.#len = 0 then init
  else begin
    if sp.#start < 0 || sp.#start + sp.#len > t.total_len then
      invalid_arg "Zbuf.span_fold: span out of bounds";
    let start_idx = find_frag_idx t sp.#start in
    let mutable acc = init in
    let mutable idx = start_idx in
    let mutable pos = sp.#start in
    let mutable remaining = sp.#len in
    while remaining > 0 do
      let frag = Dynarray.get t.fragments idx in
      let pos_in_frag = pos - frag.base in
      let avail = frag.len - pos_in_frag in
      let to_fold = min avail remaining in
      for i = 0 to to_fold - 1 do
        acc <- f acc (Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i))
      done;
      idx <- idx + 1;
      pos <- pos + to_fold;
      remaining <- remaining - to_fold
    done;
    acc
  end

let span_blit t (sp : span) dst ~dst_off =
  if sp.#len > 0 then begin
    if sp.#start < 0 || sp.#start + sp.#len > t.total_len then
      invalid_arg "Zbuf.span_blit: span out of bounds";
    if dst_off < 0 || dst_off + sp.#len > Bytes.length dst then
      invalid_arg "Zbuf.span_blit: destination out of bounds";
    let start_idx = find_frag_idx t sp.#start in
    let mutable d_off = dst_off in
    let mutable idx = start_idx in
    let mutable pos = sp.#start in
    let mutable remaining = sp.#len in
    while remaining > 0 do
      let frag = Dynarray.get t.fragments idx in
      let pos_in_frag = pos - frag.base in
      let avail = frag.len - pos_in_frag in
      let to_copy = min avail remaining in
      Bytes.blit frag.data (frag.off + pos_in_frag) dst d_off to_copy;
      d_off <- d_off + to_copy;
      idx <- idx + 1;
      pos <- pos + to_copy;
      remaining <- remaining - to_copy
    done
  end
