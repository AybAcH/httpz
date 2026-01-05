(* zbuf.ml - Zero-copy multi-buffer spans with effect-based refilling

   Minimal cursor design: only stores frag_idx, looks up fragment via
   Dynarray.get when needed. No caching, no refresh, no sync. *)

type pos = int

type span = {
  start : pos;
  len : int;
}

type fragment = {
  data : bytes;
  off : int;
  len : int;
  base : pos;
}

type t = {
  fragments : fragment Dynarray.t;
  mutable total_len : int;
}

type cursor = {
  mutable pos : pos;
  mutable mark : pos;
  mutable frag_idx : int;
}

(* Effect for requesting more data *)
type _ Effect.t += Need_data : (t * cursor) -> bool Effect.t

(* Refill function: push data to t. Push len=0 fragment for EOF. *)
type refill = t -> unit

let create () =
  { fragments = Dynarray.create (); total_len = 0 }

let push t data ~off ~len =
  if off < 0 || len < 0 || off + len > Bytes.length data then
    invalid_arg "Zbuf.push: invalid offset or length";
  (* Always add fragment, even len=0 (EOF marker). Only len>0 adds to total. *)
  let frag = { data; off; len; base = t.total_len } in
  Dynarray.add_last t.fragments frag;
  if len > 0 then t.total_len <- t.total_len + len

let length t = t.total_len
let limit t = t.total_len
let fragment_count t = Dynarray.length t.fragments

let cursor t ~pos =
  if pos < 0 || pos > t.total_len then
    invalid_arg "Zbuf.cursor: position out of bounds";
  let count = Dynarray.length t.fragments in
  if count = 0 then
    { pos; mark = pos; frag_idx = 0 }
  else begin
    (* Find fragment containing pos *)
    let rec find idx =
      let frag = Dynarray.get t.fragments idx in
      if pos < frag.base + frag.len || idx = count - 1 then idx
      else find (idx + 1)
    in
    { pos; mark = pos; frag_idx = find 0 }
  end

let cursor_pos cur = cur.pos

let cursor_set t cur new_pos =
  if new_pos < 0 || new_pos > t.total_len then
    invalid_arg "Zbuf.cursor_set: position out of bounds";
  cur.pos <- new_pos;
  (* Reset frag_idx if moving backward *)
  let count = Dynarray.length t.fragments in
  if count > 0 && cur.frag_idx < count then begin
    let frag = Dynarray.get t.fragments cur.frag_idx in
    if new_pos < frag.base then cur.frag_idx <- 0
  end;
  (* Advance frag_idx to contain new_pos *)
  while cur.frag_idx < count - 1 &&
        let frag = Dynarray.get t.fragments cur.frag_idx in
        new_pos >= frag.base + frag.len do
    cur.frag_idx <- cur.frag_idx + 1
  done

let cursor_mark cur = cur.mark <- cur.pos
let cursor_marked cur = cur.mark
let cursor_remaining t cur = t.total_len - cur.pos

(* Check if last fragment is EOF marker (len=0) *)
let last_frag_is_eof t =
  let count = Dynarray.length t.fragments in
  if count = 0 then false
  else (Dynarray.get t.fragments (count - 1)).len = 0

(* Effect handler for refilling *)
let with_refill zb cur refill f =
  Effect.Deep.match_with f ()
    { retc = (fun x -> x);
      exnc = raise;
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Need_data (zb', cur') when zb' == zb && cur' == cur ->
          Some (fun (k : (a, _) Effect.Deep.continuation) ->
            refill zb;
            (* Check if refill pushed EOF marker (len=0 fragment) *)
            let got_data = not (last_frag_is_eof zb) in
            Effect.Deep.continue k got_data)
        | _ -> None }

let[@inline] request_data t cur =
  Effect.perform (Need_data (t, cur))

(* Peek at offset from cursor position - auto-refills via effects *)
let rec peek t cur off =
  let pos = cur.pos + off in
  if pos < 0 then invalid_arg "Zbuf.peek: negative position";
  if pos >= t.total_len then begin
    (* Need more data - request via effect *)
    if request_data t cur then
      peek t cur off  (* Retry after refill *)
    else
      raise End_of_file  (* EOF reached *)
  end else begin
    let count = Dynarray.length t.fragments in
    if cur.frag_idx < count then begin
      let frag = Dynarray.get t.fragments cur.frag_idx in
      if pos >= frag.base && pos < frag.base + frag.len then
        Bytes.unsafe_get frag.data (frag.off + pos - frag.base)
      else begin
        (* Scan forward from current *)
        let rec find idx =
          let f = Dynarray.get t.fragments idx in
          if pos < f.base + f.len then
            Bytes.unsafe_get f.data (f.off + pos - f.base)
          else find (idx + 1)
        in
        find (cur.frag_idx + 1)
      end
    end else
      (* No fragments yet but pos < total_len - shouldn't happen *)
      invalid_arg "Zbuf.peek: inconsistent state"
  end

let[@inline always] advance cur n =
  cur.pos <- cur.pos + n

(* Scan while predicate holds, using effects to refill when needed *)
let scan_while pred t cur =
  let start_pos = cur.pos in
  let rec scan () =
    let count = Dynarray.length t.fragments in
    if cur.frag_idx >= count then begin
      (* No fragment, try to refill *)
      if request_data t cur then scan ()
    end else begin
      let frag = Dynarray.get t.fragments cur.frag_idx in
      let frag_end = frag.base + frag.len in
      if cur.pos >= frag_end then begin
        (* Move to next fragment *)
        cur.frag_idx <- cur.frag_idx + 1;
        scan ()
      end else begin
        (* Scan within current fragment *)
        let pos_in_frag = cur.pos - frag.base in
        let frag_remaining = frag.len - pos_in_frag in
        let rec scan_frag i =
          if i >= frag_remaining then i
          else
            let c = Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i) in
            if pred c then scan_frag (i + 1) else i
        in
        let consumed = scan_frag 0 in
        cur.pos <- cur.pos + consumed;
        if consumed = frag_remaining then begin
          (* Exhausted fragment, move to next *)
          cur.frag_idx <- cur.frag_idx + 1;
          if cur.frag_idx < count then scan ()
          else if request_data t cur then scan ()
        end
      end
    end
  in
  scan ();
  cur.pos - start_pos

(* Ensure at least n bytes are available. Returns true if ok, false if EOF. *)
let ensure t cur n =
  let rec fill () =
    let available = t.total_len - cur.pos in
    if available >= n then true
    else if request_data t cur then fill ()
    else available > 0
  in
  fill ()

let take cur n =
  let sp = { start = cur.mark; len = cur.pos + n - cur.mark } in
  cur.pos <- cur.pos + n;
  cur.mark <- cur.pos;
  sp

(* Direct position access - scan from start *)
let get t pos =
  if pos < 0 || pos >= t.total_len then
    invalid_arg "Zbuf.get: position out of bounds";
  let rec find idx =
    let frag = Dynarray.get t.fragments idx in
    if pos < frag.base + frag.len then
      Bytes.unsafe_get frag.data (frag.off + pos - frag.base)
    else
      find (idx + 1)
  in
  find 0

(* Span construction *)
let span ~start ~len = { start; len }
let span_since_mark cur = { start = cur.mark; len = cur.pos - cur.mark }
let span_empty pos = { start = pos; len = 0 }
let span_len (sp : span) = sp.len

(* Find fragment index containing position *)
let find_frag_idx t pos =
  let rec find idx =
    let frag = Dynarray.get t.fragments idx in
    if pos < frag.base + frag.len then idx
    else find (idx + 1)
  in
  find 0

(* Span operations - iterate through fragments *)
let span_to_string t (sp : span) =
  if sp.len = 0 then ""
  else begin
    if sp.start < 0 || sp.start + sp.len > t.total_len then
      invalid_arg "Zbuf.span_to_string: span out of bounds";
    let result = Bytes.create sp.len in
    let start_idx = find_frag_idx t sp.start in
    let rec copy dst_off idx pos remaining =
      if remaining <= 0 then ()
      else begin
        let frag = Dynarray.get t.fragments idx in
        let pos_in_frag = pos - frag.base in
        let avail = frag.len - pos_in_frag in
        let to_copy = min avail remaining in
        Bytes.blit frag.data (frag.off + pos_in_frag) result dst_off to_copy;
        copy (dst_off + to_copy) (idx + 1) (pos + to_copy) (remaining - to_copy)
      end
    in
    copy 0 start_idx sp.start sp.len;
    Bytes.unsafe_to_string result
  end

let span_equal_with ~char_equal ~err_msg t (sp : span) s =
  if sp.len <> String.length s then false
  else if sp.len = 0 then true
  else begin
    if sp.start < 0 || sp.start + sp.len > t.total_len then
      invalid_arg err_msg;
    let start_idx = find_frag_idx t sp.start in
    let rec check str_off idx pos remaining =
      if remaining <= 0 then true
      else begin
        let frag = Dynarray.get t.fragments idx in
        let pos_in_frag = pos - frag.base in
        let avail = frag.len - pos_in_frag in
        let to_check = min avail remaining in
        let rec check_bytes i =
          if i >= to_check then true
          else
            let c1 = Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i) in
            let c2 = String.unsafe_get s (str_off + i) in
            if not (char_equal c1 c2) then false
            else check_bytes (i + 1)
        in
        if check_bytes 0 then
          check (str_off + to_check) (idx + 1) (pos + to_check) (remaining - to_check)
        else
          false
      end
    in
    check 0 start_idx sp.start sp.len
  end

let span_equal t sp s =
  span_equal_with ~char_equal:Char.equal
    ~err_msg:"Zbuf.span_equal: span out of bounds" t sp s

let span_equal_caseless t sp s =
  span_equal_with
    ~char_equal:(fun c1 c2 -> Char.lowercase_ascii c1 = Char.lowercase_ascii c2)
    ~err_msg:"Zbuf.span_equal_caseless: span out of bounds" t sp s

let span_iter f t (sp : span) =
  if sp.len > 0 then begin
    if sp.start < 0 || sp.start + sp.len > t.total_len then
      invalid_arg "Zbuf.span_iter: span out of bounds";
    let start_idx = find_frag_idx t sp.start in
    let rec iter idx pos remaining =
      if remaining > 0 then begin
        let frag = Dynarray.get t.fragments idx in
        let pos_in_frag = pos - frag.base in
        let avail = frag.len - pos_in_frag in
        let to_iter = min avail remaining in
        for i = 0 to to_iter - 1 do
          f (Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i))
        done;
        iter (idx + 1) (pos + to_iter) (remaining - to_iter)
      end
    in
    iter start_idx sp.start sp.len
  end

let span_fold f t (sp : span) init =
  if sp.len = 0 then init
  else begin
    if sp.start < 0 || sp.start + sp.len > t.total_len then
      invalid_arg "Zbuf.span_fold: span out of bounds";
    let start_idx = find_frag_idx t sp.start in
    let rec fold acc idx pos remaining =
      if remaining <= 0 then acc
      else begin
        let frag = Dynarray.get t.fragments idx in
        let pos_in_frag = pos - frag.base in
        let avail = frag.len - pos_in_frag in
        let to_fold = min avail remaining in
        let rec fold_bytes acc i =
          if i >= to_fold then acc
          else fold_bytes (f acc (Bytes.unsafe_get frag.data (frag.off + pos_in_frag + i))) (i + 1)
        in
        let acc' = fold_bytes acc 0 in
        fold acc' (idx + 1) (pos + to_fold) (remaining - to_fold)
      end
    in
    fold init start_idx sp.start sp.len
  end

let span_blit t (sp : span) dst ~dst_off =
  if sp.len > 0 then begin
    if sp.start < 0 || sp.start + sp.len > t.total_len then
      invalid_arg "Zbuf.span_blit: span out of bounds";
    if dst_off < 0 || dst_off + sp.len > Bytes.length dst then
      invalid_arg "Zbuf.span_blit: destination out of bounds";
    let start_idx = find_frag_idx t sp.start in
    let rec blit d_off idx pos remaining =
      if remaining > 0 then begin
        let frag = Dynarray.get t.fragments idx in
        let pos_in_frag = pos - frag.base in
        let avail = frag.len - pos_in_frag in
        let to_copy = min avail remaining in
        Bytes.blit frag.data (frag.off + pos_in_frag) dst d_off to_copy;
        blit (d_off + to_copy) (idx + 1) (pos + to_copy) (remaining - to_copy)
      end
    in
    blit dst_off start_idx sp.start sp.len
  end
