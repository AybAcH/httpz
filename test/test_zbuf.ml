(* test_zbuf.ml - Tests for zero-copy multi-buffer spans (OxCaml version) *)

open Zbuf

(* ============================================================
   Test Utilities
   ============================================================ *)

let assert_equal ~msg expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL: %s\n  expected: %s\n  actual: %s\n"
      msg (String.escaped expected) (String.escaped actual);
    assert false
  end

let assert_equal_int ~msg expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL: %s\n  expected: %d\n  actual: %d\n"
      msg expected actual;
    assert false
  end

let assert_equal_char ~msg expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL: %s\n  expected: %c\n  actual: %c\n"
      msg expected actual;
    assert false
  end

let assert_true ~msg cond =
  if not cond then begin
    Printf.printf "FAIL: %s\n" msg;
    assert false
  end

let assert_raises ~msg f =
  try
    f ();
    Printf.printf "FAIL: %s - expected exception\n" msg;
    assert false
  with Invalid_argument _ -> ()

let pass msg = Printf.printf "PASS: %s\n" msg

(* ============================================================
   Basic Buffer Tests
   ============================================================ *)

let test_create () =
  let zb = create () in
  assert_equal_int ~msg:"initial length" 0 (length zb);
  assert_equal_int ~msg:"initial limit" 0 (limit zb);
  assert_equal_int ~msg:"initial fragment count" 0 (fragment_count zb);
  pass "create"

let test_push_single () =
  let zb = create () in
  let data = Bytes.of_string "hello" in
  push zb data ~off:0 ~len:5;
  assert_equal_int ~msg:"length after push" 5 (length zb);
  assert_equal_int ~msg:"fragment count" 1 (fragment_count zb);
  pass "push_single"

let test_push_multiple () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  push zb (Bytes.of_string " world") ~off:0 ~len:6;
  push zb (Bytes.of_string "!") ~off:0 ~len:1;
  assert_equal_int ~msg:"total length" 12 (length zb);
  assert_equal_int ~msg:"fragment count" 3 (fragment_count zb);
  pass "push_multiple"

let test_push_with_offset () =
  let zb = create () in
  let data = Bytes.of_string "XXXhelloYYY" in
  push zb data ~off:3 ~len:5;
  assert_equal_int ~msg:"length" 5 (length zb);
  assert_equal_char ~msg:"first char" 'h' (get zb 0);
  assert_equal_char ~msg:"last char" 'o' (get zb 4);
  pass "push_with_offset"

let test_push_empty () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:0;
  assert_equal_int ~msg:"length after empty push" 0 (length zb);
  (* len=0 push still adds a fragment (EOF marker convention) *)
  assert_equal_int ~msg:"fragment count" 1 (fragment_count zb);
  pass "push_empty"

let test_push_invalid () =
  let zb = create () in
  let data = Bytes.of_string "hello" in
  assert_raises ~msg:"negative offset" (fun () -> push zb data ~off:(-1) ~len:3);
  assert_raises ~msg:"negative length" (fun () -> push zb data ~off:0 ~len:(-1));
  assert_raises ~msg:"offset + len > size" (fun () -> push zb data ~off:3 ~len:5);
  pass "push_invalid"

(* ============================================================
   Direct Access Tests
   ============================================================ *)

let test_get_single_fragment () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  assert_equal_char ~msg:"get 0" 'h' (get zb 0);
  assert_equal_char ~msg:"get 1" 'e' (get zb 1);
  assert_equal_char ~msg:"get 4" 'o' (get zb 4);
  pass "get_single_fragment"

let test_get_multiple_fragments () =
  let zb = create () in
  push zb (Bytes.of_string "hel") ~off:0 ~len:3;
  push zb (Bytes.of_string "lo ") ~off:0 ~len:3;
  push zb (Bytes.of_string "world") ~off:0 ~len:5;
  (* "hello world" across 3 fragments *)
  assert_equal_char ~msg:"get 0" 'h' (get zb 0);
  assert_equal_char ~msg:"get 2" 'l' (get zb 2);
  assert_equal_char ~msg:"get 3" 'l' (get zb 3);  (* start of fragment 2 *)
  assert_equal_char ~msg:"get 5" ' ' (get zb 5);
  assert_equal_char ~msg:"get 6" 'w' (get zb 6);  (* start of fragment 3 *)
  assert_equal_char ~msg:"get 10" 'd' (get zb 10);
  pass "get_multiple_fragments"

let test_get_out_of_bounds () =
  let zb = create () in
  push zb (Bytes.of_string "abc") ~off:0 ~len:3;
  assert_raises ~msg:"get negative" (fun () -> ignore (get zb (-1)));
  assert_raises ~msg:"get past end" (fun () -> ignore (get zb 3));
  assert_raises ~msg:"get way past end" (fun () -> ignore (get zb 100));
  pass "get_out_of_bounds"

(* ============================================================
   Cursor Tests
   Note: Using cursor_global for tests since local cursors
   must not escape their scope.
   ============================================================ *)

let test_cursor_basic () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:0 in
  assert_equal_int ~msg:"initial pos" 0 (cursor_pos cur);
  assert_equal_int ~msg:"initial mark" 0 (cursor_marked cur);
  assert_equal_int ~msg:"remaining" 5 (cursor_remaining zb cur);
  pass "cursor_basic"

let test_cursor_at_end () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:5 in
  assert_equal_int ~msg:"pos at end" 5 (cursor_pos cur);
  assert_equal_int ~msg:"remaining at end" 0 (cursor_remaining zb cur);
  pass "cursor_at_end"

let test_cursor_set () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:0 in
  cursor_set zb cur 3;
  assert_equal_int ~msg:"pos after set" 3 (cursor_pos cur);
  assert_equal_int ~msg:"remaining after set" 2 (cursor_remaining zb cur);
  pass "cursor_set"

let test_cursor_mark () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:0 in
  advance cur 2;
  cursor_mark cur;
  assert_equal_int ~msg:"marked pos" 2 (cursor_marked cur);
  advance cur 2;
  assert_equal_int ~msg:"current pos" 4 (cursor_pos cur);
  assert_equal_int ~msg:"mark unchanged" 2 (cursor_marked cur);
  pass "cursor_mark"

let test_cursor_advance () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:0 in
  advance cur 2;
  assert_equal_int ~msg:"pos after advance" 2 (cursor_pos cur);
  advance cur 3;
  assert_equal_int ~msg:"pos after second advance" 5 (cursor_pos cur);
  pass "cursor_advance"

let test_cursor_peek () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let cur = cursor_global zb ~pos:0 in
  assert_equal_char ~msg:"peek 0" 'h' (peek zb cur 0);
  assert_equal_char ~msg:"peek 1" 'e' (peek zb cur 1);
  assert_equal_char ~msg:"peek 4" 'o' (peek zb cur 4);
  advance cur 2;
  assert_equal_char ~msg:"peek 0 after advance" 'l' (peek zb cur 0);
  pass "cursor_peek"

let test_cursor_peek_across_fragments () =
  let zb = create () in
  push zb (Bytes.of_string "hel") ~off:0 ~len:3;
  push zb (Bytes.of_string "lo") ~off:0 ~len:2;
  let cur = cursor_global zb ~pos:2 in
  assert_equal_char ~msg:"peek 0 (in frag 1)" 'l' (peek zb cur 0);
  assert_equal_char ~msg:"peek 1 (in frag 2)" 'l' (peek zb cur 1);
  assert_equal_char ~msg:"peek 2 (in frag 2)" 'o' (peek zb cur 2);
  pass "cursor_peek_across_fragments"

let test_cursor_take () =
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  let cur = cursor_global zb ~pos:0 in
  cursor_mark cur;
  let sp = take cur 5 in
  (* Access unboxed span fields with .# *)
  assert_equal_int ~msg:"span start" 0 sp.#start;
  assert_equal_int ~msg:"span len" 5 sp.#len;
  assert_equal_int ~msg:"cursor pos after take" 5 (cursor_pos cur);
  assert_equal_int ~msg:"mark after take" 5 (cursor_marked cur);
  pass "cursor_take"

let test_multiple_cursors () =
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  let cur1 = cursor_global zb ~pos:0 in
  let cur2 = cursor_global zb ~pos:6 in
  (* Cursors are independent *)
  advance cur1 5;
  assert_equal_int ~msg:"cur1 pos" 5 (cursor_pos cur1);
  assert_equal_int ~msg:"cur2 pos unchanged" 6 (cursor_pos cur2);
  assert_equal_char ~msg:"cur1 peek" ' ' (peek zb cur1 0);
  assert_equal_char ~msg:"cur2 peek" 'w' (peek zb cur2 0);
  pass "multiple_cursors"

(* ============================================================
   Span Tests (Unboxed)
   ============================================================ *)

let test_span_construction () =
  let sp = span ~start:5 ~len:10 in
  (* Access unboxed span fields with .# *)
  assert_equal_int ~msg:"span start" 5 sp.#start;
  assert_equal_int ~msg:"span len from field" 10 sp.#len;
  assert_equal_int ~msg:"span_len" 10 (span_len sp);
  pass "span_construction"

let test_span_since_mark () =
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  let cur = cursor_global zb ~pos:0 in
  cursor_mark cur;
  advance cur 5;
  let sp = span_since_mark cur in
  assert_equal_int ~msg:"span start" 0 sp.#start;
  assert_equal_int ~msg:"span len" 5 sp.#len;
  pass "span_since_mark"

let test_span_empty () =
  let sp = span_empty 42 in
  assert_equal_int ~msg:"empty span start" 42 sp.#start;
  assert_equal_int ~msg:"empty span len" 0 sp.#len;
  pass "span_empty"

let test_span_to_string_single_fragment () =
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  let sp1 = span ~start:0 ~len:5 in
  assert_equal ~msg:"span 1" "hello" (span_to_string zb sp1);
  let sp2 = span ~start:6 ~len:5 in
  assert_equal ~msg:"span 2" "world" (span_to_string zb sp2);
  let sp3 = span ~start:0 ~len:11 in
  assert_equal ~msg:"span 3" "hello world" (span_to_string zb sp3);
  pass "span_to_string_single_fragment"

let test_span_to_string_across_fragments () =
  let zb = create () in
  push zb (Bytes.of_string "hel") ~off:0 ~len:3;
  push zb (Bytes.of_string "lo ") ~off:0 ~len:3;
  push zb (Bytes.of_string "world") ~off:0 ~len:5;
  (* Span within first fragment *)
  let sp1 = span ~start:0 ~len:2 in
  assert_equal ~msg:"within frag 1" "he" (span_to_string zb sp1);
  (* Span crossing fragments 1 and 2 *)
  let sp2 = span ~start:2 ~len:3 in
  assert_equal ~msg:"crossing frag 1-2" "llo" (span_to_string zb sp2);
  (* Span crossing all three fragments *)
  let sp3 = span ~start:1 ~len:9 in
  assert_equal ~msg:"crossing all" "ello worl" (span_to_string zb sp3);
  (* Full span *)
  let sp4 = span ~start:0 ~len:11 in
  assert_equal ~msg:"full" "hello world" (span_to_string zb sp4);
  pass "span_to_string_across_fragments"

let test_span_to_string_empty () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let sp = span ~start:2 ~len:0 in
  assert_equal ~msg:"empty span" "" (span_to_string zb sp);
  pass "span_to_string_empty"

let test_span_equal_single_fragment () =
  let zb = create () in
  push zb (Bytes.of_string "GET /path HTTP/1.1") ~off:0 ~len:18;
  let sp = span ~start:0 ~len:3 in
  assert_true ~msg:"equal GET" (span_equal zb sp "GET");
  assert_true ~msg:"not equal POST" (not (span_equal zb sp "POST"));
  assert_true ~msg:"not equal GE" (not (span_equal zb sp "GE"));
  assert_true ~msg:"not equal GETT" (not (span_equal zb sp "GETT"));
  pass "span_equal_single_fragment"

let test_span_equal_across_fragments () =
  let zb = create () in
  push zb (Bytes.of_string "GE") ~off:0 ~len:2;
  push zb (Bytes.of_string "T ") ~off:0 ~len:2;
  let sp = span ~start:0 ~len:3 in
  assert_true ~msg:"equal GET across frags" (span_equal zb sp "GET");
  assert_true ~msg:"not equal POST" (not (span_equal zb sp "POST"));
  pass "span_equal_across_fragments"

let test_span_equal_caseless () =
  let zb = create () in
  push zb (Bytes.of_string "Content-Type") ~off:0 ~len:12;
  let sp = span ~start:0 ~len:12 in
  assert_true ~msg:"exact" (span_equal_caseless zb sp "Content-Type");
  assert_true ~msg:"lowercase" (span_equal_caseless zb sp "content-type");
  assert_true ~msg:"uppercase" (span_equal_caseless zb sp "CONTENT-TYPE");
  assert_true ~msg:"mixed" (span_equal_caseless zb sp "cOnTeNt-TyPe");
  assert_true ~msg:"not equal" (not (span_equal_caseless zb sp "Content-Length"));
  pass "span_equal_caseless"

let test_span_equal_caseless_across_fragments () =
  let zb = create () in
  push zb (Bytes.of_string "Cont") ~off:0 ~len:4;
  push zb (Bytes.of_string "ent-") ~off:0 ~len:4;
  push zb (Bytes.of_string "Type") ~off:0 ~len:4;
  let sp = span ~start:0 ~len:12 in
  assert_true ~msg:"caseless across frags" (span_equal_caseless zb sp "content-type");
  pass "span_equal_caseless_across_fragments"

let test_span_iter () =
  let zb = create () in
  push zb (Bytes.of_string "ab") ~off:0 ~len:2;
  push zb (Bytes.of_string "cd") ~off:0 ~len:2;
  let sp = span ~start:0 ~len:4 in
  let chars = ref [] in
  span_iter (fun c -> chars := c :: !chars) zb sp;
  let result = List.rev !chars |> List.to_seq |> String.of_seq in
  assert_equal ~msg:"iter result" "abcd" result;
  pass "span_iter"

let test_span_fold () =
  let zb = create () in
  push zb (Bytes.of_string "12") ~off:0 ~len:2;
  push zb (Bytes.of_string "34") ~off:0 ~len:2;
  let sp = span ~start:0 ~len:4 in
  let sum = span_fold (fun acc c -> acc + Char.code c - Char.code '0') zb sp 0 in
  assert_equal_int ~msg:"fold sum" 10 sum;  (* 1+2+3+4 *)
  pass "span_fold"

let test_span_blit () =
  let zb = create () in
  push zb (Bytes.of_string "hel") ~off:0 ~len:3;
  push zb (Bytes.of_string "lo") ~off:0 ~len:2;
  let sp = span ~start:0 ~len:5 in
  let dst = Bytes.create 10 in
  Bytes.fill dst 0 10 'X';
  span_blit zb sp dst ~dst_off:2;
  assert_equal ~msg:"blit result" "XXhelloXXX" (Bytes.to_string dst);
  pass "span_blit"

let test_span_blit_partial () =
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  let sp = span ~start:6 ~len:5 in
  let dst = Bytes.create 5 in
  span_blit zb sp dst ~dst_off:0;
  assert_equal ~msg:"blit partial" "world" (Bytes.to_string dst);
  pass "span_blit_partial"

let test_span_out_of_bounds () =
  let zb = create () in
  push zb (Bytes.of_string "hello") ~off:0 ~len:5;
  let sp = span ~start:3 ~len:5 in  (* exceeds buffer *)
  assert_raises ~msg:"span_to_string oob" (fun () -> ignore (span_to_string zb sp));
  (* span_equal with matching length to trigger bounds check *)
  assert_raises ~msg:"span_equal oob" (fun () -> ignore (span_equal zb sp "xxxxx"));
  pass "span_out_of_bounds"

(* ============================================================
   Fragment Growth Tests
   ============================================================ *)

let test_many_fragments () =
  let zb = create () in
  (* Push more than initial capacity (64) fragments *)
  for i = 0 to 100 do
    let s = string_of_int i in
    push zb (Bytes.of_string s) ~off:0 ~len:(String.length s);
  done;
  assert_equal_int ~msg:"fragment count" 101 (fragment_count zb);
  (* Verify we can still access data *)
  assert_equal_char ~msg:"first char" '0' (get zb 0);
  pass "many_fragments"

(* ============================================================
   Edge Cases
   ============================================================ *)

let test_single_byte_fragments () =
  let zb = create () in
  let s = "hello" in
  for i = 0 to String.length s - 1 do
    push zb (Bytes.of_string (String.make 1 s.[i])) ~off:0 ~len:1;
  done;
  assert_equal_int ~msg:"length" 5 (length zb);
  assert_equal_int ~msg:"fragment count" 5 (fragment_count zb);
  let sp = span ~start:0 ~len:5 in
  assert_equal ~msg:"reassembled" "hello" (span_to_string zb sp);
  pass "single_byte_fragments"

let test_cursor_fragment_cache () =
  (* Test that cursor fragment caching works correctly when moving around *)
  let zb = create () in
  push zb (Bytes.of_string "aaaa") ~off:0 ~len:4;
  push zb (Bytes.of_string "bbbb") ~off:0 ~len:4;
  push zb (Bytes.of_string "cccc") ~off:0 ~len:4;
  let cur = cursor_global zb ~pos:0 in
  (* Move forward through fragments *)
  assert_equal_char ~msg:"at 0" 'a' (peek zb cur 0);
  cursor_set zb cur 4;
  assert_equal_char ~msg:"at 4" 'b' (peek zb cur 0);
  cursor_set zb cur 8;
  assert_equal_char ~msg:"at 8" 'c' (peek zb cur 0);
  (* Move backward *)
  cursor_set zb cur 2;
  assert_equal_char ~msg:"back to 2" 'a' (peek zb cur 0);
  cursor_set zb cur 6;
  assert_equal_char ~msg:"to 6" 'b' (peek zb cur 0);
  pass "cursor_fragment_cache"

let test_empty_buffer_operations () =
  let zb = create () in
  assert_equal_int ~msg:"empty length" 0 (length zb);
  assert_raises ~msg:"get on empty" (fun () -> ignore (get zb 0));
  (* Can create cursor at pos 0 on empty buffer *)
  let cur = cursor_global zb ~pos:0 in
  assert_equal_int ~msg:"cursor pos" 0 (cursor_pos cur);
  assert_equal_int ~msg:"cursor remaining" 0 (cursor_remaining zb cur);
  pass "empty_buffer_operations"

(* ============================================================
   Local Cursor Test
   ============================================================ *)

let test_local_cursor () =
  (* Test that local cursor creation works correctly *)
  let zb = create () in
  push zb (Bytes.of_string "hello world") ~off:0 ~len:11;
  (* Use local cursor within a function scope *)
  let result =
    let local_ cur = cursor zb ~pos:0 in
    cursor_mark cur;
    advance cur 5;
    let sp = span_since_mark cur in
    span_to_string zb sp
  in
  assert_equal ~msg:"local cursor parse" "hello" result;
  pass "local_cursor"

(* ============================================================
   Run All Tests
   ============================================================ *)

let () =
  Printf.printf "\n=== Zbuf Tests (OxCaml) ===\n\n";

  Printf.printf "--- Basic Buffer Tests ---\n";
  test_create ();
  test_push_single ();
  test_push_multiple ();
  test_push_with_offset ();
  test_push_empty ();
  test_push_invalid ();

  Printf.printf "\n--- Direct Access Tests ---\n";
  test_get_single_fragment ();
  test_get_multiple_fragments ();
  test_get_out_of_bounds ();

  Printf.printf "\n--- Cursor Tests ---\n";
  test_cursor_basic ();
  test_cursor_at_end ();
  test_cursor_set ();
  test_cursor_mark ();
  test_cursor_advance ();
  test_cursor_peek ();
  test_cursor_peek_across_fragments ();
  test_cursor_take ();
  test_multiple_cursors ();

  Printf.printf "\n--- Span Tests (Unboxed) ---\n";
  test_span_construction ();
  test_span_since_mark ();
  test_span_empty ();
  test_span_to_string_single_fragment ();
  test_span_to_string_across_fragments ();
  test_span_to_string_empty ();
  test_span_equal_single_fragment ();
  test_span_equal_across_fragments ();
  test_span_equal_caseless ();
  test_span_equal_caseless_across_fragments ();
  test_span_iter ();
  test_span_fold ();
  test_span_blit ();
  test_span_blit_partial ();
  test_span_out_of_bounds ();

  Printf.printf "\n--- Fragment Growth Tests ---\n";
  test_many_fragments ();

  Printf.printf "\n--- Edge Cases ---\n";
  test_single_byte_fragments ();
  test_cursor_fragment_cache ();
  test_empty_buffer_operations ();

  Printf.printf "\n--- Local Cursor Tests ---\n";
  test_local_cursor ();

  Printf.printf "\n=== All Zbuf tests passed! ===\n"
