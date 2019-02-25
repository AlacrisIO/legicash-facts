open Ppx_deriving_rlp_runtime
open Rlp
open Rlp_encode
open Rlp_decode
open OUnit2

let show_string s = "\"" ^ String.escaped s ^ "\""

let check_rlp_str ~ctxt:ctxt rlp str =
  (* assert_equal expected actual *)
  assert_equal ~ctxt ~printer:show_string str (rlp_item_to_rlp rlp);
  assert_equal ~ctxt ~printer:show_rlp_item rlp (rlp_item_of_rlp str)

(* examples from the RLP section of the Ethereum wiki:
   https://github.com/ethereum/wiki/wiki/RLP *)
let wiki_ex1 ctxt = check_rlp_str ~ctxt (RlpItem "dog") "\x83dog"
let wiki_ex2 ctxt = check_rlp_str ~ctxt (RlpItems [RlpItem "cat"; RlpItem "dog"]) "\xc8\x83cat\x83dog"
let wiki_ex3 ctxt = check_rlp_str ~ctxt (RlpItem "") "\x80"
let wiki_ex4 ctxt = check_rlp_str ~ctxt (RlpItems []) "\xc0"
let wiki_ex5 ctxt = check_rlp_str ~ctxt (nat_to_rlp_item (Z.of_int 0)) "\x80"
let wiki_ex6 ctxt = check_rlp_str ~ctxt (nat_to_rlp_item (Z.of_int 15)) "\x0f"
let wiki_ex7 ctxt = check_rlp_str ~ctxt (nat_to_rlp_item (Z.of_int 1024)) "\x82\x04\x00"
let wiki_ex8 ctxt = check_rlp_str ~ctxt
                      (RlpItems [RlpItems []; RlpItems [RlpItems []]; RlpItems [RlpItems []; RlpItems [RlpItems []]]])
                      "\xc7\xc0\xc1\xc0\xc3\xc0\xc1\xc0"
let wiki_ex9 ctxt = check_rlp_str ~ctxt
                      (RlpItem "Lorem ipsum dolor sit amet, consectetur adipisicing elit")
                      "\xb8\x38Lorem ipsum dolor sit amet, consectetur adipisicing elit"

let test10 ctxt = let str = ("abcdefghi jklmnopqr stuvwxyz, 32abcdefghi jklmnopqr stuvwxyz, 64abcdefghi jklmnopqr stuvwxyz, 96" ^
                             "abcdefghi jklmnopqr stuvwxyz 128abcdefghi jklmnopqr stuvwxyz 160abcdefghi jklmnopqr stuvwxyz 192" ^
                             "abcdefghi jklmnopqr stuvwxyz 224abcdefghi jklmnopqr stuvwxyz 256abcdefghi jklmnopqr stuvwxyz 288" ^
                             "abcdefghi jklmnopqr stuvwxyz 320abcdefghi jklmnopqr stuvwxyz 352abcdefghi jklmnopqr stuvwxyz 384" ^
                             "abcdefghi jklmnopqr stuvwxyz 416abcdefghi jklmnopqr stuvwxyz 448abcdefghi jklmnopqr stuvwxyz 480" ^
                             "abcdefghi jklmnopqr stuvwxyz 512abcdefghi jklmnopqr stuvwxyz 544abcdefghi jklmnopqr stuvwxyz 576" ^
                             "abcdefghi jklmnopqr stuvwxyz 608abcdefghi jklmnopqr stuvwxyz 640abcdefghi jklmnopqr stuvwxyz 672" ^
                             "abcdefghi jklmnopqr stuvwxyz 704abcdefghi jklmnopqr stuvwxyz 736abcdefghi jklmnopqr stuvwxyz 768" ^
                             "abcdefghi jklmnopqr stuvwxyz 800abcdefghi jklmnopqr stuvwxyz 832abcdefghi jklmnopqr stuvwxyz 864" ^
                             "abcdefghi jklmnopqr stuvwxyz 896abcdefghi jklmnopqr stuvwxyz 928abcdefghi jklmnopqr stuvwxyz 960" ^
                             "abcdefghi jklmnopqr stuvwxyz 992abcdefghi jklmnopqr stuvwxyz1024")
                  in check_rlp_str ~ctxt (RlpItem str) ("\xb9\x04\x00" ^ str)

let test11 ctxt = check_rlp_str ~ctxt (RlpItem "2re3mi1do1do5sol") "\x902re3mi1do1do5sol"
let test12 ctxt = check_rlp_str ~ctxt (RlpItem "5sol7ti2re5sol1do") "\x915sol7ti2re5sol1do"

let suite =
"rlp_encode_test">:::
 ["wiki_ex1">:: wiki_ex1;
  "wiki_ex2">:: wiki_ex2;
  "wiki_ex3">:: wiki_ex3;
  "wiki_ex4">:: wiki_ex4;
  "wiki_ex5">:: wiki_ex5;
  "wiki_ex6">:: wiki_ex6;
  "wiki_ex7">:: wiki_ex7;
  "wiki_ex8">:: wiki_ex8;
  "wiki_ex9">:: wiki_ex9;
  "test10">:: test10;
  "test11">:: test11;
  "test12">:: test12]

let () = run_test_tt_main suite
