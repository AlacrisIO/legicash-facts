open Ppx_deriving_rlp_runtime
open Rlptest_util
open OUnit2

(* Read the tests from
   https://github.com/ethereum/tests/blob/develop/RLPTests/rlptest.json
   and run them.
   *)

let rlptest_json_file = Filename.concat rlptest_json_dir "rlptest.json"
let rlptest_json      = Yojson.Safe.from_file rlptest_json_file

(* --------------------------------------------------------------- *)

(* Generating test cases *)

let rlptest_case (name, in_out) =
  let in_json  = Yojson.Safe.Util.member "in" in_out
  and out_json = Yojson.Safe.Util.member "out" in_out in
  let in_rlp_item    = json_to_rlp_item in_json
  and out_rlp_string = json_hex_string_to_byte_string out_json in
  let test ctxt =
    assert_equal ~ctxt
                 ~printer:show_string
                 out_rlp_string
                 (Rlp_encode.rlp_item_to_rlp in_rlp_item);
    assert_equal ~ctxt
                 ~printer:Rlp.show_rlp_item
                 in_rlp_item
                 (Rlp_decode.rlp_item_of_rlp out_rlp_string)
  in
    name >:: test

let rlptest_suite json =
  match json with
  | `Assoc assoc -> "rlptest" >::: (List.map rlptest_case assoc)
  | _            -> failwith "expected rlptest.json to be a Json Object"

(* --------------------------------------------------------------- *)

(* Running the tests using rlptest.json as the input *)

let () = run_test_tt_main (rlptest_suite rlptest_json)

