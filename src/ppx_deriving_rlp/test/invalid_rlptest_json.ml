open Ppx_deriving_rlp_runtime
open Rlptest_util
open OUnit2

(* Read the tests from
   https://github.com/ethereum/tests/blob/develop/RLPTests/invalidRLPTest.json
   and run them.
   *)

let invalid_rlptest_json_file = Filename.concat rlptest_json_dir "invalidRLPTest.json"
let invalid_rlptest_json      = Yojson.Safe.from_file invalid_rlptest_json_file

(* --------------------------------------------------------------- *)

(* Generating test cases *)

let invalid_rlptest_case (name, in_out) =
  let in_json  = Yojson.Safe.Util.member "in" in_out
  and out_json = Yojson.Safe.Util.member "out" in_out in
  (if not (in_json = `String "INVALID") then failwith "expected `INVALID`");
  let out_rlp_string = json_hex_string_to_byte_string out_json in
  let test ctxt =
    (* check that decoding fails *)
    assert_equal ~ctxt
                 ~printer:[%show: Rlp.rlp_item option]
                 None
                 (Rlp_decode.rlp_item_of_rlp_opt out_rlp_string);
    (* check that the exception raised contains the invalid string *)
    try
      ignore (Rlp_decode.rlp_item_of_rlp out_rlp_string);
      assert_failure "expected RLP decoding to fail"
    with
    | Rlp.Rlp_unmarshaling_error (_, _, actual_string) ->
      assert_equal ~ctxt ~printer:show_string out_rlp_string actual_string
  in
    name >:: test

let invalid_rlptest_suite json =
  match json with
  | `Assoc assoc -> "invalid_rlptest" >::: (List.map invalid_rlptest_case assoc)
  | _            -> failwith "expected rlptest.json to be a Json Object"

(* --------------------------------------------------------------- *)

(* Running the tests using rlptest.json as the input *)

let () = run_test_tt_main (invalid_rlptest_suite invalid_rlptest_json)

