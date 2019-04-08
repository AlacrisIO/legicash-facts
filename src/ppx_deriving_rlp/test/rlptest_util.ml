open Ppx_deriving_rlp_runtime

(* --------------------------------------------------------------- *)

(* Parsing the JSON inputs and outputs in the tests *)

let rec json_to_rlp_item json =
  match json with
  | `Int i    -> int_to_rlp_item i
  | `String s -> (* Strings that start with `#` are parsed as decimal numbers,
                    see mediumint4, mediumint5, and bigint for examples. *)
                 if (1 <= String.length s) && ("#" = String.sub s 0 1)
                 then z_to_rlp_item (Z.of_substring s ~pos:1 ~len:(String.length s - 1))
                 else string_to_rlp_item s
  | `List l   -> Rlp.RlpItems (List.map json_to_rlp_item l)
  | _         -> failwith ("invalid Json Rlp: " ^ Yojson.Safe.show json)

let json_hex_string_to_byte_string json =
  let hex_str = Yojson.Safe.Util.to_string json in
  (if not ((2 <= String.length hex_str) && ("0x" = String.sub hex_str 0 2))
   then failwith "expected `0x`");
  let n = ((String.length hex_str) - 2) / 2
  and byte_at i = Z.of_substring_base 16 hex_str ~pos:(2 + (i*2)) ~len:2 in
  let char_at i = Char.chr (Z.to_int (byte_at i)) in
  String.init n char_at

(* --------------------------------------------------------------- *)

(* Show function for test cases *)

let show_string s = "\"" ^ String.escaped s ^ "\""
