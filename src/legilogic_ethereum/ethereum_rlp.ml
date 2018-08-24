(* ethereum_rlp.ml -- Ethereum's notion of RLP ("Recursive Length Prefix") encoding *)
(* reference:

   https://github.com/ethereum/wiki/wiki/RLP
*)

open Legilogic_lib
open Lib
open Hex

type rlp_item = RlpItem of string | RlpItems of rlp_item list

type t = RlpEncoding of string [@@deriving show]

(* convert number to string, used for
   - encoding lengths within RLP-encodings
   - encoding integers to string, which can then be RLP-encoded
*)
let rec encode_int_as_string n =
  if n == 0 then "" else encode_int_as_string (n / 0x100) ^ String.make 1 (Char.chr (n mod 0x100))

(* encode length of string to prepend to RLP-encoding *)
let encode_length len offset =
  if len <= 55 then String.make 1 (Char.chr (len + offset))
  else
    let len_string = encode_int_as_string len in
    let first_byte = String.make 1 (Char.chr (String.length len_string + offset + 55)) in
    first_byte ^ len_string

let rec encode data =
  match data with RlpItem _ -> rlp_encode_item data | RlpItems _ -> rlp_encode_items data

and rlp_encode_item = function
  | RlpItem item ->
    let len = String.length item in
    (* For a single byte whose value is in the [0x00, 0x7f] range, that byte is its own RLP encoding. *)
    if len == 1 && Char.code item.[0] <= 0x7F then RlpEncoding item
    else
      let encoded_length = encode_length len 0x80 in
      RlpEncoding (encoded_length ^ item)
  | RlpItems _ -> bork "Expected single item to RLP-encode, got list of items"

and rlp_encode_items = function
  | RlpItem _ -> bork "Expected list of items to RLP-encode, got single item"
  | RlpItems items ->
    let encodings = List.map encoded_string items in
    let merged_encodings = String.concat "" encodings in
    let encoded_length = encode_length (String.length merged_encodings) 0xC0 in
    RlpEncoding (encoded_length ^ merged_encodings)

and to_string (RlpEncoding s) = s

and encoded_string r = r |> encode |> to_string

let encode_string s = rlp_encode_item (RlpItem s)

let encode_bytes bytes = encode_string (Bytes.to_string bytes)

let encode_int n = encode_string (encode_int_as_string n)

(* decoding *)
(* inverse of encode_int_as_string *)
let rec decode_int_string n =
  let len = String.length n in
  if len = 0 then 0
  else if len = 1 then Char.code n.[0]
  else Char.code n.[len - 1] + (decode_int_string (String.sub n 0 (len - 1)) * 0x100)

type rlp_length_bounds = {start: int; length: int}

type rlp_decoded_length =
  | RlpItemLengthDecoded of rlp_length_bounds
  | RlpItemsLengthDecoded of rlp_length_bounds

(* decode encoded length, determining whether we have an item or items *)
let decode_length input =
  let len = String.length input in
  if len = 0 then bork "decode_length: empty input"
  else
    let prefix = Char.code input.[0] in
    if prefix <= 0x7F then RlpItemLengthDecoded {start= 0; length= 1}
    else if prefix <= 0xB7 && len > prefix - 0x80 then
      RlpItemLengthDecoded {start= 1; length= prefix - 0x80}
    else if
      prefix <= 0xbf
      && len > prefix - 0xb7
      && len > prefix - 0xB7 + decode_int_string (String.sub input 1 (prefix - 0xB7))
    then
      let len_of_strlen = prefix - 0xB7 in
      let strlen = decode_int_string (String.sub input 1 len_of_strlen) in
      RlpItemLengthDecoded {start= 1 + len_of_strlen; length= strlen}
    else if prefix <= 0xF7 && len > prefix - 0xC0 then
      RlpItemsLengthDecoded {start= 1; length= prefix - 0xC0}
    else if
      prefix <= 0xFF
      && len > prefix - 0xF7
      && len > prefix - 0xF7 + decode_int_string (String.sub input 1 (prefix - 0xF7))
    then
      let len_of_items_len = prefix - 0xF7 in
      let items_len = decode_int_string (String.sub input 1 len_of_items_len) in
      RlpItemsLengthDecoded {start= 1 + len_of_items_len; length= items_len}
    else bork "decode_length: nonconforming RLP encoding"

(* entry point for RLP decoding *)
let decode (RlpEncoding s as encoding) =
  (* for string, return the decoded part paired with unconsumed part of the string *)
  let rec decode_with_leftover s =
    let len = String.length s in
    if len = 0 then (RlpItem "", "")
    else
      match decode_length s with
      | RlpItemLengthDecoded {start; length} ->
        let decoded_len = start + length in
        (* prefix and data *)
        (RlpItem (String.sub s start length), String.sub s decoded_len (len - decoded_len))
      | RlpItemsLengthDecoded {start; length} ->
        let rec item_loop s0 accum =
          (* decode items until string consumed *)
          if s0 = "" then List.rev accum
          else
            let item, leftover = decode_with_leftover s0 in
            item_loop leftover (item :: accum)
        in
        let decoded_len = start + length in
        (* prefix and data for all items *)
        let items = String.sub s start length in
        (RlpItems (item_loop items []), String.sub s decoded_len (len - decoded_len))
  in
  let decoded, leftover = decode_with_leftover s in
  if leftover = "" then decoded
  else bork "For encoding: %s, got leftover data: %s" (show encoding) leftover

module Test = struct
  (* tests of encoding, from reference given at top *)

  let%test "empty_string_rlp" = encode_string "" = RlpEncoding (String.make 1 (Char.chr 0x80))

  let%test "dog_string_rlp" = encode_string "dog" = RlpEncoding "ƒdog"

  (* from https://github.com/ethereum/tests/blob/develop/RLPTests/rlptest.json *)
  let%test "short_list_rlp" =
    let items = List.map (fun it -> RlpItem it) ["dog"; "god"; "cat"] in
    to_string (encode (RlpItems items))
    = parse_0x_string "0xcc83646f6783676f6483636174"

  (* from https://github.com/ethereum/tests/blob/develop/RLPTests/rlptest.json *)
  let%test "dict_rlp" =
    let mk_row its = RlpItems (List.map (fun it -> RlpItem it) its) in
    let row1 = mk_row ["key1"; "val1"] in
    let row2 = mk_row ["key2"; "val2"] in
    let row3 = mk_row ["key3"; "val3"] in
    let row4 = mk_row ["key4"; "val4"] in
    let items = RlpItems [row1; row2; row3; row4] in
    to_string (encode items)
    = parse_0x_string
        "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34"

  let%test "small_int_rlp" =
    to_string (encode_int 1000) = parse_0x_string "0x8203e8"

  let%test "cat_dog_rlp" =
    let cat_item = RlpItem "cat" in
    let dog_item = RlpItem "dog" in
    let items = RlpItems [cat_item; dog_item] in
    rlp_encode_items items = RlpEncoding "Èƒcatƒdog"

  let%test "empty_list_rlp" =
    let empty_list = RlpItems [] in
    rlp_encode_items empty_list = RlpEncoding "À"

  let%test "two_set_rlp" =
    let zero = RlpItems [] in
    let one = RlpItems [zero] in
    let two = RlpItems [zero; one; RlpItems [zero; one]] in
    rlp_encode_items two = RlpEncoding "ÇÀÁÀÃÀÁÀ"

  let%test "fifteen_rlp" = encode_int 15 = RlpEncoding "\015"

  let%test "kilo_rlp" = encode_int 1024 = RlpEncoding "‚\004\000"

  let%test "latin_rlp" =
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit" in
    encode_string s = RlpEncoding ("¸8" ^ s)

  (* tests of int (not rlp) encoding / decoding *)

  let test_int_encoding n = decode_int_string (encode_int_as_string n) = n

  let%test "int_encoding_inverse_1" = test_int_encoding 42

  let%test "int_encoding_inverse_2" = test_int_encoding 1024

  let%test "int_encoding_inverse_3" = test_int_encoding 1000000

  let%test "int_encoding_inverse_4" = test_int_encoding 0

  let%test "int_encoding_inverse_5" = test_int_encoding 1

  (* test that encode and decode are inverses *)

  let make_rlp_encode_decode_test item = decode (encode item) = item

  let%test "encode_decode_1" = make_rlp_encode_decode_test (RlpItem "this is a test")

  let%test "encode_decode_2" =
    make_rlp_encode_decode_test
      (RlpItem
         "a very long string a very long string a very long string a very long string a very long string a very long string a very long string")

  let%test "encode_decode_3" =
    make_rlp_encode_decode_test
      (RlpItems
         [ RlpItem "something"
         ; RlpItems
             [RlpItem "anything"; RlpItems [RlpItem "une chose quelconque"; RlpItem "un truc"]] ])

  let%test "encode_decode_4" = make_rlp_encode_decode_test (RlpItems [])

  let%test "encode_decode_5" = make_rlp_encode_decode_test (RlpItem "")

  let%test "encode_decode_6" =
    make_rlp_encode_decode_test
      (RlpItems
         [ RlpItems
             [ RlpItems
                 [ RlpItems
                     [ RlpItems
                         [RlpItems [RlpItems [RlpItems [RlpItems [RlpItem "hi, I'm a leaf"]]]]] ]
                 ] ] ])

  let%test "encode-list" =
    (* based on example at https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f;
       the expected value below is what the NodeJS implementation gives *)
    let items =
      [ "0x0"
      ; "0x04a817c800"
      ; "0x0186a0"
      ; "0x687422eea2cb73b5d3e242ba5456b782919afc85"
      ; "0x03e8"
      ; "0xc0de"
      ; "0x1c"
      ; "0x668ed6500efd75df7cb9c9b9d8152292a75453ec2d11030b0eec42f6a7ace602"
      ; "0x3efcbbf4d53e0dfa4fde5c6d9a73221418652abc66dff7fddd78b81cc28b9fbf" ]
    in
    let rlp_items = RlpItems (List.map (fun it -> RlpItem (parse_0x_string it)) items) in
    let rlp = to_string (encode rlp_items) in
    let expected_rlp =
      "0xf869808504a817c800830186a094687422eea2cb73b5d3e242ba5456b782919afc858203e882c0de1ca0668ed6500efd75df7cb9c9b9d8152292a75453ec2d11030b0eec42f6a7ace602a03efcbbf4d53e0dfa4fde5c6d9a73221418652abc66dff7fddd78b81cc28b9fbf"
    in
    unparse_0x_string rlp = expected_rlp
end
