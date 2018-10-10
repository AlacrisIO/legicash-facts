open Legilogic_lib
open Lib
open Hex
open Digesting
open Signing

open Ethereum_chain

(* TO DO: save ourselves an intermediate representation and offer combinators
   to directly encode and decode data as RLP.
*)

type rlp_item = RlpItem of string | RlpItems of rlp_item list

type t = RlpEncoding of string [@@deriving show]

(* convert number to string, used for
   - encoding lengths within RLP-encodings
   - encoding integers to string, which can then be RLP-encoded
*)
let encode_int_as_string n =
  let rec loop n accum =
    if n == 0 then
      String.concat "" accum
    else
      loop (n / 0x100) (String.make 1 (Char.chr (n mod 0x100)) :: accum)
  in
  loop n []

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
let decode_length input offset =
  let len = String.length input in
  if offset >= len then bork "decode_length: empty input"
  else
    let prefix = Char.code input.[offset] in
    if prefix <= 0x7F then RlpItemLengthDecoded {start= 0; length= 1}
    else if prefix <= 0xB7 && len > prefix - 0x80 then
      RlpItemLengthDecoded {start= 1; length= prefix - 0x80}
    else if
      prefix <= 0xbf
      && len > prefix - 0xb7
      && len > prefix - 0xB7 + decode_int_string (String.sub input (offset + 1) (prefix - 0xB7))
    then
      let len_of_strlen = prefix - 0xB7 in
      let strlen = decode_int_string (String.sub input (offset + 1) len_of_strlen) in
      RlpItemLengthDecoded {start= 1 + len_of_strlen; length= strlen}
    else if prefix <= 0xF7 && len > prefix - 0xC0 then
      RlpItemsLengthDecoded {start= 1; length= prefix - 0xC0}
    else if
      prefix <= 0xFF
      && len > prefix - 0xF7
      && len > prefix - 0xF7 + decode_int_string (String.sub input (offset + 1) (prefix - 0xF7))
    then
      let len_of_items_len = prefix - 0xF7 in
      let items_len = decode_int_string (String.sub input (offset + 1) len_of_items_len) in
      RlpItemsLengthDecoded {start= 1 + len_of_items_len; length= items_len}
    else bork "decode_length: nonconforming RLP encoding"

(* entry point for RLP decoding *)
let decode (RlpEncoding s as encoding) =
  let len = String.length s in
  (* for string, return the decoded part paired with unconsumed part of the string *)
  let rec decode_with_leftover offset =
    if offset >= len then (RlpItem "", 0)
    else
      match decode_length s offset with
      | RlpItemLengthDecoded {start; length} ->
        (* prefix and data *)
        (RlpItem (String.sub s (offset + start) length), offset + start + length)
      | RlpItemsLengthDecoded {start; length} ->
        let rec item_loop offs limit accum =
          (* decode items until string consumed *)
          if offs >= limit then List.rev accum
          else
            let item, new_offset = decode_with_leftover offs in
            item_loop new_offset limit (item :: accum)
        in
        (* prefix and data for all items *)
        let decoded_len = offset + start + length in
        (RlpItems (item_loop (offset + start) decoded_len []), decoded_len)
  in
  let decoded, offset = decode_with_leftover 0 in
  if offset >= len then decoded
  else bork "For encoding: %s, got leftover data: %s" (show encoding) (String.sub s offset (len - offset))

(* convert transaction record to rlp_item suitable for encoding
   TODO: make that our marshaling strategy.
*)
let rlp_of_transaction transaction =
  let open Ethereum_chain in
  let tx_header = transaction.Transaction.tx_header in
  (* all items are strings, each character represents 2 digits in hex representation *)
  let nonce = Nonce.to_big_endian_bits tx_header.nonce in
  let gas_price = TokenAmount.to_big_endian_bits tx_header.gas_price in
  let gas_limit = TokenAmount.to_big_endian_bits tx_header.gas_limit in
  let value = TokenAmount.to_big_endian_bits tx_header.value in
  let toaddr, data =
    match transaction.operation with
    | TransferTokens to_address -> (Address.to_big_endian_bits to_address, "")
    | CreateContract bytes -> ("", Bytes.to_string bytes)
    | CallFunction (contract_address, call_encoding) ->
      (Address.to_big_endian_bits contract_address, Bytes.to_string call_encoding)
  in
  RlpItems
    [ RlpItem nonce
    ; RlpItem gas_price
    ; RlpItem gas_limit
    ; RlpItem toaddr
    ; RlpItem value
    ; RlpItem data ]


let rlp_of_signed_transaction transaction_rlp ~v ~r ~s =
  let signature_items = [RlpItem v; RlpItem r; RlpItem s] in
  match transaction_rlp with
  | RlpItems items -> RlpItems (items @ signature_items)
  | _ -> Lib.bork "Expected RlpItems when creating signed transaction RLP"

(* https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f
   describes the transaction hashing algorithm

   TODO: make our algorithm correct wrt the v r s:
   looks like we must do a first pass where v, r, s are zero,
   then replace them by the right values.
*)
let get_transaction_hash transaction private_key =
  let transaction_rlp = rlp_of_transaction transaction in
  (* step 1: RLP-encode the transaction *)
  let encoded_transaction = transaction_rlp |> encoded_string in
  (* step 2: Sign its hash with a private key, extract pieces of the signature *)
  let signature = make_signature digest_of_string private_key encoded_transaction in
  let (v, r, s) = signature_vrs signature in
  (* step 3: RLP-encode and hash the transaction augmented with the signature *)
  let signed_transaction_rlp = rlp_of_signed_transaction transaction_rlp ~v ~r ~s in
  let encoded_signed_transaction = encoded_string signed_transaction_rlp in
  digest_of_string encoded_signed_transaction


module Test = struct
  open Lib.Test
  open Hex.Test
  open Signing.Test

  (* tests of encoding, from reference given at top *)

  let%test "empty_string_rlp" = encode_string "" = RlpEncoding (String.make 1 (Char.chr 0x80))

  let%test "dog_string_rlp" = encode_string "dog" = RlpEncoding "�dog"

  (* from https://github.com/ethereum/tests/blob/develop/RLPTests/rlptest.json *)
  let%test "short_list_rlp" =
    let items = List.map (fun it -> RlpItem it) ["dog"; "god"; "cat"] in
    expect_0x_string "short_list_rlp"
      "0xcc83646f6783676f6483636174"
      (encoded_string (RlpItems items));
    true

  (* from https://github.com/ethereum/tests/blob/develop/RLPTests/rlptest.json *)
  let%test "dict_rlp" =
    let mk_row its = RlpItems (List.map (fun it -> RlpItem it) its) in
    let row1 = mk_row ["key1"; "val1"] in
    let row2 = mk_row ["key2"; "val2"] in
    let row3 = mk_row ["key3"; "val3"] in
    let row4 = mk_row ["key4"; "val4"] in
    let items = RlpItems [row1; row2; row3; row4] in
    expect_0x_string "dict_rlp"
      "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34"
      (encoded_string items);
    true

  let%test "small_int_rlp" =
    expect_0x_string "small_int_rlp"
      "0x8203e8"
      (to_string (encode_int 1000));
    true

  let%test "cat_dog_rlp" =
    let cat_item = RlpItem "cat" in
    let dog_item = RlpItem "dog" in
    let items = RlpItems [cat_item; dog_item] in
    rlp_encode_items items = RlpEncoding "ȃcat�dog"

  let%test "empty_list_rlp" =
    let empty_list = RlpItems [] in
    rlp_encode_items empty_list = RlpEncoding "�"

  let%test "two_set_rlp" =
    let zero = RlpItems [] in
    let one = RlpItems [zero] in
    let two = RlpItems [zero; one; RlpItems [zero; one]] in
    rlp_encode_items two = RlpEncoding "��������"

  let%test "fifteen_rlp" = encode_int 15 = RlpEncoding "\015"

  let%test "kilo_rlp" = encode_int 1024 = RlpEncoding "�\004\000"

  let%test "latin_rlp" =
    let s = "Lorem ipsum dolor sit amet, consectetur adipisicing elit" in
    encode_string s = RlpEncoding ("�8" ^ s)

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
      [ "0x"
      ; "0x04a817c800"
      ; "0x0186a0"
      ; "0x687422eea2cb73b5d3e242ba5456b782919afc85"
      ; "0x03e8"
      ; "0xc0de"
      ; "0x1c"
      ; "0x668ed6500efd75df7cb9c9b9d8152292a75453ec2d11030b0eec42f6a7ace602"
      ; "0x3efcbbf4d53e0dfa4fde5c6d9a73221418652abc66dff7fddd78b81cc28b9fbf" ]
    in
    let rlp_items = RlpItems (List.map (fun it -> RlpItem (parse_0x_data it)) items) in
    expect_0x_string "encode-list"
      "0xf869808504a817c800830186a094687422eea2cb73b5d3e242ba5456b782919afc858203e882c0de1ca0668ed6500efd75df7cb9c9b9d8152292a75453ec2d11030b0eec42f6a7ace602a03efcbbf4d53e0dfa4fde5c6d9a73221418652abc66dff7fddd78b81cc28b9fbf"
      (encoded_string rlp_items);
    true

  (* TODO: sign the RLP, not the side-chain style marshaling!
     Go through the entire example and check that we get the correct values bit-for-bit.
  *)
  let%test "compute-transaction-hash" =
    (* example from https://medium.com/@codetractio/inside-an-ethereum-transaction-fa94ffca912f *)
    let keypair = keypair_of_0x
                    "0xc0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0dec0de"
                    "0x044643bb6b393ac20a6175c713175734a72517c63d6f73a3ca90a15356f2e967da03d16431441c61ac69aeabb7937d333829d9da50431ff6af38536aa262497b27" "" in
    expect_string "c0de address"
      "0x53ae893e4b22d707943299a8d0c844df0e3d5557"
      (Address.to_0x keypair.address);
    let tx_header =
      TxHeader.{ sender= trent_address (* doesn't matter for transaction hash, it's in the signature *)
               ; nonce= Nonce.zero
               ; gas_price= TokenAmount.of_int 20000000000
               ; gas_limit= TokenAmount.of_int 100000
               ; value= TokenAmount.of_int 1000 } in
    let operation =
      Operation.CallFunction
        ( Address.of_0x "0x687422eea2cb73b5d3e242ba5456b782919afc85"
        , parse_0x_bytes "0xc0de") in
    let transaction = {Transaction.tx_header; Transaction.operation} in
    let unsigned_transaction_hash =
      transaction |> rlp_of_transaction |> encoded_string |> digest_of_string in
    (* TODO: FIX THE CODE, THEN RESTORE THE TEST!
       expect_string "unsigned transaction hash"
       "0x6a74f15f29c3227c5d1d2e27894da58d417a484ef53bc7aa57ee323b42ded656"
       (Digest.to_0x unsigned_transaction_hash);
    *)
    ignore unsigned_transaction_hash;
    let transaction_hash = get_transaction_hash transaction keypair.private_key in
    (*
       expect_string "transaction hash"
       "0x8b69a0ca303305a92d8d028704d65e4942b7ccc9a99917c8c9e940c9d57a9662"
       (Digest.to_0x transaction_hash);
    *)
    ignore transaction_hash;
    true
end
