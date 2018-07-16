(* ethereum_util.ml -- utility code for Ethereum main chain *)

open Lib
open Crypto

let hash s = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s
let hash_bytes bytes = hash (Bytes.to_string bytes)

(* Hexadecimal support. See https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding *)

let hex_string_of_number nat = "0x" ^ UInt256.to_hex_string nat

let validate_0x_prefix hs =
  let len = String.length hs in
  if not (len >= 2 && hs.[0] = '0' && hs.[1] = 'x') then
    raise (Internal_error "Hex string does not strictly begin with 0x") ;
  if len = 2 then
    raise (Internal_error "Hex string has no digits") ;
  ()

let number_of_hex_string hs =
  validate_0x_prefix hs ;
  let len = String.length hs in
  if hs.[2] != '0' then
    UInt256.of_hex_string (String.sub hs 2 (len - 2))
  else if len = 3 then
    UInt256.zero
  else
    raise (Internal_error "Hex number starts with 0")

let hex_string_of_address address =
  "0x" ^ Address.to_hex_string address

let address_of_hex_string hs =
  validate_0x_prefix hs ;
  if String.length hs != 42 then
    raise (Internal_error "Invalid length for hex address") ;
  parse_hex_substring hs 2 (String.length hs - 2)
  |> Address.of_big_endian_bits

let hex_string_of_address_with_checksum address =
  let hex_digits = Address.to_hex_string address in
  (* Char.code 'a' - Char.code 'A' *)
  let uppercase_difference = 32 in
  let hashed_digits = hash hex_digits in
  "0x" ^ String.init (2 * Address.address_size)
    (fun i ->
       let ch = hex_digits.[i] in
       match ch with
       | '0'..'9' -> ch
       | 'a'..'f' -> if hex_digit_of_string hashed_digits 0 i < 8 then
           ch
         else
           Char.chr (Char.code ch - uppercase_difference)
       | _ -> raise (Internal_error "Unexpected digit in hex string"))

let validate_address_checksum hs =
  (* see https://www.quora.com/How-can-we-do-Ethereum-address-validation *)
  (* which says to check a bit of the hash, but means byte *)
  let hex_len = String.length hs - 2 in (* strip 0x *)
  let hex_digits = String.sub hs 2 hex_len in
  let lower_hex_digits = String.lowercase_ascii hex_digits in
  let hashed_digits = unparse_hex_string (hash lower_hex_digits) in
  let flag_error ndx =
    raise
      (Internal_error (Format.sprintf "Invalid address checksum at index %d for %s" (ndx + 2) hs))
  in
  for ndx = 0 to hex_len - 1 do
    match hex_digits.[ndx] with
    | '0'..'9' -> ()
    | 'a'..'f' -> ( match hashed_digits.[ndx] with '0'..'7' -> () | _ -> flag_error ndx )
    | 'A'..'F' -> ( match hashed_digits.[ndx] with '0'..'7' -> flag_error ndx | _ -> () )
    | _ ->
      raise
        (Internal_error
           (Format.sprintf "Invalid hex digit at index %d in address %s" (ndx + 2) hs))
  done

let address_of_hex_string_with_checksum hs =
  validate_address_checksum hs ;
  address_of_hex_string hs

let hex_string_of_string s =
  if s = "" then "0x0" else "0x" ^ unparse_hex_substring s 0 (String.length s)

let string_of_hex_string hs =
  if hs = "0x0" then "" else
    (validate_0x_prefix hs ;
     let len = String.length hs in
     if len mod 2 != 0 then
       raise (Internal_error "Odd number of digits in hex string") ;
     parse_hex_substring hs 2 (len - 2))

let hex_string_of_bytes bs = hex_string_of_string (Bytes.to_string bs)

let bytes_of_hex_string hs = Bytes.of_string (string_of_hex_string hs)

let bytes_of_address address = Bytes.of_string (Address.to_big_endian_bits address)

let bits_of_token_amount token_amount =
  Main_chain.TokenAmount.to_big_endian_bits token_amount

let hex_string_of_nonce nonce = hex_string_of_number (Main_chain.Nonce.z_of nonce)

let bits_of_nonce nonce = Main_chain.Nonce.to_big_endian_bits nonce

let hex_string_of_token_amount token_amount =
  hex_string_of_number (Main_chain.TokenAmount.z_of token_amount)

(* TokenAmount.of_string doesn't grok hex strings *)
let token_amount_of_hex_string s = Main_chain.TokenAmount.of_z (number_of_hex_string s)

module Test = struct

  let%test "hex_string_of_number" =
    List.for_all
      (fun (n, hex) -> let num = UInt256.of_int n in
        hex_string_of_number num = hex
        && UInt256.equal num (number_of_hex_string hex))
      [(0,"0x0");(1,"0x1");(10,"0xa");(291,"0x123");(61453,"0xf00d");(0xabcde,"0xabcde")]

  let%test "number_of_hex_string error" =
    List.for_all
      (fun (hex, err) -> try ignore (number_of_hex_string hex) ; false with
           Internal_error x -> x = err)
      [("0x", "Hex string has no digits");
       ("0x0400","Hex number starts with 0");
       ("ff","Hex string does not strictly begin with 0x")]

  let%test "hex_string_of_string" =
    List.for_all
      (fun (bits, hex) -> hex_string_of_string bits = hex
                          && bits = string_of_hex_string hex)
      [("","0x0");("\000","0x00");("\000\000","0x0000");("\001\035","0x0123");
       ("abcd","0x61626364");("\r\n","0x0d0a");
       ("\236\202\132key1\132val1\202\132key2\132val2\202\132key3\132val3\202\132key4\132val4",
        "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34")]

  let%test "string_of_hex_string error" =
    List.for_all
      (fun (hex, err) -> try ignore (string_of_hex_string hex) ; false with
           Internal_error x -> x = err)
      [("0x", "Hex string has no digits");
       ("0xf0f0f","Odd number of digits in hex string");
       ("004200","Hex string does not strictly begin with 0x")]

  let%test "hex_string_of_address" =
    List.for_all
      (fun (hex, ethhex) -> let address = Address.of_hex_string hex in
        hex_string_of_address_with_checksum address = ethhex
        && hex_string_of_address address = String.lowercase_ascii ethhex
        && Address.equal address (address_of_hex_string_with_checksum ethhex)
        && Address.equal address (address_of_hex_string ethhex))
      [("9797809415e4b8efea0963e362ff68b9d98f9e00","0x9797809415E4B8efEa0963E362ff68B9d98F9e00");
       ("507877c2e26f1387432d067d2daafa7d0420d90a","0x507877C2E26f1387432D067D2DaAfa7d0420d90a")]

  let%test "address_of_hex_string error" =
    List.for_all
      (fun (hex, err) -> try ignore (address_of_hex_string_with_checksum hex) ; false with (Internal_error x) -> x = err)
      [("0x9797809415e4b8efea0963e362ff68b9d98f9e00", "Invalid address checksum at index 12 for 0x9797809415e4b8efea0963e362ff68b9d98f9e00");
       ("0x507877C2E26f1387432D067D2DaAfa7D0420d90a", "Invalid address checksum at index 33 for 0x507877C2E26f1387432D067D2DaAfa7D0420d90a");
       ("0x507877", "Invalid length for hex address")]

end
