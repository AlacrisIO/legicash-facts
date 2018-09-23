(* ethereum_util.ml -- utility code for Ethereum main chain *)

open Legilogic_lib
open Lib
open Hex
open Digesting
open Signing

(* Hexadecimal support. See https://github.com/ethereum/wiki/wiki/JSON-RPC#hex-value-encoding *)

let hex_string_of_address_with_checksum address =
  let hex_digits = Address.to_hex_string address in
  (* Char.code 'a' - Char.code 'A' *)
  let uppercase_difference = 32 in
  let hashed_digits = keccak256_string hex_digits in
  "0x" ^ String.init (2 * Address.size_in_bytes)
           (fun i ->
              let ch = hex_digits.[i] in
              match ch with
              | '0'..'9' -> ch
              | 'a'..'f' -> if hex_digit_of_string hashed_digits 0 i < 8 then
                  ch
                else
                  Char.chr (Char.code ch - uppercase_difference)
              | _ -> bork "Unexpected digit in hex string")

let validate_address_checksum hs =
  (* see https://www.quora.com/How-can-we-do-Ethereum-address-validation *)
  (* which says to check a bit of the hash, but means byte *)
  let hex_len = String.length hs - 2 in (* strip 0x *)
  let hex_digits = String.sub hs 2 hex_len in
  let lower_hex_digits = String.lowercase_ascii hex_digits in
  let hashed_digits = unparse_hex_string (keccak256_string lower_hex_digits) in
  let flag_error ndx =
    bork "Invalid address checksum at index %d for %s" (ndx + 2) hs
  in
  for ndx = 0 to hex_len - 1 do
    match hex_digits.[ndx] with
    | '0'..'9' -> ()
    | 'a'..'f' -> ( match hashed_digits.[ndx] with '0'..'7' -> () | _ -> flag_error ndx )
    | 'A'..'F' -> ( match hashed_digits.[ndx] with '0'..'7' -> flag_error ndx | _ -> () )
    | _ ->
      bork "Invalid hex digit at index %d in address %s" (ndx + 2) hs
  done

let address_of_hex_string_with_checksum hs =
  validate_address_checksum hs ;
  Address.of_0x_string hs

let bytes_of_address address = Bytes.of_string (Address.to_big_endian_bits address)

module Test = struct
  let%test "0x_string <-> address" =
    List.for_all
      (fun (hex, ethhex) -> let address = Address.of_hex_string hex in
        hex_string_of_address_with_checksum address = ethhex
        && Address.to_0x_string address = String.lowercase_ascii ethhex
        && Address.equal address (address_of_hex_string_with_checksum ethhex)
        && Address.equal address (Address.of_0x_string ethhex))
      [("9797809415e4b8efea0963e362ff68b9d98f9e00","0x9797809415E4B8efEa0963E362ff68B9d98F9e00");
       ("507877c2e26f1387432d067d2daafa7d0420d90a","0x507877C2E26f1387432D067D2DaAfa7d0420d90a")]

  let%test "address_of_hex_string error" =
    List.for_all
      (fun (hex, err) -> try ignore (address_of_hex_string_with_checksum hex) ; false with (Internal_error x) -> x = err)
      [("0x9797809415e4b8efea0963e362ff68b9d98f9e00", "Invalid address checksum at index 12 for 0x9797809415e4b8efea0963e362ff68b9d98f9e00");
       ("0x507877C2E26f1387432D067D2DaAfa7D0420d90a", "Invalid address checksum at index 33 for 0x507877C2E26f1387432D067D2DaAfa7D0420d90a");
       ("0x507877", "String length not the expected 40 bytes: \"507877\"")]
end
