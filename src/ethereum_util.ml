(* ethereum_util.ml -- utility code for Ethereum main chain *)

open Legibase
open Lib

let rec zero_code = Char.code '0'

and a_code = Char.code 'a'

and big_a_code = Char.code 'A'

and string_of_hex_string hs =
  if hs = "0x0" then ""
  else
    let len = String.length hs in
    if not (hs.[0] = '0' && hs.[1] = 'x') then
      raise (Internal_error "Hex string does not begin with 0x") ;
    let odd_len = len mod 2 = 1 in
    let unhex_digit hd =
      match hd with
      | '0'..'9' -> Char.code hd - zero_code
      | 'a'..'f' -> Char.code hd - a_code + 0xa
      | 'A'..'F' -> Char.code hd - big_a_code + 0xa (* be liberal in what we accept *)
      | _ -> raise (Internal_error (Printf.sprintf "Invalid hex digit %c" hd))
    in
    if odd_len then
      String.init
        ((len - 1) / 2)
        (fun ndx ->
          let ndx2 = 2 + 2 * ndx in
          let hi_nybble = if ndx = 0 then 0 else unhex_digit hs.[ndx2 - 1] in
          let lo_nybble = unhex_digit hs.[ndx2] in
          Char.chr (hi_nybble lsl 4 + lo_nybble) )
    else
      String.init
        ((len - 2) / 2)
        (fun ndx ->
          let ndx2 = 2 + 2 * ndx in
          let hi_nybble = unhex_digit hs.[ndx2] in
          let lo_nybble = unhex_digit hs.[ndx2 + 1] in
          Char.chr (hi_nybble lsl 4 + lo_nybble) )


and hex_string_of_string ?(left_pad= false) s =
  if s = "" then "0x0"
  else
    let len = String.length s in
    let to_hex_digit byte =
      if byte < 0xa then Char.chr (byte + zero_code) (* 0 - 9 *)
      else if byte >= 0xa && byte <= 0xf then (* a - f *)
        Char.chr (a_code + (byte - 0xa))
      else raise (Internal_error "Not a valid hex digit")
    in
    let trim_leading_zero = not left_pad && Char.code s.[0] lsr 4 = 0 in
    (* if no leading zero, hex string is shorter by one, and
       even indexes, rather than odd, get low hex digit *)
    let hex_len, parity, get_ndx2 =
      if trim_leading_zero then (2 * len - 1, 1, fun ndx -> (ndx + 1) / 2)
      else (2 * len, 0, fun ndx -> ndx / 2)
    in
    let get_hex_digit ndx =
      let ndx2 = get_ndx2 ndx in
      let bytes = Char.code s.[ndx2] in
      let byte = if ndx mod 2 = parity then bytes lsr 4 else bytes mod 0x10 in
      to_hex_digit byte
    in
    let hex_digits = String.init hex_len get_hex_digit in
    "0x" ^ hex_digits


(* allow leading 0 in hex representation of bytes *)
let hex_string_of_bytes bs = hex_string_of_string ~left_pad:true (Bytes.to_string bs)

(* allow leading 0 in hex representation of address *)
let hex_string_of_address address = hex_string_of_string ~left_pad:true (Address.to_string address)

let address_of_hex_string hs = Address.of_string (string_of_hex_string hs)

let hex_string_of_int64 num64 = Format.sprintf "0x%Lx" num64

let string_of_int64 num64 = string_of_hex_string (hex_string_of_int64 num64)

let hex_string_of_token_amount token_amount =
  hex_string_of_int64 (Main_chain.TokenAmount.to_int64 token_amount)


let string_of_token_amount token_amount =
  string_of_int64 (Main_chain.TokenAmount.to_int64 token_amount)


let hex_string_of_nonce nonce = hex_string_of_int64 (Main_chain.Nonce.to_int64 nonce)

let string_of_nonce nonce = string_of_int64 (Main_chain.Nonce.to_int64 nonce)

let hex_string_of_token_amount token_amount =
  hex_string_of_int64 (Main_chain.TokenAmount.to_int64 token_amount)


let hex_string_of_nonce nonce = hex_string_of_int64 (Main_chain.Nonce.to_int64 nonce)

(* TokenAmount.of_string doesn't grok hex strings *)
let token_amount_of_hex_string s = Main_chain.TokenAmount.of_int64 (Int64.of_string s)

let hash s = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s

module Test = struct
  let make_hex_unhex_test s = hex_string_of_string (string_of_hex_string s) = s

  let make_hex_unhex_pad_test s = hex_string_of_string ~left_pad:true (string_of_hex_string s) = s

  [%%test
  let "hex-unhex-hash" =
    make_hex_unhex_test
      "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34"]

  [%%test
  let "hex-unhex-zer0" = make_hex_unhex_test "0x0"]

  [%%test
  let "hex-unhex-no-pad" = make_hex_unhex_test "0x123"]

  [%%test
  let "hex-unhex-pad" = make_hex_unhex_pad_test "0x0123"]
end
