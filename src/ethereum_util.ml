(* ethereum_util.ml -- utility code for Ethereum main chain *)

open Legibase
open Lib

let rec zero_code = Char.code '0'

and a_code = Char.code 'a'

and big_a_code = Char.code 'A'

and string_of_hex_string hs =
  let len = String.length hs in
  if not (hs.[0] = '0' && hs.[1] = 'x') then
    raise (Internal_error "Hex string does not begin with 0x") ;
  if len mod 2 = 1 then raise (Internal_error "Hex string contains odd number of characters") ;
  let unhex_digit hd =
    match hd with
    | '0'..'9' -> Char.code hd - zero_code
    | 'a'..'f' -> Char.code hd - a_code + 0xa
    | 'A'..'F' -> Char.code hd - big_a_code + 0xa (* be liberal in what we accept *)
    | _ -> raise (Internal_error (Printf.sprintf "Invalid hex digit %c" hd))
  in
  String.init
    ((len - 2) / 2)
    (fun ndx ->
      let ndx2 = 2 + (2 * ndx) in
      let hi_nybble = unhex_digit hs.[ndx2] in
      let lo_nybble = unhex_digit hs.[ndx2 + 1] in
      Char.chr ((hi_nybble lsl 4) + lo_nybble) )

and hex_string_of_string s =
  let len = String.length s in
  let to_hex_digit byte =
    if byte < 0xa then Char.chr (byte + zero_code) (* 0 - 9 *)
    else if byte >= 0xa && byte <= 0xf then (* a - f *)
      Char.chr (a_code + (byte - 0xa))
    else raise (Internal_error "Not a valid hex digit")
  in
  let get_hex_digit ndx =
    let ndx2 = ndx / 2 in
    let bytes = Char.code s.[ndx2] in
    let byte = if ndx mod 2 = 0 then bytes lsr 4 else bytes mod 0x10 in
    to_hex_digit byte
  in
  let hex_digits = String.init (2 * len) get_hex_digit in
  "0x" ^ hex_digits

(* TokenAmount.of_string doesn't grok hex strings *)
let token_amount_of_hex_string s = Main_chain.TokenAmount.of_int64 (Int64.of_string s)
