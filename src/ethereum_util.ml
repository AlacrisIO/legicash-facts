(* ethereum_util.ml -- utility code for Ethereum main chain *)

open Legibase
open Lib
open Crypto

let hash s = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) s

let rec zero_code = Char.code '0'

and a_code = Char.code 'a'

and big_a_code = Char.code 'A'

and validate_address_checksum hs =
  (* see https://www.quora.com/How-can-we-do-Ethereum-address-validation *)
  (* which says to check a bit of the hash, but means byte *)
  let hex_len = String.length hs - 2 in
  let hex_digits = String.sub hs 2 hex_len in
  let lower_hex_digits = String.lowercase_ascii hex_digits in
  let hashed_digits =
    let hash_in_hex = hex_string_of_string ~left_pad:true (hash lower_hex_digits) in
    (* remove 0x *)
    String.sub hash_in_hex 2 (String.length hash_in_hex - 2)
  in
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


and string_of_hex_string ?(is_address= false) hs =
  if hs = "0x0" then ""
  else
    let len = String.length hs in
    if not (hs.[0] = '0' && hs.[1] = 'x') then
      raise (Internal_error "Hex string does not begin with 0x") ;
    let _ = if is_address then validate_address_checksum hs in
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


and hex_string_of_string ?(left_pad= false) ?(is_address= false) s =
  if s = "" then "0x0"
  else
    let len = String.length s in
    let to_hex_digit byte =
      if byte < 0xa then Char.chr (byte + zero_code) (* 0 - 9 *)
      else if byte >= 0xa && byte <= 0xf then (* a - f *)
        Char.chr (a_code + (byte - 0xa))
      else
        raise
          (Internal_error
             (Format.sprintf "Value %d can't be represented with a valid hex digit" byte))
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
    (* if we have an address, upper/lowercase by checksum convention *)
    let get_checksummed_hex_digits () =
      (* Char.code 'a' - Char.code 'A' *)
      let uppercase_difference = 32 in
      let hashed_digits =
        let hash_in_hex = hex_string_of_string ~left_pad:true (hash hex_digits) in
        (* remove 0x *)
        String.sub hash_in_hex 2 (String.length hash_in_hex - 2)
      in
      String.init hex_len (fun ndx ->
        let ch = hex_digits.[ndx] in
        match ch with
        | '0'..'9' -> ch
        | 'a'..'f' -> (
            match hashed_digits.[ndx] with
            | '0'..'7' -> ch
            | _ -> Char.chr (Char.code ch - uppercase_difference) )
        | _ -> raise (Internal_error "Unexpected digit in hex string") )
    in
    "0x" ^ if is_address then get_checksummed_hex_digits () else hex_digits


(* allow leading 0 in hex representation of bytes *)
let hex_string_of_bytes bs = hex_string_of_string ~left_pad:true (Bytes.to_string bs)

let bytes_of_hex_string hs = Bytes.of_string (string_of_hex_string hs)

(* allow leading 0 in hex representation of address *)
let hex_string_of_address address =
  hex_string_of_string ~left_pad:true ~is_address:false (Address.to_string address)


let hex_string_of_address_with_checksum address =
  hex_string_of_string ~left_pad:true ~is_address:true (Address.to_string address)


let address_of_hex_string_validate_checksum hs =
  Address.of_string (string_of_hex_string ~is_address:true hs)


let address_of_hex_string hs = Address.of_string (string_of_hex_string ~is_address:false hs)

let bytes_of_address address = Bytes.of_string (Address.to_string address)

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

  [%%test
    let "valid-address-make-checksum" =
      let address = address_of_hex_string "0x9797809415e4b8efea0963e362ff68b9d98f9e00" in
      let hex_address = hex_string_of_address_with_checksum address in
      validate_address_checksum hex_address ;
      true]

  [%%test
    let "invalid-address-checksum" =
      try
        validate_address_checksum "0x9797809415e4b8efea0963e362ff68b9d98f9e00" ;
        false
      with Internal_error _ -> true]

  [%%test
    let "valid-address-checksum-already-checksummed" =
      validate_address_checksum "0x507877C2E26f1387432D067D2DaAfa7d0420d90a" ;
      true]

  [%%test
    let "invalid-address-checksum-already-checksummed" =
      try
        (* one bad character at index 33 *)
        validate_address_checksum "0x507877C2E26f1387432D067D2DaAfa7D0420d90a" ;
        false
      with Internal_error _ -> true]
end
