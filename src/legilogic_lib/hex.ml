(* Hexadecimal *)
open Lib

let a_code     = Char.code 'a'
let big_a_code = Char.code 'A'
let zero_code  = Char.code '0'

let int_of_hex_char hex =
  let hex_code = Char.code hex in
  match hex with
    | '0'..'9' -> hex_code - zero_code (* assume ASCII / UTF-8 *)
    | 'a'..'f' -> hex_code - a_code     + 0xa
    | 'A'..'F' -> hex_code - big_a_code + 0xa
    | _        -> bork "Invalid hex character %c" hex

let parse_hex_nibble string pos =
  int_of_hex_char string.[pos]

let parse_hex_byte string pos =
   (parse_hex_nibble string  pos) lsl 4
  + parse_hex_nibble string (pos + 1)

let parse_hex_substring string pos len =
  let start_pos = pos - (len mod 2) in
  String.init
    ((len + 1) / 2)
    (fun i ->
       let p = start_pos + 2 * i
       in Char.chr (if p < pos then parse_hex_nibble string (p + 1)
                               else parse_hex_byte   string p))

let parse_hex_string string =
  parse_hex_substring string 0 (String.length string)

let parse_coloned_hex_string hex_string =
  let invalid () = bork "Not a valid hex string: %s" hex_string
  and hex_len    = String.length hex_string

  in if hex_len > 0 && (hex_len + 1) mod 3 <> 0
    then invalid ();

  let parse_char i =
    let offset = i * 3
    in if offset > 0 && hex_string.[offset - 1] != ':'
      then invalid ();
    Char.chr (parse_hex_byte hex_string offset)

  in String.init ((hex_len + 1) / 3) parse_char


let hex_char_of_int ?(upper_case = false) digit =
  if (digit < 0 || digit > 16) then
    bork "Invalid hex digit %d" digit
  else
    Char.chr (if digit < 10 then zero_code + digit
                         else if upper_case then big_a_code - 10 + digit
                         else    a_code - 10 + digit)

let hex_digit_of_string string start index =
  let byte = Char.code string.[start+(index lsr 1)]
  in if index mod 2 = 0 then byte lsr 4
                        else byte mod 16

let unparse_hex_substring string pos len =
  String.init
    (len lsl 1)
    (fun i -> hex_char_of_int (hex_digit_of_string string pos i))

let unparse_hex_string string =
  unparse_hex_substring string 0 (String.length string)

let unparse_coloned_hex_string string =
  String.concat ":"
    (List.init (String.length string)
    (fun i -> unparse_hex_substring string i 1))

(** Raise an Internal_error if the string doesn't strictly start with "0x" *)
let validate_0x_prefix hs =
  if not (String.length hs >= 2 && hs.[0] = '0' && hs.[1] = 'x') then
    bork "Hex string does not begin with 0x"

let parse_0x_quantity hs =
  validate_0x_prefix hs;
  let len = String.length hs in
  if len = 2 then
    bork "Hex quantity has no digits" (* 0 is "0x0" *)
  else if hs.[2] = '0' then
    if len = 3 then Z.zero
               else bork "Hex quantity has leading zero"
  else
    parse_hex_substring hs 2 (len - 2) |> string_reverse |> Z.of_bits

let unparse_0x_quantity z =
  let nybbles = (Z.numbits z + 3) / 4
  in if nybbles = 0 then
    "0x0"
  else
    String.init (nybbles + 2)
      (fun i -> if i >= 2
              then let k    = (nybbles + 1 - i) lxor 1
                   and bits = Z.to_bits z
                   in hex_char_of_int (hex_digit_of_string bits 0 k)
              else if i = 0 then '0'
              else 'x')

let parse_0x_data hs =
  validate_0x_prefix hs;
  let len = String.length hs
  in if len mod 2 != 0 then
    bork "Odd number of digits in hex string" ;
  parse_hex_substring hs 2 (len - 2)

let unparse_0x_data s =
  "0x" ^ unparse_hex_substring s 0 (String.length s)

let parse_0x_bytes hs   = Bytes.of_string (parse_0x_data hs)
let unparse_0x_bytes bs = unparse_0x_data (Bytes.to_string bs)

let parse_0x_prefix parser hs =
  validate_0x_prefix hs;
  parser (String.sub hs 2 (String.length hs - 2))

let unparse_0x_prefix printer x = "0x" ^ (printer x)


let remove_0x_from_string : string -> string =
  fun str_in ->
  let len = String.length str_in in
  let str_out = String.sub str_in 2 (len-2) in
  str_out

module Test = struct
  open Lib.Test

  let expect_0x_string description expected string =
    expect_string description expected (unparse_0x_data string)

  let expect_0x_bytes description expected bytes =
    expect_0x_string description expected (Bytes.to_string bytes)

  let%test "hex_string" =
    List.for_all
      (fun (bits, hex) -> unparse_hex_string bits = hex
                       && parse_hex_string hex    = bits)
      [ ("",         "")
      ; ("\000",     "00")
      ; ("\000\000", "0000")
      ; ("abcd",     "61626364")
      ; ("\r\n",     "0d0a")
      ]

  let%test "parse_0x_quantity" =
    List.for_all
      (fun (dec, hex) -> unparse_0x_quantity (Z.of_string dec) = hex
                      && Z.to_string (parse_0x_quantity hex)   = dec)
      [ ("0",          "0x0")
      ; ("10",         "0xa")
      ; ("3735928559", "0xdeadbeef")
      ; ("291",        "0x123")
      ; ( "2172320085121171917150527218272133101249531734298482375974185397385364336251450517199607845825731097406827572"
        , "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34" )
      ]

  let%test "parse_0x_quantity error" =
    List.for_all
      (fun (hex, err) -> try ignore (parse_0x_quantity hex); false
                        with Internal_error x -> x = err)
      [ ("0",      "Hex string does not begin with 0x")
      ; ("",       "Hex string does not begin with 0x")
      ; ("0x",     "Hex quantity has no digits")
      ; ("0x0123", "Hex quantity has leading zero")
      ]

  let%test "parse_0x_data" =
    List.for_all
      (fun (bits, hex) -> unparse_0x_data bits = hex
                       && bits                 = parse_0x_data hex)
      [ ("",         "0x")
      ; ("\000",     "0x00")
      ; ("\000\000", "0x0000")
      ; ("\001\035", "0x0123")
      ; ("abcd",     "0x61626364")
      ; ("\r\n",     "0x0d0a")
      ; ( "\236\202\132key1\132val1\202\132key2\132val2\202\132key3\132val3\202\132key4\132val4"
        , "0xecca846b6579318476616c31ca846b6579328476616c32ca846b6579338476616c33ca846b6579348476616c34" )
      ]

  let%test "parse_0x_data error" =
    List.for_all
      (fun (hex, err) -> try ignore (parse_0x_data hex); false
                        with Internal_error x -> x = err)
      [ ("0x0",     "Odd number of digits in hex string")
      ; ("0xf0f0f", "Odd number of digits in hex string")
      ; ("004200",  "Hex string does not begin with 0x")
      ]

  let invert_hex hex_str =
    unparse_coloned_hex_string (parse_coloned_hex_string hex_str)

  (* test that hex parsing and unparsing are inverses *)
  let%test "inverse_1" =
    let key =
      "d5:69:84:dc:08:3d:76:97:01:71:4e:eb:1d:4c:47:a4:54:25:5a:3b:bc:3e:9f:44:84:20:8c:52:bd:a3:b6:4e"
    in invert_hex key = key

  let%test "inverse_2" =
    let key =
      "04:7d:52:54:04:9f:02:3e:e7:aa:ea:1e:fa:4f:17:ae:70:0f:af:67:23:24:02:5a:a9:b5:32:5a:92:1f:d0:f1:51:0e:68:31:f1:bf:90:b4:a1:df:e1:cd:49:e5:03:ec:7d:b5:9f:6e:78:73:d0:3a:3a:09:6c:46:5c:87:22:22:69"
    in invert_hex key = key
end
