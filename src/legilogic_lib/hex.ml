(* Hexadecimal *)
open Lib

(** Char code for lower case a *)
let a_code = Char.code 'a'

(** Char code for upper case A *)
let big_a_code = Char.code 'A'

(** Char code for 0 *)
let zero_code = Char.code '0'

let int_of_hex_char hex =
  let hex_code = Char.code hex in
  match hex with
  | '0'..'9' -> hex_code - zero_code (* assume ASCII / UTF-8 *)
  | 'a'..'f' -> hex_code - a_code + 0xa
  | 'A'..'F' -> hex_code - big_a_code + 0xa
  | _ -> raise (Internal_error (Printf.sprintf "Invalid hex character %c" hex))

let parse_hex_nibble string pos =
  int_of_hex_char string.[pos]

let parse_hex_byte string pos =
  (parse_hex_nibble string pos) lsl 4 + parse_hex_nibble string (pos + 1)

let parse_hex_substring string pos len =
  let start_pos = pos - (len mod 2) in
  String.init
    ((len + 1) / 2)
    (fun i ->
       let p = start_pos + 2 * i in
       let single_digit = p < pos in
       Char.chr
         (if single_digit then
            parse_hex_nibble string (p + 1)
          else
            parse_hex_byte string p))

let parse_hex_string string = parse_hex_substring string 0 (String.length string)

let parse_coloned_hex_string hex_string =
  let invalid () = raise (Internal_error (Printf.sprintf "Not a valid hex string: %s" hex_string)) in
  let hex_len = String.length hex_string in
  if hex_len > 0 && (hex_len + 1) mod 3 <> 0 then invalid () ;
  let len = (hex_len + 1) / 3 in
  let parse_char i =
    let offset = i * 3 in
    if offset > 0 && hex_string.[offset - 1] != ':' then invalid () ;
    Char.chr (parse_hex_byte hex_string offset)
  in
  String.init len parse_char

let hex_char_of_int ?(upper_case = false) digit =
  if (digit < 0 || digit > 16) then
    raise (Internal_error (Printf.sprintf "Invalid hex digit %d" digit))
  else
    Char.chr
      (if digit < 10 then
         zero_code + digit
       else if upper_case then
         big_a_code - 10 + digit
       else
         a_code - 10 + digit)

let hex_digit_of_string string start index =
  let byte = Char.code string.[start+(index lsr 1)] in
  if index mod 2 = 0 then byte lsr 4 else byte mod 16

let unparse_hex_substring string pos len =
  String.init (len lsl 1) (fun i -> hex_char_of_int (hex_digit_of_string string pos i))

let unparse_hex_string string =
  unparse_hex_substring string 0 (String.length string)

let unparse_coloned_hex_string string =
  String.concat ":" (List.init (String.length string) (fun i -> unparse_hex_substring string i 1))

let validate_0x_prefix hs =
  let len = String.length hs in
  if not (len >= 2 && hs.[0] = '0' && hs.[1] = 'x') then
    raise (Internal_error "Hex string does not strictly begin with 0x") ;
  if len = 2 then
    raise (Internal_error "Hex string has no digits") ;
  ()

let parse_0x_prefix parser hs =
  validate_0x_prefix hs;
  parser (String.sub hs 2 (String.length hs - 2))

let unparse_0x_prefix printer x = "0x" ^ (printer x)

let parse_0x_string = parse_0x_prefix parse_hex_string

let unparse_0x_string = unparse_0x_prefix unparse_hex_string


module Test = struct
  let%test "hex_string" =
    List.for_all
      (fun (bits, hex) -> unparse_hex_string bits = hex && parse_hex_string hex = bits)
      [("","");("\000","00");("\000\000","0000");
       ("abcd","61626364");("\r\n","0d0a")]
end
