exception Not_implemented

exception Internal_error of string

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let list_of_option = function None -> [] | Some x -> [x]
(* TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)

let option_map f = function
  | Some x -> Some (f x)
  | None -> None

let hex_char_to_int hex =
  let hex_code = Char.code hex in
  match hex with
  | '0'..'9' -> hex_code - Char.code '0' (* assume ASCII / UTF-8 *)
  | 'a'..'f' -> hex_code - Char.code 'a' + Char.code '\n'
  | 'A'..'F' -> hex_code - Char.code 'A' + Char.code '\n'
  | _ -> raise (Internal_error "Invalid hex character")

(* hex strings are of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
let parse_hex hex_string =
  let hex_len = String.length hex_string in
  let _ =
    if hex_len > 0 && (hex_len + 1) mod 3 <> 0 then
      raise
        (Internal_error
           (Printf.sprintf "Not a valid hex string: %s" hex_string))
  in
  let len = (hex_len + 1) / 3 in
  let parse_char ndx =
    let offset = ndx * 3 in
    let hi_nybble = hex_char_to_int hex_string.[offset] in
    let lo_nybble = hex_char_to_int hex_string.[offset + 1] in
    Char.chr (0x10 * hi_nybble + lo_nybble)
  in
  String.init len parse_char

let unparse_hex s =
  let len = String.length s in
  let rec loop ndx accum =
    if ndx >= len then String.concat ":" (List.rev accum)
    else
      let hex = Printf.sprintf "%02x" (Char.code s.[ndx]) in
      loop (ndx + 1) (hex :: accum)
  in
  loop 0 []

(** SKI combinators *)
let identity x = x

let konstant x y = x

let schoenfinkel x y z = x z (y z)

let defaulting default = function None -> default () | Some x -> x

