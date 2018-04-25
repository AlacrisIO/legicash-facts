open Legibase

exception Not_implemented

let bottom : 'a -> 'b = fun _ -> raise Not_implemented

let list_of_option = function None -> [] | Some x -> [x]
(* TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)

let is_odd_64 x = (Int64.logand x Int64.one) == Int64.one

let constantly x _ = x

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

(* test digests *)

let mk_digest_test data expected =
  let digest = get_digest data in
  expected = (unparse_hex (Data256.to_string digest))

let%test "digest_1" =
  mk_digest_test
    "this is a test"
    "d5:02:39:01:b6:e1:b3:fd:03:54:3a:a1:ee:40:3b:77:36:a9:08:5a:b0:4e:71:a0:47:d4:5b:2a:57:7f:72:e8"

let%test "digest_2" =
  mk_digest_test
    (Some "nonsense")
    "e2:9d:d9:ae:ca:d9:44:3b:f6:ea:17:3d:70:57:d3:22:1c:97:cb:94:1a:c9:aa:93:86:ab:ed:ac:e7:16:88:d0"

let%test "digest_3" =
  mk_digest_test
   Int64.one
   "c6:c6:80:47:7d:5c:20:cd:35:1e:ab:56:54:05:85:3a:9f:09:00:f4:93:d0:3e:c4:e5:72:c6:f5:98:53:41:83"

let%test "digest_4" =
  mk_digest_test
    [99.9; 100.4; 22.0; 1033.7]
    "f4:d7:ee:d0:ed:86:14:cf:aa:4c:f1:af:0f:f5:dc:23:45:a4:a6:62:d5:aa:57:ed:7a:9b:f4:75:94:50:65:4a"
