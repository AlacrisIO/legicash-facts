open Rlp

(* length_of_length : natural -> natural
 * length_of_length n = ⎡log_{256} (1+n)⎤
     where ⎡x⎤ is the ceiling of x and log_b (x) is the logarithm base b of x
   length_of_length n = ⎡(Z.numbits n) / 8⎤
     where ⎡x⎤ is the ceiling of x and / is rational-number division (not integer division)
 *)
let length_of_length (n : Z.t) : int = (Z.numbits n + 7) / 8

(* ------------------------------------------- *)

(* Encoding nonnegative integers according to the RLP spec *)

(* Marshal a nonnegative integer into a buffer, used for length.
   ASSUME n >= 0
   ASSUME nn = length_of_length n
   Since these are supposed to be unsigned integers, this marshals
   the absolute value of `n`, without the sign.
   Note that by itself this does not produce valid RLP bytes since it
   doesn't have the length. *)
let marshal_nat buffer nn (n : Z.t) =
  let little_endian_string = Z.to_bits n in
  for i = (nn - 1) downto 0 do
    Buffer.add_char buffer (little_endian_string.[i])
  done

(* convert number to string, used for
   - encoding lengths within RLP-encodings
   - encoding integers to string, which can then be RLP-encoded
   Note that by itself this does not produce valid RLP bytes since it
   doesn't have the length. *)
let encode_nat_as_string (n : Z.t) =
  if Z.zero <= n
  then let nn = length_of_length n in
       let buffer = Buffer.create nn
       in (marshal_nat buffer nn n;
           Buffer.contents buffer)
  else failwith ("rlp encode_nat_as_string: expected a nonnegative integer, given: " ^ Z.to_string n)

let nat_to_rlp_item n = Rlp.RlpItem (encode_nat_as_string n)

let string_to_rlp_item s = Rlp.RlpItem s

(* ------------------------------------------- *)

(* Encoding RLP data as a byte string *)

let string_marshal_rlp buffer s =
    let n = String.length s
    in if n = 1 && Char.code s.[0] < 0x80
       then Buffer.add_string buffer s
       else if n < 56
       then (Buffer.add_char buffer (Char.chr (0x80 + n));
             Buffer.add_string buffer s)
       else let n = Z.of_int n in
            let nn = length_of_length n
            in (Buffer.add_char buffer (Char.chr (0xb7 + nn));
                marshal_nat buffer nn n;
                Buffer.add_string buffer s)

let rec rlp_item_marshal_rlp buffer rlp =
  match rlp with
  | RlpItem s  -> string_marshal_rlp buffer s
  | RlpItems l ->
    let payload = Buffer.create 16 in
    List.iter (rlp_item_marshal_rlp payload) l;
    let n = Z.of_int (Buffer.length payload)
    in if n < Z.of_int 56
       then (Buffer.add_char buffer (Char.chr (0xc0 + Z.to_int n));
             Buffer.add_buffer buffer payload)
       else let nn = length_of_length n
            in (Buffer.add_char buffer (Char.chr (0xf7 + nn));
                marshal_nat buffer nn n;
                Buffer.add_buffer buffer payload)

(* ------------------------------------------- *)

let string_to_rlp s : string =
  let n = String.length s in
  let buffer = Buffer.create (n + length_of_length (Z.of_int n) + 1)
  in (string_marshal_rlp buffer s;
      Buffer.contents buffer)

let nat_marshal_rlp buffer n =
  rlp_item_marshal_rlp buffer (nat_to_rlp_item n)

let nat_to_rlp n : string =
  let buffer = Buffer.create (length_of_length n + 2)
  in (nat_marshal_rlp buffer n;
      Buffer.contents buffer)

let rlp_item_to_rlp rlp : string =
  let buffer = Buffer.create 16
  in (rlp_item_marshal_rlp buffer rlp;
      Buffer.contents buffer)

