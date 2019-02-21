open Rlp

(* TODO: detect overflow or use the Z type from zarith *)

(* For the inverse of encode_nat_as_string.
   The bits in the string do not include the sign,
   so this always produces a nat. *)
let unmarshal_nat s offset len =
  let last = offset + len - 1 in
  Z.of_bits (String.init len (fun i -> s.[last - i]))

(* Inverse of encode_nat_as_string.
   The bits in the string do not include the sign,
   so this always produces a nat. *)
let decode_nat_of_string s =
  unmarshal_nat s 0 (String.length s)

let nat_of_rlp_item n =
  match n with
  | Rlp.RlpItem s -> decode_nat_of_string s
  | _ -> raise (Rlp_data_type_mismatch ("rlp nat_of_rlp_item: expected an RlpItem", n))

let string_of_rlp_item s =
  match s with
  | Rlp.RlpItem s -> s
  | _ -> raise (Rlp_data_type_mismatch ("rlp string_of_rlp_item: expected an RlpItem", s))

(* ------------------------------------------- *)

(* Decoding RLP data from a byte string *)

(* Produces a tuple of two values:
    - The decoded RLP item
    - The new offset, for the point after it *)
let rec unmarshal s offset limit =
  (if limit <= offset then raise (Rlp_unmarshaling_error ("first byte goes past limit", offset, s)));
  let first_char = s.[offset] in
  let first_byte = Char.code first_char in
  if first_byte < 0xc0
  then 
    (* item *)
    (if first_byte < 0x80
     then 
       (* 1 byte item *)
       (Rlp.RlpItem (String.make 1 first_char), offset + 1)
     else if first_byte < 0xb8
     then
       (* 0-55 byte item *)
       let n = first_byte - 0x80
       and offset_a = offset + 1 in
       let offset_b = offset_a + n in
       (if limit < offset_b then raise (Rlp_unmarshaling_error ("string length goes past limit", offset, s)));
       (if n = 1 && Char.code s.[offset_a] < 0x80 then raise (Rlp_unmarshaling_error ("string should be represented as 1 byte", offset, s)));
       (Rlp.RlpItem (String.sub s offset_a n), offset_b)
     else
       (* >55 byte item *)
       let nn = first_byte - 0xb7
       and offset_a = offset + 1 in
       let offset_b = offset_a + nn in
       (if limit < offset_b then raise (Rlp_unmarshaling_error ("string length-of-length goes past limit", offset, s)));
       let n = Z.to_int (unmarshal_nat s offset_a nn) in
       let offset_c = offset_b + n in
       (if limit < offset_c then raise (Rlp_unmarshaling_error ("string length goes past limit", offset, s)));
       (if n < 56 then raise (Rlp_unmarshaling_error ("string should be represented with length<=55 mode", offset, s)));
       (Rlp.RlpItem (String.sub s offset_b n), offset_c))
  else
    (* list *)
    (if first_byte < 0xf8
     then
       (* 0-55 byte list *)
       let n = first_byte - 0xc0
       and offset_a = offset + 1 in
       let offset_b = offset_a + n in
       (if limit < offset_b then raise (Rlp_unmarshaling_error ("list payload length goes past limit", offset, s)));
       unmarshal_list_payload s offset_a offset_b []
     else
       (* >55 byte list *)
       let nn = first_byte - 0xf7
       and offset_a = offset + 1 in
       let offset_b = offset_a + nn in
       (if limit < offset_b then raise (Rlp_unmarshaling_error ("list payload length-of-length goes past limit", offset, s)));
       let n = Z.to_int (unmarshal_nat s offset_a nn) in
       let offset_c = offset_b + n in
       (if limit < offset_c then raise (Rlp_unmarshaling_error ("list payload length goes past limit", offset, s)));
       (if n < 56 then raise (Rlp_unmarshaling_error ("list should be represented with length<=55 mode", offset, s)));
       unmarshal_list_payload s offset_b offset_c [])

and unmarshal_list_payload s start_offset end_offset acc =
  if start_offset = end_offset
  then (Rlp.RlpItems (List.rev acc), start_offset)
  else if start_offset < end_offset
  then let (next_item, next_offset) = unmarshal s start_offset end_offset
       in unmarshal_list_payload s next_offset end_offset (next_item :: acc)
  else raise (Rlp_unmarshaling_error ("list payload goes past limit", start_offset, s))

(* Produces a tuple of two values:
    - The decoded RLP item
    - The new offset, for the point after it *)
let rlp_item_unmarshal_rlp offset s =
  unmarshal s offset (String.length s)

let rlp_item_of_rlp s =
  let (v, _) = rlp_item_unmarshal_rlp 0 s
  in v

let rlp_item_unmarshal_rlp_opt offset s =
  try Some (rlp_item_unmarshal_rlp offset s)
  with Rlp.Rlp_unmarshaling_error _ -> None

let rlp_item_of_rlp_opt s =
  try Some (rlp_item_of_rlp s)
  with Rlp.Rlp_unmarshaling_error _ -> None

(* ------------------------------------------- *)

let nat_unmarshal_rlp start_offset s =
  let (rlp, end_offset) = rlp_item_unmarshal_rlp start_offset s
  in (nat_of_rlp_item rlp, end_offset)

let nat_of_rlp s =
  nat_of_rlp_item (rlp_item_of_rlp s)

let string_unmarshal_rlp start_offset s =
  let (rlp, end_offset) = rlp_item_unmarshal_rlp start_offset s
  in (string_of_rlp_item rlp, end_offset)

let string_of_rlp s =
  string_of_rlp_item (rlp_item_of_rlp s)

