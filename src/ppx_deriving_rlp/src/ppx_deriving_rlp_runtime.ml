include Ppx_deriving_rlp_runtime_core
open Rlping

module Ppx_deriving_rlp_runtime_core = Ppx_deriving_rlp_runtime_core

(* ------------------------------------------------------------------------- *)

(* Adapting Rlping from one type to another if they are isomorphic *)

(** Given a type `a` which has rlping, and conversion functions
    between `a` and `b`, produce the rlping for type `b` *)
let rlping_by_isomorphism a_to_b b_to_a a_rlping =
  let to_rlp_item b = a_rlping.Rlping.to_rlp_item (b_to_a b)
  and of_rlp_item a = (a_to_b (a_rlping.Rlping.of_rlp_item a)) in
  rlping { to_rlp_item; of_rlp_item }

(* ------------------------------------------------------------------------- *)

(* unit / empty-tuple *)

let unit_to_rlp_item () = Rlp.RlpItems []
let unit_of_rlp_item = function (Rlp.RlpItems []) -> ()
                              | v -> raise (Rlp.Rlp_data_type_mismatch ("unit_of_rlp_item: expected an empty RlpItems list", v))
let unit_rlping = rlping { to_rlp_item = unit_to_rlp_item; of_rlp_item = unit_of_rlp_item }
let unit_to_rlp = unit_rlping.to_rlp
let unit_of_rlp = unit_rlping.of_rlp
let unit_marshal_rlp = unit_rlping.marshal_rlp
let unit_unmarshal_rlp = unit_rlping.unmarshal_rlp

(* strings *)

let string_to_rlp_item s = Rlp.RlpItem s
let string_of_rlp_item = function (Rlp.RlpItem s) -> s
                                | v -> raise (Rlp.Rlp_data_type_mismatch ("string_of_rlp_item: expected an RlpItem", v))
let string_rlping = rlping { to_rlp_item = string_to_rlp_item; of_rlp_item = string_of_rlp_item }
let string_to_rlp = string_rlping.to_rlp
let string_of_rlp = string_rlping.of_rlp
let string_marshal_rlp = string_rlping.marshal_rlp
let string_unmarshal_rlp = string_rlping.unmarshal_rlp

(* chars *)

let char_to_rlp_item c = Rlp.RlpItem (String.make 1 c)
let char_of_rlp_item = function (Rlp.RlpItem s) when String.length s = 1 -> s.[0]
                              | v -> raise (Rlp.Rlp_data_type_mismatch ("char_of_rlp_item: expected an RlpItem of 1 byte", v))
let char_rlping = rlping { to_rlp_item = char_to_rlp_item; of_rlp_item = char_of_rlp_item }
let char_to_rlp = char_rlping.to_rlp
let char_of_rlp = char_rlping.of_rlp
let char_marshal_rlp = char_rlping.marshal_rlp
let char_unmarshal_rlp = char_rlping.unmarshal_rlp

(* signed integers *)

(* Naturals (zero or positive) are encoded using an RlpItem
   containing the big-endian byte encoding.
   Negatives are encoded using a single-element RlpItems list
   containing the absolute value.
   NOTE: This is our own convention, not a convention that comes from
         Ethereum. *)

let z_to_rlp_item i =
  if i >= Z.zero then
    Rlp_encode.nat_to_rlp_item i
  else
    Rlp.RlpItems [Rlp_encode.nat_to_rlp_item (Z.neg i)]

let z_of_rlp_item i =
  match i with
  | Rlp.RlpItem s -> Rlp_decode.decode_nat_of_string s
  | Rlp.RlpItems [Rlp.RlpItem s] -> Z.neg (Rlp_decode.decode_nat_of_string s)
  | v -> raise (Rlp.Rlp_data_type_mismatch ("invalid RLP int", v))

let z_rlping = rlping { to_rlp_item = z_to_rlp_item; of_rlp_item = z_of_rlp_item }

let z_to_rlp = z_rlping.to_rlp
let z_of_rlp = z_rlping.of_rlp
let z_marshal_rlp = z_rlping.marshal_rlp
let z_unmarshal_rlp = z_rlping.unmarshal_rlp

let z_to_int_guarded z =
  if Z.fits_int z
  then Z.to_int z
  else raise (Rlp.Rlp_data_type_mismatch ("int doesn't fit", z_to_rlp_item z))

let int_rlping = rlping_by_isomorphism z_to_int_guarded Z.of_int z_rlping

let int_to_rlp_item = int_rlping.to_rlp_item
let int_of_rlp_item = int_rlping.of_rlp_item
let int_to_rlp = int_rlping.to_rlp
let int_of_rlp = int_rlping.of_rlp
let int_marshal_rlp = int_rlping.marshal_rlp
let int_unmarshal_rlp = int_rlping.unmarshal_rlp

(* floats *)

let z_bits_of_float x = Z.of_int64 (Int64.bits_of_float x)
let float_of_z_bits z =
  if Z.fits_int64 z
  then Int64.float_of_bits (Z.to_int64 z)
  else raise (Rlp.Rlp_data_type_mismatch ("float doesn't fit", z_to_rlp_item z))

let float_rlping = rlping_by_isomorphism float_of_z_bits z_bits_of_float z_rlping
let float_to_rlp_item = float_rlping.to_rlp_item
let float_of_rlp_item = float_rlping.of_rlp_item
let float_to_rlp = float_rlping.to_rlp
let float_of_rlp = float_rlping.of_rlp
let float_marshal_rlp = float_rlping.marshal_rlp
let float_unmarshal_rlp = float_rlping.unmarshal_rlp

(* booleans *)

let bool_to_rlp_item b = int_to_rlp_item (if b then 1 else 0)
let bool_of_rlp_item b = 0 <> (int_of_rlp_item b)
let bool_rlping = rlping { to_rlp_item = bool_to_rlp_item; of_rlp_item = bool_of_rlp_item }
let bool_to_rlp = bool_rlping.to_rlp
let bool_of_rlp = bool_rlping.of_rlp
let bool_marshal_rlp = bool_rlping.marshal_rlp
let bool_unmarshal_rlp = bool_rlping.unmarshal_rlp

(* lists *)

let list_to_rlp_item (elem_to) loe = Rlp.RlpItems (List.map elem_to loe)
let list_of_rlp_item (elem_of) = function (Rlp.RlpItems loe) -> (List.map elem_of loe)
                                        | v -> raise (Rlp.Rlp_data_type_mismatch ("list_of_rlp_item: expected an RlpItems list", v))
let list_to_rlp (elem_to) loe = Rlp_encode.rlp_item_to_rlp (list_to_rlp_item elem_to loe)
let list_of_rlp (elem_of) v = list_of_rlp_item elem_of (Rlp_decode.rlp_item_of_rlp v)
let list_marshal_rlp (elem_to) = Private.marshal_of_to (list_to_rlp_item elem_to)
let list_unmarshal_rlp (elem_of) = Private.unmarshal_of_of (list_of_rlp_item elem_of)
let list_rlping (elem_rlping) = Private.rlping_of_to_and_of (list_to_rlp_item elem_rlping.Rlping.to_rlp_item) (list_of_rlp_item elem_rlping.Rlping.of_rlp_item)

(* options *)

let option_map_to_list f = function Some e -> [f e] | None -> []

let option_to_rlp_item (elem_to) oe = Rlp.RlpItems (option_map_to_list elem_to oe)
let option_of_rlp_item (elem_of) = function Rlp.RlpItems [e] -> Some (elem_of e)
                                          | Rlp.RlpItems [] -> None
                                          | v -> raise (Rlp.Rlp_data_type_mismatch ("option_of_rlp_item: expected an RlpItems list of length 0 or 1", v))
let option_to_rlp (elem_to) oe = Rlp_encode.rlp_item_to_rlp (option_to_rlp_item elem_to oe)
let option_of_rlp (elem_of) v = option_of_rlp_item elem_of (Rlp_decode.rlp_item_of_rlp v)
let option_marshal_rlp (elem_to) = Private.marshal_of_to (option_to_rlp_item elem_to)
let option_unmarshal_rlp (elem_of) = Private.unmarshal_of_of (option_of_rlp_item elem_of)
let option_rlping (elem_rlping) = Private.rlping_of_to_and_of (option_to_rlp_item elem_rlping.Rlping.to_rlp_item) (option_of_rlp_item elem_rlping.Rlping.of_rlp_item)

(* results *)

type ('a, 'b) result = ('a, 'b) Pervasives.result =
  | Ok of 'a
  | Error of 'b
[@@deriving rlp]

