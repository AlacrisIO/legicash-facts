module Rlp = Rlp
module Rlping = Rlping
module Rlp_encode = Rlp_encode
module Rlp_decode = Rlp_decode

(* private functions,
   end users should not use these,
   but ppx_deriving_rlp can insert references to them *)
module Private = struct
  (* marshal_of_to : 'a to_rlp_item -> 'a marshal_rlp *)
  let marshal_of_to to_rlp_item =
    let marshal_rlp b v = Rlp_encode.rlp_item_marshal_rlp b (to_rlp_item v)
    in marshal_rlp

  (* unmarshal_of_of : 'a of_rlp_item -> 'a unmarshal_rlp *)
  let unmarshal_of_of of_rlp_item =
    let unmarshal_rlp i s = let (v,j) = Rlp_decode.rlp_item_unmarshal_rlp i s
                            in (of_rlp_item v, j)
    in unmarshal_rlp

  (* rlping_of_to_and_of : 'a to_rlp_item -> 'a of_rlp_item -> 'a rlping *)
  let rlping_of_to_and_of to_rlp_item of_rlp_item =
    let to_rlp v = Rlp_encode.rlp_item_to_rlp (to_rlp_item v)
    and of_rlp s = of_rlp_item (Rlp_decode.rlp_item_of_rlp s)
    and marshal_rlp = marshal_of_to to_rlp_item
    and unmarshal_rlp = unmarshal_of_of of_rlp_item in
    let of_rlp_item_opt v = try Some (of_rlp_item v) with
                            | Rlp.Rlp_data_type_mismatch _ -> None
    and of_rlp_opt s = try Some (of_rlp s) with
                       | Rlp.Rlp_unmarshaling_error _ -> None
                       | Rlp.Rlp_data_type_mismatch _ -> None
    and unmarshal_rlp_opt i s = try Some (unmarshal_rlp i s) with
                                | Rlp.Rlp_unmarshaling_error _ -> None
                                | Rlp.Rlp_data_type_mismatch _ -> None
    in (* local open Rlping *)
       Rlping.
         { to_rlp_item; of_rlp_item; of_rlp_item_opt;
           to_rlp; of_rlp; of_rlp_opt;
           marshal_rlp; unmarshal_rlp; unmarshal_rlp_opt }
end

(* ------------------------------------------------------------------------- *)

(* Adapting Rlping from one type to another if they are isomorphic *)

(** Given a type `a` which has rlping, and conversion functions
    between `a` and `b`, produce the rlping for type `b` *)
let rlping_by_isomorphism a_to_b b_to_a a_rlping =
  let a_to_rlp_item b = a_rlping.Rlping.to_rlp_item (b_to_a b)
  and a_of_rlp_item a = (a_to_b (a_rlping.Rlping.of_rlp_item a)) in
  Private.rlping_of_to_and_of a_to_rlp_item a_of_rlp_item

(* ------------------------------------------------------------------------- *)

(* unit / empty-tuple *)

let unit_to_rlp_item () = Rlp.RlpItems []
let unit_of_rlp_item = function (Rlp.RlpItems []) -> ()
                              | v -> raise (Rlp.Rlp_data_type_mismatch ("unit_of_rlp_item: expected an empty RlpItems list", v))
let unit_rlping = Private.rlping_of_to_and_of unit_to_rlp_item unit_of_rlp_item
let unit_to_rlp = unit_rlping.to_rlp
let unit_of_rlp = unit_rlping.of_rlp
let unit_marshal_rlp = unit_rlping.marshal_rlp
let unit_unmarshal_rlp = unit_rlping.unmarshal_rlp

(* strings *)

let string_to_rlp_item s = Rlp.RlpItem s
let string_of_rlp_item = function (Rlp.RlpItem s) -> s
                                | v -> raise (Rlp.Rlp_data_type_mismatch ("string_of_rlp_item: expected an RlpItem", v))
let string_rlping = Private.rlping_of_to_and_of string_to_rlp_item string_of_rlp_item
let string_to_rlp = string_rlping.to_rlp
let string_of_rlp = string_rlping.of_rlp
let string_marshal_rlp = string_rlping.marshal_rlp
let string_unmarshal_rlp = string_rlping.unmarshal_rlp

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

let z_rlping = Private.rlping_of_to_and_of z_to_rlp_item z_of_rlp_item

let z_to_rlp = z_rlping.to_rlp
let z_of_rlp = z_rlping.of_rlp
let z_marshal_rlp = z_rlping.marshal_rlp
let z_unmarshal_rlp = z_rlping.unmarshal_rlp

let int_rlping = rlping_by_isomorphism Z.to_int Z.of_int z_rlping

let int_to_rlp_item = int_rlping.to_rlp_item
let int_of_rlp_item = int_rlping.of_rlp_item
let int_to_rlp = int_rlping.to_rlp
let int_of_rlp = int_rlping.of_rlp
let int_marshal_rlp = int_rlping.marshal_rlp
let int_unmarshal_rlp = int_rlping.unmarshal_rlp

(* floats *)

let z_bits_of_float x = Z.of_int64 (Int64.bits_of_float x)
let float_of_z_bits z = Int64.float_of_bits (Z.to_int64 z)

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
let bool_rlping = Private.rlping_of_to_and_of bool_to_rlp_item bool_of_rlp_item
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

