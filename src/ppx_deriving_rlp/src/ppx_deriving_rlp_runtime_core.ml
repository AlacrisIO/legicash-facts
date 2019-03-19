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

(* Rlping from to_rlp_item and of_rlp_item functions *)

(** Given a type `a` which has pre_rlping, produce the full
    rlping for it. *)
let rlping : 'a Rlping.pre_rlping -> 'a Rlping.rlping =
  fun pre ->
    Private.rlping_of_to_and_of pre.to_rlp_item pre.of_rlp_item

