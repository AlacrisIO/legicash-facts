open Rlping

module Rlp = Rlp
module Rlping = Rlping
module Rlp_encode = Rlp_encode
module Rlp_decode = Rlp_decode

module Private : sig
  val marshal_of_to       : 'a to_rlp_item -> 'a marshal_rlp
  val unmarshal_of_of     : 'a of_rlp_item -> 'a unmarshal_rlp
  val rlping_of_to_and_of : 'a to_rlp_item -> 'a of_rlp_item -> 'a rlping
end

(** Given a type `a` which has pre_rlping, produce the full
    rlping for it. *)
val rlping : 'a pre_rlping -> 'a rlping

