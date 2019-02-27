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
val rlping : 'a Rlping.pre_rlping -> 'a Rlping.rlping

(* Adapting Rlping from one type to another if they are isomorphic *)

(** Given a type `a` which has rlping, and conversion functions
    between `a` and `b`, produce the rlping for type `b` *)
val rlping_by_isomorphism : ('a -> 'b) -> ('b -> 'a) -> 'a rlping -> 'b rlping

(* unit / empty-tuple *)

val unit_to_rlp_item   : unit to_rlp_item
val unit_of_rlp_item   : unit of_rlp_item
val unit_to_rlp        : unit to_rlp
val unit_of_rlp        : unit of_rlp
val unit_marshal_rlp   : unit marshal_rlp
val unit_unmarshal_rlp : unit unmarshal_rlp
val unit_rlping        : unit rlping

(* strings *)

val string_to_rlp_item   : string to_rlp_item
val string_of_rlp_item   : string of_rlp_item
val string_to_rlp        : string to_rlp
val string_of_rlp        : string of_rlp
val string_marshal_rlp   : string marshal_rlp
val string_unmarshal_rlp : string unmarshal_rlp
val string_rlping        : string rlping

(* signed integers *)

val z_to_rlp_item   : Z.t to_rlp_item
val z_of_rlp_item   : Z.t of_rlp_item
val z_to_rlp        : Z.t to_rlp
val z_of_rlp        : Z.t of_rlp
val z_marshal_rlp   : Z.t marshal_rlp
val z_unmarshal_rlp : Z.t unmarshal_rlp
val z_rlping        : Z.t rlping

val int_to_rlp_item   : int to_rlp_item
val int_of_rlp_item   : int of_rlp_item
val int_to_rlp        : int to_rlp
val int_of_rlp        : int of_rlp
val int_marshal_rlp   : int marshal_rlp
val int_unmarshal_rlp : int unmarshal_rlp
val int_rlping        : int rlping

(* floats *)

val float_to_rlp_item   : float to_rlp_item
val float_of_rlp_item   : float of_rlp_item
val float_to_rlp        : float to_rlp
val float_of_rlp        : float of_rlp
val float_marshal_rlp   : float marshal_rlp
val float_unmarshal_rlp : float unmarshal_rlp
val float_rlping        : float rlping

(* booleans *)

val bool_to_rlp_item   : bool to_rlp_item
val bool_of_rlp_item   : bool of_rlp_item
val bool_to_rlp        : bool to_rlp
val bool_of_rlp        : bool of_rlp
val bool_marshal_rlp   : bool marshal_rlp
val bool_unmarshal_rlp : bool unmarshal_rlp
val bool_rlping        : bool rlping

(* lists *)

val list_to_rlp_item   : ('elem to_rlp_item) -> ('elem list) to_rlp_item
val list_of_rlp_item   : ('elem of_rlp_item) -> ('elem list) of_rlp_item
val list_to_rlp        : ('elem to_rlp_item) -> ('elem list) to_rlp
val list_of_rlp        : ('elem of_rlp_item) -> ('elem list) of_rlp
val list_marshal_rlp   : ('elem to_rlp_item) -> ('elem list) marshal_rlp
val list_unmarshal_rlp : ('elem of_rlp_item) -> ('elem list) unmarshal_rlp
val list_rlping        : ('elem rlping)      -> ('elem list) rlping

(* options *)

val option_to_rlp_item   : ('elem to_rlp_item) -> ('elem option) to_rlp_item
val option_of_rlp_item   : ('elem of_rlp_item) -> ('elem option) of_rlp_item
val option_to_rlp        : ('elem to_rlp_item) -> ('elem option) to_rlp
val option_of_rlp        : ('elem of_rlp_item) -> ('elem option) of_rlp
val option_marshal_rlp   : ('elem to_rlp_item) -> ('elem option) marshal_rlp
val option_unmarshal_rlp : ('elem of_rlp_item) -> ('elem option) unmarshal_rlp
val option_rlping        : ('elem rlping)      -> ('elem option) rlping

