open Rlping

(* Naming Conventions:
   For a type foo:
    - foo_to_rlp_item       : foo -> rlp_item
    - foo_of_rlp_item       : rlp_item -> foo
    - foo_of_rlp_item_opt   : rlp_item -> foo option
    - foo_to_rlp            : foo -> string                  (* string containing RLP bytes *)
    - foo_of_rlp            : string -> foo
    - foo_of_rlp_opt        : string -> foo option
    - foo_marshal_rlp       : buffer -> foo -> unit          (* the buffer stores its own "cursor" position *)
    - foo_unmarshal_rlp     : int -> string -> (foo * int)   (* the ints are "cursor" offsets into the string *)
    - foo_unmarshal_rlp_opt : int -> string -> (foo * int) option
   *)

val nat_of_rlp_item   : Z.t of_rlp_item
val nat_of_rlp        : Z.t of_rlp
val nat_unmarshal_rlp : Z.t unmarshal_rlp

val string_of_rlp_item   : string of_rlp_item
val string_of_rlp        : string of_rlp
val string_unmarshal_rlp : string unmarshal_rlp

val rlp_item_of_rlp            : Rlp.rlp_item of_rlp
val rlp_item_of_rlp_opt        : Rlp.rlp_item of_rlp_opt
val rlp_item_unmarshal_rlp     : Rlp.rlp_item unmarshal_rlp
val rlp_item_unmarshal_rlp_opt : Rlp.rlp_item unmarshal_rlp_opt

(* used in ppx_deriving_rlp_runtime *)
val decode_nat_of_string : string -> Z.t
