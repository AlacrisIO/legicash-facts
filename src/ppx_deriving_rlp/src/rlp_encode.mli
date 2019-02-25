open Rlping

(* Naming Conventions:
   For a type foo:
    - foo_to_rlp_item   : foo -> rlp_item
    - foo_of_rlp_item   : rlp_item -> foo
    - foo_to_rlp        : foo -> string                  (* string containing RLP bytes *)
    - foo_of_rlp        : string -> foo
    - foo_marshal_rlp   : buffer -> foo -> unit          (* the buffer stores its own "cursor" position *)
    - foo_unmarshal_rlp : int -> string -> (foo * int)  (* the ints are "cursor" offsets into the string *)
   *)

val nat_to_rlp_item : Z.t to_rlp_item
val nat_to_rlp      : Z.t to_rlp
val nat_marshal_rlp : Z.t marshal_rlp

val string_to_rlp_item : string to_rlp_item
val string_to_rlp      : string to_rlp
val string_marshal_rlp : string marshal_rlp

val rlp_item_to_rlp      : Rlp.rlp_item to_rlp
val rlp_item_marshal_rlp : Rlp.rlp_item marshal_rlp

(* used in ppx_deriving_rlp_runtime *)
val encode_nat_as_string : Z.t -> string
