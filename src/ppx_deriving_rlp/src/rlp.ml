
(* A Datatype for RLP items *)

type rlp_item = RlpItem of string | RlpItems of rlp_item list
[@@deriving show]

let rlp_item_of_rlp_item item = item
let rlp_item_to_rlp_item item = item

(* An exception for unmarshalling errors *)

exception Rlp_unmarshaling_error of string * int * string
(* The three tuple values represent:
    * the message string
    * the position within the input string 
    * the input string
   
   This is meant to be used when the bytes cannot be parsed into
   a valid RLP tree. It should _not_ be used for when it is a valid
   RLP tree but some other type expected a tree of a certain shape.
   For example: The string "\xc2\x8f" is not a valid RLP tree because
   the list and string payloads refer to bytes that do not exist.
   If there were more bytes after this it would still not be valid
   because the string's payload goes past its parent-list's payload. *)

exception Rlp_data_type_mismatch of string * rlp_item
(* The two tuple values represent:
    * the message string
    * the input RLP tree
   
   This is meant to be used when there is a valid RLP tree, but it
   doesn't match the data type that was expected.
   For example: the string "\x83abc" is valid RLP, but if you were
   expecting a list, it's a type mismatch. *)
