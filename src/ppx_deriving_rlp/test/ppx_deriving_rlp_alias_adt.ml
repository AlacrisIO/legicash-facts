
(* This other module defines a datatype but does not define
   rlp operations for it *)
module OtherModule : sig
  type t = N of int
         | S of string
         | L of t list
         | O of (string * t) list
end = struct
  type t = N of int
         | S of string
         | L of t list
         | O of (string * t) list
end

type t_alias = OtherModule.t = N of int
                             | S of string
                             | L of t_alias list
                             | O of (string * t_alias) list
[@@deriving rlp]

