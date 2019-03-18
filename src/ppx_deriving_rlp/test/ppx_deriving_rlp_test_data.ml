open Ppx_deriving_rlp_runtime

type foo = A of int | B of float * bool
[@@deriving rlp]

type loi = Loimt | Loicons of { first : int; rest : loi }
[@@deriving rlp]

type wrapped_list1 = Wrap1 of int list
[@@deriving rlp]
type wrapped_list2 = Wrap2 of { value: int list }
[@@deriving rlp]
type wrapped_list3 = { value: int list }
[@@deriving rlp]


(* Type aliases *)

type alias_int = int
[@@deriving rlp]

type alias_list = int list
[@@deriving rlp]

type alias_unit = unit
[@@deriving rlp]


(* Polymorphic variants *)

type matter1 =
  [ `Solid of string
  | `Liquid of int
  | `Gas of float ]
[@@deriving rlp]

type matter2 =
  [ matter1
  | `Plasma of char
  | `Unknown ]
[@@deriving rlp]


(* Type parameters *)

type ('k, 'v) seq_tree_map = StmLeaf of 'v
                           | StmNode of ('k * (('k,'v) seq_tree_map)) list
[@@deriving rlp]


(* Using { rlping = expression } attribute *)

type int_seq = int Seq.t
[@@deriving rlp { rlping = rlping_by_isomorphism List.to_seq List.of_seq (list_rlping int_rlping) }]
