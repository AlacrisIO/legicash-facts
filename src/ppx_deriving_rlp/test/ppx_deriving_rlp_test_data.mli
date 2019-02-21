
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


type alias_int = int
[@@deriving rlp]

type alias_list = int list
[@@deriving rlp]

type alias_unit = unit
[@@deriving rlp]



type ('k, 'v) seq_tree_map = StmLeaf of 'v
                           | StmNode of ('k * (('k,'v) seq_tree_map)) list
[@@deriving rlp]
