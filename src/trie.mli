(* Big Endian Patricia Trees (Tries)
   See notably the article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Crypto
open Marshaling

(** A signature for the computation of a synthesized attribute from a binary tree *)
module type TreeSynthS = sig
  type value
  type t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
end

(** A signature for the computation of a synthesized attribute from a Patricia tree *)
module type TrieSynthS = sig
  include TreeSynthS
  type key
  val skip : int -> int -> key -> t -> t
end

(** A module to synthesize the cardinal of a trie as an attribute of said trie *)
module TrieSynthCardinal (Key : IntS) (Value : T) : sig
  type key = Key.t
  type value = Value.t
  type t = Z.t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
  val skip : int -> int -> key -> t -> t
end

(** A module to synthesize attributes for Skip by reducing it to Branch and Leaf. *)
module TrieSynthComputeSkip (Key : IntS) (Synth: TrieSynthS) : sig
  include TrieSynthS with type key := Key.t
end

(** A module for Big-Endian Patricia Tree. *)
module type TrieS = sig
  type key
  type value
  type synth

  type t =
    | Empty
    | Leaf of {value: value; synth: synth}
    | Branch of {left: t; right: t; height: int; synth: synth}
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}

  type (+'a) step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: key; length: int}

  type costep = { height: int ; index: key }

  type (+'a) path = {costep: costep; steps: 'a step list}

  type 'a unstep =
    { unstep_left: key -> int -> 'a -> 'a -> 'a
    ; unstep_right: key -> int -> 'a -> 'a -> 'a
    ; unstep_skip: key -> int -> int -> key -> 'a -> 'a }

  val symmetric_unstep:
    branch:(key -> int -> 'a -> 'a -> 'a) ->
    skip:(key -> int -> int -> key -> 'a -> 'a) -> 'a unstep

  val step_apply : 'a unstep -> ('a * costep) -> 'a step -> ('a * costep)
  val path_apply : 'a unstep -> 'a -> 'a path -> ('a * costep)

  include MapS
    with type key := key
     and type value := value
     and type t := t
     and type (+'a) step := 'a step
     and type (+'a) path := 'a path

  val trie_height : t -> int
  val ensure_height : int -> t -> t
  val ensure_same_height : t -> t -> t*t
  val get_synth : t -> synth
  val check_invariant : t -> bool
  val verify : t -> t
  val step_length : 'a step -> int
  val check_path_consistency : 'a path -> bool

  (** [iterate_over_tree ~recursek ~branchk ~skipk ~leafk ~empty ~i ~tree ~k]
      Describes a recursive computation over a tree in great generality,
      using continuation-passing style. This style allows the calculation to
      abort and return the calculation result if it has been determined before
      the last node of the tree is visited, or continue processing other nodes
      if necessary.

      The following are caller-specified types:

      - ['r]: type for (total or partial) synthesized result of the computation for the trie of a subtrie.

      - ['o]: return type of the outer continuation of iterate_over_matching_tree_pair itself.

      Inputs:

      - [recursek ~i ~tree ~k]: Result from recursing into the [i]th nodes of [tree].

      - [branchk ~i ~height ~leftr ~rightr ~k]: Result from recursing down both
      branches from node [i] at specified [height], given results [leftr] and
      [rightr] from the two children of that node.

      - [skipk ~i ~height ~length ~bits ~childr ~k]: Result from recursing
      down [length] skipped children of node [i], given that the result from
      extant child of the skipped nodes, with further skipped bits [bits], is [childr].

      - [leafk ~i ~value ~k]: Result from processing a leaf node at
      index [i], with value [value].

      - [empty ~k]: Result given that the tree is empty.

      - [i]: Prefix index of [tree] node.

      - [tree]: the input tree.

      - [k]: Continuation called on the result of the above recursion, to
      (eventually) produce the final result.
  *)
  val iterate_over_tree:
    recursek:(i:key -> tree:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> synth:synth ->
             k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r -> synth:synth ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> value:value -> synth:synth -> k:('r -> 'o) -> 'o) ->
    emptyk:(k:('r -> 'o) -> 'o) ->
    i:key -> tree:t -> k:('r -> 'o) -> 'o

  (** [iterate_over_tree_pair ~recursek ~branchk ~skipk ~leafk ~onlyak ~onlybk ~i ~treea ~treeb ~k]
      Describes a recursive computation over a pair of trees in great generality,
      using continuation-passing style. This style allows the calculation to
      abort and return the calculation result if it has been determined before
      the last node of the tree is visited, or continue processing other nodes
      if necessary.

      The following are caller-specified types:

      - ['r]: type for (total or partial) synthesized result of the computation for the trie of a subtrie.

      - ['o]: return type of the outer continuation of iterate_over_matching_tree_pair itself.

      Inputs:

      - [recursek ~i ~treea ~treeb ~k]: Result from recursing into the [i]th nodes of
      [treea] and [treeb].

      - [branchk ~i ~height ~leftr ~rightr ~k]: Result from recursing down both
      branches from node [i] at specified [height], given results [leftr] and
      [rightr] from the two children of that node.

      - [skipk ~i ~height ~length ~bits ~childr ~k]: Result from recursing
      down [length] skipped children of node [i], given that the result from
      extant child of the skipped nodes, with further skipped bits [bits], is [childr].

      - [leafk ~i ~valuea ~valueb ~k]: Result from comparing two leaf nodes at
      index [i], with values [valuea] and [valueb].

      - [onlyak ~i ~anode ~k]: Result given that [i]th node [anode] is only
      explicitly present in the first tree argument, [treea].

      - [onlybk ~i ~bnode ~k]: Like [onlyak], mutatis mutandis.

      - [i]: Prefix index of [treea], [treeb] nodes.

      - [treea], [treeb]: The two input trees.

      - [k]: Continuation called on the result of the above recursion, to
      (eventually) produce the final result.
  *)
  val iterate_over_tree_pair:
    recursek:(i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o) ->
    branchk:(i:key -> height:int -> leftr:'r -> rightr:'r -> k:('r -> 'o) -> 'o) ->
    skipk:(i:key -> height:int -> length:int -> bits:key -> childr:'r ->
           k:('r -> 'o) -> 'o) ->
    leafk:(i:key -> valuea:value -> valueb:value -> k:('r -> 'o) -> 'o) ->
    onlyak:(i:key -> anode:t -> k:('r -> 'o) -> 'o) ->
    onlybk:(i:key -> bnode:t -> k:('r -> 'o) -> 'o) ->
    i:key -> treea:t -> treeb:t -> k:('r -> 'o) -> 'o

end

(** A module for Big-Endian Patricia Tree, a.k.a. Trie. *)
module Trie (Key : IntS) (Value : T)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
  : TrieS with type key = Key.t
           and type value = Value.t
           and type synth = Synth.t

(** A signature for Merklizing a Trie . *)
module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = Digest.t
  val leaf_digest : t -> t
  module Marshalable : MarshalableS
  include DigestibleS with type t := t
end

(** A module for Merklizing a Trie . *)
module TrieSynthMerkle (Key : IntS) (Value : DigestibleS) : sig
  include TrieSynthMerkleS
    with type key = Key.t
     and type value = Value.t
     and type t = Digest.t
end

(** Merkle Trie . *)
module type MerkleTrieS = sig
  type key
  type value
  module Synth : TrieSynthMerkleS with type key = key and type value = value and type t = Digest.t
  include TrieS
    with type key := key
     and type value := value
     and type synth = Synth.t
  type proof =
    { key : key
    ; trie : Digest.t
    ; value : Digest.t
    ; steps : (Digest.t step) list
    }
  val empty_key : key
  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path
  val get_proof : key -> t -> proof option
  val check_proof_consistency : proof -> bool
  val json_of_proof : proof -> Yojson.Basic.json
  module type MarshalNodeS = sig
    (* marshaling here means producing a marshaled version of a single node

       subtrees are referred to by their synths ("pointers")

       invariant: a subtree is never Empty

       because we use pointers, we can't unmarshal to produce the original node, except
        by consulting the database; hence we don't have an instance of Marshalable
    *)

    type trie = t

    (* like TrieS.t, except subtrees are of type synth, i.e., they are pointers *)
    type t =
      | NodeEmpty
      | NodeLeaf of { value: value; synth: synth }
      | NodeBranch of { left: synth; right: synth; height: int; synth: synth }
      | NodeSkip of { child: synth; bits: key; length: int; height: int; synth: synth }

    val marshal_empty : Buffer.t -> unit
    val marshal_leaf : Buffer.t -> value -> synth -> unit
    val marshal_branch : Buffer.t -> synth -> synth -> int -> synth -> unit
    val marshal_skip : Buffer.t -> synth -> key -> int -> int -> synth -> unit
    val marshal_trie : Buffer.t -> trie -> unit
    val unmarshal_to_node : ?start:int -> Bytes.t -> t * int
  end

  module MarshalNode : MarshalNodeS
end

module MerkleTrie (Key : IntS) (Value : DigestibleS) :
  MerkleTrieS with type key = Key.t and type value = Value.t

module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  type t = T.t
  include Set.S with type elt := elt and type t := t

  type proof =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t T.step) list
    }

  val trie_digest : t -> Digest.t
  val get_proof : elt -> t -> proof option
  val check_proof_consistency : proof -> bool
  val lens : elt -> (t, bool) Lens.t
end

module MerkleTrieSet (Elt : IntS) : (MerkleTrieSetS with type elt = Elt.t)

module DigestSet : (MerkleTrieSetS with type elt = Digest.t)
