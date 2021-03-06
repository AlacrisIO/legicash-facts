(* Big Endian Patricia Trees (Tries)
   See notably the article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Yojsoning
open Persisting
open Types

(** A signature for the computation of a synthesized attribute from a binary tree *)
module type TreeSynthS = sig
  type value
  [@@deriving rlp]
  type t
  [@@deriving rlp]
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
end

(** A signature for the computation of a synthesized attribute from a Patricia tree *)
module type TrieSynthS = sig
  include TreeSynthS
  type key
  [@@deriving rlp]
  val skip : int -> int -> key -> t -> t
end

(** A module to synthesize nothing *)
module TrieSynthUnit (Key : UIntS) (Value : TypeRlpS)
  : TrieSynthS with type t = unit and type key = Key.t and type value = Value.t

(** A module to synthesize the cardinal of a trie as an attribute of said trie *)
module TrieSynthCardinal (Key : UIntS) (Value : TypeRlpS)
  : TrieSynthS with type t = Z.t and type key = Key.t and type value = Value.t

(** A module to synthesize attributes for Skip by reducing it to Branch and Leaf. *)
module TrieSynthComputeSkip (Key : UIntS) (Synth: TrieSynthS) : sig
  include TrieSynthS with type key := Key.t
end

(* For a concrete usage of this, see [Merkle_trie.MerkleTrieType]. *)
module type TrieTypeS = sig
  type key
  [@@deriving rlp]
  (** Type of keys *)

  type value
  [@@deriving rlp]
  (** Type of leaves *)

  type synth
  [@@deriving rlp]
  (** Type of synthesized attribute, which is recursively computed from the leaf
      values. E.g., the cardinality of the leaves. *)

  type +'a wrap
  [@@deriving rlp]
  (** This the type constructor used to wrap an arbitrary type ['a] into a type
      that contains an ['a] while addressing other aspects --- in practice,
      in [Merkle_trie.MerkleTrieType], ['a wrap] is ['a Types.dv], which wraps
      a ['a] in a data structure that will lazily load the actual value from
      the db as needed. We abstract over this wrap type in order to decomplect
      the logical trie structure from the generic database handling. *)

  (** Not just the top node but each and every recursive node in the [trie] will
      be wrapped using the [wrap] constructor above into a [t]. *)
  type t = trie wrap
  and trie =
    (** Empty trie. Never appears as a subtrie of a non-empty trie. *)
    | Empty
    (** Leaf node with given value and synthesized attribute *)
    | Leaf of {value: value; synth: synth}
    (** Branch node with given left and right subtries, at given height with
        given synthesized attribute *)
    | Branch of {left: t; right: t; height: int; synth: synth}
    (** Skip node, a trie of given [height] wherein all keys in the trie have
        the next [length] bits in common, being the sequence [bits], down to
        [child] which is either a [Leaf] or a [Branch]. *)
    | Skip of {child: t; bits: key; length: int; height: int; synth: synth}
  [@@deriving rlp]

  (** Accessor for synthesized attribute. *)
  val trie_synth : trie -> synth

  (** [trie_leaf value] constructs a [trie] with a single [Leaf] with given [value],
      and automatically computing the basic synth attribute *)
  val trie_leaf : value -> trie

  (** [trie_branch get height left right], given an accessor [get] unwrapping a
      [t] into a [trie], a [height], and [left] and [right] wrapped subtries
      (of type [t]), constructs a [Branch] [trie] with the given contents
      and automatically synthesizes the combined synth attribute *)
  val trie_branch : (t -> trie) -> int -> t -> t -> trie

  (** [trie_skip get height length bits child], given an accessor [get]
      unwrapping a [t] into a [trie], a [height] and a [length], a sequence of
      [bits], and a wrapped [child] subtrie (of type [t]), constructs a [Skip]
      [trie] with the given contents and automatically synthesizes the combined
      synth attribute *)
  val trie_skip : (t -> trie) -> int -> int -> key -> t -> trie
end

module TrieType
    (Key : UIntS) (Value : TypeRlpS) (WrapType : WrapTypeS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
  : TrieTypeS with type key = Key.t
               and type value = Value.t
               and type synth = Synth.t
               and type +'a wrap = 'a WrapType.t

(** A module for Big-Endian Patricia Tree. *)
module type TrieS = sig
  include TrieTypeS

  (** Represents having done a movement down the tree, with enough information
      to rebuild the wider trie from the subtrie. Here ['a] is typically [t]
      for use in a zipper, or [digest] for use in a Merkle proof. *)
  type +'a step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: key; length: int}
  [@@deriving rlp]

  (** Context for a step: where you are in the tree *)
  type costep = { height: int option; index: key }

  (** [costep] is the current location, given that we've followed the given
      [step]s from a wider trie. *)
  type +'a path = {costep: costep; steps: 'a step list}

  (** Methods for undoing an ['a path] from a child ['b] into a parent ['b],
      where ['a] and ['b] are both typically a [t] for zipping, or a [digest] for Merkle
      proofs. *)
  type ('trunk, 'branch) unstep =
    { (** [unstep_left k h l r] undoes a [LeftBranch] step, reconstituting
          the parent value [: 'trunk] with base key [k] at height [h] from the
          left child value [l : 'trunk] (from the subject and context) and the
          right child value [r : 'branch] (from the step). *)
      unstep_left: key -> int -> 'trunk -> 'branch -> 'trunk ;
      (** [unstep_right k h l r] undoes a [LeftBranch] step, reconstituting
          the parent value [: 'trunk] with base key [k] at height [h] from the
          left child value [l : 'branch] (from the step) and the right child
          value [r : 'trunk] (from the subject and context). *)
      unstep_right: key -> int -> 'branch -> 'trunk -> 'trunk ;
      (** [unstep_skip k h l b c] undoes a [SkipChild] step with base key [k]
          at height [h] given length [l] and bits [b] from the step and child
          value [c : 'trunk] from the subject. *)
      unstep_skip: key -> int -> int -> key -> 'trunk -> 'trunk }

  (** Constructor for a [unstep], assuming equal treatment of left and right. *)
  val symmetric_unstep:
    branch:(key -> int -> 'a -> 'a -> 'a) ->
    skip:(key -> int -> int -> key -> 'a -> 'a) -> ('a, 'a) unstep

  (** [step_apply unstep (trie, costep) step] walks back the [step]
      from the subject [value] subject and context [costep] given the
      methods from [unstep], yielding a subject and context for the
      reconstituted wider trie. *)
  val step_apply : ('trunk, 'branch) unstep -> ('trunk * costep) -> 'branch step -> ('trunk * costep)

  (** [path_apply unstep value (costep, steps)] walks back the entire [path],
      folding [step_apply] on each step in [steps] starting from the subject
      [value] and the context of height and key [costep] *)
  val path_apply : ('trunk, 'branch) unstep -> 'trunk -> 'branch path -> ('trunk * costep)

  include MapS
    with type key := key
     and type value := value
     and type t := t
     and type (+'a) step := 'a step
     and type (+'a) path := 'a path

  (** [trie_height t] returns None iff [t] is empty,
      and otherwise returns [Some h] where 2^h
      (or [1 lsl h]) is the width of [t]. *)
  val trie_height : t -> int option

  (** [ensure_height h t] is a tree of height exactly [h] with the same
      mappings as [t], with a [Skip] of adequate length with bits [Key.zero]
      if needed.
      If [t] is empty, it still returns empty. *)
  val ensure_height : int option -> t -> t

  (** Given two trees or possibly distinct heights, return a pair of trees of
      the same height as each other (being the max of the heights of the two
      input tries), each with same contents as the respective argument trie. *)
  val ensure_same_height : t -> t -> t*t

  (** Extract the cached synthesized attribute computed with a wrapped node *)
  val get_synth : t -> synth

  (** Assert consistent values across all nodes in tree *)
  val check_invariant : t -> bool

  (** Assert consistent values across all nodes in tree, and return tree *)
  val verify : t -> t

  (** How many levels of a tree the step traverses *)
  val step_length : 'a step -> int

  (** True if path is consistent with tree context at start of path *)
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

  include YojsonableS with type t := t
end

(** A module for Big-Endian Patricia Tree, a.k.a. Trie. *)
module Trie
    (Key : UIntS) (Value : YojsonableRlpS) (WrapType : WrapTypeS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
    (TrieType : TrieTypeS with type key = Key.t
                           and type value = Value.t
                           and type +'a wrap = 'a WrapType.t
                           and type synth = Synth.t)
    (Wrap : WrapS with type value = TrieType.trie and type t = TrieType.trie WrapType.t)
  : TrieS with type key = Key.t
           and type value = Value.t
           and type +'a wrap = 'a WrapType.t
           and type synth = Synth.t
           and type trie = TrieType.trie
           and type t = TrieType.trie WrapType.t

module type TrieSetS = sig
  type elt
  module T : TrieS with type key = elt and type value = unit
  type t = T.t
  [@@deriving rlp]
  include Set.S with type elt := elt and type t := T.t
  val lens : elt -> (t, bool) Lens.t
  include PersistableS with type t := t
end

module TrieSet (Elt : UIntS) (T : TrieS with type key = Elt.t and type value = unit) :
  TrieSetS with type elt = Elt.t and module T = T

module type SimpleTrieS = TrieS with type 'a wrap = 'a and type synth = unit

module SimpleTrie (Key : UIntS) (Value : YojsonableRlpS) : SimpleTrieS with type key = Key.t and type value = Value.t

module SimpleTrieSet (Elt : UIntS) : TrieSetS with type elt = Elt.t

module RevisionSet : TrieSetS with type elt = Revision.t
