(* Big Endian Patricia Trees (Tries)
   See notably the article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Integer
open Crypto

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
  include TreeSynthS
  type key = Key.t
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

  type (+'a) path = {index: key; height: int; steps: 'a step list}

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
end

(** A module for Big-Endian Patricia Tree, a.k.a. Trie. *)
module Trie (Key : IntS) (Value : T)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
  : TrieS with type key = Key.t
           and type value = Value.t
           and type synth = Synth.t

(** A signature for Merklizing a Trie . *)
module type TrieSynthMerkleS = sig
  include TrieSynthS
  val leaf_digest : Digest.t -> t
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
  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path
  val get_proof : key -> t -> proof option
  val check_proof_consistency : proof -> bool
end

module MerkleTrie (Key : IntS) (Value : DigestibleS) :
  MerkleTrieS with type key = Key.t and type value = Value.t
