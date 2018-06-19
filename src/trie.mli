(* Big Endian Patricia Trees (Tries)
   See article "Fast Mergable Integer Maps" by Chris Okasaki & Andrew Gill, 1998
   http://www.eecs.usma.edu/webs/people/okasaki/ml98maps.ps
*)

(* A big endian patricia tree maps non-negative integers to values.
*)
open Lib
open Integer
open Crypto

module type TrieSynthS = sig
  type value
  type t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
end

module type TrieSkipSynthS = sig
  include TrieSynthS
  type key
  val skip : int -> int -> key -> t -> t
end

module TrieSynthCardinal (Key : UnsignedS) (Value : T) : sig
  type key = Key.t
  type value = Value.t
  type t = Z.t
  val empty : t
  val leaf : value -> t
  val branch : int -> t -> t -> t
  val skip : int -> int -> key -> t -> t
end

module TrieSynthComputeSkip (Key : UnsignedS) (Synth: TrieSynthS) : sig
  include TrieSynthS
  type key = Key.t
end

module type TrieS = sig
  type trie_key
  type trie_value
  type synth

  type trie =
    | Empty
    | Leaf of {value: trie_value; synth: synth}
    | Branch of {left: trie; right: trie; height: int; synth: synth}
    | Skip of {child: trie; bits: trie_key; length: int; height: int; synth: synth}

  type 'a trie_step =
    | LeftBranch of {right: 'a}
    | RightBranch of {left: 'a}
    | SkipChild of {bits: trie_key; length: int}

  type 'a trie_path = {index: trie_key; height: int; steps: 'a trie_step list}

  include MapS
    with type key = trie_key
     and type value = trie_value
     and type t = trie
     and type 'a step = 'a trie_step
     and type 'a path = 'a trie_path

  val trie_height : t -> int
  val ensure_height : int -> t -> t
  val ensure_same_height : t -> t -> t*t
  val get_synth : t -> synth
  val check_invariant : t -> bool
  val verify : t -> t
  val step_length : 'a step -> int
  val check_path_consistency : 'a path -> bool
end

module Trie (Key : UnsignedS) (Value : T)
    (Synth : TrieSkipSynthS with type key = Key.t and type value = Value.t)
  : TrieS with type trie_key = Key.t
           and type trie_value = Value.t
           and type synth = Synth.t

module type TrieSynthMerkleS = sig
  include TrieSkipSynthS
  val leaf_digest : Digest.t -> t
end

module TrieSynthMerkle (Key : UnsignedS) (Value : Digestible) : sig
  include TrieSynthMerkleS
    with type key = Key.t
     and type value = Value.t
     and type t = Digest.t
end

module MerkleTrie (Key : UnsignedS) (Value : Digestible) : sig
  module Synth : TrieSynthMerkleS with type key = Key.t and type value = Value.t and type t = Digest.t
  include TrieS
    with type trie_key = Key.t
     and type trie_value = Value.t
     and type synth = Synth.t

  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path
  val get_proof : key -> t -> (key*Digest.t*Digest.t*(Digest.t step list)) option
  val check_proof_consistency : key*Digest.t*Digest.t*(Digest.t step list) -> bool
end
