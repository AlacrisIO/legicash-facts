(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Yojsoning
open Marshaling
open Crypto
open Db
open Trie

module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = digest
  val marshal_empty : unit marshaler
  val marshal_leaf : value marshaler
  val marshal_branch : (int*digest*digest) marshaler
  val marshal_skip : (int*int*key*digest) marshaler
end

module TrieSynthMerkle (Key : UIntS) (Value : PersistableS)
  : TrieSynthMerkleS with type key = Key.t
                      and type value = Value.t

module type MerkleTrieTypeS = sig
  include TrieTypeS
  module Trie : PersistableS with type t = trie
  module T : PersistableS with type t = t
end

module MerkleTrieType (Key : UIntS) (Value : PersistableS)
    (Synth : TrieSynthMerkleS with type key = Key.t and type value = Value.t)
  : MerkleTrieTypeS with type key = Key.t
                     and type value = Value.t

module type MerkleTrieProofS = sig
  type key
  type value
  type mtrie
  type 'a pstep
  type t =
    { key : key
    ; trie : Digest.t
    ; leaf : Digest.t
    ; steps : (Digest.t pstep) list
    }
  val get : key -> mtrie -> t option
  val check : t -> mtrie -> key -> value -> bool
  include YojsonableS with type t := t
end

(** Merkle Trie *)
module type MerkleTrieS = sig
  type key
  type value
  module Synth : TrieSynthMerkleS with type key = key and type value = value
  module Type : TrieTypeS with type key = key and type value = value
  include TrieS
    with type key := key
     and type value := value
     and type 'a wrap = 'a dv
  module Wrap : WrapS with type value = trie and type t = t
  include PersistableS
    with type t := t

  val trie_digest : t -> Digest.t
  val path_digest : t path -> Digest.t path

  module Proof : MerkleTrieProofS
    with type key = key and type value = value and type mtrie = t and type 'a pstep = 'a step
end

module MerkleTrie (Key : UIntS) (Value : PersistableS)
  : MerkleTrieS with type key = Key.t and type value = Value.t

module type MerkleTrieSetProofS = sig
  type elt
  type mts
  type 'a pstep
  type t =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t pstep) list }
  val get : elt -> mts -> t option
  val check : t -> mts -> elt -> bool
  include YojsonableS with type t := t
end

module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  include PersistableS with type t = T.t
  include Set.S with type elt := elt and type t := t
  module Proof : MerkleTrieSetProofS
    with type elt = elt and type mts = t and type 'a pstep = 'a T.step
  val trie_digest : t -> Digest.t
  val lens : elt -> (t, bool) Lens.t
end

module MerkleTrieSet (Elt : UIntS) : (MerkleTrieSetS with type elt = Elt.t)

module DigestSet : (MerkleTrieSetS with type elt = Digest.t)
