(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Crypto
open Marshaling
open Db
open Trie

module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = digest
  val leaf_digest : digest -> t
  val marshal_empty : unit marshaler
  val marshal_leaf : digest marshaler
  val marshal_branch : (int*digest*digest) marshaler
  val marshal_skip : (int*int*key*digest) marshaler
end

module TrieSynthMerkle (Key : IntS) (Value : DigestibleS)
  : TrieSynthMerkleS with type key = Key.t
                      and type value = Value.t

module type MerkleTrieTypeS = sig
  include TrieTypeS
  module T : PersistableS with type t = t
  module Trie : PersistableS with type t = trie
  include T with type t := t
end

module MerkleTrieType (Key : IntS) (Value : PersistableS)
    (Synth : TrieSynthMerkleS with type key = Key.t and type value = Value.t)
  : MerkleTrieTypeS with type key = Key.t
                     and type value = Value.t

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
  val json_of_proof : proof -> Yojson.Basic.json
end

module MerkleTrie (Key : IntS) (Value : PersistableS)
  : MerkleTrieS with type key = Key.t and type value = Value.t

module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  include PersistableS with type t = T.t
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
