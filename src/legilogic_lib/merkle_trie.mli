(* A big endian patricia tree maps non-negative integers to values. *)
open Lib
open Yojsoning
open Marshaling
open Digesting
open Persisting
open Types
open Trie

(** Signature for Trie which synthesizes digests from marshaled node values. *)
module type TrieSynthMerkleS = sig
  include TrieSynthS with type t = digest
  val marshal_empty : unit marshaler
  val marshal_leaf : value marshaler
  val marshal_branch : (int*digest*digest) marshaler
  val marshal_skip : (int*int*key*digest) marshaler
end

(** Persistable merkle tree. [Key]s are bit paths (right/left), [Value]s are the
    tree type. *)
module TrieSynthMerkle (Key : UIntS) (Value : PersistableS)
  : TrieSynthMerkleS with type key = Key.t
                      and type value = Value.t

(** Peristable merkle tree over persistable values of type [t] wrapped by type
    [T] *)
module type MerkleTrieTypeS = sig
  include TrieTypeS
  module Trie : PersistableS with type t = trie
  module T : PersistableS with type t = t
end

(** Persistable merkle tree with methods for recursive synthesization of node
    digests. *)
module MerkleTrieType (Key : UIntS) (Value : PersistableS)
    (Synth : TrieSynthS with type key = Key.t and type value = Value.t)
  : MerkleTrieTypeS with type key = Key.t
                     and type value = Value.t

(** Signature for the merkle proof *)
module type MerkleTrieProofS = sig
  type key
  type value
  type mtrie
  type 'a step (* [step] ends up being identified with [MerkleTrie.step] *)
  type t =
    {
      (** Database key for the leaf node *)
      key : key
    (** Root commitment (the actual merkle hash for the trie) *)
    ; trie : Digest.t
    (** Hash value for the leaf *)
    ; leaf : Digest.t
    (** Hashes to pair with in the proof, and their locations *)
    ; steps : (Digest.t step) list
    }
  (** [get k mt] is the proof that the object referenced in the db by k is in
      mt. *)
  val get : key -> mtrie -> t option
  (** [check proof mt k v] verifies that [proof] is a proof for membership of
      [v] in [mt], where [v] is referenced by [k] in the db. *)
  val check : t -> mtrie -> key -> value -> bool
  include PersistableS with type t := t
end

(** Merkle Trie *)
module type MerkleTrieS = sig
  type key
  type value
  module Synth : TrieSynthS with type t = unit and type key = key and type value = value
  (* Contains the logic for recursively computing merkle digest of tree: *)
  module SynthMerkle : TrieSynthMerkleS with type key = key and type value = value
  module Type : TrieTypeS with type key = key and type value = value
  include TrieS
    with type key := key
     and type value := value
     and type 'a wrap = 'a dv
  module Wrap : WrapS with type value = trie and type t = t
  include PersistableS
    with type t := t

  (* Return the merkle commitment for the trie *)
  val trie_digest : t -> Digest.t
  (* Return the merkle proof for the given path *)
  val path_digest : t path -> Digest.t path

  module Proof : MerkleTrieProofS
    with type key = key and type value = value and type mtrie = t and type 'a step = 'a step
end

module MerkleTrie (Key : UIntS) (Value : PersistableS)
  : MerkleTrieS with type key = Key.t and type value = Value.t

(** Proofs that [elt]s are members of tries [mts].  *)
module type MerkleTrieSetProofS = sig
  type elt
  type mts
  type 'a step
  type t =
    { elt : elt
    ; trie : Digest.t
    ; steps : (Digest.t step) list }
  val get : elt -> mts -> t option
  val check : t -> mts -> elt -> bool
  include YojsonableS with type t := t
end

(** Merkle tries where the only concern is membership, not tree location. *)
module type MerkleTrieSetS = sig
  type elt
  module T : MerkleTrieS with type key = elt and type value = unit
  include PersistableS with type t = T.t
  include Set.S with type elt := elt and type t := t
  module Proof : MerkleTrieSetProofS
    with type elt = elt and type mts = t and type 'a step = 'a T.step
  val trie_digest : t -> Digest.t
  (** get/set membership of [elt]s **)
  val lens : elt -> (t, bool) Lens.t
end

module MerkleTrieSet (Elt : UIntS) : (MerkleTrieSetS with type elt = Elt.t)

module DigestSet : (MerkleTrieSetS with type elt = Digest.t)
