(* A big endian patricia tree maps non-negative integers to values. *)
open Crypto
open Marshaling
open Db
open Trie

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
