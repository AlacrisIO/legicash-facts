open Legibase

val secp256k1_ctx : Secp256k1.Context.t
(** Secp256k1 context for signing and validation *)

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(** a cryptographic digest, "hash", for an object of type 'a *)
module Digest : sig
  include module type of Integer.Nat

  val make : 'a -> t
  (** polymorphic 256-bit digest maker; for demos/tests
      use instances of DigestibleS, below, for real data *)
end

type 'a digest = Digest.t

val null_digest : Digest.t

module DigestSet : sig
  include Set.S with type elt = Digest.t

  val lens : Digest.t -> (t, bool) Lens.t
end

module type DigestibleS = sig
  type t
  val digest: t -> t digest
end

module Address : sig
  type t [@@deriving show]

  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t

  val of_string : string -> t

  val to_string : t -> string

  val equal : t -> t -> bool

  val digest : t -> t digest
end

(** a signature for an object of type 'a *)
type 'a signature

val is_signature_valid : Address.t -> 'a signature -> 'a -> bool

val make_signature : private_key -> 'a -> 'a signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: 'a signature}

val sign : private_key -> 'a -> 'a signed

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
*)

(** a pure mapping from PublicKey.t to 'a suitable for use in interactive merkle proofs *)
module AddressMap : MapS with type key = Address.t
