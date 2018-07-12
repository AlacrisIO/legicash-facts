open Lib
open Integer

val secp256k1_ctx : Secp256k1.Context.t
(** Secp256k1 context for signing and validation *)

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(** a cryptographic digest, "hash", for an object of type 'a *)
module Digest : sig
  include IntS

  val make : 'a -> t
  (** polymorphic 256-bit digest maker; for demos/tests
      use instances of DigestibleS, below, for real data *)

  val digest : t -> t
  (** recursively digest a digest *)
end

type 'a digest = Digest.t

val null_digest : Digest.t

module type DigestibleS = sig
  type t
  val digest: t -> t digest
end

module type IntS = sig
  include Integer.IntS
  include DigestibleS with type t := t
end

module Nat : IntS with type t = Integer.Nat.t
module UInt64 : IntS with type t = Integer.UInt64.t
module UInt256 : IntS with type t = Z.t

(** sequence number for changes in a side-chain *)
module Revision : IntS

(** type of a timestamp *)
module Timestamp : IntS

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : IntS

module Address : sig
  include IntS
  include DigestibleS with type t := t
  include ShowableS with type t := t
  val address_size : int
  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t
end

(** a signature for an object of type 'a *)
type 'a signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: 'a signature}

val is_signature_valid : Address.t -> 'a signature -> 'a -> bool
(** check signature for given value *)

val is_signed_value_valid : Address.t -> 'a signed -> bool
(** check signature for payload within a signed value *)

val make_signature : private_key -> 'a -> 'a signature

val sign : private_key -> 'a -> 'a signed

(** count of changes in an object.
    A positive integer less than 2**63, incremented at every change to a notional object's state.
    In Quake, we used to call that the mod-count (modification count).
    Can serve as a local counter in a Lamport clock.
    Should a user record have two of them, one for the user one for the server?
    Not for now: the server can use the global clock for the server.
*)

module StringT : sig
  include DigestibleS with type t = string
end

module Unit : sig
  include DigestibleS with type t = unit
  include ShowableS with type t := unit
end
