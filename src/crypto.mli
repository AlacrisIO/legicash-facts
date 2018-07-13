open Lib
open Marshaling
open Integer

val secp256k1_ctx : Secp256k1.Context.t
(** Secp256k1 context for signing and validation *)

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

type 'a digest = Nat.t

module type DigestibleS = sig
  include MarshalableS
  val marshal_bytes : t -> Bytes.t
  val unmarshal_bytes : Bytes.t -> t
  val digest : t -> t digest
end

module type IntS = sig
  include Integer.IntS
  include DigestibleS with type t := t
end

val digest_of_string : string -> UInt256.t
val digest_of_marshal_bytes : ('a -> Bytes.t) -> 'a -> UInt256.t

module DigestibleOfMarshalable (M : MarshalableS) : DigestibleS with type t = M.t

module UInt256 : IntS with type t = Z.t
module Data256 : IntS with type t = Z.t
module Digest : IntS with type t = Z.t
module UInt64 : IntS with type t = Integer.UInt64.t

val digest_of_string : string -> Digest.t
val null_digest : Digest.t

(** sequence number for changes in a side-chain *)
module Revision : IntS

(** type of a timestamp *)
module Timestamp : IntS

(** duration in terms of nanoseconds, for use in timeouts. TODO: should the unit be consensus cycles instead? *)
module Duration : IntS

module Address : sig
  include IntS with type t = UInt256.t
  include DigestibleS with type t := t
  include ShowableS with type t := t
  val address_size : int
  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t
end

(** a signature for an object of type 'a *)
type signature

module Signature : MarshalableS with type t = signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: signature}

val is_signature_valid : ('a -> Digest.t) -> Address.t -> signature -> 'a -> bool
(** check signature for given value *)

val is_signed_value_valid : ('a -> Digest.t) -> Address.t -> 'a signed -> bool
(** check signature for payload within a signed value *)

val make_signature : ('a -> Digest.t) -> private_key -> 'a -> signature

val sign : ('a -> Digest.t) -> private_key -> 'a -> 'a signed

val marshal_signed : 'a marshaler -> 'a signed marshaler

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
