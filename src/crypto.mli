open Marshaling
open Integer

val secp256k1_ctx : Secp256k1.Context.t
(** Secp256k1 context for signing and validation *)

type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private counterpart to public key *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

module Digest : IntS

type digest = Digest.t

module type DigestibleS = sig
  type t
  val digest: t -> digest
end

module DigestibleOfMarshalable (M : MarshalableS) : DigestibleS with type t = M.t

module DigestibleOfPreMarshalable (P : PreMarshalableS) : DigestibleS with type t = P.t

val digest_of_marshal_bytes : ('a -> Bytes.t) -> 'a -> Digest.t
val digest_of_marshal : 'a marshaler -> 'a -> Digest.t
val digest_of_string : string -> digest
val null_digest : digest

(** An Address identifies a party (user, facilitator)
    per Ethereum, use the last 20 bytes of the Keccak256 hash of the party's public key *)
module Address : sig
  include IntS (* with type t = Z.t *)
  val address_size : int
  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t
end

(** a signature for an object of type 'a *)
type signature

module Signature : PreMarshalableS with type t = signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: signature}

val is_signature_valid : ('a -> digest) -> Address.t -> signature -> 'a -> bool
(** check signature for given value *)

val is_signed_value_valid : ('a -> digest) -> Address.t -> 'a signed -> bool
(** check signature for payload within a signed value *)

val make_signature : ('a -> digest) -> private_key -> 'a -> signature

val sign : ('a -> digest) -> private_key -> 'a -> 'a signed

val marshal_signed : 'a marshaler -> 'a signed marshaler
(** marshaler for 'a signed, parameterized by the marshaler for the payload of type 'a *)

val unmarshal_signed : 'a unmarshaler -> 'a signed unmarshaler
(** unmarshaler for 'a signed, parameterized by the unmarshaler for the payload of type 'a *)

val marshaling_signed : 'a marshaling -> 'a signed marshaling

