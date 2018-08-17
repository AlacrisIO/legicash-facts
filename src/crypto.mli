open Yojsoning
open Marshaling
open Integer

module Digest : UIntS

(** type for digests (using keccak256) *)
type digest = Digest.t

(** public key in public-key cryptography *)
type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private key in public-key cryptography *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(** a signature for an object of type 'a *)
type signature

(** an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: signature}

(** given a string, return its keccak256 digest as a big-endian string *)
val keccak256_string : string -> string

(** given a string, return its keccak256 digest as an object *)
val digest_of_string : string -> digest

(** given a marshaler and an object, return its digest *)
val digest_of_marshal : 'a marshaler -> 'a -> digest

(** given a marshal_bytes method and an object, return its digest *)
val digest_of_marshal_bytes : ('a -> Bytes.t) -> 'a -> digest

(** a digest object full of zeroes, as a placeholder *)
val null_digest : digest

(** Secp256k1 context for signing and validation *)
val secp256k1_ctx : Secp256k1.Context.t

(** An Address identifies a party (user, facilitator)
    per Ethereum, use the last 20 bytes of the Keccak256 hash of the party's public key *)
module Address : sig
  include UIntS (* with type t = Z.t *)
  val size_in_bits : int
  val size_in_bytes : int
  val of_public_key : public_key -> t
end

(* type for a pair of public and private keys *)
type keypair =
  { private_key: private_key
  ; public_key: public_key
  ; address: Address.t }

module Signature : YojsonMarshalableS with type t = signature

val is_signature_valid : ('a -> digest) -> Address.t -> signature -> 'a -> bool
(** check signature for given value *)

val is_signed_value_valid : ('a -> digest) -> Address.t -> 'a signed -> bool
(** check signature for payload within a signed value *)

val make_signature : ('a -> digest) -> private_key -> 'a -> signature

val signed : ('a -> digest) -> private_key -> 'a -> 'a signed

val marshal_signed : 'a marshaler -> 'a signed marshaler
(** marshaler for 'a signed, parameterized by the marshaler for the payload of type 'a *)

val unmarshal_signed : 'a unmarshaler -> 'a signed unmarshaler
(** unmarshaler for 'a signed, parameterized by the unmarshaler for the payload of type 'a *)

val marshaling_signed : 'a marshaling -> 'a signed marshaling

val signed_to_yojson : 'a to_yojson -> 'a signed to_yojson
val signed_of_yojson : 'a of_yojson -> 'a signed of_yojson
val signed_of_yojson_exn : 'a of_yojson_exn -> 'a signed of_yojson_exn

module type DigestibleS = sig
  include MarshalableS
  val digest : t -> digest
  val signed : keypair -> t -> t signed
end

module Digestible (M : MarshalableS) : DigestibleS with type t = M.t

module DigestibleOfPreMarshalable (P : PreMarshalableS) : DigestibleS with type t = P.t
