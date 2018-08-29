(** Signing data using Secp256k1 public-key cryptography *)
open Yojsoning
open Marshaling
open Integer
open Digesting

(** Address identifying a party (user, facilitator).
    Per Ethereum, use the low 160-bits of the Keccak256 digest of the party's public key *)
module Address : sig
  include UIntS
  include YojsonMarshalableS with type t := t
end
type address = Address.t

(** Public key in Secp256k1 public-key cryptography *)
module PublicKey : YojsonMarshalableS (* with type t = Secp256k1.Key.public Secp256k1.Key.t *)
type public_key = PublicKey.t

(** Private key in Secp256k1 public-key cryptography *)
module PrivateKey : YojsonMarshalableS (* with type t = Secp256k1.Key.secret Secp256k1.Key.t *)
type private_key = PrivateKey.t

(** Pair of public and private keys for Secp256k1 public key cryptography,
    with matching Ethereum address *)
module Keypair : sig
  type t = { address: Address.t; public_key: PublicKey.t; private_key: PrivateKey.t }
  [@@deriving lens {prefix=true}]
  include YojsonMarshalableS with type t := t
end
type keypair = Keypair.t

(** Signature of a message per Secp256k1 public-key cryptography *)
type signature
module Signature : YojsonMarshalableS with type t = signature

(** Record of an object of type 'a with its signature by one party *)
type 'a signed = {payload: 'a; signature: signature}


module type SignableS = sig
  include DigestibleS
  val signed : keypair -> t -> t signed
end

module Signable (M : MarshalableS) : sig
  include DigestibleS with type t = M.t
  val signed : keypair -> t -> t signed
end

val signed_of_digest : ('a -> digest) -> keypair -> 'a -> 'a signed

(** Secp256k1 context for signing and validation *)
(* val secp256k1_ctx : Secp256k1.Context.t *)

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keypair_from_hex : string -> string -> keypair

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keypair : string -> string -> keypair

(** given hex-string public key, generate Secp256k1 public key *)
val make_public_key : string -> public_key

(* TODO: deduce public_key from private_key *)

(** given hex-string private key, generate Secp256k1 private key *)
val make_private_key : string -> private_key

(** Register a keypair -- typical usage would be to do that from reading a configuration file,
    or with the notional equivalent of ssh-add. *)
val register_keypair : keypair -> unit

(** Unregister a keypair *)
val unregister_keypair : keypair -> unit

(** given an address, find the corresponding keypair in suitable configuration files *)
val keypair_of_address : address -> keypair

val address_of_public_key : public_key -> address

val is_signature_valid : ('a -> digest) -> address -> signature -> 'a -> bool
(** check signature for given value *)

val is_signed_value_valid : ('a -> digest) -> address -> 'a signed -> bool
(** check signature for payload within a signed value *)

val make_signature : ('a -> digest) -> private_key -> 'a -> signature

(* Extract the v, r, s parameters of the signature, as a triplet of big endian strings *)
val signature_vrs : signature -> string * string * string

val signed : ('a -> digest) -> private_key -> 'a -> 'a signed

val marshal_signed : 'a marshaler -> 'a signed marshaler
(** marshaler for 'a signed, parameterized by the marshaler for the payload of type 'a *)

val unmarshal_signed : 'a unmarshaler -> 'a signed unmarshaler
(** unmarshaler for 'a signed, parameterized by the unmarshaler for the payload of type 'a *)

val marshaling_signed : 'a marshaling -> 'a signed marshaling

val signed_to_yojson : 'a to_yojson -> 'a signed to_yojson
val signed_of_yojson : 'a of_yojson -> 'a signed of_yojson
val signed_of_yojson_exn : 'a of_yojson_exn -> 'a signed of_yojson_exn

(* keys, address for tests, demos *)
module Test : sig
  val trent_keys : keypair
  val trent_address : address
  val alice_keys : keypair
  val alice_address : address
  val bob_keys : keypair
  val bob_address : address
end