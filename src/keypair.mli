(* keypair.mli *)
open Crypto
open Db

(** the type of public-key key pairs *)
type t = keypair

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keypair_from_hex : string -> string -> t

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keypair : string -> string -> t

(** given hex-string public key, generate Secp256k1 public key *)
val make_public_key : string -> public_key

(* TODO: deduce public_key from private_key *)

(** given hex-string private key, generate Secp256k1 private key *)
val make_private_key : string -> private_key

(** Register a keypair -- typical usage would be to do that from reading a configuration file,
    or with the notional equivalent of ssh-add. *)
val register_keypair : t -> unit

(** Unregister a keypair *)
val unregister_keypair : t -> unit

(** given an address, find the corresponding keypair in suitable configuration files *)
val keypair_of_address : Address.t -> t

include PersistableS with type t := t

(* keys, address for tests, demos *)
module Test : sig
  val trent_keys : t
  val trent_address : Address.t
  val alice_keys : t
  val alice_address : Address.t
  val bob_keys : t
  val bob_address : Address.t
end
