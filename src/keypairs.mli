(* keypairs.mli *)

(** the type of public-key key pairs *)
type t =
  { private_key: Secp256k1.Key.secret Secp256k1.Key.t
  ; public_key: Secp256k1.Key.public Secp256k1.Key.t
  ; address: Legibase.Address.t
  }

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keys_from_hex : string -> string -> t

(** does address match the address associated with the public key *)
val address_matches_public_key : Legibase.Address.t -> Secp256k1.Key.public Secp256k1.Key.t -> bool
