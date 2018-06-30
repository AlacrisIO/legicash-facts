(* keypair.mli *)

(** the type of public-key key pairs *)
type t =
  { private_key: Secp256k1.Key.secret Secp256k1.Key.t
  ; public_key: Secp256k1.Key.public Secp256k1.Key.t
  ; address: Crypto.Address.t }

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keys_from_hex : string -> string -> t

(** given hex-string public, private keys, generate Secp256k1 key pair *)
val make_keys : string -> string -> t

(** given hex-string public key, generate Secp256k1 public key *)
val make_public_key : string -> Secp256k1.Key.public Secp256k1.Key.t

(** given hex-string private key, generate Secp256k1 private key *)
val make_private_key : string -> Secp256k1.Key.secret Secp256k1.Key.t

