(* keypair.mli *)

(** the type of public-key key pairs *)
type t =
  { private_key: Secp256k1.Key.secret Secp256k1.Key.t
  ; public_key: Secp256k1.Key.public Secp256k1.Key.t
  ; address: Crypto.Address.t }

val make_keys_from_hex : string -> string -> t
(** given hex-string public, private keys, generate Secp256k1 key pair *)
