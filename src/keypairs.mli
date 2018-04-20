open Base
open Bigarray

type t =
  { private_key: Secp256k1.Key.secret Secp256k1.Key.t
  ; public_key: Secp256k1.Key.public Secp256k1.Key.t }

val alice_keys : t
(** some sample key pairs *)

val bob_keys : t

val trent_keys : t
