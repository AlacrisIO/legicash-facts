(* 256-bit keys in Legicash *)

type t

exception Invalid_key_length

(** public key from a list of 32 chars *)
val of_list : char list -> t

(** public key from array of 32 chars *)
val of_array : char array -> t

(** generate a random public key *)
val generate : unit -> t

(** compare two public keys, needed when passing this module to the Map.S functor *)
val compare : t -> t -> int

(** null value which doesn't have a knowable private key (or preimage when used as digest) *)
val zero : t

(** TODO: fake value until we have actual stuff *)
val one : t
