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
val compare : t-> t -> int
