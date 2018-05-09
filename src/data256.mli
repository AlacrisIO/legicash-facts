(* 256-bit keys in Legicash *)

type 'a t

exception Invalid_key_length

(** from a list of 32 chars *)
val of_list : char list -> 'a t

(** from array of 32 chars *)
val of_array : char array -> 'a t

(** from string of 32 chars *)
val of_string : string -> 'a t

(** from Data256.t to string of 32 chars *)
val to_string : 'a t -> string

(** generate a random public key *)
val generate : unit -> 'a t

(** compare two public keys, needed when passing this module to the Map.S functor *)
val compare : 'a t -> 'a t -> int

(** null value which doesn't have a knowable private key (or preimage when used as digest) *)
val zero : 'a t

(** TODO: fake value until we have actual stuff *)
val one : 'a t
