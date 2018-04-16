(** exception raised when we haven't implemented the damn thing yet *)
exception Not_implemented

(** the bottom function turns anything into anything, by raising an exception *)
val bottom : 'a -> 'b

(** Trivial functor from option to list *)
val list_of_option : 'a option -> 'a list

(** a pure mapping from Key256.t to 'a suitable for use in interactive merkle proofs *)
module Key256Map : Map.S
type 'a key256_patricia_merkle_trie

(** is that Int64 number odd? *)
val is_odd_64 : Int64.t -> bool
