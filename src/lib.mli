(* General purpose library
   Stuff here should probably be imported from existing libraries if possible,
   or else upstreamed then imported if possible,
   or else published as their own library.
 *)

(** exception raised when we haven't implemented the damn thing yet *)
exception Not_implemented

(** the bottom function turns anything into anything, by raising an exception *)
val bottom : 'a -> 'b

(** Trivial functor from option to list *)
val list_of_option : 'a option -> 'a list

(** is that Int64 number odd? *)
val is_odd_64 : Int64.t -> bool

val constantly : 'a -> 'b -> 'a

val option_map : ('a -> 'b) -> 'a option -> 'b option

(** hex strings, of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
val parse_hex : string -> string
val unparse_hex : string -> string
