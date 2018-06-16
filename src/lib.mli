(* General purpose library
   Stuff here should probably be imported from existing libraries if possible,
   or else upstreamed then imported if possible,
   or else published as their own library.
*)

(* Common exceptions *)

(** exception raised when we haven't implemented the damn thing yet *)
exception Not_implemented

(** internal_error, which should never happen in production, with explanation *)
exception Internal_error of string

(** the bottom function turns anything into anything, by raising an exception *)
val bottom : 'a -> 'b

(* SKI combiantors and their name:
   https://www.johndcook.com/blog/2014/02/06/schonfinkel-combinators/ *)

(** SKI combinators, 1: Identitätsfunktion *)
val identity : 'a -> 'a

(** SKI combinators, 2: Konstanzfunktion, nice to use with defaulting *)
val konstant : 'a -> 'b -> 'a

(** SKI combinators, 3: verSchmelzungsfunktion (amalgamation function; smelting) *)
val schoenfinkel : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(** SKI combinators, 4: verTauschungsfunktion (exchange function) *)
val transpose : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** SKI combinators, 5: Zusammensetzungsfunktion (composition function) *)
val zcompose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c


(* Options *)

(** Unwrap an option with a default if None *)
val defaulting : (unit -> 'a) -> 'a option -> 'a

(** Unwrap an option, throwing Not_found if None *)
val unwrap_option : 'a option -> 'a

(** Return true if the option is Some _ *)
val is_option_some : 'a option -> bool

(** Trivial functor from option to list *)
val list_of_option : 'a option -> 'a list

(** Map a function to the content of the option, if any *)
val option_map : ('a -> 'b) -> 'a option -> 'b option

(** Parse a hex string into a binary string,
    where the string is of form "nn:nn:...:nn", where nn represents a char as a hex-digit pair *)
val parse_hex : string -> string

(** Unparse a binary string into a hex string, reversing parse_hex *)
val unparse_hex : string -> string
