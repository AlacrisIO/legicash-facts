(* TODO: combine the two sets of combinators for Yojsoning and Marshaling? *)
open Lib

type yojson = Yojson.Safe.json
val string_of_yojson : yojson -> string
val yojson_of_string : string -> yojson

module YoJson : sig
  include module type of Yojson.Safe.Util
  val mem : string -> yojson -> bool
end


type 'a to_yojson = 'a -> yojson
type 'a of_yojson = yojson -> ('a, string) result
type 'a of_yojson_exn = yojson -> 'a
type 'a yojsoning = { to_yojson: 'a to_yojson; of_yojson: 'a of_yojson }

val of_yojson_exn_of_of_yojson : 'a of_yojson -> 'a of_yojson_exn
val of_yojson_of_of_yojson_exn : 'a of_yojson_exn -> 'a of_yojson
val to_yojson_string_of_to_yojson : 'a to_yojson -> ('a -> string)
val of_yojson_string_exn_of_of_yojson_exn : 'a of_yojson_exn -> (string -> 'a)

val to_yojson_map : ('x -> 'a) -> 'a to_yojson -> 'x to_yojson
val of_yojson_map : ('a -> 'x) -> 'a of_yojson -> 'x of_yojson
val yojsoning_map : ('x -> 'a) -> ('a -> 'x) -> 'a yojsoning -> 'x yojsoning


(* NB: These functions assume that the basic encoding does NOT have `Null, and encode None as `Null. *)
val option_to_yojson : 'a to_yojson -> 'a option to_yojson
val option_of_yojson : 'a of_yojson -> 'a option of_yojson
val option_of_yojson_exn : 'a of_yojson_exn -> 'a option of_yojson_exn
val option_yojsoning : 'a yojsoning -> 'a option yojsoning

val list_to_yojson : 'a to_yojson -> 'a list to_yojson
val list_of_yojson : 'a of_yojson -> 'a list of_yojson
val list_of_yojson_exn : 'a of_yojson_exn -> 'a list of_yojson_exn
val list_yojsoning : 'a yojsoning -> 'a list yojsoning

val string_yojsoning : string yojsoning

val string_of_char : char -> string
val char_of_string : string -> char
val char_yojsoning : char yojsoning

module type PreYojsonableS = sig
  type t
  val yojsoning : t yojsoning
end

module type YojsonableS = sig
  include PreYojsonableS
  val to_yojson : t to_yojson
  val of_yojson : t of_yojson
  val of_yojson_exn : t of_yojson_exn
  val to_yojson_string : t -> string
  val of_yojson_string_exn : string -> t
end

module Yojsonable (P : PreYojsonableS) : YojsonableS with type t = P.t

module NotYojsonable (T : TypeS) : YojsonableS with type t = T.t

val string_0x_yojsoning : string yojsoning

val bytes_yojsoning : Bytes.t yojsoning

module Bytes : sig
  include module type of Bytes
  include YojsonableS with type t := t
end