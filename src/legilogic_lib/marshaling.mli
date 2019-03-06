(** Logic for conversion of various data types to string representations *)

open Lib
open Yojsoning
open Ppx_deriving_rlp_runtime.Rlping

exception Marshaling_error of string
exception Unmarshaling_error of string*int*Bytes.t

(** Marshaler: side-effect a Buffer.t to add bytes (or chars, or strings). *)
type 'a marshaler = Buffer.t -> 'a -> unit

(** Unmarshaler: purely read bytes from a Bytes.t from a start position, return object and next position. *)
type 'a unmarshaler = int -> Bytes.t -> 'a*int

(** A record of a marshaler and unmarshaler methods, from which to achieve simultaneous composition
    of marshaling for various data structures *)
type 'a marshaling =
  { marshal: 'a marshaler; unmarshal: 'a unmarshaler }

val marshaling_of_rlping : 'a rlping -> 'a marshaling

(** A module containing the bare minimum methods from which to deduce all utility functions
    about marshaling. *)
module type PreMarshalableS = sig
  type t
  val marshaling : t marshaling
end
(* TODO: instead of [marshaling], maybe have two methods to_yojson and of_yojson here,
   then use first-class modules for PreMarshalableS instead of marshaling? *)

(** Object which can be converted to and from binary representation *)
module type MarshalableS = sig
  include PreMarshalableS
  val marshal: t marshaler
  val unmarshal: t unmarshaler
  val marshal_bytes: t -> Bytes.t
  val unmarshal_bytes: Bytes.t -> t
  val marshal_string: t -> string
  val unmarshal_string: string -> t
end

module type MarshalableRlpS = sig
  type t
  [@@deriving rlp]
  include MarshalableS with type t := t
end

(** Auto-generated methods for object which can be converted to and from binary
    representation *)
module Marshalable (P : PreMarshalableS) : MarshalableS with type t = P.t

module MarshalableRlp (P : PreMarshalableS) : MarshalableRlpS with type t = P.t

val marshal_of_sized_string_of : int -> ('a -> string) -> 'a marshaler
val unmarshal_of_sized_of_string : int -> (string -> 'a) -> 'a unmarshaler
(** Marshaler which verifies that binary representation has specified length *)
val marshaling_sized_string : int -> ('a -> string) -> (string -> 'a) -> 'a marshaling

val marshal_bytes_of_marshal : 'a marshaler -> 'a -> Bytes.t
val unmarshal_bytes_of_unmarshal : 'a unmarshaler -> Bytes.t -> 'a

val marshal_string_of_marshal : 'a marshaler -> 'a -> string
val unmarshal_string_of_unmarshal : 'a unmarshaler -> string -> 'a

val char_marshaling : char marshaling

val bool_marshaling : bool marshaling

val string_marshaling : string marshaling

val list_marshaling : 'a marshaling -> 'a list marshaling

val marshal_map : ('x -> 'a) -> 'a marshaler -> 'x marshaler
val unmarshal_map : ('a -> 'x) -> 'a unmarshaler -> 'x unmarshaler
(** [marshaling_map f g marshaling] is a marshaler which marshals ['x]s as
    ['a]s. Assumes [g] is the inverse of [f] on [f]'s image, and vice versa.) *)
val marshaling_map : ('x -> 'a) -> ('a -> 'x) -> 'a marshaling -> 'x marshaling

(** The following are like marshal_map with multiple arguments,
    for product data structures, records, etc. *)

val marshal2 : ('x -> 'a*'b) -> 'a marshaler -> 'b marshaler
  -> 'x marshaler
val unmarshal2 : ('a -> 'b -> 'x) -> 'a unmarshaler -> 'b unmarshaler
  -> 'x unmarshaler
(** Marshaling of pairs *)
val marshaling2 : ('x -> 'a*'b) -> ('a -> 'b -> 'x) -> 'a marshaling -> 'b marshaling -> 'x marshaling

val marshal3 : ('x -> 'a*'b*'c) -> 'a marshaler -> 'b marshaler -> 'c marshaler
  -> 'x marshaler
val unmarshal3 : ('a -> 'b -> 'c -> 'x) -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler
  -> 'x unmarshaler
(** Marshaling of triples *)
val marshaling3 : ('x -> 'a*'b*'c) -> ('a -> 'b -> 'c -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling
  -> 'x marshaling

val marshal4 : ('x -> 'a*'b*'c*'d) -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'x marshaler
val unmarshal4 : ('a -> 'b -> 'c -> 'd -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'x unmarshaler
(** Marshaling of 4-tuples *)
val marshaling4 : ('x -> 'a*'b*'c*'d) -> ('a -> 'b -> 'c -> 'd -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'x marshaling

val marshal5 : ('x -> 'a*'b*'c*'d*'e)
  -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler -> 'e marshaler
  -> 'x marshaler
val unmarshal5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'e unmarshaler
  -> 'x unmarshaler
(** Marshaling of 5-tuples *)
val marshaling5 : ('x -> 'a*'b*'c*'d*'e) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling
  -> 'x marshaling

val marshal6 : ('x -> 'a*'b*'c*'d*'e*'f)
  -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'e marshaler -> 'f marshaler
  -> 'x marshaler
val unmarshal6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'e unmarshaler -> 'f unmarshaler
  -> 'x unmarshaler
(** Marshaling of 6-tuples *)
val marshaling6 : ('x -> 'a*'b*'c*'d*'e*'f) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling -> 'f marshaling
  -> 'x marshaling

val marshal7 : ('x -> 'a*'b*'c*'d*'e*'f*'g)
  -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'e marshaler -> 'f marshaler -> 'g marshaler
  -> 'x marshaler
val unmarshal7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'e unmarshaler -> 'f unmarshaler -> 'g unmarshaler
  -> 'x unmarshaler
(** Marshaling of 4-tuples *)
val marshaling7 : ('x -> 'a*'b*'c*'d*'e*'f*'g) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling -> 'f marshaling -> 'g marshaling
  -> 'x marshaling

val marshal8 : ('x -> 'a*'b*'c*'d*'e*'f*'g*'h)
  -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'e marshaler -> 'f marshaler -> 'g marshaler -> 'h marshaler
  -> 'x marshaler
val unmarshal8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'e unmarshaler -> 'f unmarshaler -> 'g unmarshaler -> 'h unmarshaler
  -> 'x unmarshaler
(** Marshaling of 8-tuples *)
val marshaling8 : ('x -> 'a*'b*'c*'d*'e*'f*'g*'h)
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling -> 'f marshaling -> 'g marshaling -> 'h marshaling
  -> 'x marshaling

val marshal9 : ('x -> 'a*'b*'c*'d*'e*'f*'g*'h*'i)
  -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'e marshaler -> 'f marshaler -> 'g marshaler -> 'h marshaler -> 'i marshaler
  -> 'x marshaler
val unmarshal9 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'e unmarshaler -> 'f unmarshaler -> 'g unmarshaler -> 'h unmarshaler
  -> 'i unmarshaler
  -> 'x unmarshaler
(** Marshaling of 9-tuples *)
val marshaling9 : ('x -> 'a*'b*'c*'d*'e*'f*'g*'h*'i)
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling -> 'f marshaling -> 'g marshaling -> 'h marshaling
  -> 'i marshaling
  -> 'x marshaling

val marshal_not_implemented : 'a marshaler
val unmarshal_not_implemented : 'a unmarshaler
(** Marshaler which simply errors if you try to use it *)
val marshaling_not_implemented : 'a marshaling

(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
module OCamlMarshaling (T: TypeS) : PreMarshalableS with type t = T.t

(** A type with RLP support *)
module type RlpingS = sig
  type t
  val rlping : t rlping
end

(** Marshalable to binary, and (separately) convertible to json. *)
module type PreYojsonMarshalableS = sig
  include PreMarshalableS
  include PreYojsonableS with type t := t
end

module type PreYojsonMarshalableRlpS = sig
  type t
  [@@deriving rlp]
  include PreYojsonMarshalableS with type t := t
end

(** Marshalable to binary, and (separately) convertible to json. *)
module type YojsonMarshalableS = sig
  include MarshalableS
  include YojsonableS with type t := t
end

module type YojsonMarshalableRlpS = sig
  type t
  [@@deriving rlp]
  include YojsonMarshalableS with type t := t
end

(** Marshalable as RLP *)
module MarshalableOfRlp (R : RlpingS) : (MarshalableS with type t = R.t)

(** "Marshalable" *as* json. *)
module MarshalableOfYojsonable (J : YojsonableS) : YojsonMarshalableS with type t = J.t

val to_yojson_of_marshal_string : ('a -> string) -> 'a -> yojson
val of_yojson_of_unmarshal_string : (string -> 'a) -> yojson -> ('a, string) result
val yojsoning_of_marshal_string_unmarshal_string : ('a -> string) -> (string -> 'a) -> 'a yojsoning
val yojsoning_of_marshaling : 'a marshaling -> 'a yojsoning
val marshaling_of_yojsoning : 'a yojsoning -> 'a marshaling

(** JSON-encodes an PreMarshable object as its marshaled string *)
module YojsonableOfPreMarshalable (P : PreMarshalableS) : YojsonMarshalableS with type t = P.t

(** JSON-encodes an Marshable object as its marshaled string *)
module YojsonableOfMarshalable (M : MarshalableS) : YojsonMarshalableS with type t = M.t

(** Complete both Yojsonable and Marshalable interfaces from precursors *)
module YojsonMarshalable (P : PreYojsonMarshalableS) : YojsonMarshalableS with type t = P.t

module type LengthS = sig
  include PreMarshalableS with type t := int
  val max_length : int
end

(* Should the names reflect number of bits, rather than (approximate) maxima?
   e.g. Length6bit, Length30bit, StringL6b, StringL30b ?
   There is no way that isn't confusing without having very long names like
   Length_that_fit_in_30_bit
   String_whose_length_fit_in_30_bit
   And why 30 bits? Because that's what fits as a non-negative value in an unboxed int
   on a 32-bit OCaml platform.
*)

(** 6-bit representation of string length *)
module Length63 : LengthS

(** 30-bit representation of string length *)
module Length1G : LengthS

(** Marshalable as length-prefixed string of constrained length [L] *)
module StringL (L: LengthS) : YojsonMarshalableS with type t = string

(** Marshalable as length-prefixed string of length expressible in 6 bits
    Why? Because we want a memo to be variable length, not too long,
    yet able to hold at least a digest, for use in relevant protocols.
*)
module String63 : YojsonMarshalableS with type t = string

(** Marshalable as length-prefixed string of length expressible in 30 bits
*)
module String1G : YojsonMarshalableS with type t = string

module Data : sig
  type t = string
  [@@deriving rlp]
  include YojsonMarshalableS with type t := t
  include ShowableS with type t := t
end

val yojson_marshaling : yojson marshaling
