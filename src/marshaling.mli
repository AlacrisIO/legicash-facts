open Lib
open Yojsoning

exception Marshaling_error of string
exception Unmarshaling_error of string*int*Bytes.t

(** Marshaler: side-effect a Buffer.t to add bytes (or chars, or strings). *)
type 'a marshaler = Buffer.t -> 'a -> unit

(** Unmarshaler: purely read bytes from a Bytes.t from a start position, return object and next position. *)
type 'a unmarshaler = ?start:int -> Bytes.t -> 'a*int

type 'a marshaling =
  { marshal: 'a marshaler; unmarshal: 'a unmarshaler }

module type PreMarshalableS = sig
  type t
  val marshaling : t marshaling
end

module type MarshalableS = sig
  include PreMarshalableS
  val marshal: t marshaler
  val unmarshal: t unmarshaler
  val marshal_bytes: t -> Bytes.t
  val unmarshal_bytes: Bytes.t -> t
  val marshal_string: t -> string
  val unmarshal_string: string -> t
end

module Marshalable (P : PreMarshalableS) : MarshalableS with type t = P.t

val marshal_of_sized_string_of : int -> ('a -> string) -> 'a marshaler
val unmarshal_of_sized_of_string : int -> (string -> 'a) -> 'a unmarshaler
val marshaling_sized_string : int -> ('a -> string) -> (string -> 'a) -> 'a marshaling

val marshal_bytes_of_marshal : 'a marshaler -> 'a -> Bytes.t
val unmarshal_bytes_of_unmarshal : 'a unmarshaler -> Bytes.t -> 'a

val marshal_string_of_marshal : 'a marshaler -> 'a -> string
val unmarshal_string_of_unmarshal : 'a unmarshaler -> string -> 'a

val marshal_char : char marshaler
val unmarshal_char : char unmarshaler
val char_marshaling : char marshaling

val marshal_bool : bool marshaler
val unmarshal_bool : bool unmarshaler
val bool_marshaling : bool marshaling

val marshal_map : ('x -> 'a) -> 'a marshaler -> 'x marshaler
val unmarshal_map : ('a -> 'x) -> 'a unmarshaler -> 'x unmarshaler
val marshaling_map : ('x -> 'a) -> ('a -> 'x) -> 'a marshaling -> 'x marshaling

val marshal2 : ('x -> 'a*'b) -> 'a marshaler -> 'b marshaler
  -> 'x marshaler
val unmarshal2 : ('a -> 'b -> 'x) -> 'a unmarshaler -> 'b unmarshaler
  -> 'x unmarshaler
val marshaling2 : ('x -> 'a*'b) -> ('a -> 'b -> 'x) -> 'a marshaling -> 'b marshaling -> 'x marshaling

val marshal3 : ('x -> 'a*'b*'c) -> 'a marshaler -> 'b marshaler -> 'c marshaler
  -> 'x marshaler
val unmarshal3 : ('a -> 'b -> 'c -> 'x) -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler
  -> 'x unmarshaler
val marshaling3 : ('x -> 'a*'b*'c) -> ('a -> 'b -> 'c -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling
  -> 'x marshaling

val marshal4 : ('x -> 'a*'b*'c*'d) -> 'a marshaler -> 'b marshaler -> 'c marshaler -> 'd marshaler
  -> 'x marshaler
val unmarshal4 : ('a -> 'b -> 'c -> 'd -> 'x)
  -> 'a unmarshaler -> 'b unmarshaler -> 'c unmarshaler -> 'd unmarshaler
  -> 'x unmarshaler
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
val marshaling9 : ('x -> 'a*'b*'c*'d*'e*'f*'g*'h*'i)
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'x)
  -> 'a marshaling -> 'b marshaling -> 'c marshaling -> 'd marshaling
  -> 'e marshaling -> 'f marshaling -> 'g marshaling -> 'h marshaling
  -> 'i marshaling
  -> 'x marshaling

val marshal_not_implemented : 'a marshaler
val unmarshal_not_implemented : 'a unmarshaler
val marshaling_not_implemented : 'a marshaling

(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
module OCamlMarshaling (T: TypeS) : PreMarshalableS with type t = T.t

module type PreYojsonMarshalableS = sig
  include PreMarshalableS
  include PreYojsonableS with type t := t
end

module type YojsonMarshalableS = sig
  include MarshalableS
  include YojsonableS with type t := t
end

module MarshalableOfYojsonable (J : YojsonableS) : YojsonMarshalableS with type t = J.t

module YojsonableOfMarshalable (M : MarshalableS) : YojsonMarshalableS with type t = M.t

module String63 : MarshalableS with type t = string
