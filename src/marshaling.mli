open Lib

(** Marshaler: side-effect a Buffer.t to add bytes (or chars, or strings). *)
type 'a marshaler = Buffer.t -> 'a -> unit

(** Unmarshaler: purely read bytes from a Bytes.t from a start position, return object and next position. *)
type 'a unmarshaler = ?start:int -> Bytes.t -> 'a*int

module type MarshalableS = sig
  type t
  val marshal : t marshaler
  val unmarshal : t unmarshaler
end

val marshal_of_sized_string_of : int -> ('a -> string) -> 'a marshaler
val unmarshal_of_sized_of_string : int -> (string -> 'a) -> 'a unmarshaler

val marshal_bytes_of_marshal : 'a marshaler -> 'a -> Bytes.t
val unmarshal_bytes_of_unmarshal : 'a unmarshaler -> Bytes.t -> 'a

val marshal_string_of_marshal : 'a marshaler -> 'a -> string
val unmarshal_string_of_unmarshal : 'a unmarshaler -> string -> 'a

val marshal_char : char marshaler
val unmarshal_char : char unmarshaler

val marshal_bool : bool marshaler
val unmarshal_bool : bool unmarshaler

val unmarshal_not_implemented : 'a unmarshaler

module OCamlMarshaling (Type: T) : MarshalableS with type t = Type.t
(** Do NOT use this module in production. Only for demos and temporary cut-throughs *)
