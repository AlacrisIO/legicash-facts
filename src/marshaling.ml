open Lib

type 'a marshaler = Buffer.t -> 'a -> unit
type 'a unmarshaler = ?start:int -> Bytes.t -> 'a*int

module type MarshalableS = sig
  type t
  val marshal : t marshaler
  val unmarshal : t unmarshaler
end

let marshal_of_sized_string_of num_bytes string_of b x =
  let s = string_of x in
  if String.length s != num_bytes then
    raise (Internal_error
             (Printf.sprintf "marshal_of_sized_string_of expected %d bytes but got string %S of length %d"
                num_bytes s (String.length s)));
  Buffer.add_string b s

let unmarshal_of_sized_of_string num_bytes of_string ?start:(start=0) b =
  let s = Bytes.sub_string b start num_bytes in
  (of_string s, start + num_bytes)

let marshal_bytes_of_marshal marshal x =
  let buffer = Buffer.create 256 in
  marshal buffer x ;
  Buffer.to_bytes buffer

let unmarshal_bytes_of_unmarshal (unmarshal: 'value unmarshaler) buffer =
  let (value, length) = unmarshal buffer in
  assert (length = Bytes.length buffer) ;
  value

let marshal_string_of_any v = Marshal.to_string v [Marshal.Compat_32]

let marshal_any b v =
  Buffer.add_string b (marshal_string_of_any v)
[@@deprecated "Use marshal_any only as a stopgap for demos and testing."]

let marshal_bool b bool = Buffer.add_char b (if bool then '\001' else '\000')

let unmarshal_not_implemented ?start:(_start=0) _bytes = bottom ()
