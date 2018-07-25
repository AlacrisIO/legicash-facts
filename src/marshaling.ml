open Lib

type 'a marshaler = Buffer.t -> 'a -> unit
type 'a unmarshaler = ?start:int -> Bytes.t -> 'a * int

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

let default_buffer_size = 256

let marshal_bytes_of_marshal marshal x =
  let buffer = Buffer.create default_buffer_size in
  marshal buffer x ;
  Buffer.to_bytes buffer

let unmarshal_bytes_of_unmarshal (unmarshal: 'value unmarshaler) buffer =
  let (value, length) = unmarshal buffer in
  assert (length = Bytes.length buffer) ;
  value

let marshal_string_of_marshal marshal x =
  let buffer = Buffer.create default_buffer_size in
  marshal buffer x ;
  Buffer.contents buffer

let marshal_string_of_any value = Marshal.to_string value [Marshal.Compat_32]

let marshal_char buffer ch = Buffer.add_char buffer ch
let unmarshal_char ?(start=0) bytes = (Bytes.get bytes start,start + 1)

let true_char = '\001'
let false_char = '\000'

let marshal_bool buffer bool = marshal_char buffer (if bool then true_char else false_char)
let unmarshal_bool ?(start=0) bytes =
  let (ch,offset) = unmarshal_char ~start bytes in
  if ch == true_char then (true,offset)
  else if ch == false_char then (false,offset)
  else raise (Internal_error
                (Format.sprintf "Unexpected character %c when unmarshaling boolean" ch))

let unmarshal_not_implemented ?start:(_start=0) _bytes = bottom ()

module OCamlMarshaling (Type: T) = struct
  include Type
  let marshal buffer value = Buffer.add_string buffer (marshal_string_of_any value)
  let unmarshal ?(start=0) buffer =
    let value = Marshal.from_bytes buffer start in
    let size = Marshal.total_size buffer start in
    value, start + size
end
