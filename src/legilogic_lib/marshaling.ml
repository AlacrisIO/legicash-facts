open Lib
open Yojsoning

exception Marshaling_error of string
exception Unmarshaling_error of string*int*Bytes.t

type 'a marshaler = Buffer.t -> 'a -> unit
type 'a unmarshaler = int -> Bytes.t -> 'a * int
type 'a marshaling = {marshal: 'a marshaler; unmarshal: 'a unmarshaler}

let marshal_of_sized_string_of num_bytes string_of b x =
  let s = string_of x in
  if String.length s != num_bytes then
    raise (Marshaling_error
             (Printf.sprintf "marshal_of_sized_string_of expected %d bytes but got string %S of length %d"
                num_bytes s (String.length s)));
  Buffer.add_string b s

let unmarshal_of_sized_of_string num_bytes of_string start b =
  let s = Bytes.sub_string b start num_bytes in
  (of_string s, start + num_bytes)

let marshaling_sized_string num_bytes to_string of_string =
  { marshal=marshal_of_sized_string_of num_bytes to_string
  ; unmarshal=unmarshal_of_sized_of_string num_bytes of_string }

let default_buffer_size = 256

let marshal_bytes_of_marshal marshal x =
  let buffer = Buffer.create default_buffer_size in
  marshal buffer x ;
  Buffer.to_bytes buffer

let unmarshal_bytes_of_unmarshal (unmarshal: 'value unmarshaler) bytes =
  let (value, length) = unmarshal 0 bytes in
  if not (length = Bytes.length bytes) then
    raise (Unmarshaling_error ("leftover bytes while unmarshaling", length, bytes));
  value

let marshal_string_of_marshal marshal x =
  let buffer = Buffer.create default_buffer_size in
  marshal buffer x ;
  Buffer.contents buffer

let unmarshal_string_of_unmarshal unmarshal x =
  unmarshal_bytes_of_unmarshal unmarshal (Bytes.of_string x)

let marshal_string_of_any value = Marshal.to_string value [Marshal.Compat_32]

let marshal_map f marshal buffer x = marshal buffer (f x)
(*let unmarshal_map f unmarshal start bytes = unmarshal start bytes |> map_fst f*)
let unmarshal_map f (unmarshal : 'a unmarshaler) start bytes =
  unmarshal start bytes |> map_fst f
let marshaling_map f g marshaling =
  {marshal=marshal_map f marshaling.marshal;unmarshal=unmarshal_map g marshaling.unmarshal}

let marshal_char buffer ch = Buffer.add_char buffer ch
let unmarshal_char start bytes = (Bytes.get bytes start, start + 1)
let char_marshaling={marshal=marshal_char;unmarshal=unmarshal_char}

let bool_of_char = function
  | '\000' -> false
  | '\001' -> true
  | c -> bork "Bad bool char %c" c

let char_of_bool = function
  | false -> '\000'
  | true -> '\001'

let marshal_bool = marshal_map char_of_bool marshal_char
let unmarshal_bool = unmarshal_map bool_of_char unmarshal_char
let bool_marshaling={marshal=marshal_bool;unmarshal=unmarshal_bool}

let marshal_not_implemented _buffer _x = bottom ()
let unmarshal_not_implemented _start _bytes = bottom ()
let marshaling_not_implemented = {marshal=marshal_not_implemented;unmarshal=unmarshal_not_implemented}

let marshal2 f m1 m2 buffer x =
  match f x with (x1, x2) -> m1 buffer x1 ; m2 buffer x2
let unmarshal2 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) : 'x unmarshaler =
  fun start bytes ->
    let (x1, p) = u1 start bytes in
    let (x2, p) = u2 p bytes in
    (f x1 x2, p)
let marshaling2 f g a b =
  {marshal=marshal2 f a.marshal b.marshal
  ;unmarshal=unmarshal2 g a.unmarshal b.unmarshal}

let marshal3 f m1 m2 m3 buffer x =
  match f x with (x1, x2, x3) -> m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3
let unmarshal3 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler) start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  (f x1 x2 x3, p)
let marshaling3 f g a b c =
  {marshal=marshal3 f a.marshal b.marshal c.marshal
  ;unmarshal=unmarshal3 g a.unmarshal b.unmarshal c.unmarshal}

let marshal4 f m1 m2 m3 m4 buffer x =
  match f x with (x1, x2, x3, x4) -> m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4
let unmarshal4 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler) (u4: 'd unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  (f x1 x2 x3 x4, p)
let marshaling4 f g a b c d =
  {marshal=marshal4 f a.marshal b.marshal c.marshal d.marshal
  ;unmarshal=unmarshal4 g a.unmarshal b.unmarshal c.unmarshal d.unmarshal}

let marshal5 f m1 m2 m3 m4 m5 buffer x =
  match f x with (x1, x2, x3, x4, x5) ->
    m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4 ; m5 buffer x5
let unmarshal5 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler)
      (u4: 'd unmarshaler) (u5: 'e unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  let (x5, p) = u5 p bytes in
  (f x1 x2 x3 x4 x5, p)
let marshaling5 f g a b c d e =
  {marshal=marshal5 f a.marshal b.marshal c.marshal d.marshal e.marshal
  ;unmarshal=unmarshal5 g a.unmarshal b.unmarshal c.unmarshal d.unmarshal e.unmarshal}

let marshal6 f m1 m2 m3 m4 m5 m6 buffer x =
  match f x with (x1, x2, x3, x4, x5, x6) ->
    m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4 ; m5 buffer x5 ; m6 buffer x6
let unmarshal6 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler)
      (u4: 'd unmarshaler) (u5: 'e unmarshaler) (u6: 'f unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  let (x5, p) = u5 p bytes in
  let (x6, p) = u6 p bytes in
  (f x1 x2 x3 x4 x5 x6, p)
let marshaling6 xt tx a b c d e f =
  {marshal=marshal6 xt a.marshal b.marshal c.marshal d.marshal e.marshal f.marshal
  ;unmarshal=unmarshal6 tx a.unmarshal b.unmarshal c.unmarshal d.unmarshal e.unmarshal f.unmarshal}

let marshal7 f m1 m2 m3 m4 m5 m6 m7 buffer x =
  match f x with (x1, x2, x3, x4, x5, x6, x7) ->
    m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4 ; m5 buffer x5 ;
    m6 buffer x6 ; m7 buffer x7
let unmarshal7 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler)
      (u4: 'd unmarshaler) (u5: 'e unmarshaler) (u6: 'f unmarshaler) (u7: 'g unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  let (x5, p) = u5 p bytes in
  let (x6, p) = u6 p bytes in
  let (x7, p) = u7 p bytes in
  (f x1 x2 x3 x4 x5 x6 x7, p)
let marshaling7 xt tx a b c d e f g =
  {marshal=marshal7 xt a.marshal b.marshal c.marshal d.marshal e.marshal f.marshal g.marshal
  ;unmarshal=unmarshal7 tx a.unmarshal b.unmarshal c.unmarshal d.unmarshal
               e.unmarshal f.unmarshal g.unmarshal}

let marshal8 f m1 m2 m3 m4 m5 m6 m7 m8 buffer x =
  match f x with (x1, x2, x3, x4, x5, x6, x7, x8) ->
    m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4 ; m5 buffer x5 ;
    m6 buffer x6 ; m7 buffer x7 ; m8 buffer x8
let unmarshal8 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler)
      (u4: 'd unmarshaler) (u5: 'e unmarshaler) (u6: 'f unmarshaler)
      (u7: 'g unmarshaler) (u8: 'h unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  let (x5, p) = u5 p bytes in
  let (x6, p) = u6 p bytes in
  let (x7, p) = u7 p bytes in
  let (x8, p) = u8 p bytes in
  (f x1 x2 x3 x4 x5 x6 x7 x8, p)
let marshaling8 xt tx a b c d e f g h =
  {marshal=marshal8 xt a.marshal b.marshal c.marshal d.marshal e.marshal
             f.marshal g.marshal h.marshal
  ;unmarshal=unmarshal8 tx a.unmarshal b.unmarshal c.unmarshal d.unmarshal
               e.unmarshal f.unmarshal g.unmarshal h.unmarshal}

let marshal9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 buffer x =
  match f x with (x1, x2, x3, x4, x5, x6, x7, x8, x9) ->
    m1 buffer x1 ; m2 buffer x2 ; m3 buffer x3 ; m4 buffer x4 ; m5 buffer x5 ;
    m6 buffer x6 ; m7 buffer x7 ; m8 buffer x8 ; m9 buffer x9
let unmarshal9 f (u1: 'a unmarshaler) (u2: 'b unmarshaler) (u3: 'c unmarshaler)
      (u4: 'd unmarshaler) (u5: 'e unmarshaler) (u6: 'f unmarshaler)
      (u7: 'g unmarshaler) (u8: 'h unmarshaler) (u9: 'i unmarshaler)
      start bytes =
  let (x1, p) = u1 start bytes in
  let (x2, p) = u2 p bytes in
  let (x3, p) = u3 p bytes in
  let (x4, p) = u4 p bytes in
  let (x5, p) = u5 p bytes in
  let (x6, p) = u6 p bytes in
  let (x7, p) = u7 p bytes in
  let (x8, p) = u8 p bytes in
  let (x9, p) = u9 p bytes in
  (f x1 x2 x3 x4 x5 x6 x7 x8 x9, p)
let marshaling9 xt tx a b c d e f g h i =
  {marshal=marshal9 xt a.marshal b.marshal c.marshal d.marshal e.marshal
             f.marshal g.marshal h.marshal i.marshal
  ;unmarshal=unmarshal9 tx a.unmarshal b.unmarshal c.unmarshal d.unmarshal
               e.unmarshal f.unmarshal g.unmarshal h.unmarshal i.unmarshal}

(*
   let marshal_if f tag (m: 'a marshaler) (melse: 'a marshaler) buffer x =
   if f x then
   Tag.marshal_exact tag m buffer x
   else
   melse buffer x

   let unmarshal_if tag u uelse start bytes =
   let (t, p) = Tag.unmarshal start bytes in
   if t = tag then
   u p bytes
   else
   ufalse start bytes

   let marshaling_if f tag m melse =
   {marshal = marshal_if f tag m.marshal melse.marshal
   unmarshal = unmarshal_if tag m.unmarshal melse.unmarshal}
*)

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

module Marshalable (P : PreMarshalableS) = struct
  include P
  let marshal = marshaling.marshal
  let unmarshal = marshaling.unmarshal
  let marshal_bytes = marshal_bytes_of_marshal marshal
  let unmarshal_bytes = unmarshal_bytes_of_unmarshal unmarshal
  let marshal_string = marshal_string_of_marshal marshal
  let unmarshal_string = unmarshal_string_of_unmarshal unmarshal
end

module OCamlMarshaling (T: TypeS) = struct
  include T
  let marshaling =
    { marshal = (fun buffer value -> Buffer.add_string buffer (marshal_string_of_any value))
    ; unmarshal = fun start buffer ->
        let value = Marshal.from_bytes buffer start in
        let size = Marshal.total_size buffer start in
        value, start + size }
end

module type PreYojsonMarshalableS = sig
  include PreMarshalableS
  include PreYojsonableS with type t := t
end

module type YojsonMarshalableS = sig
  include MarshalableS
  include YojsonableS with type t := t
end

let to_yojson_of_marshal_string marshal_string x =
  `String (x |> marshal_string |> Hex.unparse_hex_string)
let of_yojson_of_unmarshal_string unmarshal_string = function
  | `String a -> Ok (unmarshal_string (Hex.parse_hex_string a))
  | _ -> Error "bad json"
let yojsoning_of_marshal_string_unmarshal_string marshal_string unmarshal_string =
  { to_yojson=to_yojson_of_marshal_string marshal_string
  ; of_yojson=of_yojson_of_unmarshal_string unmarshal_string }
let yojsoning_of_marshaling marshaling =
  yojsoning_of_marshal_string_unmarshal_string
    (marshal_string_of_marshal marshaling.marshal)
    (unmarshal_string_of_unmarshal marshaling.unmarshal)

module YojsonMarshalable (P : PreYojsonMarshalableS) = struct
  include Marshalable(P)
  include (Yojsonable(P) : YojsonableS with type t := t)
end

module YojsonableOfMarshalable (M : MarshalableS) = struct
  include M
  include (Yojsonable(struct
             type nonrec t = t
             let yojsoning = yojsoning_of_marshal_string_unmarshal_string marshal_string unmarshal_string
           end) : YojsonableS with type t := t)
end

module YojsonableOfPreMarshalable (P : PreMarshalableS) =
  YojsonableOfMarshalable(Marshalable(P))

module type LengthS = sig
  include PreMarshalableS with type t := int
  val max_length : int
end

(** Length-prefixed string. Marshals to <length><string-of-that-length> *)
module StringL (L : LengthS) = struct
  let check_length ?(fail=bork "%s") l =
    if l > L.max_length then
      fail (Printf.sprintf "Invalid string length %d (max %d)" l L.max_length)
  let checked_string_length ?(fail=bork "%s") s =
    let l = String.length s in check_length ~fail l; l
  let check_string_length ?(fail=bork "%s") s =
    ignore (checked_string_length ~fail s); s

  module P = struct
    type t = string
    let marshal buffer string =
      let len = checked_string_length ~fail:(fun x -> raise (Marshaling_error x)) string in
      L.marshaling.marshal buffer len;
      Buffer.add_string buffer string
    let unmarshal start bytes =
      let len, p = L.marshaling.unmarshal start bytes in
      let fail x = raise (Unmarshaling_error (x, start, bytes)) in
      check_length ~fail len;
      if len + p <= Bytes.length bytes then
        Bytes.sub_string bytes p len, len + p
      else
        fail (Printf.sprintf "declared length %d, but only %d characters left" len (Bytes.length bytes - p))
    let marshaling={marshal;unmarshal}
    let check_yojson_string s = check_string_length ~fail:(fun x -> Yojson.json_error x) s
    let yojsoning=yojsoning_map check_yojson_string check_yojson_string string_yojsoning
  end
  include Marshalable(P)
  include (Yojsonable(P) : YojsonableS with type t := t)
end

module Length63 = struct
  let max_length = 63
  let check63 l =
    if l > 63 then bork "Invalid length %d (max 63)" l;
    l
  let marshaling = marshaling_map
                     (check63 >> Char.chr) (Char.code >> check63) char_marshaling
end

module String63 = StringL(Length63)

(** Length which reliably fits in a native int, even on a 32-bit platform. *)
module Length1G = struct
  let max_length = 1 lsl 30 - 1
  let check_length l = if l > max_length then
      bork "Invalid length %d (max %d)" l max_length
  let marshal buffer len =
    check_length len;
    Buffer.add_char buffer (Char.chr (len lsr 24));
    Buffer.add_char buffer (Char.chr (255 land (len lsr 16)));
    Buffer.add_char buffer (Char.chr (255 land (len lsr 8)));
    Buffer.add_char buffer (Char.chr (255 land len))
  let unmarshal start bytes =
    if start + 4 > Bytes.length bytes then
      raise (Unmarshaling_error ("not enough length to read a 30-bit integer", start, bytes))
    else
      let hihi = Char.code (Bytes.get bytes start) in
      let lohi = Char.code (Bytes.get bytes (start + 1)) in
      let hilo = Char.code (Bytes.get bytes (start + 2)) in
      let lolo = Char.code (Bytes.get bytes (start + 3)) in
      if hihi > 63 then
        raise (Unmarshaling_error ("invalid high byte for a 30-bit integer", start, bytes));
      (hihi lsl 24) + (lohi lsl 16) + (hilo lsl 8) + lolo, start + 4
  let marshaling = {marshal;unmarshal}
end

(** Length-prefixed string where the length fits in 32 bits *)
module String1G = StringL(Length1G)

let marshal_list m buffer l =
  let len = List.length l in
  Length1G.marshal buffer len;
  List.iter (m buffer) l
let unmarshal_list (u : 'a unmarshaler) start bytes =
  let (len, p) = Length1G.unmarshal start bytes in
  let rec loop i start acc =
    if i = 0 then (List.rev acc, start) else
      let (v, p) = u start bytes in
      loop (i - 1) p (v::acc) in
  loop len p []
let list_marshaling m = {marshal=marshal_list m.marshal; unmarshal=unmarshal_list m.unmarshal}


let marshaling_of_yojsoning yojsoning =
  let to_yojson_string = to_yojson_string_of_to_yojson yojsoning.to_yojson in
  let of_yojson_string_exn = of_yojson_string_exn_of_of_yojson yojsoning.of_yojson in
  marshaling_map to_yojson_string of_yojson_string_exn String1G.marshaling

(** Object which can be marshaled based on json representation. Marshaled
    representation must fit in a gigabyte (!) *)
module MarshalableOfYojsonable (Y : YojsonableS) = struct
  include Y
  include (Marshalable (struct
             type nonrec t = t
             let marshaling = marshaling_of_yojsoning yojsoning
           end) : MarshalableS with type t := t)
end

let yojson_marshaling = marshaling_of_yojsoning yojson_yojsoning

