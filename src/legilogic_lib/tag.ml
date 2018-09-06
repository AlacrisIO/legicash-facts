(** Tags used for Persistence.
    Tags are important for
    1- Discriminating cases in a sum type.
    2- Discriminating between things that are *signed*
    which can be seen as a big open sum type (like exception in OCaml).
*)
open Lib
open Yojsoning
open Marshaling
open Integer

let base_trie = 0x40
let empty = 0x40
let leaf = 0x41
let branch = 0x42
let skip = 0x43

let none = 0x44
let some = 0x45

let keypair = 0x60


(** Strict upper limit on all tags defined so far *)
let tag_limit = 0xFF

let bad_tag_error start bytes =
  raise (Unmarshaling_error ("bad tag", start, bytes))

module UInt16int = struct
  module U = struct
    type t = int
    let verify x =
      if 0 <= x && x <= 0xFFFF then x else bork "bad UInt16int"
    let marshaling = marshaling_map UInt16.of_int UInt16.to_int UInt16.marshaling
    let yojsoning =
      { to_yojson = (fun x -> `Int x)
      ; of_yojson = function
          | `Int x -> Ok (verify x)
          | _ -> Error "not a json Integer" }
  end
  include YojsonMarshalable(U)
end

module Tag = struct
  include UInt16int
  let marshaling =
    { marshal = marshal
    ; unmarshal = fun start bytes ->
        unmarshal_map
          (fun u -> if u < tag_limit then u else
              raise (Unmarshaling_error
                       (Printf.sprintf "bad tag %d" u, start, bytes)))
          unmarshal start bytes }
end
include Marshalable (Tag)

let marshal_tagged value m buffer x =
  Tag.marshal buffer value; m buffer x
let unmarshal_tagged value (u: 'a unmarshaler) start bytes =
  let (v, p) = unmarshal start bytes in
  (if not (v = value) then bad_tag_error start bytes);
  u p bytes
let marshaling_tagged (tag: t) (m: 'a marshaling) : 'a marshaling =
  { marshal=marshal_tagged tag m.marshal
  ; unmarshal=unmarshal_tagged tag m.unmarshal}

let marshal_2cases f tag1 tag2 m1 m2 buffer x =
  if f x then
    marshal_tagged tag1 m1 buffer x
  else
    marshal_tagged tag2 m2 buffer x
let unmarshal_2cases tag1 tag2 (u1: 'a unmarshaler) (u2: 'a unmarshaler) start bytes =
  let (tag, p) = unmarshal start bytes in
  if tag = tag1 then
    u1 p bytes
  else if tag = tag2 then
    u2 p bytes
  else
    bad_tag_error start bytes
let marshaling_2cases f tag1 tag2 m1 m2 =
  { marshal=marshal_2cases f tag1 tag2 m1.marshal m2.marshal
  ; unmarshal=unmarshal_2cases tag1 tag2 m1.unmarshal m2.unmarshal }

(* the marshal_cases would be so much nicer with dependent arrays *)
let marshal_cases tag_of base_tag (cases : 'a marshaling array) buffer x =
  let tag = tag_of x in
  marshal buffer tag;
  cases.(tag - base_tag).marshal buffer x
let unmarshal_cases base_tag (cases : 'a marshaling array) start bytes =
  let (tag, p) = unmarshal start bytes in
  let i = tag - base_tag in
  if i >= Array.length cases then bad_tag_error start bytes;
  cases.(i).unmarshal p bytes
let marshaling_cases tag_of base_tag (cases : 'a marshaling array) =
  { marshal = marshal_cases tag_of base_tag cases
  ; unmarshal = unmarshal_cases base_tag cases }
let new_marshaling_cases n = Array.make n marshaling_not_implemented
let init_marshaling_cases base_tag (cases : 'a marshaling array) l =
  assert (List.length l = Array.length cases);
  (fun (tag, m) ->
     let i = tag - base_tag in
     assert (cases.(i) == marshaling_not_implemented);
     cases.(i) <- m)
  |> (fun f -> List.iter f l)

let marshal_option m buffer = function
  | None -> marshal buffer none
  | Some x -> marshal buffer some; m buffer x
let unmarshal_option (u : 'a unmarshaler) start bytes =
  let (tag, p) = unmarshal start bytes in
  if tag = none then
    (None, p)
  else if tag = some then
    unmarshal_map (fun x -> Some x) u p bytes
  else
    bad_tag_error start bytes
let option_marshaling m = {marshal=marshal_option m.marshal; unmarshal=unmarshal_option m.unmarshal}
