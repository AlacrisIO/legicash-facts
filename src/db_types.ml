(** Tags used for Persistence *)
open Marshaling
open Db


let marshal_tagged value m buffer x =
  Tag.marshal buffer value; m buffer x
let unmarshal_tagged value (u: 'a unmarshaler) ?(start=0) bytes =
  let (v, p) = Tag.unmarshal ~start bytes in
  (if not (v = value) then Tag.bad_tag_error start bytes);
  u ~start:p bytes
let marshaling_tagged (tag: Tag.t) (m: 'a marshaling) : 'a marshaling =
  { marshal=marshal_tagged tag m.marshal
  ; unmarshal=unmarshal_tagged tag m.unmarshal}

let marshal_2cases f tag1 tag2 m1 m2 buffer x =
  if f x then
    marshal_tagged tag1 m1 buffer x
  else
    marshal_tagged tag2 m2 buffer x
let unmarshal_2cases tag1 tag2 (u1: 'a unmarshaler) (u2: 'a unmarshaler) ?(start=0) bytes =
  let (tag, p) = Tag.unmarshal ~start:start bytes in
  if tag = tag1 then
    u1 ~start:p bytes
  else if tag = tag2 then
    u2 ~start:p bytes
  else
    Tag.bad_tag_error start bytes
let marshaling_2cases f tag1 tag2 m1 m2 =
  { marshal=marshal_2cases f tag1 tag2 m1.marshal m2.marshal
  ; unmarshal=unmarshal_2cases tag1 tag2 m1.unmarshal m2.unmarshal }

(* the marshal_cases would be so much nicer with dependent arrays *)
let marshal_cases tag_of base_tag (cases : 'a marshaling array) buffer x =
  let tag = tag_of x in
  Tag.marshal buffer tag;
  cases.(tag - base_tag).marshal buffer x
let unmarshal_cases base_tag (cases : 'a marshaling array) ?(start=0) bytes =
  let (tag, p) = Tag.unmarshal ~start:start bytes in
  if tag >= Array.length cases then Tag.bad_tag_error start bytes;
  cases.(tag - base_tag).unmarshal ~start:p bytes
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

let marshal_list m buffer l =
  let len = List.length l in
  UInt32.marshal buffer (UInt32.of_int len);
  List.iter (m buffer) l
let unmarshal_list (u : 'a unmarshaler) ?(start=0) bytes =
  let (len, p) = unmarshal_map UInt32.to_int UInt32.unmarshal ~start bytes in
  if len < 0 then raise (Unmarshaling_error ("bad length", start, bytes));
  let rec loop i start acc =
    if i = 0 then (List.rev acc, start) else
      let (v, p) = u ~start bytes in
      loop (i - 1) p (v::acc) in
  loop len p []
let list_marshaling m = {marshal=marshal_list m.marshal; unmarshal=unmarshal_list m.unmarshal}

let marshal_option m buffer = function
  | None -> Tag.marshal buffer Tag.none
  | Some x -> Tag.marshal buffer Tag.some; m buffer x
let unmarshal_option (u : 'a unmarshaler) ?(start=0) bytes =
  let (tag, p) = Tag.unmarshal ~start bytes in
  if tag = Tag.none then
    (None, p)
  else if tag = Tag.some then
    unmarshal_map (fun x -> Some x) u ~start:p bytes
  else
    Tag.bad_tag_error start bytes
let option_marshaling m = {marshal=marshal_option m.marshal; unmarshal=unmarshal_option m.unmarshal}
