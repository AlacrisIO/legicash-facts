(** Tags used for Persistence *)
open Lib
open Yojsoning
open Marshaling
open Integer

let base_trie = 0x80
let empty = 0x80
let leaf = 0x81
let branch = 0x82
let skip = 0x83

let none = 0x84
let some = 0x85

let keypair = 0xA0

let side_chain_state = 0xC0
let side_chain_invoice = 0xC1
let base_side_chain_operation = 0xC2
let side_chain_deposit = 0xC2
let side_chain_payment = 0xC3
let side_chain_withdrawal = 0xC4
let side_chain_rx_header = 0xC8
let side_chain_request = 0xC9
let side_chain_user_account_state_per_facilitator = 0xCA
let side_chain_facilitator_state = 0xD0
let side_chain_tx_header = 0xD1
let side_chain_confirmation = 0xD2
let side_chain_account_state = 0xD3
let side_chain_facilitator_fee_schedule = 0xD4

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
    ; unmarshal = fun ?(start=0) bytes ->
        unmarshal_map
          (fun u -> if u < tag_limit then u else
              raise (Unmarshaling_error
                       (Printf.sprintf "bad tag %d" u, start, bytes)))
          unmarshal ~start bytes }
end
include Marshalable (Tag)

let marshal_tagged value m buffer x =
  Tag.marshal buffer value; m buffer x
let unmarshal_tagged value (u: 'a unmarshaler) ?(start=0) bytes =
  let (v, p) = unmarshal ~start bytes in
  (if not (v = value) then bad_tag_error start bytes);
  u ~start:p bytes
let marshaling_tagged (tag: t) (m: 'a marshaling) : 'a marshaling =
  { marshal=marshal_tagged tag m.marshal
  ; unmarshal=unmarshal_tagged tag m.unmarshal}

let marshal_2cases f tag1 tag2 m1 m2 buffer x =
  if f x then
    marshal_tagged tag1 m1 buffer x
  else
    marshal_tagged tag2 m2 buffer x
let unmarshal_2cases tag1 tag2 (u1: 'a unmarshaler) (u2: 'a unmarshaler) ?(start=0) bytes =
  let (tag, p) = unmarshal ~start:start bytes in
  if tag = tag1 then
    u1 ~start:p bytes
  else if tag = tag2 then
    u2 ~start:p bytes
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
let unmarshal_cases base_tag (cases : 'a marshaling array) ?(start=0) bytes =
  let (tag, p) = unmarshal ~start:start bytes in
  let i = tag - base_tag in
  if i >= Array.length cases then bad_tag_error start bytes;
  cases.(i).unmarshal ~start:p bytes
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
let unmarshal_option (u : 'a unmarshaler) ?(start=0) bytes =
  let (tag, p) = unmarshal ~start bytes in
  if tag = none then
    (None, p)
  else if tag = some then
    unmarshal_map (fun x -> Some x) u ~start:p bytes
  else
    bad_tag_error start bytes
let option_marshaling m = {marshal=marshal_option m.marshal; unmarshal=unmarshal_option m.unmarshal}
