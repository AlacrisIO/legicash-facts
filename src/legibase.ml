(* legibase.ml -- base module for Legicash *)

exception Not_implemented

exception Internal_error of string

exception Timeout of string

exception Double_spend of string

type 'a legi_result = ('a, exn) result

type ('a, 'b, 'c) action = 'c * 'a -> 'c * 'b legi_result

(** run the action, with side-effects and all *)
let effect_action action state_ref x =
  match action (!state_ref, x) with
  | s, Ok y ->
      state_ref := s ;
      y
  | s, Error e ->
      state_ref := s ;
      raise e


let no_action (s, a) = (s, Ok a)

let fail_action failure (s, a) = (s, Error failure)

(** compose two actions *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with
  | t, Error e -> (t, Error e)
  | t, Ok b -> c_of_b (t, b)


(* TODO: Infix syntax for action_seq, etc. *)

(** compose a list of actions *)
let compose_action_list action_list (s, a) =
  let rec loop action_list (s, a) =
    match action_list with
    | [] -> (s, Ok a)
    | action :: more_actions ->
      match action (s, a) with
      | t, Ok b -> loop more_actions (t, b)
      | (t, Error e) as x -> x
  in
  loop action_list (s, a)

let do_action (state, value) action = action (state, value)

let action_seq action1 action2 = compose_actions action2 action1

exception Assertion_failed

let action_assert pure_action (state, value) =
  if pure_action (state, value) then (state, Ok value)
  else (state, Error Assertion_failed)

type ('a, 'b, 'c) pure_action = 'c * 'a -> 'b

let action_of_pure_action f (state, value) = (state, Ok (f (state, value)))

(** compose two pure actions *)
let compose_pure_actions c_of_b b_of_a (s, a) = c_of_b (s, b_of_a (s, a))

let pure_action_seq b_of_a c_of_b (s, a) =
  compose_pure_actions c_of_b b_of_a (s, a)


(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Secp256k1.Key.public Secp256k1.Key.t

(** private key in public-key cryptography *)
type private_key = Secp256k1.Key.secret Secp256k1.Key.t

(*module Int256 : Int with type t = Z.t : sig
end*)

type 'a signature = Secp256k1.Sign.plain Secp256k1.Sign.t

type 'a signed = {payload: 'a; signature: 'a signature}

(* convert OCaml string to Secp256k1 msg format
   for strings representing hashes, the msg format is suitable for signing
 *)
let string_to_secp256k1_msg s =
  let open Bigarray in
  let sz = String.length s in
  let buffer = Array1.create char c_layout sz in
  let _ = for i = 0 to sz - 1 do Bigarray.Array1.set buffer i s.[i] done in
  match Secp256k1.Sign.msg_of_bytes buffer with
  | Some msg -> msg
  | None ->
      raise (Internal_error "Could not create SECP256K1.Sign.msg from string")


(* convert arbitrary OCaml value to a Secp256k1 msg representing a hash *)
let data_to_secp256k1_hashed data =
  (* data is of arbitrary type, marshal to string *)
  let data_string = Marshal.to_string data [Marshal.Compat_32] in
  let hash = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) in
  let hashed = hash data_string in
  string_to_secp256k1_msg hashed


(* create context just once, because expensive operation; assumes
   single instantiation of this module
 *)
let secp256k1_ctx = Secp256k1.Context.create [Sign; Verify]

(* digital signature is encrypted hash *)
let make_signature private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_hashed = data_to_secp256k1_hashed data in
  match Secp256k1.Sign.sign secp256k1_ctx private_key secp256k1_hashed with
  | Ok signature -> signature
  | Error s -> raise (Internal_error s)


let is_signature_valid (public_key: public_key) (signature: 'a signature) data =
  let hashed = data_to_secp256k1_hashed data in
  match
    Secp256k1.Sign.verify secp256k1_ctx ~pk:public_key ~msg:hashed ~signature
  with
  | Ok b -> b
  | Error s -> raise (Internal_error s)


let sign private_key data =
  {payload= data; signature= make_signature private_key data}


type 'a digest = Data256.t

let get_digest data =
  let data_string = Marshal.to_string data [Marshal.Compat_32] in
  let hash = Cryptokit.Hash.keccak 256 in
  Data256.of_string (Cryptokit.hash_string hash data_string)


(** Special magic digest for None. A bit ugly. *)
let null_digest = Data256.zero

module Revision = Int64
module Duration = Int64
module Timestamp = Int64

type conversation

module Address : sig
  type t

  val of_public_key : Secp256k1.Key.public Secp256k1.Key.t -> t

  val compare : t -> t -> int

  val to_string : t -> string

  val equal : t -> t -> bool
end = struct
  (* an address identifies a party (user, facilitator)
     this is per Ethereum: use the last 20 bytes of the party's public key *)

  type t = char array

  let address_size = 20

  let of_public_key public_key =
    let open Bigarray in
    let buffer =
      Secp256k1.Key.to_bytes ~compress:false secp256k1_ctx public_key
    in
    let buffer_len = Array1.dim buffer in
    let offset = buffer_len - address_size in
    Array.init address_size (fun ndx -> Array1.get buffer (offset + ndx))


  let compare address1 address2 = Pervasives.compare address1 address2

  let equal address1 address2 = compare address1 address2 = 0

  let to_string address = String.init address_size (Array.get address)
end

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
 *)
module type MapS = sig
  include Map.S

  val lens : key -> ('a t, 'a) Lens.t

  val find_defaulting : (unit -> 'a) -> key -> 'a t -> 'a
end

let defaulting default = function None -> default () | Some x -> x

module MapMake (Key : Map.OrderedType) = struct
  include Map.Make (Key)

  let lens k = Lens.{get= find k; set= add k}

  let find_defaulting default k m = defaulting default (find_opt k m)
end

module AddressMap = MapMake (Address)
module Int64Map = MapMake (Int64)

(*Lib_crypto.Blake2B.Make_merkle_tree something?*)
(* module Int64Utils = *)

let identity x = x

let konstant x y = x

let schoenfinkel x y z = x z (y z)
