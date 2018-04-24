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

(** compose two actions *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with
  | t, Error e -> (t, Error e)
  | t, Ok b -> c_of_b (t, b)

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

type int256 = Data256.t

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
   single instantation of this module
 *)
let rec signing_ctx = Secp256k1.Context.create [Sign]

(* digital signature is encrypted hash *)
and make_signature private_key data =
  (* change representation of data to use Secp256k1 signing *)
  let secp256k1_hashed = data_to_secp256k1_hashed data in
  match Secp256k1.Sign.sign signing_ctx private_key secp256k1_hashed with
  | Ok signature -> signature
  | Error s -> raise (Internal_error s)


(* create context just once, because expensive operation; assumes
   single instantiation of this module
 *)
let rec verify_ctx = Secp256k1.Context.create [Verify]

and is_signature_valid (public_key: public_key) (signature: 'a signature) data =
  let hashed = data_to_secp256k1_hashed data in
  match
    Secp256k1.Sign.verify verify_ctx ~pk:public_key ~msg:hashed ~signature
  with
  | Ok b -> b
  | Error s -> raise (Internal_error s)

let sign private_key data =
  {payload= data; signature= make_signature private_key data}

type 'a digest = int256

(** TODO: unstub the digest function *)
let get_digest _ = Data256.one

(** Special magic digest for None. A bit ugly. *)
let null_digest = Data256.zero

module Revision = Int64
module Duration = Int64
module Timestamp = Int64

type conversation

module Address : sig
  type t

  val of_public_key : public_key -> t

  val compare : t -> t -> int
end = struct
  (* an address identifies a party (user, facilitator)
     this is per Ethereum: use the last 20 bytes of the party's public key *)

  type t = char array

  let address_size = 20

  let of_public_key (pk: public_key) =
    let buffer = Secp256k1.Key.to_buffer pk in
    let buffer_size = Bigarray.Array1.dim buffer in
    let mk_array_entry ndx = Bigarray.Array1.get buffer (buffer_size - address_size + ndx) in
    Array.init address_size mk_array_entry

  let compare address1 address2 = Pervasives.compare address1 address2
end

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
 *)
module AddressMap = Map.Make (Address)
module Int64Map = Map.Make (Int64)

(*Lib_crypto.Blake2B.Make_merkle_tree something?*)
(* module Int64Utils = *)
