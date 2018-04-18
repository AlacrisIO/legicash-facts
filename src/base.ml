exception Not_implemented

exception Timeout of string

exception Double_spend of string

type 'a legi_result = ('a, exn) result

type ('a, 'b, 'c) action = 'c * 'a -> 'c * 'b legi_result

(** run the action, with side-effects and all *)
let effect_action action state_ref x =
  match action (!state_ref, x) with
  | (s, Ok y) -> state_ref := s ; y
  | (s, Error e) -> state_ref := s ; raise e

(** compose two actions *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with
  | (t, Error e) -> (t, Error e)
  | (t, Ok b) -> c_of_b (t, b)

(** compose a list of actions *)
let compose_action_list action_list (s, a) =
  let rec loop action_list (s, a) =
    match action_list with
    | [] -> (s, Ok a)
    | action::more_actions ->
       match action (s, a) with
       | (t, Ok b) -> loop more_actions (t, b)
       | (t, Error e) as x -> x in
  loop action_list (s, a)

let do_action (state, value) action = action (state, value)

let action_seq action1 action2 = compose_actions action2 action1

type ('a, 'b, 'c) pure_action = 'c * 'a -> 'b

let action_of_pure_action f (state, value) = (state, Ok (f (state,value)))

(** compose two pure actions *)
let compose_pure_actions c_of_b b_of_a (s, a) =
  c_of_b (s, b_of_a (s, a))


(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Key256.t (* WAS: Tezos_crypto.Crypto_box.public_key *)

(** private key in public-key cryptography *)
type private_key = Key256.t

type int256 = Key256.t
(*module Int256 : Int with type t = Z.t : sig
end*)

type 'a signature = int256

let is_signature_valid public_key signature data = (Key256.compare public_key signature) == 0

(** TODO: unstub the signature function *)
let make_signature private_key data = private_key

type 'a signed = {payload: 'a; signature: 'a signature}

let sign private_key data = {payload= data; signature= make_signature private_key data}

type 'a digest = int256

(** TODO: unstub the digest function *)
let get_digest = fun _ -> Key256.one

(** Special magic digest for None. A bit ugly. *)
let null_digest = Key256.zero

module Revision = Int64

module Duration = Int64

module Timestamp = Int64

(* TODO: use same as Tezos *)

type conversation

(** A pure mapping from 'a to 'b suitable for use in interactive merkle proofs
    Let's cheat for now.
    TODO: Tezos must have something we should use.
    probably Tezos_crypto.S.MERKLE_TREE or Tezos_crypto.Blake2B.Make_merkle_tree
 *)
module PublicKey = Key256

module Key256Map = Map.Make(PublicKey)

module Int64Map = Map.Make(Int64)
(*Lib_crypto.Blake2B.Make_merkle_tree something?*)

(* module Int64Utils = *)
