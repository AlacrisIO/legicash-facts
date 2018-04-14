(* base.ml -- base types for Legicash platform *)

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

(** compose two actions, but left to right *)
let compose_actions c_of_b b_of_a (s, a) =
  match b_of_a (s, a) with
  | (t, Error e) -> (t, Error e)
  | (t, Ok b) -> c_of_b (t, b)

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Tezos_crypto.Crypto_box.public_key

type int256 =
  {field1: Int64.t; field2: Int64.t; field3: Int64.t; field4: Int64.t}

type 'a signature = int256

let is_signature_valid key payload signature = raise Not_implemented

type 'a signed = {payload: 'a; signature: 'a signature}

type 'a digest = int256

type revision = Int64.t

type timestamp = Int64.t

(* TODO: use same as Tezos *)

type duration = Int32.t

type conversation
