(* base.ml -- base types for Legicash platform *)

(*
exception Timeout of string

exception Double_spend of string
*)

type 'a legi_result = ('a, exn) result

(** unique identifier for all parties, that is, customers and facilitators *)
type public_key = Tezos_crypto.Crypto_box.public_key

type int256 =
  { field1: Int64.t
  ; field2: Int64.t
  ; field3: Int64.t
  ; field4: Int64.t }

type 'a signature = int256

val is_signature_valid: public_key -> 'a -> 'a signature -> bool = raise "foo"

type 'a signed =
  { payload: 'a
  ; signature: 'a signature }

type 'a digest = int256

(** represents account balance *)
type token_amount (* = tez *)

type revision = Int64.t

type timestamp = Int64.t (* TODO: use same as Tezos *)

type duration = Int32.t

type main_chain_heigth = Int32.t

type main_chain_state

type main_chain_transaction

type main_chain_transaction_confirmation

type conversation
