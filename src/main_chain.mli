open Base

(** Represents account balance *)
module TokenAmount = Int64
(* = tez *)

(** Transaction request (to be) posted to the main chain (i.e. Tezos) *)
type main_chain_request

(** State of a main chain block.
    In tezos, it's a Block_header.t *)
type main_chain_state =
  { main_chain_revision: Revision.t
  ; main_chain_accounts: TokenAmount.t Key256Map.t }

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type main_chain_confirmation

(** Main chain operation + knowledge about the operation *)
type main_chain_episteme =
  { main_chain_request: main_chain_request
  ; main_chain_confirmation_option: main_chain_confirmation option }

val genesis_main_chain_state : main_chain_state
