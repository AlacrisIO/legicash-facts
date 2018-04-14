open Base

(** Represents account balance *)
type token_amount = Int64.t
(* = tez *)

(** Number of consensus rounds if linear block-based. Timestamp if DAG-based? *)
type main_chain_height

(** Transaction request (to be) posted to the main chain (i.e. Tezos) *)
type main_chain_request

(** State of a main chain block.
    In tezos, it's a Block_header.t *)
type main_chain_state

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type main_chain_confirmation

(** Main chain operation + knowledge about the operation *)
type main_chain_episteme =
  { main_chain_request: main_chain_request
  ; maybe_main_chain_confirmation: main_chain_confirmation option }

