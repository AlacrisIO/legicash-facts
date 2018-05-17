(* ethereum_transaction.mli *)

open Legibase

val send_transaction_to_net : Main_chain.transaction -> Yojson.Basic.json Lwt.t
(** post a side chain transaction to Ethereum network *)

val send_balance_request_to_net : Address.t -> Yojson.Basic.json Lwt.t
(** for a given address, request its balance on Ethereum network *)
