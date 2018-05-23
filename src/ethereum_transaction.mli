(* ethereum_transaction.mli *)

open Legibase

val send_transaction_to_net : Main_chain.transaction_signed -> Yojson.Basic.json Lwt.t
(** post a transaction to Ethereum network *)

val send_balance_request_to_net : Address.t -> Yojson.Basic.json Lwt.t
(** for a given address, request its balance on Ethereum network *)

val get_transaction_receipt : string -> Yojson.Basic.json Lwt.t
(** get receipt for transaction identified by its transaction hash *)

val transaction_executed : string -> bool
(** has a transaction given by a hash executed; looks for a block number and block hash in the transaction receipt *)

val transaction_execution_matches_transaction : string -> Main_chain.transaction_signed -> bool
(** do the parameters of a transaction match what the Ethereum network reports for the transaction *)
