(* ethereum_transaction.mli -- run transactions on Ethereum net *)

open Crypto

val sign_transaction : Keypair.t -> Main_chain.Transaction.t -> Main_chain.Transaction.t signed

val send_transaction_to_net : Main_chain.TransactionSigned.t -> Yojson.Basic.json Lwt.t
(** post a transaction to Ethereum network *)

val send_balance_request_to_net : Address.t -> Yojson.Basic.json Lwt.t
(** for a given address, request its balance on Ethereum network *)

val get_transaction_count : Address.t -> Yojson.Basic.json Lwt.t
(** get count of transactions sent from an address; use to find next transaction nonce *)

val get_transaction_receipt : string -> Yojson.Basic.json Lwt.t
(** get receipt for transaction identified by its transaction hash *)

val transaction_executed : string -> bool
(** has a transaction given by a hash executed; looks for a block number and block hash in the transaction receipt *)

val transaction_execution_matches_transaction : string -> Main_chain.TransactionSigned.t -> bool
(** do the parameters of a transaction match what the Ethereum network reports for the transaction *)

module Test : sig
  val json_contains_error : Yojson.Basic.json -> bool
  (** whether JSON record contains "error" field *)
  val json_result_to_int : Yojson.Basic.json -> int
  (** convert "result" field, given as a string, to an int *)
  val list_accounts : unit -> Yojson.Basic.json Lwt.t
  (** JSON list of account addresses on net *)
  val new_account : unit -> Yojson.Basic.json Lwt.t
  (** creates new account with given JSON address on net *)
  val unlock_account : ?duration:int -> Address.t -> Yojson.Basic.json Lwt.t
  (** unlocks account for given duration on net *)
  val get_first_account : unit -> Yojson.Basic.json
  (** get first account listed on net; for dev network, this is the prefunded account *)
end
