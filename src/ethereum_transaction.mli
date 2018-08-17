(* ethereum_transaction.mli -- run transactions on Ethereum net *)

open Yojsoning
open Crypto

val send_transaction_to_net : Main_chain.TransactionSigned.t -> yojson Lwt.t
(** post a transaction to Ethereum network *)

val send_balance_request_to_net : Address.t -> yojson Lwt.t
(** for a given address, request its balance on Ethereum network *)

val get_transaction_count : Address.t -> yojson Lwt.t
(** get count of transactions sent from an address; use to find next transaction nonce *)

val get_transaction_receipt : string -> yojson Lwt.t
(** get receipt for transaction identified by its transaction hash *)

val transaction_executed : string -> bool Lwt.t
(** has a transaction given by a hash executed; looks for a block number and block hash in the transaction receipt *)

val transaction_execution_matches_transaction : string -> Main_chain.TransactionSigned.t -> bool Lwt.t
(** do the parameters of a transaction match what the Ethereum network reports for the transaction *)

module Test : sig
  val assert_json_error_free : string -> yojson -> unit
  (** whether JSON record contains "error" field *)
  val json_result_to_int : yojson -> int
  (** convert "result" field, given as a string, to an int *)
  val list_accounts : unit -> yojson Lwt.t
  (** JSON list of account addresses on net *)
  val new_account : unit -> yojson Lwt.t
  (** creates new account with given JSON address on net *)
  val unlock_account : ?duration:int -> Address.t -> yojson Lwt.t
  (** unlocks account for given duration (in seconds) on net *)
  val get_first_account : unit -> yojson Lwt.t
  (** get first account listed on net; for dev network, this is the prefunded account *)
  val wait_for_contract_execution : string -> unit Lwt.t
  (** return when contract has completed *)
end
