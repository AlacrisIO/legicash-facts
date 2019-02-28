(* ethereum_transaction.mli -- run transactions on Ethereum net via JSON RPC *)
open Legilogic_lib
open Action
open Digesting
open Signing

open Ethereum_chain

(** Has a transaction given by a hash executed?
    Looks for a block number and block hash in the transaction receipt *)
val transaction_executed : digest -> bool Lwt_exn.t

val transaction_execution_matches_transaction : digest -> Transaction.t -> bool Lwt_exn.t
(** do the parameters of a transaction match what the Ethereum network reports for the transaction *)

val ensure_private_key : ?timeout:float -> ?log:bool -> Keypair.t -> Address.t Lwt_exn.t
(** Ensure that the private_key for the keypair exists in the Ethereum client,
    with the given password string. Return the corresponding address.
    Do not raise an error if the key was already imported. *)

val ensure_eth_signing_address : ?timeout:float -> ?log:bool -> Address.t -> unit Lwt_exn.t
(** Ensure that the Ethereum JSON RPC node (typically geth) can sign messages for the given address
    with the registered key and password. *)

val list_accounts : unit -> Address.t list Lwt_exn.t
(** JSON list of account addresses on net *)

val get_first_account : unit -> Address.t Lwt_exn.t
(** get first account listed on net; for dev network, this is the prefunded account *)

exception Bad_password

val unlock_account : ?duration:int -> address -> unit Lwt_exn.t
(** unlocks account for given duration (in seconds) on net *)

val is_receipt_successful : Ethereum_json_rpc.TransactionReceipt.t -> Transaction.t -> bool