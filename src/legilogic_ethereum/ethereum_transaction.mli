(* ethereum_transaction.mli -- run transactions on Ethereum net via JSON RPC *)
open Legilogic_lib
open Action
open Signing
open Types
open Ethereum_chain
open Ethereum_json_rpc

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

val block_depth_for_receipt : Revision.t
(** How many additional blocks should one wait for before to consider a transaction receipt
    after it was included in the blockchain? *)

exception Still_pending
(** Exception thrown when you depend on a transaction being confirmed, but it's still pending *)

exception Invalid_transaction_confirmation of string

val is_receipt_sufficiently_confirmed : TransactionReceipt.t -> Revision.t -> bool
(* is the given receipt sufficiently confirmed as of the given block number? *)

val check_receipt_sufficiently_confirmed : TransactionReceipt.t -> TransactionReceipt.t Lwt_exn.t

val check_transaction_confirmation :
      sender:Address.t -> recipient:Address.t -> SignedTransactionData.t -> Confirmation.t
      -> 'a -> 'a Lwt_exn.t
