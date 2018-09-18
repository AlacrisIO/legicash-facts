(* ethereum_transaction.mli -- run transactions on Ethereum net via JSON RPC *)
open Legilogic_lib
open Action
open Yojsoning
open Digesting
open Signing

open Main_chain

(** Has a transaction given by a hash executed?
    Looks for a block number and block hash in the transaction receipt *)
val transaction_executed : digest -> bool Lwt_exn.t

val transaction_execution_matches_transaction : digest -> Transaction.t -> bool Lwt_exn.t
(** do the parameters of a transaction match what the Ethereum network reports for the transaction *)

(** Ensure that the private_key for the keypair exists in the Ethereum client,
    with the given password string. Return the corresponding address.
    Do not raise an error if the key was already imported. *)
val ensure_private_key : ?timeout:float -> ?log:bool -> keypair * string -> Address.t Lwt_exn.t

val unlock_account : ?duration:int -> address -> bool Lwt_exn.t
(** unlocks account for given duration (in seconds) on net *)

module Test : sig
  val json_result_to_int : yojson -> int
  (** convert "result" field, given as a string, to an int *)

  val list_accounts : unit -> Address.t list Lwt_exn.t
  (** JSON list of account addresses on net *)

  val new_account : unit -> Address.t Lwt_exn.t
  (** creates new account with given JSON address on net *)

  val get_first_account : unit -> Address.t Lwt_exn.t
  (** get first account listed on net; for dev network, this is the prefunded account *)

  val wait_for_contract_execution : digest -> unit Lwt_exn.t
  (** return when contract has completed *)
end
