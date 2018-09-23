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

(** Ensure that the private_key for the keypair exists in the Ethereum client,
    with the given password string. Return the corresponding address.
    Do not raise an error if the key was already imported. *)
val ensure_private_key : ?timeout:float -> ?log:bool -> keypair * string -> Address.t Lwt_exn.t

val list_accounts : unit -> Address.t list Lwt_exn.t
(** JSON list of account addresses on net *)

val get_first_account : unit -> Address.t Lwt_exn.t
(** get first account listed on net; for dev network, this is the prefunded account *)

module Test : sig
  val get_prefunded_address : unit -> Address.t Lwt_exn.t
  (** get the prefunded address on the test network *)

  val display_balance : (string -> string -> string -> 'a) -> Address.t -> TokenAmount.t -> 'a
  (** display an account having the given balance given a way to print address, optional name and balance *)

  val ensure_address_prefunded : Address.t -> TokenAmount.t -> Address.t -> unit Lwt_exn.t
  (** Given a prefunded address and a minimum amount of tokens, ensure that the second given address
      is prefunded to the tune of at least the given amount *)

  val ensure_test_account : ?min_balance:TokenAmount.t -> Address.t
    -> (string * keypair, unit) Lwt_exn.arr
  (** Given a minimum amount of tokens and a prefunded address with lots of them,
      ensure that our private Ethereum network has an account with given nickname and address,
      an empty geth password, and at least a billion tokens in it. *)

  val fund_accounts : ?min_balance:TokenAmount.t -> (unit, unit) Lwt_exn.arr
  (** transfers funds from funding account to account with given address, if balance less than min_balance *)
end
