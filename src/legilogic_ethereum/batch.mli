open Legilogic_lib
open Action
open Signing

open Ethereum_chain
open Ethereum_json_rpc
open Contract_config

val batch_contract_init : Address.t -> string
(** Given the address of the owner, return a string of EVM bytecode to initialize
    a batch transfer contract with given owner, that may be passed as argument to
    a CreateContract operation *)

val ensure_batch_contract : (Address.t, ContractConfig.t) Lwt_exn.arr
(** Ensure the existence of a batch sending contract for the given address
    as confirmed on the blockchain, its ContractConfig saved to the application database.
    Return said ContractConfig.
 *)

val batch_transfer : Address.t -> (Address.t * TokenAmount.t) list -> TransactionReceipt.t Lwt_exn.t
(** From given address, send to each of the listed recipient addresses the listed amount,
    Using the sender's batch contract (creating a new one if needs be). *)

module Test : sig
  val ensure_test_accounts : ?min_balance:TokenAmount.t -> ?accounts:(string * Keypair.t) list
                             -> (unit, TransactionReceipt.t) Lwt_exn.arr
  (** Transfers funds from Croesus enough to match the given min_balance (by default 1 ETH),
      to given test accounts (by default Alice, Bob and Trent).
      Registers an empty password with geth for each of the test accounts.
      Assumes our private geth with its own test Ethereum network. *)

  val ensure_addresses_prefunded :
    Address.t -> TokenAmount.t -> Address.t list -> TransactionReceipt.t Lwt_exn.t
  (** Given a prefunded address and a minimum amount of tokens, ensure that
      each address in the given list is prefunded to the tune of at least the given amount *)
end
