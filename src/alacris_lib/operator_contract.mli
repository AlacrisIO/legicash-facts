(* operator-contract.mli -- get encodings of calls to the contract *)

open Legilogic_lib
open Types
open Signing
open Legilogic_ethereum
open Ethereum_chain
open Side_chain
open Action

val topic_of_hash:     Digest.t      -> Bytes.t option

val topic_of_deposited : Bytes.t option
val topic_of_state_update : Bytes.t option
val topic_of_claim_withdrawal : Bytes.t option
val topic_of_withdraw : Bytes.t option

val get_contract_address : unit -> Address.t Lwt_exn.t
(** get the contract address of the contract on Ethereum *)

val pre_deposit : operator:Address.t -> amount:TokenAmount.t -> contract_address:Address.t -> PreTransaction.t
(** Create a PreTransaction for a contract call that deposits the amount
    into the sender's account on the operator *)

val make_claim_withdrawal_call
   : contract_address:Address.t
  -> operator:Address.t
  -> operator_revision:Revision.t
  -> value:TokenAmount.t
  -> confirmed_state_update:StateUpdate.t
  -> Ethereum_chain.Operation.t

val make_withdraw_call
   : contract_address:Address.t
  -> operator:Address.t
  -> operator_revision:Revision.t
  -> value:TokenAmount.t
  -> bond:TokenAmount.t
  -> confirmed_state_update:StateUpdate.t
  -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "withdraw" function of the operator contract
    arguments: operator address, bond amount, confirmed state
*)

val make_challenge_withdrawal_too_large_revision
    : contract_address:Address.t
  -> claimant:Address.t
  -> operator:Address.t
  -> operator_revision:Revision.t
  -> value:TokenAmount.t
  -> bond:TokenAmount.t
  -> confirmed_state_update:StateUpdate.t
  -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "challenge_withdrawal_too_large_revision"
    function of the operator contract arguments:
    this is a way to provide some challenge
*)

val make_operation_has_claim_been_rejected
   : contract_address:Address.t
  -> claimant:Address.t
  -> operator:Address.t
  -> operator_revision:Revision.t
  -> value:TokenAmount.t
  -> bond:TokenAmount.t
  -> confirmed_state_update:StateUpdate.t
  -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "challenge_withdrawal_too_large_revision"
    function of the operator contract arguments:
    this is a way to provide some challenge
*)

val make_state_update_call : contract_address:Address.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t
(** Operator address, contract address, and the ethereum main chain *)
