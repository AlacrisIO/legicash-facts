(* operator-contract.mli -- get encodings of calls to the contract *)

open Legilogic_lib
open Types
open Signing
open Legilogic_ethereum
open Ethereum_chain
open Action

val topic_of_address:  Address.t     -> Bytes.t option
val topic_of_revision: Revision.t    -> Bytes.t option
val topic_of_amount:   TokenAmount.t -> Bytes.t option
val topic_of_hash:     Digest.t      -> Bytes.t option


type contract_address_config =
  { contract_address : Address.t
  ; code_hash : Digest.t
  ; creation_hash : Digest.t
  ; creation_block : Revision.t
  }


val retrieve_contract_config : Digest.t -> contract_address_config Lwt_exn.t



val get_contract_address : unit -> Address.t Lwt_exn.t
(** get the contract address of the contract on Ethereum *)

val get_contract_address_exn : unit -> Address.t Lwt.t
(** get the contract address of the contract on Ethereum *)

val pre_deposit : operator:Address.t -> amount:TokenAmount.t -> contract_address:Address.t -> PreTransaction.t
(** Create a PreTransaction for a contract call that deposits the amount
    into the sender's account on the operator *)

val make_claim_withdrawal_call
   : contract_address:Address.t
  -> operator:Address.t
  -> Revision.t
  -> value:TokenAmount.t
  -> confirmed_state:Digest.t
  -> Ethereum_chain.Operation.t

val make_withdraw_call
   : contract_address:Address.t
  -> operator:Address.t
  -> Revision.t
  -> value:TokenAmount.t
  -> bond:TokenAmount.t
  -> confirmed_state:Digest.t
  -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "withdraw" function of the operator contract
    arguments: operator address, bond amount, confirmed state
*)

val make_state_update_call : contract_address:Address.t -> operator_digest:Digest.t -> operator_revision:Revision.t -> Ethereum_chain.Operation.t
(** Operator address, contract address, and the ethereum main chain *)
(* TODO: signature from the smart court registry () *)
