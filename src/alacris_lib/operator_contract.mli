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


type quadruple_contract = {contract_address: Address.t; code_hash: Digest.t; creation_hash: Digest.t; creation_block: Revision.t}

val test_equality_quadruple : quadruple_contract -> quadruple_contract -> bool
(** return true if the quadruples are equal and false otherwise. Could be done via metaprogramming *)

val retrieve_contract_address_quadruple : Digest.t -> quadruple_contract Lwt_exn.t

val convert_quad_format : (Address.t * Digest.t * Digest.t * Revision.t) -> quadruple_contract

val get_contract_address_general : quadruple_contract -> Address.t Lwt_exn.t


val get_contract_address : unit -> Address.t
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

val make_state_update_call : Digest.t -> Revision.t -> Ethereum_chain.Operation.t
(** Operator address, contract address, and the ethereum main chain *)
(* TODO: signature from the smart court registry () *)
