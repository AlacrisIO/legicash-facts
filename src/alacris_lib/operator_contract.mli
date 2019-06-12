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


val retrieve_contract_address_quadruple : Digest.t -> (Address.t * Digest.t * Digest.t * Revision.t) Lwt_exn.t


val set_contract_address : Address.t -> unit
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call (?) *)

val set_contract_block_number : Revision.t -> unit
(** set the block under which the contract is stored on Ethereum *)

val get_contract_address : unit -> Address.t
(** get the contract address of the contract on Ethereum
    TODO: use a fixed address, obviating this call *)

val get_contract_block_number : unit -> Revision.t
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call *)

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
