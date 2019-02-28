(* operator-contract.mli -- get encodings of calls to the contract *)

open Legilogic_lib
open Types
open Signing
open Action

open Legilogic_ethereum
open Ethereum_chain


val topic_of_address : Address.t -> Bytes.t option

val topic_of_revision : Revision.t -> Bytes.t option

val topic_of_amount : TokenAmount.t -> Bytes.t option

val topic_of_hash : Digest.t -> Bytes.t option


val set_contract_address : Address.t -> unit
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call (?) *)

val get_contract_address : unit -> Address.t
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call *)

val pre_deposit : operator:Address.t -> TokenAmount.t -> PreTransaction.t
(** Create a PreTransaction for a contract call that deposits the amount
    into the sender's account on the operator *)

val deposit : Address.t -> (Address.t * TokenAmount.t, Transaction.t * Ethereum_json_rpc.TransactionReceipt.t) Lwt_exn.arr

val make_claim_withdrawal_call : Address.t -> Address.t -> Revision.t -> TokenAmount.t -> Digest.t -> Ethereum_chain.Operation.t


val make_withdraw_call : Address.t -> Address.t -> Revision.t -> TokenAmount.t -> TokenAmount.t -> Digest.t -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "withdraw" function of the operator contract
    arguments: operator address, bond amount, confirmed state
*)


val make_state_update_call : Digest.t -> Ethereum_chain.Operation.t
(** Operator address, contract address, and the ethereum main chain *)
(* TODO: signature from the smart court registry () *)
