(* facilitator-contract.mli -- get encodings of calls to the contract *)

open Legilogic_lib
open Types
open Signing

open Legilogic_ethereum
open Ethereum_chain


val set_contract_address : Address.t -> unit
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call
*)

val get_contract_address : unit -> Address.t
(** set the address of the contract on Ethereum
    TODO: use a fixed address, obviating this call
*)

val make_deposit_call : Address.t -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "deposit" function of the facilitator contract
    address argument is the facilitator
*)

val make_withdraw_call : Address.t -> Revision.t -> TokenAmount.t -> Digest.t -> Ethereum_chain.Operation.t
(** build the encoding of a call to the "withdraw" function of the facilitator contract
    arguments: facilitator address, bond amount, confirmed state
*)
