(* operator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Types
open Signing
open Action
open Lib
open Digesting
open Yojsoning

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi
open Side_chain
open Side_chain_server_config
open Contract_config

let operator_contract_log = true

let topic_of_hash (hash : Digest.t) : Bytes.t option =
  Some (encode_function_parameters [abi_digest hash])

(* Topics below correspond to events in the operator.sol code
   Be careful of adjusting everything when you change the type like adding a balance.
 *)

let topic_of_signature = digest_of_string >> topic_of_hash

let (topic_of_deposited: Bytes.t option) =
  topic_of_signature "Deposited(address,address,uint256,uint256)"

let (topic_of_state_update: Bytes.t option) =
  topic_of_signature "StateUpdate(address,bytes32,uint64)"

let (topic_of_claim_withdrawal: Bytes.t option) =
  topic_of_signature "ClaimWithdrawal(address,address,uint64,uint256,uint256,bytes32,uint64)"

let (topic_of_withdraw: Bytes.t option) =
  topic_of_signature "Withdrawal(address,uint64,uint256,uint256,bytes32)"


(** build the encoding of a call to the "deposit" function of the operator contract
    address argument is the operator *)
let make_deposit_call : operator:Address.t -> contract_address:Address.t -> Ethereum_chain.Operation.t =
  fun ~operator ~contract_address ->
  if operator_contract_log then
    Logging.log "Beginning of make_deposit_call";
  let parameters = [ abi_address operator ] in
  let call = encode_function_call { function_name = "deposit"; parameters } in
  Operation.CallFunction (contract_address, call)


let pre_deposit : operator:Address.t -> amount:TokenAmount.t -> contract_address:Address.t -> PreTransaction.t =
  fun ~operator ~amount ~contract_address ->
  if operator_contract_log then
    Logging.log "Beginning of pre_deposit";
  let oper = make_deposit_call ~operator ~contract_address in
  PreTransaction.{operation=oper; value=amount; gas_limit=Side_chain_server_config.deposit_gas_limit}

let make_claim_withdrawal_call : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~confirmed_state_update ->
  if operator_contract_log then
    Logging.log "Beginning of make_claim_withdrawal_call";
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_digest confirmed_state_update.state
                   ; abi_revision confirmed_state_update.revision ] in
  let call = encode_function_call { function_name = "claim_withdrawal"; parameters } in
  Operation.CallFunction (contract_address, call)



let make_withdraw_call : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_state_update ->
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state_update.state
                   ; abi_revision confirmed_state_update.revision ] in
  let call = encode_function_call { function_name = "withdraw"; parameters } in
  Operation.CallFunction (contract_address, call)




let make_challenge_withdrawal_too_large_revision : contract_address:Address.t -> claimant:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~claimant ~operator ~operator_revision ~value ~bond ~confirmed_state_update ->
  let parameters = [ abi_address operator
                   ; abi_address claimant
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state_update.state
                   ; abi_revision confirmed_state_update.revision ] in
  let call = encode_function_call { function_name = "challenge_withdrawal__too_large_revision"; parameters } in
  Operation.CallFunction (contract_address, call)


let make_operation_has_claim_been_rejected : contract_address:Address.t -> claimant:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~claimant ~operator ~operator_revision ~value ~bond ~confirmed_state_update ->
  let parameters = [ abi_address operator
                   ; abi_address claimant
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state_update.state
                   ; abi_revision confirmed_state_update.revision ] in
  let call = encode_function_call { function_name = "has_claim_been_rejected"; parameters } in
  Operation.CallFunction (contract_address, call)




let make_state_update_call : contract_address:Address.t -> confirmed_state_update:StateUpdate.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~confirmed_state_update ->
  if operator_contract_log then
    Logging.log "Beginning of make_state_update_call";
  let parameters = [ abi_digest confirmed_state_update.state;
                     abi_revision confirmed_state_update.revision] in
  let (call : bytes) = encode_function_call { function_name = "claim_state_update"; parameters } in
  Operation.CallFunction (contract_address, call)

let contract_address =
  lazy (contract_config_of_config_file "contract_config.json").contract_address

let get_contract_address : unit -> Address.t Lwt_exn.t =
  Lwt_exn.catching_arr (fun () -> Lazy.force contract_address)

(* TODO Add support for including a bond with the claim.
   Which routine to include? Bonds contains:
   ---get_gas_cost_estimate
   ---minimum_bond
   ---require_bond
 *)
