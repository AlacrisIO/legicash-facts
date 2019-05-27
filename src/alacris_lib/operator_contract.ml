(* operator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Types
open Signing

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi
open Side_chain
open Side_chain_server_config
open Digesting




let topic_of_address (addr : Address.t) : Bytes.t option =
  Some (encode_function_parameters [abi_address addr])

let topic_of_revision (rev : Revision.t) : Bytes.t option =
  Some (encode_function_parameters [abi_revision rev])

let topic_of_amount (amnt : TokenAmount.t) : Bytes.t option =
  Some (encode_function_parameters [abi_token_amount amnt])

let topic_of_hash (hash : Digest.t) : Bytes.t option =
  Some (encode_function_parameters [abi_digest hash])

(* Topics below correspond to events in the operator.sol code
   Be careful of adjusting everything when you change the type like adding a balance.
 *)

let (topic_of_deposited: Bytes.t option) =
  topic_of_hash (digest_of_string "Deposited(address,address,uint256,uint256)")

let (topic_of_state_update: Bytes.t option) =
  topic_of_hash (digest_of_string "StateUpdate(address,bytes32,uint256,uint64,uint64)")

let (topic_of_claim_withdrawal: Bytes.t option) =
  topic_of_hash (digest_of_string "ClaimWithdrawal(address,uint64,uint256,bytes32,uint64,uint256,uint256,uint64)")

let (topic_of_withdraw: Bytes.t option) =
  topic_of_hash (digest_of_string "Withdrawal(address,uint64,uint256,uint256,bytes32)")

let (topic_of_rejected_claim_status: Bytes.t option) =
  topic_of_hash (digest_of_string "RejectedClaimStatus(uint64)")



let contract_address = ref Address.zero

let set_contract_address address = contract_address := address

let get_contract_address () =
  Logging.log "get_contract_address : contract_address=%s" (Address.to_0x !contract_address);
  !contract_address

(** build the encoding of a call to the "deposit" function of the operator contract
    address argument is the operator *)
let make_deposit_call : operator:Address.t -> contract_address:Address.t -> Ethereum_chain.Operation.t =
  fun ~operator ~contract_address ->
  let parameters = [ abi_address operator ] in
  let call = encode_function_call { function_name = "deposit"; parameters } in
  Operation.CallFunction (contract_address, call)

let pre_deposit : operator:Address.t -> amount:TokenAmount.t -> contract_address:Address.t -> PreTransaction.t =
  fun ~operator ~amount ~contract_address ->
  let oper = make_deposit_call ~operator ~contract_address in
  PreTransaction.{operation=oper; value=amount; gas_limit=Side_chain_server_config.deposit_gas_limit}



let make_claim_withdrawal_call : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> confirmed_pair:PairRevisionDigest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~confirmed_pair ->
  let (confirmed_revision, confirmed_state) = confirmed_pair in
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_digest confirmed_state
                   ; abi_revision confirmed_revision ] in
  let call = encode_function_call { function_name = "claim_withdrawal"; parameters } in
  Operation.CallFunction (contract_address, call)





(* The generic code which is repeated since many calls have the same structure *)
let make_operation_related_withdraw : name_oper:String.t -> contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_pair:PairRevisionDigest.t -> Ethereum_chain.Operation.t =
  fun ~name_oper ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair ->
  let (confirmed_revision, confirmed_state) = confirmed_pair in
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state
                   ; abi_revision confirmed_revision ] in
  let call = encode_function_call { function_name = name_oper; parameters } in
  Operation.CallFunction (contract_address, call)


let make_withdraw_call : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_pair:PairRevisionDigest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair ->
  let name_oper = "withdraw" in
  make_operation_related_withdraw ~name_oper ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair


let make_challenge_withdrawal_too_large_revision : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_pair:PairRevisionDigest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair ->
  let name_oper = "challenge_withdrawal__too_large_revision" in
  make_operation_related_withdraw ~name_oper ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair



let make_operation_has_claim_been_rejected : contract_address:Address.t -> operator:Address.t -> operator_revision:Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_pair:PairRevisionDigest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair ->
  let name_oper = "challenge_withdrawal__too_large_revision" in
  make_operation_related_withdraw ~name_oper ~contract_address ~operator ~operator_revision ~value ~bond ~confirmed_pair




(* calls the "claim_state_update" that calls "make_claim" that works with a mapping
   from bytes32 to integers.
   We have Revision = UInt64

 *)
let make_state_update_call : Digest.t -> Revision.t -> Ethereum_chain.Operation.t =
  fun state_digest operator_revision ->
  let parameters = [ abi_digest state_digest; abi_revision operator_revision] in
  let (call : bytes) = encode_function_call { function_name = "claim_state_update"; parameters } in
  Operation.CallFunction (get_contract_address (), call)




(* TODO Add support for including a bond with the claim.
   Which routine to include? Bonds contains:
   ---get_gas_cost_estimate
   ---minimum_bond
   ---require_bond
 *)
