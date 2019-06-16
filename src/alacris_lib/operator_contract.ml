(* operator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Types
open Signing
open Action
open Logging
open Lib
open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi
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


let retrieve_contract_address_quadruple : Digest.t -> (Address.t * Digest.t * Digest.t * Revision.t) Lwt_exn.t =
  fun transaction_hash ->
  let open Lwt_exn in
  Ethereum_json_rpc.eth_get_transaction_receipt transaction_hash
  >>= function
  | None -> bork "No tx receipt for contract creation"
  | Some receipt ->
     let contract_address = Option.get receipt.contract_address in
     let contract_block_number = receipt.block_number in
     Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
     >>= fun trans ->
     let code_hash = Digesting.digest_of_string (Bytes.to_string trans.input) in
     return (contract_address, code_hash, transaction_hash, contract_block_number)




let contract_address = ref Address.zero
let contract_block_number = ref Revision.zero

let set_contract_address address = contract_address := address

let set_contract_block_number blk_number = contract_block_number := blk_number

let get_contract_address () =
  if operator_contract_log then
    log "get_contract_address : contract_address=%s" (Address.to_0x !contract_address);
  !contract_address

let get_contract_block_number () =
  if operator_contract_log then
    log "get_contract_block_number : contract_block_number=%s" (Revision.to_string !contract_block_number);
  !contract_block_number

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



let make_claim_withdrawal_call : contract_address:Address.t -> operator:Address.t -> Revision.t -> value:TokenAmount.t -> confirmed_state:Digest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address  ~operator  operator_revision  ~value  ~confirmed_state ->
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "claim_withdrawal"; parameters } in
  Operation.CallFunction (contract_address, call)


(* Here abi_revision = abi_uint64 because Revision = UInt64 *)
let make_withdraw_call : contract_address:Address.t -> operator:Address.t -> Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_state:Digest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address  ~operator  operator_revision  ~value  ~bond  ~confirmed_state ->
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "withdraw"; parameters } in
  Operation.CallFunction (contract_address, call)



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
