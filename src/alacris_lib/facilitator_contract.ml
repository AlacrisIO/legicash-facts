(* facilitator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Signing

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi

let contract_address = ref Address.zero

let set_contract_address address = contract_address := address

let get_contract_address () = !contract_address

let make_deposit_call facilitator =
  let memo = "" in
  let parameters = [ abi_address facilitator
                   ; abi_bytes_dynamic_of_string memo ] in
  let call = encode_function_call { function_name = "deposit"; parameters } in
  Operation.CallFunction (get_contract_address (), call)

let make_withdraw_call facilitator ticket bond confirmed_state =
  let parameters = [ abi_address facilitator
                   ; abi_revision ticket
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "withdraw"; parameters } in
  Operation.CallFunction (get_contract_address (), call)
