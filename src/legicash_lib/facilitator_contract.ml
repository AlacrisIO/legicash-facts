(* facilitator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Types
open Signing

open Legilogic_ethereum
open Main_chain
open Ethereum_abi

let contract_address = ref Address.zero

let set_contract_address address = contract_address := address

let make_deposit_call facilitator_address =
  assert (!contract_address != Address.zero);
  let parameters = [ abi_address_of_address facilitator_address
                   (* TODO: a better memo? *)
                   ; abi_bytes_dynamic_of_bytes (Bytes.of_string "Deposit") ] in
  let signature = function_signature_hash { function_name = "deposit"; parameters } in
  (* TODO: this looks fishy; the hash only has the parameter types and loses the actual parameter values *)
  Operation.CallFunction (!contract_address, Bytes.of_string signature)

let make_withdraw_call facilitator_address ticket bond confirmed_state =
  let digest_bytes = Digest.to_big_endian_bits confirmed_state |> Bytes.of_string in
  let parameters = [ abi_address_of_address facilitator_address
                   ; abi_uint64_of_int64 ticket
                   ; abi_uint_of_int bond
                   ; abi_bytes_of_bytes digest_bytes ] in
  let function_signature_hash = function_signature_hash { function_name = "withdraw"; parameters } in
  (* TODO: this looks fishy; the hash only has the parameter types and loses the actual parameter values *)
  Operation.CallFunction (!contract_address, Bytes.of_string function_signature_hash)
