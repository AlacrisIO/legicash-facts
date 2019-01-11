(* operator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Signing
open Action

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi

let contract_address = ref Address.zero

let set_contract_address address = contract_address := address

let get_contract_address () = !contract_address

(** build the encoding of a call to the "deposit" function of the operator contract
    address argument is the operator *)
let make_deposit_call : Address.t -> Ethereum_chain.Operation.t =
  fun operator ->
    let memo = "" in
    let parameters = [ abi_address operator
                     ; abi_bytes_dynamic_of_string memo ] in
    let call = encode_function_call { function_name = "deposit"; parameters } in
    Operation.CallFunction (get_contract_address (), call)

let deposit_gas_limit = TokenAmount.of_int 100000

let pre_deposit ~operator amount =
  PreTransaction.{operation=make_deposit_call operator;value=amount;gas_limit=deposit_gas_limit}

(* Create a signed transaction to call the contract to deposit money onto
   an account managed by the operator, ready to be committed on the main chain
   TODO: get rid of this, have proper state machine in side_chain_user. *)
let deposit user (operator, amount) =
  let open Ethereum_user in
  OngoingTransactionStatus.Wanted (pre_deposit ~operator amount)
  |> Lwt_exn.(add_ongoing_transaction user
              >>> of_lwt track_transaction
              >>> check_transaction_confirmed)

let make_withdraw_call operator ticket bond confirmed_state =
  let parameters = [ abi_address operator
                   ; abi_revision ticket
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "withdraw"; parameters } in
  Operation.CallFunction (get_contract_address (), call)
