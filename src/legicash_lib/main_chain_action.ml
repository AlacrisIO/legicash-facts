open Legilogic_ethereum
open Ethereum_action
open Main_chain

open Facilitator_contract

let deposit_gas_limit = TokenAmount.of_int 1000000

(* call facilitator deposit function on main chain *)
let deposit (facilitator_address, amount) =
  issue_transaction (make_deposit_call facilitator_address, amount, deposit_gas_limit)
