open Legilogic_ethereum
open Ethereum_action
open Main_chain

open Facilitator_contract

let deposit_gas_limit = TokenAmount.of_int 1000000

(* create a signed deposit transaction, ready to be  call facilitator deposit function on main chain *)
let make_deposit (facilitator_address, amount) =
  make_signed_transaction (make_deposit_call facilitator_address) amount deposit_gas_limit

let deposit (facilitator_address, amount) =
  let open UserAsyncAction in
  make_deposit (facilitator_address, amount)
  >>= confirm_transaction
