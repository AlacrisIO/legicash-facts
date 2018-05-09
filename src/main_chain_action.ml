open Legibase
open Main_chain

let stub_state = ref genesis_state

let stub_state_digest = ref (Digest.make genesis_state)

(** Stub for gas price. Here set at 50 wei. *)
let stub_gas_price = ref (Int64.of_int 50)

let update_stub_state new_state =
  stub_state := new_state ;
  stub_state_digest := Digest.make new_state


let make_tx_header (user_state, (gas_limit, value)) =
  { sender= user_state.keypair.address
  ; nonce= user_state.nonce
  ; gas_price= !stub_gas_price
  ; gas_limit
  ; value }


let add_pending_transaction (user_state, transaction) =
  ( { user_state with
      pending_transactions= transaction :: user_state.pending_transactions }
  , Ok transaction )


let issue_transaction =
  (fun (user_state, (operation, value, gas_limit)) ->
    (user_state, (value, gas_limit)) ^|>
      (action_of_pure_action
         (pure_action_seq make_tx_header (fun (user_state, tx_header) ->
              sign user_state.keypair.private_key {tx_header; operation} ))))
  ^>> add_pending_transaction


let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (user_state, (recipient, amount)) =
  issue_transaction
    (user_state, (TransferTokens recipient, amount, transfer_gas_limit))
