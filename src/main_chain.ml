open Base
open Data256
open Lib
module TokenAmount = Int64
module Nonce = Int64
module ContractAddress = Data256

(** State of a main chain block.
    In tezos, it's a Block_header.t *)
type main_chain_state =
  { main_chain_revision: Revision.t
  ; main_chain_accounts: TokenAmount.t Data256Map.t }

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type main_chain_confirmation = main_chain_state digest

let genesis_main_chain_state =
  {main_chain_revision= Int64.zero; main_chain_accounts= Data256Map.empty}


type main_chain_tx_header =
  { sender: PublicKey.t
  ; nonce: Nonce.t
  ; gas_price: TokenAmount.t
  ; gas_limit: Int32.t }

type main_chain_send_details =
  { recipient: PublicKey.t
  ; amount: TokenAmount.t
  ; fee: TokenAmount.t
  ; tx_header: main_chain_tx_header }

type main_chain_transfer_tokens_details =
  {recipient: PublicKey.t; amount: TokenAmount.t}

type main_chain_contract_details = {amount: TokenAmount.t; code: Bytes.t}

type main_chain_function_details =
  {amount: TokenAmount.t; contract: ContractAddress.t; data: Bytes.t}

type main_chain_operation =
  | TransferTokens of main_chain_transfer_tokens_details
  | CreateContract of main_chain_contract_details
  | CallFunction of main_chain_function_details

(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
type main_chain_transaction =
  { main_chain_tx_header: main_chain_tx_header
  ; main_chain_operation: main_chain_operation }

type main_chain_transaction_signed = main_chain_transaction signed

type main_chain_user_state =
  { public_key: public_key
  ; private_key: private_key
  ; main_chain_pending_transactions: main_chain_transaction_signed list
  ; main_chain_nonce: Nonce.t }

type ('a, 'b) main_chain_user_action = ('a, 'b, main_chain_user_state) action

let stub_main_chain_state = ref genesis_main_chain_state

let stub_main_chain_state_digest = ref (get_digest genesis_main_chain_state)

(** Stub for gas price. Here set at 50 wei. *)
let stub_gas_price = ref (Int64.of_int 50)

let update_stub_main_chain_state new_state =
  stub_main_chain_state := new_state ;
  stub_main_chain_state_digest := get_digest new_state


let make_main_chain_tx_header (main_chain_user_state, gas_limit) =
  { sender= main_chain_user_state.public_key
  ; nonce= main_chain_user_state.main_chain_nonce
  ; gas_price= !stub_gas_price
  ; gas_limit }


let add_main_chain_pending_transaction (main_chain_user_state, transaction) =
  ( { main_chain_user_state with
      main_chain_pending_transactions=
        transaction :: main_chain_user_state.main_chain_pending_transactions }
  , Ok transaction )


let issue_main_chain_transaction =
  action_seq
    (fun (user_state, (main_chain_operation, gas_limit)) ->
      do_action (user_state, gas_limit)
        (action_of_pure_action
           (pure_action_seq make_main_chain_tx_header
              (fun (user_state, tx_header) ->
                sign user_state.private_key
                  {main_chain_tx_header= tx_header; main_chain_operation} )))
      )
    add_main_chain_pending_transaction


let transfer_tokens (user_state, invoice) =
  issue_main_chain_transaction
    (user_state, (TransferTokens invoice, Int32.of_int 21000))
