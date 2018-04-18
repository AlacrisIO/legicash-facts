open Base
open Key256
open Lib

module TokenAmount = Int64
module Nonce = Int64
module ContractAddress = Key256

(** Transaction request (to be) posted to the main chain (i.e. Tezos) *)
type main_chain_request

(** State of a main chain block.
    In tezos, it's a Block_header.t *)
type main_chain_state =
  { main_chain_revision: Revision.t
  ; main_chain_accounts: TokenAmount.t Key256Map.t }

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type main_chain_confirmation = main_chain_state digest

let genesis_main_chain_state =
  {main_chain_revision= Int64.zero; main_chain_accounts= Key256Map.empty}


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

type main_chain_transaction =
  { main_chain_tx_header: main_chain_tx_header
  ; main_chain_operation: main_chain_operation }

type main_chain_transaction_signed = main_chain_transaction signed

type main_chain_user_state =
  { public_key: public_key
  ; private_key: private_key
  ; main_chain_pending_transactions: main_chain_transaction_signed list
  ; main_chain_nonce: Nonce.t }

type main_chain_deposit_invoice =
  {facilitator: PublicKey.t; amount: TokenAmount.t}

type ('a, 'b) main_chain_user_action = ('a, 'b, main_chain_user_state) action

let make_main_chain_tx_header = bottom

let add_main_chain_pending_transaction = bottom

let issue_main_chain_transaction =
  action_seq
    (fun (user_state, main_chain_operation) ->
      do_action (user_state, ())
        (action_seq make_main_chain_tx_header
           (action_of_pure_action (fun (user_state, tx_header) ->
                sign user_state.private_key
                  {main_chain_tx_header= tx_header; main_chain_operation} )))
      )
    (fun (user_state, transaction) ->
      ( add_main_chain_pending_transaction user_state transaction
      , Ok transaction ) )


let transfer_tokens (user_state, invoice) = bottom ()
