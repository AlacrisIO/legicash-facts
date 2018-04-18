open Base

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
type main_chain_confirmation

(* type main_chain_tx_header =
   { sender : PublicKey.t
   ; confirmed_main_chain_state_digest: main_chain_state digest
   ; confirmed_main_chain_state_revision: Revision.t
   ; fee : TokenAmount.t
   ; validity_within: Duration.t } *)

(* derived from Ethereum transfer data *)
type main_chain_tx_header =
   { sender : PublicKey.t
   ; nonce : Nonce.t
   ; gas_price : TokenAmount.t
   ; gas_limit : Int32.t
   }

type main_chain_transfer_tokens_details =
  { recipient : PublicKey.t
  ; amount : TokenAmount.t
  }

type main_chain_contract_details =
  { amount : TokenAmount.t
  ; code : Bytes.t
  }

type main_chain_function_details =
  { amount : TokenAmount.t
  ; contract : ContractAddress.t
  ; data : Bytes.t
  }

type main_chain_operation =
  | TransferTokens of main_chain_transfer_tokens_details
  | CreateContract of main_chain_contract_details
  | CallFunction of main_chain_function_details

type main_chain_transaction =
  { main_chain_tx_header : main_chain_tx_header
  ; main_chain_operation : main_chain_operation
  }

type main_chain_transaction_signed = main_chain_transaction signed

type main_chain_user_state =
  { public_key: public_key
  ; private_key: private_key
  ; main_chain_pending_transactions: main_chain_transaction_signed list
  ; main_chain_nonce : Nonce.t
  }

type ('a, 'b) main_chain_user_action = ('a, 'b, main_chain_user_state) action

val transfer_tokens : (main_chain_transfer_tokens_details, main_chain_request) main_chain_user_action

val genesis_main_chain_state : main_chain_state
