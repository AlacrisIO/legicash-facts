(* See documentation in main_chain.mli *)

open Action
open Crypto
open Trie

module TokenAmount = UInt64
module Nonce = UInt64
module ContractAddress = Address

module AccountMap = MerkleTrie (Address) (TokenAmount)

type state = {revision: Revision.t; accounts: AccountMap.t} [@@deriving lens]

type confirmation =
  { transaction_hash: Digest.t
  ; transaction_index: Unsigned.UInt64.t
  ; block_number: Revision.t
  ; block_hash: Digest.t }

(** TODO: have an actual confirmation
    For Ethereum, we might check the transaction hashes match, or
    perform a Merkle proof using the transactionsRoot in the given block
*)
let is_confirmation_valid _confirmation _transaction = true

let genesis_state = {revision= Revision.zero; accounts= AccountMap.empty}

type tx_header =
  { sender: Address.t
  ; nonce: Nonce.t
  ; gas_price: TokenAmount.t
  ; gas_limit: TokenAmount.t
  ; value: TokenAmount.t }
[@@deriving lens]

type operation =
  | TransferTokens of Address.t
  | CreateContract of Bytes.t
  | CallFunction of Address.t * Bytes.t

(* contract, data *)

(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
type transaction = {tx_header: tx_header; operation: operation} [@@deriving lens]

type transaction_signed = transaction signed

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: state digest
  ; confirmed_balance:
      TokenAmount.t (* Only store the confirmed state, and have any updates in pending *)
  ; pending_transactions: transaction_signed list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action
