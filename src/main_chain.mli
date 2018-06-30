open Legibase
open Action
open Crypto
open Keypair
open Trie

module TokenAmount : IntS

module Nonce : IntS

module AccountMap : (MerkleTrieS with type key = Address.t and type value = TokenAmount.t)

(** State of a main chain block.
    TODO:
    1- Make it work with Ethereum, where it describes a block
    2- Make it work for tezos, where it's a Block_header.t (?)
    3- Abstract into a module signature that can be provided by one or the other.
*)
type state = {revision: Revision.t; accounts: AccountMap.t} [@@deriving lens]

(** TODO: make sure it matches Ethereum transfer data *)
type tx_header =
  { sender: Address.t
  ; nonce: Nonce.t
  ; gas_price: TokenAmount.t
  ; gas_limit: TokenAmount.t
  ; value: TokenAmount.t }
[@@deriving lens]

type operation =
  | TransferTokens of Address.t
  (* recipient *)
  | CreateContract of Bytes.t
  (* code *)
  | CallFunction of Address.t * Bytes.t

(* contract, data *)

type transaction = {tx_header: tx_header; operation: operation} [@@deriving lens]

type transaction_signed = transaction signed

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
*)
type confirmation =
  { transaction_hash: Digest.t
  ; transaction_index: Unsigned.UInt64.t
  ; block_number: Revision.t
  ; block_hash: Digest.t }

val is_confirmation_valid : confirmation -> transaction_signed -> bool

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: state digest
  ; confirmed_balance:
      TokenAmount.t (* Only store the confirmed state, and have any updates in pending *)
  ; pending_transactions: transaction_signed list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action

val genesis_state : state
