open Legicash_base
open Legibase
module TokenAmount = Int64
module Nonce = Int64

(** State of a main chain block.
    TODO:
    1- Make it work with Ethereum, where it describes a block
    2- Make it work for tezos, where it's a Block_header.t (?)
    3- Abstract into a module signature that can be provided by one or the other.
 *)
type state =
  {revision: Revision.t; accounts: TokenAmount.t AddressMap.t}
  [@@deriving lens]

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type confirmation

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

type transaction =
  {tx_header: tx_header; operation: operation}
  [@@deriving lens]

type transaction_signed = transaction signed

type user_state =
  { address: Address.t
  ; public_key: public_key
  ; private_key: private_key
  ; pending_transactions: transaction_signed list
  ; nonce: Nonce.t }
  [@@deriving lens]

type ('a, 'b) user_action = ('a, 'b, user_state) action

val genesis_state : state
