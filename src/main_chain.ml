open Legibase
open Data256
open Lib
module TokenAmount = Int64
module Nonce = Int64
module ContractAddress = Address

(** State of a main chain block.
    In tezos, it's a Block_header.t *)
type state =
  {revision: Revision.t; accounts: TokenAmount.t AddressMap.t}
  [@@deriving lens]

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
    *)
type confirmation = (* state *) Digest.t

(** TODO: have an actual confirmation *)
let is_confirmation_valid confirmation transaction = true

let genesis_state = {revision= Int64.zero; accounts= AddressMap.empty}

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

(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
type transaction =
  {tx_header: tx_header; operation: operation}
  [@@deriving lens]

type transaction_signed = transaction signed

type user_state =
  { keypair: Keypairs.t
  ; pending_transactions: transaction_signed list
  ; nonce: Nonce.t }
  [@@deriving lens]

type ('a, 'b) user_action = ('a, 'b, user_state) action

module TransactionDigestSet = Set.Make(Digest)
