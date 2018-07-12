open Action
open Crypto
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
module State : sig
  type t = {revision: Revision.t; accounts: AccountMap.t} [@@deriving lens]
  include DigestibleS with type t := t
end

(** TODO: make sure it matches Ethereum transfer data *)
module TxHeader : sig
  type t =
    { sender: Address.t
    ; nonce: Nonce.t
    ; gas_price: TokenAmount.t
    ; gas_limit: TokenAmount.t
    ; value: TokenAmount.t }
  [@@deriving lens]
  include DigestibleS with type t := t
end

module Operation : sig
  type t =
    | TransferTokens of Address.t
    (* recipient *)
    | CreateContract of Bytes.t
    (* code *)
    | CallFunction of Address.t * Bytes.t
  include DigestibleS with type t := t
end

(* contract, data *)

module Transaction : sig
  type t = {tx_header: TxHeader.t; operation: Operation.t} [@@deriving lens]
  include DigestibleS with type t := t
end

module TransactionSigned : sig
  type t = Transaction.t signed
  include DigestibleS with type t := t
end

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
*)
module Confirmation : sig
  type t = { transaction_hash: Digest.t
           ; transaction_index: Unsigned.UInt64.t
           ; block_number: Revision.t
           ; block_hash: Digest.t }
  include DigestibleS with type t := t
end

val is_confirmation_valid : Confirmation.t -> TransactionSigned.t -> bool

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: State.t digest
  ; confirmed_balance:
      TokenAmount.t (* Only store the confirmed state, and have any updates in pending *)
  ; pending_transactions: TransactionSigned.t list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action

val genesis_state : State.t
