open Action
open Crypto
open Db
open Merkle_trie

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
  type t = {revision: Revision.t; accounts: AccountMap.t}
  [@@deriving lens { prefix=true } ]
  include PersistableS with type t := t
end

(** TODO: make sure it matches Ethereum transfer data *)
module TxHeader : sig
  type t =
    { sender: Address.t
    ; nonce: Nonce.t
    ; gas_price: TokenAmount.t
    ; gas_limit: TokenAmount.t
    ; value: TokenAmount.t }
  [@@deriving lens { prefix=true } ]
  include PersistableS with type t := t
end

module Operation : sig
  type t =
    | TransferTokens of Address.t
    (* recipient *)
    | CreateContract of Bytes.t
    (* code *)
    | CallFunction of Address.t * Bytes.t
  include PersistableS with type t := t
end

(* contract, data *)

module Transaction : sig
  type t = {tx_header: TxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true } ]
  include PersistableS with type t := t
end

module TransactionSigned : sig
  type t = Transaction.t signed
  include PersistableS with type t := t
end

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
*)
module Confirmation : sig
  type t = { transaction_hash: digest
           ; transaction_index: UInt64.t
           ; block_number: Revision.t
           ; block_hash: digest }
  include PersistableS with type t := t
end

val is_confirmation_valid : Confirmation.t -> TransactionSigned.t -> bool

(** State for the user client.
    confirmed_state is a digest of the confirmed Main_chain.State that this is relative to.
    confirmed_balance is the balance of the user account relative to that confirmed_state.
*)
type user_state =
  { keypair: Keypair.t
  ; confirmed_state: digest
  ; confirmed_balance: TokenAmount.t
  ; pending_transactions: TransactionSigned.t list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action

type ('input, 'output) user_async_action = ('input, 'output, user_state) async_action

val genesis_state : State.t
