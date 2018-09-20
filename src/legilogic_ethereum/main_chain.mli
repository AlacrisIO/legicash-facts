(* TODO: rename this module to ethereum_chain or some such ? *)
open Legilogic_lib
open Yojsoning
open Digesting
open Signing
open Persisting
open Types
open Merkle_trie

(** TODO: use Quantity everywhere, like the yellow paper, or maybe use Cardinal and Ordinal *)

module Quantity : UIntS with type t = UInt256.t

module TokenAmount : UIntS with type t = UInt256.t

module Nonce : UIntS with type t = Revision.t

module AccountMap : (MerkleTrieS with type key = Address.t and type value = TokenAmount.t)

(** State of a main chain block.
    TODO:
    1- Make it work with Ethereum, where it describes a block
    2- Make another variant work for tezos, where it's a Block_header.t (?)
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
  [@@deriving lens { prefix=true }]
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

(** Confirmation of a transaction on the main chain
    an old enough block on the main chain
    TODO: maybe also include a path and/or merkle tree from there?
*)
module Confirmation : sig
  type t = { transaction_hash: digest
           ; transaction_index: Revision.t
           ; block_number: Revision.t
           ; block_hash: digest }
  include PersistableS with type t := t
end

val is_confirmation_valid : Confirmation.t -> Transaction.t -> bool

val genesis_state : State.t
