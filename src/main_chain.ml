(* See documentation in main_chain.mli *)

open Action
open Marshaling
open Crypto
open Db
open Merkle_trie

module TokenAmount = UInt64
module Nonce = UInt64
module ContractAddress = Address

module AccountMap = MerkleTrie (Address) (TokenAmount)

module State = struct
  (* TODO: have an actual model of the Ethereum main chain, marshaled the Ethereum way. *)
  type t = {revision: Revision.t; accounts: AccountMap.t}
  [@@deriving lens { prefix=true } ]
  module PreMarshalable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {revision;accounts} -> (revision, accounts))
        (fun revision accounts -> {revision; accounts})
        Revision.marshaling AccountMap.marshaling
    let make_persistent = normal_persistent
    let walk_dependencies =
      one_dependency (fun x -> x.accounts) AccountMap.dependency_walking
  end
  include (Persistable (PreMarshalable) : PersistableS with type t := t)
end

module Confirmation = struct
  type t = { transaction_hash: digest
           ; transaction_index: UInt64.t
           ; block_number: Revision.t
           ; block_hash: digest }
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun {transaction_hash; transaction_index; block_number; block_hash} ->
           (transaction_hash, transaction_index, block_number, block_hash))
        (fun transaction_hash transaction_index block_number block_hash ->
           {transaction_hash; transaction_index; block_number; block_hash})
        Digest.marshaling UInt64.marshaling Revision.marshaling Digest.marshaling
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

(** TODO: have an actual confirmation
    For Ethereum, we might check the transaction hashes match, or
    perform a Merkle proof using the transactionsRoot in the given block
*)
let is_confirmation_valid _confirmation _transaction = true

let genesis_state = State.{revision= Revision.zero; accounts= AccountMap.empty}

module TxHeader = struct
  type t = { sender: Address.t
           ; nonce: Nonce.t
           ; gas_price: TokenAmount.t
           ; gas_limit: TokenAmount.t
           ; value: TokenAmount.t }
  [@@deriving lens { prefix=true } ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling5
        (fun {sender; nonce; gas_price; gas_limit; value} ->
           (sender, nonce, gas_price, gas_limit, value))
        (fun sender nonce gas_price gas_limit value ->
           {sender; nonce; gas_price; gas_limit; value})
        Address.marshaling Nonce.marshaling
        TokenAmount.marshaling TokenAmount.marshaling TokenAmount.marshaling
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module Operation = struct
  type t =
    | TransferTokens of Address.t
    | CreateContract of Bytes.t
    | CallFunction of Address.t * Bytes.t
  module PrePersistable = OCamlPersistable (struct type nonrec t = t end)
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

(* contract, data *)
(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
module Transaction = struct
  type t = {tx_header: TxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true } ]
  module PrePersistable = OCamlPersistable (struct type nonrec t = t end)
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module TransactionSigned = struct
  type t = Transaction.t signed
  module PrePersistable = OCamlPersistable (struct type nonrec t = t end)
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: digest
  ; confirmed_balance: TokenAmount.t
  ; pending_transactions: TransactionSigned.t list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action
(** type of synchronous actions on the main chain user state *)

type ('input, 'output) user_async_action = ('input, 'output, user_state) async_action
(** type of asynchronous actions on the main chain user state *)

module TransactionDigestSet = DigestSet
