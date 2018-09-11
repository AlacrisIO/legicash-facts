open Legilogic_lib
open Action
open Yojsoning
open Digesting
open Signing
open Persisting
open Types
open Merkle_trie

(** TODO: use Quantity everywhere, like the yellow paper, or maybe use Cardinal and Ordinal *)

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
  include SignableS with type t := t
end

(* Result of eth_getTransactionByHash *)
module TransactionInformation : sig
  type t =
    { blockHash: Digest.t option (* hash of the block where this transaction was in. null when its pending. *)
    ; blockNumber: Revision.t option (* block number where this transaction was in. null when its pending. *)
    ; from: Address.t (* address of the sender. *)
    ; gas: TokenAmount.t (* gas provided by the sender. *)
    ; gasPrice: TokenAmount.t (* gas price provided by the sender in Wei. *)
    ; hash: Digest.t (* hash of the transaction. *)
    ; input: Yojsoning.Bytes.t (* the data send along with the transaction. *)
    ; nonce: Nonce.t (* the number of transactions made by the sender prior to this one. *)
    ; to_: Address.t option [@key "to"] (* DATA, address of the receiver. null when its a contract creation transaction. *)
    ; transactionIndex: Revision.t (* integer of the transactions index position in the block. null when its pending. *)
    ; value: TokenAmount.t (* value transferred in Wei. *)
    ; v: string (* QUANTITY - ECDSA recovery id *)
    ; r: string (* DATA, 32 Bytes - ECDSA signature r *)
    ; s: string (* DATA, 32 Bytes - ECDSA signature s *) }
  include YojsonableS with type t := t
end

module LogObject : sig
  type t =
    { removed: bool (* true when the log was removed, due to a chain reorganization. false if its a valid log. *)
    ; logIndex: Revision.t option (* integer of the log index position in the block. null when its pending log. *)
    ; transactionIndex: Revision.t option (* integer of the transactions index position log was created from. null when its pending log. *)
    ; transactionHash: Digest.t option (* hash of the transactions this log was created from. null when its pending log. *)
    ; blockNumber: Revision.t option (* hash of the block where this log was in. null when its pending. null when its pending log. *)
    ; blockHash: Digest.t option (* the block number where this log was in. null when its pending. null when its pending log. *)
    ; address: Address.t (* address from which this log originated. *)
    ; data: Yojsoning.Bytes.t (* contains the non-indexed arguments of the log. *)
    ; topics: Digest.t list (* Array of 0 to 4 32 Bytes DATA of indexed log arguments. (In solidity: The first topic is the hash of the signature of the event (e.g. Deposit(address,bytes32,uint256)), except you declared the event with the anonymous specifier.) *) }
  include YojsonableS with type t := t
end

module Bloom : YojsonableS with type t = Bytes.t

module TransactionReceipt : sig
  type t =
    { blockHash: Digest.t (* hash of the block where this transaction was in. *)
    ; blockNumber: Revision.t (* block number where this transaction was in. *)
    ; contractAddress: Address.t option (* The contract address created, if the transaction was a contract creation, otherwise null. *)
    ; cumulativeGasUsed: TokenAmount.t (* The total amount of gas used when this transaction was executed in the block. *)
    ; from: Address.t (* The address of the sender. *)
    ; to_: Address.t option [@key "to"] (* The address of the receiver. null when it’s a contract creation transaction. *)
    ; gasUsed: TokenAmount.t (* The amount of gas used by this specific transaction alone. *)
    ; logs: LogObject.t list (* Array of log objects, which this transaction generated. *)
    ; logsBloom: Bloom.t (* A bloom filter of logs/events generated by contracts during transaction execution. Used to efficiently rule out transactions without expected logs. *)
    ; root: Digest.t option (* Merkle root of the state trie after the transaction has been executed (optional after Byzantium hard fork EIP609) *)
    ; status: TokenAmount.t option (* ‘0x0’ indicates transaction failure , ‘0x1’ indicates transaction success. Set for blocks mined after Byzantium hard fork EIP609, null before. *)
    ; transactionHash: Digest.t (* hash of the transaction. *)
    ; transactionIndex: Revision.t (* Integer of the transactions index position in the block. *) }
  include YojsonableS with type t := t
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
           ; transaction_index: Revision.t
           ; block_number: Revision.t
           ; block_hash: digest }
  include PersistableS with type t := t
end

val is_confirmation_valid : Confirmation.t -> Transaction.t -> bool

(** State for the user client.
    confirmed_state is a digest of the confirmed Main_chain.State that this is relative to.
    confirmed_balance is the balance of the user account relative to that confirmed_state.
*)
module UserState : sig
  type t =
    { keypair: Keypair.t
    ; confirmed_state: digest
    ; confirmed_balance: TokenAmount.t
    ; pending_transactions: Transaction.t list
    ; nonce: Nonce.t }
  [@@deriving lens { prefix=true }]
  val init : keypair -> t
  include PersistableS with type t := t
end

module UserAction : ActionS with type state = UserState.t
module UserAsyncAction : AsyncActionS with type state = UserState.t

val genesis_state : State.t

