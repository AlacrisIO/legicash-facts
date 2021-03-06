(* JSON RPC interface to Ethereum node.
   Classical reference:
   * eth_* functionality:
     https://github.com/ethereum/wiki/wiki/JSON-RPC
     https://ethereumbuilders.gitbooks.io/guide/content/en/ethereum_json_rpc.html
     https://wiki.parity.io/JSONRPC
   * personal_* functionality:
     https://github.com/ethereum/go-ethereum/wiki/Managing-your-accounts
     https://github.com/ethereum/go-ethereum/wiki/Management-APIs
     https://wiki.parity.io/JSONRPC-personal-module
   Note that geth documentation is sometimes inaccurate. Parity wallet documentation tends
   to be of greater quality, but is not 100% compatible with geth (which is what we currently use),
   nor are either perfectly compatible with web3.
   An abstraction layer is required somewhere at some point.
*)
open Legilogic_lib
open Action
open Yojsoning
open Persisting
open Types
open Signing

open Ethereum_chain

(** Send a JSON-RPC 2.0 request to the Ethereum client, and return the response. *)
val ethereum_json_rpc :
  string -> (yojson -> 'a) -> ('b -> yojson) ->
  ?timeout:float -> ?log:bool -> 'b -> 'a Lwt_exn.t

(** defaultBlock parameter for ethereum queries
    eth_estimateGas, eth_getBalance, eth_getCode, eth_getTransactionCount, eth_getStorageAt, eth_call
*)
module BlockParameter : sig
  type t =
    | Block_number of Revision.t
    | Latest
    | Earliest
    | Pending
  include PersistableS with type t := t
end

(*
(* A contract address or a list of address *)
module ContractAoListA : sig
  type t =
    | Contract_address of Address.t
    | List_addresses of Address.t list
  include PersistableS with type t := t
end
 *)

module TransactionCondition : sig
  type t =
    | Block_number of Revision.t
    | UTC_timestamp of Revision.t (* in seconds *)
    | Null
  include PersistableS with type t := t
end

(** Parameters for a transaction as per Ethereum JSON RPC interface for
    eth_estimateGas, eth_sendTransaction, eth_signTransaction, personal_signTransaction *)
module TransactionParameters : sig
  type t =
    { from: Address.t (* 20 Bytes - The address the transaction is send from. *)
    ; to_: Address.t option [@key "to"] (* 20 Bytes - The address the transaction is directed to. *)
    ; gas: TokenAmount.t option (* Integer of the gas provided for the transaction execution. eth_call consumes zero gas, but this parameter may be needed by some executions. *)
    ; gas_price: TokenAmount.t option [@key "gasPrice"] (* Integer of the gas price used for each paid gas. *)
    ; value: TokenAmount.t option (* Integer of the value sent with this transaction. *)
    ; data: Yojsoning.Bytes.t option (* 4 byte hash of the method signature followed by encoded parameters. For details see Ethereum Contract ABI. *)
    ; nonce: Nonce.t option (* Integer of a nonce. This allows to overwrite your own pending transactions that use the same nonce. *)
    ; condition: TransactionCondition.t option } (* Conditional submission of the transaction. Can be either an integer block number { block: 1 } or UTC timestamp (in seconds) { time: 1491290692 } or null. *)
  include PersistableS with type t := t
  val of_operation : Address.t -> Operation.t -> TokenAmount.t -> t
  val of_pre_transaction : Address.t -> PreTransaction.t -> t
  val of_transaction : Transaction.t -> t
end

(* Result of eth_getTransactionByHash *)
module TransactionInformation : sig
  type t =
    { hash: Digest.t (* hash of the transaction. *)
    ; nonce: Nonce.t (* the number of transactions made by the sender prior to this one. *)
    ; block_hash: Digest.t option [@key "blockHash"] (* hash of the block where this transaction was in. null when its pending. *)
    ; block_number: Revision.t option [@key "blockNumber"] (* block number where this transaction was in. null when its pending. *)
    ; transaction_index: Revision.t option [@key "transactionIndex"] (* integer of the transactions index position in the block. null when its pending. *)
    ; from: Address.t option (* address of the sender. *)
    ; to_: Address.t option [@key "to"] (* DATA, address of the receiver. null when its a contract creation transaction. *)
    ; value: TokenAmount.t (* value transferred in Wei. *)
    ; gas_price: TokenAmount.t [@key "gasPrice"] (* gas price provided by the sender in Wei. *)
    ; gas: TokenAmount.t (* gas provided by the sender. *)
    ; input: Yojsoning.Bytes.t (* the data send along with the transaction. *)
    ; v: Quantity.t option (* QUANTITY - ECDSA recovery id *)
    ; standard_v: Quantity.t option (* QUANTITY - ECDSA recovery id, standardized to 0 or 1 -- parity *)
    ; r: UInt256.t option (* DATA, 32 Bytes - ECDSA signature r *)
    ; s: UInt256.t option (* DATA, 32 Bytes - ECDSA signature s *)
    ; raw: Data.t option (* raw transaction data *)
    ; public_key: public_key option (* public key of the signer *)
    ; network_id: Quantity.t option (* the network id of the transaction, if any *)
    ; creates: Digest.t option (* creates contract hash *)
    ; condition: yojson option (* conditional submission, Block number in block or timestamp in time or null. (parity-feature) *) } [@@deriving show]
  include YojsonableS with type t := t
end

(* Input fields for the eth_getLogs routine *)
module EthObject : sig
  type t =
    { from_block: BlockParameter.t option [@key "fromBlock"] (* optional. Value latest if absent *)
    ; to_block: BlockParameter.t option [@key "toBlock"] (* optional. Value latest if absent *)
    ; address : Address.t option (* Contract address or list of addresses *)
    ; topics : Bytes.t option list option (* List of topics to search for *)
    ; blockhash : Digest.t option (* the block hash *)
    }
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
    { block_hash: Digest.t (* hash of the block where this transaction was in. *)
    ; block_number: Revision.t (* quantity "or tag" (?) block number where this transaction was in. *)
    ; contract_address: Address.t option (* The contract address created, if the transaction was a contract creation, otherwise null. *)
    ; cumulative_gas_used: TokenAmount.t (* The total amount of gas used when this transaction was executed in the block. *)
    ; from: Address.t (* The address of the sender. *)
    ; to_: Address.t option [@key "to"] (* The address of the receiver. null when it’s a contract creation transaction. *)
    ; gas_used: TokenAmount.t (* The amount of gas used by this specific transaction alone. *)
    ; logs: LogObject.t list (* Array of log objects, which this transaction generated. *)
    ; logs_bloom: Bloom.t (* A bloom filter of logs/events generated by contracts during transaction execution. Used to efficiently rule out transactions without expected logs. *)
    ; status: TokenAmount.t (* ‘0x0’ indicates transaction failure , ‘0x1’ indicates transaction success. Set for blocks mined after Byzantium hard fork EIP609, null before. *)
    ; transaction_hash: Digest.t (* hash of the transaction. *)
    ; transaction_index: Revision.t (* Integer of the transactions index position in the block. *) }
  include PersistableS with type t := t
  val to_confirmation : t -> Confirmation.t
end

module EthListLogObjects : sig
  type t = LogObject.t list (* The list of matching objects *)
  include YojsonableS with type t := t
end

(** Make a call or transaction, which won’t be added to the blockchain and returns the used gas,
    which can be used for estimating the used gas. *)
val eth_accounts :
  ?timeout:float -> ?log:bool
  -> unit -> Address.t list Lwt_exn.t

val eth_block_number :
  ?timeout:float -> ?log:bool
  -> unit -> Revision.t Lwt_exn.t
(** Get the latest block number *)

(** Parameters for eth_call as per Ethereum JSON RPC interface *)
module CallParameters : sig
  type t =
    { from: Address.t (* 20 Bytes - The address the transaction is send from. *)
    ; to_: Address.t option [@key "to"] (* 20 Bytes - The address the transaction is directed to. *)
    ; gas: TokenAmount.t option (* Integer of the gas provided for the transaction execution. eth_call consumes zero gas, but this parameter may be needed by some executions. *)
    ; gas_price: TokenAmount.t option [@key "gasPrice"] (* Integer of the gas price used for each paid gas. *)
    ; value: TokenAmount.t option (* Integer of the value sent with this transaction. *)
    ; data: Yojsoning.Bytes.t option } (* 4 byte hash of the method signature followed by encoded parameters. For details see Ethereum Contract ABI. *)
  include PersistableS with type t := t
  val of_operation : Address.t -> Operation.t -> t
  val of_pre_transaction : Address.t -> PreTransaction.t -> t
  val of_transaction : Transaction.t -> t
end

val eth_call :
  ?timeout:float -> ?log:bool
  -> CallParameters.t * BlockParameter.t -> Data.t Lwt_exn.t
(** Get value computed by function at given block *)

val eth_chain_id :
  ?timeout:float -> ?log:bool
  -> unit -> UInt256.t option Lwt_exn.t

val eth_estimate_gas :
  ?timeout:float -> ?log:bool
  -> TransactionParameters.t -> TokenAmount.t Lwt_exn.t
(** Make a call or transaction, which won’t be added to the blockchain and returns the used gas,
    which can be used for estimating the used gas. *)

val eth_gas_price :
  ?timeout:float -> ?log:bool
  -> unit -> TokenAmount.t Lwt_exn.t
(** Get the current gas price in wei *)

val eth_get_balance :
  ?timeout:float -> ?log:bool
  -> Address.t * BlockParameter.t -> TokenAmount.t Lwt_exn.t
(** Get the balance on given account at given block *)

val eth_get_code :
  ?timeout:float -> ?log:bool
  -> Address.t * BlockParameter.t -> Bytes.t Lwt_exn.t
(** Get the code for the contract at given address at given block *)

val eth_get_transaction_by_hash :
  ?timeout:float -> ?log:bool
  -> Digest.t -> TransactionInformation.t Lwt_exn.t

(** Returns the number of transactions sent from an address. *)
val eth_get_transaction_count :
  ?timeout:float -> ?log:bool
  -> Address.t * BlockParameter.t -> Nonce.t Lwt_exn.t

val eth_get_transaction_receipt :
  ?timeout:float -> ?log:bool
  -> Digest.t -> TransactionReceipt.t option Lwt_exn.t

val eth_send_raw_transaction :
  ?timeout:float -> ?log:bool
  -> Data.t -> Digest.t Lwt_exn.t
(** Send a raw transaction *)

val eth_get_logs :
  ?timeout:float -> ?log:bool
  -> EthObject.t -> EthListLogObjects.t Lwt_exn.t
(** Send a raw transaction *)

val eth_send_transaction :
  ?timeout:float -> ?log:bool
  -> TransactionParameters.t -> Digest.t Lwt_exn.t
(** Send a transaction *)

val eth_sign :
  ?timeout:float -> ?log:bool
  -> Address.t * Data.t -> Data.t Lwt_exn.t
(** Sign some data *)

module ParitySignedTransaction : sig
  type t =
    { raw: Data.t
    ; tx:  TransactionInformation.t
    } [@@deriving show]
  include PersistableS with type t := t
end

val eth_sign_transaction :
  ?timeout:float -> ?log:bool
  -> TransactionParameters.t -> ParitySignedTransaction.t Lwt_exn.t
(** Sign a transaction from an unlocked account.
    NB: This operation exists in the Parity wallet but not in geth, and we therefore don't use it. *)

(** Import the private_key in the Ethereum client with given password string,
    and return the address of the corresponding account.
    Raise an error if the private key already exists.
*)
val personal_import_raw_key :
  ?timeout:float -> ?log:bool
  -> private_key * string -> Address.t Lwt_exn.t

val personal_list_accounts :
  ?timeout:float -> ?log:bool
  -> unit -> Address.t list Lwt_exn.t

val personal_lock_account :
  ?timeout:float -> ?log:bool
  -> address -> bool Lwt_exn.t

val personal_new_account :
  ?timeout:float -> ?log:bool
  -> string -> Address.t Lwt_exn.t

module SignedTx : sig
  [@warning "-39"]
  type t =
    { nonce : Revision.t
    ; gas_price : TokenAmount.t
    ; gas : TokenAmount.t
    ; to_ : Address.t option
    ; value : TokenAmount.t
    ; input : Data.t
    ; v : UInt256.t option (* before signing it's the chain ID, after it's from the signature *)
    ; r : UInt256.t option (* before signing it's 0; after it's from the signature *)
    ; s : UInt256.t option (* before signing it's 0; after it's from the signature *)
    ; hash : Digest.t }
  [@@deriving show]
  include PersistableS with type t := t
end

module SignedTransaction : sig
  type t =
    { raw: Data.t
    ; tx:  SignedTx.t
    } [@@deriving show]
  include PersistableS with type t := t
end

val transaction_data_of_signed_transaction : SignedTransaction.t -> SignedTransactionData.t

val personal_sign_transaction :
  ?timeout:float -> ?log:bool
  -> TransactionParameters.t * string -> SignedTransaction.t Lwt_exn.t
(** Sign a transaction without unlocking.
    NB: The result type isn't standard across implementations.
    geth, parity and web3 all have different return types.
    This is for geth.
 *)

val personal_unlock_account :
  ?timeout:float -> ?log:bool
  -> address * string * int option -> bool Lwt_exn.t

module TxPoolContent : sig
  type entry =
    { block_hash:        Digest.t          [@key "blockHash"]
    ; block_number:      Revision.t option [@key "blockNumber"]
    ; from:              Address.t         [@key "from"]
    ; gas:               TokenAmount.t     [@key "gas"]
    ; gas_price:         TokenAmount.t     [@key "gasPrice"]
    ; hash:              Digest.t          [@key "hash"]
    ; input:             Yojsoning.Bytes.t [@key "input"]
    ; nonce:             Nonce.t           [@key "nonce"]
    ; to_:               Address.t         [@key "to"]
    ; transaction_index: Revision.t option [@key "transactionIndex"]
    ; value:             TokenAmount.t     [@key "value"]
    } [@@deriving yojson {strict=false; exn=true}]

  type t =
    { pending: (Address.t * (Nonce.t * entry list) list) list
    ; queued:  (Address.t * (Nonce.t * entry list) list) list
    }
end

val txpool_content :
  ?timeout:float -> ?log:bool
  -> unit -> TxPoolContent.t Lwt_exn.t
