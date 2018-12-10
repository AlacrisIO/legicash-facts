open Legilogic_lib
open Yojsoning
open Marshaling
open Signing
open Persisting
open Merkle_trie
open Types

(* The Ethereum notion of Quantity, as used everywhere in the Yellow Paper *)
module Quantity = UInt256

(* The number of tokens will probably not go (much) over 87 bits (100M ethers, each 1e18 wei),
   so UInt96 should be more than enough.
   But the binary API specifies a maximum of 256 bits for value transfers.
   So we make it 256 bits, at least for now.
   Note that we could make it smaller just for our side-chain and contracts, and that
   with Ethereum mostly uses variable-length encoding RLP for hashing and signing token amounts,
   so that disk space isn't wasted too much for them.
   Maybe we should also choose a representation that isn't too bad, maybe adopt RLP, too,
   and then it's OK to use 256 in memory.
*)
module TokenAmount = UInt256

(* The API maximum is 256 bits, but it'll most probably fit in 32 bits, definitely in 64 bits. *)
module Nonce = Revision

module AccountMap = MerkleTrie (Address) (TokenAmount)

(* The address of a contract.
   For Ethereum, that's the same type as a plain old address,
   though it is stripped from the digest of the creator address plus their creation nonce,
   rather than of a Secp256k1 public key.
   On other blockchains, it could be different (do we care?)

   module ContractAddress = Address
*)


module State = struct
  (* TODO: have an actual model of the Ethereum main chain, marshaled the Ethereum way. *)
  type t = {revision: Revision.t; accounts: AccountMap.t}
  [@@deriving lens { prefix=true }]
  module PrePersistable = struct
    module M = struct
      type nonrec t = t
      let marshaling =
        marshaling2
          (fun {revision;accounts} -> (revision, accounts))
          (fun revision accounts -> {revision; accounts})
          Revision.marshaling AccountMap.marshaling
    end
    include YojsonableOfMarshalable(Marshalable(M))
    let make_persistent = normal_persistent
    let walk_dependencies =
      one_dependency (fun x -> x.accounts) AccountMap.dependency_walking
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module Confirmation = struct
  type t = { transaction_hash: Digest.t
           ; transaction_index: Revision.t
           ; block_number: Revision.t
           ; block_hash: Digest.t }
  [@@deriving yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun {transaction_hash; transaction_index; block_number; block_hash} ->
           (transaction_hash, transaction_index, block_number, block_hash))
        (fun transaction_hash transaction_index block_number block_hash ->
           {transaction_hash; transaction_index; block_number; block_hash})
        Digest.marshaling Revision.marshaling Revision.marshaling Digest.marshaling
    let yojsoning = {to_yojson; of_yojson}
  end
  include (TrivialPersistable (PrePersistable) : (PersistableS with type t := t))
end

let genesis_state = State.{revision= Revision.zero; accounts= AccountMap.empty}

(* TODO: use whichever way is used to compute on-chain hashes for marshaling.
   Is that RLP? Find out! *)
module TxHeader = struct
  [@warning "-39"]
  type t = { sender: Address.t
           ; nonce: Nonce.t
           ; gas_price: TokenAmount.t
           ; gas_limit: TokenAmount.t
           ; value: TokenAmount.t }
  [@@deriving lens { prefix=true }, yojson]
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
    let yojsoning = {to_yojson;of_yojson}
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

(* TODO: use whichever way is used to compute on-chain hashes for marshaling.
   Is that RLP? Find out! *)
module Operation = struct
  [@warning "-39"]
  type t =
    | TransferTokens of Address.t
    | CreateContract of Yojsoning.Bytes.t
    | CallFunction of Address.t * Yojsoning.Bytes.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module PreTransaction = struct
  [@warning "-39"]
  type t = {operation: Operation.t; value: TokenAmount.t; gas_limit: TokenAmount.t}
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
end

(* TODO: use whichever way is used to compute on-chain hashes for marshaling.
   Is that RLP? Find out! *)
(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
module Transaction = struct
  [@warning "-39"]
  type t = {tx_header: TxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true }, yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
  let pre_transaction {tx_header={value;gas_limit}; operation} =
    PreTransaction.{operation;value;gas_limit}
end

(** TODO: have an actual confirmation that a contract could check.
    For Ethereum, we might check the transaction hashes match, or
    perform a Merkle proof using the transactionsRoot in the given block
    NOTE: We should not compute the transaction hash ourself.
    --- 1 : When we post, a hash is computed and returned.
        (It is in TransactionReceipt.transaction_hash) returned by eth_get_transaction_receipt.
    --- 2 : When we query the client it returns a confirmation object telling whether the 
            transaction completed successfully.
    ---Therefore, this code below will never work.
*)
let is_confirmation_valid (_confirmation: Confirmation.t) (_transaction: Transaction.t) : bool = true


                   
module TransactionDigestSet = DigestSet
