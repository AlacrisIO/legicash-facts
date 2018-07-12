(* See documentation in main_chain.mli *)

open Lib
open Action
open Crypto
open Trie

module TokenAmount = UInt64
module Nonce = UInt64
module ContractAddress = Address

module AccountMap = MerkleTrie (Address) (TokenAmount)

module State = struct
  (* TODO: have an actual model of the Ethereum main chain *)
  type t = {revision: Revision.t; accounts: AccountMap.t} [@@deriving lens]
  module Marshallable = struct
    type tt = t
    type t = tt
    let marshall b {revision; accounts=_} =
      Revision.marshall b revision
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : DigestibleS with type t := t)
end

module Confirmation = struct
  type t = { transaction_hash: Digest.t
           ; transaction_index: Unsigned.UInt64.t
           ; block_number: Revision.t
           ; block_hash: Digest.t }
  module Marshallable : (MarshallableS with type t = t) = struct
    type tt = t
    type t = tt
    let marshall = marshall_any
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : (DigestibleS with type t := t))
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
  [@@deriving lens]
  module Marshallable : (MarshallableS with type t = t) = struct
    type tt = t
    type t = tt
    let marshall = marshall_any
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : (DigestibleS with type t := t))
end

module Operation = struct
  type t =
    | TransferTokens of Address.t
    | CreateContract of Bytes.t
    | CallFunction of Address.t * Bytes.t
  module Marshallable : (MarshallableS with type t = t) = struct
    type tt = t
    type t = tt
    let marshall = marshall_any
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : (DigestibleS with type t := t))
end

(* contract, data *)
(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
module Transaction = struct
  type t = {tx_header: TxHeader.t; operation: Operation.t} [@@deriving lens]
  module Marshallable : (MarshallableS with type t = t) = struct
    type tt = t
    type t = tt
    let marshall = marshall_any
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : (DigestibleS with type t := t))
end

module TransactionSigned = struct
  type t = Transaction.t signed
  module Marshallable : (MarshallableS with type t = t) = struct
    type tt = t
    type t = tt
    let marshall = marshall_any
    let unmarshall = bottom
  end
  include (DigestibleOfMarshallable (Marshallable) : (DigestibleS with type t := t))
end

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: State.t digest
  ; confirmed_balance: TokenAmount.t
  ; pending_transactions: TransactionSigned.t list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action
