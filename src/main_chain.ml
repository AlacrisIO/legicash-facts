(* See documentation in main_chain.mli *)

open Action
open Marshaling
open Crypto
open Merkle_trie

module TokenAmount = UInt64
module Nonce = UInt64
module ContractAddress = Address

module AccountMap = MerkleTrie (Address) (TokenAmount)

module State = struct
  (* TODO: have an actual model of the Ethereum main chain *)
  type t = {revision: Revision.t; accounts: AccountMap.t}
  [@@deriving lens { prefix=true } ]
  module Marshalable = struct
    type nonrec t = t
    let marshal b {revision; accounts=_} =
      Revision.marshal b revision
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module Confirmation = struct
  type t = { transaction_hash: Digest.t
           ; transaction_index: Unsigned.UInt64.t
           ; block_number: Revision.t
           ; block_hash: Digest.t }
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : (DigestibleS with type t := t))
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
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : (DigestibleS with type t := t))
end

module Operation = struct
  type t =
    | TransferTokens of Address.t
    | CreateContract of Bytes.t
    | CallFunction of Address.t * Bytes.t
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : (DigestibleS with type t := t))
end

(* contract, data *)
(** Transaction (to be) posted to the main chain (i.e. Ethereum) *)
module Transaction = struct
  type t = {tx_header: TxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true } ]
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : (DigestibleS with type t := t))
end

module TransactionSigned = struct
  type t = Transaction.t signed
  module Marshalable = OCamlMarshaling (struct type nonrec t = t end)
  include (DigestibleOfMarshalable (Marshalable) : (DigestibleS with type t := t))
end

type user_state =
  { keypair: Keypair.t
  ; confirmed_state: State.t digest
  ; confirmed_balance: TokenAmount.t
  ; pending_transactions: TransactionSigned.t list
  ; nonce: Nonce.t }
[@@deriving lens]

type ('input, 'output) user_action = ('input, 'output, user_state) action
(** type of synchronous actions on the main chain user state *)

type ('input, 'output) user_async_action = ('input, 'output, user_state) async_action
(** type of asynchronous actions on the main chain user state *)

module TransactionDigestSet = DigestSet
