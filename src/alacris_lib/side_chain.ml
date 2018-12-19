open Lwt.Infix

open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Tag
open Persisting
open Types
open Signing
open Merkle_trie

open Legilogic_ethereum
open Side_chain_server_config
module TokenAmount = Ethereum_chain.TokenAmount

module Invoice = struct
  [@warning "-39"]
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: string}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling3
        (fun {recipient; amount; memo} -> recipient, amount, memo)
        (fun recipient amount memo -> {recipient; amount; memo})
        Address.marshaling TokenAmount.marshaling String63.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module UserOperation = struct
  [@warning "-39"]

  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit: Ethereum_chain.Transaction.t
    ; main_chain_deposit_confirmation: Ethereum_chain.Confirmation.t }
  [@@deriving lens, yojson]

  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens, yojson]

  type withdrawal_details =
    {withdrawal_amount: TokenAmount.t; withdrawal_fee: TokenAmount.t}
  [@@deriving lens, yojson]

  type t =
    | Deposit of deposit_details
    | Payment of payment_details
    | Withdrawal of withdrawal_details
  [@@deriving yojson]

  let operation_tag = function
    | Deposit _ -> Side_chain_tag.deposit
    | Payment _ -> Side_chain_tag.payment
    | Withdrawal _ -> Side_chain_tag.withdrawal
  module PrePersistable = struct
    type nonrec t = t
    let case_table =
      [| marshaling4
           (function
             | Deposit
                 { deposit_amount; deposit_fee; main_chain_deposit; main_chain_deposit_confirmation } ->
               (deposit_amount, deposit_fee, main_chain_deposit, main_chain_deposit_confirmation)
             | _ -> bottom ())
           (fun deposit_amount deposit_fee main_chain_deposit
             main_chain_deposit_confirmation ->
             Deposit
               { deposit_amount; deposit_fee; main_chain_deposit; main_chain_deposit_confirmation })
           TokenAmount.marshaling
           TokenAmount.marshaling
           Ethereum_chain.Transaction.marshaling
           Ethereum_chain.Confirmation.marshaling
       ; marshaling3
           (function
             | Payment {payment_invoice; payment_fee; payment_expedited} ->
               (payment_invoice, payment_fee, payment_expedited)
             | _ -> bottom ())
           (fun payment_invoice payment_fee payment_expedited ->
              Payment {payment_invoice; payment_fee; payment_expedited})
           Invoice.marshaling TokenAmount.marshaling bool_marshaling
       ; marshaling2
           (function
             | Withdrawal {withdrawal_amount; withdrawal_fee} ->
               (withdrawal_amount, withdrawal_fee)
             | _ -> bottom ())
           (fun withdrawal_amount withdrawal_fee ->
              Withdrawal {withdrawal_amount; withdrawal_fee})
           TokenAmount.marshaling TokenAmount.marshaling |]
    let marshaling =
      marshaling_cases operation_tag Side_chain_tag.base_operation case_table
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module RxHeader = struct
  [@warning "-39"]
  type t =
    { operator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: Digest.t
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: Digest.t
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens {prefix=true}, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling8
        (fun { operator
             ; requester
             ; requester_revision
             ; confirmed_main_chain_state_digest
             ; confirmed_main_chain_state_revision
             ; confirmed_side_chain_state_digest
             ; confirmed_side_chain_state_revision
             ; validity_within } ->
          operator, requester, requester_revision,
          confirmed_main_chain_state_digest, confirmed_main_chain_state_revision,
          confirmed_side_chain_state_digest, confirmed_side_chain_state_revision,
          validity_within)
        (fun operator requester requester_revision
          confirmed_main_chain_state_digest confirmed_main_chain_state_revision
          confirmed_side_chain_state_digest confirmed_side_chain_state_revision
          validity_within ->
          { operator
          ; requester
          ; requester_revision
          ; confirmed_main_chain_state_digest
          ; confirmed_main_chain_state_revision
          ; confirmed_side_chain_state_digest
          ; confirmed_side_chain_state_revision
          ; validity_within })
        Address.marshaling Address.marshaling Revision.marshaling
        Digest.marshaling Revision.marshaling Digest.marshaling Revision.marshaling
        Duration.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module UserTransactionRequest = struct
  [@warning "-39"]
  type t = {rx_header: RxHeader.t; operation: UserOperation.t}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {rx_header; operation} -> rx_header, operation)
        (fun rx_header operation -> {rx_header; operation})
        RxHeader.marshaling UserOperation.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module SignedUserTransactionRequest = Signed(UserTransactionRequest)

module TxHeader = struct
  [@warning "-39"]
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {tx_revision; updated_limit} -> tx_revision, updated_limit)
        (fun tx_revision updated_limit -> {tx_revision; updated_limit})
        Revision.marshaling TokenAmount.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module AdminTransactionRequest = struct
  [@warning "-39"]
  type t =
    | StateUpdate of Revision.t * Digest.t
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
    let marshaling =
      marshaling2
        (function StateUpdate (r, d) -> r, d)
        (fun r d -> StateUpdate (r, d))
        Revision.marshaling Digest.marshaling
  end
  include (TrivialPersistable(P) : PersistableS with type t := t)
end

module UserQueryRequest = struct
  [@warning "-39"]
  type t =
    | Get_account_balance of {address: Address.t}
    | Get_account_balances
    | Get_account_state of {address: Address.t}
    | Get_account_status of {address: Address.t}
    | Get_recent_transactions of {address: Address.t; count: Revision.t option}
    | Get_proof of {tx_revision: Revision.t}
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module AdminQueryRequest = struct
  [@warning "-39"]
  type t =
    | Get_all_balances
    | Get_transaction_rate
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module TransactionRequest = struct
  [@warning "-39"]
  type t =
    [ `UserTransaction of UserTransactionRequest.t signed
    | `AdminTransaction of AdminTransactionRequest.t ]
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
    let marshal buffer = function
      | `UserTransaction utrs ->
        Tag.marshal buffer Side_chain_tag.user_transaction;
        marshal_signed UserTransactionRequest.marshal buffer utrs
      | `AdminTransaction x ->
        Tag.marshal buffer Side_chain_tag.admin_transaction;
        AdminTransactionRequest.marshal buffer x
    let unmarshal start bytes =
      let (tag, p) = Tag.unmarshal start bytes in
      if tag = Side_chain_tag.user_transaction then
        let (utrs, p) = unmarshal_signed UserTransactionRequest.unmarshal p bytes in
        `UserTransaction utrs, p
      else if tag = Side_chain_tag.admin_transaction then
        let (x, p) = AdminTransactionRequest.unmarshal p bytes in
        `AdminTransaction x, p
      else raise (Unmarshaling_error ("bad tag", start, bytes))
    let marshaling = {marshal;unmarshal}
  end
  include (TrivialPersistable(P) : PersistableS with type t := t)
  let signed_request = function
    | `UserTransaction x -> x
    | _ -> bork "Not a user transaction"
  let request x = (x |> signed_request).payload
end

module Query = struct
  [@warning "-39"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `AdminQuery of AdminQueryRequest.t ]
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module UserRequest = struct
  [@warning "-39"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed ]
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module AdminRequest = struct
  [@warning "-39"]
  type t =
    [ `AdminQuery of UserQueryRequest.t
    | `AdminTransaction of AdminTransactionRequest.t ]
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module ExternalRequest = struct
  [@warning "-39"]
  type t =
    [ `UserQuery of UserQueryRequest.t
    | `UserTransaction of UserTransactionRequest.t signed
    | `AdminQuery of AdminQueryRequest.t ]
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable(P) : PersistableS with type t := t)
end

module Transaction = struct
  [@warning "-39"]
  type t = {tx_header: TxHeader.t; tx_request: TransactionRequest.t}
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {tx_header; tx_request} -> tx_header, tx_request)
        (fun tx_header tx_request -> {tx_header; tx_request})
        TxHeader.marshaling TransactionRequest.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module AccountState = struct
  [@warning "-39"]
  type t = {balance: TokenAmount.t; account_revision: Revision.t}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {balance; account_revision} -> balance, account_revision)
        (fun balance account_revision -> {balance; account_revision})
        TokenAmount.marshaling Revision.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  (** Default (empty) state for a new operator *)
  let empty = {balance= TokenAmount.zero; account_revision= Revision.zero}
end


(* Module for Maps from Side_chain.TxHeader.tx_revision to (unsigned) Transaction *)
module TransactionMap = struct
  include MerkleTrie (Revision) (Transaction)
end

module AccountMap = struct
  include MerkleTrie (Address) (AccountState)
end

module State = struct
  [@warning "-39"]
  type t = { operator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           ; accounts: AccountMap.t
           ; transactions: TransactionMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens { prefix=true }, yojson]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      (* TODO: add a big prefix for the signing? *)
      marshaling_tagged Side_chain_tag.state
        (marshaling5
           (fun { operator_revision
                ; spending_limit
                ; accounts
                ; transactions
                ; main_chain_transactions_posted } ->
             (operator_revision, spending_limit, accounts, transactions, main_chain_transactions_posted))
           (fun operator_revision spending_limit accounts transactions main_chain_transactions_posted ->
              { operator_revision
              ; spending_limit
              ; accounts
              ; transactions
              ; main_chain_transactions_posted })
           Revision.marshaling TokenAmount.marshaling
           AccountMap.marshaling TransactionMap.marshaling DigestSet.marshaling)
    let walk_dependencies _methods context {accounts; transactions; main_chain_transactions_posted} =
      walk_dependency AccountMap.dependency_walking context accounts
      >>= (fun () -> walk_dependency TransactionMap.dependency_walking context transactions)
      >>= (fun () -> walk_dependency DigestSet.dependency_walking context main_chain_transactions_posted)
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { operator_revision= Revision.zero
    ; spending_limit= TokenAmount.zero
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= DigestSet.empty }
end

module SignedState = Signed(State)

module OperatorFeeSchedule = struct
  [@warning "-39"]
  type t =
    { deposit_fee: TokenAmount.t
    ; withdrawal_fee: TokenAmount.t
    ; per_account_limit: TokenAmount.t
    ; fee_per_billion: TokenAmount.t }
  [@@deriving lens { prefix=true}, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { deposit_fee ; withdrawal_fee ; per_account_limit ; fee_per_billion } ->
           deposit_fee, withdrawal_fee, per_account_limit, fee_per_billion)
        (fun deposit_fee withdrawal_fee per_account_limit fee_per_billion ->
           { deposit_fee ; withdrawal_fee ; per_account_limit ; fee_per_billion })
        TokenAmount.marshaling TokenAmount.marshaling TokenAmount.marshaling TokenAmount.marshaling
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module TransactionCommitment = struct
  [@warning "-39"]
  type t =
    { transaction: Transaction.t
    ; tx_proof: TransactionMap.Proof.t
    ; operator_revision: Revision.t
    ; spending_limit: TokenAmount.t
    ; accounts: Digest.t
    ; main_chain_transactions_posted: Digest.t
    ; signature: Signature.t }
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling7
        (fun { transaction
             ; tx_proof
             ; operator_revision
             ; spending_limit
             ; accounts
             ; main_chain_transactions_posted
             ; signature } ->
          transaction, tx_proof, operator_revision, spending_limit,
          accounts, main_chain_transactions_posted, signature)
        (fun transaction tx_proof operator_revision spending_limit
          accounts main_chain_transactions_posted signature ->
          { transaction
          ; tx_proof
          ; operator_revision
          ; spending_limit
          ; accounts
          ; main_chain_transactions_posted
          ; signature })
        Transaction.marshaling TransactionMap.Proof.marshaling Revision.marshaling
        TokenAmount.marshaling Digest.marshaling Digest.marshaling Signature.marshaling
    let yojsoning = {to_yojson;of_yojson}
  end
  include (TrivialPersistable (PrePersistable) : PersistableS with type t := t)
end

module Confirmation = TransactionCommitment

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

type update = { current_state: Digest.t (* State.t *)
              ; availability_proof: court_clerk_confirmation list}

exception No_operator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

let one_second = Duration.of_int 1000000000

let challenge_duration = Duration.mul one_second (Duration.of_int 7200)

let one_billion_tokens = TokenAmount.of_int 1000000000

module SignaturePrefix = struct
  include Address
  let state_update = Address.of_hex_string "7E91CA540000000057A7E009DA7E000000000001"
end

(* 1 ether = 1e18 wei = 242 USD (as of 2018-09-23), with gas price of ~4.1 gwei *)
(*                       
let initial_fee_schedule =
  OperatorFeeSchedule.
    { deposit_fee= TokenAmount.of_string "10000000000000" (* 1e13 wei = 1e-5 ether ~= .24 cent *)
    ; withdrawal_fee= TokenAmount.of_string "10000000000000" (* 1e13 wei = 1e-5 ether ~= .24 cent *)
    ; per_account_limit= TokenAmount.of_string "10000000000000000000" (* 1e19 wei = 10 ether ~= 2420 USD *)
    ; fee_per_billion= TokenAmount.of_string "1000000" } (* 1e6/1e9 = 1e-3 = .1% *)
 *)
                       
let initial_fee_schedule =
  OperatorFeeSchedule.
    { deposit_fee = Side_chain_server_config.deposit_fee_v
    ; withdrawal_fee = Side_chain_server_config.withdrawal_fee_v
    ; per_account_limit = Side_chain_server_config.per_account_limit_v
    ; fee_per_billion = Side_chain_server_config.fee_per_billion_v
    }
