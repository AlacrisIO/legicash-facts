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
    { facilitator: Address.t
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
        (fun { facilitator
             ; requester
             ; requester_revision
             ; confirmed_main_chain_state_digest
             ; confirmed_main_chain_state_revision
             ; confirmed_side_chain_state_digest
             ; confirmed_side_chain_state_revision
             ; validity_within } ->
          facilitator, requester, requester_revision,
          confirmed_main_chain_state_digest, confirmed_main_chain_state_revision,
          confirmed_side_chain_state_digest, confirmed_side_chain_state_revision,
          validity_within)
        (fun facilitator requester requester_revision
          confirmed_main_chain_state_digest confirmed_main_chain_state_revision
          confirmed_side_chain_state_digest confirmed_side_chain_state_revision
          validity_within ->
          { facilitator
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
    | StateUpdate
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
    let marshaling =
      { marshal=(fun _buffer _ -> ())
      ; unmarshal=(fun start _bytes -> StateUpdate, start) }
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
  (** Default (empty) state for a new facilitator *)
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
  type t = { facilitator_revision: Revision.t
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
           (fun { facilitator_revision
                ; spending_limit
                ; accounts
                ; transactions
                ; main_chain_transactions_posted } ->
             (facilitator_revision, spending_limit, accounts, transactions, main_chain_transactions_posted))
           (fun facilitator_revision spending_limit accounts transactions main_chain_transactions_posted ->
              { facilitator_revision
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
    { facilitator_revision= Revision.zero
    ; spending_limit= TokenAmount.zero
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= DigestSet.empty }
end

module SignedState = Signed(State)

module FacilitatorFeeSchedule = struct
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

exception Facilitator_not_found of string

module FacilitatorState = struct
  [@warning "-39"]
  type t = { keypair: Keypair.t
           ; committed: State.t signed
           ; current: State.t
           ; fee_schedule: FacilitatorFeeSchedule.t }
  [@@deriving lens { prefix=true}, yojson]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { keypair; committed; current ; fee_schedule } ->
           keypair.address, committed, current, fee_schedule)
        (fun address committed current fee_schedule ->
           { keypair= keypair_of_address address; committed; current ; fee_schedule })
        Address.marshaling (signed_marshaling State.marshaling)
        State.marshaling FacilitatorFeeSchedule.marshaling
    let walk_dependencies _methods context {committed; current} =
      walk_dependency SignedState.dependency_walking context committed
      >>= fun () -> walk_dependency State.dependency_walking context current
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  (** TODO: somehow only save the current/committed state and the fee schedule *)
  let facilitator_state_key facilitator_address =
    "LCFS0001" ^ (Address.to_big_endian_bits facilitator_address)
  let save facilitator_state =
    save facilitator_state (* <-- use inherited binding *)
    >>= fun () ->
    let address = facilitator_state.keypair.address in
    let key = facilitator_state_key address in
    Db.put key (Digest.to_big_endian_bits (digest facilitator_state))
  let load facilitator_address =
    facilitator_address |> facilitator_state_key |> Db.get
    |> (function
      | Some x -> x
      | None -> raise (Facilitator_not_found
                         (Printf.sprintf "Facilitator %s not found in the database"
                            (Address.to_0x_string facilitator_address))))
    |> Digest.unmarshal_string |> db_value_of_digest unmarshal_string
end

module TransactionCommitment = struct
  [@warning "-39"]
  type t =
    { transaction: Transaction.t
    ; tx_proof: TransactionMap.Proof.t
    ; facilitator_revision: Revision.t
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
             ; facilitator_revision
             ; spending_limit
             ; accounts
             ; main_chain_transactions_posted
             ; signature } ->
          transaction, tx_proof, facilitator_revision, spending_limit,
          accounts, main_chain_transactions_posted, signature)
        (fun transaction tx_proof facilitator_revision spending_limit
          accounts main_chain_transactions_posted signature ->
          { transaction
          ; tx_proof
          ; facilitator_revision
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

exception No_facilitator_yet

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

module Test = struct

  open Signing.Test

  (* a sample facilitator state *)

  let trent_fee_schedule =
    FacilitatorFeeSchedule.
      { deposit_fee= TokenAmount.of_int 5
      ; withdrawal_fee= TokenAmount.of_int 5
      ; per_account_limit= TokenAmount.of_int 20000
      ; fee_per_billion= TokenAmount.of_int 42 }

  let confirmed_trent_state =
    State.{ facilitator_revision= Revision.of_int 0
          ; spending_limit= TokenAmount.of_int 1000000
          ; accounts= AccountMap.empty
          ; transactions= TransactionMap.empty
          ; main_chain_transactions_posted= DigestSet.empty }

  let trent_state =
    let open FacilitatorState in
    { keypair= trent_keys
    ; committed= SignedState.make trent_keys confirmed_trent_state
    ; current= confirmed_trent_state
    ; fee_schedule= trent_fee_schedule }

  let%test "db-save-retrieve" =
    (* test whether retrieving a saved facilitator state yields the same state
       here, the account and confirmation maps are empty, so it doesn't really
       exercise the node-by-node persistence machinery
       in Side_chain_action.Test, the "deposit_and_payment_valid" test does
       a save and retrieval with nonempty such maps
    *)
    Db.run ~db_name:"alacris-server"
      (fun () ->
         FacilitatorState.save trent_state
         >>= Db.commit
         >>= (fun () ->
           let retrieved_state = FacilitatorState.load trent_address in
           Lwt.return (FacilitatorState.to_yojson_string retrieved_state
                       = FacilitatorState.to_yojson_string trent_state)))
end
