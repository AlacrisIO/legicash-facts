(* Types for LegiCash Facilitator side-chains *)
(* NB: Comments are in the .mli file *)
open Lib
open Action
open Yojsoning
open Marshaling
open Crypto
open Db
open Db_types
open Merkle_trie
open Lwt.Infix

module TokenAmount = Main_chain.TokenAmount

module Invoice = struct
  [@warning "-39"]
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: string}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling_tagged Tag.side_chain_invoice
        (marshaling3
           (fun {recipient; amount; memo} -> (recipient, amount, memo))
           (fun recipient amount memo -> {recipient; amount; memo})
           Address.marshaling TokenAmount.marshaling String63.marshaling)
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module Operation = struct
  [@warning "-39"]
  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens, yojson]

  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit_signed: Main_chain.TransactionSigned.t
    ; main_chain_deposit_confirmation: Main_chain.Confirmation.t
    ; deposit_expedited: bool }
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
    | Deposit _ -> Tag.side_chain_deposit
    | Payment _ -> Tag.side_chain_payment
    | Withdrawal _ -> Tag.side_chain_withdrawal

  module PrePersistable = struct
    type nonrec t = t
    let case_table =
      [| marshaling5
           (function
             | Deposit
                 { deposit_amount
                 ; deposit_fee
                 ; main_chain_deposit_signed
                 ; main_chain_deposit_confirmation
                 ; deposit_expedited } ->
               (deposit_amount, deposit_fee, main_chain_deposit_signed,
                main_chain_deposit_confirmation, deposit_expedited)
             | _ -> bottom ())
           (fun deposit_amount deposit_fee main_chain_deposit_signed
             main_chain_deposit_confirmation deposit_expedited ->
             Deposit
               { deposit_amount
               ; deposit_fee
               ; main_chain_deposit_signed
               ; main_chain_deposit_confirmation
               ; deposit_expedited })
           TokenAmount.marshaling
           TokenAmount.marshaling
           Main_chain.TransactionSigned.marshaling
           Main_chain.Confirmation.marshaling
           bool_marshaling
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
      marshaling_cases operation_tag Tag.base_side_chain_operation case_table
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
      marshaling_tagged Tag.side_chain_rx_header
        (marshaling8
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
           Digest.marshaling Revision.marshaling
           Digest.marshaling Revision.marshaling
           Duration.marshaling)
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

module Request = struct
  [@warning "-39"]
  type t = {rx_header: RxHeader.t; operation: Operation.t}
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {rx_header; operation} -> rx_header, operation)
        (fun rx_header operation -> {rx_header; operation})
        RxHeader.marshaling Operation.marshaling
    let yojsoning = {to_yojson;of_yojson}
    let make_persistent = normal_persistent
    let walk_dependencies = no_dependencies
  end
  include (Persistable (PrePersistable) : (PersistableS with type t := t))
end

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

module Confirmation = struct
  [@warning "-39"]
  type t = {tx_header: TxHeader.t; signed_request: Request.t signed}
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling_tagged Tag.side_chain_confirmation
        (marshaling2
           (fun {tx_header; signed_request} -> tx_header, signed_request)
           (fun tx_header signed_request -> {tx_header; signed_request})
           TxHeader.marshaling (marshaling_signed Request.marshaling))
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


(* Module for Maps from Side_chain.TxHeader.tx_revision to (unsigned) Confirmation *)
module ConfirmationMap = struct
  include MerkleTrie (Revision) (Confirmation)
end

module AccountMap = struct
  include MerkleTrie (Address) (AccountState)
end

module State = struct
  [@warning "-39"]
  type t = { previous_main_chain_state: Digest.t
           ; previous_side_chain_state: Digest.t
           ; facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           ; bond_posted: TokenAmount.t
           ; accounts: AccountMap.t
           ; operations: ConfirmationMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens { prefix=true }, yojson]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling_tagged Tag.side_chain_state
        (marshaling8
           (fun { previous_main_chain_state
                ; previous_side_chain_state
                ; facilitator_revision
                ; spending_limit
                ; bond_posted
                ; accounts
                ; operations
                ; main_chain_transactions_posted } ->
             (previous_main_chain_state, previous_side_chain_state, facilitator_revision,
              spending_limit, bond_posted, accounts, operations, main_chain_transactions_posted))
           (fun previous_main_chain_state previous_side_chain_state facilitator_revision
             spending_limit bond_posted accounts operations main_chain_transactions_posted ->
             { previous_main_chain_state
             ; previous_side_chain_state
             ; facilitator_revision
             ; spending_limit
             ; bond_posted
             ; accounts
             ; operations
             ; main_chain_transactions_posted })
           Digest.marshaling Digest.marshaling Revision.marshaling
           TokenAmount.marshaling TokenAmount.marshaling
           AccountMap.marshaling ConfirmationMap.marshaling DigestSet.marshaling)
    let walk_dependencies _methods context {accounts; operations; main_chain_transactions_posted} =
      walk_dependency AccountMap.dependency_walking context accounts
      >>= (fun () -> walk_dependency ConfirmationMap.dependency_walking context operations)
      >>= (fun () -> walk_dependency DigestSet.dependency_walking context main_chain_transactions_posted)
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { previous_main_chain_state= null_digest
    ; previous_side_chain_state= null_digest
    ; facilitator_revision= Revision.zero
    ; spending_limit= TokenAmount.zero
    ; bond_posted= TokenAmount.zero
    ; accounts= AccountMap.empty
    ; operations= ConfirmationMap.empty
    ; main_chain_transactions_posted= DigestSet.empty }
end

type verifier_state

type ('input, 'output) verifier_action = ('input, 'output, verifier_state) action

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

module FacilitatorState = struct
  [@warning "-39"]
  type t = { keypair: Keypair.t
           ; previous: State.t option
           ; current: State.t
           ; fee_schedule: FacilitatorFeeSchedule.t
           }
  [@@deriving lens { prefix=true}, yojson]

  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { keypair ; previous ; current ; fee_schedule } ->
           keypair, previous, current, fee_schedule)
        (fun keypair previous current fee_schedule ->
           { keypair ; previous ; current ; fee_schedule })
        Keypair.marshaling (option_marshaling State.marshaling)
        State.marshaling FacilitatorFeeSchedule.marshaling
    let walk_dependencies _methods context {previous ; current} =
      let walk = walk_dependency State.dependency_walking context in
      Option.iter_lwt walk previous >>= (fun () -> walk current)
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let facilitator_state_key facilitator_address =
    "LCFS0001" ^ (Address.to_big_endian_bits facilitator_address)
  let save facilitator_state =
    save facilitator_state (* <-- use inherited binding *)
    >>= (fun () ->
      let address = facilitator_state.keypair.address in
      let key = facilitator_state_key address in
      put_db key (Digest.to_big_endian_bits (digest facilitator_state)))
  let load facilitator_address =
    facilitator_address |> facilitator_state_key |> get_db |> Option.get |> Digest.unmarshal_string |>
    db_value_of_digest unmarshal_string
end

type ('input, 'output) facilitator_action = ('input, 'output, FacilitatorState.t) action

type ('input, 'output) facilitator_async_action = ('input, 'output, FacilitatorState.t) async_action

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

type update = { current_state: Digest.t (* State.t *)
              ; availability_proof: court_clerk_confirmation list}

exception No_facilitator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

exception Invalid_operation of Operation.t

let one_second = Duration.of_int 1000000000

let challenge_duration = Duration.mul one_second (Duration.of_int 7200)

module Test = struct

  open Keypair.Test

  (* a sample facilitator state *)

  let trent_fee_schedule : FacilitatorFeeSchedule.t =
    { deposit_fee= TokenAmount.of_int 5
    ; withdrawal_fee= TokenAmount.of_int 5
    ; per_account_limit= TokenAmount.of_int 20000
    ; fee_per_billion= TokenAmount.of_int 42 }

  let confirmed_trent_state =
    State.{ previous_main_chain_state= Digest.zero
          ; previous_side_chain_state= Digest.one
          ; facilitator_revision= Revision.of_int 0
          ; spending_limit= TokenAmount.of_int 1000000
          ; bond_posted= TokenAmount.of_int 5000000
          ; accounts= AccountMap.empty
          ; operations= ConfirmationMap.empty
          ; main_chain_transactions_posted= DigestSet.empty }

  let trent_state =
    let open FacilitatorState in
    { keypair= trent_keys
    ; previous= None
    ; current= confirmed_trent_state
    ; fee_schedule= trent_fee_schedule }

  let%test "db-save-retrieve" =
    (* test whether retrieving a saved facilitator state yields the same state
       here, the account and confirmation maps are empty, so it doesn't really
       exercise the node-by-node persistence machinery
       in Side_chain_action.Test, the "deposit_and_payment_valid" test does
       a save and retrieval with nonempty such maps
    *)
    Db.run ~db_name:Legibase.db_name
      (fun () ->
         FacilitatorState.save trent_state
         >>= commit
         >>= (fun () ->
           let retrieved_state = FacilitatorState.load trent_address in
           Lwt.return (FacilitatorState.to_yojson_string retrieved_state
                       = FacilitatorState.to_yojson_string trent_state)))
end
