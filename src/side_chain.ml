(* Types for LegiCash Facilitator side-chains *)
(* NB: Comments are in the .mli file *)
open Action
open Marshaling
open Crypto
open Trie

module TokenAmount = Main_chain.TokenAmount

type fraud_proof

module KnowledgeStage = struct
  type t = Unknown | Pending | Confirmed | Rejected
  module Marshalable = struct
    type tt = t
    type t = tt
    let to_char = function Unknown -> 'U' | Pending -> 'P' | Confirmed -> 'C' | Rejected -> 'R'
    let marshal b x = Buffer.add_char b (to_char x)
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

type memo = string option

module Invoice = struct
  type t = {recipient: Address.t; amount: TokenAmount.t; memo: memo} [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal = marshal_any
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module Operation = struct
  type payment_details =
    {payment_invoice: Invoice.t; payment_fee: TokenAmount.t; payment_expedited: bool}
  [@@deriving lens]

  type deposit_details =
    { deposit_amount: TokenAmount.t
    ; deposit_fee: TokenAmount.t
    ; main_chain_deposit_signed: Main_chain.TransactionSigned.t
    ; main_chain_deposit_confirmation: Main_chain.Confirmation.t
    ; deposit_expedited: bool }
  [@@deriving lens]

  type withdrawal_details =
    {withdrawal_amount: TokenAmount.t; withdrawal_fee: TokenAmount.t}
  [@@deriving lens]

  type t =
    | Deposit of deposit_details
    | Payment of payment_details
    | Withdrawal of withdrawal_details

  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal b = function
      | Deposit { deposit_amount
                ; deposit_fee
                ; main_chain_deposit_signed
                ; main_chain_deposit_confirmation
                ; deposit_expedited } ->
        Buffer.add_char b 'D' ;
        TokenAmount.marshal b deposit_amount ;
        TokenAmount.marshal b deposit_fee ;
        marshal_signed Main_chain.Transaction.marshal b main_chain_deposit_signed ;
        Main_chain.Confirmation.marshal b main_chain_deposit_confirmation ;
        marshal_bool b deposit_expedited
      | Payment {payment_invoice; payment_fee; payment_expedited} ->
        Buffer.add_char b 'P' ;
        Invoice.marshal b payment_invoice;
        TokenAmount.marshal b payment_fee;
        marshal_bool b payment_expedited
      | Withdrawal {withdrawal_amount; withdrawal_fee} ->
        Buffer.add_char b 'W' ;
        TokenAmount.marshal b withdrawal_amount;
        TokenAmount.marshal b withdrawal_fee
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module RxHeader = struct
  type t =
    { facilitator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: Main_chain.State.t digest
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: Digest.t
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens]
  let marshal b { facilitator
                ; requester
                ; requester_revision
                ; confirmed_main_chain_state_digest
                ; confirmed_main_chain_state_revision
                ; confirmed_side_chain_state_digest
                ; confirmed_side_chain_state_revision
                ; validity_within } =
    Address.marshal b facilitator ;
    Address.marshal b requester ;
    Revision.marshal b requester_revision ;
    Digest.marshal b confirmed_main_chain_state_digest ;
    Revision.marshal b confirmed_main_chain_state_revision ;
    Digest.marshal b confirmed_side_chain_state_digest ;
    Revision.marshal b confirmed_side_chain_state_revision ;
    Duration.marshal b validity_within
end

module Request = struct
  type t = {rx_header: RxHeader.t; operation: Operation.t} [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal b {rx_header; operation} =
      RxHeader.marshal b rx_header ; Operation.marshal b operation
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module TxHeader = struct
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t} [@@deriving lens]
  let marshal b {tx_revision; updated_limit} =
    Revision.marshal b tx_revision ; TokenAmount.marshal b updated_limit
end

module Confirmation = struct
  type t = {tx_header: TxHeader.t; signed_request: Request.t signed} [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal b {tx_header; signed_request} =
      TxHeader.marshal b tx_header ; marshal_signed Request.marshal b signed_request
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module AccountState = struct
  type t = {balance: TokenAmount.t; account_revision: Revision.t} [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal b {balance; account_revision} =
      TokenAmount.marshal b balance ; Revision.marshal b account_revision
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module ConfirmationMap = MerkleTrie (Revision) (Confirmation)

module AccountMap = MerkleTrie (Address) (AccountState)

module State = struct
  type t = { previous_main_chain_state: Main_chain.State.t digest
           ; previous_side_chain_state: t digest
           ; facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           ; bond_posted: TokenAmount.t
           ; accounts: AccountMap.t
           ; operations: ConfirmationMap.t
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal = marshal_any
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

type episteme =
  { request: Request.t signed
  ; confirmation_option: Confirmation.t signed option
  ; main_chain_confirmation_option: Main_chain.Confirmation.t option }
[@@deriving lens]

module UserAccountStatePerFacilitator = struct
  type t =
    { facilitator_validity: KnowledgeStage.t
    ; confirmed_state: AccountState.t
    ; pending_operations: episteme list }
  [@@deriving lens]
  module Marshalable = struct
    type tt = t
    type t = tt
    let marshal b { facilitator_validity
                  ; confirmed_state
                  ; pending_operations=_ } =
      KnowledgeStage.marshal b facilitator_validity ;
      AccountState.marshal b confirmed_state ;
      () (* TODO: handle the list pending_operation *)
    let unmarshal = unmarshal_not_implemented
  end
  include (DigestibleOfMarshalable (Marshalable) : DigestibleS with type t := t)
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountStatePerFacilitator)

type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: UserAccountStateMap.t }
[@@deriving lens]

type ('input, 'action) user_action = ('input, 'action, user_state) action

type verifier_state

type ('input, 'output) verifier_action = ('input, 'output, verifier_state) action

type facilitator_fee_schedule =
  { deposit_fee: TokenAmount.t
  ; withdrawal_fee: TokenAmount.t
  ; per_account_limit: TokenAmount.t
  ; fee_per_billion: TokenAmount.t }
[@@deriving lens]

type facilitator_state =
  { keypair: Keypair.t
  ; previous: State.t option
  ; current: State.t
  ; fee_schedule: facilitator_fee_schedule }
[@@deriving lens]

type ('input, 'output) facilitator_action = ('input, 'output, facilitator_state) action

type court_clerk_confirmation = {clerk: public_key; signature: signature} [@@deriving lens]

type update = {current_state: State.t digest; availability_proof: court_clerk_confirmation list}

type user_to_user_message

type user_to_facilitator_message

type facilitator_to_user_message

type facilitator_to_facilitator_message

exception No_facilitator_yet

exception Already_open

exception Already_closed

exception Account_closed_or_nonexistent

exception Invalid_confirmation

exception Invalid_operation of Operation.t

let one_second = Duration.of_int 1000000000

let challenge_duration = Duration.mul one_second (Duration.of_int 7200)
