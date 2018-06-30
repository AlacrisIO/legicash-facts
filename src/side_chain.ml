(* Types for LegiCash Facilitator side-chains *)
(* NB: Comments are in the .mli file *)
open Legibase
open Action
open Crypto
open Trie

module TokenAmount = Main_chain.TokenAmount

type fraud_proof

type knowledge_stage = Unknown | Pending | Confirmed | Rejected

type memo = string option

type invoice = {recipient: Address.t; amount: TokenAmount.t; memo: memo} [@@deriving lens]

type payment_details =
  {payment_invoice: invoice; payment_fee: TokenAmount.t; payment_expedited: bool}
[@@deriving lens]

type deposit_details =
  { deposit_amount: TokenAmount.t
  ; deposit_fee: TokenAmount.t
  ; main_chain_deposit_signed: Main_chain.transaction_signed
  ; main_chain_deposit_confirmation: Main_chain.confirmation
  ; deposit_expedited: bool }
[@@deriving lens]

type withdrawal_details =
  {withdrawal_amount: TokenAmount.t; withdrawal_fee: TokenAmount.t}
[@@deriving lens]

type operation =
  | Deposit of deposit_details
  | Payment of payment_details
  | Withdrawal of withdrawal_details

module RxHeader = struct
  type t =
    { facilitator: Address.t
    ; requester: Address.t
    ; requester_revision: Revision.t
    ; confirmed_main_chain_state_digest: Main_chain.state digest
    ; confirmed_main_chain_state_revision: Revision.t
    ; confirmed_side_chain_state_digest: Digest.t
    ; confirmed_side_chain_state_revision: Revision.t
    ; validity_within: Duration.t }
  [@@deriving lens]
end

module Request = struct
  type t = {rx_header: RxHeader.t; operation: operation} [@@deriving lens]
end

module TxHeader = struct
  type t = {tx_revision: Revision.t; updated_limit: TokenAmount.t} [@@deriving lens]
end

module Confirmation = struct
  type t = {tx_header: TxHeader.t; signed_request: Request.t signed} [@@deriving lens]
  let digest = Digest.make
end

module AccountState = struct
  type t = {balance: TokenAmount.t; account_revision: Revision.t} [@@deriving lens]
  let digest account_state =
    let open Ethereum_rlp in
    let balance_item = RlpItem (TokenAmount.to_string account_state.balance) in
    let revision_item = RlpItem (Revision.to_string account_state.account_revision) in
    let rlp_items = RlpItems [balance_item; revision_item] in
    let encoded = encode rlp_items in
    let hashed = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) (to_string encoded) in
    Digest.of_bits hashed
end

module ConfirmationMap = MerkleTrie (Revision) (Confirmation)

module AccountMap = MerkleTrie (Address) (AccountState)

module State = struct
  type t = { previous_main_chain_state: Main_chain.state digest
           ; previous_side_chain_state: t digest (* state previously posted on the above *)
           ; facilitator_revision: Revision.t
           ; spending_limit: TokenAmount.t
           (* expedited limit still unspent since confirmation. TODO: find a good way to update it back up when things get confirmed *)
           ; bond_posted: TokenAmount.t
           ; accounts: AccountMap.t
           ; operations: ConfirmationMap.t
           (* TODO: it's not an AddressMap, it's a RevisionMap --- a verifiable vector of operations *)
           ; main_chain_transactions_posted: DigestSet.t }
  [@@deriving lens]
end

type episteme =
  { request: Request.t signed
  ; confirmation_option: Confirmation.t signed option
  ; main_chain_confirmation_option: Main_chain.confirmation option }
[@@deriving lens]

module UserAccountStatePerFacilitator = struct
  type t =
    { facilitator_validity:
        knowledge_stage
    (* do we know the facilitator to be a liar? If so, Rejected. Or should it be just a bool? *)
    ; confirmed_state: AccountState.t
    ; pending_operations: episteme list }
  [@@deriving lens]
  let digest = Digest.make
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

type court_clerk_confirmation = {clerk: public_key; signature: State.t signature} [@@deriving lens]

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

exception Invalid_operation of operation

let one_second = Duration.of_int 1000000000

let challenge_duration = Duration.mul one_second (Duration.of_int 7200)
