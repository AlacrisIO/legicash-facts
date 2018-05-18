(* Types for LegiCash Facilitator side-chains *)
(* NB: Comments are in the .mli file *)
open Legibase
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

(** an operation on a facilitator side-chain *)
type operation =
  | Deposit of deposit_details
  | Payment of payment_details
  | Withdrawal of withdrawal_details

type rx_header =
  { facilitator: Address.t
  ; requester: Address.t
  ; requester_revision: Revision.t
  ; confirmed_main_chain_state_digest: Main_chain.state digest
  ; confirmed_main_chain_state_revision: Revision.t
  ; confirmed_side_chain_state_digest: state digest
  ; confirmed_side_chain_state_revision: Revision.t
  ; validity_within: Duration.t }
  [@@deriving lens]

and request = {rx_header: rx_header; operation: operation} [@@deriving lens]

and tx_header = {tx_revision: Revision.t; updated_limit: TokenAmount.t} [@@deriving lens]

and confirmation = {tx_header: tx_header; signed_request: request signed} [@@deriving lens]

and account_state = {balance: TokenAmount.t; account_revision: Revision.t} [@@deriving lens]

and state =
  { previous_main_chain_state: Main_chain.state digest
  ; previous_side_chain_state: state digest
  ; facilitator_revision: Revision.t
  ; spending_limit: TokenAmount.t
  ; bond_posted: TokenAmount.t
  ; accounts: account_state AddressMap.t
  ; operations: confirmation AddressMap.t
  ; main_chain_transactions_posted: Main_chain.TransactionDigestSet.t }
  [@@deriving lens]

type episteme =
  { request: request signed
  ; confirmation_option: confirmation signed option
  ; main_chain_confirmation_option: Main_chain.confirmation option }
  [@@deriving lens]

type user_account_state_per_facilitator =
  { facilitator_validity:
      knowledge_stage (* do we know the facilitator to be a liar? If so, Rejected *)
  ; confirmed_state: account_state
  ; pending_operations: episteme list }
  [@@deriving lens]

type user_state =
  { main_chain_user_state: Main_chain.user_state
  ; facilitators: user_account_state_per_facilitator AddressMap.t }
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
  ; previous: state option
  ; current: state
  ; fee_schedule: facilitator_fee_schedule }
  [@@deriving lens]

type ('input, 'output) facilitator_action = ('input, 'output, facilitator_state) action

type court_clerk_confirmation = {clerk: public_key; signature: state signature} [@@deriving lens]

type update = {current_state: state digest; availability_proof: court_clerk_confirmation list}

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
