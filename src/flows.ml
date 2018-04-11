(* LegiCash flows *)

exception Not_implemented

let bottom () : 'a = raise Not_implemented

open Base

type message_type = Int32.t

type side_chain_state =
  { previous_main_chain_state: main_chain_state digest
  ; previous_side_chain_state: side_chain_state digest }

type tx_header = {message_type: message_type; tx_revision: revision}

type rx_header =
  { message_type: message_type
  ; facilitator: public_key
  ; requester: public_key
  ; confirmed_main_chain_state_digest: main_chain_state digest
  ; confirmed_main_chain_state_timestamp: timestamp
  ; confirmed_side_chain_state_digest: side_chain_state digest
  ; validity_within: duration }

type memo = string option

type invoice = {recipient: public_key; amount: token_amount; memo: memo}

type check =
  {header: rx_header; invoice: invoice; fee: token_amount; expedited: bool}

type certified_check =
  {header: tx_header; signed_check: check signed; spending_limit: token_amount}

type account_state =
  {active: revision; balance: token_amount; revision: revision}

type account_operation

type user_state

type ('a, 'b) user_action = user_state * 'a -> user_state * 'b legi_result

type verifier_state

type ('a, 'b) verifier_action =
  verifier_state * 'a -> verifier_state * 'b legi_result

type facilitator_state =
  { latest_registered_state: facilitator_state digest
  ; latest_parent_state: main_chain_state digest
  ; account_states: (public_key, account_state) Hashtbl.t
  ; account_operations: (public_key, account_operation list) Hashtbl.t
  ; bond_posted: token_amount
  ; current_revision: revision (* incremented at every change *)
  ; current_limit:
      token_amount (* expedited limit still unspent during this cycle *)
  ; per_account_limit: token_amount
  ; confirmed_chain_state: main_chain_state
  ; (* when did we last update? *)
  last_posted_side_chain_root: side_chain_state
  ; (* what did we last post? *)
  pending_transactions: certified_check signed list
  (* Q: indexed by expedited or not? *)
  (* pending lawsuits ? *) }

type ('a, 'b) facilitator_action =
  facilitator_state * 'a -> facilitator_state * 'b legi_result

type court_clerk_confirmation =
  {clerk: public_key; signature: side_chain_state signature}

(** The update itself has to be signed by the facilitator *)
type side_chain_update =
  { current_side_chain_state: side_chain_state digest
  ; availability_proof: court_clerk_confirmation list }

type check_t =
  { sender: public_key
  ; recipient: public_key
  ; facilitator: public_key
  ; amount: token_amount
  ; fee: token_amount
  ; prev_change_number: revision
  ; current_change_number: revision
  ; chain_root: main_chain_state
  ; side_chain_root: side_chain_state
  ; expires_at: duration
  ; invoice_id: memo
  ; expedited: bool }

type certified_check_t =
  { facilitator: public_key
  ; side_chain_revision: revision
  ; previous_side_chain_revision: revision
  ; spending_limit: token_amount }

type settlement_proposal_t =
  { sender: public_key
  ; sender_facilitator: public_key
  ; recipient: public_key
  ; recipient_facilitator: public_key }

(* associate facilitators with their accounts *)

type facilitator_tbl = (public_key, facilitator_state) Hashtbl.t

type user_to_facilitator_message

type facilitator_to_user_message

type message =
  (* invariant: signer same as sender *)
  | Signed_check of check_t signed
  (* invariant: signer same as facilitator *)
  | Certified_check of certified_check_t signed
  (* invariant: signer same as both facilitators *)
  | Double_spend_denunciation of
      certified_check_t signed * certified_check_t signed
  | Settlement_proposal of settlement_proposal_t signed

(* type client_state = xxx *)

let make_check_for_certification check conv = raise Not_implemented

let send_certified_check check conv = raise Not_implemented

let commit_side_chain_state = raise Not_implemented

type x_facilitator_preconditions

let send_message payload conv = raise Not_implemented

type account_activity_status_request = {rx_header: rx_header; count: revision}

type account_activity_status_confirmation =
  {header: tx_header; status: account_activity_status_request}

type deposit_request =
  { header: rx_header
  ; amount: token_amount
  ; fee: token_amount
  ; tx_confirmation: main_chain_transaction_confirmation }

type deposit_confirmation = {header: tx_header; request: deposit_request}

type account_liquidation_request = {header: rx_header; details: invoice}

type account_liquidation_confirmation =
  {header: tx_header; request: account_liquidation_request}

(** missing types to be implemented *)

type facilitator_to_facilitator_message

type user_to_user_message

(** missing values to be implemented *)

let collect_account_liquidation_funds = bottom ()

let request_account_liquidation = bottom ()

let check_main_chain_for_exits = bottom ()

let initiate_individual_exit = bottom ()

let send_certified_check_signed = bottom ()

let send_check_signed = bottom ()

let account_activity_status_confirmation_signed = bottom ()

let account_activity_status_request_signed = bottom ()

let accept_payment = bottom ()

let publish_certified_check = bottom ()

let certify_check = bottom ()

let create_check = bottom ()

let confirm_deposit = bottom ()

let request_deposit = bottom ()

let deposit = bottom ()

let confirm_account_activity_status = bottom ()

let is_account_activity_status_open = bottom ()

let close_account = bottom ()

let open_account = bottom ()

let detect_main_chain_facilitator_issues = bottom ()

let confirm_account_liquidation = bottom ()

let collect_account_liquidation_funds = bottom ()
