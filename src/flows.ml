(* LegiCash flows *)

exception Not_implemented

open Base
open Lens

(** account state
    maybe include a digest of the history?
 *)
type account_state =
  {active: revision_t; balance: token_amount; revision: revision}

(* associate users with their accounts *)

type facilitator_state =
  { latest_registered_state: facilitator_state hash
  ; latest_parent_state: main_state hash
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
  pending_transactions: certified_check_t signed list
  (* Q: indexed by expedited or not? *)
  (* pending lawsuits ? *) }

type side_chain_state =
  { previous_main_chain_state: main_chain_state digest
  ; previous_side_chain_state: side_chain_state digest
  (*  ;  *) }

(** The update itself has to be signed by the facilitator *)
type side_chain_update =
  { current_side_chain_state: side_chain_state digest
  ; availability_proof: court_clerk_confirmation list }

type court_clerk_confirmation =
  {clerk: public_key; signature: side_chain_state signature}

type memo = string option

type check_t =
  { sender: public_key
  ; recipient: public_key
  ; facilitator: public_key
  ; amount: token_amount
  ; fee: token_amount
  ; prev_change_number: revision_t
  ; current_change_number: revision_t
  ; chain_root: main_chain_state
  ; side_chain_root: side_chain_state
  ; expires_at: duration_t
  ; invoice_id: memo
  ; expedited: bool }

type certified_check_t =
  { facilitator: public_key
  ; side_chain_revision: revision_t
  ; previous_side_chain_revision: revision_t
  ; spending_limit: token_amount }

type facilitator_state =
  { bond_posted: token_amount
  ; accounts: account_tbl
  ; current_change_number: revision_t
  ; current_limit:
      token_amount (* how much limit is unspent during this cycle *)
  ; per_account_limit: token_amount
  ; chain_root: main_chain_state
  ; (* when did we last update? *)
  last_posted_side_chain_root: side_chain_state
  ; (* what did we last post? *)
  pending_transactions: certified_check_t signed list
  (* Q: indexed by expedited or not? *)
  (* pending lawsuits ? *) }

type settlement_proposal_t =
  { sender: public_key
  ; sender_facilitator: public_key
  ; recipient: public_key
  ; recipient_facilitator: public_key }

(* associate facilitators with their accounts *)

type facilitator_tbl = (public_key, facilitator_state) Hashtbl.t

type user_to_facilitator_message

type facilitator_to_user_message

type message_type = Int32.t

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

let commit_side_chain_state () = raise Not_implemented

type x_facilitator_preconditions

type tx_header_t =
  { message_type: Int32.t
  ; (* Type of the message *)
  tx_revision: revision_t
  (* *) }

type rx_header_t =
  {root: main_chain_state; date: main_chain_height_t; timeout: duration_t}

val send_message : 'a -> conversation -> unit legi_result
