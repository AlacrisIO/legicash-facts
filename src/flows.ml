(* LegiCash flows *)

exception Not_implemented

open Base

type account_state =
  {active: bool; balance: token_amount; change_number: revision_t}

(* associate users with their accounts *)

type account_tbl = (public_key, account_state) Hashtbl.t

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

type message =
  (* invariant: signer same as sender *)
  | Signed_check of check_t signed
  (* invariant: signer same as facilitator *)
  | Certified_check of certified_check_t signed
  (* invariant: signer same as both facilitators *)
  | Double_spend_denunciation of
      certified_check_t signed * certified_check_t signed
  | Settlement_proposal of settlement_proposal_t signed

let send_check_for_certification check conv = raise Not_implemented

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

type account_status_t =
  { (* Number of times the account was previously closed or opened. *)
  rx_header: rx_header_t
  ; count: Int32.t }

let account_open acct_status = raise Not_implemented

type account_status_confirmation_t =
  {tx_header: tx_header_t; request: account_status_t signed}

type message_for_enter_settlement_flow_t =
  (* Step 1: Alice signs the new desired status and sends it to Trent. *)
  | Account_status_change_request of account_status_t signed
  (* Step 2: Trent signs the request, stores the confirmation, sends the confirmation to Alice. *)
  | Account_status_change_confirmation of account_status_confirmation_t signed
(* Step 3: Alice sends money to Trent's contract on the main chain, using the normal mechanism,
     Plus some annotation to signify the public_key of the address on the side-chain,
     and including some fees to pre-pay UTXO consolidation.
     Otherwise, her UTXO is locked and all she can do is take it out.
   *)
(* Step 5: Trent commits transaction to side-chain
Alice sends money to Trent's contract on the main chain, using the normal mechanism *)
(* Actors: Alice (sender), Trent (facilitator)
 *)
(*
Preconditions:
* Alice has an account on the main chain with balance X+F+R.
* Trent has an account for Alice (voluntary or involuntary) with balance Y (or no account, and balance 0)
* Trent already has an open contract on the main

Postconditions:
* Alice has an account on the main chain with balance R.
* Alice has a voluntary account on Trent's side-chain with balance X+Y.
* Trent receives fee F from Alice.

1. Alice sends X+F to Trent's contract on the main chain
2. Trent write M1...
3. Trent commits M1 to side-chain
4. Trent sends M1 to Alice (OKish if fails)
5. Trent commits M1 to main chain

1 is enough for Alice to have her money back even if Trent fails.
If 5 didn't happen, Alice still can get her money back, which
the adversarial exit thing must take into account.

 *)
