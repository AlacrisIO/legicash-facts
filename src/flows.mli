(* types for LegiCash flows *)

exception Timeout of string

exception Double_spend of string

type 'a result = Success of 'a | Failure of exn

(* unique identifier for all parties, that is, customers and facilitators *)

type public_key

(* represents account balance *)

type token_amount

type revision_id = Int64.t

type chain_state

type side_chain_state

type account_state = {active: bool; balance: token_amount; change_number: revision_id}

(* associate users with their accounts *)

type account_tbl = (public_key, account_state) Hashtbl.t

type memo = string option

type expiry

type check_t =
  { sender: public_key
  ; recipient: public_key
  ; facilitator: public_key
  ; amount: token_amount
  ; fee: token_amount
  ; prev_change_number: revision_id
  ; current_change_number: revision_id
  ; chain_root: chain_state
  ; side_chain_root: side_chain_state
  ; expires_at: expiry
  ; invoice_id: memo
  ; expedited: bool }

type int256 = {field1: Int64.t; field2: Int64.t; field3: Int64.t; field4: Int64.t}

type signature_t = int256

type 'a signed = Signed of {payload: 'a; signer: public_key; signature: signature_t}

type certified_check_t =
  { facilitator: public_key
  ; side_chain_revision: revision_id
  ; previous_side_chain_revision: revision_id
  ; spending_limit: token_amount }

type facilitator_state =
  { bond_posted: token_amount
  ; accounts: account_tbl
  ; current_change_number: revision_id
  ; current_limit: token_amount
  (* how much limit is unspent during this cycle *)
  ; per_account_limit: token_amount
  ; chain_root: chain_state
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
  | Double_spend_denunciation of certified_check_t signed * certified_check_t signed
  | Settlement_proposal of settlement_proposal_t signed

(* endpoint + state of communication + possibility of reconnection *)
type conversation

(* step 2 of Payment Flow *)
val send_check_for_certification : check_t signed -> conversation -> certified_check_t signed
(** side effects:
    - communicate over the network to facilitator mentioned in the check
    - get back a signed certified check from the facilitator
    - maybe raise a Timeout exception
 *)

(* message-sending operations *)
(* for all the following operations, can return a Timeout exception *)
(* Alice does this to Bob. Does Trent do it to Alice, or does he have a separate interface??? *)

(* step 5,6,7 of Payment Flow *)
(* TODO: maybe pack more info in result for success case *)
val send_certified_check : certified_check_t signed -> conversation -> unit result

(** side effects:
    - communicate with the gossip network to check that the certified check isn't a double-spend
    - maybe return a Double_spend exception
    - because parametric in conversation, can also be used to check double-spending on gossip network
 *)

val commit_side_chain_state : unit -> unit result
(** for a facilitator, commit the state of the side-chain to the main-chain *)

(* flow operations *)

type x_facilitator_preconditions
