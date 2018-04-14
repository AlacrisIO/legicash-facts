(* complex types for LegiCash flows *)

open Base
open Main_chain

(** state stored by a user *)
type user_state

(** function from 'a to 'b that acts on a user_state *)
type ('a, 'b) user_action = ('a, 'b, user_state) action


(** state stored by a facilitator *)
type facilitator_state

(** function from 'a to 'b that acts on a facilitator_state *)
type ('a, 'b) facilitator_action =
  facilitator_state * 'a -> facilitator_state * 'b legi_result

(** operation on the side chain *)
type side_chain_operation

(** user request for operation on the side chain *)
type side_chain_request

(** facilitator confirmation of transaction for the request *)
type side_chain_confirmation

(** side chain state is a public extract of the facilitator state regularly posted on the main chain *)
type side_chain_state

(** side chain update to be posted on the main chain *)
type side_chain_update

(** state stored by a verifier *)
type verifier_state

(** function from 'a to 'b that acts on a verifier_state *)
type ('a, 'b) verifier_action =
  verifier_state * 'a -> verifier_state * 'b legi_result

val detect_main_chain_facilitator_issues : (unit, unit) verifier_action
(** constantly watch the main chain and search for prosecutable issues relating to facilitators *)

(** state for a single user account at a single facilitator *)
type facilitator_account_state_per_user

(** single operation on an account *)
(* type account_operation *)

(** memo to include in a check *)
type memo = string option

(** message from user to user *)
type user_to_user_message

(** message from user to facilitator *)
type user_to_facilitator_message

(** message from facilitator to user *)
type facilitator_to_user_message

(** message from facilitator to facilitator *)
type facilitator_to_facilitator_message

(** header for all transactions
    Future Optimization: when storing the transaction in memory or on disk,
    common data that can be deduced from context is omitted from storage and computed instead.
    But when signing the transaction and showing the evidence of the transaction to clients,
    the full data is included.
 *)
type tx_header

(** header for all requests
    Request header.
    Every client request that initiates a transaction comes with a request window,
    that puts a cap on the validity of the request in terms of inclusion in the main chain.
    Thus, the other parties cannot hold the requestor's resource indefinitely on hold.
    Additionally, the request may contains reference to the root of the main chain consensus,
    so that it is clear which fork the transaction happens in;
    in some cases, it might be OK to be active in multiple forks;
    in other cases, it might lead to the requestor being punished in both.
    Note that the request_window_t data could be summarized in a hash,
    so the details can be omitted in future state logs, saving space;
    but the content would still need to be published for present validation,
    so that is a space loss in the short run (and/or for long-term archivers).
    Alternatively, to save space, the root may not be stored in places where the validity
    requires the root to be the same as *the* known consensual root at the given date.
 *)
type rx_header

val issue_user_request: (side_chain_operation, side_chain_request) user_action

(* Flow 1: Opening an account *)

(** Account activity status request / result *)
type account_activity_status_request =
  { rx_header: rx_header
  ; count: revision
  (* Number of times the account was previously closed or opened. 0 is never opened. *)
  }

type account_activity_status_confirmation =
  {header: tx_header; status: account_activity_status_request}

val open_account :
  (public_key, account_activity_status_request signed) user_action

(** Flow 1 Step 1: ensure an account is open.
    Idempotent.
    (current type assumes a single facilitator per user)
 *)

val close_account :
  (public_key, account_activity_status_request signed) user_action

(** Ensure an account is closed.
    Idempotent.
    (current type assumes a single facilitator per user)
 *)

val is_account_activity_status_open : account_activity_status_request -> bool
(** An account status is open if it was opened one more time than it was closed,
    i.e. iff its revision number is odd.
 *)

val confirm_account_activity_status :
  ( account_activity_status_request signed
  , account_activity_status_confirmation signed )
  facilitator_action
(** Flow 1 Step 2: Confirm account status for facilitator *)

val deposit : (token_amount, main_chain_request) user_action
(** Flow 1 Step 3: user sends money on the main chain *)

(** deposit request *)
type deposit_request =
  { header: rx_header
  ; amount: token_amount
  ; fee: token_amount
  ; tx_confirmation: main_chain_confirmation }

val request_deposit :
  (token_amount * main_chain_confirmation, deposit_request signed) user_action
(** Flow 1 Step 4: user pays entry fee on the side chain *)

(** Type for deposit confirmation *)
type deposit_confirmation = {header: tx_header; request: deposit_request}

val confirm_deposit :
  (deposit_request signed, deposit_confirmation signed) facilitator_action
(** Flow 1 Step 5: facilitator acknowledges deposit, stores it and sends it to user *)

(* Flow 2: Payment *)

(** invoice
    TODO: should we specify a deadline for the invoice as part of on-chain data? In what unit?
 *)
type invoice = {recipient: public_key; amount: token_amount; memo: memo}

(*
(** check to be signed by the sender *)
type check =
  {header: rx_header; invoice: invoice; fee: token_amount; expedited: bool}

val create_check : (invoice, check signed) user_action
(** Flow 2 Step 1: Alice fills in the details of a check from an initial invoice, then signs it.
    In practice, the system interactively offers the user the facilitators, fees, delays, etc.,
    available to pay the merchant and let him decide.
 *)
 *)
(*
(** certified check to be signed by the facilitator *)
type certified_check =
  {header: tx_header; signed_check: check signed; spending_limit: token_amount}

val certify_check : (check signed, certified_check signed) facilitator_action
(** Flow 2 Step 2: Trent verifies that everything's fine and signs a certified check,
    store it to database and returns it to Alice who transmits it to Bob.
 *)
 *)

(** Flow 2 Step 3: Bob does due diligence by publishing the certified check to the Gossip
    network's shard that watches Trent and waits for propagation and non-contradiction
    Maybe he asks gossip nodes for signed affidavits of non-contradiction?
    Contradiction would be a positive proof of double-spend or other irregularity by Trent
    (numbers that don't match).
    For affidavits to be actually useful, they would have to be recognized during the
    subsequent liquidation process of a failed Trent,
    i.e. the gossipers would be court clerks.
    Do we also want to detect lies by clerks and liquidate them?
    There's no end to the madness!

    side effects:
    - communicate with the gossip network to check that the certified check isn't a double-spend
    - maybe return a Double_spend exception
    - because parametric in conversation, can also be used to check double-spending on gossip network
 *)
(*
val publish_certified_check : (certified_check signed, unit) user_action

val accept_payment : (certified_check signed, unit) user_action
(** Flow 2 Step 4: Bob accepts the payment, notifies Alice and delivers the service *)
 *)
(** message-sending operations *)

val send_message : 'a -> conversation -> unit legi_result
(** Send a message
    TODO: somehow use bounded polymorphism to restrict 'a to marshallizable classes?
    TODO: To be implemented but not exposed
 *)

val account_activity_status_request_signed :
  account_activity_status_request signed -> conversation -> unit legi_result

val account_activity_status_confirmation_signed :
  account_activity_status_confirmation signed -> conversation
  -> unit legi_result
(*
val send_check_signed : check signed -> conversation -> unit legi_result *)
(*
val send_certified_check_signed :
  certified_check signed -> conversation -> unit legi_result
 *)
(* TODO: and send functions for all messages of all flows. *)

val commit_side_chain_state : (unit, unit) facilitator_action
(** For a facilitator, commit the state of the side-chain to the main-chain *)

(* Flow 3: Individual Adversarial Exit *)

val initiate_individual_exit : (unit, main_chain_request) user_action
(** Flow 3 Step 1: Alice posts an account_activity_status request for closing the account
 on the *main chain*.
 *)

(* val embed_request: (user_request, main_chain_transaction) user_action *)

val check_main_chain_for_exits :
  (unit, account_activity_status_request list) facilitator_action
(** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
    When one is found, Trent is on notice to post an update of his side-chain within
    an allowed deadline, that features a confirmation for these requests.
    Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
 *)

(** Flow 3 Step 3: Alice, who can see the final state of her account,
    posts on the main chain a demand for the final funds.
    This is signed then posted on the *main chain* by invoking the contract.
    This puts Trent and all verifiers on notice to check that Alice isn't lying,
    and post a lawsuit within a timeout window.
 *)
type account_liquidation_request = {header: rx_header; details: invoice}

val request_account_liquidation : (invoice, main_chain_request) user_action

(** Flow 3 Step 4: Trent signs and posts a confirmation on his side-chain.
 *)
type account_liquidation_confirmation =
  {header: tx_header; request: account_liquidation_request}

val confirm_account_liquidation :
  ( account_liquidation_request signed
  , account_liquidation_confirmation )
  facilitator_action

val collect_account_liquidation_funds :
  (unit, main_chain_request) user_action
(** Flow 3 Step 5: After no one speaks up during a challenge period,
    Alice invokes the contract on the main chain that actually pays the recipient
    specified in her invoice.
 *)
