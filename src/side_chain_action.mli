open Legibase
open Action
open Crypto
open Side_chain

(* the "flows" mentioned here are those mentioned in the file "demo.md" *)

val detect_main_chain_facilitator_issues : (unit, unit) verifier_action
(** constantly watch the main chain and search for prosecutable issues relating to facilitators *)

val issue_user_request : (Operation.t, Request.t signed) user_action

(** Flow 1 Step 2: Confirm account status for facilitator *)

val deposit : (Address.t * TokenAmount.t, Request.t signed) user_async_action
(** Flow 1 Step 3: user sends money on the main chain to the side chain *)

val request_deposit : (TokenAmount.t * Main_chain.Confirmation.t, Request.t signed) user_action
(** deposit request *)

val push_side_chain_action_to_main_chain : FacilitatorState.t -> (Confirmation.t signed,Main_chain.Confirmation.t) user_async_action
(** reflect action on side chain on main chain *)

(* Flow 1 Step 4: user pays entry fee on the side chain *)

val confirm_request : (Request.t signed, Confirmation.t signed) facilitator_async_action
(** Flow 1 Step 5: facilitator acknowledges deposit, stores it and sends it to user *)

(* Flow 2: Payment
   Step 1: Alice fills in the details of a check from an initial invoice, then signs it.
   In practice, the system interactively offers the user the facilitators, fees, delays, etc.,
   available to pay the merchant and let him decide.

   Step 2: Trent verifies that everything's fine and signs a certified check,
   store it to database and returns it to Alice who transmits it to Bob.

   Step 3: Bob does due diligence by publishing the certified check to the Gossip
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

   Step 4: Bob accepts the payment, notifies Alice and delivers the service
*)

(* Flow 3: Withdrawal *)

val withdrawal : (Address.t * TokenAmount.t, Request.t signed) user_async_action
(** Flow 3 Step 3: user sends money from the side chain to the main chain *)

val payment : (Address.t * Address.t * TokenAmount.t, Request.t signed) user_action

(** message-sending operations *)

val send_message : 'a -> conversation -> unit legi_result
(** Send a message
    TODO: somehow use bounded polymorphism to restrict 'a to marshallizable classes?
    TODO: To be implemented but not exposed
*)

val send_user_request : user_state -> Request.t signed -> conversation -> unit legi_result

val send_facilitator_confirmation :
  FacilitatorState.t -> Confirmation.t signed -> conversation -> unit legi_result

val commit_facilitator_state : (unit, unit) facilitator_action
(** For a facilitator, commit the state of the side-chain to the main-chain *)

(* Flow 3: Individual Adversarial Exit *)

val initiate_individual_exit : (unit, Main_chain.TransactionSigned.t) user_action
(** Flow 3 Step 1: Alice posts an account_activity_status request for closing the account
    on the *main chain*.
*)

(* val embed_request: (user_request, Main_chain.transaction) user_action *)

val check_main_chain_for_exits : (unit, Request.t list) facilitator_action
(** Flow 3 Step 2: Trent, who follows the main chain, checks for such exit requests.
    When one is found, Trent is on notice to post an update of his side-chain within
    an allowed deadline, that features a confirmation for these requests.
    Alternatively, Trent fails, and bankruptcy proceedings start — see Flow 6, 7 and 8.
*)

val request_account_liquidation : (Invoice.t, Main_chain.TransactionSigned.t) user_action
(** Flow 3 Step 3: Alice, who can see the final state of her account,
    posts on the main chain a demand for the final funds.
    This is signed then posted on the *main chain* by invoking the contract.
    This puts Trent and all verifiers on notice to check that Alice isn't lying,
    and post a lawsuit within a timeout window.
*)

val collect_account_liquidation_funds : (unit, Main_chain.TransactionSigned.t) user_action
(** Flow 3 Step 4: Trent signs and posts a confirmation on his side-chain.
*)
(* Flow 3 Step 5: After no one speaks up during a challenge period,
   Alice invokes the contract on the main chain that actually pays the recipient
   specified in her invoice.
*)

(* useful functions for other test code *)
module Test : sig
  val install_contract : unit -> unit Lwt.t
  (** installs facilitator contract on main chain, enabling corresponding actions on side chain *)
  val create_account_on_testnet : Keypair.t -> unit Lwt.t
  (** creates an account on network with address created for side chain *)
  val get_prefunded_address : unit -> Address.t Lwt.t
  (** gets the prefunded address on the test network *)
  val fund_account : ?min_balance:int -> Address.t -> Keypair.t -> unit Lwt.t
  (** transfers funds from funding account to account with given keys, if balance less than min_balance *)
end
