open Legilogic_lib
open Action
open Signing

open Legilogic_ethereum
open Ethereum_chain

(* the "flows" mentioned here are those mentioned in the file "demo.md" *)

(*
   val detect_main_chain_facilitator_issues : (unit, unit) verifier_action
   (** constantly watch the main chain and search for prosecutable issues relating to facilitators *)
*)

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

(** message-sending operations *)
(*
   val send_message : 'a -> conversation -> unit or_exn
   (** Send a message
   TODO: somehow use bounded polymorphism to restrict 'a to marshallizable classes?
   TODO: To be implemented but not exposed
 *)*)

(*
   val send_user_request : UserState.t -> Request.t signed -> conversation -> unit or_exn

   val send_facilitator_confirmation :
   FacilitatorState.t -> Confirmation.t signed -> conversation -> unit or_exn
*)

val check_side_chain_contract_created : Address.t -> Address.t Lwt_exn.t

val create_side_chain_contract : Address.t -> string -> Address.t Lwt_exn.t
(** Given an address for whoever will pay to install it and its Ethereum JSON-RPC password,
    installs the facilitator contract on main chain, enabling the side chain *)

val ensure_side_chain_contract_created : Address.t -> string -> unit Lwt_exn.t
(** Given an address for whoever will pay to install it and its Ethereum JSON-RPC password,
    ensures that a suitable contract is installed on the main chain, enabling corresponding actions on side chain *)



(* useful functions for other test code *)
module Test : sig
  val create_account_on_testnet : Keypair.t -> address Lwt_exn.t
  (** creates an account on network with address created for side chain *)

  val fund_account : ?min_balance:TokenAmount.t -> Address.t -> Address.t -> unit Lwt_exn.t
  (** transfers funds from funding account to account with given address, if balance less than min_balance *)
end
