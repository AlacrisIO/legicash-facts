(* Types for LegiCash Facilitator side-chains *)

open Legibase

(** Internal witness for proof that Trent is a liar
 *)
type fraud_proof

(** Stage of knowledge of one actor about an operation

  Main chain status: we assume Judy is honest and stable and never goes from Confirmed to Rejected.
  Transitions for the consensus:
    Unknown => Pending, Confirmed, Rejected
    Pending => Confirmed, Rejected

  Self status: Alice assumes she is honest and stable, but she relies on Trent who can lie.
  We don't need to represent self-status: if we don't know about it, we have nothing to represent;
  and if we do know about it, there is no transition about that, only about the status of Trent and Judy.

  Trent status: Alice weakly assumes honesty of Trent (or wouldn't even bother dealing with Trent),
  but has to take into account the possibility that Trent goes rogue at some point,
  and status of some operations go from Confirmed to Rejected via incompetence or malice.

  confirmation/rejection: either we move that to a functor so we have the proper kind,
  or we leave that aside.
 *)
type knowledge_stage =
  | Unknown
  (* 0. that actor never heard of it *)
  | Pending
  (* 1. that actor heard of it but hasn't confirmed or rejected yet *)
  | Confirmed
  (* of operation_confirmation *)
  (* 2. that actor confirmed it *)
  | Rejected
(* of operation_rejection *)
(* 3. that actor rejected it, timed out, or lied, etc. *)

(** memo identifying the invoice
    The merchant chooses this memo to match payments to invoices on his side;
    the customer must include the proper memo on its payment *)
type memo = string option

(** invoice sent from payee to payer
    TODO: should we specify a deadline for the invoice as part of on-chain data? In what unit?
 *)
type invoice =
  {recipient: Address.t; amount: Main_chain.TokenAmount.t; memo: memo}
  [@@deriving lens]

type payment_details =
  { payment_invoice: invoice
  ; payment_fee: Main_chain.TokenAmount.t
  ; payment_expedited: bool }
  [@@deriving lens]

type deposit_details =
  { deposit_amount: Main_chain.TokenAmount.t
  ; deposit_fee: Main_chain.TokenAmount.t
  ; main_chain_transaction_signed: Main_chain.transaction_signed
  ; main_chain_confirmation: Main_chain.confirmation
  ; deposit_expedited: bool }
  [@@deriving lens]

type withdrawal_details =
  {withdrawal_invoice: invoice; withdrawal_fee: Main_chain.TokenAmount.t}
  [@@deriving lens]

(** an operation on a facilitator side-chain *)
type operation =
  | Open_account of public_key
  | Deposit of deposit_details
  | Payment of payment_details
  | Withdrawal of withdrawal_details
  | Close_account
(*
| Settlement of settlement_details

type settlement_details =
  { sender: public_key
  ; sender_facilitator: public_key
  ; recipient: public_key
  ; recipient_facilitator: public_key }
*)

(** headers for a request to a facilitator
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
type rx_header =
  { facilitator: Address.t
  ; requester: Address.t
  ; requester_revision: Revision.t
  ; confirmed_main_chain_state_digest: (* Main_chain.state *) Digest.t
  ; confirmed_main_chain_state_revision: Revision.t
  ; confirmed_side_chain_state_digest: (* state *) Digest.t
  ; confirmed_side_chain_state_revision: Revision.t
  ; validity_within: Duration.t }
  [@@deriving lens]

(** request from user to facilitator for operation on the side chain
    an operation, plus headers that provide a reference to the past and a timeout
    *)
and request = {rx_header: rx_header; operation: operation} [@@deriving lens]

(** header for a confirmation from a facilitator:

    Future Optimization: when storing the transaction in memory or on disk,
    common data that can be deduced from context is omitted from storage and computed instead.
    But when signing the transaction and showing the evidence of the transaction to clients,
    the full data is included.

    give a revision so contradiction is trivial to check.
    Should we also provide log(n) digests to the previous confirmation
    whose revision is a multiple of 2**k for all k?
    *)
and tx_header =
  {tx_revision: Revision.t; spending_limit: Main_chain.TokenAmount.t}
  [@@deriving lens]

(** A transaction confirmation from a facilitator:
    a request, plus headers that help validate against fraud.
    *)
and confirmation =
  {tx_header: tx_header; signed_request: request signed}
  [@@deriving lens]
(* TODO: actually maintain the user_revision;
   pass rx_header to apply_side_chain_request (replacing _operation) to account for user_revision *)

(** public state of the account of a user with a facilitator as visible in the public side-chain *)
and account_state =
  { active: bool
  ; balance: Main_chain.TokenAmount.t
  ; account_revision: Revision.t
  ; user_key: public_key }
  [@@deriving lens]

(** public state of a facilitator side-chain, as posted to the court registry and main chain
    *)
and state =
  { previous_main_chain_state: (* Main_chain.state *) Digest.t
  ; previous_side_chain_state: (* state *) Digest.t
      (* state previously posted on the above *)
  ; side_chain_revision: Revision.t
  ; user_accounts: account_state AddressMap.t
  ; user_keys: public_key AddressMap.t
  ; operations: confirmation AddressMap.t
  ; deposited: Main_chain.TransactionDigestSet.t }
  [@@deriving lens]

(** side chain operation + knowledge about the operation *)
type episteme =
  { request: request signed
  ; confirmation_option: confirmation signed option
  ; main_chain_confirmation_option: Main_chain.confirmation option }
  [@@deriving lens]

(** private state a user keeps for his account with a facilitator *)
type user_account_state_per_facilitator =
  { facilitator_validity:
      knowledge_stage
      (* do we know the facilitator to be a liar? If so, Rejected *)
  ; confirmed_state: account_state
  ; pending_operations: episteme list }
  [@@deriving lens]

(** User state (for Alice)
    For now, only one facilitator; but in the future, allow for many.

    Because the system is asynchronous and trustless, we must always maintain multiple views
    of the state of the system, and the status of each change in the system.

    main chain status:
      J0 => J1, J2, J3; J1 => J2, J3; J2; J3

    side chain status:
      T0 => T1, T2, T3; T1 => T2, T3; T2 => T3 (ouch); T3 pulls in Ursula(!), with *a separate data structure*
        (T2.J0: unknown to Judy yet
         OR T2.J1: almost confirmed by Judy (seen on the main blockchain, not confirmed yet)
         OR T2.J2: confirmed all the way to Judy
         OR T3.J0: Trent is a liar, got to do something about it -- send to Ursula
         OR T3.J3: LOSER: overridden by another lie of Trent that made it to Judy first.

         OR T3.U1: Trent is a liar, we sent the claim to Ursula
         OR T3.U2.J0: SOME Ursula accepted to replace Trent, Judy doesn't know
         OR T3.U2.J1: SOME Ursula accepted to replace Trent, posted to Judy, who didn't confirm yet
         OR T3.U2.J2: SOME Ursula accepted to replace Trent, posted to Judy, who confirmed
         OR T3.U3.J0: ALL Ursulas are dishonest, do your own thing, quick,
                   do an individual exit or become a facilitator yourself, etc.
         OR T3.U3.J1: ALL Ursulas are dishonest, did our thing, waiting for confirmation.
         OR T3.U3.J2: ALL Ursulas are dishonest, did our thing, won.)

   A. We start from the last state S confirmed by Judy (summary of all operations of status J2).
   B. We want to maintain a list/set of operations that currently matter to the user.
      WHEN the operations are either confirmed or rejected by Judy (status J2 or J3),
      then the user may flush them out of active memory (but they are logged to disk for accounting).
   C. The operations are indexed somehow by knowledge_stage of Trent, Judy, etc.? by type?
   D. The user can play all the operations, and get an idea of what's confirmed,
      what's the expected result if everything goes right,
      what are the upper and lower bounds if some things go wrong.
   E. If Trent lies, we want to be able to divert the unconfirmed *incoming* transactions
      to Ursula and/or Judy (TODO: and their dependency history if any?)
 *)
type user_state =
  { latest_main_chain_confirmation: (* Main_chain.state *) Digest.t
  ; latest_main_chain_confirmed_balance:
      Main_chain.TokenAmount.t
      (* Only store the confirmed state, and have any updates in pending *)
  ; facilitators: user_account_state_per_facilitator AddressMap.t
  ; main_chain_user_state: Main_chain.user_state }
  [@@deriving lens]

(** function from 'a to 'b that acts on a user_state *)
type ('a, 'b) user_action = ('a, 'b, user_state) action

(** state stored by a verifier *)
type verifier_state

(** function from 'a to 'b that acts on a verifier_state *)
type ('a, 'b) verifier_action = ('a, 'b, verifier_state) action

(** Fee structure for a facilitator
    NB: an important constraint is that we need to advertise this fee structure to users
    *)
type facilitator_fee_structure =
  { deposit_fee: Main_chain.TokenAmount.t (* fee to accept a deposit *)
  ; per_account_limit:
      Main_chain.TokenAmount.t (* limit for pending expedited transactions per user *)
  ; fee_per_billion: int
  (* function TokenAmount.t -> TokenAmount.t ? *) }
  [@@deriving lens]

(** Private state of a facilitator (as opposed to what's public in the side-chain)
    TODO: lawsuits? index expedited vs non-expedited transactions? multiple pending confirmations?
    *)
type facilitator_state =
  { keypair: Keypairs.t
  ; confirmed_state: state (* latest confirmed public state *)
  ; current_revision: Revision.t (* incremented at every change *)
  ; current_limit:
      Main_chain.TokenAmount.t (* expedited limit still unspent since confirmation *)
  ; bond_posted: Main_chain.TokenAmount.t
  ; account_states: account_state AddressMap.t
  ; pending_operations: episteme list AddressMap.t
  ; fee_structure: facilitator_fee_structure }
  [@@deriving lens]

(** function from 'a to 'b that acts on a facilitator_state *)
type ('a, 'b) facilitator_action = ('a, 'b, facilitator_state) action

type court_clerk_confirmation =
  {clerk: public_key; signature: state signature}
  [@@deriving lens]

(** Side chain update to be posted on the main chain, including signatures by court registry clerks.
    The update itself has to be signed by the facilitator *)
type update =
  { current_state: (* state *) Digest.t
  ; availability_proof: court_clerk_confirmation list }
(*[@@deriving lens { prefix = true }]*)

(** message from user to user *)
type user_to_user_message

(** message from user to facilitator *)
type user_to_facilitator_message

(** message from facilitator to user *)
type facilitator_to_user_message

(** message from facilitator to facilitator *)
type facilitator_to_facilitator_message

exception No_facilitator_yet

exception Already_open

exception Already_closed

exception Invalid_operation of operation
