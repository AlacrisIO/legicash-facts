(* LegiCash flows *)

open Base
open Main_chain
open Lib

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

(** invoice sent from payee to payer *)
type invoice = {recipient: public_key; amount: TokenAmount.t; memo: memo}

(** an operation on a facilitator side-chain *)
type side_chain_operation =
  | Activity_status of Revision.t
  | Payment of
      { payment_invoice: invoice
      ; payment_fee: TokenAmount.t
      ; payment_expedited: bool }
  | Deposit of
      { deposit_amount: TokenAmount.t
      ; deposit_fee: TokenAmount.t
      ; main_chain_transaction_signed: main_chain_transaction_signed
      ; main_chain_confirmation: main_chain_confirmation
      ; deposit_expedited: bool }
  | Withdrawal of {withdrawal_invoice: invoice; withdrawal_fee: TokenAmount.t}

(*
| Settlement of
  { sender: public_key
  ; sender_facilitator: public_key
  ; recipient: public_key
  ; recipient_facilitator: public_key }
*)
(** headers for a request to a facilitator
    provide a reference to the past and a timeout in the future
    *)
and rx_header =
  { facilitator: public_key
  ; requester: public_key
  ; confirmed_main_chain_state_digest: main_chain_state digest
  ; confirmed_main_chain_state_revision: Revision.t
  ; confirmed_side_chain_state_digest: side_chain_state digest
  ; confirmed_side_chain_state_revision: Revision.t
  ; validity_within: Duration.t }

(** request to a facilitator:
    an operation, plus headers that provide a reference to the past and a timeout
    *)
and side_chain_request =
  {rx_header: rx_header; side_chain_operation: side_chain_operation}

(** headers for a confirmation from a facilitator:
    give a revision so contradiction is trivial to check.
    Should we also provide log(n) digests to the previous confirmation
    whose revision is a multiple of 2**k for all k?
    *)
and tx_header = {tx_revision: Revision.t; spending_limit: TokenAmount.t}

(** A transaction confirmation from a facilitator:
    a request, plus headers that help validate against fraud.
    *)
and side_chain_confirmation =
  {tx_header: tx_header; signed_request: side_chain_request signed}

(* TODO: actually maintain the user_revision;
   add it to the rx_header, and pass rx_header to apply_side_chain_request (replacing _operation) *)
(** public state of a user's account in the facilitator's side-chain *)
and facilitator_account_state_per_user =
  {active: Revision.t; balance: TokenAmount.t; user_revision: Revision.t}

(** public state of a facilitator side-chain, as posted to the court registry and main chain
    *)
and side_chain_state =
  { previous_main_chain_state: main_chain_state digest (* Tezos state *)
  ; previous_side_chain_state:
      side_chain_state digest
      (* state previously posted on the above *)
  ; side_chain_revision: Revision.t
  ; user_accounts: facilitator_account_state_per_user Data256Map.t
  ; operations: side_chain_confirmation Data256Map.t }

(** side chain operation + knowledge about the operation *)
type side_chain_episteme =
  { side_chain_request: side_chain_request signed
  ; side_chain_confirmation_option: side_chain_confirmation signed option
  ; main_chain_confirmation_option: main_chain_confirmation option }

(** private state a user keeps for his account with a facilitator *)
type user_account_state_per_facilitator =
  { (* do we know the facilitator to be a liar? If so, Rejected *)
  facilitator_validity: knowledge_stage
  ; confirmed_state: facilitator_account_state_per_user
  ; pending_operations: side_chain_episteme list }

(** User state (for Alice)
    For now, only one facilitator; but in the future, allow for many.

    Because the system is asynchronous and trustless, we must always maintain multiple views
    of the state of the system, and the status of each change in the system.

    main chain status:
      J0 => J1, J2, J3; J1 => J2, J3; J2; J3

    side chain status:
      T0 => T1, T2, T3; T1 => T2, T3; T2 => T3 (ouch); T3 pulls in Ursula(!)
        (T2.J0: unknown to Judy yet
         OR T2.J1: almost confirmed by Judy (seen on the main blockchain, not confirmed yet)
         OR T2.J2: confirmed all the way to Judy
         OR T3.J0: Trent is a liar, we've got to do something about it
         OR T3.U1: Trent is a liar, we sent the claim to Ursula (may Ursulas), etc.
         OR T3.U2.J0: SOME Ursula accepted to replace Trent, Judy doesn't know
         OR T3.U2.J1: SOME Ursula accepted to replace Trent, posted to Judy, who didn't confirm yet
         OR T3.U2.J2: SOME Ursula accepted to replace Trent, posted to Judy, who confirmed
         OR T3.J3: LOSER: overridden by another lie of Trent that made it to Judy first.
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

type side_chain_user_state =
  { latest_main_chain_confirmation: main_chain_state digest
  ; latest_main_chain_confirmed_balance:
      TokenAmount.t
      (* Only store the confirmed state, and have any updates in pending *)
  ; facilitators: facilitator_account_state_per_user Data256Map.t
  ; main_chain_user_state: main_chain_user_state }

type ('a, 'b) user_action = ('a, 'b, side_chain_user_state) action

type verifier_state

type ('a, 'b) verifier_action = ('a, 'b, verifier_state) action

(** Fee structure for a facilitator
    NB: an important constraint is that we need to advertise this fee structure to users
    *)
type facilitator_fee_structure =
  { deposit_fee: TokenAmount.t (* fee to accept a deposit *)
  ; per_account_limit:
      TokenAmount.t (* limit for pending expedited transactions per user *)
  ; fee_per_billion: int
  (* function TokenAmount.t -> TokenAmount.t ? *) }

(** private state of a facilitator
    TODO: lawsuits? index expedited vs non-expedited transactions? multiple pending confirmations?
    *)
type facilitator_state =
  { confirmed_state: side_chain_state (* latest confirmed public state *)
  ; bond_posted: TokenAmount.t
  ; current_limit:
      TokenAmount.t (* expedited limit still unspent since confirmation *)
  ; account_states: facilitator_account_state_per_user Data256Map.t
  ; pending_operations: side_chain_episteme list Data256Map.t
  ; current_revision: Revision.t (* incremented at every change *)
  ; fee_structure: facilitator_fee_structure }

type ('a, 'b) facilitator_action = ('a, 'b, facilitator_state) action

type court_clerk_confirmation =
  {clerk: public_key; signature: side_chain_state signature}

(** The update itself has to be signed by the facilitator *)
type side_chain_update =
  { current_side_chain_state: side_chain_state digest
  ; availability_proof: court_clerk_confirmation list }

(* associate facilitators with their accounts *)

type user_to_facilitator_message

type facilitator_to_user_message

let stub_confirmed_main_chain_state = ref genesis_main_chain_state

let stub_confirmed_main_chain_state_digest =
  ref (get_digest genesis_main_chain_state)


let genesis_side_chain_state =
  { previous_main_chain_state= get_digest genesis_main_chain_state
  ; previous_side_chain_state= null_digest
  ; side_chain_revision= Revision.zero
  ; user_accounts= Data256Map.empty
  ; operations= Data256Map.empty }


let stub_confirmed_side_chain_state = ref genesis_side_chain_state

let stub_confirmed_side_chain_state_digest =
  ref (get_digest genesis_side_chain_state)


let get_facilitator side_chain_user_state =
  option_map fst
    (Data256Map.find_first_opt (constantly true)
       side_chain_user_state.facilitators)


(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Int64.of_int 256

let make_rx_header (side_chain_user_state, _) =
  match get_facilitator side_chain_user_state with
  | None -> (side_chain_user_state, Error Not_found)
  | Some facilitator ->
      ( side_chain_user_state
      , Ok
          { facilitator
          ; requester= side_chain_user_state.main_chain_user_state.public_key
          ; confirmed_main_chain_state_digest=
              !stub_confirmed_main_chain_state_digest
          ; confirmed_main_chain_state_revision=
              !stub_confirmed_main_chain_state.main_chain_revision
          ; confirmed_side_chain_state_digest=
              !stub_confirmed_side_chain_state_digest
          ; confirmed_side_chain_state_revision=
              !stub_confirmed_side_chain_state.side_chain_revision
          ; validity_within= default_validity_window } )


let mk_rx_episteme rx =
  { side_chain_request= rx
  ; side_chain_confirmation_option= None
  ; main_chain_confirmation_option= None }


let mk_tx_episteme tx =
  { side_chain_request= tx.payload.signed_request
  ; side_chain_confirmation_option= Some tx
  ; main_chain_confirmation_option= None }


let add_user_episteme side_chain_user_state episteme =
  (* TODO: use lenses? *)
  (*  update_pending state (fun pending -> episteme :: pending)*)
  {side_chain_user_state with facilitators= side_chain_user_state.facilitators}


let issue_user_request =
  action_seq
    (fun (side_chain_user_state, side_chain_operation) ->
      do_action (side_chain_user_state, ())
        (action_seq make_rx_header
           (action_of_pure_action (fun (side_chain_user_state, rx_header) ->
                sign side_chain_user_state.main_chain_user_state.private_key
                  {rx_header; side_chain_operation} ))) )
    (fun (side_chain_user_state, request) ->
      ( add_user_episteme side_chain_user_state (mk_rx_episteme request)
      , Ok request ) )


type account_activity_status_request = {rx_header: rx_header; count: Revision.t}

type account_activity_status_confirmation =
  {header: tx_header; status: account_activity_status_request}

type deposit_request =
  { header: rx_header
  ; amount: TokenAmount.t
  ; fee: TokenAmount.t
  ; tx_confirmation: main_chain_confirmation }

type deposit_confirmation = {header: tx_header; request: deposit_request}

type account_liquidation_request = {header: rx_header; details: invoice}

type account_liquidation_confirmation =
  {header: tx_header; request: account_liquidation_request}

(** Default (empty) state for a new facilitator *)
let new_facilitator_account_state_per_user =
  {active= Int64.zero; balance= Int64.zero; user_revision= Revision.zero}


(** Default (empty) state for a new facilitator *)
let new_user_account_state_per_facilitator =
  { facilitator_validity= Confirmed
  ; confirmed_state= new_facilitator_account_state_per_user
  ; pending_operations= [] }


(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
 *)
let update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operation ({active; balance} as facilitator_account_state_per_user) =
  match trusted_operation with
  | Activity_status revision ->
      if Int64.compare revision active > 0 then
        {facilitator_account_state_per_user with active= revision}
      else raise (Internal_error "I mistrusted your activity status operation")
  | Payment {payment_invoice; payment_fee} ->
      let decrement = Int64.add payment_invoice.amount payment_fee in
      if Int64.compare balance decrement >= 0 then
        { facilitator_account_state_per_user with
          balance= Int64.sub balance decrement }
      else raise (Internal_error "I mistrusted your payment operation")
  | Deposit {deposit_amount; deposit_fee} ->
      if true (* check that everything is correct *) then
        { facilitator_account_state_per_user with
          balance= Int64.add balance deposit_amount }
      else raise (Internal_error "I mistrusted your deposit operation")
  | Withdrawal {withdrawal_invoice; withdrawal_fee} ->
      if true (* check that everything is correct *) then
        { facilitator_account_state_per_user with
          balance=
            Int64.sub balance
              (Int64.add withdrawal_invoice.amount withdrawal_fee) }
      else raise (Internal_error "I mistrusted your withdrawal operation")


(** We assume most recent operation is to the left of the changes list,
 *)
let update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operations facilitator_account_state_per_user =
  List.fold_right
    update_facilitator_account_state_per_user_with_trusted_operation
    trusted_operations facilitator_account_state_per_user


let is_valid_episteme episteme = bottom ()

(* match episteme.consensus_stage with Rejected _ -> false | _ -> true *)

let optimistic_state state = bottom ()

(* let relevant_changes = filter is_valid_episteme state.pending in
  apply_operations state relevant_changes *)

let user_activity_revision_for_facilitator
    (side_chain_user_state, facilitator_pk) =
  match
    Data256Map.find_opt facilitator_pk side_chain_user_state.facilitators
  with
  | Some {active} -> active
  | None -> Revision.zero


let is_account_open (side_chain_user_state, facilitator_pk) =
  is_odd_64
    (user_activity_revision_for_facilitator
       (side_chain_user_state, facilitator_pk))


(**
  TODO: take into account not just the facilitator name, but the fee schedule, too.
  TODO: exception if facilitator dishonest.
 *)
let open_account (side_chain_user_state, facilitator_pk) = bottom ()

(*
  let os = optimistic_state state in
  let revision = user_activity_revision_for_facilitator os in
  if is_odd revision
  then (state, None)
  else let rx = make_account_status_request state facilitator_pk (revision + 1) in
       (add_episteme state (mk_rx_episteme rx), Some rx) *)

(** missing values to be implemented *)

let close_account = bottom

let collect_account_liquidation_funds = bottom

let request_account_liquidation = bottom

let check_main_chain_for_exits = bottom

let initiate_individual_exit = bottom

let send_certified_check_signed = bottom

let send_check_signed = bottom

let account_activity_status_confirmation_signed = bottom

let account_activity_status_request_signed = bottom

let accept_payment = bottom

let publish_certified_check = bottom

let certify_check = bottom

let create_check = bottom

let confirm_deposit = bottom

let request_deposit = bottom

let lift_main_chain_user_action_to_side_chain action
    (side_chain_user_state, input) =
  let main_chain_user_state = side_chain_user_state.main_chain_user_state in
  let new_main_chain_user_state, result =
    action (main_chain_user_state, input)
  in
  ( { side_chain_user_state with
      main_chain_user_state= new_main_chain_user_state }
  , result )


let deposit (side_chain_user_state, input) =
  lift_main_chain_user_action_to_side_chain transfer_tokens
    (side_chain_user_state, input)


let confirm_account_activity_status = bottom

let is_account_activity_status_open account_activity_status_request = bottom ()

(* is_odd_64 account_activity_status_request.status *)

let detect_main_chain_facilitator_issues = bottom

let confirm_account_liquidation = bottom

let collect_account_liquidation_funds = bottom

let send_user_request = bottom

let send_facilitator_confirmation = bottom

(** missing types to be implemented *)

type facilitator_to_facilitator_message

type user_to_user_message

let send_certified_check check conv = bottom ()

let commit_side_chain_state = bottom ()

let send_message payload conv = bottom ()

exception Invalid_side_chain_operation of side_chain_operation
