(* LegiCash flows *)

exception Not_implemented

let bottom () : 'a = raise Not_implemented

(** TODO: find which is canonical according to the style guide between this and
let list_of_option x = match x with None -> [] | Some x -> [x]
  and/or define a new style guide rule with motivation.
 *)
let list_of_option = function None -> [] | Some x -> [x]

open Base




(** Witness for confirmation
    For a rx from Alice, the tx from Trent
    For a main chain tx, the block from Judy
    TODO: use GADTs, where type parameter indicates
      who is affected (facilitator or customer)
 *)
type operation_confirmation =
  Transaction (* TODO: of tx from Trent *)
| Block (* TODO: block from Judy *)

(** Witness for proof that Trent is a liar
 *)
type fraud_proof

(** Witness for rejection
    For a rx from Alice, timeout or Trent is a liar
    For a main chain tx, timeout
 *)
type operation_rejection =
  Timeout
| Fraud of fraud_proof

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
 *)
type knowledge_stage =
  Unknown (* 0. that actor never heard of it *)
| In_flight (* 1. that actor heard of it but hasn't confirmed or rejected yet *)
| Confirmed of operation_confirmation (* 2. that actor confirmed it *)
| Rejected of operation_rejection (* 3. that actor rejected it, timed out, or lied, etc. *)

(** TODO: pure object hierarchy for rx and tx on main chain or side chain *)
type operation

(** TODO: operation + knowledge about the operation *)
type episteme =
  { operation: operation
  ; consensus_stage: knowledge_stage
  ; facilitator_stage: knowledge_stage (* where there is no facilitator, it is always Unknown *) }

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

type facilitator_account_state_per_user =
  { active: revision
  ; balance: token_amount
  ; revision: revision}

type user_account_state_per_facilitator =
  { (* do we know the facilitator to be a liar? If so, Rejected *)
    facilitator_validity: knowledge_stage
  ; (* Current revision of our interactions with facilitator *)
    revision: revision
  ; (* Are we open or closed? (TODO: per facilitator) *)
    latest_activity_status_confirmation: account_activity_status_confirmation option
  ; (* How much do we have on the side-chain? (TODO: per facilitator) *)
    latest_side_chain_confirmed_balance: token_amount
  }

type account_operation


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
type user_state =
  { latest_main_chain_confirmation: main_chain_state digest
  ; latest_main_chain_confirmed_balance: token_amount
  ; pending_operations: episteme list

  (* Only store the confirmed state, and have any updates in pending *)
  ; facilitators: (public_key, facilitator_account_state_per_user) Hashtbl.t
  }

type ('a, 'b) user_action = user_state * 'a -> user_state * 'b legi_result

type verifier_state

type ('a, 'b) verifier_action =
  verifier_state * 'a -> verifier_state * 'b legi_result

type facilitator_state =
  { latest_registered_state: facilitator_state digest
  ; latest_parent_state: main_chain_state digest
  ; account_states: (public_key, facilitator_account_state_per_user) Hashtbl.t
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

let make_check_for_certification check conv = bottom ()

let send_certified_check check conv = bottom ()

let commit_side_chain_state = bottom ()

let send_message payload conv = bottom ()

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

let is_account_confirmed_open state =
  match state.latest_activity_status_confirmation in
    None => false
  | Some rx => is_account_activity_status_open rx

let apply_operation state change =
  bottom ()

let apply_operations state changes =
  reduce apply_operation state change

let is_valid_episteme episteme =
  match episteme.consensus_stage with Rejected _ -> false | _ -> true

let optimistic_state state =
  let relevant_changes = filter is_valid_episteme state.pending in
  apply_operations state relevant_changes

let user_activity_revision_for_facilitator user_state =
  match os.facilitators.get(facilitator_pk) with None -> 0 | Some x -> x.active

let mk_rx_episteme rx = mk_episteme rx Unknown Unknown

let mk_tx_episteme tx = mk_episteme tx Unknown (Confirmed tx)

let add_episteme state episteme =
  update_pending state (fun pending -> episteme :: pending)

(**
  TODO: take into account not just the facilitator name, but the fee schedule, too.
  TODO: exception if facilitator dishonest.
 *)
let open_account (state, facilitator_pk) =
  let os = optimistic_state state in
  let revision = user_activity_revision_for_facilitator os in
  if is_odd revision
  then (state, None)
  else let rx = make_account_status_request state facilitator_pk (revision + 1) in
       (add_episteme state (mk_rx_episteme rx), Some rx)

(** missing values to be implemented *)

let close_account = bottom ()

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

let is_odd x = (x % 2) == 1

let is_account_activity_status_open account_activity_status_request =
  is_odd account_activity_status_request.status

let detect_main_chain_facilitator_issues = bottom ()

let confirm_account_liquidation = bottom ()

let collect_account_liquidation_funds = bottom ()
