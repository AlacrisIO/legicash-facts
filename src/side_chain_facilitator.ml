open Lib
open Action
open Crypto
open Db
open Keypair
open Merkle_trie
open Main_chain
open Side_chain
open Lens.Infix
open Lwt.Infix

type account_lens = (FacilitatorState.t, AccountState.t) Lens.t

let facilitator_account_lens address =
  FacilitatorState.lens_current |-- State.lens_accounts
  |-- defaulting_lens (konstant AccountState.empty) (AccountMap.lens address)

(** Given a signed request, handle the special case of opening an account, and return
    the request, the account state (new or old), the lens to set the account back at the end, and
    the user's public key *)

let ensure_user_account :
  (Request.t signed, Request.t signed * AccountState.t * account_lens * Address.t) facilitator_action =
  function
  | state, ({payload= {rx_header= {requester}}} as rx) ->
    let account_lens = facilitator_account_lens requester in
    let account_state = account_lens.get state in
    (state, Ok (rx, account_state, account_lens, requester))

(** Is the request well-formed?
    This function should include all checks that can be made without any non-local side-effect
    beside reading pure or monotonic data, which is allowed.
    Thus, we can later parallelize this check.
*)
let is_side_chain_request_well_formed :
  FacilitatorState.t * (Request.t signed * AccountState.t * account_lens * Address.t) -> bool =
  function
  | ( state
    , ( { payload=
            { rx_header=
                { requester
                ; requester_revision }
            ; operation } as payload
        ; signature }
      , {balance; account_revision}
      , _
      , _signing_address ) ) ->
    requester_revision = Revision.add account_revision Revision.one
    (* TODO: check confirmed main & side chain state + validity window *)
    && is_signature_valid Request.digest requester signature payload
    (* Check that the numbers add up: *)
    && match operation with
    | Deposit
        { deposit_amount
        ; deposit_fee
        ; main_chain_deposit_signed=
            { signature
            ; payload= {tx_header= {value}} as payload
            } as main_chain_deposit_signed
        ; main_chain_deposit_confirmation
        ; deposit_expedited=_deposit_expedited } ->
      TokenAmount.is_sum value deposit_amount deposit_fee
      (* TODO: delegate the same signature checking protocol to the main chain *)
      && is_signature_valid Transaction.digest requester signature payload
      && Main_chain.is_confirmation_valid main_chain_deposit_confirmation
           main_chain_deposit_signed
      && TokenAmount.equal deposit_fee state.fee_schedule.deposit_fee
    | Payment {payment_invoice; payment_fee; payment_expedited=_payment_expedited} ->
      TokenAmount.is_add_valid payment_invoice.amount payment_fee
      && TokenAmount.compare balance (TokenAmount.add payment_invoice.amount payment_fee) >= 0
      (* TODO: make per_account_limit work on the entire floating thing *)
      && TokenAmount.compare state.fee_schedule.per_account_limit payment_invoice.amount >= 0
      (* TODO: make sure the fee multiplication cannot overflow! *)
      && TokenAmount.is_product payment_fee
           state.fee_schedule.fee_per_billion
           (TokenAmount.div payment_invoice.amount (TokenAmount.of_int 1000000000))
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
      TokenAmount.is_add_valid withdrawal_amount withdrawal_fee
      && TokenAmount.compare balance (TokenAmount.add withdrawal_amount withdrawal_fee) >= 0
      && TokenAmount.equal withdrawal_fee state.fee_schedule.withdrawal_fee

(** Check that the request is basically well-formed, or else fail *)
let check_side_chain_request_well_formed = action_assert __LOC__ is_side_chain_request_well_formed

let make_request_confirmation :
  (Request.t signed * AccountState.t * account_lens, Confirmation.t signed) facilitator_action =
  fun (facilitator_state, (signed_request, account_state, account_lens)) ->
    let current_state = facilitator_state.current in
    let new_revision = Revision.add current_state.facilitator_revision Revision.one in
    let confirmation =
      { tx_header=TxHeader.{ tx_revision= new_revision
                           ; updated_limit= facilitator_state.current.spending_limit
                           }
      ; Confirmation.signed_request
      }
    in
    let new_facilitator_state =
      account_lens.set
        { account_state with account_revision= signed_request.payload.rx_header.requester_revision }
        { facilitator_state with
          previous = Some current_state;
          current = { current_state with
                      facilitator_revision = new_revision;
                      operations = ConfirmationMap.add new_revision confirmation current_state.operations
                    }
        }
    in
    let signed_confirmation = sign Confirmation.digest facilitator_state.keypair.private_key confirmation in
    ( new_facilitator_state
    , Ok signed_confirmation)

exception Spending_limit_exceeded

(** Facilitator actions to use up some of the limit *)
let spend_spending_limit amount (state, x) =
  let open FacilitatorState in
  if TokenAmount.compare amount state.current.spending_limit <= 0 then
    ( (FacilitatorState.lens_current |-- State.lens_spending_limit).set
        (TokenAmount.sub state.current.spending_limit amount)
        state
    , Ok x )
  else (state, Error Spending_limit_exceeded)

let maybe_spend_spending_limit is_expedited amount (state, x) =
  if is_expedited then spend_spending_limit amount (state, x) else (state, Ok x)

exception Already_posted

(* To prevent double-deposit or double-withdrawal of a same main_chain_transaction_signed,
   we put those transactions in a set of already posted transactions.
   (Future: prune that set by expiring deposit requests?
   Have more expensive process to account for old deposits?)
*)
let check_against_double_accounting main_chain_transaction_signed (state, x) =
  let witness = Main_chain.TransactionSigned.digest main_chain_transaction_signed in
  let lens =
    FacilitatorState.lens_current |-- State.lens_main_chain_transactions_posted |-- DigestSet.lens witness
  in
  if lens.get state then (state, Error Already_posted) else (lens.set true state, Ok x)


(** compute the effects of a request on the account state *)
let effect_request :
  ( Request.t signed * AccountState.t * account_lens * Address.t
  , Request.t signed * AccountState.t * account_lens )
    facilitator_action = function
  | state, (({payload= {operation}} as rx), account_state, account_lens, _user_key) ->
    match operation with
    | Deposit
        { deposit_amount
        ; deposit_fee=_deposit_fee
        ; main_chain_deposit_signed
        ; main_chain_deposit_confirmation=_main_chain_deposit_confirmation
        ; deposit_expedited } ->
      ( state
      , ( rx
        , Lens.modify AccountState.lens_balance (TokenAmount.add deposit_amount) account_state
        , account_lens ) )
      ^|> check_against_double_accounting main_chain_deposit_signed
      ^>> maybe_spend_spending_limit deposit_expedited deposit_amount
    | Payment {payment_invoice; payment_fee; payment_expedited} ->
      ( Lens.modify
          (facilitator_account_lens payment_invoice.recipient |-- AccountState.lens_balance)
          (TokenAmount.add payment_invoice.amount)
          state
      , ( rx
        , { account_state with
            balance= TokenAmount.sub account_state.balance
                       (TokenAmount.add payment_invoice.amount payment_fee) }
        , account_lens ) )
      ^|> maybe_spend_spending_limit payment_expedited payment_invoice.amount
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
      ( state
      , Ok
          ( rx
          , { account_state with
              balance= TokenAmount.sub account_state.balance
                         (TokenAmount.add withdrawal_amount withdrawal_fee) }
          , account_lens ) )


(** TODO: have a server do all the effect_requests sequentially, after they have been validated in parallel *)
let post_validated_request :
  ( Request.t signed * AccountState.t * account_lens * Address.t
  , Request.t signed * AccountState.t * account_lens ) facilitator_async_action =
  make_action_async effect_request


(** TODO:
 * save this initial state, and only use the new state if the confirmation was committed to disk,
    i.e. implement a try-catch in our monad
 * commit the confirmation to disk and remote replicas before to return it
 * parallelize, batch, etc., to have decent performance
*)
let process_request : (Request.t signed, Confirmation.t signed) facilitator_async_action =
  fun (facilitator_state, signed_request) ->
    (facilitator_state, signed_request) |>
    (make_action_async (ensure_user_account ^>> check_side_chain_request_well_formed))
    ^>>+ post_validated_request
    ^>>+ (make_action_async make_request_confirmation)
    ^>>+ fun (updated_facilitator_state, signed_confirmation) ->
    Side_chain.FacilitatorState.save updated_facilitator_state
    >>= (fun () -> Lwt.return (updated_facilitator_state, Ok signed_confirmation))

(** We assume that the operation will correctly apply:
    balances are sufficient for spending,
    deposits confirmation will check out,
    active revision will only increase, etc.
*)

let commit_facilitator_state = bottom

let check_main_chain_for_exits = bottom

