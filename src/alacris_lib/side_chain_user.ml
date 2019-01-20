open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Marshaling
open Signing
open Persisting
open Types
open Merkle_trie
open Json_rpc
open Trie

open Legilogic_ethereum
open Side_chain_server_config
open Ethereum_watch
open Ethereum_json_rpc
(* open Ethereum_abi *)
open State_update
open Operator_contract
   
open Side_chain

(** TODO: query the network, whatever, and find the fee schedule *)
let get_operator_fee_schedule _operator_address =
  Lwt_exn.return initial_fee_schedule

(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let stub_confirmed_main_chain_state = ref Ethereum_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Ethereum_chain.State.digest Ethereum_chain.genesis_state)

let stub_confirmed_side_chain_state = ref Side_chain.State.empty

let stub_confirmed_side_chain_state_digest = ref (State.digest Side_chain.State.empty)

[@@@warning "-32-27"]
let get_keypair_of_address user =
  Lwt_exn.catching_arr keypair_of_address user


  
let wait_for_contract_event (contract_address : Address.t) (operator : Address.t) (revision : Revision.t) (validx : Revision.t)   : Ethereum_chain.Confirmation.t Lwt_exn.t =
  Logging.log "wait_for_operator_state_update, step 1";
  let (delay : float) = Side_chain_server_config.delay_wait_ethereum_watch_in_seconds in
  let (topics : Bytes.t option list) = [None; (topic_of_address operator); (topic_of_revision revision); (topic_of_revision validx)] in
  Logging.log "wait_for_operator_state_update, step 2";
  Lwt_exn.bind (retrieve_relevant_single_logs delay contract_address topics)
  (fun (_lobj : LogObject.t) ->
  Logging.log "wait_for_operator_state_update, step 3";
  Lwt_exn.return
    Ethereum_chain.Confirmation.
      { transaction_hash= Digest.zero
      ; transaction_index= Revision.zero
      ; block_number= Revision.zero
      ; block_hash= Digest.zero })


let wait_for_operator_state_update (contract_address : Address.t) (operator : Address.t) (revision : Revision.t) : Ethereum_chain.Confirmation.t Lwt_exn.t =
  wait_for_contract_event contract_address operator revision Revision.zero

let wait_for_withdrawal_event (contract_address : Address.t) (operator : Address.t) (revision : Revision.t) : unit Lwt_exn.t =
  Lwt_exn.bind (wait_for_contract_event contract_address operator revision Revision.one) (fun _ -> Lwt_exn.return ())
      


let final_claim_withdrawal_operation (tc : TransactionCommitment.t) (operator : Address.t) : unit Lwt_exn.t =
  match (tc.transaction.tx_request |> TransactionRequest.request).operation with
  | Deposit _ -> Lwt_exn.return ()
  | Payment _ -> Lwt_exn.return ()
  | Withdrawal {withdrawal_amount; withdrawal_fee} ->
     Logging.log "Beginning of final_main_chain_operation";
     Lwt_exn.bind (emit_claim_withdrawal_operation tc.contract_address operator tc.operator_revision withdrawal_amount Side_chain_server_config.bond_value_v tc.state_digest)
       (fun _ ->
         Logging.log "Before wait_for_withdrawal_event";
         wait_for_withdrawal_event tc.contract_address operator tc.operator_revision)

  

         

let final_withdraw_operation (tc : TransactionCommitment.t) (operator : Address.t) : unit Lwt_exn.t =
  let (first_part_withdraw_operation : TransactionCommitment.t -> Address.t -> unit Lwt_exn.t) =
    fun tc operator ->
    match (tc.transaction.tx_request |> TransactionRequest.request).operation with
    | Deposit _ -> Lwt_exn.return ()
    | Payment _ -> Lwt_exn.return ()
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
       Logging.log "Beginning of final_main_chain_operation";
       Lwt_exn.bind (sleep_delay_exn Side_chain_server_config.challenge_duration_in_seconds_f) (fun () -> emit_withdraw_operation tc.contract_address operator tc.operator_revision withdrawal_amount Side_chain_server_config.bond_value_v tc.state_digest) in
  Lwt_exn.bind (first_part_withdraw_operation tc operator)
    (fun () ->
      let (delay : float) = Side_chain_server_config.delay_wait_ethereum_watch_in_seconds in
      let (topics2 : Bytes.t option list) = [None; (topic_of_address operator); (topic_of_revision tc.operator_revision); (topic_of_amount (TokenAmount.of_int 2))] in
      let (topics3 : Bytes.t option list) = [None; (topic_of_address operator); (topic_of_revision tc.operator_revision); (topic_of_amount (TokenAmount.of_int 3))] in
      let (list_topics : Bytes.t option list list) = [topics2; topics3] in 
      let rec fct_get_correct_event unit : unit Lwt_exn.t =
        Lwt_exn.bind (retrieve_relevant_list_logs_group delay tc.contract_address list_topics)
          (fun x_llogs_group ->
            (*            let (len2 : int) = List.length (List.nth x_llogs_group 0) in*)
            let (len3 : int) = List.length (List.nth x_llogs_group 1) in
            if len3 > 0 then
              Lwt_exn.return ()
            else
              Lwt_exn.bind (sleep_delay_exn delay) (fct_get_correct_event)
          )
      in fct_get_correct_event ()
    )
  
  
let make_rx_header (user : Address.t) (operator : Address.t) (revision : Revision.t) : RxHeader.t Lwt.t =
  Lwt.return RxHeader.
               { operator
               ; requester= user
               ; requester_revision= revision
               ; confirmed_main_chain_state_digest= !stub_confirmed_main_chain_state_digest
               ; confirmed_main_chain_state_revision= !stub_confirmed_main_chain_state.revision
               ; confirmed_side_chain_state_digest= !stub_confirmed_side_chain_state_digest
               ; confirmed_side_chain_state_revision=
                   !stub_confirmed_side_chain_state.operator_revision
               ; validity_within= default_validity_window }

let make_user_transaction_request (user : Address.t) (operator : Address.t) (revision : Revision.t) (operation : UserOperation.t) : SignedUserTransactionRequest.t Lwt_exn.t =
  let open Lwt_exn in
  of_lwt (make_rx_header user operator) revision
  >>= fun rx_header ->
  let request = UserTransactionRequest.{rx_header; operation} in
  get_keypair_of_address user
  >>= fun keypair ->
  SignedUserTransactionRequest.make keypair request |> return

module DepositWanted = struct
  [@@@warning "-39"]
  type t =
    { operator: Address.t
    ; deposit_amount: TokenAmount.t }
  [@@deriving yojson]
end

module PaymentWanted = struct
  [@@@warning "-39"]
  type t =
    { operator: Address.t
    ; recipient: Address.t
    ; amount: TokenAmount.t
    ; memo: string
    ; payment_expedited: bool }
  [@@deriving yojson]
end

module WithdrawalWanted = struct
  [@@@warning "-39"]
  type t =
    { operator: Address.t
    ; withdrawal_amount: TokenAmount.t }
  [@@deriving yojson]
end

module OngoingTransactionStatus = struct
  (* TODO: include a strong reference to the TransactionTracker, so it won't get garbage collected
     at just the wrong moment; make sure it can properly be persisted. Sigh.
     Need to clarify this problem. *)
  [@@@warning "-39"]
  type t =
    | DepositWanted of DepositWanted.t * TokenAmount.t
    | DepositPosted of DepositWanted.t * TokenAmount.t * Ethereum_user.TransactionTracker.Key.t
    | DepositConfirmed of DepositWanted.t * TokenAmount.t * Ethereum_chain.Transaction.t * Ethereum_chain.Confirmation.t
    | Requested of UserTransactionRequest.t signed (* for all operation *)
    | SignedByOperator of TransactionCommitment.t (* for all operation *)
    | PostedToRegistry of TransactionCommitment.t (* for all operation *)
    | PostedToMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t (* for withdrawal only *)
    | ConfirmedOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t (* for withdrawal only *)
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | DepositWanted _ | DepositPosted _ | DepositConfirmed _ -> None
    | Requested signed_request -> Some signed_request
    | SignedByOperator tc | PostedToRegistry tc
    | PostedToMainChain (tc, _) | ConfirmedOnMainChain (tc, _) ->
      tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
  let signed_request = signed_request_opt >> Option.get
  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get
  let status_operator = function
    | DepositWanted (w, _) | DepositPosted (w, _, _) | DepositConfirmed (w, _, _, _) ->
      w.DepositWanted.operator
    | x -> (request x).rx_header.operator
end

module FinalTransactionStatus = struct
  type t =
    | SettledOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | SettledOnMainChain (tc, _) -> tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
    | Failed (ts, _) -> OngoingTransactionStatus.signed_request_opt ts
end

module TransactionStatus = struct
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable (P) : PersistableS with type t := t)

  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | Ongoing x -> OngoingTransactionStatus.signed_request_opt x
    | Final x -> FinalTransactionStatus.signed_request_opt x
  let signed_request = signed_request_opt >> Option.get
  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get
end

exception TransactionFailed of OngoingTransactionStatus.t * exn

type revision_generator = (unit, Revision.t) Lwter.arr

module TransactionTracker = struct
  module Base = struct
    module Key = struct
      [@@@warning "-39"]
      type t= { user : Address.t; operator : Address.t; revision : Revision.t } [@@deriving yojson]
      include (YojsonMarshalable(struct
                 type nonrec t = t
                 let yojsoning = {to_yojson;of_yojson}
                 let marshaling = marshaling3
                                    (fun {user;operator;revision} -> user,operator,revision)
                                    (fun user operator revision -> {user;operator;revision})
                                    Address.marshaling Address.marshaling Revision.marshaling
               end): YojsonMarshalableS with type t := t)
    end
    type key = Key.t
    let key_prefix = "ALTT"
    type context = revision_generator
    module State = TransactionStatus
    type state = State.t
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string

    (* TODO: inspection? cancellation? split private and public activities? *)
    type t = Key.t * FinalTransactionStatus.t Lwt.t

    open Lwter
    let make_activity revision_generator key saving state =
      let Key.{user; operator; revision} = key in
      let rec update (status : TransactionStatus.t) =
        saving status >>= Db.committing >>= loop
      and continue (status : OngoingTransactionStatus.t) =
        TransactionStatus.Ongoing status |> update
      and finalize (status : FinalTransactionStatus.t) =
        (* TODO: remove the request from the ongoing_transactions set!
           -- this requires some function added to the context! *)
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
        finalize (Failed (transaction_status, error))
      and loop (status: TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
        match status with
        | Ongoing ongoing ->
          let open OngoingTransactionStatus in
          (match ongoing with
           | DepositWanted (({operator; deposit_amount} as deposit_wanted), deposit_fee) ->
             Logging.log "TR_LOOP, DepositWanted operation";
             let pre_transaction =
               TokenAmount.(add deposit_amount deposit_fee)
               |> Operator_contract.pre_deposit ~operator in
             (* TODO: have a single transaction for queueing the Wanted and the DepositPosted *)
             (Ethereum_user.add_ongoing_transaction user (Wanted pre_transaction)
              >>= function
              | Error error -> invalidate ongoing error
              | Ok (tracker_key, _, _) ->
                DepositPosted (deposit_wanted, deposit_fee, tracker_key) |> continue)
           | DepositPosted (deposit_wanted, deposit_fee, tracker_key) ->
             Logging.log "TR_LOOP, DepositPosted operation";
             let (_, promise, _) = Ethereum_user.TransactionTracker.get () tracker_key in
             (promise >>= function
              | Failed (_, error) -> invalidate ongoing error (* TODO: keep the ethereum ongoing transaction status? *)
              | Confirmed (transaction, confirmation) ->
                DepositConfirmed (deposit_wanted, deposit_fee, transaction, confirmation) |> continue)
           | DepositConfirmed ({deposit_amount}, deposit_fee,
                               main_chain_deposit, main_chain_deposit_confirmation) ->
             Logging.log "TR_LOOP, DepositConfirmed operation";
             revision_generator () >>= fun (revision : Revision.t) ->
             (make_user_transaction_request (user : Address.t) (operator : Address.t) (revision : Revision.t)
                (Deposit
                   { deposit_amount
                   ; deposit_fee
                   ; main_chain_deposit
                   ; main_chain_deposit_confirmation })
              >>= function
              | Ok request -> Requested request |> continue
              | Error error -> invalidate ongoing error)
           | Requested request ->
             Logging.log "TR_LOOP, Requested operation";
             (* TODO: handle retries. But it should be in the side_chain_operator *)
             (request
              |> Side_chain_client.post_user_transaction_request
              >>= function
              | Ok (tc : TransactionCommitment.t) ->
                 Logging.log "side_chain_user: TrTracker, Ok case";
                 SignedByOperator tc |> continue
              | Error (error : exn) ->
                 Logging.log "side_chain_user: exn=%s" (Printexc.to_string error);
                 Logging.log "side_chain_user: TrTracker, Error case";
                 invalidate ongoing error)
           | SignedByOperator (tc : TransactionCommitment.t) ->
             Logging.log "TR_LOOP, SignedByOperator operation";
             (* TODO: add support for Shared Knowledge Network / "Smart Court Registry" *)
             PostedToRegistry tc |> continue
           | PostedToRegistry (tc : TransactionCommitment.t) ->
             Logging.log "TR_LOOP, PostedToRegistry operation";
             (* TODO: add support for Shared Knowledge Network / "Smart Court Registry" *)
             (wait_for_operator_state_update tc.contract_address operator tc.operator_revision
              >>= function
              | Ok (c : Ethereum_chain.Confirmation.t) ->
                (match (tc.transaction.tx_request |> TransactionRequest.request).operation with
                 | Deposit _ | Payment _ -> FinalTransactionStatus.SettledOnMainChain (tc, c) |> finalize
                 | Withdrawal _ -> PostedToMainChain (tc, c) |> continue)
              | Error error -> invalidate ongoing error)
           | PostedToMainChain ((tc : TransactionCommitment.t), (confirmation : Ethereum_chain.Confirmation.t)) ->
             Logging.log "TR_LOOP, PostedToMainChain operation";
             (* Withdrawal that we're going to have to claim *)
             (* TODO: wait for confirmation on the main chain and handle lawsuits
                Right now, no lawsuit *)
             Lwt.bind (final_claim_withdrawal_operation tc operator) (fun _ ->
                 ConfirmedOnMainChain (tc, confirmation) |> continue)
           | ConfirmedOnMainChain ((tc : TransactionCommitment.t), (confirmation : Ethereum_chain.Confirmation.t)) ->
             Logging.log "TR_LOOP, ConfirmedOnMainChain operation";
             (* Confirmed Withdrawal that we're going to have to execute *)
             (* TODO: post a transaction to actually get the money *)
             Lwt.bind (final_withdraw_operation tc operator) (fun _ ->
                 FinalTransactionStatus.SettledOnMainChain (tc, confirmation) |> finalize))
        | Final x -> return x
      in key, loop state
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  let wait promise =
    let open Lwter in
    promise >>= function
    | FinalTransactionStatus.SettledOnMainChain (t, c) -> Lwt_exn.return (t, c)
    | FinalTransactionStatus.Failed (o, e) -> Lwt_exn.fail (TransactionFailed (o, e))
end

module UserAccountState = struct
  [@warning "-39"]
  type t =
    { is_operator_valid: bool
    ; confirmed_state: AccountState.t
    ; side_chain_revision: Revision.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }, yojson ]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling5
        (fun { is_operator_valid
             ; confirmed_state
             ; side_chain_revision
             ; transaction_counter
             ; ongoing_transactions } ->
          is_operator_valid, confirmed_state, side_chain_revision,
          transaction_counter, ongoing_transactions)
        (fun is_operator_valid confirmed_state side_chain_revision
          transaction_counter ongoing_transactions ->
          { is_operator_valid
          ; confirmed_state
          ; side_chain_revision
          ; transaction_counter
          ; ongoing_transactions })
        bool_marshaling AccountState.marshaling Revision.marshaling
        Revision.marshaling RevisionSet.marshaling
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { is_operator_valid= true
    ; confirmed_state= AccountState.empty
    ; side_chain_revision= Revision.zero
    ; transaction_counter = Revision.zero
    ; ongoing_transactions= RevisionSet.empty }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountState)

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; operators: UserAccountStateMap.t
    ; notification_counter: Revision.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }, yojson]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling =
      marshaling4
        (fun { address; operators; notification_counter; notifications } ->
           address, operators, notification_counter, notifications)
        (fun address operators notification_counter notifications ->
           { address; operators; notification_counter; notifications })
        Address.marshaling UserAccountStateMap.marshaling Revision.marshaling
        (list_marshaling (marshaling2 identity pair Revision.marshaling yojson_marshaling))
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAsyncAction = AsyncAction(UserState)

let operator_lens : Address.t -> (UserState.t, UserAccountState.t) Lens.t =
  fun operator ->
    UserState.lens_operators |--
    defaulting_lens (konstant UserAccountState.empty)
      (UserAccountStateMap.lens operator)

  
let get_next_account_revision : Address.t -> unit -> UserState.t -> (Revision.t * UserState.t) Lwt.t =
  fun operator () state ->
    let revision_lens = operator_lens operator |-- UserAccountState.lens_side_chain_revision in
    let (revision : Revision.t) = revision_lens.get state in
    Logging.log "get_next_account_revision revision=%i" (Revision.to_int revision);
    Lwt.return (revision, (state |> revision_lens.set Revision.(add one revision)))

module User = struct
  module Base = struct
    module Key = Address
    let key_prefix = "ALUS"
    module State = UserState
    type t = State.t SimpleActor.t
    type context = Key.t -> t (* The get function its own context to pass around! *)
    let make_default_state _context user =
      UserState.
        { address= user
        ; operators= UserAccountStateMap.empty
        ; notification_counter= Revision.zero
        ; notifications= [] }
    let next_side_chain_revision user_actor user operator =
      SimpleActor.action (user_actor user) (get_next_account_revision operator)
    let resume_transactions user_actor user (state : State.t) =
      flip UserAccountStateMap.iter state.operators
        (fun operator account ->
           let revision_generator = next_side_chain_revision user_actor user operator in
           flip RevisionSet.iter account.ongoing_transactions
             (fun revision ->
                TransactionTracker.get revision_generator {user; operator; revision} |> ignore))
    let make_activity user_actor user saving state =
      let wrapper transform = Lwter.(transform >>> saving) in
      let actor = SimpleActor.make ~wrapper state in
      (* TODO: maybe just use Lwt_mvar.create state and leave it to users to transact on it ? *)
      resume_transactions user_actor user state; (* TODO: pass the actor as context to that? *)
      actor
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  (* Tie the knot for the fix point to pass as context *)
  let rec user_actor user = get user_actor user
  let make_tracker_context = Base.next_side_chain_revision user_actor
  let action user = SimpleActor.action (user_actor user)
  let transaction user transaction parameters =
    Lwt_exn.(action user transaction parameters >>= fun (_, tracker_promise) ->
             TransactionTracker.wait tracker_promise)
end

let add_ongoing_side_chain_transaction :
  (OngoingTransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
  fun transaction_status user_state ->
    let user = user_state.address in
    let operator = transaction_status |> OngoingTransactionStatus.status_operator in
    let revision_lens = (operator_lens operator |-- UserAccountState.lens_transaction_counter) in
    let revision = revision_lens.get user_state in
    let open Lwter in
    TransactionTracker.(make (User.make_tracker_context user operator)
                          Key.{user; operator; revision}
                          ((|>) (TransactionStatus.Ongoing transaction_status)))
    >>= fun tracker ->
    UserAsyncAction.return tracker
      (user_state
       |> revision_lens.set Revision.(add one revision)
       |> (operator_lens operator |-- UserAccountState.lens_ongoing_transactions
           |-- RevisionSet.lens revision).set true)

let deposit_fee_for OperatorFeeSchedule.{deposit_fee} _deposit_amount =
  deposit_fee

let payment_fee_for OperatorFeeSchedule.{fee_per_billion} payment_amount =
  TokenAmount.(div (mul fee_per_billion payment_amount) one_billion_tokens)

let withdrawal_fee_for OperatorFeeSchedule.{withdrawal_fee} _withdrawal_amount =
  withdrawal_fee

let deposit DepositWanted.{operator; deposit_amount} =
  let open UserAsyncAction in
  of_lwt_exn get_operator_fee_schedule operator
  >>= fun fee_schedule ->
  let deposit_fee = deposit_fee_for fee_schedule deposit_amount in
  let status = OngoingTransactionStatus.DepositWanted ({operator; deposit_amount}, deposit_fee) in
  add_ongoing_side_chain_transaction status

let get_user_address : (unit, Address.t) UserAsyncAction.arr =
  fun () user_state -> UserAsyncAction.return user_state.UserState.address user_state

let direct_operation :
  Address.t -> (OperatorFeeSchedule.t -> UserOperation.t, TransactionTracker.t) UserAsyncAction.arr =
  fun operator make_operation ->
    Logging.log "running direct_operation function";
    let open UserAsyncAction in
    of_lwt_exn get_operator_fee_schedule operator >>= fun fee_schedule ->
    let operation = make_operation fee_schedule in
    of_lwt_state (get_next_account_revision operator) () >>= fun revision ->
    get_user_address () >>= fun user ->
    of_lwt_exn (make_user_transaction_request user operator revision) operation
    >>= fun signed_request ->
    let status = OngoingTransactionStatus.Requested signed_request in
    add_ongoing_side_chain_transaction status

let payment PaymentWanted.{operator; recipient; amount; memo; payment_expedited} : TransactionTracker.t UserAsyncAction.t =
  direct_operation operator
    (fun fee_schedule ->
       let payment_invoice = Invoice.{recipient; amount; memo} in
       let payment_fee = payment_fee_for fee_schedule amount in
       UserOperation.Payment {payment_invoice; payment_fee; payment_expedited})

let withdrawal WithdrawalWanted.{operator; withdrawal_amount} : TransactionTracker.t UserAsyncAction.t =
  direct_operation operator
    (fun fee_schedule ->
       let withdrawal_fee = withdrawal_fee_for fee_schedule withdrawal_amount in
       UserOperation.Withdrawal {withdrawal_amount; withdrawal_fee})

(*
   let remove_ongoing_transaction operator revision user_state =
   (operator_lens operator
   |-- UserAccountState.lens_ongoing_transactions
   |-- RevisionSet.lens revision).set false user_state

   let get_first_operator_state_option :
   (unit, (Address.t * UserAccountState.t) option) UserAsyncAction.readonly =
   fun () user_state ->
   UserAccountStateMap.find_first_opt (konstant true) user_state.operators

   let get_first_operator =
   UserAsyncAction.(of_readonly get_first_operator_state_option
   >>> function
   | None -> fail No_operator_yet
   | Some (address, _) -> return address)

   (* TODO: is this used? should balances and revisions be updated in effect_request?
   looks like balances already are *)

   let update_account_state_with_trusted_operation
   trusted_operation ({balance} as account_state : AccountState.t) =
   let f =
   {account_state with account_revision= Revision.add account_state.account_revision Revision.one} in
   match trusted_operation with
   | Operation.Deposit {deposit_amount; deposit_fee=_deposit_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.add balance deposit_amount}
   else bork "I mistrusted your deposit operation"
   | Operation.Payment {payment_invoice; payment_fee} ->
   let decrement = TokenAmount.add payment_invoice.amount payment_fee in
   if TokenAmount.compare balance decrement >= 0 then
   {f with balance= TokenAmount.sub balance decrement}
   else bork "I mistrusted your payment operation"
   | Operation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.sub balance (TokenAmount.add withdrawal_amount withdrawal_fee)}
   else bork "I mistrusted your withdrawal operation"

   (** We assume most recent operation is to the left of the changes list, *)
   let update_account_state_with_trusted_operations trusted_operations account_state =
   List.fold_right update_account_state_with_trusted_operation trusted_operations account_state

   let [@warning "-32"] optimistic_operator_account_state operator user_state =
   match UserAccountStateMap.find_opt operator user_state.UserState.operators with
   | None -> AccountState.empty
   | Some {is_operator_valid; confirmed_state; pending_operations} ->
   match is_operator_valid with
   | Rejected -> confirmed_state
   | _ ->
   update_account_state_with_trusted_operations
   (List.map (fun x -> x.TransactionStatus.request.payload.operation) pending_operations)
   confirmed_state

   (* TODO: find the actual gas limit *)
   let withdrawal_gas_limit = TokenAmount.of_int 1000000

   (* in Lwt monad, because we'll push the request to the main chain *)
   let withdrawal (operator, withdrawal_amount) =
   let open UserAsyncAction in
   of_lwt_exn get_operator_fee_schedule operator
   >>= fun {withdrawal_fee} ->
   issue_user_transaction_request
   (Withdrawal { withdrawal_amount ; withdrawal_fee })


   (** Given a withdrawal on the side chain, reflect that on the main chain
   We should be signing the RLP, not the marshaling! *)
   let make_main_chain_withdrawal_transaction :
   address -> (UserOperation.withdrawal_details, Ethereum_chain.Transaction.t * Ethereum_json_rpc.SignedTransaction.t) Ethereum_user.UserAsyncAction.arr =
   fun operator UserOperation.{withdrawal_amount;withdrawal_fee} state ->
   (* TODO: should the withdrawal fee agree with the operator state fee schedule? where to enforce? *)
   let ticket = Revision.zero in (* TODO: implement ticketing *)
   let confirmed_state = Digest.zero in (* TODO: is this just a digest of the operator state here? *)
   let bond = TokenAmount.zero in (* TODO: where does this come from? *)
   let operation = Operator_contract.make_withdraw_call
   operator ticket bond confirmed_state in
   let value = TokenAmount.sub withdrawal_amount withdrawal_fee in
   Ethereum_user.(UserAsyncAction.of_lwt_exn
   (make_signed_transaction state.UserState.address operation value) withdrawal_gas_limit)
   state

   let push_side_chain_withdrawal_to_main_chain
   (operator : Address.t)
   (transaction : Transaction.t) =
   let request = transaction.tx_request |> TransactionRequest.request in
   (* We assume it's a transaction of the current user, that the operator committed to *)
   match request.operation with
   | Withdrawal details ->
   details
   |> ethereum_action
   Ethereum_user.UserAsyncAction.
   (make_main_chain_withdrawal_transaction operator
   >>> Ethereum_user.confirm_transaction)
   | Payment _
   | Deposit _ ->
   bork "Side chain transaction does not need subsequent interaction with main chain"

   let add_notification notification_type to_yojson x state =
   let counter = state.UserState.notification_counter in
   UserAsyncAction.return counter
   (state
   |> UserState.lens_notification_counter.set Revision.(add one counter)
   |> Lens.modify UserState.lens_notifications
   (List.cons (counter, `List [`String notification_type; to_yojson x])))

   let add_error_notification = add_notification "error_notification" ErrorNotification.to_yojson

   let _notify_error status error = add_error_notification {status;error}
*)
