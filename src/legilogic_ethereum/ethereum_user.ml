open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Tag
open Persisting
open Types
open Signing
open Action
open Lwt_exn
open Json_rpc
open Trie

open Ethereum_chain
open Ethereum_json_rpc

let stream_of_poller : delay:float -> (unit, 'value, 'state) async_exn_action -> 'state ->
  'value AsyncStream.t Lwt.t =
  let open Lwter in
  fun ~delay poller state ->
    let nap () = Lwt_unix.sleep delay in
    let rec continue state () =
      poller () state
      >>= function
      | Ok value, new_state ->
        Lwt.return @@ AsyncStream.cons value
                        (nap () >>= continue new_state |> join)
      | Error _, new_state ->
        nap () >>= continue new_state
    in
    continue state ()

let main_chain_block_notification_stream
      ?(delay=30.0) ?(start_block=Revision.zero)
      ?(get_block=(Ethereum_json_rpc.eth_block_number)) () =
  let rec poller () next_block =
    let open Lwt in
    get_block ()
    >>= function
    | Ok block_number ->
      (* Is this block at least as big as next_block? *)
      if Revision.compare block_number next_block >= 0 then
        (* This is a previously unobserved block at or past the next_block,
           so send a notification about it. The happy path. *)
        Lwt.return (Ok block_number, Revision.(add one block_number))
      else
        Lwt.return (Error (Internal_error "Start block not reached yet"), next_block)
    | Error e -> Lwt.return (Error e, next_block) in
  stream_of_poller ~delay poller start_block

module OngoingTransactionStatus = struct
  [@warning "-39"]
  type t =
    | Wanted of PreTransaction.t
    | Signed of Transaction.t * SignedTransaction.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Wanted p -> p
    | Signed (tx, _) -> Transaction.pre_transaction tx
end

module FinalTransactionStatus = struct
  [@@@warning "-39"]
  type t =
    | Confirmed of Transaction.t * Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Confirmed (tx, _) -> Transaction.pre_transaction tx
    | Failed (ots, _) -> OngoingTransactionStatus.pre_transaction ots
end

module TransactionStatus = struct
  [@warning "-39"]
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let of_ongoing : OngoingTransactionStatus.t -> t = fun x -> Ongoing x
  let of_final : FinalTransactionStatus.t -> t = fun x -> Final x
  let pre_transaction : t -> PreTransaction.t = function
    | Ongoing x -> OngoingTransactionStatus.pre_transaction x
    | Final x -> FinalTransactionStatus.pre_transaction x
  let operation = fun x -> (x |> pre_transaction).operation
end

let confirmation_of_transaction_receipt =
  function
    TransactionReceipt.{transaction_hash ; transaction_index; block_number; block_hash} ->
    Confirmation.{transaction_hash; transaction_index; block_number; block_hash}

(** Number of blocks required for a transaction to be considered confirmed *)
(* TODO: for production, use 100, not 0 *)
let block_depth_for_confirmation = Revision.of_int 0

exception Still_pending
exception TransactionFailed of OngoingTransactionStatus.t * exn
exception NonceTooLow

let check_confirmation_deep_enough (confirmation : Confirmation.t) : Confirmation.t t =
  eth_block_number ()
  >>= fun block_number ->
  if Revision.(is_add_valid confirmation.block_number block_depth_for_confirmation
               && compare block_number (add confirmation.block_number block_depth_for_confirmation)
                  >= 0) then
    return confirmation
  else
    fail Still_pending

type nonce_operation = Peek | Next | Reset [@@deriving yojson]

module NonceTracker = struct
  open Lwter
  module Base = struct
    type context = unit
    module Key = Address
    type key = Key.t
    let key_prefix = "ETNT"
    module State = TrivialPersistable(struct
        type t = Nonce.t option
        let yojsoning = option_yojsoning Nonce.yojsoning
        let marshaling = option_marshaling Nonce.marshaling
      end)
    type state = State.t
    (* zero is often wrong, but just let it fail and resynchronize *)
    let make_default_state _ _ = None
    type t = (nonce_operation, Nonce.t) Lwter.arr
    let make_activity () address saving =
      sequentialize
        (fun op state ->
           let reset () =
             Lwt_exn.run_lwt
               (retry ~retry_window:0.01 ~max_window:5.0 ~max_retries:None
                  Ethereum_json_rpc.eth_get_transaction_count)
               (address, BlockParameter.Latest) in
           let continue result state =
             saving state >>= const (result, state) in
           let next nonce = continue nonce (Some Nonce.(add one nonce)) in
           (match (op, state) with
            | (Reset, _) ->
              continue Nonce.zero None
            | (Peek, None) ->
              reset () >>= fun nonce -> continue nonce (Some nonce)
            | (Peek, Some nonce) ->
              return (nonce, Some nonce)
            | (Next, None) ->
              reset () >>= next
            | (Next, Some nonce) -> next nonce)
           >>= fun (result, new_state) -> Logging.log "NonceTracker %s %s %s => %s %s" (Address.to_0x address) (op |> nonce_operation_to_yojson |> string_of_yojson) (State.to_yojson_string state) (Revision.to_0x result) (State.to_yojson_string new_state) ; return (result, new_state))
  end
  include PersistentActivity(Base)
  module State = Base.State
  let reset address = get () address Reset >>= const ()
  let peek address = get () address Peek
  let next address = get () address Next
end

let make_tx_header (sender, value, gas_limit) =
  (* TODO: get gas price and nonce from geth *)
  eth_gas_price () >>= fun gas_price ->
  of_lwt NonceTracker.next sender >>= fun nonce ->
  Logging.log "make_tx_header sender=%s value=%s gas_limit=%s gas_price=%s nonce=%s" (Address.to_0x sender) (TokenAmount.to_string value) (TokenAmount.to_string gas_limit) (TokenAmount.to_string gas_price) (Nonce.to_0x nonce);
  return TxHeader.{sender; nonce; gas_price; gas_limit; value}

exception Missing_password
exception Bad_password

let sign_transaction : (Transaction.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr =
  fun transaction ->
    let address = transaction.tx_header.sender in
    (try return (keypair_of_address address).password with
     | Not_found ->
       Logging.log "Couldn't find registered keypair for %s" (nicknamed_string_of_address address);
       fail Missing_password)
    >>= fun password -> personal_sign_transaction (transaction_to_parameters transaction, password)
    >>= fun signed -> return (transaction, signed)

let make_signed_transaction sender operation value gas_limit =
  make_tx_header (sender, value, gas_limit)
  >>= fun tx_header ->
  sign_transaction Transaction.{tx_header; operation}

let nonce_too_low address =
  Logging.log "nonce too low for %s" (nicknamed_string_of_address address);
  (* TODO: Send Notification to end-user via UI! *)
  Lwter.(NonceTracker.reset address >>= const (Error NonceTooLow))

(** Wait until a transaction has been confirmed by the main chain. *)
let send_and_confirm_transaction :
  (Transaction.t * SignedTransaction.t, Confirmation.t) Lwt_exn.arr =
  fun (transaction, SignedTransaction.{raw}) ->
    let open Lwter in
    let sender = transaction.tx_header.sender in
    eth_send_raw_transaction raw
    >>= function
    | Error (Rpc_error {code= -32000; message="nonce too low"}) ->
      nonce_too_low sender
    | Error e -> return (Error e)
    | Ok transaction_hash ->
      let open Lwt_exn in
      Ethereum_json_rpc.eth_get_transaction_receipt transaction_hash
      >>= function
      | None ->
        let nonce = transaction.tx_header.nonce in
        Ethereum_json_rpc.eth_get_transaction_count (sender, BlockParameter.Latest)
        >>= fun sender_nonce ->
        if Nonce.(compare sender_nonce nonce > 0) then
          nonce_too_low sender
        else
          fail Still_pending
      | Some receipt ->
        receipt
        |> confirmation_of_transaction_receipt
        |> check_confirmation_deep_enough

(* TODO: A better, non-linear state machine to get wanted transactions through.

   Obvious strategies don't work:
   - If I never re-send a transaction, but for some reason one transaction doesn't go through,
   then I deadlock.
   - If I always re-send a transaction, but never update the nonce, and some other client
   or some other transaction on the same client races me, then I deadlock.
   - If I always re-send a transaction, and I update the nonce if I see mine is out-of-date,
   then I can race myself and/or other transactions into sending multiple copies of a same
   transaction and end up spending many times over what I wanted to spend (very bad).
   - Whatever decisions I make based on what geth tells me, it can give me hints about things that are
   going on, but nothing it says is authoritative until it is, 30 minutes later (or 10 minutes, if I
   take the risk of lower security), and even what it does say doesn't have much consistency.
   - The safest would be to nurse each and every transaction to either completion or definite failure
   before even attempting the next one, but then that's only one transaction per account every 30
   minutes minimum, and maybe much worse depending on how "definite failure" is defined.
   This suggests that having multiple accounts could be a requirement for playing safe
   with smart contracts.

   One problem is that local geth (and/or, in a real network, whichever remote geth will eventually
   issue the blocks, sometimes just drop our signed transactions, for whatever reasons:
   not enough eth, not enough gas, gas price too low, nonce out of synch, network error,
   block-buying attack, etc.
   We have to resend, sometimes with updated gas price, sometimes with updated nonce,
   sometimes with updated contract parameters, etc.
   Yet, we should be wary of changing any thing about a transaction being sent,
   or we can race ourselves, and end up spending twice.
   A good strategy might take into account what did or didn't happen in not-fully-confirmed blocks,
   yet (obviously) would not consider anything confirmed until it's confirmed.
   It is unclear how best to deal with multiple queued transactions —
   the happy case of sending consecutive nonces automatically is great,
   but when things break down (including due to the aforementioned re-send issues)
   it's a hell that's hard to recover from, since new transactions will race the old ones,
   and any sequential dependency between them becomes quite tricky to enforce.

   It might be good to send multiple transactions atomically via a generic contract
   that makes a bunch of calls at once, so you only have to deal with a single network event,
   which makes one nursed transaction per 30 minute limit much more bearable,
   but also implies a better-than-trivial queueing strategy.
   Contracts could also help, onerously, with avoiding to spending twice in the context where
   you do want to be able to race yourself in case your have multiple strong obligations
   to fulfill in a short deadline, but the current chain is wobbly due to some attack.
   In some case, the right thing to do might be to consult back with the user,
   and ask them to add more ether, to update their gas price strategy, to deal with potentially
   or actually broken contracts, to watch the missing nodes of their personal database, etc.
   By default, we probably want to queue transactions one by one to avoid nonce-overlap issues;
   we may be more or less aggressive in terms of using nonces without partial or total confirmation
   from previous transactions, depending on the nature of the transactions.

   There is potentially a LOT of complexity, and if possible we want to partner with other people
   to define sound strategies... just that is a topic for itself, and there are sometimes games
   that people can play with lock out strategy via paying extra in gas to buy enough blocks to
   lock rivals out of a contract, etc. A generic strategy, DSL for strategies, etc., could be
   a research topic in itself. Sigh.

   Here, for now, we follow a very dumb strategy, of having only one active transaction per address.
   Furthermore, we only sign once, and resending blindly afterwards,
   trusting the gas computation, and trusting the nonce until it's found to be too low.

   TODO: implement an asynchronous way for the UI to peek at the status of a transaction
   while it's going along its slow progress.
*)
module TransactionTracker = struct
  open Lwter
  module Base = struct
    type context = unit (* TODO: Revision tracking actor *)
    module Key = struct
      [@@@warning "-39"]
      type t= { user : Address.t; revision : Revision.t } [@@deriving yojson]
      include (YojsonMarshalable(struct
                 type nonrec t = t
                 let yojsoning = {to_yojson;of_yojson}
                 let marshaling = marshaling2
                                    (fun {user;revision} -> user,revision)
                                    (fun user revision -> {user;revision})
                                    Address.marshaling Revision.marshaling
               end): YojsonMarshalableS with type t := t)
    end
    type key = Key.t
    let key_prefix = "ETTT"
    module State = TransactionStatus
    type state = State.t
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string
    (* TODO: inspection? cancellation? *)
    type t = Key.t * FinalTransactionStatus.t Lwt.t
    let make_activity () key saving state =
      let rec update (status : TransactionStatus.t) =
        saving status >>= Db.committing >>= loop
      and continue (status : OngoingTransactionStatus.t) =
        TransactionStatus.Ongoing status |> update
      and finalize (status : FinalTransactionStatus.t) =
        (* TODO: remove the request from the ongoing_transactions set! *)
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
        finalize (Failed (transaction_status, error))
      and loop (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
        (*Logging.log "Stepping into %s" (TransactionStatus.to_yojson_string status);*)
        match status with
        | Ongoing ongoing ->
          (match ongoing with
           | Wanted {operation; value; gas_limit} ->
             make_signed_transaction key.Key.user operation value gas_limit
             >>= (function
               | Ok (t,c) -> OngoingTransactionStatus.Signed (t,c) |> continue
               | Error error -> invalidate ongoing error)
           | Signed (transaction, signed) ->
             (transaction, signed)
             |> Lwt_exn.(run_lwt
                           (retry ~retry_window:0.05 ~max_window:30.0 ~max_retries:None
                              (trying send_and_confirm_transaction
                               >>> (function
                                 | Ok confirmation ->
                                   return (Ok confirmation)
                                 | Error NonceTooLow ->
                                   return (Error NonceTooLow)
                                 | Error e -> fail e))))
             >>= (function
               | Ok confirmation ->
                 FinalTransactionStatus.Confirmed (transaction, confirmation) |> finalize
               | Error NonceTooLow ->
                 OngoingTransactionStatus.Wanted (Transaction.pre_transaction transaction) |> continue
               | Error error -> invalidate ongoing error))
        | Final x -> return x in
      key, loop state
  end
  include PersistentActivity(Base)
  module Key = Base.Key
end

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }, yojson]
  let marshaling =
    marshaling3
      (fun { address; transaction_counter; ongoing_transactions} ->
         address, transaction_counter, RevisionSet.elements ongoing_transactions)
      (fun address transaction_counter ongoing_transactions_keys ->
         {address; transaction_counter;
          ongoing_transactions= RevisionSet.of_list ongoing_transactions_keys})
      Address.marshaling Revision.marshaling (list_marshaling Revision.marshaling)
  include (TrivialPersistable (struct
             type nonrec t = t
             let marshaling = marshaling
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
end

module User = struct
  module Base = struct
    type context = unit
    module Key = Address
    type key = Key.t
    let key_prefix = "ETUS"
    module State = UserState
    type state = UserState.t
    type t = state SimpleActor.t
    let make_default_state _context user =
      UserState.
        { address= user
        ; transaction_counter= Revision.zero
        ; ongoing_transactions= RevisionSet.empty }
    let resume_transactions context user (state : State.t) =
      RevisionSet.iter
        (fun revision -> TransactionTracker.get context {user; revision} |> ignore)
        state.ongoing_transactions
    let make_activity context user saving state =
      let with_transaction transform = Lwter.(transform >>> saving) in
      let actor = SimpleActor.make ~with_transaction state in
      (* TODO: maybe just use Lwt_mvar.create state and leave it to users to transact on it ? *)
      resume_transactions context user state; (* TODO: pass the actor as context to that? *)
      actor
  end
  include PersistentActivity(Base)
end

module UserAsyncAction = AsyncAction(UserState)

let user_action address action input =
  SimpleActor.action (User.get () address) action input

let add_ongoing_transaction : (OngoingTransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
  fun transaction_status user_state ->
    let open Lwt in
    let transaction_counter = user_state.transaction_counter in
    TransactionTracker.(make () Key.{user= user_state.address; revision= transaction_counter}
                          (Lwter.const (TransactionStatus.Ongoing transaction_status)))
    >>= fun tracker ->
    UserAsyncAction.return tracker
      (user_state
       |> Lens.modify UserState.lens_transaction_counter Revision.(add one)
       |> Lens.modify UserState.lens_ongoing_transactions
            (RevisionSet.add transaction_counter))

let unlock_account ?(duration=5) address =
  catching_arr keypair_of_address address >>= fun keypair ->
  Logging.log "unlock_account %s" (Address.to_0x address);
  Ethereum_json_rpc.personal_unlock_account (address, keypair.password, Some duration)
  >>= function
  | true -> return ()
  | false -> fail Bad_password

let issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) UserAsyncAction.arr =
  fun (t, s) -> OngoingTransactionStatus.Signed (t, s) |> add_ongoing_transaction

let track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) UserAsyncAction.arr =
  let open Lwt in
  fun (_, promise) state -> promise >>= fun x -> UserAsyncAction.return x state

let check_transaction_confirmed :
  (FinalTransactionStatus.t, Transaction.t * Confirmation.t) UserAsyncAction.arr
  = function
    | FinalTransactionStatus.Confirmed (t, c) -> UserAsyncAction.return (t, c)
    | FinalTransactionStatus.Failed (t, e) -> UserAsyncAction.fail (TransactionFailed (t, e))

let confirm_transaction =
  let open UserAsyncAction in
  issue_transaction
  >>> track_transaction
  >>> check_transaction_confirmed

let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (sender, recipient, amount) =
  make_signed_transaction sender (Operation.TransferTokens recipient) amount transfer_gas_limit

module Test = struct
(*
     let%test "exercise main_chain_block_notification_stream" =
     let open Revision in
     let open Lwt_exn in
     let current_block = ref zero in (* Mock for current mainchain block num *)
     let throw_error = ref None in (* Whether to throw when getting block *)
     let get_block ?timeout ?log () =
     ignore timeout ; ignore log ;
     match !throw_error with
     | None -> let cb = !current_block in current_block := succ cb; return cb
     | Some e -> throw_error := None; fail e in
     let start_block = of_int 10 in
     Lwt_exn.run
     (of_lwt (main_chain_block_notification_stream ~start_block ~get_block)
     >>> catching_lwt (AsyncStream.split 2)
     >>> (fun (l, s) ->
     assert(l = [start_block; add one start_block]);
     catching_lwt (AsyncStream.split 1) s)
     >>> (fun (l, _s) ->
     assert(l = [add start_block (of_int 2)]);
     return true
     (* Deals gracefully with errors? *)
     (*         throw_error := Some (Internal_error "You FAIL!!!"); *)
     (*         trying (catching_lwt (AsyncStream.split 1)) s)
     >>> (function
     | Error (Internal_error "You FAIL!!!") -> return true
     | Error e -> raise e
     | Ok (l, _) -> raise (Internal_error "blah %s, _" (string_of_yojson (`List (List.map Revision.to_string l))))) *)
     ))
     ()
  *)
end
