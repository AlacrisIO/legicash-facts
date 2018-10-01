open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Persisting
open Types
open Signing
open Action
open Lwt_exn
open Json_rpc

open Ethereum_chain
open Ethereum_json_rpc

module OngoingTransactionStatus = struct
  [@warning "-39"]
  type t =
    | Wanted of PreTransaction.t
    | Signed of Transaction.t * SignedTransaction.t
    | Sent of Transaction.t * SignedTransaction.t * Digest.t
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | Wanted p -> p
    | Signed (tx, _) | Sent (tx, _, _) -> Transaction.pre_transaction tx
end

module FinalTransactionStatus = struct
  [@warning "-39"]
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

(** Wait until a transaction has been confirmed by the main chain. *)
let wait_for_confirmation : (Transaction.t * Digest.t, Confirmation.t OrExn.t) Lwt_exn.arr =
  Lwt_exn.retry ~retry_window:0.01 ~max_window:60.0 ~max_retries:None
    (fun (transaction, transaction_hash) ->
       Ethereum_json_rpc.eth_get_transaction_receipt transaction_hash
       >>= (function
         | None ->
           let TxHeader.{sender;nonce} = transaction.Transaction.tx_header in
           Ethereum_json_rpc.eth_get_transaction_count (sender, BlockParameter.Pending)
           >>= fun sender_nonce ->
           if Nonce.(compare sender_nonce nonce > 0) then
             return (Error NonceTooLow)
           else
             fail Still_pending
         | Some x -> x |> confirmation_of_transaction_receipt |> check_confirmation_deep_enough
           >>= arr Result.return))

let send_transaction =
  transaction_to_parameters >> Ethereum_json_rpc.eth_send_transaction

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

let make_tx_header (sender, value, gas_limit) =
  (* TODO: get gas price and nonce from geth *)
  eth_gas_price ()
  >>= fun gas_price ->
  eth_get_transaction_count (sender, BlockParameter.Pending)
  >>= fun nonce ->
  return TxHeader.{sender; nonce; gas_price; gas_limit; value}

exception Missing_password
exception Bad_password

let sign_transaction : (Transaction.t, Transaction.t * SignedTransaction.t) Lwt_exn.arr =
  fun transaction ->
    let address = transaction.tx_header.sender in
    (try return (password_of_address address) with
     | Not_found ->
       Logging.log "Couldn't find password for %s" (nicknamed_string_of_address address);
       fail Missing_password)
    >>= fun password -> personal_sign_transaction (transaction_to_parameters transaction, password)
    >>= fun signed -> return (transaction, signed)

let make_signed_transaction sender operation value gas_limit =
  make_tx_header (sender, value, gas_limit)
  >>= fun tx_header ->
  sign_transaction Transaction.{tx_header; operation}

(* TODO: take as parameter a nonce-tracking actor passed around by the caller
   (ultimately, the user state for the given address *)
module TransactionTracker = struct
  open Lwter
  module Base = struct
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
    let key_prefix = "ETTT"
    type context = unit (* TODO: Revision tracking actor *)
    module State = TransactionStatus
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string
    (* TODO: inspection? cancellation? *)
    type t = FinalTransactionStatus.t Lwt.t
    let commit_behavior = Synchronous
    let on_commit = None
    let behavior () key actor =
      let (promise, notify) = Lwt.task () in
      let continue (status : OngoingTransactionStatus.t) =
        return (true, TransactionStatus.Ongoing status) in
      let finalize (status : FinalTransactionStatus.t) =
        Lwt.wakeup_later notify status;
        return (false, TransactionStatus.Final status) in
      let invalidate transaction_status error =
        finalize (Failed (transaction_status, error)) in
      let step () (status : TransactionStatus.t) : (bool * TransactionStatus.t) Lwt.t =
        (*Logging.log "Stepping into %s" (TransactionStatus.to_yojson_string status);*)
        match status with
        | Ongoing ongoing ->
          (match ongoing with
           | Wanted {operation; value; gas_limit} ->
             make_signed_transaction key.Key.user operation value gas_limit
             >>= (function
               | Ok (t,c) -> OngoingTransactionStatus.Signed (t,c) |> continue
               | Error error -> invalidate ongoing error)
           | Signed ((transaction: Transaction.t), (SignedTransaction.{raw} as signed)) ->
             Lwt_exn.(run_lwt (retry ~retry_window:5.0 ~max_window:60.0 ~max_retries:None
                                 (trying eth_send_raw_transaction
                                  >>> function
                                  | Ok _ as r -> return r
                                  | Error (Rpc_error {code= -32000; message="nonce too low"}) ->
                                    return (Error NonceTooLow)
                                  | Error e -> fail e))
                        raw)
             >>= (function
               | Ok transaction_hash ->
                 OngoingTransactionStatus.Sent (transaction, signed, transaction_hash) |> continue
               | Error NonceTooLow ->
                 OngoingTransactionStatus.Wanted (Transaction.pre_transaction transaction) |> continue
               | Error error -> invalidate ongoing error)
           | Sent (transaction, _signed, transaction_hash) ->
             (* TODO: Also add the possibility of invalidating the transaction,
                by e.g. querying the blockchain for the sending address's current nonce,
                and marking the transaction invalidated if a more recent nonce was found.
                TODO: retry sending after timeout if still not sent. *)
             Lwt_exn.run_lwt wait_for_confirmation (transaction, transaction_hash)
             >>= (function
               | Ok confirmation -> FinalTransactionStatus.Confirmed (transaction, confirmation) |> finalize
               | Error NonceTooLow ->
                 OngoingTransactionStatus.Wanted (Transaction.pre_transaction transaction) |> continue
               | Error error -> invalidate ongoing error))
        | Final x -> finalize x in
      let rec loop () =
        (*Logging.log "In the loop for %s!" (Key.to_yojson_string key);*)
        SimpleActor.action actor step ()
        >>= function
        | true -> loop ()
        | false -> Lwt.return_unit in
      promise, loop
  end
  include PersistentActivity(Base)
  module Key = Base.Key
end

module OngoingTransactions = struct
  (* TODO: implement and use SimpleTrieSet *)
  include Trie.SimpleTrie (Revision) (Unit)
  let keys = bindings >> List.map fst
  let of_keys = List.map (fun key -> key, ()) >> of_bindings
end

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: OngoingTransactions.t }
  [@@deriving lens { prefix=true }, yojson]
  let marshaling =
    marshaling3
      (fun { address; transaction_counter; ongoing_transactions} ->
         address, transaction_counter, OngoingTransactions.keys ongoing_transactions)
      (fun address transaction_counter ongoing_transactions_keys ->
         {address; transaction_counter;
          ongoing_transactions= OngoingTransactions.of_keys ongoing_transactions_keys})
      Address.marshaling Revision.marshaling (list_marshaling Revision.marshaling)
  include (TrivialPersistable (struct
             type nonrec t = t
             let marshaling = marshaling
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
end

module User = struct
  module Base = struct
    module Key = Address
    let key_prefix = "ETUS"
    type context = unit
    module State = UserState
    type t = State.t SimpleActor.t
    let make_default_state _context user =
      UserState.
        { address= user
        ; transaction_counter= Revision.zero
        ; ongoing_transactions= OngoingTransactions.empty }
    let resume_transactions context user actor () =
      let State.{ongoing_transactions} = SimpleActor.peek actor in
      OngoingTransactions.keys ongoing_transactions
      |> List.iter (fun revision -> TransactionTracker.get context {user; revision} |> ignore);
      Lwt.return_unit
    let commit_behavior = Asynchronous
    let on_commit = None
    let behavior context user actor = actor, resume_transactions context user actor
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
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
            (OngoingTransactions.add transaction_counter ()))

let unlock_account ?(duration=5) address =
  let password = password_of_address address in
  Logging.log "unlock_account %s" (Address.to_0x_string address);
  Ethereum_json_rpc.personal_unlock_account (address, password, Some duration)
  >>= function
  | true -> return ()
  | false -> fail Bad_password

let issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) UserAsyncAction.arr =
  fun (t, s) -> OngoingTransactionStatus.Signed (t, s) |> add_ongoing_transaction

let track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) UserAsyncAction.arr =
  let open Lwt in
  fun promise state -> promise >>= fun x -> UserAsyncAction.return x state

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
