open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Persisting
open Types
open Signing
open Action
open Lwt_exn

open Ethereum_chain
open Ethereum_json_rpc

exception TransactionInvalidated of Transaction.t * yojson
exception NonceTooLow

module FinalTransactionStatus = struct
  [@warning "-39"]
  type t =
    [ `Confirmed of Transaction.t * Confirmation.t
    (* TODO: have a proper data structure, not "yojson" *)
    | `Invalidated of Transaction.t * yojson ]
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionStatus = struct
  [@warning "-39"]
  type t =
    [ `Wanted of PreTransaction.t
    | `Signed of Transaction.t * SignedTransaction.t
    | `Sent of Transaction.t * SignedTransaction.t * Digest.t
    | FinalTransactionStatus.t ]
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let pre_transaction : t -> PreTransaction.t = function
    | `Wanted p -> p
    | `Signed (tx, _) | `Sent (tx, _, _) | `Confirmed (tx, _) | `Invalidated (tx, _) ->
      Transaction.pre_transaction tx
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

let failed_operation operation sender value gas_limit =
  Transaction.
    { tx_header={sender;nonce=Nonce.zero;gas_price=TokenAmount.zero;gas_limit;value}
    ; operation }

let exn_to_yojson = function
  | Json_rpc.Rpc_error e -> Json_rpc.error_to_yojson e
  | e -> `String (Printexc.to_string e)

(* TODO: take as parameter a nonce-tracking actor passed around by the caller
   (ultimately, the user state for the given address *)
module TransactionTracker = struct
  [@warning "-39"]
  type t =
    { address : Address.t
    ; revision : Revision.t
    ; promise : FinalTransactionStatus.t Lwt.t
    ; get : unit -> TransactionStatus.t }
  let db_key address revision =
    Printf.sprintf "ETHT%s%s" (Address.to_big_endian_bits address) (Revision.to_big_endian_bits revision)
  (* TODO: do we really not need a mutex here? *)
  (* TODO: add a transaction between this and the UserState *)
  let trackers : (Address.t * Revision.t, t) Hashtbl.t = Hashtbl.create 64
  let make : Address.t -> Revision.t -> TransactionStatus.t -> t = fun address revision status ->
    let db_key = db_key address revision in
    let open Lwt in
    let open TransactionStatus in
    let status_ref = ref status in
    let get () = !status_ref in
    let rec continue (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
      match status with
      | `Wanted {operation; value; gas_limit} ->
        make_signed_transaction address operation value gas_limit
        >>= ((function
          | Ok s -> `Signed s
          | Error error -> invalidate ~error (failed_operation operation address value gas_limit))
         >> update)
      | `Signed ((transaction: Transaction.t), (SignedTransaction.{raw} as signed)) ->
        Lwt_exn.(run_lwt (retry ~retry_window:5.0 ~max_window:60.0 ~max_retries:None
                            (trying eth_send_raw_transaction
                             >>> function
                             | Ok _ as r -> return r
                             | Error (Json_rpc.Rpc_error {code= -32000; message="nonce too low"}) ->
                               return (Error NonceTooLow)
                             | Error e -> fail e))
                   raw)
        >>= ((function
          | Ok transaction_hash ->
            `Sent (transaction, signed, transaction_hash)
          | Error NonceTooLow ->
            `Wanted (Transaction.pre_transaction transaction)
          | Error error -> invalidate ~error transaction)
         >> update)
      | `Sent (transaction, signed, transaction_hash) ->
        (* TODO: Also add the possibility of invalidating the transaction,
           by e.g. querying the blockchain for the sending address's current nonce,
           and marking the transaction invalidated if a more recent nonce was found.
           TODO: retry sending after timeout if still not sent. *)
        Lwt_exn.run_lwt wait_for_confirmation (transaction, transaction_hash)
        >>= ((function
          | Ok confirmation -> `Confirmed (transaction, confirmation)
          | Error NonceTooLow ->
            `Wanted (Transaction.pre_transaction transaction)
          | Error error ->
            invalidate ~signed ~error transaction)
         >> update)
      (* TODO: send notifications for confirmed and invalidated. *)
      | `Confirmed x -> `Confirmed x |> return
      | `Invalidated x -> `Invalidated x |> return
    and invalidate ?signed ?hash ~error transaction =
      `Invalidated (transaction,
                    `Assoc
                      (Option.(to_list (map (fun s -> ("signed", SignedTransaction.to_yojson s)) signed))
                       @ Option.(to_list (map (fun h -> ("transaction_hash", Digest.to_yojson h)) hash))
                       @ [("error", exn_to_yojson error)]))
    and update (status : TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
      Db.put db_key (marshal_string status)
      >>= Db.commit
      >>= fun () ->
      status_ref := status;
      continue status in
    {address; revision; promise= continue status; get}
    |> fun tracker -> Hashtbl.replace trackers (address, revision) tracker; tracker
  (* TODO: create a memoization helper in Lib and use it here *)
  let load address revision =
    db_key address revision
    |> Db.get
    |> Option.get
    |> TransactionStatus.unmarshal_string
    |> make address revision
  let get address revision = memoize ~table:trackers (uncurry load) (address, revision)
  module P = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {address; revision} -> address, revision) get
        Address.marshaling Revision.marshaling
    let yojsoning = yojsoning_of_marshaling marshaling
  end
  include (TrivialPersistable (P) : PersistableS with type t := t)
end

module OngoingTransactions = struct
  include Trie.SimpleTrie (Revision) (TransactionTracker)
  let keys = bindings >> List.map fst
  let load address keys =
    List.map (fun key -> key, TransactionTracker.load address key) keys |> of_bindings
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
      (fun {address; transaction_counter; ongoing_transactions} ->
         address, transaction_counter, OngoingTransactions.keys ongoing_transactions)
      (fun address transaction_counter ongoing_transactions_keys ->
         {address; transaction_counter;
          ongoing_transactions= OngoingTransactions.load address ongoing_transactions_keys})
      Address.marshaling Revision.marshaling (list_marshaling Revision.marshaling)
  include (TrivialPersistable (struct
             type nonrec t = t
             let marshaling = marshaling
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
  let db_key address =
    "ETUS" ^ (Address.to_big_endian_bits address)
  let init address =
    { address
    ; transaction_counter= Revision.zero
    ; ongoing_transactions= OngoingTransactions.empty }
  let load address =
    Db.get (db_key address) |> Option.get |> unmarshal_string
  let save x = x |> marshal_string |> Db.put (db_key x.address)
  let user_table = Hashtbl.create 4
  let user_table_mutex = Lwt_mutex.create ()
  let make_user_actor = SimpleActor.make ~commit:Lwter.(save >>> Db.commit)
  let get address =
    Lwt_mutex.with_lock user_table_mutex
      (fun () ->
         Lwt.return
           (match Hashtbl.find_opt user_table address with
            | Some x -> x
            | None -> (try load address with Not_found -> init address) |> make_user_actor))
end

module UserAsyncAction = AsyncAction(UserState)

let user_action address action input =
  Lwt.(UserState.get address >>= fun actor -> SimpleActor.action actor action input)

let add_ongoing_transaction : (TransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
  fun transaction_status user_state ->
    Db.with_transaction
      (fun () ->
         let transaction_counter = user_state.transaction_counter in
         let tracker = TransactionTracker.make user_state.address transaction_counter transaction_status in
         user_state
         |> Lens.modify UserState.lens_transaction_counter Revision.(add one)
         |> Lens.modify UserState.lens_ongoing_transactions
              (OngoingTransactions.add transaction_counter tracker)
         |> fun new_state ->
         Lwt.bind (UserState.save new_state)
           (fun () -> UserAsyncAction.return tracker new_state))

let ensure_account_unlocked ?(duration=5) address =
  let password = password_of_address address in
  Logging.log "ensure_account_unlocked";
  Ethereum_json_rpc.personal_unlock_account (address, password, Some duration)
  >>= function
  | true -> return ()
  | false -> fail Bad_password

let unlock_account ?(duration=5) () state =
  UserAsyncAction.of_lwt_exn (ensure_account_unlocked ~duration) state.UserState.address state

let issue_transaction : (Transaction.t * SignedTransaction.t, TransactionTracker.t) UserAsyncAction.arr =
  fun x -> `Signed x |> add_ongoing_transaction

let track_transaction : (TransactionTracker.t, FinalTransactionStatus.t) UserAsyncAction.arr =
  fun {promise} state -> Lwt.bind promise (fun x -> UserAsyncAction.return x state)

let check_transaction_confirmed :
  (FinalTransactionStatus.t, Transaction.t * Confirmation.t) UserAsyncAction.arr
  = function
    | `Confirmed x -> UserAsyncAction.return x
    | `Invalidated (tx, yo) -> UserAsyncAction.fail (TransactionInvalidated (tx, yo))

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
