(* actions.ml -- actions behind the endpoints *)

(* TODO: actually separate actions between client-side actions and facilitator-side actions,
   running in two different sets of threads,
   if possible on two different processes on two different machines,
   with access to different credentials.
*)

open Legilogic_lib
open Action
open Lwt_exn
open Yojsoning
open Types
open Signing

open Legilogic_ethereum

open Legicash_lib
open Side_chain
open Side_chain_client
open Side_chain_user

open Accounts

(* user account after a deposit or withdrawal, with transaction hash on the net *)
type transaction_result =
  { side_chain_account_state : AccountState.t
  ; side_chain_tx_revision : Revision.t
  ; main_chain_confirmation : Main_chain.Confirmation.t
  }
[@@deriving to_yojson]

type payment_result =
  { sender_account : AccountState.t
  ; recipient_account : AccountState.t
  ; amount_transferred : TokenAmount.t
  ; side_chain_tx_revision : Revision.t
  }
[@@deriving to_yojson]

type tps_result =
  { transactions_per_second : int
  ; time : string
  }
[@@deriving to_yojson]

let trent_address = Signing.Test.trent_address

let error_json fmt =
  Printf.ksprintf (fun x -> `Assoc [("error",`String x)]) fmt

(* table of id's to Lwt threads *)
let (id_to_thread_tbl : (int,yojson Lwt.t) Hashtbl.t) = Hashtbl.create 1031

(* add Lwt.t thread to table, return its id *)
let add_main_chain_thread thread =
  let thread_find_limit = 100000 in
  let rec find_thread_id n =
    if n > thread_find_limit then
      Lib.bork "Can't find id for main chain thread"
    else
      let id = Random.int 100000000 in
      if Hashtbl.mem id_to_thread_tbl id then
        find_thread_id (n + 1)
      else (
        Hashtbl.add id_to_thread_tbl id thread;
        `Assoc [("result",`Assoc [("thread",`Int id)])])
  in
  find_thread_id 0

let thread_pending_json = `Assoc [("result",`String "The operation is pending")]

(* lookup id in thread table; if completed, return result, else return boilerplate *)
let apply_main_chain_thread id : yojson =
  try
    let thread = Hashtbl.find id_to_thread_tbl id in
    match Lwt.state thread with
    | Return json -> json
    | Fail exn ->
      error_json "Thread exception: %s\nStack: %s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
    | Sleep -> thread_pending_json
  with Not_found ->
    error_json "Thread %d not found" id

let user_state_from_address address_t =
  try
    Hashtbl.find address_to_user_state_tbl address_t
  with Not_found ->
    Lib.bork "Could not find user state for address: %s" (Address.to_0x_string address_t)

let update_user_state address_t user_state =
  Hashtbl.replace address_to_user_state_tbl address_t user_state


(* operations posted to facilitator *)

(* query operations *)

let get_proof tx_revision =
  UserQueryRequest.Get_proof { tx_revision }
  |> post_user_query_request_to_side_chain

let get_balance_on_trent address =
  UserQueryRequest.Get_account_balance { address }
  |> post_user_query_request_to_side_chain

let get_status_on_trent_and_main_chain address =
  UserQueryRequest.Get_account_state { address }
  |> post_user_query_request_to_side_chain

let get_all_balances_on_trent () =
  UserQueryRequest.Get_account_balances
  |> post_user_query_request_to_side_chain

let get_recent_user_transactions_on_trent address maybe_limit =
  UserQueryRequest.Get_recent_transactions { address; count = maybe_limit }
  |> post_user_query_request_to_side_chain

(* side-effecting operations *)

(* format deposit and withdrawal result *)
let make_transaction_result address tx_revision main_chain_confirmation =
  UserQueryRequest.Get_account_state { address }
  |> post_user_query_request_to_side_chain
  >>= fun account_state_json ->
  (* TODO: JSON to AccountState to JSON, is there a better way *)
  match AccountState.of_yojson account_state_json with
  | Error _ ->
    return (error_json "Could not get account state for withdrawing user")
  | Ok account_state ->
    let side_chain_account_state = account_state in
    let side_chain_tx_revision = tx_revision in
    let deposit_result = { side_chain_account_state
                         ; side_chain_tx_revision
                         ; main_chain_confirmation } in
    return (transaction_result_to_yojson deposit_result)

let deposit_to_trent address amount =
  let open Ethereum_transaction.Test in
  let user_state = ref (user_state_from_address address) in
  let thread () =
    unlock_account address
    >>= fun _unlock_json ->
    UserAsyncAction.run_lwt_exn user_state deposit (trent_address, amount)
    >>= fun signed_request ->
    update_user_state address !user_state;
    post_user_transaction_request_to_side_chain signed_request
    >>= fun transaction ->
    let tx_revision = transaction.tx_header.tx_revision in
    (* get transaction hash for main chain *)
    let operation = signed_request.payload.operation in
    let main_chain_confirmation =
      match operation with
      | Deposit details -> details.main_chain_deposit_confirmation
      | _ -> Lib.bork "Expected deposit request"
    in
    make_transaction_result address tx_revision main_chain_confirmation
  in
  add_main_chain_thread (run_lwt thread ())

let withdrawal_from_trent facilitator_address user_address amount =
  let open Ethereum_transaction.Test in
  let user_state = ref (user_state_from_address user_address) in
  let thread () =
    unlock_account user_address
    >>= fun _unlock_json ->
    UserAsyncAction.run_lwt_exn user_state withdrawal (facilitator_address, amount)
    >>= fun signed_request ->
    update_user_state user_address !user_state;
    post_user_transaction_request_to_side_chain signed_request
    >>= fun transaction ->
    (* TODO: move the push to server side *)
    let tx_revision = transaction.tx_header.tx_revision in
    let open Lwt in
    push_side_chain_withdrawal_to_main_chain facilitator_address transaction !user_state
    >>= fun (maybe_main_chain_confirmation, user_state2) ->
    (* update user state, which refers to main chain state *)
    user_state := user_state2;
    update_user_state user_address user_state2;
    let main_chain_confirmation =
      match maybe_main_chain_confirmation with
      | Error exn -> raise exn
      | Ok confirmation -> confirmation in
    make_transaction_result user_address tx_revision main_chain_confirmation
  in
  add_main_chain_thread (run_lwt thread ())

(* every payment generates a timestamp in this array, treated as circular buffer *)
(* should be big enough to hold one minute's worth of payments on a fast machine *)
let num_timestamps = 100000
let payment_timestamps = Array.make num_timestamps 0.0

(* offset where next timestamp goes *)
let payment_timestamps_cursor = ref 0

let payment_timestamp () =
  payment_timestamps.(!payment_timestamps_cursor) <- Unix.gettimeofday ();
  incr payment_timestamps_cursor;
  (* wrap if needed *)
  if !payment_timestamps_cursor >= num_timestamps then
    payment_timestamps_cursor := 0

let payment_on_trent sender recipient amount =
  let sender_state = user_state_from_address sender in
  let sender_state_ref = ref sender_state in
  UserAsyncAction.run_lwt_exn sender_state_ref payment (trent_address, recipient, amount)
  >>= fun signed_request ->
  update_user_state sender !sender_state_ref;
  post_user_transaction_request_to_side_chain signed_request
  >>= fun transaction ->
  (* set timestamp, now that all processing on Trent is done *)
  payment_timestamp ();
  (* remaining code is preparing response *)
  let tx_revision = transaction.tx_header.tx_revision in
  UserQueryRequest.Get_account_state { address = sender }
  |> post_user_query_request_to_side_chain
  >>= fun sender_account_json ->
  UserQueryRequest.Get_account_state { address = recipient }
  |> post_user_query_request_to_side_chain
  >>= fun recipient_account_json ->
  let maybe_sender_account = AccountState.of_yojson sender_account_json in
  let maybe_recipient_account = AccountState.of_yojson recipient_account_json in
  match maybe_sender_account,maybe_recipient_account with
  | Error _ ,_
  | _, Error _ ->
    return (error_json "Could not get account information for sender and recipient")
  | Ok sender_account, Ok recipient_account ->
  let side_chain_tx_revision = tx_revision in
  let payment_result =
    { sender_account
    ; recipient_account
    ; amount_transferred = amount
    ; side_chain_tx_revision
    }
  in
  return (payment_result_to_yojson payment_result)

(* other actions, not involving posting to server mailbox *)

(* Use a (subset) of ISO 8601 format, minus the timezone.
   TODO: Use Jane Street's Core.Std.Time instead.
*)
let string_of_timeofday tod =
  let tm = Unix.localtime tod in
  Format.sprintf "%d-%02d-%02dT%02d:%02d:%02d"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec

(* TODO: maybe have server track payment timestamps *)
let get_transaction_rate_on_trent () =
  (* start search from last timestamp *)
  let current_cursor = !payment_timestamps_cursor in
  let last_cursor =
    if current_cursor = 0 then
      num_timestamps - 1
    else
      current_cursor - 1
  in
  let now = Unix.gettimeofday () in
  let minute_ago = now -. 60.0 in
  let rec count_transactions ndx count =
    (* traversed entire array, shouldn't happen *)
    if ndx = current_cursor then
      Lib.bork "Timestamps array is not big enough"
    else if payment_timestamps.(ndx) <= minute_ago then
      count
    else (* decrement, or wrap backwards *)
      count_transactions (if ndx = 0 then num_timestamps - 1 else ndx - 1) (count + 1)
  in
  let raw_count = count_transactions last_cursor 0 in
  let transactions_per_second = raw_count / 60 in
  let time = string_of_timeofday now in
  let tps_result = { transactions_per_second ; time } in
  tps_result_to_yojson tps_result
