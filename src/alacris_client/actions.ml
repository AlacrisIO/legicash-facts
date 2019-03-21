(* actions.ml -- actions behind the endpoints *)

(* TODO: actually separate actions between client-side actions and operator-side actions,
   running in two different sets of threads,
   if possible on two different processes on two different machines,
   with access to different credentials.
*)

open Legilogic_lib
open Action
open Lwt_exn
open Yojsoning
open Signing
open Types
open Json_rpc

open Legilogic_ethereum

open Alacris_lib
open Side_chain
open Side_chain_client
open Side_chain_user
open Side_chain_server_config

(* user account after a deposit or withdrawal, with transaction hash on the net *)
type transaction_result =
  { side_chain_account_state: AccountState.t
  ; side_chain_tx_revision:   Revision.t
  ; main_chain_confirmation:  Ethereum_chain.Confirmation.t
  ; request_guid:             RequestGuid.t
  ; requested_at:             Timestamp.t
  } [@@deriving to_yojson]

type tps_result =
  { transactions_per_second: int
  ; time:                    string
  } [@@deriving to_yojson]

let error_json fmt =
  Printf.ksprintf (fun x -> `Assoc [("error",`String x)]) fmt

(* table of id's to Lwt threads *)
let (id_to_thread_tbl : (int, yojson Lwt_exn.t) Hashtbl.t) = Hashtbl.create 1031

(* add Lwt.t thread to table, return its id *)
let add_main_chain_thread request_guid requested_at thread =
  let thread_find_limit = 100000 in
  let rec find_thread_id n =
    if n > thread_find_limit then
      Lib.bork "Can't find ID for main chain thread"
    else
      let id = Random.int 100000000 in
      if Hashtbl.mem id_to_thread_tbl id then
        find_thread_id (n + 1)
      else (
        Hashtbl.add id_to_thread_tbl id thread;
        let rg = RequestGuid.to_string request_guid
        and ra = Timestamp.to_0x       requested_at
        in
        `Assoc [("result", `Assoc [ ("thread",       `Int id)
                                  ; ("request_guid", `String rg)
                                  ; ("requested_at", `String ra)
                                  ])])
  in
  find_thread_id 0

let thread_pending_json = `Assoc [("result",`String "The operation is pending")]

(* lookup id in thread table; if completed, return result, else return boilerplate *)
let apply_main_chain_thread id : yojson =
  try
    Logging.log "Beginning of apply_main_chain_thread";
    let thread = Hashtbl.find id_to_thread_tbl id in
    match Lwt.state thread with
    (* TODO: make proper JSON RPC response *)
    | Return (Ok json) ->
       Logging.log "Branch 1 of apply_main_chain_thread";
       json
    | Return (Error e) ->
       Logging.log "Branch 2 of apply_main_chain_thread";
       `Assoc [("error", exn_to_yojson e)]
    | Fail exn ->
       Logging.log "Branch 3 of apply_main_chain_thread";
      error_json "Thread exception: %s\nStack: %s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
    | Sleep -> thread_pending_json
  with Not_found ->
    error_json "Thread %d not found" id

(* operations posted to operator *)

(* query operations *)

let get_proof tx_revision =
  UserQueryRequest.Get_proof { tx_revision }
  |> post_user_query_request

let get_balance_on ~operator address =
  ignore operator; (* TODO: refactor so it makes sense WTF? *)
  UserQueryRequest.Get_account_balance { address }
  |> post_user_query_request

let get_status_on_trent_and_main_chain address =
  UserQueryRequest.Get_account_state { address }
  |> post_user_query_request

let get_all_balances_on_trent () =
  UserQueryRequest.Get_account_balances
  |> post_user_query_request

let get_recent_user_transactions_on_trent address maybe_limit =
  UserQueryRequest.Get_recent_transactions { address; count = maybe_limit }
  |> post_user_query_request

(* side-effecting operations *)

(* format deposit and withdrawal result *)
let make_transaction_result (request_guid:            RequestGuid.t)
                            (requested_at:            Timestamp.t)
                            (address:                 Address.t)
                            (side_chain_tx_revision:  Revision.t)
                            (main_chain_confirmation: Ethereum_chain.Confirmation.t)
                          : yojson OrExn.t Lwt.t =
  UserQueryRequest.Get_account_state { address }
    |> post_user_query_request
    >>= fun account_state_json ->
      (* TODO: JSON to AccountState to JSON, is there a better way *)
      match AccountState.of_yojson (YoJson.member "account_state" account_state_json) with
        | Error _ ->
           error_json "Could not get account state for depositing or withdrawing user"
                      |> return
        | Ok account_state ->
           let rec side_chain_account_state = account_state
           and r = { side_chain_account_state
                   ; side_chain_tx_revision
                   ; main_chain_confirmation
                   ; request_guid
                   ; requested_at
                   }
           in return (transaction_result_to_yojson r)

(* The type 'a is DepositWanted or WithdrawalWanted *)
let schedule_transaction (request_guid: RequestGuid.t)
                         (user:         Address.t)
                         (transaction:  'a -> TransactionTracker.t UserAsyncAction.t)
                         (wanted:       Timestamp.t -> 'a)
                       : yojson =
  let requested_at = Timestamp.now () in

  add_main_chain_thread
    request_guid
    requested_at
    (User.transaction user transaction (wanted requested_at)
     >>= fun (transaction_commitment, main_chain_confirmation) ->
       let tx_revision = transaction_commitment.transaction.tx_header.tx_revision in
       make_transaction_result request_guid
                               requested_at
                               user
                               tx_revision
                               main_chain_confirmation)

let deposit_to ~(operator:       Address.t)
                (request_guid:   RequestGuid.t)
                (user:           Address.t)
                (deposit_amount: TokenAmount.t)
              : yojson =
  schedule_transaction request_guid user deposit @@ fun requested_at ->
    DepositWanted.{ operator; deposit_amount; request_guid; requested_at }

let withdrawal_from ~(operator:          Address.t)
                     (request_guid:      RequestGuid.t)
                     (user:              Address.t)
                     (withdrawal_amount: TokenAmount.t)
                   : yojson =
  schedule_transaction request_guid user withdrawal @@ fun requested_at ->
    WithdrawalWanted.{ operator; withdrawal_amount; request_guid; requested_at }

(* every payment generates a timestamp in this array, treated as circular buffer *)
(* should be big enough to hold one minute's worth of payments on a fast machine *)
let payment_timestamps = Array.make Side_chain_server_config.num_timestamps 0.0

(* offset where next timestamp goes *)
let payment_timestamps_cursor = ref 0

let payment_timestamp () =
  payment_timestamps.(!payment_timestamps_cursor) <- Unix.gettimeofday ();
  incr payment_timestamps_cursor;
  (* wrap if needed *)
  if !payment_timestamps_cursor >= Side_chain_server_config.num_timestamps then
    payment_timestamps_cursor := 0

(*TODO: move instrumentation to a proper place and leave it at that:
  let payment_on_trent sender recipient amount memo =
  schedule_transaction sender payment
  PaymentWanted.{operator; recipient; amount; memo; payment_expedited= false } *)

(* TODO: simplify this all away. Problem is that schedule_transaction cannot be used
   because two addresses are used. *)
let payment_on ~(operator:     Address.t)
                (request_guid: RequestGuid.t)
                (sender:       Address.t)
                (recipient:    Address.t)
                (amount:       TokenAmount.t)
                (memo:         string)
              : yojson =

  let requested_at = Timestamp.now () in

  add_main_chain_thread
    request_guid
    requested_at
    (User.action sender
                 payment
                 PaymentWanted.{ operator
                               ; recipient
                               ; amount
                               ; memo
                               ; payment_expedited = false
                               ; request_guid
                               ; requested_at
                               }

     >>= fun (_key, tracker_promise) ->
       (* set timestamp, now that initial processing on Trent is done *)
       payment_timestamp ();
       TransactionTracker.wait tracker_promise

     >>= fun (transaction_commitment, main_chain_confirmation) ->
       let tx_revision = transaction_commitment.transaction.tx_header.tx_revision in
       make_transaction_result request_guid
                               requested_at
                               sender
                               tx_revision
                               main_chain_confirmation)


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
      Side_chain_server_config.num_timestamps - 1
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
      count_transactions (if ndx = 0 then Side_chain_server_config.num_timestamps - 1 else ndx - 1) (count + 1)
  in
  let raw_count = count_transactions last_cursor 0 in
  let transactions_per_second = raw_count / 60 in
  let time = string_of_timeofday now in
  let tps_result = { transactions_per_second ; time } in
  tps_result_to_yojson tps_result
