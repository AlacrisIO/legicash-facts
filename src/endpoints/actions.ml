(* actions.ml -- actions behind the endpoints *)

(* TODO: actually separate actions between client-side actions and facilitator-side actions,
   running in two different sets of threads,
   if possible on two different processes on two different machines,
   with access to different credentials.
*)

open Lwt

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Types

open Legilogic_ethereum

open Legicash_lib
open Side_chain
open Side_chain_facilitator
open Side_chain_user

open Accounts

(* user account balance on Trent *)
type side_chain_account_state =
  { address : Address.t
  ; user_name : string
  ; balance : TokenAmount.t
  ; revision : Revision.t
  }
[@@deriving to_yojson]

(* user account on Ethereum *)
type main_chain_account_state =
  { address : Address.t
  ; balance : TokenAmount.t
  ; revision : Revision.t
  }
[@@deriving to_yojson]

type user_status =
  { side_chain_account : side_chain_account_state
  ; main_chain_account : main_chain_account_state
  }

(* user account after a deposit or withdrawal, with transaction hash on the net *)
type transaction_result =
  { side_chain_account_state : side_chain_account_state
  ; side_chain_tx_revision : Revision.t
  ; main_chain_confirmation : Main_chain.Confirmation.t
  }
[@@deriving to_yojson]

type payment_result =
  { sender_account : side_chain_account_state
  ; recipient_account : side_chain_account_state
  ; amount_transferred : TokenAmount.t
  ; side_chain_tx_revision : Revision.t
  }
[@@deriving to_yojson]

type tps_result =
  { transactions_per_second : int
  ; time : string
  }
[@@deriving to_yojson]

let error_json fmt =
  Printf.ksprintf (fun x -> `Assoc [("error",`String x)]) fmt

(* table of id's to Lwt threads *)
let (id_to_thread_tbl : (int,yojson Lwt.t) Hashtbl.t) = Hashtbl.create 1031

(* add Lwt.t thread to table, return its id *)
let add_main_chain_thread thread =
  let thread_find_limit = 100000 in
  let rec find_thread_id n =
    if n > thread_find_limit then
      bork "Can't find id for main chain thread"
    else
      let id = Random.int 100000000 in
      if Hashtbl.mem id_to_thread_tbl id then
        find_thread_id (n + 1)
      else (
        Hashtbl.add id_to_thread_tbl id thread;
        let uri = Format.sprintf "api/thread?id=%d" id in
        `Assoc [("result",`String uri);])
  in
  find_thread_id 0

(* get Merkle proof for side chain transaction identified by tx_revision *)
let get_proof tx_revision : yojson =
  let tx_revision_t = Revision.of_int tx_revision in
  let trent_state = get_trent_state () in
  let transactions = trent_state.current.transactions in
  match TransactionMap.Proof.get tx_revision_t transactions with
  | None ->
    error_json "Cannot provide proof for tx-revision: %d" tx_revision
  | Some proof ->
    TransactionMap.Proof.to_yojson proof

(* lookup id in thread table; if completed, return result, else return boilerplate *)
let apply_main_chain_thread id : yojson =
  try
    let thread = Hashtbl.find id_to_thread_tbl id in
    match state thread with
    | Return json -> json
    | Fail exn ->
      error_json "Thread exception: %s\nStack: %s"
        (Printexc.to_string exn)
        (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
    | Sleep ->
      `Assoc [("result",`String "The operation is pending")]
  with Not_found ->
    error_json "Thread %d not found" id

let user_state_from_address address_t =
  try
    Hashtbl.find address_to_user_state_tbl address_t
  with Not_found ->
    bork "Could not find user state for address: %s" (Address.to_0x_string address_t)

let update_user_state address_t user_state =
  Hashtbl.replace address_to_user_state_tbl address_t user_state

let deposit_to_trent address amount =
  let open Ethereum_transaction.Test in
  let user_state = ref (user_state_from_address address) in
  let thread =
    unlock_account address
    >>= fun _unlock_json ->
    UserAsyncAction.run_lwt user_state deposit (trent_address, amount)
    >>= fun signed_request ->
    update_user_state address !user_state;
    Lwt_exn.run_lwt process_request (signed_request, false)
    >>= fun confirmation ->
    let tx_revision = confirmation.tx_header.tx_revision in
    (* get transaction hash for main chain *)
    let operation = signed_request.payload.operation in
    let main_chain_confirmation =
      match operation with
      | Deposit details -> details.main_chain_deposit_confirmation
      | _ -> bork "Expected deposit request"
    in
    (* get user account info on Trent *)
    let user_account_on_trent = get_user_account address in
    let balance = user_account_on_trent.balance in
    let revision = user_account_on_trent.account_revision in
    let user_name = get_user_name address in
    let side_chain_account_state = { address
                                   ; user_name
                                   ; balance
                                   ; revision
                                   }
    in
    let side_chain_tx_revision = tx_revision in
    let deposit_result = { side_chain_account_state
                         ; side_chain_tx_revision
                         ; main_chain_confirmation } in
    return (transaction_result_to_yojson deposit_result)
  in
  add_main_chain_thread thread

let withdrawal_from_trent address amount =
  let open Ethereum_transaction.Test in
  let user_state = ref (user_state_from_address address) in
  let user_account_on_trent = get_user_account address in
  let balance = user_account_on_trent.balance in
  if TokenAmount.compare amount balance > 0 then
    bork "Insufficient balance to withdraw specified amount";
  let thread =
    unlock_account address
    >>= fun _unlock_json ->
    UserAsyncAction.run_lwt user_state withdrawal (trent_address, amount)
    >>= fun signed_request ->
    update_user_state address !user_state;
    Lwt_exn.run_lwt process_request (signed_request, false)
    >>= fun confirmation ->
    let tx_revision = confirmation.tx_header.tx_revision in
    let trent_state = get_trent_state () in
    push_side_chain_action_to_main_chain trent_state confirmation !user_state
    >>= fun (maybe_main_chain_confirmation, user_state2) ->
    (* update user state, which refers to main chain state *)
    user_state := user_state2;
    update_user_state address user_state2;
    let main_chain_confirmation =
      match maybe_main_chain_confirmation with
      | Error exn -> raise exn
      | Ok confirmation -> confirmation
    in
    let user_account_on_trent = get_user_account address in
    let balance = user_account_on_trent.balance in
    let revision = user_account_on_trent.account_revision in
    let user_name = get_user_name address in
    let side_chain_account_state = { address
                                   ; user_name
                                   ; balance
                                   ; revision
                                   }
    in
    let side_chain_tx_revision = tx_revision in
    let withdrawal_result = { side_chain_account_state
                            ; side_chain_tx_revision
                            ; main_chain_confirmation
                            }
    in
    return (transaction_result_to_yojson withdrawal_result)
  in
  add_main_chain_thread thread

let get_balance_on_trent address =
  let user_account_on_trent = get_user_account address in
  let balance = user_account_on_trent.balance in
  let user_name = get_user_name address in
  let revision = user_account_on_trent.account_revision in
  let side_chain_account_state = { address
                                 ; user_name
                                 ; balance
                                 ; revision
                                 }
  in
  side_chain_account_state_to_yojson side_chain_account_state

let get_status_on_trent_and_main_chain address =
  let open Ethereum_transaction in
  let side_chain_json = get_balance_on_trent address in
  send_balance_request_to_net address
  >>= fun balance_json ->
  if YoJson.mem "error" balance_json then
    return (error_json "Could not get main chain balance for address %s: %s"
              (Address.to_0x_string address) YoJson.(member "error" balance_json |> to_string))
  else
    let balance =
      TokenAmount.of_string YoJson.(member "result" balance_json |> to_string)
    in
    get_transaction_count address
    >>= fun revision_json ->
    if YoJson.mem "error" revision_json then
      return (error_json "Could not get main chain revision for address %s: %s"
                (Address.to_0x_string address) YoJson.(member "error" revision_json |> to_string))
    else
      let revision =
        Revision.of_string YoJson.(member "result" revision_json |> to_string)
      in
      let main_chain_account =
        { address
        ; balance
        ; revision
        }
      in
      return (`Assoc [("side_chain_account",side_chain_json)
                     ; ("main_chain_account",main_chain_account_state_to_yojson main_chain_account)
                     ])

let get_all_balances_on_trent () =
  let make_balance_json address_t (account : Side_chain.AccountState.t) accum =
    let user_name = get_user_name address_t in
    let account_state = { address = address_t
                        ; user_name
                        ; balance = account.balance
                        ; revision = account.account_revision
                        }
    in account_state::accum
  in
  let trent_state = get_trent_state () in
  let side_chain_account_states = AccountMap.fold make_balance_json trent_state.current.accounts [] in
  let sorted_side_chain_account_states =
    List.sort
      (fun bal1 bal2 -> String.compare bal1.user_name bal2.user_name)
      side_chain_account_states
  in
  let sorted_balances_json = List.map side_chain_account_state_to_yojson sorted_side_chain_account_states in
  return (`List sorted_balances_json)

(* convert Request to nice JSON *)
let make_operation_json address (revision:Revision.t) (request:Request.t) =
  let revision_field = ("facilitator_revision",`Int (Revision.to_int revision)) in
  match request.operation with
    Deposit details ->
    `Assoc
      [ ("transaction_type",`String "deposit")
      ; revision_field
      ; ("address",`String (Address.to_0x_string address))
      ; ("amount",`Int (TokenAmount.to_int details.deposit_amount))
      ]
  | Payment details ->
    `Assoc
      [ ("transaction_type",`String "payment")
      ; revision_field
      ; ("sender",`String (Address.to_0x_string address))
      ; ("recipient",`String (Address.to_hex_string details.payment_invoice.recipient))
      ; ("amount",`Int (TokenAmount.to_int details.payment_invoice.amount))
      ]
  | Withdrawal details ->
    `Assoc
      [ ("transaction_type",`String "withdrawal")
      ; revision_field
      ; ("address",`String (Address.to_0x_string address))
      ; ("amount",`Int (TokenAmount.to_int details.withdrawal_amount))
      ]

let get_recent_transactions_on_trent address maybe_limit =
  let exception Reached_limit of yojson list in
  let trent_state = get_trent_state () in
  let all_transactions = trent_state.current.transactions in
  let get_operation_for_address _rev (transaction:Transaction.t) ((count,transactions) as accum) =
    if Option.is_some maybe_limit &&
       count >= Option.get maybe_limit then
      raise (Reached_limit transactions);
    let request = (transaction.signed_request).payload in
    let requester = request.rx_header.requester in
    if requester = address then
      (count+1,make_operation_json address transaction.tx_header.tx_revision request::transactions)
    else
      accum
  in
  let transactions =
    try
      let _,ops =
        TransactionMap.fold_right
          get_operation_for_address
          all_transactions (0,[])
      in ops
    with Reached_limit ops -> ops
  in
  `List transactions

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
  if sender = recipient then
    bork "Sender and recipient are the same";
  let sender_state = user_state_from_address sender in
  let sender_account = get_user_account sender in
  if TokenAmount.compare sender_account.balance amount < 0 then
    bork "Sender has insufficient balance to make this payment";
  let sender_state_ref = ref sender_state in
  UserAsyncAction.run_lwt sender_state_ref payment
    (trent_address, recipient, amount)
  >>= fun signed_request ->
  update_user_state sender !sender_state_ref;
  Lwt_exn.run_lwt process_request (signed_request, false)
  >>= fun transaction ->
  (* set timestamp, now that all processing on Trent is done *)
  payment_timestamp ();
  (* remaining code is preparing response *)
  let tx_revision = transaction.tx_header.tx_revision in
  let sender_name = get_user_name sender in
  let recipient_name = get_user_name recipient in
  let sender_account = get_user_account sender in
  let recipient_account = get_user_account recipient in
  let make_account_state address name (account : AccountState.t) =
    { address
    ; user_name = name
    ; balance = account.balance
    ; revision = account.account_revision
    }
  in
  let sender_account = make_account_state sender sender_name sender_account in
  let recipient_account = make_account_state recipient recipient_name recipient_account in
  let side_chain_tx_revision = tx_revision in
  let payment_result =
    { sender_account
    ; recipient_account
    ; amount_transferred = amount
    ; side_chain_tx_revision
    }
  in
  return (payment_result_to_yojson payment_result)

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
      bork "Timestamps array is not big enough"
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
