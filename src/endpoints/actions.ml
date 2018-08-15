(* actions.eliom -- actions behind the endpoints *)
(* TODO: actually separate actions between client-side actions and facilitator-side actions,
   running in two different sets of threads,
   if possible on two different processes on two different machines,
   with access to different credentials.
*)

open Lwt

open Legicash_lib

open Lib
open Crypto
open Db
open Side_chain
open Side_chain_facilitator
open Side_chain_user

open Accounts

(* types have to be JSON-friendly, so don't use, for example, TokenAmount.t *)

(* user account balance on Trent *)
type user_account_state =
  { address : string
  ; user_name : string
  ; balance : int
  }
[@@deriving yojson]

(* confirmed transaction on main chain *)
type main_chain_confirmation =
  { transaction_hash : string
  ; transaction_index : int
  ; block_number : int
  ; block_hash : string
  }
[@@deriving yojson]

(* user account after a deposit or withdrawal, with transaction hash on the net *)
type transaction_result =
  { user_account_state : user_account_state
  ; side_chain_tx_revision : int64
  ; main_chain_confirmation : main_chain_confirmation
  }
[@@deriving yojson]

type payment_result =
  { sender_account : user_account_state
  ; recipient_account : user_account_state
  ; amount_transferred : int
  ; side_chain_tx_revision : int64
  }
[@@deriving yojson]

type tps_result =
  { transactions_per_second : int
  ; time : string
  }
[@@deriving yojson]


let ensure_ok = function state, Ok x -> (state, x) | _state, Error y -> raise y

let ( |^>> ) v f = v |> f |> ensure_ok
(* Lwt-monadic version of |^>> *)
let ( |^>>+ ) v f = v |> f >>= Lwt.map ensure_ok

(* table of id's to Lwt threads *)
let id_to_thread_tbl = Hashtbl.create 1031

(* add Lwt.t thread to table, return its id *)
let add_main_chain_thread thread =
  let thread_find_limit = 100000 in
  let rec find_thread_id n =
    if n > thread_find_limit then
      raise (Internal_error "Can't find id for main chain thread")
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
let get_proof tx_revision : Yojson.Safe.json =
  let tx_revision_t = Revision.of_int tx_revision in
  let operations = !trent_state.current.operations in
  match ConfirmationMap.Proof.get tx_revision_t operations with
  | None ->
    `Assoc [("error",`String (Format.sprintf "Cannot provide proof for tx-revision: %d" tx_revision))]
  | Some proof ->
    ConfirmationMap.Proof.to_yojson proof

(* lookup id in thread table; if completed, return result, else return boilerplate *)
let apply_main_chain_thread id : Yojson.Safe.json =
  try
    let thread = Hashtbl.find id_to_thread_tbl id in
    match state thread with
    | Return json -> json
    | Fail exn -> raise exn
    | Sleep ->
      `Assoc [("result",`String "The operation is pending")]
  with Not_found ->
    `Assoc [("error",`String (Format.sprintf "Thread %d not found" id))]

let user_state_from_address address_t =
  try
    Hashtbl.find address_to_user_state_tbl address_t
  with Not_found ->
    raise (Internal_error (Format.sprintf "Could not find user state for address: 0x%s"
                             (Address.to_hex_string address_t)))

(* convert main chain confirmation to JSON-friendly types *)
let jsonable_confirmation_of_confirmation (confirmation : Main_chain.Confirmation.t) =
  { transaction_hash = "0x" ^ (confirmation.transaction_hash |> Digest.to_hex_string)
  ; transaction_index = confirmation.transaction_index |> UInt64.to_int
  ; block_number = confirmation.block_number |> Revision.to_int
  ; block_hash = "0x" ^ (confirmation.block_hash |> Digest.to_hex_string)
  }

let json_of_exn exn = `Assoc [("error",`String (Printexc.to_string exn))]

let deposit_to_trent address amount =
  let open Ethereum_transaction.Test in
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_state = user_state_from_address address_t in
  let thread =
    unlock_account address_t
    >>= fun _unlock_json ->
    (user_state, (trent_address,TokenAmount.of_int amount))
    |> deposit |> Lwt.map ensure_ok
    >>= fun (user_state1, signed_request) ->
    (!trent_state, signed_request)
    |> process_request |> Lwt.map ensure_ok
    >>= fun (trent_state1,signed_confirmation) ->
    let confirmation = signed_confirmation.payload in
    let tx_revision = confirmation.tx_header.tx_revision in
    Hashtbl.replace address_to_user_state_tbl address_t user_state1;
    set_trent_state trent_state1;
    (* get transaction hash for main chain *)
    let operation = signed_request.payload.operation in
    let main_chain_confirmation =
      match operation with
      | Deposit details -> jsonable_confirmation_of_confirmation details.main_chain_deposit_confirmation
      | _ -> raise (Internal_error "Expected deposit request")
    in
    (* get user account info on Trent *)
    let user_account_on_trent = AccountMap.find address_t !trent_state.current.accounts in
    let balance = TokenAmount.to_int (user_account_on_trent.balance) in
    let user_name = get_user_name address_t in
    let user_account_state = { address
                             ; user_name
                             ; balance
                             }
    in
    let side_chain_tx_revision = Revision.to_int64 tx_revision in
    let deposit_result = { user_account_state
                         ; side_chain_tx_revision
                         ; main_chain_confirmation
                         }
    in
    return (transaction_result_to_yojson deposit_result)
  in
  add_main_chain_thread thread

let withdrawal_from_trent address amount =
  let open Ethereum_transaction.Test in
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_state = user_state_from_address address_t in
  let user_account_on_trent = AccountMap.find address_t !trent_state.current.accounts in
  let balance = user_account_on_trent.balance in
  if TokenAmount.compare (TokenAmount.of_int amount) balance > 0 then
    raise (Internal_error "Insufficient balance to withdraw specified amount");
  let thread =
    unlock_account address_t
    >>= fun _unlock_json ->
    (user_state, (trent_address,TokenAmount.of_int amount))
    |> withdrawal |> Lwt.map ensure_ok
    >>= fun (user_state1, signed_request1) ->
    (!trent_state,signed_request1)
    |> process_request |> Lwt.map ensure_ok
    >>= fun (trent_state2, signed_confirmation2) ->
    let confirmation = signed_confirmation2.payload in
    let tx_revision = confirmation.tx_header.tx_revision in
    (* update trent state *)
    set_trent_state trent_state2;
    push_side_chain_action_to_main_chain trent_state2 (user_state1,signed_confirmation2)
    >>= fun (user_state2, maybe_main_chain_confirmation) ->
    (* update user state, which refers to main chain state *)
    Hashtbl.replace address_to_user_state_tbl address_t user_state2;
    let main_chain_confirmation =
      match maybe_main_chain_confirmation with
      | Error exn -> raise exn
      | Ok confirmation -> jsonable_confirmation_of_confirmation confirmation
    in
    let user_account_on_trent = AccountMap.find address_t !trent_state.current.accounts in
    let balance = TokenAmount.to_int (user_account_on_trent.balance) in
    let user_name = get_user_name address_t in
    let user_account_state = { address
                             ; user_name
                             ; balance
                             }
    in
    let side_chain_tx_revision = Revision.to_int64 tx_revision in
    let withdrawal_result = { user_account_state
                            ; side_chain_tx_revision
                            ; main_chain_confirmation
                            }
    in
    return (transaction_result_to_yojson withdrawal_result)
  in
  add_main_chain_thread thread

let get_balance_on_trent address =
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_account_on_trent = AccountMap.find address_t !trent_state.current.accounts in
  let balance = TokenAmount.to_int (user_account_on_trent.balance) in
  let user_name = get_user_name address_t in
  let user_account_state = { address
                           ; user_name
                           ; balance
                           }
  in
  user_account_state_to_yojson user_account_state

let get_all_balances_on_trent () =
  let make_balance_json address_t (account : Side_chain.AccountState.t) accum =
    let user_name = get_user_name address_t in
    let account_state = { address = Ethereum_util.hex_string_of_address address_t
                        ; user_name
                        ; balance = TokenAmount.to_int account.balance
                        }
    in account_state::accum
  in
  let user_account_states = AccountMap.fold make_balance_json !trent_state.current.accounts [] in
  let sorted_user_account_states =
    List.sort
      (fun bal1 bal2 -> String.compare bal1.user_name bal2.user_name)
      user_account_states
  in
  let sorted_balances_json = List.map user_account_state_to_yojson sorted_user_account_states in
  `List sorted_balances_json

(* convert Request to nice JSON *)
let make_operation_json address (revision:Revision.t) (request:Request.t) =
  let revision_field = ("facilitator_revision",`Int (Revision.to_int revision)) in
  match request.operation with
    Deposit details ->
    `Assoc
      [ ("transaction_type",`String "deposit")
      ; revision_field
      ; ("address",`String address)
      ; ("amount",`Int (TokenAmount.to_int details.deposit_amount))
      ]
  | Payment details ->
    `Assoc
      [ ("transaction_type",`String "payment")
      ; revision_field
      ; ("sender",`String address)
      ; ("recipient",`String (Address.to_hex_string details.payment_invoice.recipient))
      ; ("amount",`Int (TokenAmount.to_int details.payment_invoice.amount))
      ]
  | Withdrawal details ->
    `Assoc
      [ ("transaction_type",`String "withdrawal")
      ; revision_field
      ; ("address",`String address)
      ; ("amount",`Int (TokenAmount.to_int details.withdrawal_amount))
      ]

let get_recent_transactions_on_trent address maybe_limit =
  let exception Reached_limit of Yojson.Safe.json list in
  let address_t = Ethereum_util.address_of_hex_string address in
  let all_operations = !trent_state.current.operations in
  let get_operation_for_address _rev (confirmation:Confirmation.t) ((count,operations) as accum) =
    if Option.is_some maybe_limit &&
       count >= Option.get maybe_limit then
      raise (Reached_limit operations);
    let request = (confirmation.signed_request).payload in
    let requester = request.rx_header.requester in
    if requester = address_t then
      (count+1,make_operation_json address confirmation.tx_header.tx_revision request::operations)
    else
      accum
  in
  let operations =
    try
      let _,ops =
        ConfirmationMap.fold_right
          get_operation_for_address
          all_operations (0,[])
      in ops
    with Reached_limit ops -> ops
  in
  `List operations



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
    raise (Internal_error "Sender and recipient are the same");
  let sender_address_t = Ethereum_util.address_of_hex_string sender in
  let recipient_address_t = Ethereum_util.address_of_hex_string recipient in
  let sender_state = Hashtbl.find address_to_user_state_tbl sender_address_t in
  let starting_accounts = !trent_state.current.accounts in
  let sender_account = AccountMap.find sender_address_t starting_accounts in
  if (TokenAmount.to_int sender_account.balance) < amount then
    raise (Internal_error "Sender has insufficient balance to make this payment");
  (sender_state, (trent_address, recipient_address_t, TokenAmount.of_int amount))
  |^>> payment
  |> fun (sender_state_after_payment, signed_request) ->
  Hashtbl.replace address_to_user_state_tbl sender_address_t sender_state_after_payment ;
  (!trent_state, signed_request)
  |> process_request |> Lwt.map ensure_ok
  >>= fun (trent_state_after_confirmation, signed_confirmation) ->
  set_trent_state trent_state_after_confirmation;
  (* set timestamp, now that all processing on Trent is done *)
  payment_timestamp ();
  (* remaining code is preparing response *)
  let confirmation = signed_confirmation.payload in
  let tx_revision = confirmation.tx_header.tx_revision in
  let sender_name = get_user_name sender_address_t in
  let recipient_name = get_user_name recipient_address_t in
  let accounts = !trent_state.current.accounts in
  let sender_account = AccountMap.find sender_address_t accounts in
  let recipient_account = AccountMap.find sender_address_t accounts in
  let make_account_state address_t name (account : AccountState.t) =
    { address = Ethereum_util.hex_string_of_address address_t
    ; user_name = name
    ; balance = TokenAmount.to_int account.balance
    }
  in
  let sender_account = make_account_state sender_address_t sender_name sender_account in
  let recipient_account = make_account_state recipient_address_t recipient_name recipient_account in
  let side_chain_tx_revision = Revision.to_int64 tx_revision in
  let payment_result =
    { sender_account
    ; recipient_account
    ; amount_transferred = amount
    ; side_chain_tx_revision
    }
  in
  return (payment_result_to_yojson payment_result)

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
      raise (Internal_error "Timestamps array is not big enough")
    else if payment_timestamps.(ndx) <= minute_ago then
      count
    else (* decrement, or wrap backwards *)
      count_transactions (if ndx = 0 then num_timestamps - 1 else ndx - 1) (count + 1)
  in
  let raw_count = count_transactions last_cursor 0 in
  let transactions_per_second = raw_count / 60 in
  let tm = Unix.localtime now in
  let time =
    Format.sprintf "%d:%02d:%02d %02d:%02d:%02d"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
  in
  let tps_result = { transactions_per_second
                   ; time
                   } in
  tps_result_to_yojson tps_result
