(* actions.eliom -- actions behind the endpoints *)

open Lwt

open Legicash_lib

open Legibase
open Lib
open Crypto
open Side_chain
open Side_chain_action

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
  ; main_chain_confirmation : main_chain_confirmation
  }
  [@@deriving yojson]

type payment_result =
  { sender_account : user_account_state
  ; recipient_account : user_account_state
  ; amount_transferred : int
  }
  [@@deriving yojson]

let ( |^>> ) v f = v |> f |> function state, Ok x -> (state, x) | state, Error y -> raise y
(* Lwt-monadic version of |^>> *)
let ( |^>>+ ) v f = v |> f >>= function (state, Ok x) -> return (state, x) | state, Error y -> raise y

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
    let keys =
      try
        Hashtbl.find address_to_keys_tbl address_t
      with Not_found -> raise (Internal_error "Can't find address for deposit")
    in
    let new_user_state = create_side_chain_user_state keys in
    Hashtbl.add address_to_user_state_tbl address_t new_user_state;
    new_user_state

(* convert main chain confirmation to JSON-friendly types *)
let jsonable_confirmation_of_confirmation (confirmation : Main_chain.confirmation) =
        { transaction_hash = "0x" ^ (confirmation.transaction_hash |> Digest.to_hex_string)
        ; transaction_index = confirmation.transaction_index |> Unsigned.UInt64.to_int
        ; block_number = confirmation.block_number |> Revision.to_int
        ; block_hash = "0x" ^ (confirmation.block_hash |> Digest.to_hex_string)
        }

let deposit_to_trent address amount =
  let open Side_chain_action in
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_state = user_state_from_address address_t in
  let thread =
    (user_state, (trent_address,TokenAmount.of_int amount))
    |^>>+ deposit
    >>= fun (user_state1, signed_request) -> (!trent_state,signed_request)
    |^>> confirm_request
    |> fun (trent_state1,signed_confirmation) ->
    Hashtbl.replace address_to_user_state_tbl address_t user_state1;
    trent_state := trent_state1;
    (* get transaction hash for main chain *)
    let operation = signed_request.payload.operation in
    let main_chain_confirmation =
      match operation with
        Deposit details -> jsonable_confirmation_of_confirmation details.main_chain_deposit_confirmation
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
    let deposit_result = { user_account_state
                         ; main_chain_confirmation
                         }
    in
    return (transaction_result_to_yojson deposit_result)
  in
  add_main_chain_thread thread

let withdrawal_from_trent address amount =
  let open Side_chain_action in
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_state = user_state_from_address address_t in
  let user_account_on_trent = AccountMap.find address_t !trent_state.current.accounts in
  let balance = user_account_on_trent.balance in
  if TokenAmount.compare (TokenAmount.of_int amount) balance > 0 then
    raise (Internal_error "Insufficient balance to withdraw specified amount");
  let thread =
    (user_state, (trent_address,TokenAmount.of_int amount))
    |^>>+
    withdrawal
    >>= fun (user_state1, signed_request1) ->
    (!trent_state,signed_request1)
    |^>>
    confirm_request
    |> fun (trent_state2, signed_confirmation2) ->
    (* update trent state *)
    trent_state := trent_state2;
    push_side_chain_action_to_main_chain trent_state2 (user_state1,signed_confirmation2)
    >>= fun (user_state2,maybe_main_chain_confirmation) ->
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
    let withdrawal_result = { user_account_state
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
  |^>> confirm_request
  |> fun (trent_state_after_confirmation, signed_confirmation) ->
  (* let confirmation_digest = Digest.make signed_confirmation in *)
  trent_state := trent_state_after_confirmation;
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
  let payment_result =
    { sender_account
    ; recipient_account
    ; amount_transferred = amount
    }
  in payment_result_to_yojson payment_result
