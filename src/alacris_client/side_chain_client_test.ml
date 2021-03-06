(* test_endpoints.ml -- integration tests for endpoints *)

(* requires that the endpoints server (nginx + SCGI) is running *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

open Legilogic_lib
open Lib
open Signing
open Yojsoning
open Action

open Alacris_lib
open Side_chain
open Side_chain_operator

open Side_chain_client_lib

let _ = Config.set_application_name "alacris"
let _ = Logging.set_log_file "logs/alacris-client.log"

let _ =
  let keys_file_ref = ref ("demo-keys-small.json" |> Config.get_config_filename) in
  let args = ref [] in
  Arg.parse_argv Sys.argv
    [("--keys", Set_string keys_file_ref, "file containing keys to the managed accounts")]
    (fun x -> args := x :: !args)
    "side_chain_client.exe";
  register_file_keypairs !keys_file_ref


(* types to use as JSON in POST bodies *)

type deposit_withdrawal_json =
  { address : Address.t
  ; amount : TokenAmount.t }
[@@deriving to_yojson]

type address_json =
  { address : Address.t }
[@@deriving to_yojson]

type payment_json =
  { sender : Address.t
  ; recipient : Address.t
  ; amount : TokenAmount.t }
[@@deriving to_yojson]

let make_api_url query endpoint =
  let path = "api/" ^ endpoint in
  Uri.make ~scheme:"http" ~host:"localhost" ~path ~query ~port:8081 ()

let handle_response (response,body) =
  let _ = response |> Response.status |> Code.code_of_status in
  Cohttp_lwt.Body.to_string body
  >>= fun response_str ->
  Lwt.return (yojson_of_string response_str)

let send_get ?(query=[]) endpoint =
  Client.get (make_api_url query endpoint)
  >>= handle_response

let send_post ?(query=[]) endpoint json =
  let json_str = string_of_yojson json in
  Client.post
    ~body:(Cohttp_lwt__.Body.of_string json_str)
    ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
    (make_api_url query endpoint)
  >>= handle_response

let exit_code = ref 0

let set_error_exit () =
  exit_code := 1

let do_exit () =
  Lwt.return (exit !exit_code)

let json_has_error json =
  match json with
  | `Assoc _ ->
    YoJson.mem "error" json
  | _ -> false

let get_operator_transactions () =
  let operator_state = Test.get_operator_state () in
  operator_state.current.transactions

let make_threaded_test endpoint name address amount =
  deposit_withdrawal_json_to_yojson { address; amount }
  |> send_post endpoint
  >>= fun result_json ->
  if json_has_error result_json then
    let _ = Printf.eprintf "*** ERROR *** : Deposit test %s failed: %s\n%!"
              name YoJson.(member "error" result_json |> to_string) in
    return_unit
  else if YoJson.mem "result" result_json then
    let result = YoJson.member "result" result_json in
    let thread_id = YoJson.(member "thread" result |> to_int) in
    let rec thread_loop () : unit Lwt.t =
      send_get ~query:[("id",[string_of_int thread_id])] "thread"
      >>= fun thread_result ->
      if thread_result = Actions.thread_pending_json then
        Lwt_unix.sleep 0.1
        >>= thread_loop
      else if json_has_error thread_result then (
        Printf.eprintf "*** ERROR *** : %s\n%!" YoJson.(member "error" thread_result |> to_string);
        set_error_exit ();
        return_unit)
      else (
        Printf.printf "RESULT: %s\n%!" (string_of_yojson thread_result);
        return_unit) in
    thread_loop ()
  else
    ( Printf.eprintf "Test %s failed, got neither error nor result\n%!" name;
      return_unit )

let make_deposit_test = make_threaded_test "deposit"
let make_withdrawal_test = make_threaded_test "withdrawal"

let make_address_test endpoint ?(query=[]) name address =
  address_json_to_yojson { address }
  |> send_post endpoint ~query
  >>= fun result_json ->
  if json_has_error result_json then (
    Printf.eprintf "*** ERROR *** : Balance test for %s failed: %s\n%!"
      name YoJson.(member "error" result_json |> to_string);
    return_unit)
  else (
    Printf.printf "RESULT: %s\n%!" (string_of_yojson result_json);
    return_unit)

let make_balance_test = make_address_test "balance"
let make_status_test = make_address_test "status"
let make_recent_transactions_test = make_address_test "recent_transactions"
let make_recent_transactions_with_limit_test n =
  make_address_test "recent_transactions" ~query:[("limit",[string_of_int n])]

let make_all_balances_test () =
  send_get "balances"
  >>= fun result_json ->
  if json_has_error result_json then (
    Printf.eprintf "*** ERROR *** : All balances test failed: %s\n%!"
      YoJson.(member "error" result_json |> to_string);
    return_unit)
  else (
    Printf.printf "RESULT: %s\n%!" (string_of_yojson result_json);
    return_unit)

let make_payment_test sender_name sender recipient_name recipient amount=
  payment_json_to_yojson { sender; recipient; amount }
  |> send_post "payment"
  >>= fun result_json ->
  if json_has_error result_json then (
    Printf.eprintf "*** ERROR *** : Payment test from %s to %s failed: %s\n%!"
      sender_name recipient_name YoJson.(member "error" result_json |> to_string);
    return_unit)
  else (
    Printf.printf "RESULT: %s\n%!" (string_of_yojson result_json);
    return_unit)

(* must be odd to prevent self-payment below; also less-than-or-equal than total number of demo users *)
let num_users_to_test = 9

let accounts =
  lazy
    (nicknames_with_registered_keypair ()
     |> List.sort compare
     |> list_take num_users_to_test
     |> List.map (fun name -> name, address_of_nickname name)
     |> Array.of_list)

let get_user : int -> string * Address.t =
  fun ndx ->
    Logging.log "get_user %d" ndx;
    (Lazy.force accounts).(ndx)
    |> fun (name, address) -> Logging.log "%s %s" name (Address.to_0x address); (name, address)

let szabo = TokenAmount.of_string "1000000000000" (* a.k.a. microether *)
let random_int : ?min:int -> max:int -> int =
  fun ?(min=0) ~max -> min + Random.int (max - min + 1)
let random_szabo : ?min:int -> max:int -> TokenAmount.t =
  fun ?min ~max -> let rand = random_int ?min ~max in TokenAmount.(mul (of_int rand) szabo)

let test_deposits () =
  Logging.log "test_deposits";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let name, address = get_user ndx in
      let amount = random_szabo ~min:2000000 ~max:3000000 in
      Printf.printf "DEPOSIT:  Name: %s; Address: %s; Tokens: %s\n%!"
        name (Address.to_0x address) (TokenAmount.to_string amount);
      make_deposit_test name address amount
      >>= fun () ->
      loop (ndx + 1)
    else
      return_unit
  in
  loop 0

let test_withdrawals () =
  Logging.log "test_withdrawals";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let name, address = get_user ndx in
      let amount = random_szabo ~min:1 ~max:1000000 in (* users have at least this much after deposit *)
      Printf.printf "WITHDRAWAL:  Name: %s; Address: %s; Tokens: %s\n%!"
        name (Address.to_0x address) (TokenAmount.to_string amount);
      make_withdrawal_test name address amount
      >>= fun () ->
      loop (ndx + 1)
    else
      return_unit
  in
  loop 0

let test_payments () =
  Logging.log "test_payments";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let sender_name, sender_address = get_user ndx in
      let recipient_name, recipient_address = get_user ((ndx + 2) mod num_users_to_test) in
      let amount = random_szabo ~min:1 ~max:1000000 in
      Printf.printf "PAYMENT:  Sender Name: %s; Sender Address: %s; Recipient Name: %s; Recipient Address: %s; Tokens: %s\n%!"
        sender_name (Address.to_0x sender_address)
        recipient_name (Address.to_0x recipient_address)
        (TokenAmount.to_string amount);
      make_payment_test sender_name sender_address recipient_name recipient_address amount
      >>= fun () ->
      loop (ndx + 1)
    else
      return_unit
  in
  loop 0

let test_balances () =
  Logging.log "test_balances";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let name,address = get_user ndx in
      Printf.printf "BALANCE:  Name: %s; Address: %s\n%!" name (Address.to_0x address);
      make_balance_test name address
      >>= fun () -> loop (ndx + 1)
    else
      return_unit
  in
  loop 0

let test_all_balances () =
  Logging.log "test_all_balances";
  Printf.printf "ALL BALANCES\n%!";
  make_all_balances_test ()

let test_statuses () =
  Logging.log "test_statuses";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let name,address = get_user ndx in
      Printf.printf "STATUS:  Name: %s; Address: %s\n%!" name (Address.to_0x address);
      make_status_test name address
      >>= fun () -> loop (ndx + 1)
    else
      return_unit
  in
  loop 0

let test_recent_transactions ?(limit=None) () =
  Logging.log "test_recent_transactions";
  let rec loop ndx =
    if ndx < num_users_to_test then
      let name,address = get_user ndx in
      (match limit with
       | None ->
         Printf.printf "RECENT TRANSACTIONS:  Name: %s; Address: %s\n%!" name (Address.to_0x address);
         make_recent_transactions_test name address
       | Some n ->
         Printf.printf "RECENT TRANSACTIONS WITH LIMIT = %d:  Name: %s; Address: %s\n%!" n name (Address.to_0x address);
         make_recent_transactions_with_limit_test n name address)
      >>= fun () -> loop (ndx + 1)
    else
      return_unit
  in
  loop 0

(* TODO: add proofs for transactions
   issue: this code doesn't have access to operator state containing TransactionMap
   possible solution is to have operator reveal transaction details through an endpoint

   Not providing test for transaction rate endpoint, seems out of scope here
*)

let _ =
  (*Db.run ~db_name:"alacris_client_test_db" ???*)
  Lwt_main.run
    (() |>
     Lwter.(test_deposits
            >>> test_withdrawals
            >>> test_balances
            >>> test_statuses
            >>> test_all_balances
            >>> test_payments
            >>> test_recent_transactions
            >>> test_recent_transactions ~limit:(Some 3)
            >>> do_exit))
