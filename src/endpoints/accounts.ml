open Legilogic_lib
open Lib
open Signing
open Types
open Action
open Lwt_exn

open Legilogic_ethereum
open Main_chain
open Yojsoning

open Legicash_lib
open Side_chain
open Side_chain_user
open Side_chain_action.Test
open Side_chain_client

(* users *)
let user_names =
  [ "Alice"
  ; "Bob"
  ; "Charlie"
  ; "Delores"
  ; "Edward"
  ; "Frank"
  ; "Geraldine"
  ; "Henrietta"
  ; "Igor"
  ; "Jerry"
  ; "Katherine"
  ; "Laverne"
  ; "Michael"
  ; "Norman"
  ; "Oscar"
  ; "Pablo"
  ; "Quentin"
  ; "Roxanne"
  ; "Suzanne"
  ; "Theodore"
  ; "Ursula"
  ; "Vernon"
  ; "Walter"
  ; "Xavier"
  ; "Yolanda"
  ; "Zander" ]

let account_names = user_names

(* use code below for 1300 accounts *)
(*  let rec loop count accum =
    if count < 0 then accum
    else
    let names = List.map (fun name -> name ^ (string_of_int count)) user_names in
    loop (count - 1) (names @ accum)
    in
    loop 49 [] *)

(* register keypairs from disk, path is relative to _run *)
let _ =
  let key_file = Config.get_config_filename "demo-keys-small.json" in
  Signing.register_file_keypairs ~path:key_file

(* for 1300 accounts:
   let _ = Signing.register_file_keypairs ~path:"../src/endpoints/demo-keys-big.json"
*)

(* create local data structures reflecting registered keys *)

let account_key_list =
  List.map
    (fun name ->
       let keys = Signing.(address_of_nickname name |> keypair_of_address) in
       (name, keys)) account_names
let account_key_array = Array.of_list account_key_list
let number_of_accounts = Array.length account_key_array

let get_user_keys ndx = account_key_array.(ndx)

let _ =
  Array.iter
    (fun (name, keys) ->
       Hashtbl.add address_to_account_tbl keys.Keypair.address name)
    account_key_array

let trent_address = Signing.Test.trent_address

let _ = Signing.register_keypair "Trent" trent_keys

let get_user_name address =
  try
    nickname_of_address address
  with Not_found -> Lib.bork "Can't find user name for address %s" (Address.to_0x_string address)

(* store keys on Ethereum test net. TODO: don't do this on real net!  *)
let store_keys_on_testnet (name,keys) =
  let open Lwt_exn in
  let password = "" in
  Ethereum_transaction.ensure_private_key (keys, password)
  >>= fun address ->
  printf "Created account for %s on test net with address: %s\n"
    name (Address.to_0x_string address)

(* prepare test network with accounts, contract *)

let (address_to_user_state_tbl : (Address.t,Side_chain_user.UserState.t) Hashtbl.t) = Hashtbl.create number_of_accounts

let create_side_chain_user_state user_keys =
  let main_chain_user_state =
    Main_chain.UserState.
      { keypair= user_keys
      ; confirmed_state= Digest.zero
      ; confirmed_balance= TokenAmount.zero
      ; pending_transactions= []
      ; nonce= Nonce.zero }
  in
  let (user_account_state : UserAccountState.t) = UserAccountState.empty
  in
  let facilitators = UserAccountStateMap.singleton trent_address user_account_state in
  UserState.{main_chain_user_state; facilitators; notifications= []}

let create_user_states () =
  List.iter
    (fun (_name,(keys:Keypair.t)) ->
       Hashtbl.add address_to_user_state_tbl keys.address
         (create_side_chain_user_state keys))
    account_key_list

let get_user_account address =
  UserQueryRequest.Get_account_state { address }
  |> post_user_query_request_to_side_chain

(* TODO: the error handling in this function and the next needs improvement *)
let user_account_map_from_address address : UserAccountStateMap.t Lwt_exn.t =
  let open UserAccountState in
  get_user_account address
  >>= (fun json ->
    if YoJson.mem "account_state" json then
      match YoJson.member "account_state" json |> AccountState.of_yojson with
      | Ok acct_state -> return acct_state
      | Error s -> Lwt.fail (Internal_error s)
    else Lwt.fail (Internal_error "Could not find user account state"))
  >>= fun user_account ->
  let account_state = {UserAccountState.empty with confirmed_state= user_account}
  in
  UserAccountStateMap.singleton trent_address account_state |> return

let store_user_accounts () =
  let user_addresses =
    Array.to_list account_key_array |> List.map (fun (_,(keys:Signing.Keypair.t)) -> keys.address)
  in
  list_iter_s
    (fun address ->
       Lwt.catch
         (fun () ->
            user_account_map_from_address address
            >>= fun new_user_accounts ->
            (try
               let user_state = Hashtbl.find address_to_user_state_tbl address in
               Hashtbl.replace address_to_user_state_tbl address
                 { user_state with facilitators = new_user_accounts };
               return ()
             with Not_found -> (* should never happen *)
               bork "Address to user table has no entry for %s" (Address.to_0x_string address)))
         (fun _exn -> return ()))
    user_addresses

(* let chunk_list elts chunk_size =
   let rec mk_chunk elts accum count =
   if count >= chunk_size then
   (List.rev accum,elts) (* chunk, remaining elts *)
   else
   match elts with
   | [] -> (List.rev accum,[])
   | h::t ->
   mk_chunk t (h::accum) (count + 1)
   in
   let rec loop elts accum =
   match elts with
   | [] -> List.rev accum
   | _ ->
   let chunk,remaining = mk_chunk elts [] 0 in
   loop remaining (chunk::accum)
   in
   loop elts []
*)

(*
   let log_account_state address =
   address
   |> get_user_account
   |> fun {balance; account_revision} ->
   Logging.log "%s account %s balance %s revision %s\n"
   (nickname_of_address address) (Address.to_0x_string address)
   (TokenAmount.to_0x_string balance) (Revision.to_0x_string account_revision)
   |> Lwt_exn.return
*)

let prepare_server =
  let open Lwt_exn in
  Printf.printf "*** PREPARING SCGI SERVER / SIDE CHAIN CLIENT, PLEASE WAIT ***\n%!";
  arr create_user_states
  >>> (fun () -> list_iter_s store_keys_on_testnet account_key_list)
        (* Use this when we have all 1300 accounts:
           list_iter_s (list_iter_p store_keys_on_testnet) (chunk_list account_key_list 13) *)
  >>> store_user_accounts
  >>> (* Ethereum dev mode provides prefunded address with a very large balance *)
  get_prefunded_address
  >>> (fun prefunded_address ->
    list_iter_s
      (fun (name,(keys:Signing.keypair)) ->
         printf "Funding account: %s\n" name
         >>= (fun () -> fund_account prefunded_address keys.address))
      account_key_list)
  >>> fun () -> printf "*** READY ***\n"
