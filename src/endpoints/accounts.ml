open Lwt

open Legilogic_lib
open Signing
open Types
open Action

open Legilogic_ethereum
open Main_chain

open Legicash_lib
open Side_chain
open Side_chain_facilitator
open Side_chain_user
open Side_chain_action.Test

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
let _ = Signing.register_file_keypairs ~path:"../src/endpoints/demo-keys-small.json"

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
let address_to_account_tbl = Hashtbl.create number_of_accounts

let get_user_keys ndx = account_key_array.(ndx)

let _ =
  Array.iter
    (fun (name, keys) ->
       Hashtbl.add address_to_account_tbl keys.Keypair.address name)
    account_key_array

let trent_keys =
  Signing.make_keypair_from_hex
    "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
    "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"

let trent_address = trent_keys.address

let _ =
  Hashtbl.add address_to_account_tbl trent_address "Trent"

let get_user_name address_t =
  try
    Hashtbl.find address_to_account_tbl address_t
  with Not_found -> Lib.bork "Can't find user name for address %s" (Address.to_0x_string address_t)

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

let new_facilitator_state facilitator_keypair =
  let fee_schedule = Side_chain.Test.trent_fee_schedule in
  let (confirmed_state : Side_chain.State.t) =
    { facilitator_revision= Revision.of_int 0
    ; spending_limit= TokenAmount.of_int 100000000
    ; accounts= AccountMap.empty
    ; transactions= TransactionMap.empty
    ; main_chain_transactions_posted= Merkle_trie.DigestSet.empty } in
  FacilitatorState.
    { keypair= facilitator_keypair
    ; current= confirmed_state
    ; fee_schedule= fee_schedule }

let get_trent_state () =
  Side_chain_facilitator.Test.get_facilitator_state ()

let get_user_account address =
  (Side_chain_facilitator.facilitator_account_lens address).get (get_trent_state ())

let user_accounts_from_trent_state address =
  let open UserAccountState in
  let trent_state = get_trent_state () in
  let accounts = trent_state.current.accounts in
  try
    let user_account = AccountMap.find address accounts in
    let account_state = {UserAccountState.empty with confirmed_state= user_account}
    in
    UserAccountStateMap.singleton trent_address account_state
  with Not_found ->
    Lib.bork "Could not find user state for address: %s" (Address.to_0x_string address)

let load_trent_state () =
  Printf.printf "Loading the facilitator state...%!";
  Db.check_connection ();
  let facilitator_state =
    try
      Side_chain.FacilitatorState.load trent_address
      |> fun x -> Printf.printf "done\n%!"; x
    with
      Facilitator_not_found _ ->
      Printf.printf "not found, generating a new demo facilitator\n%!";
      new_facilitator_state trent_keys in
  FacilitatorState.save facilitator_state
  >>= Db.commit
  >>= fun () ->
  (* update user states with retrieved trent state *)
  Hashtbl.iter
    (fun address user_state ->
       try
         let new_user_accounts = user_accounts_from_trent_state address in
         Hashtbl.replace address_to_user_state_tbl address
           { user_state with facilitators = new_user_accounts }
       with _ -> ())
    address_to_user_state_tbl;
  Lwt_exn.return ()

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

let prepare_server =
  let open Lwt_exn in
  Printf.printf "*** PREPARING SERVER, PLEASE WAIT ***\n%!";
  of_lwt (fun () -> Db.open_connection ~db_name:Legibase.db_name)
  >>> load_trent_state
  >>> (fun () -> start_facilitator trent_address)
  >>> arr create_user_states
  >>> (fun () -> list_iter_s store_keys_on_testnet account_key_list)
  (* Use this when we have all 1300 accounts:
     list_iter_s (list_iter_p store_keys_on_testnet) (chunk_list account_key_list 13) *)
  >>> (fun () -> store_keys_on_testnet ("Trent",trent_keys))
  >>> (fun () -> printf "Funding account: Trent\n")
  >>> (* Ethereum dev mode provides prefunded address with a very large balance *)
  get_prefunded_address
  >>> (fun prefunded_address ->
    fund_account prefunded_address trent_keys
    >>= fun () ->
    list_iter_s
      (fun (name,keys) ->
         printf "Funding account: %s\n" name
         >>= (fun () -> fund_account prefunded_address keys))
      account_key_list)
  >>> (fun () -> printf "Installing facilitator contract...")
  >>> trying (load_contract >>> fun () -> printf "done\n%!")
  >>> handling
        (fun _ ->
           printf "failed, installing contract...%!"
           >>= install_contract
           >>= fun () -> printf "done\n%!")
  >>> fun () -> printf "*** READY ***\n"
