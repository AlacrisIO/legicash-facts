(* account for endpoints demo  *)

open Lwt

open Legicash_lib

open Legibase
open Lib
open Crypto
open Action
open Main_chain
open Side_chain
open Side_chain_action

(* users *)
let account_names =
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

let account_keys =
  List.map
    (fun (priv, pub) -> Keypair.make_keys_from_hex priv pub)
    [ ( "fd:c8:f1:5b:2d:d9:22:9b:0b:92:46:09:43:93:af:c2:3b:3b:70:5c:07:e6:74:f6:cb:61:41:20:d1:62:78:18"
      , "04:55:62:69:5c:85:f8:8f:6c:ba:ec:12:1d:2a:3d:a6:66:6c:5d:c8:54:0d:86:35:8b:d5:69:a1:88:2b:be:6d:dc:f4:5b:76:f5:64:31:33:93:9c:8e:7a:33:99:47:ca:1b:11:52:90:d5:77:34:30:23:d7:9c:25:6d:bc:54:bc:97"
      )
    ; ( "9b:21:b9:b0:6b:a7:78:24:b8:ba:6a:81:5f:5a:07:52:29:a7:08:ae:88:ba:7f:d9:35:c9:68:fe:2c:3d:f1:72"
      , "04:9e:0a:7e:3c:05:e3:32:8c:60:3b:0c:27:fb:fd:fc:50:30:c9:5d:9a:d1:79:a4:31:c1:4f:81:e3:0a:64:ce:95:f6:25:44:7e:18:2a:8b:e7:18:d4:5f:9a:b9:72:3f:9b:85:71:dd:5c:57:52:da:a6:6f:eb:84:93:8b:09:58:05"
      )
    ; ( "84:be:c5:9c:0e:83:f5:21:fb:92:10:01:1e:80:61:87:5c:99:0a:86:f8:5a:52:87:9d:76:28:41:c9:24:c5:11"
      , "04:37:d4:d4:f5:3e:08:18:9d:07:1d:94:e1:a9:37:72:47:20:63:4c:a0:4b:67:ae:1e:18:7b:f0:09:3b:bb:f3:ab:f1:fb:3f:03:d4:48:06:bf:ef:a6:ad:5c:d0:d3:7d:14:0a:6c:e5:e2:7e:0c:73:4e:58:62:50:51:05:ff:09:be"
      )
    ; ( "b9:cf:cb:11:ee:8e:fe:22:31:7a:c8:7d:98:eb:90:08:da:ca:37:49:43:1b:ea:d5:56:7c:92:91:40:a4:1a:60"
      , "04:54:83:dc:8a:40:d1:33:bb:6b:18:4e:5c:31:2c:9e:b2:cb:3b:e8:78:ec:a0:b1:dc:4c:e0:4f:49:8e:c8:58:22:f6:60:c1:d9:5d:4b:c1:fd:03:4c:09:0a:30:56:bd:8c:5f:36:a8:09:43:ff:3b:94:0a:c4:87:98:dc:61:de:77"
      )
    ; ( "fc:14:92:9f:ca:fa:9f:97:7f:7d:cc:6b:ce:e3:5e:32:8b:83:af:49:77:e6:49:88:76:8a:bb:59:e6:84:b6:34"
      , "04:b9:24:f3:72:e4:fb:18:b8:9f:ec:f7:99:5d:71:b6:ef:1e:2e:21:54:a8:ab:7a:78:d4:db:18:b9:36:2a:38:88:1c:05:f4:51:d8:a6:7f:82:7a:a2:66:44:cf:49:04:5f:ce:7e:ff:71:9d:3d:22:9e:d4:3f:0d:91:bc:42:0d:9f"
      )
    ; ( "12:c4:6c:e7:5e:f9:10:f3:cf:0f:94:3a:66:a4:b6:7b:c8:1f:38:d6:02:58:3b:14:45:1c:d0:d6:4d:cd:af:ed"
      , "04:3e:56:ba:c3:db:3e:75:11:61:09:ef:52:1e:e4:05:b1:24:72:86:7f:39:84:ae:c0:7b:62:fe:14:02:81:c7:38:24:62:b8:43:f5:4a:ee:11:d6:9b:b1:d1:09:6d:c7:b6:80:c9:56:bc:56:36:1e:15:2e:41:5d:b7:4f:1a:7a:e8"
      )
    ; ( "5a:c0:af:13:04:4f:2f:13:f8:63:74:aa:2a:4d:4e:90:4b:ce:d9:6b:b9:77:e9:6b:ca:46:97:ce:e5:f3:76:3c"
      , "04:3c:2f:e3:4c:8c:07:c3:ec:5f:5d:48:74:fa:f3:38:a1:6a:d4:9c:c6:09:1f:a6:b9:23:0d:d3:20:1e:c3:5e:80:90:64:1e:2c:d2:ff:d4:fe:87:b0:b6:98:48:a4:d9:e7:7a:18:67:1a:a0:3d:09:00:f2:2a:90:80:e5:55:99:f6"
      )
    ; ( "5c:7a:70:85:b3:aa:43:58:39:53:b2:b7:0a:7d:06:d7:25:8b:38:18:67:5a:dc:47:1e:27:14:c3:67:6d:b9:1a"
      , "04:81:49:13:ff:28:8c:e3:44:45:5e:86:98:1f:c7:d7:79:a7:5e:87:89:08:02:04:6b:5a:22:2b:5c:ab:ee:08:c1:0d:86:f6:69:59:17:f6:e4:36:d7:08:68:a1:4b:ea:fd:6b:16:a5:0c:fa:a7:65:a7:cf:d5:8b:d3:d3:2b:6d:b7"
      )
    ; ( "8e:ef:df:e5:f8:8e:01:a9:ba:ac:55:47:5d:9a:17:42:54:25:7d:79:e6:df:8e:b6:1a:e5:c3:3e:73:51:b3:50"
      , "04:c7:50:78:e6:94:ee:6e:3e:6c:f8:c0:19:97:e2:42:2a:7b:7f:6d:ce:26:d9:df:44:3c:c6:b0:75:ff:26:b8:10:cb:c3:86:e9:3c:f6:b1:79:02:f5:b4:41:6c:17:c3:5d:f6:64:d3:41:9f:0e:a1:c4:74:31:d4:5d:4e:1a:cb:f8"
      )
    ; ( "09:62:2b:dd:37:b5:3f:37:e7:f9:10:36:aa:e0:e5:18:f6:02:81:b3:24:1e:be:a1:b8:8a:f2:8b:33:70:5c:ee"
      , "04:01:8e:79:50:80:6f:e7:76:69:57:2c:e0:6f:b5:17:eb:9d:d5:9d:42:cb:e0:e9:a9:c1:59:60:fc:13:6e:40:93:8b:d4:f1:19:b4:57:86:c8:26:53:db:f9:85:7f:5e:81:d9:b9:f4:93:4c:e3:2a:cf:31:35:05:7a:44:cb:27:28"
      )
    ; ( "54:76:82:16:ad:f7:35:d6:fe:eb:8d:ed:9e:1c:98:6d:10:0c:11:2c:61:61:e2:71:35:c0:1f:d7:6d:b3:fd:75"
      , "04:c4:32:0a:bf:50:a9:eb:dc:0b:d9:bc:9e:f6:2b:1f:bb:36:df:61:6a:c2:cc:a4:68:4f:6b:d0:93:7f:db:de:f6:8f:e5:ab:5e:a6:f5:f1:55:5d:58:35:1a:a0:34:f6:b4:85:6b:0a:0c:b5:22:aa:6e:23:2f:96:7a:4e:00:e8:62"
      )
    ; ( "8a:ed:5e:49:15:0f:58:9a:c1:d7:79:7c:52:82:cd:cb:8d:25:41:df:62:97:34:88:f2:0f:a6:6a:f6:13:bf:b8"
      , "04:8a:8f:bf:47:5e:40:db:67:19:b7:4a:72:4f:79:91:75:96:bb:39:96:24:d4:b1:72:3c:0e:62:8d:dc:35:d1:b4:30:f2:e3:ff:ac:63:c5:f9:7d:a2:fc:e9:dc:9a:dd:19:27:6c:b3:b3:d7:8a:c4:c9:b5:cd:8a:80:21:85:ca:1d"
      )
    ; ( "77:e4:54:30:9a:76:44:9c:13:1d:c2:f0:78:fd:c0:bd:5f:19:fd:ce:63:16:0d:99:ca:8a:00:84:07:f2:02:b1"
      , "04:9b:a5:66:cc:64:1a:28:a8:c4:ca:21:55:83:51:b6:c9:ad:29:e6:ab:8f:c1:02:d2:ac:7f:88:49:88:22:74:ea:70:88:d9:10:45:3e:af:8a:64:62:e4:3a:20:97:9b:09:2a:47:2a:81:17:f2:74:7b:fe:37:a0:d6:c5:13:c4:7b"
      )
    ; ( "16:0c:b2:b2:bc:dc:bd:8d:ad:15:30:d0:00:93:dc:08:21:b7:1e:76:53:9b:1b:d8:9b:b7:1b:4c:4f:3d:15:47"
      , "04:bd:26:a1:65:5c:07:1d:82:a8:5f:72:ed:8e:7a:91:5e:e9:d0:5a:9c:b2:87:f3:64:40:1e:6d:c3:ab:8b:3f:6f:8f:a7:b6:57:50:88:a8:e9:7f:db:0a:80:04:d7:85:d0:aa:c7:6a:ce:e6:02:cc:76:65:c1:a3:62:e9:11:cd:b2"
      )
    ; ( "95:96:ef:a4:9d:4c:a9:58:4f:80:15:2d:59:62:c7:aa:50:96:d7:08:d8:b7:89:c3:e3:f8:1e:88:32:0f:69:d4"
      , "04:81:44:73:49:13:7f:d3:10:53:6e:cd:92:b2:af:53:83:0d:95:13:1d:5a:4a:6b:84:6c:8d:1f:31:25:2b:48:de:f8:fa:2e:e1:5e:04:c1:b0:e2:79:f1:5f:52:63:dd:b8:b1:38:e0:26:de:3f:47:13:a3:c8:df:0e:a3:86:17:49"
      )
    ; ( "ef:f7:b3:f2:2a:f2:ae:d2:9f:07:f4:05:a2:cd:17:50:7d:c9:86:59:a2:3a:f7:90:5b:15:4b:a6:2b:94:12:15"
      , "04:8b:0e:ea:d0:3f:c8:2a:0c:1d:21:ec:f6:af:c7:1d:c0:a5:ea:f9:b5:69:1e:33:33:d8:72:1c:72:4a:4f:4d:3a:a2:43:1f:3e:50:21:55:df:78:07:45:27:a5:22:e8:b1:51:4c:c2:be:f2:f2:c9:16:92:77:ff:68:1b:06:be:c4"
      )
    ; ( "1f:e2:6a:92:65:32:38:7e:dd:56:de:81:6d:b1:10:b4:22:d9:04:91:94:ad:15:a0:ea:13:38:b9:c4:66:27:f5"
      , "04:b6:1d:3c:51:9f:d9:97:0a:25:17:a6:62:e0:70:31:fb:a0:e1:c0:6e:1c:ca:13:5e:34:a9:60:c4:d9:08:6e:66:05:d1:62:98:65:86:bc:61:fa:d5:66:b8:3c:4c:ab:e7:62:b5:30:46:88:55:32:fb:e6:c0:ea:ea:32:0a:c1:e0"
      )
    ; ( "ef:4a:51:42:ef:17:fe:a8:ab:0a:d6:6e:8a:c6:2a:1a:9a:2c:56:a2:e6:68:5f:43:0f:74:c3:c9:9c:8d:c9:2f"
      , "04:d3:10:47:df:f0:8c:a4:18:42:6a:41:7a:1d:1b:ac:5c:b3:1b:f0:1e:b8:9a:18:26:c0:a0:77:75:53:a1:eb:e6:60:2d:ac:1c:d9:7f:a7:18:78:63:a9:cf:ac:0e:6d:d5:83:9b:5f:25:71:94:8c:1b:8b:9f:16:e5:eb:5d:df:fc"
      )
    ; ( "79:85:db:0b:85:38:9d:08:fc:22:9d:11:fe:e9:62:5d:d3:9b:4e:13:14:a0:63:fc:12:33:78:09:f9:3c:2b:96"
      , "04:de:36:fe:6d:f0:57:be:1f:22:55:5a:23:3b:85:98:ad:9f:28:6b:b6:e4:83:4e:1f:bc:e8:39:54:08:bc:d2:99:0d:ca:91:13:b2:2a:8b:a2:8c:3e:99:0f:56:c1:9e:17:8d:cd:8f:a4:b5:2f:47:cb:de:78:4e:ce:72:96:71:f7"
      )
    ; ( "86:20:a3:b5:13:57:2f:e2:5d:c8:29:ab:a5:6b:bf:d9:c2:51:79:3b:6e:2b:01:f6:62:18:a5:2a:f1:12:60:86"
      , "04:34:2a:16:21:cb:86:13:0f:22:3f:25:db:b8:10:f5:17:e4:9c:1e:ab:06:70:44:9b:ec:3d:4c:1c:a8:24:5b:54:f2:74:1e:d7:dd:dd:0d:5e:ae:84:82:da:20:8a:d3:96:59:04:f4:83:46:79:ff:6c:85:b2:27:05:9b:72:65:4a"
      )
    ; ( "26:e1:5d:43:93:a9:cb:e5:49:38:eb:c3:34:61:a2:c1:d0:e1:2f:ef:13:d5:8c:f5:96:ff:7c:16:00:b0:aa:91"
      , "04:d5:3e:33:ca:66:ed:27:28:ef:09:71:e5:c0:d9:c6:ef:3f:70:8f:c8:e7:e8:b8:a4:0a:85:62:90:3b:18:fc:f4:0e:38:9c:16:ed:df:77:6f:a4:09:15:8f:cf:ff:56:40:5f:20:1f:dc:01:58:f2:b8:44:49:0c:51:56:6f:b9:0f"
      )
    ; ( "06:c1:3d:c8:df:47:c1:46:42:a0:a3:01:69:f6:63:f9:35:9c:f7:8e:71:41:ed:7a:ee:64:b8:73:b6:69:c0:51"
      , "04:4f:4e:0c:96:c8:26:d6:fb:3b:bd:c2:0e:0e:6b:41:dd:6b:f4:eb:31:51:6a:09:b8:8e:c1:de:ca:95:3a:8f:34:0d:12:8c:ae:21:9e:e1:64:04:97:b7:73:46:02:e3:85:02:97:c6:8b:09:a1:ba:ab:11:7b:75:ab:9a:5d:9a:1d"
      )
    ; ( "d8:25:0c:a1:84:4c:e5:aa:74:3f:5a:cc:4c:d4:a3:bb:6e:e3:c1:03:d5:20:20:a8:9e:cd:36:e2:5c:75:55:5d"
      , "04:f3:84:88:c8:1c:e6:58:ca:ff:99:a6:d6:dc:6c:d8:87:77:e9:81:ae:23:1f:b2:2c:35:66:52:9e:d9:f6:e5:c0:84:95:37:50:0a:b3:59:d6:8e:a4:34:49:0f:86:2b:8a:52:a5:17:12:6f:35:ee:61:73:e2:72:b5:c1:a6:05:95"
      )
    ; ( "e7:ed:74:d7:6b:c9:14:81:26:04:ca:b2:29:4a:af:18:0a:ab:14:0b:96:f4:0d:ee:25:8e:14:fa:1e:5c:df:95"
      , "04:0d:33:70:09:a5:e4:0f:04:13:09:8a:fa:2e:72:79:52:67:94:00:27:f2:19:35:ec:f2:28:ce:20:c5:8f:d8:40:0a:88:40:74:b8:45:92:1d:92:68:2d:4c:da:c9:75:58:fb:39:3d:df:77:0c:64:1f:c7:e2:a9:bb:9f:17:88:02"
      )
    ; ( "ae:dc:de:a2:b9:1d:e2:4d:1f:e2:c8:ae:4b:60:68:7f:b3:82:66:12:96:25:53:fa:3d:0b:84:86:e3:22:aa:a7"
      , "04:9d:f2:cf:38:33:4e:87:89:3f:75:79:24:4c:dc:61:bb:64:4f:0d:87:79:8c:73:aa:d0:0d:e6:d4:bf:08:70:de:88:81:5b:35:5d:b3:73:f6:a6:eb:4f:f3:96:0e:3d:5f:3f:02:7a:04:90:2f:58:b4:ba:89:c9:7c:56:b1:50:24"
      )
    ; ( "48:84:b1:bd:ef:82:81:b4:0c:ad:15:f5:52:5d:72:a5:c9:a5:db:18:f2:13:ab:f2:8a:46:bf:ab:8b:ff:2a:5f"
      , "04:16:60:26:f2:e1:9a:15:ec:59:7f:b1:1b:7d:93:2f:b5:f6:32:82:b3:eb:37:c9:1e:98:c7:a6:4c:fd:7f:9c:c7:54:5e:02:81:52:bf:44:5b:9b:c9:71:ed:5f:06:db:05:e3:18:0a:23:20:20:ab:9e:78:5b:15:4f:45:d1:c4:db"
      ) ]

(* data structures for fast access *)
let account_key_array =
  Array.of_list (List.map2 (fun name keys -> (name, keys)) account_names account_keys)

let number_of_accounts = Array.length account_key_array

let address_to_account_tbl = Hashtbl.create number_of_accounts

let _ =
  let open Keypair in
  Array.iter
    (fun (name, keys) -> Hashtbl.add address_to_account_tbl keys.address name)
    account_key_array

let get_user_name address_t =
  try
    Hashtbl.find address_to_account_tbl address_t
  with Not_found -> raise (Internal_error "Can't find user name for address")

let address_to_keys_tbl = Hashtbl.create number_of_accounts

let _ =
  let open Keypair in
  List.iter
    (fun keys -> Hashtbl.add address_to_keys_tbl keys.address keys)
    account_keys

let address_to_user_state_tbl = Hashtbl.create number_of_accounts

let trent_keys =
  Keypair.make_keys_from_hex
    "b6:fb:0b:7e:61:36:3e:e2:f7:48:16:13:38:f5:69:53:e8:aa:42:64:2e:99:90:ef:f1:7e:7d:e9:aa:89:57:86"
    "04:26:bd:98:85:f2:c9:e2:3d:18:c3:02:5d:a7:0e:71:a4:f7:ce:23:71:24:35:28:82:ea:fb:d1:cb:b1:e9:74:2c:4f:e3:84:7c:e1:a5:6a:0d:19:df:7a:7d:38:5a:21:34:be:05:20:8b:5d:1c:cc:5d:01:5f:5e:9a:3b:a0:d7:df"

let trent_address = trent_keys.address

let create_side_chain_user_state user_keys =
  let main_chain_user_state =
    { keypair= user_keys
    ; confirmed_state= Digest.zero
    ; confirmed_balance= TokenAmount.zero
    ; pending_transactions= []
    ; nonce= Nonce.zero }
  in
  let new_account_state = {balance= TokenAmount.zero; account_revision= Revision.zero} in
  let user_account_state =
    {facilitator_validity= Confirmed; confirmed_state= new_account_state; pending_operations= []}
  in
  let facilitators = AddressMap.singleton trent_address user_account_state in
  {main_chain_user_state; facilitators}

let trent_fee_schedule =
  { deposit_fee= TokenAmount.of_int 5
  ; withdrawal_fee= TokenAmount.of_int 5
  ; per_account_limit= TokenAmount.of_int 20000
  ; fee_per_billion= TokenAmount.of_int 42 }

let confirmed_trent_state =
  { previous_main_chain_state= Digest.zero
  ; previous_side_chain_state= Digest.one
  ; facilitator_revision= Revision.of_int 17
  ; spending_limit= TokenAmount.of_int 1000000
  ; bond_posted= TokenAmount.of_int 5000000
  ; accounts= AddressMap.empty
  ; operations= AddressMap.empty
  ; main_chain_transactions_posted= DigestSet.empty }

let trent_initial_state =
  { keypair= trent_keys
  ; previous= None
  ; current= confirmed_trent_state
  ; fee_schedule= trent_fee_schedule }

let trent_state = ref trent_initial_state

(* types have to be JSON-friendly, so don't use, for example, TokenAmount.t *)
type user_account_state =
  { address : string
  ; user_name : string
  ; balance : int
  }
  [@@deriving yojson]

type payment_result =
  { sender_account : user_account_state
  ; recipient_account : user_account_state
  ; amount_transferred : int
  }
  [@@deriving yojson]

let ( |^>> ) v f = v |> f |> function state, Ok x -> (state, x) | state, Error y -> raise y
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

let deposit_to_trent address amount =
  let open Side_chain_action in
  let address_t = Ethereum_util.address_of_hex_string address in
  let user_state =
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
  in
  let thread =
    (user_state, (trent_address,TokenAmount.of_int amount))
    |^>>+ deposit
    >>= fun (user_state1, signed_request) -> (!trent_state,signed_request)
    |^>> confirm_request
    |> fun (trent_state1,signed_confirmation) ->
    Hashtbl.replace address_to_user_state_tbl address_t user_state1;
    trent_state := trent_state1;
    (* get user account info on Trent *)
    let user_account_on_trent = AddressMap.find address_t !trent_state.current.accounts in
    let balance = TokenAmount.to_int (user_account_on_trent.balance) in
    let user_name = get_user_name address_t in
    let user_account_state = { address
                             ; user_name
                             ; balance
                             }
    in
    return (user_account_state_to_yojson user_account_state)
  in
  add_main_chain_thread thread

let get_balance_on_trent address =
  let address_t = Address.of_string (Ethereum_util.string_of_hex_string address) in
  let user_account_on_trent = AddressMap.find address_t !trent_state.current.accounts in
  let balance = TokenAmount.to_int (user_account_on_trent.balance) in
  let user_name = get_user_name address_t in
  let user_account_state = { address
                           ; user_name
                           ; balance
                           }
  in
  user_account_state_to_yojson user_account_state

let get_all_balances_on_trent () =
  let make_balance_json address_t (account : Side_chain.account_state) accum =
    let user_name = get_user_name address_t in
    let account_state = { address = Ethereum_util.hex_string_of_address address_t
                        ; user_name
                        ; balance = TokenAmount.to_int account.balance
                        }
    in account_state::accum
  in
  let user_account_states = AddressMap.fold make_balance_json !trent_state.current.accounts [] in
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
  let sender_account = AddressMap.find sender_address_t starting_accounts in
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
  let sender_account = AddressMap.find sender_address_t accounts in
  let recipient_account = AddressMap.find sender_address_t accounts in
  let make_account_state address_t name (account : Side_chain.account_state) =
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
