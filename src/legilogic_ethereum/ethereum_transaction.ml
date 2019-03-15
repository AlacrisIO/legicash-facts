(** Somewhat higher-level wrappers around the basic functionality in ethereum_json_rpc *)
open Legilogic_lib
open Signing
open Action
open Lwt_exn
open Json_rpc

let ensure_private_key ?timeout ?log (keypair : Keypair.t) =
  Logging.log "ethereum_transaction : ensure_private_key";
  (keypair.private_key, keypair.password)
  |> trying (Ethereum_json_rpc.personal_import_raw_key ?timeout ?log)
  >>= handling
        (function
          | Rpc_error x as e ->
            if x.message = "account already exists"
            then return keypair.address
            else fail e
          | e -> fail e)

let ensure_eth_signing_address ?timeout ?log (*!rpc_log*) address =
  (try keypair_of_address address |> return
   with Not_found -> bork "ensure_eth_signing_address: No registered keypair for address %s" (Address.to_0x address))
  >>= ensure_private_key ?timeout ?log
  >>= fun actual_address ->
  if actual_address = address then
    return ()
  else
    bork "keypair registered for address %s actually had address %s"
      (Address.to_0x address) (Address.to_0x actual_address)

let list_accounts () =
  Ethereum_json_rpc.personal_list_accounts ()

let get_first_account =
  list_accounts >>> catching_arr List.hd

exception Bad_password

let unlock_account ?(duration=5) address =
  Logging.log "ethereum_transaction : unlock_account";
  catching_arr keypair_of_address address >>= fun keypair ->
  Logging.log "unlock_account %s" (Address.to_0x address);
  Ethereum_json_rpc.personal_unlock_account (address, keypair.password, Some duration)
  >>= function
  | true -> return ()
  | false -> fail Bad_password

module Test = struct
  open Ethereum_json_rpc

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  let is_ethereum_net_up =
    retry ~retry_window:0.05 ~max_window:1.0 ~max_retries:(Some 10) eth_block_number
    >>> const true

  let%test "poll-for-testnet" =
    run is_ethereum_net_up () || Lib.bork "Could not connect to Ethereum network"
end
