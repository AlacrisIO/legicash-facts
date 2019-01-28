(** Somewhat higher-level wrappers around the basic functionality in ethereum_json_rpc *)
open Legilogic_lib
open Lib
open Signing
open Action
open Lwt_exn
open Json_rpc
open Digesting
   
open Ethereum_chain

(* TODO: when to return false vs raise an exception? Add timeout & log
   Used only by next routine transaction_execution_matches_transaction 
   Which is used only for tests *)
let transaction_executed transaction_hash =
  Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
  >>= fun info ->
  return (Option.is_some info.block_hash && Option.is_some info.block_number)


(* TODO: factor this function into parsing a transaction and comparing transaction objects. *)
(* This function below is used only for tests (in ethereum_user) at present time *)
let transaction_execution_matches_transaction (transaction_hash: digest) (transaction: Transaction.t) : bool Lwt_exn.t =
  transaction_executed transaction_hash
  >>= fun executed ->
  if not executed then
    return false
  else
    Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
    >>= fun info ->
    return
      (try
         (* for all operations, check these fields.
            Shall we add in the checks "&& info.hash = transaction_hash" ???  *)
         let tx_header = transaction.tx_header in
         info.from = Some tx_header.sender
         && info.nonce = tx_header.nonce
         && TokenAmount.compare info.gas tx_header.gas_limit <= 0
         && TokenAmount.compare info.gas_price tx_header.gas_price <= 0
         && TokenAmount.compare info.value tx_header.value = 0
         && (* operation-specific checks *)
         match transaction.operation with
         | TransferTokens recipient_address -> info.to_ = Some recipient_address
         | CreateContract data -> info.input = data
         | CallFunction (contract_address, call_input) ->
           info.to_ = Some contract_address && info.input = call_input
       with _ -> false)

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
