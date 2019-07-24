(** Somewhat higher-level wrappers around the basic functionality in ethereum_json_rpc *)
open Legilogic_lib
open Lib
open Action
open Signing
open Types
open Lwt_exn
open Json_rpc

open Ethereum_chain
open Ethereum_config
open Ethereum_json_rpc

let ensure_private_key ?timeout ?log (keypair : Keypair.t) =
  Logging.log "ethereum_transaction : ensure_private_key";
  (keypair.private_key, keypair.password)
  |> trying (personal_import_raw_key ?timeout ?log)
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

let list_accounts () = personal_list_accounts ()

let get_first_account = list_accounts >>> catching_arr List.hd

exception Bad_password

let unlock_account ?(duration=5) address =
  Logging.log "ethereum_transaction : unlock_account";
  catching_arr keypair_of_address address >>= fun keypair ->
  Logging.log "unlock_account %s" (Address.to_0x address);
  Ethereum_json_rpc.personal_unlock_account (address, keypair.password, Some duration)
  >>= function
  | true -> return ()
  | false -> fail Bad_password

exception TransactionRejected

let check_transaction_receipt_status (receipt : TransactionReceipt.t) =
  match receipt with
    TransactionReceipt.{status} ->
      if TokenAmount.sign status = 0 then
        fail TransactionRejected
      else
        return receipt

let is_receipt_sufficiently_confirmed (receipt : TransactionReceipt.t) block_number =
  let lazy confirmation_height = minimal_confirmation_height_in_blocks in
  Revision.(is_add_valid receipt.block_number confirmation_height
            && compare block_number (add receipt.block_number confirmation_height) >= 0)

exception Still_pending
exception Invalid_transaction_confirmation of string

let check_receipt_sufficiently_confirmed receipt =
  eth_block_number ()
  >>= fun block_number ->
  if is_receipt_sufficiently_confirmed receipt block_number then
    return receipt
  else
    fail Still_pending


(** Given a putative sender, some transaction data, and a confirmation,
    make sure that it all matches.
   TODO: Make sure we can verify the confirmation from the Ethereum contract,
    by checking the merkle tree and using e.g. Andrew Miller's contract to access old
    block hashes https://github.com/amiller/ethereum-blockhashes *)
let check_transaction_confirmation :
      sender:Address.t -> recipient:Address.t -> SignedTransactionData.t -> Confirmation.t
      -> 'a -> 'a Lwt_exn.t =
  fun ~sender ~recipient txdata confirmation x ->
  let open Lwt_exn in
  (* TODO: Use RLP marshaling for SignedTransactionData then we can just check the hash.
  let hash = confirmation.transaction_hash in
  if not (recipient = txdata.to_address) then
    fail (Invalid_transaction_confirmation "Recipient does not match Transaction data")
  else if not (Ethereum_chain.SignedTransactionData.digest txdata = hash) then
    fail (Malformed_request "Transaction data digest does not match the provided confirmation") *)
  Ethereum_json_rpc.eth_get_transaction_by_hash confirmation.transaction_hash
  >>= fun txinfo ->
  if not (Some sender = txinfo.from
          && recipient = Option.defaulting (konstant Address.zero) txinfo.to_
          && recipient = txdata.to_address
          && txinfo.nonce = txdata.nonce
          && txinfo.value = txdata.value
          && txinfo.gas_price = txdata.gas_price
          && txinfo.gas = txdata.gas_limit
          && txinfo.input = Bytes.of_string txdata.data) then
    fail (Invalid_transaction_confirmation "transaction information doesn't match provided data")
  else if not (confirmation.transaction_hash = txinfo.hash
               && Some confirmation.transaction_index = txinfo.transaction_index
               && Some confirmation.block_number = txinfo.block_number
               && Some confirmation.block_hash = txinfo.block_hash) then
    fail (Invalid_transaction_confirmation "confirmation doesn't match transaction information")
  else
    Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
    >>= function
    | None -> fail (Invalid_transaction_confirmation "Transaction not included in the blockchain")
    | Some receipt ->
       Ethereum_json_rpc.eth_block_number ()
       >>= arr (is_receipt_sufficiently_confirmed receipt)
       >>= function
       | false -> fail (Invalid_transaction_confirmation "Transaction still pending")
       | true -> if not (sender = receipt.from) then
                   fail (Invalid_transaction_confirmation "Transaction sender doesn't match")
                 else
                   return x

module Test = struct
  open Ethereum_json_rpc

  let%test "move logs aside" = Logging.set_log_file "test.log"; true

  let is_ethereum_net_up =
    retry
      ~retry_window:0.05
      ~max_window:1.0
      ~max_retries:(Some 10)
      @@ eth_block_number ?timeout:(Some 1.0)
    >>> const true

  let%test "poll-for-testnet" =
    run is_ethereum_net_up () || Lib.bork "Could not connect to Ethereum network"
end
