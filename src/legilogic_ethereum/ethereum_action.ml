open Legilogic_lib
open Lib
open Yojsoning
open Types

open Main_chain

let stub_state = ref genesis_state

let stub_state_digest = ref (Digest.zero)

(** Stub for gas price. Here set at 50 wei. *)
let stub_gas_price = ref (TokenAmount.of_int 50)

let [@warning "-32"] update_stub_state new_state =
  stub_state := new_state ;
  stub_state_digest := Digest.zero (* TODO: extract digest from new_state *)

let make_tx_header (value, gas_limit) (user_state: UserState.t) =
  UserAction.return
    { TxHeader.sender= user_state.keypair.address
    ; TxHeader.nonce= user_state.nonce
    ; TxHeader.gas_price= !stub_gas_price
    ; TxHeader.gas_limit
    ; TxHeader.value }
    user_state

let add_pending_transaction transaction (user_state: UserState.t) =
  UserAction.return transaction
    {user_state with
     pending_transactions= transaction :: user_state.pending_transactions ;
     nonce= Nonce.(add one user_state.nonce) }

let sign_transaction transaction user_state =
  UserAction.return (Transaction.signed user_state.UserState.keypair transaction) user_state

let issue_transaction (operation,value,gas_limit) =
  (value, gas_limit) |>
  let open UserAction in
  to_async (make_tx_header
            >>> (fun tx_header -> return Transaction.{tx_header; operation})
            >>> sign_transaction
            >>> add_pending_transaction)

let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (recipient, amount) =
  issue_transaction (TransferTokens recipient, amount, transfer_gas_limit)

(* TODO: move to Action? *)
let rec retry_until_some interval_seconds x action =
  let open Lwt in
  x |> action >>=
  function
  | None -> Lwt_unix.sleep interval_seconds
    >>= (fun () -> retry_until_some interval_seconds x action)
  | Some z -> return z

let confirmation_of_transaction_receipt receipt_json =
  if (YoJson.mem "error" receipt_json) then
    let error = string_of_yojson (YoJson.member "error" receipt_json) in
    Some (Error (Internal_error error))
  else
    let result_json = YoJson.member "result" receipt_json in
    if result_json == `Null then
      None
    else
      (* remove leading 0x; shouldn't need to validate that the prefix exists, should always be well-formed *)
      let transaction_hash = YoJson.member "transactionHash" result_json |> YoJson.to_string |> Digest.of_0x_string in
      (* Revision, UInt64 "of_string" reads hex numbers as 0, so convert to OCaml int *)
      let transaction_index = YoJson.member "transactionIndex" result_json |> YoJson.to_string |> int_of_string |> UInt64.of_int in
      let block_number = YoJson.member "blockNumber" result_json |> YoJson.to_string |> int_of_string |> Revision.of_int in
      let block_hash = YoJson.member "blockHash" result_json |> YoJson.to_string |> Digest.of_0x_string in
      Some (Ok Confirmation.{transaction_hash; transaction_index; block_number; block_hash})

let get_confirmation transaction_hash =
  let interval_seconds = 10.0 in
  let open Lwt in
  retry_until_some interval_seconds transaction_hash
    (fun h -> Ethereum_transaction.get_transaction_receipt h
      >>= (confirmation_of_transaction_receipt >> return))

let wait_for_confirmation (signed_transaction: TransactionSigned.t) (user_state: UserState.t) =
  let open Lwt in
  Ethereum_transaction.send_transaction_to_net signed_transaction
  >>= fun transaction_json ->
  if (YoJson.mem "error" transaction_json) then
    let error = string_of_yojson (YoJson.member "error" transaction_json) in
    return (Error (Internal_error error), user_state)
  else
    let transaction_hash_string = YoJson.member "result" transaction_json |> YoJson.to_string in
    get_confirmation transaction_hash_string
    >>= fun confirmation ->
    (* TODO: update user state, e.g., with confirmed balance *)
    return (confirmation, user_state)
