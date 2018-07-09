open Action
open Crypto
open Main_chain

let stub_state = ref genesis_state

let stub_state_digest = ref (Digest.zero)

(** Stub for gas price. Here set at 50 wei. *)
let stub_gas_price = ref (TokenAmount.of_int 50)

let [@warning "-32"] update_stub_state new_state =
  stub_state := new_state ;
  stub_state_digest := Digest.zero (* TODO: extract digest from new_state *)

let make_tx_header (user_state, (value, gas_limit)) =
  { TxHeader.sender= user_state.keypair.address
  ; TxHeader.nonce= user_state.nonce
  ; TxHeader.gas_price= !stub_gas_price
  ; TxHeader.gas_limit
  ; TxHeader.value }

let add_pending_transaction (user_state, transaction) =
  ( {user_state with
     pending_transactions= transaction :: user_state.pending_transactions ;
     nonce= Nonce.add Nonce.one user_state.nonce }
  , Ok transaction )

let issue_transaction =
  (fun (user_state, (operation, value, gas_limit)) ->
     (user_state, (value, gas_limit))
     ^|> action_of_pure_action
           (pure_action_seq make_tx_header (fun (user_state, tx_header) ->
              Ethereum_transaction.sign_transaction user_state.keypair {tx_header; operation} )) )
  ^>> add_pending_transaction

let issue_async_transaction (user_state, (operation,value,gas_limit)) =
  let open Lwt in
  (user_state, (value, gas_limit))
  |>
  (async_action_of_pure_action
     (pure_action_seq
        make_tx_header
        (fun (user_state, tx_header) ->
           sign user_state.keypair.private_key {tx_header; operation})))
  ^>>+ fun (user_state,transaction_signed) -> return (add_pending_transaction (user_state,transaction_signed))

let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (user_state, (recipient, amount)) =
  issue_async_transaction (user_state, (TransferTokens recipient, amount, transfer_gas_limit))

let wait_for_confirmation ((user_state: user_state), (_signed_transaction: TransactionSigned.t)) =
  let open Lwt in
  let open Yojson in
  if true then (* TODO: remove this dummy confirmation *)
      let confirmation = { transaction_hash = Digest.make (Random.int 100000)
                         ; transaction_index = Unsigned.UInt64.zero
                         ; block_number = Revision.zero
                         ; block_hash = Digest.make (Random.int 100000)
                         }
      in
      return (user_state, Ok confirmation)
  else (* TODO: run this code *)
  Ethereum_transaction.send_transaction_to_net signed_transaction
  >>= fun transaction_json ->
  let keys = Basic.Util.keys transaction_json in
  if (List.mem "error" keys) then
    let error = Basic.to_string (Basic.Util.member "error" transaction_json) in
    return (user_state, Error (Internal_error error))
  else
    let transaction_hash_string = Basic.Util.member "result" transaction_json |> Basic.Util.to_string in
    Ethereum_transaction.get_transaction_receipt transaction_hash_string
    >>=
    fun receipt_json ->
    let keys = Basic.Util.keys receipt_json in
    if (List.mem "error" keys) then
      let error = Basic.to_string (Basic.Util.member "error" receipt_json) in
      return (user_state, Error (Internal_error error))
    else
      let transaction_hash = Basic.Util.member "transactionHash" receipt_json |> Basic.Util.to_string |> Digest.of_hex_string in
      let transaction_index = Basic.Util.member "transactionIndex" receipt_json |> Basic.Util.to_string |> Unsigned.UInt64.of_string in
      let block_number = Basic.Util.member "blockNumber" receipt_json |> Basic.Util.to_string |> Revision.of_string in
      let block_hash = Basic.Util.member "blockHash" receipt_json |> Basic.Util.to_string |> Digest.of_hex_string in
      let confirmation = { transaction_hash
                         ; transaction_index
                         ; block_number
                         ; block_hash
                         }
      in
      (* TODO: update user state, like confirmed_balance *)
      return (user_state, Ok confirmation)
