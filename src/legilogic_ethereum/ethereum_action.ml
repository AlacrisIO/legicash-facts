open Legilogic_lib
open Lib
open Types
open Action
open Lwt_exn

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

(*
   let sign_transaction transaction user_state =
   UserAction.return (Transaction.signed user_state.UserState.keypair transaction) user_state
*)

let issue_transaction (operation,value,gas_limit) =
  (value, gas_limit) |>
  let open UserAction in
  to_async (make_tx_header
            >>> (fun tx_header -> return Transaction.{tx_header; operation})
            >>> add_pending_transaction)

let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (recipient, amount) =
  issue_transaction (TransferTokens recipient, amount, transfer_gas_limit)

(* TODO: move to Action? *)
let rec retry_until_some interval_seconds x action =
  let open Lwt_exn in
  x |> action >>=
  function
  | None -> of_lwt Lwt_unix.sleep interval_seconds
    >>= (fun () -> retry_until_some interval_seconds x action)
  | Some z -> return z

let confirmation_of_transaction_receipt =
  Lwt_exn.arr
    (Option.map
       (function
           TransactionReceipt.
             { transactionHash = transaction_hash
             ; transactionIndex = transaction_index
             ; blockNumber = block_number
             ; blockHash = block_hash } ->
           Confirmation.{transaction_hash; transaction_index; block_number; block_hash}))

let get_confirmation transaction_hash =
  let interval_seconds = 10.0 in
  let open Lwt_exn in
  retry_until_some interval_seconds transaction_hash
    (Ethereum_json_rpc.eth_get_transaction_receipt >>> confirmation_of_transaction_receipt)

let wait_for_confirmation =
  UserAsyncAction.of_lwt_exn
    (Ethereum_json_rpc.eth_send_transaction >>> get_confirmation)
(* TODO: update user state, e.g., with confirmed balance *)

let main_chain_block_notification_stream
  ?(delay=30.0) ?(start_block=Revision.zero) () =
  let open Lwt in
  let open EventStream in
  (* Loop which actually produces the stream. [last_block_number_or_infinity] is
     initially [max_int], a block number which can never be reached, to ensure
     that the result from the first poll is reported if necessary. After that,
     it's the last observed block.

     This trick does prevent us from verifying the monotonicity of the block
     numbers. But note that the block numbers are not actually monotonic, since
     there can be rollbacks due to competition between uncles. So we need more
     complex logic than this provides... XXX: *)
  let rec subsequent_event_stream (last_block_number_or_infinity: Revision.t) =
    (* Poll again, after the specified delay, and compare against [block_number]
       to check whether the result is new or not. *)
    let delayed_subsequent_event_stream ?(delay=delay) block_number =
      Lwt_unix.sleep delay >>= fun _ -> subsequent_event_stream block_number in
    Ethereum_json_rpc.eth_block_number ()
    >>= function
    | Ok block_number ->
      let is_new_block = block_number <> last_block_number_or_infinity in
      let reached_start_block = (* Is this block at least as big as start_block? *)
        Revision.compare block_number start_block >= 0 in
      if reached_start_block && is_new_block then
        (* This is a previously unobserved block at or past the start_block,
           so send a notification about it. The happy path. *)
        Lwt.return { current_event = block_number
                   ; subsequent_event_stream =
                       delayed_subsequent_event_stream block_number
                   }
      else
        (* This block_number matches the last value, so  *)
        delayed_subsequent_event_stream last_block_number_or_infinity
    | Error _ -> delayed_subsequent_event_stream
                   ~delay:0.1 last_block_number_or_infinity
  in subsequent_event_stream Revision.max_int
