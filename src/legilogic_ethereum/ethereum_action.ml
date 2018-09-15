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
  (* XXX: Y2K-like event soon, for int of milliseconds?? *)
  let last_poll = ref 0. in (* Time of last poll in ms since the epoch *)
  let last_block_number = ref None in (* Last observed block number *)
  let rec subsequent_event_stream : unit -> Revision.t t = fun () ->
    let time_since_last_poll = Unix.gettimeofday () -. !last_poll in
    if time_since_last_poll >= delay then
      Ethereum_json_rpc.eth_block_number ()
      >>= function
      | Ok block_number ->
        last_poll := Unix.gettimeofday (); (* Record successful poll time *)
        let new_block = match !last_block_number with
          | Some bn -> bn <> block_number 
          | None -> true in
        let reached_start_block =
          Revision.compare block_number @@ start_block >= 0 in
        if reached_start_block && new_block then
          (* This is a previously unobserved block at or past the start_block,
             so send a notification about it. *)
          Lwt.return { current_event = block_number
                     ; subsequent_event_stream = subsequent_event_stream () }
        else
          subsequent_event_stream ()
      | Error _ ->
        Lwt_unix.sleep 0.1 (* Poll failed; retry... but not too quickly! *)
        >>= subsequent_event_stream 
    else
      (* Add slightly more time, to ensure we're past the delay point on the
         next iteration. *)
      Lwt_unix.sleep @@ (delay -. time_since_last_poll +. 1.) /. 1000.
      >>= subsequent_event_stream 
  in subsequent_event_stream()

