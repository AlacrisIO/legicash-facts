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

let stream_of_poller : delay:float -> (unit, 'value, 'state) async_exn_action -> 'state ->
  'value AsyncStream.t Lwt.t =
  let open Lwt in
  fun ~delay poller state ->
    let nap () = Lwt_unix.sleep delay in
    let rec continue state () =
      poller () state
      >>= function
      | Ok value, new_state ->
        Lwt.return @@ AsyncStream.cons value
                        (nap () >>= continue new_state >>= identity)
      | Error _, new_state ->
        nap () >>= continue new_state
    in
    continue state ()

let main_chain_block_notification_stream
      ?(delay=30.0) ?(start_block=Revision.zero)
      ?(get_block=(Ethereum_json_rpc.eth_block_number)) () =
  let rec poller () next_block =
    let open Lwt in
    get_block ()
    >>= function
    | Error e -> Lwt.return (Error e, next_block)
    | Ok block_number ->
      (* Is this block at least as big as start_block? *)
      if Revision.compare block_number next_block >= 0 then
        (* This is a previously unobserved block at or past the start_block,
           so send a notification about it. The happy path. *)
        Lwt.return (Ok block_number, Revision.(add one block_number))
      else
        Lwt.return (Error (Internal_error "Start block not reached yet"), next_block) in
  stream_of_poller ~delay poller start_block

module Test = struct
  let%test "exercise main_chain_block_notification_stream" =
    let open Revision in
    let open Lwt_exn in
    let current_block = ref zero in (* Mock for current mainchain block num *)
    let throw_error = ref None in (* Whether to throw when getting block *)
    let get_block ?timeout ?log () =
      ignore timeout ; ignore log ;
      if !throw_error <> None then (
        throw_error := None;
        fail @@ Option.get !throw_error)
      else (current_block := succ !current_block; return @@ pred !current_block) in
    let start_block = of_int 10 in
    Lwt_exn.run
      (of_lwt (main_chain_block_notification_stream ~start_block ~get_block)
       >>> catching_lwt (AsyncStream.split 2)
       >>> const true)
      ()
end

(* >>= fun (l, s) ->
 * assert(l = [start_block; add one start_block]);
 * catching_lwt (AsyncStream.take_list 1) s
 * >>= func (l, s) ->
 * assert(l = [add start_block (of_int 2)]);
 * throw_error := Some (Internal_error "You FAIL!!!");
 * (\* Deals gracefully with errors? *\)
 * throws (Internal_error "You FAIL!!!")
 *   (fun () -> AsyncStream.take_list stream 1); *)
