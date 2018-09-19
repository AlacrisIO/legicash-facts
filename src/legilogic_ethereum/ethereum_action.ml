open Legilogic_lib
open Lib
open Yojsoning
open Marshaling
open Persisting
open Types
open Signing
open Action
open Lwt_exn

open Main_chain
open Ethereum_json_rpc

module FinalTransactionStatus = struct
  type t =
    [ `Confirmed of Transaction.t * Digest.t * Confirmation.t
    | `Invalidated of Transaction.t * yojson ]
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

module TransactionStatus = struct
  type t =
    [ FinalTransactionStatus.t
    | `Signed of Transaction.t * SignedTransaction.t
    | `Sent of Transaction.t * SignedTransaction.t * Digest.t ]
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let transaction = function
    | `Signed (tx, _) | `Sent (tx, _, _) | `Confirmed (tx, _, _) | `Invalidated (tx, _) -> tx
end

let confirmation_of_transaction_receipt =
  function
    TransactionReceipt.
      { transactionHash = transaction_hash
      ; transactionIndex = transaction_index
      ; blockNumber = block_number
      ; blockHash = block_hash } ->
    Confirmation.{transaction_hash; transaction_index; block_number; block_hash}

(** Number of blocks required for a transaction to be considered confirmed *)
(* TODO: for production, use 100, not 0 *)
let block_depth_for_confirmation = Revision.of_int 0

exception Not_confirmed_yet

let check_confirmation_deep_enough (confirmation : Confirmation.t) =
  eth_block_number ()
  >>= fun block_number ->
  if Revision.(is_add_valid confirmation.block_number block_depth_for_confirmation
               && compare block_number (add confirmation.block_number block_depth_for_confirmation)
                  >= 0) then
    return confirmation
  else
    fail Not_confirmed_yet

let wait_for_confirmation =
  Lwt_exn.retry ~retry_window:10.0 ~max_window:60.0 ~max_retries:None
    (Ethereum_json_rpc.eth_get_transaction_receipt
     >>> catching_arr Option.get
     >>> arr confirmation_of_transaction_receipt
     >>> check_confirmation_deep_enough)

let send_transaction =
  transaction_to_parameters >> Ethereum_json_rpc.eth_send_transaction

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
    | Ok block_number ->
      (* Is this block at least as big as next_block? *)
      if Revision.compare block_number next_block >= 0 then
        (* This is a previously unobserved block at or past the next_block,
           so send a notification about it. The happy path. *)
        Lwt.return (Ok block_number, Revision.(add one block_number))
      else
        Lwt.return (Error (Internal_error "Start block not reached yet"), next_block)
    | Error e -> Lwt.return (Error e, next_block) in
  stream_of_poller ~delay poller start_block

module TransactionTracker = struct
  type t =
    { address : Address.t
    ; revision : Revision.t
    ; promise : FinalTransactionStatus.t Lwt.t
    ; get : unit -> TransactionStatus.t }
  let db_key address revision =
    Printf.sprintf "ETHT%s%s" (Address.to_big_endian_bits address) (Revision.to_big_endian_bits revision)
  let make address revision status =
    let db_key = db_key address revision in
    let open Lwt in
    let open TransactionStatus in
    let status_ref = ref status in
    let get () = !status_ref in
    let rec continue = function
      | `Signed (t, SignedTransaction.{raw; tx}) ->
        (* TODO: handle failure gracefully *)
        Lwt_exn.(run_lwt (retry ~retry_window:5.0 ~max_window:60.0 ~max_retries:None
                            eth_send_raw_transaction) raw)
        >>= fun transaction_hash ->
        `Sent (t, SignedTransaction.{raw;tx}, transaction_hash) |> update
      | `Sent (transaction, _, transaction_hash) ->
        (* TODO: Also add the possibility of invalidating the transaction,
           by e.g. querying the blockchain for the sending address's current nonce,
           and marking the transaction invalidated if a more recent nonce was found.
           TODO: retry sending after timeout if still not sent. *)
        Lwt_exn.run_lwt wait_for_confirmation transaction_hash
        >>= fun confirmation ->
        `Confirmed (transaction, transaction_hash, confirmation) |> update
      | `Confirmed x -> `Confirmed x |> return
      | `Invalidated x -> `Invalidated x |> return
    and update status =
      Db.put db_key (marshal_string status)
      >>= Db.commit
      >>= fun () ->
      status_ref := status;
      continue status in
    {address; revision; promise= continue status; get}
  let load address revision =
    let db_key = db_key address revision in
    Db.get db_key |> Option.get |> TransactionStatus.unmarshal_string |> make address revision
  module P = struct
    type nonrec t = t
    let marshaling =
      marshaling2
        (fun {address; revision} -> address, revision) load
        Address.marshaling Revision.marshaling
    let yojsoning = yojsoning_of_marshaling marshaling
  end
  include (TrivialPersistable (P) : PersistableS with type t := t)
end

module OngoingTransactions = struct
  include Trie.SimpleTrie (Revision) (TransactionTracker)
  let keys = bindings >> List.map fst
  let load address keys =
    List.map (fun key -> key, TransactionTracker.load address key) keys |> of_bindings
end

module XXUserState = struct
  type t =
    { address: Address.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: OngoingTransactions.t }
  [@@deriving lens { prefix=true }, yojson]
  let marshaling =
    marshaling3
      (fun {address; transaction_counter; ongoing_transactions} ->
         address, transaction_counter, OngoingTransactions.keys ongoing_transactions)
      (fun address transaction_counter ongoing_transactions_keys ->
         {address; transaction_counter;
          ongoing_transactions= OngoingTransactions.load address ongoing_transactions_keys})
      Address.marshaling Revision.marshaling (list_marshaling Revision.marshaling)
  include (TrivialPersistable (struct
             type nonrec t = t
             let marshaling = marshaling
             let yojsoning = {to_yojson;of_yojson}
           end) : PersistableS with type t := t)
end

let user_loop _address =
  bottom ()

(** Stub for gas price. Here set at 50 wei. *)
let stub_gas_price = ref (TokenAmount.of_int 50)

let make_tx_header (value, gas_limit) (user_state: UserState.t) =
  (* TODO: get gas price and nonce from geth
     let open UserAsyncAction in
     user_state
     |> of_lwt_exn eth_gas_price TransactionParameters.t * BlockParameter.t
     >>= fun gas_price ->
  *)
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
  let open UserAction in
  (value, gas_limit)
  |> to_async (make_tx_header
               >>> (fun tx_header -> return Transaction.{tx_header; operation})
               >>> add_pending_transaction)

let transfer_gas_limit = TokenAmount.of_int 21000

let transfer_tokens (recipient, amount) =
  issue_transaction (TransferTokens recipient, amount, transfer_gas_limit)

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
