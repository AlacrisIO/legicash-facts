open Lens.Infix

open Legilogic_lib
open Lib
open Action
open Yojsoning
open Marshaling
open Signing
open Persisting
open Types
open Merkle_trie
open Json_rpc
open Trie
open Side_chain_client

open Legilogic_ethereum
open Side_chain_server_config
open Ethereum_json_rpc
open State_update
open Ethereum_watch
open Ethereum_abi
open Operator_contract
open Digesting

open Side_chain

(** TODO: query the network, whatever, and find the fee schedule *)
let get_operator_fee_schedule _operator_address =
  Lwt_exn.return initial_fee_schedule

(* Topics below correspond to events in the operator.sol code
   Be careful of adjusting everything when you change the type like adding a balance.
 *)


let (topic_of_deposited: Bytes.t option) =
  topic_of_hash (digest_of_string "Deposited(address,address,uint256,uint256)")

let (topic_of_state_update: Bytes.t option) =
  topic_of_hash (digest_of_string "StateUpdate(address,bytes32,uint256,uint64)")

let (topic_of_claim_withdrawal: Bytes.t option) =
  topic_of_hash (digest_of_string "ClaimWithdrawal(address,uint64,uint256,bytes32,uint256,uint256,uint64)")

let (topic_of_withdraw: Bytes.t option) =
  topic_of_hash (digest_of_string "Withdrawal(address,uint64,uint256,uint256,bytes32)")

(** TODO: find and justify a good default validity window in number of blocks *)
let default_validity_window = Duration.of_int 256

let stub_confirmed_main_chain_state = ref Ethereum_chain.genesis_state

let stub_confirmed_main_chain_state_digest = ref (Ethereum_chain.State.digest Ethereum_chain.genesis_state)

let stub_confirmed_side_chain_state = ref Side_chain.State.empty

let stub_confirmed_side_chain_state_digest = ref (State.digest Side_chain.State.empty)

[@@@warning "-32-27"]
let get_keypair_of_address user =
  Lwt_exn.catching_arr keypair_of_address user


module ContractAddrType = struct
  [@warning "-39"]
  type t = { contract_address : Address.t }
  [@@deriving yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end


let get_contract_address_from_client_exn_req () =
  let open Lwt_exn in
  UserQueryRequest.Get_contract_address
    |> post_user_query_request
  >>= fun x ->
    return (ContractAddrType.of_yojson_exn x).contract_address

let contract_address_from_client_ref : (Address.t option ref) = ref None

let get_contract_address_from_client_exn : unit -> Address.t Lwt_exn.t =
  fun () ->
  match !contract_address_from_client_ref with
  | Some x ->
     Logging.log "get_contract_address_from_client_exn 1 : x=%s" (Address.to_0x x);
     Lwt_exn.return x
  | None ->
     Lwt_exn.bind (get_contract_address_from_client_exn_req ())
       (fun x ->
         Logging.log "get_contract_address_from_client_exn 2 : x=%s" (Address.to_0x x);
         contract_address_from_client_ref := Some x;
         Lwt_exn.return x)

let get_contract_address_from_client : unit -> Address.t Lwt.t =
  fun () ->
  Lwt.bind
    (get_contract_address_from_client_exn ())
    (fun x ->
      match x with
      | Error e -> bork "Error in obtaining the contract_address"
      | Ok x -> Lwt.return x)

let get_option : 'a -> 'a option -> 'a =
  fun x_val x_opt ->
  match x_opt with
  | None -> x_val
  | Some x -> x

let wait_for_operator_state_update (contract_address: Address.t)
                                   (operator:         Address.t)
                                   (transaction_hash: Digest.t)
                                 : Ethereum_chain.Confirmation.t Lwt_exn.t =
  let open Lwt_exn in
  Logging.log "Beginning of wait_for_operator_state_update";
  Logging.log "wait_for_operator_state_update contract_address=%s" (Address.to_0x contract_address);
  Logging.log "Before wait_for_contract_event CONTEXT state_update";
  wait_for_contract_event
    ~contract_address
    ~transaction_hash:(Some transaction_hash)
    ~topics:[topic_of_state_update]
    [Address; Bytes 32; Uint 256; Uint 64]
    [Some (Address_value operator); None; None; None]
  >>= fun x ->
  let (x_logo, x_vals) = x in
  let transhash : Digest.t = get_option Digest.zero x_logo.transactionHash in
  Logging.log "wait_for_operator_state_update, transaction_hash=%s" (Digest.to_0x transaction_hash);
  Logging.log "wait_for_operator_state_update,  transhash=%s" (Digest.to_0x transhash);
  Logging.log "wait_for_operator_state_update, RETURN balance=%s" (print_abi_value_uint256 (List.nth x_vals 2));

  (* TODO defaulting to zero is wrong and the presence of `null`s indicates
   * something's broken with the confirmation data; we should instead capture
   * the possibility of invalid state at the type level and force consuming
   * code to deal with it explicitly and unambiguously.
   * TODO: Either only return a TransactionCommitment, or actually wait for Confirmation,
   * but don't return a fake Confirmation. Maybe have separate functions for one
   * and the other, or for one and the work that remains to do for the other.
   *)
  return Ethereum_chain.Confirmation.
    { transaction_hash  = get_option Digest.zero x_logo.transactionHash
    ; transaction_index = get_option Revision.zero x_logo.transactionIndex
    ; block_number      = get_option Revision.zero x_logo.blockNumber
    ; block_hash        = get_option Digest.zero x_logo.blockHash
    }



let wait_for_claim_withdrawal_event (contract_address: Address.t)
                                    (transaction_hash: Digest.t)
                                    (_sender:          Address.t)
                                    (operator:         Address.t)
                                    (revision:         Revision.t)
                                  : unit Lwt_exn.t =
  Logging.log "Beginning of wait_for_claim_withdrawal_event";
  Logging.log "wait_for_claim_withdrawal_event contract_address=%s" (Address.to_0x contract_address);
  let (topics : Bytes.t option list) = [topic_of_claim_withdrawal] in
  let (list_data_type : abi_type list) = [Address; Uint 64; Uint 256; Bytes 32; Uint 256; Uint 256; Uint 64] in
  let (data_value_search : abi_value option list) = [Some (Address_value operator);
                                                     Some (abi_value_from_revision revision);
                                                     None; None; None; None; None] in
  let (transaction_hash_val : Digest.t option) = Some transaction_hash in
  let open Lwt_exn in
  Logging.log "Before wait_for_contract_event CONTEXT claim_withdrawal";
  wait_for_contract_event ~contract_address ~transaction_hash:transaction_hash_val ~topics list_data_type data_value_search
  >>= (fun (x : (LogObject.t * (abi_value list))) ->
    let (_log_object, abi_list_val) = x in
    Logging.log "Now exiting the wait_for_claim_withdrawal_event |b|=%d" (List.length abi_list_val);
    Logging.log "claim_withdrawal, RETURN    bond=%s" (print_abi_value_uint256 (List.nth abi_list_val 4));
    Logging.log "claim_withdrawal, RETURN balance=%s" (print_abi_value_uint256 (List.nth abi_list_val 5));
    Logging.log "claim_withdrawal, RETURN     res=%s" (print_abi_value_uint64  (List.nth abi_list_val 6));
    Lwt_exn.return ())

let emit_claim_withdrawal_operation : Address.t -> Address.t -> Address.t -> Revision.t -> TokenAmount.t -> TokenAmount.t -> Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun contract_address sender operator operator_revision value bond digest ->
  Logging.log "emit_claim_withdrawal_operation : beginning of operation bond=%s" (TokenAmount.to_string bond);
  let (operation : Ethereum_chain.Operation.t) = make_claim_withdrawal_call contract_address operator operator_revision value digest in
  post_operation_general operation sender bond


let emit_withdraw_operation : Address.t -> Address.t -> Address.t -> Revision.t -> TokenAmount.t -> TokenAmount.t -> Digest.t -> TransactionReceipt.t Lwt_exn.t =
  fun contract_address sender operator operator_revision value bond digest ->
  Logging.log "emit_withdraw_operation : beginning of operation";
  Logging.log "emit_withdraw_operation contract_address=%s" (Address.to_0x contract_address);
  let (operation : Ethereum_chain.Operation.t) = make_withdraw_call contract_address operator operator_revision value bond digest in
  let (value_send : TokenAmount.t) = TokenAmount.zero in
  post_operation_general operation sender value_send



let post_operation_deposit (tc:       TransactionCommitment.t) (operator: Address.t) : unit Lwt_exn.t =
  Logging.log "Beginning of post_operation_deposit";
  Logging.log "contract_address contr_addr=%s" (Address.to_0x tc.contract_address);
  let (topics : Bytes.t option list) = [topic_of_deposited] in
  let (list_data_type : abi_type list) = [Address; Address; Uint 256; Uint 256] in
  let (data_value_search : abi_value option list) = [Some (Address_value operator);
                                                     None; None; None] in
  Logging.log "Before wait_for_contract_event CONTEXT deposit";
  Lwt_exn.bind (wait_for_contract_event ~contract_address:tc.contract_address ~transaction_hash:None ~topics list_data_type data_value_search)
    (fun (x : (LogObject.t * (abi_value list))) ->
      let (_log_object, abi_list_val) = x in
      Logging.log "post_operation_deposit, RETURN value=%s" (print_abi_value_uint256 (List.nth abi_list_val 2));
      Logging.log "post_operation_deposit, RETURN balance=%s" (print_abi_value_uint256 (List.nth abi_list_val 3));
      Lwt_exn.return ())


let post_claim_withdrawal_operation (tc:       TransactionCommitment.t)
      (sender: Address.t)
      (operator: Address.t)
    : unit Lwt_exn.t =
  let open Lwt_exn in
  match (tc.transaction.tx_request |> TransactionRequest.request).operation with
    | Deposit _ -> bork "This part should not occur"
    | Payment _ -> bork "This part should not occur"
    | Withdrawal {withdrawal_amount; withdrawal_fee} ->
        Logging.log "Beginning of post_claim_withdrawal_operation, withdrawal";
        emit_claim_withdrawal_operation
           tc.contract_address
           sender
           operator
           tc.tx_proof.key
           withdrawal_amount
           Side_chain_server_config.bond_value_v
           tc.state_digest

        >>= fun tr ->
        Logging.log "post_claim_withdrawal_operation status=%s" (print_status_receipt tr);
        wait_for_claim_withdrawal_event
          tc.contract_address
          tr.transaction_hash
          sender
          operator
          tc.tx_proof.key
        >>= fun () ->
        Logging.log "After the wait_for_claim_withdrawal_event";
        return ()


let final_withdraw_operation_spec (tc:       TransactionCommitment.t)
                             (withdrawal_amount: TokenAmount.t)
                             (sender:   Address.t)
                             (operator: Address.t)
                           : unit Lwt_exn.t =
  let open Lwt_exn in
  let await_challenge_or_emit =
    (* TODO: the challenge duration should be in BLOCKS, not in seconds *)
    Logging.log "Beginning of final_withdraw_operation";
    sleep_delay_exn Side_chain_server_config.challenge_duration_in_seconds_f
    (* TODO actually accept challenges and handle accordingly *)
    >>= fun () -> emit_withdraw_operation
                    tc.contract_address
                    sender
                    operator
                    tc.tx_proof.key
                    withdrawal_amount
                    Side_chain_server_config.bond_value_v
                    tc.state_digest
  in await_challenge_or_emit
    >>= fun tr ->
      Logging.log "final_withdraw_operation_spec status=%s" (print_status_receipt tr);
      let (data_value_search: abi_value option list) =
        [ Some (Address_value operator)
        ; Some (abi_value_from_revision tc.tx_proof.key)
        ; None
        ; None
        ; None
        ] in
      Logging.log "Before wait_for_contract_event CONTEXT withdraw";
      wait_for_contract_event
        ~contract_address:tc.contract_address
        ~transaction_hash:(Some tr.transaction_hash)
        ~topics:[topic_of_withdraw]
        [Address; Uint 64; Uint 256; Uint 256; Bytes 32]
        data_value_search
       >>= const ()

(* TODO: should be more like post_withdrawal or execute_withdrawal *)
let final_withdraw_operation (tc:       TransactionCommitment.t)
                             (sender:   Address.t)
                             (operator: Address.t)
                           : unit Lwt_exn.t =
  match (tc.transaction.tx_request |> TransactionRequest.request).operation with
  | Deposit _ | Payment _ -> Lwt_exn.return ()
  | Withdrawal {withdrawal_amount; withdrawal_fee} ->
     final_withdraw_operation_spec   tc   withdrawal_amount   sender   operator



(* TODO: unstub the stubs *)
let make_rx_header (user:     Address.t)
                   (operator: Address.t)
                   (revision: Revision.t)
                 : RxHeader.t Lwt.t =
  Lwt.return RxHeader.
    { operator
    ; requester                           = user
    ; requester_revision                  = revision
    ; confirmed_main_chain_state_digest   = !stub_confirmed_main_chain_state_digest
    ; confirmed_main_chain_state_revision = !stub_confirmed_main_chain_state.revision
    ; confirmed_side_chain_state_digest   = !stub_confirmed_side_chain_state_digest
    ; confirmed_side_chain_state_revision = !stub_confirmed_side_chain_state.operator_revision
    ; validity_within                     = default_validity_window
    }

let make_user_transaction_request (user:      Address.t)
                                  (operator:  Address.t)
                                  (revision:  Revision.t)
                                  (operation: UserOperation.t)
                                : SignedUserTransactionRequest.t Lwt_exn.t =
  let open Lwt_exn in
  of_lwt (make_rx_header user operator) revision

  >>= fun rx_header ->
    let request = UserTransactionRequest.{rx_header; operation} in
    get_keypair_of_address user

  >>= fun keypair ->
    SignedUserTransactionRequest.make keypair request |> return

module DepositWanted = struct
  [@@@warning "-39"]
  type t =
    { operator:       Address.t
    ; deposit_amount: TokenAmount.t
    ; request_guid:   RequestGuid.t
    ; requested_at:   Timestamp.t
    } [@@deriving yojson]
end

module PaymentWanted = struct
  [@@@warning "-39"]
  type t =
    { operator:          Address.t
    ; recipient:         Address.t
    ; amount:            TokenAmount.t
    ; memo:              string
    ; payment_expedited: bool
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving yojson]
end

module WithdrawalWanted = struct
  [@@@warning "-39"]
  type t =
    { operator:          Address.t
    ; withdrawal_amount: TokenAmount.t
    ; request_guid:      RequestGuid.t
    ; requested_at:      Timestamp.t
    } [@@deriving yojson]
end

module OngoingTransactionStatus = struct
  (* TODO: include a strong reference to the TransactionTracker, so it won't get garbage collected
     at just the wrong moment; make sure it can properly be persisted. Sigh.
     Need to clarify this problem. *)
  [@@@warning "-39"]
  type t =
    | DepositWanted of DepositWanted.t * TokenAmount.t

    | DepositPosted
      of DepositWanted.t
       * TokenAmount.t
       * Ethereum_user.TransactionTracker.Key.t

    | DepositConfirmed
      of DepositWanted.t
       * TokenAmount.t
       * Ethereum_chain.SignedTransactionData.t
       * Ethereum_chain.Confirmation.t

    (* for all operations *)
    | Requested        of UserTransactionRequest.t signed
    | SignedByOperator of TransactionCommitment.t
    | PostedToRegistry of TransactionCommitment.t

    (* for withdrawal only *)
    | PostedToMainChain    of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | ConfirmedOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
  [@@deriving yojson]

  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))

  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | DepositWanted    _
    | DepositPosted    _
    | DepositConfirmed _
      -> None

    | Requested signed_request -> Some signed_request

    | SignedByOperator     tc
    | PostedToRegistry     tc
    | PostedToMainChain    (tc, _)
    | ConfirmedOnMainChain (tc, _)
      -> tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return

  let signed_request = signed_request_opt >> Option.get

  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)

  let request = request_opt >> Option.get

  let status_operator = function
    | DepositWanted    (w, _)
    | DepositPosted    (w, _, _)
    | DepositConfirmed (w, _, _, _)
      -> w.DepositWanted.operator

    | x -> (request x).rx_header.operator

  let status_guid_and_utc = function
    | DepositWanted (w, _) -> (w.request_guid, w.requested_at)

    | DepositPosted    (w, _, _)
    | DepositConfirmed (w, _, _, _)
      -> (w.DepositWanted.request_guid, w.DepositWanted.requested_at)

    | Requested s -> UserOperation.guid_and_utc s.payload.operation

    | _ -> bork "Constructor must be one of: `Requested` or `Deposit{Wanted|Posted|Confirmed}`"
end

module FinalTransactionStatus = struct
  type t =
    | SettledOnMainChain of TransactionCommitment.t * Ethereum_chain.Confirmation.t
    | Failed of OngoingTransactionStatus.t * exn
  [@@deriving yojson]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | SettledOnMainChain (tc, _) -> tc.transaction.tx_request |> TransactionRequest.signed_request |> Option.return
    | Failed (ts, _) -> OngoingTransactionStatus.signed_request_opt ts
end

module TransactionStatus = struct
  type t =
    | Ongoing of OngoingTransactionStatus.t
    | Final of FinalTransactionStatus.t
  [@@deriving yojson]
  module P = struct
    type nonrec t = t
    let yojsoning = {to_yojson;of_yojson}
  end
  include (YojsonPersistable (P) : PersistableS with type t := t)

  let signed_request_opt : t -> UserTransactionRequest.t signed option = function
    | Ongoing x -> OngoingTransactionStatus.signed_request_opt x
    | Final x -> FinalTransactionStatus.signed_request_opt x
  let signed_request = signed_request_opt >> Option.get
  let request_opt : t -> UserTransactionRequest.t option =
    signed_request_opt >> Option.map (fun x -> x.payload)
  let request = request_opt >> Option.get
end

exception TransactionFailed of OngoingTransactionStatus.t * exn
exception NotEnoughFundSec of string

let _ = Printexc.register_printer
          (function
           | TransactionFailed (s, exn) ->
              Some (Printf.sprintf "Side_chain_user.TransactionFailed (%s, %s)"
                      (s |> OngoingTransactionStatus.to_yojson |> string_of_yojson)
                      (Printexc.to_string exn))
           | _                -> None)

let print_ots_state (o: OngoingTransactionStatus.t) : string =
  match o with
  | DepositWanted _ -> "depositwanted"
  | DepositPosted _ -> "depositposted"
  | DepositConfirmed _ -> "depositconfirmed"
  | Requested _ -> "requested"
  | SignedByOperator _ -> "signedbyoperator"
  | PostedToRegistry _ -> "postedtoregistry"
  | PostedToMainChain _ -> "postedtomainchain"
  | ConfirmedOnMainChain _ -> "confirmedonmainchain"

type revision_generator = (unit, Revision.t) Lwter.arr

(*
let retrieve_of_pre_transaction_in_deposit : Address.t -> TokenAmount.t -> OngoingTransactionStatus.t -> Ethereum_user.TransactionTracker.t Lwt_exn.t =
  fun user amount ongoing ->
  Lwt_exn.bind (eth_get_balance (user, BlockParameter.Pending))
    (fun ebalance ->
      let _test : int = TokenAmount.(compare ebalance amount) in
      Ethereum_user.add_ongoing_transaction user ongoing
        (*
      if test >= 0 then
      else
        Lwt.return (Error NotEnoughfund) *)
    )
 *)


module TransactionTracker = struct
  module Base = struct
    module Key = struct
      [@@@warning "-39"]
      type t = { user:         Address.t
               ; operator:     Address.t
               ; revision:     Revision.t
               ; request_guid: RequestGuid.t
               ; requested_at: Timestamp.t
               }
      [@@deriving yojson, rlp]

      include (YojsonMarshalable(struct
           type nonrec t = t
           let yojsoning = {to_yojson; of_yojson}
           let marshaling = marshaling_of_rlping rlping
         end): YojsonMarshalableS with type t := t)
    end

    type key = Key.t
    let key_prefix = "ALTT"
    type context = revision_generator
    module State = TransactionStatus
    type state = State.t
    let make_default_state = persistent_actor_no_default_state key_prefix Key.to_yojson_string

    (* TODO: inspection? cancellation? split private and public activities? *)
    type t = Key.t * FinalTransactionStatus.t Lwt.t

    open Lwter
    let make_activity revision_generator key saving state =
      let Key.{user; operator; revision; request_guid} = key in
      let rec update (status : TransactionStatus.t) =
        saving status >>= Db.committing >>= loop
      and continue (status : OngoingTransactionStatus.t) =
        TransactionStatus.Ongoing status |> update
      and finalize (status : FinalTransactionStatus.t) =
        (* TODO: remove the request from the ongoing_transactions set!
           -- this requires some function added to the context! *)
        Logging.log "Side_chain_user: Beginning of finalize operation";
        TransactionStatus.Final status |> update
      and invalidate transaction_status error =
        Logging.log "Side_chain_user: Beginning of invalidate operation";
        finalize (Failed (transaction_status, error))
      and loop (status: TransactionStatus.t) : FinalTransactionStatus.t Lwt.t =
        match status with
        | Ongoing ongoing ->
          let open OngoingTransactionStatus in
          (match ongoing with
           | DepositWanted (({ operator
                             ; deposit_amount
                             ; request_guid
                             ; requested_at
                             } as deposit_wanted)
                            , deposit_fee) ->
             Logging.log "TR_LOOP, DepositWanted operation";
             (* TODO: have a single transaction for queueing the Wanted and the DepositPosted *)
             let amount = TokenAmount.(add deposit_amount deposit_fee) in
             Lwt.bind
               (Lwt.bind (get_contract_address_from_client ())
                  (fun x ->
                    Logging.log "After the get_contract_address";
                    Lwt.return (Operator_contract.pre_deposit ~operator amount x)))
               (fun x_pre_transaction ->
               Logging.log "We have x_pre_transaction";
               Lwt.bind (eth_get_balance (user, BlockParameter.Pending))
                 (function
                  | Error error ->
                     Logging.log "Error case in eth_get_balance";
                     invalidate ongoing error
                  | Ok ebalance ->
                     Logging.log "Ok case of eth_get_balance";
                     (* test_compar >= 0 corresponds to ebalance >= amount *)
                     let test_compar = TokenAmount.(compare ebalance amount) in
                     Logging.log "test_compar=%i" test_compar;
                     if test_compar < 0 then
                       (Logging.log "invalidate case because of not enough balance";
                        let error_str = Printf.sprintf "Balance %s while trying to deposit %s for a total cost of %s" (TokenAmount.to_string ebalance) (TokenAmount.to_string deposit_amount) (TokenAmount.to_string amount) in
                        let except_ret : exn = raise (NotEnoughFundSec error_str) in
                        invalidate ongoing except_ret)
                     else
                       (Logging.log "Before add_ongoing_transaction, operation";
                        Ethereum_user.add_ongoing_transaction user (Wanted x_pre_transaction)
                        >>= function
                        | Error error -> invalidate ongoing error
                        | Ok (tracker_key, _, _) ->
                           DepositPosted (deposit_wanted, deposit_fee, tracker_key) |> continue)))

             (*
             let contr_addr = get_contract_address () in
             let pre_transaction = Operator_contract.pre_deposit ~operator amnt contr_addr in
             (Ethereum_user.add_ongoing_transaction user (Wanted pre_transaction)
              >>= function
                | Error error -> invalidate ongoing error
                | Ok (tracker_key, _, _) ->
                  DepositPosted (deposit_wanted, deposit_fee, tracker_key) |> continue)
              *)

           | DepositPosted (deposit_wanted, deposit_fee, tracker_key) ->
             Logging.log "TR_LOOP, DepositPosted operation";
             let (_, promise, _) = Ethereum_user.TransactionTracker.get () tracker_key in
             (promise >>= function
              | Failed (_, error) ->
                 Logging.log "DepositPosted, Failed case";
                 Logging.log "error=%s" (Printexc.to_string error);
                 invalidate ongoing error (* TODO: keep the ethereum ongoing transaction status? *)
              | Confirmed (transaction, signed, receipt) ->
                 let txdata = Ethereum_json_rpc.transaction_data_of_signed_transaction signed in
                 let confirmation = Ethereum_json_rpc.TransactionReceipt.to_confirmation receipt in
                 DepositConfirmed (deposit_wanted, deposit_fee, txdata, confirmation) |> continue)
           | DepositConfirmed ( { deposit_amount; request_guid; requested_at }
                              , deposit_fee
                              , main_chain_deposit
                              , main_chain_deposit_confirmation
                              ) ->
             Logging.log "TR_LOOP, DepositConfirmed operation";
             revision_generator ()
             >>= fun (revision : Revision.t) ->
               (make_user_transaction_request
                  (user:     Address.t)
                  (operator: Address.t)
                  (revision: Revision.t)
                  (Deposit { deposit_amount
                           ; deposit_fee
                           ; main_chain_deposit
                           ; main_chain_deposit_confirmation
                           ; request_guid
                           ; requested_at
                           })
              >>= function
                | Ok request  -> Requested request |> continue
                | Error (error : exn) ->
                   Logging.log "DepositConfirmed: side_chain_user, TrTracker, exn=%s" (Printexc.to_string error);
                   Logging.log "DepositConfirmed: side_chain_user: TrTracker, Error case";
                   invalidate ongoing error)

           | Requested request ->
             Logging.log "TR_LOOP, Requested operation";
             (* TODO: handle retries. But it should be in the side_chain_operator *)
             (request
              |> Side_chain_client.post_user_transaction_request
              >>= function
              | Ok (tc : TransactionCommitment.t) ->
                 Logging.log "Requested: side_chain_user: TrTracker, Ok case tc.contr_addr=%s" (Address.to_0x tc.contract_address);
                 SignedByOperator tc |> continue
              | Error (error : exn) ->
                 Logging.log "Requested: side_chain_user: exn=%s" (Printexc.to_string error);
                 Logging.log "Requested: side_chain_user: TrTracker, Error case";
                 invalidate ongoing error)

           | SignedByOperator (tc : TransactionCommitment.t) ->
             Logging.log "TR_LOOP, SignedByOperator operation tc.contr_addr=%s" (Address.to_0x tc.contract_address);
             (* TODO: add support for Shared Knowledge Network / "Smart Court Registry" *)
             PostedToRegistry tc |> continue

           | PostedToRegistry (tc : TransactionCommitment.t) ->
             Logging.log "TR_LOOP, PostedToRegistry operation tc.contr_addr=%s" (Address.to_0x tc.contract_address);
             (* TODO: add support for Mutual Knowledge Base / "Smart Court Registry" *)
             (wait_for_operator_state_update tc.contract_address operator tc.state_update_transaction_hash
              >>= function
              | Ok (c : Ethereum_chain.Confirmation.t) ->
                 Logging.log "PostedToRegistry: side_chain_user: TrTracker, Ok case";
                (match (tc.transaction.tx_request |> TransactionRequest.request).operation with
                 | Deposit _ ->
                    FinalTransactionStatus.SettledOnMainChain (tc, c) |> finalize
                 | Payment _ -> FinalTransactionStatus.SettledOnMainChain (tc, c) |> finalize
                 | Withdrawal _ -> PostedToMainChain (tc, c) |> continue)
              | Error error ->
                 Logging.log "PostedToRegistry: side_chain_user: TrTracker, Error case exn=%s" (Printexc.to_string error);
                 invalidate ongoing error)

           | PostedToMainChain ( tc, confirmation ) ->
             Logging.log "TR_LOOP, PostedToMainChain operation";
             (* Withdrawal that we're going to have to claim *)
             (* TODO: wait for confirmation on the main chain and handle lawsuits
                Right now, no lawsuit *)

             Lwt.bind (post_claim_withdrawal_operation tc user operator) (fun _ ->
                 Logging.log "After post_claim_withdrawal_operation";
                 ConfirmedOnMainChain (tc, confirmation) |> continue)

           | ConfirmedOnMainChain (tc, confirmation) ->
             Logging.log "TR_LOOP, ConfirmedOnMainChain operation";
             (* Confirmed Withdrawal that we're going to have to execute *)
             (* TODO: post a transaction to actually get the money *)
             Lwt.bind (final_withdraw_operation tc user operator) (fun _ ->
                 Logging.log "After final_withdraw_operation";
                 FinalTransactionStatus.SettledOnMainChain (tc, confirmation) |>
                   finalize))

        | Final x -> return x

      in key, loop state
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  let wait promise =
    Logging.log "Beginning of wait operation";
    let open Lwter in
    promise >>= function
    | FinalTransactionStatus.SettledOnMainChain (t, c) ->
       Lwt_exn.return (t, c)
    | FinalTransactionStatus.Failed (o, e) ->
        (match e with
            | Side_chain_operator.Malformed_request r ->
                Logging.log "Malformed request: %s" r
            | _ -> ());
        Lwt_exn.fail (TransactionFailed (o, e))
end



module UserAccountState = struct
  [@warning "-39"]
  type t =
    { is_operator_valid: bool
    ; confirmed_state: AccountState.t
    ; side_chain_revision: Revision.t
    ; transaction_counter: Revision.t
    ; ongoing_transactions: RevisionSet.t }
  [@@deriving lens { prefix=true }, yojson, rlp]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling = marshaling_of_rlping rlping
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
  let empty =
    { is_operator_valid= true
    ; confirmed_state= AccountState.empty
    ; side_chain_revision= Revision.zero
    ; transaction_counter = Revision.zero
    ; ongoing_transactions= RevisionSet.empty }
end

module UserAccountStateMap = MerkleTrie (Address) (UserAccountState)

module UserState = struct
  [@warning "-39"]
  type t =
    { address: Address.t
    ; operators: UserAccountStateMap.t
    ; notification_counter: Revision.t
    ; notifications: (Revision.t * yojson) list }
  [@@deriving lens { prefix=true }, yojson, rlp]
  module PrePersistable = struct
    type nonrec t = t
    let marshaling = marshaling_of_rlping rlping
    let walk_dependencies = no_dependencies
    let make_persistent = normal_persistent
    let yojsoning = {to_yojson;of_yojson}
  end
  include (Persistable (PrePersistable) : PersistableS with type t := t)
end

module UserAsyncAction = AsyncAction(UserState)

let operator_lens : Address.t -> (UserState.t, UserAccountState.t) Lens.t =
  fun operator ->
    UserState.lens_operators |--
    defaulting_lens (konstant UserAccountState.empty)
      (UserAccountStateMap.lens operator)


let get_next_account_revision : Address.t -> unit -> UserState.t -> (Revision.t * UserState.t) Lwt.t =
  fun operator () state ->
    let revision_lens = operator_lens operator |-- UserAccountState.lens_side_chain_revision in
    let (revision : Revision.t) = revision_lens.get state in
    (*    Logging.log "get_next_account_revision revision=%s" (Revision.to_string revision);*)
    Lwt.return (revision, (state |> revision_lens.set Revision.(add one revision)))

module User = struct
  module Base = struct
    module Key = Address
    let key_prefix = "ALUS"
    module State = UserState
    type t = State.t SimpleActor.t
    type context = Key.t -> t (* The get function its own context to pass around! *)

    let make_default_state _context user =
      UserState.{ address              = user
                ; operators            = UserAccountStateMap.empty
                ; notification_counter = Revision.zero
                ; notifications        = [] }

    let next_side_chain_revision user_actor user operator =
      SimpleActor.action (user_actor user) (get_next_account_revision operator)

    let resume_transactions user_actor user (state : State.t) =
      flip UserAccountStateMap.iter state.operators
        (fun operator account ->
           let revision_generator = next_side_chain_revision user_actor user operator in
           flip RevisionSet.iter account.ongoing_transactions
             (fun revision -> TransactionTracker.get
                revision_generator { user
                                   ; operator
                                   ; revision
                                   ; request_guid = RequestGuid.nil
                                   ; requested_at = Timestamp.now ()
                                   } |> ignore))

    let make_activity user_actor user saving state =
      let wrapper transform = Lwter.(transform >>> saving) in
      let actor = SimpleActor.make ~wrapper state in
      (* TODO: maybe just use Lwt_mvar.create state and leave it to users to transact on it ? *)
      resume_transactions user_actor user state; (* TODO: pass the actor as context to that? *)
      actor
  end
  include PersistentActivity(Base)
  module Key = Base.Key
  module State = Base.State
  (* Tie the knot for the fix point to pass as context *)
  let rec user_actor user = get user_actor user
  let make_tracker_context = Base.next_side_chain_revision user_actor
  let action user = SimpleActor.action (user_actor user)
  let transaction user transaction parameters =
    Lwt_exn.(action user transaction parameters >>= fun (_, tracker_promise) ->
             TransactionTracker.wait tracker_promise)
end

let add_ongoing_side_chain_transaction :
  (OngoingTransactionStatus.t, TransactionTracker.t) UserAsyncAction.arr =
  fun transaction_status user_state ->
    let user          = user_state.address in
    let operator      = transaction_status |> OngoingTransactionStatus.status_operator in
    let revision_lens = (operator_lens operator |-- UserAccountState.lens_transaction_counter) in
    let revision      = revision_lens.get user_state in

    let (request_guid, requested_at) =
      OngoingTransactionStatus.status_guid_and_utc transaction_status in

    let open Lwter in
    TransactionTracker.(make
      (User.make_tracker_context user operator)
      Key.{ user
          ; operator
          ; revision
          ; request_guid
          ; requested_at
          }
      ((|>) (TransactionStatus.Ongoing transaction_status)))

    >>= fun tracker ->
      UserAsyncAction.return
        tracker
        (user_state
         |> revision_lens.set Revision.(add one revision)
         |> (operator_lens operator |-- UserAccountState.lens_ongoing_transactions
                                    |-- RevisionSet.lens revision).set true)

let deposit_fee_for OperatorFeeSchedule.{deposit_fee} _deposit_amount =
  deposit_fee

let payment_fee_for OperatorFeeSchedule.{fee_per_billion} payment_amount =
  TokenAmount.(div (mul fee_per_billion payment_amount) one_billion_tokens)

let withdrawal_fee_for OperatorFeeSchedule.{withdrawal_fee} _withdrawal_amount =
  withdrawal_fee

let deposit DepositWanted.{operator; deposit_amount; request_guid; requested_at} =
  let open UserAsyncAction in
  Logging.log "deposit(side_chain_user): using add_ongoing_side_chain_transaction";
  of_lwt_exn get_operator_fee_schedule operator
  >>= fun fee_schedule ->
  let deposit_fee = deposit_fee_for fee_schedule deposit_amount in
  let status = OngoingTransactionStatus.DepositWanted ({ operator
                                                       ; deposit_amount
                                                       ; request_guid
                                                       ; requested_at
                                                       }, deposit_fee) in
  add_ongoing_side_chain_transaction status

let get_user_address : (unit, Address.t) UserAsyncAction.arr =
  fun () user_state -> UserAsyncAction.return user_state.UserState.address user_state

let direct_operation :
  Address.t -> (OperatorFeeSchedule.t -> UserOperation.t, TransactionTracker.t) UserAsyncAction.arr =
  fun operator make_operation ->
    Logging.log "running direct_operation function";
    let open UserAsyncAction in
    of_lwt_exn get_operator_fee_schedule operator

    >>= fun fee_schedule ->
      let operation = make_operation fee_schedule in
      of_lwt_state (get_next_account_revision operator) ()

    >>= fun revision ->
      get_user_address ()

    >>= fun user ->
      of_lwt_exn (make_user_transaction_request user operator revision) operation

    >>= fun signed_request ->
      let status = OngoingTransactionStatus.Requested signed_request in
      add_ongoing_side_chain_transaction status


let payment PaymentWanted.{ operator
                          ; recipient
                          ; amount
                          ; memo
                          ; payment_expedited
                          ; request_guid
                          ; requested_at
                          } : TransactionTracker.t UserAsyncAction.t =
  direct_operation operator (fun fee_schedule ->
       let payment_invoice = Invoice.{recipient; amount; memo}
       and payment_fee     = payment_fee_for fee_schedule amount
       in UserOperation.Payment { payment_invoice
                                ; payment_fee
                                ; payment_expedited
                                ; request_guid
                                ; requested_at
                                })

let withdrawal WithdrawalWanted.{ operator
                                ; withdrawal_amount
                                ; request_guid
                                ; requested_at
                                } : TransactionTracker.t UserAsyncAction.t =
  direct_operation operator (fun fee_schedule ->
       let withdrawal_fee = withdrawal_fee_for fee_schedule withdrawal_amount in
       UserOperation.Withdrawal { withdrawal_amount
                                ; withdrawal_fee
                                ; request_guid
                                ; requested_at
                                })

(*
   let remove_ongoing_transaction operator revision user_state =
   (operator_lens operator
   |-- UserAccountState.lens_ongoing_transactions
   |-- RevisionSet.lens revision).set false user_state

   let get_first_operator_state_option :
   (unit, (Address.t * UserAccountState.t) option) UserAsyncAction.readonly =
   fun () user_state ->
   UserAccountStateMap.find_first_opt (konstant true) user_state.operators

   let get_first_operator =
   UserAsyncAction.(of_readonly get_first_operator_state_option
   >>> function
   | None -> fail No_operator_yet
   | Some (address, _) -> return address)

   (* TODO: is this used? should balances and revisions be updated in effect_request?
   looks like balances already are *)

   let update_account_state_with_trusted_operation
   trusted_operation ({balance} as account_state : AccountState.t) =
   let f =
   {account_state with account_revision= Revision.add account_state.account_revision Revision.one} in
   match trusted_operation with
   | Operation.Deposit {deposit_amount; deposit_fee=_deposit_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.add balance deposit_amount}
   else bork "I mistrusted your deposit operation"
   | Operation.Payment {payment_invoice; payment_fee} ->
   let decrement = TokenAmount.add payment_invoice.amount payment_fee in
   if TokenAmount.compare balance decrement >= 0 then
   {f with balance= TokenAmount.sub balance decrement}
   else bork "I mistrusted your payment operation"
   | Operation.Withdrawal {withdrawal_amount; withdrawal_fee} ->
   if true (\* check that everything is correct *\) then
   {f with balance= TokenAmount.sub balance (TokenAmount.add withdrawal_amount withdrawal_fee)}
   else bork "I mistrusted your withdrawal operation"

   (** We assume most recent operation is to the left of the changes list, *)
   let update_account_state_with_trusted_operations trusted_operations account_state =
   List.fold_right update_account_state_with_trusted_operation trusted_operations account_state

   let [@warning "-32"] optimistic_operator_account_state operator user_state =
   match UserAccountStateMap.find_opt operator user_state.UserState.operators with
   | None -> AccountState.empty
   | Some {is_operator_valid; confirmed_state; pending_operations} ->
   match is_operator_valid with
   | Rejected -> confirmed_state
   | _ ->
   update_account_state_with_trusted_operations
   (List.map (fun x -> x.TransactionStatus.request.payload.operation) pending_operations)
   confirmed_state

   (* TODO: find the actual gas limit *)
   let withdrawal_gas_limit = TokenAmount.of_int 1000000

   (* in Lwt monad, because we'll push the request to the main chain *)
   let withdrawal (operator, withdrawal_amount) =
   let open UserAsyncAction in
   of_lwt_exn get_operator_fee_schedule operator
   >>= fun {withdrawal_fee} ->
   issue_user_transaction_request
   (Withdrawal { withdrawal_amount ; withdrawal_fee })


   (** Given a withdrawal on the side chain, reflect that on the main chain
   We should be signing the RLP, not the marshaling! *)
   let make_main_chain_withdrawal_transaction :
   address -> (UserOperation.withdrawal_details, Ethereum_chain.Transaction.t * Ethereum_json_rpc.SignedTransaction.t) Ethereum_user.UserAsyncAction.arr =
   fun operator UserOperation.{withdrawal_amount;withdrawal_fee} state ->
   (* TODO: should the withdrawal fee agree with the operator state fee schedule? where to enforce? *)
   let ticket = Revision.zero in (* TODO: implement ticketing *)
   let confirmed_state = Digest.zero in (* TODO: is this just a digest of the operator state here? *)
   let bond = TokenAmount.zero in (* TODO: where does this come from? *)
   let operation = Operator_contract.make_withdraw_call
   operator ticket bond confirmed_state in
   let value = TokenAmount.sub withdrawal_amount withdrawal_fee in
   Ethereum_user.(UserAsyncAction.of_lwt_exn
   (make_signed_transaction state.UserState.address operation value) withdrawal_gas_limit)
   state

   let push_side_chain_withdrawal_to_main_chain
   (operator : Address.t)
   (transaction : Transaction.t) =
   let request = transaction.tx_request |> TransactionRequest.request in
   (* We assume it's a transaction of the current user, that the operator committed to *)
   match request.operation with
   | Withdrawal details ->
   details
   |> ethereum_action
   Ethereum_user.UserAsyncAction.
   (make_main_chain_withdrawal_transaction operator
   >>> Ethereum_user.confirm_transaction)
   | Payment _
   | Deposit _ ->
   bork "Side chain transaction does not need subsequent interaction with main chain"

   let add_notification notification_type to_yojson x state =
   let counter = state.UserState.notification_counter in
   UserAsyncAction.return counter
   (state
   |> UserState.lens_notification_counter.set Revision.(add one counter)
   |> Lens.modify UserState.lens_notifications
   (List.cons (counter, `List [`String notification_type; to_yojson x])))

   let add_error_notification = add_notification "error_notification" ErrorNotification.to_yojson

   let _notify_error status error = add_error_notification {status;error}
*)
