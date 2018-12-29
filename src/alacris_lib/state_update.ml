open Legilogic_lib
open Lib
open Signing
open Action
open Lwt_exn
open Json_rpc

open Legilogic_ethereum

open Side_chain
open Side_chain_operator
open Side_chain_user
open Operator_contract
(* open Types *)
(* open Ethereum_chain *)
open Ethereum_user
open Side_chain_server_config
   
type state_update_service =
  { address : Address.t
  ; oper_ref : OperatorState.t}


let the_state_update_service_ref : (state_update_service option ref) = ref None

(* Alert to take care of:
   ---lack of gas 
   ---transaction not passed
 *)
let push_state (digest : Digesting.Digest.t) : unit Lwt_exn.t =
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest in
  let (value : TokenAmount.t) = TokenAmount.zero in
  let (gas_limit_val : TokenAmount.t option) = Some TokenAmount.zero in (* Will not work of course *)
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in 
  Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value 
  >>= Ethereum_user.confirm_pre_transaction oper_addr
  >>= fun (_tx, confirmation) ->
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= function
  | None -> bork "No tx receipt for contract creation"
  | Some receipt -> return ()


                                                                     
let do_update (oper_state : OperatorState.t) : unit Lwt_exn.t =
(*let (current_signed : Signature.t) = (SignedState.make oper_state.keypair oper_state.current).signature in *)
  let (current_digest : Digesting.digest) = State.digest oper_state.current in
  push_state current_digest
(*  if (oper_state.committed.signature != current_signed) then
    push_state current_signed
  else
    return None*)

(*
let do_sleep_unix : unit -> unit Lwt_exn.t =
  fun () -> Lwt_unix.sleep Side_chain_server_config.time_state_update_sec
  
  
let do_update_lwt : unit -> unit Lwt.t =
  fun () ->
  (fun () -> match !the_state_update_service_ref with
             | None -> bork "the_stateupdate_service_ref was not initialized"
             | Some x -> do_update x.oper_ref)
  |> OrExn.get 
  |> do_sleep_unix
      
      

  
let forever_state_update : unit -> unit Lwt_exn.t =
  fun () ->
  Lwt.bind do_update_lwt forever_state_update
                                                                     

                                                                     
let start_state_updater address =
  match !the_state_updater_service_ref with
  | Some x ->
    if Address.equal x.address address then
      (Logging.log "State updater service already running for address %s, not starting another one"
         (Address.to_0x address);
       return ())
    else
      bork "Cannot start a state updater service for address %s because there's already one for %s"
        (Address.to_0x address) (Address.to_0x x.address)
  | None ->
    let operator_state =
      try
        OperatorState.load address
      with Not_found -> initial_operator_state address
    in
    let state_ref = ref operator_state in
    the_operator_service_ref := Some { address; state_ref };
    Lwt.async (const state_ref >>> inner_state_update_request_loop);
    Lwt_exn.return ()

let get_update_state () : OperatorState.t =
  !the_operator_service_ref
  |> (function Some x -> x | None -> bork "Operator service not started")
  |> fun service -> !(service.state_ref)


                                                                         
 *)
