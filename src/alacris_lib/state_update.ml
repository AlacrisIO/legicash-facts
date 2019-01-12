(* open Json_rpc *)
(* open Side_chain_operator *)
(* open Side_chain_user *)
(* open Ethereum_chain *)
(* open Lib *)
(* open Ethereum_user *)

open Legilogic_lib
open Signing
open Action
open Lwt_exn

open Types

open Legilogic_ethereum
open Side_chain
open Operator_contract
open Digesting
open Side_chain_server_config


type digest_entry =
  { revision : Revision.t
  ; oper_digest : Digest.t}


let init_state : unit -> digest_entry =
  fun () -> {revision = Revision.of_int 0; oper_digest = null_digest}
  



    
let the_digest_entry_ref : (digest_entry ref) = ref (init_state ())

(* Alert to take care of:
   ---lack of gas 
   ---transaction not passed
 *)
let push_state_digest_exn (digest : Digest.t) (operator_revision : Revision.t) (bond : TokenAmount.t) (value : TokenAmount.t) : Digest.t Lwt_exn.t =
  Logging.log "push_state_digest_exn : beginning of function operator_revision=%i" (Revision.to_int operator_revision);
  let (operation : Ethereum_chain.Operation.t) = make_state_update_call digest operator_revision bond in
  let (gas_limit_val : TokenAmount.t option) = None in (* Some kind of arbitrary choice *)
  let (oper_addr : Address.t) = Side_chain_server_config.operator_address in
  Logging.log "push_state_digest_exn : before make_pre_transaction";
  Ethereum_user.make_pre_transaction ~sender:oper_addr operation ?gas_limit:gas_limit_val value 
  >>= fun x ->
  Logging.log "push_state_digest_exn : before confirm_pre_transaction";
  Ethereum_user.confirm_pre_transaction oper_addr x
  >>= fun (_tx, confirmation) ->
  Logging.log "push_state_digest_exn : before eth_get_transaction_receipt";
  Ethereum_json_rpc.eth_get_transaction_receipt confirmation.transaction_hash
  >>= fun x ->
  Logging.log "push_state_digest_exn : after eth_get_transaction_receipt";
  match x with
  | None -> bork "No tx receipt for contract creation"
  | Some _receipt -> return digest


(* Eating the exception, very bad! But needed *)
(*                   
let push_state_digest (digest : Digest.t) : Digest.t Lwt.t =
  Lwt.bind (push_state_digest_exn digest) (fun (_val : Digest.t OrExn.t) -> Lwt.return digest)
 *)

(* Reverse operation: Turning a Lwt.t into a Lwt_exn.t *)
let do_sleep_unix : unit -> unit Lwt_exn.t =
  fun () -> 
  Lwt.bind (Lwt_unix.sleep Side_chain_server_config.time_state_update_sec) (fun () -> return ())


    
(*
let push_update (oper_state : OperatorState.t) : unit Lwt_exn.t =
  let (current_digest : Digesting.digest) = State.digest oper_state.current in
  push_state_digest current_digest
(*  if (oper_state.committed.signature != current_signed) then
    push_state current_signed
  else
    return None*)
 *)


    
(*
let do_update_lwt : unit -> unit Lwt.t =
  fun () ->
  (fun () -> do_update x.oper_digest
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
