open Side_chain
open Types
   
type state_update_service =
  { address : Address.t
  ; oper_ref : OperatorState.t ref }


let the_state_update_service_ref : (state_update_service option ref) = ref None


let push_state (sign : Digest.t) : unit =
  let (result_oper : Ethereum_chain.Operation.t) = make_state_update_call sign in
  match result_oper with
  | TransferTokens _ -> bork "That is not supposed to happen 1"
  | CreateContract _ -> bork "That is not supposed to happen 2"
  | CallFunction result_call -> 
    

                                                                     
let do_update (oper_state : OperatorState.t) : unit =
  let current_signed = SignedState.make oper_state.keypair oper_state in
  if (oper_state.committed.signature != current_signed) then
    push_state oper_state current_state

let do_update_lwt : unit -> unit Lwt.t =
  fun () ->
  do_update !oper_ref;
  Lwt_unix.sleep Side_chain_server_config.time_state_update_sec
      

  
let forever_state_update : unit -> unit Lwt.t =
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
    let facilitator_state =
      try
        FacilitatorState.load address
      with Not_found -> initial_facilitator_state address
    in
    let state_ref = ref facilitator_state in
    the_facilitator_service_ref := Some { address; state_ref };
    Lwt.async (const state_ref >>> inner_state_update_request_loop);
    Lwt_exn.return ()

let get_update_state () : FacilitatorState.t =
  !the_facilitator_service_ref
  |> (function Some x -> x | None -> bork "Facilitator service not started")
  |> fun service -> !(service.state_ref)


                                                                         
