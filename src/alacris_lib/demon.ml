


open Ethereum_json_rpc

open

type LastEthereumBlock = (Revision.t * float) (* Ethereum_block / time of day *)


let retrieve_last_ethereum_block : unit -> LastEthereumBlock Lwt.t =
  fun () ->
  simple_client request_state_update_mailbox
    (fun ((_x_unit, x_resolv) : (unit * PairRevisionBlock Lwt.u)) -> GetLastEthereumBlock x_resolv) ()

let inner_demon_loop () =
  let open Lwt in
  let eth_block_float_ref : LastEthereumBlock ref = ref (Revision.zero; Float.of_int 0) in
  let rec inner_loop : unit -> unit Lwt.t =
    fun () ->
    Lwt_mvar.take request_demon_mailbox
    >>= function
    | GetLastEthereumBlock (block_u : Revision.t) ->
       Logging.log "Step of GetLastEthereumBlock";
       let time_of_day = Unix.gettimeofday () in
       let delta_time = time_of_day - !eth_block_float_ref.time in
       if (delta_time < 1) then
         (Lwt.wakeup_later block_u !eth_block_float_ref.block_nbr;
          inner_loop ())
       else
         (infinite_retry Ethereum_json_rpc.eth_block_number ()
          >>= fun block_nbr ->
          Logging.log "We found block_nbr=%s" (Revision.to_string block_nbr);
          eth_block_float_ref.time := {time=time_of_day, block_nbr=block_nbr};
          Lwt.wakeup_later block_u block_nbr;
          inner_loop ())
  in inner_loop ()
