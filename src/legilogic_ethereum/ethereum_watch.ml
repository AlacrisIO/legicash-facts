open Legilogic_lib
open Lib
open Types
open Action

(* TODO: move the two functions below to their own file ethereum_watch or something,
   that lets you watch the progress of transactions on the ethereum blockchain. *)
let stream_of_poller : delay:float -> (unit, 'value, 'state) async_exn_action -> 'state ->
  'value AsyncStream.t Lwt.t =
  let open Lwter in
  fun ~delay poller state ->
    let nap () = Lwt_unix.sleep delay in
    let rec continue state () =
      poller () state
      >>= function
      | Ok value, new_state ->
        Lwt.return @@ AsyncStream.cons value
                        (nap () >>= continue new_state |> join)
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

