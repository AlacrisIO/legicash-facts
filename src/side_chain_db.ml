(* side_chain_db.ml -- persistence for facilitator state *)

open Lib
open Crypto
open Keypair
open Side_chain
open LevelDB

let db_key_of_facilitator_address address =
  Format.sprintf "facilitator-0x%s" (Address.to_hex_string address)

let side_chain_db_name = "facilitator_state"

let get_db =
  let db = ref None in
  function () ->
    (* open database on first use; finalizer closes it automatically *)
    if !db == None then
      db := Some (open_db side_chain_db_name);
   option_get !db

let save_facilitator_state (facilitator_state : FacilitatorState.t) =
  let db = get_db () in
  let db_key = db_key_of_facilitator_address facilitator_state.keypair.address in
  put db db_key (FacilitatorState.marshal_string facilitator_state)

let retrieve_facilitator_state facilitator_address =
  let db = get_db () in
  let db_key = db_key_of_facilitator_address facilitator_address in
  let unmarshaled =
    match get db db_key with
    | Some s -> Bytes.of_string s (* TODO: can we avoid copying? *)
    | None -> raise (Internal_error
                       (Format.sprintf "No facilitator state saved for address 0x%s"
                          (Address.to_hex_string facilitator_address)))
  in
  FacilitatorState.unmarshal_bytes unmarshaled

module Test = struct

  open Keypair.Test
  open Side_chain.Test

  let%test "db-save-retrieve" =
    save_facilitator_state trent_state;
    let retrieved_state = retrieve_facilitator_state trent_address in
    retrieved_state = trent_state

end
