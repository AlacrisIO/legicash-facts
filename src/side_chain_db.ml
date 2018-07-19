(* side_chain_db.ml -- persistence for facilitator state *)

open Crypto
open Keypair
open Side_chain

let db_key_of_facilitator_keys keypair =
  Format.sprintf "facilitator-0x%s" (Address.to_hex_string keypair.address)

let side_chain_db_name = "side_chain_state"

(* log facilitator state to disk *)
let log_facilitator_state facilitator_state : unit = Obj.magic 42
(*  let open LevelDB in
  let db = ref None in
  let db_key = ref None
  function (facilitator_state,signed_confirmation) ->
    match db, db_key with
      None, None ->
      (* open on first entry, finalizer will close it *)
      db := Some (open_db side_chain_db_name);
      (* compute this name just once *)
      db_key := Some (db_key_of_facilitator_keys facilitator_state.keypair)
    | Some db, Some db_key ->
      let marshaled =

    | _ -> raise (Internal_error "Inconsistent database handle, database name")
*)
