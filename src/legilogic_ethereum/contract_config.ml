open Legilogic_lib
open Lib
open Action
open Lwt_exn
open Yojsoning
open Signing
open Persisting
open Types

module ContractConfig = struct
  [@warning "-39-32"]
  type t = { contract_address : Address.t
           ; code_hash : Digest.t
           ; creation_hash : Digest.t
           ; creation_block : Revision.t
           } [@@deriving lens { prefix=true }, rlp, show, eq, yojson {strict = false}]
  include (YojsonPersistable (struct
             type nonrec t = t
             let yojsoning = {to_yojson;of_yojson}
           end) : (PersistableS with type t := t))
end

exception Not_found_because of string

let contract_config_of_config_file config_filename =
  let full_filename = Config.get_config_filename config_filename in
  if not (Sys.file_exists full_filename) then
    raise @@ Not_found_because
               (Printf.sprintf "Contract configuration file %s does not exist" full_filename);
  full_filename |> yojson_of_file |> ContractConfig.of_yojson_exn

let contract_config_to_file config_full_filename =
  catching_arr (ContractConfig.to_yojson >> yojson_to_file config_full_filename)

let contract_config_to_config_file = Config.get_config_filename >> contract_config_to_file

let contract_config_of_db db_key =
  match Db.get db_key with
  | Some s -> ContractConfig.unmarshal_string s
  | None -> raise @@ Not_found_because
                       (Printf.sprintf "No contract configuration in DB at key %s"
                        @@ Hex.unparse_hex_string db_key)

let contract_config_to_db db_key =
  ContractConfig.marshal_string >> of_lwt (Db.put db_key) >>> of_lwt Db.committing

let contract_config_of_creation_hash creation_hash =
  Ethereum_json_rpc.eth_get_transaction_receipt creation_hash
  >>= function
  | None -> bork "No receipt for contract creation tx %s" (Digest.to_0x creation_hash)
  | Some receipt ->
     Ethereum_json_rpc.eth_get_transaction_by_hash creation_hash
     >>= fun transaction_info ->
     let contract_address = Option.get receipt.contract_address in
     let creation_block = receipt.block_number in
     let code_hash = Digesting.digest_of_string (Bytes.to_string transaction_info.input) in
     return ContractConfig.{contract_address; code_hash; creation_hash; creation_block}

let verify_contract_config (config : ContractConfig.t) =
  contract_config_of_creation_hash config.creation_hash >>= fun chain_config ->
  if not (ContractConfig.equal config chain_config) then
    bork "Contract configuration %s not matched by on-chain transaction %s"
      (ContractConfig.to_yojson_string config) (ContractConfig.to_yojson_string chain_config)
  else
    return config

let ensure_contract : ('a -> ContractConfig.t) -> ('a -> ContractConfig.t -> unit Lwt_exn.t) -> 'a
                      -> (unit -> Digest.t Lwt_exn.t) -> ContractConfig.t Lwt_exn.t
  = fun getter setter arg maker ->
  (*Logging.log "ensure_contract looking for contract";*)
  OrExn.catching_arr getter arg |>
    function
    | Ok config ->
       (*Logging.log "ensure_contract: found\n%s" (ContractConfig.to_yojson_string config);*)
       verify_contract_config config
    | Error (Not_found_because _) ->
       (*Logging.log "ensure_contract: making a new one";*)
       maker () >>=
         contract_config_of_creation_hash >>= fun config ->
       (*Logging.log "ensure_contract: made one\n%s" (ContractConfig.to_yojson_string config);*)
       setter arg config >>= const config
    | Error e ->
       (*Logging.log "ensure_contract: failed with %s" (Printexc.to_string e);*)
       fail e

let ensure_contract_of_config_file =
  (fun filename -> Printf.printf "In ensure_contract_of_config_file %S\n%!" filename; filename) >>
  ensure_contract contract_config_of_config_file contract_config_to_config_file

let ensure_contract_of_db =
  (fun db_key -> Printf.printf "In ensure_contract_of_db %s\n%!" (Hex.unparse_hex_string db_key); db_key) >>
  ensure_contract contract_config_of_db contract_config_to_db

