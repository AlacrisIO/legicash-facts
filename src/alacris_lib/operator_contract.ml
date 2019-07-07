(* operator_contract.ml -- OCaml interface to contract entry points *)

open Legilogic_lib
open Types
open Signing
open Action
open Lib
open Legilogic_ethereum
open Ethereum_chain
open Ethereum_abi
open Side_chain_server_config
open Digesting
open Yojsoning

let operator_contract_log = false

let topic_of_address (addr : Address.t) : Bytes.t option =
  Some (encode_function_parameters [abi_address addr])

let topic_of_revision (rev : Revision.t) : Bytes.t option =
  Some (encode_function_parameters [abi_revision rev])

let topic_of_amount (amnt : TokenAmount.t) : Bytes.t option =
  Some (encode_function_parameters [abi_token_amount amnt])

let topic_of_hash (hash : Digest.t) : Bytes.t option =
  Some (encode_function_parameters [abi_digest hash])





(** build the encoding of a call to the "deposit" function of the operator contract
    address argument is the operator *)
let make_deposit_call : operator:Address.t -> contract_address:Address.t -> Ethereum_chain.Operation.t =
  fun ~operator ~contract_address ->
  if operator_contract_log then
    Logging.log "Beginning of make_deposit_call";
  let parameters = [ abi_address operator ] in
  let call = encode_function_call { function_name = "deposit"; parameters } in
  Operation.CallFunction (contract_address, call)



let pre_deposit : operator:Address.t -> amount:TokenAmount.t -> contract_address:Address.t -> PreTransaction.t =
  fun ~operator ~amount ~contract_address ->
  if operator_contract_log then
    Logging.log "Beginning of pre_deposit";
  let oper = make_deposit_call ~operator ~contract_address in
  PreTransaction.{operation=oper; value=amount; gas_limit=Side_chain_server_config.deposit_gas_limit}





type contract_address_config =
  { contract_address : Address.t
  ; code_hash : Types.Digest.t
  ; creation_hash : Types.Digest.t
  ; creation_block : Revision.t
  } [@@deriving of_yojson]




let rec get_contract_address_config_iter : unit -> contract_address_config Lwt.t =
  fun () ->
  let open Lwt in
  Logging.log "One call to get_contract_address_config_iter";
  let full_filename = Config.get_config_filename "contract_address.json" in
  let test = Sys.file_exists full_filename in
  if test then
    (let config_client = full_filename |> yojson_of_file |> contract_address_config_of_yojson in
     match config_client with
     | Ok x -> Lwt.return x
     | Error msg -> Lib.bork "Error loading side chain client configuration: %s" msg)
  else
    (Lwt_unix.sleep 1.0
     >>= get_contract_address_config_iter)




let test_equality_contract_config : contract_address_config -> contract_address_config -> bool =
  fun e_config f_config ->
  let result = ref true in
  if not (String.equal (Address.to_string e_config.contract_address) (Address.to_string f_config.contract_address)) then
    (if operator_contract_log then
       Logging.log "Equality failure at contract_address a=%s b=%s" (Address.to_string e_config.contract_address) (Address.to_string f_config.contract_address);
     result := false
    );
  if not (String.equal (Digest.to_string e_config.code_hash) (Digest.to_string f_config.code_hash)) then
    (if operator_contract_log then
       Logging.log "Equality failure at code_hash a=%s b=%s" (Digest.to_string e_config.code_hash) (Digest.to_string f_config.code_hash);
     result := false
    );
  if not (String.equal (Digest.to_string e_config.creation_hash) (Digest.to_string f_config.creation_hash)) then
    (if operator_contract_log then
       Logging.log "Equality failure at transaction_hash a=%s b=%s" (Digest.to_string e_config.creation_hash) (Digest.to_string f_config.creation_hash);
     result := false
    );
  if not (Revision.equal e_config.creation_block f_config.creation_block) then
    (if operator_contract_log then
       Logging.log "Equality failure at block_number a=%s b=%s" (Revision.to_string e_config.creation_block) (Revision.to_string f_config.creation_block);
     result := false
    );
  !result







let retrieve_contract_config : Digest.t -> contract_address_config Lwt_exn.t =
  fun transaction_hash ->
  let open Lwt_exn in
  Ethereum_json_rpc.eth_get_transaction_receipt transaction_hash
  >>= function
  | None -> bork "No tx receipt for contract creation"
  | Some receipt ->
     let contract_address = Option.get receipt.contract_address in
     let contract_block_number = receipt.block_number in
     Ethereum_json_rpc.eth_get_transaction_by_hash transaction_hash
     >>= fun trans ->
     let code_hash = Digesting.digest_of_string (Bytes.to_string trans.input) in
     return {contract_address; code_hash; creation_hash=transaction_hash; creation_block=contract_block_number}


let get_contract_address_internal : unit -> Address.t Lwt_exn.t =
  fun () ->
  let open Lwt_exn in
  (Lwt_exn.of_lwt get_contract_address_config_iter) ()
  >>= fun config ->
  retrieve_contract_config config.creation_hash
  >>= fun config_b ->
  let result = test_equality_contract_config config config_b in
  Logging.log "result ? %B" result;
  if result then
    (if operator_contract_log then
       Logging.log "contract_address retrieve successfully";
     return config.contract_address
    )
  else
    (if operator_contract_log then
       Logging.log "Error case for contract_address retrieval";
    bork "inconsistent input for the contract")


let contract_address_ref = ref None

let get_contract_address : unit -> Address.t Lwt_exn.t =
  fun () ->
  match !contract_address_ref with
  | Some x -> Lwt_exn.return x
  | None ->
     (let open Lwt_exn in
      get_contract_address_internal ()
      >>= fun contract_address ->
      contract_address_ref := Some contract_address;
      Lwt_exn.return contract_address)

let rec get_contract_address_exn : unit -> Address.t Lwt.t =
  fun () ->
  let open Lwt in
  get_contract_address ()
  >>= fun address_exn ->
  match address_exn with
  | Ok x -> return x
  | Error _ -> get_contract_address_exn ()



let make_claim_withdrawal_call : contract_address:Address.t -> operator:Address.t -> Revision.t -> value:TokenAmount.t -> confirmed_state:Digest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator operator_revision ~value ~confirmed_state ->
  if operator_contract_log then
    Logging.log "Beginning of make_claim_withdrawal_call";
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "claim_withdrawal"; parameters } in
  Operation.CallFunction (contract_address, call)


(* Here abi_revision = abi_uint64 because Revision = UInt64 *)
let make_withdraw_call : contract_address:Address.t -> operator:Address.t -> Revision.t -> value:TokenAmount.t -> bond:TokenAmount.t -> confirmed_state:Digest.t -> Ethereum_chain.Operation.t =
  fun ~contract_address  ~operator  operator_revision  ~value  ~bond  ~confirmed_state ->
  if operator_contract_log then
    Logging.log "Beginning of make_withdraw";
  let parameters = [ abi_address operator
                   ; abi_revision operator_revision
                   ; abi_token_amount value
                   ; abi_token_amount bond
                   ; abi_digest confirmed_state ] in
  let call = encode_function_call { function_name = "withdraw"; parameters } in
  Operation.CallFunction (contract_address, call)



(* calls the "claim_state_update" that calls "make_claim" that works with a mapping
   from bytes32 to integers.
   We have Revision = UInt64

 *)
let make_state_update_call : contract_address:Address.t -> operator_digest:Digest.t -> operator_revision:Revision.t -> Ethereum_chain.Operation.t =
  fun ~contract_address ~operator_digest ~operator_revision ->
  if operator_contract_log then
    Logging.log "Beginning of make_state_update_call";
  let parameters = [ abi_digest operator_digest; abi_revision operator_revision] in
  let (call : bytes) = encode_function_call { function_name = "claim_state_update"; parameters } in
  Operation.CallFunction (contract_address, call)




(* TODO Add support for including a bond with the claim.
   Which routine to include? Bonds contains:
   ---get_gas_cost_estimate
   ---minimum_bond
   ---require_bond
 *)
