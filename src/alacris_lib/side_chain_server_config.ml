(* side_chain_server_config -- configuration for the side_chain_server.
   TODO:
   * make the loading lazy, so we can link this library into some code
     without requiring the configuration file to exist and be read.
   * move this module to alacris_lib, it doesn't belong in legilogic_ethereum
 *)

open Legilogic_lib
open Types
open Action
open Signing

open Legilogic_ethereum
open Ethereum_chain

module Side_chain_server_config = struct

  (** TODO: encrypt the damn file! *)
  type operator_keys_config =
    { nickname : string
    ; keypair  : Keypair.t
    } [@@deriving of_yojson]

  let operator_address =
    "operator_keys.json"
    |> Config.get_config_filename
    |> Yojsoning.yojson_of_file
    |> operator_keys_config_of_yojson
    |> OrString.get
    |> fun { nickname; keypair } ->
    let address = keypair.address in
    (* Logging.log "Using operator keypair %S %s" nickname (Address.to_0x address);*)
    register_keypair nickname keypair;
    address

  type leveldb_config_t =
    { batch_timeout_trigger_in_seconds : float
    ; batch_size_trigger_in_requests   : int
    } [@@deriving of_yojson]

  type sidechain_config_t =
    { num_timestamps                       : int
    ; challenge_period_in_blocks           : int
    ; state_update_period_in_seconds       : int
    ; time_state_update_in_seconds         : float
    ; deposit_gas_limit                    : int
    } [@@deriving of_yojson]

  type fee_schedule_config_t =
    { deposit_fee       : string
    ; withdrawal_fee    : string
    ; per_account_limit : string
    ; fee_per_billion   : string
    ; bond_value        : string
    } [@@deriving of_yojson]

  type side_chain_server_config =
    { port                : int
    ; leveldb_config      : leveldb_config_t
    ; sidechain_config    : sidechain_config_t
    ; fee_schedule_config : fee_schedule_config_t
    } [@@deriving of_yojson]

  let load_server_config : unit -> side_chain_server_config =
    fun () ->
    "side_chain_server_config.json"
    |> Config.get_config_filename
    |> Yojsoning.yojson_of_file
    |> side_chain_server_config_of_yojson
    |> function
    | Ok config -> config
    | Error msg -> Lib.bork "Error loading side chain server configuration: %s" msg

  let the_server_config_ref : (side_chain_server_config option ref) = ref None

  let get_server_config : unit -> side_chain_server_config =
    fun () ->
    match !the_server_config_ref with
    | Some x -> x
    | None ->
       let (the_config : side_chain_server_config) = load_server_config() in
       the_server_config_ref := Some the_config;
       the_config

  let config = get_server_config ()

  let (batch_timeout_trigger_in_seconds : float) =
    config.leveldb_config.batch_timeout_trigger_in_seconds

  let (batch_size_trigger_in_requests : int) =
    config.leveldb_config.batch_size_trigger_in_requests

  let (deposit_gas_limit : TokenAmount.t) = TokenAmount.of_int 100000

  let (time_state_update_sec : float) = config.sidechain_config.time_state_update_in_seconds

  let (num_timestamps : int) = config.sidechain_config.num_timestamps

  let (challenge_period_in_blocks : Revision.t) = Revision.of_int config.sidechain_config.challenge_period_in_blocks

  let (state_update_period_in_seconds_f : float) = Float.of_int config.sidechain_config.state_update_period_in_seconds

  (* Recommended default values:
     deposit_fee       = "10000000000000" (* 1e13 wei = 1e-5 ether ~= .24 cent *)
     withdrawal_fee    = "10000000000000" (* 1e13 wei = 1e-5 ether ~= .24 cent *)
     per_account_limit = "10000000000000000000" (* 1e19 wei = 10 ether ~= 2420 USD *)
     fee_per_billion   = "1000000" } (* 1e6/1e9 = 1e-3 = .1%  *)
   *)

  let (deposit_fee_v : TokenAmount.t) = TokenAmount.of_string config.fee_schedule_config.deposit_fee

  let (withdrawal_fee_v : TokenAmount.t) = TokenAmount.of_string config.fee_schedule_config.withdrawal_fee

  let (per_account_limit_v : TokenAmount.t) = TokenAmount.of_string config.fee_schedule_config.per_account_limit

  let (fee_per_billion_v : TokenAmount.t) = TokenAmount.of_string config.fee_schedule_config.fee_per_billion

  let (bond_value_v : TokenAmount.t) = TokenAmount.of_string config.fee_schedule_config.bond_value
end
