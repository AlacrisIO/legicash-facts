(* side_chain_server_config -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Types
open Action
open Signing
open Ethereum_chain
   
module Side_chain_server_config = struct 

  (** TODO: encrypt the damn file! *)
  type operator_keys_config =
    { nickname : string
    ; keypair : Keypair.t }
  [@@deriving of_yojson]

  let operator_address =
    "operator_keys.json"
    |> Config.get_config_filename
    |> Yojsoning.yojson_of_file
    |> operator_keys_config_of_yojson
    |> OrString.get
    |> fun { nickname; keypair } ->
    let address = keypair.address in
    Logging.log "Using operator keypair %S %s" nickname (Address.to_0x address);
    register_keypair nickname keypair;
    address

  
  type ethereum_config_t =
    { minimal_height_block_for_confirmation : int
    ; max_connection_geth : int
    ; deposit_gas_limit : int
    ; time_state_update_in_seconds : float
    }
  [@@deriving of_yojson]

  type leveldb_config_t =
    { batch_timeout_trigger_in_seconds : float
    ; batch_size_trigger_in_requests : int
    }
  [@@deriving of_yojson]

  type sidechain_config_t =
    { num_timestamps : int
    ; delay_wait_ethereum_watch_in_seconds : float
    ; challenge_duration_in_seconds : int
    }
  [@@deriving of_yojson]

  type fee_schedule_config_t =
    { deposit_fee : string
    ; withdrawal_fee : string
    ; per_account_limit : string
    ; fee_per_billion : string
    ; bond_value : string
    }
  [@@deriving of_yojson]
    


    
  type side_chain_server_config =
    { port : int
    ; ethereum_config : ethereum_config_t
    ; leveldb_config : leveldb_config_t
    ; sidechain_config : sidechain_config_t
    ; fee_schedule_config : fee_schedule_config_t
    }
  [@@deriving of_yojson]

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

       
  let config = get_server_config()

  let (minNbBlockConfirm : Revision.t) = Revision.of_int config.ethereum_config.minimal_height_block_for_confirmation

  let (batch_timeout_trigger_in_seconds : float) = config.leveldb_config.batch_timeout_trigger_in_seconds


                                                 
  let (batch_size_trigger_in_requests : int) = config.leveldb_config.batch_size_trigger_in_requests

  (** This is a hardcoded value in Ethereum so we cannot change it *)
  let (transfer_gas_limit : TokenAmount.t) = TokenAmount.of_int 21000

  let (deposit_gas_limit : TokenAmount.t) = TokenAmount.of_int 100000
                                           
  let (time_state_update_sec : float) = config.ethereum_config.time_state_update_in_seconds

  let (num_timestamps : int) = config.sidechain_config.num_timestamps

  let (delay_wait_ethereum_watch_in_seconds : float) = config.sidechain_config.delay_wait_ethereum_watch_in_seconds

  let (challenge_duration_in_seconds_i : int) = config.sidechain_config.challenge_duration_in_seconds

  let (challenge_duration_in_seconds_f : float) = Float.of_int challenge_duration_in_seconds_i
                                                

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
                          
