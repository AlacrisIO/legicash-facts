(* side_chain_server_config -- TCP/IP server to receive client requests *)

open Legilogic_lib
open Types

module Side_chain_server_config = struct 

  type ethereum_parameter_config =
    { minimal_number_block_for_confirmation : int }
  [@@deriving of_yojson]

  type leveldb_parameter_config =
    { batch_timeout_trigger_in_seconds : float
    ; batch_size_trigger_in_requests : int
    }
  [@@deriving of_yojson]

  type side_chain_server_config =
    { port : int
    ; eth_param_conf : ethereum_parameter_config
    ; leveldb_param_conf : leveldb_parameter_config
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
                 
  let (config : side_chain_server_config) = 
    "side_chain_server_config.json"
    |> Config.get_config_filename
    |> Yojsoning.yojson_of_file
    |> side_chain_server_config_of_yojson
    |> function
    | Ok config -> config
    | Error msg -> Lib.bork "Error loading side chain server configuration: %s" msg




    
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, config.port))

  (* let minNbBlockConfirm = lazy (match config with lazy {minimal_number_block_for_confirmation={nb}} -> nb) *)
  let (minNbBlockConfirm : Revision.t) = Revision.of_int config.eth_param_conf.minimal_number_block_for_confirmation

  let (batch_timeout_trigger_in_seconds : float) = config.leveldb_param_conf.batch_timeout_trigger_in_seconds

  let (batch_size_trigger_in_requests : int) = config.leveldb_param_conf.batch_size_trigger_in_requests
                        
end
                          
