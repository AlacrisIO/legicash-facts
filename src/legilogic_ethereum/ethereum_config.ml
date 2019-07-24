open Legilogic_lib
open Lib
open Yojsoning
open Types

module ServerAddress = struct
  type t =
    { scheme : string
    ; host : string
    ; port : int }
      [@@deriving of_yojson]
end

module EthereumConfig = struct
  type t =
    { node_address                          : ServerAddress.t
    ; need_keep_alive                       : bool
    ; minimal_confirmation_height_in_blocks : int
    ; max_connections                       : int
    ; polling_delay_in_seconds              : float
    } [@@deriving of_yojson]
end

let ethereum_config =
  lazy
    (let config_file = Config.get_config_filename "ethereum_config.json" in
     match yojson_of_file config_file |> EthereumConfig.of_yojson with
     | Ok config -> config
     | Error msg -> bork "Error loading Ethereum configuration from %S: %s" config_file msg)

(** Network parameters for geth or other node on localhost *)
let ethereum_net =
  lazy
    (let lazy config = ethereum_config in
     let ServerAddress.{ scheme; host; port } = config.node_address in
     Uri.make ~scheme ~host ~port ())

let minimal_confirmation_height_in_blocks =
  lazy (Revision.of_int (Lazy.force ethereum_config).minimal_confirmation_height_in_blocks)

let polling_delay_in_seconds =
  lazy (Lazy.force ethereum_config).polling_delay_in_seconds

let need_keep_alive =
  lazy (Lazy.force ethereum_config).need_keep_alive
