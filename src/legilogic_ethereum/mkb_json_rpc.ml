type mkb_rpc_config =
  { scheme : string
  ; host : string
  ; port : int
  ; topic : string
  ; username : string
  ; committee_size : int
  ; min_interval_insertion_micros : int
  ; total_capacity_mem : int
  ; instant_capacity_mem : int
  ; total_throughput_per_min : int
  ; total_throughput_per_sec : int
  ; retention_time : int
  ; retention_size : int
  ; hash_method : string }
[@@deriving of_yojson]

type mkb_topic_description =
  { topic : string
  ; committee_size : int
  ; min_interval_insertion_micros : int
  ; total_capacity_mem : int
  ; instant_capacity_mem : int
  ; total_throughput_per_min : int
  ; total_throughput_per_sec : int
  ; retention_time : int
  ; retention_size : int
  ; hash_method : string }
[@@deriving of_yojson]

type mkb_add_registrar =
  { topic : string
  } [@@deriving of_yojson]


type mkb_send_data =
  {
  }


let mkb_rpc_config =
  lazy
    (let config_file = Config.get_config_filename "mkb_config.json" in
     match yojson_of_file config_file
           |> mkb_rpc_config_of_yojson with
     | Ok config -> config
     | Error msg -> bork "Error loading Mutual Knowledge Base JSON RPC configuration: %s" msg)

let mkb_net =
  lazy
    (let lazy { scheme; host; port } = mkb_rpc_config in
     Uri.make ~scheme ~host ~port ())


let mkb_topic_creation =
  mkb_json_rpc "topic_creation"
    Digest.of_yojson_exn
    (yojson_1arg mkb_topic_description.to_yojson)

let mkb_add_registrar =
  mkb_json_rpc "add_registrar"
    Digest.of_yojson_exn
    (yojson_2arg mkb_topic_description.to_yojson Address.to_yojson)

let mkb_add_account =
  mkb_json_rpc "add_registrar"
    Digest.of_yojson_exn
    (yojson_3arg mkb_topic_description.to_yojson Address.to_yojson Address.to_yojson)

let mkb_send_data =
  mkb_json_rpc "send_data"
    Digest.of_yojson_exn
    (yojson_4arg String.to_yojson String.to_yojson String.to_yojson String.to_yojson)
