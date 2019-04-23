type mkb_rpc_config =
  { scheme : string
  ; host : string
  ; port : int }
[@@deriving of_yojson]



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


    
