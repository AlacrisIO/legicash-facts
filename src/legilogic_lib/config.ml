(* config.ml -- location of configuration files *)

let get_config_dir () =
  let source_root =
    try Sys.getenv "SOURCE_ROOT"
    with _ -> "."
  in
  source_root ^ "/config"

let get_config_filename file =
  let config_dir = get_config_dir () in
  config_dir ^ "/" ^ file
