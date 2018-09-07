(* config.ml -- location of configuration files *)

let config_envvar_name = "LEGICASH_CONFIG_DIR"

let default_config_dir = "../config"

let get_config_dir () =
  try Sys.getenv config_envvar_name
  with Not_found ->
    Logging.log "Did not find environment variable %s, using default config directory %s"
      config_envvar_name default_config_dir;
    default_config_dir

let get_config_filename file =
  let config_dir = get_config_dir () in
  config_dir ^ "/" ^ file
