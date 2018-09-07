(* config.ml -- location of configuration files *)

let legicash_home_envvar= "LEGICASH_HOME"

let default_config_dir = "../config"

let get_config_dir () =
  try
    let legicash_home = Sys.getenv legicash_home_envvar in
    legicash_home ^ "/config"
  with Not_found ->
    Logging.log "Did not find environment variable %s, using default config directory %s"
      legicash_home_envvar default_config_dir;
    default_config_dir

let get_config_filename file =
  let config_dir = get_config_dir () in
  config_dir ^ "/" ^ file
