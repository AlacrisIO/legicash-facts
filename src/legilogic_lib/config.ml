(* config.ml -- location of configuration files *)

let application_name = ref "legicash"

let set_application_name s = application_name := s

let get_home_environment_variable () =
  String.uppercase_ascii !application_name ^ "_HOME"

let default_config_dir = "config"

let get_config_dir () =
  let home_var = get_home_environment_variable () in
  try
    let home_dir = Sys.getenv home_var in
    home_dir ^ "/config"
  with Not_found ->
    Logging.log "Did not find environment variable %s, using default config directory %s"
      home_var default_config_dir;
    default_config_dir

let get_config_filename file =
  let config_dir = get_config_dir () in
  config_dir ^ "/" ^ file
