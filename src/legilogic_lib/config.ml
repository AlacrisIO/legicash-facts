(* config.ml -- location of configuration files *)

let application_name = ref "application"

let set_application_name s = application_name := s

let get_home_environment_variable () =
  String.uppercase_ascii !application_name ^ "_HOME"

let get_application_home_dir () =
  let home_var = get_home_environment_variable () in
  try
    Sys.getenv home_var
  with Not_found ->
    let cwd = Sys.getcwd () in
    Logging.log "Did not find environment variable %s, using current directory %s"
      home_var cwd;
    cwd

let get_config_dir () =
  get_application_home_dir () ^ "/config"

let get_config_filename file =
  let config_dir = get_config_dir () in
  config_dir ^ "/" ^ file
