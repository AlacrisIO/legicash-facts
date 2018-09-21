(* config.mli -- configuration file location *)

val set_application_name : string -> unit
(** sets application name, used to create environment variable indicating home directory;
    default is "application"
*)

val get_home_environment_variable : unit -> string
(** get environment variable that indicates home directory for the application *)

val get_config_filename : string -> string
(** get full path of config file, relies on home directory environment variable;
    at make time provided by the Makefile; at run-time, it should be provided by
    the shell
*)
