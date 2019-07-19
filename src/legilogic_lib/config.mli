(* config.mli -- configuration file location *)

val set_application_name : string -> unit
(** Set the application name, used to create environment variable indicating home directory.
    The default is "application" *)

val get_home_environment_variable : unit -> string
(** Get the name of the environment variable that indicates the home directory for the application.
    The environment variable is the ASCII uppercase of the application name followed by _HOME *)

val get_application_home_dir : unit -> string
(** Get the application home directory for the current application. *)

val get_config_filename : string -> string
(** Get the full path of a configuration file.
    This function relies on the home environment variable, then appends "/config/"
    followed by the provided name. At build time, the variable should be set by the Makefile.
    At run-time, it should be provided by the shell configuration. *)

val get_build_filename : string -> string
(** Get full path of build file, relies on home directory environment variable.
    This function and its result must be used at build time only:
    the build directory might not exist at run-time. *)

val get_run_filename : string -> string
(** Get full path of runtime data file, relies on home directory environment variable.
    This function and its result must be used at runtime only:
    the run directory might not exist at build-time. *)
