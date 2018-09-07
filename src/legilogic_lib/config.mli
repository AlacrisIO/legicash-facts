(* config.mli -- configuration file location *)

val get_config_filename : string -> string
(** get full path of config file, relies on $LEGICASH_CONFIG_DIR,
    which is provided by the Makefile at make time; at run-time,
    it should be provided by the shall
*)
