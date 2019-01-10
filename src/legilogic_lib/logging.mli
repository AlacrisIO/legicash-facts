(* TODO: use http://erratique.ch/software/logs or https://github.com/codinuum/volt *)

val log : ('a, unit, string, unit) format4 -> 'a

val set_log_channel : out_channel -> unit

val set_log_file : string -> unit

