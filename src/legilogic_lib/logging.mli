(* TODO: use http://erratique.ch/software/logs or https://github.com/codinuum/volt *)

val operator_contract_log : bool
val mkb_json_rpc_log : bool
val side_chain_user_log : bool
val side_chain_log : bool
val side_chain_operator_log : bool
val side_chain_client_log : bool
val side_chain_server_log : bool
val state_update_log : bool
val ethereum_user_log : bool
val ethereum_watch_log : bool
val db_log : bool
val persisting_log : bool
val rpc_log     : bool


val log : ('a, unit, string, unit) format4 -> 'a

val set_log_channel : out_channel -> unit

val set_log_file : string -> unit

