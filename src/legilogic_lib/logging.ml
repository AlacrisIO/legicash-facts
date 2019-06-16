open Lib


let operator_contract_log = true
let mkb_json_rpc_log = true
let side_chain_log = true
let side_chain_user_log = true
let side_chain_operator_log = true
let side_chain_client_log = true
let side_chain_server_log = true
let state_update_log = true
let ethereum_user_log = true
let ethereum_watch_log = true
let db_log = true
let persisting_log = true
let rpc_log = true


let log_channel = ref stderr

let set_log_channel x = log_channel := x

let open_log_file = open_out_gen [Open_wronly;Open_append;Open_creat;Open_text] 0o666

let set_log_file = open_log_file >> set_log_channel

let output_line out line = output_string out line; output_char out '\n'; flush out

let log_line s = output_line !log_channel s

let log fmt = Printf.ksprintf log_line fmt

module Test = struct
  let%test "move logs aside" = set_log_file "test.log"; true
end
