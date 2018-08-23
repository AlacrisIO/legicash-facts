open Lib

let log_channel = ref stderr

let set_log_channel x = log_channel := x

let open_log_file = open_out_gen [Open_wronly;Open_append;Open_creat;Open_text] 0o666

let log_to_file = open_log_file >> set_log_channel

let output_line out line = output_string out line; output_char out '\n'; flush out

let log_line s = output_line !log_channel s

let log fmt = Printf.ksprintf log_line fmt
