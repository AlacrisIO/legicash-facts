
let log_channel =
  ref stderr

let set_log_channel x =
  log_channel := x

let log_to_file path =
  set_log_channel (open_out_gen [Open_wronly;Open_append;Open_creat;Open_text] 0o666 path)

let log_line s =
  let oc = !log_channel in
  output_string oc s; output_char oc '\n'; flush oc

let log fmt = Printf.ksprintf log_line fmt
