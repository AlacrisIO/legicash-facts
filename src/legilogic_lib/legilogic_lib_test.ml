(* Testing legilogic_lib *)
open Lwt.Infix

open Legilogic_lib
open Integer

let rec range a b =
  if a > b then []            (* Base case *)
  else a :: range (a+1) b     (* Recursive case *)

let test_base = 1000 * Random.int 1000

let test_commit i =
  let key = Nat.(add (of_0x_string "0xdeadbeef00000000") (of_int i) |> to_big_endian_bits) in
  let data = string_of_int (test_base + i) in
  (*Logging.log "Test %d: Putting %s in %s" i (Hex.unparse_0x_data key) (Hex.unparse_0x_data data);*)
  Db.put key data
  >>= fun () ->
  (*Logging.log "Test %d: Committing!" i;*)
  Db.commit ()
  >>= fun () ->
  (*Logging.log "Test %d: Committed, value at %s now %s!"
    i (Hex.unparse_0x_data key) (key |> Db.get |> Lib.Option.get |> Hex.unparse_0x_data);*)
  Lwt.return_unit

let max_test_int = 100

let _ =
  Db.run ~db_name:"testdb"
    (fun () ->
       Db.Test.get_batch_id ()
       >>= fun initial_batch_id ->
       Lwt.join (List.map test_commit (range 1 max_test_int))
       >>= fun () ->
       Db.Test.get_batch_id ()
       >>= fun final_batch_id ->
       let count = final_batch_id - initial_batch_id in
       (*Logging.log "%d distinct commits!" count;*)
       assert (1 <= count && count < 4) ;
       Lwt.return_unit)
