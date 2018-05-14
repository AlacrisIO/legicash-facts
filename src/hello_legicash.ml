(* main Legicash executable *)

open Legicash_lib

let _ = Printf.printf "Hello, world from Legicash\n"

let _ = Ethereum_transaction.go ()
