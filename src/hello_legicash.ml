(* main Legicash executable *)

open Legicash_lib

let _ =
  Printf.printf "Hello, world from Legicash\n" ;
  Printf.printf "Version: %s\n" (Version.get_version_string ())
