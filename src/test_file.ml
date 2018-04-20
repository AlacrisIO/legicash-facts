(* test file to demonstrate use of jbuilder/dune *)

open Base
open Main_chain
open Side_chain
open Keypairs

let hello () = ignore (Printf.printf "Hello, world from Legicash\n")

let _ = hello ()

let test_signature_verification () =
  let alice_data = "some arbitrary string for Alice to sign" in
  let alice_signature = make_signature alice_keys.private_key alice_data in
  let alice_result =
    is_signature_valid alice_keys.public_key alice_signature alice_data
  in
  let bob_data = "some arbitrary string for Bob to sign" in
  let bob_signature = make_signature bob_keys.private_key bob_data in
  let bob_result =
    is_signature_valid bob_keys.public_key bob_signature bob_data
  in
  let trent_data = "some arbitrary string for Trent to sign" in
  let trent_signature = make_signature trent_keys.private_key trent_data in
  let trent_result =
    is_signature_valid trent_keys.public_key trent_signature trent_data
  in
  Printf.printf
    "Validity of signatures:\n  Alice: %b\n  Bob: %b\n  Trent: %b\n"
    alice_result bob_result trent_result


let _ = test_signature_verification ()
