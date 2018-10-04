open Arg

open Legilogic_lib
open Lib
open Yojsoning
open Signing
open Action
open Lwt_exn

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_transaction.Test

let _ = Config.set_application_name "alacris"
let _ = Logging.set_log_file (Config.get_application_home_dir () ^ "/_run/logs/ethereum_prefunder.log")

let amount_ref = ref "100000000000000000000"
let args = ref []

let try_first_match a l =
  list_foldlk
    (fun () r k -> match (try Some (r a) with _ -> None) with
       | None -> k ()
       | Some x -> x)
    () l (fun () -> Lib.bork "Could not interpret argument %S" a)

let ensure_prefunded prefunded_address amount string =
  try_first_match string
    [ Address.of_0x_string >> pair None >> singleton
    ; yojson_of_file
      >> (fun x ->
        let nickname = x |> YoJson.member "nickname" |> YoJson.to_string in
        let keypair = x |> YoJson.member "keypair" |> Keypair.of_yojson_exn in
        [(Some nickname, keypair.address)])
    ; yojson_of_file
      >> decode_keypairs
      >> List.map (fun (nickname, keypair) -> Some nickname, keypair.Keypair.address)]
  |> list_iter_p (fun (nickname, address) ->
    (match nickname with
     | Some name -> register_address name address
     | None -> ());
    ensure_address_prefunded prefunded_address amount address
    >>= fun () -> Ethereum_transaction.ensure_eth_signing_address address)

let _ =
  parse_argv Sys.argv
    [("--amount", Set_string amount_ref, "minimal amount at which to set each account, in wei")]
    (fun x -> args := x :: !args)
    "ethereum_prefunder.exe";
  let amount = TokenAmount.of_string !amount_ref in
  let open Lwt_exn in
  Db.run ~db_name:(Config.get_application_home_dir () ^ "/_run/alacris-server")
    (get_prefunded_address
     >>> fun prefunded_address ->
     (* TODO: Fix race condition #7 and make sure it works with list_iter_p here and above. *)
     list_iter_p (ensure_prefunded prefunded_address amount) (List.rev !args))
