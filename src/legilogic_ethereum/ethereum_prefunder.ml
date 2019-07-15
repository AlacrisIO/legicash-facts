open Arg

open Legilogic_lib
open Lib
open Yojsoning
open Signing
open Action
open Json_rpc

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_user.Test
open Batch.Test

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

let with_error_logging : (unit -> string) -> ('i, unit) Lwt_exn.arr -> ('i, unit) Lwt_exn.arr
      = fun make_msg f arg ->
  let open Lwt in
  f arg >>= function
  | Ok _ -> Lwt_exn.return ()
  | Error e -> Logging.log "%s: %s" (make_msg ()) (exn_to_yojson e |> string_of_yojson); Lwt_exn.return ()

let ensure_signing_address address =
  with_error_logging
    (fun () -> Printf.sprintf "Error trying to register key for %s"
                 (nicknamed_string_of_address address))
    Ethereum_transaction.ensure_eth_signing_address
    address

let argument_addresses string =
  try_first_match string
    [ Address.of_0x >> singleton
    ; yojson_of_file
      >> (fun x ->
        let nickname = x |> YoJson.member "nickname" |> YoJson.to_string in
        let keypair = x |> YoJson.member "keypair" |> Keypair.of_yojson_exn in
        Signing.register_keypair nickname keypair;
        [keypair.address])
    ; yojson_of_file
      >> decode_keypairs
      >> List.map (fun (nickname, keypair) ->
        Signing.register_keypair nickname keypair;
        keypair.Keypair.address) ]


let _ =
  parse_argv Sys.argv
    [("--amount", Set_string amount_ref, "minimal amount at which to set each account, in wei")]
    (fun x -> args := x :: !args)
    "ethereum_prefunder.exe";
  let amount = TokenAmount.of_string !amount_ref in
  let open Lwt_exn in
  Logging.log "Ethereum Prefunder";
  Db.run ~db_name:(Config.get_application_home_dir () ^ "/_run/ethereum_prefunder_db")
     let addresses = List.concat (List.map argument_addresses (List.rev !args)) in
     ensure_addresses_prefunded amount addresses >>= fun _ ->
     list_iter_s ensure_signing_address addresses
