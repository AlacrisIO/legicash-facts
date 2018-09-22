open Arg

open Legilogic_lib
open Lib
open Yojsoning
open Signing
open Action
open Lwt_exn

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_json_rpc
open Ethereum_transaction.Test

let _ = Config.set_application_name "legicash"
let _ = Logging.set_log_file (Config.get_application_home_dir () ^ "/_run/logs/ethereum_prefunder.log")

let amount_ref = ref "1000000000000"
let args = ref []

let display_balance display address balance =
  display
    (Address.to_0x_string address)
    (try address |> nickname_of_address |> Printf.sprintf " (%s)" with Not_found -> "")
    (TokenAmount.to_0x_string balance)

let ensure_address_prefunded amount address =
  let open TokenAmount in
  eth_get_balance (address, BlockParameter.Pending)
  >>= fun balance ->
  if compare balance amount >= 0 then
    display_balance (printf "Account %s%s contains %s wei.\n") address balance
  else
    begin
      display_balance (printf "Account %s%s only contains %s wei. Funding.\n") address balance
      >>= fun () ->
      get_prefunded_address ()
      >>= fun prefunded ->
      Ethereum_user.(user_action prefunded
                       UserAsyncAction.(transfer_tokens >>> confirm_transaction))
        (address, sub amount balance)
      >>= fun _ ->
      eth_get_balance (address, BlockParameter.Pending)
      >>= fun balance ->
      display_balance (printf "Account %s%s now contains %s wei.\n") address balance
    end

let try_first_match a l =
  list_foldlk
    (fun () r k -> match (try Some (r a) with _ -> None) with
       | None -> k ()
       | Some x -> x)
    () l (fun () -> Lib.bork "Could not interpret argument %S" a)

let ensure_prefunded amount string =
  try_first_match string
    [ Address.of_0x_string >> pair None >> singleton
    ; yojson_of_file
      >> (fun x ->
        (x |> YoJson.member "nickname" |> YoJson.to_string |> Option.return,
         x |> YoJson.member "address" |> Address.of_yojson_exn))
      >> singleton
    ; yojson_of_file
      >> (fun x ->
        (x |> YoJson.member "nickname" |> YoJson.to_string |> Option.return,
         x |> YoJson.member "keypair" |> YoJson.member "address" |> Address.of_yojson_exn))
      >> singleton
    ; yojson_of_file
      >> decode_keypairs
      >> List.map (fun (x, y) -> Some x, y.Keypair.address)]
  |> list_iter_s (fun (nickname, address) ->
    (match nickname with
     | Some name -> register_address name address
     | None -> ());
    ensure_address_prefunded amount address)

let _ =
  parse_argv Sys.argv
    [("amount", Set_string amount_ref, "minimal amount at which to set each account, in wei")]
    (fun x -> args := x :: !args)
    "ethereum_prefunder.exe";
  let amount = TokenAmount.of_string !amount_ref in
  let open Lwt_exn in
  Db.run ~db_name:(Config.get_application_home_dir () ^ "/_run/legicash-server")
    (fun () -> list_iter_s (ensure_prefunded amount) !args)
