open Cohttp
open Cohttp_lwt_unix

open Lib
open Yojsoning
open Action
open Lwt_exn

[@@@warning "-32"]

type error =
  { code : int
  ; message : string
  ; data : yojson [@default `Null] }
[@@deriving yojson, show]

exception Timeout
exception Malformed_request of exn
exception Rpc_error of error
exception Malformed_response of string * exn
exception Bad_status of Code.status_code

(** Type of a JSON-RPC request. *)
type request =
  { json_rpc_version : string [@key "jsonrpc"] (* Must be "2.0" *)
  ; method_name : string [@key "method"]
  ; params : yojson
  ; id : yojson } (* SHOULD be an integer *)
[@@deriving yojson, show]

(** Type of a successful JSON-RPC response. *)
type result_response =
  { jsonrpc : string
  ; result : yojson
  ; id : yojson }
[@@deriving yojson, show]

(** Type of a failed JSON-RPC response. *)
type error_response =
  { jsonrpc : string
  ; error : error
  ; id : yojson }
[@@deriving yojson, show]


let json_rpc_version = "2.0"

(* Global counter to correlate responses and answers in logs. *)
let id_counter = make_counter ()

let rpc_timeout = 10.0

let rpc_log = ref true

let exn_to_yojson = function
  | Rpc_error e -> error_to_yojson e
  | e -> `String (Printexc.to_string e)

let exn_of_yojson yo =
  let open OrString in
  (error_of_yojson yo >>| fun e -> Rpc_error e)
  >>=| fun () ->
  match yo with
  | `String s -> Ok (Internal_error s)
  | _ -> Error (Printf.sprintf "bad exn json: %s" (string_of_yojson yo))

let parse_error e =
  Rpc_error {code= -32700;
             message="An error occurred on the server while parsing the JSON text.";
             data=exn_to_yojson e}
let invalid_request request =
  Rpc_error {code= -32600;
             message="The JSON sent is not a valid Request object.";
             data=request}
let method_not_found name =
  Rpc_error {code= -32601;
             message="The method does not exist / is not available.";
             data= `Assoc [("method", `String name)]}
let invalid_params params =
  Rpc_error {code= -32602;
             message="Invalid method parameter(s).";
             data=params}
let internal_error exn =
  Rpc_error {code= -32603;
             message="Internal JSON-RPC error.";
             data=`String (Printexc.to_string exn)}
(* Additional XML RPC errors:
   let application_error ... = ... -32500 ...
   let system_error ... = ... -32400 ...
   let transport_error ... = ... -32300 ... *)

let make_request : string -> ('a -> yojson) -> 'a -> request Lwt_exn.t =
  fun method_name param_encoder params ->
    (try Ok (param_encoder params) with e -> Error (Malformed_request e))
    |> Lwt.return
    >>= fun params ->
    let id = `Int (id_counter ()) in
    return {json_rpc_version;method_name;params;id}

let decode_response : (yojson -> 'b) -> yojson -> string -> 'b Lwt_exn.t =
  fun result_decoder request_id response ->
    let malformed_response exn = fail (Malformed_response (response, exn)) in
    let checking jsonrpc x id =
      if not (jsonrpc = json_rpc_version) then
        malformed_response (Internal_error "bad json_rpc_version")
      else if not (id = request_id) then
        malformed_response (Internal_error "bad id")
      else
        return x in
    trying (catching_arr yojson_of_string) response
    >>= handling malformed_response
    >>= fun response_json ->
    response_json
    |> trying (catching_arr (result_response_of_yojson >> OrString.get))
    >>= handling (fun _ ->
      response_json
      |> trying (catching_arr (error_response_of_yojson >> OrString.get))
      >>= handling malformed_response
      >>= fun {jsonrpc;error;id} -> checking jsonrpc error id
      >>= fun e -> fail (Rpc_error e))
    >>= fun {jsonrpc;result;id} ->
    checking jsonrpc result id
    >>= trying (catching_arr result_decoder)
    >>= handling malformed_response

let json_rpc server method_name result_decoder param_encoder
      ?(timeout=rpc_timeout) ?(log= !rpc_log) params =
  let (>>==) = Lwt.bind in
  make_request method_name param_encoder params
  >>= fun request ->
  let request_id = request.id in
  let request_str = request |> request_to_yojson |> string_of_yojson in
  if log then
    Logging.log "Sending rpc request to %s: %s" (Uri.to_string server) request_str;
  let timeout_thread =
    Lwt_unix.sleep timeout >>== fun () -> fail Timeout in
  let post_thread =
    Client.post
      ~body:(Cohttp_lwt__.Body.of_string request_str)
      ~headers:(Cohttp.Header.add (Cohttp.Header.init ()) "Content-Type" "application/json")
      server
    >>== fun (resp, body) ->
    let status = Response.status resp in
    (try ignore (Code.code_of_status status); Ok ()
     with _ -> Error (Bad_status status))
    |> Lwt.return
    >>= fun () ->
    Cohttp_lwt.Body.to_string body
    >>== fun response_str ->
    if log then
      Logging.log "Receiving rpc response from %s: %s" (Uri.to_string server) response_str;
    decode_response result_decoder request_id response_str
  in
  Lwt.pick [timeout_thread; post_thread]

module Test = struct

  let%test "encode stuff" =
    let id = id_counter ~increment:0 () in
    Lwt_main.run (make_request "foo" identity (`Int 42) >>= Lwt_exn.arr request_to_yojson)
    = Ok (`Assoc [("jsonrpc", `String "2.0");
                  ("method", `String "foo");
                  ("params", `Int 42);
                  ("id", `Int id)])

  let%test "decode result" =
    Lwt_main.run
      (decode_response YoJson.to_int (`Int 42) "{\"jsonrpc\":\"2.0\",\"result\":1776,\"id\":42}")
    |> OrExn.get |> (=) 1776

  let%test "decode error 1" =
    Lwt_main.run
      (decode_response YoJson.to_int (`Int 42)
         "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-151,\"message\":\"foo\",\"data\":[1]},\"id\":42}")
    = Error (Rpc_error {code= -151; message="foo"; data=`List [`Int 1]})

  let%test "decode error 2" =
    Lwt_main.run
      (decode_response YoJson.to_int (`Int 15)
         "{\"jsonrpc\":\"2.0\",\"id\":15,\"error\":{\"code\":-32602,\"message\":\"non-array args\"}}\n")
    = Error (Rpc_error {code= -32602; message="non-array args"; data=`Null})

  let%test "decode malformed 1" =
    Lwt_main.run
      (decode_response YoJson.to_int (`Int 42) "{\"jsonrpc\":\"2.0\",\"error\":1776,\"id\":42}")
    = Error (Malformed_response ("{\"jsonrpc\":\"2.0\",\"error\":1776,\"id\":42}", Internal_error "Json_rpc.error"))

  let%test "decode malformed 2" =
    Lwt_main.run
      (decode_response YoJson.to_int (`Int 42) "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-1,\"message\":\"foo\"},\"id\":41}")
    = Error (Malformed_response
               ("{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-1,\"message\":\"foo\"},\"id\":41}",
                Internal_error "bad id"))

end
