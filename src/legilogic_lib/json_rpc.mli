(** Trivial client for JSON-RPC 2.0 over HTTP(S?)
    See https://www.jsonrpc.org/specification *)
open Cohttp

open Action
open Yojsoning

(** Type of a JSON-RPC error. **)
type error =
  { code : int
  ; message : string
  ; data : yojson }
[@@deriving yojson, show]

exception Timeout
exception Malformed_request of exn
exception Rpc_error of error
exception Malformed_response of string * exn
exception Bad_status of Code.status_code

(** Default RPC timeout, in seconds *)
val rpc_timeout : float

(** Should we log all RPC calls by default? *)
val rpc_log : bool ref

(** Run the call given by the JSON, the Lwt_exn way. *)
val json_rpc : Uri.t -> string -> (yojson -> 'a) -> ('b -> yojson) ->
  ?timeout:float -> ?log:bool -> 'b -> 'a Lwt_exn.t
