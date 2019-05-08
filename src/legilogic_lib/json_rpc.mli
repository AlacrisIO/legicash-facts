(** Trivial client for JSON-RPC 2.0 over HTTP(S?)
    See https://www.jsonrpc.org/specification *)
open Cohttp
open Action
open Yojsoning

(** Type of a JSON-RPC error. **)
type error =
  { code:    int
  ; message: string
  ; data:    yojson
  } [@@deriving yojson, show]

exception Timeout
exception Malformed_request  of exn              (* Client submitted malformed request *)
exception Rpc_error          of error            (* Error received from server, or sent as a server *)
exception Malformed_response of string * exn     (* Unintelligible response from server *)
exception Bad_status         of Code.status_code (* We can't make sense of the RPC status code *)

(** Default RPC timeout, in seconds *)
val rpc_timeout : float

(** Should we log all RPC calls by default? *)
val rpc_log : bool ref

(** Run the call given by the JSON, the Lwt_exn way. *)
val json_rpc
   : Uri.t
  -> string
  -> (yojson -> 'a)
  -> ('b -> yojson)
  -> ?timeout:float
  -> ?log:bool
  -> 'b
  -> 'a Lwt_exn.t

val exn_to_yojson : exn to_yojson
val exn_of_yojson : exn of_yojson

(** The error codes from and including -32768 to -32000 are reserved for pre-defined errors.

    Any code within this range, but not defined explicitly below is reserved
    for future use. The error codes are nearly the same as those suggested for
    XML-RPC at the following url:
      http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php

    The remainder of the space is available for application defined errors.
*)

val invalid_request:  yojson -> exn (* -32600 The JSON sent is not a valid Request object. *)
val method_not_found: string -> exn (* -32601 The method does not exist / is not available. *)
val invalid_params:   yojson -> exn (* -32602 Invalid method parameter(s). *)
val internal_error:   exn    -> exn (* -32603 Internal JSON-RPC error. *)
val parse_error:      exn    -> exn (* -32700 Invalid JSON was received by the server. An error
                                              occurred on the server while parsing the JSON text. *)

(** other Server errrors: -32000 to -32099 *)
