(* ethereum_json_rpc.ml -- JSON RPC interface to Ethereum node *)
open Yojsoning

(** constructors for particular JSON RPC calls *)
type ethereum_rpc_call =
  (* DApps methods, use anywhere *)
  | Eth_getBalance
  | Eth_sendTransaction
  | Eth_getTransactionByHash
  | Eth_getTransactionCount
  | Eth_getTransactionReceipt
  (* Geth-specific methods, should only be used in tests *)
  | Personal_importRawKey
  | Personal_listAccounts
  | Personal_newAccount
  | Personal_unlockAccount
[@@deriving show]

val send_rpc_call_to_net : yojson -> yojson Lwt.t
(** run the call given by the JSON *)

val build_json_rpc_call : ethereum_rpc_call -> string list -> yojson
(** build the JSON from the call constructor and a list of string parameters *)

val build_json_rpc_call_with_tagged_parameters :
  ethereum_rpc_call -> yojson list -> yojson
(** build the JSON from the call constructor and a type-tagged list of parameters *)
