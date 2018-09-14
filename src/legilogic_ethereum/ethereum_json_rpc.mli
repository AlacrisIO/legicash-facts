(* JSON RPC interface to Ethereum node
   https://github.com/ethereum/wiki/wiki/JSON-RPC
   https://ethereumbuilders.gitbooks.io/guide/content/en/ethereum_json_rpc.html
   https://wiki.parity.io/JSONRPC
*)
open Legilogic_lib
open Action
open Yojsoning
open Types
open Signing

open Main_chain

(** defaultBlock parameter for ethereum queries
    eth_estimateGas, eth_getBalance, eth_getCode, eth_getTransactionCount, eth_getStorageAt, eth_call
*)
type block_parameter =
  | Block_number of Revision.t
  | Latest
  | Earliest
  | Pending

(** Send a JSON-RPC 2.0 request to the Ethereum client, and return the response. *)
val ethereum_json_rpc :
  string -> (yojson -> 'a) -> ('b -> yojson) ->
  ?timeout:float -> ?log:bool -> 'b -> 'a Lwt_exn.t

(** Make a call or transaction, which won’t be added to the blockchain and returns the used gas,
    which can be used for estimating the used gas. *)
val eth_accounts :
  ?timeout:float -> ?log:bool
  -> unit -> Address.t list Lwt_exn.t

(**  *)
val eth_block_number :
  ?timeout:float -> ?log:bool
  -> unit -> Revision.t Lwt_exn.t

(** Make a call or transaction, which won’t be added to the blockchain and returns the used gas,
    which can be used for estimating the used gas. *)
val eth_estimate_gas :
  ?timeout:float -> ?log:bool
  -> Transaction.t * block_parameter -> TokenAmount.t Lwt_exn.t

(** Get the balance on given account at given block *)
val eth_get_balance :
  ?timeout:float -> ?log:bool
  -> address * block_parameter -> TokenAmount.t Lwt_exn.t

val eth_get_transaction_by_hash :
  ?timeout:float -> ?log:bool
  -> Digest.t -> TransactionInformation.t Lwt_exn.t

(** Returns the number of transactions sent from an address. *)
val eth_get_transaction_count :
  ?timeout:float -> ?log:bool
  -> address * block_parameter -> Nonce.t Lwt_exn.t

val eth_get_transaction_receipt :
  ?timeout:float -> ?log:bool
  -> Digest.t -> TransactionReceipt.t option Lwt_exn.t

(** Send a transaction *)
val eth_send_transaction :
  ?timeout:float -> ?log:bool
  -> Transaction.t -> Digest.t Lwt_exn.t (* TODO: better type *)

(* https://github.com/ethereum/go-ethereum/wiki/Management-APIs *)

(** Import the private_key in the Ethereum client with given password string,
    and return the address of the corresponding account.
    Raise an error if the private key already exists.
*)
val personal_import_raw_key :
  ?timeout:float -> ?log:bool
  -> private_key * string -> Address.t Lwt_exn.t

val personal_list_accounts :
  ?timeout:float -> ?log:bool
  -> unit -> Address.t list Lwt_exn.t

val personal_new_account :
  ?timeout:float -> ?log:bool
  -> string -> Address.t Lwt_exn.t

val personal_unlock_account :
  ?timeout:float -> ?log:bool
  -> address * string * int option -> bool Lwt_exn.t
