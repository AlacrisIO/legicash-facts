open Legilogic_lib
open Types

(** Configuration specific to the Ethereum network you're using.
    This will be found at runtime in config/ethereum_config.json
    and the parameters will depend on whether this is a private test network (localhost),
    a shared test network (Rinkeby) or the real production network (main net).
 *)

module ServerAddress : sig
  (* TODO: move this module to somewhere in Legilogic_lib *)
  type t =
    { scheme : string
    ; host : string
    ; port : int }
      [@@deriving of_yojson]
end

module EthereumConfig : sig
  type t =
    { node_address                          : ServerAddress.t (* trusted node we're talking to *)
    ; need_keep_alive                       : bool
    ; minimal_confirmation_height_in_blocks : int
    ; max_connections                       : int
    ; polling_delay_in_seconds              : float
    (* TODO:
    ; network_id : int (* will be v parameter for TransactionData pre-signature *)
    ; chain_id : char (* will be v parameter for TransactionData pre-signature; or should we get it from eth_chainId ? *)
       *)
    } [@@deriving of_yojson]
end

val ethereum_config : EthereumConfig.t Lazy.t

(** Network parameters for geth or other node on localhost *)
val ethereum_net : Uri.t Lazy.t

val minimal_confirmation_height_in_blocks : Revision.t Lazy.t
(** Number of blocks required for a transaction to be considered confirmed.
    For tests, 1 is enough, maybe even 0 until we get keep_alive right.
    For production, use 100 for serious business, although metamask uses 12. *)

val polling_delay_in_seconds : float Lazy.t
(** Amount of time to wait when polling the blockchain for new blocks.
    In a private test net, maybe 0.5.
    On Rinkeby, maybe 5?
    On the main net, maybe 5 or 7.5. *)

val need_keep_alive : bool Lazy.t
(** Is this a test Ethereum network that requires dummy transactions to be sent
    to keep the blockchain alive and producing new blocks?
    In a private test net, true.
    On Rinkeby or main net or anything that actually mines or is shared, false. *)
