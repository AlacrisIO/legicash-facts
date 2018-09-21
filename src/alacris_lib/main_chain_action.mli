open Legilogic_lib
open Signing

open Legilogic_ethereum
open Main_chain
open Ethereum_json_rpc
open Ethereum_action

(** deposit user tokens to facilitator on the main chain; asynchronous *)
val make_deposit : (Address.t * TokenAmount.t, Transaction.t * SignedTransaction.t) UserAsyncAction.arr

val deposit : (Address.t * TokenAmount.t, Transaction.t * Confirmation.t) UserAsyncAction.arr

