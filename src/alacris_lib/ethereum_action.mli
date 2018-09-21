open Legilogic_lib
open Signing

open Legilogic_ethereum
open Ethereum_chain
open Ethereum_json_rpc
open Ethereum_user

(** deposit user tokens to facilitator on the main chain; asynchronous *)
val make_deposit : (Address.t * TokenAmount.t, Transaction.t * SignedTransaction.t) UserAsyncAction.arr

val deposit : (Address.t * TokenAmount.t, Transaction.t * Confirmation.t) UserAsyncAction.arr

