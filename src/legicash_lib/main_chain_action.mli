open Legilogic_lib
open Signing

open Legilogic_ethereum
open Main_chain

(** deposit user tokens to facilitator on the main chain; asynchronous *)
val deposit : (Address.t * TokenAmount.t, Transaction.t) UserAsyncAction.arr

