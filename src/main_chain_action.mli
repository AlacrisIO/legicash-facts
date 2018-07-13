open Crypto
open Main_chain

val transfer_tokens : (Address.t * TokenAmount.t, TransactionSigned.t) user_action

val wait_for_confirmation : (TransactionSigned.t, Confirmation.t) user_action
