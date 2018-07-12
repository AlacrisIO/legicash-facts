open Crypto
open Main_chain

val transfer_tokens : (Address.t * TokenAmount.t, transaction_signed) user_action

val wait_for_confirmation : (transaction_signed, confirmation) user_action
