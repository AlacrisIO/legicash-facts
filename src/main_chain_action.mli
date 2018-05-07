open Legibase
open Main_chain

val transfer_tokens :
  (Address.t * TokenAmount.t, transaction_signed) user_action
