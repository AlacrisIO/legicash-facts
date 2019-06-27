open Legilogic_lib.Action

module LL = Legilogic_lib
module EC = Ethereum_chain
module ECToken = Ethereum_chain.TokenAmount

(****************************** Modules ******************************)

module rec Bid : sig
  type t =
  { blockHash        : LL.Types.Digest.t
  ; blockNumber      : LL.Types.Revision.t option
  ; from             : LL.Signing.Address.t
  ; gas              : ECToken.t
  ; gasPrice         : ECToken.t
  ; hash             : LL.Types.Digest.t
  ; input            : LL.Yojsoning.Bytes.t
  ; nonce            : Ethereum_chain.Nonce.t
  ; to_              : LL.Signing.Address.t
  ; transactionIndex : LL.Types.Revision.t option
  ; value            : ECToken.t }
  (* Calculates the bid price using gasPrice and gas limit *)
  val fee     : Bid.t -> ECToken.t
  (* Creates a new Bid with all fields set to either
     given values or min_int/max_int or neg_infinity/infinity *)
  (* val create  : EC.Nonce.t -> ECToken.t -> ECToken.t -> Bid.t *)
  (* Returns a new Bid with the gas price calculated from the given Bid
     fee and everything else kept the same *)
  (* val set_fee : Bid.t -> ECToken.t -> Bid.t *)
end

and Bidder : sig
  type t =
    { id   : LL.Signing.Address.t
    ; bids : Bid.t list }
    (* Creates a new Bidder with a new Bid with all fields set to
       given values or min_int/max_int or neg_infinity/infinity *)
    (* val create   : LL.Signing.Address.t -> EC.Nonce.t
                   -> ECToken.t list -> ECToken.t
                   -> Bidder.t *)
    (* Calculates the total fees of the Bidder's Bids *)
    val fee      : Bidder.t -> ECToken.t
    (* Compares two Bidders based on the Bid.fee of the last nonce
       of each Bidder *)
    val compare  : Bidder.t -> Bidder.t -> int
    (* Sets the given Bidder's lastBids to the given Bid list *)
    val set_bids : Bidder.t -> Bid.t list -> Bidder.t
    (* Uses the given bidder and given new total fee to calculate the
       individual fee for each of the bids) *)
    (* val set_fee  : Bidder.t -> ECToken.t -> Bidder.t *)
end

and Probability : sig
  type t =
  { probs : float array
  ; lower : int
  ; upper : int }
end

and Distance : sig
  type t =
  | Now
  | Short
  | Medium
  | Long
  (* val long   : LL.Types.Revision.t
  val medium : LL.Types.Revision.t *)
  val maybe_update : LL.Types.Revision.t -> LL.Types.Revision.t -> Distance.t
end

and TxPoolPending : sig
  [@warning "-32"]
  type t =
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  (* Find minimum bidder in TxPool *)
  val find_min_fee        : TxPoolPending.t -> ECToken.t
  (* Reverse each Bidder's Bid list so that the (last) most recent
     nonce is first *)
  val reverse_bidder_bids : TxPoolPending.t -> Bidder.t list
end

and TxPool : sig
  type t =
  { pending : TxPoolPending.t
  ; queued  : (LL.Signing.Address.t * (EC.Nonce.t * Bid.t list) list) list }
  val maybe_update : TxPool.t -> TxPool.t
end

and AuctionEnvironment : sig
  type t =
  { block  : LL.Types.Revision.t
  ; txPool : TxPool.t
  ; prob   : Probability.t }
end

module BidderHeap : sig
end

module User : sig
  type t =
  { id        : LL.Signing.Address.t
  ; nonce     : EC.Nonce.t
  ; gasLimits : ECToken.t list
  ; maxValue  : ECToken.t
  ; start     : LL.Types.Revision.t
  ; deadline  : LL.Types.Revision.t
  ; distance  : Distance.t
  ; previous  : ECToken.t list option list }
end

module ResultBids : sig
  [@warning "-32"]
  type t =
  { bids    : ECToken.t list option list
  ; index   : int
  ; timings : LL.Types.Revision.t list }
end

(********************** Stupid Hardcoded Things **********************)

val userNonce : EC.Nonce.t

val userMaxValue : Ethereum_chain.TokenAmount.t

val userStartBlock : LL.Types.Revision.t

val userDeadline : LL.Types.Revision.t

val fakeProbs : Probability.t

val translate_to_posting_txPool : Ethereum_json_rpc.TxPoolContent.t -> TxPool.t

(*************************** Main Functions **************************)

(* Aggressively tries to get onto the block now or returns original bid
   if the fee of the minimum bidder is greater than our max value *)
val post_now : AuctionEnvironment.t -> User.t -> ECToken.t list option

(* Generates next bidder (either Some Bidder or None) using either
   historical probabilities of transaction fees or predictive
   transaction fees based on distance from deadline *)
val post : AuctionEnvironment.t -> User.t -> ECToken.t list option

(* Runs the post handling function *)
val run_post : AuctionEnvironment.t -> User.t -> ResultBids.t
