(****************************** Modules ******************************)

module rec Bid : sig
  type t =
  { blockHash        : int
  ; blockNumber      : int
  ; from             : int
  ; gas              : float
  ; gasPrice         : float
  ; hash             : int
  ; input            : int
  ; nonce            : int
  ; to_              : int
  ; transactionIndex : int
  ; value            : int }
  (* Calculates the bid price using gasPrice and gas limit *)
  val fee     : Bid.t -> float
  (* Creates a new Bid with all fields set to either
     given values or min_int/max_int or neg_infinity/infinity *)
  val create  : int -> float -> float -> Bid.t
  (* Returns a new Bid with the gas price calculated from the given Bid
     fee and everything else kept the same *)
  val set_fee : Bid.t -> float -> Bid.t
end

and Bidder : sig
  type t =
    { id       : int
    ; lastBids : Bid.t list }
    (* Creates a new Bidder with a new Bid with all fields set to
       given values or min_int/max_int or neg_infinity/infinity *)
    val create   : int -> int -> float list -> float -> Bidder.t
    (* Calculates the total fees of the Bidder's Bids *)
    val fee      : Bidder.t -> float
    (* Compares two Bidders based on the Bid.fee of the last nonce
       of each Bidder *)
    val compare  : Bidder.t -> Bidder.t -> int
    (* Sets the given Bidder's lastBids to the given Bid list *)
    val set_bids : Bidder.t -> Bid.t list -> Bidder.t
    (* Uses the given bidder and given new total fee to calculate the
       individual fee for each of the bids) *)
    val set_fee  : Bidder.t -> float -> Bidder.t
end

and Probability : sig
  type t =
  { probs : float array
  ; lower : int
  ; upper : int }
  val maybe_update : Probability.t -> Probability.t
end

and Distance : sig
  type t =
  | Now
  | Short
  | Medium
  | Long
  val maybe_update : int -> int -> Distance.t
end

and TxPool : sig
  type t =
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  val maybe_update        : TxPool.t -> TxPool.t
  (* Find minimum bidder in TxPool *)
  val find_min_fee        : TxPool.t -> float
  (* Reverse each Bidder's Bid list so that the (last) most recent
     nonce is first *)
  val reverse_bidder_bids : TxPool.t -> Bidder.t list
end

and AuctionEnvironment : sig
  type t =
  { block  : int
  ; txPool : TxPool.t
  ; prob   : Probability.t
  ; fees   : (Distance.t, float array) BatHashtbl.t }
  val maybe_update_block : int -> int
  val maybe_update_fees  : (Distance.t, float array) BatHashtbl.t
                           -> (Distance.t, float array) BatHashtbl.t
  val maybe_update       : AuctionEnvironment.t -> AuctionEnvironment.t
end

module BidderHeap : sig
end

module User : sig
  type t =
  { id        : int
  ; nonce     : int
  ; gasLimits : float list
  ; maxValue  : float
  ; start     : int
  ; deadline  : int
  ; distance  : Distance.t
  ; previous  : Bidder.t option list }
  val maybe_update_nonce : int -> int
end

(***************************** Functions *****************************)

(* Aggressively tries to get onto the block now or returns original bid
   if the fee of the minimum bidder is greater than our max value *)
val post_now : AuctionEnvironment.t -> User.t -> Bidder.t option

(* Generates next bidder (either Some Bidder or None) using either
   historical probabilities of transaction fees or predictive
   transaction fees based on distance from deadline *)
val post : AuctionEnvironment.t -> User.t -> Bidder.t option

(* Runs the post handling function *)
val run_post : AuctionEnvironment.t -> User.t -> Bidder.t option list
