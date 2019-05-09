open Batteries

(******************************* Notes *******************************)
(* $1. Potentially use Hashtables instead of Lists to more closely
       match the txpool structure
   $2. Correct units to be what they actually are.
   $3. How do you know when some of your bid was confirmed?
   $4. Add error checking: user transaction fee is over limit.. etc.
   $5. Query for actual updates/values. *)

(******************** Variables & Basic Functions ********************)

(* $5 Query *)
let blocksToConfirm : float = 6.

(* $ I think it's actually 21000? *)
let minimumFee : float = 1.

(* $5 Query. I think? *)
let maximumSize : float = 8000000.

(* $5 Query *)
let get_block_confirmation_time : float = 15.

(* Helper function to sum a list of floats *)
let sum : float list -> float =
  fun fees -> List.fold_left (+.) 0. fees

(****************************** Modules ******************************)

(* The txpool.content can be queried to view an object with two fields:
   - pending: all the transactions currently pending for inclusion in
              the next block(s).
   - queued:  all the ones that are being scheduled for future
              execution only.
   Each of these fields are associative arrays, in which each entry
   maps an origin-address to a batch of scheduled transactions.
   These batches themselves are maps associating nonces with actual
   transactions.
   Example: { blockHash: "0x00000000000000000000000000000000000000000",
              blockNumber: null,
              from: "0x0216d5032f356960cd3749c31ab34eeff21b3395",
              gas: "0x5208",
              gasPrice: "0xba43b7400",
              hash: "0xaf953a2d01f55cfe080c0c94150a60105e8ac3d5115305",
              input: "0x",
              nonce: "0x326",
              to: "0x7f69a91a3cf4be60020fb58b893b7cbb65376db8",
              transactionIndex: null,
              value: "0x19a99f0cf456000" } *)
(* $2 *)
module rec Bid : sig
  [@warning "-32"]
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
  [@@deriving show]
  val fee     : Bid.t -> float
  val create  : int -> float -> float -> Bid.t
  val set_fee : Bid.t -> float -> Bid.t
end = struct
  [@warning "-32"]
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
  [@@deriving show]
  (* Calculates the bid price using gas price and gas limit *)
  let fee : Bid.t -> float =
      fun bid -> bid.gasPrice *. bid.gas
  (* Creates a new Bid with all fields set to either
     given values or min_int/max_int or neg_infinity/infinity *)
  let create : int -> float -> float -> Bid.t =
    fun nonce limit price ->
      Bid.{ blockHash= 0
          ; blockNumber= 0
          ; from= 0
          ; gas= limit
          ; gasPrice= price
          ; hash= 0
          ; input= 0
          ; nonce= nonce
          ; to_= 0
          ; transactionIndex= 0
          ; value= 0 }
  (* Returns a new Bid with the gas price calculated from the given Bid
     fee and everything else kept the same *)
  let set_fee : Bid.t -> float -> Bid.t =
    fun bid newFee ->
      if bid.gas <= 0. then
        failwith "set_fee: gas limit for this bid is 0"
      else
        let newPrice = newFee /. bid.gas in
          Bid.{ blockHash= bid.blockHash
              ; blockNumber= bid.blockNumber
              ; from= bid.from
              ; gas= bid.gas
              ; gasPrice= newPrice
              ; hash= bid.hash
              ; input= bid.input
              ; nonce= bid.nonce
              ; to_= bid.to_
              ; transactionIndex= bid.transactionIndex
              ; value= bid.value }
end

and Bidder : sig
  [@warning "-32"]
  type t =
    (* $1 *)
    { id       : int
    ; lastBids : Bid.t list }
    [@@deriving show]
    val create   : int -> int -> float list -> float -> Bidder.t
    val fee      : Bidder.t -> float
    val compare  : Bidder.t -> Bidder.t -> int
    val set_bids : Bidder.t -> Bid.t list -> Bidder.t
    val set_fee  : Bidder.t -> float -> Bidder.t
end = struct
 type t =
    { id       : int
    ; lastBids : Bid.t list }
    [@@deriving show]
    (* Creates a new Bidder with a new Bid with all fields set to
       given values or min_int/max_int or neg_infinity/infinity *)
    let create : int -> int -> float list -> float -> Bidder.t =
      fun id nonce limits price ->
        if id < 0 || nonce < 0 || price < 0. then
          failwith "Bidder.create: id and nonce must be >= 0"
        else if price <> neg_infinity
             && price <> infinity
             && price <> 0. then
          failwith "Bidder.create: price must be neg_infinity,
                    infinity, 0"
        else
          let bids =
            List.fold_left
              (fun (b : Bid.t list) (limit : float) ->
                if limit < 0. then
                  failwith "Bidder.create: all limits must be >= 0"
                else
                  let newNonce = (nonce + List.length b) in
                  (Bid.create newNonce limit price) :: b)
              []
              limits in
            Bidder.{ id= id
                   ; lastBids= List.rev bids }
    (* Calculates the total fees of the Bidder's Bids *)
    let fee : Bidder.t -> float =
      fun bidder ->
        sum (List.map Bid.fee bidder.lastBids)
    (* Compares two Bidders based on the Bid.fee of the last nonce
       of each Bidder *)
    let compare : Bidder.t -> Bidder.t -> int =
      fun (bidder1 : Bidder.t) (bidder2 : Bidder.t) ->
        let bid1 = List.hd bidder1.lastBids in
        let bid2 = List.hd bidder2.lastBids in
          if Bid.fee bid1 = Bid.fee bid2 then
            0
          else if Bid.fee bid1 < Bid.fee bid2 then
            -1
          else
            1
    (* Sets the given Bidder's lastBids to the given Bid list *)
    let set_bids : Bidder.t -> Bid.t list -> Bidder.t =
      fun bidder bids ->
        Bidder.{ id= bidder.id
               ; lastBids= bids }
    (* Uses the given bidder and given new total fee to calculate the
       individual fee for each of the bids) *)
    let set_fee : Bidder.t -> float -> Bidder.t =
      fun bidder totalFee ->
        let numBids = (float_of_int (List.length bidder.lastBids)) in
        let bidFee  = totalFee /. numBids in
        let newBids = List.map
                        (fun (bid : Bid.t) -> Bid.set_fee bid bidFee)
                        bidder.lastBids in
          Bidder.{ id= bidder.id
                 ; lastBids= newBids }
end

and Probability : sig
  type t =
  { probs : float array
  ; lower : int
  ; upper : int }
  val maybe_update : Probability.t -> Probability.t
end = struct
  type t =
  { probs : float array
  ; lower : int
  ; upper : int }
  (* $5 Query *)
  let maybe_update : Probability.t -> Probability.t =
    fun probs -> probs
end

and Distance : sig
  [@warning "-32"]
  type t =
  | Now
  | Short
  | Medium
  | Long
  val long : float
  val medium : float
  val maybe_update : int -> int -> Distance.t
end = struct
  type t =
  | Now
  | Short
  | Medium
  | Long
  let long : float = 180. (* minutes *)
  let medium : float = 5. (* minutes *)
  let maybe_update : int -> int -> Distance.t =
    fun deadline block ->
      let distance = float_of_int (deadline - block) in
      let confirmationTime = get_block_confirmation_time in
      let longTime = ((long *. 60. (* seconds per minute *))
                     /. confirmationTime) in
      let mediumTime = ((medium *. 60. (* seconds per minute *))
                     /. confirmationTime) in
      if distance < 0. then
        failwith "Distance.maybe_update: distance is negative"
      else if distance >= 0. && distance <= blocksToConfirm then
        Now
      else if distance > blocksToConfirm && distance < mediumTime then
        Short
      else if distance > mediumTime && distance < longTime then
        Medium
      else
        Long
end

(* $ Add pending & queued *)
and TxPool : sig
  [@warning "-32"]
  type t =
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  val maybe_update        : TxPool.t -> TxPool.t
  val find_min_fee        : TxPool.t -> float
  val reverse_bidder_bids : TxPool.t -> Bidder.t list
end = struct
  type t =
  (* $1 *)
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  (* $5 Query. What should we do if last one is accepted? *)
  let maybe_update : TxPool.t -> TxPool.t =
    fun txPool -> txPool
  (* Find minimum bidder in TxPool *)
  let find_min_fee : TxPool.t -> float =
    fun pool ->
      List.fold_left
        (fun (min : float) (bidder : Bidder.t) ->
          let bidderFee = Bidder.fee bidder in
          if bidderFee < min then
            bidderFee
          else
            min)
        infinity
        pool.bidders
  (* Reverse each Bidder's Bid list so that the (last) most recent
     nonce is first *)
  let reverse_bidder_bids : TxPool.t -> Bidder.t list =
    fun pool ->
      List.map
        (fun (bidder : Bidder.t) ->
          let revBids = List.rev bidder.lastBids in
            Bidder.set_bids bidder revBids)
        pool.bidders
end

and AuctionEnvironment : sig
  [@warning "-32"]
  type t =
  { block  : int
  ; txPool : TxPool.t
  ; prob   : Probability.t
  ; fees   : (Distance.t, float array) BatHashtbl.t }
  val maybe_update_block : int -> int
  val maybe_update_fees  : (Distance.t, float array) BatHashtbl.t
                           -> (Distance.t, float array) BatHashtbl.t
  val maybe_update       : AuctionEnvironment.t -> AuctionEnvironment.t
end = struct
  type t =
  { block  : int
  ; txPool : TxPool.t
  ; prob   : Probability.t
  (* $1 Should these be not mutable? *)
  ; fees   : (Distance.t, float array) BatHashtbl.t }
  (* $5 Query *)
  let maybe_update_block : int -> int =
    fun block -> block + 1
  (* $5 Query *)
  let maybe_update_fees : (Distance.t, float array) BatHashtbl.t
                          -> (Distance.t, float array) BatHashtbl.t =
  fun fees -> fees
  (* $5 Query *)
  let maybe_update : AuctionEnvironment.t -> AuctionEnvironment.t =
    fun auction ->
      AuctionEnvironment.{ block= maybe_update_block auction.block
                         ; txPool= TxPool.maybe_update auction.txPool
                         ; prob= Probability.maybe_update auction.prob
                         ; fees= maybe_update_fees auction.fees }
end

module BidderHeap = BatHeap.Make (Bidder)

(* $ Should we allow user to update maxValue and/or deadline? *)
module rec User : sig
  [@warning "-32"]
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
end = struct
  type t =
  { id        : int
  ; nonce     : int
  ; gasLimits : float list
  ; maxValue  : float
  ; start     : int
  ; deadline  : int
  ; distance  : Distance.t
  ; previous  : Bidder.t option list }
  (* $3$5 Currently set to not increase nonce.
     Check if some of your bids have been selected or confirmed. *)
  let maybe_update_nonce : int -> int =
    fun nonce ->
      (* (* $ This part should probably be outside of this function *)
      let nonceSelected = get_last_nonce_selected_on_block in
      if get_blocks_on_top_of_nonce = blocksToConfirm then
        nonceSelected + blocksToComfirm
      else *)
        nonce
end

(****************************** HELPERS ******************************)

(* Global variable for debug printing *)
let debug : bool = false

(* Global variable for debug printing *)
let debugAll : bool = false

(* A helper to print debug statements *)
let print_help : string -> float -> unit =
  fun s i ->
    if debug then
      (print_string s;
      print_float i;
      print_newline ())

(* Helper to print distance *)
let print_distance : Distance.t -> unit =
  fun dist ->
    if debug then
      match dist with
      | Distance.Long -> (print_string "Long"; print_newline ())
      | Distance.Medium -> (print_string "Medium"; print_newline ())
      | Distance.Short -> (print_string "Short"; print_newline ())
      | _ -> (print_string "Now (shouldn't happen)"; print_newline ())

(* Helper to print outbid list *)
let print_outbids : Bid.t list -> unit =
  fun list ->
    if debug && debugAll then
      for i = 0 to List.length list - 1 do
        let bid = List.nth list i in
        print_string "Bid ";
        print_int i;
        print_string ": gasPrice= ";
        print_float bid.gasPrice;
        print_string ", gas= ";
        print_float bid.gas;
        print_newline ();
      done

(* Helper to print post when using probabilities *)
let print_post_probs : int -> int list -> float -> float -> unit =
  fun block section ratio guess ->
    if debug then
      (print_string "Post with Probabilities: ";
      print_newline ();
      print_help "Block: " (float_of_int block);
      print_string "Range: ";
      print_int (List.nth section 0);
      print_string " - ";
      print_int (List.nth section 1);
      print_newline ();
      print_help "Ratio: " ratio;
      print_help "Guess: " guess;
      print_newline ())

(* Helper to print post when using predictive fees *)
let print_post_fees : int -> Distance.t -> float array -> float -> unit =
  fun block distance fees guess ->
    if debug then
      (print_string "Post with Predictive Fees: ";
      print_newline ();
      print_help "Block: " (float_of_int block);
      print_distance distance;
      print_string "fees.(0): ";
      print_float fees.(0);
      print_string " -> fees.(";
      print_int (Array.length fees - 1);
      print_string "): ";
      print_float fees.(Array.length fees - 1);
      print_newline ();
      print_help "Guess: " guess;
      print_newline ())

(* Helper to print post_now when enough size left *)
let print_post_now_enough : float -> float -> unit =
  fun sizeLeft minBidder ->
    if debug && debugAll then
      (print_string "Post Now when enough: ";
      print_newline ();
      print_help "Size Left: " sizeLeft;
      print_help "Minimum_bidder fee: " minBidder;
      print_newline ())

(* Helper to print post_now when not enough size left *)
let print_post_now : float -> Bid.t list -> float -> unit =
  fun sizeLeft outbids guess ->
    if debug && debugAll then
      (print_string "Post Now when not enough: ";
      print_newline ();
      print_help "Size Left: " sizeLeft;
      print_outbids outbids;
      print_help "Guess: " guess;
      print_newline ())

(* Helper to find the next block for re-bidding *)
let find_next : int -> int -> int -> int =
  fun last deadline block ->
    last - ((deadline - block) / 2)

(* Helper to find the subset of Bids that need to be outbid *)
let rec find_outbids : BidderHeap.t -> Bid.t list -> float
                       -> Bid.t list =
  fun bidderPQ outbids sizeLeft ->
    (* If you have outbid enough people to fit on this block, *)
    if sizeLeft <= 0. then
      (* Then return the outbid list. You don't need to reverse
         the list because order doesn't matter *)
      outbids
    (* Else, find the next min bid to outbid *)
    else
      (* "Pop" bidder with minimum bid out of all the largest nonces *)
      let bidder = BidderHeap.find_min bidderPQ in
      let popPQ  = BidderHeap.del_min bidderPQ in
      (* Get minimum bid from that Bidder *)
      let minBid = List.hd bidder.lastBids in
      let pushPQ =
        (* If there was only 1 one bid left for min bidder, *)
        if List.length bidder.lastBids = 1 then
          (* Then, use the rest of heap as is *)
          popPQ
        else
          (* Else, push the rest of minimum bidder's bid back onto
             heap *)
          let restBids = List.tl bidder.lastBids in
          let restBidder = Bidder.set_bids bidder restBids in
          BidderHeap.add restBidder popPQ in
      (* Calculate new sizeLeft to outbid and *)
      let newSize = sizeLeft -. minBid.gas in
      (* Add minBid to the outbid list and keep going *)
      find_outbids pushPQ (minBid :: outbids) newSize

(****************************** POST NOW *****************************)

(* Aggressively tries to get onto the block now or returns original bid
   if the fee of the minimum bidder is greater than our max value *)
let post_now : AuctionEnvironment.t -> User.t -> Bidder.t option =
  fun auction me ->
    let mySize = sum me.gasLimits in
    (* Calculates the total size that we need to reclaim to get
       onto the block *)
    let sizeLeft = mySize -. auction.txPool.sizeLeft in
    (* Depending on how much space is left on the block and how much
       space we require, we will need to handle how we find the needed
       fee differently *)
    let needFee =
      (* If there is already enough space for us to fit then
         find the minimum Bidder's fee and underbid that by 1 so we
         can ensure that we're close enough to be included behind
         the min bidder in the pending pool *)
      if sizeLeft <= 0. then
        let minBidder = TxPool.find_min_fee auction.txPool in
        print_post_now_enough sizeLeft minBidder;
        minBidder -. 1.
      (* Otherwise, find outbid list and sum its Bid fees and add 1: *)
      else
        (* Reverse each Bidder's Bid list so the last nonce is first.
           You can't outbid the previous nonce without also outbidding
           the nonce after, so just start with the nonce after *)
        let revBidList = TxPool.reverse_bidder_bids auction.txPool in
        (* Stick all the reversed Bidders into a Heap using the above
           Bidder.compare which will sort Bidders by their last nonce's
           Bid.fee *)
        let revBidsPQ = BidderHeap.of_list revBidList in
        (* Build a list of Bids we need to beat by repeatedly taking
           the min Bid (using Bid.fee) from each Bidder's largest nonce
           on the Heap until we have enough space to fit *)
        let outbidList = find_outbids revBidsPQ [] sizeLeft in
        let feeList = List.map Bid.fee outbidList in
        (* Finally, sum those fees and add 1 *)
        print_post_now sizeLeft outbidList ((sum feeList) +. 1.);
        (sum feeList) +. 1. in
    (* If the needed fee is greater than our maximum value for the
       transaction, *)
    if needFee > me.maxValue || needFee < minimumFee then
      (* Just return None. You're going to fail. *)
      None
    else
      (* Otherwise, set your bid(s) to the appropriate values to
         achieve the needed fee *)
      let bidder = Bidder.create me.id me.nonce me.gasLimits 0. in
      Some (Bidder.set_fee bidder needFee)

(******************************** POST *******************************)

(* Generates next bidder (either Some Bidder or None) using either
   historical probabilities of transaction fees or predictive
   transaction fees based on distance from deadline *)
let post : AuctionEnvironment.t -> User.t -> Bidder.t option =
  fun auction me ->
    if BatHashtbl.is_empty auction.fees
    && Array.length auction.prob.probs = 0 then
      failwith "post: neither fees nor probability initialized"
    (* else if not (BatHashtbl.is_empty auction.fees)
    && Array.length auction.prob.probs > 1 then
      failwith "post: both fees and probability initialized" *)
    else
      let guess =
        (* If using historical probabilities, *)
        if Array.length auction.prob.probs > 0 then
          let prob = auction.prob in
          (* Find the ratio of time that has passed since posting, *)
          let ratio = (float_of_int auction.block) /.
                      (float_of_int (me.deadline - me.start)) in
          (* Identify if we should use low, normal, or high probability
             fees *)
          let section =
            match me.distance with
            | Distance.Long   ->
              [ 0 ; prob.lower ]
            | Distance.Medium ->
              [ prob.lower ; prob.upper ]
            | Distance.Short  ->
              [ prob.upper ; Array.length prob.probs - 1 ]
            | _ -> failwith "post: shouldn't have Distance.Now" in
          (* And use the ratio and section to calculate a guess fee.
             Currently we are using the lowest of the low fees, the
             middle of the normal fees, and the highest of the high
             fees *)
          let temp = (float_of_int (List.nth section 1 - List.nth section 0)) *. ratio in
          print_post_probs auction.block section ratio
                           ((float_of_int (List.nth section 0)) +. temp);
          (float_of_int (List.nth section 0)) +. temp
        (* Else, if using predictive fees *)
        else
          let fees = BatHashtbl.find auction.fees me.distance in
          if auction.block >= Array.length fees then
            failwith "post: deadline is greater than predictive fees"
          else
            print_post_fees auction.block me.distance fees (fees.(auction.block));
            (* Simply get the guess at the current block *)
            fees.(auction.block) in
      (* $ This would be faster if we did a set of all bids we've done,
           but more memory. *)
      (* Check if any of previous fees have the exact same guess or
         have a higher guess, *)
      let fitList =
        List.filter
          (fun (bidder : Bidder.t option) ->
            match bidder with
            | Some b -> Bidder.fee b >= guess
            | None -> false)
          me.previous in
      (* In which case, if we have a previous guess that is similar or
         greater, don't bid again. Also don't bid if the guess fee is
         less than minimum allowed fee or greater than user's desired
         maximum value for posting this transaction *)
      if List.length fitList > 0
      || guess < minimumFee
      || guess > me.maxValue then
        None
      else
        (* $ Maybe Bidder.create and Bidder.set_fee can be streamlined? *)
        let bidder = Bidder.create me.id me.nonce me.gasLimits 0. in
        Some (Bidder.set_fee bidder guess)

(****************************** RUN POST *****************************)

(* Runs the post handling function *)
let rec run_post : AuctionEnvironment.t -> User.t -> int
                   -> Bidder.t option list =
  fun auction me next ->
    if me.deadline < auction.block then
      failwith "run_post: deadline has already passed"
    else if sum me.gasLimits > maximumSize then
      (* $ But this is the gas given, not gas used...? *)
      failwith "run_post: desired transaction exceeds maximum block size"
    (* $ Is block the current confirmed block? *)
    (* If you need to get onto the block now, then bid aggresively *)
    else if me.distance = Distance.Now then
      List.rev ((post_now auction me) :: me.previous)
    (* Else, bid logarithmically using either historical probabilities
       or predictive fees *)
    else
      (* Increment the auction 1 tick, in real world we will query *)
      let nextAuction = AuctionEnvironment.maybe_update auction in
      (* Update the distance for this next world *)
      let newDistance = Distance.maybe_update me.deadline nextAuction.block in
      (* Generate a new user, checking for potentially updated nonces
         and the new distance, with correct bids *)
      let newMe bids = User.{ id= me.id
                            ; nonce= maybe_update_nonce me.nonce
                            ; gasLimits= me.gasLimits
                            ; maxValue= me.maxValue
                            ; start= me.start
                            ; deadline= me.deadline
                            ; distance= newDistance
                            ; previous= bids } in
      (* If it is time to bid, update your bids and new next bidding
         time *)
      if auction.block + next = me.deadline then
        let newBids = ((post auction me) :: me.previous) in
        (* $ Makes sure this ends *)
        let newNext = find_next next me.deadline auction.block in
        run_post nextAuction (newMe newBids) newNext
      (* Else, just increment the block and continue on *)
      else
        run_post nextAuction (newMe me.previous) next

(* Runs the post handling function and initializes the next variable *)
let run_post : AuctionEnvironment.t -> User.t -> Bidder.t option list =
  fun auction me ->
    let next = me.deadline - auction.block in
    run_post auction me next

(******************************* TESTS *******************************)
(* 1. A Bidder with a maximum value greater than the minimum pending
      bid should be able to outbid that bid.
   2. A Bidder with a maximum value less than the minimum pending bid
      should not be able to outbit that bid. *)

(*********************** Test Helper Functions ***********************)

(* Helper to create bids for tests *)
let make_bid : int -> float -> float -> Bid.t =
  fun nonce gasPrice gas ->
    if nonce < 0 || gasPrice < 0. || gasPrice < 0. then
      failwith "make_bid: values must be >= 0"
    else
      Bid.{ blockHash= 0
          ; blockNumber= 0
          ; from= 0
          ; gas= gas
          ; gasPrice= gasPrice
          ; hash= 0
          ; input= 0
          ; nonce= nonce
          ; to_= 0
          ; transactionIndex= 0
          ; value= 0 }

(* Helper to create bidders for tests *)
let make_bidder : int -> Bid.t list -> Bidder.t =
  fun id lastBids ->
    if id < 0 then
      failwith "make_bidder: id must be >= 0"
    else
      Bidder.{ id= id
             ; lastBids= lastBids }

(* Helper to create txPool for tests *)
let make_txPool : float -> Bidder.t list -> TxPool.t =
  fun sizeLeft bidders ->
    if sizeLeft < 0. then
      failwith "make_txPool: size left must be >= 0"
    else
      TxPool.{ sizeLeft= sizeLeft
             ; bidders= bidders }

(* Helper to create probability arrays for tests *)
let make_probs : float array -> int -> int -> Probability.t =
  fun probs lower upper ->
    if lower < 0 || upper < 0 then
      failwith "make_probs: upper and lower bounds must be >= 0"
    else
      Probability.{ probs= probs
                  ; lower= lower
                  ; upper= upper }

(* Helper to create auction environments for tests *)
let make_auction_environment : TxPool.t
                               -> Probability.t
                               -> (Distance.t, float array) BatHashtbl.t
                               -> AuctionEnvironment.t =
  fun txPool prob fees ->
    AuctionEnvironment.{ block= 0
                       ; txPool= txPool
                       ; prob= prob
                       ; fees= fees }

(* Helper to create users for tests *)
let make_user : int -> int -> float list -> float -> int
                -> Distance.t -> Bidder.t option list
                -> User.t =
  fun id nonce gasLimits maxValue deadline distance previous ->
    if id < 0 || nonce < 0 || maxValue < 0. || deadline < 0 then
      failwith "make_user: id, nonce, maxValue, and deadline >= 0"
    else
      let fitList = List.filter
                      (fun (lim : float) -> lim < 0.)
                      gasLimits in
      if List.length fitList > 0 then
        failwith "make_user: all gas limits must be >= 0"
      else
        User.{ id= id
            ; nonce= nonce
            ; gasLimits= gasLimits
            ; maxValue= maxValue
            ; start= 0
            ; deadline= deadline
            ; distance= distance
            ; previous= previous }

(*************************** POST NOW TESTS **************************)
(************************ Post Now Structures ************************)

let bid1Mx1     = make_bid 0 1000000. 1.
let bid1000x100 = make_bid 0 1000.    100.
let bid500x50   = make_bid 0 500.     50.
let bid100x20   = make_bid 0 100.     20.
let bid100x10   = make_bid 0 100.     10.

let bidder1Mx1     = make_bidder 4 [bid1Mx1]
let bidder1000x100 = make_bidder 0 [bid1000x100]
let bidder500x50   = make_bidder 1 [bid500x50]
let bidder100x20   = make_bidder 2 [bid100x20]
let bidder100x10   = make_bidder 2 [bid100x10]

let pending0 = [ bidder1000x100
               ; bidder500x50
               ; bidder100x10 ]
let pending1 = [ bidder1000x100
               ; bidder500x50
               ; bidder100x10
               ; bidder100x10 ]
let pending2 = [ bidder1000x100
               ; bidder500x50
               ; bidder100x20 ]
let pending3 = [ bidder1000x100
               ; bidder500x50
               ; bidder100x10
               ; bidder100x10
               ; bidder1Mx1 ]

let txPool0 = make_txPool 0.  pending0
let txPool1 = make_txPool 0.  pending1
let txPool2 = make_txPool 0.  pending2
let txPool3 = make_txPool 0.  pending3
let txPool4 = make_txPool 5.  pending1
let txPool5 = make_txPool 10. pending1
let txPool6 = make_txPool 15. pending1
let txPool7 = make_txPool 20. pending1

let probs0 = make_probs [||] 0 0

let fees0 = BatHashtbl.create 1

let auction0 = make_auction_environment txPool0 probs0 fees0
let auction1 = make_auction_environment txPool1 probs0 fees0
let auction2 = make_auction_environment txPool2 probs0 fees0
let auction3 = make_auction_environment txPool3 probs0 fees0
let auction4 = make_auction_environment txPool4 probs0 fees0
let auction5 = make_auction_environment txPool5 probs0 fees0
let auction6 = make_auction_environment txPool6 probs0 fees0
let auction7 = make_auction_environment txPool7 probs0 fees0

let user10      = make_user 5 0 [10.] infinity 0 Distance.Now []
let user10x1000 = make_user 5 0 [10.] 1000.    0 Distance.Now []
let user20      = make_user 6 0 [20.] infinity 0 Distance.Now []

(********************** Trivial Post Now Tests ***********************)

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 0
   when my size is 10 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 10 size = 100.1 gas price
   100.1x10 *)
let%test "post_now test 1" =
  (run_post auction0 user10)
  =
  [Some (make_bidder 5 [(make_bid 0 100.1 10.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 0
   when my size is 10, but not enough value -> outbidList =
   [ ] which means no bid or None *)
let%test "post_now test 2" =
  (run_post auction0 user10x1000)
  =
  [None]

(*********************** Normal Post Now Tests ***********************)

(* [ 1000; 500; 100; 100 ] gas prices with
   [  100;  50;  10;  10 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x10; 100x10 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "post_now test 3" =
  (run_post auction1 user20)
  =
  [Some (make_bidder 6 [(make_bid 0 100.05 20.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  20 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x20 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "post_now test 4" =
  (run_post auction2 user20)
  =
  [Some (make_bidder 6 [(make_bid 0 100.05 20.)])]

(* [ 1000; 500; 100; 100; 1M ] gas prices with
   [  100;  50;  10;  10;  1 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x10; 100x10 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "post_now test 5" =
  (run_post auction3 user20)
  =
  [Some (make_bidder 6 [(make_bid 0 100.05 20.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 5
   when my size is 10 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 10 size = 100.1 gas price
   100.1x10 *)
let%test "post_now test 6" =
  (run_post auction4 user10)
  =
  [Some (make_bidder 5 [(make_bid 0 100.1 10.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 10
   when my size is 10 -> outbidList =
   [  ] which means an outbid of min bid 1000 - 1 = 999
   999 / 10 size = 99.9 gas price
   99.9x10 *)
let%test "post_now test 7" =
  (run_post auction5 user10)
  =
  [Some (make_bidder 5 [(make_bid 0 99.9 10.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 15
   when my size is 20 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 20 size = 50.05 gas price
   50.05x20 *)
let%test "post_now test 8" =
  (run_post auction6 user20)
  =
  [Some (make_bidder 6 [(make_bid 0 50.05 20.)])]

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 20
   when my size is 20 -> outbidList =
   [  ] which means an outbid of min bid 1000 - 1 = 999
   999 / 20 size = 49.95 gas price
   49.95x20 *)
let%test "post_now test 9" =
  (run_post auction7 user20)
  =
  [Some (make_bidder 6 [(make_bid 0 49.95 20.)])]

(***************** Complex Post Now Tests Structures *****************)

let bid2n500x1 = make_bid 2 500. 1.
let bid0n200x2 = make_bid 0 200. 2.
let bid0n200x1 = make_bid 0 200. 1.
let bid1n200x1 = make_bid 1 200. 1.
let bid0n50x1  = make_bid 0 50.  1.
let bid1n50x1  = make_bid 1 50.  1.
let bid0n10x1  = make_bid 0 10.  1.
let bid0n5x1   = make_bid 0 5.   1.
let bid1n5x1   = make_bid 1 5.   1.
let bid1n1x1   = make_bid 1 1.   1.

let bidder500x50'1x1      = make_bidder 0 [ bid500x50
                                          ; bid1n1x1 ]
let bidder50x1'50x1'500x1 = make_bidder 0 [ bid0n50x1
                                          ; bid1n50x1
                                          ; bid2n500x1 ]
let bidder200x2           = make_bidder 0 [ bid0n200x2 ]
let bidder200x1'200x1     = make_bidder 0 [ bid0n200x1
                                          ; bid1n200x1 ]
let bidder10x1            = make_bidder 0 [ bid0n10x1 ]
let bidder5x1'5x1         = make_bidder 0 [ bid0n5x1
                                          ; bid1n5x1 ]

let pending4 = [ bidder500x50'1x1
               ; bidder50x1'50x1'500x1
               ; bidder200x1'200x1
               ; bidder5x1'5x1 ]
let pending5 = [ bidder500x50
               ; bidder50x1'50x1'500x1
               ; bidder200x2
               ; bidder10x1 ]

let txPool8 = make_txPool 5. pending4
let txPool9 = make_txPool 5. pending5

let auction8 = make_auction_environment txPool8 probs0 fees0
let auction9 = make_auction_environment txPool9 probs0 fees0

let user10  = make_user 5 0 [10.]     infinity 0 Distance.Now []
let user5'5 = make_user 0 0 [5. ; 5.] infinity 0 Distance.Now []
let user2'8 = make_user 0 0 [2. ; 8.] infinity 0 Distance.Now []

(*********************** Complex Post Now Tests **********************)

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 10 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 = 412
   412 / 10 size = 41.2 gas price
   41.2x10 *)
let%test "post_now test 10" =
  (run_post auction8 user10)
  =
  [Some (make_bidder 5 [(make_bid 0 41.2 10.)])]

(* [ bidder500x50
   ; bidder50x1'50x1'500x1
   ; bidder200x2
   ; bidder10x1 ] and size left = 5
   when my size is 10 -> outbidList =
   [ 10x1; 200x2; 500x1; 50x1 ] which means an outbid of
   [   10;   400;   500;   50 ] + 1 = 961
   961 / 10 size = 91.6 gas price
   91.6x10 *)
let%test "post_now test 11" =
  (run_post auction9 user10)
  =
  [Some (make_bidder 5 [(make_bid 0 96.1 10.)])]

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 5&5 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 =
   412 / 2 bids total = 206 each bid
   206 / 5 size each = 41.2 gas price each bid
   [ 41.2x5; 41.2x5 ] *)
let%test "post_now test 12" =
  (run_post auction8 user5'5)
  =
  [Some (make_bidder 0 [(make_bid 0 41.2 5.) ; (make_bid 1 41.2 5.)])]

(* [ bidder500x50
   ; bidder50x1'50x1'500x1
   ; bidder200x2
   ; bidder10x1 ] and size left = 5
   when my size is 5&5 -> outbidList =
   [ 10x1; 200x2; 500x1; 50x1 ] which means an outbid of
   [   10;   400;   500;   50 ] + 1 = 961
   961 / 2 = 480.5 each bid
   480.5 / 5 = 96.1 gas price each bid
   [ 91.6x5; 91.6x5 ] *)
let%test "post_now test 13" =
  (run_post auction9 user5'5)
  =
  [Some (make_bidder 0 [(make_bid 0 96.1 5.) ; (make_bid 1 96.1 5.)])]

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 2&8 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 =
   412 / 2 each bid = 206 each bid
   206 / 2 size = 103 gas price and 206 / 8 size = 25.75 gas price
   [ 103x2; 25.75x8 ] *)
let%test "post_now test 14" =
  (run_post auction8 user2'8)
  =
  [Some (make_bidder 0 [(make_bid 0 103. 2.) ; (make_bid 1 25.75 8.)])]

(* [ bidder500x50
   ; bidder50x1'50x1'500x1
   ; bidder200x2
   ; bidder10x1 ] and size left = 5
   when my size is 2&8 -> outbidList =
   [ 10x1; 200x2; 500x1; 50x1 ] which means an outbid of
   [   10;   400;   500;   50 ] + 1 = 961
   961 / 2 = 480.5 each bid
   480.5 / 2 = 240.25 gas price and 480.5 / 8 = 60.0625 gas price
   [ 240.25x2; 60.0625x8 ] *)
let%test "post_now test 15" =
  (run_post auction9 user2'8)
  =
  [Some (make_bidder 0 [(make_bid 0 240.25 2.) ; (make_bid 1 60.0625 8.)])]

(***************************** POST TESTS ****************************)

let fill_probs : float array -> unit =
  fun probs ->
    let half = Array.length probs / 2 in
    for i = 0 to half do
      probs.(i) <- (float_of_int i)
    done;
    for i = half + 1 to Array.length probs - 1 do
      let n = Array.length probs - i in
      probs.(i) <- (float_of_int n)
    done

let fill_fees : float array -> float -> unit =
  fun fees delta ->
    if delta > 0. then
      for i = 0 to (Array.length fees) - 1 do
        let n = ((float_of_int i) *. delta) in
        fees.(i) <- n
      done
    else
      for i = Array.length fees - 1 downto 0 do
        let delta = delta *. (-1.) in
        let n = ((float_of_int i) +. delta) in
        fees.(i) <- n
      done

(************************** Post Structures **************************)

let length0 = 100

let bid500x50 = make_bid 0 500. 50.
let bid100x10 = make_bid 0 100. 10.
let bid50x5   = make_bid 0 50.  5.

let bidder500x50 = make_bidder 1 [bid500x50]
let bidder100x10 = make_bidder 2 [bid100x10]
let bidder50x5   = make_bidder 3 [bid50x5]

let pending6 = [ bidder500x50
               ; bidder100x10
               ; bidder50x5 ]

let txPool9 = make_txPool 4. pending6

let probArray = Array.make length0 0.
let _ = fill_probs probArray
let probs1 = make_probs probArray 16 84

let fees1 = BatHashtbl.create 1
let longFees = Array.make length0 0.
let _ = fill_fees longFees 1.
let mediumFees = Array.make length0 50.
let shortFees = Array.make length0 0.
let _ = fill_fees shortFees (-1.)
let _ = BatHashtbl.add fees1 Distance.Long longFees
let _ = BatHashtbl.add fees1 Distance.Medium mediumFees
let _ = BatHashtbl.add fees1 Distance.Short shortFees

let auction10 = make_auction_environment txPool9 probs1 fees0
let auction11 = make_auction_environment txPool9 probs0 fees1

let userV0  = make_user 6 0 [5.] 0.       length0 Distance.Long []
let userV50 = make_user 7 0 [5.] 50.      length0 Distance.Long []
let userS5  = make_user 8 0 [5.] infinity length0 Distance.Long []

(********************** Trivial Post Prob Tests **********************)

(* A maxValue of 0 should result in a list of Nones *)
let%test "post test 16" =
  (run_post auction10 userV0)
  =
  [ None
  ; None
  ; None
  ; None
  ; None
  ; None ]

(* A maxValue of 50 should result in a only a Bidder with a price 10 *)
let%test "post test 17" =
  (run_post auction10 userV50)
  =
  [ None
  ; Some (make_bidder 7 [(make_bid 0 10. 5.)])
  ; None
  ; None
  ; None
  ; None ]

(********************** Trivial Post Fees Tests **********************)

(* A maxValue of 0 should result in a list of Nones *)
let%test "post test 18" =
  (run_post auction11 userV0)
  =
  [ None
  ; None
  ; None
  ; None
  ; None
  ; None ]

(* A maxValue of 50 should result in a only a Bidder with a price 10 *)
let%test "post test 19" =
  (run_post auction11 userV50)
  =
  [ None
  ; Some (make_bidder 7 [(make_bid 0 10. 5.)])
  ; None
  ; None
  ; None
  ; None ]

(*********************** Normal Post Prob Tests **********************)

(* A bell curve probabilities with:
   [ 0 - 100 ] lower bound: 16, upper nound 84.
   and a txPool where there are:
   [ 500; 100; 50 ] gas prices with
   [  50;  10;  5 ] gas limits and size left = 4
   where my size is 5 *)
let%test "post test 20" =
  (run_post auction10 userS5)
  =
  [ None
  ; Some (make_bidder 8 [(make_bid 0 10.   5.)])
  ; Some (make_bidder 8 [(make_bid 0 13.4  5.)])
  ; Some (make_bidder 8 [(make_bid 0 19.41 5.)])
  ; Some (make_bidder 8 [(make_bid 0 19.59 5.)])
  ; Some (make_bidder 8 [(make_bid 0 50.2  5.)]) ]

(*********************** Normal Post Fees Tests **********************)

(* Predictive fees (each has an array of size 100):
   long term: [ 100 - 0 ]
   medium term: [ 50 all the way through ]
   short term: [ 0 - 100 ]
   and a txPool where there are:
   [ 500; 100; 50 ] gas prices with
   [  50;  10;  5 ] gas limits and size left = 4
   where my size is 5 *)
let%test "post test 21" =
  (run_post auction11 userS5)
  =
  [ None
  ; Some (make_bidder 8 [(make_bid 0 10.  5.)])
  ; None
  ; Some (make_bidder 8 [(make_bid 0 17.6 5.)])
  ; Some (make_bidder 8 [(make_bid 0 18.8 5.)])
  ; Some (make_bidder 8 [(make_bid 0 50.2 5.)]) ]

(******************* Complex Post Tests Structures *******************)
(************************* Complex Post Tests ************************)