open Batteries
open Legilogic_lib
open Action

module LL = Legilogic_lib
module EC = Ethereum_chain
module ECToken = Ethereum_chain.TokenAmount
type   entry   = Ethereum_json_rpc.TxPoolContent.entry

(* exception TxPool_bad_inputs *)

(******************************* Notes *******************************)
(* $1. Potentially use Hashtables instead of Lists to more closely
       match the txpool structure
   $2. How do you know when some of your bid was confirmed?
   $3. Query for actual updates/values. *)

(*********************** Stupid Hardcoded Values ***********************)

let userNonce = EC.Nonce.zero

let userMaxValue = ECToken.of_int 1000000000000000000

let userStartBlock = LL.Types.Revision.zero

let userDeadline = LL.Types.Revision.of_int 800

(******************** Variables & Basic Functions ********************)

(* ECToken.max_int
   ECToken.max_int turns out to be -1 so I will use this *)
let ecTokenMaxValue = ECToken.of_int 1000000000000000000

(* Converts ECTokens to floats *)
let ecTokenToFloat : ECToken.t -> float =
  fun token ->
    Int64.to_float (ECToken.to_int64 token)

(* Converts ECTokens to floats *)
let ecTokenOfFloat : float -> ECToken.t  =
  fun float ->
    ECToken.of_int64 (Int64.of_float float)

(* Converts Revisions to floats *)
let llTRevisionToFloat : LL.Types.Revision.t -> float =
  fun revision ->
    (Int64.to_float (LL.Types.Revision.to_int64 revision))

(* Converts Revisions to Tokens *)
let tokenToRevision : ECToken.t -> LL.Types.Revision.t =
  fun token ->
  (LL.Types.Revision.of_int64 (ECToken.to_int64 token))

(* $ Use Mathieu's *)
let blocksToConfirm : ECToken.t = ECToken.of_int 6

(* Minimum transaction fee of 1 for ethereum *)
let minimumFee : ECToken.t = ECToken.of_int 1

(* Minimum transaction size of 21000 for ethereum *)
let minimumSize : ECToken.t = ECToken.of_int
1
(* 21000 *)

(* $3 Query. I think? *)
let maximumSize : ECToken.t = ECToken.of_int 8000000

(* $3 Query *)
let get_block_confirmation_time : ECToken.t = ECToken.of_int 15

(* Helper function to sum a list of floats *)
let sum : ECToken.t list -> ECToken.t =
  fun fees -> List.fold_left (ECToken.add) ECToken.zero fees

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
module rec Bid : sig
  [@warning "-32"]
  type t =
  { blockHash        : LL.Types.Digest.t
  ; blockNumber      : LL.Types.Revision.t option
  ; from             : LL.Signing.Address.t
  ; gas              : ECToken.t
  ; gasPrice         : ECToken.t
  ; hash             : LL.Types.Digest.t
  ; input            : LL.Yojsoning.Bytes.t
  ; nonce            : EC.Nonce.t
  ; to_              : LL.Signing.Address.t
  ; transactionIndex : LL.Types.Revision.t option
  ; value            : ECToken.t }
  [@@deriving show]
  (* val size    : Bid.t -> ECToken.t *)
  val fee     : Bid.t -> ECToken.t
  (* val create  : EC.Nonce.t -> ECToken.t -> ECToken.t -> Bid.t *)
  (* val set_fee : Bid.t -> ECToken.t -> Bid.t *)
  val set_fee : ECToken.t -> ECToken.t -> ECToken.t
end = struct
  [@warning "-32"]
  type t =
  { blockHash        : LL.Types.Digest.t
  ; blockNumber      : LL.Types.Revision.t option
  ; from             : LL.Signing.Address.t
  ; gas              : ECToken.t
  ; gasPrice         : ECToken.t
  ; hash             : LL.Types.Digest.t
  ; input            : LL.Yojsoning.Bytes.t
  ; nonce            : EC.Nonce.t
  ; to_              : LL.Signing.Address.t
  ; transactionIndex : LL.Types.Revision.t option
  ; value            : ECToken.t }
  [@@deriving show]
  (* Returns the bid gas limit *)
  let size : Bid.t -> ECToken.t =
    fun bid -> bid.gas
  (* Calculates the bid price using gas price and gas limit *)
  let fee : Bid.t -> ECToken.t =
      fun bid -> ECToken.mul bid.gasPrice bid.gas
  (* Creates a new Bid with all fields set to either
     given values or min_int/max_int or neg_infinity/infinity *)
  let create : EC.Nonce.t -> ECToken.t -> ECToken.t -> Bid.t =
    fun nonce limit price ->
      Bid.{ blockHash=  LL.Types.Digest.zero
          ; blockNumber= None
          ; from= LL.Signing.Address.zero
          ; gas= limit
          ; gasPrice= price
          ; hash=  LL.Types.Digest.zero
          ; input= LL.Yojsoning.Bytes.empty
          ; nonce= nonce
          ; to_= LL.Signing.Address.zero
          ; transactionIndex= None
          ; value= ECToken.zero }
  (* Returns a new Bid with the gas price calculated from the given Bid
     fee and everything else kept the same *)
  (* let set_fee : Bid.t -> ECToken.t -> Bid.t =
    fun bid newFee ->
      if ECToken.(equal bid.gas zero) then
        failwith "set_fee: gas limit for this bid is less than minimum"
      else
        let newPrice = ECToken.div newFee bid.gas in
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
              ; value= bid.value } *)
  let set_fee : ECToken.t -> ECToken.t -> ECToken.t =
    fun size newFee ->
      if ECToken.(equal size zero) then
        failwith "set_fee: gas limit for this bid is 0"
      else
        ECToken.div newFee size
end

and Bidder : sig
  [@warning "-32"]
  type t =
  (* $1 *)
  { id   : LL.Signing.Address.t
  ; bids : Bid.t list }
  [@@deriving show]
  (* val create   : LL.Signing.Address.t -> EC.Nonce.t
                 -> ECToken.t list -> ECToken.t
                 -> Bidder.t *)
  (* val size     : Bidder.t -> ECToken.t *)
  val fee      : Bidder.t -> ECToken.t
  val compare  : Bidder.t -> Bidder.t -> int
  val set_bids : Bidder.t -> Bid.t list -> Bidder.t
  (* val set_fee  : Bidder.t -> ECToken.t -> Bidder.t *)
  val set_fee : ECToken.t list -> ECToken.t -> ECToken.t list
end = struct
 type t =
  { id   : LL.Signing.Address.t
  ; bids : Bid.t list }
  [@@deriving show]
  (* Creates a new Bidder with a new Bid with all fields set to
     given values or min_int/max_int or neg_infinity/infinity *)
  (* let create : LL.Signing.Address.t -> EC.Nonce.t
               -> ECToken.t list -> ECToken.t
               -> Bidder.t =
    fun id nonce limits price ->
      if not (LL.Signing.Address.is_non_negative id)
      || not (EC.Nonce.is_non_negative nonce)
      || not (ECToken.is_non_negative price) then
        failwith "Bidder.create: id and nonce must be >= 0"
      else if
              (* price <> ecTokenMinInt
           &&  *)
              price <> ecTokenMaxValue
           && price <> ECToken.zero then
        failwith "Bidder.create: price must be neg_infinity,
                  infinity, 0"
      else
        let bids =
          List.fold_left
            (fun (b : Bid.t list) (limit : ECToken.t) ->
              if not (ECToken.is_non_negative limit) then
                failwith "Bidder.create: all limits must be >= 0"
              else
                let newNonce = EC.Nonce.(add nonce
                                             (of_int (List.length b))) in
                (Bid.create newNonce limit price) :: b)
            []
            limits in
          Bidder.{ id= id
                 ; bids= List.rev bids } *)
  (* Returns the total size of the bidder's bids *)
  (* let size : Bidder.t -> ECToken.t =
    fun bidder ->
      (sum (List.map Bid.size bidder.bids)) *)
  (* Calculates the total fees of the Bidder's Bids *)
  let fee : Bidder.t -> ECToken.t =
    fun bidder ->
      sum (List.map Bid.fee bidder.bids)
  (* Compares two Bidders based on the Bid.fee of the last nonce
     of each Bidder *)
  let compare : Bidder.t -> Bidder.t -> int =
    fun (bidder1 : Bidder.t) (bidder2 : Bidder.t) ->
      let bid1 = List.hd bidder1.bids in
      let bid2 = List.hd bidder2.bids in
        if Bid.fee bid1 = Bid.fee bid2 then
          0
        else if Bid.fee bid1 < Bid.fee bid2 then
          -1
        else
          1
  (* Sets the given Bidder's bids to the given Bid list *)
  let set_bids : Bidder.t -> Bid.t list -> Bidder.t =
    fun bidder bids ->
      Bidder.{ id= bidder.id
             ; bids= bids }
  (* Uses the given bidder and given new total fee to calculate the
     individual fee for each of the bids) *)
  (* let set_fee : Bidder.t -> ECToken.t -> Bidder.t =
    fun bidder totalFee ->
      let numBids = (ECToken.of_int (List.length bidder.bids)) in
      let bidFee  = ECToken.div totalFee numBids in
      let newBids = List.map
                      (fun (bid : Bid.t) -> Bid.set_fee bid bidFee)
                      bidder.bids in
        Bidder.{ id= bidder.id
               ; bids= newBids } *)
  let set_fee : ECToken.t list -> ECToken.t -> ECToken.t list =
    fun sizes totalFee ->
      let numBids = (ECToken.of_int (List.length sizes)) in
      let bidFee  = ECToken.div totalFee numBids in
      List.map
        (fun (size : ECToken.t) -> (Bid.set_fee size bidFee))
        sizes
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
  (* $3 Query *)
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
  val maybe_update : LL.Types.Revision.t -> LL.Types.Revision.t -> Distance.t
end = struct
  type t =
  | Now
  | Short
  | Medium
  | Long
  let long   : LL.Types.Revision.t = LL.Types.Revision.of_int 180 (* minutes *)
  let medium : LL.Types.Revision.t = LL.Types.Revision.of_int 5   (* minutes *)
  let maybe_update : LL.Types.Revision.t -> LL.Types.Revision.t -> Distance.t =
    fun block deadline ->
      let distance = LL.Types.Revision.sub deadline block in
      let confirmationTime = tokenToRevision get_block_confirmation_time in
      let longTime = LL.Types.Revision.(div (mul long (of_int 60)) (* seconds per minute *)
                                        confirmationTime) in
      let mediumTime = LL.Types.Revision.(div (mul medium (of_int 60)) (* seconds per minute *)
                                          confirmationTime) in
      if llTRevisionToFloat distance < 0. then
        failwith "Distance.maybe_update: distance is negative"
      else if llTRevisionToFloat distance >= 0.
      && llTRevisionToFloat distance <= ecTokenToFloat blocksToConfirm then
        Now
      else if llTRevisionToFloat distance > ecTokenToFloat blocksToConfirm
      && distance < mediumTime then
        Short
      else if distance > mediumTime && distance < longTime then
        Medium
      else
        Long
end

and TxPoolPending : sig
  [@warning "-32"]
  type t =
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  (* val find_sizes          : TxPoolPending.t -> ECToken.t *)
  val find_min_fee        : TxPoolPending.t -> ECToken.t
  val reverse_bidder_bids : TxPoolPending.t -> Bidder.t list
end = struct
  type t =
  (* $1 *)
  { sizeLeft : float
  ; bidders  : Bidder.t list }
  (* Finds the size of a pending pool *)
  (* let find_sizes : TxPoolPending.t -> ECToken.t =
    fun txPoolPending ->
      let pendingLimit = maximumSize in
      let pendingSize  = sum (List.map Bidder.size txPoolPending.bidders) in
      ECToken.sub pendingLimit pendingSize *)
  (* Find minimum bidder in TxPool *)
  let find_min_fee : TxPoolPending.t -> ECToken.t =
    fun pool ->
      List.fold_left
        (fun (min : ECToken.t) (bidder : Bidder.t) ->
          let bidderFee = Bidder.fee bidder in
          if bidderFee < min then
            bidderFee
          else
            min)
        (ecTokenOfFloat 1000000000000000000.)
        pool.bidders
  (* Reverse each Bidder's Bid list so that the (last) most recent
     nonce is first *)
  let reverse_bidder_bids : TxPoolPending.t -> Bidder.t list =
    fun pool ->
      List.map
        (fun (bidder : Bidder.t) ->
          let revBids = List.rev bidder.bids in
            Bidder.set_bids bidder revBids)
        pool.bidders
end

and TxPool : sig
  type t =
  { pending : TxPoolPending.t
  ; queued  : (LL.Signing.Address.t * (EC.Nonce.t * Bid.t list) list) list }
  val maybe_update : TxPool.t -> TxPool.t
end = struct
  type t =
  { pending : TxPoolPending.t
  ; queued  : (LL.Signing.Address.t * (EC.Nonce.t * Bid.t list) list) list }
  (* $3 Query for size also. What should we do if last one is accepted? *)
  let maybe_update : TxPool.t -> TxPool.t =
    fun txPool -> txPool
end

and AuctionEnvironment : sig
  [@warning "-32"]
  type t =
  { block  : LL.Types.Revision.t
  ; txPool : TxPool.t
  ; prob   : Probability.t }
  val maybe_update_block : LL.Types.Revision.t -> LL.Types.Revision.t
  val maybe_update       : AuctionEnvironment.t -> AuctionEnvironment.t
end = struct
  type t =
  { block  : LL.Types.Revision.t
  ; txPool : TxPool.t
  ; prob   : Probability.t }
  (* $3 Query *)
  let maybe_update_block : LL.Types.Revision.t -> LL.Types.Revision.t =
    fun block -> LL.Types.Revision.add block (LL.Types.Revision.of_int 1)
  (* $3 Query *)
  let maybe_update : AuctionEnvironment.t -> AuctionEnvironment.t =
    fun auction ->
      AuctionEnvironment.{ block= maybe_update_block auction.block
                         ; txPool= TxPool.maybe_update auction.txPool
                         ; prob= Probability.maybe_update auction.prob }
end

module BidderHeap = BatHeap.Make (Bidder)

(* $ Should we allow user to update maxValue and/or deadline? *)
module rec User : sig
  [@warning "-32"]
  type t =
  { id        : LL.Signing.Address.t
  ; nonce     : EC.Nonce.t
  ; gasLimits : ECToken.t list
  ; maxValue  : ECToken.t
  ; start     : LL.Types.Revision.t
  ; deadline  : LL.Types.Revision.t
  ; distance  : Distance.t
  ; previous  : ECToken.t list option list }
  val maybe_update_nonce : EC.Nonce.t -> EC.Nonce.t
end = struct
  type t =
  { id        : LL.Signing.Address.t
  ; nonce     : EC.Nonce.t
  ; gasLimits : ECToken.t list
  ; maxValue  : ECToken.t
  ; start     : LL.Types.Revision.t
  ; deadline  : LL.Types.Revision.t
  ; distance  : Distance.t
  ; previous  : ECToken.t list option list }
  (* $2$3 Currently set to not increase nonce.
     Check if some of your bids have been selected or confirmed. *)
  let maybe_update_nonce : EC.Nonce.t -> EC.Nonce.t =
    fun nonce ->
      (* (* $ This part should probably be outside of this function *)
      let nonceSelected = get_last_nonce_selected_on_block in
      if get_blocks_on_top_of_nonce = blocksToConfirm then
        nonceSelected + blocksToComfirm
      else *)
        nonce
end

module rec ResultBids : sig
  [@warning "-32"]
  type t =
  { bids    : ECToken.t list option list
  ; index   : int
  ; timings : LL.Types.Revision.t list }
  val maybe_update : ECToken.t list option list -> int
                     -> LL.Types.Revision.t list -> ResultBids.t
end = struct
  type t =
  { bids    : ECToken.t list option list
  ; index   : int
  ; timings : LL.Types.Revision.t list }
  let maybe_update : ECToken.t list option list -> int
                     -> LL.Types.Revision.t list -> ResultBids.t =
    fun bids index timings ->
      let bid = List.at bids 0 in
      let index =
        if index < 0 && bid != None then
          List.length bids - 1
        else
          index
          in
      ResultBids.{ bids= bids
                 ; index= index
                 ; timings= timings }
end

(************************ TRANSLATE FUNCTIONS ************************)

(* Helper to translate Bids *)
let translate_bid : entry -> Bid.t =
  fun entry ->
    if entry.gas_price < ECToken.zero
    || entry.gas < ECToken.zero then
      failwith "make_bid: gas_price and gas can't be negative"
    else
      Bid.{ blockHash= entry.block_hash
          ; blockNumber= entry.block_number
          ; from= entry.from
          ; gas= entry.gas
          ; gasPrice= entry.gas_price
          ; hash= entry.hash
          ; input= entry.input
          ; nonce= entry.nonce
          ; to_= entry.to_
          ; transactionIndex= entry.transaction_index
          ; value= entry.value }

(* Helper to translate Bids *)
let translate_bidList : EC.Nonce.t -> entry list -> Bid.t =
  fun nonce entryList ->
    if nonce < Types.Revision.zero then
      failwith "make_bid: nonce cannot be negative"
    else if List.length entryList < 1 then
      failwith "nonce with no bid"
    else if List.length entryList > 1 then
      failwith "more than one bid per nonce"
    else
      let bids =
        List.map
          (fun entry -> translate_bid entry)
          entryList in
      List.at bids 0

(* Helper to translate Bidder *)
let translate_bidder : LL.Signing.Address.t -> (EC.Nonce.t * entry list) list
                       -> Bidder.t =
  fun address nxeListList ->
    if address < Signing.Address.zero then
      failwith "translate_bidder: address must be >= 0"
    else
      let bidMap = BatHashtbl.create 10 in
      (* $ Will this reverse and reverse again? *)
      let _ =
        List.iter
          (fun ((nonce : EC.Nonce.t), (entryList : entry list)) ->
            match BatHashtbl.find_option bidMap nonce with
            | Some entry -> BatHashtbl.replace bidMap nonce (entry @ entryList)
            | None       -> BatHashtbl.add     bidMap nonce entryList)
          nxeListList in
      let bids =
        BatHashtbl.fold
          (fun (nonce : EC.Nonce.t) (entryList : entry list) (bidList : Bid.t list) ->
            (translate_bidList nonce entryList) :: bidList)
          bidMap
          [] in
      Bidder.{ id= address
             ; bids= bids }

(* Helper to translate Bidder list *)
let translate_bidderList : (LL.Signing.Address.t * (EC.Nonce.t * entry list) list) list
                            -> Bidder.t list =
  fun axNxEListListList ->
  let bidderMap = BatHashtbl.create 10 in
  let _ =
    List.iter
      (fun ((add : LL.Signing.Address.t), (nxeListList : (EC.Nonce.t * entry list) list)) ->
        match BatHashtbl.find_option bidderMap add with
        | Some entry -> BatHashtbl.replace bidderMap add (entry @ nxeListList)
        | None       -> BatHashtbl.add     bidderMap add nxeListList)
      axNxEListListList in
  let _ =
    BatHashtbl.iter
      (fun (add : LL.Signing.Address.t) (nxeListList : (EC.Nonce.t * entry list) list) ->
        BatHashtbl.replace bidderMap add (List.rev nxeListList))
      bidderMap in
  BatHashtbl.fold
    (fun (add : LL.Signing.Address.t)
         (nxeListList : (EC.Nonce.t * entry list) list) (bidders : Bidder.t list) ->
      (translate_bidder add nxeListList) :: bidders)
    bidderMap
    []

(* Helper to translate txPool.pending *)
let translate_txPoolPending : (LL.Signing.Address.t * (EC.Nonce.t * entry list) list) list
                              -> TxPoolPending.t =
  fun axNxEListListList ->
    let pendingSize = List.fold_left
                        (fun (_ : float)
                             ((_add : LL.Signing.Address.t),
                               (nxeListList : (EC.Nonce.t * entry list) list)) ->
                          List.fold_left
                            (fun (_ : float)
                                 ((_n : EC.Nonce.t), (entryList : entry list)) ->
                              List.fold_left
                                (fun (total : float) (entry : entry) ->
                                  (ecTokenToFloat entry.gas) +. total)
                                0.
                                entryList)
                            0.
                            nxeListList)
                        0.
                        axNxEListListList in
    let bidderList = translate_bidderList axNxEListListList in
    TxPoolPending.{ sizeLeft= pendingSize
                  ; bidders= bidderList }

(* Helper to translatte txPool.queued *)
let translate_txPoolQueued : (LL.Signing.Address.t * (EC.Nonce.t * entry list) list) list ->
                             (LL.Signing.Address.t * (EC.Nonce.t * Bid.t list) list) list =
  fun ejrTxPoolQueued ->
    List.map
      (fun (address, nxeListList) ->
        let nxbListList =
          List.map
            (fun (nonce, entryList) ->
              let bidList =
                List.map
                  (fun entry ->
                    translate_bid entry)
                  entryList in
              nonce, bidList)
            nxeListList in
        address, nxbListList)
      ejrTxPoolQueued

(* Helper to translatte txPool *)
let translate_to_posting_txPool : Ethereum_json_rpc.TxPoolContent.t -> TxPool.t =
  fun ejrTxPool ->
    let pending = translate_txPoolPending ejrTxPool.pending in
    let queued  = translate_txPoolQueued  ejrTxPool.queued in
    TxPool.{ pending= pending
           ; queued= queued }

(*************************** DEBUG HELPERS ***************************)

let debug : bool = false

let print_bid : ECToken.t list -> unit =
  fun result ->
    if debug then
      (Logging.log "  Bid: [ ";
      for i = 0 to List.length result - 1 do
        Logging.log "    Some %f" (ecTokenToFloat (List.at result i));
      done;
      Logging.log "  ]")

let print_bids : ECToken.t list option list -> unit =
  fun result ->
    if debug then
      (Logging.log "Bids: [ ";
      for i = 0 to List.length result - 1 do
        match List.at result i with
        | Some list -> print_bid list;
        | None      -> Logging.log "  None";
      done;
      Logging.log "]")

let print_timings : LL.Types.Revision.t list -> unit =
  fun timings ->
    if debug then
      (Logging.log "Timings: [ ";
      for i = 0 to List.length timings - 1 do
        Logging.log "  %d, " (LL.Types.Revision.to_int (List.at timings i))
      done;
      Logging.log "]")

let print_result : ResultBids.t -> unit =
  fun result ->
    if debug then
      (print_bids result.bids;
      Logging.log "Index: %d" result.index;
      print_timings result.timings;
      Logging.log "")

(****************************** HELPERS ******************************)

(* Helper to populate probabilities *)
let fill_probs : float array -> float -> float -> unit =
  fun probs low hi ->
    let half = Array.length probs / 2 in
    let delta = hi /. (float_of_int half) in
    for i = 0 to half do
      let cur = low +. (delta *. (float_of_int i)) in
      probs.(i) <- cur
    done;
    for i = half + 1 to Array.length probs - 1 do
      let n = Array.length probs - i in
      let cur = hi -. (delta *. (float_of_int n)) in
      probs.(n) <- cur
    done

let fakeArray = Array.make 1000 0.
let _ = fill_probs fakeArray 21000. 12800000000000000.
let fakeProbs = Probability.{ probs= fakeArray
                            ; lower= 160
                            ; upper= 840 }

(* Helper to find the next block for re-bidding *)
let find_next : LL.Types.Revision.t -> LL.Types.Revision.t ->
                LL.Types.Revision.t -> LL.Types.Revision.t =
  fun last deadline block ->
    LL.Types.Revision.(sub last (div (sub deadline block) (of_int 2)))

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
      let minBid = List.hd bidder.bids in
      let pushPQ =
        (* If there was only 1 one bid left for min bidder, *)
        if List.length bidder.bids = 1 then
          (* Then, use the rest of heap as is *)
          popPQ
        else
          (* Else, push the rest of minimum bidder's bid back onto
             heap *)
          let restBids = List.tl bidder.bids in
          let restBidder = Bidder.set_bids bidder restBids in
          BidderHeap.add restBidder popPQ in
      (* Calculate new sizeLeft to outbid and *)
      let newSize = sizeLeft -. (ecTokenToFloat minBid.gas) in
      (* Add minBid to the outbid list and keep going *)
      find_outbids pushPQ (minBid :: outbids) newSize

(****************************** POST NOW *****************************)

(* Aggressively tries to get onto the block now or returns original bid
   if the fee of the minimum bidder is greater than our max value *)
let post_now : AuctionEnvironment.t -> User.t -> ECToken.t list option =
  fun auction me ->
    let mySize = sum me.gasLimits in
    (* Calculates the total size that we need to reclaim to get
       onto the block *)
    let pending = auction.txPool.pending in
    let sizeLeft = (ecTokenToFloat mySize) -. pending.sizeLeft in
    (* Depending on how much space is left on the block and how much
       space we require, we will need to handle how we find the needed
       fee differently *)
    let needFee =
      (* If there is already enough space for us to fit then
         find the minimum Bidder's fee and underbid that by 1 so we
         can ensure that we're close enough to be included behind
         the min bidder in the pending pool *)
      if sizeLeft <= 0. then
        let minBidder = TxPoolPending.find_min_fee pending in
        (* print_post_now_enough sizeLeft minBidder; *)
        ECToken.(sub minBidder one)
      (* Otherwise, find outbid list and sum its Bid fees and add 1: *)
      else
        (* Reverse each Bidder's Bid list so the last nonce is first.
           You can't outbid the previous nonce without also outbidding
           the nonce after, so just start with the nonce after *)
        let revBidList = TxPoolPending.reverse_bidder_bids pending in
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
        (* print_post_now sizeLeft outbidList ((sum feeList) +. 1.); *)
        ECToken.(add (sum feeList) one) in
    (* If the needed fee is greater than our maximum value for the
       transaction, *)
    if needFee > me.maxValue then
      (* Just return None. You're going to fail. *)
      None
    else
      Some (Bidder.set_fee me.gasLimits needFee)

(******************************** POST *******************************)

(* Generates next bidder (either Some Bidder or None) using either
   historical probabilities of transaction fees *)
let post : AuctionEnvironment.t -> User.t -> ECToken.t list option =
  fun auction me ->
    if Array.length auction.prob.probs = 0 then
      failwith "post: probabilities are not initialized"
    else
      let guess =
        let prob = auction.prob in
        (* Find the ratio of time that has passed since posting, *)
        let ratio = (llTRevisionToFloat auction.block) /.
                    ((llTRevisionToFloat me.deadline) -. (llTRevisionToFloat me.start)) in
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
        (* print_post_probs auction.block section ratio
                         (ECToken.mul (ECToken.of_int (List.nth section 0)) temp); *)
        ecTokenOfFloat ((float_of_int (List.nth section 0)) +. temp) in
      (* $ This would be faster if we did a set of all bids we've done,
           but more memory. *)
      (* Check if any of previous fees have the exact same guess or
         have a higher guess, *)
      let fitList =
        List.filter
          (fun (bidder : ECToken.t list option) ->
            match bidder with
            | Some list ->
              let total = sum list in
              total >= guess
            | None      -> false)
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
        Some (Bidder.set_fee me.gasLimits guess)

(****************************** RUN POST *****************************)

(* Runs the post handling function *)
let rec run_post : AuctionEnvironment.t -> User.t -> ResultBids.t
                   -> ResultBids.t =
  fun auction me result ->
    if me.deadline < auction.block then
      failwith "run_post: deadline has already passed"
    else if sum me.gasLimits > maximumSize || sum me.gasLimits < minimumSize then
      failwith "run_post: desired transaction size either too large or small"
    (* $ Is block the current confirmed block? *)
    (* If you need to get onto the block now, then bid aggresively *)
    else if me.distance = Distance.Now then
      let bids = List.rev ((post_now auction me) :: me.previous) in
      print_result (ResultBids.maybe_update bids result.index result.timings);
      ResultBids.maybe_update bids result.index result.timings
    (* Else, bid logarithmically using either historical probabilities *)
    else
      (* Increment the auction 1 tick, in real world we will query *)
      let nextAuction = AuctionEnvironment.maybe_update auction in
      (* Update the distance for this next world *)
      let newDistance = Distance.maybe_update nextAuction.block me.deadline in
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
      let next = List.at result.timings 0 in
      if LL.Types.Revision.to_int auction.block + LL.Types.Revision.to_int next
      =  LL.Types.Revision.to_int me.deadline then
        let newBids = ((post auction me) :: me.previous) in
        let newNext = find_next next me.deadline auction.block in
        let newResult = ResultBids.maybe_update newBids
                                                result.index
                                                (newNext :: result.timings) in
        print_result newResult;
        run_post nextAuction (newMe newBids) newResult
      (* Else, just increment the block and continue on *)
      else
        run_post nextAuction (newMe me.previous) result

(* Runs the post handling function and initializes the next variable *)
let run_post : AuctionEnvironment.t -> User.t -> ResultBids.t =
  fun auction me ->
    let next = LL.Types.Revision.sub me.deadline auction.block in
    let result = ResultBids.{ bids= []
                            ; index= -1
                            ; timings= [next] } in
    run_post auction me result

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
      Bid.{ blockHash= LL.Types.Digest.zero
          ; blockNumber= None
          ; from= LL.Signing.Address.zero
          ; gas= ecTokenOfFloat gas
          ; gasPrice= ecTokenOfFloat gasPrice
          ; hash= LL.Types.Digest.zero
          ; input= LL.Yojsoning.Bytes.empty
          ; nonce= EC.Nonce.of_int nonce
          ; to_= LL.Signing.Address.zero
          ; transactionIndex= None
          ; value= ECToken.zero }

(* Helper to create bidders for tests *)
let make_bidder : int -> Bid.t list -> Bidder.t =
  fun id bids ->
    if id < 0 then
      failwith "make_bidder: id must be >= 0"
    else
      Bidder.{ id= LL.Signing.Address.of_int id
             ; bids= bids }

(* Helper to create txPoolPendiing for tests *)
let make_txPoolPending : float -> Bidder.t list -> TxPoolPending.t =
  fun sizeLeft bidders ->
    if sizeLeft < 0. then
      failwith "make_txPool: size left must be >= 0"
    else
      TxPoolPending.{ sizeLeft= sizeLeft
                    ; bidders= bidders }

(* Helper to create txPool for tests *)
let make_txPool : TxPoolPending.t
                  -> (LL.Signing.Address.t * (EC.Nonce.t * Bid.t list) list) list
                  -> TxPool.t =
  fun pending queued ->
    TxPool.{ pending= pending
           ; queued= queued }

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
let make_auction_environment : TxPool.t -> Probability.t
                               -> AuctionEnvironment.t =
  fun txPool prob ->
    AuctionEnvironment.{ block= LL.Types.Revision.zero
                       ; txPool= txPool
                       ; prob= prob }

(* Helper to create users for tests *)
let make_user : int -> int -> float list -> float -> int
                -> Distance.t -> ECToken.t list option list
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
        let gasLimits = List.map ecTokenOfFloat gasLimits in
        User.{ id= LL.Signing.Address.of_int id
            ; nonce= EC.Nonce.of_int nonce
            ; gasLimits= gasLimits
            ; maxValue= ecTokenOfFloat maxValue
            ; start= LL.Types.Revision.zero
            ; deadline= LL.Types.Revision.of_int deadline
            ; distance= distance
            ; previous= previous }

let make_result : ECToken.t list option list -> int -> int list
                  -> ResultBids.t =
  fun bids index timings ->
    let timings =
      List.map
        (fun timing ->
          LL.Types.Revision.of_int timing)
      timings in
    ResultBids.{ bids= bids
               ; index= index
               ; timings= timings }

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

let txPoolQueued   = [LL.Signing.Address.zero, [EC.Nonce.zero, [bid1Mx1]]]

let txPoolPending0 = make_txPoolPending 0.  pending0
let txPoolPending1 = make_txPoolPending 0.  pending1
let txPoolPending2 = make_txPoolPending 0.  pending2
let txPoolPending3 = make_txPoolPending 0.  pending3
let txPoolPending4 = make_txPoolPending 5.  pending1
let txPoolPending5 = make_txPoolPending 10. pending1
let txPoolPending6 = make_txPoolPending 15. pending1
let txPoolPending7 = make_txPoolPending 20. pending1

let txPool0 = make_txPool txPoolPending0 txPoolQueued
let txPool1 = make_txPool txPoolPending1 txPoolQueued
let txPool2 = make_txPool txPoolPending2 txPoolQueued
let txPool3 = make_txPool txPoolPending3 txPoolQueued
let txPool4 = make_txPool txPoolPending4 txPoolQueued
let txPool5 = make_txPool txPoolPending5 txPoolQueued
let txPool6 = make_txPool txPoolPending6 txPoolQueued
let txPool7 = make_txPool txPoolPending7 txPoolQueued

let probs0 = make_probs [||] 0 0

let auction0 = make_auction_environment txPool0 probs0
let auction1 = make_auction_environment txPool1 probs0
let auction2 = make_auction_environment txPool2 probs0
let auction3 = make_auction_environment txPool3 probs0
let auction4 = make_auction_environment txPool4 probs0
let auction5 = make_auction_environment txPool5 probs0
let auction6 = make_auction_environment txPool6 probs0
let auction7 = make_auction_environment txPool7 probs0

let user10      = make_user 5 0 [10.] 1000000. 0 Distance.Now []
let user10x1000 = make_user 5 0 [10.] 1000.    0 Distance.Now []
let user20      = make_user 6 0 [20.] 1000000. 0 Distance.Now []

(********************** Trivial Post Now Tests ***********************)

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 0
   when my size is 10 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 10 size = 100.1 gas price
   100.1x10 *)
let%test "run_post test 1" =
  (run_post auction0 user10)
  =
  (make_result [Some [(ecTokenOfFloat 100.1)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 0
   when my size is 10, but not enough value -> outbidList =
   [ ] which means no bid or None *)
let%test "run_post test 2" =
  (run_post auction0 user10x1000)
  =
  (make_result [None] (-1) [0])

(*********************** Normal Post Now Tests ***********************)

(* [ 1000; 500; 100; 100 ] gas prices with
   [  100;  50;  10;  10 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x10; 100x10 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "run_post test 3" =
  (run_post auction1 user20)
  =
  (make_result [Some [(ecTokenOfFloat 100.05)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  20 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x20 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "run_post test 4" =
  (run_post auction2 user20)
  =
  (make_result [Some [(ecTokenOfFloat 100.05)]] 0 [0])

(* [ 1000; 500; 100; 100; 1M ] gas prices with
   [  100;  50;  10;  10;  1 ] gas limits and size left = 0
   when my size is 20 -> outbidList =
   [ 100x10; 100x10 ] which means an outbid of 2000 + 1 = 2001
   2001 / 20 size = 100.05 gas price
   100.05x20 *)
let%test "run_post test 5" =
  (run_post auction3 user20)
  =
  (make_result [Some [(ecTokenOfFloat 100.05)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 5
   when my size is 10 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 10 size = 100.1 gas price
   100.1x10 *)
let%test "run_post test 6" =
  (run_post auction4 user10)
  =
  (make_result [Some [(ecTokenOfFloat 100.1)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 10
   when my size is 10 -> outbidList =
   [  ] which means an outbid of min bid 1000 - 1 = 999
   999 / 10 size = 99.9 gas price
   99.9x10 *)
let%test "run_post test 7" =
  (run_post auction5 user10)
  =
  (make_result [Some [(ecTokenOfFloat 99.9)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 15
   when my size is 20 -> outbidList =
   [ 100x10 ] which means an outbid of 1000 + 1 = 1001
   1001 / 20 size = 50.05 gas price
   50.05x20 *)
let%test "run_post test 8" =
  (run_post auction6 user20)
  =
  (make_result [Some [(ecTokenOfFloat 50.05)]] 0 [0])

(* [ 1000; 500; 100 ] gas prices with
   [  100;  50;  10 ] gas limits and size left = 20
   when my size is 20 -> outbidList =
   [  ] which means an outbid of min bid 1000 - 1 = 999
   999 / 20 size = 49.95 gas price
   49.95x20 *)
let%test "run_post test 9" =
  (run_post auction7 user20)
  =
  (make_result [Some [(ecTokenOfFloat 49.95)]] 0 [0])

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

let txPoolPending8 = make_txPoolPending 5. pending4
let txPoolPending9 = make_txPoolPending 5. pending5

let txPool8 = make_txPool txPoolPending8 txPoolQueued
let txPool9 = make_txPool txPoolPending9 txPoolQueued

let auction8 = make_auction_environment txPool8 probs0
let auction9 = make_auction_environment txPool9 probs0

let user10  = make_user 5 0 [10.]     1000000. 0 Distance.Now []
let user5'5 = make_user 0 0 [5. ; 5.] 1000000. 0 Distance.Now []
let user2'8 = make_user 0 0 [2. ; 8.] 1000000. 0 Distance.Now []

(*********************** Complex Post Now Tests **********************)

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 10 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 = 412
   412 / 10 size = 41.2 gas price
   41.2x10 *)
let%test "run_post test 10" =
  (run_post auction8 user10)
  =
  (make_result [Some [(ecTokenOfFloat 41.2)]] 0 [0])

(* [ bidder500x50
   ; bidder50x1'50x1'500x1
   ; bidder200x2
   ; bidder10x1 ] and size left = 5
   when my size is 10 -> outbidList =
   [ 10x1; 200x2; 500x1; 50x1 ] which means an outbid of
   [   10;   400;   500;   50 ] + 1 = 961
   961 / 10 size = 91.6 gas price
   91.6x10 *)
let%test "run_post test 11" =
  (run_post auction9 user10)
  =
  (make_result [Some [(ecTokenOfFloat 96.1)]] 0 [0])

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 5&5 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 =
   412 / 2 bids total = 206 each bid
   206 / 5 size each = 41.2 gas price each bid
   [ 41.2x5; 41.2x5 ] *)
let%test "run_post test 12" =
  (run_post auction8 user5'5)
  =
  (make_result [Some [(ecTokenOfFloat 41.2) ; (ecTokenOfFloat 41.2)]] 0 [0])

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
let%test "run_post test 13" =
  (run_post auction9 user5'5)
  =
  (make_result [Some [(ecTokenOfFloat 96.1) ; (ecTokenOfFloat 96.1)]] 0 [0])

(* [ bidder500x50'1x1
   ; bidder50x1'50x1'500x1
   ; bidder200x1'200x1
   ; bidder5x1'5x1 ] and size left = 5
   when my size is 2&8 -> outbidList =
   [ 1x1; 5x1; 5x1; 200x1; 200x1 ] which means an outbid 411 + 1 =
   412 / 2 each bid = 206 each bid
   206 / 2 size = 103 gas price and 206 / 8 size = 25.75 gas price
   [ 103x2; 25.75x8 ] *)
let%test "run_post test 14" =
  (run_post auction8 user2'8)
  =
  (make_result [Some [(ecTokenOfFloat 103.) ; (ecTokenOfFloat 25.75)]] 0 [0])

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
let%test "run_post test 15" =
  (run_post auction9 user2'8)
  =
  (make_result [Some [(ecTokenOfFloat 240.25) ; (ecTokenOfFloat 60.0625)]] 0 [0])

(***************************** POST TESTS ****************************)
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

let txPoolPending9 = make_txPoolPending 4. pending6

let txPool9 = make_txPool txPoolPending9 txPoolQueued

let probArray = Array.make length0 0.
let _ = fill_probs probArray 0. 100.
let probs1 = make_probs probArray 16 84

let auction10 = make_auction_environment txPool9 probs1

let userV0  = make_user 6 0 [5.] 1.       length0 Distance.Long []
let userV50 = make_user 7 0 [5.] 50.      length0 Distance.Long []
let userS5  = make_user 8 0 [5.] 1000000. length0 Distance.Long []

let timings0 = [4; 7; 13; 25; 50; 100]

(********************** Trivial Post Prob Tests **********************)

(* A maxValue of 0 should result in a list of Nones *)
let%test "post test 16" =
  (run_post auction10 userV0)
  =
  (make_result
    [ None
    ; None
    ; None
    ; None
    ; None
    ; None ]
    (-1)
    timings0)

(* A maxValue of 50 should result in a only a Bidder with a price 10 *)
let%test "post test 17" =
  (run_post auction10 userV50)
  =
  (make_result
    [ None
    ; Some [(ecTokenOfFloat 10.)]
    ; None
    ; None
    ; None
    ; None ]
    1
    timings0)

(********************** Trivial Post Fees Tests **********************)
(*********************** Normal Post Prob Tests **********************)

(* A bell curve probabilities with:
   [ 0 - 100 ] lower bound: 16, upper nound 84.
   and a txPool where there are:
   [ 500; 100; 50 ] gas prices with
   [  50;  10;  5 ] gas limits and size left = 4
   where my size is 5 *)
let%test "post test 18" =
  (run_post auction10 userS5)
  =
  (make_result
    [ None
    ; Some [(ecTokenOfFloat 10.)]
    ; Some [(ecTokenOfFloat 13.4)]
    ; Some [(ecTokenOfFloat 19.41)]
    ; Some [(ecTokenOfFloat 19.59)]
    ; Some [(ecTokenOfFloat 50.2)]]
    1
    timings0)

(*********************** Normal Post Fees Tests **********************)
(******************* Complex Post Tests Structures *******************)
(************************ Complex Post Tests *************************)
