# Posting Transactions

## Structures
#### Bid
- blockHash: int
- blockNumber: int
- from: int
- gas: float
- gasPrice: float
- hash: int
- input: int
- nonce: int
- to_ : int
- transactionIndex: int
- value: int

#### Bidder
- id: int
- last_bids: Bid list

#### Probability
- probs: float array of low, normal, and high fees
- lower: int that represents the bound between low and normal fees
- upper: int that represents the bound between normal and high fees

#### Distance
One of:
- Now: meaning the user must post aggressively now to be included on the block, usually means a high transaction fee.
- Short: meaning the user is nearing the deadline and should use historically higher fees
- Medium: meaning the user is posting at a normal time and can use historically normal fees
- Long: meaning the user is far from the deadline and can use historically low fees

#### TxPool
- sizeLeft: the size left in the TxPool, if any
- bidders: the Bidder list in the pending section of the TxPool, the bidders to outbid.

#### AuctionEnvironment
- block: int representing the current block
- txPool: TxPool representing the current TxPool
- prob: Probability, the current historical probabilities for transaction fees
- fees: Distance to float array HashTable, a mapping of distance to predictive guesses for transaction fees

#### User
- id: int
- nonce: int
- gasLimits: float list of estimated size of transactions
- maxValue: float representing user's maximum value for posting these transactions
- start: int of the block when user began posting
- deadline: int of block user would like to post by
- distance: Distance from the deadline
- previous: Bidder.t option list of previous bids

## Functions
#### Bid Helper Functions
- `fee`: calculates the bid price using gasPrice and gas limit
- `create`: creates a new Bid with all fields set to either given values or min_int/max_int or neg_infinity/infinity
- `set_fee`: returns a new Bid with the gas price calculated from the given Bid fee and everything else kept the same

#### Bidder Helper Functions
- `create`: creates a new Bidder with a new Bid with all fields set to given values or min_int/max_int or neg_infinity/infinity
- `fee`: calculates the total fees of the Bidder's Bids
- `compare`: compares two Bidders based on the Bid.fee of the last nonce of each Bidder
- `set_bids`: sets the given Bidder's lastBids to the given Bid list
- `set_fee`: uses the given bidder and given new total fee to calculate the individual fee for each of the bids

#### TxPool Helper Functions
- `find_min_fee`: find minimum bidder in TxPool
- `reverse_bidder_bids`: reverse each Bidder's Bid list so that the (last) most recent nonce is first

#### Main Helper Functions
- Various printing and debugging functions
- `find_outbids`: helper to find the subset of Bids that need to be outbid

#### Main Functions
- `post_now`: aggressively tries to get onto the block now or returns original bid if the fee of the minimum bidder is greater than our max value
- `post`: generates next bidder (either Some Bidder or None) using either historical probabilities of transaction fees or predictive transaction fees based on distance from deadline
- `run_post`: runs the post handling function

## Tests
- Tests use the structure as listed in [`test_structure.ml`](test_structure.ml):
    ```
    let%test _ = __LINE__ =: [%show: <type>] =:
        actual_expr
        =>
        expected_expr
    ```

## Next Version Ideas
- Potentially use Hashtables instead of Lists to more closely match the txpool structure
- Correct units to be what they actually are.
- How do you know when some of your bid was confirmed?
- Add error checking: user transaction fee is over limit.. etc.
- Query for actual updates/values.
