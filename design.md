# Design for offchain/onchain interactions

## Purpose of this document

Precise English-language specification of Legicash onchain/offchain interactions

## Designs

### Design with a court registry

Key idea is that facilitator is responsible for reporting all transactions to
interested parties, on pain of penalty. Need a way to prevent DoS attacks by
demanding too much information.

#### Data Availability / Liveness issue

This is the central problem facing a Legicash-style architecture. How to
incentivize provision of data which may otherwise be harmful to the interests of
a dishonest participant. The main issue is liveness/performance guarantees. It's
easy enough to mandate that a transaction request from Alice to Trent isn't
valid until both have collaborated on a signature, but what if Alice's initial
request goes unheeded by Trent? Alice reports to the network, "This request I
gossiped about; I never heard back from Trent." Trent says "No, I did respond:
Here's the response." Now we seemingly have to fall back on network observation
consenus to resolve the question of whether Trent's response was generally
available.

The problem is particularly acute for performance-assurance of the court
registry, because it's essential to generating Merkle proofs during disputes
that an independent, complete view of each facilitator's activity be available.
So not only does the registry need to record the facilitators' actions, someone
needs to be scrutinizing the registry's responsiveness, leading to an infinite
regress...

It's likely that some kind of gossip network of putatively indepedent registries
could be useful for this issue, and a sketch of that network is included below,
without assertion of any security guarantees at this stage. Those guarantees
will likely be probabilistic, and assume a threshold number of honest
participants.

#### Actors

Alice:   A transmitter of funds on the side-chain.
Bob:     A receiver of funds on the side-chain.
Trent:   A side-chain transaction-facilitator.
Terry_n: Competing side-chain transaction-facilitators.
Judy:    Smart-contract on-chain/off-chain interface.

Each of these has an ETH address, represented by `0x<name>`, e.g. `0xalice`

#### Scenarios

##### Gossip duties

Each facilitator has a duty to gossip with other facilitators about all the
transactions they've seen. Other people may sign up for this duty as well, by
posting an adequate bond.

There are two sets of gossip: One about duties facilitators are observed to
have, but not yet performed; the other about duties which have been performed. A
performed duty is signed by the relevant facilitator, and he/she assigns a
number to it in a linearization of all their known duties. The signed, numbered
version is then reported to other facilitators, who take it off their list of
unperformed duties for that facilitator. That whole linearization goes into
their next on-chain Merkle hash.

Facilitators use a VRF to choose who to gossip with, and report on the VRF
output when initiating gossip. That initiation is itself a performed duty the
facilitator then reports in other gossips, and the response is a duty of the
interlocutor. A rapid means for facilitators to identify which duty reports to
share needs to be figured out. This is very similar to the problems faced by a
write-heavy distributed database, so that may be a fruitful place to look for
solutions.

##### Setting up a facilitator 

Trent wishes to become a facilitator. He sends a bond of $x$ ETH from `0xtrent`
to smart-contract jurisdiction `0xjudy`, registering his intent, and the address
where his off-chain service may be contacted. In doing so, he takes on the
following duties:

1. Processing deposits and withdrawals on the main chain.
2. Responding to and reporting on state transitions in his and other side chains
   in his jurisdiction. (Probably in a yet-to-be-specified gossip protocol
   between the jurisdiction's facilitators, which should probably involve signed
   gossip handshakes mandated by VRFs, which are themselves gossiped about.
   Gossips are sent to multiple randomly chosen facilitators simultaneously, and
   anyone who doesn't respond to a gossip before the next block can be punished,
   but there is an infinite regress of reporting duties here, which it *might*
   be possible to mitigate by sharing increasingly abstract summaries of the
   data, as consensus about it between two parties is established.)
3. Providing, on request, the data and Merkle proof for any side-chain
   transaction he's heard about and can compute. Again, requests can be reported
   to multiple parties simultaneously, and if Trent doesn't provide a signed
   response within a block, he can be punished.
4. Responding to transaction requests on his side chain, and reporting the
   transitions. When someone requests a transaction from him, they report it to
   a random selection of other facilitators, and if he doesn't provide a signed
   response by the next block, he can be punished.
   

##### Deposit from the main chain

Alice at address 0xa11ce sends 1 ETH to a smart-contract jurisdiction J, with a
note that it's for Trent's side-chain.

Trent has a duty to recognize this by assigning 1 ETH to 0xalice on his side
chain within 100 blocks. Otherwise, he can be fined, and custody of the money is
returned to Alice, in terms of methods on Judy which will allow her to transfer
the ETH to any other address, or to another Judy-jurisdiction facilitator.

Other facilitators have a duty to gossip about 0xalice's deposit.

##### Withdrawal from side chain to main chain

##### Request for Merkle proof

##### Sharing observed state transitions with other facilitators

##### Response to transaction request

##### Complaint about unrecognized on-chain deposit

##### Complaint about badly formatted performed duty

The smart contract needs to be able to parse and validate the performed-duty
records.

Structure of complaint: An on-chain message containing the Merkle proof for the
ill-formed report, and perhaps instructions on how to invalidate it. Signature
of the responsible facilitator must always be verified, though, because they are
fined.

##### Complaint about transaction unsigned by sender, but signed by facilitator

Structure of

##### Complaint about invalid transaction 

Structure of complaint: An on-chain message containing the Merkle proof for the
account balance and the transaction which overspends it. Sender and facilitator
signatures must be verified, because they are both fined in this case.
