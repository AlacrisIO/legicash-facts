# Design for offchain/onchain interactions

## Purpose of this document

Precise English-language specification of Legicash onchain/offchain interactions

**XXX**: Note that at the moment, this document assumes that each facilitator
has their own Merkle root hash in the on-chain contract. It [might be
possible](https://legicash.slack.com/archives/G9XDQA4UA/p1527796141000710) to
aggregate all global state into a single root hash, which some facilitator is
mandated to report to the contract.

## Designs

### Design with a court registry

Key idea is that facilitator is responsible for reporting all transactions to
interested parties, on pain of penalty. Need a way to prevent DoS attacks by
demanding too much information.

#### Data Availability / Liveness issue

This is the central problem facing a Legicash-style architecture: How to
incentivize provision of data which may otherwise be harmful to the interests of
a dishonest participant. The main issue is liveness/performance guarantees. It's
easy enough to mandate that a transaction request from Alice to Trent isn't
valid until both have collaborated on a signature, but what if Alice's initial
request goes unheeded by Trent? Alice reports to the network, "This request I
gossiped about; I never heard back from Trent." Trent says "No, I did respond:
Here's the response." Now we seemingly have to fall back on network observation
consensus to resolve the question of whether Trent's response was generally
available.

The problem is particularly acute for performance-assurance of the court
registry, because it's essential to generating Merkle proofs during disputes
that an independent, complete view of each facilitator's activity be available.
So not only does the registry need to record the facilitators' actions, someone
needs to be scrutinizing the registry's responsiveness, leading to an infinite
regress...

It's likely that some kind of gossip network of putatively independent registries
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

Each of these has an ETH address, represented by `<name>`, e.g. `alice`

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

Since this will lead to n<sup>2</sup> transmission and storage, we may want to
break the facilitators into random cliques of, say, size 20-100, all of whom are
responsible for tracking all events in their clique.

###### Questions about the gossip protocol

- <sup>**"Why do we need a VRF for gossip?"**</sup>

  It mandates who to talk to, so you can't just talk to your collaborators, in
  the event that you are colluding to defraud the system. Selection can be based
  on Trent's current perception of the participants.
  
- <sup>**"...how do you react if someone is slow to reply? whose fault is it
  when A and B fail to talk to each other?"**</sup>

  The details for this aren't clear, yet, but the idea is that A and B are
  reporting to others at the same time, and the results of their VRF
  calculation, who they've heard back from, *etc.*, are part of those reports.
  So B's response to A is an unperformed duty for B, reported by A to several
  other people. If, for instance, A hasn't heard back from B after some
  deadline, that is something they can/must report to the network, and can be
  checked against B's reports to others. Also, A will be able to get B's report
  from others if there's a network failure in B->A which the facilitator report
  graph can route around. Yes, this is a lot of communication overhead, but I
  think this can be guaranteed secure given an honesty threshold.
  
- <sup>**"...what if some facilitators band against an honest one to get him
  expelled?"**</sup>

  The shifting mandated gossip paths are going to make that difficult.
  
- <sup>**"Are you going directly for the TCR / Open Registry, or for a closed 
  registry?"**</sup>

  Open: The facilitators are gossips. Maybe other people can gossip, too, if
  they post a bond.
  
- <sup>**"With what weight quorum wise, though?""**</sup>

  This is a probabilistic calculation which will require a formalism about the
  gossip network I need to develop. The question is, roughly, given the random
  graph implied by the VRF's and the number of simultaneous reports (graph
  degree), what is the probability that a hostile node gets to gossip only to
  his collaborators, assuming a given threshold of facilitators are honest?
  
- <sup>**"What if I have extra paths beyond what the vrf mandates? What about
  rewarding the vrf paths?"**</sup>
  
  Using such paths for assessing performance guarantees would undermine the
  security provided by the mandated paths, but obviously any data received by
  those paths can be used to construct Merkle proofs for disputes. We could
  reward such behavior, or set up an auction/pay-per-use for such performance.
  
##### Setting up a facilitator 

Trent wishes to become a facilitator. He sends a bond of *x* ETH from `trent` to
smart-contract jurisdiction `judy`, registering his intent, and the address
where his off-chain service may be contacted. In doing so, he takes on the
following duties:

1. Processing deposits and withdrawals on the main chain.
2. Responding to and reporting on state transitions in his and other side chains
   in his jurisdiction, per the gossip protocol.
3. Providing, on request, the data and Merkle proof for any side-chain
   transaction he's heard about and can compute. Again, requests can be reported
   to multiple parties simultaneously, and if Trent doesn't provide a signed
   response within a block, he can be punished.
4. Responding to transaction requests on his side chain, and reporting the
   transitions. When someone requests a transaction from him, they report it to
   a random selection of other facilitators, and if he doesn't provide a signed
   response by the next block, he can be punished.
   
##### Deposit from the main chain

Alice at address `alice` sends 1 ETH to smart-contract jurisdiction Judy, with a
note that it's for Trent's side-chain.

Trent has a duty to recognize this by assigning 1 ETH to `alice` on his side
chain within 100 blocks. Otherwise, he can be fined, and custody of the money is
returned to Alice, in terms of methods on Judy which will allow her to transfer
the ETH to any other address, or to another Judy-jurisdiction facilitator.
Transfers from this state to another facilitator are treated essentially the
same way as a deposit from another ETH address.

Other facilitators have a duty to gossip about `alice`'s deposit.

##### Withdrawal from side chain to main chain

Alice signs a request to Trent to transfer 1 ETH back to her custody on Judy.
She sends the request to a number of facilitators, including Trent. They have a
duty to gossip about it. Trent must assign 1 ETH to Alice by a certain date, or
be punished.

Once Trent re-assigns via a message to Judy, Alice's balance doesn't go straight
back to the ETH address `alice`, but `alice` messages to a `judy` method will
then have the authority to transfer her balance to other addresses, or to other
Judy-jurisdiction facilitators.

##### Request for Merkle proof

To initiate a dispute process, anyone can ask for a Merkle proof for any event
in the system, from any facilitator, and gossip about that to other
facilitators. If the facilitator doesn't know about the event, it can report
that. If another facilitator shared that event with them

##### Sharing observed state transitions with other facilitators

##### Response to transaction request

##### Complaint about unrecognized on-chain deposit

##### Complaint about unrecognized on-chain withdrawal

See [Complaint about unperformed duty](#complaint-about-unperformed-duty). 

##### Complaint about badly formatted performed duty

The smart contract needs to be able to parse and validate the performed-duty
records.

Structure of complaint: An on-chain message containing the Merkle proof for the
ill-formed report, and perhaps instructions on how to invalidate it. Signature
of the responsible facilitator must always be verified, though, because they are
fined.

##### Complaint about unperformed duty

A proof is constructed for Alice's withdrawal request, as a Merkle path in some
facilitator's gossip about Trent's unperformed duties, if not a performed-duty
report signed by Trent himself. The proof is submitted with a bond, by someone.
It doesn't have to be Alice; she may not have sufficient funds for the bond.
Judy checks the Merkle path, and that no relevant custody transfer to Alice has
been made by Trent. The appropriate bond is slashed, and Alice's funds are
released, if appropriate.

##### Complaint about transaction unsigned by sender, but signed by facilitator

Structure of proof: Merkle path to transaction.

##### Complaint about invalid transaction 

Structure of complaint: An on-chain message containing the Merkle proof for the
account balance and the transaction which overspends it. Sender and facilitator
signatures must be verified, because they are both fined in this case.

Note that the account balance must be part of the verified Merkle tree root, for
this to work.
