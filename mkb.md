# Design for the MKB

This document details designs for the Mutual Knowledge Base.
Some items are annotated with a milestone number (M0, M1, M2, M3) at which it becomes relevant.

[[_TOC_]]

## Overview

The Mutual Knowledge Base (MKB) is an economic validation network creating Mutual Knowledge.
In the lingo of the Ethereum Research Foundation, it is an "availability engine":
it forces side-chain operators to publish their data, thereby preventing "block-withholding attacks".
In the analogy of the blockchain with a court system, the MKB is analogous to a Court Registry.
Members of the MKB are called registrars.

In the base usage case for the MKB, users communicate data elements in parallel
to each active registrar of the MKB. Each data element must fit within a registered "topic",
that specifies a data schema that the element must abide by.
Registrars check that elements indeed fit within the topic before accepting them.
Users may also request from registrars that they sign the data elements.
In the simplest use case, the signatures are independent and can be obtained in parallel,
without MKB registrars needing to communicate with each other.
Having signatures from two thirds of the registrars is considered proof of Mutual Knowledge.

To count as two thirds of the registrars, signatures are weighed by the registrars' shares in the MKB,
as recorded in some ledger.
In a simple case (M0), every registrar can be assumed to have an equal share,
and the set of registrars is assumed to be known and constant.
In a slightly more elaborate case (M1), the ledger recording shares is an ERC20 contract on Ethereum,
or equivalent on another blockchain;
a committee of a few hundred members is picked at random to handle the data,
using a Verifiable Random Function (VRF);
membership is rolling, with some old members retired and new members added, at random,
every few hours, which allows new members to synchronize with the network before they become active,
without disrupting the entire network all the time.
In a yet more elaborate case (M2), the ledger recording shares is managed by the MKB itself,
by layering a consensus protocol that build Common Knowledge on top of the Mutual Knowledge protocol.

Finally, MKB registrars can use an m-of-n signing scheme to create synthetic ECDSA signatures.
The MKB can then be a general-purpose validator for contract-less side-chains
in the style of the Liquid Network.
This can typically be done with a fixed number of rounds of interaction,
driven by the user.


## Layering a CKB on top of the MKB

This section deals with future plans (M2) to layer a CKB on top of the MKB
by building a consensus protocol based on gossiping between registrars.

*The rest of this file is stitched from documents written by Alex Coventry as part of !62.*
See discussion at: https://gitlab.com/legicash/legicash-facts/merge_requests/62
Some of the design is made obsolete in the current context, where we want to clearly separate
the MKB from a DApp's operator network, so that we may blind the MKB
so as to make it less capable of censorship.


#### Registration of fundamental side-chain events

Each of these events has two flows associated with it: The initial report, and
obtaining the Merkle path associated with it once it's committed to the
main-chain root hash. The Merkle-path request generates a new event, which
generates gossip about the request itself.

1. Main-chain deposit by Alice.
2. Main-chain request by Alice to deposit to Trent's side chain.
3. Trent's confirmation of deposit request, including new balance.
4. Request for side-chain transfer from Alice to Bob, via Trent.
5. Trent's confirmation of transfer request, including new account balances.
6. Trent's confirmation of withdrawal request, including new account balance.
   (Alice requests withdrawal on the main chain. Initially, this will set
   contract state. Later, the request transaction itself will be noticed.)
7. Trent's call to main-chain contract, giving Alice custody of withdrawn funds.

In addition, all on-chain contract-state changes must be gossiped.

#### Registration of fundamental side-chain complaints

This section should cover all Court-registry behaviors which are necessary for
fraud proof, assuming the registrar is honest.

These events are complaints which must be registered with Court registrars. As
with the above events, these have both report and retrieval flows. In addition,
they should eventually have refutation flows where possible, though this should
not be part of the demo goals. (It's probably sensible to allow for refutation
on the side-chain first, to reduce on-chain costs when things get out of sync.)

1. Trent did not acknowledge Alice's deposit request from flow 2, after a
   time-out. Refutation: A timely gossip containing Trent's acknowledgement.
2. Alice tried to deposit more from her main-chain account to Trent's side chain
   than she has. This is irrefutable.
3. Alice tried to transfer more from her Trent account than she has.
   Irrefutable.
4. Trent did not acknowledge Alice's side-chain transfer to Bob from flow 4
   above, after a time-out. Refutation: Timely gossip about Trent's
   acknowledgement.
5. Alice tried to withdraw, to the main chain, from her Trent account, more than
   her balance. Irrefutable.
6. Trent failed to acknowledge Alice's withdrawal request. Refutation: A timely
   gossip.

#### Registrar-specific events

1. Reggie posts a registrar bond on the main chain, reporting the point at which
   he will begin behaving as a registrar. This point should be in terms of the
   gossip epistemology: Reggie has requested an update about all live activity
   from all extant registrars, and all those transfers have saturated the gossip
   network, or something like that.
2. With that in mind, as soon as he has posted the bond he can request a
   one-sided version of the gossip protocol below, where he only tells Renée
   what he knows, and she gives him any extra information she has.
3. Reggie's registrar tenure ends, or he exits the role, and his bond is
   returned, after a delay long enough for any reports of him misbehaving to
   saturate the gossip network.

##### Gossip

There are interactions between registrars to mitigate the risk of fraudulent
collusion between side-chain participants by sharing information between
hopefully-independent observers. We probably don't want this for the first demo,
but will want it for production with independent, untrusted registrars.

1. A new main-chain block is published. Registrar computes which other
   registrars to share with, based on VRF, with blockhash as input.

   When there are uncles, registrars are obliged to gossip according to all
   plausibly live chains.
2. Reggie initiates gossip with fellow registrar Renée for the given blockhash,
   sharing, among other things the VRF of registrars he's mandated to gossip
   with.
3. Renée verifies that they haven't performed this gossip already.
4. Renée and Reggie share, for each operator, the last event number for which
   they have the full history of that operator.
5. For each operator and registrar, they share the event numbers they have
   beyond the last-full-history numbers.
6. For each operator and registrar, they share the events they've concluded
   the other is missing.
7. They generate commitments to the events they've each learned of from the
   other. These could be a list of the event coordinates (say, operator
   index, and event index, per the operator linearization, plus a Merkle root
   hash for the events.)
8. They sign each others' commitments.
9. Those commitments become part of the history of their registrar events, and
   they share them in future gossip sessions.

###### Gossip complaints

1. No VRF report is available from Reggie from n time slots ago, where n is some
   function of gossip-network saturation time. Refutation: Timely gossip of VRF
   report.

   If main-chain blocks are used to mark the time slots, it's probably
   acceptable to require registrars to follow parallel gossip for all live
   uncles.

   This may lead to an incentive to be as slow as allowed for the gossip
   interactions (i.e., n timeslots). Complaint 3. below mitigates this risk.
2. No record of mandated gossip interaction between Reggie and Renée.
   Refutation: Timely gossip of gossip.
3. Renée perceived Reggie's gossip request for block b after she'd perceived
   block b+1. Irrefutable. (Reggie has the **option to abandon the gossip*** once
   he learns of the new block from Renée, and potentially be punished for gossip
   SLA violation via the next complaint, **or** to **wear this tardiness complaint**
   from Renée **by signing off on their gossip interaction**.)
4. Number of mandated gossip interactions Reggie completed in some timeslot is
   below SLA threshold. Refutation: Timely gossip of the gossips demonstrating
   SLA compliance.
5. Reggie's spammed Renée with repeated or ill-formed gossip requests.
   Irrefutable, unless the ill-formedness results from a bad signature, which
   we're assuming is cheap enough to check freely.
6. Reggie's hosed Renée by requesting events he's attested to having seen on
   prior gossips. Irrefutable.
7. Reggie subsequently attested to ignorance of an event he'd previously
   implicitly represented as known to him, during the interaction with Renée.
   Irrefutable.
8. Reggie subsequently attested to ignorance of an event he'd previously
   implicitly represented as known to him, in response to a request for a Merkle
   proof. Irrefutable.
9. Reggie contacted Renée to gossip after he's indicated that he's exiting the
   registrar role on the main chain.
10. The causal path to Reggie's knowledge of some fact (the chain of reports and
   gossip which led to him knowing the fact) is unclear, after some time-out.
   Refutation: the chain.
11. Reggie's signed off on some commitment from step 7. of the previous section
    which is ill-formed or acausal. Refutation: Ill-formed is irrefutable.
    Refutation of acausality is the causal path.
12. Reggie reported inconsistent events in the gossip for two different
    main-chain forks.

##### Poison gossip

It may be sensible to make a quick-check style generator of corrupted gossip,
and include VRF-mandated sharing of this poison between gossipers. If during the
handshake a gossiper fails to catch this poison, they could be punished.
Gossipers could check that the poison they received was valid after the initial
handshake. This would mitigate the issue of gossipers being tempted to skimp on
their verification duties for short-term gain.

Issue: making it impossible for registrars to distinguish poison gossip from bad gossip
can add significant latency to it to the Mutual Knowledge, to the point that
it might be cheaper to "just" reach Consensus.


# Design for offchain/onchain interactions

## Purpose of this document

Precise English-language specification of Legicash onchain/offchain interactions

**XXX**: Note that at the moment, this document assumes that each operator
has their own Merkle root hash in the on-chain contract. It [might be
possible](https://legicash.slack.com/archives/G9XDQA4UA/p1527796141000710) to
aggregate all global state into a single root hash, which some operator is
mandated to report to the contract.

## Designs

### Design with a court registry

Key idea is that operator is responsible for reporting all transactions to
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
that an independent, complete view of each operator's activity be available.
So not only does the registry need to record the operators' actions, someone
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
Trent:   A side-chain transaction-operator.
Terry_n: Competing side-chain transaction-operators.
Judy:    Smart-contract on-chain/off-chain interface.

Each of these has an ETH address, represented by `<name>`, e.g. `alice`

#### Scenarios

##### Gossip duties

Each operator has a duty to gossip with other operators about all the
transactions they've seen. Other people may sign up for this duty as well, by
posting an adequate bond.

There are two sets of gossip, bifurcating on whether the duty the gossip
pertains to has been acknowledged and/or performed by the relevant agent. (Since
this is just a registrar, side-chain actions need only be acknowledged, and
later failure to perform will be justified to the main chain in terms of that
signed acknowledgment. However, there are duties associated with the registrar
role, and performance of those duties is synonomous with acknowledging them.)

An acknowledged/performed duty is signed by the relevant operator, and he/she
assigns a number to it in a linearization of all their known duties. The signed,
numbered version is then reported to other operators, who take it off their
list of unperformed duties for that operator. That whole linearization goes
into their next on-chain Merkle hash.

Operators use a VRF to choose who to gossip with, and report on the VRF
output when initiating gossip. That initiation is itself a performed duty by the
operator, and those he gossiped to then have a duty to gossip their signed
acknowledgment back to him. A rapid means for operators to identify which
duty reports to share needs to be figured out. This is very similar to the
problems faced by a write-heavy distributed database, so that may be a fruitful
place to look for solutions.

Since this will lead to n<sup>2</sup> transmission and storage, we may want to
break the operators into random cliques of, say, size 20-100, all of whom are
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
  from others if there's a network failure in B->A which the operator report
  graph can route around. Yes, this is a lot of communication overhead, but I
  think this can be guaranteed secure given an honesty threshold.

- <sup>**"...what if some operators band against an honest one to get him
  expelled?"**</sup>

  The shifting mandated gossip paths are going to make that difficult.

- <sup>**"Are you going directly for the TCR / Open Registry, or for a closed
  registry?"**</sup>

  Open: The operators are gossips. Maybe other people can gossip, too, if
  they post a bond.

- <sup>**"With what weight quorum wise, though?""**</sup>

  This is a probabilistic calculation which will require a formalism about the
  gossip network I need to develop. The question is, roughly, given the random
  graph implied by the VRF's and the number of simultaneous reports (graph
  degree), what is the probability that a hostile node gets to gossip only to
  his collaborators, assuming a given threshold of operators are honest?

- <sup>**"What if I have extra paths beyond what the vrf mandates? What about
  rewarding the vrf paths?"**</sup>

  Using such paths for assessing performance guarantees would undermine the
  security provided by the mandated paths, but obviously any data received by
  those paths can be used to construct Merkle proofs for disputes. We could
  reward such behavior, or set up an auction/pay-per-use for such performance.

##### Setting up a operator

Trent wishes to become a operator. He sends a bond of *x* ETH from `trent` to
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
   a random selection of other operators, and if he doesn't provide a signed
   response by the next block, he can be punished.

##### Deposit from the main chain

Alice at address `alice` sends 1 ETH to smart-contract jurisdiction Judy, with a
note that it's for Trent's side-chain.

Trent has a duty to recognize this by assigning 1 ETH to `alice` on his side
chain within 100 blocks. Otherwise, he can be fined, and custody of the money is
returned to Alice, in terms of methods on Judy which will allow her to transfer
the ETH to any other address, or to another Judy-jurisdiction operator.
Transfers from this state to another operator are treated essentially the
same way as a deposit from another ETH address.

Other operators have a duty to gossip about `alice`'s deposit.

##### Withdrawal from side chain to main chain

Alice signs a request to Trent to transfer 1 ETH back to her custody on Judy.
She sends the request to a number of operators, including Trent. They have a
duty to gossip about it. Trent must assign 1 ETH to Alice by a certain date, or
be punished.

Once Trent re-assigns via a message to Judy, Alice's balance doesn't go straight
back to the ETH address `alice`, but `alice` messages to a `judy` method will
then have the authority to transfer her balance to other addresses, or to other
Judy-jurisdiction operators.

##### Request for Merkle proof

To initiate a dispute process, anyone can ask for a Merkle proof for any event
in the system, from any operator, and gossip about that to other
operators. If the operator doesn't know about the event, it can report
that. If another operator shared that event with them

##### Sharing observed state transitions with other operators

##### Response to transaction request

##### Complaint about unrecognized on-chain deposit

##### Complaint about unrecognized on-chain withdrawal

See [Complaint about unperformed duty](#complaint-about-unperformed-duty).

##### Complaint about badly formatted performed duty

The smart contract needs to be able to parse and validate the performed-duty
records.

Structure of complaint: An on-chain message containing the Merkle proof for the
ill-formed report, and perhaps instructions on how to invalidate it. Signature
of the responsible operator must always be verified, though, because they are
fined.

##### Complaint about unperformed duty

A proof is constructed for Alice's withdrawal request, as a Merkle path in some
operator's gossip about Trent's unperformed duties, if not a performed-duty
report signed by Trent himself. The proof is submitted with a bond, by someone.
It doesn't have to be Alice; she may not have sufficient funds for the bond.
Judy checks the Merkle path, and that no relevant custody transfer to Alice has
been made by Trent. The appropriate bond is slashed, and Alice's funds are
released, if appropriate.

##### Complaint about transaction unsigned by sender, but signed by operator

Structure of proof: Merkle path to transaction.

##### Complaint about invalid transaction

Structure of complaint: An on-chain message containing the Merkle proof for the
account balance and the transaction which overspends it. Sender and operator
signatures must be verified, because they are both fined in this case.

Note that the account balance must be part of the verified Merkle tree root, for
this to work.


## Court-registry invariants

### Background

The Court Registry solves the "Data Availability Issue" by ensuring that data
submitted to it is well-formed and available for review by third-party
verifiers. These well-formedness and availability ensure that any invalid entry
in a registered side-chain can be detected and opposed to the side-chain manager
in a timely fashion on their main-chain smart contract. As compared to other
Oracles that sign data for use by smart contracts, the Court Registry only has
its members sign something that they directly control, which makes implementing
it a slightly more tractable problem than implementing an arbitrary Oracle.

The "Data Availability Issue" concerns liveness/performance guarantees: It's
easy enough to mandate that a transaction request from Alice to Trent isn't
valid until both have collaborated on a signature, but what if Alice's initial
request goes unheeded by Trent? Alice reports to the network, "This request I
gossiped about: I never heard back from Trent." Trent says "No, I did respond:
Here's the response." Now we seemingly have to fall back on network observation
consensus to resolve the question of whether Trent's response was generally
available.

Thus, registrars are obliged to track and gossip about the duties of
operators and other registrars, and report when an irregularity occurs.
Registrars maintain two data structures: A set of duties for each operator /
registrar for which no signature from the responsible agent has been observed
(i.e., duties not yet acknowledged), and linearized sets of acknowledged duties,
from which Merkle trees and Merkle proof-paths can be computed. For
operators, there are two such sets of duties: The transaction duties, and the
gossip duties. For other registrars, there are only the gossip duties.

After a sufficient delay to allow any duty and the corresponding signature to
saturate the network, an accusation about an unacknowledged duty can be
reported to the main chain with a Merkle proof showing the observation of the
duty on some registrar's gossip-duties tree. That registrar can post the
acknowledgment in refutation.

Since this is only for disputes, the process of information propagation does not
have to be particularly fast. However, any dispute deadlines must take the
expected saturation time for relevant documents into account. It may be sensible
to allow deadline extensions until some cited document is provided by someone.

All operators are mandated Court registrars. Others may participate as
registrars, if they wish, by posting a suitable bond. They receive rewards for
the gossip and Merkle proofs they provide, though the exact reward they should
receive is yet to be determined. It may be that operators' rewards for
performance of Court-registrar duties should be zero, for instance.

### Gossip hand-shake

This is a first pass at a gossip-sharing protocol. There are many opportunities
for improvement of it, which should be taken. It is written mainly to convince
myself that gossip is a plausible strategy for enforcing performance guarantees
and data availability on the Court registrars. At this point, I'm 95% convinced
this can work, as long as the pool of registrars is large enough, and enough of
the registrars are honest. I believe the amount of computation and network
traffic involved is bilinear in the number of transactions and the number of
registrars.

On each time slot (probably one main-chain block, and multiple parallel gossip
duties arise from contemporaneous uncles), each registrar R is mandated to
gossip with a number of other registrars, according to the output of a VRF using
R's signing key and the block height. The reason for using a VRF here is to make
it impossible for colluders to form gossip-network cliques (because it mandates
who they interact with), and to make it impossible for them to predict which
non-colluding participants are sharing their perceptions.

Gossip between two registrars R and S occurs via some protocol like this:

1. Based on each other's perception of what the other already knows from prior
   gossip, they make a list of side-chain duties the other may not have seen.
   This list includes

   - Side-chain requests, and main-chain events requiring side-chain action.
   - All reports of who has gossiped what to whom, as mandated by registrar VRF
     outputs.
   - All registrar signatures on each of the first two items (which is used to
     construct the perception of what each registrar knows, in order to
     construct this list.)

2. They exchange lists of the events they have, which they don't know the other
   has. Duties would be represented by a triple: an index over the participants
   in the network, indicating some party participating in the transaction the
   duty pertains to; an index into the serialized event history for events that
   agent has; and some hash of the event data long enough to make the
   probability of collision less than 1e-100.

   During the hash construction, they also check that it's complex enough to
   avoid false positives on the events they know about. That way, they can be
   sure that if a false positive happens, it's on an event the other party is
   going to share with them. Avoidable false positives can be reported as
   disputes.

   There may be a more efficient way to establish this consensus on what to
   share; this is just a first pass. Bit strings on linearized events related to
   each operator/registrar/account, perhaps?

3. They exchange the entries they've each identified as missing from the other's
   list according to the manifests shared in the last step.

4. They repeat steps 1-3 until they've verified that they've updated each other
   on all duties they've observed.

5. They compare hashes on the data. If the hashes don't match, they
   collaboratively logarithmically search the hashed data structure until they
   find the point of disagreement, à la Truebit. If they find an inconsistency
   in someone else's attested data, they report that together.

Their reports to each other on what they themselves have gossiped are lists of
indexes into the events, similar to the ones described in step 2. above, and a
Merkle root-hash commitment to the specific data in those events.

If this handshake fails at any point, R and S report that on subsequent gossip
time slots. A certain amount of such failure is tolerated, to make the
probability that collusion among x% of registrars could result in them framing
another registrar for noncompliance less than 1e-20. Probably this will result
in registrars making short-term optimizations on the amount of gossiping they
do, unless the reward for gossiping exceeds the cost. In order to reduce the
probability of framing to an acceptable level, the pool of registrars and the
number of registrars each is mandated to gossip with needs to be fairly large.

### Forks of the main chain, and gossip

If there are forks on the main chain with material variations, the gossip for
each fork could be different. On the other hand, presumably most of the time,
the gossip will be the same, and most of it will be the same even when there
*are* material variations, because those will only pertain to a few accounts. So
the optimum would be to allow for shared data between the forks, where it's
consistent. That sounds very complex to write, though.

### Gossip-Data-Structure Invariants

  * All data is verified *before* it is signed.
  * All the [Side-chain Well-formedness](#side-chain-well-formedness)
    constraints, in line with the linearized acknowledged duties, for each
    operator, or a dispute launched by this registrar, or received as gossip
    from another registrar.
  * Last-known state for each of the other registrars in the system is
    maintained, based on the gossip they've been observed to attest to in the
    gossip received.
  * Known-cause DAG for gossip. List of all currently acausal gossip.
  * Causal consistency of gossip: Having received a gossip, a path from its
    origin to this registrar should also be available in the current set of
    data.
  * The Merkle tree for the next update, based on the last set of data shared
    with another registrar. (This is what people will use to prove events during
    disputes.)
  * These data structures exist for all live main-chain forks. There needs to be
    tracking of consistency between the gossip on each fork. If a duty is
    reported by a registrar on one fork, and it is consistent with the
    main-chain state on another fork at that block height, the registrar should
    report that duty there, as well. **This is a complex invariant which needs
    to be nailed down.**
  * All observed gossip recorded in the operator's transaction record
    corresponds to VRF-mandated gossip transactions with no causal
    contradictions, and all sufficiently old gossip which has been reported as
    transmitted to a operator has been acknowledged by them.

