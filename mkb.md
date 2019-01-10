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
Credit for good ideas go to Alex Coventry.


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
4. Renée and Reggie share, for each facilitator, the last event number for which
   they have the full history of that facilitator.
5. For each facilitator and registrar, they share the event numbers they have
   beyond the last-full-history numbers.
6. For each facilitator and registrar, they share the events they've concluded
   the other is missing.
7. They generate commitments to the events they've each learned of from the
   other. These could be a list of the event coordinates (say, facilitator
   index, and event index, per the facilitator linearization, plus a Merkle root
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
can prevent Mutual Knowledge to be established for sure before Consensus is reached.
