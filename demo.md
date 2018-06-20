# Plan for our demo

[[_TOC_]]

## Flows to get right

We identified the following flows in Legicash FaCTS.
For the M1 demo, we can stub most of them,
and here are the basic ones we believe are really essential:

  1. Deposit Money (Necessary)
  2. Payment (It's the whole point)
  3. Withdraw money (Necessary)
  4. Prove Fraud (Must illustrate interactive proof)

These are advanced flows necessary for the M2 Feature Complete release:

  1. Forced transaction on main chain (necessary for repudiability, atomic
     transactions and layer 3 contracts)
  2. Atomic Cross-chain Settlement Flow (necessary to scale multi-side-chains)
  3. Failed or Closed Facilitator Recovery (necessary for security that scales
     and is fair)
  4. Mass Transfers(?) (necessary for security that scales?)
  5. Forced transaction on side chain (necessary for making repudiability scale)
  6. Create Facilitator (very incomplete version for M1)
  7. Update facilitator sidechain state + pay penalties (partial for M1)
  8. Close facilitator (necessary for facilitators to get their money back)
  9. All fraud proofs (not just a few for demo)
  
### Court-registrar flows

#### Registration of fundamental side-chain events

Each of these events has two flows associated with it: The initial report, and
obtaining the Merkle path associated with it once it's committed to the
main-chain root hash. The Merkle-path request generates a new event, which
generates gossip about the request itself.

1. Main-chain deposit by Alice.
2. Main-chain request by Alice to deposit to Trent's side chain.
3. Trent's acknowledgment of deposit request, including new balance.
4. Request for side-chain transfer from Alice to Bob, via Trent.
5. Trent's acknowledgment of transfer request, including new account balances.
6. Withdrawal request by Alice to Trent.
7. Trent's acknowledgment of withdrawal request, including new account balance.
8. Trent's call to main-chain contract, giving Alice custody of withdrawn funds.

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
2. Reggie's registrar tenure ends, or he exits the role, and his bond is
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

## Basic Flows

### Deposit Money

Actors: Alice (sender), Trent (facilitator)

Preconditions:
* Alice has an account on the main chain with balance X+F+R.
* Trent already has an open contract as a facilitator on the main chain.
* Alice has an account on Trent's side-chain with balance Y (0 if never used before).
* Trent has an account for Alice with balance Y.

Postconditions:
* Alice has an account on the main chain with balance R.
* Alice has an account on Trent's side-chain with balance X+Y.
* Trent received fee F from Alice.

1. Alice sends X+F to Trent's contract on the main chain.
2. Alice waits for confirmation then sends deposit request to Trent.
3. Trent writes deposit confirmation M1...
4. Trent commits M1 to side-chain
5. Trent sends M1 to Alice (OKish if fails)
6. Trent commits M1 to main chain

1 is enough for Alice to have her money back even if Trent fails.
If 3, 4, 5 or 6 didn't happen, Alice still can get her money back, which
the adversarial exit thing must take into account.

TODO: in a future version, use contracts on the side-chain,
with a contract that if Alice sends money to Trent2 on the main chain
(or in another side-chain, e.g. announcing some USD transactions),
then Trent will credit Alice on his side-chain.



### Payment Flow

Actors: Alice (sender), Bob (recipient), Trent (facilitator).

Preconditions:
* Alice has an account on Trent's side-chain, with balance X=Y+F+R.
* Bob has an account on Trent's side-chain, with balance Z.
* F is larger than the fee required by Trent for the payment.
* If the payment expedited, Trent's limit L is larger than Y.

Postconditions:
* Alice has an account on Trent's side-chain, with balance R.
* Bob has an account on Trent's side-chain, with balance Z+Y.
* Trent received F as a fee.
* If the payment expedited, Trent's limit L was decreased by Y.

1. Alice write a message M1, signed by her, that says:
  "Trent, please pay Z to Bob and receivee your fee F.
  My previous state with you was P.
  This message is written at time T0 (identified by main chain state M, side-chain state S).
  This message is valid until date T1. -- signed by Alice"
2. Alice sends message M1 to Trent for underwriting.
3. Trents verifies that M1 is correct, then signs a message M2 certifying M1:
  "I, Trent, certify M1 as valid and promise to make good on it in my next update to the main chain.
  Latest side-chain update was PS. My current TXID is T. My new limit for this cycle is NL."
4. Trent commits M2 to his side-chain (including remote replicas)
5. Trent sends M2 to Alice (and Bob?)
6. Alice sends M2 to Bob.
7. Bob sends M2 to the Gossip Network, waits to check there is no double-spend by Trent.
8. Bob accepts the payment, and starts servicing Alice.
9. Trent updates the main chain and the payment is fully cleared.


### Withdraw Money

Actors: Alice (sender), Trent (facilitator)

Preconditions:
* Trent already has an open contract as a facilitator on the main chain.
* Alice has an account on Trent's side chain with balance X=Y+F+R.

Postconditions:
* Alice has an account on Trent's side chain with balance R.
* Trent's contract sent Y to an address of Alice's choice.
* Trent received fee F from Alice.

1. Alice signs a check for withdrawal of Y on Trent's side-chain, with fee F.
2. Trent signs a confirmation, commits it, then (optionally) sends it back to Alice.
3. Alice waits for the confirmation to be confirmed on the main chain.
4. Alice then submits on the main-chain a claim for withdrawal.
5. Alice waits for a challenge period during which no one disputes her request.
6. Alice then exerts her undisputed (or successfully defended) claim on the main chain.


### Fraud Proof Flow

Same as above, but first Alice may have to publish her request on the main chain
to put Trent on notice that he must close Alice's account of be proven invalid.

1. Alice sends claim to Trent's contract on the main chain with all details
 (including list of pending transactions). Details may be posted on court registry
 instead of included in extenso in court proceedings.
2. a. If details invalid or incomplete,
  Trent contests and gets Alice thrown out "come back with your file in order"
  b. Trent goes missing or invalid. See Facilitator invalidation story, then go back to 2c.
  c. Trent has to post his side of the story, with a side-chain state update.
3. Interactive proof without mutable state, in predictable, finite number of steps.
4. Sanction.

The sanction can punish Trent for mismanaging his side-chain,
can punish Alice for making a spurious claim.


## Advanced Flows

TODO: update these flows.

### Cross-Facilitator Settlement Flow

Actors: Alice (sender), Bob (recipient), Trent (facilitator), Tara (facilitator).

Preconditions:
* Alice has a voluntary account on Trent's side-chain with balance X+F+R.
* Alice has an account on Tara's side-chain with balance Z (or no account and balance 0)
* Bob has a voluntary account on Tara's side-chain with balance Y+G+S.
* Bob has an account on Trent's side-chain with balance T (or no account and balance 0)

Postconditions:
* Alice has a voluntary account on Tara's side-chain with balance Y.
* Bob has a voluntary account on Trent's side-chain with balance X.
* Alice has a voluntary account on Trent's side-chain with balance R.
* Bob has a voluntary account on Tara's side-chain with balance S.
* Trent receives fee F from Alice.
* Tara receives fee G from Bob.

1. Alice signs M1..., contract conditional on all signatures, etc., within timeout
2. Alice sends M1 to Bob
3. Bob signs M2 ...[M1]
4. Bob sends M2 to Trent
5. Trent underwrites M3...[M2] (acquire lock)
6. Trent sends to Tara
7. Tara underwrites M4...[M3] (acquire lock)
8. Tara commits M4 to side-chain
9. Tara sends M4 to Trent
10. Trent commits M4 to side-chain
11. Tara commits M4 to main chain
12. Trent commits M4 to main chain
13. Payment confirmed on Tara's chain (resource unlocked)
14. Payment confirmed on Trent's chain (resource unlocked)

If 10 happens and Bob trusts both Tara and Trent, and Tara trusts Trent, then Bob can accept payment.

If any of the steps before 12 (included) fails, the settlement is incomplete and no effect takes place.

Now, the adversarial exit flow HAS to take into account the potentially incomplete transactions!!!
(True for *all* kinds of transactions.)

NB: is it worth going more than 2 facilitators, in the style of the Lightning Network?


### Exit Settlement Flow

Actors: Bob (recipient), Trent (facilitator)

Preconditions:
* Bob has an account on Trent's side-chain with balance X+F+R.
* Trent has main-chain account Trent2 for exit settlements, unencombered by contractual obligations, with balance larger than X plus a main-chain fee.

Postconditions:
* Bob has an account on the main chain with balance X.
* Bob has a voluntary account on Trent's side-chain with balance R (or no account, and balance 0).
* Trent receives fee F from Bob.

1. Bob signs M1 ... contract
2. Trent signs M2... [M1]
3. Trent commits M2 to side-chain
4. Trent sends M2 to Bob (OKish if fails)
5. Trent sends X to Bob on main chain via Trent2 settlement account.

If 5 didn't happen, Bob still can get her money back, which
the adversarial exit thing must take into account.


### Facilitator Invalidation Flow

1. a. Trent posts invalid state update. Someone proves it in court.
   b. Trent times out.
2. Victor, Vanna, etc., post adversarial updates of Trent's side-chain state, within some time limit.
3. After time limit elapses, Trent's new state is whatever can be reconstituted
   from the adversarial updates in priority order of first posted first valid
   in case of double-spending conflict. PLUS pending lawsuits.
   [People with lawsuit data better make sure any relevant data is posted in those updates.]
4. Bankrupcy proceedings via mass voluntary and involuntary transfers [TODO: EXPAND!]


### Involuntary Transfer Assignment Flow

Actors: Bob1..N (recipient), Trent (facilitator), Tara1..M (recipient facilitator)

Precondition:
* Trent has been found invalid, and his final state has been determined.

1. An open window of duration D lets Tara1..M put their stake for involuntary transfers
  and/or let Bob1..N assign voluntary transfer status to a TaraK of their choice.
  [TODO: details for conflicts]
2. At the end of the window, it's clear who transfers what where:
  The first facilitator who posts to main chain a voluntary transfer request for a user wins it.
  For involuntary, a lottery is run based on whatever consensus data. [TODO: details]


### Mass Transfers

Actors: Bob1..N (recipient), Trent (facilitator), Tara (recipient facilitator)

Preconditions:
* Bob1..N have chosen Tara or (if involuntary) been assigned Tara as exit person

Postconditions:
* Bob1..N now have their account with Tara.

1. Tara posts her challenge first all details on the court registry, then claim in court (main chain).
2. Trent either posts final update on all these people,
   or goes invalid (then, go back to 1 after involuntary transfer assignment).
3. Tara will in parallel, run all M steps of the verification for each of the N Bobs with Trent.
   BUT, each of the proofs, in parallel, can be forked by Victor1..P at each step,
   in case either Tara or Trent fails.
4. At the end of the proof window, we can compare Tara to each of the Victor1..P
   to check that Tara didn't cheat, by comparing with the verifiers.
5. a. If Tara cheated, she loses her license, and both Tara and Trent go back to the invalidation flow.
   b. Tara is right, and makes a consolidated claim, that Trent and/or Victors can check
6. After final proof window, the claim clears and Trent is compelled to send the money to Tara.
7. Any remaining Bobs who got ejected from the parallel verification can make a new claim with Tara
   or her competitors.

NB: Modal operator fork down (arecurrence) or fork up (coarecurrence)
 analogous to other computability logic operators brecurrence and cobrecurrence:
 the argument can be forked adversarially each by a verifier.


## Other questions to get right

### Predicting court fees as prelude to determining bond.

The bond for someone starting a lawsuit should be expected with very high probability
to more than cover all legal fees in the lawsuit,
multiplied by a factor sufficient to impose a penalty on frivolous lawsuits (to be determined).

The legal fees are proportional to the maximum depth of the argument by
the expected price of the legal fees, within so many standard deviations.
We can deduce the maximum depth of the argument from the formula being argued itself.
As for the expected price of the legal fees:
in a short-term contract, the price that the loser will pay the winner in legal fees
should be agreed upon by the participants based when signing the contract,
based on recent court fees and their recent standard deviation and historical spikes.
For long-term contracts, they would better depend on an oracle;
but then what if they rise higher than the bond posted by either party?
What if it is hard to make such an oracle?
If the blockchain has a builtin oracle for court fees, or some otherwise trusted oracle, great.
If not, one way is to trust and agree as valid in a far enough future
the past utterances of an oracle the validity of which all parties can check a posteriori;
if the oracle ever lies, the participants can exit the contract
before lying oracle values become effective;
the limitation is that this supposes that participants can effectively price in advance
(based on more basically trusted oracles) an exit from the contract
without either party being a clear loser.
Based on the same kind of oracles, parties may be required to up their ante and post a higher bond,
or be considered in default with some penalty and/or loss of license as a consequence
(or reputation, if applicable).

If two third-party lawsuits for the same case (or subcase) are registered within the same block,
one of them is anterior, and will take precedence.
The other ones must be retracted in favor of whichever anterior third-party argument is valid,
within a retraction window;
or they will themselves be found frivolous if the anterior lawsuit concludes the case.
Just like every two-party case can always be settled,
every third-party lawsuit can be retracted in favor of an anterior one.


### Getting the logic model right

The programming / logic model has to deal with
public databases that atomically include records,
and relations between consecutive states of these databases.

(How) can the contract "see" things outside of the contract state?
Does the Ethereum and/or Tezos API help with that?
If not (and/or not cheaply, easily, reliably, in the long run, etc.),
can we identify what API entry points Tezos or Ethereum would need to add?

A signature by a manager is a promise to include some data in the next version of the database.
How do we deal with failing managers who withhold signature on an ongoing transaction?

### Dealing with UTXOs

Bitcoin maintains a model of UTXOs. Ethereum and Tezos just have account balances.
(TODO: look what Cardano uses. I bet also account balances and not UTXOs for contract sanity.)
At first we only intend to support Ethereum and Tezos, that don't use UTXOs;
but in the future we may want to support Bitcoin, that does.

Plasma tries very hard to preserve a notion of UTXOs so as to avoid double-exit.
However, any non-trivial series of payments on the side-chain becomes impossible,
short of repeating the same series on the main chain (under the ownership of the contract to itself),
which would totally defeat the purpose of scaling.
That's absurd, and
[more absurdity ensues](https://ethresear.ch/t/one-proposal-for-plasma-cash-with-coin-splitting-and-merging/1447)
trying to make that work.
The [Plasma Cash](https://ethresear.ch/t/plasma-cash-plasma-with-much-less-per-user-data-checking/1298)
proposal even does away with divisibility of UTXOs,
making regular payments impractical and micropayments impossible.

Our solution (at least for now) is to NOT deal with UTXOs,
and instead always keep all the money in the contract consolidated
at all (or most) times in a single UTXO (if the main chain has UTXOs).
*If* the main chain model both has UTXOs and allows for them to be sent to a contract
without the contract agreeing to it via some function call
then said UTXOs are not validated as part of the user account
until the user (or, in a marketing gesture, the manager) pays a fee (inside the side-chain)
that covers the cost (on the main chain) of merging the UTXO into "the" UTXO for the contract
as part of the next side-chain state update.
We don't need to introduce UTXOs for our exit model, because
we instead use a court registry, and map/reduce over parallel proofs
as a way to compute proven-correct exit balances.
