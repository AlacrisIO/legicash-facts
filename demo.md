# Plan for our demo

[[_TOC_]]

## Flows to get right

We identified the following flows in Legicash FaCTS.
For the M1 demo, we can stub most of them,
and here are those we believe are really essential:

  1. Enter Settlement Flow / Account Creation (Necessary)
  2. Payment Flow (It's the whole point)
  3. Adversarial Exit Flow (Must illustrate interactive proof)

These are necessary for the M2 Feature Complete release:
  4. Cross-Facilitator Settlement Flow (necessary to scale multi-side-chains)
  5. Exit Settlement Flow (necessary for practical use of facilitators)
  6. Facilitator Invalidation Flow (necessary for security)
  7. Involuntary Transfer Assignment Flow (necessary for security that scales and is fair)
  8. Mass Transfers (necessary for security that scales)

Smaller flows necessary for M2:
  9. Create Facilitator (very incomplete version for M1)
  10. Update facilitator sidechain state + pay penalties (partial for M1)
  11. Close facilitator (necessary for facilitators to get their money back)
  12. Activate account (stub for M1?)
  13. Denounce facilitator (both minor and major violations, no double jeopardy)


### Payment Flow

Actors: Alice (sender), Bob (recipient), Trent (facilitator).

Preconditions:
* Alice has an account on Trent's side-chain. The account is voluntary, and has balance X.
* Bob has an account on Trent's side-chain. The account is voluntary, and has balance Y.

Alice wants to send amount Z to Bob.
Trent charges a fee F that goes to his Fred account.
Trent has a current limit of L.
Z+F <= X.
Z+F <= L.

Alice will send to Bob a "certified check".

1. Alice write a message M1, signed by her, that says:
  "Trent, please pay Z to Bob and receivee your fee F.
  My previous state with you was P.
  This message is written at time T0 (identified by main chain state M, side-chain state S).
  This message is valid until date T1. -- signed by Alice"

2. Alice sends message M1 to Trent for underwriting.

3. Trents verifies that M1 is correct, then signs a message M2 certifying M1:
  "I, Trent, certify M1 as valid and promise to make good on it in my next update to the main chain.
  Latest side-chain update was PS. My current TXID is T. My new limit for this cycle is NL."

4. Trent commits M2 to his side-chain.

5. Trent sends M2 to Alice (and Bob?)

6. Alice sends M2 to Bob.

7. Bob sends M2 to the Gossip Network, waits to check there is no double-spend by Trent.

8. Bob accepts the payment, and starts servicing Alice.

9. Trent updates the main chain and the payment is fully cleared.

### Enter Settlement Flow / Account Creation

Actors: Alice (sender), Trent (facilitator)

Preconditions:
* Alice has an account on the main chain with balance X+F+R.
* Trent has an account for Alice (voluntary or involuntary) with balance Y (or no account, and balance 0)
* Trent already has an open contract on the main

Postconditions:
* Alice has an account on the main chain with balance R.
* Alice has a voluntary account on Trent's side-chain with balance X+Y.
* Trent receives fee F from Alice.

1. Alice sends a signed account (re)activation request to Trent.
2. Trents signs the request, stores the confirmation, sends the confirmation to Alice.
3. Alice sends X+F to Trent's contract on the main chain
4. Alice waits for confirmation then sends deposit request to Trent
4. Trent writes deposit confirmation M1...
5. Trent commits M1 to side-chain
6. Trent sends M1 to Alice (OKish if fails)
7. Trent commits M1 to main chain

1 is enough for Alice to have her money back even if Trent fails.
If 5 didn't happen, Alice still can get her money back, which
the adversarial exit thing must take into account.

TODO: in a future version, use contracts on the side-chain,
with a contract that if Alice sends money to Trent2 on the main chain
(or in another side-chain, e.g. announcing some USD transactions),
then Trent will credit Alice on his side-chain.


### Cooperative Exit Flow

Actors: Alice (recipient), Trent (facilitator)

Preconditions:
* Alice has an account on Trent's side-chain with balance X.
* Alice has an account on the main-chain with balance F or more.
* Alice and Trent agree on an exit fee and cooperate.

Postconditions:
* Alice has an account on the main chain with balance X.
* Alice has no account on Trent's side-chain (balance 0).
* Alice pays up to F in court fees.
* Trent pays up to G in court fees.

1. Alice writes, signs and sends Trent an exit request.

2. Trent completes all transactions regarding Alice and signs a confirmation for the exit.

3. Trent updates the main chain.
   It is now common knowledge that there are no floating transactions for Alice,
   and that Alice will be able to take money away.

4. Alice waits for a challenge period during which Trent's chain may be proven fraudulent.

5. Past the challenge period (and past any lawsuits that may settle Alice's rights),
   Alice may take any remaining funds out of the account.
   Alice may also take out any funds deposited for which Trent refused to write a deposit slip.


### Adversarial Exit Flow

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
