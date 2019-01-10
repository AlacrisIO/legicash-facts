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

  5. Forced transaction on main chain (necessary for repudiability, atomic transactions and layer 3 contracts)
  6. Atomic Cross-chain Settlement Flow (necessary to scale multi-side-chains)
  7. Failed or Closed Facilitator Recovery (necessary for security that scales and is fair)
  8. Mass Transfers(?) (necessary for security that scales?)
  9. Forced transaction on side chain (necessary for making repudiability scale)
  10. Create Facilitator (very incomplete version for M1)
  11. Update facilitator sidechain state + pay penalties (partial for M1)
  12. Close facilitator (necessary for facilitators to get their money back)
  13. All fraud proofs (not just a few for demo)

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

Steps:
* Alice sends X+F to Trent's contract on the main chain.
* Alice waits for confirmation then sends deposit request to Trent.
* Trent includes the transaction in his side-chain
* Trent commits his side-chain (locally; TODO M2: remotely)
* Trent signs a commitment to this and other transactions
* Trent sends the commitment to Alice
* Trent posts a state update to the main chain (TODO M1)

At every step, Alice is guaranteed to have her money back even if Trent fails,
though she may have to do force a transaction using another operator and/or on the main chain.

STATUS: mostly done, except that Trent needs to better check for the
confirmation to be correct (which is expensive and must be done asynchronously).

TODO: In a future version, also support atomic swap contracts between
the side-chain and not just the main chain but any arbitrary other blockchain,
whereby Trent, or any Bob on the side-chain, will accept payment on the other
blockchain in exchange for time-locked tokens on the side-chain.
Such atomic swaps allow for "deposits" converted from any asset, and may
in general be faster and cheaper than main chain deposits.
However, we still need regular main chain deposits as above for the sake
of adjusting the overall balance of the two-way peg.


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

Steps:
* Alice signs a transaction request for a payment to Bob, and sends it to Trent
* Trent includes the transaction in his side-chain
* Trent commits his side-chain (locally; TODO M2: remotely)
* Trent signs a commitment to this and other transactions
* OPTIONAL: for expedited payments, Trent gets the commitment signed by two thirds of the MKB. (TODO M2)
* Trent sends the commitment to Alice (with MKB signature if applicable), who can gives it to Bob.
* Trent posts a state update to the main chain (TODO M1)

STATUS: mostly done, except for the
confirmation to be correct (which is expensive and must be done asynchronously).


### Withdraw Money

Actors: Alice (sender), Trent (facilitator)

Preconditions:
* Trent already has an open contract as a facilitator on the main chain.
* Alice has an account on Trent's side chain with balance X=Y+F+R.

Postconditions:
* Alice has an account on Trent's side chain with balance R.
* Trent's contract sent Y to Alice.
* Trent received fee F from Alice.

Steps:
* Alice signs a transaction request for withdrawal, and sends it to Trent
* Trent includes the transaction in his side-chain, which counts as a withdrawal ticket
* Trent commits his side-chain (locally; TODO M2: remotely)
* Trent signs a commitment to this and other transactions
* Trent sends the commitment to Alice
* Trent posts a state update to the main chain (TODO M1)
* Alice waits for the state update to be confirmed (TODO M1)
* Alice posts a claim for the withdrawal on the main chain, referring to the state update and the ticket
* Anyone can counter Alice's claim, during a challenge period.
* IF the claim is complex (NB: hopefully, no need for it), then longer lawsuits may ensue
  based on the counter-claims. If the claim is simple, the counter-claims can be checked in one step
  (TODO M1: do at least simple claims; hopefully manage to design the side-chain
  so that no complex claim is necesary, or else handle them in M2).
* Alice waits for the challenge period to be over, partakes in any complex lawsuit and wins them.
  (TODO M1: watch for claims)
* Alice posts a transaction to the main-chain that executes the now confirmed claim.


### Fraud Proof Flow

Alice and/or Trent should be watching the chain for bad claims and making counter-claims.

TODO M1: have a bad guy do invalid claims, have the good guy watch it and making a simple counter-claim.
Use gross manual approximations for the collateral computation (gas limit * gas price).

TODO M2: be sure to implement all cases for claims and counter-claims,
automatically generated from a description of the data types and constraints.
Automatically extract precise collateral computation.

TODO M3: add proper timing to prioritize the parties before to authorize third party litigation.
Add complex claims.


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

### Fallback for when the Court Registry Fails

If bad guys can win big by capturing the court registry, this will give them a big incentive to do so.
Is there a good way to prevent the bad guys from winning more money than the authorized floating amount
even if they capture the court registry?
At the very least, can we ensure that those people who trusted an honest facilitator
won't be defrauded if the facilitator remains honest in face of a captured court registry?

Ideas:

1. Some privileged party can raise an alarm, at which point we're back to Plasma-style games.
   Especially in a one-contract-per-facilitator setting, that could be the facilitator themselves.
   Of course, this only incentivizes the bad guys to capture that trusted party, first.
2. Anyone can post a large amount of money as a bond and claim there is a problem with the court registry.
   Optionally, the bond could be bought back by the facilitator, and an auction could occur between
   those claiming the court registry was captured and those claiming it wasn't.
   At that point, we're back to Plasma-style games, and a vote by the depositors who recover their money
   concludes who does or doesn't get their bond back.
   Now the bad guys have to capture a large share of the depositors' money before it is profitable
   to even try capturing the court registry. Of course, if the registry and the deposits are
   undercapitalized compared to the bad guys, they can "just" invest a lot funds to capture a smaller
   yet profitable amount of money.
3. Somehow, the data structures are so well-indexed that any invalid data can be pinpointed to the judge
   after a logarithmic amount of interactive explorations.
   Then, you still need to drop a large bond to accuse the court of impropriety,
   but you can prove your case in finite enough time, at which point there's an objective reason
   to drop into a fallback mode of Plasma-style exit games.
4. If there remains at least one honest registrar, he can serve as the fallback guy for peer-to-peer
   exchange of confirmations, from which honest people can identify which withdrawals to investigate;
   but the bad guys who captured the registry can create a very large amount of withheld transactions,
   and issue plenty of good ones to taunt the good guys into challenging them,
   before they start issuing bad ones, and the good guys may lose a lot of money challenging
   "actually good" withdrawals before they hit the bad ones.
5. The bad guys can play a game whereas they withhold blocks, but don't do anything bad yet.
   The good guys with a clue will can exit early, and the bad guys will let them,
   leaving the clueless victims without capital to defend them as the bad guys now do the actual attack.
6. If the facilitator is honest but the court registry claims he's not by withholding signatures,
   is there a way to survive in a one-contract-per-facilitator and/or ERC-20 world?
   Without allowing a dishonest facilitator to spuriously cause a bank run situation?

