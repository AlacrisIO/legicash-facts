# Plan for our demo

## Flows to get right

We identified twelve flows in Legicash FaCTS. We can stub most of them.

* create facilitator (stub?)
* close facilitator (stub)
* update facilitator sidechain state + pay penalties (stub?)
* open account (stub?)
* close account (need to do the adversarial kind)
* send payment (need to do)
* settle payment between side chains (need to do)
* settle payment from main chain to side chain (need to do)
* settle payment from side chain to main chain (need to do)
* denounce facilitator (minor / major) (stub)
* mass transfer account (voluntary / involuntary) (stub)
* Trent closes facilitator

In the end, the three flows we really need to implement non-trivially for the demo are:

* payment
* settlement of main-chain vs side-chain swap (including account creation / deletion?)
* adversarial exit (with the simplest invariants only, both success / failure cases)

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

1. Alice sends X+F to Trent's contract on the main chain
2. Trent write M1...
3. Trent commits M1 to side-chain
4. Trent sends M1 to Alice (OKish if fails)
5. Trent commits M1 to main chain

1 is enough for Alice to have her money back even if Trent fails.
If 5 didn't happen, Alice still can get her money back, which
the adversarial exit thing must take into account.


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


### Adversarial Exit Flow

Actors: Bob (recipient), Trent (facilitator)

Preconditions:
* Bob has an account on Trent's side-chain with balance X.
* Bob has an account on the main-chain with balance F or more.

Postconditions:
* Bob has an account on the main chain with balance X.
* Bob has no account on Trent's side-chain (balance 0).
* Bob pays up to F in court fees.
* Trent pays up to G in court fees.

1. Bob sends claim to Trent's contract on the main chain with all details
 (including list of pending transactions). Details may be posted on court registry
 instead of included in extenso in court proceedings.
2. a. If details invalid or incomplete,
  Trent contests and gets Bob thrown out "come back with your file in order"
  b. Trent goes missing or invalid. See Facilitator invalidation story, then go back to 2c.
  c. Trent has to post his side of the story, with a side-chain state update.
3. Interactive proof without mutable state, in predictable, finite number of steps.
4. Sanction.


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


## Bibliography

[Alice and Bob](https://en.wikipedia.org/wiki/Alice_and_Bob)

https://wiki.xenproject.org/wiki/OCaml_Best_Practices_for_Developers
http://dpt.sourceforge.net/conventions.html
https://ocaml.org/learn/tutorials/guidelines.html
https://www.cs.cornell.edu/courses/cs3110/2011sp/Handouts/style.htm
https://caml.inria.fr/resources/doc/guides/guidelines.en.html
https://www.seas.upenn.edu/~cis341/current/programming_style.shtml
https://cs.brown.edu/courses/cs017/content/docs/ocaml-style.pdf

http://www.csc.villanova.edu/~japaridz/CL/#Subsection3a1

https://github.com/kframework/k-legacy/tree/master/k-distribution/tutorial/1_k/1_lambda/lesson_2

