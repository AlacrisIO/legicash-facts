# Endpoints API

Types
-----

The type "hex-string" is a string beginning with "0x", followed by hex digits.

The type "address" is a hex-string with 20 hex-digit pairs.

The type "request-guid" is a
[UUIDv4](https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)).

The type "requested-at" is a hex-encoded UNIX timestamp.

The type "side\_chain\_account" is a JSON record of the form:

```
{ "balance":  hex-string
, "revision": hex-string
}
```

The type main\_chain\_account is a JSON record of the form:

```
{ "address": address
, "balance": hex-string
, "revision: hex-string
}
```

The type "hash" is a hex-string with 32 hex-digit pairs.

The type "main\_chain\_confirmation" is a JSON record of the form:

```
{ "transaction_hash":  hash
, "transaction_index": int
, "block_number":      int
, "block_hash":        hash
}
```

The type user\_status is a JSON record of the form:

```
{ "side_chain_account": side_chain_account
, "main_chain_account": main_chain_account
}
```

The type "proof" is a JSON record of the form:

```
{ "key":               hex-string
, "transaction_index": hash
, "value":             hash
, "steps":             step list
}
```

Where a "step" is a JSON record of one of the following forms:

```
{ "left": hash }

or

{ "right": hash }

or

{ "bits":   hex-string
, "length": number
}
```

Deposit
-------

POST /api/deposit

Content-Type: application/json

The body is a JSON record of the form:

```
{ "address":      address
, "amount":       hex-string
, "request_guid": request-guid
}
```

The result is a JSON record of the form:

```
{ "result": { "thread": nn
            , "request_guid": request-guid
            , "requested_at": requested-at
            }}
```

Where "nn" is an integer. The thread with the given id transfers funds from the
main chain to the side chain.

Withdrawal
----------

POST /api/withdrawal

Content-Type: application/json

The body is a JSON record of the form:

```
{ "address":      address
, "amount":       hex-string
, "request_guid": request-guid
}
```

The result is a JSON record of the form:

```
{ "result": { "thread": nn
            , "request_guid": request-guid
            , "requested_at": requested-at
            }}
```

Where "nn" is an integer. The thread with the given id transfers funds from the
side chain to the main chain.

Error message:

```
{ "error": "Insufficient balance to withdraw specified amount" }
```

Payment
-------

POST /api/payment

Content-Type: application/json

The body is a JSON record of the form:

```
{ "sender":       address
, "recipient":    address
, "amount":       hex-string
, "request_guid": request-guid
}
```

The result is a JSON record of the form:

```
{ "result": { "thread": nn
            , "request_guid": request-guid
            , "requested_at": requested-at
            }}
```

Balance
-------

POST /api/balance

Content-Type: application/json

The body is a JSON record of the form:

```
{ "address": address }
```

The result is a JSON record of type "side\_chain\_account".

Balances
--------

GET /api/balances

The result is a JSON list of "side\_chain\_account" records, sorted by user
name.

Status
------

POST /api/status

Content-Type: application/json

The body is a JSON record of the form:

```
{ "address": address }
```

The result is a JSON record of type "user\_status".

Transaction rate
----------------

GET /api/tps

The result is a JSON record of the form:

```
{ "transaction_rate": number
, "time":             string
}
```

Indicating the transactions per second in the minute preceding
the request, and the time when the request was received. The
time format is YYYY:MM:DD HH:MM:SS.

Proofs
------

GET /api/proof?tx-revision=nn, where "nn" is an integer

If there's a valid Merkle proof for the transaction with "tx-revision", a JSON
record of type "proof" is returned. The "trie" field should published on the
main chain.  Otherwise, a JSON record with an "error" field is returned.

Note that "key" field of the proof is a hex-string, while this endpoint below
takes an ordinary integer in the "tx-revision" parameter. Those two values
agree on their numeric value.

Recent transactions
-------------------

POST /api/recent_transactions

or

POST /api/recent_transactions?limit=nn, where "nn" is an integer

Content-Type: application/json

The body is a JSON record of the form:

```
{ "address": address }
```

The result is a JSON list of transactions. If the "limit" parameter is given,
the list is limited to that number of the most recent transactions requested by
the user with the given address; otherwise, the list is all the transactions by
that user.

The transactions are deposits, withdrawals, or payments, along with their
details.

Each element of the list is an object. Here is the typescript definition for
what's currently used from these objects, on the front end:

```typescript
type transaction_string = "Deposit" | "Withdrawal" | "Payment"
type hex_string = string

interface IDeposit {
    deposit_amount: number,
    deposit_fee: number,
    main_chain_deposit: { tx_header: { sender: hex_string } }
}

/* tslint:disable:no-empty-interface */
interface IPayment { /* empty */ }

interface IWithdrawal { /* empty */ }

interface IResponse {
    tx_header: {},
    tx_request: ["UserTransaction", {
        payload: {
            operation: [transaction_string, IDeposit | IPayment | IWithdrawal]
        }
    }]
}
```

These are taken from `src/sagas/recent_transactions.tsx` in
`legicash-demo-frontend`.

However, the canonical definitions are in `src/alacris_lib/side_chain.mli`.
The actual objects being sent back are `TransactionRequest`s, of subtype
`UserTransaction`.


Deposit/withdrawal threads
--------------------------

GET /api/thread?id=nn, where "nn" is an integer

If the thread is still running, the result is a JSON record:

```
{ "result": "The operation is pending" }
```

If the thread has completed for a deposit or withdrawal, the result is a JSON
record:

```
{ "user_account_state":      side_chain_account
, "side_chain_tx_revision":  hex-string
, "main_chain_confirmation": main_chain_confirmation
}
```

Where "side\_chain\_tx\_revision" is a sequence number for the request
confirmed by the side chain operator, and "main\_chain\_confirmation" has the
details of the transaction in a block on the main chain.

Validation of proofs
--------------------

For the validation of a [proof](#proofs) to be complete, the serialization of
the transaction (leaf value) must be hashed for comparison to the proof leaf.
For now, we're skipping that on the front end.

The hash for each `step` in has to be computed from the serialization of that
step. Those serializations are defined in `merkle_trie.ml`, in the module
`TrieSynthMerkle`. They are a concatenation of big-endian representations of
unsigned integer. The strings returned by the `proof` endpoint are big-endian.

The following details pertain to commit 4f49fa6, "Fix withdrawal."

The trie from which the proofs are constructed is a `MerkleTrie (Revision)
(Confirmation)`, defined as `ConfirmationMap` in `side_chain.ml`. `Revision`
is defined in `db.ml` as a `UInt64`. `Confirmation` is defined in
`side_chain.ml`

Each component of the serialization begins with a `UInt16`, which is a tag
defined in `tag.ml`.

### Leaf nodes

  - Initial tag is `Tag.leaf = 0x81`.
    Note that currently, we are not checking this, so these details could be
    wrong, but it seems that the marshaling for the leaves is done in
    `Side_chain.Confirmation.marshaling`.

### Branch nodes

  - Initial tag is `Tag.branch = 0x82`
  - Height is a `UInt16`. (`UInt16int.marshal buffer height;`).
  - Left is a `Digest`, which is a `Uint256` defined in `db.ml`
    (`module Digest = DBInt(Crypto.Digest)`)
  - Right is another 256-bit digest.

  These nodes correspond to `left` and `right` steps. For a `right` step, the
  hash supplied in the step goes on the right, and the partial hash computed so
  far goes on the left. Vice versa for `left` steps.

### Skip nodes

  - Initial tag is `Tag.skip = 0x83`.
  - Height is UInt16
  - Length is UInt16
  - Key is a `Revision`, i.e. a UInt64
  - child is a `Digest`, i.e. a UInt256. This is the hash from the last step.

  These nodes correspond to `skip` steps. The partial hash computed so far is
  the child digest.
