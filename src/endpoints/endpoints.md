# Endpoints API

Types
-----

The type "hex-string" is a string beginning with "0x", followed by hex digits.

The type "address" is a hex-string with 20 hex-digit pairs.

The type "account" is a JSON record of the form:

     { "address" : address,
       "user_name" : string,
       "balance" : integer
     }

The type "hash" is a hex-string with 32 hex-digit pairs.

The type "main\_chain\_confirmation" is a JSON record of the form:

     { "transaction_hash" : hash,
       "transaction_index" : int,
	   "block_number" : int,
	   "block_hash" : hash
     }

The type "proof" is a JSON record of the form:

     { "key" : hex-string
       "transaction_index" : hash,
	   "value" : hash,
	   "steps" : step list
     }

where a "step" is a JSON record of one of the following forms:

     { "left" : hash }, or

     { "right" : hash }, or

     { "bits" : hex-string,
	   "length" : int
	 }

Deposit
-------

  URL: api/deposit

  POST / Content-Type: application/json

  The body is a JSON record of the form:

    { "address" : address,
      "amount" : integer
    }

  The result is a JSON record of the form

    { "result":"api/thread?id=nn" }

  where "nn" is an integer. The thread with the given id transfers funds from the main chain to the side chain.

Withdrawal
----------

  URL: api/withdrawal

  POST / Content-Type: application/json

  The body is a JSON record of the form:

    { "address" : address,
      "amount" : integer
    }

  The result is a JSON record of the form

    { "result":"api/thread?id=nn" }

  where "nn" is an integer. The thread with the given id transfers funds from the side chain to the main chain.

  Error message:

    { "error":"Insufficient balance to withdraw specified amount" }

Payment
-------

  URL: api/payment

  POST / Content-Type: application/json

  The body is a JSON record of the form:

    { "sender" : address,
      "recipient" : address,
      "amount" : integer
    }

  The result is a JSON record of the form:

  { "sender_account" : account,
    "recipient_account" : account,
    "amount_transferred" : integer,
    "side_chain_tx_revision" : integer
  }

  reflecting the accounts after payment has been made.

Balance
-------

  URL: api/balance

  POST / Content-Type: application/json

  The body is a JSON record of the form:

    { "address" : address }

  The result is a JSON record of type "account".

Balances
--------

  URL: api/balances

  GET

  The result is a JSON list of "account" records, sorted by user name.

Proofs
------

  URL: api/proof?tx-revision=nn, where "nn" is an integer
  
  GET
  
  If there's a valid Merkle proof for the transaction with "tx-revision", a JSON record
  of type "proof" is returned. The "trie" field should published on the main chain.
  Otherwise, a JSON record with an "error" field is returned.

  Note that "key" field of the proof is a hex-string, while this endpoint below takes an ordinary 
  integer in the "tx-revision" parameter. Those two values agree on their numeric value.

Deposit/withdrawal threads
--------------------------

  URL: api/thread?id=nn, where "nn" is an integer

  GET

  If the thread is still running, the result is a JSON record:

    { "result" : "The operation is pending" }

  If the thread has completed for a deposit or withdrawal, the result is a JSON record:

    { "user_account_state" : account_state,
      "side_chain_tx_revision" : integer,
      "main_chain_confirmation" : main_chain_confirmation
    }

  where "side\_chain\_tx\_revision" is a sequence number for the request confirmed by the side chain
  facilitator, and "main\_chain\_confirmation" has the details of the transaction in a block on the 
  main chain.
