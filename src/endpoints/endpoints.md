# Endpoints API

Types
-----

The type "address" is a JSON string, beginning with "0x", followed by 20 hex-digit pairs.

The type "account" is a JSON record of the form:

     { "address" : address,
       "user_name" : string,
       "balance" : integer
     }

The type "hash" is a JSON string, beginning with "0x", followed by 32 hex-digit pairs.

The type "main\_chain\_confirmation" is a JSON record of the form:

     { "transaction_hash" : hash,
       "transaction_index" : int,
	   "block_number" : int,
	   "block_hash" : hash
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
    "amount_transferred" : integer
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

Deposit/withdrawal threads
--------------------------

  URL: api/thread?id=nn, where "nn" is an integer

  GET

  If the thread is still running, the result is a JSON record:

    { "result" : "The operation is pending" }

  If the thread has completed for a deposit or withdrawal, the result is a JSON record:

    { "user_account_state" : account_state,
      "main_chain_confirmation" : main_chain_confirmation
    }

  where the transaction hash identifies the transaction on the main chain.
