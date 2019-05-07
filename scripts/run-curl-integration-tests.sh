#!/usr/bin/env bash

# TODO the `sleep` statements below are meant to allow enough time for async
# processing to complete but the durations we've chosen are totally arbitrary -
# consequently these will almost certainly break in the future. We also don't
# have a straightforward way to pluck thread IDs out of responses, so we can't
# easily poll against the status endpoint waiting for confirmation that next
# steps may proceed.
#
# Moreover, `bash` struggles with multiline arguments to functions so we're
# limited in how DRY we can make this script without resorting to odd shell
# hacks (hence the obvious repetition).
#
# We should replace this script altogether and use a language that's better
# suited to the task than `bash`.

DEPOSIT_10ETH_TO_BOB='
{ "address":      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "amount":       "0x8ac7230489e80000"
, "request_guid": "528288f3-f5fb-49dd-9823-3b177023ef65"
}
'

PAY_12ETH_FROM_BOB_TO_ALICE='
{ "sender":       "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "recipient":    "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce"
, "amount":       "0xa688906bd8b00000"
, "request_guid": "528288f3-f5fb-49dd-9823-3a177023ef65"
}
'

WITHDRAW_5ETH_TO_BOB='
{ "address":      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "amount":       "0x4563918244f40000"
, "request_guid": "5c5891b2-a8fe-4020-b405-8c924d3e2a4c"
}
'

function deposit-10eth-to-bob {
  echo "Depositing 10eth to Bob..."
  curl \
    -H 'content-type: application/json; charset=utf-8' \
    --data "${DEPOSIT_10ETH_TO_BOB}" \
    "http://app.legi.cash:8081/api/deposit"
  printf "\n\n"
}

function pay-12eth-from-bob-to-alice {
  echo "Paying 12eth from Bob to Alice..."
  curl \
    -H 'content-type: application/json; charset=utf-8' \
    --data "${PAY_12ETH_FROM_BOB_TO_ALICE}" \
    "http://app.legi.cash:8081/api/payment"
  printf "\n\n"
}

function withdraw-5eth-to-bob {
  echo "Withdrawing 5eth to Bob..."
  curl \
    -H 'content-type: application/json; charset=utf-8' \
    --data "${WITHDRAW_5ETH_TO_BOB}" \
    "http://app.legi.cash:8081/api/withdrawal"
  printf "\n\n"
}

function query-balances {
  echo "Querying balances endpoint..."
  curl \
    -H 'content-type: application/json; charset=utf-8' \
    "http://app.legi.cash:8081/api/balances"
  printf "\n\n"
}

deposit-10eth-to-bob
deposit-10eth-to-bob
deposit-10eth-to-bob
deposit-10eth-to-bob
echo "Awaiting deposit confirmations..."
sleep 10
echo

pay-12eth-from-bob-to-alice
echo "Awaiting payment confirmation..."
sleep 5
echo

withdraw-5eth-to-bob
withdraw-5eth-to-bob
echo "Awaiting withdrawal confirmations..."
sleep 30
echo

query-balances
