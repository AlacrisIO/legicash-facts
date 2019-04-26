#!/usr/bin/env bash

PAYLOAD='
{ "address":      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "amount":       "0x16345785d8a0000"
, "request_guid": "528288f3-f5fb-49dd-9823-3b177023ef65"
}
'

echo "Deposit 1"
curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/deposit'


echo "Deposit 2"
curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/deposit'

echo "Deposit 3"
curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/deposit'

echo "Deposit 4"
curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/deposit'

