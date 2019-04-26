#!/usr/bin/env bash

PAYLOAD='
{ "address":      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "amount":       "0x2b5e3af16b1880000"
, "request_guid": "528288f3-f5fb-49dd-9823-3b177023ef65"
}
'

curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/deposit'

