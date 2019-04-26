#!/usr/bin/env bash

PAYLOAD='
{ "address":      "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "amount":       "0x4563918244f40000"
, "request_guid": "5c5891b2-a8fe-4020-b405-8c924d3e2a4c"
}
'

curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/withdrawal'
