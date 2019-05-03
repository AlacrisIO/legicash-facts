#!/bin/bash
# Transfer of 10 ethers

PAYLOAD='
{ "sender":       "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69"
, "recipient":    "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce"
, "amount":       "0xa688906bd8b00000"
, "request_guid": "528288f3-f5fb-49dd-9823-3a177023ef65"
}
'

curl \
  -H 'content-type: application/json; charset=utf-8' \
  --data "${PAYLOAD}" \
  'http://app.legi.cash:8081/api/payment'
