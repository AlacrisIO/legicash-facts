#!/bin/bash
# Trivial script for Alice to deposit 256

curl -s -X POST -H 'Content-Type: application/json' -i http://localhost:8081/api/deposit --data '{
     "address": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
     "amount": "0x100"
}'
