#!/bin/bash
# Trivial script for Alice to withdraw 256

curl -s -X POST -H 'Content-Type: application/json' -i http://localhost:8081/api/withdrawal --data '{
     "address": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
     "request_guid": "5c5891b2-a8fe-4020-b405-8c924d3e2a4c",x
     "amount": "0x100"
}'
