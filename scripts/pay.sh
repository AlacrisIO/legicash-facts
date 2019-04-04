#!/bin/bash
# Trivial script for Alice to pay 256 tokens to Bob

curl -s -X POST -H 'Content-Type: application/json' -i http://app.legi.cash:8081/api/payment --data '{
     "sender": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
     "recipient": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69",
     "amount": "0x100"
}'
