#!/bin/bash
# Trivial script to fund the Alice and Bob accounts

curl -s -X POST -H 'Content-Type: application/json' -i http://localhost:8081/api/deposit --data '{
    "address": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
    "amount": "0xFFFFF"
}'

curl -s -X POST -H 'Content-Type: application/json' -i http://localhost:8081/api/deposit --data '{
    "address": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69",
    "amount": "0xFFFFF"
}'
