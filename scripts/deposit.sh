#!/bin/bash
# Trivial script for Alice to deposit 256
#     "amount": "0x100"
# Trivial script for Alice to deposit 50 ethers
#     "amount": "0x2b5e3af16b1880000"


curl -s -X POST -H 'Content-Type: application/json' -i http://app.legi.cash:8081/api/deposit --data '{
     "address": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
#     "amount": "0x2b5e3af16b1880000"
}'
