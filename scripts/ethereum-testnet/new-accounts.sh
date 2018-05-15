#!/bin/sh

# script to create accounts on the test network

DATADIR=$(pwd)
NUM_ACCOUNTS=5

echo "When prompted, enter the empty password"

for num in `seq 1 $NUM_ACCOUNTS`; do
    geth --datadir $DATADIR account new
done	   

echo "Now enter those addresses and their funding amounts in genesis.json"
echo "Then run init.sh"
