#!/bin/sh -eu

HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of legicash-facts
mkdir -p _run
cd _run

# run the Ethereum test net

PORT=30303
RPCPORT=8545
DATADIR=geth-data

# clean out existing data dir
if [ -d $DATADIR ]; then
    rm -rf $DATADIR
fi

mkdir $DATADIR

geth --dev --identity "LegicashEthereumTestNet" --datadir $DATADIR \
     --nodiscover --maxpeers 0 --rpc --rpcapi "db,eth,net,debug,web3,light,personal" --rpcport $RPCPORT --rpccorsdomain "*" \
     --port $PORT --networkid 17 --nat "any" --ipcpath .ethereum/geth.ipc > $DATADIR/testnet.log 2>&1  &
