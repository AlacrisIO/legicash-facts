#!/bin/sh

# run the test net; make sure to run init.sh first

PORT=30303
RPCPORT=8080
DATADIR=geth-data

geth --identity "LegicashEthereumTestNet" --datadir $DATADIR \
      --nodiscover --maxpeers 0 --rpc --rpcapi "db,eth,net,web3,light,personal" --rpcport $RPCPORT --rpccorsdomain "*" \
      --port $PORT --networkid 17 --nat "any" --ipcpath .ethereum/geth.ipc > $DATADIR/testnet.log &
