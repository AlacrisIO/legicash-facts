#!/bin/sh -eu

HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of legicash-facts

LOGDIR=$(pwd)/_run/logs
mkdir -p $LOGDIR

GETH_RUNDIR=$(pwd)/_ethereum
mkdir -p $GETH_RUNDIR

cd $GETH_RUNDIR

# run the Ethereum test net

PORT=30303
RPCPORT=8545
DATADIR=geth-data

# clean out existing data dir
if [ -d $DATADIR ]; then
    rm -rf $DATADIR
fi

mkdir $DATADIR

# kill any existing geth
killall geth > /dev/null 2>&1 || true

geth \
    --dev \
    --mine \
    --identity "LegicashEthereumDevNet" \
    --datadir $DATADIR \
    --nodiscover \
    --maxpeers 0 \
    --rpc --rpcapi "db,eth,net,debug,web3,light,personal" --rpcport $RPCPORT --rpccorsdomain "*" \
    --port $PORT \
    --networkid 17 \
    --nat "any" \
    --ipcpath .ethereum/geth.ipc \
    > $LOGDIR/testnet.log 2>&1  &
