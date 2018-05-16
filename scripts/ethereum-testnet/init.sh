#!/bin/sh

# script to initialize the test network

DATADIR=$HOME/geth-data

geth --datadir $DATADIR init genesis.json
