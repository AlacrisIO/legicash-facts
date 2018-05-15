#!/bin/sh

# script to initialize the test network

DATADIR=$(pwd)

geth --datadir $DATADIR init genesis.json
