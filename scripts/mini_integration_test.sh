#!/bin/sh

# NB: If this gets any more complicated, make an OCaml script out of it.

HERE=$(dirname "$0")
cd "$HERE/../" # Change to toplevel directory of legicash-facts
: ${ALACRIS_HOME:=$(pwd)}
export ALACRIS_HOME

LOGDIR=${ALACRIS_HOME}/_run/logs
mkdir -p $LOGDIR

GETH_RUNDIR=$(pwd)/_ethereum
mkdir -p $GETH_RUNDIR

MAKE () {(set -x ; make "$@")}
RESET () { MAKE reset; }

trap EXIT RESET

set -eu

RESET

MAKE run_ethereum_net

# NB: ethereum_prefunder will do the retrying so we don't have to sleep
MAKE fund_accounts

sleep 2
MAKE run_side_chain_server &

sleep 2
MAKE run_side_chain_client &

sleep 2
MAKE nginx &

sleep 2
MAKE test_side_chain_client
result=$?

if [ $result = 0 ] ; then
   echo "mini_integration_test: SUCCESS!" ; exit 0
else
   echo "mini_integration_test: FAILURE!" ; exit $result
fi
