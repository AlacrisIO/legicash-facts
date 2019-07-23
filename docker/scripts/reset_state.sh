#!/usr/bin/env bash

# Identify where to run things from where this script is.
HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of git repository
TOPDIR=$(pwd) # Top directory for the git repository

run() {
  $*
  if [ $? -ne 0 ]
  then
    echo "$* failed with exit code $?"
    return 1
  else
    return 0
  fi
}

# TODO: use docker run and/or sudo to chown everything to appuser (1100)

echo "Cleanup of state and log directories"
[ -d /tmp/legilogic ] && run rm -rf /tmp/legilogic || true

echo "Creating new log and state directories"
run mkdir -p \
    /tmp/legilogic/alacris-private-ethereum-node/{_ethereum,_run/logs} \
    /tmp/legilogic/{alacris_side_chain_manager,alacris_client}/_run/logs \
    /tmp/legilogic/config

echo "Copying configuration"
run rsync -a docker/config/ /tmp/legilogic/config/

# TODO: This is not safe on multiuser machines. We need a better solution.
echo "Setting permissions"
run chmod -R a+rwX /tmp/legilogic
