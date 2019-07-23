#!/usr/bin/env bash

# Identify where to run things from where this script is.
HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of git repository
TOPDIR=$(pwd) # Top directory for the git repository

APP_DIR=/var/www/app/legicash-facts
DOCKER_IMAGE=gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

echo "Resetting configuration"
docker run \
  --rm \
  -i \
  -w $APP_DIR \
  -v $PWD:$APP_DIR \
  -v /tmp:/tmp \
  --network=docker_legicash-demo \
  -u 0 \
  $DOCKER_IMAGE \
  /bin/bash -ceu '\
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

echo "Cleanup of state and log directories"
[ -d /tmp/legilogic ] && run rm -rf /tmp/legilogic || true

echo "Creating new log and state directories"
run mkdir -p \
    /tmp/legilogic/alacris-private-ethereum-node/{_ethereum,_run/logs} \
    /tmp/legilogic/alacris_side_chain_manager/_run/{logs,side_chain_server,supervisord} \
    /tmp/legilogic/alacris_client/_run/{alacris_client,logs,nginx,supervisord} \
    /tmp/legilogic/config

echo "Copying configuration"
run cp -a /var/www/app/legicash-facts/docker/config/. /tmp/legilogic/config/

echo "Setting permissions"
run chown -R appuser:appuser /tmp/legilogic
'
