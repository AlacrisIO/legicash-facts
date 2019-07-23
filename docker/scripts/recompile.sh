#!/usr/bin/env bash
set -eu

APP_DIR=/var/www/app/legicash-facts
BUILD_DIR=/_build/default/src/
DOCKER_IMAGE=gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

echo "Resetting configuration" # Note: preserves the contract from docker
docker run \
  --rm \
  -i \
  -w $APP_DIR \
  -v $PWD:$APP_DIR \
  -v /tmp/legilogic:/tmp/legilogic \
  --network=docker_legicash-demo \
  -u 1100 \
  $DOCKER_IMAGE \
  /bin/bash -c "\
cp -af ${APP_DIR}/docker/config/. /tmp/legilogic/config/
"

echo "Recompile and test application"
rm -rf _build || :
docker run \
  --rm \
  -i \
  -w $APP_DIR \
  -v $PWD:$APP_DIR \
  -v /tmp/legilogic/config:$APP_DIR/config \
  --network=docker_legicash-demo \
  -u $(id -u) \
  $DOCKER_IMAGE \
  /bin/bash -c "make toplevel test_hello test all"
# NB: make test uses the server from below, so we cannot stop it

echo "Stop side chain manager and client"
docker exec alacris-side-chain-manager supervisorctl stop side-chain-server
docker exec alacris-client supervisorctl stop alacris-client

echo "Copying new code to containers"
docker cp ${PWD}$BUILD_DIR/alacris_lib/side_chain_server.exe alacris-client:$APP_DIR/_bin/side_chain_server.exe
docker cp ${PWD}$BUILD_DIR/alacris_client/side_chain_client.exe alacris-client:$APP_DIR/_bin/side_chain_client.exe

echo "Cleaning side chain manager and client state"
docker run \
  --rm \
  -i \
  -w $APP_DIR \
  -v $PWD:$APP_DIR \
  -v /tmp/legilogic:/tmp/legilogic \
  --network=docker_legicash-demo \
  -u 1100 \
  $DOCKER_IMAGE \
  /bin/bash -c "\
rm -rf /tmp/legilogic/alacris_side_chain_manager /tmp/legilogic/alacris_client && \
mkdir -p /tmp/legilogic/alacris_side_chain_manager/_run/logs /tmp/legilogic/alacris_client/_run/logs
"

echo "Starting side chain manager and client"
docker exec alacris-side-chain-manager supervisorctl start side-chain-server
docker exec alacris-client supervisorctl start alacris-client
