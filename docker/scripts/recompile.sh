#!/usr/bin/env bash

APP_DIR=/var/www/app/legicash-facts
BUILD_DIR=/_build/default/src/
DOCKER_IMAGE=gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1

echo "Recompile step 1"
# remove build_dir
rm -rf _build
# recompile app
docker run \
  --rm \
  -i \
  -w $APP_DIR \
  -v $PWD:$APP_DIR \
  -v ${PWD}/docker/config:${APP_DIR}/config \
  --network=docker_legicash-demo \
  -u $(id -u) \
  $DOCKER_IMAGE \
  /bin/bash -c "make toplevel test_hello test"

echo "Recompile step 2"
# recompile app
docker run --rm -i -w $APP_DIR -v $PWD:$APP_DIR -u $(id -u) $DOCKER_IMAGE make

echo "Recompile step 3"
# stop side chain manager
docker exec alacris-side-chain-manager supervisorctl stop side-chain-server

echo "Recompile step 4"
# Copy new code to container
docker cp ${PWD}$BUILD_DIR/alacris_lib/side_chain_server.exe alacris-client:$APP_DIR/_run/side_chain_server.exe

# Clean side chain manager state
echo "Deleting side chain manager state"
docker exec alacris-side-chain-manager rm -rf $APP_DIR/_run/alacris_server_db > /dev/null 2>&1

#start side chain manager
docker exec alacris-side-chain-manager supervisorctl start side-chain-server

#stop alacris-client
docker exec alacris-client supervisorctl stop alacris-client

# copy new code to container
docker cp ${PWD}$BUILD_DIR/alacris_client/side_chain_client.exe alacris-client:$APP_DIR/_run/side_chain_client.exe

# Clean side chain manager state
echo "Deleting alacris client state"
docker exec alacris-client rm -rf $APP_DIR/_run/alacris_client_db > /dev/null 2>&1

# start alacris-client
docker exec alacris-client supervisorctl start alacris-client
