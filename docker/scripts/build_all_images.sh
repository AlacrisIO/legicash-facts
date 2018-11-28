#!/usr/bin/env bash
# Build prerequisites image used for building side chain manager and client
echo "Building prerequisites images....please wait, this might take a while depending on your Internet connection speed"

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

echo "Building build prerequisite image"
run docker build -t gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 -f containers/build-prerequisites/Dockerfile .
echo "Building client runtime prerequisite image"
run docker build -t gcr.io/legicash-demo-1950/legicash-demo/alacris_client_container:v1 -f containers/alacris_client_container/Dockerfile .
echo "Building side chain manager runtime prerequisite image"
run docker build -t gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1 -f containers/alacris_side_chain_manager_container/Dockerfile .
echo "Cleanup of state and log directories"
run rm -rf /tmp/legilogs
echo "Creating new log and state directories"
run mkdir -p /tmp/legilogs/{alacris_client_db,alacris_server_db,ethereum_prefunder_db,_ethereum}
echo "Seting permissions"
run chmod -R a+rwX /tmp/legilogs
echo "Building application images"
run docker-compose build