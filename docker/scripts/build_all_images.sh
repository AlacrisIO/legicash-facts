#!/usr/bin/env bash
# Build prerequisites image used for building side chain manager and client
echo "Building prerequisites images... please wait, this might take a while depending on your Internet connection speed"

# Identify where to run things from where this script is.
HERE=$(dirname "$0")
cd "$HERE/../../" # Change to toplevel directory of git repository
TOPDIR=$(pwd) # Top directory for the git repository

set -eu

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

if [ -n "${NO_DOCKER_CACHE-}" ] ; then
  NO_DOCKER_CACHE="--no-cache"
fi

run ./docker/scripts/source.sh

echo "Building build prerequisite image"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 \
  -f docker/containers/build-prerequisites/Dockerfile .

echo "Building build image"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/build:v1 \
  -f docker/containers/build/Dockerfile .
rm -f .source.tar.gz

echo "Building client runtime container image"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_client_container:v1 \
  -f docker/containers/alacris_client_container/Dockerfile .

echo "Building side chain manager runtime container image"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1 \
  -f docker/containers/alacris_side_chain_manager_container/Dockerfile .

echo "Building Ethereum test network image"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_private_ethereum_node:v1 \
  -f docker/containers/alacris_private_ethereum_node/Dockerfile .

echo "Building frontend ${FRONTEND_BRANCH:-master} branch for ${ENVIRONMENT:-dev} env"
run docker build \
  ${NO_DOCKER_CACHE-} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_frontend:v1 \
  --build-arg FRONTEND_BRANCH="${FRONTEND_BRANCH:-master}" --build-arg ENVIRONMENT="${ENVIRONMENT:-dev}" \
  -f docker/containers/alacris_frontend/Dockerfile .

${TOPDIR}/docker/scripts/reset_state.sh

echo "Building application images"
run docker-compose -f docker/docker-compose.yml build ${NO_DOCKER_CACHE-}
