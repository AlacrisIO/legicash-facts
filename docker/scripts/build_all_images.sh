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

if [ -n "${NO_DOCKER_CACHE}" ] ; then
  NO_DOCKER_CACHE="--no-cache"
fi

echo "Building build prerequisite image"
run docker build \
  ${NO_DOCKER_CACHE} \
  -t gcr.io/legicash-demo-1950/legicash-demo/build-prerequisites:v1 \
  -f docker/containers/build-prerequisites/Dockerfile .

echo "Building client runtime prerequisite image"
run docker build \
  ${NO_DOCKER_CACHE} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_client_container:v1 \
  -f docker/containers/alacris_client_container/Dockerfile .

echo "Building side chain manager runtime prerequisite image"
run docker build \
  ${NO_DOCKER_CACHE} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_side_chain_manager_container:v1 \
  -f docker/containers/alacris_side_chain_manager_container/Dockerfile .

echo "Building Ethereum test network image"
run docker build \
  ${NO_DOCKER_CACHE} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_private_ethereum_node:v1 \
  -f docker/containers/alacris_private_ethereum_node/Dockerfile .

echo "Building frontend ${FRONTEND_BRANCH:-master} branch for ${ENVIRONMENT:-dev} env"
run docker build \
  ${NO_DOCKER_CACHE} \
  -t gcr.io/legicash-demo-1950/legicash-demo/alacris_frontend:v1 \
  --build-arg FRONTEND_BRANCH="${FRONTEND_BRANCH:-master}" --build-arg ENVIRONMENT="${ENVIRONMENT:-dev}" \
  -f docker/containers/alacris_frontend/Dockerfile .

echo "Cleanup of state and log directories"
[ -d /tmp/legilogs ] && run rm -rf /tmp/legilogs || true

echo "Creating new log and state directories"
run mkdir -p /tmp/legilogs/{alacris_client_db,alacris_server_db,ethereum_prefunder_db,_ethereum}

echo "Setting permissions"
run chmod -R a+rwX /tmp/legilogs

echo "Building application images"
run docker-compose -f docker/docker-compose.yml build ${NO_DOCKER_CACHE}
