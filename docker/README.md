
## Table of Contents
* [About](#about)
* [Quick start](#quick-start)
* [Directory structure](#directory-structure)
* [Legicash containers](#list-of-containers)
   * [build-prerequisites](#build-prerequisites)
   * [alacris_side_chain_manager_container](#alacris_side_chain_manager_container)
   * [alacris_private_ethereum_node_node](#alacris_private_ethereum_node_node)
   * [alacris_client_container](#alacris_client_container) 
   * [alacris_side_chain_manager](#alacris_side_chain_manager)
   * [alacris_client](#alacris_client)
   * [stress_tester](#stress_tester)
* [Logs](#logs)

## About
This directory contains Legicash Docker configuration used for building images and running containers/services needed for legicash system to work in local Docker environment in reliable and repeatable way.

[Docker](https://docs.docker.com/) Images are created with Dockerfiles

[docker-compose](https://docs.docker.com/compose/overview/) is used to describe the desired state of Legicash services, thus implementing Infrastructure as Code (IaC) approach.

## Quick start
**Prerequisites:** 
  - Install and configure the most recent version of the [Cloud SDK](https://cloud.google.com/sdk/docs/), which includes the gcloud command-line tool (use `us-central-1a` region and `legicash-demo-1950` project)
  - Install [Docker](https://docs.docker.com/install/) minimum required version `17.12.0-ce`
  - Install [docker-compose](https://docs.docker.com/compose/install/) minimum required version `1.16.1`
  - Get [access to the registries which](https://cloud.google.com/container-registry/docs/access-control) you will be pushing to and pulling from
  - Configure Docker to use `gcloud` as a credential helper, or are use another [authentication method](https://cloud.google.com/container-registry/docs/advanced-authentication). To use `gcloud` as the crediential helper, run the command:
 ```bash
    gcloud auth configure-docker
``` 

#### Build all images
NOTE: This takes a lot of time to build everything from scratch. Please use `docker-pull` and `docker-build` targets instead
```bash
$ make docker-build-all
```

#### Pull build and runtime prerequisites images:

```bash
$  make docker-pull
```
Check **docker-compose.yml** file for detailed service configuration.

#### Build app images

```bash
$ make docker-build
```

#### List available containers
To list all containers run: 
```bash
$ make docker-list
```
```
alacris_private_ethereum_node
alacris_side_chain_manager
alacris_client
stress_tester

```

#### Cleaning states
Before each run we need to clean up states with command:
```bash
$ make docker-reset-state
```

#### Run all containers
To run all containers at once run the command: 
```bash
$ make docker-up
```

To run all containers at once in detached mode run the command: 
```bash
$ make docker-start
```

For `frontend` app to be able to connect to client add hostname to `/etc/hosts` file with the command:
```bash
echo  "127.0.0.1       app.legi.cash" | sudo tee --append /etc/hosts > /dev/null
```
#### Important URLs
Frontend application: http://app.legi.cash:8800

Ethereum block exporer: http://app.legi.cash:8000

#### Recompile applications
If you want to apply your code changes while all containers are running runt the command:
```bash
make docker-recompile
```
Output:
```bash
$ make docker-recompile
Recompiling Alacris apps
side-chain-server: stopped
Deleting side chain manager state
side-chain-server: started
alacris-client: stopped
Deleting alacris client state
alacris-client: started

```
What happens here is that new code gets built and application binaries get replaced inside of the container. Something like "deploy to local environment" 


## Directory structure
```
docker
├── config
│   ├── demo-keys-big.json
│   ├── demo-keys-small.json
│   ├── demo-keys-small.json.old
│   ├── ethereum_config.json
│   ├── operator_keys.json
│   ├── side_chain_client_config.json
│   └── side_chain_server_config.json
├── containers
│   ├── alacris_client
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   ├── nginx
│   │       │   │   ├── nginx.conf
│   │       │   │   ├── scgi_params
│   │       │   │   └── sites
│   │       │   │       ├── frontend.conf
│   │       │   │       └── scgi.conf
│   │       │   ├── side_chain_client_config.json
│   │       │   └── supervisord.conf
│   │       ├── private_key
│   │       └── scripts
│   │           ├── run-nginx.sh
│   │           └── run-scgi.sh
│   ├── alacris_client_container
│   │   └── Dockerfile
│   ├── alacris_private_ethereum_node
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           ├── run-block-explorer.sh
│   │           └── run-ethereum.sh
│   ├── alacris_side_chain_manager
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   ├── side_chain_server_config.json
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           ├── prefund.sh
│   │           └── run-side-chain-server.sh
│   ├── alacris_side_chain_manager_container
│   │   └── Dockerfile
│   ├── build-prerequisites
│   │   ├── Dockerfile
│   │   └── files
│   └── stress_tester
│       ├── Dockerfile
│       └── files
│           ├── run-locust.sh
│           └── test-locust.py
├── docker-compose.yml
├── README.md
└── scripts
    ├── build_all_images.sh
    ├── pull_images.sh
    ├── recompile.sh
    └── reset_state.sh
```

## Containers
#### build-prerequisites 
This image is used as a build image for legicash-facts and base image for alacris_client container. Built in previous step

#### alacris_side_chain_manager_container
Legicash-facts side chain server run prerequisite. Container used for running the app

#### alacris_client_container
Legicash-facts side chain client, nginx and scgi run prerequisite. Container used for running the app


#### alacris_private_ethereum_node
To build alacris private ethereum node run the command:
```bash
$ make docker-build c=alacris_private_ethereum_node
```

#### alacris_side_chain_manager
To build alacris side chain manager run the command
```bash
$ make docker-build c=alacris_side_chain_manager
```
To start alacris side chain manager run the command
```bash
$ make docker-start c=alacris_side_chain_manager
```

## alacris_client
To build alacris client run the command:
```bash
$ make docker-build c=alacris_client
```
To start alacris client run the command:
```bash
$ make docker-start c=alacris_client
```

#### stress_tester
stress_tester container running in standalone mode. Single node “cluster”, where the instance plays both the role of the master and the worker. Distribuded mode will be added soon.
```bash
$ docker-compose build stress_tester
```
## Connect into containers
As application user
```bash
$ docker exec -ti containername bash
```
As root user
```bash
$ docker exec -ti -u 0 containername bash
```

## Logs
Application logs that are not in `stdout` of running containers are for now mounted in `/tmp/legilogs` on local machines until we agree on final destination
