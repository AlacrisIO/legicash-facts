
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
   * [alacris_frontend](#alacris_frontend)
   * [stress_tester](#stress_tester)
* [Logs](#logs)

## About
This directory contains Docker configuration used for building images and
running containers/services needed for the `legicash-facts` system to work in
local Docker environments in a reliable and repeatable way.

[Docker](https://docs.docker.com/) images are created with `Dockerfile`s.

[docker-compose](https://docs.docker.com/compose/overview/) is used to describe
the desired state of Legicash services, thus implementing an Infrastructure as
Code (IaC) approach.

## Quick start
**Prerequisites:**
  - Install [Docker](https://docs.docker.com/install/) minimum required version
    `17.12.0-ce`
  - Install [docker-compose](https://docs.docker.com/compose/install/) minimum
    required version `1.16.1`

#### Pull build and runtime prerequisites images:

```bash
$ make docker-pull
```
Check **docker-compose.yml** file for detailed service configuration.

#### Build app images

```bash
$ make docker-build
```

#### Build all images
NOTE: This takes a lot of time to build everything from scratch. Please use
`docker-pull` and `docker-build` targets instead.
```bash
$ make docker-build-all
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
alacris_frontend

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

For `frontend` app to be able to connect to client add hostname to `/etc/hosts`
file with the command:
```bash
echo  "127.0.0.1       app.legi.cash" | sudo tee --append /etc/hosts > /dev/null
```
#### Important URLs
Frontend application: http://app.legi.cash:8800

Ethereum block explorer: http://app.legi.cash:8000

#### Recompile applications
If you want to apply your code changes while all containers are running
execute:
```bash
make docker-recompile
```
What happens here is that new code gets built, tests get run and application binaries get
replaced inside of the container. Something like "deploy to local environment"


## Directory structure
```
docker
├── config
│   ├── demo-keys-big.json
│   ├── demo-keys-small.json
│   ├── demo-keys-small.json_V1
│   ├── demo-keys-small.json_V2
│   ├── ethereum_config.json
│   ├── mkb_config.json
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
│   │       │   │       └── scgi.conf
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           ├── run-nginx.sh
│   │           └── run-scgi.sh
│   ├── alacris_client_container
│   │   └── Dockerfile
│   ├── alacris_frontend
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   └── nginx
│   │       │       ├── nginx.conf
│   │       │       └── sites
│   │       │           └── frontend.conf
│   │       └── private_key
│   ├── alacris_private_ethereum_node
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           └── run-ethereum.sh
│   ├── alacris_side_chain_manager
│   │   ├── Dockerfile
│   │   └── files
│   │       ├── conf
│   │       │   └── supervisord.conf
│   │       └── scripts
│   │           ├── prefund.sh
│   │           └── run-side-chain-server.sh
│   ├── alacris_side_chain_manager_container
│   │   └── Dockerfile
│   ├── build-prerequisites
│   │   └── Dockerfile
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
This image is used as a build image for legicash-facts and base image for
alacris_client container. Built in previous step.

#### alacris_side_chain_manager_container
Legicash-facts side chain server run prerequisite. Container used for running
the app.

#### alacris_client_container
Legicash-facts side chain client, nginx and scgi run prerequisite. Container
used for running the app.

#### alacris_private_ethereum_node
To build alacris private ethereum node run the command:
```bash
$ make docker-build-geth
```

#### alacris_side_chain_manager
To build alacris side chain manager run the command:
```bash
$ make docker-build c=alacris_side_chain_manager
```
To start alacris side chain manager run the command:
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

## alacris_frontend
Alacris frontend image accepts 2 parameters. ENVIRONMENT to build for and
FRONTEND_BRANCH. If none is set on CLI defaults are used. To build alacris
frontend run the command:
```bash
$ make docker-build-frontend ENVIRONMENT=dev FRONTEND_BRANCH=my-feature-branch
```
To start alacris client run the command:
```bash
$ make docker-start c=alacris_frontend
```

#### stress_tester
stress_tester container running in standalone mode. Single node “cluster”,
where the instance plays both the role of the master and the worker.
Distributed mode will be added soon.
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
Application logs that are not in `stdout` of running containers are for now
mounted in `/tmp/legilogs` on local machines until we agree on a final
destination.

## Shell functions
```
dockit_tmp () {
  docker run --rm -it -v `pwd`:/src -p 8081:8081 registry.gitlab.com/legicash/legicash-facts:build-env
}

dockit () {
  docker run --name mystate -it -v /home/fare/src:/src -p 8081:8081 registry.gitlab.com/legicash/legicash-facts:build-env
}
#docker start mystate
#docker attach mystate
#docker exec -it mystate /bin/bash
#docker rm mystate
# --cpuset-cpu=0-3
# for i in lwt lwt_ppx lwt_react ; do opam pin add $i /src/ocaml/lwt ; done
```
