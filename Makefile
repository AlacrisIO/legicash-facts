# Makefile for Alacris

# strategy:
# - use dune to build from OCaml sources
# - use make targets for other tasks more easily done from make

# make VERBOSE nonempty to see raw commands (or provide on command line)
ifndef VERBOSE
VERBOSE:=
endif

# use SHOW to inform user of commands
SHOW:=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE:=$(if $(VERBOSE),,@)

# Our applications recognize this as the top directory for the project, and look for files there
# at runtime, e.g. for configuration.
export ALACRIS_HOME:=$(shell pwd)

# Some tests (for legilogic_lib and legilogic_ethereum) are run before the application is set
# to "alacris" and so need this variable instead.
export APPLICATION_HOME:=$(ALACRIS_HOME)

BUILD_DIR:=_build/default

all: build_all

.PHONY: all build_all force \
	legilogic_lib legilogic_lib_test test_legilogic_lib \
	legilogic_ethereum ethereum_prefunder legilogic_ethereum_test test_legilogic_ethereum \
	contract alacris_lib side_chain_client_lib \
	side_chain_server side_chain_client side_chain_client_test \
	install uninstall \
	toplevel repl test_hello test \
	run_ethereum_net fund_accounts run_side_chain_server run_side_chain_client nginx \
	test_side_chain_client \
	stop_nginx clean reset wc

ML_SOURCES:=$(wildcard src/*.ml src/*.mli src/*/*.ml src/*/*.mli src/dune src/*/dune)


### All the things to build:

LEGILOGIC_LIB:=src/legilogic_lib/legilogic_lib.cmxs
legilogic_lib: $(BUILD_DIR)/$(LEGILOGIC_LIB)
$(BUILD_DIR)/$(LEGILOGIC_LIB): $(ML_SOURCES)
	$(SHOW) "Building Legilogic library"
	$(HIDE) dune build $(LEGILOGIC_LIB)

LEGILOGIC_LIB_TEST:=src/legilogic_lib/legilogic_lib_test.exe
legilogic_lib_test: $(BUILD_DIR)/$(LEGILOGIC_LIB_TEST)
$(BUILD_DIR)/$(LEGILOGIC_LIB_TEST): src/legilogic_lib/legilogic_lib_test.ml $(ML_SOURCES)
	$(SHOW) "Building test legilogic_lib executable"
	$(HIDE) dune build $(LEGILOGIC_LIB_TEST)

test_legilogic_lib : $(LEGILOGIC_LIB_TEST)
	$(SHOW) "Testing legilogic_lib"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(LEGILOGIC_LIB_TEST)

LEGILOGIC_ETHEREUM:=src/legilogic_ethereum/legilogic_ethereum.cmxs
legilogic_ethereum: $(BUILD_DIR)/$(LEGILOGIC_ETHEREUM)
$(BUILD_DIR)/$(LEGILOGIC_ETHEREUM): $(ML_SOURCES)
	$(SHOW) "Building Legilogic ethereum support"
	$(HIDE) dune build $(LEGILOGIC_ETHEREUM)

ETHEREUM_PREFUNDER:=src/legilogic_ethereum/ethereum_prefunder.exe
ethereum_prefunder: $(BUILD_DIR)/$(ETHEREUM_PREFUNDER)
$(BUILD_DIR)/$(ETHEREUM_PREFUNDER): $(ML_SOURCES)
	$(SHOW) "Building Ethereum prefunder executable"
	$(HIDE) dune build $(ETHEREUM_PREFUNDER)

# for the side_chain_client demo, this is a simplified contract that just
# logs deposits and withdrawals
CONTRACT:=src/alacris_lib/facilitator_contract_binary.ml
contract: $(CONTRACT)
$(CONTRACT) : contracts/deposit-withdraw.sol $(wildcard contracts/*.sol)
	$(SHOW) "Compiling facilitator contract"
	$(HIDE) cd contracts/ && solc --bin -o ../_build/contracts --overwrite court.sol
	$(HIDE) awk '{ printf ("let contract_bytes = Legilogic_lib.Hex.parse_0x_bytes \"0x%s\"\n",$$1); }' < ./_build/contracts/Court.bin > $@.tmp && if cmp -s $@.tmp /dev/null ; then rm $@.tmp ; exit 1 ; else mv -f $@.tmp $@ ; fi

ALACRIS_LIB:=src/alacris_lib/alacris_lib.cmxs
alacris_lib: $(BUILD_DIR)/$(ALACRIS_LIB)
$(BUILD_DIR)/$(ALACRIS_LIB): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Alacris library"
	$(HIDE) dune build $(ALACRIS_LIB)

SIDE_CHAIN_CLIENT_LIB:=src/alacris_client/side_chain_client_lib.cmxs
side_chain_client_lib: $(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_LIB)
$(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_LIB): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building side_chain_client library"
	$(HIDE) dune build $(SIDE_CHAIN_CLIENT_LIB)

SIDE_CHAIN_SERVER:=src/alacris_lib/side_chain_server.exe
sidechain_server: $(BUILD_DIR)/$(SIDE_CHAIN_SERVER)
$(BUILD_DIR)/$(SIDE_CHAIN_SERVER): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Alacris side chain server executable"
	$(HIDE) dune build $(SIDE_CHAIN_SERVER)

SIDE_CHAIN_CLIENT:=src/alacris_client/side_chain_client.exe
side_chain_client: $(BUILD_DIR)/$(SIDE_CHAIN_CLIENT)
$(BUILD_DIR)/$(SIDE_CHAIN_CLIENT): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Alacris side chain client executable"
	$(HIDE) dune build $(SIDE_CHAIN_CLIENT)

SIDE_CHAIN_CLIENT_TEST:=src/alacris_client/side_chain_client_test.exe
side_chain_client_test: $(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_TEST)
$(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_TEST): src/alacris_client/side_chain_client_test.ml $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Alacris side_chain_client_test executable"
	$(HIDE) dune build $(SIDE_CHAIN_CLIENT_TEST)

# You don't usually need to install using opam, but if you want to:
install: $(ALACRIS_LIB)
ifeq ($(shell ocamlfind query -qe alacris 2> /dev/null),)
	$(SHOW) "Installing Alacris library to OPAM"
	$(HIDE) ./scripts/mk-opam-install.sh
	$(HIDE) opam pin -y add alacris . -n
	$(HIDE) opam install alacris
else
	$(SHOW) "Alacris library already installed in OPAM"
endif

uninstall:
ifneq ($(shell ocamlfind query -qe alacris 2> /dev/null),)
	$(SHOW) "Uninstalling Alacris library from OPAM"
	$(HIDE) opam uninstall alacris
	$(HIDE) opam pin remove alacris
else
	$(SHOW) "Alacris library not installed in OPAM"
endif

### Building a toplevel for interaction with our code:
TOPLEVEL:=src/toplevel.exe
toplevel: $(BUILD_DIR)/$(TOPLEVEL)
$(BUILD_DIR)/$(TOPLEVEL): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) dune build $(TOPLEVEL)

### Playing our code from an OCaml toplevel:
repl: ./bin/legicaml $(BUILD_DIR)/$(TOPLEVEL)
	$(SHOW) "Starting Alacris OCaml toplevel..." ; echo
	$(HIDE) rlwrap $<

### Build all the stuff to build
build_all: $(CONTRACT) force
	$(HIDE) dune build $(LEGILOGIC_LIB) $(LEGILOGIC_ETHEREUM) $(ETHEREUM_PREFUNDER) $(ALACRIS_LIB) $(SIDE_CHAIN_SERVER) $(SIDE_CHAIN_CLIENT) $(SIDE_CHAIN_CLIENT_TEST) $(TOPLEVEL)

### Running smoke test for the build:
test_hello: toplevel $(ML_SOURCES) $(CONTRACT) force
	$(HIDE) [ "$$(echo 'Printf.printf "%s" (Hex.parse_0x_data "0x48656c6c6f2c20776f726c64210a"); exit 0;;' | ./bin/legicaml -no-version -noprompt -noinit)" = "Hello, world!" ]

### Running unit tests
test: $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Running Alacris tests"
	$(HIDE) dune runtest -j 1

### TO RUN OUR INTEGRATION TESTS:
# 1- Run a private Ethereum network
run_ethereum_net :
	$(SHOW) "Starting private Ethereum devnet"
	$(HIDE) scripts/ethereum-devnet/run.sh

# 2- Fund test accounts on the private Ethereum network,
# importantly including the facilitator's account
fund_accounts : $(BUILD_DIR)/$(ETHEREUM_PREFUNDER) force
	$(HIDE) $(BUILD_DIR)/$(ETHEREUM_PREFUNDER) ./config/facilitator_keys.json ./config/demo-keys-small.json

# 3- Run our server
run_side_chain_server: $(BUILD_DIR)/$(SIDE_CHAIN_SERVER)
	$(SHOW) "Running side chain server"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(BUILD_DIR)/$(SIDE_CHAIN_SERVER)

# 4- Run our client
run_side_chain_client: $(BUILD_DIR)/$(SIDE_CHAIN_CLIENT)
	$(SHOW) "Running side chain client (SCGI server)"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(BUILD_DIR)/$(SIDE_CHAIN_CLIENT)

# 5- Run nginx as a front-end to our client
nginx:
	$(HIDE) ./src/alacris_client/nginx/start.sh

# 6- Now you can run our integration tests
test_side_chain_client : $(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_TEST)
	$(SHOW) "Testing side_chain_client"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(BUILD_DIR)/$(SIDE_CHAIN_CLIENT_TEST)

# Putting it together: mini integration test
test_mini_integration : all
	$(HIDE) ./scripts/mini_integration_test.sh

# You don't usually need to stop nginx, but in case you want to:
stop_nginx:
	$(HIDE) ./src/alacris_client/nginx/stop.sh

# A good cleaning of our system, except that we preserve the state of the ethereum main chain.
clean:
	$(SHOW) "Cleaning via dune"
	$(HIDE) dune clean
	$(SHOW) "Removing contract binary"
	$(HIDE) rm -f src/alacris_lib/facilitator_contract_binary.ml
	$(SHOW) "Removing OPAM install file"
	$(HIDE) rm -f alacris.install
	$(SHOW) "Removing run directory"
	$(HIDE) rm -rf _run

# Reset all servers
reset:
	$(SHOW) "Resetting Alacris state"
	$(SHOW) " Stopping Ethereum network"
	$(HIDE) killall -q geth 2> /dev/null || true
	$(SHOW) " Stopping alacris server"
	$(HIDE) killall -q side_chain_server.exe 2> /dev/null || true
	$(SHOW) " Stopping alacris client"
	$(HIDE) killall -q side_chain_client.exe 2> /dev/null || true
	$(SHOW) " Stopping nginx"
	$(HIDE) ./src/alacris_client/nginx/stop.sh 2> /dev/null || true
	$(SHOW) " Removing all databases, preserving old logs if any"
	$(HIDE) rm -rf _old_logs ; \
	if [ -d _run/logs ] ; then mv _run/logs _old_logs ; fi ; \
	rm -rf _run _ethereum ; \
	if [ -d _old_logs ] ; then mkdir _run ; mv _old_logs _run/old_logs ; fi

# Source code stats
wc: force
	$(SHOW) "Lines of .ml or .mli code"
	$(HIDE) for i in src/*/ ; do printf "%9s    %s\n" "$$(wc -l $$(find $$i -name '*.ml*') | tail -1 | (read a b ; echo $$a))" "$$i" ; done
	$(HIDE) printf "%9s    %s\n" "$$(wc -l $$(find ./src -name '*.ml*') | tail -1 | (read a b ; echo $$a))" total


# Docker part
DOCKER_COMPOSE = docker-compose
DOCKER_COMPOSE_FILE = docker/docker-compose.yml

docker-pull: ## Pull Alacris prerequsites images
	$(SHOW) " Pulling Alacris Docker images"
	$(HIDE) docker/scripts/pull_images.sh

docker-build-all: ## Build all or c=<name> containers in foreground
	$(SHOW) " BUilding Alacris Docker images"
	$(SHOW) docker/scripts/build_all_images.sh

docker-build: ## Build all or c=<name> containers in foreground
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) build $(c)

docker-list: ## List available services
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) config --services

docker-up: ## Start all or c=<name> containers in foreground
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up $(c)

docker-start: ## Start all or c=<name> containers in background
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d $(c)

docker-stop: ## Stop all or c=<name> containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(c)

docker-restart: ## Restart all or c=<name> containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(c)
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d $(c)

docker-status: ## Show status of containers
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) ps

docker-clean:  ## Clean all data
	@$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) down -v

docker-recompile: ## Recompile application
	$(SHOW) "Recompiling Alacris apps"
	@$ docker/scripts/recompile.sh

docker-reset-state: ## Clean Alacris state data
	$(SHOW) "Reseting Alacris states"
	$(HIDE) sudo docker/scripts/reset_state.sh
