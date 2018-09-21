# Makefile for Legicash

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

export LEGICASH_HOME:=$(shell pwd)

BUILD_DIR:=_build/default

all: side_chain_client sidechain_server side_chain_client_test

.PHONY: all force legilogic_lib legilogic_ethereum legicash_lib side_chain_client side_chain_client run test_side_chain_client ethereum_net hello_legicash install uninstall test toplevel clean reset contract nginx stop_nginx

ML_SOURCES:=$(wildcard src/*.ml src/*.mli src/*/*.ml src/*/*.mli src/dune src/*/dune)


### All the things to build:

LEGILOGIC_LIB:=$(BUILD_DIR)/src/legilogic_lib/legilogic_lib.cmxs
legilogic_lib: $(LEGILOGIC_LIB)
$(LEGILOGIC_LIB): $(ML_SOURCES)
	$(SHOW) "Building Legilogic library"
	$(HIDE) dune build src/legilogic_lib/legilogic_lib.a src/legilogic_lib/legilogic_lib.cmxa src/legilogic_lib/legilogic_lib.cmxs src/legilogic_lib/legilogic_lib.cma

LEGILOGIC_LIB_TEST:=$(BUILD_DIR)/src/legilogic_lib/legilogic_lib_test.exe
legilogic_lib_test: $(LEGILOGIC_LIB_TEST)
$(LEGILOGIC_LIB_TEST): src/legilogic_lib/legilogic_lib_test.ml $(ML_SOURCES)
	$(SHOW) "Building test legilogic_lib executable"
	$(HIDE) dune build src/legilogic_lib/legilogic_lib_test.exe

test_legilogic_lib : $(LEGILOGIC_LIB_TEST)
	$(SHOW) "Testing legilogic_lib"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(LEGILOGIC_LIB_TEST)

LEGILOGIC_ETHEREUM:=$(BUILD_DIR)/src/legilogic_ethereum/legilogic_ethereum.cmxs
legilogic_ethereum: $(LEGILOGIC_ETHEREUM)
$(LEGILOGIC_ETHEREUM): $(ML_SOURCES)
	$(SHOW) "Building Legilogic ethereum support"
	$(HIDE) dune build src/legilogic_ethereum/legilogic_ethereum.a src/legilogic_ethereum/legilogic_ethereum.cmxa src/legilogic_ethereum/legilogic_ethereum.cmxs src/legilogic_ethereum/legilogic_ethereum.cma

# for the side_chain_client demo, this is a simplified contract that just
# logs deposits and withdrawals
CONTRACT:=src/legicash_lib/facilitator_contract_binary.ml
contract: $(CONTRACT)
$(CONTRACT) : contracts/deposit-withdraw.sol $(wildcard contracts/*.sol)
	$(SHOW) "Compiling facilitator contract"
	$(HIDE)	cd contracts/ && solc --bin -o ../_build/contracts --overwrite court.sol
	$(HIDE) awk '{ printf ("let contract_bytes = Legilogic_lib.Hex.parse_0x_bytes \"0x%s\"\n",$$1); }' < ./_build/contracts/Court.bin > $@.tmp && if cmp -s $@.tmp /dev/null ; then rm $@.tmp ; exit 1 ; else mv -f $@.tmp $@ ; fi

LEGICASH_LIB:=$(BUILD_DIR)/src/legicash_lib/legicash_lib.cmxs
legicash_lib: $(LEGICASH_LIB)
$(LEGICASH_LIB): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Legicash library"
	$(HIDE) dune build src/legicash_lib/legicash_lib.a src/legicash_lib/legicash_lib.cmxa src/legicash_lib/legicash_lib.cmxs src/legicash_lib/legicash_lib.cma

SIDE_CHAIN_CLIENT_LIB:=$(BUILD_DIR)/src/endpoints/side_chain_client_lib.cmxs
side_chain_client_lib: $(SIDE_CHAIN_CLIENT_LIB)
$(SIDE_CHAIN_CLIENT_LIB): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building side_chain_client library"
	$(HIDE) dune build src/endpoints/side_chain_client_lib.cmxs

SIDE_CHAIN_SERVER:=$(BUILD_DIR)/src/legicash_lib/side_chain_server.exe
sidechain_server: $(SIDE_CHAIN_SERVER)
$(SIDE_CHAIN_SERVER): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Legicash side chain server executable"
	$(HIDE) dune build src/legicash_lib/side_chain_server.exe

SIDE_CHAIN_CLIENT:=$(BUILD_DIR)/src/endpoints/side_chain_client.exe
side_chain_client: $(SIDE_CHAIN_CLIENT)
$(SIDE_CHAIN_CLIENT): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Legicash side chain client executable"
	$(HIDE) dune build src/endpoints/side_chain_client.exe

SIDE_CHAIN_CLIENT_TEST:=$(BUILD_DIR)/src/endpoints/side_chain_client_test.exe
side_chain_client_test: $(SIDE_CHAIN_CLIENT_TEST)
$(SIDE_CHAIN_CLIENT_TEST): src/endpoints/side_chain_client_test.ml $(SIDE_CHAIN_CLIENT) $(CONTRACT)
	$(SHOW) "Building Legicash side_chain_client_test executable"
	$(HIDE) dune build src/endpoints/side_chain_client_test.exe

# You don't usually need to install using opam, but if you want to:
install: $(LEGICASH_LIB)
ifeq ($(shell ocamlfind query -qe legicash 2> /dev/null),)
	$(SHOW) "Installing Legicash library to OPAM"
	@ ./scripts/mk-opam-install.sh
	@ opam pin -y add legicash . -n
	@ opam install legicash
else
	$(SHOW) "Legicash library already installed in OPAM"
endif

uninstall:
ifneq ($(shell ocamlfind query -qe legicash 2> /dev/null),)
	$(SHOW) "Uninstalling Legicash library from OPAM"
	@ opam uninstall legicash
	@ opam pin remove legicash
else
	$(SHOW) "Legicash library not installed in OPAM"
endif

### Building a toplevel for interaction with our code:
TOPLEVEL=$(BUILD_DIR)/src/legicaml.exe
toplevel: $(TOPLEVEL)
$(TOPLEVEL): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) dune build src/legicaml.exe

### Playing our code from an OCaml toplevel:
repl: ./bin/legicaml $(TOPLEVEL)
	$(HIDE) echo "Starting Legicash OCaml toplevel..." ; echo
	$(HIDE) rlwrap $<

### Running smoke test for the build:
test_hello: repl $(ML_SOURCES) $(CONTRACT) force
	[ "$$(echo 'Printf.printf "%s" (Hex.parse_0x_data "0x48656c6c6f2c20776f726c64210a"); exit 0;;' | ./bin/legicaml -no-version -noprompt -noinit)" = "Hello, world!" ]

### Running unit tests
test: $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Running Legicash tests"
	$(HIDE) dune runtest -j 1

### TO RUN OUR INTEGRATION TESTS:
# 1- Run Ethereum
run_ethereum_net :
	$(SHOW) "Starting private Ethereum net for testing"
	$(HIDE) scripts/ethereum-testnet/run.sh

# 2- Run our server
run_side_chain_server: $(SIDE_CHAIN_SERVER)
	$(SHOW) "Running side chain server"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(SIDE_CHAIN_SERVER)

# 3- Run our client
run_side_chain_client: $(SIDE_CHAIN_CLIENT)
	$(SHOW) "Running side chain client (SCGI server)"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(SIDE_CHAIN_CLIENT)

# 4- Run nginx as a front-end to our client
nginx:
	./src/endpoints/nginx/start.sh

# 5- (Optional) To play with our system, you might want to fund a few accounts.
fund_alice_and_bob:
	./scripts/fund.sh

# 6- Now you can run our integration tests
test_side_chain_client : $(SIDE_CHAIN_CLIENT_TEST)
	$(SHOW) "Testing side_chain_client"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(SIDE_CHAIN_CLIENT_TEST)

# You don't usually need to stop nginx, but in case you want to:
stop_nginx:
	./src/endpoints/nginx/stop.sh

# A good cleaning of our system, except that we preserve the state of the ethereum main chain.
clean:
	$(SHOW) "Cleaning via dune"
	$(HIDE) dune clean
	$(SHOW) "Removing contract binary"
	$(HIDE) rm -f src/legicash_lib/facilitator_contract_binary.ml
	$(SHOW) "Removing OPAM install file"
	$(HIDE) rm -f legicash.install
	$(SHOW) "Removing run directory"
	$(HIDE) rm -rf _run

# Reset all servers
reset:
	$(SHOW) "Resetting Legicash state"
	$(SHOW) " Stopping Ethereum network"
	$(HIDE) killall -q geth 2> /dev/null || true
	$(SHOW) " Stopping legicash server"
	$(HIDE) killall -q side_chain_server.exe 2> /dev/null || true
	$(SHOW) " Stopping legicash client"
	$(HIDE) killall -q side_chain_client.exe 2> /dev/null || true
	$(SHOW) " Removing Legicash databases"
	$(HIDE) rm -rf _run/legicash _run/legicash_client
