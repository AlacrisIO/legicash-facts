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

export LEGICASH_HOME=$(shell pwd)

BUILD_DIR:=_build/default

all: api_scgi endpoints_test hello_legicash

.PHONY: all force legilogic_lib legilogic_ethereum legicash_lib endpoints api_scgi run test-endpoints ethereum-net hello_legicash install uninstall test toplevel clean reset contract nginx stop_nginx install_contract

ML_SOURCES:=$(wildcard src/*.ml src/*.mli src/*/*.ml src/*/*.mli src/dune src/*/dune)

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

test-legilogic_lib : $(LEGILOGIC_LIB_TEST)
	$(SHOW) "Testing legilogic_lib"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(LEGILOGIC_LIB_TEST)

LEGILOGIC_ETHEREUM:=$(BUILD_DIR)/src/legilogic_ethereum/legilogic_ethereum.cmxs
legilogic_ethereum: $(LEGILOGIC_ETHEREUM)
$(LEGILOGIC_ETHEREUM): $(ML_SOURCES)
	$(SHOW) "Building Legilogic ethereum support"
	$(HIDE) dune build src/legilogic_ethereum/legilogic_ethereum.a src/legilogic_ethereum/legilogic_ethereum.cmxa src/legilogic_ethereum/legilogic_ethereum.cmxs src/legilogic_ethereum/legilogic_ethereum.cma

# for the endpoints demo, this is a simplified contract that just
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

ENDPOINTS:=$(BUILD_DIR)/src/endpoints/endpoints.cmxs
endpoints: $(ENDPOINTS)
$(ENDPOINTS): $(LEGICASH_LIB) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building endpoints library"
	$(HIDE) dune build src/endpoints/endpoints.cmxs

API_SCGI:=$(BUILD_DIR)/src/endpoints/api_scgi.exe
api_scgi: $(API_SCGI)
$(API_SCGI): $(ENDPOINTS) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building Legicash API SCGI executable"
	$(HIDE) dune build src/endpoints/api_scgi.exe

ENDPOINTS_TEST:=$(BUILD_DIR)/src/endpoints/endpoints_test.exe
endpoints_test: $(ENDPOINTS_TEST)
$(ENDPOINTS_TEST): src/endpoints/endpoints_test.ml $(ENDPOINTS) $(LEGILOGIC_LIB)
	$(SHOW) "Building test endpoints executable"
	$(HIDE) dune build src/endpoints/endpoints_test.exe

run: $(API_SCGI)
	$(SHOW) "Running SCGI server"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(API_SCGI)

test-endpoints : $(ENDPOINTS_TEST)
	$(SHOW) "Testing endpoints"
	$(HIDE) mkdir -p _run/logs ; cd _run && ../$(ENDPOINTS_TEST)

ethereum-net :
	$(SHOW) "Starting Ethereum net"
	$(HIDE) scripts/ethereum-testnet/run.sh

HELLO_LEGICASH:=$(BUILD_DIR)/src/hello_legicash.exe
hello_legicash: $(HELLO_LEGICASH)
$(HELLO_LEGICASH): $(LEGICASH_LIB) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building main Legicash executable"
	$(HIDE) dune build src/hello_legicash.exe

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

test: $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Running Legicash tests"
	$(HIDE) dune runtest -j 1

TOPLEVEL=$(BUILD_DIR)/src/legicaml.exe
toplevel: $(TOPLEVEL)
$(TOPLEVEL): $(ML_SOURCES) $(CONTRACT) legilogic_lib legilogic_ethereum legicash_lib endpoints
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) dune build src/legicaml.exe

# name of custom toplevel
repl: ./bin/legicaml $(TOPLEVEL)
	$(HIDE) rlwrap $<

# Launch nginx:
nginx:
	./src/endpoints/nginx/start.sh

stop_nginx:
	./src/endpoints/nginx/stop.sh

install_contract: legicash_lib src/install_contract.ml $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Installing facilitator contract on main chain"
	$(HIDE) dune build src/install_contract.exe

clean:
	$(SHOW) "Cleaning via dune"
	$(HIDE) dune clean
	$(SHOW) "Removing contract binary"
	$(HIDE) rm -f src/legicash_lib/facilitator_contract_binary.ml
	$(SHOW) "Removing OPAM install file"
	$(HIDE) rm -f legicash.install
	$(SHOW) "Removing run directory"
	$(HIDE) rm -rf _run

reset:
	$(SHOW) "Resetting Legicash state"
	$(SHOW) " Stopping Ethereum network"
	$(HIDE) killall -q geth 2> /dev/null || true
	$(SHOW) " Stopping SCGI server"
	$(HIDE) killall -q api_scgi.exe 2> /dev/null || true
	$(SHOW) " Removing Legicash database"
	$(HIDE) rm -rf _run/legicash
