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

BUILD_DIR:=src/_build/default

all: api_scgi

.PHONY: all force legilogic_lib legilogic_ethereum legicash_lib endpoints api_scgi run hello_legicash install uninstall test toplevel install-contract clean real_contract contract

ML_SOURCES:=$(wildcard src/*.ml src/*.mli src/*/*.ml src/*/*.mli)

LEGILOGIC_LIB:=$(BUILD_DIR)/legilogic_lib/legilogic_lib.cmxs
legilogic_lib: $(LEGILOGIC_LIB)
$(LEGILOGIC_LIB): $(ML_SOURCES)
	$(SHOW) "Building Legilogic library"
	$(HIDE) dune build --root=src legilogic_lib/legilogic_lib.a legilogic_lib/legilogic_lib.cmxa legilogic_lib/legilogic_lib.cmxs legilogic_lib/legilogic_lib.cma

LEGILOGIC_ETHEREUM:=$(BUILD_DIR)/legilogic_ethereum/legilogic_ethereum.cmxs
legilogic_ethereum: $(LEGILOGIC_ETHEREUM)
$(LEGILOGIC_ETHEREUM): $(ML_SOURCES)
	$(SHOW) "Building Legilogic ethereum support"
	$(HIDE) dune build --root=src legilogic_ethereum/legilogic_ethereum.a legilogic_ethereum/legilogic_ethereum.cmxa legilogic_ethereum/legilogic_ethereum.cmxs legilogic_ethereum/legilogic_ethereum.cma

# for the endpoints demo, this is a simplified contract that just
# logs deposits and withdrawals
CONTRACT:=src/legicash_lib/facilitator_contract_binary.ml
contract: $(CONTRACT)
$(CONTRACT) : contracts/deposit-withdraw.sol
	$(SHOW) "Compiling facilitator contract"
	$(HIDE) solc --bin $< | tail -n +4 | awk '{ printf ("let facilitator_contract = Bytes.of_string \"%s\"\n",$$1); }' > $@.tmp && if cmp -s $@.tmp /dev/null ; then rm $@.tmp ; exit 1 ; else mv -f $@.tmp $@ ; fi

LEGICASH_LIB:=$(BUILD_DIR)/legicash_lib/legicash_lib.cmxs
legicash_lib: $(LEGICASH_LIB)
$(LEGICASH_LIB): $(ML_SOURCES) $(CONTRACT)
	$(SHOW) "Building Legicash library"
	$(HIDE) dune build --root=src legicash_lib/legicash_lib.a legicash_lib/legicash_lib.cmxa legicash_lib/legicash_lib.cmxs legicash_lib/legicash_lib.cma

ENDPOINTS:=$(BUILD_DIR)/endpoints/endpoints.cmxs
endpoints: $(ENDPOINTS)
$(ENDPOINTS): $(LEGICASH_LIB) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building endpoints library"
	$(HIDE) dune build --root=src endpoints/endpoints.cmxs

API_SCGI:=$(BUILD_DIR)/endpoints/api_scgi.exe
api_scgi: $(API_SCGI)
$(API_SCGI): $(ENDPOINTS) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building Legicash API SCGI executable"
	$(HIDE) dune build --root=src endpoints/api_scgi.exe

run: $(API_SCGI)
	cd src/endpoints && ../../$(API_SCGI)

HELLO_LEGICASH:=$(BUILD_DIR)/hello_legicash.exe
hello_legicash: $(HELLO_LEGICASH)
$(HELLO_LEGICASH): $(LEGICASH_LIB) $(ML_SOURCES) $(CONTRACT) force
	$(SHOW) "Building main Legicash executable"
	$(HIDE) dune build --root=src hello_legicash.exe

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
	$(HIDE) dune runtest --root=src -j 1

toplevel: $(ML_SOURCES) $(CONTRACT) legilogic_lib legilogic_ethereum legicash_lib endpoints
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) dune build --root=src legicaml.exe

# name of custom toplevel
TOPLEVEL:=legicaml
repl: toplevel
	$(HIDE) rlwrap ./bin/$(TOPLEVEL)

install-contract: legicash_lib src/install_contract.ml
	$(SHOW) "Installing facilitator contract on main chain"
	$(HIDE) dune build --root=src install_contract.exe

clean:
	$(SHOW) "Cleaning via dune"
	$(HIDE) dune clean --root=src
	$(SHOW) "Removing contract binary"
	$(HIDE) rm -f src/legicash_lib/facilitator_contract_binary.ml
	$(SHOW) "Removing OPAM install file"
	$(HIDE) rm -f legicash.install

# real contract
real_contract:
	(cd contracts/ && solc court.sol)
