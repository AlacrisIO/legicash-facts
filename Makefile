# Makefile for Legicash

# strategy:
# - use jbuilder/dune to build from OCaml sources
# - use make targets for other tasks more easily done from make

# make VERBOSE nonempty to see raw commands (or provide on command line)
ifndef VERBOSE
VERBOSE=
endif

# use SHOW to inform user of commands
SHOW=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE := $(if $(VERBOSE),,@)

BUILDER=dune

# name of custom toplevel
TOPLEVEL=legicaml

all : hello_legicash

.PHONY: hello_legicash test clean

legicash_lib :
	$(SHOW) "Building Legicash library"
	$(HIDE) $(BUILDER) build --root=src legicash_lib.a legicash_lib.cmxa legicash_lib.cmxs legicash_lib.cma

hello_legicash : legicash_lib
	$(SHOW) "Building main Legicash executable"
	$(HIDE) $(BUILDER) build --root=src hello_legicash.exe

install : legicash_lib
ifeq ($(shell ocamlfind query -qe legicash),)
	$(SHOW) "Installing Legicash library to OPAM"
	@ ./scripts/mk-opam-install.sh
	@ opam pin -y add legicash . -n
	@ opam install legicash
else
	$(SHOW) "Legicash library already installed in OPAM"
endif

uninstall :
ifneq ($(shell ocamlfind query -qe legicash),)
	$(SHOW) "Uninstalling Legicash library from OPAM"
	@ opam uninstall legicash
	@ opam pin remove legicash
else
	$(SHOW) "Legicash library not installed in OPAM"
endif

test :
	$(SHOW) "Running Legicash tests"
	$(HIDE) $(BUILDER) runtest --root=src

toplevel : legicash_lib
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) $(BUILDER) build --root=src legicaml.exe

repl : toplevel
	$(HIDE) rlwrap ./bin/$(TOPLEVEL)

endpoints : legicash_lib
	make -C src/endpoints test.opt

clean :
	$(SHOW) "Cleaning via dune"
	$(HIDE) $(BUILDER) clean --root=src
	$(SHOW) "Removing OPAM install file"
	$(HIDE) rm -f legicash.install
	$(SHOW) "Cleaning endpoints code"
	$(HIDE) make -C src/endpoints distclean

contract:
	(cd contracts/ && solc court.sol)
