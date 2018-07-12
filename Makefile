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
LIBCMO=legicash_lib.cmo

all : hello_legicash

.PHONY: hello_legicash test clean

legicash_lib :
	$(SHOW) "Building Legicash library"
	$(HIDE) $(BUILDER) build --root=src legicash_lib.a

hello_legicash : legicash_lib
	$(SHOW) "Building main Legicash executable"
	$(HIDE) $(BUILDER) build --root=src hello_legicash.exe

test :
	$(SHOW) "Running Legicash tests"
	$(HIDE) $(BUILDER) runtest --root=src

toplevel : legicash_lib
	$(SHOW) "Building custom OCaml toplevel"
	$(HIDE) $(BUILDER) build --root=src legicaml.exe

repl : toplevel
	rlwrap ./bin/legicaml

clean :
	$(SHOW) "Cleaning via dune"
	$(HIDE) $(BUILDER) clean --root=src

contract:
	(cd contracts/ && solc facilitator.sol)
