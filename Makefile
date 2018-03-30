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

# will change to dune when released
BUILDER=jbuilder

all : test_file

test_file : src/test_file.ml
	$(SHOW) "Running jbuilder on sources"
	$(HIDE) $(BUILDER) build --root=src test_file.exe


