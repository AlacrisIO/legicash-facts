## This Makefile contains the rules to make the Legicash endpoints demo.
## The project is configured through the variables in the file Makefile.options.
#
# TODO: have only one Makefile, at the toplevel, and also move the dune-project there,
# and reorganize the entire repository tree.


VERBOSE=

# use SHOW to inform user of commands
SHOW=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE := $(if $(VERBOSE),,@)

# all : endpoints_lib $(CONFIG) $(CONFIG_TEST)
all : api_scgi

.PHONY: all endpoints api_scgi run clean

BUILD_DIR=../_build/default/endpoints

ENDPOINTS_LIB=$(BUILD_DIR)/endpoints.cmxs
endpoints : $(ENDPOINTS_LIB)
$(ENDPOINTS_LIB) : $(LIBDIR) $(wildcard *.ml *.mli)
	$(SHOW) "Building endpoints library"
	$(HIDE) dune build --root=.. endpoints/endpoints.cmxs

API_SCGI=$(BUILD_DIR)/api_scgi.exe
api_scgi : $(API_SCGI)
$(API_SCGI) : $(ENDPOINTS_LIB)
	$(SHOW) "Building Legicash API SCGI executable"
	$(HIDE) dune build --root=.. endpoints/api_scgi.exe

run: $(API_SCGI)
	../_build/default/endpoints/api_scgi.exe

clean:
	-rm -rf $(PREFIX)
	-rm -rf _build
	-rm -rf legicash
