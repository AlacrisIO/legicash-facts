## This Makefile contains the rules to make the Legicash endpoints demo.
## The project is configured through the variables in the file Makefile.options.

VERBOSE=

# use SHOW to inform user of commands
SHOW=@echo

# use HIDE to run commands invisibly, unless VERBOSE defined
HIDE := $(if $(VERBOSE),,@)

BUILDER=dune
OCSIGENSERVER=ocsigenserver.opt
SED=sed
CONFIG=${PROJECT_NAME}.conf
CONFIG_TEST=${PROJECT_NAME}-test.conf

include Makefile.options

all : endpoints_lib $(CONFIG) $(CONFIG_TEST)

.PHONY: endpoints_lib

ENDPOINTS_LIB=endpoints.cmxs

endpoints_lib : $(LIBDIR)
	$(SHOW) "Building endpoints library"
	$(HIDE) $(BUILDER) build --root=. $(ENDPOINTS_LIB)
	$(HIDE) cp _build/default/$(ENDPOINTS_LIB) $(LIBDIR)/

# subdirectories

$(LIBDIR) :
	$(SHOW) "Creating library directory"
	$(HIDE) mkdir -p $(LIBDIR)
$(ETCDIR) :
	$(SHOW) "Creating etc directory"
	$(HIDE) mkdir -p $(ETCDIR)
$(RUNDIR) :
	$(SHOW) "Creating run directory"
	$(HIDE) mkdir -p $(RUNDIR)
$(LOGDIR) :
	$(SHOW) "Creating log directory"
	$(HIDE) mkdir -p $(LOGDIR)
$(LOGFILE) : $(LOGDIR)
	$(SHOW) "Creating empty access log"
	echo -n > $@
	chmod a+rw $@
$(DATADIR) :
	$(SHOW) "Creating data directory"
	$(HIDE) mkdir -p $(DATADIR)
$(STATICDIR) :
	$(SHOW) "Creating static directory"
	$(HIDE) mkdir -p $(STATICDIR)

DIRS=$(LIBDIR) $(ETCDIR) $(RUNDIR) $(LOGDIR) $(DATADIR) $(STATICDIR)

# config files

FINDLIB_PACKAGES=$(patsubst %,\<extension\ findlib-package=\"%\"\ /\>,$(SERVER_PACKAGES))
EDIT_WARNING=DON\'T EDIT THIS FILE! It is generated from $(PROJECT_NAME).conf.in, edit that one, or the variables in Makefile.options
SED_ARGS := -e "/^ *%%%/d"
SED_ARGS += -e "s|%%PROJECT_NAME%%|$(PROJECT_NAME)|g"
SED_ARGS += -e "s|%%DATABASE_NAME%%|$(DATABASE_NAME)|g"
SED_ARGS += -e "s|%%DATABASE_USER%%|$(DATABASE_USER)|g"
SED_ARGS += -e "s|%%CMDPIPE%%|$(CMDPIPE)|g"
SED_ARGS += -e "s|%%LOGDIR%%|$(LOGDIR)|g"
SED_ARGS += -e "s|%%DATADIR%%|$(DATADIR)|g"
SED_ARGS += -e "s|%%PERSISTENT_DATA_BACKEND%%|$(PERSISTENT_DATA_BACKEND)|g"
SED_ARGS += -e "s|%%LIBDIR%%|$(LIBDIR)|g"
SED_ARGS += -e "s|%%WARNING%%|$(EDIT_WARNING)|g"
SED_ARGS += -e "s|%%PACKAGES%%|$(FINDLIB_PACKAGES)|g"
ifeq ($(DEBUG),yes)
  SED_ARGS += -e "s|%%DEBUGMODE%%|\<debugmode /\>|g"
else
  SED_ARGS += -e "s|%%DEBUGMODE%%||g"
endif

LOCAL_SED_ARGS := -e "s|%%PORT%%|$(TEST_PORT)|g"
LOCAL_SED_ARGS += -e "s|%%STATICDIR%%|$(LOCAL_STATIC)|g"
LOCAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
GLOBAL_SED_ARGS := -e "s|%%PORT%%|$(PORT)|g"
GLOBAL_SED_ARGS += -e "s|%%STATICDIR%%|$(STATICDIR)|g"
ifeq ($(WWWUSER)$(WWWGROUP),)
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%||g"
else
  GLOBAL_SED_ARGS += -e "s|%%USERGROUP%%|<user>$(WWWUSER)</user><group>$(WWWGROUP)</group>|g"
endif

${CONFIG} : ${PROJECT_NAME}.conf.in Makefile.options 
	$(SHOW) "Making Ocsigen config"
	$(HIDE) $(SED) $(SED_ARGS) $(GLOBAL_SED_ARGS) $< > $@
${CONFIG_TEST} : ${PROJECT_NAME}.conf.in Makefile.options 
	$(SHOW) "Making Ocsigen test config"
	$(HIDE) $(SED) $(SED_ARGS) $(LOCAL_SED_ARGS) $< > $@

# end config files

.PHONY: run clean

run.test: ${CONFIG_TEST} endpoints_lib $(DIRS)
	$(OCSIGENSERVER) $(RUN_DEBUG) -c $<

run: ${CONFIG} endpoints_lib $(DIRS)
	$(OCSIGENSERVER) $(RUN_DEBUG) -c $< 

clean:
	-rm -rf $(PREFIX)
	-rm -f $(CONFIG) $(CONFIG_TEST)
	-rm -rf _build
	-rm -rf accounts confirmations facilitator_state

