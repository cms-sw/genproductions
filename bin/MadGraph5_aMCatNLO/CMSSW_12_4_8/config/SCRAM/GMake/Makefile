ifndef THISDIR
$(error "You must pass in a value for THISDIR (normally current dir name from LOCALTOP).")
endif
ifndef SCRAM_ADMIN_DIR
SCRAM_ADMIN_DIR:= .SCRAM/$(SCRAM_ARCH)
endif
ifndef SCRAM_SCRIPT_EXT
SCRAM_SCRIPT_EXT:=.py
endif
ifndef SCRAM_TOOLS_DIR
SCRAM_TOOLS_DIR:=$(SCRAM_ADMIN_DIR)/$(if $(strip $(filter V2_%,$(SCRAM_VERSION))),timestamps,tools)
endif
EXTERNAL_LINK_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/linkexternal$(SCRAM_SCRIPT_EXT)
TOOL_MAKE_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/updateToolMK$(SCRAM_SCRIPT_EXT)
FIND_DEPS_TREE_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/find-deps-tree$(SCRAM_SCRIPT_EXT)
PROJECT_AREA_RENAME_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/projectAreaRename$(SCRAM_SCRIPT_EXT)
PROJECT_INFO_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/projectInfo$(SCRAM_SCRIPT_EXT)
CHECK_PRIVATE_HEADERS_SCRIPT:=$(LOCALTOP)/$(SCRAM_CONFIGDIR)/SCRAM/check-product-src$(SCRAM_SCRIPT_EXT)

SCRAM_MAKEFILE := $(firstword $(MAKEFILE_LIST))
ToolTimeStamps := $(wildcard $(SCRAM_TOOLS_DIR)/*)
DIRCACHE_MKDIR := $(SCRAM_INTwork)/MakeData
PUB_DIRCACHE_MKDIR := $(SCRAM_ADMIN_DIR)/MakeData
TOOLS_MKDIR    := $(PUB_DIRCACHE_MKDIR)/Tools

$(SCRAM_INTwork)/Makefile: $(TOOLS_MKDIR).mk
	@echo -include $(PUB_DIRCACHE_MKDIR)/variables.mk           > $@.tmp &&\
	echo  -include $(SCRAM_CONFIGDIR)/SCRAM/GMake/Makefile       >> $@.tmp &&\
	echo  -include $(SCRAM_CONFIGDIR)/SCRAM/GMake/Makefile.rules >> $@.tmp  &&\
	mv $@.tmp $@

$(TOOLS_MKDIR).mk: $(ToolTimeStamps)
	@mkdir -p $(TOOLS_MKDIR);\
	if [ ! -f $@ ] ; then \
	  $(TOOL_MAKE_SCRIPT) $(SCRAM_ARCH) > $@.tmp ;\
	else \
	  $(TOOL_MAKE_SCRIPT) $(SCRAM_ARCH) $(notdir $?) > $@.tmp ;\
	fi
	@cat $(TOOLS_MKDIR)/*.mk >> $@.tmp
	@mv $@.tmp $@

$(SCRAM_TOOLS_DIR)/%: $(SCRAM_CONFIGDIR)/toolbox/$(SCRAM_ARCH)/tools/selected/%.xml
	@for tool in $*; do $(SCRAM) setup $* && touch $@; done

$(SCRAM_CONFIGDIR)/% $(PUB_DIRCACHE_MKDIR)/% $(DIRCACHE_MKDIR)/%:
