include ../../make_opts

all: mll_bias

clean:
	$(RM) *.o $(BIASLIBDIR)$(BIASLIBRARY)

#
# Compilation of the module mll_bias
#
mll_bias.o: mll_bias.f ../bias.inc
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o mll_bias.o mll_bias.f

mll_bias: mll_bias.o
	$(call CREATELIB, $(BIASLIBDIR)$(BIASLIBRARY), $^)

#
# List of the requirements for this module.
# 'VALID' is the keyword that *must* be returned if everything is in order.
#
requirements:	
ifeq ($(shell $(call CHECK_MG5AMC_VERSION,2.4.2)),True)
	@echo "VALID"
else
	@echo "Error:: MG5aMC is not recent enough (found "$(MG5AMC_VERSION)")"
	@echo "FAIL"
endif
