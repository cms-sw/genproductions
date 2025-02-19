include ../../make_opts

all: mass_bias

clean:
	$(RM) *.o $(BIASLIBDIR)$(BIASLIBRARY)

#
# Compilation of the module ptj_bias
#
mass_bias.o: mass_bias.f ../bias.inc
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o mass_bias.o mass_bias.f

mass_bias: mass_bias.o
	$(call CREATELIB, $(BIASLIBDIR)$(BIASLIBRARY), $^)

#
# List of the requirements for this module.
# 'VALID' is the keyword that *must* be returned if everything is in order.
#
requirements:	
	@echo "VALID"

