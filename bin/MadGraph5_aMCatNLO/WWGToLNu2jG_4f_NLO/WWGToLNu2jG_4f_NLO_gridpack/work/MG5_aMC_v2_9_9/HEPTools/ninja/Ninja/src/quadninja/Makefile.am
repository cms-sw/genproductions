ACLOCAL_AMFLAGS = -I m4

GENQUADSCRIPT=$(top_builddir)/src/quadsources/genquadsources.sed

library_includedir=$(includedir)/quadninja
QUADNINJAHEADERS = ninja.hh ninja_in.hh tensor_ninja.hh \
	zero_float.hh types.hh  s_mat.hh \
	momentum.hh  num_defs.hh spinors.hh  \
	static_arrays.hh \
	integral_library.hh thread_safe_integral_library.hh \
	rambo.hh ninjanumgen.hh
if AVHOLO
QUADNINJAHEADERS +=   avholo.hh
endif
if LOOPTOOLS
QUADNINJAHEADERS +=  looptools.hh
endif
QUADNINJAHEADERS +=  quadruple.hh

nodist_library_include_HEADERS = $(QUADNINJAHEADERS)
nodist_library_include_HEADERS += ninja_config.h

nodistdir=$(includedir)/quadninja

$(QUADNINJAHEADERS): %.hh : ../ninja/%.hh $(GENQUADSCRIPT)
	$(SED) -f $(GENQUADSCRIPT) < $< > $@

ninja_config.h : ../ninja/ninja_config.h $(GENQUADSCRIPT)
	$(SED) -f $(GENQUADSCRIPT) < $< > $@
	cat $< >> $@
	printf "\n#undef QUADNINJA_USE_LOOPTOOLS\n" >> $@

clean-local:
	rm -rf *.hh ninja_config.h
