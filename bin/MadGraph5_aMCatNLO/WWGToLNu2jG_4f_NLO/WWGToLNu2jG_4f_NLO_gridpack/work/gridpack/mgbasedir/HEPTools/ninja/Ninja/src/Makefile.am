lib_LTLIBRARIES = libninja.la
bin_PROGRAMS = ninja-config

GENQUADSCRIPT=$(top_builddir)/src/quadsources/genquadsources.sed

EXTRA_DIST = $(GENQUADSCRIPT)

ninja_config_SOURCES = ninja_config.cc

libninja_la_SOURCES = basis.cc  cuts_utils.cc  ninja.cc  Xcuts.cc \
	cuts.cc Xninja.cc spinors.cc tensor_ninja.cc \
	coefficient_level_subs.cc	\
	integral_library.cc avholo_interface.cc \
	rambo.cc ninja_wraps.cc
if QUADNINJA
nodist_libninja_la_SOURCES = quadsources/basis.cc  quadsources/cuts_utils.cc \
	quadsources/ninja.cc  quadsources/Xcuts.cc \
	quadsources/cuts.cc quadsources/Xninja.cc quadsources/spinors.cc \
	quadsources/tensor_ninja.cc \
	quadsources/coefficient_level_subs.cc	\
	quadsources/integral_library.cc quadsources/avholo_interface.cc \
	quadsources/rambo.cc quadsources/ninja_wraps.cc
endif

if AVHOLO
libninja_la_SOURCES += avholo_wrapper.F90 ninjavholo_wraps.cc ninjavholo.F90
if QUADNINJA
nodist_libninja_la_SOURCES += quadsources/avholo_wrapper.F90
endif
if NINJAGOSAMCONTRIB
AM_FCFLAGS = -I$(top_builddir)/avh_olo-2.2.1
else
AM_FCFLAGS = $(FCINCLUDE)
endif
endif
if LOOPTOOLS
libninja_la_SOURCES +=  looptools_interface.cc
endif
if GOSAM
libninja_la_SOURCES +=  gosam_interface.cc  ninjago.F90
endif
if F90MODULE
libninja_la_SOURCES +=  mninja.F90
endif

noinst_HEADERS = ninja_implem.cxx  polys_implem.cxx basis.hh \
    cuts.hh  cuts_utils.hh  Xcuts.hh  cuts_utils-inl.hh \
	coefficient_level_subs.hh	\
	integermath.hh  tmp_utils.hh  ninja_scoped_array.hh  cuts_vector.hh \
	integral_library_wrapper.hh s_mat_wrapper.hh avholo_decls.hh \
	ninja_hash_table.hh integral_cache.hh \
	gosam_interface.hh
if QUADNINJA
nodist_noinst_HEADERS = quadsources/ninja_implem.cxx \
	quadsources/polys_implem.cxx quadsources/basis.hh \
    quadsources/cuts.hh  quadsources/cuts_utils.hh  quadsources/Xcuts.hh \
	quadsources/cuts_utils-inl.hh \
	quadsources/coefficient_level_subs.hh	\
	quadsources/integermath.hh  quadsources/tmp_utils.hh \
	quadsources/ninja_scoped_array.hh  quadsources/cuts_vector.hh \
	quadsources/integral_library_wrapper.hh \
	quadsources/s_mat_wrapper.hh quadsources/avholo_decls.hh \
	quadsources/ninja_hash_table.hh quadsources/integral_cache.hh
#	quadsources/gosam_interface.hh
endif

library_includedir=$(includedir)
nodistdir=$(includedir)

libninja_la_LIBADD=

if NINJAGOSAMCONTRIB
nodist_pkginclude_HEADERS =
else
nodist_library_include_HEADERS = 
endif

if GOSAM
if NINJAGOSAMCONTRIB
nodist_pkginclude_HEADERS += ninjago_module.mod
libninja_la_LIBADD += -L$(top_builddir)/avh_olo-2.2.1 -lavh_olo
else
nodist_library_include_HEADERS += ninjago_module.mod
endif
endif

if AVHOLO
if NINJAGOSAMCONTRIB
nodist_pkginclude_HEADERS += ninjavholo.mod
else
nodist_library_include_HEADERS += ninjavholo.mod
endif
endif

if F90MODULE
if NINJAGOSAMCONTRIB
nodist_pkginclude_HEADERS += mninja.mod
else
nodist_library_include_HEADERS += mninja.mod
endif
endif

libninja_la_LIBADD += $(AVHFLAGS) $(QUADRUPLEFLAGS)

if QUADNINJA
$(nodist_libninja_la_SOURCES): quadsources/% : % $(nodist_noinst_HEADERS) $(GENQUADSCRIPT)
	$(SED) -f $(GENQUADSCRIPT) < $< > $@
$(nodist_noinst_HEADERS): quadsources/% : % $(GENQUADSCRIPT)
	$(SED) -f $(GENQUADSCRIPT) < $< > $@
clean-local:
	rm -rf quadsources/*.cc quadsources/*.hh quadsources/*.cxx quadsources/*.F90
endif

ACLOCAL_AMFLAGS = -I m4
SUBDIRS = ninja
if QUADNINJA
SUBDIRS += quadninja
endif
