ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = ninja_config.h.in

library_includedir=$(includedir)/ninja
library_include_HEADERS = ninja.hh ninja_in.hh tensor_ninja.hh \
	zero_float.hh types.hh  s_mat.hh \
	momentum.hh  num_defs.hh spinors.hh  \
	static_arrays.hh \
	integral_library.hh thread_safe_integral_library.hh \
	rambo.hh ninjanumgen.hh
if AVHOLO
library_include_HEADERS +=   avholo.hh
endif
if LOOPTOOLS
library_include_HEADERS +=  looptools.hh
endif
if QUADRUPLE
library_include_HEADERS +=  quadruple.hh
endif

nodistdir=$(includedir)/ninja
nodist_library_include_HEADERS = ninja_config.h
