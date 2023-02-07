#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([Ninja], [1.1.0], [tiziano.peraro@ed.ac.uk])
AC_CONFIG_SRCDIR([src/ninja_implem.cxx])
AC_CONFIG_HEADERS([config.h src/ninja/ninja_config.h])
AC_CONFIG_MACRO_DIR([m4])
m4_include([m4/m4_ax_cxx_compile_stdcxx_11.m4])
m4_include([m4/m4_ax_cxx_tryandadd_flag.m4])
m4_include([m4/m4_ax_cxx_quadmath.m4])
AM_INIT_AUTOMAKE([subdir-objects foreign])
LT_INIT

# Checks for programs.
AC_PROG_SED
AC_PROG_CXX
AC_PROG_FC

# From now on is c++ stuff
AC_LANG_PUSH([C++])

# AC_OPENMP
ACX_PTHREAD

# Check C++11 compiler support
AX_CXX_COMPILE_STDCXX_11
AX_CXX_COMPILE_STDCXX_11_INIT_LIST
AX_CXX_COMPILE_STDCXX_11_RANDOM

# Check support for optional compiler flags
AC_ARG_ENABLE([autoflags],
    AS_HELP_STRING([--disable-autoflags],
    [do not add automatic compiler flags]))
AS_IF([test "x$enable_autoflags" != xno],
            [AX_CXX_TRYANDADD_FLAG("-fcx-fortran-rules")
            AX_CXX_TRYANDADD_FLAG("-complex-limited-range")]
)

# this is only defined in config.h
AC_DEFINE([NINJA_CONFIG_H_INTERNAL],
          [1],
          [Define to 1 if this an internal is a source file.])

# Choose whether the interface to the OneLoop library should be
# compiled
AC_ARG_WITH([avholo],
    AS_HELP_STRING([--with-avholo@<:@=FLAGS@:>@],
    [include an interface with the OneLoop integral library,\
    specifying the corresponding flags for dynamic linking \
    (if the Fortran module 'avh_olo' is not in a standard path, \
    you should add its directory to the FCINCLUDE variable)]),
    [],
    [with_avholo=no])
AS_IF([test "x$with_avholo" == xyes],
       [AC_SUBST([AVHFLAGS], ["-lavh_olo"])
        AC_DEFINE([NINJA_USE_ONELOOP], [1],
        [Define to 1 if you have OneLoop.])
        [avholo=true]
        ],
       [test "x$with_avholo" != xno],
       [AC_SUBST([AVHFLAGS], ["$with_avholo"])
        AC_DEFINE([NINJA_USE_ONELOOP], [1],
        [Define to 1 if you have OneLoop.])
        [avholo=true]
        ],
        [avholo=false])
AM_CONDITIONAL(AVHOLO, test "x$avholo" == xtrue)

# Choose whether an internal integral cache for the OneLoop library
# should be used
AC_ARG_ENABLE([avholo_cache],
    AS_HELP_STRING([--disable-avholo_cache],
    [do not include a cache of Master Integrals for OneLoop]))
AS_IF([test "x$enable_avholo_cache" != xno],
      [AC_DEFINE([NINJA_USE_ONELOOP_WITH_CACHE], [1],
      [Define to 1 if a cache of Master Integrals is implemented for OneLoop.])
      ],
      [])

# Choose whether the interface to the LoopTools library should be
# compiled
AC_ARG_WITH([looptools],
   AS_HELP_STRING([--with-looptools@<:@=FLAGS@:>@],
                  [include an interface with the LoopTools library, \
                  specifying the corresponding flags for static linking. \
                  (if the header file cloopttols.h is not in a standard \
                  path, you should add its directory to the \
                  CPPFLAGS variable)]),
                  [],
                  [with_looptools=no])
AS_IF([test "x$with_looptools" == xyes],
       [AC_SUBST([LTFLAGS], ["-looptools"])
        AC_DEFINE([NINJA_USE_LOOPTOOLS], [1], [Define if you have LoopTools.])
        [looptools=true]
        ],
       [test "x$with_looptools" != xno],
       [AC_SUBST([LTFLAGS], ["$with_looptools"])
        AC_DEFINE([NINJA_USE_LOOPTOOLS], [1], [Define if you have LoopTools.])
        [looptools=true]
        ],
        [looptools=false])
AM_CONDITIONAL(LOOPTOOLS, test "x$looptools" == xtrue)
AS_IF([test "x$looptools" == xtrue],
    [[case $host_os in
      darwin*)
         exninjastatic=true
    esac]])
AM_CONDITIONAL(EXNINJASTATIC, test "x$exninjastatic" == xtrue)

# Always enable massless specialization
AC_DEFINE([NINJA_MASSLESS], [1],
          [Define to 1 massless specialization is enabled.])

# Choose whether to include higher-rank support
AC_ARG_ENABLE([higher_rank],
    AS_HELP_STRING([--disable-higher_rank],
                   [disable support for higher-rank numerators]))
AS_IF([test "x$enable_higher_rank" != xno],
      [AC_DEFINE([NINJA_X1RANK], [1],
      [Define to 1 if higher-rank support is enabled.])])

# Choose whether to compile in quadruple precision
AC_ARG_WITH([quadruple],
    AS_HELP_STRING([--with-quadruple@<:@=FLAGS@:>@],
    [compile the library in quadruple precision (requires GCC libquadmath), \
    specifying the corresponding flags for the linker]),
    [],
    [with_quadruple=no])

# Choose whether to compile quadninja
AC_ARG_ENABLE([quadninja],
    AS_HELP_STRING([--enable-quadninja@<:@=FLAGS@:>@],
    [build quadninja, a copy of ninja in quadruple precision \
    (requires GCC libquadmath) alongside ninja, \
    specifying the corresponding flags for the linker]),
    [],
    [enable_quadninja=no])

# Check support for quadruple precision if needed
AS_IF([test "x$with_quadruple" != xno] || [test "x$enable_quadninja" != xno],
    [AX_CXX_COMPILE_LIBQUADMATH]
)
AS_IF([test "x$with_quadruple" != xno] || [test "x$enable_quadninja" != xno],
    [AX_CXX_COMPILE_STDCXX_11_FLOAT128_RANDOM]
)

# Quadruple precision
AS_IF([test "x$with_quadruple" == xyes],
       AC_SUBST([QUADRUPLEFLAGS], ["-lquadmath"])
       AC_DEFINE([NINJA_QUADRUPLE], [1],
       [Define to 1 if Ninja is compiled in quadruple precision.])
       [quadruple=true],
       [test "x$with_quadruple" != xno],
       AC_SUBST([QUADRUPLEFLAGS], ["$with_quadruple"])
       AC_DEFINE([NINJA_QUADRUPLE], [1],
       [Define to 1 if Ninja is compiled in quadruple precision.])
       [quadruple=true],
       [quadruple=false])
AM_CONDITIONAL(QUADRUPLE, test "x$quadruple" == xtrue)

# Quadninja
AS_IF([test "x$enable_quadninja" == xyes],
       AC_SUBST([QUADRUPLEFLAGS], ["-lquadmath"])
       AC_DEFINE([QUADNINJA], [1],
       [Define to 1 if QuadNinja is built.])
       [quadninja=true],
       [test "x$enable_quadninja" != xno],
       AC_SUBST([QUADRUPLEFLAGS], ["$enable_quadninja"])
       AC_DEFINE([QUADNINJA], [1],
       [Define to 1 if QuadNinja is built.])
       [quadninja=true],
       [quadninja=false])
AM_CONDITIONAL(QUADNINJA, test "x$quadninja" == xtrue)

# Set up macros for QuadNinja
AS_IF([test "x$enable_quadninja" != xno],
    AS_IF([test "x$with_quadruple" != xno],
    AC_MSG_ERROR(Option --with-quadruple is not compatible with option --enable-quadninja))
    AC_DEFINE([QUADNINJA_CONFIG_H_INTERNAL], [1],
              [Macro for QuadNinja])
    AS_IF([test "x$with_avholo" != xno],
    AC_DEFINE([QUADNINJA_USE_ONELOOP], [1],
              [Macro for QuadNinja]))
    AS_IF([test "x$enable_avholo_cache" != xno],
    AC_DEFINE([QUADNINJA_USE_ONELOOP_WITH_CACHE], [1],
              [Macro for QuadNinja]))
    #AS_IF([test "x$with_looptools" != xno],
    #AC_DEFINE([QUADNINJA_USE_LOOPTOOLS], [NINJA_USE_LOOPTOOLS],
    #          [Macro for QuadNinja]))
    AC_DEFINE([QUADNINJA_MASSLESS], [1],
              [Macro for QuadNinja])
    AS_IF([test "x$enable_higher_rank" != xno],
    AC_DEFINE([QUADNINJA_X1RANK], [1],
              [Macro for QuadNinja]))
)

# Choose whether compiling interfaces: gosam
AC_ARG_ENABLE([gosam],
    AS_HELP_STRING([--disable-gosam], [do not include GoSam interface]))
AS_IF([test "x$enable_gosam" != xno], [gosam=true],
 [gosam=false]
  )
AM_CONDITIONAL(GOSAM, test "x$gosam" == xtrue)
AM_CONDITIONAL(NINJAGOSAMCONTRIB, false)

# Choose whether compiling interfaces: f90 module
AC_ARG_ENABLE([f90module],
    AS_HELP_STRING([--disable-f90module], [do not include Fortran-90 interface]))
AS_IF([test "x$enable_f90module" != xno], [f90module=true],
 [f90module=false]
  )
AM_CONDITIONAL(F90MODULE, test "x$f90module" == xtrue)

AC_ARG_VAR([FCINCLUDE],[additional Fortran include flags, e.g. \
                        -I<include dir> if you have fortran modules \
                        installed in a nonstandard directory <include dir>])

# Checks for typedefs, structures, and compiler characteristics.
# AC_CHECK_HEADER_STDBOOL
# AC_C_INLINE
# AC_TYPE_SIZE_T

# Checks for library functions.
# AC_CHECK_FUNCS([floor pow rint sqrt])


AC_CONFIG_FILES([Makefile
                 src/Makefile
                 src/ninja/Makefile
                 src/quadninja/Makefile
				 examples/Makefile])
AC_OUTPUT
