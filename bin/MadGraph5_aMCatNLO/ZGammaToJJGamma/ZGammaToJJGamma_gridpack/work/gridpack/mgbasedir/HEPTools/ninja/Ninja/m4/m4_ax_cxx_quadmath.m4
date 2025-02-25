m4_define([_AX_CXX_COMPILE_LIBQUADMATH_testbody], [[
    extern "C" {
    #include <quadmath.h>
    }
	__float128 testfloat()
    {
      return sqrtq(2.0q);
    }
    __complex128 testcfloat()
    {
      return csqrtq(2.0q + (__extension__ 1.0iQ)*1.0q);
    }
]])

m4_define([_AX_CXX_COMPILE_STDCXX_11_FLOAT128_RANDOM_testbody], [[

    extern "C" {
    #include <quadmath.h>
    }
    #include <random>
	__float128 testfunc(int n)
    {
	  std::mt19937 gen_;
	  std::uniform_real_distribution<__float128> rnd_(0,1);
      gen_.seed(n);
      return rnd_(gen_);
    }
	
]])

AC_DEFUN([AX_CXX_COMPILE_LIBQUADMATH], [dnl
  AC_LANG_ASSERT([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports quadruple precision with libquadmath,
  ax_cv_cxx_compile_libquadmath,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_LIBQUADMATH_testbody])],
    [ax_cv_cxx_compile_libquadmath=yes],
    [ax_cv_cxx_compile_libquadmath=no])])
  if test x$ax_cv_cxx_compile_libquadmath = xyes; then
    ac_success=yes
  fi
  if test x$ac_success != xyes; then
    if test "x$with_quadruple" != xno; then
      AC_MSG_ERROR([Quadruple precision is not supported by your compiler.  You can still use Ninja in double precision by removing the --with-quadruple otion or use a version of g++ which supports libquamath.])
    fi
    if test "x$enable_quadninja" != xno; then
      AC_MSG_WARN([Quadruple precision is not supported by your compiler.  The --enable-quadninja option will be removed and you can still use Ninja in double precision.  Alternatively, configure again with a version of g++ which supports libquamath.])
      enable_quadninja=no
    fi
  fi
])

AC_DEFUN([AX_CXX_COMPILE_STDCXX_11_FLOAT128_RANDOM], [dnl
  AC_LANG_ASSERT([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports C++11 random numbers in quadruple precision,
  ax_cv_cxx_compile_cxx11_float128_random,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_FLOAT128_RANDOM_testbody])],
    [ax_cv_cxx_compile_cxx11_float128_random=yes],
    [ax_cv_cxx_compile_cxx11_float128_random=no])])
  if test x$ax_cv_cxx_compile_cxx11_float128_random = xyes; then
    ac_success=yes
  fi
  if test x$ac_success = xyes; then
    AC_DEFINE([HAVE_CXX11_FLOAT128_RANDOM], [1], [Define to 1 if you have C++11 random numbers library compatible with __float128 type of libquadmath.])
  fi
])
