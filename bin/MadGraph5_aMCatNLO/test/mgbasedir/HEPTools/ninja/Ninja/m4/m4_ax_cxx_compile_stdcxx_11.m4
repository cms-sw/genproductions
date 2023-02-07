# File heavily modified by Tiziano Peraro.
#
# The original version can be found at the webpage:
# http://www.gnu.org/software/autoconf-archive/ax_cxx_compile_stdcxx_11.html


# LICENSE
#
#   Copyright (c) 2008 Benjamin Kosnik <bkoz@redhat.com>
#   Copyright (c) 2012 Zack Weinberg <zackw@panix.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.


m4_define([_AX_CXX_COMPILE_STDCXX_11_testbody], [

    #if defined(__clang__)
    #  if !__has_feature(cxx_generalized_initializers)
    #    error "no ninja-compatible c++11"
    #  endif
    #elif (defined __INTEL_COMPILER && __INTEL_COMPILER < 1400) || (defined __ICC && __ICC < 1400)
    #    error "no ninja-compatible c++11"
    #elif defined(__GNUC__) && defined(__GNUC_MINOR__)
    #  if (__GNUC__ < 4) ||  (__GNUC__ == 4 && __GNUC_MINOR__ < 6) || !defined(__GXX_EXPERIMENTAL_CXX0X__)
    #    error "no ninja-compatible c++11"
    #  endif
    #endif
	
	#include<type_traits>											
    int a;
    decltype(a) b;
	typename std::common_type<float,double>::type c;
	
])

m4_define([_AX_CXX_COMPILE_STDCXX_11_INIT_LIST_testbody], [[
	
    #include<complex>
	typedef std::complex<double> Complex;
	
	template<typename T>
    struct Array4D {

      Array4D(const T & v0, const T & v1, const T & v2, const T & v3)
        : data{v0,v1,v2,v3} {}

      T data[4];
    };

 	// Definition of a class for complex four-momenta
  	class ComplexMomentum
  	{

    public:

      // Default constructor
      ComplexMomentum(): data(Complex(), Complex(),
                              Complex(), Complex()) {}
  
    private:
      Array4D<Complex> data;
  
    }; // class ComplexMomentum

    ComplexMomentum my_momentum;
	
]])

m4_define([_AX_CXX_COMPILE_STDCXX_11_RANDOM_testbody], [[

    #include <random>
	double testfunc(int n)
    {
	  std::mt19937 gen_;
	  std::uniform_real_distribution<double> rnd_(0,1);
      gen_.seed(n);
      return rnd_(gen_);
    }
	
]])

AC_DEFUN([AX_CXX_COMPILE_STDCXX_11], [dnl
  m4_if([$1], [], [],
        [$1], [ext], [],
        [$1], [noext], [],
        [m4_fatal([invalid argument `$1' to AX_CXX_COMPILE_STDCXX_11])])dnl
  AC_LANG_ASSERT([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports C++11 features by default,
  ax_cv_cxx_compile_cxx11,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
    [ax_cv_cxx_compile_cxx11=yes],
    [ax_cv_cxx_compile_cxx11=no])])
  if test x$ax_cv_cxx_compile_cxx11 = xyes; then
    ac_success=yes
  fi

  m4_if([$1], [noext], [], [dnl
  if test x$ac_success = xno; then
    for switch in -std=gnu++11 -std=gnu++0x; do
      cachevar=AS_TR_SH([ax_cv_cxx_compile_cxx11_$switch])
      AC_CACHE_CHECK(whether $CXX supports C++11 features with $switch,
                     $cachevar,
        [ac_save_CXXFLAGS="$CXXFLAGS"
         CXXFLAGS="$CXXFLAGS $switch"
         AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
          [eval $cachevar=yes],
          [eval $cachevar=no])
         CXXFLAGS="$ac_save_CXXFLAGS"])
      if eval test x\$$cachevar = xyes; then
        CXXFLAGS="$CXXFLAGS $switch"
        ac_success=yes
        break
      fi
    done
  fi])

  m4_if([$1], [ext], [], [dnl
  if test x$ac_success = xno; then
    for switch in -std=c++11 -std=c++0x; do
      cachevar=AS_TR_SH([ax_cv_cxx_compile_cxx11_$switch])
      AC_CACHE_CHECK(whether $CXX supports C++11 features with $switch,
                     $cachevar,
        [ac_save_CXXFLAGS="$CXXFLAGS"
         CXXFLAGS="$CXXFLAGS $switch"
         AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_testbody])],
          [eval $cachevar=yes],
          [eval $cachevar=no])
         CXXFLAGS="$ac_save_CXXFLAGS"])
      if eval test x\$$cachevar = xyes; then
        CXXFLAGS="$CXXFLAGS $switch"
        ac_success=yes
        break
      fi
    done
  fi])

  if test x$ac_success = xyes; then
    AC_DEFINE([HAVE_DECLTYPE], [1], [Define to 1 if you have C++11 `decltype' operator and std::common_type traits.])
  fi
])


AC_DEFUN([AX_CXX_COMPILE_STDCXX_11_INIT_LIST], [dnl
  AC_LANG_ASSERT([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports C++11 initializer lists,
  ax_cv_cxx_compile_cxx11initlist,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_INIT_LIST_testbody])],
    [ax_cv_cxx_compile_cxx11initlist=yes],
    [ax_cv_cxx_compile_cxx11initlist=no])])
  if test x$ax_cv_cxx_compile_cxx11initlist = xyes; then
    ac_success=yes
  fi
  if test x$ac_success = xyes; then
    AC_DEFINE([HAVE_INIT_LIST], [1], [Define to 1 if you have C++11 initizializer lists in array-data-members initialization.])
  fi
])


AC_DEFUN([AX_CXX_COMPILE_STDCXX_11_RANDOM], [dnl
  AC_LANG_ASSERT([C++])dnl
  ac_success=no
  AC_CACHE_CHECK(whether $CXX supports C++11 random numbers,
  ax_cv_cxx_compile_cxx11random,
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([_AX_CXX_COMPILE_STDCXX_11_RANDOM_testbody])],
    [ax_cv_cxx_compile_cxx11random=yes],
    [ax_cv_cxx_compile_cxx11random=no])])
  if test x$ax_cv_cxx_compile_cxx11random = xyes; then
    ac_success=yes
  fi
  if test x$ac_success = xyes; then
    AC_DEFINE([HAVE_CXX11_RANDOM], [1], [Define to 1 if you have C++11 random numbers library.])
  fi
])
