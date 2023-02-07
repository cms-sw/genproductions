dnl From https://stackoverflow.com/questions/1354996/need-an-autoconf-macro-that-detects-if-m64-is-a-valid-compiler-option
dnl Modified by Johann Felix v. Soden-Fraunhofen for GoSam and by Tiziano Peraro for Ninja

dnl @synopsis CXX_FLAGS_CHECK [compiler flags] [result_variable]
dnl @summary check whether compiler supports given C++ flags or not and if yes it adds it to CXXFLAGS
AC_DEFUN([AX_CXX_TRYANDADD_FLAG],
[dnl
   AC_MSG_CHECKING([whether $CXX accepts $1 flag])
   AC_LANG_PUSH([C++])
   ac_saved_cxxflags="$CXXFLAGS"
   CXXFLAGS="-Werror $1"
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
     [AC_MSG_RESULT([yes])
     CXXFLAGS="$ac_saved_cxxflags $1"],
     [AC_MSG_RESULT([no])
     CXXFLAGS="$ac_saved_cxxflags"]
  )
  AC_LANG_POP([C++])
])
