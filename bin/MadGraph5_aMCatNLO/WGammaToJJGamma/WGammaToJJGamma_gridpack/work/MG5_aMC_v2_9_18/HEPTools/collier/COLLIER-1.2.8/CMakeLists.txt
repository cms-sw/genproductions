#==============================================================================#
#                               COLLIER MAKEFILE                               #
#==============================================================================#

#=========#
#  Usage  #
#=========#

#==========================#
#  Makefile & Compilation  #
#==========================#

# 1. CMakeLists.txt must be placed in the main directory of collier.

# 2. Invoking "cmake ." or "cmake CMakelists.txt" in the main directory generates
#    the file "Makefile".

# (2.) Optionally, the makefile can be configured to generate the static library
#    `libcollier.a` instead of the shared library `libcollier.so` which is
#    controlled by the variable `static`. (See 4. in Flags & Compiler)

# 3. Invoking "make" in the main directory builds the collier library.
#    Paralellized compilation via "make -j" (see make).

# (3.) In addition, the demos can be compiled:
#       - "make demo"
#       - "make democache"


#====================#
#  Flags & Compiler  #
#====================#

# Two sets of flags are defined, the Release and Debug flags.

# 1. With "cmake ." the default flags will be used, which is RELEASE. 

# 2. With "cmake -DCMAKE_BUILD_TYPE=DEBUG" the DEBUG flags will be used

# 3. With "cmake -DCMAKE_BUILD_TYPE=RELEASE" the RELEASE flags will be used

# 4. With "cmake -Dstatic=ON" collier is built as static library and
#    with "cmake -Dstatic=OFF" collier is built as shared library (DEFAULT:
#    static=OFF)

# Collier can be compiled with different compilers and has been successfully
# built with gfortran and ifort. 

# 5. gfortran is the default if multiple compilers are found.

# 6. Force the use of a specific compiler via, e.g. 
#    "cmake -DCMAKE_Fortran_COMPILER=ifort",
#    "cmake -DCMAKE_Fortran_COMPILER=gfortran", 
#    "cmake -DCMAKE_Fortran_COMPILER=pgf90" (not tested)

# 7. The collier library can be installed with "make install".
#    The default prefix can be altered via the option CMAKE_INSTALL_PREFIX.
#    For instance, "cmake -DCMAKE_INSTALL_PREFIX=$HOME"
#    will install collier to the home directory.

#=============#
#  Usage END  #
#=============#

set(CMAKE_MACOSX_RPATH 1) 
set(CMAKE_CACHEFILE_DIR  CMakeFiles/)

#=======================#
#  Project declaration  #
#=======================#

cmake_minimum_required (VERSION 2.8.7)
project (collier)
set(COLLIER_VERSION 1.2.8)
enable_language(Fortran)

#================#
#  Get Compiler  #
#================#

# This command writes the default fortran compiler in the variable 
# Fortran_COMPILER_NAME
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message("Using Fortran compiler: ${Fortran_COMPILER_NAME}")

#=========================#
#  Setting default Flags  #
#=========================#

add_definitions(-Dcollierdd -DSING -fPIC)
#add_definitions(-DCritPoints2 -DCritPointsCOLI) # added AD

if (Fortran_COMPILER_NAME MATCHES "gfortran.*" OR Fortran_COMPILER_NAME MATCHES "f95")
#==================#
#  gfortran Flags  #
#==================#
#-
  set (basic "-ffixed-line-length-132  -fno-default-integer-8")
  set (opt_RELEASE "-O2 -funroll-loops -ffast-math -Wtabs")
  set (opt_DEBUG "-O0 -frange-check -g -fcheck=bounds,do,mem,pointer,recursion")
  set (opt_DEBUG "${opt_DEBUG} -Wall -Wtabs -Wextra -Wno-unused")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-dummy-argument")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-parameter")
  set (opt_DEBUG "${opt_DEBUG} -Wno-compare-reals")
  set (opt_DEBUG "${opt_DEBUG} -ffpe-trap=invalid,overflow -fdump-core -fbacktrace")
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} ${opt_RELEASE}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${basic} ${opt_DEBUG}")

elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
#===============#
#  ifort Flags  #
#===============#
# TODO: (AD 2016-01-20) set up debug & release flags
#-
#  set (basic "${basic} -132 -vec-report0")
  set (basic "${basic} -132")
  set (opt_RELEASE "-O2")
  set (opt_DEBUG "-O0 -check -check noarg_temp_created -g")
  set (opt_DEBUG "${opt_DEBUG} -warn all -warn nodeclarations -warn nounused")
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} ${opt_RELEASE}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${basic} ${opt_DEBUG}")

#-
elseif (Fortran_COMPILER_NAME MATCHES "pgf90")
#=============#
#  pgf Flags  #
#=============#
# TODO: (AD 2016-01-20) Not tested
#-
  set (basic "${basic} -ffixed-line-length-132  -g77libs -Msecond_underscore")
  set (opt_RELEASE "-O2 -fast")
  set (opt_DEBUG "-O0 -Mbounds -frange-check -g -fcheck=all")
  set (opt_DEBUG "${opt_DEBUG} -Wall -Wtabs -Wextra -Wno-unused")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-dummy-argument")
  set (opt_DEBUG "${opt_DEBUG} -Wno-unused-parameter")
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} ${opt_RELEASE}")
  set (CMAKE_Fortran_FLAGS_DEBUG "${basic} ${opt_DEBUG}")

#-
else ()
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  include(CheckFortranCompilerFlag)
  # collier will not compile without this flag, try to see if it is supported by
  # the compiler
  check_fortran_compiler_flag("-ffixed-line-length-132" _fll)
  if(_fll)
    set(basic ${basic} "-ffixed-line-length-132")
  endif()
  set (CMAKE_Fortran_FLAGS_RELEASE "${basic} -O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "${basic} -O0 -g")
endif ()

string(TOLOWER "${CMAKE_BUILD_TYPE}" CBT)
if (CBT MATCHES debug)
  message("Build type: Debug")
  message ("Collier:  Fortran compiler  = ${Fortran_COMPILER_NAME}")
  message ("          Fortran flags    = ${CMAKE_Fortran_FLAGS_DEBUG}" \n)
else()
  message("Build type: Release")
  set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS_RELEASE})
  message ("Collier:  Fortran compiler  = ${Fortran_COMPILER_NAME}")
  message ("          Fortran flags    = ${CMAKE_Fortran_FLAGS_RELEASE}" \n)
endif ()

#========================#
#  collier source files  #
#========================#

set(COLLIER_FILES "")
set(COLLIER_FILES ${COLLIER_FILES} src/collier_aux.F90)
set(COLLIER_FILES ${COLLIER_FILES} src/collier_coefs.F90)
set(COLLIER_FILES ${COLLIER_FILES} src/collier_global.F90)
set(COLLIER_FILES ${COLLIER_FILES} src/COLLIER.F90)
set(COLLIER_FILES ${COLLIER_FILES} src/collier_init.F90)
set(COLLIER_FILES ${COLLIER_FILES} src/collier_tensors.F90)

set(TENSOR_FILES "")
set(TENSOR_FILES ${TENSOR_FILES} src/tensors/InitTensors.F90)
set(TENSOR_FILES ${TENSOR_FILES} src/tensors/BuildTensors.F90)
set(TENSOR_FILES ${TENSOR_FILES} src/tensors/TensorReduction.F90)

set(AUX_FILES "")
SET(AUX_FILES ${AUX_FILES} src/Aux/Combinatorics.F90)
SET(AUX_FILES ${AUX_FILES} src/Aux/master.F90)
SET(AUX_FILES ${AUX_FILES} src/Aux/cache.F90)

set(COLI_FILES "")
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_stat.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/reductionAB.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_aux2.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/reductionC.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/reductionD.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/reductionEFG.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/reductionTN.F90)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_b0.F)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_d0.F)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_d0reg.F)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_aux.F)
SET(COLI_FILES ${COLI_FILES} src/COLI/coli_c0.F)

set(DDLIB_FILES "")
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_global.F90)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_aux.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_5pt.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_6pt.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_3pt.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_to_COLLIER.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_2pt.F)
SET(DDLIB_FILES ${DDLIB_FILES} src/DDlib/DD_4pt.F)

#=======================#
#  Merging the sources  #
#=======================#

set(SOURCE
    ${COLLIER_FILES}
    ${AUX_FILES}
    ${TENSOR_FILES}
    ${COLI_FILES}
    ${DDLIB_FILES}
)

#====================#
#  helper functions  #
#====================#

# excludes a target from the `make` command, i.e. the target is not built when
# invoking make, but can be built by invoking "make `target_name`".
function( _exclude_target target_name )
    set_target_properties(
        ${target_name} PROPERTIES
        EXCLUDE_FROM_DEFAULT_BUILD TRUE
        EXCLUDE_FROM_ALL TRUE
    )
endfunction()


#====================#
#    Output paths    #
#====================#

set(LIB_LOCAL_DIR ${PROJECT_SOURCE_DIR})
set(INCLUDE_LOCAL_DIR ${LIB_LOCAL_DIR}/modules)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${LIB_LOCAL_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${LIB_LOCAL_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${LIB_LOCAL_DIR})
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${LIB_LOCAL_DIR}/config)
set(CMAKE_Fortran_MODULE_DIRECTORY ${INCLUDE_LOCAL_DIR})


#===================#
#  Collier library  #
#===================#

option (static "shared_libraries" OFF)
if (static)
  message("Building COLLIER as static library")
  add_library (collier STATIC ${SOURCE})
else()
  message("Building COLLIER as shared library")
  add_library (collier SHARED ${SOURCE})
endif ()

#========================#
#  Collier installation  #
#========================#

# Define canonical relative paths
set(LIB_INSTALL_DIR lib CACHE PATH "Relative installation path for the library")
set(INCLUDE_INSTALL_DIR include CACHE PATH "Relative installation path for the headers") 
set(SYSCONFIG_INSTALL_DIR lib/cmake CACHE PATH "Relative installation path for the configs") 

# Make relative paths absolute (if not modified by user, needed later on)
if(NOT IS_ABSOLUTE "${LIB_INSTALL_DIR}")
  set(LIB_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}")
endif()
if(NOT IS_ABSOLUTE "${INCLUDE_INSTALL_DIR}")
  set(INCLUDE_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${INCLUDE_INSTALL_DIR}")
endif()

message("Collier install include prefix: ${INCLUDE_INSTALL_DIR}")
message("Collier install library prefix: ${LIB_INSTALL_DIR}")

set(COLLIER_HEADERS "${CMAKE_Fortran_MODULE_DIRECTORY}/collier.mod")
set_target_properties(collier PROPERTIES PUBLIC_HEADER "${COLLIER_HEADERS}")

include(PackageConfigInstall)
set(SYSCONFIG_LOCAL_DIR ${LIB_LOCAL_DIR}/config)
PACKAGE_CONFIG_INSTALL(collier ${COLLIER_VERSION})

#=========#
#  Demos  #
#=========#

add_executable(demo demos/demo.f90)
set_target_properties(demo PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${LIB_LOCAL_DIR}/demos)
target_link_libraries(demo collier)
_exclude_target(demo)

add_executable(democache demos/democache.f90)
set_target_properties(democache PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${LIB_LOCAL_DIR}/demos)
target_link_libraries(democache collier)
_exclude_target(democache)
