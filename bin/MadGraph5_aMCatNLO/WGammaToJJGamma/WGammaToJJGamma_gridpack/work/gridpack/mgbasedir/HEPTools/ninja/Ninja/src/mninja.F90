! f90 interface of ninja

! note: at the moment, this only exposes the methods for the reduction
! of tensor numerators

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
  
module mninja

  use, intrinsic :: iso_c_binding
  implicit none

  ! precision
  public :: ki_nin
#ifdef QUADNINJA
  public :: ki_qnin
#endif

  ! subroutines
  public :: ninja_tensor_evaluate
  public :: ninja_tensor_evaluate_rm, ninja_tensor_evaluate_smat_rm
  public :: ninja_tensor_evaluate_cm, ninja_tensor_evaluate_smat_cm
  public :: ninja_tensor_evaluate_nm, ninja_tensor_evaluate_smat_nm
#ifdef QUADNINJA
  public :: quadninja_tensor_evaluate_rm, quadninja_tensor_evaluate_smat_rm
  public :: quadninja_tensor_evaluate_cm, quadninja_tensor_evaluate_smat_cm
  public :: quadninja_tensor_evaluate_nm, quadninja_tensor_evaluate_smat_nm
#endif
  public :: ninja_clear_integral_cache, ninja_free_integral_cache
  public :: ninja_set_test, ninja_set_test_tolerance
  public :: ninja_set_verbosity, ninja_set_output_precision
  public :: ninja_set_integral_library

  ! parameter flags
  public :: NINJA_SUCCESS, NINJA_TEST_FAILED, NINJA_UNSTABLE_KINEMATICS
  public :: NINJA_ONELOOP, NINJA_LOOPTOOLS
  public :: NINJA_TEST_NONE
  public :: NINJA_TEST_ALL
  public :: NINJA_TEST_GLOBAL
  public :: NINJA_TEST_LOCAL_4
  public :: NINJA_TEST_LOCAL_3
  public :: NINJA_TEST_LOCAL_2
  public :: NINJA_TEST_LOCAL_1
  public :: NINJA_TEST_LOCAL
  public :: NINJA_OUTPUT_NONE
  public :: NINJA_OUTPUT_ALL
  public :: NINJA_OUTPUT_TEST_GLOBAL
  public :: NINJA_OUTPUT_TEST_LOCAL_4
  public :: NINJA_OUTPUT_TEST_LOCAL_3
  public :: NINJA_OUTPUT_TEST_LOCAL_2
  public :: NINJA_OUTPUT_TEST_LOCAL_1
  public :: NINJA_OUTPUT_TEST_LOCAL
  public :: NINJA_OUTPUT_TESTS
  public :: NINJA_OUTPUT_C5
  public :: NINJA_OUTPUT_C4
  public :: NINJA_OUTPUT_C3
  public :: NINJA_OUTPUT_C2
  public :: NINJA_OUTPUT_C1
  public :: NINJA_OUTPUT_COEFFS
  public :: NINJA_OUTPUT_RESULT
  public :: NINJA_OUTPUT_INTEGRALS

#ifndef NINJA_QUADRUPLE
# define KI_NIN c_double
#else
# define KI_NIN 16
#endif
#ifdef QUADNINJA
# define KI_QNIN 16
#endif
  
  integer, parameter :: ki_nin = KI_NIN
#ifdef QUADNINJA
  integer, parameter :: ki_qnin = KI_QNIN
#endif

  ! Return status parameters of ninja
  integer(c_int), parameter :: NINJA_SUCCESS = 0
  integer(c_int), parameter :: NINJA_TEST_FAILED = 1
  integer(c_int), parameter :: NINJA_UNSTABLE_KINEMATICS = 2 ! 1 << 1

  ! Flags corresponding to the Integral Libraries.  Used by the
  ! subrotine ninja_set_integral_library
  integer(c_int), parameter :: NINJA_ONELOOP = 1
  integer(c_int), parameter :: NINJA_LOOPTOOLS = 2

  ! Test flags
  integer(c_int), parameter :: NINJA_TEST_NONE = 0
  integer(c_int), parameter :: NINJA_TEST_ALL = -1
  integer(c_int), parameter :: NINJA_TEST_GLOBAL = 1
  integer(c_int), parameter :: NINJA_TEST_LOCAL_4 = 16  ! 1 << 4
  integer(c_int), parameter :: NINJA_TEST_LOCAL_3 = 8   ! 1 << 3
  integer(c_int), parameter :: NINJA_TEST_LOCAL_2 = 4   ! 1 << 2
  integer(c_int), parameter :: NINJA_TEST_LOCAL_1 = 2   ! 1 << 1
  integer(c_int), parameter :: NINJA_TEST_LOCAL = 30

  ! Verbosity flags
  integer(c_int), parameter :: NINJA_OUTPUT_NONE = 0
  integer(c_int), parameter :: NINJA_OUTPUT_ALL = -1
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_GLOBAL = 1
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_LOCAL_4 = 16  ! 1 << 4
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_LOCAL_3 = 8   ! 1 << 3
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_LOCAL_2 = 4   ! 1 << 2
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_LOCAL_1 = 2   ! 1 << 1
  integer(c_int), parameter :: NINJA_OUTPUT_TEST_LOCAL = 30
  integer(c_int), parameter :: NINJA_OUTPUT_TESTS = 31
  integer(c_int), parameter :: NINJA_OUTPUT_C5 = 521  ! 1 << (4+5)
  integer(c_int), parameter :: NINJA_OUTPUT_C4 = 256  ! 1 << (4+4)
  integer(c_int), parameter :: NINJA_OUTPUT_C3 = 128  ! 1 << (4+3)
  integer(c_int), parameter :: NINJA_OUTPUT_C2 = 64   ! 1 << (4+2)
  integer(c_int), parameter :: NINJA_OUTPUT_C1 = 32   ! 1 << (4+1)
  integer(c_int), parameter :: NINJA_OUTPUT_COEFFS = 992
  integer(c_int), parameter :: NINJA_OUTPUT_RESULT = 1024
  integer(c_int), parameter :: NINJA_OUTPUT_INTEGRALS = 2048

  private

  
  ! The routines in this interface evaluate tensor integrals of n
  ! internal legs and rank r, with a symmetric tensor numerator.  The
  ! tensor numerator is defined by a unidimensional array of
  ! coefficients, which must follow the convention of the
  ! representation of Eq. (C.15) in http://arxiv.org/abs/1405.0301,
  ! and is passed as input
  !
  ! The routines in this interface differ by the type of the internal
  ! masses (real, complex or null) and the presence of the s_mat input
  ! parameter
  !
  !
  ! The parameters are:
  !
  ! INPUT PARAMETERS:
  !
  ! tensor: unidimensional array with the entries of the symmetric
  ! tensor numerator, as explained above
  !
  ! n: number of loop denominators
  !
  ! r: rank of the numerator
  !
  ! pi and m2: momenta and squared masses of the loop denominators,
  ! defined as D(i) = (q + p(i))^2 - m2(i), for i=1,...,n
  !
  ! mur2: the square of the renormalization scale
  !
  ! s_mat: S-Matrix of invariants s_mat(i,j) = (p(i)-p(j))^2.  If
  ! provided, the master integrals might be more accurate and/or more
  ! likely to accurately detect infrared cases
  !
  ! OUTPUT PARAMETERS:
  !
  ! tot: result of the calculation, tot(i) = eps^-i term for i=0,1,2
  !
  ! totr: rational part of the result (this is already included in the
  ! total, hence typically not needed)
  !
  ! return_status: by default it will be NINJA_SUCCESS and can be
  ! safely ignored.  For debugging purposes, one can activate internal
  ! tests and if any of these fail, return_status will be
  ! NINJA_TEST_FAILED.
  interface ninja_tensor_evaluate

     ! Real masses without s_mat
     subroutine ninja_tensor_evaluate_rm(tensor,&
       & n, r,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_rm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       real(KI_NIN), dimension(*), intent(in) :: m2
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_rm

     ! Complex masses without s_mat
     subroutine ninja_tensor_evaluate_cm(tensor,&
       & n, r,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_cm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       complex(KI_NIN), dimension(*), intent(in) :: m2
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_cm

     ! Massless loop without s_mat
     subroutine ninja_tensor_evaluate_nm(tensor,&
       & n, r,&
            & pi, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_nm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_nm

     ! Real masses with s_mat
     subroutine ninja_tensor_evaluate_smat_rm(tensor,&
       & n, r, s_mat,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_smat_rm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(n,n), intent(in) :: s_mat
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       real(KI_NIN), dimension(*), intent(in) :: m2
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_smat_rm

     ! Complex masses with s_mat
     subroutine ninja_tensor_evaluate_smat_cm(tensor,&
       & n, r, s_mat,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_smat_cm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(n,n), intent(in) :: s_mat
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       complex(KI_NIN), dimension(*), intent(in) :: m2
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_smat_cm

     ! Massless loop with s_mat
     subroutine ninja_tensor_evaluate_smat_nm(tensor,&
       & n, r, s_mat,&
            & pi, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="ninja_tensor_evaluate_smat_nm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_NIN), dimension(n,n), intent(in) :: s_mat
       real(KI_NIN), dimension(0:3,*), intent(in) :: pi
       real(KI_NIN), intent(in) :: mur2
       complex(KI_NIN), dimension(0:2), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine ninja_tensor_evaluate_smat_nm

#if QUADNINJA
     ! Real masses without s_mat
     subroutine quadninja_tensor_evaluate_rm(tensor,&
       & n, r,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_rm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       real(KI_QNIN), dimension(*), intent(in) :: m2
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_rm

     ! Complex masses without s_mat
     subroutine quadninja_tensor_evaluate_cm(tensor,&
       & n, r,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_cm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       complex(KI_QNIN), dimension(*), intent(in) :: m2
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_cm

     ! Massless loop without s_mat
     subroutine quadninja_tensor_evaluate_nm(tensor,&
       & n, r,&
            & pi, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_nm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_nm

     ! Real masses with s_mat
     subroutine quadninja_tensor_evaluate_smat_rm(tensor,&
       & n, r, s_mat,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_smat_rm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(n,n), intent(in) :: s_mat
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       real(KI_QNIN), dimension(*), intent(in) :: m2
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_smat_rm

     ! Complex masses with s_mat
     subroutine quadninja_tensor_evaluate_smat_cm(tensor,&
       & n, r, s_mat,&
            & pi, m2, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_smat_cm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(n,n), intent(in) :: s_mat
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       complex(KI_QNIN), dimension(*), intent(in) :: m2
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_smat_cm

     ! Massless loop with s_mat
     subroutine quadninja_tensor_evaluate_smat_nm(tensor,&
       & n, r, s_mat,&
            & pi, mur2,&
            & tot, totr, return_status) &
            & bind(c, name="quadninja_tensor_evaluate_smat_nm_")
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_QNIN), dimension(*), intent(in):: tensor
       integer(c_int), intent(in) :: n, r
       real(KI_QNIN), dimension(n,n), intent(in) :: s_mat
       real(KI_QNIN), dimension(0:3,*), intent(in) :: pi
       real(KI_QNIN), intent(in) :: mur2
       complex(KI_QNIN), dimension(0:2), intent(out) :: tot
       complex(KI_QNIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_tensor_evaluate_smat_nm     
#endif

  end interface ninja_tensor_evaluate


  ! This clears the internal cache of Master Integrals in ninja (or
  ! LoopTools if that is used).  We suggesting calling this once per
  ! phase-space point during a phase-space integration.
  interface
     subroutine ninja_clear_integral_cache() &
          & bind (c,name='ninja_clear_integral_cache_')
     end subroutine ninja_clear_integral_cache
  end interface

  ! This completely frees the memory allocated by the internal cache
  ! of Master Integrals in the Ninja interface of OneLoop (note: you
  ! usually don't want to call this ever, call
  ! ninja_clear_integral_cache() instead, unless you're not computing
  ! integrals anymore).
  interface
     subroutine ninja_free_integral_cache() &
          & bind (c,name='ninja_free_integral_cache_')
     end subroutine ninja_free_integral_cache
  end interface

  interface
     subroutine ninja_set_verbosity(verbosity) &
          bind (c,name='ninja_set_verbosity_')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in) :: verbosity
     end subroutine ninja_set_verbosity
  end interface

  interface
     subroutine ninja_set_test(val) &
          bind (c,name='ninja_set_test_')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in) :: val
     end subroutine ninja_set_test
  end interface

  interface
     subroutine ninja_set_test_tolerance(val) &
          bind (c,name='ninja_set_test_tolerance_')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: val
     end subroutine ninja_set_test_tolerance
  end interface

  interface
     subroutine ninja_set_output_precision(val) &
          bind (c,name='ninja_set_output_precision_')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in) :: val
     end subroutine ninja_set_output_precision
  end interface

  interface
     subroutine ninja_set_integral_library(libflag) &
          bind (c,name='ninja_set_integral_library_')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in) :: libflag
     end subroutine ninja_set_integral_library
  end interface

  interface
     subroutine ninja_set_floating_point_threshold(thr) &
          bind (c,name='ninja_set_floating_point_threshold_')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: thr
     end subroutine ninja_set_floating_point_threshold
  end interface

#if QUADNINJA
  interface
     subroutine quadninja_set_floating_point_threshold(thr) &
          bind (c,name='quadninja_set_floating_point_threshold_')
       use, intrinsic :: iso_c_binding
       real(KI_QNIN), intent(in) :: thr
     end subroutine quadninja_set_floating_point_threshold
  end interface
#endif
  
end module mninja
