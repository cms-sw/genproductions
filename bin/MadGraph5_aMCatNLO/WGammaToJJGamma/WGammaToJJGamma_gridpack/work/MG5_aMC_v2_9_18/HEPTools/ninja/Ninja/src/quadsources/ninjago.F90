! Interface of ninja for Gosam

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define QUADNINJA_QUADRUPLE 1
#ifndef QUADNINJA_QUADRUPLE
# define KI_NIN c_double
#else
# define KI_NIN 16
#endif

module quadninjago_module
  use, intrinsic :: iso_c_binding
  implicit none

  ! precision
  public :: ki_nin

  ! subroutines
  public :: quadninja_diagram
  public :: quadninja_diagram_rm
  public :: quadninja_diagram_cm
  public :: quadninja_diagram_nm
  public :: quadninja_clear_integral_cache
  public :: quadninja_free_integral_cache
  public :: quadninja_set_test
  public :: quadninja_set_test_tolerance
  public :: quadninja_set_verbosity
  public :: quadninja_set_output_precision
  public :: quadninja_set_integral_library
  public :: quadninja_fp_check_default_threshold
  public :: quadninja_fp_check_threshold
  public :: quadninja_fp_check

  ! parameter flags
  public :: QUADNINJA_SUCCESS, QUADNINJA_TEST_FAILED, QUADNINJA_UNSTABLE_KINEMATICS
  public :: QUADNINJA_ONELOOP, QUADNINJA_LOOPTOOLS
  public :: QUADNINJA_TEST_NONE
  public :: QUADNINJA_TEST_ALL
  public :: QUADNINJA_TEST_GLOBAL
  public :: QUADNINJA_TEST_LOCAL_4
  public :: QUADNINJA_TEST_LOCAL_3
  public :: QUADNINJA_TEST_LOCAL_2
  public :: QUADNINJA_TEST_LOCAL_1
  public :: QUADNINJA_TEST_LOCAL
  public :: QUADNINJA_OUTPUT_NONE
  public :: QUADNINJA_OUTPUT_ALL
  public :: QUADNINJA_OUTPUT_TEST_GLOBAL
  public :: QUADNINJA_OUTPUT_TEST_LOCAL_4
  public :: QUADNINJA_OUTPUT_TEST_LOCAL_3
  public :: QUADNINJA_OUTPUT_TEST_LOCAL_2
  public :: QUADNINJA_OUTPUT_TEST_LOCAL_1
  public :: QUADNINJA_OUTPUT_TEST_LOCAL
  public :: QUADNINJA_OUTPUT_TESTS
  public :: QUADNINJA_OUTPUT_C5
  public :: QUADNINJA_OUTPUT_C4
  public :: QUADNINJA_OUTPUT_C3
  public :: QUADNINJA_OUTPUT_C2
  public :: QUADNINJA_OUTPUT_C1
  public :: QUADNINJA_OUTPUT_COEFFS
  public :: QUADNINJA_OUTPUT_RESULT
  public :: QUADNINJA_OUTPUT_INTEGRALS

  integer, parameter :: ki_nin = KI_NIN

  ! Return status parameters of ninja
  integer(c_int), parameter :: QUADNINJA_SUCCESS = 0
  integer(c_int), parameter :: QUADNINJA_TEST_FAILED = 1
  integer(c_int), parameter :: QUADNINJA_UNSTABLE_KINEMATICS = 2 ! 1 << 1

  ! Flags corresponding to the Integral Libraries.  Used by the
  ! subrotine ninja_set_integral_library
  integer(c_int), parameter :: QUADNINJA_ONELOOP = 1
  integer(c_int), parameter :: QUADNINJA_LOOPTOOLS = 2

  ! Test flags
  integer(c_int), parameter :: QUADNINJA_TEST_NONE = 0
  integer(c_int), parameter :: QUADNINJA_TEST_ALL = -1
  integer(c_int), parameter :: QUADNINJA_TEST_GLOBAL = 1
  integer(c_int), parameter :: QUADNINJA_TEST_LOCAL_4 = 16  ! 1 << 4
  integer(c_int), parameter :: QUADNINJA_TEST_LOCAL_3 = 8   ! 1 << 3
  integer(c_int), parameter :: QUADNINJA_TEST_LOCAL_2 = 4   ! 1 << 2
  integer(c_int), parameter :: QUADNINJA_TEST_LOCAL_1 = 2   ! 1 << 1
  integer(c_int), parameter :: QUADNINJA_TEST_LOCAL = 30

  ! Verbosity flags
  integer(c_int), parameter :: QUADNINJA_OUTPUT_NONE = 0
  integer(c_int), parameter :: QUADNINJA_OUTPUT_ALL = -1
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_GLOBAL = 1
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_LOCAL_4 = 16  ! 1 << 4
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_LOCAL_3 = 8   ! 1 << 3
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_LOCAL_2 = 4   ! 1 << 2
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_LOCAL_1 = 2   ! 1 << 1
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TEST_LOCAL = 30
  integer(c_int), parameter :: QUADNINJA_OUTPUT_TESTS = 31
  integer(c_int), parameter :: QUADNINJA_OUTPUT_C5 = 521  ! 1 << (4+5)
  integer(c_int), parameter :: QUADNINJA_OUTPUT_C4 = 256  ! 1 << (4+4)
  integer(c_int), parameter :: QUADNINJA_OUTPUT_C3 = 128  ! 1 << (4+3)
  integer(c_int), parameter :: QUADNINJA_OUTPUT_C2 = 64   ! 1 << (4+2)
  integer(c_int), parameter :: QUADNINJA_OUTPUT_C1 = 32   ! 1 << (4+1)
  integer(c_int), parameter :: QUADNINJA_OUTPUT_COEFFS = 992
  integer(c_int), parameter :: QUADNINJA_OUTPUT_RESULT = 1024
  integer(c_int), parameter :: QUADNINJA_OUTPUT_INTEGRALS = 2048

  private

  ! The kinematics must be passed as a multidimensional array
  ! Vi(0:3,nprops).
  !
  ! S-matrix is a 2-dimensional array s_mat where S(i,j) =
  ! (Vi(i)-Vi(j))^2.
  interface quadninja_diagram

     ! Interface for real masses
     subroutine quadninja_diagram_rm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, msq, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="quadninjago_diag_rm")
       use, intrinsic :: iso_c_binding
       implicit none
       interface
          subroutine numerator(ncut, Q, mu2,num) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: Q
            complex(KI_NIN), intent(in) :: mu2
            complex(KI_NIN), intent(out) :: num
          end subroutine numerator
       end interface
       interface
          subroutine numerator_t3(ncut, a, b, c, param, ndeg, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a, b, c
            complex(KI_NIN), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t3
       end interface
       interface
          subroutine numerator_t2(ncut, a0, a1, b, c, param, ndeg, coeffs) &
               & bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a0, a1, b, c
            complex(KI_NIN), dimension(0:2), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t2
       end interface
       interface
          subroutine numerator_d(ncut, a, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3,*), intent(in) :: a
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_d
       end interface
       integer(c_int), intent(in) :: nprops_group
       integer(c_int), intent(in) :: nprops
       real(KI_NIN), dimension(0:3,*) :: Vi
       real(KI_NIN), dimension(*) :: msq
       real(KI_NIN), dimension(nprops_group,nprops_group), &
            & intent(in) :: s_mat
       integer(c_int), intent(in) :: rk
       integer(c_int), intent(in) :: istop
       real(KI_NIN), intent(in) :: scale2
       integer(c_int), intent(in), dimension(*) :: subset
       complex(KI_NIN), dimension(-2:0), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_diagram_rm

     ! Interface for complex masses
     subroutine quadninja_diagram_cm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, msq, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="quadninjago_diag_cm")
       use, intrinsic :: iso_c_binding
       implicit none
       interface
          subroutine numerator(ncut, Q, mu2,num) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: Q
            complex(KI_NIN), intent(in) :: mu2
            complex(KI_NIN), intent(out) :: num
          end subroutine numerator
       end interface
       interface
          subroutine numerator_t3(ncut, a, b, c, param, ndeg, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a, b, c
            complex(KI_NIN), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t3
       end interface
       interface
          subroutine numerator_t2(ncut, a0, a1, b, c, param, ndeg, coeffs) &
               & bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a0, a1, b, c
            complex(KI_NIN), dimension(0:2), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t2
       end interface
       interface
          subroutine numerator_d(ncut, a, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3,*), intent(in) :: a
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_d
       end interface
       integer(c_int), intent(in) :: nprops_group
       integer(c_int), intent(in) :: nprops
       real(KI_NIN), dimension(0:3,*) :: Vi
       complex(KI_NIN), dimension(*) :: msq
       real(KI_NIN), dimension(nprops_group,nprops_group), &
            & intent(in) :: s_mat
       integer(c_int), intent(in) :: rk
       integer(c_int), intent(in) :: istop
       real(KI_NIN), intent(in) :: scale2
       integer(c_int), intent(in), dimension(*) :: subset
       complex(KI_NIN), dimension(-2:0), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_diagram_cm

     ! Interface for massless loop-propagators (the mass argumet 'msq'
     ! is omitted)
     subroutine quadninja_diagram_nm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="quadninjago_diag_nm")
       use, intrinsic :: iso_c_binding
       implicit none
       interface
          subroutine numerator(ncut, Q, mu2,num) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: Q
            complex(KI_NIN), intent(in) :: mu2
            complex(KI_NIN), intent(out) :: num
          end subroutine numerator
       end interface
       interface
          subroutine numerator_t3(ncut, a, b, c, param, ndeg, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a, b, c
            complex(KI_NIN), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t3
       end interface
       interface
          subroutine numerator_t2(ncut, a0, a1, b, c, param, ndeg, coeffs) &
               & bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3), intent(in) :: a0, a1, b, c
            complex(KI_NIN), dimension(0:2), intent(in) :: param
            integer(c_int), intent(in) :: ndeg
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_t2
       end interface
       interface
          subroutine numerator_d(ncut, a, coeffs) bind(c)
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int), intent(in) :: ncut
            complex(KI_NIN), dimension(0:3,*), intent(in) :: a
            complex(KI_NIN), dimension(0:*), intent(out):: coeffs
          end subroutine numerator_d
       end interface
       integer(c_int), intent(in) :: nprops_group
       integer(c_int), intent(in) :: nprops
       real(KI_NIN), dimension(0:3,*) :: Vi
       real(KI_NIN), dimension(nprops_group,nprops_group), &
            & intent(in) :: s_mat
       integer(c_int), intent(in) :: rk
       integer(c_int), intent(in) :: istop
       real(KI_NIN), intent(in) :: scale2
       integer(c_int), intent(in), dimension(*) :: subset
       complex(KI_NIN), dimension(-2:0), intent(out) :: tot
       complex(KI_NIN), intent(out) :: totr
       integer(c_int), intent(out) :: return_status
     end subroutine quadninja_diagram_nm

  end interface quadninja_diagram


  ! This clears the internal cache of Master Integrals in ninja.
  interface
     subroutine quadninja_clear_integral_cache() &
          & bind (c,name='quadninjago_clear_integral_cache')
     end subroutine quadninja_clear_integral_cache
  end interface

  ! This completely frees the memory allocated by the internal cache
  ! of Master Integrals in the Ninja interface of OneLoop (note: you
  ! usually don't want to call this, call ninja_clear_integral_cache()
  ! instead, unless you're not computing integrals anymore).
  interface
     subroutine quadninja_free_integral_cache() &
          & bind (c,name='quadninjago_free_integral_cache')
     end subroutine quadninja_free_integral_cache
  end interface

  interface
     subroutine quadninja_set_verbosity(verbosity) &
          bind (c,name='quadninjago_set_verbosity')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: verbosity
     end subroutine quadninja_set_verbosity
  end interface

  interface
     subroutine quadninja_set_test(val) &
          bind (c,name='quadninjago_set_test')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: val
     end subroutine quadninja_set_test
  end interface

  interface
     subroutine quadninja_set_test_tolerance(val) &
          bind (c,name='quadninjago_set_test_tolerance')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: val
     end subroutine quadninja_set_test_tolerance
  end interface

  interface
     subroutine quadninja_set_output_precision(val) &
          bind (c,name='quadninjago_set_output_precision')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: val
     end subroutine quadninja_set_output_precision
  end interface

  interface
     subroutine quadninja_set_integral_library(libflag) &
          bind (c,name='quadninjago_set_integral_library')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: libflag
     end subroutine quadninja_set_integral_library
  end interface

  interface quadninja_fp_check

     subroutine quadninja_fp_check_default_threshold() &
          bind (c,name='quadninjago_fp_check_default_threshold')
       use, intrinsic :: iso_c_binding
     end subroutine quadninja_fp_check_default_threshold

     subroutine quadninja_fp_check_threshold(val) &
          bind (c,name='quadninjago_fp_check_threshold')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: val
     end subroutine quadninja_fp_check_threshold

  end interface quadninja_fp_check

end module quadninjago_module
