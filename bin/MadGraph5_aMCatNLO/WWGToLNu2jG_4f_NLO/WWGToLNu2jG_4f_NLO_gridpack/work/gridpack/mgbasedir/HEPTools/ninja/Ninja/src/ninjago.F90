! Interface of ninja for Gosam

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

module ninjago_module
  use, intrinsic :: iso_c_binding
  implicit none

  ! precision
  public :: ki_nin

  ! subroutines
  public :: ninja_diagram, ninja_diagram_rm, ninja_diagram_cm, ninja_diagram_nm
  public :: ninja_clear_integral_cache, ninja_free_integral_cache
  public :: ninja_set_test, ninja_set_test_tolerance
  public :: ninja_set_verbosity, ninja_set_output_precision
  public :: ninja_set_integral_library
  public :: ninja_fp_check_default_threshold, ninja_fp_check_threshold
  public :: ninja_fp_check

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
  integer, parameter :: ki_nin = KI_NIN

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

  ! The kinematics must be passed as a multidimensional array
  ! Vi(0:3,nprops).
  !
  ! S-matrix is a 2-dimensional array s_mat where S(i,j) =
  ! (Vi(i)-Vi(j))^2.
  interface  ninja_diagram

     ! Interface for real masses
     subroutine ninja_diagram_rm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, msq, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="ninjago_diag_rm")
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
     end subroutine ninja_diagram_rm

     ! Interface for complex masses
     subroutine ninja_diagram_cm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, msq, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="ninjago_diag_cm")
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
     end subroutine ninja_diagram_cm

     ! Interface for massless loop-propagators (the mass argumet 'msq'
     ! is omitted)
     subroutine ninja_diagram_nm(numerator, numerator_t3, numerator_t2,&
          & numerator_d, &
          & nprops_group, nprops, rk, subset,&
          & Vi, s_mat, scale2, istop,&
          & tot, totr, return_status) &
          & bind(c, name="ninjago_diag_nm")
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
     end subroutine ninja_diagram_nm

  end interface ninja_diagram


  ! This clears the internal cache of Master Integrals in ninja.
  interface
     subroutine ninja_clear_integral_cache() &
          & bind (c,name='ninjago_clear_integral_cache')
     end subroutine ninja_clear_integral_cache
  end interface

  ! This completely frees the memory allocated by the internal cache
  ! of Master Integrals in the Ninja interface of OneLoop (note: you
  ! usually don't want to call this, call ninja_clear_integral_cache()
  ! instead, unless you're not computing integrals anymore).
  interface
     subroutine ninja_free_integral_cache() &
          & bind (c,name='ninjago_free_integral_cache')
     end subroutine ninja_free_integral_cache
  end interface

  interface
     subroutine ninja_set_verbosity(verbosity) &
          bind (c,name='ninjago_set_verbosity')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: verbosity
     end subroutine ninja_set_verbosity
  end interface

  interface
     subroutine ninja_set_test(val) &
          bind (c,name='ninjago_set_test')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: val
     end subroutine ninja_set_test
  end interface

  interface
     subroutine ninja_set_test_tolerance(val) &
          bind (c,name='ninjago_set_test_tolerance')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: val
     end subroutine ninja_set_test_tolerance
  end interface

  interface
     subroutine ninja_set_output_precision(val) &
          bind (c,name='ninjago_set_output_precision')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: val
     end subroutine ninja_set_output_precision
  end interface

  interface
     subroutine ninja_set_integral_library(libflag) &
          bind (c,name='ninjago_set_integral_library')
       use, intrinsic :: iso_c_binding
       integer(c_int), intent(in), value :: libflag
     end subroutine ninja_set_integral_library
  end interface

  interface ninja_fp_check
     
     subroutine ninja_fp_check_default_threshold() &
          bind (c,name='ninjago_fp_check_default_threshold')
       use, intrinsic :: iso_c_binding
     end subroutine ninja_fp_check_default_threshold

     subroutine ninja_fp_check_threshold(val) &
          bind (c,name='ninjago_fp_check_threshold')
       use, intrinsic :: iso_c_binding
       real(KI_NIN), intent(in) :: val
     end subroutine ninja_fp_check_threshold
     
  end interface ninja_fp_check

end module ninjago_module
