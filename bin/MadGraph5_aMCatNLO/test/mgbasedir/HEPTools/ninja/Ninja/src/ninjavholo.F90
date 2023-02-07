! Interface of ninja for Gosam

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

module ninjavholo
  use, intrinsic :: iso_c_binding
  implicit none

  ! precision
  public :: ki_nin

  ! subroutines
  public :: ninjavholo_set_ir_threshold
  public :: ninjavholo_clear_cache, ninjavholo_free_cache
  public :: ninjavholo_init
  public :: ninjavholo_get_mi4, ninjavholo_get_mi4_rm, ninjavholo_get_mi4_cm
  public :: ninjavholo_get_mi3, ninjavholo_get_mi3_rm, ninjavholo_get_mi3_cm
  public :: ninjavholo_get_mi2_rank2
  public :: ninjavholo_get_mi2_rank2_rm, ninjavholo_get_mi2_rank2_cm
  public :: ninjavholo_get_mi1, ninjavholo_get_mi1_rm, ninjavholo_get_mi1_cm

#ifndef NINJA_QUADRUPLE
# define KI_NIN c_double
#else
# define KI_NIN 16
#endif
  integer, parameter :: ki_nin = KI_NIN

  private

  interface
     subroutine ninjavholo_set_ir_threshold(threshold) &
          & bind(c, name='ninjavholo_set_ir_threshold_')
       use, intrinsic :: iso_c_binding
       implicit none       
       real(KI_NIN), intent(in) :: threshold
     end subroutine ninjavholo_set_ir_threshold
  end interface

  interface
     subroutine ninjavholo_clear_cache() &
          & bind(c, name='ninjavholo_clear_cache_')
       use, intrinsic :: iso_c_binding
       implicit none       
     end subroutine ninjavholo_clear_cache
  end interface

  interface
     subroutine ninjavholo_free_cache() &
          & bind(c, name='ninjavholo_free_cache_')
       use, intrinsic :: iso_c_binding
       implicit none       
     end subroutine ninjavholo_free_cache
  end interface

  interface
     subroutine ninjavholo_init(mur2) &
          & bind(c, name='ninjavholo_init_')
       use, intrinsic :: iso_c_binding
       implicit none       
       real(KI_NIN), intent(in) :: mur2
     end subroutine ninjavholo_init
  end interface


  interface ninjavholo_get_mi4

     subroutine ninjavholo_get_mi4_rm(rslt, s21, s32, s43, s14, s31, s42, &
          & m1, m2, m3, m4) &
          & bind(c, name='ninjavholo_get_mi4_rm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       real(KI_NIN), intent(in) :: s21, s32, s43, s14, s31, s42
       real(KI_NIN), intent(in) :: m1, m2, m3, m4
     end subroutine ninjavholo_get_mi4_rm

     subroutine ninjavholo_get_mi4_cm(rslt, s21, s32, s43, s14, s31, s42, &
          & m1, m2, m3, m4) &
          & bind(c, name='ninjavholo_get_mi4_cm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       real(KI_NIN), intent(in) :: s21, s32, s43, s14, s31, s42
       complex(KI_NIN), intent(in) :: m1, m2, m3, m4
     end subroutine ninjavholo_get_mi4_cm

  end interface ninjavholo_get_mi4


  interface ninjavholo_get_mi3

     subroutine ninjavholo_get_mi3_rm(rslt, s21, s32, s13, m1, m2, m3) &
          & bind(c, name='ninjavholo_get_mi3_rm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       real(KI_NIN), intent(in) :: s21, s32, s13
       real(KI_NIN), intent(in) :: m1, m2, m3
     end subroutine ninjavholo_get_mi3_rm

     subroutine ninjavholo_get_mi3_cm(rslt, s21, s32, s13, m1, m2, m3)&
          & bind(c, name='ninjavholo_get_mi3_cm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       real(KI_NIN), intent(in) :: s21, s32, s13
       complex(KI_NIN), intent(in) :: m1, m2, m3
     end subroutine ninjavholo_get_mi3_cm

  end interface ninjavholo_get_mi3


  interface ninjavholo_get_mi2_rank2

     subroutine ninjavholo_get_mi2_rank2_rm(b11, b1, b0, s21, m1, m2) &
          & bind(c, name='ninjavholo_get_mi2_rank2_rm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: b11, b1, b0
       real(KI_NIN), intent(in) :: s21
       real(KI_NIN), intent(in) :: m1, m2
     end subroutine ninjavholo_get_mi2_rank2_rm

     subroutine ninjavholo_get_mi2_rank2_cm(b11, b1, b0, s21, m1, m2) &
          & bind(c, name='ninjavholo_get_mi2_rank2_cm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: b11, b1, b0
       real(KI_NIN), intent(in) :: s21
       complex(KI_NIN), intent(in) :: m1, m2
     end subroutine ninjavholo_get_mi2_rank2_cm

  end interface ninjavholo_get_mi2_rank2


  interface ninjavholo_get_mi1

     subroutine ninjavholo_get_mi1_rm(rslt, m0)&
          & bind(c, name='ninjavholo_get_mi1_rm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       real(KI_NIN), intent(in) :: m0
     end subroutine ninjavholo_get_mi1_rm

     subroutine ninjavholo_get_mi1_cm(rslt, m0)&
          & bind(c, name='ninjavholo_get_mi1_cm_')
       use, intrinsic :: iso_c_binding
       implicit none
       complex(KI_NIN), dimension(0:2), intent(out) :: rslt
       complex(KI_NIN), intent(in) :: m0
     end subroutine ninjavholo_get_mi1_cm

  end interface ninjavholo_get_mi1


end module ninjavholo
