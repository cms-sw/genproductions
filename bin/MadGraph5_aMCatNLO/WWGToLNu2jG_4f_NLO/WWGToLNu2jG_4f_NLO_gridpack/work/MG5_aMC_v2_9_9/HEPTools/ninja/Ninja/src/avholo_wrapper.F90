! Wrapper of avh_olo routines with C bindings

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

!!quadninja!!#define NINJA_QUADRUPLE 1
#ifndef NINJA_QUADRUPLE
# define KI_NIN c_double
#else
# define KI_NIN 16
# ifndef NINJA_GOSAM
#  define AVH_ONSHELL_KIND(x) real(x,kind(1d0))
# endif
#endif

#ifndef AVH_ONSHELL_KIND
# define AVH_ONSHELL_KIND(x) x
#endif

      subroutine ninjavholo_onshell(thrs)  bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      real(KI_NIN), intent(in) :: thrs
      call olo_onshell(AVH_ONSHELL_KIND(thrs))
      end subroutine

      subroutine ninjavholo_d0_cm(rslt,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,rmu) &
           & bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      complex(KI_NIN), intent(in)  :: p1,p2,p3,p4,p12,p23,m1,m2,m3,m4
      real(KI_NIN), intent(in) :: rmu
      call olo_d0(rslt,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,rmu)
      end subroutine

      subroutine ninjavholo_d0_rm(rslt,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,rmu) &
           & bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      real(KI_NIN), intent(in)  :: p1,p2,p3,p4,p12,p23,m1,m2,m3,m4
      real(KI_NIN), intent(in) :: rmu
      call olo_d0(rslt,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,rmu)
      end subroutine

      subroutine ninjavholo_c0_cm(rslt,p1,p2,p3,m1,m2,m3,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      complex(KI_NIN), intent(in)  :: p1,p2,p3,m1,m2,m3
      real(KI_NIN), intent(in) :: rmu
      call olo_c0(rslt,p1,p2,p3,m1,m2,m3,rmu)
      end subroutine

      subroutine ninjavholo_c0_rm(rslt,p1,p2,p3,m1,m2,m3,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      real(KI_NIN), intent(in)  :: p1,p2,p3,m1,m2,m3
      real(KI_NIN), intent(in) :: rmu
      call olo_c0(rslt,p1,p2,p3,m1,m2,m3,rmu)
      end subroutine

      subroutine ninjavholo_b0_cm(rslt,pp,m1,m2,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      complex(KI_NIN), intent(in)  :: pp,m1,m2
      real(KI_NIN), intent(in) :: rmu
      call olo_b0(rslt,pp,m1,m2,rmu)
      end subroutine

      subroutine ninjavholo_b0_rm(rslt,pp,m1,m2,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      real(KI_NIN), intent(in)  :: pp,m1,m2
      real(KI_NIN), intent(in) :: rmu
      call olo_b0(rslt,pp,m1,m2,rmu)
      end subroutine

      subroutine ninjavholo_b11_cm(b11,b00,b1,b0,pp,m1,m2,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: b11,b00,b1,b0
      complex(KI_NIN), intent(in)  :: pp,m1,m2
      real(KI_NIN), intent(in) :: rmu
      call olo_b11(b11,b00,b1,b0,pp,m1,m2,rmu)
      end subroutine

      subroutine ninjavholo_b11_rm(b11,b00,b1,b0,pp,m1,m2,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: b11,b00,b1,b0
      real(KI_NIN), intent(in)  :: pp,m1,m2
      real(KI_NIN), intent(in) :: rmu
      call olo_b11(b11,b00,b1,b0,pp,m1,m2,rmu)
      end subroutine

      subroutine ninjavholo_a0_cm(rslt,mm,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      complex(KI_NIN), intent(in)  :: mm
      real(KI_NIN), intent(in) :: rmu
      call olo_a0(rslt,mm,rmu)
      end subroutine

      subroutine ninjavholo_a0_rm(rslt,mm,rmu) bind (c)
      use, intrinsic :: iso_c_binding
      use avh_olo
      implicit none
      complex(KI_NIN), dimension(0:2), intent(out) :: rslt
      real(KI_NIN), intent(in)  :: mm
      real(KI_NIN), intent(in) :: rmu
      call olo_a0(rslt,mm,rmu)
      end subroutine
