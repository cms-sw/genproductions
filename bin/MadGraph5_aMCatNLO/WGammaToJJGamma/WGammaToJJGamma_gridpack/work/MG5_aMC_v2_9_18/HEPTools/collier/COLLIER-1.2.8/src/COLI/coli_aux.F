!!
!!  File coli_aux.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

#include "global_coli.h"

c#define WIB
# define ERREST

************************************************************************
*                                                                      *
*     auxiliary functions for COLI library                             *
*                                                                      *
************************************************************************
*                                                                      *
*     last changed  09.11.12  Ansgar Denner                            *
*     errorflags    22.05.13  Ansgar Denner                            *
*                   04.06.13  chdet, chinv moved here                  *
*                   30.03.15  irratterms included                      *
*                   24.01.19  error included in chinv(e)               *
*                                                                      *
************************************************************************
* subroutines:                                                         *
* initcoli,initcoli_in_collier                                         *
* setmuuv2_coli, setmuir2_coli, setdeltauv_coli, setdeltair_coli       *
* getmuuv2_coli, getmuir2_coli, getdeltauv_coli, getdeltair_coli       *
* setshiftms2_coli, getshiftms2_coli                                   *
* setminf2_coli, clearcoliminf2                                        *
* setminfscale2_coli, getminfscale2_coli, modminfscale2_coli           *
* setirratterms_coli, unsetirratterms_coli                             *
* setprecpars_coli                                                     *
* setinfo_coli, unsetinfo_coli                                         *
* chinv,chinve                                                         *
* functions:                                                           *
* cln_coli, cspenc_coli, cspenh_coli, cspcon_coli, cspcos_coli         *
* csp2con_coli, csp2cos_coli, csp3con_coli, csp4con_coli               *
* eta2_coli, eta2s_coli                                                *
* chdet                                                                *
* minfscaledown_coli, minfscaledown2_coli                              *
* elimminf_coli, elimminf2_coli                                        *
************************************************************************
#ifndef ADcode
      subroutine initcoli_in_collier_save
************************************************************************
* fixing of default values for various variables                       *
*----------------------------------------------------------------------*
* 28.08.08  Ansgar Denner         last changed  26.09.11               *
************************************************************************

      use coli_aux2

      implicit   none
      real*8     calacc,impacc
      integer    sym

      common /impacc/ impacc
      common /calacc/ calacc
      common /sym/    sym

      logical    init
      data init /.false./
      save init

      if (init) return
      init = .true.

c print no information messages  (done by COLLIER)
      call unsetinfo_coli
c mu^2 of dimensional regularization in uv-divergent loop integrals
c     call setmuuv2_coli(1d0)
c photon/gluon mass in ir-divergent loop integrals
c mu^2 of dimensional regularization in ir-divergent loop integrals
c     call setmuir2_coli(1d0)
c scale factor for mass-singular masses
      call setminfscale2_coli(1d0)
#ifdef SING
c uv-divergence 2/(4-D)
c     call setdeltauv_coli(0d0)
c ir-divergence 2/(4-D)
c     call setdeltair_coli(0d0,0d0)
c extra term for mass-singular squared logarithms
      call setshiftms2_coli(0d0)
#endif
#ifdef ADcode
c should be set to the accuracy of the computer: n digits -> 1d-n
      calacc = dprec_coli
c size of imaginary parts below which explicit ieps take over
      impacc = dprec_coli/4d4
#endif
c include IR rational terms in derivatives of 2-point functions
      call setIRRatTerms_coli
c sym = 1 calculation of symmetric tensor coefficients
c sym = 0 no calculation of symmetric tensor coefficients
c     sym = 0


      end

************************************************************************
      subroutine setprecpars_coli(dprec)
************************************************************************
* fixing of internal precision parameters of COLI                      *
*----------------------------------------------------------------------*
* 13.07.15  Ansgar Denner         last changed  13.07.14               *
************************************************************************
      use coli_aux2

      implicit   none
      real*8     dprec,calacc,impacc

      common /impacc/ impacc
      common /calacc/ calacc

c should be set to the accuracy of the computer: n digits -> 1d-n
      calacc = dprec
c size of imaginary parts below which explicit ieps take over
      impacc = dprec/4d4

      end
#endif

************************************************************************
      subroutine initcoli
************************************************************************
* fixing of default values for various variables                       *
*----------------------------------------------------------------------*
* 28.08.08  Ansgar Denner         last changed  26.09.11               *
************************************************************************
      implicit   none
      real*8     eps,calacc,impacc
      integer    sym

      common /impacc/ impacc
      common /calacc/ calacc
      common /sym/    sym


      logical    init
      data init /.false./
      save init

      if (init) return
      init = .true.
      print *,'======================================================='
      print *,' COLI 1.0beta: a package to evaluate one-loop integrals'
      print *,'written by A. Denner, PSI, Villigen, Switzerland'
      print *,'======================================================='
      print *,'THIS CODE IS PRELIMINARY!!!'
      print *,'There is no guarantee that the results are correct.'
      print *,'======================================================='
      print *,'If you use the code, you must refer to it'
      print *,'and cite the papers it is based on.'
      print *,'A. Denner and S. Dittmaier, Nucl.Phys.B734:62-115,2006'
      print *,'A. Denner and S. Dittmaier, Nucl.Phys.B658:175-202,2003'
      print *,'  for the tensor integral reduction,'
      print *,'A. Denner, U. Nierste, and R. Scharf,'
      print *,'Nucl.Phys.B367:637-656,1991'
      print *,'A. Denner and W. Beenakker, Nucl.Phys.B338:349-370,1990'
      print *,'  for the scalar integrals.'
      print *,'========================================================'


c print information messages
      call setinfo_coli
c mu^2 of dimensional regularization in uv-divergent loop integrals
      call setmuuv2_coli(1d0)
c photon/gluon mass in ir-divergent loop integrals
c mu^2 of dimensional regularization in ir-divergent loop integrals
      call setmuir2_coli(1d0)
c scale factor for mass-singular masses
      call setminfscale2_coli(1d0)
#ifdef SING
c uv-divergence 2/(4-D)
      call setdeltauv_coli(0d0)
c ir-divergence 2/(4-D)
      call setdeltair_coli(0d0,0d0)
c extra term for mass-singular squared logarithms
      call setshiftms2_coli(0d0)
#endif
c infinitesimal parameter
      eps    = 1d-20
c size of imaginary parts below which explicit ieps take over
      impacc = 1d-16
c should be set to the accuracy of the computer: n digits -> 1d-n
      calacc = 5d-16

c sym = 1 calculation of symmetric tensor coefficients
c sym = 0 no calculation of symmetric tensor coefficients
c     sym = 0




      end

************************************************************************
      function cln_fast_coli(z)
************************************************************************
*     complex logarithm of z                                           *
*----------------------------------------------------------------------*
*     02.06.20 Alexander Voigt                                         *
************************************************************************
      implicit   none
      real*8     norm2,arg,re,im
      complex*16 cln_fast_coli,z

      re = real(z)
      im = aimag(z)

      norm2 = re*re + im*im
      arg = datan2(im, re)

      cln_fast_coli = dcmplx(0.5d0*log(norm2), arg)

      end

************************************************************************
      function cln_coli(cz,eps)
************************************************************************
*     complex logarithm of cz + i*eps                                  *
*----------------------------------------------------------------------*
*     09.01.90 Ansgar Denner         last changed 15.03.04             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     eps
      complex*16 cln_coli,cz,cln_fast_coli
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"



#ifdef CHECK
      if(argcheck) then
         if(abs(aimag(cz)).lt.impacc*abs(real(cz)).and.
     &        real(cz).le.0d0.and.eps.eq.0d0.or.
     &        abs(cz).eq.0d0) then
            call setErrFlag_coli(-4)
            call ErrOut_coli('cln_coli', 'argument on cut',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*) 'cln_coli:  argument on cut '
              write(nerrout_coli,*) 'cln_coli:  eps = ',eps
              write(nerrout_coli,*) 'cln_coli:  cz  = ',cz
            endif
         endif
      endif
#endif

      if(abs(aimag(cz)).gt.impacc*abs(real(cz)).or.real(cz).gt.0d0)
     &    then
        cln_coli=cln_fast_coli(cz)
      elseif(eps.ne.0) then
        cln_coli=cln_fast_coli(-cz)+dcmplx(0d0,pi)*sign(1d0,eps)
      elseif(aimag(cz).ne.0d0) then
        cln_coli=cln_fast_coli(cz)
        call setErrFlag_coli(-1)
        call ErrOut_coli('cln_coli', 'argument effectively on cut',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*)
     &         'cln_coli:  argument effectively on cut '
          write(nerrout_coli,*) 'cln_coli:  eps = ',eps
          write(nerrout_coli,*) 'cln_coli:  cz  = ',cz
        endif
      else
        cln_coli=cln_fast_coli(-cz)+dcmplx(0d0,pi)
        call setErrFlag_coli(-4)
        call ErrOut_coli('cln_coli', 'argument on cut',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*) 'cln_coli:  argument on cut '
          write(nerrout_coli,*) 'cln_coli:  eps = ',eps
          write(nerrout_coli,*) 'cln_coli:  cz  = ',cz
        endif
      endif

      end
************************************************************************
      function cspenc_coli(cz,eps)
************************************************************************
*       complex spence function  of cz + i*eps                         *
*       calculated by mapping on the area where there is a quickly     *
*       convergent series                                              *
*----------------------------------------------------------------------*
*     08.01.90 Ansgar Denner        last changed  3.02.97 ad           *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     eps
      complex*16 cspenc_coli,cz
      real*8     az,rz,az1
      complex*16 cz1,cspenh_coli,cln_coli
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"

#ifdef CHECK
      if(argcheck) then
         if(real(cz).gt.1d0.and.
     &        abs(aimag(cz)).lt.impacc*abs(real(cz)).and.eps.eq.0)then
           call setErrFlag_coli(-4)
           call ErrOut_coli('cspenc_coli', 'argument on cut',
     &          errorwriteflag)
           if (errorwriteflag) then
             write(nerrout_coli,*) 'cspen_coli:  argument on cut '
             write(nerrout_coli,*) 'cspen_coli:  eps = ',eps
             write(nerrout_coli,*) 'cspen_coli:  cz  = ',cz
           endif
         endif
      endif
#endif

      cz1     = 1d0-cz
      az1     = abs(cz1)
      az      = abs(cz)
      rz      = real(cz)


      if (az1.lt.calacc) then
         cspenc_coli = pi2_6
      else if (rz.lt.0.5d0) then
         if (az.lt.1d0) then
            cspenc_coli =  cspenh_coli(cz,eps)
         else
            cspenc_coli = -pi2_6 - .5d0*cln_coli(-cz,-eps)**2
     &                -  cspenh_coli(1d0/cz,-eps)
         endif
      else
         if (az1.lt.1d0) then
            cspenc_coli =  pi2_6 - cln_coli(cz,eps)*cln_coli(cz1,-eps)
     &                       -  cspenh_coli(cz1,-eps)
         else
            cspenc_coli = 2d0*pi2_6 + .5d0*cln_coli(-cz1,-eps)**2
     &              - cln_coli(cz,eps)*cln_coli(cz1,-eps)
     &              +  cspenh_coli(1d0/cz1,eps)
         endif
      endif
      end

************************************************************************
      function cspenh_coli(cz,eps)
************************************************************************
*       complex spence function of cz + i*eps                          *
*       in convergence region                                          *
*       calculation of bernoulli series                                *
*----------------------------------------------------------------------*
*     09.01.90 Ansgar Denner        last changed 02.09.08              *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 cspenh_coli,cln_coli,cz,x,x2
      real*8     eps
      real*8 :: b(11) = (/
     1   0.1666666666666666666666666667d0,
     2  -0.0333333333333333333333333333d0,
     3   0.0238095238095238095238095238d0,
     4  -0.0333333333333333333333333333d0,
     5   0.0757575757575757575757575758d0,
     6  -0.2531135531135531135531135531d0,
     7   1.1666666666666666666666666667d0,
     8  -7.0921568627450980392156862745d0,
     9  54.97117794486215538847117794486d0,
     +  -529.124242424242424242424242424242d0,
     1  6192.123188405797101449275362318d0  /)
c     note:                 b(n)=b2n
c     b(1)=1./6.
c     b(2)=-1./30.
c     b(3)=1./42.
c     b(4)=-1./30.
c     b(5)=5./66.
c     b(6)=-691./2730.
c     b(7)=7./6.
c     b(8)=-3617./510.
c     b(9)=43867./798.
c     b(10)=-174611./330.
c     b(11)=854513./138.
c     pi=3.1415926535897932384
c     pi*pi/6.=1.6449..., pi*pi/3=3.28986...
c
      integer    j
      real*8     factor
      complex*16 power,term,csp
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"

#ifdef CHECK
      if(argcheck) then
        if(abs(cz).gt.1d0+calacc.or.real(cz).gt.0.5d0)then
          call setErrFlag_coli(-4)
          call ErrOut_coli('cspenh_coli',
     &        'argument not in convergence region',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &          'cspenh_coli:  argument not in convergence region'
            write(nerrout_coli,*) 'cspenh_coli:  eps = ',eps
            write(nerrout_coli,*) 'cspenh_coli:  cz  = ',cz
            write(nerrout_coli,*) 'cspenh_coli:  |cz|= ',
     &          abs(cz),abs(cz)-1d0,abs(cz).gt.1d0
            write(nerrout_coli,*) 'cspenh_coli: Re(z)= ',
     &          real(cz),real(cz)-.5d0,real(cz).gt.0.5d0
          endif
        endif
      endif
#endif

      b(11)  =    854513d0/ 138d0
      b(10)  =  - 174611d0/ 330d0
      b(9)   =     43867d0/ 798d0
      b(8)   =  -   3617d0/ 510d0
      b(7)   =         7d0/   6d0
      b(6)   =  -    691d0/2730d0
      b(5)   =         5d0/  66d0
      b(4)   =  -      1d0/  30d0
      b(3)   =         1d0/  42d0
      b(2)   =  -      1d0/  30d0
      b(1)   =         1d0/   6d0
      x      =  -cln_coli(1d0-cz,-eps)
      x2     =  x*x
      power  =  x
      factor =  1d0
      cspenh_coli =  x - x2/4d0
      do 10 j=2,22,2
         factor = factor / j / (j+1)
         power  = power * x2
         term   = b(j/2) * factor * power
         csp    = cspenh_coli + term
         if (csp.eq.cspenh_coli) return
         cspenh_coli = csp
10    continue
#ifdef WARN
      if (abs(term/csp).gt.calacc) then
        call setErrFlag_coli(-1)
        call ErrOut_coli('cspenh_coli','bad convergence',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*) 'cspenh_coli converges badly  ',
     &        cz,x
          write(nerrout_coli,*) 'cspenh_coli converges badly  ',
     &        csp-term,cspenh_coli,term
        endif
      endif
#endif
      end


************************************************************************
      function cspcon_coli(z1,z2,pz,ompz,i1,i2)
************************************************************************
*  complex spence function   sp(1-z1*z2)                               *
*                          plus continuation terms                     *
*  pz = z1*z2, ompz = 1-pz                                             *
*  i1,i2 determine signs of infinitesimal imaginary parts of           *
*  z1,z2                                                               *
*----------------------------------------------------------------------*
*  16.10.08 Ansgar Denner      last changed 09.11.12                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,pz,ompz
      real*8     i1,i2,ip
      complex*16 cspcon_coli,cspenc_coli,cln_coli,sumln
      logical    errorwriteflag


#include "params_coli.h"
#include "checkparams_coli.h"


#ifdef CHECK
      if(argcheck) then
        if(abs(z1*z2-pz).gt.1d4*calacc*abs(pz).or.
     &      abs(1d0-pz-ompz).gt.
     &      1d7*calacc*max(abs(pz),abs(1d0-pz))) then
          call setErrFlag_coli(-4)
          call ErrOut_coli('cspcon_coli','inconsistent arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &         'cspcon_coli:  arguments inconsistent  '
            write(nerrout_coli,100) 'cspcon_coli: z1 = ',z1
            write(nerrout_coli,100) 'cspcon_coli: z2 = ',z2
            write(nerrout_coli,100) 'cspcon_coli: zp = ',pz,z1*z2
            write(nerrout_coli,100) 'cspcon_coli: omzp = ',ompz,1d0-pz
            write(nerrout_coli,100) 'cspcon_coli: test1  ',
     &          abs(z1*z2-pz),1d4*calacc*abs(pz)
            write(nerrout_coli,100) 'cspcon_coli: test2  ',
     &          abs(1d0-pz-ompz),
     &          1d7*calacc*max(abs(pz),abs(1d0-pz))
 100        format(a21,8g25.17)
          endif
        endif
      endif
#endif

c to ensure correct continuation for infinitesimal imaginary parts
      ip=-sign(1d0,i1)
      if(pz.eq.0d0)then
        cspcon_coli = pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            cspcon_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            cspcon_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.1d0) then
          if(real(pz).lt.2d0) then
            cspcon_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &          -.5d0*(sumln)**2
          else
            cspcon_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
          endif
        else
          cspcon_coli =0d0
#ifdef WARN
          if(abs(sumln).gt.1d1*calacc) then
            call setErrFlag_coli(-1)
            call ErrOut_coli('cspcon_coli',
     &          'continuation possibly wrong',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*)
     &            'cspcon_coli: continuation possibly wrong'
              write(nerrout_coli,*) 'cspcon_coli: z1 = ',z1
              write(nerrout_coli,*) 'cspcon_coli: z2 = ',z2
              write(nerrout_coli,*) 'cspcon_coli: pz = ',pz
              write(nerrout_coli,*) 'cspcon_coli:ompz= ',ompz
              write(nerrout_coli,*) 'cspcon_coli: i1 = ',i1
              write(nerrout_coli,*) 'cspcon_coli: i2 = ',i2
              write(nerrout_coli,*) 'cspcon_coli: sumln = ',sumln
            endif
          endif
#endif
        endif
      endif



      end

************************************************************************
      function cspcos_coli(z1,z2,i1,i2)
************************************************************************
*  complex spence function   sp(1-z1*z2)                               *
*                          plus continuation terms                     *
*  i1,i2 determine signs of infinitesimal imaginary parts of           *
*  z1,z2                                                               *
*----------------------------------------------------------------------*
*  30.04.08 Ansgar Denner      last changed 28.10.08                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,pz,ompz
      real*8     i1,i2,ip
      complex*16 cspcos_coli,cspenc_coli,cln_coli,sumln
      logical    errorwriteflag


#include "params_coli.h"
#include "checkparams_coli.h"


 100  format(a21,8g25.17)
      pz=z1*z2
      ompz=1d0-pz
c to ensure correct continuation for infinitesimal imaginary parts
      if(i2.ne.0d0)then
        ip = i2*sign(1d0,real(z1))
      else
        ip = i1*sign(1d0,real(z2))
      endif
      if(pz.eq.0d0)then
        cspcos_coli = pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            cspcos_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            cspcos_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.1d0) then
          if(real(pz).lt.2d0) then
            cspcos_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &          -.5d0*(sumln)**2
          else
            cspcos_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
          endif
        else
          cspcos_coli =0d0
          if(abs(sumln).gt.1d1*calacc) then
            call setErrFlag_coli(-1)
            call ErrOut_coli('cspcon_coli',
     &        'continuation possibly wrong',
     &         errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*)
     &            'cspcos_coli: continuation possibly wrong'
              write(nerrout_coli,*) 'cspcos_coli: z1 = ',z1
              write(nerrout_coli,*) 'cspcos_coli: z2 = ',z2
              write(nerrout_coli,*) 'cspcos_coli: i1 = ',i1
              write(nerrout_coli,*) 'cspcos_coli: i2 = ',i2
              write(nerrout_coli,*) 'cspcos_coli: sumln = ',sumln,
     &            cln_coli(z1,i1),cln_coli(z2,i2)
            endif
          endif
        endif
      endif



      end


************************************************************************
      function csp2con_coli(z1,z2,pz,ompz,i1,i2)
************************************************************************
*  complex spence function   sp(1-z1*z2)                               *
*                          plus continuation terms                     *
*  pz = z1*z2, ompz = 1-pz                                             *
*  i1,i2 determine signs of infinitesimal imaginary parts of           *
*  z1,z2                                                               *
*----------------------------------------------------------------------*
*  30.04.08 Ansgar Denner      last changed 03.06.08                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,pz,ompz
      real*8     i1,i2,ip
      complex*16 csp2con_coli,cspenc_coli,cln_coli,sumln
      logical    errorwriteflag


#include "params_coli.h"
#include "checkparams_coli.h"


#ifdef CHECK
      if(argcheck) then
         if(abs(z1*z2-pz).gt.1d2*calacc*abs(pz).or.
     &      abs(1d0-pz-ompz).gt.
     &      1d2*calacc*max(abs(pz),abs(1d0-pz))) then
           call setErrFlag_coli(-4)
           call ErrOut_coli('csp2con_coli',
     &         ' arguments inconsistent',
     &         errorwriteflag)
           if (errorwriteflag) then
             write(nerrout_coli,*)
     &         'csp2con_coli:  arguments inconsistent'
             write(nerrout_coli,100) 'csp2con_coli: z1 = ',z1
             write(nerrout_coli,100) 'csp2con_coli: z2 = ',z2
             write(nerrout_coli,100) 'csp2con_coli: zp = ',pz,z1*z2
             write(nerrout_coli,100) 'csp2con_coli: omzp = ',ompz,1d0-pz
 100         format(a21,4g25.17)
           endif
         endif
      endif
#endif

      ip=1d0
      if(pz.eq.0d0)then
        csp2con_coli = pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            csp2con_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            csp2con_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.1d0) then
          if(real(pz).lt.2d0) then
#ifdef WIB
            csp2con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &          -.5d0*(sumln)**2
#else
            csp2con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
     &        -.5d0*(cln_coli(pz,ip))**2
#endif
          else
#ifdef WIB
            csp2con_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
#else
            csp2con_coli = 2d0*pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
     &        +.5d0*(cln_coli(-pz,-ip))**2
#endif
          endif
        else
          csp2con_coli =0d0
        endif
      endif



      end
************************************************************************
      function csp2cos_coli(z1,z2,i1,i2)
************************************************************************
*  complex spence function   sp(1-z1*z2)                               *
*                          plus continuation terms                     *
*  i1,i2 determine signs of infinitesimal imaginary parts of           *
*  z1,z2                                                               *
*----------------------------------------------------------------------*
*  30.04.08 Ansgar Denner      last changed 22.09.08                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,pz,ompz
      real*8     i1,i2,ip
      complex*16 csp2cos_coli,cspenc_coli,cln_coli,sumln


#include "params_coli.h"
#include "checkparams_coli.h"


 100  format(a21,8g25.17)
      pz=z1*z2
      ompz=1d0-pz
      ip=1d0
      if(pz.eq.0d0)then
        csp2cos_coli = pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            csp2cos_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            csp2cos_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.1d0) then
          if(real(pz).lt.2d0) then
#ifdef WIB
            csp2cos_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &          -.5d0*(sumln)**2
#else
            csp2cos_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
     &        -.5d0*(cln_coli(pz,ip))**2
#endif
          else
#ifdef WIB
            csp2cos_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
#else
            csp2cos_coli = 2d0*pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
     &        +.5d0*(cln_coli(-pz,-ip))**2
#endif
          endif
        else
          csp2cos_coli =0d0
          if(sumln.ne.cd0) write(nerrout_coli,*)
     &        'csp2cos_coli: continuation possibly wrong'
        endif
      endif



      end

************************************************************************
      function csp3con_coli(z1,z2,z3,pz,ompz,i1,i2,i3)
************************************************************************
*  complex spence function   sp(1-z1*z2*z3)                            *
*                          plus continuation terms                     *
*  pz = z1*z2*z3, ompz = 1-pz                                          *
*  i1,i2,i3 determine signs of infinitesimal imaginary parts of        *
*  z1,z2,z3                                                            *
*----------------------------------------------------------------------*
*  30.04.08 Ansgar Denner      last changed 03.06.08                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,z3,pz,ompz
      real*8     i1,i2,i3,ip
      complex*16 csp3con_coli,cspenc_coli,cln_coli,sumln
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"

#ifdef CHECK
      if(argcheck) then
        if(abs(z1*z2*z3-pz).gt.1d3*calacc*abs(pz).or.
     &         abs(1d0-pz-ompz).gt.
     &      1d3*calacc*max(abs(pz),abs(1d0-pz))) then
          call setErrFlag_coli(-4)
          call ErrOut_coli('csp3con_coli',
     &        ' arguments inconsistent',
     &         errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &         'csp3con_coli:  arguments inconsistent  '
            write(nerrout_coli,100) 'csp3con_coli: z1 = ',z1
            write(nerrout_coli,100) 'csp3con_coli: z2 = ',z2
            write(nerrout_coli,100) 'csp3con_coli: z3 = ',z3
            write(nerrout_coli,100) 'csp3con_coli: zp = ',pz,z1*z2*z3
            write(nerrout_coli,100) 'csp3con_coli: omzp = ',ompz,1d0-pz
            write(nerrout_coli,100) 'test1 ',
     &          abs(z1*z2*z3-pz),1d3*calacc*abs(pz),
     &          abs(z1*z2*z3-pz).gt.1d3*calacc*abs(pz)
            write(nerrout_coli,100) 'test2 ',abs(1d0-pz-ompz),
     &          1d3*calacc*max(abs(pz),abs(1d0-pz)),
     &          abs(1d0-pz-ompz).gt.
     &          1d3*calacc*max(abs(pz),abs(1d0-pz))
 100        format(a21,4g25.17)
          endif
        endif
      endif
#endif


      ip=1d0
      if(pz.eq.0d0)then
        csp3con_coli = pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)+cln_coli(z3,i3)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            csp3con_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            csp3con_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.(1d0,0d0)) then
          if(real(pz).lt.2d0) then
#ifdef WIB
            csp3con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &          -.5d0*(sumln)**2
#else
            csp3con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
     &        -.5d0*(cln_coli(pz,ip))**2
#endif
          else
#ifdef WIB
            csp3con_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
#else
            csp3con_coli = 2d0*pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
     &        +.5d0*(cln_coli(-pz,-ip))**2
#endif
          endif
        else
          csp3con_coli =0d0
        endif
      endif



      end

************************************************************************
      function csp4con_coli(z1,z2,z3,z4,pz,ompz,i1,i2,i3,i4)
************************************************************************
*  complex spence function   sp(1-z1*z2*z3)                            *
*                          plus continuation terms                     *
*  pz = z1*z2*z3*z4, ompz = 1-pz                                       *
*  i1,i2,i3,i4 determine signs of infinitesimal imaginary parts of     *
*  z1,z2,z3,z4                                                         *
*----------------------------------------------------------------------*
*  02.05.08 Ansgar Denner      last changed 03.06.08                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 z1,z2,z3,z4,pz,ompz
      real*8     i1,i2,i3,i4,ip
      complex*16 csp4con_coli,cspenc_coli,cln_coli,sumln
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"


#ifdef CHECK
      if(argcheck) then
        if(abs(z1*z2*z3*z4-pz).gt.1d3*calacc*abs(pz).or.
     &         abs(1d0-pz-ompz).gt.
     &      1d3*calacc*max(abs(pz),abs(1d0-pz))) then
          call setErrFlag_coli(-4)
          call ErrOut_coli('csp4con_coli',
     &        ' arguments inconsistent',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &         'csp4con_coli:  arguments inconsistent  '
            write(nerrout_coli,100) 'csp4con_coli: z1 = ',z1
            write(nerrout_coli,100) 'csp4con_coli: z2 = ',z2
            write(nerrout_coli,100) 'csp4con_coli: z3 = ',z3
            write(nerrout_coli,100) 'csp4con_coli: z4 = ',z4
            write(nerrout_coli,100) 'csp4con_coli: zp = ',pz,z1*z2*z3*z4
            write(nerrout_coli,100) 'csp4con_coli: omzp = ',ompz,1d0-pz
            write(nerrout_coli,100) 'test1 ',abs(z1*z2*z3*z4-pz),
     &          1d3*calacc*abs(pz)
            write(nerrout_coli,100) 'test2 ',abs(1d0-pz-ompz),
     &          1d3*calacc*max(abs(pz),abs(1d0-pz))
 100        format(a21,4g25.17)
          endif
        endif
      endif
#endif

      ip=1d0
      if(pz.eq.0d0)then
         csp4con_coli =pi2_6
      else
        sumln=cln_coli(z1,i1)+cln_coli(z2,i2)
     &      +cln_coli(z3,i3)+cln_coli(z4,i4)
        if(abs(pz).lt.1d0) then
          if(real(pz).gt.0.5d0) then
            csp4con_coli = cspenc_coli(1d0-pz,-ip)+cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
          else
            csp4con_coli = pi2_6-cspenc_coli(pz,ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
          endif
        elseif(pz.ne.(1d0,0d0)) then
          if(real(pz).lt.2d0) then
#ifdef WIB
            csp4con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-1d0/pz,ip)
     &          *(cln_coli(pz,ip)-sumln)
     &        -.5d0*(sumln)**2
#else
            csp4con_coli = -cspenc_coli(1d0-1d0/pz,ip)
     &          +cln_coli(1d0-pz,-ip)
     &          *(cln_coli(pz,ip)-sumln)
     &        -.5d0*(cln_coli(pz,ip))**2
#endif
          else
#ifdef WIB
            csp4con_coli = -pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-1d0/pz,ip)*sumln
     &          -.5d0*(sumln)**2
#else
            csp4con_coli = 2d0*pi2_6+cspenc_coli(1d0/pz,-ip)
     &          -cln_coli(1d0-pz,-ip)*sumln
     &        +.5d0*(cln_coli(-pz,-ip))**2
#endif
          endif
        else
          csp4con_coli =0d0
        endif
      endif


      end

************************************************************************
      function eta2_coli(z1,z2,pz,i1,i2,ip)
************************************************************************
*     complex eta-function with explicit infinitesimal im parts        *
*     pz = z1*z2                                                      *
*----------------------------------------------------------------------*
*     06.10.08    Ansgar Denner       last changed   06.10.08          *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 eta2_coli,z1,z2,pz
      real*8     i1,i2,ip
      real*8     im1,im2,imp,re1,re2,rep
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"


#ifdef CHECK
      if(argcheck) then
        if(abs(z1*z2-pz).gt.1d3*calacc*abs(pz)) then
          call setErrFlag_coli(-4)
          call ErrOut_coli('eta2_coli',
     &         ' arguments inconsistent',
     &         errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) 'eta2_coli:  arguments inconsistent  '
            write(nerrout_coli,100) 'eta2_coli: z1 = ',z1
            write(nerrout_coli,100) 'eta2_coli: z2 = ',z2
            write(nerrout_coli,100) 'eta2_coli: zp = ',pz,z1*z2
            write(nerrout_coli,100) 'test1 ',abs(z1*z2-pz),
     &          1d3*calacc*abs(pz)
 100        format(a21,4g25.17)
          endif
        endif
      endif
#endif

      im1   = aimag(z1)
      im2   = aimag(z2)
      imp   = aimag(pz)
      re1   = real(z1)
      re2   = real(z2)
      rep   = real(pz)


      if (abs(im1).lt.impacc*abs(re1)) im1 = 0d0
      if (abs(im2).lt.impacc*abs(re2)) im2 = 0d0
      if (abs(imp).lt.impacc*abs(rep)) imp = 0d0


      if (im1.ne.0d0.or.im2.ne.0d0.or.imp.ne.0d0) then
        if(im1.eq.0d0) im1 = i1
        if(im2.eq.0d0) im2 = i2
        if(imp.eq.0d0) imp = ip
        if(im1.lt.0d0.and.im2.lt.0d0.and.imp.gt.0d0) then
          eta2_coli = dcmplx(0d0,2d0*pi)
        else if (im1.gt.0d0.and.im2.gt.0d0.and.imp.lt.0d0) then
          eta2_coli = dcmplx(0d0,-2d0*pi)
        else
          eta2_coli = dcmplx(0d0)
#ifdef CHECK
          if(im1.eq.0.and.re1.lt.0d0 .or.
     &         im2.eq.0.and.re2.lt.0d0 .or.
     &         imp.eq.0.and.rep.lt.0d0) then
            call setErrFlag_coli(-4)
            call ErrOut_coli('eta2_coli',
     &          ' inconsistency detected',
     &           errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*) ' eta2_coli not defined '
              write(nerrout_coli,*) ' eta2_coli:  z1 = ',z1,i1
              write(nerrout_coli,*) ' eta2_coli:  z2 = ',z2,i2
              write(nerrout_coli,*) ' eta2_coli:  pz = ',pz,ip
            endif
          endif
#endif
        endif
      else
        eta2_coli = dcmplx(0d0)
        if (re1.lt.0d0.and.re2.lt.0d0) then
          if(i1.gt.0d0.and.i2.gt.0d0) then
            eta2_coli = dcmplx(0d0,-2d0*pi)
          else   if(i1.lt.0d0.and.i2.lt.0d0) then
            eta2_coli = dcmplx(0d0,2d0*pi)
          endif
        else if (rep.lt.0d0.and.re1.lt.0d0) then
          if(i1.gt.0d0.and.ip.lt.0d0) then
            eta2_coli = dcmplx(0d0,-2d0*pi)
          else   if(i1.lt.0d0.and.ip.gt.0d0) then
            eta2_coli = dcmplx(0d0,2d0*pi)
          endif
        else if (rep.lt.0d0.and.re2.lt.0d0) then
          if(i2.gt.0d0.and.ip.lt.0d0) then
            eta2_coli = dcmplx(0d0,-2d0*pi)
          else   if(i2.lt.0d0.and.ip.gt.0d0) then
            eta2_coli = dcmplx(0d0,2d0*pi)
          endif
        endif
      endif


      end

************************************************************************
      function eta2s_coli(z1,z2,i1,i2,ip)
************************************************************************
*     complex eta-function with explicit infinitesimal im parts        *
*     pz = z1*z2                                                       *
*----------------------------------------------------------------------*
*     06.10.08    Ansgar Denner       last changed   06.10.08          *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 eta2s_coli,z1,z2,pz
      real*8     i1,i2,ip
      real*8     im1,im2,imp,re1,re2,rep
      logical    errorwriteflag

#include "params_coli.h"
#include "checkparams_coli.h"


      pz=z1*z2

      im1 = aimag(z1)
      im2 = aimag(z2)
      imp = aimag(pz)
      re1 = real(z1)
      re2 = real(z2)
      rep = real(pz)


      if (abs(im1).lt.impacc*abs(re1)) im1 = 0d0
      if (abs(im2).lt.impacc*abs(re2)) im2 = 0d0
      if (abs(imp).lt.impacc*abs(rep)) imp = 0d0


      if (im1.ne.0d0.or.im2.ne.0d0.or.imp.ne.0d0) then
        if(im1.eq.0d0) im1 = i1
        if(im2.eq.0d0) im2 = i2
        if(imp.eq.0d0) imp = ip
        if(im1.lt.0d0.and.im2.lt.0d0.and.imp.gt.0d0) then
          eta2s_coli = dcmplx(0d0,2d0*pi)
        else if (im1.gt.0d0.and.im2.gt.0d0.and.imp.lt.0d0) then
          eta2s_coli = dcmplx(0d0,-2d0*pi)
        else
          eta2s_coli = dcmplx(0d0)
#ifdef CHECK
          if(im1.eq.0.and.re1.lt.0d0 .or.
     &         im2.eq.0.and.re2.lt.0d0 .or.
     &         imp.eq.0.and.rep.lt.0d0) then
            call setErrFlag_coli(-4)
            call ErrOut_coli('eta2s_coli',
     &          ' inconsistency detected',
     &           errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*) ' eta2s_coli not defined '
              write(nerrout_coli,*) ' eta2s_coli:  z1 = ',z1,i1
              write(nerrout_coli,*) ' eta2s_coli:  z2 = ',z2,i2
              write(nerrout_coli,*) ' eta2s_coli:  pz = ',pz,ip
            endif
          endif
#endif
        endif
      else
        eta2s_coli = dcmplx(0d0)
        if (re1.lt.0d0.and.re2.lt.0d0) then
          if(i1.gt.0d0.and.i2.gt.0d0) then
            eta2s_coli = dcmplx(0d0,-2d0*pi)
          else   if(i1.lt.0d0.and.i2.lt.0d0) then
            eta2s_coli = dcmplx(0d0,2d0*pi)
          endif
        else if (rep.lt.0d0.and.re1.lt.0d0) then
          if(i1.gt.0d0.and.ip.lt.0d0) then
            eta2s_coli = dcmplx(0d0,-2d0*pi)
          else   if(i1.lt.0d0.and.ip.gt.0d0) then
            eta2s_coli = dcmplx(0d0,2d0*pi)
          endif
        else if (rep.lt.0d0.and.re2.lt.0d0) then
          if(i2.gt.0d0.and.ip.lt.0d0) then
            eta2s_coli = dcmplx(0d0,-2d0*pi)
          else   if(i2.lt.0d0.and.ip.gt.0d0) then
            eta2s_coli = dcmplx(0d0,2d0*pi)
          endif
        endif
      endif


      end

************************************************************************
      function chdet(n,cai)
************************************************************************
*     Calculation of the determinant of a complex n x n matrix         *
*     The matrix is first reduced via Householder transformations into *
*     the form A = QR                                                  *
*     where  Q is a orthogonal and R a upper triangular matrix.        *
*     See Stoer, Numerische Mathematik, chapter 4.7                    *
*     The determinant is then obtained as                              *
*     Det(a) = prod_i (-r_ii)                                          *
*     ca(i,j) j.ge.i contains the elements of u_i that form the        *
*             Householder matrix P_i = 1 - beta_i u x u^H              *
*     ca(i,j) j.lt.i contains the nondiagonal entries of R             *
*     cd contains the diagonal elements of R                           *
*----------------------------------------------------------------------*
*     20.04.04  Ansgar Denner     last changed  08.06.04               *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      integer    n
      complex*16 chdet
      real*8     aabs,sigma,rs,beta,norm
      complex*16 ca(n,n),cd(n),cq(n,n),cr(n,n),catest(n,n)
      complex*16 cai(n,n)
      complex*16 cdet,csum,cs
      integer    i,j,k
      logical    errorwriteflag
#include "params_coli.h"

c saving of input

      do 10 j=1,n
        do 20 i=1,n
          ca(i,j) = cai(i,j)
 20     continue
 10   continue


c  Householder transformation

      do 100 j=1,n

c  calculation of transformation matrix

        aabs = abs(ca(j,j))
        sigma = aabs**2
        do 200 i=j+1,n
           sigma = sigma +  abs(ca(i,j))**2
 200    continue
        if (sigma.eq.0) then
          chdet = 0d0
#ifdef CHECK
c         call setErrFlag_coli(-1)
c         call ErrOut_coli('chdet',' zero determinant',
c    &       errorwriteflag)
c         if (errorwriteflag) then
c           write(nerrout_coli,*) 'sigma = chdet =',chdet
c         endif
#endif
          return
        end if
        rs = sqrt(sigma)
        if (aabs.gt.0d0) then
          cs = rs* ca(j,j)/aabs
        else
          cs = rs
        end if
        beta = 1d0/(sigma + rs * aabs)
        ca(j,j) = ca(j,j) + cs

c multiplication of a(i,j) with the transformation matrix

        cd(j) = -cs
        do 300 k=j+1,n
          csum = 0d0
          do 400 i=j,n
            csum = csum + conjg(ca(i,j))*ca(i,k)
 400      continue
          csum = csum*beta
          do 500 i=j,n
            ca(i,k) = ca(i,k) - ca(i,j) * csum
 500      continue
 300    continue

 100  continue


c calculation of determinant
      cdet = 1d0
      do 600 i=1,n
        cdet = -cdet * cd(i)
 600  continue
      chdet = cdet


      return

c calculation of matrix Q

      do 700 j=n,1,-1
        norm = 0d0
        do 800 i=j,n
          norm = norm + abs(ca(i,j))**2
 800    continue
        cq(j,j) = 1 - 2d0 * abs(ca(j,j))**2/norm
        do 900 i=j+1,n
          cq(i,j) = - 2d0 * ca(i,j)*conjg(ca(j,j))/norm
 900    continue
        do 1000 k=j+1,n
          csum = 0d0
          do 1100 i=j+1,n
            csum = csum + conjg(ca(i,j))*cq(i,k)
 1100     continue
          csum = 2d0*csum/norm
          cq(j,k) =  - ca(j,j) * csum
          do 1200 i=j+1,n
            cq(i,k) = cq(i,k) - ca(i,j) * csum
 1200     continue
 1000   continue
 700  continue

c determination of matrix R

      do i=1,n
      do k=1,n
        cr(i,k) = 0d0
      enddo
      enddo
      do i=1,n
        cr(i,i) = cd(i)
      do k=i+1,n
        cr(i,k) = ca(i,k)
      enddo
      enddo

c check of unitarity of Q

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,n
        catest(i,k) = catest(i,k) + cq(i,j)*conjg(cq(k,j))
      enddo
      enddo
      enddo

c check of decomposition
      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,k
        catest(i,k) = catest(i,k) + cq(i,j)*cr(j,k)
      enddo
      enddo
      enddo

      end

!***********************************************************************
      subroutine chinve(n,cai,cainv,cdeta,cdetaaccloss)
!***********************************************************************
!     Calculation of the inverse of a complex n x n matrix             *
!     The matrix is first reduced via Householder transformations into *
!     the form A = QR                                                  *
!     where  Q is a hermitean and R a upper triangular matrix.         *
!     See Stoer, Numerische Mathematik, chapter 4.7                    *
!     The inverse is then obtained as                                  *
!     Ainv = Rinv * Qadj                                               *
!     ca(i,j) j.ge.i contains the elements of u_i that form the        *
!             Householder matrix P_i = 1 - beta_i u x u^H              *
!     ca(i,j) j.lt.i contains the nondiagonal entries of R             *
!     cd contains the diagonal elements of R                           *
!     flag = 1 signals vanishing determinant                           *
!     cdetaaccloss:  error in cdeta propto  1/cdetaaccloss             *
!       log(cdetaaccloss)/log(10d0) = number of lost digits of cdeta   *
!----------------------------------------------------------------------*
!     07.06.05  Ansgar Denner     last changed  24.01.19               *
!***********************************************************************
      implicit   none
      complex*16, intent(out) :: cdeta
      real*8,    intent(out) :: cdetaaccloss
      integer    n
      complex*16 chdet
      real*8     aabs,sigma,rs,beta,norm
      complex*16 ca(n,n),cd(n),cq(n,n),cr(n,n),catest(n,n),crtest(n,n)
      complex*16 cai(n,n),crinv(n,n),cainv(n,n)
      complex*16 csum,cs,canew
      real*8     caaccloss(n),cdaccloss(n),csaccloss
      integer    i,j,k
      logical    errorwriteflag

! saving of input
      do 10 j=1,n
        caaccloss(j) = 1d0
        do 20 i=1,n
          ca(i,j) = cai(i,j)
 20     continue
 10   continue

!      if(n.eq.4) then
!      write(*,2) cai(1,1:4)
!      write(*,2) cai(2,1:4)
!      write(*,2) cai(3,1:4)
!      write(*,2) cai(4,1:4)
!      end if

 2    format(5('(',g21.14,',',g21.14,') ':))

!  Householder transformation

      do 100 j=1,n

!  calculation of transformation matrix

        aabs = abs(ca(j,j))
        sigma = aabs**2
        do 200 i=j+1,n
           sigma = sigma +  abs(ca(i,j))**2
 200    continue
        if (sigma.eq.0) then
          cdeta = 0d0
#ifdef NONE
!         det=0 taken care of by reduction subroutines!
          call setErrFlag_coli(-7)
          call ErrOut_coli('chinve',' zero determinant',
     &       errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) 'sigma = cdeta =',cdeta
            write(nerrout_coli,*) 'n = ',n
          endif
#endif
          return
        end if
        rs = sqrt(sigma)
        if (aabs.gt.0d0) then
          cs = rs* (ca(j,j)/aabs)
#ifdef ERREST
!          csaccloss = caaccloss(j)
!  propagation of error of ca(j,j)
          if (imag(ca(j,j))*real(ca(j,j)).ne.0d0) then
            csaccloss = min(1d0, caaccloss(j)*sigma/aabs**2,
     &          caaccloss(j)*aabs**2/abs(imag(ca(j,j))*real(ca(j,j))))
          else
            csaccloss = min(1d0,caaccloss(j)*sigma/aabs**2)
          end if
#endif
        else
          cs = rs
#ifdef ERREST
          csaccloss = 1d0
#endif
        end if

        beta = 1d0/(sigma + rs * aabs)
        ca(j,j) = ca(j,j) + cs

! multiplication of a(i,j) with the transformation matrix

        cd(j) = -cs
#ifdef ERREST
        cdaccloss(j) = csaccloss
#endif
        do 300 k=j+1,n
          csum = 0d0
          do 400 i=j,n
            csum = csum + conjg(ca(i,j))*ca(i,k)
 400      continue
          csum = csum*beta
          do 500 i=j,n
            canew = ca(i,k) - ca(i,j) * csum
#ifdef ERREST
!  estimate error of ca(k,k)
            if (i.eq.k) then
              if(ca(i,k).ne.0d0) then
                if(ca(i,j)*csum.ne.0d0) then
                  caaccloss(i) = min(1d0,
     &                caaccloss(i)* abs(canew/abs(ca(i,k))),
     &                abs(canew/abs(ca(i,j)*csum)))
                else
                  caaccloss(i) = min(1d0,
     &                caaccloss(i)* abs(canew/abs(ca(i,k))))
                endif
              else
                caaccloss(i) = 1d0
              endif
            end if
#endif
            ca(i,k) = canew
 500      continue
 300    continue

 100  continue

!      write(*,*) 'chinve cdaccloss = ',cdaccloss

! calculation of determinant
      cdeta = 1d0
#ifdef ERREST
      cdetaaccloss = 1d0
#endif
      do 600 i=1,n
        cdeta = -cdeta * cd(i)
#ifdef ERREST
        cdetaaccloss = cdetaaccloss * cdaccloss(i)
#endif
 600  continue


! calculation of matrix Q

      do 700 j=n,1,-1
        norm = 0d0
        do 800 i=j,n
          norm = norm + abs(ca(i,j))**2
 800    continue
        cq(j,j) = 1 - 2d0 * abs(ca(j,j))**2/norm
        do 900 i=j+1,n
          cq(i,j) = - 2d0 * ca(i,j)*conjg(ca(j,j))/norm
 900    continue
        do 1000 k=j+1,n
          csum = 0d0
          do 1100 i=j+1,n
            csum = csum + conjg(ca(i,j))*cq(i,k)
 1100     continue
          csum = 2d0*csum/norm
          cq(j,k) =  - ca(j,j) * csum
          do 1200 i=j+1,n
            cq(i,k) = cq(i,k) - ca(i,j) * csum
 1200     continue
 1000   continue
 700  continue

! determination of matrix R

      do i=1,n
      do k=1,n
        cr(i,k) = 0d0
      end do
      end do
      do i=1,n
        cr(i,i) = cd(i)
      do k=i+1,n
        cr(i,k) = ca(i,k)
      end do
      end do

! determination of inverse of R

      do i=1,n
      do k=1,n
        crinv(i,k) = 0d0
      end do
      end do
      do k=1,n
        crinv(k,k) = 1d0/cr(k,k)
      do i=1,k-1
      do 2200 j=i,k-1
        crinv(i,k) = crinv(i,k) - crinv(i,j)* cr(j,k)/cr(k,k)
 2200 continue
      end do
      end do

! determination of inverse of A

      do i=1,n
      do k=1,n
        cainv(i,k) = 0d0
      do j=1,n
        cainv(i,k) = cainv(i,k) + crinv(i,j)*conjg(cq(k,j))
      end do
      end do
      end do

      return

! check of unitarity of Q

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,n
        catest(i,k) = catest(i,k) + cq(i,j)*conjg(cq(k,j))
      end do
      end do
      end do


! check of decomposition

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,k
        catest(i,k) = catest(i,k) + cq(i,j)*cr(j,k)
      end do
      end do
      end do


! check of inverse of Q
      do i=1,n
      do k=1,n
        crtest(i,k) = 0d0
      do j=1,n
        crtest(i,k) = crtest(i,k) + cr(i,j)*crinv(j,k)
      end do
      end do
      end do

! check of inverse of A

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,n
        catest(i,k) = catest(i,k) + cai(i,j)*cainv(j,k)
      end do
      end do
      end do

      end

************************************************************************
      subroutine chinv(n,cai,cainv,cdeta)
************************************************************************
*     Calculation of the inverse of a complex n x n matrix             *
*     The matrix is first reduced via Householder transformations into *
*     the form A = QR                                                  *
*     where  Q is a hermitean and R a upper triangular matrix.         *
*     See Stoer, Numerische Mathematik, chapter 4.7                    *
*     The inverse is then obtained as                                  *
*     Ainv = Rinv * Qadj                                               *
*     ca(i,j) j.ge.i contains the elements of u_i that form the        *
*             Householder matrix P_i = 1 - beta_i u x u^H              *
*     ca(i,j) j.lt.i contains the nondiagonal entries of R             *
*     cd contains the diagonal elements of R                           *
*     flag = 1 signals vanishing determinant                           *
*----------------------------------------------------------------------*
*     07.06.05  Ansgar Denner     last changed  21.06.18               *
************************************************************************
      implicit   none
      complex*16, intent(out), optional :: cdeta
      integer    n
      complex*16 chdet
      real*8     aabs,sigma,rs,beta,norm
      complex*16 ca(n,n),cd(n),cq(n,n),cr(n,n),catest(n,n),crtest(n,n)
      complex*16 cai(n,n),crinv(n,n),cainv(n,n)
      complex*16 csum,cs
      integer    i,j,k
      logical    errorwriteflag

c saving of input
      do 10 j=1,n
        do 20 i=1,n
          ca(i,j) = cai(i,j)
 20     continue
 10   continue

 2    format(5('(',g11.4,',',g11.4,') ':))

c  Householder transformation

      do 100 j=1,n

c  calculation of transformation matrix

        aabs = abs(ca(j,j))
        sigma = aabs**2
        do 200 i=j+1,n
           sigma = sigma +  abs(ca(i,j))**2
 200    continue
        if (sigma.eq.0) then
          chdet = 0d0
#ifdef NONE
!         det=0 taken care of by reduction subroutines!
          call setErrFlag_coli(-7)
          call ErrOut_coli('chinv',' zero determinant',
     &       errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) 'sigma = chdet =',chdet
            write(nerrout_coli,*) 'n = ',n
          endif
#endif
          if(present(cdeta)) cdeta = chdet
          return
        end if
        rs = sqrt(sigma)
        if (aabs.gt.0d0) then
          cs = rs* ca(j,j)/aabs
        else
          cs = rs
        end if
        beta = 1d0/(sigma + rs * aabs)
        ca(j,j) = ca(j,j) + cs

c multiplication of a(i,j) with the transformation matrix

        cd(j) = -cs
        do 300 k=j+1,n
          csum = 0d0
          do 400 i=j,n
            csum = csum + conjg(ca(i,j))*ca(i,k)
 400      continue
          csum = csum*beta
          do 500 i=j,n
            ca(i,k) = ca(i,k) - ca(i,j) * csum
 500      continue
 300    continue

 100  continue


c calculation of determinant
      cdeta = 1d0
      do 600 i=1,n
        cdeta = -cdeta * cd(i)
 600  continue


c calculation of matrix Q

      do 700 j=n,1,-1
        norm = 0d0
        do 800 i=j,n
          norm = norm + abs(ca(i,j))**2
 800    continue
        cq(j,j) = 1 - 2d0 * abs(ca(j,j))**2/norm
        do 900 i=j+1,n
          cq(i,j) = - 2d0 * ca(i,j)*conjg(ca(j,j))/norm
 900    continue
        do 1000 k=j+1,n
          csum = 0d0
          do 1100 i=j+1,n
            csum = csum + conjg(ca(i,j))*cq(i,k)
 1100     continue
          csum = 2d0*csum/norm
          cq(j,k) =  - ca(j,j) * csum
          do 1200 i=j+1,n
            cq(i,k) = cq(i,k) - ca(i,j) * csum
 1200     continue
 1000   continue
 700  continue

c determination of matrix R

      do i=1,n
      do k=1,n
        cr(i,k) = 0d0
      enddo
      enddo
      do i=1,n
        cr(i,i) = cd(i)
      do k=i+1,n
        cr(i,k) = ca(i,k)
      enddo
      enddo

c determination of inverse of R

      do i=1,n
      do k=1,n
        crinv(i,k) = 0d0
      enddo
      enddo
      do k=1,n
        crinv(k,k) = 1d0/cr(k,k)
      do i=1,k-1
      do 2200 j=i,k-1
        crinv(i,k) = crinv(i,k) - crinv(i,j)* cr(j,k)/cr(k,k)
 2200 continue
      enddo
      enddo

c determination of inverse of A

      do i=1,n
      do k=1,n
        cainv(i,k) = 0d0
      do j=1,n
        cainv(i,k) = cainv(i,k) + crinv(i,j)*conjg(cq(k,j))
      enddo
      enddo
      enddo

      return

c check of unitarity of Q

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,n
        catest(i,k) = catest(i,k) + cq(i,j)*conjg(cq(k,j))
      enddo
      enddo
      enddo


c check of decomposition

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,k
        catest(i,k) = catest(i,k) + cq(i,j)*cr(j,k)
      enddo
      enddo
      enddo


c check of inverse of Q
      do i=1,n
      do k=1,n
        crtest(i,k) = 0d0
      do j=1,n
        crtest(i,k) = crtest(i,k) + cr(i,j)*crinv(j,k)
      enddo
      enddo
      enddo

c check of inverse of A

      do i=1,n
      do k=1,n
        catest(i,k) = 0d0
      do j=1,n
        catest(i,k) = catest(i,k) + cai(i,j)*cainv(j,k)
      enddo
      enddo
      enddo

      end


************************************************************************
      subroutine setmuuv2_coli(muuv2in)
************************************************************************
*     sets UV-regulator mass squared to muuv2in                        *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     muuv2in

#include "common_coli.h"
#include "params_coli.h"

      muuv2 = muuv2in

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: UV regulator mass squared set to muuv2 = ',
     &      muuv2
      endif

      end
************************************************************************
      subroutine getmuuv2_coli(muuv2out)
************************************************************************
*     returns UV-regulator mass squared to muuv2in                     *
*-----------------------------------------------------------------------
*     23.10.08 Ansgar Denner         last changed  23.10.08            *
************************************************************************
      implicit   none
      real*8     muuv2out

#include "common_coli.h"

      muuv2out = muuv2

      end

************************************************************************
      subroutine setmuir2_coli(muir2in)
************************************************************************
*     sets photon mass squared regulator to lambda2in                  *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     muir2in

#include "common_coli.h"
#include "params_coli.h"

      muir2 = muir2in

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: IR regularor mass squared set to muir2 = ',
     &      muir2
      endif

      end
************************************************************************
      subroutine getmuir2_coli(muir2out)
************************************************************************
*     returns UV-regulator mass squared to muir2in                     *
*-----------------------------------------------------------------------
*     23.10.08 Ansgar Denner         last changed  23.10.08            *
************************************************************************
      implicit   none
      real*8     muir2out

#include "common_coli.h"

      muir2out = muir2

      end


#ifdef SING
************************************************************************
      subroutine setdeltauv_coli(deltauvin)
************************************************************************
*     sets UV pole 2/(4-D) to deltauv                                  *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     deltauvin

#include "common_coli.h"
#include "params_coli.h"

      deltauv = deltauvin
      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: UV pole set to                 deltauv = ',
     &    deltauv
      endif

      end
************************************************************************
      subroutine getdeltauv_coli(deltauvout)
************************************************************************
*     returns UV pole 2/(4-D)  deltauv                                 *
*-----------------------------------------------------------------------
*     23.10.08 Ansgar Denner         last changed  23.10.08            *
************************************************************************
      implicit   none
      real*8     deltauvout

#include "common_coli.h"

      deltauvout = deltauv

      end
************************************************************************
      subroutine setdeltair_coli(delta1irin,delta2irin)
************************************************************************
*     sets IR pole 2/(4-D) to delta1ir and squared pole to delta2ir    *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     delta1irin,delta2irin

#include "common_coli.h"
#include "params_coli.h"

      delta1ir = delta1irin
      delta2ir = delta2irin
      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: IR pole set to                delta1ir = ',
     &      delta1ir
        write(ninfout_coli,*)
     &      'COLI: squared IR pole set to        delta2ir = ',
     &      delta2ir
      endif
      end
************************************************************************
      subroutine getdeltair_coli(delta1irout,delta2irout)
************************************************************************
*     returns UV pole 2/(4-D)  deltauv                                 *
*-----------------------------------------------------------------------
*     23.10.08 Ansgar Denner         last changed  23.10.08            *
************************************************************************
      implicit   none
      real*8     delta1irout,delta2irout

#include "common_coli.h"

      delta1irout = delta1ir
      delta2irout = delta2ir

      end
************************************************************************
      subroutine setshiftms2_coli(shiftms2in)
************************************************************************
*     sets  shift for squared mass-singular logs to shiftms2in         *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     shiftms2in

#include "common_coli.h"
#include "params_coli.h"

      colishiftms2 = shiftms2in

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: shift for squared mass-singular logs set to'
        write(ninfout_coli,*)
     &      '                               colishiftms2  = ',
     &      colishiftms2
      endif

      end

************************************************************************
      subroutine getshiftms2_coli(shiftms2out)
************************************************************************
*     gets shift for squared mass-singular logs colishiftms2           *
*-----------------------------------------------------------------------
*     15.09.08 Ansgar Denner         last changed  15.09.08            *
************************************************************************
      implicit   none
      real*8     shiftms2out

#include "common_coli.h"

      shiftms2out=colishiftms2

      end
#endif

************************************************************************
      subroutine setminf2_coli(m2)
************************************************************************
*     adds m2 to list of mass squared to be neglected                  *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 m2
      integer    i

#include "common_coli.h"
#include "params_coli.h"


      do 10 i=1,ncoliminf
        if(m2.eq.coliminffix2(i)) return
 10   continue

      ncoliminf = ncoliminf+1
      coliminffix(ncoliminf) = sqrt(m2)
      coliminffix2(ncoliminf) = m2
      coliminf2(ncoliminf) = m2/coliminfscale2
      coliminf(ncoliminf) = sqrt(coliminf2(ncoliminf))

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: added to small masses squared:  m2 =  ',m2
      endif

      end
************************************************************************
      subroutine clearcoliminf2()
************************************************************************
*     clears list of masses squared to be neglected                    *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      integer    i

#include "common_coli.h"
#include "params_coli.h"

      do 10 i=1,ncoliminf
        coliminffix2(i) = 0d0
        coliminffix(i) = 0d0
        coliminf2(i) = 0d0
        coliminf(i) = 0d0
 10   continue

      ncoliminf = 0

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: list of complex small masses cleared'
      endif

      end

************************************************************************
      subroutine setminfscale2_coli(scale2)
************************************************************************
*     sets factor with which small masses are scaled up in             *
*     mass-singular logarithms                                         *
*     and rescales masses in /coliminf/ /coliminf2/                    *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     scale2

#include "common_coli.h"
#include "params_coli.h"

      integer    i
      coliminfscale2 = scale2
      coliminfscale = sqrt(scale2)

      do 10 i=1,ncoliminf
        coliminf(i) = coliminffix(i)/coliminfscale
        coliminf2(i) = coliminffix2(i)/coliminfscale2
 10   continue

      if(coliinfo)then
        write(ninfout_coli,*)
     &      'COLI: small masses scaled with coliminfscale = ',
     &      coliminfscale
        write(ninfout_coli,*) '      in mass-singular logarithms'
      endif

      end

************************************************************************
      subroutine modminfscale2_coli(scale2)
************************************************************************
*     sets factor with which small masses are scaled up in             *
*     mass-singular logarithms                                         *
*-----------------------------------------------------------------------
*     15.09.09 Ansgar Denner         last changed  15.09.09            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      real*8     scale2

#include "common_coli.h"
#include "params_coli.h"

      coliminfscale2 = scale2
      coliminfscale = sqrt(scale2)
      if(coliinfo)then
        write(ninfout_coli,*) 'COLI: scalefactor for small masses ',
     &      ' in mass-singular logarithms'
        write(ninfout_coli,*)
     &      '     changed to coliminfscale = ',
     &      coliminfscale
      endif

      end

************************************************************************
      subroutine getminfscale2_coli(scale2)
************************************************************************
*     gets factor with which small masses are scaled up in             *
*     mass-singular logarithms                                         *
*-----------------------------------------------------------------------
*     15.09.09 Ansgar Denner         last changed  15.09.09            *
************************************************************************
      implicit   none
      real*8     scale2

#include "common_coli.h"

      scale2=coliminfscale2
      end

************************************************************************
      function minfscaledown_coli(m)
************************************************************************
*     scales mass down by coliminfscale if m in /coliminf/             *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
      implicit   none
      complex*16 minfscaledown_coli,m

#include "common_coli.h"

      integer    i

      minfscaledown_coli = m
      do 10 i=1,ncoliminf
         if(m.eq.coliminffix(i)) then
           minfscaledown_coli = coliminf(i)
           return
         endif
 10   continue

      end

************************************************************************
      function minfscaledown2_coli(m2)
************************************************************************
*     scales mass down by coliminfscale^2 if m2 in /coliminf2/         *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
      implicit   none
      complex*16 minfscaledown2_coli,m2

#include "common_coli.h"

      integer    i
      minfscaledown2_coli = m2
      do 10 i=1,ncoliminf
         if(m2.eq.coliminffix2(i)) then
           minfscaledown2_coli = coliminf2(i)
           return
         endif
 10   continue

      end

************************************************************************
      function elimminf_coli(m)
************************************************************************
*     if m = coliminfs(i)  then  elimcoliminf = 0                      *
*                          else  elimcoliminf  = m                     *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  08.09.08            *
************************************************************************
      implicit   none
      complex*16 m,elimminf_coli

#include "common_coli.h"

      integer    i

      elimminf_coli = m
      do 10 i=1,ncoliminf
         if(m.eq.coliminf(i)) then
           elimminf_coli = 0d0
           return
         endif
 10   continue
      end

************************************************************************
      function elimminf2_coli(m2)
************************************************************************
*     if m2 = coliminf2(i)  then  elimminf2_coli = 0                   *
*                           else  elimminf2_coli = m2                  *
*-----------------------------------------------------------------------
*     28.08.08 Ansgar Denner         last changed  28.08.08            *
************************************************************************
      implicit   none
      complex*16 m2,elimminf2_coli

#include "common_coli.h"

      integer    i


      elimminf2_coli = m2
      do 10 i=1,ncoliminf
         if(m2.eq.coliminf2(i)) then
           elimminf2_coli = 0d0
           return
         endif
 10   continue


      end

************************************************************************
      subroutine setirratterms_coli
************************************************************************
*     sets flag to include IR rational terms                           *
*     in derivatives of 2-point functions                              *
*-----------------------------------------------------------------------
*     27.03.15 Ansgar Denner         last changed  27.03.15            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none

#include "common_coli.h"
#include "params_coli.h"

      ir_rat_terms=.true.
      if(coliinfo)then
        write(ninfout_coli,*) 'COLI: IR rational terms included'
      end if

      end

************************************************************************
      subroutine unsetirratterms_coli
************************************************************************
*     unsets flag to include IR rational terms                         *
*     in derivatives of 2-point functions                              *
*-----------------------------------------------------------------------
*     27.03.15 Ansgar Denner         last changed  27.03.15            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none

#include "common_coli.h"
#include "params_coli.h"

      ir_rat_terms=.false.
      if(coliinfo)then
         write(ninfout_coli,*) 'COLI: IR rational terms not included'
      endif

      end

************************************************************************
      subroutine setinfo_coli
************************************************************************
*     sets photon mass squared regulator to lambda2in                  *
*-----------------------------------------------------------------------
*     15.09.09 Ansgar Denner         last changed  15.09.09            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none

#include "common_coli.h"
#include "params_coli.h"

#ifndef ADcode
      InfLev_coli = .true.
#endif
      coliinfo=.true.
#ifdef ADcode
      if(ninfout_coli.ne.6.and.ninfout_coli.ne.0) then
        write(*,*) 'COLI: information printed'
      end if
#endif
      write(ninfout_coli,*) 'COLI: information printed'

      end
************************************************************************
      subroutine unsetinfo_coli
************************************************************************
*     sets photon mass squared regulator to lambda2in                  *
*-----------------------------------------------------------------------
*     15.09.09 Ansgar Denner         last changed  15.09.09            *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none

#include "common_coli.h"
#include "params_coli.h"

      coliinfo=.false.
#ifndef ADcode
      InfLev_coli = .false.
#endif
#ifdef ADcode
      if(ninfout_coli.ne.6.and.ninfout_coli.ne.0) then
        write(ninfout_coli,*) 'COLI: no information printed'
      endif
      write(*,*) 'COLI: no information printed'
#endif

      end


