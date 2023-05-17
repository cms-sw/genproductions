!!
!!  File coli_c0.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

#include "global_coli.h"

#define QCONT

************************************************************************
************************************************************************
*                                                                      *
*     Scalar 3- point functions                                        *
*                                                                      *
************************************************************************
*                                                                      *
*     last changed  19.02.13  Ansgar Denner                            *
*     errorflags    22.05.13  Ansgar Denner   updated 26.03.15         *
*                                                                      *
************************************************************************
* Subroutines:                                                         *
* Functions:                                                           *
* C0_coli                                                              *
* C0reg_coli,C0m0_coli,C0m0_colia,C02m0_coli,C03m0_coli                *
* C0ms0ir1_coli, C0ms1ir0_coli, C0ms1ir1_coli, C0ms2ir1_coli           *
************************************************************************

************************************************************************
      function C0_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     scalar 3-point function                                          *
*                                                                      *
*                      p23                                             *
*                       |                                              *
*                       |                                              *
*                      / \                                             *
*                     /   \                                            *
*              m22   /     \   m32                                     *
*                   /       \                                          *
*                  /         \                                         *
*       p12  ---------------------  p13                                *
*                      m12                                             *
*                                                                      *
************************************************************************
*     20.08.08 Ansgar Denner        last changed  05.08.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 C0_coli
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      complex*16 p2(3,3),m2(3)

      complex*16 elimminf2_coli
      complex*16 C0reg_coli
      complex*16 C0m0_coli,C02m0_coli,C03m0_coli
      complex*16 C0ms0ir1_coli
      complex*16 C0ms1ir0_coli,C0ms1ir1_coli
      complex*16 C0ms2ir1_coli

      integer i,j,k
      integer nsm,nsoft,ncoll
      logical smallm2(3),smallp2(3,3),soft(3,3,3),coll(3,3),onsh(3,3)
      logical errorwriteflag

      data coll /9*.false./, onsh /9*.false./,
     &    soft /27*.false./ ,smallm2 /3*.false./, smallp2 /9*.false./

#include "params_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 101  format(a22,g24.17)
 111  format(a22,2('(',g24.17,',',g24.17,') ':))


      C0_coli = undefined

      p2(1,2) = p12
      p2(1,3) = p13
      p2(2,3) = p23
      m2(1)   = m12
      m2(2)   = m22
      m2(3)   = m32

c determine infinitesimal parameters
      ms12 = elimminf2_coli(m12)
      ms22 = elimminf2_coli(m22)
      ms32 = elimminf2_coli(m32)
      ps12 = elimminf2_coli(p12)
      ps23 = elimminf2_coli(p23)
      ps13 = elimminf2_coli(p13)


      smallm2(1)=ms12.eq.cd0
      smallm2(2)=ms22.eq.cd0
      smallm2(3)=ms32.eq.cd0
      smallp2(1,2)=ps12.eq.cd0
      smallp2(1,3)=ps13.eq.cd0
      smallp2(2,3)=ps23.eq.cd0

      nsm=0
      do i=1,3
        if(smallm2(i)) nsm=nsm+1
      enddo

c determine on-shell momenta
      onsh(1,2)=ps12.eq.ms22
      onsh(1,3)=ps13.eq.ms32
      onsh(2,1)=ps12.eq.ms12
      onsh(2,3)=ps23.eq.ms32
      onsh(3,1)=ps13.eq.ms12
      onsh(3,2)=ps23.eq.ms22

c determine collinear singularities
      ncoll=0
      do i=1,2
      do j=2,3
        coll(i,j)=smallm2(i).and.smallm2(j).and.smallp2(i,j)
        if(coll(i,j).and.(.not.
     &      ( (p2(i,j).eq.cd0.and.m2(i).eq.m2(j)).or.
     &        (m2(i).eq.cd0.and.onsh(i,j)).or.
     &        (m2(j).eq.cd0.and.onsh(j,i)) ))) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0_coli',' case not supported',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &          ' C0_coli: structure of collinear singularity ',
     &          '    not supported'
            write(nerrout_coli,111) ' C0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0_coli: m32 = ',m32
            write(nerrout_coli,*)   ' C0_coli: i,j = ',i,j
            write(nerrout_coli,*)
     &          ' C0_coli: t1  = ',p2(i,j).eq.cd0,m2(i).eq.m2(j)
            write(nerrout_coli,*)
     &          ' C0_coli: t2  = ',m2(i).eq.cd0,onsh(i,j)
            write(nerrout_coli,*)
     &          ' C0_coli: t3  = ',m2(j).eq.cd0,onsh(j,i)
          endif
          C0_coli = undefined
          return
        endif
        coll(j,i)=coll(i,j)
        if(coll(i,j)) ncoll=ncoll+1
      enddo
      enddo
c determine soft singularities
      nsoft=0
      do i=1,3
        do j=1,2
          if(i.ne.j)then
            do k=j,3
              if(k.ne.i.and.k.ne.j)then
                soft(i,j,k)=smallm2(i).and.onsh(i,j).and.onsh(i,k)
                soft(i,k,j)=soft(i,j,k)
                if(soft(i,j,k)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo


c regular cases
      if(ncoll.eq.0.and.nsoft.eq.0)then
c     regular case
        if(nsm.eq.0)then
            C0_coli = C0reg_coli(ps12,ps23,ps13,ms12,ms22,ms32)
        elseif (nsm.eq.1)then
          if (ms12.eq.cd0) then
            C0_coli = C0m0_coli(ps12,ps23,ps13,cd0,ms22,ms32)
          else if (ms22.eq.cd0) then
            C0_coli = C0m0_coli(ps23,ps13,ps12,cd0,ms32,ms12)
          else if (ms32.eq.cd0) then
            C0_coli = C0m0_coli(ps13,ps12,ps23,cd0,ms12,ms22)
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
             write(nerrout_coli,*) ' C0_coli: inconsistency 0ir 0ms 1m0'
            endif
#endif
          endif
        elseif(nsm.eq.2)then
          if (ms22.ne.cd0) then
            C0_coli = C02m0_coli(ps12,ps23,ps13,cd0,ms22,cd0)
          else if (ms32.ne.cd0) then
            C0_coli = C02m0_coli(ps23,ps13,ps12,cd0,ms32,cd0)
          else if (ms12.ne.cd0) then
            C0_coli = C02m0_coli(ps13,ps12,ps23,cd0,ms12,cd0)
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
             write(nerrout_coli,*) ' C0_coli: inconsistency 0ir 0ms 2m0'
            endif
#endif
          endif
        elseif (nsm.eq.3)then
          C0_coli = C03m0_coli(ps13,ps12,ps23,cd0,cd0,cd0)
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' C0_coli: inconsistency 0ir 0ms'
          endif
#endif
        endif

c IR singular cases without collinear singularities
      elseif(ncoll.eq.0.and.nsoft.ge.1)then
        if(soft(1,2,3))then
          C0_coli = C0ms0ir1_coli(ps12,ps23,ps13,m12,ms22,ms32)
        elseif(soft(2,3,1))then
          C0_coli = C0ms0ir1_coli(ps23,ps13,p12,m22,ms32,ms12)
        elseif(soft(3,1,2))then
          C0_coli = C0ms0ir1_coli(ps13,ps12,ps23,m32,ms12,ms22)
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' C0_coli: inconsistency 1ir 0ms'
          endif
#endif
        endif

c single collinear case
      elseif(ncoll.eq.1)then
        if(coll(1,2))then
          if(soft(1,2,3))then
            C0_coli = C0ms1ir1_coli(p12,ps23,ps13,m12,m22,ms32)
          elseif(soft(2,3,1))then
            C0_coli = C0ms1ir1_coli(p12,ps13,ps23,m22,m12,ms32)
          else
            C0_coli = C0ms1ir0_coli(p12,ps23,ps13,m12,m22,ms32)
          endif
        elseif(coll(2,3))then
          if(soft(2,3,1))then
            C0_coli = C0ms1ir1_coli(p23,ps13,ps12,m22,m32,ms12)
          elseif(soft(3,1,2))then
            C0_coli = C0ms1ir1_coli(p23,ps12,ps13,m32,m22,ms12)
          else
            C0_coli = C0ms1ir0_coli(p23,ps13,ps12,m22,m32,ms12)
          endif
        elseif(coll(1,3))then
          if(soft(3,1,2))then
            C0_coli = C0ms1ir1_coli(p13,ps12,ps23,m32,m12,ms22)
          elseif(soft(1,2,3))then
            C0_coli = C0ms1ir1_coli(p13,ps23,ps12,m12,m32,ms22)
          else
            C0_coli = C0ms1ir0_coli(p13,ps12,ps23,m32,m12,ms22)
          endif
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' C0_coli: inconsistency 1ms'
          endif
#endif
        endif

c double collinear case
      elseif(ncoll.eq.2)then
        if(.not.coll(2,3))then
            C0_coli = C0ms2ir1_coli(p12,ps23,p13,m12,m22,m32)
        elseif(.not.coll(1,3))then
            C0_coli = C0ms2ir1_coli(p23,ps13,p12,m22,m32,m12)
        elseif(.not.coll(1,2))then
            C0_coli = C0ms2ir1_coli(p13,ps12,p23,m32,m12,m22)
        else
          write(nerrout_coli,*) ' C0_coli: inconsistency 2ms'
        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('C0_coli',' case not implemented',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*)
     &        ' C0_coli: case with more than 2 collinear ',
     &        'singularities not implemented'
        endif
#endif
      endif


      end

************************************************************************
      function C0reg_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*  scalar 3-point function, regular case                               *
*                                                                      *
*                      p23                                             *
*                       |                                              *
*                       |                                              *
*                      / \                                             *
*                     /   \                                            *
*              m22   /     \   m32                                     *
*                   /       \                                          *
*                  /         \                                         *
*       p12  ---------------------  p13                                *
*                      m12                                             *
*                                                                      *
*     y(i,1) = y0(i), y(i,2) = y0(i)-1                                 *
*     y(i,3) = y(i-) = y0(i) - x(i-), y(i,4) = y(i+) = y0(i) - x(i+)   *
*     x0(i,1) = -x(i-), x0(i,2) = -x(i+)                               *
*     x1(i,1) = 1-x(i-), x1(i,2) = 1-x(i+)                             *
*----------------------------------------------------------------------*
*  10.05.04 Ansgar Denner      last changed 19.02.13                   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 mi2(5),mi(5),swap
      complex*16 q2(5)
      complex*16 a,b(3),y(3,4),yh,yh0,yh1,yh2,yh3,yh4,yh5
      complex*16 x0(3,2),x1(3,2),deleta
      real*8     iy(3,4),ix0(3,2),iy1y3,iy2y3,iy1y4,iy2y4
      complex*16 cspenc_coli,cspcon_coli,eta2s_coli,eta2_coli,cln_coli
      complex*16 C0reg_coli,C0reg_check

      integer i,j,k
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      complex*16 eta
      logical flag(0:7)
      data    flag /8*.true./
      save    flag
#endif

#include "params_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g24.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(m12.eq.cd0.or.m22.eq.cd0.or.m32.eq.cd0
     &      .or.ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &      .or.ps13.eq.cd0.and.ms12.eq.cd0.and.ms32.eq.cd0
     &      .or.ps23.eq.cd0.and.ms22.eq.cd0.and.ms32.eq.cd0
     &      .or.ms12.eq.cd0.and.ps12.eq.ms22.and.ps13.eq.ms32
     &      .or.ms22.eq.cd0.and.ps12.eq.ms12.and.ps23.eq.ms32
     &      .or.ms32.eq.cd0.and.ps13.eq.ms12.and.ps23.eq.ms22
     &      .or.aimag(p12).ne.0d0.or.aimag(p13).ne.0d0
     &      .or.aimag(p23).ne.0d0
     &   ) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0reg_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0reg_coli called improperly:'
            write(nerrout_coli,100) '     or case not implemented    '
            write(nerrout_coli,111) ' C0reg_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0reg_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0reg_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0reg_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0reg_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0reg_coli: m32 = ',m32
          endif
        endif
      endif
#endif

c C0reg_coli becomes unstable for zero masses and finite width
c in the would-be IR limit


      C0reg_coli=cd0
      C0reg_check=cd0

      mi2(1)=m12
      mi2(2)=m22
      mi2(3)=m32
      mi2(4)=mi2(1)
      mi2(5)=mi2(2)
      mi(1)=sqrt(m12)
      mi(2)=sqrt(m22)
      mi(3)=sqrt(m32)
      mi(4)=mi(1)
      mi(5)=mi(2)
      q2(1)=p12
      q2(2)=p23
      q2(3)=p13
      q2(4)=q2(1)
      q2(5)=q2(2)

      if (p12*p23*p13.ne.cd0) then


        a = sqrt(p12*p12 + p23*p23 + p13*p13
     &       - 2d0*(p12*p23 + p23*p13 + p12*p13))

        if(abs(a).lt.calacc*
     &      max(abs(p12*p12),abs(p23*p23),abs(p13*p13))) then
          call setErrFlag_coli(-7)
          call ErrOut_coli('C0reg_coli','case not implemented',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' C0reg_coli: singular determinant'
            write(nerrout_coli,111)' C0reg_coli: 0= a = ',a
            write(nerrout_coli,111) ' C0reg_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0reg_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0reg_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0reg_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0reg_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0reg_coli: m32 = ',m32
          endif
          C0reg_coli = undefined
          if (abs(a).eq.0d0) return
        endif

        do 10 i=1,3
          j = i+1
          k = i+2
          yh = q2(i)* (q2(i)-q2(j)-q2(k)
     &         +2d0*mi2(k)-mi2(i)-mi2(j))
     &         -(mi2(i)-mi2(j))*(q2(j)-q2(k))
          y(i,1)=(yh+a*( q2(i)-mi2(i)+mi2(j)))/(2d0*a*q2(i))
          y(i,2)=(yh+a*(-q2(i)-mi2(i)+mi2(j)))/(2d0*a*q2(i))
          if (mi2(i)*mi2(j).ne.cd0) then
            b(i)=sqrt((q2(i)-(mi(i)+mi(j))**2)
     &           *(q2(i)-(mi(i)-mi(j))**2))
          else if (mi2(i).ne.cd0) then
            b(i) = q2(i) - mi2(i)
          else
            b(i) = q2(i) - mi2(j)
          endif
          y(i,3)=(yh+a*b(i))/(2d0*a*q2(i))
          y(i,4)=(yh-a*b(i))/(2d0*a*q2(i))


          yh0 = q2(i)*q2(j)*q2(k) + q2(i)*(q2(i)-q2(j)-q2(k))*mi2(k)
     &         + q2(i)*(mi2(k)-mi2(i))*(mi2(k)-mi2(j))
     &         - (q2(j)-q2(k))*mi2(k)*(mi2(i)-mi2(j))
     &         + (mi2(i)-mi2(j))*(q2(j)*mi2(i)-q2(k)*mi2(j))
          yh3 = yh0 + q2(j)*(q2(j)-q2(k)-q2(i))*mi2(i)
     &         + q2(k)*(q2(k)-q2(i)-q2(j))*mi2(j)
          if(abs(y(i,3)).lt.abs(y(i,4))) then
            y(i,3) = yh3/(a*a*q2(i)*y(i,4))
          else
            y(i,4) = yh3/(a*a*q2(i)*y(i,3))
          endif
          if(real(b(i)).ne.0d0) then
            iy(i,3) = real(b(i))
            iy(i,4) = -iy(i,3)
          else
            iy(i,3) =  1d0
            iy(i,4) = -1d0
          endif
          yh1=(yh-a*( q2(i)-mi2(i)+mi2(j)))/(2d0*a*q2(i))
          if(abs(y(i,1)).lt.abs(yh1)) then
            yh4 = yh0 + q2(j)*(q2(j)-q2(k)-q2(i))*mi2(i)
     &        + (q2(k)*(q2(i)+q2(j))-(q2(i)-q2(j))*(q2(i)-q2(j)))*mi2(j)
            y(i,1) = yh4/(a*a*q2(i)*yh1)
          endif
          yh2=(yh-a*(-q2(i)-mi2(i)+mi2(j)))/(2d0*a*q2(i))
          if(abs(y(i,2)).lt.abs(yh2)) then
            yh5 = yh0 + q2(k)*(q2(k)-q2(i)-q2(j))*mi2(j)
     &        + (q2(j)*(q2(k)+q2(i))-(q2(k)-q2(i))*(q2(k)-q2(i)))*mi2(i)
            y(i,2) = yh5/(a*a*q2(i)*yh2)
          endif

          x0(i,1)=-(q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
          x0(i,2)=-(q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))
          x1(i,1)=-(-q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
          x1(i,2)=-(-q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))


          if (abs(x0(i,1)).lt.abs(x0(i,2))) then
            x0(i,1) =  mi2(j)/(q2(i)* x0(i,2))
          else
            x0(i,2) =  mi2(j)/(q2(i)* x0(i,1))
          endif
          if (abs(x1(i,1)).lt.abs(x1(i,2))) then
            x1(i,1) =  mi2(i)/(q2(i)* x1(i,2))
          else
            x1(i,2) =  mi2(i)/(q2(i)* x1(i,1))
          endif


          if(abs(aimag(x0(i,1))).lt.impacc*abs(real(x0(i,1))).or.
     &        abs(aimag(x1(i,1))).lt.impacc*abs(real(x1(i,1)))) then
            x0(i,1)=real(x0(i,1))
            x1(i,1)=real(x1(i,1))
          endif
          if(abs(aimag(x0(i,2))).lt.impacc*abs(real(x0(i,2))).or.
     &        abs(aimag(x1(i,2))).lt.impacc*abs(real(x1(i,2)))) then
            x0(i,2)=real(x0(i,2))
            x1(i,2)=real(x1(i,2))
          endif


c-> 15.01.07:  iy2y3,iy2y4,iy1y3,iy1y4 introduced
          if(y(i,2).ne.0d0)then
            iy2y3=iy(i,3)/real(y(i,2))
          else
            iy2y3=0d0
          endif
          if(y(i,2).ne.0d0)then
            iy2y4=iy(i,4)/real(y(i,2))
          else
            iy2y3=0d0
          endif
          if(y(i,1).ne.0d0)then
            iy1y3=iy(i,3)/real(y(i,1))
          else
            iy1y3=0d0
          endif
          if(y(i,1).ne.0d0)then
            iy1y4=iy(i,4)/real(y(i,1))
          else
            iy1y4=0d0
          endif
          C0reg_coli = C0reg_coli
     &        +cspcon_coli(x1(i,1),1d0/y(i,3),x1(i,1)/y(i,3),
     &                          y(i,2)/y(i,3),iy(i,3),-iy(i,3))
     &         -cspcon_coli(x0(i,1),1d0/y(i,3),x0(i,1)/y(i,3),
     &                          y(i,1)/y(i,3),iy(i,3),-iy(i,3))
     &         +cspcon_coli(x1(i,2),1d0/y(i,4),x1(i,2)/y(i,4),
     &                          y(i,2)/y(i,4),iy(i,4),-iy(i,4))
     &         -cspcon_coli(x0(i,2),1d0/y(i,4),x0(i,2)/y(i,4),
     &                          y(i,1)/y(i,4),iy(i,4),-iy(i,4))
     &        -(eta2_coli(x0(i,1),x0(i,2),mi2(j)/q2(i)
     &           ,iy(i,3),iy(i,4),-real(1d0/q2(i)))
     &         - eta2_coli(y(i,3),y(i,4), yh3/(a*a*q2(i))
     &           ,iy(i,3),iy(i,4),-real(1d0/q2(i))))
     &         * cln_coli(y(i,2)/y(i,1),1d0)




          if(aimag(yh3/(a*a*q2(i))).lt.0d0.and.real(q2(i)).lt.0d0)
     &         C0reg_coli=C0reg_coli+2d0*pi*dcmplx(0d0,1d0)
     &         *cln_coli(y(i,2)/y(i,1),1d0)



 10     continue
        C0reg_coli=C0reg_coli/a




      else if (p23*p12+p13*p23+p12*p13.ne.0d0) then


        if (p23.eq.0d0) then
          mi2(1)=m22
          mi2(2)=m32
          mi2(3)=m12
          mi2(4)=mi2(1)
          mi2(5)=mi2(2)
          swap = mi(1)
          mi(1)=mi(2)
          mi(2)=mi(3)
          mi(3)=swap
          mi(4)=mi(1)
          mi(5)=mi(2)
          q2(1)=p23
          q2(2)=p13
          q2(3)=p12
          q2(4)=q2(1)
          q2(5)=q2(2)
        else if (p13.eq.0d0) then
          mi2(1)=m32
          mi2(2)=m12
          mi2(3)=m22
          mi2(4)=mi2(1)
          mi2(5)=mi2(2)
          swap = mi(1)
          mi(1)=mi(3)
          mi(3)=mi(2)
          mi(2)=swap
          mi(4)=mi(1)
          mi(5)=mi(2)
          q2(1)=p13
          q2(2)=p12
          q2(3)=p23
          q2(4)=q2(1)
          q2(5)=q2(2)
        endif

        a=q2(2)-q2(3)

        if(a.ne.0d0)then


          y(2,1) = ( q2(2)-q2(3)+mi2(1)-mi2(2)) / a
          y(2,2) = ( mi2(1)-mi2(2)) / a
          y(3,1) = ( mi2(2)-mi2(1)) / a
          y(3,2) = ( q2(3)-q2(2)+mi2(2)-mi2(1)) / a

          do 30 i=2,3
            j=i+1
            k=i+2
            b(i)=sqrt((q2(i)-(mi(i)+mi(j))**2)*(q2(i)
     &          -(mi(i)-mi(j))**2))
            yh = q2(i)* (q2(i)-q2(j)-q2(k)
     &          +2d0*mi2(k)-mi2(i)-mi2(j))
     &          -(mi2(i)-mi2(j))*(q2(j)-q2(k))
            y(i,3) = (yh+a*b(i))/(2d0*a*q2(i))
            y(i,4) = (yh-a*b(i))/(2d0*a*q2(i))


            yh0 = q2(i)*q2(j)*q2(k) + q2(i)*(q2(i)-q2(j)-q2(k))*mi2(k)
            if(.not.((mi2(k).eq.mi2(i).and.q2(k).eq.cd0).or.
     &               (mi2(k).eq.mi2(j).and.q2(j).eq.cd0))) then
              yh0 = yh0
     &          + q2(i)*(mi2(k)-mi2(i))*(mi2(k)-mi2(j))
     &          - (q2(j)-q2(k))*mi2(k)*(mi2(i)-mi2(j))
     &          + (mi2(i)-mi2(j))*(q2(j)*mi2(i)-q2(k)*mi2(j))
            end if

            yh3 = yh0 + q2(j)*(q2(j)-q2(k)-q2(i))*mi2(i)
     &          + q2(k)*(q2(k)-q2(i)-q2(j))*mi2(j)


            if(abs(y(i,3)).lt.abs(y(i,4))) then
              y(i,3) = yh3/(a*a*q2(i)*y(i,4))
            else
              y(i,4) = yh3/(a*a*q2(i)*y(i,3))
            endif


            if(real(b(i)).ne.0d0) then
              iy(i,3) = real(b(i))
              iy(i,4) = -iy(i,3)
            else
              iy(i,3) =  1d0
              iy(i,4) = -1d0
            endif
            x0(i,1)=-(q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
            x0(i,2)=-(q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))
            x1(i,1)=-(-q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
            x1(i,2)=-(-q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))


            if (abs(x0(i,1)).lt.abs(x0(i,2))) then
              x0(i,1) =  mi2(j)/(q2(i)* x0(i,2))
            else
              x0(i,2) =  mi2(j)/(q2(i)* x0(i,1))
            endif
            if (abs(x1(i,1)).lt.abs(x1(i,2))) then
              x1(i,1) =  mi2(i)/(q2(i)* x1(i,2))
            else
              x1(i,2) =  mi2(i)/(q2(i)* x1(i,1))
            endif

            if(abs(aimag(x0(i,1))).lt.impacc*abs(real(x0(i,1))).or.
     &          abs(aimag(x1(i,1))).lt.impacc*abs(real(x1(i,1)))) then
              x0(i,1)=real(x0(i,1))
              x1(i,1)=real(x1(i,1))
            endif
            if(abs(aimag(x0(i,2))).lt.impacc*abs(real(x0(i,2))).or.
     &          abs(aimag(x1(i,2))).lt.impacc*abs(real(x1(i,2)))) then
              x0(i,2)=real(x0(i,2))
              x1(i,2)=real(x1(i,2))
            endif

            if(y(i,2).ne.0d0)then
              iy2y3=iy(i,3)/real(y(i,2))
            else
              iy2y3=0d0
            endif
            if(y(i,2).ne.0d0)then
              iy2y4=iy(i,4)/real(y(i,2))
            else
              iy2y3=0d0
            endif
            if(y(i,1).ne.0d0)then
              iy1y3=iy(i,3)/real(y(i,1))
            else
              iy1y3=0d0
            endif
            if(y(i,1).ne.0d0)then
              iy1y4=iy(i,4)/real(y(i,1))
            else
              iy1y4=0d0
            endif



            C0reg_coli = C0reg_coli
     &         +cspcon_coli(x1(i,1),1d0/y(i,3),x1(i,1)/y(i,3)
     &                          ,y(i,2)/y(i,3),iy(i,3),-iy(i,3))
     &         +cspcon_coli(x1(i,2),1d0/y(i,4),x1(i,2)/y(i,4)
     &                          ,y(i,2)/y(i,4),iy(i,4),-iy(i,4))
     &         -cspcon_coli(x0(i,1),1d0/y(i,3),x0(i,1)/y(i,3)
     &                          ,y(i,1)/y(i,3),iy(i,3),-iy(i,3))
     &         -cspcon_coli(x0(i,2),1d0/y(i,4),x0(i,2)/y(i,4)
     &                          ,y(i,1)/y(i,4),iy(i,4),-iy(i,4))


            deleta = eta2_coli(x0(i,1),x0(i,2),mi2(j)/q2(i)
     &           ,iy(i,3),iy(i,4),-real(1d0/q2(i)))
     &         - eta2_coli(y(i,3),y(i,4), yh3/(a*a*q2(i))
     &           ,iy(i,3),iy(i,4),-real(1d0/q2(i)))

            if(deleta.ne.0d0)then
              C0reg_coli = C0reg_coli
     &            -deleta*cln_coli(y(i,2)/y(i,1),1d0)


            endif


            if(aimag(yh3/(a*a*q2(i))).lt.-abs(yh3/(a*a*q2(i)))*impacc
     &          .and.real(q2(i)).lt.0d0)
     &          C0reg_coli=C0reg_coli+2d0*pi*dcmplx(0d0,1d0)
     &          *cln_coli(y(i,2)/y(i,1),1d0)



 30       continue
          C0reg_coli=C0reg_coli/a



        else

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('C0reg_coli',
     &          'new function called: p12=0, p23=p13',
     &          errorwriteflag)
            write(nerrout_coli,100)
     &          ' C0reg_coli: p12=0, p23=p13 or symm. '
            write(nerrout_coli,100) '    case not yet tested at all'
            write(nerrout_coli,111) ' C0reg_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0reg_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0reg_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0reg_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0reg_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0reg_coli: m32 = ',m32
            flag(3)=.false.
          endif
#endif

          do 31 i=2,3
            j=i+1
            k=i+2
            b(i)=sqrt((q2(i)-(mi(i)+mi(j))**2)
     &          *(q2(i)-(mi(i)-mi(j))**2))
            if(real(b(i)).ne.0d0) then
              ix0(i,1) = real(b(i))
              ix0(i,2) = -ix0(i,1)
            else
              ix0(i,1) =  1d0
              ix0(i,2) = -1d0
            endif
            x0(i,1)=-(q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
            x0(i,2)=-(q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))
            x1(i,1)=-(-q2(i)-mi2(i)+mi2(j)-b(i))/(2d0*q2(i))
            x1(i,2)=-(-q2(i)-mi2(i)+mi2(j)+b(i))/(2d0*q2(i))


            if (abs(x0(i,1)).lt.abs(x0(i,2))) then
              x0(i,1) =  mi2(j)/(q2(i)* x0(i,2))
            else
              x0(i,2) =  mi2(j)/(q2(i)* x0(i,1))
            endif
            if (abs(x1(i,1)).lt.abs(x1(i,2))) then
              x1(i,1) =  mi2(i)/(q2(i)* x1(i,2))
            else
              x1(i,2) =  mi2(i)/(q2(i)* x1(i,1))
            endif

            if(abs(aimag(x0(i,1))).lt.impacc*abs(real(x0(i,1))).or.
     &          abs(aimag(x1(i,1))).lt.impacc*abs(real(x1(i,1)))) then
              x0(i,1)=real(x0(i,1))
              x1(i,1)=real(x1(i,1))
            endif
            if(abs(aimag(x0(i,2))).lt.impacc*abs(real(x0(i,2))).or.
     &          abs(aimag(x1(i,2))).lt.impacc*abs(real(x1(i,2)))) then
              x0(i,2)=real(x0(i,2))
              x1(i,2)=real(x1(i,2))
            endif

 31       continue



          C0reg_coli =
     &         x1(2,1)
     &        *(cln_coli(x1(2,1),ix0(2,1))-cln_coli(x0(2,1),ix0(2,1)))
     &        +x1(2,2)
     &        *(cln_coli(x1(2,2),ix0(2,2))-cln_coli(x0(2,2),ix0(2,2)))
     &        +x0(3,1)
     &        *(cln_coli(x0(3,1),ix0(3,1))-cln_coli(x1(3,1),ix0(3,1)))
     &        +x0(3,2)
     &        *(cln_coli(x0(3,2),ix0(3,2))-cln_coli(x1(3,2),ix0(3,2)))


          C0reg_coli=C0reg_coli/(mi2(1)-mi2(2))
        endif


      else
        if (p23.ne.0d0) then
          mi2(1)=m32
          mi2(2)=m12
          mi2(3)=m22
          mi2(4)=mi2(1)
          mi2(5)=mi2(2)
          swap = mi(1)
          mi(1)=mi(3)
          mi(3)=mi(2)
          mi(2)=swap
          mi(4)=mi(1)
          mi(5)=mi(2)
          q2(1)=p13
          q2(2)=p12
          q2(3)=p23
          q2(4)=q2(1)
          q2(5)=q2(2)
        else if (p12.ne.0d0) then
          mi2(1)=m22
          mi2(2)=m32
          mi2(3)=m12
          mi2(4)=mi2(1)
          mi2(5)=mi2(2)
          swap = mi(1)
          mi(1)=mi(2)
          mi(2)=mi(3)
          mi(3)=swap
          mi(4)=mi(1)
          mi(5)=mi(2)
          q2(1)=p23
          q2(2)=p13
          q2(3)=p12
          q2(4)=q2(1)
          q2(5)=q2(2)
        endif

        a=-q2(3)

        if(abs(a).lt.calacc*
     &      max(abs(m12),abs(m22),abs(m32))) then
          call setErrFlag_coli(-7)
          call ErrOut_coli('C0reg_coli','case not implemented',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' C0reg_coli: 3 vanishing invariants'
            write(nerrout_coli,111) ' C0reg_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0reg_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0reg_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0reg_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0reg_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0reg_coli: m32 = ',m32
          endif
          C0reg_coli = undefined
          if (abs(a).eq.0d0) return
        endif


#ifdef WARN
        if(aimag(a).ne.0d0) then
          call setErrFlag_coli(-4)
          call ErrOut_coli('C0reg_coli',' imaginary a not supported',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' etas required'
          endif
        endif
#endif

        b(3)=sqrt((q2(3)-(mi(3)+mi(1))**2)*(q2(3)-(mi(3)-mi(1))**2))
        yh = q2(3)* (q2(3) + 2d0*mi2(2)-mi2(3)-mi2(1))
        y(3,1)=2d0*q2(3)*( mi2(2)-mi2(1))
        y(3,2)=2d0*q2(3)*( q2(3)+mi2(2)-mi2(1))
        y(3,3)=yh+a*b(3)
        y(3,4)=yh-a*b(3)
        yh0 = q2(3)*q2(3)*mi2(2)
     &           + q2(3)*(mi2(2)-mi2(3))*(mi2(2)-mi2(1))
        if(abs(y(3,3)).lt.abs(y(3,4))) then
           y(3,3) = 4d0*yh0*q2(3)/y(3,4)
        else
           y(3,4) = 4d0*yh0*q2(3)/y(3,3)
        endif


        if (mi2(2).ne.mi2(3)) then

#ifdef NEWCHECK
          if(flag(4))then
            call ErrOut_coli('C0reg_coli',
     &          'new function called: p12=0=p23, m22=/=m32',
     &          errorwriteflag)
            write(nerrout_coli,100) ' C0reg_coli: p12=0=p23, m22=/=m32'
            write(nerrout_coli,100)
     &          '    case not yet tested in physical process'
            write(nerrout_coli,111) ' C0reg_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0reg_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0reg_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0reg_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0reg_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0reg_coli: m32 = ',m32
            flag(4)=.false.
          endif
#endif

          y(2,1) = (-q2(3)+mi2(1)-mi2(2)) / a
          y(2,2) = ( mi2(1)-mi2(2)) / a
          y(2,3) = (mi2(1)-mi2(2)-q2(3)*mi2(2)/(mi2(2)-mi2(3))) / a
          iy(2,3) = -real(1d0/(mi2(2)-mi2(3)))
          x1(2,1) = y(2,3)-y(2,2)
          x1(2,1) = mi2(2)/(mi2(2)-mi2(3))
          x0(2,1) = y(2,3)-y(2,1)
          x0(2,1) = mi2(3)/(mi2(2)-mi2(3))


          if(y(2,2).ne.0d0)then
            iy2y3=iy(2,3)/real(y(2,2))
          else
            iy2y3=0d0
          endif
          if(y(2,2).ne.0d0)then
            iy2y4=iy(2,4)/real(y(2,2))
          else
            iy2y4=0d0
          endif

          C0reg_coli =
     &       cspcon_coli(x1(2,1),1d0/y(2,3),x1(2,1)/y(2,3),y(2,2)/y(2,3)
     &      ,iy(2,3),-iy(2,3))
     &      -cspcon_coli(x0(2,1),1d0/y(2,3),x0(2,1)/y(2,3),y(2,1)/y(2,3)
     &      ,iy(2,3),-iy(2,3))


          deleta = eta2_coli(mi2(2)-mi2(3),x0(2,1),mi2(3)
     &        ,-1d0,iy(2,3),-1d0)
     &        - eta2_coli(mi2(2)-mi2(3),y(2,3),(mi2(2)-mi2(3))*y(2,3)
     &        ,-1d0,iy(2,3),-1d0)


          if(deleta.ne.0d0)then
            C0reg_coli = C0reg_coli
     &          -deleta*cln_coli(y(2,2)/y(2,1),1d0)


          endif


        else


        endif

        y(3,1) = ( mi2(2)-mi2(1)) / a
        y(3,2) = ( q2(3)-q2(2)+mi2(2)-mi2(1)) / a

        b(3)=sqrt((q2(3)-(mi(3)+mi(1))**2)*(q2(3)-(mi(3)-mi(1))**2))
        yh = q2(3)*(q2(3)+2d0*mi2(2)-mi2(3)-mi2(1))
        y(3,3) = (yh+a*b(3))/(2d0*a*q2(3))
        y(3,4) = (yh-a*b(3))/(2d0*a*q2(3))
        yh0 = q2(3)*q2(3)*mi2(2)
     &           + q2(3)*(mi2(2)-mi2(3))*(mi2(2)-mi2(1))
        yh3 = yh0
        if(abs(y(3,3)).lt.abs(y(3,4))) then
          y(3,3) = yh3/(a*a*q2(3)*y(3,4))
        else
          y(3,4) = yh3/(a*a*q2(3)*y(3,3))
        endif
        if(real(b(3)).ne.0d0) then
          iy(3,3) = real(b(3))
          iy(3,4) = -iy(3,3)
        else
          iy(3,3) =  1d0
          iy(3,4) = -1d0
        endif
        x0(3,1)=-(q2(3)-mi2(3)+mi2(1)-b(3))/(2d0*q2(3))
        x0(3,2)=-(q2(3)-mi2(3)+mi2(1)+b(3))/(2d0*q2(3))
        x1(3,1)=-(-q2(3)-mi2(3)+mi2(1)-b(3))/(2d0*q2(3))
        x1(3,2)=-(-q2(3)-mi2(3)+mi2(1)+b(3))/(2d0*q2(3))


        if (abs(x0(3,1)).lt.abs(x0(3,2))) then
          x0(3,1) =  mi2(1)/(q2(3)* x0(3,2))
        else
          x0(3,2) =  mi2(1)/(q2(3)* x0(3,1))
        endif
        if (abs(x1(3,1)).lt.abs(x1(3,2))) then
          x1(3,1) =  mi2(3)/(q2(3)* x1(3,2))
        else
          x1(3,2) =  mi2(3)/(q2(3)* x1(3,1))
        endif

        if(abs(aimag(x0(3,1))).lt.impacc*abs(real(x0(3,1))).or.
     &      abs(aimag(x1(3,1))).lt.impacc*abs(real(x1(3,1)))) then
          x0(3,1)=real(x0(3,1))
          x1(3,1)=real(x1(3,1))
        endif
        if(abs(aimag(x0(3,2))).lt.impacc*abs(real(x0(3,2))).or.
     &      abs(aimag(x1(3,2))).lt.impacc*abs(real(x1(3,2)))) then
          x0(3,2)=real(x0(3,2))
          x1(3,2)=real(x1(3,2))
        endif

        if(y(3,2).ne.0d0)then
          iy2y3=iy(3,3)/real(y(3,2))
        else
          iy2y3=0d0
        endif
        if(y(3,2).ne.0d0)then
          iy2y4=iy(3,4)/real(y(3,2))
        else
          iy2y3=0d0
        endif
        if(y(3,1).ne.0d0)then
          iy1y3=iy(3,3)/real(y(3,1))
        else
          iy1y3=0d0
        endif
        if(y(3,1).ne.0d0)then
          iy1y4=iy(3,4)/real(y(3,1))
        else
          iy1y4=0d0
        endif


        C0reg_coli = C0reg_coli
     &      +cspcon_coli(x1(3,1),1d0/y(3,3),x1(3,1)/y(3,3),y(3,2)/y(3,3)
     &      ,iy(3,3),-iy(3,3))
     &      +cspcon_coli(x1(3,2),1d0/y(3,4),x1(3,2)/y(3,4),y(3,2)/y(3,4)
     &      ,iy(3,4),-iy(3,4))
     &      -cspcon_coli(x0(3,1),1d0/y(3,3),x0(3,1)/y(3,3),y(3,1)/y(3,3)
     &      ,iy(3,3),-iy(3,3))
     &      -cspcon_coli(x0(3,2),1d0/y(3,4),x0(3,2)/y(3,4),y(3,1)/y(3,4)
     &      ,iy(3,4),-iy(3,4))


        deleta = eta2_coli(x0(3,1),x0(3,2),mi2(1)/q2(3)
     &      ,iy(3,3),iy(3,4),-real(1d0/q2(3)))
     &      - eta2_coli(y(3,3),y(3,4), yh3/(a*a*q2(3))
     &      ,iy(3,3),iy(3,4),-real(1d0/q2(3)))


        if(deleta.ne.0d0)then
          C0reg_coli = C0reg_coli
     &        -deleta*cln_coli(y(3,2)/y(3,1),1d0)


        endif

        if(aimag(yh3/(a*a*q2(3))).lt.0d0.and.real(q2(3)).lt.0d0)
     &      C0reg_coli=C0reg_coli+2d0*pi*dcmplx(0d0,1d0)
     &      *cln_coli(y(3,2)/y(3,1),1d0)




        C0reg_coli=C0reg_coli/a

      endif



      end

************************************************************************
      function C0m0_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     regular scalar 3-point function with m12 = 0                     *
*     assumes no mass singularities and no ir singularities            *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*                   m22    /     \   m32                               *
*                         /       \                                    *
*                        /         \                                   *
*              p12  ---------------------  p13                         *
*                          m12 = 0                                     *
*                                                                      *
*----------------------------------------------------------------------*
*     13.05.04 Ansgar Denner        last changed  26.09.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13,m12,m22,m32
      complex*16 C0m0_coli

      complex*16 m2,m3
      complex*16 k
      complex*16 r,det,rp,detp
      real*8     ir,irp
      complex*16 mm22,mm32,q13,q12,sq12,sq13
      real*8     ix1,iz1,ix3
      complex*16 x1,x2,z1,z2,x3,invx3
      complex*16 cspenc_coli,cspcos_coli,cln_coli
      complex*16 C0m0_check
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13,ms12,ms22,ms32
      logical flag(0:8)
      data    flag /9*.true./
      save    flag
#endif

#include "params_coli.h"
#include "checkparams_coli.h"


 100  format((a))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g24.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(m12.ne.cd0.or.ps12.eq.ms22.and.ps13.eq.ms32
     &        .or.ms32.eq.cd0.or.ms22.eq.cd0
     &        .or.ps23.eq.cd0.and.ms32.eq.cd0.and.ms22.eq.cd0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0m0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0m0_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented'
            write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32,ms32
          endif
          C0m0_coli = undefined
          if(abs(ms32).eq.0d0.or.abs(ms22).eq.0d0) return
        endif
      endif
#endif

      if (p12.ne.cd0.and.p13.ne.cd0.and.p23.ne.cd0) then


        if (m32.ne.p13) then
          mm32 = m32
          mm22 = m22
          q12 = p12
          q13 = p13
        else
          mm32 = m22
          mm22 = m32
          q12 = p13
          q13 = p12
        endif

        m2 = sqrt(mm22)
        m3 = sqrt(mm32)
        det = sqrt((p23-(m2+m3)*(m2+m3))*(p23-(m2-m3)*(m2-m3)))

        k = (mm32+mm22-p23)/(m2*m3)
        det = sqrt((p23-(m2+m3)*(m2+m3))*
     &      (p23-(m2-m3)*(m2-m3))) * sign(1d0,real(k))
        r = 0.5d0*k + det/(2d0*m2*m3)


        if(real(k).le.-2d0+calacc) then
          ir = sign(1d0,1d0-abs(r))
          if(ir.eq.0d0) ir=1d0            ! check
        else
          ir = 0D0
        endif
        sq13 = sqrt(q13)
        sq12 = sqrt(q12)
        if (real(q12).ge.0d0.and.real(q13).ge.0d0) then
          detp = sqrt((p23-(sq12+sq13)*(sq12+sq13))*
     &        (p23-(sq12-sq13)*(sq12-sq13)))
        else
          detp = sqrt((q13-p23)**2 + q12*q12 - 2d0*q12*(q13+p23))
        endif

        if(abs(detp).lt.calacc*
     &      max(abs(q12*q12),abs(p23*p23),abs(q13*q13))) then
          call setErrFlag_coli(-7)
          call ErrOut_coli('C0m0_coli','case not implemented',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0m0_coli: singular determinant'
            write(nerrout_coli,111) ' C0m0_coli: det = ',detp
            write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32
          endif
          C0m0_coli = undefined
          if (abs(detp).eq.0d0) return
        endif




        detp=detp* sign(1d0,real(q13+q12-p23))
        rp=((q13+q12-p23)+detp)/(2d0*sq12*sq13)
        irp =-1d0


        C0m0_coli = cspenc_coli(1d0-sq13/(sq12*rp),irp)
     &      - cspenc_coli(1d0-sq13*rp/sq12,-irp)
     &      +(cln_coli(sq13*rp/sq12,irp)
     &      -cln_coli(sq13/(sq12*rp),-irp))
     &      *cln_coli((mm32-q13)/(mm32),-1d0)
     &      + cspcos_coli(m2/(m3*r),sq13*rp/sq12,-ir,irp)
     &      + cspcos_coli(m2*r/m3,sq13*rp/sq12,ir,irp)
     &      - cspcos_coli(m2/(m3*r),sq13/(sq12*rp),-ir,-irp)
     &      - cspcos_coli(m2*r/m3,sq13/(sq12*rp),ir,-irp)
     &      + cspcos_coli((mm22-q12)/(mm32-q13),sq13/(sq12*rp)
     &      ,real(mm22-q12-mm32+q13),-irp)
     &        - cspcos_coli((mm22-q12)/(mm32-q13),sq13*rp/sq12
     &      ,real(mm22-q12-mm32+q13),irp)



        C0m0_coli = C0m0_coli/detp



      else if (p23.eq.cd0) then
        if(p12.ne.p13) then
          if (m32.ne.p13.and.p12.ne.cd0) then
            mm32 = m32
            mm22 = m22
            q12 = p12
            q13 = p13
          else
            mm32 = m22
            mm22 = m32
            q12 = p13
            q13 = p12
          endif
          x1 = -mm22/mm32
          x3 = -(mm22-q12)/(mm32-q13)
          ix1 = real(mm32 - mm22)
          ix3 = real(mm32-q13 - mm22+q12)


          C0m0_coli = cspenc_coli(1d0+x3,ix3) - cspenc_coli(1+x1,ix1)
          if(q13.ne.cd0) then
            z1 = -q12/q13
            iz1 = real(q12 - q13)
            C0m0_coli = C0m0_coli
     &           + cln_coli(-z1,-iz1)
     &          *cln_coli(mm32/(mm32-q13),real(p13))
     &           + cspcos_coli(-x1,-1d0/z1,-ix1,iz1)
     &           - cspcos_coli(-x3,-1d0/z1,-ix3,iz1)
          endif
          C0m0_coli = C0m0_coli/(q13-q12)

        elseif(p12.ne.cd0)then


          if (m32.ne.m22) then

#ifdef NEWCHECK
            if(flag(3))then
              call ErrOut_coli('C0m0_coli',
     &              'new function called: p23=0, p12=p13',
     &            errorwriteflag)
              write(nerrout_coli,100) ' C0m0_coli: p23=0, p12=p13 '
              write(nerrout_coli,100) '    case not yet tested at all'
              write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32
              flag(3)=.false.
            endif
#endif

            C0m0_coli = (1d0-m22/p12)*cln_coli(1d0-m22/p12,real(p12))
     &          -(1d0-m32/p12)*cln_coli(1d0-m32/p12,real(p12))
     &          +m22/p12*cln_coli(-m22/p12,real(p12))
     &          -m32/p12*cln_coli(-m32/p12,real(p12))
            C0m0_coli = C0m0_coli/(m32-m22)
          else

#ifdef NEWCHECK
            if(flag(7))then
              call ErrOut_coli('C0m0_coli',
     &           'new function called: p23=0, p12=p13, m22=m32',
     &          errorwriteflag)
              write(nerrout_coli,100)
     &            ' C0m0_coli: p23=0, p12=p13, m22=m32 '
              write(nerrout_coli,100) '    case not yet tested at all'
              write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32
              flag(7)=.false.
            endif
#endif

            C0m0_coli =
     &          (cln_coli(1d0-m32/p12,real(p12))
     &          -cln_coli(-m32/p12,real(p12)))/p12
          endif

        elseif(m22.ne.m32)then

#ifdef NEWCHECK
          if(flag(4))then
            call ErrOut_coli('C0m0_coli',
     &           'new function called: p23=0=p12=p13',
     &          errorwriteflag)
            write(nerrout_coli,100) ' C0m0_coli: p23=0=p12=p13 '
            write(nerrout_coli,100) '    case not yet tested at all'
            write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32
            flag(4)=.false.
          endif
#endif

          C0m0_coli = log(m32/m22)/(m22-m32)
        else

#ifdef NEWCHECK
          if(flag(8))then
            call ErrOut_coli('C0m0_coli',
     &            'new function called: p23=0=p12=p13 m32=m22',
     &          errorwriteflag)
            write(nerrout_coli,100) ' C0m0_coli: p23=0=p12=p13 m32=m22'
            write(nerrout_coli,100) '    case not yet tested at all'
            write(nerrout_coli,111) ' C0m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0m0_coli: m32 = ',m32
            flag(8)=.false.
          endif
#endif

          C0m0_coli = -1d0/m32
        endif

      else

        if (p13.ne.cd0) then
          mm32 = m32
          mm22 = m22
          q13 = p13
        else
          mm32 = m22
          mm22 = m32
          q13 = p12
        endif
        m2 = sqrt(mm22)
        m3 = sqrt(mm32)
        det = sqrt((p23-(m2+m3)*(m2+m3))*(p23-(m2-m3)*(m2-m3)))
        x2 = (p23-mm32-mm22-det)/(2d0*mm32)
        x1 = (p23-mm32-mm22+det)/(2d0*mm32)
        if (abs(x2).lt.abs(x1)) then
          x2 = mm22/(mm32*x1)
        else
          x1 = mm22/(mm32*x2)
        endif
        if(det.ne.cd0)then
          ix1 = real(x1/det)
        else
          ix1 = 1d0
        endif

        if (q13.ne.cd0) then


          invx3 = -(mm32-q13)/mm22
          ix3 = real(mm32-q13-mm22)

          z1 = p23/q13-1d0
          iz1 = real(q13-p23)

          C0m0_coli = cspcos_coli(-1d0/x1,-z1,ix1,-iz1)
     &         +cspcos_coli(-1d0/x2,-z1,-ix1,-iz1)
     &         -cspenc_coli(1d0+z1,iz1)
     &         -cspcos_coli(-invx3,-z1,ix3,-iz1)
          C0m0_coli = C0m0_coli/(q13-p23)


        else


          C0m0_coli = cln_coli(-x1,-ix1)**2 +  cln_coli(-x2,ix1)**2
     &         - log(mm22/mm32)**2
          C0m0_coli = C0m0_coli/(2d0*p23)
        endif
      endif
      end


************************************************************************
      function C02m0_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     regular scalar 3-point function with m12 = m32 = 0               *
*     assumes no mass singularities and no ir singularities            *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*                    m22   /     \   m32 = 0                           *
*                         /       \                                    *
*                        /         \                                   *
*              p12  ---------------------  p13                         *
*                          m12 = 0                                     *
*                                                                      *
*----------------------------------------------------------------------*
*     18.05.04 Ansgar Denner        last changed  25.09.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13,m12,m22,m32
      complex*16 C02m0_coli

      complex*16 sp23,sp12
      complex*16 q23,q12
      real*8     ix1,iz1,ix3
      complex*16 x1,z1,z2,detp,x3
      complex*16 cspenc_coli,cspcos_coli,cln_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13,ms12,ms22,ms32
      logical flag(0:5)
      data    flag /6*.true./
      save    flag
#endif

#include "params_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      C02m0_coli=undefined

#ifdef CHECK
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(m12.ne.cd0.or.m32.ne.cd0
     &      .or.m22.eq.cd0
     &      .or.m22.eq.cd0.and.p23.eq.cd0
     &      .or.m22.eq.cd0.and.p12.eq.cd0
     &      .or.p13.eq.cd0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C02m0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C02m0_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented '
            write(nerrout_coli,111) ' C02m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C02m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C02m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C02m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C02m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C02m0_coli: m32 = ',m32
          endif
          C02m0_coli = undefined
          if (abs(m22).eq.0d0) return
        endif
      endif
#endif

      if (p12.ne.cd0.and.p13.ne.cd0.and.p23.ne.cd0) then
        if (p12.ne.m22) then
          q12 = p12
          q23 = p23
        else
          q12 = p23
          q23 = p12
        endif


        if (real(q12).ge.0d0.and.real(p13).ge.0d0) then
          sp12 = sqrt(q12)
          sp23 = sqrt(q23)
          detp = sqrt((p13-(sp12+sp23)*(sp12+sp23))*
     &         (p13-(sp12-sp23)*(sp12-sp23)))
        else
          detp = sqrt((p13-q23)**2 + q12*q12 - 2d0*q12*(p13+q23))
        endif

        if(abs(detp).lt.calacc*
     &      max(abs(q12*q12),abs(p23*p23),abs(p13*p13))) then
          call setErrFlag_coli(-7)
          call ErrOut_coli('C02m0_coli','case not implemented',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C02m0_coli: singular determinant'
            write(nerrout_coli,111) ' C02m0_coli: det = ',detp
            write(nerrout_coli,111) ' C02m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C02m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C02m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C02m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C02m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C02m0_coli: m32 = ',m32
          endif
          C02m0_coli = undefined
          if (abs(detp).eq.0d0) return
        endif

        z2 = (q12-p13-q23-detp)/(2d0*p13)
        z1 = (q12-p13-q23+detp)/(2d0*p13)
        if (abs(z2).lt.abs(z1)) then
          z2 = q23/(p13*z1)
        else
          z1 = q23/(p13*z2)
        endif
        iz1 = real(z1/detp)

        x1  = (m22-q23)/p13
        ix1 = real(-p13 - m22 + q23)
        x3  = (q12-m22)/m22
        ix3 = 1d0

        C02m0_coli = cspenc_coli(1d0+z2,-iz1)
     &       - cspenc_coli(1d0+z1,iz1)
     &       - ((cln_coli(-z2,iz1))**2 - (cln_coli(-z1,-iz1))**2)/2d0
     &       + (cln_coli(-z2,iz1)-cln_coli(-z1,-iz1))
     &           *cln_coli(-m22/p13,real(p13))
     &       + cspcos_coli(-x1,-1d0/z1,-ix1,iz1)
     &       - cspcos_coli(-x1,-1d0/z2,-ix1,-iz1)
     &       - cspcos_coli(-x3,-z2,-ix3,iz1)
     &       + cspcos_coli(-x3,-z1,-ix3,-iz1)
        C02m0_coli = -C02m0_coli/detp


      else if (p13.ne.cd0) then
        if (p23.ne.cd0) then
          q23 = p23
        else
          q23 = p12
        endif

        if (abs(q23).lt.abs(q23-m22)) then

          z1  = -q23/p13
          iz1 = real(q23-p13)
          x1  = p13/(m22-q23)
          ix1 = real(p13 + m22 - q23)

          C02m0_coli = -cspenc_coli(1d0+x1,ix1)
          if (q23.ne.cd0) then


            C02m0_coli = C02m0_coli
     &           + cln_coli(-z1,-iz1)
     &          *cln_coli(m22/(m22-q23),real(q23))
     &           + cspcos_coli(-x1,-z1,-ix1,-iz1)
          else


            C02m0_coli = C02m0_coli + pi2_6
          endif
          C02m0_coli = C02m0_coli/(p13-q23)
        else

          z1  = -q23/p13
          iz1 = real(q23-p13)
          x1  = (m22-q23)/p13
          ix1 = real(q23-p13 - m22)

          C02m0_coli = -cln_coli(-z1,-iz1)**2/2d0
     &         + cln_coli(-z1,-iz1)*cln_coli(-m22/p13,real(p13+m22))
          if (q23.ne.m22) then


            C02m0_coli = C02m0_coli
     &           + cspenc_coli(1d0+x1,ix1)
     &           - cspcos_coli(-x1,-1d0/z1,-ix1,iz1)

          else

#ifdef NEWCHECK
            if(flag(5))then
              call ErrOut_coli('C02m0_coli',
     &          'new function called: p12=0, p23=m22',
     &            errorwriteflag)
              write(nerrout_coli,100) ' C02m0_coli: p12=0, p23=m22'
              write(nerrout_coli,100)
     &            '    case not yet tested in physical process'
              write(nerrout_coli,111) ' C02m0_coli: p12 = ',p12
              write(nerrout_coli,111) ' C02m0_coli: p23 = ',p23
              write(nerrout_coli,111) ' C02m0_coli: p13 = ',p13
              write(nerrout_coli,111) ' C02m0_coli: m12 = ',m12
              write(nerrout_coli,111) ' C02m0_coli: m22 = ',m22
              write(nerrout_coli,111) ' C02m0_coli: m32 = ',m32
              flag(5)=.false.
            endif
#endif

            C02m0_coli = C02m0_coli
          endif
          C02m0_coli = C02m0_coli/(p13-q23)
        endif
#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('C02m0_coli',' wrong arguments',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' C02m0 called with improper arguments'
          write(nerrout_coli,100) '     or case not implemented    '
          write(nerrout_coli,111) ' C02m0_coli: p12 = ',p12
          write(nerrout_coli,111) ' C02m0_coli: p23 = ',p23
          write(nerrout_coli,111) ' C02m0_coli: p13 = ',p13
          write(nerrout_coli,111) ' C02m0_coli: m12 = ',m12
          write(nerrout_coli,111) ' C02m0_coli: m22 = ',m22
          write(nerrout_coli,111) ' C02m0_coli: m32 = ',m32
        endif
#endif
      endif


      end

************************************************************************
      function C03m0_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     regular scalar 3-point function with m12 = m22 = m32 = 0         *
*     assumes no mass singularities and no ir singularities            *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*                m22 = 0   /     \   m32 = 0                           *
*                         /       \                                    *
*                        /         \                                   *
*              p12  ---------------------  p13                         *
*                          m12 = 0                                     *
*                                                                      *
*----------------------------------------------------------------------*
*     18.05.04 Ansgar Denner        last changed  28.08.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13,m12,m22,m32
      complex*16 C03m0_coli

      complex*16 sp12,sp13
      real*8     ix1,iz1
      complex*16 x1,z1,z2,detp
      complex*16 cspenc_coli,cspcos_coli,cln_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13,ms12,ms22,ms32
#endif

#include "params_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(m12.ne.cd0.or.m22.ne.cd0.or.m32.ne.cd0
     &        .or.ps12.eq.cd0.or.ps13.eq.cd0.or.ps23.eq.cd0
     &        ) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C02m0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C03m0_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented '
            write(nerrout_coli,111) ' C03m0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C03m0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C03m0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C03m0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C03m0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C03m0_coli: m32 = ',m32
          endif
        endif
      endif
#endif


      if (real(p12).ge.0d0.and.real(p13).ge.0d0) then
        sp13 = sqrt(p13)
        sp12 = sqrt(p12)
        detp = sqrt((p23-(sp12+sp13)*(sp12+sp13))*
     &       (p23-(sp12-sp13)*(sp12-sp13)))
      else
        detp = sqrt((p13-p23)**2 + p12*p12 - 2d0*p12*(p13+p23))
      endif

      if(abs(detp).lt.calacc*
     &    max(abs(p12*p12),abs(p23*p23),abs(p13*p13))) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('C03m0_coli','case not implemented',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100) ' C03m0_coli: singular determinant'
          write(nerrout_coli,111) ' C03m0_coli: det = ',detp
          write(nerrout_coli,111) ' C03m0_coli: p12 = ',p12
          write(nerrout_coli,111) ' C03m0_coli: p23 = ',p23
          write(nerrout_coli,111) ' C03m0_coli: p13 = ',p13
          write(nerrout_coli,111) ' C03m0_coli: m12 = ',m12
          write(nerrout_coli,111) ' C03m0_coli: m22 = ',m22
          write(nerrout_coli,111) ' C03m0_coli: m32 = ',m32
        endif
        C03m0_coli = undefined
        if (abs(detp).eq.0d0) return
      endif

      z2 = (p12-p13-p23-detp)/(2d0*p13)
      z1 = (p12-p13-p23+detp)/(2d0*p13)
      if (abs(z2).lt.abs(z1)) then
        z2 = p23/(p13*z1)
      else
        z1 = p23/(p13*z2)
      endif
      iz1 = real(z1/detp)

      x1  = -p23/p13
      ix1 = real(p23 - p13)

      C03m0_coli = cspenc_coli(1d0+z2,-iz1)
     &       - cspenc_coli(1d0+z1,iz1)
     &       + (cln_coli(-z2,iz1)-cln_coli(-z1,-iz1))
     &           *cln_coli(p12/p13,real(p13-p12))
     &       + cspcos_coli(-x1,-1d0/z1,-ix1,iz1)
     &       - cspcos_coli(-x1,-1d0/z2,-ix1,-iz1)

      C03m0_coli = -C03m0_coli/detp

      end
************************************************************************
      function C0ms0ir1_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     ir-divergent scalar 3-point function                             *
*     assumes    m12 = 0, p12 = m22, p13 = m32                         *
*     and no mass singularities                                        *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*                    m22   /     \   m32                               *
*                         /       \                                    *
*                        /         \                                   *
*       m22 = p12  ---------------------  p13 = m32                    *
*                            la2                                       *
*                                                                      *
*----------------------------------------------------------------------*
*     15.03.04 Ansgar Denner        last changed  28.08.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 C0ms0ir1_coli

      complex*16 a,h1,h2,h3
      complex*16 cln_coli,cspenc_coli,cspcon_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 C0ms0ir1_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      complex*16 m2,m3,k23,r23
      real*8     ir23
      logical flag(0:4)
      data    flag /5*.true./
      save    flag
#endif

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(ms12.ne.cd0.or.p12.ne.m22.or.p13.ne.m32
     &      .or.ms22.eq.cd0.or.ms32.eq.cd0
     &      .or.ps23.eq.cd0.and.ms32.eq.cd0.and.ms22.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p13).ne.0d0
     &      .or.aimag(p23).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms0ir1_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms0ir1_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented    '
            write(nerrout_coli,111) ' C0ms0ir1_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms0ir1_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms0ir1_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms0ir1_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms0ir1_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms0ir1_coli: m32 = ',m32
          endif
        endif
      endif
#endif

c W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349  (iv)
c according to Dittmaier Nucl. Phys. B675 (2003) 447   (B.5,B.6)

        if (p23.ne.cd0) then
          a=sqrt(p12*p12+p23*p23+p13*p13-2d0*(p12*p23+p23*p13+p13*p12))

          if(abs(a).lt.calacc*
     &        max(abs(p12*p12),abs(p23*p23),abs(p13*p13))) then
            call setErrFlag_coli(-7)
            call ErrOut_coli('C0ms0ir1_coli','case not implemented',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &                       'C0ms0ir1_coli: singular determinant'
              write(nerrout_coli,111) 'C0ms0ir1_coli: det = ',a
              write(nerrout_coli,111) 'C0ms0ir1_coli: p12 = ',p12
              write(nerrout_coli,111) 'C0ms0ir1_coli: p23 = ',p23
              write(nerrout_coli,111) 'C0ms0ir1_coli: p13 = ',p13
              write(nerrout_coli,111) 'C0ms0ir1_coli: m12 = ',m12
              write(nerrout_coli,111) 'C0ms0ir1_coli: m22 = ',m22
              write(nerrout_coli,111) 'C0ms0ir1_coli: m32 = ',m32
            endif
            C0ms0ir1_coli = undefined
            if (abs(a).eq.0d0) return
          endif

          if (real(p23-p12-p13).le.0d0) then
            h1=(a-p23+p12+p13)/2d0
          else
            h1=-4d0*p12*p13/(a+p23-p12-p13)/2d0
          endif
          if (real(p23-p12+p13).le.0d0) then
            h2=(a-p23+p12-p13)/2d0
          else
            h2=-4d0*p23*p13/(a+p23-p12+p13)/2d0
          endif
          if (real(p23+p12-p13).le.0d0) then
            h3=(a-p23-p12+p13)/2d0
          else
            h3=-4d0*p12*p23/(a+p23+p12-p13)/2d0
          endif

          C0ms0ir1_coli=cspenc_coli(h2/a,-1d0)
     &         +cspenc_coli(h3/a,-1d0)-pi2_6
     &         -.5d0*cln_coli(-h2/p23,-1d0)**2
     &         -.5d0*cln_coli(-h3/p23,-1d0)**2
     &         +.25d0*cln_coli(-p12/p23,-1d0)**2
     &         +.25d0*cln_coli(-p13/p23,-1d0)**2
     &         -cln_coli(-a/p23,-1d0)
     &         *(cln_coli(-h1/p23,-1d0)-cln_coli(-a/p23,-1d0))

          if(m12.eq.cd0)then


            C0ms0ir1_coli=C0ms0ir1_coli
     &          +cln_coli(-muir2/p23,-1d0)
     &          *cln_coli(h1/sqrt(p12*p13),1d0)
#ifdef SING
     &          +delta1ir
     &          *cln_coli(h1/sqrt(p12*p13),1d0)
#endif
          else

#ifdef NEWCHECK
            if(flag(2))then
              call ErrOut_coli('C0ms0ir1_coli',
     &          'new function called: (B.5)',
     &            errorwriteflag)
              write(nerrout_coli,100) ' C0ms0ir1_coli: (B.5)',
     &          '    case not yet tested in physical process'
              write(nerrout_coli,111) ' C0ms0ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms0ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms0ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms0ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms0ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms0ir1_coli: m32 = ',m32
              flag(2)=.false.
            endif
#endif

            C0ms0ir1_coli=C0ms0ir1_coli
     &          +cln_coli(-m12/p23,-1d0)
     &          *cln_coli(h1/sqrt(p12*p13),1d0)
          endif
          C0ms0ir1_coli=C0ms0ir1_coli/a
        else
           if(m12.eq.cd0)then

#ifdef NEWCHECK
            if(flag(3))then
              call ErrOut_coli('C0ms0ir1_coli',
     &          'new function called: (B.6) for s=0',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100) ' C0ms0ir1_coli: (B.6) for s=0',
     &              '    case not yet tested in physical process'
                write(nerrout_coli,111) ' C0ms0ir1_coli: p12 = ',p12
                write(nerrout_coli,111) ' C0ms0ir1_coli: p23 = ',p23
                write(nerrout_coli,111) ' C0ms0ir1_coli: p13 = ',p13
                write(nerrout_coli,111) ' C0ms0ir1_coli: m12 = ',m12
                write(nerrout_coli,111) ' C0ms0ir1_coli: m22 = ',m22
                write(nerrout_coli,111) ' C0ms0ir1_coli: m32 = ',m32
                flag(3)=.false.
              endif
            endif
#endif

             C0ms0ir1_coli=-.25d0*log(p13*p12/muir2**2)*log(p12/p13)
#ifdef SING
     &           +.5d0*delta1ir*log(p12/p13)
* 16.01.15 wrong line commented out
*     &           *cln_coli(h1/sqrt(p12*p13),1d0)
#endif
           else

#ifdef NEWCHECK
            if(flag(4))then
              call ErrOut_coli('C0ms0ir1_coli',
     &          'new function called: (B.5) for s=0',
     &            errorwriteflag)
              write(nerrout_coli,100) ' C0ms0ir1_coli: (B.5) for s=0',
     &          '    case not yet tested in physical process'
              write(nerrout_coli,111) ' C0ms0ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms0ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms0ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms0ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms0ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms0ir1_coli: m32 = ',m32
              flag(4)=.false.
            endif
#endif

            C0ms0ir1_coli=-.25d0*log(p13*p12/m12**2)*log(p12/p13)
          endif
          C0ms0ir1_coli=C0ms0ir1_coli/(p12-p13)
        endif


        end

************************************************************************
      function C0ms1ir0_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     single mass-singular 3-point function                            *
*                                                                      *
*     assumes single mass singularity                                  *
*     and no soft singularity with                                     *
*     p12, m12, m22 small                                              *
*     p23, p13, m32  finite                                            *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*              m22 small   /     \   m32                               *
*                         /       \                                    *
*                        /         \                                   *
*             p12  ---------------------  p13                          *
*            small        m12 small                                    *
*                                                                      *
*----------------------------------------------------------------------*
*     20.08.08 Ansgar Denner        last changed  30.09.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 C0ms1ir0_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      complex*16 C0ms1ir0_check
      complex*16 x1,x2,z1,z2
      real*8     ix1,iz1
#endif

      complex*16 mm22,q13,q23
      complex*16 cln_coli,cspenc_coli,cspcon_coli

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      ps23 = elimminf2_coli(p23)
      ps13 = elimminf2_coli(p13)
      if(argcheck)then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ps12.ne.cd0.or.
     &      ps13.eq.ms32.and.ps23.eq.ms32.or.
     &      ps13.eq.cd0.and.ms32.eq.cd0.or.
     &      ps23.eq.cd0.and.ms32.eq.cd0.or.
     &      ps23.eq.ps13
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir0_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented    '
            write(nerrout_coli,111) ' C0ms1ir0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111) ' C0ms1ir0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111) ' C0ms1ir0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111) ' C0ms1ir0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111) ' C0ms1ir0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111) ' C0ms1ir0_coli: m32 = ',m32,ms32
          endif
        endif
      endif
#endif

      if(abs(p13-p23).lt.calacc*abs(p13)) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('C0ms1ir0_coli','case not implemented',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100) 'C0ms1ir0_coli: singular determinant'
          write(nerrout_coli,111) 'C0ms1ir0_coli: det = ',p13-p23
          write(nerrout_coli,111) 'C0ms1ir0_coli: p12 = ',p12
          write(nerrout_coli,111) 'C0ms1ir0_coli: p23 = ',p23
          write(nerrout_coli,111) 'C0ms1ir0_coli: p13 = ',p13
          write(nerrout_coli,111) 'C0ms1ir0_coli: m12 = ',m12
          write(nerrout_coli,111) 'C0ms1ir0_coli: m22 = ',m22
          write(nerrout_coli,111) 'C0ms1ir0_coli: m32 = ',m32
        endif
        C0ms1ir0_coli = undefined
        if (abs(p13-p23).eq.0d0) return
      endif

      if(m12.eq.cd0.and.m22.eq.cd0)then
* no soft singularity and 2 zero masses   *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ m32               *
*                /  1  \                  *
*          0 ----------------  p13        *
*                   0                     *
*                                         *

#ifdef CHECK
        if(m12.ne.cd0.or.m22.ne.cd0.or.p12.ne.cd0
     &      .or.p13.eq.m32.or.p23.eq.m32)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)
     &          ' C0ms1ir0_coli: case with 2 zero masses',
     &          '    wrong arguments'
            write(nerrout_coli,111) ' C0ms1ir0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir0_coli: m32 = ',m32
          endif
        endif
#endif

c Dittmaier Nucl. Phys. B675 (2003) 447   (B.4)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    triangle 3

        if(abs(p23).gt.abs(m32).and.abs(p13).gt.abs(m32)) then


          C0ms1ir0_coli =
#ifdef SING
     &        delta1ir*(cln_coli((m32-p23),-1d0)
     &        -cln_coli((m32-p13),-1d0))
#endif
     &        +.5d0*(cln_coli((-p13)/muir2,-1d0))**2
     &        -.5d0*(cln_coli((-p23)/muir2,-1d0))**2
     &        -cspenc_coli(m32/p13,-1d0)
     &        +cspenc_coli(m32/p23,-1d0)
          if(m32.ne.cd0)then


            C0ms1ir0_coli =  C0ms1ir0_coli
     &          +cln_coli((m32-p13)/(-p13),1d0)*
     &          (cln_coli((m32-p13)/m32,-1d0)+cln_coli(-p13/muir2,-1d0))
     &          -cln_coli((m32-p23)/(-p23),1d0)*
     &          (cln_coli((m32-p23)/m32,-1d0)+cln_coli(-p23/muir2,-1d0))

         endif
        else


          C0ms1ir0_coli =
#ifdef SING
     &        delta1ir*(cln_coli((m32-p23)/m32,-1d0)
     &        -cln_coli((m32-p13)/m32,-1d0))
#endif
     &        -(cln_coli((m32-p23)/m32,-1d0))**2
     &        +(cln_coli((m32-p13)/m32,-1d0))**2
     &        -cln_coli(m32/muir2,-1d0)*(cln_coli((m32-p23)/m32,-1d0)
     &        -cln_coli((m32-p13)/m32,-1d0))
     &        +cspenc_coli(p13/m32,1d0)
     &        -cspenc_coli(p23/m32,1d0)
        endif

        C0ms1ir0_coli = C0ms1ir0_coli/(p13-p23)

      elseif(m12.eq.cd0.or.m22.eq.cd0)then
* no soft singularity and 1 zero masses
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*       m22 small /2 3\ m32               *
*                /  1  \                  *
*    p12=m22 ----------------  p13        *
*    small          0                     *
*                                         *

#ifdef CHECK
        if(m12.ne.cd0.and.m12.ne.p12.or.
     &      m22.ne.cd0.and.m22.ne.p12.or.
     &      m22*m12.ne.cd0.or.
     &      m12.eq.cd0.and.p13.eq.m32.or.
     &      m22.eq.cd0.and.p23.eq.m32)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)
     &          ' C0ms1ir0_coli: case with 1 zero mass',
     &          ': wrong arguments'
            write(nerrout_coli,111) ' C0ms1ir0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir0_coli: m32 = ',m32
          endif
        endif
#endif


        if(m12.eq.cd0)then
          mm22=m22*coliminfscale2
          q23=p23
          q13=p13
        else
          mm22=m12*coliminfscale2
          q23=p13
          q13=p23
        endif

c     Dittmaier Nucl. Phys. B675 (2003) 447   (B.2)

        if (m32.ne.cd0) then




          C0ms1ir0_coli = cspenc_coli(q23/m32,1d0)
     &        - cspenc_coli(q13/m32,1d0)
     &        - 2d0*cspenc_coli((q23-q13)/(m32-q13),real(q23-q13))
     &        + cln_coli((m32-q23)/mm22,-1d0)
     &         *cln_coli((m32-q23)/m32,-1d0)
     &        - cln_coli((m32-q13)/mm22,-1d0)
     &         *cln_coli((m32-q13)/m32,-1d0)
          C0ms1ir0_coli = C0ms1ir0_coli/(q23-q13)

        else


          C0ms1ir0_coli = cspcon_coli(mm22/(-q23),(q13-q23)/mm22,
     &        (q13-q23)/(-q23),(-q13)/(-q23),1d0,
     &        -real(q13-q23))
     &        - cspenc_coli(q23/q13,real(q13-q23))
     &        + (cln_coli(mm22/(q13-q23),real(q13-q23))
     &        - cln_coli((q13-q23)/q13,-real(q13-q23)))
     &        * cln_coli(q23/q13,real(q13-q23))
          C0ms1ir0_coli = C0ms1ir0_coli/(q13-q23)

        endif


      else
*  no soft singularity and 0 zero masses  *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*            m22  /2 3\ m32               *
*                /  1  \                  *
*          0 ----------------  p13        *
*                m12=m22                  *
*                                         *

#ifdef CHECK
        if(m12.eq.cd0.or.m22.eq.cd0.or.m22.ne.m12
     &      .or.p12.ne.cd0.or.
     &      ps23.eq.cd0.and.m32.eq.cd0.or.
     &      ps13.eq.cd0.and.m32.eq.cd0)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir0_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)
     &          ' C0ms1ir0_coli: case with no zero mass',
     &          ': wrong arguments'
            write(nerrout_coli,111) ' C0ms1ir0_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir0_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir0_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir0_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir0_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir0_coli: m32 = ',m32
          endif
        endif
#endif


        C0ms1ir0_coli=cspenc_coli(-p23/(m32-p23),-1d0)
     &      - cspenc_coli(-p13/(m32-p13),-1d0)
     &      - .5d0*cln_coli((m32-p13)/(m32-p23),real(p23-p13))**2
     &      + cln_coli((m32-p13)/(m32-p23),real(p23-p13))
     &      *cln_coli((m32-p13)/(sqrt(m12*m22)*coliminfscale2),-1d0)

        C0ms1ir0_coli = C0ms1ir0_coli/(p13-p23)


      endif

      end
************************************************************************
      function C0ms1ir1_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     single mass-singular 3-point function                            *
*                                                                      *
*     assumes single mass singularity                                  *
*     and single soft singularity with                                 *
*     p12, m12, m22 small                                              *
*     p23, p13=m32 finite                                              *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*              m22 small   /     \   m32                               *
*                         /       \                                    *
*                        /         \                                   *
*       m22 = p12  ---------------------  p13 = m32                    *
*       small             m12 small                                    *
*                                                                      *
*----------------------------------------------------------------------*
*     21.08.08 Ansgar Denner        last changed  03.10.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 C0ms1ir1_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      logical flag(0:4)
      data    flag /5*.true./
      save    flag
#endif

      complex*16 cln_coli,cspenc_coli

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      C0ms1ir1_coli=undefined

#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ps12.ne.cd0
     &        .or.ms32.eq.cd0.or.p13.ne.m32.or.p23.eq.m32
     &        .or.ps23.eq.ms32
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir1_coli called improperly'
            write(nerrout_coli,100) '     or case not implemented    '
            write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12,ps12
            write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23,ps23
            write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13,ps13
            write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12,ms12
            write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22,ms22
            write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32,ms32
            write(nerrout_coli,111) ' C0ms1ir1_coli: tests ',
     &          ms12.ne.cd0,ms22.ne.cd0,ps12.ne.cd0
     &          ,ms32.eq.cd0,p13.ne.m32,p23.eq.m32
     &          ,ps23.eq.ms32
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p13).ne.0d0
          endif
          C0ms1ir1_coli = undefined
          return
        endif
      endif
#endif

      if(abs(m32-p23).lt.calacc*abs(p23)) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('C0ms1ir1_coli','case not implemented',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100) 'C0ms1ir1_coli: singular determinant'
          write(nerrout_coli,111) 'C0ms1ir1_coli: det = ',m32-p23
          write(nerrout_coli,111) 'C0ms1ir1_coli: p12 = ',p12
          write(nerrout_coli,111) 'C0ms1ir1_coli: p23 = ',p23
          write(nerrout_coli,111) 'C0ms1ir1_coli: p13 = ',p13
          write(nerrout_coli,111) 'C0ms1ir1_coli: m12 = ',m12
          write(nerrout_coli,111) 'C0ms1ir1_coli: m22 = ',m22
          write(nerrout_coli,111) 'C0ms1ir1_coli: m32 = ',m32
        endif
        C0ms1ir1_coli = undefined
        if (abs(m32-p23).eq.0d0) return
      endif

      if(p12.eq.m22)then
        if(m22.eq.cd0)then
* one soft singularity and 2 zero masses  *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ m32               *
*                /  1  \                  *
*          0 ----------------  p13=m32    *
*                   0                     *
*                                         *

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.cd0.or.m32.eq.cd0
     &        .or.p12.ne.cd0.or.p13.ne.m32.or.p23.eq.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms1ir1_coli:  (B.12)',
     &            '    case with 1 soft singularity and 2 zero masses',
     &            '    wrong arguments'
              write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
            endif
          endif
#endif


c Dittmaier Nucl. Phys. B675 (2003) 447   (B.12)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    triangle 4
          C0ms1ir1_coli =
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli((m32-p23)/muir2,-1d0)
     &        -.5d0*cln_coli(m32/muir2,-1d0))
#endif
     &        +.5d0*(cln_coli((m32-p23)/muir2,-1d0))**2
     &        -.25d0*(cln_coli(m32/muir2,-1d0))**2
     &        -cspenc_coli(-p23/(m32-p23),-1d0)

          C0ms1ir1_coli = C0ms1ir1_coli/(p23-m32)

        else
* one soft singularity and 2 zero masses  *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*            m22  /2 3\ m32               *
*                /  1  \                  *
*    p12=m22 ----------------  p13=m32    *
*                   0                     *
*                                         *

#ifdef CHECK
          if(m12.ne.cd0.or.m22.eq.cd0.or.m32.eq.cd0
     &        .or.p12.ne.m22.or.p13.ne.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms1ir1_coli:  (B.8)',
     &            '    case with 1 soft singularity and 1 zero mass',
     &            '    wrong arguments'
              write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
            endif
          endif
#endif


c W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 iva
c according to Dittmaier Nucl. Phys. B675 (2003) 447   (B.8)
c Ellis, Zanderighi, JHEP 0802 (2008) 002    triangle 6

c single mass-singular case  m2 = small
          if(p23.ne.cd0) then
            C0ms1ir1_coli = cspenc_coli(p23/m32,1d0)
     &        + cln_coli((m32-p23)/(sqrt(m22*m32)*coliminfscale),-1d0)
     &          * cln_coli((m32-p23)*sqrt(m22/m32)*coliminfscale/muir2
     &          ,-1d0)
#ifdef SING
     &        - cln_coli((m32-p23)/(sqrt(m22*m32)*coliminfscale),-1d0)
     &          *delta1ir
     &          - 0.25*colishiftms2
#endif
            C0ms1ir1_coli=-C0ms1ir1_coli/(m32-p23)
          else
            C0ms1ir1_coli=0.5d0*log(muir2/(sqrt(m22*m32)*coliminfscale))
     &          *log(m32/(m22*coliminfscale2))
#ifdef SING
     &          + 0.25d0*colishiftms2
#endif
            C0ms1ir1_coli=C0ms1ir1_coli/m32
          endif

        endif

      elseif(m22.eq.cd0.and.p12.eq.m12)then

* 0 soft singularity and 1 zero masses    *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ m32               *
*                /  1  \                  *
*    p12=m12 ----------------  p13=m32    *
*                  m12                    *
*                                         *

#ifdef CHECK
        if(m22.ne.cd0.or.m12.eq.cd0.or.m32.eq.cd0
     &      .or.p12.ne.m12.or.p13.ne.m32.or.p23.eq.m32)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir1_coli: (B.10)',
     &          '    case with 0 soft singularity and 1 zero mass',
     &          '    wrong arguments'
            write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(3))then
          call ErrOut_coli('C0ms1ir1_coli',
     &          'new function called: (B.10)',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir1_coli: (B.10)',
     &          '    case with 0 soft singularity and 1 zero mass',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
            flag(3)=.false.
          endif
        endif
#endif

c case: Dittmaier Nucl. Phys. B675 (2003) 447  (B.10)

        C0ms1ir1_coli = -cspenc_coli(p23/m32,1d0)
     &      - cln_coli((m32-p23)/(m12*coliminfscale2),-1d0)
     &      * cln_coli((m32-p23)/m32,-1d0)
     &      - 0.25d0*(log(m32/(m12*coliminfscale2)))**2
     &      - 2.5d0*pi2_6
#ifdef SING
     &      - 0.25*colishiftms2
#endif
        C0ms1ir1_coli=C0ms1ir1_coli/(m32-p23)

      elseif(p12.eq.cd0.and.m22.eq.m12)then

* 0 soft singularity and 0 zero masses    *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*         m22=m12 /2 3\ m32               *
*                /  1  \                  *
*      p12=0 ----------------  p13=m32    *
*                  m12                    *
*                                         *

#ifdef CHECK
        if(m22.ne.m12.or.m12.eq.cd0.or.m32.eq.cd0
     &      .or.p12.ne.cd0.or.p13.ne.m32.or.p23.eq.m32)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir1_coli: (B.11)',
     &          '    case with 0 soft singularity and 0 zero mass',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(4))then
          call ErrOut_coli('C0ms1ir1_coli',
     &          'new function called: (B.11)',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms1ir1_coli: (B.11)',
     &          '    case with 0 soft singularity and 0 zero mass',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
            write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
            write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
            write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
            write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
            write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
            flag(4)=.false.
          endif
        endif
#endif

c case: Dittmaier Nucl. Phys. B675 (2003) 447  (B.10)

        C0ms1ir1_coli = -cspenc_coli(p23/m32,1d0)
     &      - (cln_coli((m32-p23)/sqrt(m32*m12*coliminfscale2),-1d0))**2
     &      - .5d0*pi2_6
#ifdef SING
     &      - 0.25*colishiftms2
#endif
        C0ms1ir1_coli=C0ms1ir1_coli/(m32-p23)

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('C0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' C0ms1ir1_coli called with wrong arguments'
          write(nerrout_coli,100) '     or case not implemented    '
          write(nerrout_coli,111) ' C0ms1ir1_coli: p12 = ',p12
          write(nerrout_coli,111) ' C0ms1ir1_coli: p23 = ',p23
          write(nerrout_coli,111) ' C0ms1ir1_coli: p13 = ',p13
          write(nerrout_coli,111) ' C0ms1ir1_coli: m12 = ',m12
          write(nerrout_coli,111) ' C0ms1ir1_coli: m22 = ',m22
          write(nerrout_coli,111) ' C0ms1ir1_coli: m32 = ',m32
        endif
#endif

      endif
      end
************************************************************************
      function C0ms2ir1_coli(p12,p23,p13,m12,m22,m32)
************************************************************************
*     mass-singular 3-point function                                   *
*                                                                      *
*     assumes double mass singularity with                             *
*     p12, p13, m12, m22, m32 small                                    *
*     p23 finite                                                       *
*                                                                      *
*                            p23                                       *
*                             |                                        *
*                             |                                        *
*                            / \                                       *
*                           /   \                                      *
*              m22 small   /     \   m32 small                         *
*                         /       \                                    *
*                        /         \                                   *
*       m22 = p12  ---------------------  p13 = m32                    *
*       small             m12 small       small                        *
*                                                                      *
*----------------------------------------------------------------------*
*     20.08.08 Ansgar Denner        last changed  26.09.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p13
      complex*16 m12,m22,m32
      complex*16 C0ms2ir1_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps13
      complex*16 ms12,ms22,ms32
      logical flag(0:6)
      data    flag /7*.true./
      save    flag
#endif

      complex*16 mm32,m2(3)
      complex*16 cln_coli
      integer    nsoft
      integer    i,j,k
      logical    soft(3),onsh(3,3)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      C0ms2ir1_coli=undefined

#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps13 = elimminf2_coli(p13)

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &        .or.ps12.ne.cd0.or.ps13.ne.cd0.or.ps23.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100) ' C0ms2ir1_coli called improperly:'
            write(nerrout_coli,100) '     or case not implemented    '
            write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12,ps12
            write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23,ps23
            write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13,ps13
            write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12,ms12
            write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22,ms22
            write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32,ms32
          endif
        endif
      endif
#endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22


c count/determine soft singularities
      nsoft=0
      do i=1,3
        do j=1,2
          if(i.ne.j)then
            do k=j,3
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

      if(nsoft.eq.1)then

        if(m22.eq.cd0.and.m32.eq.cd0)then
* one soft singularity and 3 zero masses  *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ 0                 *
*                /  1  \                  *
*          0 ----------------  0          *
*                   0                     *
*                                         *

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.cd0.or.m32.ne.cd0
     &        .or.p12.ne.cd0.or.p13.ne.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli:  (B.16)',
     &            '    case with 1 soft singularity and 3 zero masses ',
     &            '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif



c Dittmaier Nucl. Phys. B675 (2003) 447   (B.16)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    triangle 1
          C0ms2ir1_coli =
#ifdef SING
     &        delta2ir
     &        -delta1ir*cln_coli(-p23/muir2,-1d0)
#endif
     &        +.5d0*(cln_coli(-p23/muir2,-1d0))**2
     &        - pi2_6

          C0ms2ir1_coli = C0ms2ir1_coli/p23

        elseif(m22.eq.cd0.or.m32.eq.cd0)then
* one soft singularity and 2 zero masses  *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ m32               *
*                /  1  \                  *
*          0 ----------------  p13=m32    *
*                   0                     *
*                                         *

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.p12.or.m32.ne.p13
     &        .or.m22*m32.ne.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.12) m->0',
     &            '    case with 1 soft singularity and 2 zero masses',
     &            '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('C0ms2ir1_coli',
     &          'new function called: (B.12) m->0',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.12) m->0',
     &            '    case with 1 soft singularity and 2 zero masses',
     &            '    not yet tested for physical process'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
              flag(2)=.false.
            endif
          endif
#endif

          mm32=(m22+m32)*coliminfscale2

c Dittmaier Nucl. Phys. B675 (2003) 447   (B.12)   m -> 0
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002     triangle 4
          C0ms2ir1_coli =
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli(-p23/muir2,-1d0)
     &        -.5d0*cln_coli(mm32/muir2,-1d0))
     &        -.25d0*colishiftms2
#endif
     &        +.5d0*(cln_coli(-p23/muir2,-1d0))**2
     &        -.25d0*(cln_coli(mm32/muir2,-1d0))**2
     &        - pi2_6

          C0ms2ir1_coli = C0ms2ir1_coli/p23

        else
* one soft singularity and 1 zero mass    *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*            m22  /2 3\ m32               *
*                /  1  \                  *
*     p12=m22 ----------------  p13=m32   *
*                   0                     *
*                                         *

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.p12.or.m32.ne.p13
     &        .or.m22.eq.cd0.or.m32.eq.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.9)',
     &            '    case with 1 soft singularity and 1 zero mass:',
     &            '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif



c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 ivb
c Dittmaier Nucl. Phys. B675 (2003) 447   (B.9)
          C0ms2ir1_coli = pi2_6
     &        +.25d0*cln_coli(-m22*coliminfscale2/p23,1d0)**2
     &        +.25d0*cln_coli(-m32*coliminfscale2/p23,1d0)**2
     &        + cln_coli(-p23/muir2,-1d0)
     &        *cln_coli(-sqrt(m22*m32)*coliminfscale2/p23,1d0)
#ifdef SING
     &        +.5d0*colishiftms2
     &        -delta1ir
     &        *cln_coli(-sqrt(m22*m32)*coliminfscale2/p23,1d0)
#endif
          C0ms2ir1_coli=-C0ms2ir1_coli/p23

        endif

      elseif(nsoft.eq.0)then

        if(m22.eq.cd0.and.m32.eq.cd0)then
* no soft singularity and 2 zero masses   *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ 0                 *
*                /  1  \                  *
*     p12=m12 ----------------  p13=m12   *
*                   m12                   *
*                                         *

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.cd0.or.m32.ne.cd0
     &        .or.p12.ne.m12.or.p13.ne.m12.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.13)',
     &           '    case with 0 soft singularity and 2 zero masses: ',
     &           '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif


c Dittmaier Nucl. Phys. B675 (2003) 447   (B.13)
          C0ms2ir1_coli =
#ifdef SING
     &        + colishiftms2
#endif
     &        + cln_coli(-p23/(m12*coliminfscale2),-1d0)**2
     &        + 8d0*pi2_6

          C0ms2ir1_coli = C0ms2ir1_coli/(2d0*p23)


          else if(m22.eq.cd0.or.m32.eq.cd0)then
* no soft singularity and 1 zero mass     *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*              0  /2 3\ m32=m12           *
*                /  1  \                  *
*     p12=m12 ----------------  0         *
*                   m12                   *
*                                         *

#ifdef CHECK
          if(m12.eq.cd0.or.p23.eq.cd0.or.
     &        .not.(p12.eq.m12.and.m32.eq.m12.or.
     &          p13.eq.m12.and.m22.eq.m12)) then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms1ir0_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.14)',
     &           '    case with 0 soft singularity and 1 zero masses: ',
     &           '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif


c Dittmaier Nucl. Phys. B675 (2003) 447   (B.14)
          C0ms2ir1_coli = cln_coli(-p23/(m12*coliminfscale2),-1d0)**2
     &        + 4d0*pi2_6
#ifdef SING
     &        + colishiftms2
#endif

          C0ms2ir1_coli = C0ms2ir1_coli/(2d0*p23)


          else
* no soft singularity and no zero mass    *
*                                         *
*                   | p23                 *
*                   |                     *
*                  / \                    *
*        m22=m12  /2 3\ m32=m12           *
*                /  1  \                  *
*         0  ----------------  0          *
*                   m12                   *
*                                         *

#ifdef CHECK
          if(m12.eq.cd0.or.p23.eq.cd0.or.
     &       p12.ne.cd0.or.p13.ne.cd0.or.
     &       m22.ne.m12.or.m32.ne.m12) then
            call setErrFlag_coli(-10)
            call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.15)',
     &           '    case with 0 soft singularity and 0 zero masses: ',
     &           '    wrong arguments'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(6))then
            call ErrOut_coli('C0ms2ir1_coli',
     &          'new function called: (B.15)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100) ' C0ms2ir1_coli: (B.15)',
     &           '    case with 0 soft singularity and 0 zero masses: ',
     &           '    not yet tested in physical process'
              write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
              flag(6)=.false.
            endif
          endif
#endif

c Dittmaier Nucl. Phys. B675 (2003) 447   (B.15)
          C0ms2ir1_coli = cln_coli(-p23/(m12*coliminfscale2),-1d0)**2
#ifdef SING
     &        +colishiftms2
#endif

          C0ms2ir1_coli = C0ms2ir1_coli/(2d0*p23)

        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('C0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' C0ms2ir1_coli called with wrong arguments'
          write(nerrout_coli,100) '     or case not implemented    '
          write(nerrout_coli,111) ' C0ms2ir1_coli: p12 = ',p12
          write(nerrout_coli,111) ' C0ms2ir1_coli: p23 = ',p23
          write(nerrout_coli,111) ' C0ms2ir1_coli: p13 = ',p13
          write(nerrout_coli,111) ' C0ms2ir1_coli: m12 = ',m12
          write(nerrout_coli,111) ' C0ms2ir1_coli: m22 = ',m22
          write(nerrout_coli,111) ' C0ms2ir1_coli: m32 = ',m32
        endif
#endif
      endif
      end
