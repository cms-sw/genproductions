!!
!!  File coli_d0reg.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

#include "global_coli.h"

c#define WRITERP
c#define WRITESD
c#define WRITESTD

************************************************************************
*                                                                      *
*     Regular scalar 4-point function                                  *
*                                                                      *
************************************************************************
*                                                                      *
*     last changed  15.06.11  Ansgar Denner                            *
*     errorflags    24.05.13  Ansgar Denner   updated 26.03.15         *
*                                                                      *
************************************************************************
* Subroutines:                                                         *
* Functions:                                                           *
* D0regrp_coli,D0comb_coli                                             *
************************************************************************
      function D0regrp_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for r13 real positive                      *
*  regular case  based on general result of                            *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*  valid for real positive r13 and vicinity                            *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  q23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       q14  ---------------------  q34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  29.12.08 Ansgar Denner       last changed 17.03.10 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 D0regrp_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42

      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12,r13,r14,r23,r24,r34
      complex*16 r21,r31,r41,r32,r42,r43
      complex*16 a,b,c,d,det
      complex*16 x(2,4),yc(4,4)
      complex*16 ch0,ch1,ch2,ch3,ch4,ch5,l1,l2,argl1
      complex*16 eta,chdet
      real*8     ir12,ir13,ir14,ir23,ir24,ir34
      real*8     ix(2,4),ipop(2)
      real*8     test0,test1,test2,test3,test4,test5
      real*8     test01,test23,test45
      real*8     u,v
      complex*16 cspcos_coli,cln_coli,eta2s_coli
      integer    i

      logical    flag2
      save       flag2
      data       flag2 /.true./

      logical errorwriteflag
#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D0regrp_check
      integer    j
      logical flag(0:1)
      data    flag /2*.true./
      save    flag
#endif

#include "params_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0regrp_coli',' improper arguments',
     &        errorwriteflag)
          if(errorwriteflag) then
            write(nerrout_coli,100)
     &          ' D0regrp_coli called improperly:   (3.61)'
            write(nerrout_coli,111)' D0regrp_coli: p12 = ',p12
            write(nerrout_coli,111)' D0regrp_coli: p23 = ',p23
            write(nerrout_coli,111)' D0regrp_coli: p34 = ',p34
            write(nerrout_coli,111)' D0regrp_coli: p14 = ',p14
            write(nerrout_coli,111)' D0regrp_coli: p13 = ',p13
            write(nerrout_coli,111)' D0regrp_coli: p24 = ',p24
            write(nerrout_coli,111)' D0regrp_coli: m12 = ',m12
            write(nerrout_coli,111)' D0regrp_coli: m22 = ',m22
            write(nerrout_coli,111)' D0regrp_coli: m32 = ',m32
            write(nerrout_coli,111)' D0regrp_coli: m42 = ',m42
            write(nerrout_coli,*)' D0regrp_coli: test= ',
     &          ms32.eq.0d0,ms12.eq.cd0,ms22.eq.cd0,ms42.eq.cd0
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p34).ne.0d0,aimag(p14).ne.0d0
     &          ,aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif

      l12 = (m12+m22-p12)
      l13 = (m12+m32-p13)
      l14 = (m12+m42-p14)
      l23 = (m22+m32-p23)
      l24 = (m22+m42-p24)
      l34 = (m32+m42-p34)

      if(l12.ne.cd0)then
        r12 = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
      else
        r12 = dcmplx(0d0,1d0)*sqrt(m12/m22)
      endif
      r21 = r12*m22/m12

      if(l13.ne.cd0)then
        if(p13.eq.cd0)then
          r31=cd1
        elseif(abs(m32).le.abs(m12))then
          r31 = l13/(2d0*m12)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
        else
          r31 = (2d0*m32)/(l13*(1d0+sqrt(1d0-4d0*m12*m32/l13**2)))
        endif
      else
        r31 = dcmplx(0d0,1d0)*sqrt(m32/m12)
      endif

      r13 = r31*m12/m32

      if(l14.ne.cd0)then
        r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14 = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41 = r14*m42/m12
      if(l23.ne.cd0)then
        r23 = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
      else
        r23 = dcmplx(0d0,1d0)*sqrt(m22/m32)
      endif
      r32 = r23*m32/m22
      if(l24.ne.cd0)then
        r24 = l24/(2d0*m42)*(1d0+sqrt(1d0-4d0*m22*m42/l24**2))
      else
        r24 = dcmplx(0d0,1d0)*sqrt(m22/m42)
      endif
      r42 = r24*m42/m22
      if(l34.ne.cd0)then
        r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
      else
        r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
      endif
      r43 = r34*m42/m32

      r42 = 1d0/r24
      r24 = m22/m42/r24

      if( aimag(r31).ne.0d0.and.
     &    (aimag(m22*r31*r31).gt.0d0.or.aimag(m42*r31*r31).gt.0d0.or.
     &    aimag(r31*l23).gt.0d0.or.aimag(r31*l34).gt.0d0.or.
     &    aimag(r31*r31*l24).gt.0d0)) then
        r31 = 1d0/r13
        r13 = m12/m32/r13
      endif

      ir12 = sign(1d0,real((r12*r21-1d0)*r12*m12))
      ir13 = sign(1d0,real((r13*r31-1d0)*r13*m12))
      ir14 = sign(1d0,real((r14*r41-1d0)*r14*m12))
      ir23 = sign(1d0,real((r23*r32-1d0)*r23*m22))
      ir24 = sign(1d0,real((r24*r42-1d0)*r24*m22))
      ir34 = sign(1d0,real((r34*r43-1d0)*r34*m32))

      a   =  m42*(l34/r42-l23 + (l12-l14/r42)*r31)
      b   =  m12*m42*(1d0/r13-r31)*(1d0/r42-r24)+l12*l34-l14*l23
      c   =  m12*(l34*r24-l23 + (l12-l14*r24)/r13)
      d   =  l23-r31*l12 - (l34-r31*l14)*r24
      det =  sqrt(b*b-4d0*a*c)

c calculation of det via chdet implemented 23.11.2017
      if(abs(det)**2.lt.sqrt(calacc)*max(abs(b*b),abs(4d0*a*c))) then
        yc(1,2) = l12
        yc(1,3) = l13
        yc(1,4) = l14
        yc(2,3) = l23
        yc(2,4) = l24
        yc(3,4) = l34
        yc(2,1) = l12
        yc(3,1) = l13
        yc(4,1) = l14
        yc(3,2) = l23
        yc(4,2) = l24
        yc(4,3) = l34
        yc(1,1) = 2d0*m12
        yc(2,2) = 2d0*m22
        yc(3,3) = 2d0*m32
        yc(4,4) = 2d0*m42
        det = sqrt(chdet(4,yc))
      endif

      if(abs(a).lt.calacc*abs(m42)*
     &      max(abs(l34/r42),abs(l23),abs(l12*r31),abs(l14/r42*r31))
     &    .or.abs(det)**2.lt.calacc**2*max(abs(b*b),abs(4d0*a*c))) then
c changed 23.11.2017
c     &    .or.abs(det)**2.lt.calacc*max(abs(b*b),abs(4d0*a*c))) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0regrp_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &           ' D0regrp_coli: singularity encountered'
          if (abs(a).lt.calacc*abs(m42)*
     &        max(abs(l34/r42),abs(l23),abs(l12*r31),abs(l14/r42*r31)))
     &    then
            write(nerrout_coli,111)' D0regrp_coli: 0=a = ',a
          endif
          if (abs(det)**2.lt.calacc*max(abs(b*b),abs(4d0*a*c))) then
            write(nerrout_coli,111)
     &           ' D0regrp_coli: det =  ',det,b*b,4*a*c
          endif
          write(nerrout_coli,111)' D0regrp_coli: p12 =  ',p12
          write(nerrout_coli,111)' D0regrp_coli: p23 =  ',p23
          write(nerrout_coli,111)' D0regrp_coli: p34 =  ',p34
          write(nerrout_coli,111)' D0regrp_coli: p14 =  ',p14
          write(nerrout_coli,111)' D0regrp_coli: p24 =  ',p24
          write(nerrout_coli,111)' D0regrp_coli: p13 =  ',p13
          write(nerrout_coli,111)' D0regrp_coli: m12 =  ',m12
          write(nerrout_coli,111)' D0regrp_coli: m22 =  ',m22
          write(nerrout_coli,111)' D0regrp_coli: m32 =  ',m32
          write(nerrout_coli,111)' D0regrp_coli: m42 =  ',m42
          write(nerrout_coli,*)
        endif
        D0regrp_coli = undefined
        if (abs(a).eq.0d0.or.abs(det).eq.0d0) return
      endif

      x(1,4) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2,4) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1,4)).gt.abs(x(2,4))) then
        x(2,4) = c/(a*x(1,4))
      else
        x(1,4) = c/(a*x(2,4))
      endif

      ix(1,4) = -sign(1d0,real(d))
      ix(2,4) = +sign(1d0,real(d))
      ix(1,1) =  sign(1d0,ix(1,4)*real(r24)) ! needed for correct
      ix(2,1) =  sign(1d0,ix(2,4)*real(r24)) ! continuation

      x(1,1) = x(1,4)/r24
      x(2,1) = x(2,4)/r24
      D0regrp_coli = dcmplx(0d0)
      do i=1,2
        eta=eta2s_coli(-x(i,4),1d0/r24,-ix(i,4),-ir24,-ix(i,1))

        if(eta.ne.0d0)then
          ch0   = m42*(r31*x(i,4))**2+l34*r31*x(i,4)+m32
          test0 = abs(ch0)/max(abs(m42*(r31*x(i,4))**2),
     &        abs(l34*r31*x(i,4)),abs(m32))
          ch1   = m42*(x(i,4))**2+l14*x(i,4)+m12
          test1 = abs(ch1)/
     &        max(abs(m42*(x(i,4))**2),abs(l14*x(i,4)),abs(m12))
          test01=min(test0,test1)
          ch2   = m22*(r31*x(i,1))**2+l23*r31*x(i,1)+m32
          test2 = abs(ch2)/max(abs(m22*(r31*x(i,1))**2),
     &        abs(l23*r31*x(i,1)),abs(m32))
          ch3   = m22*(x(i,1))**2+l12*x(i,1)+m12
          test3 = abs(ch3)/
     &        max(abs(m22*(x(i,1))**2),abs(l12*x(i,1)),abs(m12))
          test23 = min(test2,test3)

          ch4   = (m42*r31*x(i,4)*(1d0/r42-r24)+(l23-r24*l34))*r31
          test4 = abs(ch4)/max(abs(m42*r31*r31*x(i,4)/r42),
     &        abs(m42*r31*r31*x(i,4)*r24),abs(l23*r31),
     &        abs(r31*r24*l34))
          ch5   = m42*x(i,4)*(1d0/r42-r24)+(l12-r24*l14)
          test5 = abs(ch5)/max(abs(m42*x(i,4)/r42),
     &        abs(m42*x(i,4)*r24),abs(l12),abs(r24*l14))
          test45 = min(test2,test3)

          if (test23.gt.test01.and.test23.gt.test45) then
            argl1 = ch2/ch3
          elseif(test01.gt.test45)then
            argl1 = ch0/ch1
          else
            argl1 = ch4/ch5
          endif

          if(abs(aimag(argl1)).lt.1d1*impacc*abs(argl1)) then

            if(abs(aimag(r24)).gt.impacc*abs(r24))then
              v=aimag(x(i,4))/aimag(r24)
              u=aimag(x(i,4)/r24)/aimag(1d0/r24)
              ipop(i) = real(-m12-m22*v*v-m42*u*u-l12*v-l14*u-l24*u*v)
            else                ! imaginary part results only from x4(i)
              ipop(i) = real(m42*(r24-1d0/r42)*ix(i,4)*d*r31)
            endif
          else
            ipop(i) = 0D0
          endif

          l1 = cln_coli(argl1,ipop(i))

          if(eta.ne.0d0)then
            D0regrp_coli = D0regrp_coli + (2*i-3) * (
     &          + eta*(l1-cln_coli(m32/m12,real(m32-m12))) )
          endif

        endif

        D0regrp_coli = D0regrp_coli + (2*i-3) * (
     &      - cspcos_coli(-x(i,4),r43*r31,-ix(i,4),ir34)
     &      - cspcos_coli(-x(i,4),r31/r34,-ix(i,4),-ir34)
     &      + cspcos_coli(-x(i,4),r41,-ix(i,4),ir14)
     &      + cspcos_coli(-x(i,4),1d0/r14,-ix(i,4),-ir14)
     &      - cspcos_coli(-x(i,1),r21,-ix(i,1),ir12)
     &      - cspcos_coli(-x(i,1),1d0/r12,-ix(i,1),-ir12)
     &      + cspcos_coli(-x(i,1),r31*r23,-ix(i,1),ir23)
     &      + cspcos_coli(-x(i,1),r31/r32,-ix(i,1),-ir23))

      enddo

      D0regrp_coli = D0regrp_coli/det

      end

************************************************************************
      function D0comb_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  general scalar 4-point function                                     *
*  regular case based on propagator identity                           *
*  reduces result to 2 4-point function with one vanishing pij         *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  09.03.10 Ansgar Denner       last changed 27.11.17 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 q2(0:3,0:3),m2(0:3)
      complex*16 D0comb_coli,D0regrp_coli,D0m0_coli

      real*8     lambda,ga,omga,gas,omgas,gasomgas
      complex*16 mm2,qq2,D01,D02
      integer    i,j,k,l,i1,i2,i3,i4
      logical    errorwriteflag

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
#endif

#include "params_coli.h"
#include "checkparams_coli.h"

 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      D0comb_coli = undefined

#ifdef CHECK
 100  format(((a)))
 101  format(a22,g25.17)
      if (argcheck) then
        ms12 = elimminf2_coli(m12)
        ms22 = elimminf2_coli(m22)
        ms32 = elimminf2_coli(m32)
        ms42 = elimminf2_coli(m42)
        ps12 = elimminf2_coli(p12)
        ps23 = elimminf2_coli(p23)
        ps34 = elimminf2_coli(p34)
        ps14 = elimminf2_coli(p14)
        ps24 = elimminf2_coli(p24)
        ps13 = elimminf2_coli(p13)

        if(ms32.eq.0d0.or.ms12.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0comb_coli',' improper arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0comb_coli called improperly:'
            write(nerrout_coli,111)' D0comb_coli: p12 = ',p12
            write(nerrout_coli,111)' D0comb_coli: p23 = ',p23
            write(nerrout_coli,111)' D0comb_coli: p34 = ',p34
            write(nerrout_coli,111)' D0comb_coli: p14 = ',p14
            write(nerrout_coli,111)' D0comb_coli: p13 = ',p13
            write(nerrout_coli,111)' D0comb_coli: p24 = ',p24
            write(nerrout_coli,111)' D0comb_coli: m12 = ',m12
            write(nerrout_coli,111)' D0comb_coli: m22 = ',m22
            write(nerrout_coli,111)' D0comb_coli: m32 = ',m32
            write(nerrout_coli,111)' D0comb_coli: m42 = ',m42
          endif
        endif
      endif
#endif

      q2(0,1)=p12
      q2(1,0)=p12
      q2(0,2)=p13
      q2(2,0)=p13
      q2(0,3)=p14
      q2(3,0)=p14
      q2(2,1)=p23
      q2(1,2)=p23
      q2(3,1)=p24
      q2(1,3)=p24
      q2(2,3)=p34
      q2(3,2)=p34
      m2(0)=m12
      m2(1)=m22
      m2(2)=m32
      m2(3)=m42

      do i=0,2
      do j=i+1,3
      if(j.ne.i)then
        if(q2(i,j).eq.0d0)then
          i1=i
          i3=j
          if(mod(i3-i1,2).eq.0)then
            i2=mod(i1+1,4)
            i4=mod(i1+3,4)
          elseif(mod(i3-i1,4).eq.1)then
            i2=mod(i3+1,4)
            i4=mod(i3+2,4)
          else
            i2=mod(i1+1,4)
            i4=mod(i1+2,4)
          endif

          D0comb_coli=D0regrp_coli(q2(i1,i2),q2(i2,i3),q2(i3,i4),
     &        q2(i4,i1),
     &        q2(i1,i3),q2(i2,i4),m2(i1),m2(i2),m2(i3),m2(i4))

          return

        endif
      endif
      enddo
      enddo

      i1=-1
      i2=-1
      i3=-1
      gasomgas = 0d0
      ga = 0d0
      omga = 0d0
      do i=0,3
      do j=0,3
      if(j.ne.i)then
      do k=0,3
      if(k.ne.i.and.k.ne.j)then
        if(real(q2(i,j)).ge.0d0.and.real(q2(i,k)).ge.0d0)then
          lambda=real((q2(i,j)-q2(i,k))**2-2d0*q2(j,k)*(q2(i,j)+q2(i,k))
     &      +q2(j,k)**2)
          if(lambda.ge.0d0)then
            if(real(q2(i,j)-q2(i,k)+q2(j,k)).gt.0d0)then
              ga=real((q2(i,j)-q2(i,k)+q2(j,k)+sqrt(lambda))
     &            /(2d0*q2(i,j)))

            else
              ga=real(2d0*q2(j,k)
     &              /(q2(i,j)-q2(i,k)+q2(j,k)-sqrt(lambda)))

            endif
            if(real(q2(i,j)+q2(i,k)-q2(j,k)).lt.0d0)then
              omga=real((q2(i,j)+q2(i,k)-q2(j,k)-sqrt(lambda))
     &            /(2d0*q2(i,j)))

            else
              omga=real(2d0*q2(i,k)
     &            /(q2(i,j)+q2(i,k)-q2(j,k)+sqrt(lambda)))

            endif

          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0comb_coli',
     &           ' improper call: case 1, lambda<0',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*)
     &            'D0comb_coli: case 1, lambda<0 =',lambda
              write(nerrout_coli,*) 'D0comb_coli: ijk ',i,j,k
              write(nerrout_coli,*) 'D0comb_coli: qijk ',
     &            q2(i,j),q2(i,k),q2(j,k)
              write(nerrout_coli,111)' D0comb_coli: p12 = ',p12
              write(nerrout_coli,111)' D0comb_coli: p23 = ',p23
              write(nerrout_coli,111)' D0comb_coli: p34 = ',p34
              write(nerrout_coli,111)' D0comb_coli: p14 = ',p14
              write(nerrout_coli,111)' D0comb_coli: p13 = ',p13
              write(nerrout_coli,111)' D0comb_coli: p24 = ',p24
              write(nerrout_coli,111)' D0comb_coli: m12 = ',m12
              write(nerrout_coli,111)' D0comb_coli: m22 = ',m22
              write(nerrout_coli,111)' D0comb_coli: m32 = ',m32
              write(nerrout_coli,111)' D0comb_coli: m42 = ',m42
            endif
          endif
        elseif(real(q2(i,j)).lt.0d0.and.real(q2(i,k)).lt.0d0
     &        .and.real(q2(j,k)).gt.0d0)then
          lambda=real((q2(i,j)-q2(i,k))**2-2d0*q2(j,k)*(q2(i,j)+q2(i,k))
     &      +q2(j,k)**2)
          if(lambda.ge.0d0)then
            if(real(q2(i,j)-q2(i,k)+q2(j,k)).lt.0d0)then
              ga=real((q2(i,j)-q2(i,k)+q2(j,k)-sqrt(lambda))
     &            /(2d0*q2(i,j)))


            else
              ga=real(2d0*q2(j,k)
     &            /(q2(i,j)-q2(i,k)+q2(j,k)+sqrt(lambda)))
            endif
            if(real(q2(i,j)+q2(i,k)-q2(j,k)).gt.0d0)then
              omga=real((q2(i,j)+q2(i,k)-q2(j,k)+sqrt(lambda))
     &            /(2d0*q2(i,j)))
            else
              omga=real(2d0*q2(i,k)
     &            /(q2(i,j)+q2(i,k)-q2(j,k)-sqrt(lambda)))
            endif

          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0comb_coli',
     &           ' improper call: case 2, lambda<0',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*)
     &            'D0comb_coli: case 2, lambda<0 =',lambda
              write(nerrout_coli,*) 'D0comb_coli: ijk ',i,j,k
              write(nerrout_coli,*) 'D0comb_coli: qijk ',
     &            q2(i,j),q2(i,k),q2(j,k)
              write(nerrout_coli,111)' D0comb_coli: p12 = ',p12
              write(nerrout_coli,111)' D0comb_coli: p23 = ',p23
              write(nerrout_coli,111)' D0comb_coli: p34 = ',p34
              write(nerrout_coli,111)' D0comb_coli: p14 = ',p14
              write(nerrout_coli,111)' D0comb_coli: p13 = ',p13
              write(nerrout_coli,111)' D0comb_coli: p24 = ',p24
              write(nerrout_coli,111)' D0comb_coli: m12 = ',m12
              write(nerrout_coli,111)' D0comb_coli: m22 = ',m22
              write(nerrout_coli,111)' D0comb_coli: m32 = ',m32
              write(nerrout_coli,111)' D0comb_coli: m42 = ',m42
            endif
          endif

        endif

c select variant that maximises ga*omga
        if (gasomgas.lt.ga*omga) then
          gasomgas = ga*omga
          gas = ga
          omgas = omga
          i1 = i
          i2 = j
          i3 = k
        end if
      endif
      enddo
      endif
      enddo
      enddo

      if (i1.eq.-1) then
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0comb_coli',' INCONSISTENT INPUT: '//
     &      'no viable case found'
     &      ,errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*) 'D0comb_coli: INCONSISTENT INPUT: '
          write(nerrout_coli,'(14x,(a))')
     &        'make sure that momentum invariants that correspond to'
          write(nerrout_coli,'(14x,(a))')
     &     'squared external masses assume their exact numerical value!'
          write(nerrout_coli,111)' D0comb_coli: p12 = ',p12
          write(nerrout_coli,111)' D0comb_coli: p23 = ',p23
          write(nerrout_coli,111)' D0comb_coli: p34 = ',p34
          write(nerrout_coli,111)' D0comb_coli: p14 = ',p14
          write(nerrout_coli,111)' D0comb_coli: p13 = ',p13
          write(nerrout_coli,111)' D0comb_coli: p24 = ',p24
          write(nerrout_coli,111)' D0comb_coli: m12 = ',m12
          write(nerrout_coli,111)' D0comb_coli: m22 = ',m22
          write(nerrout_coli,111)' D0comb_coli: m32 = ',m32
          write(nerrout_coli,111)' D0comb_coli: m42 = ',m42
        endif
      endif

      i4=0
      do l=0,3
        if(l.ne.i1.and.l.ne.i2.and.l.ne.i3) i4=l
      enddo

      ga = gas
      omga = omgas

      if(ga.lt.0d0.or.omga.lt.0d0)then
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0comb_coli',' inconsistent gamma',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,*)
     &        'D0comb_coli: inconsistent gamma =',ga,omga,lambda
          write(nerrout_coli,*) 'D0comb_coli: ijk ',i1,i2,i3
          write(nerrout_coli,*) 'D0comb_coli: qijk ',
     &        q2(i1,i2),q2(i1,i3),q2(i2,i3)
          write(nerrout_coli,111)' D0reg_coli: p12 = ',p12
          write(nerrout_coli,111)' D0reg_coli: p23 = ',p23
          write(nerrout_coli,111)' D0reg_coli: p34 = ',p34
          write(nerrout_coli,111)' D0reg_coli: p14 = ',p14
          write(nerrout_coli,111)' D0reg_coli: p13 = ',p13
          write(nerrout_coli,111)' D0reg_coli: p24 = ',p24
          write(nerrout_coli,111)' D0reg_coli: m12 = ',m12
          write(nerrout_coli,111)' D0reg_coli: m22 = ',m22
          write(nerrout_coli,111)' D0reg_coli: m32 = ',m32
          write(nerrout_coli,111)' D0reg_coli: m42 = ',m42
        endif
      endif

      if(ga.lt.calacc) then
        mm2=(m2(i2)-q2(i2,i3))
        qq2=(q2(i2,i4)-q2(i2,i3))

        if (mm2.ne.cd0) then
          D02= D0regrp_coli(q2(i3,i1),
     &        q2(i1,i2),qq2,q2(i4,i3),
     &        cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))
        else
          D02= D0m0_coli(q2(i3,i1),
     &        q2(i1,i2),qq2,q2(i4,i3),
     &        cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))
        endif

        D0comb_coli=D02

      else if (omga.lt.calacc) then
        mm2=(m2(i1)-q2(i1,i3))
        qq2=(q2(i1,i4)-q2(i1,i3))

        if (mm2.ne.cd0) then
          D01= D0regrp_coli(q2(i3,i2),q2(i2,i1),qq2,
     &        q2(i4,i3),cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4))
        else
          D01= D0m0_coli(q2(i3,i2),q2(i2,i1),qq2,
     &        q2(i4,i3),cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4))
        endif

        D0comb_coli=D01

      else
        mm2=ga*(m2(i1)-q2(i1,i3)) + omga*(m2(i2)-q2(i2,i3))
        qq2=ga*(q2(i1,i4)-q2(i1,i3)) + omga*(q2(i2,i4)-q2(i2,i3))

        if (mm2.ne.cd0) then
          D01= D0regrp_coli(q2(i3,i2),q2(i2,i1)*ga*ga,qq2,
     &        q2(i4,i3),cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4))
          D02= D0regrp_coli(q2(i3,i1),
     &        omga*omga*q2(i1,i2),qq2,q2(i4,i3),
     &        cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))
        else
          D01= D0m0_coli(q2(i3,i2),q2(i2,i1)*ga*ga,qq2,
     &        q2(i4,i3),cd0,q2(i2,i4),m2(i3),m2(i2),mm2,m2(i4))
          D02= D0m0_coli(q2(i3,i1),
     &        omga*omga*q2(i1,i2),qq2,q2(i4,i3),
     &        cd0,q2(i1,i4),m2(i3),m2(i1),mm2,m2(i4))
        endif

        D0comb_coli=ga*D01+omga*D02

      endif

      end



