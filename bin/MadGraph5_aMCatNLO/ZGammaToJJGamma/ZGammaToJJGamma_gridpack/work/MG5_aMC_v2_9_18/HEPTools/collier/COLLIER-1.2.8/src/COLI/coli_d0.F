!!
!!  File coli_d0.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!



#include "global_coli.h"

c#define QCONT

************************************************************************
*                                                                      *
*     Scalar 4- point functions                                        *
*                                                                      *
************************************************************************
*                                                                      *
*     last changed  15.12.11  Ansgar Denner                            *
*     errorflags    29.05.13  Ansgar Denner   updated 26.03.15         *
*                                                                      *
************************************************************************
* Subroutines:                                                         *
* Functions:                                                           *
* D0_coli                                                              *
* D0reg_coli, D0m0_coli, D02m0_coli, D03m0_coli,D04m0_coli             *
* D0ms0ir1_coli, D0ms0ir2_coli, D0ms0ir1m0_coli                        *
* D0ms1ir0_coli, D0ms1ir1_coli, D0ms1ir2_coli                          *
* D0ms2ir0_coli, D0ms2ir1_coli, D0ms2ir2_coli, D0ms2ir3_coli           *
* D0ms3ir2_coli, D0ms4ir4_coli                                         *
************************************************************************

************************************************************************
      function D0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     SCALAR 4-POINT FUNCTION                                          *
*                                                                      *
*                     m22                                              *
*       m12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | m32                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     m42                                              *
*                                                                      *
************************************************************************
*     04.08.08 Ansgar Denner        last changed  31.03.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 :: D0_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 p2(4,4),m2(4)



      complex*16 elimminf2_coli
      complex*16 D0reg_coli
      complex*16 D0m0_coli,D02m0_coli,D03m0_coli,D04m0_coli
      complex*16 D0ms0ir1_coli,D0ms0ir2_coli,D0ms0ir1m0_coli
      complex*16 D0ms1ir0_coli,D0ms1ir1_coli,D0ms1ir2_coli
      complex*16 D0ms2ir0_coli,D0ms2ir1_coli,D0ms2ir2_coli,
     &    D0ms2ir3_coli
      complex*16 D0ms3ir2_coli,D0ms4ir4_coli

      integer i,j,k
      integer nsm,nsoft,ncoll
      logical smallm2(4),smallp2(4,4),soft(4,4,4),coll(4,4),onsh(4,4)
     &    ,sonsh(4,4)
      logical errorwriteflag


      data coll /16*.false./, onsh /16*.false./,
     &    soft /64*.false./ ,smallm2 /4*.false./, smallp2 /16*.false./

#include "params_coli.h"
#include "checkparams_coli.h"

#ifdef CHECK
 101  format(a22,g25.17)
#endif
 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))


      p2(1,2) = p12
      p2(1,3) = p13
      p2(1,4) = p14
      p2(2,3) = p23
      p2(2,4) = p24
      p2(3,4) = p34
      m2(1)   = m12
      m2(2)   = m22
      m2(3)   = m32
      m2(4)   = m42

c determine infinitesimal parameters
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


      smallm2(1)=ms12.eq.cd0
      smallm2(2)=ms22.eq.cd0
      smallm2(3)=ms32.eq.cd0
      smallm2(4)=ms42.eq.cd0
      smallp2(1,2)=ps12.eq.cd0
      smallp2(1,3)=ps13.eq.cd0
      smallp2(1,4)=ps14.eq.cd0
      smallp2(2,3)=ps23.eq.cd0
      smallp2(2,4)=ps24.eq.cd0
      smallp2(3,4)=ps34.eq.cd0

      nsm=0
      do i=1,4
        if(smallm2(i)) nsm=nsm+1
      enddo

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c determine on-shell momenta for small masses
      sonsh(1,2)=ps12.eq.ms22
      sonsh(1,3)=ps13.eq.ms32
      sonsh(1,4)=ps14.eq.ms42
      sonsh(2,1)=ps12.eq.ms12
      sonsh(2,3)=ps23.eq.ms32
      sonsh(2,4)=ps24.eq.ms42
      sonsh(3,1)=ps13.eq.ms12
      sonsh(3,2)=ps23.eq.ms22
      sonsh(3,4)=ps34.eq.ms42
      sonsh(4,1)=ps14.eq.ms12
      sonsh(4,2)=ps24.eq.ms22
      sonsh(4,3)=ps34.eq.ms32



c determine collinear singularities
      ncoll=0
      do i=1,3
      do j=i+1,4
        coll(i,j)=smallm2(i).and.smallm2(j).and.smallp2(i,j)

        if(coll(i,j).and.(.not.
     &      ( (p2(i,j).eq.cd0.and.m2(i).eq.m2(j)).or.
     &        (m2(i).eq.0d0.and.onsh(i,j)).or.
     &        (m2(j).eq.0d0.and.onsh(j,i)) ))) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' case not supported',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0_coli:  collinear singularity ',
     &          '    not supported'
            write(nerrout_coli,111)' D0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0_coli: m42 = ',m42
            write(nerrout_coli,*)  ' D0_coli: i,j = ',i,j
            write(nerrout_coli,*)  ' D0_coli: t1  = ',
     &          p2(i,j).eq.cd0,m2(i).eq.m2(j)
            write(nerrout_coli,*)  ' D0_coli: t2  = ',
     &          m2(i).eq.cd0,onsh(i,j)
            write(nerrout_coli,*)  ' D0_coli: t3  = ',
     &          m2(j).eq.cd0,onsh(j,i)
          endif
          D0_coli = undefined
          return
        endif
        coll(j,i)=coll(i,j)
        if(coll(i,j)) ncoll=ncoll+1


        do k=1,i-1
          if(coll(k,i).and.coll(i,j).and.coll(k,j)) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' case not supported',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)
     &          ' D0_coli: three overlapping collinear '//
     &          ' singularities not supported'
            write(nerrout_coli,111)' D0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0_coli: m42 = ',m42
            write(nerrout_coli,*)  ' D0_coli: i,j = ',k,i,j
            write(nerrout_coli,*)  ' D0_coli: t1  = ',coll(k,i)
            write(nerrout_coli,*)  ' D0_coli: t2  = ',coll(i,j)
            write(nerrout_coli,*)  ' D0_coli: t3  = ',coll(k,j)
          endif
          D0_coli = undefined
          return
          endif
        enddo

      enddo
      enddo

c determine soft singularities for small masses
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i,j,k)=smallm2(i).and.sonsh(i,j).and.sonsh(i,k)
                soft(i,k,j)=soft(i,j,k)
                if(soft(i,j,k)) nsoft=nsoft+1


              endif
            enddo
          endif
        enddo
      enddo


c regular cases
      if(ncoll.eq.0.and.nsoft.eq.0)then
        if(nsm.eq.0)then
          D0_coli = D0reg_coli(ps12,ps23,ps34,ps14,ps13,ps24,
     &        ms12,ms22,ms32,ms42)
        elseif (nsm.eq.1)then
          if (ms12.eq.cd0) then
            D0_coli = D0m0_coli(ps34,ps14,ps12,ps23,ps13,ps24,
     &          ms32,ms42,cd0,ms22)
          elseif (ms22.eq.cd0) then
            D0_coli = D0m0_coli(ps14,ps12,ps23,ps34,ps24,ps13,
     &          ms42,ms12,cd0,ms32)
          elseif (ms32.eq.cd0) then
            D0_coli = D0m0_coli(ps12,ps23,ps34,ps14,ps13,ps24,
     &          ms12,ms22,cd0,ms42)
          elseif (ms42.eq.cd0) then
            D0_coli = D0m0_coli(ps23,ps34,ps14,ps12,ps24,ps13,
     &          ms22,ms32,cd0,ms12)
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0_coli: inconsistency 0ir 0ms 1m0'
            endif
#endif
          endif
        elseif(nsm.eq.2)then
          if (ms12.eq.cd0) then
            if(ms22.eq.cd0)then
              D0_coli = D02m0_coli(ps14,ps12,ps23,ps34,ps24,ps13,
     &            ms42,cd0,cd0,ms32)
            elseif(ms32.eq.cd0)then
              D0_coli = D02m0_coli(ps12,ps13,ps34,ps24,ps23,ps14,
     &            ms22,cd0,cd0,ms42)
            elseif(ms42.eq.cd0)then
              D0_coli = D02m0_coli(ps34,ps14,ps12,ps23,ps13,ps24,
     &            ms32,cd0,cd0,ms22)
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 2m0'
              endif
#endif
            endif
          elseif(ms22.eq.cd0) then
            if(ms32.eq.cd0)then
              D0_coli = D02m0_coli(ps12,ps23,ps34,ps14,ps13,ps24,
     &            ms12,cd0,cd0,ms42)
            elseif(ms42.eq.cd0)then
              D0_coli = D02m0_coli(ps12,ps24,ps34,ps13,ps14,ps23,
     &            ms12,cd0,cd0,ms32)
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 2m0'
              endif

#endif
            endif
          elseif(ms32.eq.cd0) then
            if(ms42.eq.cd0)then
              D0_coli = D02m0_coli(ps23,ps34,ps14,ps12,ps24,ps13,
     &            ms22,cd0,cd0,ms12)
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 2m0'
              endif
#endif
            endif
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 2m0'
            endif
#endif
          endif
        elseif (nsm.eq.3)then
          if(ms42.ne.cd0
     &        .and.ms12.eq.cd0.and.ms22.eq.cd0.and.ms32.eq.cd0
     &        )then
            D0_coli = D03m0_coli(ps14,ps12,ps23,ps34,ps24,ps13,
     &          ms42,cd0,cd0,cd0)
          elseif(ms32.ne.cd0
     &        .and.ms12.eq.cd0.and.ms22.eq.cd0.and.ms42.eq.cd0
     &          )then
            D0_coli = D03m0_coli(ps34,ps14,ps12,ps23,ps13,ps24,
     &          ms32,cd0,cd0,cd0)
          elseif(ms22.ne.cd0
     &        .and.ms12.eq.cd0.and.ms42.eq.cd0.and.ms32.eq.cd0
     &          ) then
            D0_coli = D03m0_coli(ps23,ps34,ps14,ps12,ps24,ps13,
     &          ms22,cd0,cd0,cd0)
          elseif(ms12.ne.cd0
     &        .and.ms42.eq.cd0.and.ms22.eq.cd0.and.ms32.eq.cd0
     &          ) then
            D0_coli = D03m0_coli(ps12,ps23,ps34,ps14,ps13,ps24,
     &          ms12,cd0,cd0,cd0)
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 3m0'
            endif
#endif
          endif
        elseif (nsm.eq.4)then
          if(ms12.eq.cd0.and.ms22.eq.cd0.and.ms32.eq.cd0.and.
     &        ms42.eq.cd0)then
            D0_coli = D04m0_coli(ps12,ps23,ps34,ps14,ps13,ps24,
     &          cd0,cd0,cd0,cd0)
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 0ir 0ms 4m0'
            endif
#endif
          endif
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0_coli: inconsistency 0ir 0ms'
          endif
#endif
        endif

c soft singular cases without collinear singularities
      elseif(ncoll.eq.0.and.nsoft.ge.1)then
        if(soft(1,2,4))then
          if(ms32.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p12,ps23,ps34,p14,ps13,ps24,
     &          m12,m22,ms32,m42)
          elseif(soft(3,4,2))then
            D0_coli = D0ms0ir2_coli(p12,p23,p34,p14,ps13,ps24,
     &          m12,m22,m32,m42)
          else
            D0_coli = D0ms0ir1m0_coli(p12,ps23,ps34,p14,ps13,ps24,
     &          m12,m22,ms32,m42)
          endif
        elseif(soft(2,3,1))then
          if(ms42.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p23,ps34,ps14,p12,ps24,ps13,
     &          m22,m32,ms42,m12)
          elseif(soft(4,1,3))then
            D0_coli = D0ms0ir2_coli(p23,p34,p14,p12,ps24,ps13,
     &          m22,m32,m42,m12)
          else
            D0_coli = D0ms0ir1m0_coli(p23,ps34,ps14,p12,ps24,ps13,
     &          m22,m32,ms42,m12)
          endif
        elseif(soft(3,4,2))then
          if(ms12.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p34,ps14,ps12,p23,ps13,ps24,
     &          m32,m42,ms12,m22)
          elseif(soft(1,2,4))then
            D0_coli = D0ms0ir2_coli(p34,p14,p12,p23,ps13,ps24,
     &          m32,m42,m12,m22)
          else
            D0_coli = D0ms0ir1m0_coli(p34,ps14,ps12,p23,ps13,ps24,
     &          m32,m42,ms12,m22)
          endif
        elseif(soft(4,1,3))then
          if(ms22.ne.cd0)then
            D0_coli = D0ms0ir1_coli(ps14,p12,p23,ps34,ps24,ps13,
     &          m42,m12,ms22,m32)
          elseif(soft(2,3,1))then
            D0_coli = D0ms0ir2_coli(p14,p12,p23,p34,ps24,ps13,
     &          m42,m12,m22,m32)
          else
            D0_coli = D0ms0ir1m0_coli(p14,ps12,ps23,p34,ps24,ps13,
     &          m42,m12,ms22,m32)
          endif
        else if(soft(1,2,3))then
          if(ms42.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p12,ps24,ps34,p13,ps14,ps23,
     &          m12,m22,ms42,m32)
          elseif(soft(4,3,2))then
            D0_coli = D0ms0ir2_coli(p12,p24,p34,p13,ps14,ps23,
     &          m12,m22,m42,m32)
          else
            D0_coli = D0ms0ir1m0_coli(p12,ps24,ps34,p13,ps14,ps23,
     &          m12,m22,ms42,m32)
          endif
        elseif(soft(2,3,4))then
          if(ms12.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p23,ps13,ps14,p24,ps12,ps34,
     &          m22,m32,ms12,m42)
          elseif(soft(1,4,3))then
            D0_coli = D0ms0ir2_coli(p23,p13,p14,p24,ps12,ps34,
     &          m22,m32,m12,m42)
          else
            D0_coli = D0ms0ir1m0_coli(p23,ps13,ps14,p24,ps12,ps34,
     &          m22,m32,ms12,m42)
          endif
        elseif(soft(3,4,1))then
          if(ms22.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p34,ps24,ps12,p13,ps23,ps14,
     &          m32,m42,ms22,m12)
          elseif(soft(2,1,4))then
            D0_coli = D0ms0ir2_coli(p34,p24,p12,p13,ps23,ps14,
     &          m32,m42,m22,m12)
          else
            D0_coli = D0ms0ir1m0_coli(p34,ps24,ps12,p13,ps23,ps14,
     &          m32,m42,ms22,m12)
          endif
        elseif(soft(4,1,2))then
          if(ms32.ne.cd0)then
            D0_coli = D0ms0ir1_coli(ps14,p13,p23,ps24,ps34,ps12,
     &          m42,m12,ms32,m22)
          elseif(soft(3,2,1))then
            D0_coli = D0ms0ir2_coli(p14,p13,p23,p24,ps34,ps12,
     &          m42,m12,m32,m22)
          else
            D0_coli = D0ms0ir1m0_coli(p14,ps13,ps23,p24,ps34,ps12,
     &          m42,m12,ms32,m22)
          endif
        elseif(soft(1,3,4))then
          if(ms22.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p13,ps23,ps24,p14,ps12,ps34,
     &          m12,m32,ms22,m42)
          elseif(soft(2,4,3))then
            D0_coli = D0ms0ir2_coli(p13,p23,p24,p14,ps12,ps34,
     &          m12,m32,m22,m42)
          else
            D0_coli = D0ms0ir1m0_coli(p13,ps23,ps24,p14,ps12,ps34,
     &          m12,m32,ms22,m42)
          endif
        elseif(soft(2,4,1))then
          if(ms32.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p24,ps34,ps13,p12,ps23,ps14,
     &          m22,m42,ms32,m12)
          elseif(soft(3,1,2))then
            D0_coli = D0ms0ir2_coli(p24,p34,p13,p12,ps23,ps14,
     &          m22,m42,m32,m12)
          else
            D0_coli = D0ms0ir1m0_coli(p24,ps34,ps13,p12,ps23,ps14,
     &          m22,m42,ms32,m12)
          endif
        elseif(soft(3,1,2))then
          if(ms42.ne.cd0)then
            D0_coli = D0ms0ir1_coli(p13,ps14,ps24,p23,ps34,ps12,
     &          m32,m12,ms42,m22)
          elseif(soft(4,2,1))then
            D0_coli = D0ms0ir2_coli(p13,p14,p24,p23,ps34,ps12,
     &          m32,m12,m42,m22)
          else
            D0_coli = D0ms0ir1m0_coli(p13,ps14,ps24,p23,ps34,ps12,
     &          m32,m12,ms42,m22)
          endif
        elseif(soft(4,2,3))then
          if(ms12.ne.cd0)then
            D0_coli = D0ms0ir1_coli(ps24,p12,p13,ps34,ps14,ps23,
     &          m42,m22,ms12,m32)
          elseif(soft(1,3,2))then
            D0_coli = D0ms0ir2_coli(p24,p12,p13,p34,ps14,ps23,
     &          m42,m22,m12,m32)
          else
            D0_coli = D0ms0ir1m0_coli(p24,ps12,ps13,p34,ps14,ps23,
     &          m42,m22,ms12,m32)
          endif
#ifdef CHECK
        else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 0ms'
            endif
#endif
        endif

c single collinear case
      elseif(ncoll.eq.1)then
        if(coll(1,2))then
          if(soft(1,2,4))then
            if(soft(2,1,3))then
              D0_coli = D0ms1ir2_coli(p12,ps23,ps34,ps14,ps13,ps24,
     &            m12,m22,ms32,ms42)
            else
              D0_coli = D0ms1ir1_coli(p12,ps23,ps34,ps14,ps13,ps24,
     &            m12,m22,ms32,ms42)
            endif
          elseif(soft(2,1,3))then
              D0_coli = D0ms1ir1_coli(p12,ps14,ps34,ps23,ps24,ps13,
     &            m22,m12,ms42,ms32)
          elseif(soft(1,2,3))then
            if(soft(2,1,4))then
              D0_coli = D0ms1ir2_coli(p12,ps24,ps34,ps13,ps14,ps23,
     &            m12,m22,ms42,ms32)
            else
              D0_coli = D0ms1ir1_coli(p12,ps24,ps34,ps13,ps14,ps23,
     &            m12,m22,ms42,ms32)
            endif
          elseif(soft(2,1,4))then
              D0_coli = D0ms1ir1_coli(p12,ps13,ps34,ps24,ps23,ps14,
     &            m22,m12,ms32,ms42)
          else
            D0_coli = D0ms1ir0_coli(ps34,ps14,p12,ps23,ps13,ps24,
     &          ms32,ms42,m12,m22)
          endif
        elseif(coll(2,3))then
          if(soft(2,3,1))then
            if(soft(3,2,4))then
              D0_coli = D0ms1ir2_coli(p23,ps34,ps14,ps12,ps24,ps13,
     &            m22,m32,ms42,ms12)
            else
              D0_coli = D0ms1ir1_coli(p23,ps34,ps14,ps12,ps24,ps13,
     &            m22,m32,ms42,ms12)
            endif
          elseif(soft(3,2,4))then
              D0_coli = D0ms1ir1_coli(p23,ps12,ps14,ps34,ps13,ps24,
     &            m32,m22,ms12,ms42)
          elseif(soft(2,3,4))then
            if(soft(3,2,1))then
              D0_coli = D0ms1ir2_coli(p23,ps24,ps14,ps13,ps34,ps12,
     &            m32,m22,ms42,ms12)
            else
              D0_coli = D0ms1ir1_coli(p23,ps13,ps14,ps24,ps12,ps34,
     &            m22,m32,ms12,ms42)
            endif
          elseif(soft(3,2,1))then
              D0_coli = D0ms1ir1_coli(p23,ps24,ps14,ps13,ps34,ps12,
     &            m32,m22,ms42,ms12)
          else
            D0_coli = D0ms1ir0_coli(ps14,ps12,p23,ps34,ps24,ps13,
     &          ms42,ms12,m22,m32)
          endif
        elseif(coll(3,4))then
          if(soft(3,4,1))then
            if(soft(4,3,2))then
              D0_coli = D0ms1ir2_coli(p34,ps24,ps12,ps13,ps23,ps14,
     &            m32,m42,ms22,ms12)
            else
              D0_coli = D0ms1ir1_coli(p34,ps24,ps12,ps13,ps23,ps14,
     &            m32,m42,ms22,ms12)
            endif
          elseif(soft(4,3,2))then
              D0_coli = D0ms1ir1_coli(p34,ps13,ps12,ps24,ps14,ps23,
     &            m42,m32,ms12,ms22)
          elseif(soft(3,4,2))then
            if(soft(4,3,1))then
              D0_coli = D0ms1ir2_coli(p34,ps14,ps12,ps23,ps13,ps24,
     &            m32,m42,ms12,ms22)
            else
              D0_coli = D0ms1ir1_coli(p34,ps14,ps12,ps23,ps13,ps24,
     &            m32,m42,ms12,ms22)
            endif
          elseif(soft(4,3,1))then
              D0_coli = D0ms1ir1_coli(p34,ps23,ps12,ps14,ps24,ps13,
     &            m42,m32,ms22,ms12)
          else
            D0_coli = D0ms1ir0_coli(ps12,ps23,p34,ps14,ps13,ps24,
     &          ms12,ms22,m32,m42)
          endif
        elseif(coll(1,4))then
          if(soft(1,4,3))then
            if(soft(4,1,2))then
              D0_coli = D0ms1ir2_coli(p14,ps24,ps23,ps13,ps12,ps34,
     &            m12,m42,ms22,ms32)
            else
              D0_coli = D0ms1ir1_coli(p14,ps24,ps23,ps13,ps12,ps34,
     &            m12,m42,ms22,ms32)
            endif
          elseif(soft(4,1,2))then
              D0_coli = D0ms1ir1_coli(p14,ps13,ps23,ps24,ps34,ps12,
     &            m42,m12,ms32,ms22)
          elseif(soft(1,4,2))then
            if(soft(4,1,3))then
              D0_coli = D0ms1ir2_coli(p14,ps34,ps23,ps12,ps13,ps24,
     &            m12,m42,ms32,ms22)
            else
              D0_coli = D0ms1ir1_coli(p14,ps34,ps23,ps12,ps13,ps24,
     &            m12,m42,ms32,ms22)
            endif
          elseif(soft(4,1,3))then
              D0_coli = D0ms1ir1_coli(p14,ps12,ps23,ps34,ps24,ps13,
     &            m42,m12,ms22,ms32)
          else
            D0_coli = D0ms1ir0_coli(ps23,ps34,p14,ps12,ps24,ps13,
     &          ms22,ms32,m42,m12)
          endif
        elseif(coll(1,3))then
          if(soft(1,3,4))then
            if(soft(3,1,2))then
              D0_coli = D0ms1ir2_coli(p13,ps23,ps24,ps14,ps12,ps34,
     &            m12,m32,ms22,ms42)
            else
              D0_coli = D0ms1ir1_coli(p13,ps23,ps24,ps14,ps12,ps34,
     &            m12,m32,ms22,ms42)
            endif
          elseif(soft(3,1,2))then
              D0_coli = D0ms1ir1_coli(p13,ps14,ps24,ps23,ps34,ps12,
     &            m32,m12,ms42,ms22)
          elseif(soft(1,3,2))then
            if(soft(3,1,4))then
              D0_coli = D0ms1ir2_coli(p13,ps34,ps24,ps12,ps14,ps23,
     &            m12,m32,ms42,ms22)
            else
              D0_coli = D0ms1ir1_coli(p13,ps34,ps24,ps12,ps14,ps23,
     &            m12,m32,ms42,ms22)
            endif
          elseif(soft(3,1,4))then
              D0_coli = D0ms1ir1_coli(p13,ps12,ps24,ps34,ps23,ps14,
     &            m32,m12,ms22,ms42)
          else
            D0_coli = D0ms1ir0_coli(ps24,ps34,p13,ps12,ps23,ps14,
     &          ms22,ms42,m32,m12)
          endif
        elseif(coll(2,4))then
          if(soft(2,4,1))then
            if(soft(4,2,3))then
              D0_coli = D0ms1ir2_coli(p24,ps34,ps13,ps12,ps23,ps14,
     &            m22,m42,ms32,ms12)
            else
              D0_coli = D0ms1ir1_coli(p24,ps34,ps13,ps12,ps23,ps14,
     &            m22,m42,ms32,ms12)
            endif
          elseif(soft(4,2,3))then
              D0_coli = D0ms1ir1_coli(p24,ps12,ps13,ps34,ps14,ps23,
     &            m42,m22,ms12,ms32)
          elseif(soft(2,4,3))then
            if(soft(4,2,1))then
              D0_coli = D0ms1ir2_coli(p24,ps23,ps13,ps14,ps34,ps12,
     &            m42,m22,ms32,ms12)
            else
              D0_coli = D0ms1ir1_coli(p24,ps14,ps13,ps23,ps12,ps34,
     &            m22,m42,ms12,ms32)
            endif
          elseif(soft(4,2,1))then
              D0_coli = D0ms1ir1_coli(p24,ps23,ps13,ps14,ps34,ps12,
     &            m42,m22,ms32,ms12)
          else
            D0_coli = D0ms1ir0_coli(ps13,ps12,p24,ps34,ps23,ps14,
     &          ms32,ms12,m22,m42)
          endif
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0_coli: inconsistency 1ms'
          endif
#endif
        endif

c double collinear case
      elseif(ncoll.eq.2)then
        if(coll(1,2))then
          if(coll(3,4))then
            D0_coli = D0ms2ir0_coli(p12,ps23,p34,ps14,ps13,ps24,
     &          m12,m22,m32,m42)
          elseif(coll(2,3))then
            if(soft(1,2,4))then
              if(soft(3,2,4))then
                D0_coli = D0ms2ir3_coli(p12,p23,ps34,ps14,ps13,ps24,
     &              m12,m22,m32,ms42)
              else
                D0_coli = D0ms2ir2_coli(p12,p23,ps34,ps14,ps13,ps24,
     &              m12,m22,m32,ms42)
              endif
            elseif(soft(3,2,4))then
              D0_coli = D0ms2ir2_coli(p23,p12,ps14,ps34,ps13,ps24,
     &            m32,m22,m12,ms42)
            else
              D0_coli = D0ms2ir1_coli(p23,ps34,ps14,p12,ps24,ps13,
     &              m22,m32,ms42,m12)
            endif
          elseif(coll(1,4))then
            if(soft(4,1,3))then
              if(soft(2,1,3))then
                D0_coli = D0ms2ir3_coli(p14,p12,ps23,ps34,ps24,ps13,
     &              m42,m12,m22,ms32)
              else
                D0_coli = D0ms2ir2_coli(p14,p12,ps23,ps34,ps24,ps13,
     &              m42,m12,m22,ms32)
              endif
            elseif(soft(2,1,3))then
              D0_coli = D0ms2ir2_coli(p12,p14,ps34,ps23,ps24,ps13,
     &            m22,m12,m42,m32)
            else
              D0_coli = D0ms2ir1_coli(p12,ps23,ps34,p14,ps13,ps24,
     &              m12,m22,ms32,m42)
            endif
          elseif(coll(2,4))then
            if(soft(1,2,3))then
              if(soft(4,2,3))then
                D0_coli = D0ms2ir3_coli(p12,p24,ps34,ps13,ps14,ps23,
     &              m12,m22,m42,ms32)
              else
                D0_coli = D0ms2ir2_coli(p12,p24,ps34,ps13,ps14,ps23,
     &              m12,m22,m42,ms32)
              endif
            elseif(soft(4,2,3))then
              D0_coli = D0ms2ir2_coli(p24,p12,ps13,ps34,ps14,ps23,
     &            m42,m22,m12,ms32)
            else
              D0_coli = D0ms2ir1_coli(p24,ps34,ps13,p12,ps23,ps14,
     &              m22,m42,ms32,m12)
            endif
          elseif(coll(1,3))then
            if(soft(3,1,4))then
              if(soft(2,1,4))then
                D0_coli = D0ms2ir3_coli(p13,p12,ps24,ps34,ps23,ps14,
     &              m32,m12,m22,ms42)
              else
                D0_coli = D0ms2ir2_coli(p13,p12,ps24,ps34,ps23,ps14,
     &              m32,m12,m22,ms42)
              endif
            elseif(soft(2,1,4))then
              D0_coli = D0ms2ir2_coli(p12,p13,ps34,ps24,ps23,ps14,
     &            m22,m12,m32,m42)
            else
              D0_coli = D0ms2ir1_coli(p12,ps24,ps34,p13,ps14,ps23,
     &              m12,m22,ms42,m32)
            endif
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 2ms 12'
            endif
          endif
        elseif(coll(3,4))then


          if(coll(4,1))then
            if(soft(3,4,2))then
              if(soft(1,4,2))then
                D0_coli = D0ms2ir3_coli(p34,p14,ps12,ps23,ps13,ps24,
     &              m32,m42,m12,ms22)
              else
                D0_coli = D0ms2ir2_coli(p34,p14,ps12,ps23,ps13,ps24,
     &              m32,m42,m12,ms22)
              endif
            elseif(soft(1,4,2))then
              D0_coli = D0ms2ir2_coli(p14,p34,ps23,ps12,ps13,ps24,
     &            m12,m42,m32,ms22)
            else
              D0_coli = D0ms2ir1_coli(p14,ps12,ps23,p34,ps24,ps13,
     &              m42,m12,ms22,m32)
            endif
          elseif(coll(3,2))then
            if(soft(2,3,1))then
              if(soft(4,3,1))then
                D0_coli = D0ms2ir3_coli(p23,p34,ps14,ps12,ps24,ps13,
     &              m22,m32,m42,ms12)
              else
                D0_coli = D0ms2ir2_coli(p23,p34,ps14,ps12,ps24,ps13,
     &              m22,m32,m42,ms12)
              endif
            elseif(soft(4,3,1))then
              D0_coli = D0ms2ir2_coli(p34,p23,ps12,ps14,ps24,ps13,
     &            m42,m32,m22,m12)
            else
              D0_coli = D0ms2ir1_coli(p34,ps14,ps12,p23,ps13,ps24,
     &              m32,m42,ms12,m22)
            endif
          elseif(coll(4,2))then
            if(soft(3,4,1))then
              if(soft(2,4,1))then
                D0_coli = D0ms2ir3_coli(p34,p24,ps12,ps13,ps23,ps14,
     &              m32,m42,m22,ms12)
              else
                D0_coli = D0ms2ir2_coli(p34,p24,ps12,ps13,ps23,ps14,
     &              m32,m42,m22,ms12)
              endif
            elseif(soft(2,4,1))then
              D0_coli = D0ms2ir2_coli(p24,p34,ps13,ps12,ps23,ps14,
     &            m22,m42,m32,ms12)
            else
              D0_coli = D0ms2ir1_coli(p24,ps12,ps13,p34,ps14,ps23,
     &              m42,m22,ms12,m32)
            endif
          elseif(coll(3,1))then
            if(soft(1,3,2))then
              if(soft(4,3,2))then
                D0_coli = D0ms2ir3_coli(p13,p34,ps24,ps12,ps14,ps23,
     &              m12,m32,m42,ms22)
              else
                D0_coli = D0ms2ir2_coli(p13,p34,ps24,ps12,ps14,ps23,
     &              m12,m32,m42,ms22)
              endif
            elseif(soft(4,3,2))then
              D0_coli = D0ms2ir2_coli(p34,p13,ps12,ps24,ps14,ps23,
     &            m42,m32,m12,ms22)
            else
              D0_coli = D0ms2ir1_coli(p34,ps24,ps12,p13,ps23,ps14,
     &              m32,m42,ms22,m12)
            endif
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 2ms 34'
            endif
          endif

        elseif(coll(2,3))then
          if(coll(4,1))then
            D0_coli = D0ms2ir0_coli(p23,ps34,p14,ps12,ps24,ps13,
     &          m22,m32,m42,m12)
          elseif(coll(3,1))then
            if(soft(2,3,4))then
              if(soft(1,3,4))then
                D0_coli = D0ms2ir3_coli(p23,p13,ps14,ps24,ps12,ps34,
     &              m22,m32,m12,ms42)
              else
                D0_coli = D0ms2ir2_coli(p23,p13,ps14,ps24,ps12,ps34,
     &              m22,m32,m12,ms42)
              endif
            elseif(soft(1,3,4))then
              D0_coli = D0ms2ir2_coli(p13,p23,ps24,ps14,ps12,ps34,
     &            m12,m32,m22,ms42)
            else
              D0_coli = D0ms2ir1_coli(p13,ps14,ps24,p23,ps34,ps12,
     &              m32,m12,ms42,m22)
            endif
          elseif(coll(2,4))then
            if(soft(4,2,1))then
              if(soft(3,2,1))then
                D0_coli = D0ms2ir3_coli(p24,p23,ps13,ps14,ps34,ps12,
     &              m42,m22,m32,ms12)
              else
                D0_coli = D0ms2ir2_coli(p24,p23,ps13,ps14,ps34,ps12,
     &              m42,m22,m32,ms12)
              endif
            elseif(soft(3,2,1))then
              D0_coli = D0ms2ir2_coli(p23,p24,ps14,ps13,ps34,ps12,
     &            m32,m22,m42,ms12)
            else
              D0_coli = D0ms2ir1_coli(p23,ps13,ps14,p24,ps12,ps34,
     &              m22,m32,ms12,m42)
            endif
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 23'
            endif
          endif
        elseif(coll(4,1))then
          if(coll(1,3))then
            if(soft(4,1,2))then
              if(soft(3,1,2))then
                D0_coli = D0ms2ir3_coli(p14,p13,ps23,ps24,ps34,ps12,
     &              m42,m12,m32,ms22)
              else
                D0_coli = D0ms2ir2_coli(p14,p13,ps23,ps24,ps34,ps12,
     &              m42,m12,m32,ms22)
              endif
            elseif(soft(3,1,2))then
              D0_coli = D0ms2ir2_coli(p13,p14,ps24,ps23,ps34,ps12,
     &            m32,m12,m42,ms22)
            else
              D0_coli = D0ms2ir1_coli(p13,ps23,ps24,p14,ps12,ps34,
     &              m12,m32,ms22,m42)
            endif
          elseif(coll(4,2))then
            if(soft(2,4,3))then
              if(soft(1,4,3))then
                D0_coli = D0ms2ir3_coli(p24,p14,ps13,ps23,ps12,ps34,
     &              m22,m42,m12,ms32)
              else
                D0_coli = D0ms2ir2_coli(p24,p14,ps13,ps23,ps12,ps34,
     &              m22,m42,m12,ms32)
              endif
            elseif(soft(1,4,3))then
              D0_coli = D0ms2ir2_coli(p14,p24,ps23,ps13,ps12,ps34,
     &            m12,m42,m22,ms32)
            else
              D0_coli = D0ms2ir1_coli(p14,ps13,ps23,p24,ps34,ps12,
     &              m42,m12,ms32,m22)
            endif
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 2ms 41'
            endif
          endif

        elseif(coll(1,3))then
          if(coll(2,4))then
            D0_coli = D0ms2ir0_coli(p13,ps23,p24,ps14,ps12,ps34,
     &          m12,m32,m22,m42)
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 13'
            endif
          endif

        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0_coli: inconsistency 2ms'
          endif
        endif

c triple collinear case
      elseif(ncoll.eq.3)then


        if(coll(1,2))then


          if(coll(1,4))then
            if(coll(2,3))then
              D0_coli = D0ms3ir2_coli(p12,p23,ps34,p14,ps13,ps24,
     &            m12,m22,m32,m42)
            elseif(coll(3,4))then
              D0_coli = D0ms3ir2_coli(p14,p12,ps23,p34,ps24,ps13,
     &            m42,m12,m22,m32)
            elseif(coll(2,4))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif

#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 12 14'
              endif
#endif
            endif
          elseif(coll(2,3))then
            if(coll(3,4))then
              D0_coli = D0ms3ir2_coli(p23,p34,ps14,p12,ps24,ps13,
     &            m22,m32,m42,m12)
            elseif(coll(1,3))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &            ' D0_coli: three overlapping collinear'
     &            //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 12 33'
              endif
#endif
            endif
          elseif(coll(1,3))then
            if(coll(2,4))then
              D0_coli = D0ms3ir2_coli(p12,p13,ps34,p24,ps23,ps14,
     &            m22,m12,m32,m42)
            elseif(coll(3,4))then
              D0_coli = D0ms3ir2_coli(p13,p34,ps24,p12,ps14,ps23,
     &            m12,m32,m42,m22)
            elseif(coll(2,3))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 12 13'
              endif
#endif
            endif
          elseif(coll(2,4))then
            if(coll(3,4))then
              D0_coli = D0ms3ir2_coli(p24,p34,ps13,p12,ps23,ps14,
     &            m22,m42,m32,m12)
            elseif(coll(1,2))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 12 24'
              endif
#endif
            endif
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 12 24'
            endif
#endif
          endif

        elseif(coll(3,4))then


          if(coll(2,3))then


            if(coll(1,4))then


              D0_coli = D0ms3ir2_coli(p34,p14,ps12,p23,ps13,ps24,
     &            m32,m42,m12,m22)
            elseif(coll(2,4))then
              write(nerrout_coli,*)
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 34 23'
                write(nerrout_coli,*)  ' D0_coli args=',
     &              p12,p23,p34,p14,p13,p24,m12,m22,m32,m42
                write(nerrout_coli,*)  ' D0_coli sargs=',
     &              ps12,ps23,ps34,ps14,ps13,ps24,ms12,ms22,ms32,ms42
              endif
#endif
            endif
          elseif(coll(1,3))then
            if(coll(2,4))then
              D0_coli = D0ms3ir2_coli(p34,p13,ps12,p24,ps14,ps23,
     &            m42,m32,m12,m22)
            elseif(coll(1,4))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 34 13'
              endif
#endif
            endif
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0_coli: inconsistency 3ms 34'
              endif
#endif
          endif

        elseif(coll(1,4))then
          if(coll(1,3))then
            if(coll(2,4))then
              D0_coli = D0ms3ir2_coli(p14,p13,ps23,p24,ps34,ps12,
     &            m42,m12,m32,m22)
            elseif(coll(2,3))then
              D0_coli = D0ms3ir2_coli(p13,p23,ps24,p14,ps12,ps34,
     &            m12,m32,m22,m42)
            elseif(coll(3,4))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
              write(nerrout_coli,*)
     &            ' D0_coli: three overlapping collinear'
     &            //' singularities not supported'
            endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 14 13'
              endif
#endif
            endif
          elseif(coll(2,4))then
            if(coll(2,3))then
              D0_coli = D0ms3ir2_coli(p24,p23,ps13,p14,ps34,ps12,
     &            m42,m22,m32,m12)
            elseif(coll(1,2))then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 14 24'
            endif
#endif
            endif
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 14'
            endif
#endif
          endif

        elseif(coll(2,3))then
          if(coll(2,4))then
            if(coll(1,3))then
              D0_coli = D0ms3ir2_coli(p23,p24,ps14,p13,ps34,ps12,
     &            m32,m22,m42,m12)
            elseif(coll(1,2))then
              write(nerrout_coli,*)
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' case not supported',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,*)
     &              ' D0_coli: three overlapping collinear'
     &              //' singularities not supported'
              endif
#ifdef CHECK
            else
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)
     &              ' D0_coli: inconsistency 3ms 23 24'
              endif
#endif
            endif
#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms 23'
            endif
#endif
          endif

#ifdef CHECK
        else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0_coli: inconsistency 3ms'
            endif
#endif
        endif

c quartic collinear case
      elseif(ncoll.eq.4)then
        if(coll(1,2).and.coll(2,3)
#ifdef CHECK
     &      .and.coll(3,4).and.coll(1,4)
#endif
     &      )then
          D0_coli = D0ms4ir4_coli(p12,p23,p34,p14,ps13,ps24,
     &              m12,m22,m32,m42)
        elseif(coll(1,2).and.coll(1,3)
#ifdef CHECK
     &        .and.coll(3,4).and.coll(2,4)
#endif
     &        )then
          D0_coli = D0ms4ir4_coli(p12,p24,p34,p13,ps14,ps23,
     &              m12,m22,m42,m32)
        elseif(coll(2,3).and.coll(1,3)
#ifdef CHECK
     &        .and.coll(1,4).and.coll(2,4)
#endif
     &        )then
          D0_coli = D0ms4ir4_coli(p13,p23,p24,p14,ps12,ps34,
     &              m12,m32,m22,m42)
#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0_coli',' branch not found',
     &          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0_coli: inconsistency 4ms'
          endif
#endif
        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0_coli','case not implemented',
     &          errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' D0_coli: case with more than 4 collinear ',
     &        'singularities not implemented'
        endif
#endif
      endif

      end

************************************************************************
      function D0reg_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  general scalar 4-point function                                     *
*  regular case  based on general result of                            *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
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
*  30.04.08 Ansgar Denner       last changed 15.06.11 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12a,r13a,r14a,r23a,r24a,r34a
      complex*16 r21a,r31a,r41a,r32a,r42a,r43a
      complex*16 r12b,r13b,r14b,r23b,r24b,r34b
      complex*16 r21b,r31b,r41b,r32b,r42b,r43b
      complex*16 D0reg_coli,D0regrp_coli,D0comb_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 D0reg_check,D0regsd_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      logical flag(0:8)
      data    flag /.false.,8*.true./
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
          call ErrOut_coli('D0reg_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0reg_coli called improperly:'
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
      endif
#endif

      l12 = (m12+m22-p12)
      l13 = (m12+m32-p13)
      l14 = (m12+m42-p14)
      l23 = (m22+m32-p23)
      l24 = (m22+m42-p24)
      l34 = (m32+m42-p34)

      if(l12.ne.cd0)then
        r12a = l12/(2d0*m22)*(1d0+sqrt(1d0-4d0*m12*m22/l12**2))
      else
        r12a = dcmplx(0d0,1d0)*sqrt(m12/m22)
      endif
      r21a = r12a*m22/m12
      if(l13.ne.cd0)then
        r13a = l13/(2d0*m32)*(1d0+sqrt(1d0-4d0*m12*m32/l13**2))
      else
        r13a = dcmplx(0d0,1d0)*sqrt(m12/m32)
      endif
      r31a = r13a*m32/m12
      if(l14.ne.cd0)then
        r14a = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14a = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41a = r14a*m42/m12
      if(l23.ne.cd0)then
        r23a = l23/(2d0*m32)*(1d0+sqrt(1d0-4d0*m22*m32/l23**2))
      else
        r23a = dcmplx(0d0,1d0)*sqrt(m22/m32)
      endif
      r32a = r23a*m32/m22
      if(l24.ne.cd0)then
        r24a = l24/(2d0*m42)*(1d0+sqrt(1d0-4d0*m22*m42/l24**2))
      else
        r24a = dcmplx(0d0,1d0)*sqrt(m22/m42)
      endif
      r42a = r24a*m42/m22
      if(l34.ne.cd0)then
        r34a = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
      else
        r34a = dcmplx(0d0,1d0)*sqrt(m32/m42)
      endif
      r43a = r34a*m42/m32

      r12b=1d0/r21a
      r21b = r12b*m22/m12
      r13b=1d0/r31a
      r31b = r13b*m32/m12
      r14b=1d0/r41a
      r41b = r14b*m42/m12
      r23b=1d0/r32a
      r32b = r23b*m32/m22
      r24b=1d0/r42a
      r42b = r24b*m42/m22
      r34b=1d0/r43a
      r43b = r34b*m42/m32


      if(.not.(real(p12).lt.0d0.and.real(p23).lt.0d0.and.
     &         real(p34).lt.0d0.and.real(p14).lt.0d0.and.
     &         real(p13).lt.0d0.and.real(p24).lt.0d0))then

        D0reg_coli=
     &      D0comb_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
      elseif(aimag(r31a).ne.0d0.and.real(r31a).gt.0d0.and.(
     &    aimag(m22*r31a*r31a).le.0d0.and.aimag(m42*r31a*r31a).le.0d0
     &    .and.aimag(r31a*l23).le.0d0.and.aimag(r31a*l34).le.0d0.and.
     &    aimag(r31a*r31a*l24).le.0d0 .or.
     &    aimag(m22*r31b*r31b).le.0d0.and.aimag(m42*r31b*r31b).le.0d0
     &    .and.aimag(r31b*l23).le.0d0.and.aimag(r31b*l34).le.0d0.and.
     &    aimag(r31b*r31b*l24).le.0d0) .or.
     &    aimag(r31a).eq.0d0.and.real(r31a).gt.0d0 ) then


        D0reg_coli=
     &      D0regrp_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
      elseif(aimag(r42a).ne.0d0.and.real(r42a).gt.0d0.and.(
     &    aimag(m32*r42a*r42a).le.0d0.and.aimag(m12*r42a*r42a).le.0d0
     &    .and.aimag(r42a*l34).le.0d0.and.aimag(r42a*l14).le.0d0.and.
     &    aimag(r42a*r42a*l13).le.0d0.or.
     &    aimag(m32*r42b*r42b).le.0d0.and.aimag(m12*r42b*r42b).le.0d0
     &    .and.aimag(r42b*l34).le.0d0.and.aimag(r42b*l14).le.0d0.and.
     &    aimag(r42b*r42b*l13).le.0d0) .or.
     &    aimag(r42a).eq.0d0.and.real(r42a).gt.0d0 ) then


        D0reg_coli=
     &      D0regrp_coli(p23,p34,p14,p12,p24,p13,m22,m32,m42,m12)
      elseif(aimag(r21a).ne.0d0.and.real(r21a).gt.0d0.and.(
     &    aimag(m32*r21a*r21a).le.0d0.and.aimag(m42*r21a*r21a).le.0d0
     &    .and.aimag(r21a*l23).le.0d0.and.aimag(r21a*l24).le.0d0.and.
     &    aimag(r21a*r21a*l34).le.0d0 .or.
     &    aimag(m32*r21b*r21b).le.0d0.and.aimag(m42*r21b*r21b).le.0d0
     &    .and.aimag(r21b*l23).le.0d0.and.aimag(r21b*l24).le.0d0.and.
     &    aimag(r21b*r21b*l34).le.0d0) .or.
     &    aimag(r21a).eq.0d0.and.real(r21a).gt.0d0 ) then



        D0reg_coli=
     &      D0regrp_coli(p13,p23,p24,p14,p12,p34,m12,m32,m22,m42)
      elseif(aimag(r43a).ne.0d0.and.real(r43a).gt.0d0.and.(
     &    aimag(m22*r43a*r43a).le.0d0.and.aimag(m12*r43a*r43a).le.0d0
     &    .and.aimag(r43a*l24).le.0d0.and.aimag(r43a*l14).le.0d0.and.
     &    aimag(r43a*r43a*l12).le.0d0 .or.
     &    aimag(m22*r43b*r43b).le.0d0.and.aimag(m12*r43b*r43b).le.0d0
     &    .and.aimag(r43b*l24).le.0d0.and.aimag(r43b*l14).le.0d0.and.
     &    aimag(r43b*r43b*l12).le.0d0) .or.
     &    aimag(r43a).eq.0d0.and.real(r43a).gt.0d0 ) then

#ifdef NEWCHECK
        if(flag(4))then
          call ErrOut_coli('D0reg_coli','new function called: rp 3241',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &              'D0reg_coli: used rp perm 3241',r43a,r43b
            flag(4)=.false.
          endif
        endif
#endif

        D0reg_coli=
     &      D0regrp_coli(p23,p24,p14,p13,p34,p12,m32,m22,m42,m12)
      elseif(aimag(r41a).ne.0d0.and.real(r41a).gt.0d0.and.(
     &    aimag(m22*r41a*r41a).le.0d0.and.aimag(m32*r41a*r41a).le.0d0
     &    .and.aimag(r41a*l24).le.0d0.and.aimag(r41a*l34).le.0d0.and.
     &    aimag(r41a*r41a*l23).le.0d0 .or.
     &    aimag(m22*r41b*r41b).le.0d0.and.aimag(m32*r41b*r41b).le.0d0
     &    .and.aimag(r41b*l24).le.0d0.and.aimag(r41b*l34).le.0d0.and.
     &    aimag(r41b*r41b*l23).le.0d0) .or.
     &    aimag(r41a).eq.0d0.and.real(r41a).gt.0d0 ) then

#ifdef NEWCHECK
        if(flag(5))then
          call ErrOut_coli('D0reg_coli','new function called: rp 1243',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &               'D0reg_coli: used rp perm 1243',r41a,r41b
            flag(5)=.false.
          endif
        endif
#endif

        D0reg_coli=
     &      D0regrp_coli(p12,p24,p34,p13,p14,p23,m12,m22,m42,m32)
      elseif(aimag(r32a).ne.0d0.and.real(r32a).gt.0d0.and.(
     &      aimag(m42*r32a*r32a).le.0d0.and.aimag(m12*r32a*r32a).le.0d0
     &      .and.aimag(r32a*l34).le.0d0.and.aimag(r32a*l13).le.0d0.and.
     &      aimag(r32a*r32a*l14).le.0d0.or.
     &      aimag(m42*r32b*r32b).le.0d0.and.aimag(m12*r32b*r32b).le.0d0
     &      .and.aimag(r32b*l34).le.0d0.and.aimag(r32b*l13).le.0d0.and.
     &      aimag(r32b*r32b*l14).le.0d0) .or.
     &      aimag(r32a).eq.0d0.and.real(r32a).gt.0d0 ) then

#ifdef NEWCHECK
        if(flag(6))then
          call ErrOut_coli('D0reg_coli','new function called: rp 2431',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &              'D0reg_coli: used rp perm 2431',r32a,r32b
            flag(6)=.false.
          endif
        endif
#endif

        D0reg_coli=
     &      D0regrp_coli(p24,p34,p13,p12,p23,p14,m22,m42,m32,m12)
      elseif(aimag(r24a).ne.0d0.and.real(r24a).gt.0d0.and.(
     &      aimag(m32*r24a*r24a).le.0d0.and.aimag(m12*r24a*r24a).le.0d0
     &      .and.aimag(r24a*l23).le.0d0.and.aimag(r24a*l12).le.0d0.and.
     &      aimag(r24a*r24a*l13).le.0d0.or.
     &      aimag(m32*r24b*r24b).le.0d0.and.aimag(m12*r24b*r24b).le.0d0
     &      .and.aimag(r24b*l23).le.0d0.and.aimag(r24b*l12).le.0d0.and.
     &      aimag(r24b*r24b*l13).le.0d0) .or.
     &      aimag(r24a).eq.0d0.and.real(r24a).gt.0d0 ) then

#ifdef NEWCHECK
        if(flag(7))then
          call ErrOut_coli('D0reg_coli','new function called: rp 4321',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &              'D0reg_coli: used rp perm 4321',r24a,r24b
            flag(7)=.false.
          endif
        endif
#endif

        D0reg_coli=
     &      D0regrp_coli(p34,p23,p12,p14,p24,p13,m42,m32,m22,m12)
      elseif(aimag(r34a).ne.0d0.and.real(r34a).gt.0d0.and.(
     &      aimag(m22*r34a*r34a).le.0d0.and.aimag(m12*r34a*r34a).le.0d0
     &      .and.aimag(r34a*l23).le.0d0.and.aimag(r34a*l13).le.0d0.and.
     &      aimag(r34a*r34a*l12).le.0d0.or.
     &      aimag(m22*r34b*r34b).le.0d0.and.aimag(m12*r34b*r34b).le.0d0
     &      .and.aimag(r34b*l23).le.0d0.and.aimag(r34b*l13).le.0d0.and.
     &      aimag(r34b*r34b*l12).le.0d0) .or.
     &      aimag(r34a).eq.0d0.and.real(r34a).gt.0d0 ) then

#ifdef NEWCHECK
        if(flag(8))then
          call ErrOut_coli('D0reg_coli','new function called: rp 4231',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)
     &              'D0reg_coli: used rp perm 4231',r34a,r34b
            flag(8)=.false.
          endif
        endif
#endif

        D0reg_coli=
     &      D0regrp_coli(p24,p23,p13,p14,p34,p12,m42,m22,m32,m12)

      else

#ifdef CHECK
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0reg_coli',' no viable case found',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100) ' D0reg_coli called improperly'
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
#endif

      endif


      end




************************************************************************
      function D0m0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for m32 = 0                                *
*  regular case                                                        *
*                                                                      *
*                     m22                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | 0d0                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  29.03.92 Ansgar Denner       last changed  14.05.10 Ansgar Denner   *
*                      Bug in checks removed  21.01.13 Ansgar Denner   *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 D0m0_coli
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42


      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 mm12,mm22,mm42,swap
      real*8     ir12,ir14,ir24
      real*8     ix1(2),ix4(2),iqbard(2)
      real*8     test0,test1,test2,test3,test4,test5
      real*8     test01,test23,test45
      real*8     u,v
      real*8     acc
      complex*16 r12,r14,r24,r21,r41,r42
      complex*16 a,b,c,d,det
      complex*16 x1(2),x4(2)
      complex*16 ch1,ch2,ch3,l1,ch4,ch5,l2,argl1
      complex*16 eta
      complex*16 cspcos_coli,cln_coli,eta2s_coli
      complex*16 mat(4,4),chdet
      integer    i,j
      logical errorwriteflag

      logical    flag2
      save       flag2
      data       flag2 /.true./
      data acc/1d-7/

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D0m0_check
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

        if(m32.ne.0d0.or.m12.eq.cd0.or.m22.eq.cd0.or.m42.eq.cd0
     &      .or.p23.eq.m22.and.p13.eq.m12
     &      .or.p23.eq.m22.and.p34.eq.m42
     &      .or.p13.eq.m12.and.p34.eq.m42
     &      .or.ps23.eq.cd0.and.ms22.eq.cd0
     &      .or.ps34.eq.cd0.and.ms42.eq.cd0
     &      .or.ps13.eq.cd0.and.ms12.eq.cd0
     &      .or.ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &      .or.ps24.eq.cd0.and.ms22.eq.cd0.and.ms22.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0m0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0m0_coli called improperly:'
            write(nerrout_coli,111)' D0m0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0m0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0m0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0m0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0m0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0m0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0m0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0m0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0m0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0m0_coli: m42 = ',m42
            write(nerrout_coli,*)' D0m0_coli: test= ',
     &          m32.ne.0d0,m12.eq.cd0,m22.eq.cd0,m42.eq.cd0,
     &          p23.eq.m22.and.p13.eq.m12
     &          ,p23.eq.m22.and.p34.eq.m42
     &          ,p13.eq.m12.and.p34.eq.m42
     &          ,ps23.eq.cd0.and.ms22.eq.cd0
     &          ,ps34.eq.cd0.and.ms42.eq.cd0
     &          ,ps13.eq.cd0.and.ms12.eq.cd0
     &          ,ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &          ,ps14.eq.cd0.and.ms12.eq.cd0.and.ms42.eq.cd0
     &          ,ps24.eq.cd0.and.ms22.eq.cd0.and.ms22.eq.cd0
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p34).ne.0d0,aimag(p14).ne.0d0
     &          ,aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif


c     (permutation 1 -> 3 -> 2 -> 4 -> 1 for  m -> k )
c        m1 -> m2 in normalization

      mm12=m12
      mm22=m22
      mm42=m42

      l12 = (mm22+mm12-p12)
      l13 = (mm12    -p13)
      l14 = (mm42+mm12-p14)
      l23 = (mm22    -p23)
      l24 = (mm22+mm42-p24)
      l34 = (    mm42-p34)

      if(l12.ne.cd0)then
        r12 = l12/(2d0*mm22)*(1d0+sqrt(1d0-4d0*mm12*mm22/l12**2))
      else
        r12 =  dcmplx(0d0,1d0)*sqrt(mm12/mm22)
      endif
      r21 = r12*mm22/mm12


      if (abs(l23*l14).lt.abs(l12*l34)) then
        if(l14.ne.cd0)then
          r14 = l14/(2d0*mm42)*(1d0+sqrt(1d0-4d0*mm12*mm42/l14**2))
        else
          r14 = dcmplx(0d0,1d0)*sqrt(mm12/mm42)
        endif
        if(l24.ne.cd0)then
          r24 = l24/(2d0*mm42)*(1d0+sqrt(1d0-4d0*mm22*mm42/l24**2))
        else
          r24 = dcmplx(0d0,1d0)*sqrt(mm22/mm42)
        endif
      else
        if(l14.ne.cd0)then
          r14 = 2d0*mm12/(l14*(1d0+sqrt(1d0-4d0*mm12*mm42/l14**2)))
        else
          r14 = -dcmplx(0d0,1d0)*sqrt(mm12/mm42)
        endif
        if(l24.ne.cd0)then
          r24 = 2d0*mm22/(l24*(1d0+sqrt(1d0-4d0*mm22*mm42/l24**2)))
        else
          r24 = -dcmplx(0d0,1d0)*sqrt(mm22/mm42)
        endif
      endif
      r41 = r14*mm42/mm12
      r42 = r24*mm42/mm22

      r24 = 1/r42
      r42 = r24*mm42/mm22

      a =  mm42*(l34/r42 - l23)

c swap if a=0
      if(a.eq.cd0)then
        mm12=m22
        mm22=m12
        swap=l23
        l23=l13
        l13=swap
        swap=l24
        l24=l14
        l14=swap
        swap=r12
        r12=r21
        r21=swap
        swap=r24
        r24=r14
        r14=swap
        swap=r42
        r42=r41
        r41=swap
        a = mm42*(l34/r42 - l23)
      endif

      if(a.eq.cd0)then
        mm12=m42
        mm42=m12
        swap=l34
        l34=l13
        l13=swap
        swap=l24
        l24=l12
        l12=swap
        swap=r14
        r14=r41
        r41=swap
        swap=r24
        r24=r12
        r12=swap
        swap=r42
        r42=r21
        r21=swap
        a = mm42*(l34/r42 - l23)
      endif

      if(real(l12).lt.-0d0) then
        ir12 = sign(1d1,1d0-abs(r12*r12*mm22/mm12))
      else
        ir12 = 0d0
      endif
      if(real(l14).lt.-0d0) then
        ir14 = sign(1d1,1d0-abs(r14*r14*mm42/mm12))
      else
        ir14 = 0d0
      endif
      if(real(l24).lt.-0d0) then
        ir24 = sign(1d1,1d0-abs(r24*r24*mm42/mm22))
      else
        ir24 = 0d0
      endif

      if(abs(a).lt.calacc*abs(mm42)*max(abs(l34/r42),abs(l23))) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0m0_coli','case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0m0_coli: case not implemented'
          write(nerrout_coli,100)' a = 0 '
          write(nerrout_coli,111)' D0m0_coli: p12 = ',p12
          write(nerrout_coli,111)' D0m0_coli: p23 = ',p23
          write(nerrout_coli,111)' D0m0_coli: p34 = ',p34
          write(nerrout_coli,111)' D0m0_coli: p14 = ',p14
          write(nerrout_coli,111)' D0m0_coli: p24 = ',p24
          write(nerrout_coli,111)' D0m0_coli: p13 = ',p13
          write(nerrout_coli,111)' D0m0_coli:mm12 = ',mm12
          write(nerrout_coli,111)' D0m0_coli:mm22 = ',mm22
          write(nerrout_coli,111)' D0m0_coli: m32 = ',m32
          write(nerrout_coli,111)' D0m0_coli:mm42 = ',mm42
        endif
        D0m0_coli = undefined
        if (a.eq.cd0) return
      endif

      b   =  l13*mm22*(1d0/r24-r42) + l12*l34 -l14*l23
      c   =  l13*(l12-r24*l14) - mm12*l23 + mm12*r24*l34
      d   =  l23 - r24*l34
      det =  sqrt(
     &    l12*l12*l34*l34 + l14*l14*l23*l23 + l24*l24*l13*l13
     &    - 2d0*(l12*l23*l34*l14 + l12*l24*l34*l13 + l14*l24*l23*l13)
     &    + 4d0*(mm42*l12*l23*l13 + mm12*l23*l34*l24 + mm22*l14*l34*l13)
     &    - 4d0*(mm12*mm42*l23*l23 + mm12*mm22*l34*l34
     &    + mm22*mm42*l13*l13))

c added 27.07.2018
      if (det.eq.0d0) then
        mat(1,1) = 2*mm12
        mat(2,1) = l12
        mat(3,1) = l13
        mat(4,1) = l14
        mat(1,2) = l12
        mat(2,2) = 2*mm22
        mat(3,2) = l23
        mat(4,2) = l24
        mat(1,3) = l13
        mat(2,3) = l23
        mat(3,3) = 2*m32
        mat(4,3) = l34
        mat(4,4) = 2*mm42
        mat(1,4) = l14
        mat(2,4) = l24
        mat(3,4) = l34

        det = chdet(4,mat)
      end if

      x4(1) = (-b+det)/(2d0*a)
      x4(2) = (-b-det)/(2d0*a)
      if(abs(x4(1)).gt.abs(x4(2))) then
        x4(2) = c/(a*x4(1))
      else
        x4(1) = c/(a*x4(2))
      endif
      x1(1) = x4(1)/r24
      x1(2) = x4(2)/r24
      ix4(1) = -sign(1d0,real(d))
      ix4(2) = +sign(1d0,real(d))
      ix1(1) =  sign(1d0,ix4(1)*real(r24))
      ix1(2) =  sign(1d0,ix4(2)*real(r24))

      if (abs(l13+x4(1)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &    .or.abs(l13+x4(2)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &    .or.abs(l13+x1(1)*l23).lt.acc*abs(l13).and.l13.ne.0d0
     &    .or.abs(l13+x1(2)*l23).lt.acc*abs(l13).and.l13.ne.0d0) then

        swap=mm22
        mm22=mm42
        mm42=swap
        swap=l34
        l34=l23
        l23=swap
        swap=l14
        l14=l12
        l12=swap
        swap=r24
        r24=r42
        r42=swap
        swap=r14
        r14=r12
        r12=swap
        swap=r41
        r41=r21
        r21=swap
        a = mm42*(l34/r42 - l23)

        if(real(l12).lt.-0d0) then
          ir12 = sign(1d1,1d0-abs(r12*r12*mm22/mm12))
        else
          ir12 = 0d0
        endif
        if(real(l14).lt.-0d0) then
          ir14 = sign(1d1,1d0-abs(r14*r14*mm42/mm12))
        else
          ir14 = 0d0
        endif
        if(real(l24).lt.-0d0) then
          ir24 = sign(1d1,1d0-abs(r24*r24*mm42/mm22))
        else
          ir24 = 0d0
        endif

        b   =  l13*mm22*(1d0/r24-r42) + l12*l34 -l14*l23
        c   =  l13*(l12-r24*l14) - mm12*l23 + mm12*r24*l34
        d   =  l23 - r24*l34
        det =  sqrt(
     &      l12*l12*l34*l34 + l14*l14*l23*l23 + l24*l24*l13*l13
     &      - 2d0*(l12*l23*l34*l14 + l12*l24*l34*l13 + l14*l24*l23*l13)
     &      + 4d0*(mm42*l12*l23*l13 + mm12*l23*l34*l24
     &      + mm22*l14*l34*l13)
     &      - 4d0*(mm12*mm42*l23*l23 + mm12*mm22*l34*l34
     &      + mm22*mm42*l13*l13))

        x4(1) = (-b+det)/(2d0*a)
        x4(2) = (-b-det)/(2d0*a)
        if(abs(x4(1)).gt.abs(x4(2))) then
          x4(2) = c/(a*x4(1))
        else
          x4(1) = c/(a*x4(2))
        endif
        x1(1) = x4(1)/r24
        x1(2) = x4(2)/r24
        ix4(1) = -sign(1d0,real(d))
        ix4(2) = +sign(1d0,real(d))
        ix1(1) =  sign(1d0,ix4(1)*real(r24))
        ix1(2) =  sign(1d0,ix4(2)*real(r24))

      endif


      D0m0_coli = dcmplx(0d0)
      dO i=2,1,-1
        eta=eta2s_coli(-x4(i),1d0/r24,-ix4(i),-ir24,-ix1(i))

        if(eta.ne.0d0)then

          test0 = abs(d)/max(abs(l23),abs(l34*r24))
          ch1   = l12-r24*l14-mm22*(r42-1d0/r24)*x4(i)
          test1 = abs(ch1)/max(abs(l12),abs(mm22*r42*x4(i)),
     &        abs(mm22*r24*l14),abs(1/r24*x4(i)))
          ch2    = mm12/x1(i)+l12+mm22*x1(i)
          test01=min(test0,test1)
          test2 = abs(ch2)/max(abs(mm12/x1(i)),abs(l12),
     &        abs(mm22*x1(i)))
          ch3   = l23+l13/x1(i)
          test3 = abs(ch3)/max(abs(l23),abs(l13/x1(i)))
          test23 = min(test2,test3)
          ch4    = mm12/x4(i)+l14+mm42*x4(i)
          test4 = abs(ch4)/max(abs(mm12/x4(i)),abs(l14),
     &        abs(mm42*x4(i)))
          ch5   = l34+l13/x4(i)
          test5 = abs(ch5)/max(abs(l34),abs(l13/x4(i)))
          test45 = min(test4,test5)
          if (test23.gt.test01.and.test23.gt.test45) then
            argl1 = ch2/ch3
          elseif(test01.gt.test45) then
            argl1 = ch1/d
          else
            argl1 = ch4/ch5
          endif


          if(abs(aimag(argl1)).lt.1d1*impacc*abs(argl1)) then

            if(abs(aimag(x4(i))).gt.impacc*abs(x4(i)))then
              if(abs(aimag(r24)).gt.impacc*abs(r24))then
                v=aimag(x4(i))/aimag(r24)
                u=aimag(x4(i)/r24)/aimag(1d0/r24)
                iqbard(i) =
     &              real(mm12+l12*v+l14*u+mm22*v*v+mm42*u*u+l24*u*v
     &              -(l13+l23*v+l34*u))

              else
                iqbard(i) = real(-d)

              endif

            else                ! imaginary part results only from x4(i)
              iqbard(i) =real(-mm22*(r42-1d0/r24)*ix4(i)/d)
            endif
          else
              iqbard(i) = 0D0
          endif

          l1 = cln_coli(argl1,iqbard(i))

        else
          l1 = undefined

        endif


        if (l13.ne.0d0) then



          D0m0_coli = D0m0_coli + (2*i-3) * (
     &        cspcos_coli(-x4(i),r41,-ix4(i),ir14)
     &        + cspcos_coli(-x4(i),1d0/r14,-ix4(i),-ir14)
     &        - cspcos_coli(-x4(i),l34/l13,-ix4(i),real(l34-l13))
     &        - cspcos_coli(-x1(i),r21,-ix1(i),ir12)
     &        - cspcos_coli(-x1(i),1d0/r12,-ix1(i),-ir12)
     &        + cspcos_coli(-x1(i),l23/l13,-ix1(i),real(l23-l13))
     &        )

          if(eta.ne.cd0)then
            D0m0_coli = D0m0_coli + (2*i-3) * (
     &          - eta*(l1+cln_coli(l13/mm12,-1d0))
     &          )
          endif


        else


          D0m0_coli = D0m0_coli + (2*i-3) * (
     &        cspcos_coli(-x4(i),r41,-ix4(i),ir14)
     &        + cspcos_coli(-x4(i),1d0/r14,-ix4(i),-ir14)
     &        - cspcos_coli(-x1(i),r21,-ix1(i),ir12)
     &        - cspcos_coli(-x1(i),1d0/r12,-ix1(i),-ir12)
     &        + (cln_coli(-x4(i),-ix4(i)))**2/2d0
     &        - (cln_coli(-x1(i),-ix1(i)))**2/2d0
     &        + cln_coli(-x4(i),-ix4(i))*cln_coli(l34/mm12,-1d0)
     &        - cln_coli(-x1(i),-ix1(i))*cln_coli(l23/mm12,-1d0)
     &        )

           if(eta.ne.cd0)then
             D0m0_coli = D0m0_coli + (2*i-3) *
     &           (- eta* l1)
           endif


        endif
      enddo

      D0m0_coli = D0m0_coli/det




      end

************************************************************************
      function D02m0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for m22 = m32 = 0                          *
*  regular case based on                                               *
*        A.Denner, U.Nierste and R.Scharf, Nucl. Phys. B367 (1991) 637 *
*                                                                      *
*                     0d0                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*              m12| 1     3 | 0d0                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     m42                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  14.10.08 Ansgar Denner       last changed 07.04.10 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 l12,l13,l14,l23,l24,l34
      real*8     ir14
      real*8     ix(2),ix1(2)
      complex*16 r14,r41
      complex*16 a,b,c,d
      complex*16 x(2)
      complex*16 omxy3,omxy4
      complex*16 D02m0_coli
      complex*16 cspcon_coli,cspcos_coli,cln_coli
      integer    i
      logical errorwriteflag

#ifdef CHECK
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 elimminf2_coli
      complex*16 D02m0_check,eta,eta2s_coli,etalog
      real*8     ietalog
      logical flag(0:4)
      data    flag /5*.true./
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

        if(m22.ne.cd0.or.m32.ne.cd0.or.m12.eq.cd0.or.m42.eq.cd0
     &      .or.p12.eq.m12.and.p24.eq.m42
     &      .or.p13.eq.m12.and.p34.eq.m42
     &      .or.ps23.eq.cd0
     &      .or.ps12.eq.cd0.and.ms12.eq.cd0
     &      .or.ps13.eq.cd0.and.ms12.eq.cd0
     &      .or.ps24.eq.cd0.and.ms42.eq.cd0
     &      .or.ps34.eq.cd0.and.ms42.eq.cd0
     &      .or.ps14.eq.cd0.and.ms12.eq.cd0.and.ms42.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0
     &      ) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D02m0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)'D02m0_coli called improperly:'
            write(nerrout_coli,111)'D02m0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)'D02m0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)'D02m0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)'D02m0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)'D02m0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)'D02m0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)'D02m0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)'D02m0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)'D02m0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)'D02m0_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif



c     (permutation 1 <-> 2  for  m -> k )
      l12 = (m12    -p12)
      l13 = (m12    -p13)
      l14 = (m12+m42-p14)
      l23 = (       -p23)
      l24 = (    m42-p24)
      l34 = (    m42-p34)

      if(l14.ne.cd0)then
        r14 = l14/(2d0*m42)*(1d0+sqrt(1d0-4d0*m12*m42/l14**2))
      else
        r14 = dcmplx(0d0,1d0)*sqrt(m12/m42)
      endif
      r41 = r14*m42/m12
      a   =  l34*l24-l23*m42
      b   =  l13*l24+l12*l34-l14*l23
      c   =  l13*l12-l23*m12
      d   =  l23
      x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1)).gt.abs(x(2))) then
        x(2) = c/(a*x(1))
      else
        x(1) = c/(a*x(2))
      endif
      if(real(l14).lt.0d0) then
        ir14 = sign(1d1,1d0-abs(r14*r14*m42/m12))
      else
        ir14 = 0d0
      endif
      ix(1) = -sign(1d0,real(d))
      ix(2) = +sign(1d0,real(d))


      D02m0_coli = dcmplx(0d0)

      if(l13.ne.0d0.and.l12.ne.0d0) then



        do i=1,2
          omxy3 = 1d0+x(i)*l34/l13
          omxy4 = 1d0+x(i)*l24/l12
          if (abs(omxy3).lt.abs(omxy4).and.abs(omxy3).lt.1d0
     &        .and.abs(omxy4).gt.1d2*calacc) then
            omxy3 = (m12+l14*x(i)+m42*x(i)*x(i))*l23
     &          /(l12*omxy4*l13)
          else if (abs(omxy4).lt..1d0
     &          .and.abs(omxy3).gt.1d2*calacc) then
            omxy4 = (m12+l14*x(i)+m42*x(i)*x(i))*l23
     &          /(l12*omxy3*l13)
          endif


          D02m0_coli = D02m0_coli + (2*i-3) * (
     &        cspcos_coli(-x(i),r41,-ix(i),ir14)
     &        + cspcos_coli(-x(i),1d0/r14,-ix(i),-ir14)
     &        - cspcon_coli(-x(i),l34/l13,-x(i)*l34/l13,
     &        omxy3,-ix(i),real(l34-l13))
     &        - cspcon_coli(-x(i),l24/l12,-x(i)*l24/l12,
     &        omxy4,-ix(i),real(l24-l12))
     &        + cln_coli(-x(i),-ix(i))*
     &        (cln_coli(l12/m12,-1d0)
     &        +cln_coli(l13/l23,real(l13-l23))) )



        end do
      else if(l13.ne.0d0.and.l12.eq.0d0) then



        do i=1,2
          D02m0_coli = D02m0_coli + (2*i-3) * (
     &        cspcos_coli(-x(i),r41,-ix(i),ir14)
     &        + cspcos_coli(-x(i),1d0/r14,-ix(i),-ir14)
     &        - cspcos_coli(-x(i),l34/l13,-ix(i),real(l34-l13))
     &        + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))/2d0
     &        + cln_coli(l24/m12,-1d0)
     &        + cln_coli(l13/l23,real(l13-l23))))


        end do
      else if(l13.eq.0d0.and.l12.ne.0d0) then



        do i=1,2
          D02m0_coli = D02m0_coli + (2*i-3) * (
     &        cspcos_coli(-x(i),r41,-ix(i),ir14)
     &        + cspcos_coli(-x(i),1d0/r14,-ix(i),-ir14)
     &        - cspcos_coli(-x(i),l24/l12,-ix(i),real(l24-l12))
     &        + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))/2d0
     &        + cln_coli(l34/m12,-1d0)
     &        + cln_coli(l12/l23,real(l12-l23))))


        end do
      else           !  l13=0, l12=0


#ifdef NEWCHECK
        if (flag(4)) then
          call ErrOut_coli('D02m0_coli','new function called: (3.80)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D02m0_coli:   (3.80)',
     &          '    case with l13=0 and l12=0',
     &          '    not yet tested in physical process '
            write(nerrout_coli,111)' D02m0_coli: p12 = ',p12
            write(nerrout_coli,111)' D02m0_coli: p23 = ',p23
            write(nerrout_coli,111)' D02m0_coli: p34 = ',p34
            write(nerrout_coli,111)' D02m0_coli: p14 = ',p14
            write(nerrout_coli,111)' D02m0_coli: p13 = ',p13
            write(nerrout_coli,111)' D02m0_coli: p24 = ',p24
            write(nerrout_coli,111)' D02m0_coli: m12 = ',m12
            write(nerrout_coli,111)' D02m0_coli: m22 = ',m22
            write(nerrout_coli,111)' D02m0_coli: m32 = ',m32
            write(nerrout_coli,111)' D02m0_coli: m42 = ',m42
            flag(4)=.false.
          endif
        endif
#endif
        do i=1,2
          D02m0_coli = D02m0_coli + (2*i-3) * (
     &        cspcos_coli(-x(i),r41,-ix(i),ir14)
     &        + cspcos_coli(-x(i),1d0/r14,-ix(i),-ir14)
     &        + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))
     &        + cln_coli(l24/m12,-1d0)
     &        + cln_coli(l34/l23,real(l34-l23))))
        end do
      endif


      if (D02m0_coli.ne.cd0) D02m0_coli = D02m0_coli/(a*(x(1)-x(2)))



      end

************************************************************************
      function D03m0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for m22 = m32 = m42 = 0                    *
*  regular case                                                        *
*                                                                      *
*                     0d0                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*             m12 | 1     3 | 0d0                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     0d0                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  21.08.08 Ansgar Denner       last changed 04.09.08 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 l12,l13,l14,l23,l24,l34
      real*8     acc
      complex*16 a,b,c,d,e
      complex*16 x(2)
      real*8     ix(2)
      complex*16 D03m0_coli,cspcon_coli,cln_coli
      integer    i
      logical errorwriteflag

      data acc/1d-7/

#ifdef CHECK
      complex*16 D03m0_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical    flag(0:4)
      data       flag /5*.true./
      integer    j
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

        if(m22.ne.cd0.or.m32.ne.cd0.or.m42.ne.cd0.or.ms12.eq.cd0
     &      .or.ps23.eq.cd0.or.ps34.eq.cd0.or.ps24.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0
     &      ) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D03m0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D03m0_coli called improperly:'
            write(nerrout_coli,111)' D03m0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D03m0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D03m0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D03m0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D03m0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D03m0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D03m0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D03m0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D03m0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D03m0_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif


      l12 = (m12    -p12)
      l13 = (m12    -p13)
      l14 = (m12    -p14)
      l23 = (       -p23)
      l24 = (       -p24)
      l34 = (       -p34)

      a   =  l34*l24
      b   =  l13*l24+l12*l34-l14*l23
      c   =  l13*l12-l23*m12
      d   =  l23*m12
      x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1)).gt.abs(x(2))) then
        x(2) = c/(a*x(1))
      else
        x(1) = c/(a*x(2))
      endif

c----> added 23.05.03 to avoid log(0) in nearly singular cases
      if (abs(m12+x(1)*l14).lt.acc*abs(m12)
     &    .or.abs(m12+x(2)*l14).lt.acc*abs(m12)
     &    .or.abs(l13+x(1)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &    .or.abs(l13+x(2)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &    .or.abs(l12+x(1)*l24).lt.acc*abs(l12).and.l12.ne.0d0
     &    .or.abs(l12+x(2)*l24).lt.acc*abs(l12).and.l12.ne.0d0) then
        l14 = (m12    -p12)
        l13 = (m12    -p13)
        l12 = (m12    -p14)
        l34 = (       -p23)
        l24 = (       -p24)
        l23 = (       -p34)
        a   =  l34*l24
        b   =  l13*l24+l12*l34-l14*l23
        c   =  l13*l12-l23*m12
        d   =  l23*m12
        x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
        x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
        if(abs(x(1)).gt.abs(x(2))) then
          x(2) = c/a/x(1)
        else
          x(1) = c/a/x(2)
        endif
c permute arguments for possible singularities in continuation of
c dilogs
        if (abs(m12+x(1)*l14).lt.acc*abs(m12)
     &      .or.abs(m12+x(2)*l14).lt.acc*abs(m12)
     &      .or.abs(l13+x(1)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &      .or.abs(l13+x(2)*l34).lt.acc*abs(l13).and.l13.ne.0d0
     &      .or.abs(l12+x(1)*l24).lt.acc*abs(l12).and.l12.ne.0d0
     &      .or.abs(l12+x(2)*l24).lt.acc*abs(l12).and.l12.ne.0d0) then
          l12 = (m12    -p12)
          l14 = (m12    -p13)
          l13 = (m12    -p14)
          l24 = (       -p23)
          l23 = (       -p24)
          l34 = (       -p34)
          a   =  l34*l24
          b   =  l13*l24+l12*l34-l14*l23
          c   =  l13*l12-l23*m12
          d   =  l23*m12
          x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
          x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
          if(abs(x(1)).gt.abs(x(2))) then
            x(2) = c/a/x(1)
          else
            x(1) = c/a/x(2)
          endif
        endif
      endif

c must be fixed for correct continuation unless Im(1+rx) is kept fixed!
      ix(1) = -sign(1d0,real(d))
      ix(2) = +sign(1d0,real(d))


      D03m0_coli = dcmplx(0d0)
      if(l13.ne.0d0.and.l12.ne.0d0) then


        do i=1,2

        D03m0_coli = D03m0_coli + (2*i-3) * (
     &       cspcon_coli(-x(i),l14/m12,
     &        -x(i)*l14/m12,1d0+x(i)*l14/m12,-ix(i),-1d0)
     &     - cspcon_coli(-x(i),l34/l13,
     &        -x(i)*l34/l13,1d0+x(i)*l34/l13,-ix(i),real(l34-l13))
     &     - cspcon_coli(-x(i),l24/l12,
     &        -x(i)*l24/l12,1d0+x(i)*l24/l12,-ix(i),real(l24-l12))
     &     + cln_coli(-x(i),-ix(i))
     &        *(cln_coli(l12,-1d0)+cln_coli(l13,-1d0)
     &        -cln_coli(l23,-1d0)-log(m12) ) )


        end do
      else if(l13.eq.0d0.and.l12.ne.0d0) then


        do i=1,2
        D03m0_coli = D03m0_coli + (2*i-3) * (
     &       cspcon_coli(-x(i),l14/m12,
     &        -x(i)*l14/m12,1d0+x(i)*l14/m12,-ix(i),-1d0)
     &     - cspcon_coli(-x(i),l24/l12,
     &        -x(i)*l24/l12,1d0+x(i)*l24/l12,-ix(i),real(l24-l12))
     &     + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))/2d0
     &        +cln_coli(l12,-1d0)+cln_coli(l34,-1d0)
     &        -cln_coli(l23,-1d0)-log(m12))  )
       end do

      else if(l13.ne.0d0.and.l12.eq.0d0) then


        do i=1,2
        D03m0_coli = D03m0_coli + (2*i-3) * (
     &       cspcon_coli(-x(i),l14/m12,
     &        -x(i)*l14/m12,1d0+x(i)*l14/m12,-ix(i),-1d0)
     &     - cspcon_coli(-x(i),l34/l13,
     &        -x(i)*l34/l13,1d0+x(i)*l34/l13,-ix(i),real(l34-l13))
     &     + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))/2d0
     &         +cln_coli(l24,-1d0)+cln_coli(l13,-1d0)
     &        -cln_coli(l23,-1d0)-log(m12) )  )
        end do
      else if(l13.eq.0d0.and.l12.eq.0d0) then


        do i=1,2
        D03m0_coli = D03m0_coli + (2*i-3) * (
     &         cspcon_coli(-x(i),l14/m12,
     &        -x(i)*l14/m12,1d0+x(i)*l14/m12,-ix(i),-1d0)
     &         + cln_coli(-x(i),-ix(i))*(cln_coli(-x(i),-ix(i))
     &         +cln_coli(l24,-1d0)+cln_coli(l34,-1d0)
     &        -cln_coli(l23,-1d0)-log(m12)  )  )
        end do
      endif


      D03m0_coli = D03m0_coli/(a*(x(1)-x(2)))




      end


************************************************************************
      function D04m0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*  scalar 4-point function  for m12 = m22 = m32 = m42 = 0              *
*  regular case                                                        *
*                                                                      *
*                     0d0                                              *
*       p12  ---------------------  p23                                *
*                 |    2    |                                          *
*                 |         |                                          *
*             0d0 | 1     3 | 0d0                                      *
*                 |         |                                          *
*                 |    4    |                                          *
*       p14  ---------------------  p34                                *
*                     0d0                                              *
*                                                                      *
*----------------------------------------------------------------------*
*  20.01.95 Ansgar Denner       last changed 04.02.10 Ansgar Denner    *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 a,b,c,d,det
      complex*16 x(2),eta,etalog
      real*8     ix(2),ix1(2),ietalog
      complex*16 D04m0_coli,cspcos_coli,cln_coli,eta2s_coli
      integer    i
      logical errorwriteflag

#ifdef CHECK
      complex*16 D04m0_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      integer    j
      logical    flag(0:4)
      data       flag /5*.true./

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

        if(m12.ne.0d0.and.m22.ne.0d0.or.m32.ne.0d0.or.m42.ne.0d0
     &      .or.ps12.eq.cd0.or.ps23.eq.cd0.or.ps34.eq.cd0
     &      .or.ps14.eq.cd0.or.ps13.eq.cd0.or.ps24.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0
     &      ) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D04m0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' D04m0_coli called improperly:'
            write(nerrout_coli,111)' D04m0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D04m0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D04m0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D04m0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D04m0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D04m0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D04m0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D04m0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D04m0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D04m0_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      l12 = (       -p12)
      l13 = (       -p13)
      l14 = (       -p14)
      l23 = (       -p23)
      l24 = (       -p24)
      l34 = (       -p34)
      a   =  l34*l24
      b   =  l13*l24+l12*l34-l14*l23
      c   =  l13*l12
      det =  sqrt(b*b-4d0*a*c)
      x(1) = (-b+sqrt(b*b-4d0*a*c))/(2d0*a)
      x(2) = (-b-sqrt(b*b-4d0*a*c))/(2d0*a)
      if(abs(x(1)).gt.abs(x(2))) then
        x(2) = c/(a*x(1))
      else
        x(1) = c/(a*x(2))
      endif
      d = l23
      ix(1) = -real(d)
      ix(2) = +real(d)
      ix1(1)= real(l24)*ix(1)
      ix1(2)= real(l24)*ix(2)




      D04m0_coli = dcmplx(0d0)
      do i=1,2
        D04m0_coli = D04m0_coli + (2*i-3) * (
     &      - cln_coli(-x(i),-ix(i))**2/2d0
     &      - cspcos_coli(-x(i),l34/l13,-ix(i),real(l34-l13))
     &      - cspcos_coli(-x(i),l24/l12,-ix(i),real(l24-l12))
     &      + cln_coli(-x(i),-ix(i))
     &      *(cln_coli(l12,-1d0)+cln_coli(l13,-1d0)
     &      -cln_coli(l14,-1d0)-cln_coli(l23,-1d0))  )


      enddo


      D04m0_coli = D04m0_coli/det





      end


************************************************************************
      function D0ms0ir1m0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 0 mass singularities and up to 1 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     m12 small, p12=m22, p14=m42, m32=0                               *
*                                                                      *
*                        m22                                           *
*       p12=m22  ---------------------  p23                            *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | 0                                    *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  26.04.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms0ir1m0_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:6)
      data    flag /7*.true./
      save    flag
#endif

      real*8     ir24
      complex*16 m2,m4,k24,r24
      complex*16 y,logxs,logy
      complex*16 cln_coli,cspenc_coli

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"




 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      D0ms0ir1m0_coli = undefined

#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms32.ne.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &        .or.ps12.ne.ms22.or.ps14.ne.ms42
     &        .or.ps23.eq.ms22.and.ps34.eq.ms42
     &        .or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms0ir1m0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms0ir1m0_coli called improperly:'
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42,ms42
            write(nerrout_coli,111)' D0ms0ir1m0_coli: test = ',
     &          ms12.ne.cd0,ms32.ne.cd0,ms22.eq.cd0,ms42.eq.cd0
     &          ,ps12.ne.ms22,ps14.ne.ms42
     &          ,ps23.eq.ms22.and.ps34.eq.ms42
     &          ,ps13.eq.cd0
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p34).ne.0d0,aimag(p14).ne.0d0
     &          ,aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif


c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (ib)
c          and (ii)    (D0ir0)
c Denner Dittmaier (4.4/4.5)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box15

      m2 = sqrt(m22)
      m4 = sqrt(m42)
      k24 = (m22+m42-p24)/(m2*m4)
      if (k24.ne.0d0) then
c     |r24| > 0 yields wrong results !!!!!!!    (25.06.03)
c     analytic continuation requires |r24| < 0
        r24 = 2d0/(k24*(1d0+sqrt(dcmplx(1d0-4d0/k24**2))))
      else
        r24 = dcmplx(0d0,1d0)
      endif
        ir24 = sign(1d1,1d0-abs(r24))

      if(p24.ne.(m2-m4)**2) then


        logxs = cln_coli(r24,ir24)
        if(p23.ne.m22.and.p34.ne.m42)then
          y    = m2*(p34-m42)/(m4*(p23-m22))
          logy = log(m2/m4)+cln_coli(p34-m42,1d0)-cln_coli(p23-m22,1d0)


          D0ms0ir1m0_coli =  logxs*(
     &        2d0*cln_coli(1d0-r24*r24,-real(r24)*ir24)
     &        - logxs/2d0
     &        + cln_coli(p13/(p23-m22),real(p23-m22))
     &        + cln_coli(p13/(p34-m42),real(p34-m42)))
     &        + pi2_6 + logy*logy/2d0
     &        + cspenc_coli(r24*r24,real(r24)*ir24)
     &        - cspenc_coli(r24*y,ir24*real(y))
     &        - cln_coli(1d0-r24*y,-ir24*real(y))*(logxs+logy)
     &        - cspenc_coli(r24/y,ir24/real(y))
     &        - cln_coli(1d0-r24/y,-ir24/real(y))*(logxs-logy)


          if(m12.eq.cd0)then

#ifdef NEWCHECK
            if(flag(1))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &            'new function called: (4.4) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.4) m32=0',
     &              '    general case',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(1)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(muir2/(m2*m4))
#ifdef SING
     &          -logxs*delta1ir
#endif


          else

#ifdef NEWCHECK
            if(flag(2))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &            'new function called: (4.5) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.5) m32=0',
     &              '    general case',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(2)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(m12*coliminfscale2/(m2*m4))


          endif


          D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &         *r24/(m2*m4*p13*(1d0-r24*r24))

        else if (p23.eq.m22) then

          D0ms0ir1m0_coli = ( logxs*(
     &         2d0*cln_coli(1d0-r24*r24,-real(r24)*ir24)
     &         - logxs
     &         + 2d0*cln_coli(p13/(p34-m42),real(p34-m42)) )
     &         - pi2_6
     &         + cspenc_coli(r24*r24,real(r24)*ir24) )
          if(m12.eq.cd0)then

#ifdef NEWCHECK
            if(flag(3))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &                        'new function called: (4.4) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.4) m32=0',
     &              '    case with p23=m22',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(3)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(muir2/m42)
#ifdef SING
     &          -logxs*delta1ir
#endif
          else

#ifdef NEWCHECK
            if(flag(4))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &                        'new function called: (4.5) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.5) m32=0',
     &              '    case with p23=m22',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(4)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(m12*coliminfscale2/m42)
          endif

          D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &         *r24/(m2*m4*p13*(1d0-r24*r24))

        else if (p34.eq.m42) then

          D0ms0ir1m0_coli = ( logxs*(
     &         2d0*cln_coli(1-r24*r24,-real(r24)*ir24)
     &         - logxs
     &         + 2d0*cln_coli(p13/(p23-m22),real(p23-m22)) )
     &         - pi2_6
     &         + cspenc_coli(r24*r24,real(r24)*ir24) )

          if(m12.eq.cd0)then

#ifdef NEWCHECK
            if(flag(5))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &                        'new function called : (4.4) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.4) m32=0',
     &              '    case with p34=m42',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(5)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(muir2/m22)
#ifdef SING
     &          -logxs*delta1ir
#endif
          else

#ifdef NEWCHECK
            if(flag(6))then
              call ErrOut_coli('D0ms0ir1m0_coli',
     &                        'new function called: (4.5) m32=0',
     &        errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms0ir1m0_coli: (4.5) m32=0',
     &              '    case with p34=m42',
     &              '    not yet tested in physical process '
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
                flag(6)=.false.
              endif
            endif
#endif

            D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &          -logxs*log(m12*coliminfscale2/m22)
          endif

          D0ms0ir1m0_coli = D0ms0ir1m0_coli
     &         *r24/(m2*m4*p13*(1d0-r24*r24))

        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms20ir1_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms0ir1m0_coli called improperly'
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
          endif
        endif
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms0ir1m0_coli','case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &          ' D0ms0ir1m0_coli: case not implemented'
          if (p24.eq.(m2-m4)**2) write(nerrout_coli,100)
     &          ' p24 = (m2-m4)^2 '
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms0ir1m0_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms0ir1m0_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms0ir1m0_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms0ir1m0_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms0ir1m0_coli: m42 = ',m42
        endif
      endif

      end

************************************************************************
      function D0ms0ir1_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 0 mass singularities and up to 1 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     m12 small, p12=m22, p14=m42, m32=/=0                             *
*                                                                      *
*                        m22                                           *
*       p12=m22  ---------------------  p23                            *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32                                  *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  21.12.18             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms0ir1_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 D0ms0ir1_check
      logical flag(0:2),errflag
      data    flag /3*.true./ , errflag /.true./
      save    flag,errflag
#endif

      complex*16 m1,m2,m3,m4
      complex*16 k24,k23,k34,r24,r23,r34
      real*8     ir24,ir23,ir34,ip1,ip2,ip3,ip4
      complex*16 logxs,logx2,logx3
      complex*16 cln_coli,cspenc_coli

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms32.eq.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &        .or.ps12.ne.ms22.or.ps14.ne.ms42
     &        .or.ps13.eq.ms32
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms0ir1_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms0ir1_coli called improperly:'
            write(nerrout_coli,111)' D0ms0ir1_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms0ir1_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms0ir1_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms0ir1_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms0ir1_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms0ir1_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms0ir1_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms0ir1_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms0ir1_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms0ir1_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      if (abs(m32-p13).lt.abs(m32)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms0ir1_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms0ir1_coli: p13 = m32'
            write(nerrout_coli,111)' D0ms0ir1_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms0ir1_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms0ir1_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms0ir1_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms0ir1_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms0ir1_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms0ir1_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms0ir1_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms0ir1_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms0ir1_coli: m42 = ',m42
          endif
        D0ms0ir1_coli = undefined
        if (abs(m32-p13).eq.0d0) return
      endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (ib)
c          and (i)    (D0irr)
c Denner Dittmaier (4.4/4.5)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box16

      m1 = sqrt(m12)
      m2 = sqrt(m22)
      m3 = sqrt(m32)
      m4 = sqrt(m42)
      k24 = (m22+m42-p24)/(m2*m4)
      if (k24.ne.0d0) then
c     |r24| > 0 or Im(r24)<0 yields wrong results !!!!!!!
c     analytic continuation requires |r24| < 1 and Im r24 > 0 !!!!
c     standard calculation of r24 yield wrong sign of Im r24 or |r24|=1!!
c        r24 = 2d0/(k24*(1d0+sqrt(dcmplx(1d0-4d0/k24**2))))
c     use K-function of IR paper
        r24 = sqrt(1d0-4d0*m2*m4/(p24-(m4-m2)**2))
        r24= - 4d0*m2*m4/(p24-(m4-m2)**2)/(1+r24)**2
      else
        r24 = dcmplx(0d0,1d0)
      endif
      if(real(k24).lt.-2d0) then
        ir24 = sign(1d1,1d0-abs(r24))
      else
        ir24 = 0d0
      endif

      if (abs(r24*r24-1d0).lt.calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms0ir1_coli',' s24 on threshold'//
     &      ' case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms0ir1_coli: r24 = 1d0'
          write(nerrout_coli,111)' D0ms0ir1_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms0ir1_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms0ir1_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms0ir1_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms0ir1_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms0ir1_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms0ir1_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms0ir1_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms0ir1_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms0ir1_coli: m42 = ',m42
        endif
        D0ms0ir1_coli = undefined
        if (abs(r24*r24-1d0).eq.0d0) return
      endif

      k23 = (m22+m32-p23)/(m2*m3)
      k34 = (m32+m42-p34)/(m3*m4)
      if (k23.ne.0d0) then
        r23 = 2d0/(k23*(1d0+sqrt(dcmplx(1d0-4d0/k23**2))))
      else
        r23 = dcmplx(0d0,1d0)
      endif
      if (real(k34).ne.0d0) then
        r34 = 2d0/(k34*(1d0+sqrt(dcmplx(1d0-4d0/k34**2))))
      else
        r34 = dcmplx(0d0,1d0)
      endif
      if(real(k23).lt.-2d0) then
        ir23= sign(1d1,1d0-abs(r23))
      else
        ir23= 0d0
      endif
      if(real(k34).lt.-2d0) then
        ir34= sign(1d1,1d0-abs(r34))
      else
        ir34= 0d0
      endif
      if(p24.ne.(m2-m4)**2) then

c       imaginary parts are not arbitrary if theta terms omitted!
c       corrected 21.12.2018
        ip1 =  sign(1d1,real(r24*r34*r23))     !  irrelevant	
        ip2 = -sign(1d1,real(r24/r34*r23))     !  relevant
        ip3 = -sign(1d1,real(r24*r34/r23))     !  relevant
        ip4 =  sign(1d1,real(r24/(r34*r23)))   !  relevant

        logxs = cln_coli(r24,ir24)
        logx2 = cln_coli(r23,ir23)
        logx3 = cln_coli(r34,ir34)
        D0ms0ir1_coli =
     &      2d0*logxs*cln_coli(1d0-r24*r24,-real(r24)*ir24)
     &      + 3d0*pi2_6
     &      + cspenc_coli(r24*r24,real(r24)*ir24)
     &      + (logx2)**2 + (logx3)**2
     &      - cspenc_coli(r24*r34*r23,ip1)
     &      - cln_coli(1d0-r24*r34*r23,-ip1)*(logxs+logx2+logx3)
     &      - cspenc_coli(r24/r34*r23,ip2)
     &      - cln_coli(1d0-r24/r34*r23,-ip2)*(logxs+logx2-logx3)
     &      - cspenc_coli(r24*r34/r23,ip3)
     &      - cln_coli(1d0-r24*r34/r23,-ip3)*(logxs-logx2+logx3)
     &      - cspenc_coli(r24/(r34*r23),ip4)
     &      - cln_coli(1d0-r24/(r34*r23),-ip4)*(logxs-logx2-logx3)


c added 21.12.2018  theta function terms for arbitrary ipi
c       if(abs(r24*r34*r23).gt.1d0) then
c         D0ms0ir1_coli = D0ms0ir1_coli -
c    &       (cln_coli(r24*r34*r23,ip1) - (logxs+logx2+logx3))*
c    &       (cln_coli(-r24*r34*r23,-ip1)
c    &        -(cln_coli(r24*r34*r23,ip1)+(logxs+logx2+logx3))/2d0)
c       end if
c
c       if(abs(r24/r34*r23).gt.1d0) then
c         D0ms0ir1_coli = D0ms0ir1_coli -
c    &       (cln_coli(r24/r34*r23,ip2) - (logxs+logx2-logx3))*
c    &       (cln_coli(-r24/r34*r23,-ip2)
c    &        -(cln_coli(r24/r34*r23,ip2)+(logxs+logx2-logx3))/2d0)
c       end if
c
c       if(abs(r24*r34/r23).gt.1d0) then
c         D0ms0ir1_coli = D0ms0ir1_coli -
c    &       (cln_coli(r24*r34/r23,ip3) - (logxs-logx2+logx3))*
c    &       (cln_coli(-r24*r34/r23,-ip3)
c    &        -(cln_coli(r24*r34/r23,ip3)+(logxs-logx2+logx3))/2d0)
c       end if
c
c       if(abs(r24/(r34*r23)).gt.1d0) then
c         D0ms0ir1_coli = D0ms0ir1_coli -
c    &       (cln_coli(r24/(r34*r23),ip4) - (logxs-logx2-logx3))*
c    &       (cln_coli(-r24/(r34*r23),-ip4)
c    &        -(cln_coli(r24/(r34*r23),ip4)+(logxs-logx2-logx3))/2d0)
c       end if
c end added 21.12.2018




        if(m12.eq.cd0)then


          D0ms0ir1_coli =   D0ms0ir1_coli +
     &        2d0*logxs*cln_coli((m32-p13)/(m3*sqrt(muir2)),-1d0)
#ifdef SING
     &        -logxs*delta1ir
#endif


        else

#ifdef NEWCHECK
         if(flag(2))then
            call ErrOut_coli('D0ms0ir1_coli',
     &          'new function called: (4.5)',errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir1_coli: (4.5)',
     &            '    not yet tested in physical process '
              write(nerrout_coli,111)' D0ms0ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir1_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

          D0ms0ir1_coli =   D0ms0ir1_coli +
     &        2d0*logxs*cln_coli((m32-p13)/(m3*m1*coliminfscale),-1d0)
        endif

        D0ms0ir1_coli = D0ms0ir1_coli
     &       *r24/(m2*m4*(p13-m32)*(1d0-r24*r24))

      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms0ir1_coli','case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms0ir1_coli: case not implemented'
          if (p24.eq.(m2-m4)**2)
     &         write(nerrout_coli,100)' p24 = (m2-m4)^2 '
          if (m32.eq.0) write(nerrout_coli,100)' m32 = 0'
          write(nerrout_coli,111)' D0ms0ir1: p12 = ',p12
          write(nerrout_coli,111)' D0ms0ir1: p23 = ',p23
          write(nerrout_coli,111)' D0ms0ir1: p34 = ',p34
          write(nerrout_coli,111)' D0ms0ir1: p14 = ',p14
          write(nerrout_coli,111)' D0ms0ir1: p24 = ',p24
          write(nerrout_coli,111)' D0ms0ir1: p13 = ',p13
          write(nerrout_coli,111)' D0ms0ir1: m12 = ',m12
          write(nerrout_coli,111)' D0ms0ir1: m22 = ',m22
          write(nerrout_coli,111)' D0ms0ir1: m32 = ',m32
          write(nerrout_coli,111)' D0ms0ir1: m42 = ',m42
        endif
      endif


      end

************************************************************************
      function D0ms0ir2_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 0 mass singularities and up to 2 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     m12 small, p12=m22, p14=m42, m32=0                               *
*                                                                      *
*                        m22                                           *
*       p12=m22  ---------------------  p23=m22                        *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34=m42                        *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  10.10.08             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms0ir2_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:8)
      data    flag /9*.true./
      save    flag
#endif

      real*8     ir24
      complex*16 ma2(4)
      complex*16 m2,m4,k24,r24
      complex*16 cln_coli
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms32.ne.cd0.or.ms22.eq.cd0.or.ms42.eq.cd0
     &        .or.ps12.ne.ms22.or.ps14.ne.ms42
     &        .or.ps23.ne.ms22.or.ps34.ne.ms42
     &        .or.ps24.eq.cd0.or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms0ir2_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms0ir2_coli called improperly:'
            write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      ma2(1)=m12
      ma2(2)=m22
      ma2(3)=m32
      ma2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=ma2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (ib)
c          and (iii)    (D0ir)
c Denner Dittmaier (4.9),(4.10),(4.11)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box14

      m2 = sqrt(m22)
      m4 = sqrt(m42)
      k24 = (m22+m42-p24)/(m2*m4)
      if (k24.ne.0d0) then
        r24 = 2d0/(k24*(1d0+sqrt(dcmplx(1d0-4d0/k24**2))))
      else
        r24 = dcmplx(0d0,1d0)
      endif
      if(real(k24).lt.-2d0) then
        ir24 = sign(1d1,1d0-abs(r24))
      else
        ir24 = 0d0
      endif

      if (abs(r24*r24-1d0).lt.calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms0ir2_coli',' s24 on threshold'//
     &      ' case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms0ir2_coli: r24 = 1d0'
          write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
        endif
        D0ms0ir2_coli = undefined
        if (abs(r24*r24-1d0).eq.0d0) return
      endif

      if(p24.ne.(m2-m4)**2) then
        if(m12.eq.cd0.and.m32.eq.cd0) then

#ifdef CHECK
          if(m32.ne.cd0.or.m12.ne.0d0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms0ir2_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.9)',
     &            '    case with 2 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(1))then
            call ErrOut_coli('D0ms0ir2_coli',
     &                      'new function called: (4.9)',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.9)',
     &            '    case with 2 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
              flag(1)=.false.
            endif
          endif
#endif

          D0ms0ir2_coli = 2d0*cln_coli(r24,ir24)
     &      *cln_coli(-p13/muir2,-1d0)
#ifdef SING
     &        -delta1ir*2d0*cln_coli(r24,ir24)
#endif
          D0ms0ir2_coli = D0ms0ir2_coli
     &        *r24/(m2*m4*p13*(1d0-r24*r24))


        elseif(m12.eq.cd0.or.m32.eq.cd0) then
#ifdef CHECK
          if(m32.eq.cd0.and.m12.eq.0d0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms0ir2_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.10)',
     &            '    case with 1 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms0ir2_coli',
     &                      'new function called:  (4.10)',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.10)',
     &            '    case with 1 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

          D0ms0ir2_coli = cln_coli(r24,ir24)*
     &        (cln_coli(-p13/muir2,-1d0)
     &        +cln_coli(-p13/((m12+m32)*coliminfscale2),-1d0))
     &        *r24/(m2*m4*p13*(1d0-r24*r24))
#ifdef SING
     &        -delta1ir*
     &        cln_coli(r24,ir24)*r24/(m2*m4*p13*(1d0-r24*r24))
#endif
        else
#ifdef CHECK
          if(m32.eq.cd0.or.m12.eq.cd0)then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms0ir2_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.11)',
     &            '    case with 0 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms0ir2_coli',
     &                      'new function called:  (4.11)',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms0ir2_coli:  (4.11)',
     &            '    case with 0 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif

          D0ms0ir2_coli = cln_coli(r24,ir24)*
     &        (cln_coli(-p13/(m12*coliminfscale2),-1d0)
     &        +cln_coli(-p13/(m32*coliminfscale2),-1d0))
     &        *r24/(m2*m4*p13*(1d0-r24*r24))

        endif
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms0ir2_coli','case not implemented',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms0ir2_coli:',
     &        '    not implemented for det=0 '
          write(nerrout_coli,111)' D0ms0ir2_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms0ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms0ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms0ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms0ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms0ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms0ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms0ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms0ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms0ir2_coli: m42 = ',m42
        endif
      endif

      end

************************************************************************
      function D0ms1ir0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 1 mass singularities and up to 0 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p34, m32, m42 small                                              *
*     p14=/=m12, p23=/=m22                                             *
*                                                                      *
*                        m22                                           *
*          p12  ---------------------  p23=/=m22                       *
*                     |    2    |                                      *
*                     |         |                                      *
*                 m12 | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*     p14=/=m12  ---------------------  p34 small                      *
*                      m42 small                                       *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  14.05.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms1ir0_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli,D0ms1ir0_check
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:9),flag3,flag2
      data    flag /10*.true./, flag3 /.true./,  flag2 /.true./
      save    flag
#endif

      complex*16 cln_coli,cspenc_coli,cspcos_coli,eta2s_coli
      complex*16 det,n12
      complex*16 m1,m2,m3,m4
      complex*16 mm12,mm22,mm32,mm42,q12,q13,q14,q23,q24,q34
      complex*16 h12,h23,h34,h14
      complex*16 k12,k13,k14,k23,k24,k34
      complex*16 l12,l13,l14,l23,l24,l34
      complex*16 r12,r13,r14,r23,r24,r34,r21,r31,r41,r32,r42,r43
      complex*16 r12inv
      complex*16 a,b,c,d
      complex*16 x(2),y(2),z(2)
      real*8     rd
      real*8     ir12,ir14,ir23,ir34,ir13,ir24
      real*8     iy(2),iz(2),ix(2),iqbard(2)
      real*8     rt12,rt34,rt14,rt23
      integer    i,j

      real*8     v,u,ietalog
      complex*16 mm3,mm2
      complex*16 eta,etalog
      complex*16 l1(2),sp3,sp4

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms32.ne.cd0.or.ms42.ne.cd0.or.ps34.ne.cd0
     &      .or.ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0
     &      .or.ps14.eq.ms12.or.ps23.eq.ms22
     &      .or.ps13.eq.ms12.or.ps24.eq.ms22
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms1ir0_coli called improperly:'
            write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42,ms42
            write(nerrout_coli,*)' D0ms1ir0_coli: test= ',
     &          ms32.ne.cd0,ms42.ne.cd0,ps34.ne.cd0,
     &          ps12.eq.cd0.and.ms12.eq.cd0.and.ms22.eq.cd0,
     &          ps14.eq.ms12,ps23.eq.ms22,
     &          ps13.eq.ms12,ps24.eq.ms22,
     &          aimag(p12).ne.0d0,aimag(p23).ne.0d0,
     &          aimag(p34).ne.0d0,aimag(p14).ne.0d0,
     &          aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif

      l13=m12-p13
      l14=m12-p14
      l23=m22-p23
      l24=m22-p24

      if (abs(l13*l24-l14*l23).lt.abs(l13*l24)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms1ir0_coli',' singular denominator'//
     &      ' use appropriate regularization',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms1ir0_coli: l13*l24-l14*l23 = 0'
          write(nerrout_coli,111)' D0ms1ir0_coli: l13 = ',l13
          write(nerrout_coli,111)' D0ms1ir0_coli: l24 = ',l24
          write(nerrout_coli,111)' D0ms1ir0_coli: l14 = ',l14
          write(nerrout_coli,111)' D0ms1ir0_coli: l23 = ',l23
          write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
        endif
        D0ms1ir0_coli = undefined
        if (abs(l13*l24-l14*l23).eq.0d0) return
      endif


      if(m32.eq.cd0.and.m42.eq.cd0)then
* 0 soft singularities and 2 zero masses
*
*                  m22
*        p12 ----------------  p23
*                |  2   |
*            m12 |1    3| 0
*                |   4  |
*        p14 ----------------  0
*                   0
*

#ifdef CHECK
        if(m32.ne.cd0.or.m42.ne.0d0.or.p34.ne.0d0.or.
     &      p14.eq.m12.or.p23.eq.m22)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms1ir0_coli:  (4.13)',
     &          '    case with 2 zero small masses:',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
          endif
        endif
#endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box13
c                  covers box5 for m32,m42->0
c p2^2 -> p14,  p3^2 -> p12,  p4^2 -> p23,  s12 -> p143, s23 -> p24
c m3^2 -> m12, m4_^2 -> m22
c Denner Dittmaier (4.13)

        if(m12.ne.cd0.or.m22.ne.cd0) then

#ifdef CHECK
          if(m32.ne.m42.or.p34.ne.0d0.or.m12+m22.eq.cd0
     &        .or.p14.eq.m12.or.p23.eq.m22)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0ms1ir0_coli:  (4.13)  m12+m22 =/=0',
     &            '    case with 2 zero small masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
            endif
          endif
#endif

          if(m12.ne.cd0)then
            mm42=m42
            mm12=m12
            mm22=m22
            q13=p13
            q23=p23
            q14=p14
            q24=p24
          else
            mm42=m42
            mm12=m22
            mm22=m12
            q13=p23
            q23=p13
            q14=p24
            q24=p14
          endif


          l12=mm12+mm22-p12
          l13=mm12-q13
          l14=mm12-q14
          l23=mm22-q23
          l24=mm22-q24

          if(l12.ne.cd0)then
            r21 = l12/(2d0*mm12)*(1d0+sqrt(1d0-4d0*mm12*mm22/l12**2))
            r12inv= mm22/(r21*mm12)
            if(mm22.ne.cd0)then
              ir12 = sign(1d0,1d0-abs(r21/r12inv))
            else
              ir12 = -1d0
            endif
          else
            r21 = dcmplx(0d0,1d0)*sqrt(mm22/mm12)
            r12inv= -r21
          endif


          D0ms1ir0_coli=cspcos_coli(l14/l24,r21,real(l14-l24),ir12)
     &        +cspcos_coli(l14/l24,r12inv,real(l14-l24),-ir12)
     &        -cspcos_coli(l13/l23,r21,real(l13-l23),ir12)
     &        -cspcos_coli(l13/l23,r12inv,real(l13-l23),-ir12)
     &        +2d0*cspcos_coli(l13/l23,l24/l14,real(l13-l23),
     &        -real(l14-l24))
     &        -2d0*cln_coli(l14/sqrt(mm12*muir2),-1d0)
     &        *(cln_coli(l13/l23,real(l13-l23))
     &        -cln_coli(l14/l24,real(l14-l24)))
     &        +2d0*cspenc_coli(1d0-l14/l13,-real(l14-l13))
     &        -2d0*cspenc_coli(1d0-l24/l23,-real(l24-l23))
#ifdef SING
     &        +delta1ir
     &        *(cln_coli(l13/l23,real(l13-l23))
     &        -cln_coli(l14/l24,real(l14-l24)))
#endif



          D0ms1ir0_coli=D0ms1ir0_coli/(l23*l14-l24*l13)



        else                    ! if(m12.eq.cd0.and.m22.eq.cd0)then


#ifdef CHECK
          if(m32.ne.m42.or.p34.ne.0d0.or.m12.ne.cd0.or.m22.ne.cd0
     &        .or.p14.eq.m12.or.p23.eq.m22)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0ms1ir0_coli:  (4.13)  m12=0, m22=0',
     &            '    case with 2 zero small masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
            endif
          endif
#endif


         D0ms1ir0_coli =
     &        2d0*cspenc_coli(1d0-p13/p14,real(p13-p14))
     &        -2d0*cspenc_coli(1d0-p23/p24,real(p23-p24))
     &        +2d0*cspcos_coli(p23/p24,p14/p13,real(p24-p23),
     &        real(p13-p14))
     &        +.5d0*(cln_coli(p23/p24,real(p24-p23))
     &              +cln_coli(p14/p13,real(p13-p14)))**2
     &        -(cln_coli(p23/p24,real(p24-p23))
     &          +cln_coli(p14/p13,real(p13-p14)))*
     &        (cln_coli(-p23/(muir2),-1d0)
     &          +cln_coli(p13/p12,real(p12-p13)))
#ifdef SING
     &        +delta1ir
     &        *(cln_coli(p23/p24,real(p24-p23))
     &          +cln_coli(p14/p13,real(p13-p14)))
#endif

         D0ms1ir0_coli = D0ms1ir0_coli/(p13*p24-p23*p14)

       endif



      elseif(m32.eq.cd0.or.m42.eq.cd0)then
* 0 soft singularities and 1 zero masses
*
*                  m22
*        p12 ----------------  p23
*                |  2   |
*            m12 |1    3| 0
*                |   4  |
*        p14 ----------------  p34=m42  small
*                  m42
*

#ifdef CHECK
        if(.not.(m32.eq.cd0.and.m42.eq.p34.or.
     &      m42.eq.cd0.or.m32.eq.p34).or.
     &      p14.eq.m12.or.p23.eq.m22)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms1ir0_coli:  (4.14)',
     &          '    case with 1 zero masses:',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
          endif
        endif
#endif

        if(m12.ne.cd0.or.m22.ne.cd0) then

          if(m32.eq.cd0)then
            if(m12.ne.cd0)then
              mm42=m42
              mm12=m12
              mm22=m22
              q13=p13
              q23=p23
              q14=p14
              q24=p24
            else
              mm42=m42
              mm12=m22
              mm22=m12
              q13=p23
              q23=p13
              q14=p24
              q24=p14
            endif
          else
            if(m22.ne.cd0)then
              mm42=m32
              mm12=m22
              mm22=m12
              q13=p24
              q23=p14
              q14=p23
              q24=p13
            else
              mm42=m32
              mm12=m12
              mm22=m22
              q13=p14
              q23=p24
              q14=p13
              q24=p23
            endif
          endif

c cD0ms0e
c Denner Dittmaier (4.14)         23.10.09


          l12=mm12+mm22-p12
          l13=mm12-q13
          l14=mm12-q14
          l23=mm22-q23
          l24=mm22-q24

          if(l12.ne.cd0)then
            r21 = l12/(2d0*mm12)*(1d0+sqrt(1d0-4d0*mm12*mm22/l12**2))
            r12inv= mm22/(r21*mm12)
            if(mm22.ne.cd0)then
              ir12 = sign(1d0,1d0-abs(r21/r12inv))
            else
              ir12 = -1d0
            endif
           else
            r21 = dcmplx(0d0,1d0)*sqrt(mm22/mm12)
            r12inv= -r21
          endif


          D0ms1ir0_coli=cspcos_coli(l14/l24,r21,real(l14-l24),ir12)
     &        +cspcos_coli(l14/l24,r12inv,real(l14-l24),-ir12)
     &        -cspcos_coli(l13/l23,r21,real(l13-l23),ir12)
     &        -cspcos_coli(l13/l23,r12inv,real(l13-l23),-ir12)
     &        +2d0*cspcos_coli(l13/l23,l24/l14,real(l13-l23),
     &        -real(l14-l24))
     &        -2d0*cln_coli(l14/sqrt(mm12*mm42*coliminfscale2),-1d0)
     &        *(cln_coli(l13/l23,real(l13-l23))
     &        -cln_coli(l14/l24,real(l14-l24)))


          D0ms1ir0_coli=D0ms1ir0_coli/(l23*l14-l24*l13)

        else                    ! m12=0=m22

#ifdef NEWCHECK
          if(flag(5))then
            call ErrOut_coli('D0ms1ir0_coli',
     &                      'new function called:  (4.14) m12=m22=0',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0ms1ir0_coli:  (4.14) m12=m22=0',
     &            '    case with 1 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
              flag(5)=.false.
            endif
          endif
#endif

          if(m42.ne.cd0)then
            mm42=m42
            mm12=m12
            mm22=m22
            q13=p13
            q23=p23
            q14=p14
            q24=p24
          else
            mm42=m32
            mm12=m22
            mm22=m12
            q13=p24
            q23=p14
            q14=p23
            q24=p13
          endif

          l12=-p12
          l13=-q13
          l14=-q14
          l23=-q23
          l24=-q24

          D0ms1ir0_coli=
     &        +2d0*cspcos_coli(l13/l23,l24/l14,real(l13-l23),
     &        -real(l14-l24))
     &        -0.5d0*(cln_coli(l24/l14,real(l24-l14)))**2
     &        +0.5d0*(cln_coli(l13/l23,real(l13-l23)))**2
     &        -(cln_coli(l14/(mm42*coliminfscale2),-1d0)
     &        +cln_coli(l14/l12,real(l14-l12)))
     &        *(cln_coli(l13/l23,real(l13-l23))
     &        +cln_coli(l24/l14,real(l24-l14)))


          D0ms1ir0_coli=D0ms1ir0_coli/(l23*l14-l24*l13)


        endif

      else

c cD0mse
c Denner Dittmaier (4.15)
* 0 soft singularities and 0 zero masses
*
*                  m22
*        p12 ----------------  p23
*                |  2   |
*            m12 |1    3| m32=m42
*                |   4  |
*        p14 ----------------  0
*                  m42
*

        if(m12.ne.cd0.or.m22.ne.cd0) then

#ifdef CHECK
              if(m32.ne.m42.or.p34.ne.0d0.or.m12+m22.eq.cd0
     &            .or.p14.eq.m12.or.p23.eq.m22)
     &            then
                call setErrFlag_coli(-10)
                call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &              errorwriteflag)
                if (errorwriteflag) then
                  write(nerrout_coli,100)
     &                ' D0ms1ir0_coli:  (4.15)  m12+m22 =/=0',
     &                '    case with at most 1 zero masses:',
     &                '    wrong arguments'
                  write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
                  write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
                  write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
                  write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
                  write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
                  write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
                  write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
                  write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
                  write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
                  write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
                endif
              endif
#endif

          if(m32.eq.cd0)then
            if(m12.ne.cd0)then
              mm42=m42
              mm12=m12
              mm22=m22
              q13=p13
              q23=p23
              q14=p14
              q24=p24
            else
              mm42=m42
              mm12=m22
              mm22=m12
              q13=p23
              q23=p13
              q14=p24
              q24=p14
            endif
          else
            if(m22.ne.cd0)then
              mm42=m32
              mm12=m22
              mm22=m12
              q13=p24
              q23=p14
              q14=p23
              q24=p13
            else
              mm42=m32
              mm12=m12
              mm22=m22
              q13=p14
              q23=p24
              q14=p13
              q24=p23
            endif
          endif

c cD0mse
c Denner Dittmaier (4.15)         23.10.09

#ifdef NEWCHECK
          if(flag(6))then
            call ErrOut_coli('D0ms1ir0_coli',
     &                      'new function called:  (4.15) m12+m22=/=0',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0ms1ir0_coli:  (4.15) m12+m22=/=0',
     &            '    case with at most 1 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
              write(nerrout_coli,111)' D0ms1ir0_coli: q23 = ',q23
              write(nerrout_coli,111)' D0ms1ir0_coli: q14 = ',q14
              write(nerrout_coli,111)' D0ms1ir0_coli: q24 = ',q24
              write(nerrout_coli,111)' D0ms1ir0_coli: q13 = ',q13
              write(nerrout_coli,111)' D0ms1ir0_coli: mm12= ',mm12
              write(nerrout_coli,111)' D0ms1ir0_coli: mm22= ',mm22
              write(nerrout_coli,111)' D0ms1ir0_coli: mm42= ',mm42
              flag(6)=.false.
            endif
          endif
#endif

          l12=mm12+mm22-p12
          l13=mm12-q13
          l14=mm12-q14
          l23=mm22-q23
          l24=mm22-q24

          if(l12.ne.cd0)then
            r21 = l12/(2d0*mm12)*(1d0+sqrt(1d0-4d0*mm12*mm22/l12**2))
            r12inv= mm22/(r21*mm12)
            if(mm22.ne.cd0)then
              ir12 = sign(1d0,1d0-abs(r21/r12inv))
            else
              ir12 = -1d0
            endif
          else
            r21 = dcmplx(0d0,1d0)*sqrt(mm22/mm12)
            r12inv= -r21
          endif


          D0ms1ir0_coli=cspcos_coli(l14/l24,r21,real(l14-l24),ir12)
     &        +cspcos_coli(l14/l24,r12inv,real(l14-l24),-ir12)
     &        -cspcos_coli(l13/l23,r21,real(l13-l23),ir12)
     &        -cspcos_coli(l13/l23,r12inv,real(l13-l23),-ir12)
     &        +2d0*cspcos_coli(l13/l23,l24/l14,real(l13-l23),
     &        -real(l14-l24))
     &        -2d0*cln_coli(l14/sqrt(mm12*mm42*coliminfscale2),-1d0)
     &        *(cln_coli(l13/l23,real(l13-l23))
     &        -cln_coli(l14/l24,real(l14-l24)))
     &        +2d0*cspenc_coli(1d0-l14/l13,-real(l14-l13))
     &        -2d0*cspenc_coli(1d0-l24/l23,-real(l24-l23))



          D0ms1ir0_coli=D0ms1ir0_coli/(l23*l14-l24*l13)




        else                    ! if(m12.eq.cd0.and.m22.eq.cd0)then


#ifdef CHECK
          if(m32.ne.m42.or.p34.ne.0d0.or.m12.ne.cd0.or.m22.ne.cd0
     &        .or.p14.eq.m12.or.p23.eq.m22)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir0_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &            ' D0ms1ir0_coli:  (4.15)  m12=0, m22=0',
     &            '    case with 0 zero small masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
            endif
          endif
#endif


         D0ms1ir0_coli =
     &        2d0*cspenc_coli(1d0-p13/p14,real(p13-p14))
     &        -2d0*cspenc_coli(1d0-p23/p24,real(p23-p24))
     &        +2d0*cspcos_coli(p23/p24,p14/p13,real(p24-p23),
     &        real(p13-p14))
     &        +.5d0*(cln_coli(p23/p24,real(p24-p23))
     &              +cln_coli(p14/p13,real(p13-p14)))**2
     &        -(cln_coli(p23/p24,real(p24-p23))
     &          +cln_coli(p14/p13,real(p13-p14)))*
     &        (cln_coli(-p23/(m32*coliminfscale2),-1d0)
     &          +cln_coli(p13/p12,real(p12-p13)))

         D0ms1ir0_coli = D0ms1ir0_coli/(p13*p24-p23*p14)

       endif
      endif

      end

************************************************************************
      function D0ms1ir1_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 1 mass singularities and up to 1 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p12, m12, m22 small                                              *
*     p13, p24, p14=m42, p23=/=m32, p34 finite                         *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23=/=m32                      *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32                                  *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  04.01.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms1ir1_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 D0ms1ir1_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:8)
      data    flag /9*.true./
      save    flag
#endif

      complex*16 l13,l24,l23,l34,mm12,mm22
      complex*16 r34,r43inv
      real*8     ir34

#ifdef CHECK
      complex*16 m2,m3,m4
      complex*16 k24,k23,k34
      complex*16 y,z,logx3,logy,logxs
#endif

      complex*16 cln_coli,cspcon_coli,cspcos_coli,cspenc_coli

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"


 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0
     &        .or.ms42.eq.cd0.or.ps12.ne.cd0.or.ps23.eq.ms32
     &        .or.ps23.eq.cd0.and.ms32.eq.cd0.or.ps14.ne.ms42
     &        .or.ps24.eq.ms42.or.ps13.eq.ms32
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms1ir1_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms1ir1_coli called improperly:'
            write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42,ms42
            write(nerrout_coli,*)' D0ms1ir1_coli: tests',
     &          ms12.ne.cd0,ms22.ne.cd0
     &          ,ms42.eq.cd0,ps12.ne.cd0,ps23.eq.ms32
     &          ,ps23.eq.cd0.and.ms32.eq.cd0,ps14.ne.ms42
     &          ,ps24.eq.ms42,ps13.eq.ms32
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p34).ne.0d0,aimag(p14).ne.0d0
     &          ,aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif

      if (abs(m32-p13).lt.abs(m32)*calacc.or.
     &    abs(m42-p24).lt.abs(m42)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms1ir1_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &                       ' D0ms1ir1_coli: p13 = m32 or p24 = m42'
          write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
        endif
        D0ms1ir1_coli = undefined
        if (abs(m32-p13).eq.0d0.or.abs(m42-p24).eq.0d0) return
      endif

      if(m12.eq.cd0)then
        if(m22.eq.cd0)then
* 1 soft singularities and 2 zero masses
*
*                   0
*    p12=0   ----------------  p23
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.p12.ne.0d0.or.
     &        p14.ne.m42.or.p23.eq.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.19)',
     &            '    case with 1 soft singularity and 2 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
            endif
          endif
#endif


c     according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box12
c                     includes box9 for  m4^2->0
c     p3^2 -> p34, p4^2 -> p23, m3^2 -> m42, m4^2-> m32
c     s23 -> p13, s12 -> p24
c     Denner Dittmaier (4.19)

          l34 = m32+m42-p34
          if(l34.ne.cd0)then
            r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
            r43inv= m32/(r34*m42)
            if(m32.ne.cd0)then
              ir34 = sign(1d0,1d0-abs(r34/r43inv))
            else
              ir34 = -1d0
            endif
          else
            r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
            r43inv= -r34
          endif


c code > pub:   4->3->2->1->0

          D0ms1ir1_coli=
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &        +cln_coli((m32-p13)/(m32-p23),real(p23-p13)))
#endif
     &        +2d0*cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &        *cln_coli((m32-p13)/(m32-p23),real(p23-p13))
     &        +(cln_coli((m42-p24)/sqrt(m42*muir2),-1d0))**2
     &        -pi2_6
     &        +cspcos_coli((m42-p24)/(m32-p23),r34,
     &        -real(p24-m42-p23+m32),ir34)
     &        +cspcos_coli((m42-p24)/(m32-p23),r43inv,
     &        -real(p24-m42-p23+m32),-ir34)
     &        -2d0*
     &        cspcon_coli((m32-p23),1d0/(m32-p13),(m32-p23)/(m32-p13),
     &        (p23-p13)/(m32-p13),-1d0,1d0)


          D0ms1ir1_coli=D0ms1ir1_coli/((p13-m32)*(p24-m42))


        else                    ! m12=0, m22=/=0

* 1 soft singularities and 1 zero masses
*
*                  m22 small
*    p12=m22 ----------------  p23
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.eq.0d0.or.p12.ne.m22.or.
     &        p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.20)',
     &            '    case with 1 soft singularity and 1 zero mass:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms1ir1_coli',
     &          'new function called:  (4.20)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.20)',
     &            '    case with 1 soft singularity and 1 zero mass:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

c     cD0ire
c     according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349  (ia)
c     Ellis, Zanderighi, JHEP 0802 (2008) 002    box16 m2 small
c     Denner, Dittmaier (4.20)

          mm22 = m22*coliminfscale2
          l34 = m32+m42-p34
          if(l34.ne.cd0)then
            r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
            r43inv= m32/(r34*m42)
            if(m32.ne.cd0)then
              ir34 = sign(1d0,1d0-abs(r34/r43inv))
            else
              ir34 = -1d0
            endif
          else
            r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
            r43inv= -r34
          endif

          D0ms1ir1_coli=
#ifdef SING
     &        -delta1ir*cln_coli((m42-p24)/sqrt(m42*mm22),-1d0)
     &        - 0.25d0*colishiftms2
#endif
     &        +2d0*cln_coli((m42-p24)/sqrt(m42*mm22),-1d0)
     &        *(cln_coli((m32-p13)/(m32-p23),real(p23-p13))
     &        +.5d0*log(mm22/muir2))
     &        +(cln_coli((m42-p24)/sqrt(m42*mm22),-1d0))**2
     &        -pi2_6
     &        +cspcos_coli((m42-p24)/(m32-p23),r34,
     &        -real(p24-m42-p23+m32),ir34)
     &        +cspcos_coli((m42-p24)/(m32-p23),r43inv,
     &        -real(p24-m42-p23+m32),-ir34)

          D0ms1ir1_coli=D0ms1ir1_coli/((p13-m32)*(p24-m42))



        endif                   !(m22.eq.cd0)

      elseif(m12.ne.cd0)then
        if(m22.eq.cd0)then      ! m12=/=0, m22=0
* 0 soft singularities and 1 zero masses
*
*                   0
*    p12=m12 ----------------  p23
*                |  2   |
*      small m12 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.0d0.or.p12.ne.m12.or.
     &        p23.eq.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.21)',
     &            '    case with 0 soft singularity and 1 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(5))then
            call ErrOut_coli('D0ms1ir1_coli',
     &          'new function called:  (4.21)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.21)',
     &            '    case with 0 soft singularity and 1 zero masses:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
              flag(5)=.false.
            endif
          endif
#endif

c     Denner, Dittmaier (4.21)    21.10.09

          l13=m32-p13
          l24=m42-p24
          l34=m32+m42-p34
          l23=m32-p23
          mm12=m12*coliminfscale2

          if(l34.ne.cd0)then
            r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
            r43inv= m32/(r34*m42)
            if(m32.ne.cd0)then
              ir34 = sign(1d0,1d0-abs(r34/r43inv))
            else
              ir34 = -1d0
            endif
          else
            r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
            r43inv= -r34
          endif

          D0ms1ir1_coli=-1.5d0*pi2_6
     &        -cspcos_coli(l24/l23,r34,real(l24-l23),ir34)
     &        -cspcos_coli(l24/l23,r43inv,real(l24-l23),-ir34)
     &        -(cln_coli(l24/l23,real(l24-l23))
     &              +cln_coli(l13/sqrt(mm12*m42),-1d0))**2
#ifdef SING
     &            - 0.25d0*colishiftms2
#endif

          D0ms1ir1_coli=D0ms1ir1_coli/(-l13*l24)


        else                    ! m12=/=0, m22=/=0
*     1 soft singularities and 1 zero masses
*
*                 m22=m12
*     p12=0   ----------------  p23
*                |  2   |
*            m12 |1    3| m32
*                |   4  |
*     p14=m42 ----------------  p34
*                  m42
*

#ifdef CHECK
          if(m12.ne.m22.or.m22.eq.0d0.or.p12.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.22)',
     &            '    case with 0 soft singularity and 0 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir1_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(6))then
            call ErrOut_coli('D0ms1ir1_coli',
     &          'new function called:  (4.22)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir1_coli:  (4.22)',
     &            '    case with 0 soft singularity and 0 zero masses:',
     &            '    not yet tested in physical process'

              write(nerrout_coli,111)' D0ms1ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir0_coli: m42 = ',m42
              flag(6)=.false.
            endif
          endif
#endif

c     Denner, Dittmaier (4.22)

          mm12=m12*coliminfscale2
          l13=m32-p13
          l23=m32-p23
          l24=m42-p24
          l34=m32+m42-p34

          if(l34.ne.cd0)then
            r34 = l34/(2d0*m42)*(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
            r43inv= m32/(r34*m42)
            if(m32.ne.cd0)then
              ir34 = sign(1d0,1d0-abs(r34/r43inv))
            else
              ir34 = -1d0
            endif
          else
            r34 = dcmplx(0d0,1d0)*sqrt(m32/m42)
            r43inv= -r34
          endif


          D0ms1ir1_coli =
     &        2d0*cspenc_coli(1d0-l13/l23,real(l23-l13))
     &        +cspcos_coli(l24/l23,r34,real(l24-l23),ir34)
     &        +cspcos_coli(l24/l23,r43inv,real(l24-l23),-ir34)
     &        -.5d0*pi2_6
     &        +(cln_coli(l13/sqrt(m42*mm12),-1d0)+
     &          cln_coli(l24/l23,real(l24-l23)))**2
#ifdef SING
     &        +.25d0*colishiftms2
#endif

          D0ms1ir1_coli =  D0ms1ir1_coli/(l24*l13)




        endif
      endif                     ! m22.eq.cd0


      end

************************************************************************
      function D0ms1ir2_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 1 mass singularities and up to 2 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p12, m12, m22 small                                              *
*     p13, p24, p14=m42, p23=m32, p34 finite                           *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23=m32                        *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32                                  *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  01.12.09             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms1ir2_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:3)
      data    flag /4*.true./
      save    flag
#endif

      complex*16 cln_coli
      complex*16 m2(4)
      complex*16 l13,l24,l34,r34,mm12,mm32,mm42,q13,q24
      real*8     ir34
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.eq.cd0
     &        .or.ms42.eq.cd0.or.ps12.ne.cd0.or.ps23.ne.ms32
     &        .or.ps14.ne.ms42
     &        .or.ps24.eq.ms42.or.ps13.eq.ms32
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms1ir2_coli called improperly:'
            write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      if (abs(m32-p13).lt.abs(m32)*calacc.or.
     &    abs(m42-p24).lt.abs(m42)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms1ir2_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &        errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &                ' D0ms1ir2_coli: p13 = m32 or p24 = m42'
          write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
        endif
        D0ms1ir2_coli = undefined
        if (abs(m32-p13).eq.0d0.or.abs(m42-p24).eq.0d0) return
      endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo


      if(m12.eq.cd0.and.m22.eq.cd0)then
* 2 soft singularities and 2 zero masses
*
*                   0
*    p12=0   ----------------  p23=m32
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.p12.ne.0d0.or.
     &        p14.ne.m42.or.p23.ne.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli:  (4.25)',
     &          '    case with 2 soft singularities and 2 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
            endif
          endif
#endif




c     according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box11
c     p3^2 -> p34, m3^2 -> m32, m4^2-> m42
c     s12 -> p13, s23 -> p24
c     Denner Dittmaier (4.25)

          l34 = m32+m42-p34
          r34 = l34/(2d0*sqrt(m42*m32))
     &        *(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
          ir34 = sign(1d0,1d0-abs(r34*r34))

          D0ms1ir2_coli =
#ifdef SING
     &        delta2ir
     &        -delta1ir*
     &        (cln_coli((m32-p13)/sqrt(muir2*m32),-1d0)
     &        +cln_coli((m42-p24)/sqrt(muir2*m42),-1d0))
#endif
     &        +2d0*cln_coli((m32-p13)/sqrt(muir2*m32),-1d0)
     &        *cln_coli((m42-p24)/sqrt(muir2*m42),-1d0)
     &        -(cln_coli(r34,-1d0))**2
     &        -4d0*pi2_6

          D0ms1ir2_coli = D0ms1ir2_coli/((p13-m32)*(p24-m42))



        elseif(m12.eq.cd0.or.m22.eq.cd0)then

* 2 soft singularities and 1 zero masses
*
*                   0
*    p12=m22 ----------------  p23=m32
*                |  2   |
*     small  m22 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
            if(m12+m22.eq.cd0.or.m22*m12.ne.0d0.or.p12.ne.(m22+m12).or.
     &          p14.ne.m42.or.p23.ne.m32)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms1ir2_coli:  (4.26)',
     &            '    case with 2 soft singularities and 1 zero mass:',
     &            '    wrong arguments'
                write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(2))then
              call ErrOut_coli('D0ms2ir2_coli',
     &            'new function called:  (4.26)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms1ir2_coli:  (4.26)',
     &            '    case with 2 soft singularities and 1 zero mass:',
     &            '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
                flag(2)=.false.
              endif
            endif
#endif

            if(m22.eq.cd0) then
              mm12=m12*coliminfscale2
              mm32=m32
              mm42=m42
              q13=p13
              q24=p24
            else
              mm12=m22*coliminfscale2
              mm32=m42
              mm42=m32
              q13=p24
              q24=p13
            endif


c new case (derived from IR singular case (i))
c     Denner, Dittmaier (4.26)        21.09.10

            l13=mm32-q13
            l24=mm42-q24
            l34=m32+m42-p34

            r34 = l34/(2d0*sqrt(m32*m42))
     &          *(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
            ir34= sign(1d1,1d0-abs(r34))

            D0ms1ir2_coli = 1.5d0*pi2_6 +(cln_coli(r34,ir34))**2
     &          -2d0*cln_coli(l13/sqrt(mm32*mm12),-1d0)
     &          *cln_coli(l24/sqrt(mm42*muir2),-1d0)
#ifdef SING
     &        +delta1ir*cln_coli(l13/sqrt(mm32*mm12),-1d0)
#endif

            D0ms1ir2_coli =  D0ms1ir2_coli/((q24-mm42)*(mm32-q13))



        else

* 2 soft singularities and 0 zero masses
*
*                 m22=m12
*      p12=0 ----------------  p23=m32
*                |  2   |
*      m12 small |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.ne.m22.or.m22.eq.0d0.or.p12.ne.cd0.or.
     &        p14.ne.m42.or.p23.ne.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli:  (4.27)',
     &            '    case with 2 soft singularities and 0 zero mass:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms1ir2_coli',
     &          'new function called:  (4.27)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli:  (4.27)',
     &            '    case with 2 soft singularities and 0 zero mass:',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms1ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms1ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms1ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms1ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms1ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms1ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms1ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms1ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms1ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms1ir2_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif

c     Denner, Dittmaier (4.27)

          mm12=m12*coliminfscale2
          l13=m32-p13
          l24=m42-p24
          l34=m32+m42-p34
          if(l34.ne.cd0)then
            r34 = l34/(2d0*sqrt(m32*m42))
     &          *(1d0+sqrt(1d0-4d0*m32*m42/l34**2))
          else
            r34 = dcmplx(0d0,1d0)
          endif
          ir34 = sign(1d0,1d0-abs(r34*r34))

          D0ms1ir2_coli =  -3d0*pi2_6
     &        +2d0*cln_coli(l24/sqrt(m42*mm12),-1d0)
     &          *cln_coli(l13/sqrt(m32*mm12),-1d0)
     &        -(cln_coli(r34,ir34))**2
#ifdef SING
     &        +0.5d0*colishiftms2
#endif

          D0ms1ir2_coli =  D0ms1ir2_coli/(l24*l13)

      endif

      end

************************************************************************
      function D0ms2ir0_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 2 mass singularities and up to 0 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p12, p34, m12, m22, m32, m42 small                               *
*     p13, p24, p14, p23 finite                                        *
*                                                                      *
*                      m22 small                                       *
*     p12 small  ---------------------  p23                            *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*           p14  ---------------------  p34 small                      *
*                      m42 small                                       *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  21.12.09             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms2ir0_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:7)
      data    flag /8*.true./
      save    flag
#endif

      complex*16 m2(4),mm12,mm22,mm32,mm42,q13,q24,q14,q23,x
      complex*16 a,b,c,r13,r14,r23,r24,x1,x2
      complex*16 cln_coli,cspenc_coli,cspcon_coli,cspcos_coli
      real*8     ix1
      integer    nm0
      integer    i
#ifdef CHECK
      complex*16 D0ms2ir0_check
      integer    nsoft
      integer    j,k
      logical    soft(4),onsh(4,4)
#endif

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &      .or.ms42.ne.cd0.or.ps12.ne.cd0.or.ps34.ne.cd0
     &      .or.ps23.eq.cd0.or.ps14.eq.cd0
     &      .or.ps24.eq.cd0.or.ps13.eq.cd0
     &      .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &      .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &      .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir0_coli called improperly:'
            write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      if (abs(p13*p24-p14*p23).lt.abs(p13*p24)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms2ir1_coli',' singular denominator'//
     &      ' use appropriate regularization',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms2ir0_coli: p13*p24-p14*p23 = 0'
          write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
        endif
        D0ms2ir0_coli = undefined
        if (abs(p13*p24-p14*p23).eq.0d0) return
      endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

#ifdef CHECK
c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo
#endif

c count/determine zero masses
      nm0=0
      do i=1,4
        if(m2(i).eq.cd0) nm0=nm0+1
      enddo


#ifdef CHECK
      if(nsoft.eq.0)then
#endif

        if(nm0.eq.4)then
* 0 soft singularities and 4 zero masses
*
*                   0
*          0 ----------------  p23
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        p14 ----------------  0
*                   0
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.m32.ne.cd0.or.m42.ne.0d0
     &        .or.p12.ne.cd0.or.p34.ne.cd0.or.
     &        p14.eq.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.52)',
     &            '    case with 0 soft singularities'//
     &            ' and 4 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
            endif
          endif
#endif


c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box3
c p_4^2 -> p14, p_2^2 -> p23, s12 -> p13, s23 -> p24
c Denner Dittmaier (4.52)

          D0ms2ir0_coli =
     &        2d0*(cspcos_coli(p23/p13,p14/p24,
     &        real(p13-p23),real(p24-p14))
     &        +cspenc_coli(1d0-p13/p23,real(p13-p23))
     &        +cspenc_coli(1d0-p13/p14,real(p13-p14))
     &        -cspenc_coli(1d0-p23/p24,real(p23-p24))
     &        -cspenc_coli(1d0-p14/p24,real(p14-p24))
     &        +cln_coli(muir2/(-p13),1d0)*
     &        (cln_coli(p23/p13,real(p13-p23))
     &        +cln_coli(p14/p24,real(p24-p14))))
#ifdef SING
     &        + 2d0*delta1ir*
     &        (cln_coli(p23/p13,real(p13-p23))
     &        +cln_coli(p14/p24,real(p24-p14)))
#endif

          D0ms2ir0_coli = D0ms2ir0_coli/(p13*p24-p14*p23)



        elseif(nm0.eq.3)then
* 0 soft singularities and 3 zero masses
*
*    small          0
*    p12=m12 ----------------  p23
*                |  2   |
*      m12 small |1    3| 0
*                |   4  |
*        p14 ----------------  0
*                   0
*

#ifdef CHECK
          if(.not.(m12.eq.cd0.and.m32.eq.cd0.and.m42.eq.0d0.and.
     &        m22.ne.cd0.and.p12.eq.m22).and.
     &        .not.(m22.eq.cd0.and.m32.eq.cd0.and.m42.eq.0d0.and.
     &        m12.ne.cd0.and.p12.eq.m12).and.
     &        .not.(m12.eq.cd0.and.m22.eq.cd0.and.m42.eq.0d0.and.
     &        m32.ne.cd0.and.p34.eq.m32).and.
     &        .not.(m12.eq.cd0.and.m22.eq.cd0.and.m32.eq.0d0.and.
     &        m42.ne.cd0.and.p34.eq.m42).or.
     &        p14.eq.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.53)',
     &            '    case with 0 soft singularities'//
     &            ' and 3 zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms2ir0_coli',
     &          'new function called:  (4.53)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.53)',
     &          '    case with 0 soft singularities and 3 zero masses:',
     &          '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

          if(m12.ne.cd0)then
            mm12=m12*coliminfscale2
            q23=p23
            q14=p14
            q13=p13
            q24=p24
          elseif(m22.ne.cd0)then
            mm12=m22*coliminfscale2
            q23=p14
            q14=p23
            q13=p24
            q24=p13
          elseif(m32.ne.cd0)then
            mm12=m32*coliminfscale2
            q23=p14
            q14=p23
            q13=p13
            q24=p24
          else
            mm12=m42*coliminfscale2
            q23=p23
            q14=p14
            q13=p24
            q24=p13
          endif

c 31.07.09
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box13
c m42 -> m22, m32 -> m12, p3^2 -> p12, p2^2 -> p14, p4^2 -> p23,
c s12 -> p13, s23 -> p24,       m22=0, p12=m12 small
c Denner Dittmaier (4.53)

          D0ms2ir0_coli =
     &        2d0*(cspcos_coli(q23/q13,q14/q24,
     &        real(q13-q23),real(q24-q14))
     &        +cspenc_coli(1d0-q13/q14,real(q13-q14))
     &        -cspenc_coli(1d0-q23/q24,real(q23-q24))
     &        +cln_coli(sqrt(muir2*mm12)/(-q13),1d0)*
     &        (cln_coli(q23/q13,real(q13-q23))
     &        +cln_coli(q14/q24,real(q24-q14))))
#ifdef SING
     &        + delta1ir*
     &        (cln_coli(q23/q13,real(q13-q23))
     &        +cln_coli(q14/q24,real(q24-q14)))
#endif

          D0ms2ir0_coli = D0ms2ir0_coli/(q13*q24-q14*q23)


        elseif(nm0.eq.2)then
          if(m12.eq.m22.and.m42.eq.m32)then

* 0 soft singularities and 2 zero masses on collinear leg
*
*                 m22=m12
*          0  ----------------  p23
*                |  2   |
*      m12 small |1    3| 0
*                |   4  |
*        p14 ----------------  0
*                   0
*

#ifdef CHECK
            if(p12.ne.cd0.or.p34.ne.cd0.or.
     &          m12.ne.cd0.and.m32.ne.cd0.or.
     &          m12.eq.cd0.and.m32.eq.cd0.or.
     &          p14.eq.cd0.or.p23.eq.cd0)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir0_coli:  (4.54)',
     &              '    case with 0 soft singularities'//
     &              ' and 2 zero masses on collinear leg:',
     &              '    wrong arguments'
                write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(3))then
              call ErrOut_coli('D0ms2ir0_coli',
     &            'new function called:  (4.54)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir0_coli: (4.54)',
     &              '    case with 0 soft singularities'//
     &              ' and 2 zero masses on collinear leg:',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
                flag(3)=.false.
              endif
            endif
#endif

            if(m32.eq.cd0)then
              mm12=m12*coliminfscale2
              q23=p23
              q14=p14
              q13=p13
              q24=p24
            else
              mm12=m32*coliminfscale2
              q23=p14
              q14=p23
              q13=p13
              q24=p24
            endif

c 31.07.09
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box13
c m42 -> m22, m32 -> m12, p3^2 -> p12, p2^2 -> p14, p4^2 -> p23,
c s12 -> p13, s23 -> p24,       m22=m12 small, p12=0
c Denner Dittmaier (4.54)

            D0ms2ir0_coli =
     &          2d0*(cspcos_coli(q23/q13,q14/q24,
     &          real(q13-q23),real(q24-q14))
     &          +cspenc_coli(1d0-q13/q23,real(q13-q23))
     &          +cspenc_coli(1d0-q13/q14,real(q13-q14))
     &          -cspenc_coli(1d0-q23/q24,real(q23-q24))
     &          -cspenc_coli(1d0-q14/q24,real(q14-q24))
     &          +cln_coli(sqrt(muir2*mm12)/(-q13),1d0)*
     &          (cln_coli(q23/q13,real(q13-q23))
     &          +cln_coli(q14/q24,real(q24-q14))))
#ifdef SING
     &          + delta1ir*
     &          (cln_coli(q23/q13,real(q13-q23))
     &          +cln_coli(q14/q24,real(q24-q14)))
#endif

            D0ms2ir0_coli = D0ms2ir0_coli/(q13*q24-q14*q23)

          elseif(m12.eq.m32.or.m22.eq.m42)then

* 0 soft singularities and 2 opposite zero masses
*
*    small          0
*    p12=m12 ----------------  p23
*                |  2   |
*      m12 small |1    3| m32 small
*                |   4  |
*        p14 ----------------  p34=m32
*                   0          small
*

#ifdef CHECK
            if(p12.eq.cd0.or.p34.eq.cd0.or.
     &          m12.ne.cd0.and.m22.ne.cd0.or.
     &          m12.eq.cd0.and.m22.eq.cd0.or.
     &          p14.eq.cd0.or.p23.eq.cd0)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir0_coli:  (4.55)',
     &              '    case with 0 soft singularities'//
     &              ' and 2 zero masses on opposite lines: ',
     &              '    wrong arguments'
                write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(4))then
              call ErrOut_coli('D0ms2ir0_coli',
     &            'new function called:  (4.55)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir0_coli: (4.55)',
     &              '    case with 0 soft singularities'//
     &              ' and 2 zero masses on opposite lines:',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
                flag(4)=.false.
              endif
            endif
#endif

c     cD0ms00em
c     Denner Dittmaier (4.55) = (4.56)

            if(m22.eq.cd0)then
              mm12=m12*coliminfscale2
              mm32=m32*coliminfscale2
              q14=p14
              q23=p23
              q24=p24
              q13=p13
            else
              mm12=m22*coliminfscale2
              mm32=m42*coliminfscale2
              q14=p23
              q23=p14
              q24=p13
              q13=p24
            endif

            D0ms2ir0_coli =
     &          2d0*(cspcos_coli(q23/q13,q14/q24,
     &                          real(q13-q23),real(q24-q14))
     &          +cln_coli(sqrt(mm12*mm32)/(-q13),1d0)*
     &          (cln_coli(q23/q13,real(q13-q23))
     &          +cln_coli(q14/q24,real(q24-q14))))

            D0ms2ir0_coli = D0ms2ir0_coli/(q13*q24-q14*q23)


          elseif(m12.eq.m42.or.m22.eq.m32)then

* 0 soft singularities and 2 zero masses on non-collinear leg
*
*    small          0
*    p12=m12 ----------------  p23
*                |  2   |
*      m12 small |1    3| 0
*                |   4  |
*        p14 ----------------  p34=m42
*                m42 small       small
*

#ifdef CHECK
            if(p12.eq.cd0.or.p34.eq.cd0.or.
     &          m12.ne.cd0.and.m22.ne.cd0.or.
     &          m12.eq.cd0.and.m22.eq.cd0.or.
     &          p14.eq.cd0.or.p23.eq.cd0)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir0_coli:  (4.56)',
     &              '    case with 0 soft singularities'//
     &              ' and 2 zero masses on noncollinear leg: ',
     &              '    wrong arguments'
                write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              endif
            endif
#endif


            if(m22.eq.cd0)then
              mm12=m12*coliminfscale2
              mm32=m42*coliminfscale2
              q14=p13
              q23=p24
              q24=p23
              q13=p14
            else
              mm12=m22*coliminfscale2
              mm32=m32*coliminfscale2
              q14=p24
              q23=p13
              q24=p14
              q13=p23
            endif

            D0ms2ir0_coli =
     &          2d0*(cspcos_coli(q23/q13,q14/q24,
     &                          real(q13-q23),real(q24-q14))
     &          +cln_coli(sqrt(mm12*mm32)/(-q13),1d0)*
     &          (cln_coli(q23/q13,real(q13-q23))
     &          +cln_coli(q14/q24,real(q24-q14))))

            D0ms2ir0_coli = D0ms2ir0_coli/(q13*q24-q14*q23)



#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)
     &        ' D0ms2ir0_coli: case with more than 0 soft singularities'
     &        ,'                and 2 zero masses: wrong arguments '
            write(nerrout_coli,*)' D0ms2ir0_coli: nm0 = ',nm0
            write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
          endif
#endif
        endif

      elseif(nm0.eq.1)then
* 0 soft singularities and 1 zero masses
*
*    small       m22 small
*    p12=m22 ----------------  p23
*                |  2   |
*              0 |1    3| m32 small
*                |   4  |
*        p14 ----------------  0
*                 m42=m32
*

#ifdef CHECK
          if(.not.(m12.ne.cd0.and.m32.ne.cd0.and.m42.ne.0d0.and.
     &        m22.eq.cd0.and.p12.eq.m12.and.p34.eq.cd0).and.
     &        .not.(m22.ne.cd0.and.m32.ne.cd0.and.m42.ne.0d0.and.
     &        m12.eq.cd0.and.p12.eq.m22.and.p34.eq.cd0).and.
     &        .not.(m12.ne.cd0.and.m22.ne.cd0.and.m42.ne.0d0.and.
     &        m32.eq.cd0.and.p34.eq.m42.and.p12.eq.cd0).and.
     &        .not.(m12.ne.cd0.and.m22.ne.cd0.and.m32.ne.0d0.and.
     &        m42.eq.cd0.and.p34.eq.m32.and.p12.eq.cd0).or.
     &        p14.eq.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.57)',
     &          '    case with 0 soft singularities and 1 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(6))then
            call ErrOut_coli('D0ms2ir0_coli',
     &          'new function called:  (4.57)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.57)',
     &          '    case with 0 soft singularities and 1 zero masses:',
     &          '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              flag(6)=.false.
            endif
          endif
#endif

          if(m12.eq.cd0)then
            mm12=m22*coliminfscale2
            mm32=m32*coliminfscale2
            q14=p23
            q23=p14
            q24=p13
            q13=p24
          elseif(m22.eq.cd0)then
            mm12=m12*coliminfscale2
            mm32=m32*coliminfscale2
            q14=p13
            q23=p24
            q24=p23
            q13=p14
          elseif(m32.eq.cd0)then
            mm12=m42*coliminfscale2
            mm32=m22*coliminfscale2
            q14=p14
            q23=p23
            q24=p13
            q13=p24
          else
            mm12=m32*coliminfscale2
            mm32=m22*coliminfscale2
            q14=p23
            q23=p14
            q24=p24
            q13=p13
          endif

c Denner Dittmaier (4.57)

          D0ms2ir0_coli =
     &        2d0*(cspcos_coli(q23/q13,q14/q24,
     &        real(q13-q23),real(q24-q14))
     &        +cspenc_coli(1d0-q13/q14,real(q13-q14))
     &        -cspenc_coli(1d0-q23/q24,real(q23-q24))
     &        +cln_coli(sqrt(mm12*mm32)/(-q13),1d0)*
     &        (cln_coli(q23/q13,real(q13-q23))
     &        +cln_coli(q14/q24,real(q24-q14))))

          D0ms2ir0_coli = D0ms2ir0_coli/(q13*q24-q14*q23)



        elseif(nm0.eq.0)then
* 0 soft singularities and 0 zero masses
*
*                 m22=m12
*          0 ----------------  p23
*                |  2   |
*       m12 small|1    3| m32 small
*                |   4  |
*        p14 ----------------  0
*                 m42=m32
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.eq.0d0.or.m32.eq.cd0.or.m42.eq.0d0
     &        .or.p12.ne.cd0.or.p34.ne.cd0.or.
     &        p14.eq.cd0.or.p23.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir0_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli: (4.58)',
     &            '    case with 0 soft singularities',
     &            '    and 0 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(7))then
            call ErrOut_coli('D0ms2ir0_coli',
     &          'new function called:  (4.58)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir0_coli:  (4.58)',
     &          '    case with 0 soft singularities and 0 zero masses:',
     &          '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir0_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
              flag(7)=.false.
            endif
          endif
#endif

          mm12=m12*coliminfscale2
          mm32=m32*coliminfscale2
          D0ms2ir0_coli =
     &        2d0*(cspcos_coli(p23/p13,p14/p24,
     &        real(p13-p23),real(p24-p14))
     &        +cspenc_coli(1d0-p13/p23,real(p13-p23))
     &        +cspenc_coli(1d0-p13/p14,real(p13-p14))
     &        -cspenc_coli(1d0-p23/p24,real(p23-p24))
     &        -cspenc_coli(1d0-p14/p24,real(p14-p24))
     &        +cln_coli(sqrt(mm12*mm32)/(-p13),1d0)*
     &        (cln_coli(p23/p13,real(p13-p23))
     &        +cln_coli(p14/p24,real(p24-p14))))

          D0ms2ir0_coli = D0ms2ir0_coli/(p13*p24-p14*p23)



        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms2ir0_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' D0ms2ir0_coli: case with more than 0 soft
     &        singularities not implemented '
          write(nerrout_coli,111)' D0ms2ir0_coli: nsoft = ',nsoft
          write(nerrout_coli,111)' D0ms2ir0_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir0_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir0_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir0_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir0_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir0_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir0_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir0_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir0_coli: m42 = ',m42
        endif
      endif
#endif

      end

************************************************************************
      function D0ms2ir1_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 2 mass singularities and up to 1 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p12, p14, m12, m22, m42 small                                    *
*     p13, p24, p23, p34, m32 finite                                   *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23                            *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32                                  *
*                     |         |                                      *
*                     |    4    |                                      *
*     p14 small  ---------------------  p34                            *
*                       m42 small                                      *
*                                                                      *
************************************************************************
*     27.08.08 Ansgar Denner        last changed  12.12.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms2ir1_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli,D0ms2ir1_check
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 b
      logical flag(0:7),flag2
      data    flag /8*.true./, flag2 /.true./
      save    flag,flag2
#endif

      complex*16 q12,q23,q34,q14,q13,q24,mm12,mm22,mm32,mm42
      complex*16 l12,l23,l34,l14,l13,l24
      complex*16 r12,r14,r24,r21,r41,r42
      complex*16 d
      complex*16 m2(4)
      complex*16 y(2),z(2),eta,l1,l2
      complex*16 cln_coli,cspcon_coli,cspenc_coli,cspcos_coli,eta2s_coli
      real*8     ir12,ir24,ir14,iy(2),iz(2),u,v
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"



 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms42.ne.cd0
     &        .or.ps12.ne.cd0.or.ps14.ne.cd0
     &        .or.ps34.eq.ms32.or.ps23.eq.ms32
     &        .or.ps24.eq.cd0.or.ps13.eq.ms32
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir1_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir1_coli called improperly:'
            write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42,ms42
            write(nerrout_coli,*)' D0ms2ir1_coli: test  ',
     &          ms12.ne.cd0,ms22.ne.cd0,ms42.ne.cd0
     &          ,ps12.ne.cd0,ps14.ne.cd0
     &          ,ps34.eq.ms32,ps23.eq.ms32
     &          ,ps24.eq.cd0,ps13.eq.ms32
     &          ,aimag(p12).ne.0d0,aimag(p23).ne.0d0
     &          ,aimag(p34).ne.0d0,aimag(p14).ne.0d0
     &          ,aimag(p24).ne.0d0,aimag(p13).ne.0d0
          endif
        endif
      endif
#endif

      if (abs(m32-p13).lt.abs(m32)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms2ir1_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms2ir1_coli: p13 = m32'
          write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
        endif
        D0ms2ir1_coli = undefined
        if (abs(m32-p13).eq.0d0) return
      endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

      if(nsoft.eq.1)then

        if(m22.eq.cd0.and.m42.eq.cd0)then
          if(m12.eq.cd0)then
* 1 soft singularities and 3 zero masses
*
*                   0
*          0 ----------------  p23
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*          0 ----------------  p34
*                   0
*

#ifdef CHECK
            if(m12.ne.cd0.or.m22.ne.0d0.or.m42.ne.0d0
     &          .or.p12.ne.cd0.or.p14.ne.cd0.or.
     &          p34.eq.m32.or.p23.eq.m32)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir1_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir1_coli:  (4.29)',
     &              '    case with 1 soft singularities',
     &              '    and 3 zero masses: wrong arguments'
                write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
              endif
            endif
#endif


c     according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box8
c                       covers box4 for m3^2=0
c p_3^2 -> p23, p_4^2 -> p34, m_4^2 -> m32, s12 -> p24, s23 -> p13
c     and Berger, Klasen, Tait, PRD62 (2000) 095014
c     Denner Dittmaier (4.29)
            D0ms2ir1_coli =
#ifdef SING
     &          delta2ir
     &          -delta1ir*(cln_coli((-p24)/muir2,-1d0)
     &          +cln_coli((m32-p13)/(m32-p34),real(p34-p13))
     &          +cln_coli((m32-p13)/(m32-p23),real(p23-p13)))
c?     &          -.5d0*colishiftms2
#endif
c changed to allow for zero m32
     &          +cln_coli(-p24/muir2,-1d0)
     &          *(cln_coli((m32-p13)/(m32-p34),real(p34-p13))
     &          +cln_coli((m32-p13)/(m32-p23),real(p23-p13)))
     &          -.5d0*(cln_coli((m32-p23)/(m32-p34),real(p34-p23)))**2
     &          +cspcon_coli(m32/(m32-p34),-p24/(m32-p23),
     &          -m32*p24/((m32-p34)*(m32-p23)),
     &          ((m32-p34)*(m32-p23)+m32*p24)/((m32-p34)*(m32-p23)),
     &          1d0,real(p23-m32-p24))

     &          +.5d0*(cln_coli(-p24/muir2,-1d0))**2
     &          -2d0*(
     &          cspcon_coli(m32-p34,1d0/(m32-p13),(m32-p34)/(m32-p13),
     &          (p34-p13)/(m32-p13),-1d0,1d0)
     &          +cspcon_coli(m32-p23,1d0/(m32-p13),(m32-p23)/(m32-p13),
     &          (p23-p13)/(m32-p13),-1d0,1d0) )
     &          -2d0*pi2_6

            D0ms2ir1_coli = D0ms2ir1_coli/((p13-m32)*p24)

          elseif (m32.eq.p34) then

            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir1_coli',' wrong branch',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,*)
     &            'D0ms2ir1: this branch should not be reached! '
            endif

#ifdef CHECK
          else
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir1_coli','case not implemented',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)
     &             ' D0ms2ir1_coli: case not implemented'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
#endif

          endif

        elseif(m22.eq.cd0.or.m42.eq.cd0)then
* 1 soft singularities and 2 zero masses
*
*    small        m22 small
*    p12=m22 ----------------  p23
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*          0 ----------------  p34
*                    0
*

#ifdef CHECK
          if(m12.ne.cd0.or.p14.ne.m42.or.p12.ne.m22.or.
     &        .not.(m22.eq.cd0.and.m42.ne.cd0.and.p23.ne.m32.or.
     &              m42.eq.cd0.and.m22.ne.cd0.and.p34.ne.m32))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli:  (4.30)',
     &            '    case with 1 soft singularities',
     &            '    and 2 adjacent zero masses: ',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms2ir1_coli',
     &          'new function called:  (4.30)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir1_coli:  (4.30)',
     &            '    case with 1 soft singularities',
     &            '    and 2 adjacent zero masses ',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif

          if(m22.ne.cd0) then
            mm22=m22*coliminfscale2
            q23=p23
            q34=p34
          else
            mm22=m42*coliminfscale2
            q23=p34
            q34=p23
          endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box12 m3 small
c Denner Dittmaier (4.30)



          D0ms2ir1_coli=
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli((-p24)/sqrt(mm22*muir2),-1d0)
     &        +cln_coli((m32-p13)/(m32-q34),real(q34-p13)))
     &        -.25d0*colishiftms2
#endif
     &        +.5d0*(cln_coli(-p24/sqrt(mm22*muir2),-1d0))**2
     &        + cln_coli(-p24/sqrt(mm22*muir2),-1d0)
     &        *(cln_coli((m32-p13)/(m32-q34),real(q34-p13))
     &         +cln_coli((m32-p13)/(m32-q23),real(q23-p13))
     &         + .5d0*log(mm22/muir2))
     &        - .5d0*(-cln_coli((m32-p13)/(m32-q34),real(q34-p13))
     &         +cln_coli((m32-p13)/(m32-q23),real(q23-p13))
     &         + .5d0*log(mm22/muir2))**2
     &        -2d0*pi2_6
     &        +cspcon_coli(-p24/(m32-q34),m32/(m32-q23),
     &        (-p24*m32)/((m32-q34)*(m32-q23)),
     &        ((m32-q34)*(m32-q23)+p24*m32)/((m32-q34)*(m32-q23)),
     &        -real(p24+m32-q34),1d0)
     &        -2d0*
     &        cspcon_coli((m32-q34),1d0/(m32-p13),(m32-q34)/(m32-p13),
     &        (q34-p13)/(m32-p13),-1d0,1d0)



          D0ms2ir1_coli=D0ms2ir1_coli/((p13-m32)*p24)


        else
* 1 soft singularities and 1 zero masses
*
*    small       m22 small
*    p12=m22 ----------------  p23
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*    p14=m42 ----------------  p34
*    small       m42 small
*

#ifdef CHECK
          if(m12.ne.cd0.or.p14.ne.m42.or.p12.ne.m22.or.
     &       m22.eq.cd0.or.m42.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir1_coli:  (4.31)',
     &          '    case with 1 soft singularities and 1 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
          endif
#endif


c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (ib)
c          and (iib)    (D0irem and D0ir0em)
c also Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box16, m2,m4 small
c Denner Dittmaier (4.31)

          mm22=m22*coliminfscale2
          mm42=m42*coliminfscale2

          D0ms2ir1_coli=
#ifdef SING
     &        -cln_coli(-p24/(sqrt(mm22*mm42)),-1d0)
     &        *delta1ir
     &        - 0.5d0*colishiftms2
#endif
     &        +.5d0*(cln_coli(-p24/sqrt(mm22*mm42),-1d0))**2
     &        + cln_coli(-p24/sqrt(mm22*mm42),-1d0)
     &        *(cln_coli((m32-p13)/(m32-p34),real(p34-p13))
     &         +cln_coli((m32-p13)/(m32-p23),real(p23-p13))
     &         + .5d0*log(mm22/muir2)+ .5d0*log(mm42/muir2))
     &        - .5d0*(-cln_coli((m32-p13)/(m32-p34),real(p34-p13))
     &         +cln_coli((m32-p13)/(m32-p23),real(p23-p13))
     &         + .5d0*log(mm22/muir2)- .5d0*log(mm42/muir2))**2
     &        -2d0*pi2_6
     &        +cspcon_coli(-p24/(m32-p34),m32/(m32-p23),
     &        (-p24*m32)/((m32-p34)*(m32-p23)),
     &        ((m32-p34)*(m32-p23)+p24*m32)/((m32-p34)*(m32-p23)),
     &        -real(p24+m32-p34),1d0)

          D0ms2ir1_coli = D0ms2ir1_coli/((p13-m32)*p24)



        endif

      elseif(nsoft.eq.0)then

        if(m22.eq.cd0.and.m42.eq.cd0)then
* 1 soft singularities and 3 zero masses
*
*                   0
*    p12=m12 ----------------  p23
*                |  2   |
*      m12 small |1    3| m32
*                |   4  |
*    p14=m12 ----------------  p34
*                   0
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.0d0.or.m42.ne.0d0
     &        .or.p12.ne.m12.or.p14.ne.m12.or.
     &        p34.eq.m32.or.p23.eq.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli:  (4.32)',
     &            '    case with 0 soft singularities',
     &            '    and 2 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
          endif
#endif


c D0ms00ee
c Denner Dittmaier (4.32)

          D0ms2ir1_coli=3d0*pi2_6
     &     + cspcos_coli(-p24/(m32-p23),m32/(m32-p34),
     &     -real(p24+m32-p23),1d0)
     &     +0.5d0*(cln_coli(m12*coliminfscale2/(-p24),1d0)
     &     +cln_coli((m32-p23)/(m32-p13),real(p13-p23))
     &     +cln_coli((m32-p34)/(m32-p13),real(p13-p34)))**2
#ifdef SING
     &     + 0.5d0*colishiftms2
#endif

          D0ms2ir1_coli = D0ms2ir1_coli/((p13-m32)*p24)

        elseif(m22.eq.cd0.or.m42.eq.cd0)then
* 1 soft singularities and 1 zero masses
*
*                m22=m12
*          0 ----------------  p23
*                |  2   |
*      m12 small |1    3| m32
*                |   4  |
*    p14=m12 ----------------  p34
*    small           0
*

#ifdef CHECK
          if(m12.eq.cd0.or.p14.ne.m22.or.p12.ne.m42.or.
     &        .not.(m22.eq.m12.and.m42.eq.cd0.and.p34.ne.m32.or.
     &              m42.eq.m12.and.m22.eq.cd0.and.p23.ne.m32))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms1ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms1ir2_coli: (4.33)',
     &          '    case with 0 soft singularities and 1 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
          endif
#endif


c cD0ms0ee    newly implemented 6.11.2009
c Denner Dittmaier (4.33)

          if(m42.eq.cd0) then
            l13=m32-p13
            l23=m32-p23
            l34=m32-p34
            l24=-p24
          else
            l13=m32-p13
            l23=m32-p34
            l34=m32-p23
            l24=-p24
          endif

          D0ms2ir1_coli =
     &        2d0*cspenc_coli(1d0-l13/l23,real(l23-l13))
     &        +cspcos_coli(m32/l23,l24/l34,1d0,real(l24-l34))
     &        +pi2_6
     &        +0.5d0*(cln_coli(l13/l23,real(l13-l23))
     &        +cln_coli(l24/l34,real(l24-l34))
     &        +cln_coli(l13/(m12*coliminfscale2),-1d0))**2
#ifdef SING
     &        +0.5d0*colishiftms2
#endif


          D0ms2ir1_coli = D0ms2ir1_coli/(l13*l24)



        else
* 1 soft singularities and 0 zero masses
*
*                m22=m12
*          0 ----------------  p23
*                |  2   |
*      m12 small |1    3| m32
*                |   4  |
*          0 ----------------  p34
*                m42=m12
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.m42.ne.m12.or.
     &       p12.ne.cd0.or.p14.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir1_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir1_coli:  (4.34)',
     &            '    case with 0 soft singularities',
     &            '    and 0 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(6))then
            call ErrOut_coli('D0ms2ir1_coli',
     &          'new function called:  (4.34)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir1_coli:  (4.34)',
     &           '    case with 0 soft singularities and 0 zero masses',
     &           '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir1_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
              flag(6)=.false.
            endif
          endif
#endif

c Denner Dittmaier (4.34)
c new case   02.11.09

          mm12=m12*coliminfscale2
          l13=m32-p13
          l34=m32-p34
          l23=m32-p23

          D0ms2ir1_coli =
     &        pi2_6+2d0*cspenc_coli(1d0-l34/l13,real(l13-l34))
     &        +2d0*cspenc_coli(1d0-l23/l13,real(l13-l23))
     &        -cspcos_coli(-p24/l34,m32/l23,-real(l34+p24),1d0)
     &        +.5d0*(cln_coli(l23/l34,real(l23-l34)))**2
     &        -.5d0*(cln_coli(-p24/mm12,-1d0))**2
     &        +cln_coli(-p24/mm12,-1d0)*(
     &        cln_coli(l23/l13,real(l23-l13))
     &        +cln_coli(l34/l13,real(l34-l13)))
#ifdef SING
     &        -.5d0*colishiftms2
#endif

          D0ms2ir1_coli = D0ms2ir1_coli/(l13*p24)

        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms2ir1_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms2ir1_coli: case with more than'
     &        //'one soft singularity not implemented '
          write(nerrout_coli,111)' D0ms2ir1_coli: nsoft = ',nsoft
          write(nerrout_coli,111)' D0ms2ir1_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir1_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir1_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir1_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir1_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir1_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir1_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir1_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir1_coli: m42 = ',m42
        endif
#endif
      endif

      end

************************************************************************
      function D0ms2ir2_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 2 mass singularities and up to 2 soft singularities         *
*                                                                      *
*     assumes                                                          *
*     p12, p23, m12, m22, m32 small                                    *
*     p13, p24, p14=m42, p34=/=m42 finite                              *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23 small                      *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  30.04.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms2ir2_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 D0ms2ir2_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:8)
      data    flag /9*.true./
      save    flag
#endif

      complex*16 m2(4),mm12,mm22,mm32,q23,q34,q12,q14,l24,l34
      complex*16 cln_coli,cspenc_coli
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &        .or.ms42.eq.cd0.or.ps12.ne.cd0.or.ps23.ne.cd0
     &        .or.ps14.ne.ms42
     &        .or.ps24.eq.ms42.or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir2_coli called improperly:'
            write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      if (abs(m42-p24).lt.abs(m42)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms2ir2_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms2ir2_coli: p24 = m42'
          write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
        endif
        D0ms2ir2_coli = undefined
        if (abs(m42-p24).eq.0d0) return
      endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

      if(nsoft.eq.2)then

        if(m32.eq.cd0)then
* 2 soft singularities and 3 zero masses
*
*                   0
*          0 ----------------  0
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        m42 ----------------  p34
*                  m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.m32.ne.0d0.or.ms42.eq.cd0
     &        .or.p12.ne.cd0.or.p23.ne.cd0.or.ps34.eq.ms42.or.
     &        p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.36)',
     &            '    case with 2 soft singularities',
     &            '    and 3 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
            endif
          endif
#endif


c Beenakker et al., NPB653 (2003) 151
c Ellis, Ross, Terrano, NPB 178 (1981) 421
c Ellis, Zanderighi, JHEP 0802 (2008) 002    box7
c m^2 -> m42, p4^2 -> p34, s12 -> p13, s23 -> p24
c Denner Dittmaier (4.36)

          D0ms2ir2_coli =
#ifdef SING
     &        1.5d0*delta2ir
     &        -delta1ir*(2d0*cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &        +cln_coli(-p13/muir2,-1d0)
     &        -cln_coli((m42-p34)/sqrt(m42*muir2),-1d0))
#endif
     &        +2d0*cln_coli(-p13/muir2,-1d0)
     &        *cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &        -(cln_coli((m42-p34)/sqrt(m42*muir2),-1d0))**2
     &        -2d0*
     &        cspenc_coli((p34-p24)/(m42-p24),real(p34-p24))
     &        -4d0*pi2_6

          D0ms2ir2_coli = D0ms2ir2_coli/(p13*(p24-m42))




        else
* 2 soft singularities and 2 zero masses
*
*                   0
*          0 ----------------  m32 small
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*        m42 ----------------  p34
*                  m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.m42.eq.cd0.or.m32.eq.cd0.or.
     &        p12.ne.0d0.or.p34.eq.m42.or.p14.ne.m42.or.p23.ne.m32)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.37)',
     &            '    case with 2 soft singularities',
     &          '    and 2 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms2ir2_coli',
     &          'new function called:  (4.37)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.37)',
     &            '    case with 2 soft singularities',
     &            '    and 2 zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

c Beenakker et al., NPB653 (2003) 151
c Ellis, Zanderighi, JHEP 0802 (2008) 002             box11  m3 small
c Denner Dittmaier (4.37)

          D0ms2ir2_coli =
#ifdef SING
     &        delta2ir
     &        -delta1ir*
     &        (cln_coli(-p13/sqrt(muir2*m32*coliminfscale2),-1d0)
     &        +cln_coli((m42-p24)/sqrt(muir2*m42),-1d0))
     &        -0.25d0*colishiftms2
#endif
     &        +2d0*cln_coli(-p13/sqrt(muir2*m32*coliminfscale2),-1d0)
     &        *cln_coli((m42-p24)/sqrt(muir2*m42),-1d0)
     &        -(cln_coli((m42-p34)/sqrt(m42*m32*coliminfscale2),-1d0))
     &        **2
     &        -4d0*pi2_6

          D0ms2ir2_coli = D0ms2ir2_coli/(p13*(p24-m42))

        endif

      elseif(nsoft.eq.1)then

        if(m12.ne.cd0)then
          if(m32.ne.cd0)then

* 1 soft singularity and one zero mass opposite to finite mass
*
*                   0
*  small m12 ----------------  m32 small
*                |  2   |
*            m12 |1    3| m32
*                |   4  |
*        m42 ----------------  p34
*                   m42
*

#ifdef CHECK
            if(m12.eq.cd0.or.m22.ne.cd0.or.m32.eq.cd0.or.m42.eq.cd0.or.
     &          p12.ne.m12.or.p23.ne.m32.or.p14.ne.m42.or.ps34.eq.ms42)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.41)',
     &              '    case with 1 soft singularity ',
     &              '    and 1 zero mass opposite to finite mass: ',
     &              '    wrong arguments'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                write(nerrout_coli,*)' D0ms2ir2_coli: test= ',
     &              m12.eq.cd0,m22.ne.cd0,m32.eq.cd0,m42.eq.cd0,
     &              p12.ne.m12,p23.ne.m32,p14.ne.m42,p34.eq.cd0
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(3))then
              call ErrOut_coli('D0ms2ir2_coli',
     &            'new function called:  (4.41)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.41)',
     &              '    case with 1 soft singularity ',
     &              '    and 1 zero mass opposite to finite mass ',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                flag(3)=.false.
              endif
            endif
#endif

c new 31.07.09
c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349
c        (i) with m02=m22, m12 small
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c        box16 m2,m4 small, p22=m32
c Denner, Dittmaier (4.41)

          mm12=coliminfscale2*m12
          mm32=coliminfscale2*m32

          D0ms2ir2_coli =
#ifdef SING
     &        delta1ir*cln_coli(sqrt(mm12*mm32)/(-p13),1d0)
     &        -.25d0*colishiftms2
#endif
     &        +2d0*cln_coli(sqrt(mm12*mm32)/(-p13),1d0)
     &        *cln_coli(sqrt(muir2*m42)/(m42-p24),1d0)
     &        -1.5d0*pi2_6
     &        -(cln_coli(sqrt(mm32*m42)/(m42-p34),1d0))**2

            D0ms2ir2_coli=D0ms2ir2_coli/(p13*(p24-m42))

          else   ! m32=0

* 1 soft singularity and one zero mass opposite to finite mass
*
*                   0
* small  m12 ----------------  0
*                |  2   |
*            m12 |1    3| 0
*                |   4  |
*        m42 ----------------  p34
*                   m42
*

#ifdef CHECK
            if(m12.eq.cd0.or.m22.ne.cd0.or.m32.ne.cd0.or.m42.eq.cd0.or.
     &          p12.ne.m12.or.p23.ne.cd0.or.p14.ne.m42.or.p34.eq.m42)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.38)',
     &              '    case with 1 soft singularity ',
     &              '    and two adjacent zero masses: wrong arguments'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(4))then
              call ErrOut_coli('D0ms2ir2_coli',
     &          'new function called:  (4.38)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.38)',
     &              '    case with 1 soft singularity ',
     &              '    and 2 adjacent zero masses',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                flag(4)=.false.
              endif
            endif
#endif

c new case  31.07.09
c     according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box12
c     m3^2->0, p3^2=m4^2
c     p3^2 -> m42, p4^2 -> p34, m3^2 -> m12, m4^2-> m42
c     s23 -> p24, s12 -> p13
c     Denner Dittmaier (4.38)

          mm12 = m12*coliminfscale2

          D0ms2ir2_coli=
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli(-p13/sqrt(mm12*muir2),-1d0)
     &        +cln_coli((m42-p24)/(m42-p34),real(p34-p24)))
#endif
     &        -2d0*
     &        cspenc_coli(1d0-(m42-p34)/(m42-p24),real(p34-p24))
     &        +2d0*cln_coli(-p13/sqrt(mm12*muir2),-1d0)
     &        *cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &        -(cln_coli((m42-p34)/sqrt(m42*muir2),-1d0))**2
     &        -1.5d0*pi2_6

            D0ms2ir2_coli=D0ms2ir2_coli/(p13*(p24-m42))

          endif
        else !   if(m12.eq.cd0)then
          if(m32.ne.cd0)then

* 1 soft singularity and one zero mass opposite to small mass
*
*                  m22 small
*    p12=m22 ----------------  0
*                |  2   |
*             0  |1    3| m32=m22
*                |   4  |
*        m42 ----------------  p34
*                   m42
*

#ifdef CHECK
            if(m22.eq.cd0.or.m22.ne.m32.or.m12.ne.cd0.or.m42.eq.cd0.or.
     &          p12.ne.m22.or.p23.ne.cd0.or.p14.ne.m42.or.p34.eq.m42)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.40)',
     &              '    case with 1 soft singularity ',
     &              '    and 1 zero mass opposite to small mass',
     &              '    wrong arguments'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                write(nerrout_coli,*)' D0ms2ir2_coli: test= ',
     &              m22.eq.cd0,m22.ne.m32,m12.ne.cd0,m42.eq.cd0,
     &              p12.ne.m22,p23.ne.cd0,p14.ne.m42,p34.eq.m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(5))then
              call ErrOut_coli('D0ms2ir2_coli',
     &            'new function called:  (4.40)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.40)',
     &              '    case with 1 soft singularity ',
     &              '    and 1 zero mass opposite to small mass',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                flag(5)=.false.
              endif
            endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (ic)
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box16 m22,m42 small,  p22=m32
c Denner Dittmaier (4.40)

            D0ms2ir2_coli = (
     &          -2d0*cln_coli((m42-p24)/(sqrt(m22*coliminfscale2*m42)),
     &          -1d0)
     &          *cln_coli((-p13)/sqrt(m22*coliminfscale2*muir2),-1d0)
     &          +(cln_coli((m42-p34)/sqrt(m22*coliminfscale2*m42),-1d0))
     &          **2
     &          + pi2_6
     &          + 2d0*cspenc_coli((p24-p34)/(p24-m42),real(p34-m42)) )
#ifdef SING
     &          +cln_coli((m42-p24)/(sqrt(m22*coliminfscale2*m42)),-1d0)
     &          *delta1ir
     &          - 0.25d0*colishiftms2
#endif

            D0ms2ir2_coli=D0ms2ir2_coli/((p24-m42)*(-p13))

          else   ! m32=0, m12=0

* 1 soft singularity and one zero mass opposite to finite mass
*
*                  m22 small
*        m22 ----------------  m22
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        m42 ----------------  p34
*                   m42
*

#ifdef CHECK
            if(m12.ne.cd0.or.m22.eq.cd0.or.m32.ne.cd0.or.m42.eq.cd0.or.
     &          p12.ne.m22.or.p23.ne.m22.or.p14.ne.m42.or.p34.eq.m42)
     &          then
              call setErrFlag_coli(-10)
              call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.39)',
     &              '    case with 1 soft singularity ',
     &              '    and two opposite zero masses: wrong arguments'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
              endif
            endif
#endif

#ifdef NEWCHECK
            if(flag(6))then
              call ErrOut_coli('D0ms2ir2_coli',
     &            'new function called: (4.39)',
     &            errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,100)' D0ms2ir2_coli:  (4.39)',
     &              '    case with 1 soft singularity ',
     &              '    and 2 opposite zero masses ',
     &              '    not yet tested in physical process'
                write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
                write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
                write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
                write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
                write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
                write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
                write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
                write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
                write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
                write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
                flag(6)=.false.
              endif
            endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (iic)
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box15 m22 small,  p22=m22
c Denner Dittmaier (4.39)

            D0ms2ir2_coli =
     &          cln_coli((m42-p24)/sqrt(m22*coliminfscale2*m42),-1d0)*
     &          (cln_coli((m42-p24)/sqrt(m22*coliminfscale2*m42),-1d0)
     &          -log(muir2/m42)
     &          +2d0*cln_coli(p13/(p34-m42),real(p34-m42)) )
     &          + pi2_6
#ifdef SING
     &          -cln_coli((m42-p24)/sqrt(m22*coliminfscale2*m42),-1d0)*
     &          delta1ir
     &          + 0.25d0*colishiftms2
#endif
            D0ms2ir2_coli = D0ms2ir2_coli/((p24-m42)*p13)

          endif
        endif

      elseif(nsoft.eq.0)then

        if(m32.eq.cd0)then
* no soft singularity and 1 zero mass
*
*                m22 small
*          0 ----------------  m22
*                |  2   |
*            m22 |1    3| 0
*                |   4  |
*        m42 ----------------  p34
*                  m42
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.m42.eq.cd0.or.
     &        p34.eq.m42.or.p23.ne.m12.or.p14.ne.m42.or.p12.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.42)',
     &            '    case with 0 soft singularity and 1 zero mass:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(7))then
            call ErrOut_coli('D0ms2ir2_coli',
     &          'new function called: (4.42)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.42)',
     &            '    case with 0 soft singularity and 1 zero mass',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
              flag(7)=.false.
            endif
          endif
#endif

c new case    02.11.09
c Denner Dittmaier (4.42)

          mm22=m22*coliminfscale2

          D0ms2ir2_coli=-0.5d0*pi2_6
     &        +cln_coli((m42-p24)/sqrt(mm22*m42),-1d0)*(
     &        cln_coli((m42-p24)/sqrt(mm22*m42),-1d0)
     &        +2d0*cln_coli(-p13/mm22,-1d0)
     &        -2d0*cln_coli((m42-p34)/sqrt(mm22*m42),-1d0))
#ifdef SING
     &        +.75d0*colishiftms2
#endif

          D0ms2ir2_coli=D0ms2ir2_coli/(p13*(p24-m42))

        else ! m32.ne.0
* 0 soft singularity and 1 zero mass
*
*                   m22 small
*          0 ----------------  0
*                |  2   |
*            m22 |1    3| m22
*                |   4  |
*        m42 ----------------  p34
*                   m42
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.m32.ne.m12.or.m42.eq.cd0.or.
     &         p12.ne.cd0.or.p23.ne.cd0
     &        .or.(p14.ne.m42.and.p34.ne.m42))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.43)',
     &            '    case with 0 soft singularity and 0 zero mass:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(8))then
            call ErrOut_coli('D0ms2ir2_coli',
     &          'new function called:  (4.43)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir2_coli:  (4.43)',
     &            '    case with 0 soft singularity and 0 zero mass',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
              flag(8)=.false.
            endif
          endif
#endif

          mm22=m22*coliminfscale2
          if(p14.eq.m42)then
            q23=p23
            q34=p34
            q12=p12
            q14=p14
          else
            q23=p12
            q34=p14
            q12=p23
            q14=p34
          endif

c new case   2.11.09
c Denner Dittmaier (4.43)

          l34=m42-q34
          l24=m42-p24

          D0ms2ir2_coli=2d0*cspenc_coli(1d0-l34/l24,real(l24-l34))
     &        -2d0*cln_coli(l24/sqrt(mm22*m42),-1d0)
     &        *cln_coli(-p13/mm22,-1d0)
     &        +(cln_coli(l34/sqrt(mm22*m42),-1d0))**2
     &        +2.5d0*pi2_6
#ifdef SING
     &        - 0.75d0*colishiftms2
#endif

          D0ms2ir2_coli=D0ms2ir2_coli/(l24*p13)

        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms2ir2_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' D0ms2ir2_coli: case with more than 2 soft ',
     &        'singularities not implemented '
          write(nerrout_coli,111)' D0ms2ir2_coli: nsoft = ',nsoft
          write(nerrout_coli,111)' D0ms2ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir2_coli: m42 = ',m42
        endif
#endif
      endif

      end



************************************************************************
      function D0ms2ir3_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     mass-singular 4-point function                                   *
*     with two collinear and up to 3 soft singularities                *
*     p12, p23, m12, m22, m32   small                                  *
*     p14=m42, p34, p13, p24    finite                                 *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23 small                      *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*       p14=m42  ---------------------  p34                            *
*                         m42                                          *
*                                                                      *
************************************************************************
*     22.08.08 Ansgar Denner        last changed  02.11.09             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms2ir3_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:6)
      data    flag /7*.true./
      save    flag
#endif

      complex*16 mm12,mm22,mm32,m2(4)
      complex*16 cln_coli
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))
#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &        .or.ms42.eq.cd0.or.ps12.ne.cd0.or.ps23.ne.cd0
     &        .or.ps34.ne.ms42.or.ps14.ne.ms42
     &        .or.ps24.eq.ms42.or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*)' D0ms2ir3_coli called improperly:'
            write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      if (abs(m42-p24).lt.abs(m42)*calacc) then
        call setErrFlag_coli(-7)
        call ErrOut_coli('D0ms2ir3_coli',' invariant on resonance'//
     &      ' use width or other regularization',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)' D0ms2ir3_coli: p24 = m42'
          write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
          write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
        endif
        D0ms2ir3_coli = undefined
        if (abs(m42-p24).eq.0d0) return
      endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

      if(nsoft.eq.3)then

* four soft singularities
*
*                   0
*          0 ----------------  0
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        m42 ----------------  m42
*                   m42
*

#ifdef CHECK
        if(m12.ne.cd0.or.m22.ne.0d0.or.m32.ne.0d0.or.m42.eq.cd0
     &      .or.p12.ne.cd0.or.p23.ne.cd0.or.p34.ne.m42.or.p14.ne.m42)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir3_coli:  (4.45)',
     &          '    case with 3 soft singularities: ',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(1))then
          call ErrOut_coli('D0ms2ir3_coli',
     &        'new function called:  (4.45)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir3_coli:  (4.45)',
     &          '    case with 3 soft singularities',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            flag(1)=.false.
          endif
        endif
#endif

c Beenakker, Kuiff, van Neerven, Smith, PRD 40 (1989) 54
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    box6
c        s12->p13, s23->p24
c Denner, Dittmaier (4.45)
        D0ms2ir3_coli =
#ifdef SING
     &      2d0*delta2ir
     &      -delta1ir*(2d0*cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &                 +cln_coli(-p13/muir2,-1d0))
#endif
     &      +2d0*cln_coli((m42-p24)/sqrt(m42*muir2),-1d0)
     &      *cln_coli(-p13/muir2,-1d0)
     &      -5d0*pi2_6

        D0ms2ir3_coli = D0ms2ir3_coli/(p13*(p24-m42))

      elseif(nsoft.eq.2)then

        if(m12.eq.cd0.and.m32.eq.cd0)then
* 2 soft singularities and two opposite zero masses
*
*                m22 small
*        m22  ---------------- m22
*                |  2   |
*             0  |1    3| 0
*                |   4  |
*        m42 ----------------  m42
*                  m42
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.eq.0d0.or.m32.ne.0d0.or.m42.eq.cd0
     &        .or.p12.ne.m22.or.p23.ne.m22.or.p34.ne.m42.or.p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.47)',
     &            '    case with 2 soft singularities',
     &            '    and 2 opposite zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms2ir3_coli',
     &           'new function called:  (4.47)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.47)',
     &            '    case with 2 soft singularities',
     &            '    and 2 opposite zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349  (iiia)
c Ellis, Zanderighi, JHEP 0802 (2008) 002    box14 m2 small
c Denner, Dittmaier (4.47)
          D0ms2ir3_coli =
#ifdef SING
     &        2d0*cln_coli((m42-p24)/(sqrt(m22*m42)*coliminfscale),-1d0)
     &        *delta1ir
#endif
     &        -2d0*cln_coli((m42-p24)/(sqrt(m22*m42)*coliminfscale),
     &        -1d0)
     &        *cln_coli(-p13/muir2,-1d0)
          D0ms2ir3_coli = D0ms2ir3_coli/(p13*(m42-p24))

        else
* 2 soft singularities and two adjacent zero masses
*
*                   0
*          0  ---------------- m32
*                |  2   |
*             0  |1    3| m32 small
*                |   4  |
*        m42 ----------------  m42
*                  m42
*

#ifdef CHECK
          if(m22.ne.cd0.or.m42.eq.cd0.or.
     &        .not.(m32.eq.cd0.and.m12.ne.cd0.or.
     &        m32.ne.cd0.and.m12.eq.cd0).or.
     &        p12.ne.m12.or.p23.ne.m32.or.p34.ne.m42.or.p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.46)',
     &            '    case with 2 soft singularities',
     &            '    and two adjacent zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms2ir3_coli',
     &          'new function called:  (4.46)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.46)',
     &            '    case with 2 soft singularities'//
     &            ' and 2 adjacent zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif

          if(m12.ne.cd0)then
            mm32=m12*coliminfscale2
          else
            mm32=m32*coliminfscale2
          endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002
c        box11 p32=m42, m3 small
c Denner, Dittmaier (4.46)
          D0ms2ir3_coli =
#ifdef SING
     &        delta2ir
     &        -delta1ir*(cln_coli(-p13/sqrt(muir2*mm32),-1d0)
     &        +cln_coli((m42-p24)/sqrt(muir2*m42),-1d0))
#endif
     &        +2d0*cln_coli(-p13/sqrt(muir2*mm32),-1d0)
     &        *cln_coli((m42-p24)/sqrt(muir2*m42),-1d0)
     &        -2.5d0*pi2_6

          D0ms2ir3_coli = D0ms2ir3_coli/(p13*(p24-m42))

        endif

      elseif(nsoft.eq.1)then

        if(m12.ne.cd0.and.m32.ne.cd0)then
* 1 soft singularities opposite to finite mass m4
*
*                   0
*        m12  ---------------- m32
*                |  2   |
*           m12  |1    3| m32
*                |   4  |
*        m42 ----------------  m42
*                  m42
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.0d0.or.m32.eq.0d0.or.m42.eq.cd0
     &        .or.p12.ne.m12.or.p23.ne.m32.or.p34.ne.m42.or.p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.49)',
     &            '    case with 1 soft singularity ',
     &            '    opposite to finite mass: ',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(4))then
            call ErrOut_coli('D0ms2ir3_coli',
     &          'new function called:  (4.49)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.49)',
     &            '    case with 1 soft singularity ',
     &            '    opposite to finite mass ',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
              flag(4)=.false.
            endif
          endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349
c        (i) with m02=m32=m22, m12 small
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c        box16 m2,m4 small, p22=m32=p32
c Denner, Dittmaier (4.49)
          D0ms2ir3_coli = cd0

          mm12=coliminfscale2*m12
          mm32=coliminfscale2*m32

          D0ms2ir3_coli =
#ifdef SING
     &        delta1ir*cln_coli(sqrt(mm12*mm32)/(-p13),1d0)
#endif
     &        +2d0*cln_coli(sqrt(mm12*mm32)/(-p13),1d0)
     &        *cln_coli(sqrt(muir2*m42)/(m42-p24),1d0)



          D0ms2ir3_coli = D0ms2ir3_coli/(p13*(p24-m42))

        else
* 1 soft singularity adjacent to finite mass m42
*
*                m22 small
*         0  ---------------- m22
*                |  2   |
*           m22  |1    3| 0
*                |   4  |
*        m42 ----------------  m42
*                  m42
*

#ifdef CHECK
          if(m22.eq.cd0.or.m42.eq.cd0.or.
     &        .not.(m32.eq.m22.and.m12.eq.cd0.and.
     &              p12.eq.m22.and.p23.eq.cd0.or.
     &              m12.eq.m22.and.m32.eq.cd0.and.
     &              p23.eq.m22.and.p12.eq.cd0).or.
     &        p34.ne.m42.or.p14.ne.m42)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.48)',
     &            '    case with 1 soft singularity ',
     &            '    adjacent to finite masses: ',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(5))then
            call ErrOut_coli('D0ms2ir3_coli',
     &          'new function called:  (4.48)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms2ir3_coli:  (4.48)',
     &            '    case with 1 soft singularity ',
     &            '    adjacent to finite mass',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
              flag(5)=.false.
            endif
          endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349
c        (i) with m22=0 m32=m42, m12 small
c Denner, Dittmaier (4.48)

          mm22=coliminfscale2*m22

          D0ms2ir3_coli =
#ifdef SING
     &        delta1ir*cln_coli(sqrt(mm22*m42)/(m42-p24),1d0)
     &        + .5d0*colishiftms2
#endif
     &        +2d0*cln_coli(sqrt(mm22*m42)/(m42-p24),1d0)
     &        *cln_coli(-sqrt(muir2*mm22)/p13,1d0)
     &        - 1.5d0*pi2_6

          D0ms2ir3_coli = D0ms2ir3_coli/(p13*(p24-m42))

        endif

      elseif(nsoft.eq.0)then

* no soft singularity
*
*                  m22 small
*          0 ----------------  0
*                |  2   |
*            m22 |1    3| m22
*                |   4  |
*        m42 ----------------  m42
*                  m42
*

#ifdef CHECK
        if(m12.ne.m22.or.m12.ne.m32.or.m42.eq.cd0.or.m12.eq.cd0.or.
     &      p12.ne.cd0.or.p23.ne.cd0.or.p34.ne.m42.or.p14.ne.m42)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms2ir3_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir3_coli:  (4.50)',
     &          '    case with no soft singularity: ',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(6))then
          call ErrOut_coli('D0ms2ir3_coli',
     &        'new function called:  (4.50)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms2ir3_coli:  (4.50)',
     &          '    case with no soft singularity',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms2ir3_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
            flag(6)=.false.
          endif
        endif
#endif

c new case       2.11.09
c Denner, Dittmaier (4.50)

        mm22 =m22*coliminfscale2

        D0ms2ir3_coli =
     &      2d0*cln_coli(-p13/mm22,-1d0)
     &      * cln_coli((m42-p24)/sqrt(mm22*m42),-1d0)
     &      -3d0*pi2_6
#ifdef SING
     &      + colishiftms2
#endif

        D0ms2ir3_coli = D0ms2ir3_coli/(p13*(p24-m42))


#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms2ir3_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,100)
     &        ' D0ms2ir3_coli: case with more than 3'
     &        //'soft singularities not implemented '
          write(nerrout_coli,111)' D0ms2ir3_coli: nsoft = ',nsoft
          write(nerrout_coli,111)' D0ms2ir3_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms2ir3_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms2ir3_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms2ir3_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms2ir3_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms2ir3_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms2ir3_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms2ir3_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms2ir3_coli: m42 = ',m42
        endif
#endif
      endif

      end


************************************************************************
      function D0ms3ir2_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     4-point function                                                 *
*     with 3 mass singularities and 2 soft singularities               *
*                                                                      *
*     assumes                                                          *
*     p12, p23, p14, m12, m22, m32, m42 small                          *
*     p13, p24, p34 finite                                             *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23 small                      *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*     p14 small  ---------------------  p34                            *
*                     m42 small                                        *
*                                                                      *
************************************************************************
*     05.08.08 Ansgar Denner        last changed  13.05.10             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms3ir2_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 D0ms3ir2_check
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      complex*16 l13,l23,l24,x
      real*8     ix
      logical flag(0:10)
      data    flag /11*.true./
      save    flag
#endif

      complex*16 q13,q24,mm12,mm22,mm42,mm32,m2(4),x1
      complex*16 cln_coli,cspcon_coli,cspenc_coli
      real*8     ix1
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      D0ms3ir2_coli = undefined

#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &        .or.ms42.ne.cd0.or.ps12.ne.cd0.or.ps23.ne.cd0
     &        .or.ps34.eq.cd0.or.ps14.ne.cd0
     &        .or.ps24.eq.cd0.or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0
     &        .or.ps13.eq.cd0.or.ps24.eq.cd0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms3ir2_coli called improperly:'
            write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                soft(i)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                if(soft(i)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo


      if(nsoft.eq.2)then

        if(m32.eq.cd0.and.m42.eq.cd0)then
* 2 soft singularities and 4 zero masses
*
*                   0
*          0 ----------------  0
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*          0 ----------------  p34
*                   0
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.m32.ne.0d0.or.m42.ne.cd0
     &        .or.p12.ne.cd0.or.p23.ne.cd0.or.p34.eq.cd0.or.p14.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.60)',
     &          '    case with 2 soft singularities and 4 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif


c according to Ellis, Zanderighi, JHEP 0802 (2008) 002  Box2
c and Bern, Dixon, Kosower, Nucl. Phys. B412 (1994) 751
c Ellis, Ross, Terrano, NPB 178 (1981) 421
c Denner Dittmaier (4.60)

          D0ms3ir2_coli =
#ifdef SING
     &        2d0*delta2ir
     &        -2d0*delta1ir*(cln_coli(-p13/muir2,-1d0)
     &        +cln_coli(-p24/muir2,-1d0)
     &        -cln_coli(-p34/muir2,-1d0))
#endif
     &        -(cln_coli(-p34/muir2,-1d0))**2
     &        +2d0*(
     &         cln_coli(-p13/muir2,-1d0)*cln_coli(-p24/muir2,-1d0)
C     &        -cspcon_coli(-p34,-1d0/p13,p34/p13,1d0-p34/p13,-1d0,1d0)
C     &        -cspcon_coli(-p34,-1d0/p24,p34/p24,1d0-p34/p24,-1d0,1d0)
     &        -cspenc_coli(1d0-p34/p13,real(p34-p13))
     &        -cspenc_coli(1d0-p34/p24,real(p34-p24))
     &        )
     &        -4d0*pi2_6

          D0ms3ir2_coli = D0ms3ir2_coli/(p13*p24)

        elseif(m32.eq.cd0.or.m42.eq.cd0)then
* 2 soft singularities and 3 zero masses
*
*                   0
*          0 ----------------  m32
*                |  2   |
*              0 |1    3| m32
*                |   4  |
*          0 ----------------  p34
*                   0
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.p12.ne.0d0.or.p34.eq.cd0
     &        .or.p14.ne.m42.or.p23.ne.m32.or.m32*m42.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli: (4.61)',
     &          '    case with 2 soft singularities and 3 zero masses:',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(2))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called: (4.61)',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli: (4.61)',
     &          '    case with 2 soft singularities and 3 zero masses:',
     &          '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(2)=.false.
            endif
          endif
#endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002  box7  m4->0
c from Beenakker et al, NPB 653 (2003) 151
c Denner Dittmaier (4.61)

          if(m42.eq.cd0)then
            mm32=m32*coliminfscale2
            q24=p24
            q13=p13
          else
            mm32=m42*coliminfscale2
            q24=p13
            q13=p24
          endif

          D0ms3ir2_coli =
#ifdef SING
     &        1.5d0*delta2ir
     &        -delta1ir*(2d0*cln_coli(-q13/sqrt(muir2*mm32),-1d0)
     &        +cln_coli(-q24/muir2,-1d0)
     &        -cln_coli(-p34/sqrt(muir2*mm32),-1d0))
     &        -0.25*colishiftms2
#endif
     &        +2d0*cln_coli(-q13/sqrt(muir2*mm32),-1d0)
     &        *cln_coli(-q24/muir2,-1d0)
     &        -(cln_coli(-p34/sqrt(muir2*mm32),-1d0))**2
     &        -2d0*
     &        cspenc_coli(1d0-p34/q13,real(p34-q13))
     &        -4d0*pi2_6

          D0ms3ir2_coli = D0ms3ir2_coli/(p13*p24)

        else  ! m32.ne.cd0.and.m42.ne.cd0
* 2 soft singularities and 2 zero masses
*
*                   0
*          0 ----------------  m32
*                |  2   |
*              0 |1    3| m32 small
*                |   4  |
*        m42 ----------------  p34
*                  m42 small
*

#ifdef CHECK
          if(m12.ne.cd0.or.m22.ne.0d0.or.p12.ne.0d0.or.p34.eq.cd0
     &        .or.p14.ne.m42.or.p23.ne.m32.or.m32*m42.eq.0d0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.62)',
     &            '    case with 2 soft singularities',
     &            '    and 2 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.62)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.62)',
     &          '    case with 2 soft singularities and 2 zero masses',
     &          '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box11 m32,m42 -> 0
c Denner Dittmaier (4.62)

          D0ms3ir2_coli =
#ifdef SING
     &        delta2ir
     &        -delta1ir*(cln_coli(-p13/sqrt(muir2*m32*coliminfscale2),
     &        -1d0)
     &        +cln_coli(-p24/sqrt(muir2*m42*coliminfscale2),-1d0))
     &        -.5d0*colishiftms2
#endif
     &        +2d0*cln_coli(-p13/sqrt(muir2*m32*coliminfscale2),-1d0)
     &        *cln_coli(-p24/sqrt(muir2*m42*coliminfscale2),-1d0)
     &        -(cln_coli(-p34/(sqrt(m32*m42)*coliminfscale2),-1d0))**2
     &        -4d0*pi2_6

          D0ms3ir2_coli = D0ms3ir2_coli/(p13*p24)

        endif

      elseif(nsoft.eq.1)then

        if(m32.eq.cd0.and.m42.eq.cd0.and.(m12.eq.cd0.or.m22.eq.cd0))then
* 1 soft singularity and three zero masses
*
*                   0
*         m2 ----------------  0
*                |  2   |
*    small    m2 |1    3| 0
*                |   4  |
*         m2 ----------------  p34
*                   0
*

#ifdef CHECK
          if(.not.(
     &        (m12.eq.p12.and.m12.eq.p14.and.p23.eq.cd0.and.p34.ne.cd0
     &        .and.m22.eq.cd0.and.m32.eq.cd0.and.m42.eq.cd0).or.
     &        (m22.eq.p12.and.m22.eq.p23.and.p14.eq.cd0.and.p34.ne.cd0
     &        .and.m12.eq.cd0.and.m42.eq.cd0.and.m32.eq.cd0)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.63)',
     &            '    case with 1 soft singularity ',
     &            '    and three zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(4))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.63)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.63)',
     &            '    case with 1 soft singularity and 3 zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(4)=.false.
            endif
          endif
#endif

          if(m12.ne.cd0)then
            mm12=m12*coliminfscale2
            q13=p13
            q24=p24
          else
            mm12=m22*coliminfscale2
            q13=p24
            q24=p13
          endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002   box9  p32=m2 -> 0
c p2^2 -> p34, s12 -> p24, s23 -> p13,  p3^2=m42 -> m12  small
c Denner Dittmaier (4.63)

          D0ms3ir2_coli=
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli(-q13/sqrt(mm12*muir2),-1d0)
     &        +cln_coli(q24/p34,-1d0))
     &        +.25d0*colishiftms2
#endif
     &        +2d0*cspenc_coli(1d0-q24/p34,real(q24-p34))
     &        +(cln_coli(-q13/sqrt(mm12*muir2),-1d0)
     &        +cln_coli(q24/p34,real(p34-q24)))**2
     &        +pi2_6

          D0ms3ir2_coli=D0ms3ir2_coli/(q13*q24)

#ifdef CHECK
#endif

        elseif((m12.eq.cd0.and.m42.eq.cd0).or.
     &         (m22.eq.cd0.and.m32.eq.cd0))then
* 1 soft singularity and two adjacent zero masses
*
*                   0
*         m2 ----------------  0
*                |  2   |
*       small m2 |1    3| 0
*                |   4  |
*          0 ----------------  p34
*                   m2
*

#ifdef CHECK
          if(.not.(
     &        (m12.eq.p12.and.m12.eq.m42.and.p23.eq.cd0.and.p34.ne.cd0
     &        .and.m22.eq.cd0.and.m32.eq.cd0.and.p14.eq.cd0).or.
     &        (m22.eq.p12.and.m22.eq.m32.and.p14.eq.cd0.and.p34.ne.cd0
     &        .and.m12.eq.cd0.and.m42.eq.cd0.and.p23.eq.cd0)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.64)',
     &            '    case with 1 soft singularity ',
     &            '    and two adjacent zero masses:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(5))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.64)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.64)',
     &            '    case with 1 soft singularity ',
     &            '    and 2 adjacent zero masses ',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(5)=.false.
            endif
          endif
#endif

          if(m12.ne.cd0)then
            mm12=m12*coliminfscale2
            q13=p13
            q24=p24
          else
            mm12=m22*coliminfscale2
            q13=p24
            q24=p13
          endif

c according to Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box12 m3=m4 small p32=0
c Denner Dittmaier (4.64)

          D0ms3ir2_coli=
#ifdef SING
     &        .5d0*delta2ir
     &        -delta1ir*(cln_coli(-q24/sqrt(mm12*muir2),-1d0)
     &        +cln_coli(q13/p34,real(p34-q13)))
     &        +.25d0*colishiftms2
#endif
     &        -2d0*(
     &        cspcon_coli(-p34,-1d0/q24,p34/q24,1d0-p34/q24,-1d0,1d0)
     &        +cspcon_coli(-p34,-1d0/q13,p34/q13,1d0-p34/q13,-1d0,1d0)
     &        )
     &        +2d0*cln_coli(-q24/sqrt(mm12*muir2),-1d0)
     &        *cln_coli(-q13/sqrt(mm12*muir2),-1d0)
     &        -(cln_coli(-p34/sqrt(mm12*muir2),-1d0))**2
     &        -pi2_6

          D0ms3ir2_coli=D0ms3ir2_coli/(q13*q24)

        elseif((m12.eq.cd0.and.m32.eq.cd0).or.
     &         (m22.eq.cd0.and.m42.eq.cd0))then
* 1 soft singularity and two opposite zero masses
*
*                   m22 small
*        m22 ----------------  m22
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        m42 ----------------  p34
*                   m42 small
*

#ifdef CHECK
          if(.not.(
     &        (m22.eq.p12.and.m22.eq.p23.and.p14.eq.m42.and.p34.ne.cd0
     &        .and.m12.eq.cd0.and.m32.eq.cd0).or.
     &        (m12.eq.p12.and.m12.eq.p14.and.p23.eq.m32.and.p34.ne.cd0
     &        .and.m22.eq.cd0.and.m42.eq.cd0)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.65)',
     &            '    case with 1 soft singularity '//
     &            'and two opposite zero masses: ',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif


          if(m22.ne.cd0)then
            mm22=m22*coliminfscale2
            mm42=m42*coliminfscale2
            q13=p13
            q24=p24
          else
            mm22=m12*coliminfscale2
            mm42=m32*coliminfscale2
            q13=p24
            q24=p13
          endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 iid
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box15 m22,m42 small,  p22=m22
c Denner Dittmaier (4.65)
          D0ms3ir2_coli=
     &        cln_coli(-q24/sqrt(mm22*mm42),-1d0)*
     &        (cln_coli(-q24/sqrt(mm22*mm42),-1d0)
     &        +cln_coli(mm42/muir2,0d0)
#ifdef SING
     &        -delta1ir
#endif
     &        +2d0*cln_coli(q13/p34,real(p34-q13)))
     &        + pi2_6

          D0ms3ir2_coli=D0ms3ir2_coli/(q13*q24)

        elseif(m12.eq.cd0.or.m22.eq.cd0)then
* 1 soft singularity and one zero mass
*
*                   m22 small
*        m22 ----------------  0
*                |  2   |
*              0 |1    3| m22
*                |   4  |
*        m42 ----------------  p34
*                   m42 small
*

#ifdef CHECK
          if(.not.(
     &        (m22.eq.p12.and.m22.eq.m32.and.p14.eq.m42.and.p34.ne.cd0
     &        .and.m12.eq.cd0.and.p23.eq.cd0).or.
     &        (m12.eq.p12.and.m12.eq.m42.and.p23.eq.m32.and.p34.ne.cd0
     &        .and.m22.eq.cd0.and.p14.eq.cd0)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.66)',
     &          '    case with 1 soft singularity and one zero mass: ',
     &          '    wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif


          if(m22.ne.cd0)then
            mm22=m22*coliminfscale2
            mm42=m42*coliminfscale2
            q13=p13
            q24=p24
          else
            mm22=m12*coliminfscale2
            mm42=m32*coliminfscale2
            q13=p24
            q24=p13
          endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349 (id)
c    cD0ireem
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c              box16 m22,m42 small,  p22=0, m32=m22
c Denner Dittmaier (4.66)
          D0ms3ir2_coli= 2d0*cln_coli(-q24/sqrt(mm22*mm42),-1d0)
     &     *(cln_coli(-q13/sqrt(mm22*muir2),-1d0)
#ifdef SING
     &        -.5d0*delta1ir
#endif
     &        )
     &        - (cln_coli(-p34/sqrt(mm22*mm42),-1d0))**2 - pi2_6
     &        - 2d0*
     &        cspenc_coli(1d0-p34/q24,real(p34-q24))

          D0ms3ir2_coli=D0ms3ir2_coli/(q13*q24)

#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms3ir2_coli',' branch not found',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms3ir2: inconsistency 1 soft'
          endif
#endif
        endif

      elseif(nsoft.eq.0)then

        if(m32.eq.cd0.and.m42.eq.cd0)then
* no soft singularity and two zero masses
*
*                   m2 small
*          0 ----------------  m2
*                |  2   |
*             m2 |1    3| 0
*                |   4  |
*         m2 ----------------  p34
*                   0
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.
     &        p34.eq.cd0.or.p23.ne.m12.or.p14.ne.m12.or.p12.ne.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &        errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,111)' D0ms3ir2_coli:  (4.67)',
     &            '    case with 0 soft singularity ',
     &            '    and 2 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
c eeqqg
          if(flag(8))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.67)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:  (4.67)',
     &            '    case with 0 soft singularity and 2 zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(8)=.false.
            endif
          endif
#endif

c cD0ms00eee
c Denner Dittmaier (4.67)

#ifdef SING
#endif

          mm12=m12*coliminfscale2

          D0ms3ir2_coli= -2d0*pi2_6
     &        -(cln_coli(-p24,-1d0)+cln_coli(-p13,-1d0)
     &        -cln_coli(-p34,-1d0)-cln_coli(mm12,-1d0))**2
#ifdef SING
     &        -colishiftms2
#endif

          D0ms3ir2_coli=-D0ms3ir2_coli/(p13*p24)




        elseif(m32.eq.cd0.or.m42.eq.cd0)then
* 0 soft singularity and 1 zero mass
*
*                   m2 small
*          0 ----------------  m2
*                |  2   |
*             m2 |1    3| 0
*                |   4  |
*          0 ----------------  p34
*                   m2
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.
     &        p12.ne.cd0.or.p34.eq.cd0.or.p34.eq.m12
     &        .or..not.(p23.eq.m22.and.m32.eq.cd0
     &                  .and.m12.eq.m42.and.p14.eq.cd0
     &              .or.p14.eq.m12.and.m42.eq.cd0
     &                  .and.m22.eq.m32.and.p23.eq.cd0))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:   (4.68)',
     &            '    case with 0 soft singularity ',
     &            '    and 1 zero mass: wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(9))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.68)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:   (4.68)',
     &            '    case with 0 soft singularity and 1 zero mass',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(9)=.false.
            endif
          endif
#endif

          if(m42.ne.cd0)then
            mm42=m42*coliminfscale2
            q13=p13
            q24=p24
          else
            mm42=m32*coliminfscale2
            q13=p24
            q24=p13
          endif

c new case  30.07.09
c Denner Dittmaier (4.68)

          D0ms3ir2_coli= (cln_coli(-q24/mm42,-1d0))**2
     &        +2d0*cln_coli(-q24/mm42,-1d0)
     &        *cln_coli(q13/p34,real(p34-q13))
     &        -2d0*cspenc_coli(1d0-p34/q13,real(p34-q13))
#ifdef SING
     &        +colishiftms2
#endif

          D0ms3ir2_coli=D0ms3ir2_coli/(p13*p24)

        elseif(m12.ne.cd0.and.m32.ne.cd0.and.
     &         m22.ne.cd0.and.m42.ne.cd0)then
* 0 soft singularity and 0 zero masses
*
*                   m2 small
*         0 ----------------  0
*                |  2   |
*             m2 |1    3| m2
*                |   4  |
*         0 ----------------  p34
*                   m2
*

#ifdef CHECK
          if(m12.eq.cd0.or.m22.ne.m12.or.m32.ne.m12.or.m42.ne.m12.or.
     &        p12.ne.cd0.or.p23.ne.cd0.or.p14.ne.cd0.or.p34.eq.cd0)
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms3ir2_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:   (4.69)',
     &            '    case with 0 soft singularity ',
     &            '    and 0 zero masses: wrong arguments'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(10))then
            call ErrOut_coli('D0ms3ir2_coli',
     &          'new function called:  (4.69)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms3ir2_coli:   (4.69)',
     &            '    case with 0 soft singularity and 0 zero masses',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms3ir2_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
              flag(10)=.false.
            endif
          endif
#endif

c new case   04.12.09
c Denner Dittmaier (4.69)

          D0ms3ir2_coli=
     &        (cln_coli(-p24/(m42*coliminfscale2),-1d0)
     &        +cln_coli(p13/p34,real(p34-p13)))**2
     &        +2d0*(cspenc_coli(1d0-p13/p34,real(p13-p34))
     &        +cspenc_coli(1d0-p24/p34,real(p24-p34))
     &        -pi2_6)
#ifdef SING
     &        +colishiftms2
#endif

          D0ms3ir2_coli=D0ms3ir2_coli/(p13*p24)




#ifdef CHECK
        else
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms3ir2_coli',' branch not found',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms3ir2: inconsistency 1ir'
          endif
#endif
        endif

#ifdef CHECK
      else
        call setErrFlag_coli(-10)
        call ErrOut_coli('D0ms3ir2_coli','case not implemented',
     &      errorwriteflag)
        if (errorwriteflag) then
          write(nerrout_coli,111)' D0ms3ir2_coli: case with more than'
     &        //' two soft singularities not implemented '
          write(nerrout_coli,111)' D0ms3ir2_coli: nsoft = ',nsoft
          write(nerrout_coli,111)' D0ms3ir2_coli: p23 = ',p23
          write(nerrout_coli,111)' D0ms3ir2_coli: p34 = ',p34
          write(nerrout_coli,111)' D0ms3ir2_coli: p14 = ',p14
          write(nerrout_coli,111)' D0ms3ir2_coli: p24 = ',p24
          write(nerrout_coli,111)' D0ms3ir2_coli: p13 = ',p13
          write(nerrout_coli,111)' D0ms3ir2_coli: m12 = ',m12
          write(nerrout_coli,111)' D0ms3ir2_coli: m22 = ',m22
          write(nerrout_coli,111)' D0ms3ir2_coli: m32 = ',m32
          write(nerrout_coli,111)' D0ms3ir2_coli: m42 = ',m42
        endif
#endif
      endif

      end


************************************************************************
      function D0ms4ir4_coli(p12,p23,p34,p14,p13,p24,m12,m22,m32,m42)
************************************************************************
*     quartic mass-singular 4-point function                           *
*     p12, p23, p34, p14, m12, m22, m32, m42 small                     *
*     p13, p24  finite                                                 *
*                                                                      *
*                     m22 small                                        *
*     p12 small  ---------------------  p23 small                      *
*                     |    2    |                                      *
*                     |         |                                      *
*           m12 small | 1     3 | m32 small                            *
*                     |         |                                      *
*                     |    4    |                                      *
*     p14 small  ---------------------  p34 small                      *
*                     m42 small                                        *
*                                                                      *
************************************************************************
*     05.08.08 Ansgar Denner        last changed  05.12.09             *
************************************************************************
#ifndef ADcode
      use coli_aux2
#endif
      implicit   none
      complex*16 p12,p23,p34,p14,p13,p24
      complex*16 m12,m22,m32,m42
      complex*16 D0ms4ir4_coli
      logical errorwriteflag

#ifdef CHECK
      complex*16 elimminf2_coli
      complex*16 ps12,ps23,ps34,ps14,ps13,ps24
      complex*16 ms12,ms22,ms32,ms42
      logical flag(0:6)
      data    flag /7*.true./
      save    flag
#endif

      complex*16 q13,q24,mm12,mm22,mm42,mm32,m2(4)
      complex*16 cln_coli
      integer    nsoft
      integer    i,j,k
      logical    soft(4),onsh(4,4),softt(4,4,4)

#include "params_coli.h"
#include "common_coli.h"
#include "checkparams_coli.h"

 100  format(((a)))
 111  format(a22,2('(',g24.17,',',g24.17,') ':))

      D0ms4ir4_coli = undefined

#ifdef CHECK
 101  format(a22,g25.17)
      if(argcheck)then
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

        if(ms12.ne.cd0.or.ms22.ne.cd0.or.ms32.ne.cd0
     &        .or.ms42.ne.cd0.or.ps12.ne.cd0.or.ps23.ne.cd0
     &        .or.ps34.ne.cd0.or.ps14.ne.cd0
     &        .or.ps24.eq.cd0.or.ps13.eq.cd0
     &        .or.aimag(p12).ne.0d0.or.aimag(p23).ne.0d0
     &        .or.aimag(p34).ne.0d0.or.aimag(p14).ne.0d0
     &        .or.aimag(p24).ne.0d0.or.aimag(p13).ne.0d0
     &        .or.ps13.eq.cd0.or.ps24.eq.cd0) then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli called improperly:'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12,ps12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23,ps23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34,ps34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14,ps14
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13,ps13
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24,ps24
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12,ms12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22,ms22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32,ms32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42,ms42
          endif
        endif
      endif
#endif

      m2(1)=m12
      m2(2)=m22
      m2(3)=m32
      m2(4)=m42

c determine on-shell momenta
      onsh(1,2)=p12.eq.m22
      onsh(1,3)=p13.eq.m32
      onsh(1,4)=p14.eq.m42
      onsh(2,1)=p12.eq.m12
      onsh(2,3)=p23.eq.m32
      onsh(2,4)=p24.eq.m42
      onsh(3,1)=p13.eq.m12
      onsh(3,2)=p23.eq.m22
      onsh(3,4)=p34.eq.m42
      onsh(4,1)=p14.eq.m12
      onsh(4,2)=p24.eq.m22
      onsh(4,3)=p34.eq.m32

c count/determine soft singularities
      nsoft=0
      do i=1,4
        soft(i)=.false.
        do j=1,3
          if(i.ne.j)then
            do k=j,4
              if(k.ne.i.and.k.ne.j)then
                softt(i,j,k)=m2(i).eq.cd0.and.onsh(i,j).and.onsh(i,k)
                soft(i)=softt(i,j,k).or.soft(i)
                if(softt(i,j,k)) nsoft=nsoft+1
              endif
            enddo
          endif
        enddo
      enddo

      if(nsoft.eq.4)then

* four soft singularities
*
*                   0
*          0 ----------------  0
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*          0 ----------------  0
*                   0
*

#ifdef CHECK
        if(m12.ne.cd0.or.m22.ne.0d0.or.m32.ne.0d0.or.m42.ne.cd0
     &      .or.p12.ne.cd0.or.p23.ne.cd0.or.p34.ne.cd0.or.p14.ne.cd0)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.71)',
     &          '    case with 4 soft singularities: ',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(1))then
          call ErrOut_coli('D0ms4ir4_coli',
     &        'new function called:  (4.71)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.71)',
     &          '    case with 4 soft singularities',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            flag(1)=.false.
          endif
        endif
#endif

c Bern, Dixon, Kosower, Nucl. Phys. B412 (1994) 751
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    box1
c Denner, Dittmaier (4.71)
        D0ms4ir4_coli =
#ifdef SING
     &      4d0*delta2ir
     &      -2d0*delta1ir*(cln_coli(-p13/muir2,-1d0)
     &                     +cln_coli(-p24/muir2,-1d0))
#endif
     &      +2d0*cln_coli(-p13/muir2,-1d0)
     &                     *cln_coli(-p24/muir2,-1d0)
     &      -10d0*pi2_6

        D0ms4ir4_coli = D0ms4ir4_coli/(p13*p24)

      elseif(nsoft.eq.3)then

* three soft singularities
*
*                   0
*         m2 ----------------  0
*                |  2   |
*     small   m2 |1    3| 0
*                |   4  |
*         m2 ----------------  0
*                   0
*

#ifdef CHECK
        if(.not.(
     &      (m12.eq.p12.and.m12.eq.p14.and.p23.eq.cd0.and.p34.eq.cd0
     &      .and.m22.eq.cd0.and.m32.eq.cd0.and.m42.eq.cd0).or.
     &      (m22.eq.p12.and.m22.eq.p23.and.p14.eq.cd0.and.p34.eq.cd0
     &      .and.m12.eq.cd0.and.m32.eq.cd0.and.m42.eq.cd0).or.
     &      (m32.eq.p23.and.m32.eq.p34.and.p14.eq.cd0.and.p12.eq.cd0
     &      .and.m12.eq.cd0.and.m22.eq.cd0.and.m42.eq.cd0).or.
     &      (m42.eq.p34.and.m42.eq.p14.and.p23.eq.cd0.and.p12.eq.cd0
     &      .and.m12.eq.cd0.and.m22.eq.cd0.and.m32.eq.cd0)))
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.72)',
     &          '    case with 3 soft singularities: ',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(2))then
          call ErrOut_coli('D0ms4ir4_coli',
     &        'new function called:  (4.72)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.72)',
     &          '    case with 3 soft singularities',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            flag(2)=.false.
          endif
        endif
#endif

        if(m12.ne.cd0)then
          mm12=m12*coliminfscale2
          q13=p13
          q24=p24
        elseif(m22.ne.cd0)then
          mm12=m22*coliminfscale2
          q13=p24
          q24=p13
        elseif(m32.ne.cd0)then
          mm12=m32*coliminfscale2
          q13=p13
          q24=p24
        elseif(m42.ne.cd0)then
          mm12=m42*coliminfscale2
          q13=p24
          q24=p13
        else
          mm12=undefined
          q13=undefined
          q24=undefined
        endif

c Beenakker, Kuiff, van Neerven, Smith, PRD 40 (1989) 54
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    box6  m4 small
c Denner, Dittmaier (4.72)
        D0ms4ir4_coli=
#ifdef SING
     &      2d0*delta2ir
     &      -delta1ir*(2d0*cln_coli(-q13/sqrt(mm12*muir2),-1d0)
     &                 +cln_coli(-q24/muir2,-1d0))
#endif
     &      +2d0*cln_coli(-q13/sqrt(mm12*muir2),-1d0)
     &      *cln_coli(-q24/muir2,-1d0)
     &      -5d0*pi2_6

        D0ms4ir4_coli=D0ms4ir4_coli/(q13*q24)


      elseif(nsoft.eq.2)then

        if(soft(1).and.soft(3).or.soft(2).and.soft(4))then

* two opposite soft singularities
*
*                  m22 small
*        m22 ---------------- m22
*                |  2   |
*              0 |1    3| 0
*                |   4  |
*        m42 ---------------- m42
*                  m42 small
*

#ifdef CHECK
          if(.not.(
     &        (p12.eq.m22.and.m22.eq.p23.and.m12.eq.cd0.and.m32.eq.cd0
     &        .and.p14.eq.m42.and.m42.eq.p34).or.
     &        (p23.eq.m32.and.m32.eq.p34.and.m22.eq.cd0.and.m42.eq.cd0
     &        .and.p12.eq.m12.and.m12.eq.p14).or.
     &        (p12.eq.m22.and.m22.eq.p24.and.m12.eq.cd0.and.m42.eq.cd0
     &        .and.p13.eq.m32.and.m32.eq.p34).or.
     &        (p13.eq.m32.and.m32.eq.p23.and.m12.eq.cd0.and.m22.eq.cd0
     &        .and.p14.eq.m42.and.m42.eq.p24).or.
     &        (p24.eq.m42.and.m42.eq.p34.and.m22.eq.cd0.and.m32.eq.cd0
     &        .and.p12.eq.m12.and.m12.eq.p13).or.
     &        (p23.eq.m22.and.m22.eq.p24.and.m32.eq.cd0.and.m42.eq.cd0
     &        .and.p13.eq.m12.and.m12.eq.p14)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms4ir4_coli:   (4.74)',
     &            '    case with 2 opposite soft singularities:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            endif
          endif
#endif

          if(soft(1).and.soft(3))then
            mm22=m22
            mm42=m42
            q13=p13
            q24=p24
          else
            mm22=m32
            mm42=m12
            q13=p24
            q24=p13
          endif

#ifdef NEWCHECK
          if(flag(3))then
            call ErrOut_coli('D0ms4ir4_coli',
     &          'new function called:  (4.74)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms4ir4_coli:   (4.74)',
     &            '    case with 2 opposite soft singularities ',
     &            '    not yet tested in physcial process'
              write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
              flag(3)=.false.
            endif
          endif
#endif


c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349  (iiib)
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002    box14 m2,m4 small
c Denner, Dittmaier (4.74)
          D0ms4ir4_coli =
#ifdef SING
     &        -2d0*cln_coli(-q24/(sqrt(mm22*mm42)*coliminfscale2),-1d0)
     &        *delta1ir
#endif
     &        +2d0*cln_coli(-q24/(sqrt(mm22*mm42)*coliminfscale2),-1d0)
     &        *cln_coli(-q13/muir2,-1d0)
          D0ms4ir4_coli = D0ms4ir4_coli/(q13*q24)

        else

* two adjacent soft singularities
*
*                   0
*         0  ---------------- mm2
*                |  2   |
*              0 |1    3| mm2
*                |   4  |
*        mm2 ----------------  0
*                  mm2 small
*

#ifdef CHECK
          if(.not.(
     &        (m12.eq.cd0.and.m22.eq.cd0.and.p12.eq.cd0.and.p34.eq.cd0
     &        .and.p14.eq.m42.and.m42.eq.m32.and.m32.eq.p23).or.
     &        (m22.eq.cd0.and.m32.eq.cd0.and.p23.eq.cd0.and.p14.eq.cd0
     &        .and.p12.eq.m12.and.m12.eq.m42.and.m42.eq.p34).or.
     &        (m32.eq.cd0.and.m42.eq.cd0.and.p34.eq.cd0.and.p12.eq.cd0
     &        .and.p23.eq.m22.and.m22.eq.m12.and.m12.eq.p14).or.
     &        (m42.eq.cd0.and.m12.eq.cd0.and.p14.eq.cd0.and.p23.eq.cd0
     &        .and.p34.eq.m32.and.m32.eq.m22.and.m22.eq.p12).or.
     &        (m32.eq.cd0.and.m12.eq.cd0.and.p13.eq.cd0.and.p24.eq.cd0
     &        .and.p34.eq.m42.and.m42.eq.m22.and.m22.eq.p12).or.
     &        (m42.eq.cd0.and.m22.eq.cd0.and.p24.eq.cd0.and.p13.eq.cd0
     &        .and.p34.eq.m32.and.m32.eq.m12.and.m12.eq.p12)))
     &        then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms4ir4_coli:   (4.73)',
     &            '    case with 2 adjacent soft singularities:',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            endif
          endif
#endif

#ifdef NEWCHECK
          if(flag(4))then
            call ErrOut_coli('D0ms4ir4_coli',
     &          'new function called:  (4.73)',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms4ir4_coli:   (4.73)',
     &            '    case with 2 adjacent soft singularities',
     &            '    not yet tested in physical process'
              write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
              flag(4)=.false.
            endif
          endif
#endif
          if(soft(1).and.soft(2))then
            q13=p13
            q24=p24
            mm32=m32*coliminfscale2
          elseif(soft(2).and.soft(3))then
            q13=p24
            q24=p13
            mm32=m42*coliminfscale2
          elseif(soft(3).and.soft(4))then
            q13=p13
            q24=p24
            mm32=m12*coliminfscale2
          elseif(soft(4).and.soft(1))then
            q13=p24
            q24=p13
            mm32=m22*coliminfscale2
          else
            q13=undefined
            q24=undefined
            mm32=undefined
          endif

c Hoepker, DESY-T-96-02
c according to Ellis, Zanderighi, JHEP 0802 (2008) 002
c     box11 p32=0 m3=m4
c Denner, Dittmaier (4.73)

          D0ms4ir4_coli=
#ifdef SING
     &        delta2ir
     &        -delta1ir*(cln_coli(-q13/sqrt(mm32*muir2),-1d0)
     &                 +cln_coli(-q24/sqrt(mm32*muir2),-1d0))
     &        +0.5d0*colishiftms2
#endif
     &        +2d0*cln_coli(-q13/sqrt(mm32*muir2),-1d0)
     &        *cln_coli(-q24/sqrt(mm32*muir2),-1d0)
     &        -4d0*pi2_6

          D0ms4ir4_coli=D0ms4ir4_coli/(q13*q24)

        endif

      elseif(nsoft.eq.1)then

* 1 soft singularity
*
*                   m2
*         m2 ----------------  0
*                |  2   |
*              0 |1    3| m2
*                |   4  |
*         m2 ----------------  0
*                   m2 small
*

#ifdef CHECK
        if(.not.(
     &      (m12.eq.cd0.and.p12.eq.m22.and.m22.eq.m32.and.m32.eq.m42
     &      .and.m42.eq.p14.and.p23.eq.cd0.and.p34.eq.cd0).or.
     &      (m22.eq.cd0.and.p23.eq.m32.and.m32.eq.m42.and.m42.eq.m12
     &      .and.m12.eq.p12.and.p34.eq.cd0.and.p14.eq.cd0).or.
     &      (m32.eq.cd0.and.p34.eq.m42.and.m42.eq.m12.and.m12.eq.m22
     &      .and.m22.eq.p23.and.p14.eq.cd0.and.p12.eq.cd0).or.
     &      (m42.eq.cd0.and.p14.eq.m12.and.m12.eq.m22.and.m22.eq.m32
     &      .and.m32.eq.p34.and.p12.eq.cd0.and.p23.eq.cd0)))
     &      then
            call setErrFlag_coli(-10)
            call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &          errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,100)' D0ms4ir4_coli:   (4.75)',
     &            '    case with 1 soft singularity: ',
     &            '    wrong arguments'
              write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
              write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
              write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
              write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
              write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
              write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
              write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
              write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
              write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
              write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            endif
        endif
#endif

        if(m12.eq.cd0)then
          mm32=m32*coliminfscale2
          q13=p13
          q24=p24
        elseif(m22.eq.cd0)then
          mm32=m42*coliminfscale2
          q13=p24
          q24=p13
        elseif(m32.eq.cd0)then
          mm32=m12*coliminfscale2
          q13=p13
          q24=p24
        elseif(m42.eq.cd0)then
          mm32=m22*coliminfscale2
          q13=p24
          q24=p13
        else
          mm32=undefined
          q13=undefined
          q24=undefined
        endif

#ifdef NEWCHECK
        if(flag(5)) then
          call ErrOut_coli('D0ms4ir4_coli',
     &        'new function called:  (4.75)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.75)',
     &          '    case with 1 soft singularity ',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            flag(5)=.false.
          endif
        endif
#endif

c according to W.Beenakker and A.Denner, Nucl. Phys. B338 (1990) 349  (ie)
c Ellis, Zanderighi, JHEP 0802 (2008) 002
c     box16 p32=0, p22=0  m2=m3=m4
c Denner, Dittmaier (4.75)

        D0ms4ir4_coli = (2d0*cln_coli(-q24/(mm32),-1d0)
     &     *cln_coli(-q13/sqrt(mm32*muir2),-1d0)
     &     - 3d0*pi2_6
#ifdef SING
     &      - delta1ir*cln_coli(-q24/(mm32),-1d0)
     &      + colishiftms2
#endif
     &      )
     &     /(q24*q13)


      elseif(nsoft.eq.0)then

* no soft singularity
*
*                   m2 small
*          0 ----------------  0
*                |  2   |
*             m2 |1    3| m2
*                |   4  |
*          0 ----------------  0
*                   m2
*

#ifdef CHECK
        if(m12.ne.m22.or.m12.ne.m32.or.m12.ne.m42.or.
     &      p12.ne.cd0.or.p23.ne.cd0.or.p34.ne.cd0.or.p14.ne.cd0)
     &      then
          call setErrFlag_coli(-10)
          call ErrOut_coli('D0ms4ir4_coli',' wrong arguments',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,*) ' D0ms4ir4_coli:   (4.76)',
     &          '    soft regular case: ',
     &          '    wrong arguments'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
          endif
        endif
#endif

#ifdef NEWCHECK
        if(flag(6))then
          call ErrOut_coli('D0ms4ir4_coli',
     &        'new function called:  (4.76)',
     &        errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' D0ms4ir4_coli:   (4.76)',
     &          '    soft regular case ',
     &          '    not yet tested in physical process'
            write(nerrout_coli,111)' D0ms4ir4_coli: p12 = ',p12
            write(nerrout_coli,111)' D0ms4ir4_coli: p23 = ',p23
            write(nerrout_coli,111)' D0ms4ir4_coli: p34 = ',p34
            write(nerrout_coli,111)' D0ms4ir4_coli: p14 = ',p14
            write(nerrout_coli,111)' D0ms4ir4_coli: p24 = ',p24
            write(nerrout_coli,111)' D0ms4ir4_coli: p13 = ',p13
            write(nerrout_coli,111)' D0ms4ir4_coli: m12 = ',m12
            write(nerrout_coli,111)' D0ms4ir4_coli: m22 = ',m22
            write(nerrout_coli,111)' D0ms4ir4_coli: m32 = ',m32
            write(nerrout_coli,111)' D0ms4ir4_coli: m42 = ',m42
            flag(6)=.false.
          endif
        endif
#endif

c new case     27.07.09
c Denner, Dittmaier (4.76)
        mm12=m12*coliminfscale2
        D0ms4ir4_coli =
     &      cln_coli(-p13/mm12,-1d0)*cln_coli(-p24/mm12,-1d0)-3d0*pi2_6
#ifdef SING
        D0ms4ir4_coli = D0ms4ir4_coli + colishiftms2
#endif

        D0ms4ir4_coli = D0ms4ir4_coli*2d0/(p13*p24)

      endif

      end
