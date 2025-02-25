      subroutine class_f(x,p1,p2,p3,p4,r1,r2)
c********************************************************************
c     This subroutine gets the two missing momenta for 
c     the following topology:
c
c                          ________ missing    p1
c                     r1  *________ visible    p3
c                        *
c     ------------------*
c                        * ________ missing    p2
c                      r2 *________ visible    p4
c
c********************************************************************
c
      implicit none
      include '../../../SubProcesses/phasespace.inc'
      double precision thres
      parameter (thres=1700d0)
c
c     arguments
c
      double precision x(20)
      integer p1,p2,p3,p4,r1,r2,n_var,local_var

c
c     local
c
      INTEGER IDUM
      DATA IDUM/0/
      SAVE IDUM
      double precision E3,E4,p3x,p3y,p3z,p4x,p4y,p4z
      double precision A1,A2,A3,A4,B1,B2,B3,B4
      double precision p1z_ti,p1z_E2,p1z_p2y
      double precision p2z_ti,p2z_E2,p2z_p2y
      double precision p2x_ti,p2x_E2,p2x_p2y
      double precision E1_ti,E1_E2,E1_p2y
      double precision p1x_ti, p1x_E2, p1x_p2y, p1y_ti, p1y_E2, p1y_p2y
      double precision E2,p2x,p2y,p2z
      double precision p1x,p1y,p1z,E1
      double precision g11,g12,g10,g22,g20,g00
      double precision h10,h20,h00
      double precision inv_jac,jac_loc,jac_temp
      double precision dem,b,c,alpha,beta,rho,sqrts
      double precision jac_factor
      double precision sol(2)
      double precision pboost(0:3)
      double precision measure1,measure2,ETA
      double precision CMS_mom(0:3,max_particles)
      integer index_sol
      real rand

      integer j,k,MG
c
c     global
c
      double precision pxISR, pyISR
      common /to_ISR/  pxISR, pyISR
      integer matching_type_part(3:max_particles) !modif/link between our order by type for permutation
      integer inv_matching_type_part(3:max_particles)
      common/madgraph_order_type/matching_type_part,
     & inv_matching_type_part
      integer nexternal, num_inv
      COMMON/to_num_inv/nexternal, num_inv

      double precision momenta(0:3,-max_branches:2*max_particles)  ! records the momenta of external/intermediate legs     (MG order)
      double precision mvir2(-max_branches:2*max_particles)        ! records the sq invariant masses of intermediate particles (MG order)
      common /to_diagram_kin/ momenta, mvir2
      double precision pmass(max_particles)     ! records the pole mass of any particle of the diagram  (MG order)
      common / to_mass/pmass
      double precision Etot,pztot,miss_px,miss_py
      common /to_missingP/Etot,pztot,miss_px,miss_py
      double precision              S,X1,X2,PSWGT,JAC
      common /PHASESPACE/ S,X1,X2,PSWGT,JAC
      integer ISR_mode
      common /to_correct_ISR/ISR_mode
      REAL XRAN1
      EXTERNAL XRAN1
c---
c Begin code
c---
       IDUM=0
      sqrts=dsqrt(s)
      momenta(0,p1)=-1d0
      momenta(0,p2)=-1d0
      jac_factor=1d0
      jac_loc=1d0
      if((Etot+dsqrt(miss_px**2+miss_py**2)).gt.sqrts) then
      jac=-1d0
      return
      endif
c
c      generate Bjorken fractions
c   
      call  generate_bjk_fraction(x,1, x1, jac_loc)
      call  generate_bjk_fraction(x,2, x2, jac_loc)

      miss_px=0d0
      miss_py=0d0
      ETA=0.5d0*dlog(x1/x2)

      if((x1*x2*s).lt.(dsqrt(mvir2(r1))+dsqrt(mvir2(r2)))**2) then
      jac=-1d0
      return
      endif

c
c     initialization
c 
      momenta(0,1)=sqrts*x1/2d0
      momenta(1,1)=0d0
      momenta(2,1)=0d0
      momenta(3,1)=sqrts*x1/2d0
      momenta(0,2)=sqrts*x2/2d0
      momenta(1,2)=0d0
      momenta(2,2)=0d0
      momenta(3,2)=-sqrts*x2/2d0

c     Here we need to boost the initial momenta in case of ISR:
c     the momentum associated with the boost can be obtained from 
c     the momentum of resonance r3, with 
c         m_inv(r3)^2 =   shat
c         pt_inv(r3)  = - pt(ISR)
c
c      =>  pboost = ( sqrt( pt(ISR)**2 + m_inv(r3)^2 ), -pxISR, -pyISR )
c
c     apply the boost to lab frame
      pboost(0)=dsqrt( x1*x2*s + pxISR**2 + pyISR**2)*dcosh(ETA) 
      pboost(1)=-pxISR
      pboost(2)=-pyISR
      pboost(3)=0d0

      call boostx(momenta(0,1),pboost,momenta(0,1))
      call boostx(momenta(0,2),pboost,momenta(0,2))
 
c
      E3=momenta(0,p3)
      E4=momenta(0,p4)
      p3x=momenta(1,p3)
      p3y=momenta(2,p3)
      p3z=momenta(3,p3)
      p4x=momenta(1,p4)
      p4y=momenta(2,p4)
      p4z=momenta(3,p4)

c
c     start from the following expressions for p1z and p2x:
c
c     p1z=A1 + A2 E2 + A3 p2y + A4 p2x
c     p2x=B1 + B2 E2 + B3 p2y + A4 p1z
c
      A1=(mvir2(p1)+mvir2(p3)+2d0*E3*
     & (momenta(0,1)+momenta(0,2)-Etot)
     &-2d0*p3x*miss_px-2d0*p3y*miss_py -mvir2(r1))/(2d0*p3z)
      A2=-E3/p3z
      A3=p3y/p3z
      A4=p3x/p3z
c
      B1=(mvir2(p2)+mvir2(p4)-2d0*p4z*
     & (momenta(3,1)+momenta(3,2)-pztot)-mvir2(r2))
     & /(2d0*p4x)
      B2=E4/p4x
      B3=-p4y/p4x
      B4=p4z/p4x
c
      p1z_ti=(A1+A4*B1)/(1d0-A4*B4)
      p1z_E2=(A2+A4*B2)/(1d0-A4*B4)
      p1z_p2y=(A3+A4*B3)/(1d0-A4*B4)

c
      p2x_ti=(B1+B4*A1)/(1d0-A4*B4)
      p2x_E2=(B2+B4*A2)/(1d0-A4*B4)
      p2x_p2y=(B3+B4*A3)/(1d0-A4*B4)
c
      p1x_ti=miss_px-p2x_ti
      p1x_E2=-p2x_E2
      p1x_p2y=-p2x_p2y
c
      p1y_ti=miss_py
      p1y_p2y=-1d0
      p1y_E2=0d0
c
      E1_ti=momenta(0,1)+momenta(0,2)-Etot
      E1_E2=-1d0
      E1_p2y=0d0
c
      p2z_ti=momenta(3,1)+momenta(3,2)-p1z_ti-pztot
      p2z_E2=-p1z_E2
      p2z_p2y=-p1z_p2y

c
c     mass shell conditions:     
c    
c     E1^2-p1x^2-p1y^2-p1z^2=m1^2   (1)
c     E2^2-p2x^2-p2y^2-p2z^2=m2^2   (2) 
c
c     equivalent to
c
c     g11*E2^2 + g22*p2y^2 + g12*E2*p2y + g10*E2 + g20*p2y + g00 = 0  (1)
c     h11*E2^2 + h22*p2y^2 + h12*E2*p2y + h10*E2 + h20*p2y + h00 = 0  (2)
c
      g11=E1_E2**2-p1x_E2**2-p1y_E2**2-p1z_E2**2
c
      g22=E1_p2y**2-p1x_p2y**2-p1y_p2y**2-p1z_p2y**2
c
      g12=2d0*(E1_E2*E1_p2y-p1x_E2*p1x_p2y-p1y_E2*p1y_p2y-
     & p1z_E2*p1z_p2y)
c
      g10=2d0*(E1_E2*E1_ti-p1x_E2*p1x_ti-p1y_E2*p1y_ti-p1z_E2*p1z_ti)
c 
      g20=2d0*(E1_ti*E1_p2y-p1x_ti*p1x_p2y-p1y_ti*p1y_p2y-
     & p1z_ti*p1z_p2y)
c
      g00=E1_ti**2-p1x_ti**2-p1y_ti**2-p1z_ti**2-mvir2(p1)
c
      h10=-2d0*(p2x_ti*p2x_E2+p2z_ti*p2z_E2)
c
      h20=-2d0*(p2x_p2y*p2x_ti+p2z_p2y*p2z_ti)
c
      h00=-p2x_ti**2-p2z_ti**2-mvir2(p2)
c
c     We are left with the equation
c
c     (g10-h10) E2 + (g20-h20) p2y + g00-h00 = 0
c
c      <=>   p2y = alpha + beta E2
c
      if (dabs(g20-h20).gt.1d-5) then
      alpha=-(g00-h00)/(g20-h20)
      beta=-(g10-h10)/(g20-h20)
      elseif (dabs(g10-h10).gt.1d-5) then
        E2=-(g00-h00)/(g10-h10)
        if (E2.gt.0d0) then
          dem = g22
          b=(g12*E2+g20)/dem
          c=(g11*E2**2+g10*E2+g00)/dem
          rho=b**2-4d0*c
          if (rho.eq.0d0) then   ! max 1 sol
            p2y=-b/2d0
            goto 13
          elseif (rho.gt.0d0) then
            sol(1)=(-b+dsqrt(rho))/2d0
            sol(2)=(-b-dsqrt(rho))/2d0
            index_sol=1
c            call ntuple(rand,0.0,1.0,p1)
            rand=xran1(IDUM)
            if (rand.gt.0.5) index_sol=2
            p2y=sol(index_sol)
            jac_factor=2d0
            goto 13 
          else
            jac_loc=-1d0
            jac=-1d0
            return
          endif
        else
          jac_loc=-1d0
          jac=-1d0
          return
        endif
      else
c     write(*,*) 'warning : num instability at code lksdqf'
      jac_loc=-1d0
      jac=-1d0
      return
      endif

      dem=g11+g22*beta**2+g12*beta

      if (dabs(dem).lt.0.0001d0) then
        E2=-(g22*alpha**2+g20*alpha+g00)/
     & (2D0*g22*alpha*beta+g12*alpha+g10+g20*beta)
        if  (E2.le.0d0) then
          jac_loc=-1d0
          jac=-1d0
          return
        endif
      else
        b=(2D0*g22*alpha*beta+g12*alpha+g10+g20*beta)/dem
        c=(g22*alpha**2+g20*alpha+g00)/dem
        rho=b**2-4d0*c

        
        if (rho.eq.0d0.and.b.lt.0d0) then   ! max 1 sol
          E2=-b/2d0
        elseif (rho.gt.b**2)then ! max 1 sol
          E2=(-b+dsqrt(rho))/2d0
        elseif (rho.gt.0d0.and.dsqrt(rho).lt.(-b)) then   ! max 2 sol
          sol(1)=(-b+dsqrt(rho))/2d0
          sol(2)=(-b-dsqrt(rho))/2d0



            index_sol=1
c            call ntuple(rand,0.0,1.0,p1)
            rand=xran1(IDUM)
            if (rand.gt.0.5) index_sol=2
            E2=sol(index_sol)
            jac_factor=2d0

      else 
      jac_loc=-1d0
      jac=-1d0
      return
      endif
      endif

      p2y=alpha+beta*E2
c
13    E1 =E1_ti + E1_E2*E2 + E1_p2y*p2y
      p1x=p1x_ti + p1x_E2*E2 + p1x_p2y*p2y
      p1y=p1y_ti + p1y_E2*E2 + p1y_p2y*p2y
      p1z=p1z_ti + p1z_E2*E2 + p1z_p2y*p2y

      p2x=p2x_ti + p2x_E2*E2 + p2x_p2y*p2y
      p2z=p2z_ti + p2z_E2*E2 + p2z_p2y*p2y
c
c     Now fill the set of missing momenta k
c
      momenta(0,p1)=E1
      momenta(1,p1)=p1x
      momenta(2,p1)=p1y
      momenta(3,p1)=p1z
c
      momenta(0,p2)=E2
      momenta(1,p2)=p2x
      momenta(2,p2)=p2y
      momenta(3,p2)=p2z

c      fill intermediate momenta
      do j=0,3
       momenta(j,r1)=momenta(j,p1)+momenta(j,p3)
       momenta(j,r2)=momenta(j,p2)+momenta(j,p4)
      enddo

c
c     inverse jacobian from mathematica: [inv_jac]=
c
      inv_jac=16d0*(E4*(p1z*p2y*p3x - p1y*p2z*p3x - p1z*p2x*p3y + 
     -        p1x*p2z*p3y + p1y*p2x*p3z - p1x*p2y*p3z) + 
     -     E2*p1z*p3y*p4x - E1*p2z*p3y*p4x - E2*p1y*p3z*p4x + 
     -     E1*p2y*p3z*p4x - E2*p1z*p3x*p4y + E1*p2z*p3x*p4y + 
     -     E2*p1x*p3z*p4y - E1*p2x*p3z*p4y + 
     -     (E2*p1y*p3x - E1*p2y*p3x - E2*p1x*p3y + E1*p2x*p3y)*p4z + 
     -     E3*(-(p1z*p2y*p4x) + p1y*p2z*p4x + p1z*p2x*p4y - 
     -        p1x*p2z*p4y - p1y*p2x*p4z + p1x*p2y*p4z))


       if (dabs(inv_jac).gt.thres) then
c
       jac_loc=jac_loc/dabs(inv_jac)*jac_factor/(2d0*x1*x2*s)
       else
       write(*,*) 'warning: jac^-1 is close to zero'
       write(*,*) 'inv_jac',inv_jac
       jac=-1d0
       endif
      jac=jac*jac_loc

c     correction from the measure to translate the weight to the CM frame
c     ONLY if isr = 2 

      if (isr_mode.eq.2) then

      measure1=1d0
       do j=3,nexternal-num_inv
         MG=inv_matching_type_part(j)
         measure1=measure1*dsqrt(momenta(1,MG)**2+momenta(2,MG)**2)
       enddo

      pboost(1)=-pboost(1)
      pboost(2)=-pboost(2)
       do j=3,nexternal -num_inv
c         write(*,*) "p",j,momenta(0,j), momenta(1,j),momenta(2,j),momenta(3,j)
         MG=inv_matching_type_part(j)
         call boostx(momenta(0,MG),pboost,CMS_mom(0,MG))
       enddo

      measure2=1d0
       do j=3,nexternal-num_inv
         MG=inv_matching_type_part(j)
         measure2=measure2*dsqrt(CMS_mom(1,MG)**2+CMS_mom(2,MG)**2)
       enddo

      jac=jac*measure2/measure1
      endif

      return
      end

