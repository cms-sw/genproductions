      subroutine phi3m(xth,xphi,p0,p1,p2,m1,m2,wt,*)
c     massive particle p0 in rest frame 
c     decaying into p1 fixed mass m1 and p2 fixed mass m2.
c     vectors returned p1 and p2 are in the frame in which 
C     p0 is supplied
c result is 1/8/pi * 2|p|/sqrts  * domega/(4*pi)
c     factor of (2*pi)^4 included in definition of phase space
      implicit none
      include 'constants.f'
      include 'process.f'
      double precision p0(4),p1(4),p2(4),p1cm(4)
      double precision xth,xphi,phi,s,roots,costh,sinth
      double precision wt0,wt
      double precision m1,m2,m1sq,m2sq,lambda,lambda2,smin
      integer j
      parameter(wt0=one/eight/pi)
      wt=0d0

      s=p0(4)**2-p0(1)**2-p0(2)**2-p0(3)**2  

      smin=(m1+m2)**2
      if (s .lt. smin) then
       if (case(1:4) .ne. 'vlch') then 
        write(6,*) 's<smin',s,smin
       endif
       return 1
      endif

      if (dsqrt(s)-m1-m2 .lt. 0d0) return 1 

      roots=dsqrt(s)
      m1sq=m1**2
      m2sq=m2**2
      costh=two*xth-one    
      sinth=dsqrt(one-costh**2)
      phi=twopi*xphi

      lambda2=((s+m1sq-m2sq)**2-4d0*m1sq*s)

      if (lambda2 .lt. 0d0) then
      write(6,*) 'phi3m:lambda2=', lambda2
      return 1
      endif
      lambda=dsqrt(lambda2)

      p1cm(4)=roots/two*(s+m1sq-m2sq)/s
      p1cm(1)=roots/two*lambda/s*sinth*dsin(phi)
      p1cm(2)=roots/two*lambda/s*sinth*dcos(phi)
      p1cm(3)=roots/two*lambda/s*costh

c      write(6,*) 'e',roots/two*(s+m1sq-m2sq)/s
c      write(6,*) 'p',roots/two*lambda/s

c      write(6,*) 'sinth',sinth
c      write(6,*) 'costh',costh
c      write(6,*) 'p1cm**2',p1cm(4)**2-p1cm(1)**2-p1cm(2)**2-p1cm(3)**2
c      pause

      call boost(roots,p0,p1cm,p1)
      do j=1,4
      p2(j)=p0(j)-p1(j)
      enddo


      if (  (p0(4) .lt. 0d0) 
     & .or. (p1(4) .lt. 0d0) 
     & .or. (p2(4) .lt. 0d0)) then  
      write(6,*) 'p0',p0(4),p0(4)**2-p0(1)**2-p0(2)**2-p0(3)**2,s
      write(6,*) 'p1',p1(4),p1(4)**2-p1(1)**2-p1(2)**2-p1(3)**2,m1sq
      write(6,*) 'p2',p2(4),p2(4)**2-p2(1)**2-p2(2)**2-p2(3)**2,m2sq
      write(6,*) 'in phi3m'
      return 1
      endif

      wt=wt0*lambda/s

      return
      end

