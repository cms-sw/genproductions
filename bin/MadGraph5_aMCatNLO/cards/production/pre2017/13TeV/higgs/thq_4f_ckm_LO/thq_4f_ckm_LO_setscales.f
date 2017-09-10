c
c
c For reference see: arXiv:1504.00611
c
c



      subroutine set_ren_scale(P,rscale)
c----------------------------------------------------------------------
c     This is the USER-FUNCTION to calculate the renormalization
c     scale on event-by-event basis.
c----------------------------------------------------------------------      
      implicit none
      real*8   alphas
      external alphas
c
c     INCLUDE and COMMON
c
      include 'genps.inc'
      include 'nexternal.inc'
      include 'coupl.inc'

      integer i
      include 'maxamps.inc'
      integer idup(nexternal,maxproc,maxsproc)
      integer mothup(2,nexternal)
      integer icolup(2,nexternal,maxflow,maxsproc)
      include 'leshouche.inc'
      include 'run.inc'

      double precision pmass(nexternal)
      common/to_mass/  pmass

      real*8 xptj,xptb,xpta,xptl,xmtc
      real*8 xetamin,xqcut,deltaeta
      common /to_specxpt/xptj,xptb,xpta,xptl,xmtc,xetamin,xqcut,deltaeta
      double precision tmp,xmt2
c
c     ARGUMENTS
c      
      REAL*8 P(0:3,nexternal)
      REAL*8 rscale
c
c     EXTERNAL
c
      REAL*8 R2,DOT,ET,ETA,DJ,SumDot,PT
c      integer maxflow
c      parameter (maxflow=999)
c      integer idup(nexternal,maxproc), mothup(2,nexternal,maxproc)
c     & , icolup(2,nexternal,maxflow),
      integer ipdg(nexternal)
c      common /c_leshouche_inc/idup,mothup,icolup
      
c----------
c     start
c----------
c                                                                                                                                                                               
      do i=1,nexternal
         ipdg(i)=idup(i,1,1)
      enddo

      tmp=0

      rscale=2d0
      do i=nincoming+1,nexternal
c     m^2+pt^2=p(0)^2-p(3)^2=(p(0)+p(3))*(p(0)-p(3))                                     
         xmt2=(P(0,i)+P(3,i))*(P(0,i)-P(3,i))
c     take max() to avoid numerical instabilities                                                                                                                
         if( (abs(ipdg(i)).eq.5) .or. (abs(ipdg(i)).eq.6) .or.
     &          (ipdg(i).eq.25) ) tmp=tmp+sqrt(max(xmt2,0d0))/6d0
      enddo
      rscale=max(rscale,tmp)
      
c      open(unit=123,file='mylog.txt',status='unknown')
c      write(123,*) rscale
c
c-some examples of dynamical scales
c

c      if(first) then
c         write(*,*) 'Using event- by event renormalization/factorization scale:'
c         write(*,*) 'scalefact^2*(Max of squared masses of final-state particles + '
c         write(*,*) '             sum of pT^2 for jets and massless particles)'
c      endif
c      do i=3,nexternal
c         rscale=max(rscale,pmass(i)**2)
c      enddo
c      do i=3,nexternal
c         if(iabs(idup(i,1,1)).le.5.or.idup(i,1,1).eq.21.or.pmass(i).eq.0d0)then
c            rscale=rscale+pt(p(0,i))**2
c         endif
c      enddo

c      rscale=sqrt(rscale)

c      rscale=rscale*scalefact


c---------------------------------------
c-- total transverse energy of the event 
c---------------------------------------
c     rscale=0d0
c     do i=3,nexternal
c      rscale=rscale+et(P(0,i))
c     enddo

c--------------------------------------
c-- scale^2 = \sum_i  (pt_i^2+m_i^2)  
c--------------------------------------
c     rscale=0d0
c     do i=3,nexternal
c      rscale=rscale+pt(P(0,i))**2+dot(p(0,i),p(0,i))
c     enddo
c     rscale=dsqrt(rscale)

c--------------------------------------
c-- \sqrt(s): partonic energy
c--------------------------------------
c     rscale=dsqrt(2d0*dot(P(0,1),P(0,2)))

      return
      end


      subroutine set_fac_scale(P,q2fact)
c----------------------------------------------------------------------
c     This is the USER-FUNCTION to calculate the factorization 
c     scales^2 on event-by-event basis.
c----------------------------------------------------------------------      
      implicit none

c     INCLUDE and COMMON
c
c      include 'genps.inc'
      include 'nexternal.inc'
      include 'coupl.inc'
c--masses and poles
c
c     ARGUMENTS
c      
      REAL*8 P(0:3,nexternal)
      real*8 q2fact(2)
c
c     EXTERNAL
c
      REAL*8 R2,DOT,ET,ETA,DJ,SumDot,PT
c
c     LOCAL
c

      integer i
      include 'maxamps.inc'
      integer idup(nexternal,maxproc,maxsproc)
      integer mothup(2,nexternal)
      integer icolup(2,nexternal,maxflow,maxsproc)
      include 'leshouche.inc'
c      include 'run.inc'

      
      logical first
      data first/.true./


c     integer maxflow
c      parameter (maxflow=999)
      double precision tmp,xmt2
c      integer idup(nexternal,maxproc), mothup(2,nexternal,maxproc)
c     & , icolup(2,nexternal,maxflow),
      integer ipdg(nexternal)
c      common /c_leshouche_inc/idup,mothup,icolup
      
c----------
c     start
c----------
c                                                                                                                                                                               
      do i=1,nexternal
         ipdg(i)=idup(i,1,1)
      enddo

      tmp=0

      do i=nincoming+1,nexternal
c     m^2+pt^2=p(0)^2-p(3)^2=(p(0)+p(3))*(p(0)-p(3))                                     
         xmt2=(P(0,i)+P(3,i))*(P(0,i)-P(3,i))
c     take max() to avoid numerical instabilities                                                                                                                
         if( (abs(ipdg(i)).eq.5) .or. (abs(ipdg(i)).eq.6) .or.
     &          (ipdg(i).eq.25) ) tmp=tmp+sqrt(max(xmt2,0d0))/6d0
      enddo

      
      
      q2fact(1)=tmp             !factorization scale**2 for pdf1
      q2fact(2)=tmp             !factorization scale**2 for pdf2

c      call set_ren_scale(P,q2fact(1))
c
c      q2fact(1)=q2fact(1)**2
c
c      q2fact(2)=q2fact(1)       !factorization scale**2 for pdf2
      

c
c-some examples of dynamical scales
c

c---------------------------------------
c-- total transverse energy of the event 
c---------------------------------------
c     q2fact(1)=0d0
c     do i=3,nexternal
c      q2fact(1)= q2fact(1)+et(P(0,i))**2
c     enddo
c     q2fact(2)=q2fact(1)  

c--------------------------------------
c-- scale^2 = \sum_i  (pt_i^2+m_i^2)  
c--------------------------------------
c     q2fact(1)=0d0
c     do i=3,nexternal
c      q2fact(1)=q2fact(1)+pt(P(0,i))**2+dot(p(0,i),p(0,i))
c     enddo
c     q2fact(2)=q2fact(1)  

c--------------------------------------
c-- \sqrt(s): partonic energy
c--------------------------------------
c     q2fact(1)=2d0*dot(P(0,1),P(0,2))
c     q2fact(2)=q2fact(1)  

      
      return
      end


