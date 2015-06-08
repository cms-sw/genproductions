!======this program generates two dm particles with mass dmmass
!=====the invariant mass of the pair is medmass**2
!==== initial conditions are read from input file
!=====after generation the events can be boosted to match input LHE event,
!=====which is passed at command line

      program boltdmdec
      implicit none
      include 'hepeup.f'  
      include 'heprup.f'
      double precision dmmass,medmass
      double precision ran2
      double precision p1x(4),p2x(4),pmx(4) !==== rest frame
      double precision p1(4),p2(4),pm(4) !===== lab frame
      integer idep,ic
      integer imed
      character(len=40) inp_lhe,out_lhe
      character(len=4) ending
      integer idum
      integer jj,i,j
      COMMON /ranno/ idum
      integer idmedx
      common/idmedx/idmedx
      double precision xth,xphi
      double precision wt
      logical nomore
      integer nevents
      nevents=0

      nomore=.false.
      
!==== to change the ending of the file
      ending='_dec'
!======random seed
 !     idum=3013
!======parameters for testing
 !     dmmass=10d0
 !     medmass=500d0
!=====mediator imed
!      imed=25
      open(unit=31,file='input.DAT',status='old',err=312)
      read(31,*) idum
      read(31,*) dmmass
      read(31,*) medmass
      read(31,*) imed
      
      
      write(6,*) '**** DM Decay bolter ********'
      write(6,*) '* C. Williams 2015          *'
      write(6,*) '* v1 :                      *'
      write(6,*) '*****************************'

      write(6,*) '* params *'
      write(6,*) 'dm mass : ',dmmass
      write(6,*) 'med mass : ',medmass
      write(6,*) 'idmed ',imed
      write(6,*) 'seed : ',idum
      
      idep=iargc()
      if (idep.eq.1) then
         call getarg(1,inp_lhe)
         write(6,*) '*****************************'
         write(6,*) '* Using file ',inp_lhe 
         write(6,*) '*****************************'
         ic=LEN_TRIM(inp_lhe)
         out_lhe=inp_lhe(1:ic)//ending
         write(6,*) '*****************************'
         write(6,*) '* Output file ',out_lhe 
         write(6,*) '*****************************'
      endif

!==== open LHE file
      open(unit=34,file=inp_lhe,status='old',err=333)
!==== output LHE file
      open(unit=35,file=out_lhe,status='unknown')

!===  initialize LHE
!===== seems broken at the moment 
!      call read_lhe_event_init(34,*345)
 11   continue 
!====== read LHE event 
      call readlhe_infile(34,35,nomore,pm,imed)
      if(nomore) goto 22
      nevents=nevents+1
 !     write(6,*) nevents
!=======generate decays 
      medmass=pm(4)**2-pm(3)**2-pm(2)**2-pm(1)**2
      medmass=dsqrt(medmass)
      
!=======rest frame mediator
      pmx(:)=0d0
      pmx(4)=medmass
      
      xth=ran2()
      xphi=ran2()

      call phi3m(xth,xphi,pm,p1x,p2x,dmmass,dmmass,wt,*999)

!      write(6,*) p1x,p2x
!=====write to file
      i=nup+1
      do jj=1,4
         pup(jj,nup+1)=p1x(jj)
         pup(jj,nup+2)=p2x(jj)
      enddo
      pup(5,nup+1)=dsqrt(p1x(4)**2-p1x(3)**2-p1x(2)**2-p1x(1)**2)
      pup(5,nup+2)=dsqrt(p2x(4)**2-p2x(3)**2-p2x(2)**2-p2x(1)**2)
      idup(nup+1) = 1000022
      idup(nup+2) = -1000022
      icolup(:,nup+1)=0
      icolup(:,nup+2)=0
      mothup(:,nup+1)=idmedx
      mothup(:,nup+2)=idmedx
      istup(nup+1)=1
      istup(nup+2)=1
      vtimup(nup+1)=0
      vtimup(nup+2)=0
      spinup(nup+1)=9.
      spinup(nup+2)=9.
      do i=nup+1,nup+2
      write(35,220) idup(i),istup(i),mothup(1,i),
     & mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     & vtimup(i),spinup(i)
      enddo
      write(35,'(a)') "</event>" 
      goto 11
 22   continue

      write(6,*) 'Decayed nevents = ',nevents
 220  format(1p,i8,5(1x,i5),5(1x,e16.9),1x,e12.5,1x,e10.3)

      return
 999  write(6,*) 'Error in phase space setup'
      return
 333  write(6,*) 'No file found = ',inp_lhe
      return
 345  write(6,*) 'Couldnt initialze LHE'
      return
 312  write(6,*) 'Couldnt find input.dat'
      end
      

      subroutine phi3m(xth,xphi,p0,p1,p2,m1,m2,wt,*)
c     massive particle p0 in rest frame 
c     decaying into p1 fixed mass m1 and p2 fixed mass m2.
c     vectors returned p1 and p2 are in the frame in which 
C     p0 is supplied
c result is 1/8/pi * 2|p|/sqrts  * domega/(4*pi)
c     factor of (2*pi)^4 included in definition of phase space
      implicit none
      include 'constants.f'
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

      subroutine boost(mass,p1,p_in,p_out)
c     take momenta p_in in frame in which particle one is at rest with mass 
c     "mass" 
c     and convert to frame in which particle one has fourvector p1
      implicit none
      double precision mass,p1(4),p_in(4),p_out(4)
      double precision gam,beta(1:3),bdotp,one
      parameter(one=1d0)
      integer j,k
      gam=p1(4)/mass
      bdotp=0d0
      do j=1,3
      beta(j)=-p1(j)/p1(4)
      bdotp=bdotp+p_in(j)*beta(j)
      enddo
      p_out(4)=gam*(p_in(4)-bdotp)
      do k=1,3
      p_out(k)=p_in(k)+gam*beta(k)*(gam/(gam+one)*bdotp-p_in(4))
      enddo
      return
      end
      
      double precision FUNCTION ran2()
      implicit none
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      double precision AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1d0/IM,IQ=127773,
     .IR=2836,NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=3d-16,RNMX=1d0-EPS)
      INTEGER j,k,iv(NTAB),iy
      COMMON /ranno/ idum
      DATA iv /NTAB*0/, iy /0/
      SAVE iv,iy
 
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran2=min(AM*dble(iy),RNMX)

c      write(6,*) 'idum',idum
c      write(6,*) 'AM=',AM
c      write(6,*) 'iy=',iy
c      write(6,*) 'AM*dble(iy)',AM*dble(iy)
c      write(6,*) 'ran2',ran2

      return
      END

