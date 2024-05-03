      program test_soft_col_limits
c*****************************************************************************
c     Given identical particles, and the configurations. This program identifies
c     identical configurations and specifies which ones can be skipped
c*****************************************************************************
      implicit none
      include 'nexternal.inc'
      include 'fks_info.inc'
      integer iunit
      parameter (iunit=11)

      integer nconf, dau_pos(2), mother, daughters(2), iostatus
      double precision energy
      double precision get_mass_from_id
      double precision get_width_os_from_id
      integer i
      double precision pmass(nexternal)
      include 'coupl.inc'
      include 'run.inc'
      double precision zero
      parameter (zero=0d0)
      INTEGER NFKSPROCESS
      COMMON/C_NFKSPROCESS/NFKSPROCESS
      double precision p(0:3, nexternal)
      double precision mommass, momwdth
      integer istep, nstep
      parameter (nstep=4)
      integer npoints 
      parameter (npoints=10)
      double precision pos(0:3), mos, bw_max, bw_min
      double precision dot
      DOUBLE PRECISION WGT_RE, WGT_OS, WGT
      COMMON /TO_REAL_WGTS/WGT_RE, WGT_OS
c-----
c  Begin Code
c-----

      call setrun               !Sets up run parameters
      call setpara('param_card.dat') !Sets up couplings and masses
      call setcuts              !Sets up cuts 

      include 'pmass.inc'

      open(unit=iunit, file='osinfo.dat', status='old')

      do while (.true.)
        call get_next_os_conf(iunit, nconf, dau_pos, mother, daughters, iostatus)
        if (iostatus.lt.0) goto 99

        nfksprocess=nconf
        mommass=get_mass_from_id(mother)
        momwdth=get_width_OS_from_id(mother)

        if (mommass.lt.
     $   (get_mass_from_id(daughters(1))+get_mass_from_id(daughters(2)))) then
          write(*,*) 'Skipping configuration (non-resonant spectrum)'
          cycle
        endif

        ! calculate the energy, set it to 2 times the threshold to
        ! produce the mother on-shell
        energy=mommass
        do i = nincoming+1, nexternal
          if (i.eq.dau_pos(1).or.i.eq.dau_pos(2)) cycle
          energy = energy + pmass(i)
        enddo
        energy = min(2d0 * energy, ebeam(1)+ebeam(2))
        write(*,*) 'Setting c.o.m. energy to', energy

        do istep=1, nstep
          ! ask the momenta to be such that the
          ! reconstructed mass - the true mother mass 
          ! lies between
          ! [bw_min, bw_max] times the mother width
          bw_max = 100d0/10**(istep-1)
          bw_min = 100d0/10**(istep)
          write(*,*) 'BW boundaries:', bw_min, bw_max
          do i = 1, npoints
            do while (.True.)
              call generate_momenta_rambo(energy, pmass, p)
              pos(0:3) = p(0:3,dau_pos(1)) + p(0:3,dau_pos(2))
              mos = dsqrt(dot(pos,pos))
              if (abs(mos-mommass)/momwdth.gt.bw_min.and.
     $            abs(mos-mommass)/momwdth.lt.bw_max) exit 
                 ! found a suitable confituration
            enddo 
            call smatrix_real(p, wgt)
            if (abs(1d0 - wgt_re/wgt_os).gt.bw_max) then
                write(*,*) '   Found', (mos-mommass) / momwdth,
     %                     '        FULL/OS:', wgt_re / wgt_os
            endif
          enddo
        enddo



      enddo


 99   close(iunit)

      return
      end


      subroutine generate_momenta_rambo(energy, mass, p)
      implicit none
      include 'nexternal.inc'
      double precision p(0:3,nexternal), mass(nexternal), energy
      double precision prambo(0:3,100),
     1                 mass_rambo(100)
      integer i

      double precision dot
      include 'run.inc' ! to acces the Bjorken x's
      
      if (nincoming.ne.2) then
        write(*,*) 'Error, nincoming must be 2!'
        stop 1 
      endif

      do i = nincoming+1, nexternal
        mass_rambo(i-nincoming) = mass(i)
      enddo

      call rambo(0, nexternal-nincoming, energy, 
     1             mass_rambo, prambo)

      p(0,1) = energy/2d0
      p(1,1) = 0d0
      p(2,1) = 0d0
      p(3,1) = energy/2d0

      p(0,2) = energy/2d0
      p(1,2) = 0d0
      p(2,2) = 0d0
      p(3,2) = -energy/2d0

      xbk(1:2)=p(0,1:2)/ebeam(1:2)

      do i = nincoming+1, nexternal
        p(0:3, i) = prambo(0:3, i-nincoming)
      enddo

      return
      end


      subroutine get_next_os_conf(iunit, nconf, dau_pos, moth_id, dau_id, iostatus)
      implicit none
      integer iunit
      integer nconf, dau_pos(2), moth_id, dau_id(2)
      integer iostatus
      integer nos
      character *80 buffer
      character *1 dummy
      nos = 0
      write(*,*) 
      do while (.true.) 
        read(iunit,'(a)', iostat=iostatus) buffer

        if (iostatus.ne.0) return ! end of file

        if (index(buffer, 'F').ne.0) then ! new FKS configuration
            read(buffer,*) dummy, nconf, nos
            write(*,*) 'Configuration ', nconf , 'has ', 
     $              nos, 'on-shell singularities' 
        endif
        if (index(buffer, 'O').ne.0) then ! new on-shell configuration
            read(buffer,*) dummy, moth_id, dau_id(1:2), dau_pos(1:2)
            write(*,*) '      Mother :', moth_id, ' Daughters:', dau_id, 
     $       "at position", dau_pos
            exit
        endif
      enddo
      

      return
      end

c
c
c Dummy routines
c
c
      subroutine initplot
      end
      subroutine outfun(pp,www)
      implicit none
      include 'nexternal.inc'
      real*8 pp(0:3,nexternal),www
      write(*,*)'This routine should not be called here'
      stop
      end


      SUBROUTINE RAMBO(LFLAG,N,ET,XM,P)
c------------------------------------------------------
c
c                       RAMBO
c
c    RA(NDOM)  M(OMENTA)  B(EAUTIFULLY)  O(RGANIZED)
c
c    A DEMOCRATIC MULTI-PARTICLE PHASE SPACE GENERATOR
c    AUTHORS:  S.D. ELLIS,  R. KLEISS,  W.J. STIRLING
c    THIS IS VERSION 1.0 -  WRITTEN BY R. KLEISS
c    (MODIFIED BY R. PITTAU)
c
c                INPUT                 OUTPUT
c
c    LFLAG= 0:   N, ET, XM             P, (DJ)
c    LFLAG= 1:   N, ET, XM, P          (DJ)
c
c    N  = NUMBER OF PARTICLES (>1, IN THIS VERSION <101)
c    ET = TOTAL CENTRE-OF-MASS ENERGY
c    XM = PARTICLE MASSES ( DIM=100 )
c    P  = PARTICLE MOMENTA ( DIM=(4,100) )
c    DJ = 1/(WEIGHT OF THE EVENT)
c
c------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION XM(100),P(0:3,100),Q(4,100),Z(100),R(4),
     .   B(3),P2(100),XM2(100),E(100),V(100),IWARN(5)
      SAVE ACC,ITMAX,IBEGIN,IWARN,Z,TWOPI,PO2LOG
      DATA ACC/1.D-14/,ITMAX/10/,IBEGIN/0/,IWARN/5*0/
C
C INITIALIZATION STEP: FACTORIALS FOR THE PHASE SPACE WEIGHT
      IF(IBEGIN.NE.0) GOTO 103
      IBEGIN=1
      TWOPI=8.*DATAN(1.D0)
      PO2LOG=LOG(TWOPI/4.)
      Z(2)=PO2LOG
      DO 101 K=3,100
  101 Z(K)=Z(K-1)+PO2LOG-2.*LOG(DFLOAT(K-2))
      DO 102 K=3,100
  102 Z(K)=(Z(K)-LOG(DFLOAT(K-1)))
C
C CHECK ON THE NUMBER OF PARTICLES
  103 IF(N.GT.1.AND.N.LT.101) GOTO 104
      PRINT 1001,N
      STOP
C
C CHECK WHETHER TOTAL ENERGY IS SUFFICIENT; COUNT NONZERO MASSES
  104 XMT=0.
      NM=0
      DO 105 I=1,N
      IF(XM(I).NE.0.D0) NM=NM+1
  105 XMT=XMT+ABS(XM(I))
      IF(XMT.LE.ET) GOTO 201
      PRINT 1002,XMT,ET
      STOP

  201 CONTINUE 
      if (lflag.eq.1) then
        w0= exp((2.*N-4.)*LOG(ET)+Z(N))
        do j= 1,N
          v(j)= sqrt(p(1,j)**2+p(2,j)**2+p(3,j)**2)
        enddo

        a1= 0.d0
        a3= 0.d0
        a2= 1.d0
        do j= 1,N
          a1= a1+v(j)/ET
          a2= a2*v(j)/p(0,j)
          a3= a3+v(j)*v(j)/p(0,j)/ET
        enddo
        wm= a1**(2*N-3)*a2/a3
        dj= 1.d0/w0/wm
        return
      endif
C
C THE PARAMETER VALUES ARE NOW ACCEPTED
C
C GENERATE N MASSLESS MOMENTA IN INFINITE PHASE SPACE

      DO 202 I=1,N
      call rans(RAN1)
      call rans(RAN2)
      call rans(RAN3)
      call rans(RAN4)
      C=2.*RAN1-1.
      S=SQRT(1.-C*C)
      F=TWOPI*RAN2
      Q(4,I)=-LOG(RAN3*RAN4)
      Q(3,I)=Q(4,I)*C
      Q(2,I)=Q(4,I)*S*COS(F)
  202 Q(1,I)=Q(4,I)*S*SIN(F)
C
C CALCULATE THE PARAMETERS OF THE CONFORMAL TRANSFORMATION
      DO 203 I=1,4
  203 R(I)=0.
      DO 204 I=1,N
      DO 204 K=1,4
  204 R(K)=R(K)+Q(K,I)
      RMAS=SQRT(R(4)**2-R(3)**2-R(2)**2-R(1)**2)
      DO 205 K=1,3
  205 B(K)=-R(K)/RMAS
      G=R(4)/RMAS
      A=1./(1.+G)
      X=ET/RMAS
C
C TRANSFORM THE Q'S CONFORMALLY INTO THE P'S
      DO 207 I=1,N
      BQ=B(1)*Q(1,I)+B(2)*Q(2,I)+B(3)*Q(3,I)
      DO 206 K=1,3
  206 P(K,I)=X*(Q(K,I)+B(K)*(Q(4,I)+A*BQ))
  207 P(0,I)=X*(G*Q(4,I)+BQ)
C
C CALCULATE WEIGHT AND POSSIBLE WARNINGS
      WT=PO2LOG
      IF(N.NE.2) WT=(2.*N-4.)*LOG(ET)+Z(N)
      IF(WT.GE.-180.D0) GOTO 208
      IF(IWARN(1).LE.5) PRINT 1004,WT
      IWARN(1)=IWARN(1)+1
  208 IF(WT.LE. 174.D0) GOTO 209
      IF(IWARN(2).LE.5) PRINT 1005,WT
      IWARN(2)=IWARN(2)+1
C
C RETURN FOR WEIGHTED MASSLESS MOMENTA
  209 IF(NM.NE.0) GOTO 210
      WT=EXP(WT)
      DJ= 1.d0/WT
      RETURN
C
C MASSIVE PARTICLES: RESCALE THE MOMENTA BY A FACTOR X
  210 XMAX=SQRT(1.-(XMT/ET)**2)
      DO 301 I=1,N
      XM2(I)=XM(I)**2
  301 P2(I)=P(0,I)**2
      ITER=0
      X=XMAX
      ACCU=ET*ACC
  302 F0=-ET
      G0=0.
      X2=X*X
      DO 303 I=1,N
      E(I)=SQRT(XM2(I)+X2*P2(I))
      F0=F0+E(I)
  303 G0=G0+P2(I)/E(I)
      IF(ABS(F0).LE.ACCU) GOTO 305
      ITER=ITER+1
      IF(ITER.LE.ITMAX) GOTO 304
      PRINT 1006,ITMAX
      GOTO 305
  304 X=X-F0/(X*G0)
      GOTO 302
  305 DO 307 I=1,N
      V(I)=X*P(0,I)
      DO 306 K=1,3
  306 P(K,I)=X*P(K,I)
  307 P(0,I)=E(I)
C
C CALCULATE THE MASS-EFFECT WEIGHT FACTOR
      WT2=1.
      WT3=0.
      DO 308 I=1,N
      WT2=WT2*V(I)/E(I)
  308 WT3=WT3+V(I)**2/E(I)
      WTM=(2.*N-3.)*LOG(X)+LOG(WT2/WT3*ET)
C
C RETURN FOR  WEIGHTED MASSIVE MOMENTA
      WT=WT+WTM
      IF(WT.GE.-180.D0) GOTO 309
      IF(IWARN(3).LE.5) PRINT 1004,WT
      IWARN(3)=IWARN(3)+1
  309 IF(WT.LE. 174.D0) GOTO 310
      IF(IWARN(4).LE.5) PRINT 1005,WT
      IWARN(4)=IWARN(4)+1
  310 WT=EXP(WT)
      DJ= 1.d0/WT
      RETURN
C
 1001 FORMAT(' RAMBO FAILS: # OF PARTICLES =',I5,' IS NOT ALLOWED')
 1002 FORMAT(' RAMBO FAILS: TOTAL MASS =',D15.6,' IS NOT',
     . ' SMALLER THAN TOTAL ENERGY =',D15.6)
 1004 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY UNDERFLOW')
 1005 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY  OVERFLOW')
 1006 FORMAT(' RAMBO WARNS:',I3,' ITERATIONS DID NOT GIVE THE',
     . ' DESIRED ACCURACY =',D15.6)
      END


      subroutine rans(rand)
c     Just a wrapper to ran2      
      implicit none
      double precision rand, ran2
      rand = ran2()
      return 
      end
