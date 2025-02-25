c
c Example analysis for "p p > leptons [QCD]" process.
c
c It features the HwU format for histogram booking and output.
c The details of how to process/manipulate the resulting .HwU file,
c in particular how to plot it using gnuplot, I refer the reader to this
c FAQ:
c
c      https://answers.launchpad.net/mg5amcnlo/+faq/2671
c
c It mostly relies on using the following madgraph5 module in standalone
c
c  <MG5_aMC_install_dir>/madgraph/various/histograms.py
c
c You can learn about how to run it and what options are available with
c
c  python <MG5_aMC_install_dir>/madgraph/various/histograms.py --help
c
C----------------------------------------------------------------------
      SUBROUTINE RCLOS()
C     DUMMY IF HBOOK IS USED
C----------------------------------------------------------------------
      END


C----------------------------------------------------------------------
      SUBROUTINE HWABEG
C     USER''S ROUTINE FOR INITIALIZATION
C----------------------------------------------------------------------
      INCLUDE 'HERWIG65.INC'
      include 'reweight0.inc'
c
c     The type suffix of the histogram title, with syntax 
c     |T@<type_name> is semantic in the HwU format. It allows for
c     various filtering when using the histogram.py module
c     (see comment at the beginning of this file).
c     It is in general a good idea to keep the same title for the
c     same observable (if they use the same range) and differentiate
c     them only using the type suffix.
c
      character*8 HwUtype(2)
      data HwUtype/'|T@NOCUT','|T@CUT  '/
      integer j,kk,l,jpr
      integer nsingle,ncorr,nlepton,nplots,ncuts
      common/cplots/nsingle,ncorr,nlepton,nplots,ncuts
      integer MAXELM,MAXELP,MAXMUM,MAXMUP
      common/cmaxlep/MAXELM,MAXELP,MAXMUM,MAXMUP
      character*60 tmpstr1,tmpstr2,tmpstr3
      integer maxcuts
      parameter (maxcuts=2)
      character*5 cc(maxcuts)
      data cc/'     ','cuts '/
      real*8 pi
      PARAMETER (PI=3.14159265358979312D0)
      integer sbin(100),cbin(100)
      real*8 smin(100),smax(100)
      real*8 cmin(100),cmax(100)
      integer i,icuts
      integer l0,ilep1,ilep2,io,ipair
      integer nwgt,max_weight,nwgt_analysis
      common/cnwgt/nwgt
      common/c_analysis/nwgt_analysis
      character*50 weights_info(max_weight_shower)
      common/cwgtsinfo/weights_info
c Initialize histograms
      call HwU_inithist(nwgt,weights_info)
c Set method for error estimation to '0', i.e., use Poisson statistics
c for the uncertainty estimate
      call set_error_estimation(0)
      nwgt_analysis=nwgt
c The analysis will consider:
c  MAXELM e-
c  MAXELP e+
c  MAXMUM mu-
c  MAXMUP mu+
c Set these variables here and only here
      MAXELM=0
      MAXELP=0
      MAXMUM=1
      MAXMUP=1
      nlepton=MAXELM+MAXELP+MAXMUM+MAXMUP
c For each weight and cut configuration, there will be:
c   nsingle single-inclusive plots (e.g., pt and y)
c   ncorr correlation plots (e.g., invM, ptpair, dphi, Deltay)
c to be repeated for each of the nlepton leptons and 
c nlepton*(nlepton-1)/2 lepton pairs respectively
      ncuts=1
      if(ncuts.gt.maxcuts)then
        WRITE(*,*)ncuts,maxcuts
        CALL HWWARN('HWABEG',500)
      endif
      nsingle=2
      ncorr=4
      nplots=nlepton * nsingle +
     #       nlepton*(nlepton-1)/2 * ncorr
      do i=1,100
        sbin(i)=0
        smin(i)=0.d0
        smax(i)=0.d0
        cbin(i)=0
        cmin(i)=0.d0
        cmax(i)=0.d0
      enddo
c pt
      sbin(1)=100
      smin(1)=0.d0
      smax(1)=200.d0
c y
      sbin(2)=100
      smin(2)=-5.d0
      smax(2)=5.d0
c inv M
      cbin(1)=100
      cmin(1)=0.d0
      cmax(1)=500.d0
c ptpair
      cbin(2)=100
      cmin(2)=0.d0
      cmax(2)=200.d0
c dphi
      cbin(3)=40
      cmin(3)=0.d0
      cmax(3)=pi
c Delta y
      cbin(4)=100
      cmin(4)=-5.d0
      cmax(4)=5.d0
c
      do icuts=1,ncuts
        l0=(icuts-1)*nplots
        do ilep1=1,nlepton
        do io=1,nsingle
          l=l0+nsingle*(ilep1-1)+io
          write(tmpstr1,'(i3)')ilep1
          write(tmpstr3,'(i3)')io
          call HwU_book(l,'sing '//tmpstr1(1:3)//
     &    ' '//tmpstr3(1:3)//' '
     &       //HwUtype(icuts),sbin(io),smin(io),smax(io))
        enddo
        enddo
        ipair=0
        do ilep1=1,nlepton-1
        do ilep2=ilep1+1,nlepton
        ipair=ipair+1
        do io=1,ncorr
          l=l0+nlepton*nsingle+ncorr*(ipair-1)+io
          write(tmpstr1,'(i3)')ilep1
          write(tmpstr2,'(i3)')ilep2
          write(tmpstr3,'(i3)')io
          call HwU_book(l,'corr '//tmpstr1(1:3)//' '//
     &       tmpstr2(1:3)//' '//tmpstr3(1:3)//
     &     ' '//HwUtype(icuts),cbin(io),cmin(io),cmax(io))
        enddo
        enddo
        enddo
      enddo
 999  END

C----------------------------------------------------------------------
      SUBROUTINE HWAEND
C     USER''S ROUTINE FOR TERMINAL CALCULATIONS, HISTOGRAM OUTPUT, ETC
C----------------------------------------------------------------------
      INCLUDE 'HERWIG65.INC'
      REAL*8 XNORM
      integer nsingle,ncorr,nlepton,nplots,ncuts
      common/cplots/nsingle,ncorr,nlepton,nplots,ncuts
      INTEGER I,J,kk,l,nwgt_analysis
      integer l0,ilep1,ilep2,io,ipair,icuts
      integer NPL
      parameter(NPL=15000)
      common/c_analysis/nwgt_analysis
c Convert from nb to pb using xnorm. This assumes that event weights
c *average* to the total cross section, so no extra weight needed
      xnorm=1d3
c Collect accumulated results
      call finalize_histograms(nevhep)
c Write the histograms to disk. 
      open (unit=99,file='MADatNLO.HwU',status='unknown')
      call HwU_output(99,xnorm)
      close (99)
      return
      END

C----------------------------------------------------------------------
      SUBROUTINE HWANAL
C     USER''S ROUTINE TO ANALYSE DATA FROM EVENT
C----------------------------------------------------------------------
      INCLUDE 'HERWIG65.INC'
      include 'reweight0.inc'
      DOUBLE PRECISION HWVDOT,PSUM(4)
      INTEGER ICHSUM,ICHINI,IHEP,IST,ID
      LOGICAL DIDSOF
      integer nwgt_analysis,max_weight
      common/c_analysis/nwgt_analysis
      integer maxRWGT
      parameter (maxRWGT=100)
      double precision wgtxsecRWGT(maxRWGT)
      parameter (max_weight=maxscales*maxscales+maxpdfs+maxRWGT+1)
      double precision ww(max_weight),www(max_weight)
      common/cww/ww
c
      integer nsingle,ncorr,nlepton,nplots,ncuts
      common/cplots/nsingle,ncorr,nlepton,nplots,ncuts
      integer MAXELM,MAXELP,MAXMUM,MAXMUP
      common/cmaxlep/MAXELM,MAXELP,MAXMUM,MAXMUP
c
      integer i,j,ilep,ilep1,ilep2,io,NELP,NELM,NMUP,NMUM,
     # IPAIR,kk,L,L0,ICUTS
      real*8 GETPTV4,GETRAPIDITYV4,GETINVMV4,GETDELPHIV4,PPAIR(4),
     # PELM(4,25),PELP(4,25),PMUM(4,25),PMUP(4,25),PLEP(4,25),
     # obs(100)
c
      IF (IERROR.NE.0) RETURN
      IF (WW(1).EQ.0D0) THEN
         WRITE(*,*)'WW(1) = 0. Stopping'
         STOP
      ENDIF
C INCOMING PARTONS MAY TRAVEL IN THE SAME DIRECTION: IT''S A POWER-SUPPRESSED
C EFFECT, SO THROW THE EVENT AWAY
      IF(SIGN(1.D0,PHEP(3,4)).EQ.SIGN(1.D0,PHEP(3,5)))THEN
        CALL HWWARN('HWANAL',111)
        GOTO 999
      ENDIF
      DO I=1,nwgt_analysis
         WWW(I)=EVWGT*ww(i)/ww(1)
      ENDDO
      CALL HWVSUM(4,PHEP(1,1),PHEP(1,2),PSUM)
      CALL HWVSCA(4,-1D0,PSUM,PSUM)
      ICHSUM=0
      ICHINI=ICHRG(IDHW(1))+ICHRG(IDHW(2))
      NELP=0
      NELM=0
      NMUP=0
      NMUM=0
      DO 100 IHEP=1,NHEP
        IST=ISTHEP(IHEP)
        ID=IDHEP(IHEP)
        IF (IST.EQ.1) THEN
          CALL HWVSUM(4,PHEP(1,IHEP),PSUM,PSUM)
          ICHSUM=ICHSUM+ICHRG(IDHW(IHEP))
        ENDIF
c Find electrons
        IF(ID.EQ.11.AND.IST.EQ.1) THEN
           NELM=NELM+1
           DO I=1,4
              PELM(I,NELM)=PHEP(I,IHEP)
           ENDDO
c Find positrons
        ELSEIF(ID.EQ.-11.AND.IST.EQ.1) THEN
           NELP=NELP+1
           DO I=1,4
              PELP(I,NELP)=PHEP(I,IHEP)
           ENDDO
c Find mu-
        ELSEIF(ID.EQ.13.AND.IST.EQ.1) THEN
           NMUM=NMUM+1
           DO I=1,4
              PMUM(I,NMUM)=PHEP(I,IHEP)
           ENDDO
c Find mu+
        ELSEIF(ID.EQ.-13.AND.IST.EQ.1) THEN
           NMUP=NMUP+1
           DO I=1,4
              PMUP(I,NMUP)=PHEP(I,IHEP)
           ENDDO
        ENDIF
  100 CONTINUE
      IF( NELM.LT.MAXELM .OR. NELP.LT.MAXELP .OR.
     #    NMUM.LT.MAXMUM .OR. NMUP.LT.MAXMUP )THEN
        CALL HWUEPR
        WRITE(*,*)NELM,NELP,NMUM,NMUP
        WRITE(*,*)MAXELM,MAXELP,MAXMUM,MAXMUP
        CALL HWWARN('HWANAL',500)
      ENDIF
c Keep the first MAX** leptons of the species **
      ILEP=0
      DO J=1,MAXELM
        ILEP=ILEP+1
        DO I=1,4
           PLEP(I,ILEP)=PELM(I,J)
        ENDDO
      ENDDO
      DO J=1,MAXELP
        ILEP=ILEP+1
        DO I=1,4
           PLEP(I,ILEP)=PELP(I,J)
        ENDDO
      ENDDO
      DO J=1,MAXMUM
        ILEP=ILEP+1
        DO I=1,4
           PLEP(I,ILEP)=PMUM(I,J)
        ENDDO
      ENDDO
      DO J=1,MAXMUP
        ILEP=ILEP+1
        DO I=1,4
           PLEP(I,ILEP)=PMUP(I,J)
        ENDDO
      ENDDO
      IF( ILEP.NE.nlepton )THEN
        CALL HWUEPR
        WRITE(*,*)ILEP,nlepton
        CALL HWWARN('HWANAL',501)
      ENDIF
c
      DO ILEP1=1,NLEPTON
        OBS(1)=GETPTV4(PLEP(1,ILEP1))
        OBS(2)=GETRAPIDITYV4(PLEP(1,ILEP1))
        DO KK=1,NWGT_ANALYSIS
        DO ICUTS=1,NCUTS
          L0=(KK-1)*NCUTS*NPLOTS+(ICUTS-1)*NPLOTS
          DO IO=1,NSINGLE
            L=L0+NSINGLE*(ILEP1-1)+IO
            CALL HwU_FILL(L,OBS(IO),WWW(KK))
          ENDDO
        ENDDO
        ENDDO
      ENDDO
c
      IPAIR=0
      DO ILEP1=1,NLEPTON-1
        DO ILEP2=ILEP1+1,NLEPTON
          IPAIR=IPAIR+1
          DO I=1,4
             PPAIR(I)=PLEP(I,ILEP1)+PLEP(I,ILEP2)
          ENDDO
          OBS(1)=GETINVMV4(PPAIR)
          OBS(2)=GETPTV4(PPAIR)
          OBS(3)=GETDELPHIV4(PLEP(1,ILEP1),PLEP(1,ILEP2))
          OBS(4)=GETRAPIDITYV4(PLEP(1,ILEP1))-
     #           GETRAPIDITYV4(PLEP(1,ILEP2))
          DO ICUTS=1,NCUTS
            L0=(ICUTS-1)*NPLOTS
            DO IO=1,NCORR
              L=L0+NLEPTON*NSINGLE+NCORR*(IPAIR-1)+IO
              CALL HwU_FILL(L,OBS(IO),WWW)
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      call HwU_add_points
 999  END


      FUNCTION RANDA(SEED)
*     -----------------
* Ref.: K. Park and K.W. Miller, Comm. of the ACM 31 (1988) p.1192
* Use seed = 1 as first value.
*
      IMPLICIT INTEGER(A-Z)
      DOUBLE PRECISION MINV,RANDA
      SAVE
      PARAMETER(M=2147483647,A=16807,Q=127773,R=2836)
      PARAMETER(MINV=0.46566128752458d-09)
      HI = SEED/Q
      LO = MOD(SEED,Q)
      SEED = A*LO - R*HI
      IF(SEED.LE.0) SEED = SEED + M
      RANDA = SEED*MINV
      END




      function getrapidity(en,pl)
      implicit none
      real*8 getrapidity,en,pl,tiny,xplus,xminus,y
      parameter (tiny=1.d-8)
c
      xplus=en+pl
      xminus=en-pl
      if(xplus.gt.tiny.and.xminus.gt.tiny)then
        if( (xplus/xminus).gt.tiny.and.(xminus/xplus).gt.tiny )then
          y=0.5d0*log( xplus/xminus )
        else
          y=sign(1.d0,pl)*1.d8
        endif
      else
        y=sign(1.d0,pl)*1.d8
      endif
      getrapidity=y
      return
      end


      function getpseudorap(en,ptx,pty,pl)
      implicit none
      real*8 getpseudorap,en,ptx,pty,pl,tiny,pt,eta,th
      parameter (tiny=1.d-5)
c
      pt=sqrt(ptx**2+pty**2)
      if(pt.lt.tiny.and.abs(pl).lt.tiny)then
        eta=sign(1.d0,pl)*1.d8
      else
        th=atan2(pt,pl)
        eta=-log(tan(th/2.d0))
      endif
      getpseudorap=eta
      return
      end


      function getinvm(en,ptx,pty,pl)
      implicit none
      real*8 getinvm,en,ptx,pty,pl,tiny,tmp
      parameter (tiny=1.d-5)
c
      tmp=en**2-ptx**2-pty**2-pl**2
      if(tmp.gt.0.d0)then
        tmp=sqrt(tmp)
      elseif(tmp.gt.-tiny)then
        tmp=0.d0
      else
        write(*,*)'Attempt to compute a negative mass'
        stop
      endif
      getinvm=tmp
      return
      end


      function getdelphi(ptx1,pty1,ptx2,pty2)
      implicit none
      real*8 getdelphi,ptx1,pty1,ptx2,pty2,tiny,pt1,pt2,tmp
      parameter (tiny=1.d-5)
c
      pt1=sqrt(ptx1**2+pty1**2)
      pt2=sqrt(ptx2**2+pty2**2)
      if(pt1.ne.0.d0.and.pt2.ne.0.d0)then
        tmp=ptx1*ptx2+pty1*pty2
        tmp=tmp/(pt1*pt2)
        if(abs(tmp).gt.1.d0+tiny)then
          write(*,*)'Cosine larger than 1'
          stop
        elseif(abs(tmp).ge.1.d0)then
          tmp=sign(1.d0,tmp)
        endif
        tmp=acos(tmp)
      else
        tmp=1.d8
      endif
      getdelphi=tmp
      return
      end


      function getdr(en1,ptx1,pty1,pl1,en2,ptx2,pty2,pl2)
      implicit none
      real*8 getdr,en1,ptx1,pty1,pl1,en2,ptx2,pty2,pl2,deta,dphi,
     # getpseudorap,getdelphi
c
      deta=getpseudorap(en1,ptx1,pty1,pl1)-
     #     getpseudorap(en2,ptx2,pty2,pl2)
      dphi=getdelphi(ptx1,pty1,ptx2,pty2)
      getdr=sqrt(dphi**2+deta**2)
      return
      end


      function getdry(en1,ptx1,pty1,pl1,en2,ptx2,pty2,pl2)
      implicit none
      real*8 getdry,en1,ptx1,pty1,pl1,en2,ptx2,pty2,pl2,deta,dphi,
     # getrapidity,getdelphi
c
      deta=getrapidity(en1,pl1)-
     #     getrapidity(en2,pl2)
      dphi=getdelphi(ptx1,pty1,ptx2,pty2)
      getdry=sqrt(dphi**2+deta**2)
      return
      end


      function getptv(p)
      implicit none
      real*8 getptv,p(5)
c
      getptv=sqrt(p(1)**2+p(2)**2)
      return
      end


      function getpseudorapv(p)
      implicit none
      real*8 getpseudorapv,p(5)
      real*8 getpseudorap
c
      getpseudorapv=getpseudorap(p(4),p(1),p(2),p(3))
      return
      end


      function getrapidityv(p)
      implicit none
      real*8 getrapidityv,p(5)
      real*8 getrapidity
c
      getrapidityv=getrapidity(p(4),p(3))
      return
      end


      function getdrv(p1,p2)
      implicit none
      real*8 getdrv,p1(5),p2(5)
      real*8 getdr
c
      getdrv=getdr(p1(4),p1(1),p1(2),p1(3),
     #             p2(4),p2(1),p2(2),p2(3))
      return
      end


      function getinvmv(p)
      implicit none
      real*8 getinvmv,p(5)
      real*8 getinvm
c
      getinvmv=getinvm(p(4),p(1),p(2),p(3))
      return
      end


      function getdelphiv(p1,p2)
      implicit none
      real*8 getdelphiv,p1(5),p2(5)
      real*8 getdelphi
c
      getdelphiv=getdelphi(p1(1),p1(2),
     #                     p2(1),p2(2))
      return
      end


      function getptv4(p)
      implicit none
      real*8 getptv4,p(4)
c
      getptv4=sqrt(p(1)**2+p(2)**2)
      return
      end


      function getpseudorapv4(p)
      implicit none
      real*8 getpseudorapv4,p(4)
      real*8 getpseudorap
c
      getpseudorapv4=getpseudorap(p(4),p(1),p(2),p(3))
      return
      end


      function getrapidityv4(p)
      implicit none
      real*8 getrapidityv4,p(4)
      real*8 getrapidity
c
      getrapidityv4=getrapidity(p(4),p(3))
      return
      end


      function getdrv4(p1,p2)
      implicit none
      real*8 getdrv4,p1(4),p2(4)
      real*8 getdr
c
      getdrv4=getdr(p1(4),p1(1),p1(2),p1(3),
     #              p2(4),p2(1),p2(2),p2(3))
      return
      end


      function getinvmv4(p)
      implicit none
      real*8 getinvmv4,p(4)
      real*8 getinvm
c
      getinvmv4=getinvm(p(4),p(1),p(2),p(3))
      return
      end


      function getdelphiv4(p1,p2)
      implicit none
      real*8 getdelphiv4,p1(4),p2(4)
      real*8 getdelphi
c
      getdelphiv4=getdelphi(p1(1),p1(2),
     #                      p2(1),p2(2))
      return
      end


      function getcosv4(q1,q2)
      implicit none
      real*8 getcosv4,q1(4),q2(4)
      real*8 xnorm1,xnorm2,tmp
c
      if(q1(4).lt.0.d0.or.q2(4).lt.0.d0)then
        getcosv4=-1.d10
        return
      endif
      xnorm1=sqrt(q1(1)**2+q1(2)**2+q1(3)**2)
      xnorm2=sqrt(q2(1)**2+q2(2)**2+q2(3)**2)
      if(xnorm1.lt.1.d-6.or.xnorm2.lt.1.d-6)then
        tmp=-1.d10
      else
        tmp=q1(1)*q2(1)+q1(2)*q2(2)+q1(3)*q2(3)
        tmp=tmp/(xnorm1*xnorm2)
        if(abs(tmp).gt.1.d0.and.abs(tmp).le.1.001d0)then
          tmp=sign(1.d0,tmp)
        elseif(abs(tmp).gt.1.001d0)then
          write(*,*)'Error in getcosv4',tmp
          stop
        endif
      endif
      getcosv4=tmp
      return
      end



      function getmod(p)
      implicit none
      double precision p(4),getmod

      getmod=sqrt(p(1)**2+p(2)**2+p(3)**2)

      return
      end



      subroutine getperpenv4(q1,q2,qperp)
c Normal to the plane defined by \vec{q1},\vec{q2}
      implicit none
      real*8 q1(4),q2(4),qperp(4)
      real*8 xnorm1,xnorm2
      integer i
c
      xnorm1=sqrt(q1(1)**2+q1(2)**2+q1(3)**2)
      xnorm2=sqrt(q2(1)**2+q2(2)**2+q2(3)**2)
      if(xnorm1.lt.1.d-6.or.xnorm2.lt.1.d-6)then
        do i=1,4
          qperp(i)=-1.d10
        enddo
      else
        qperp(1)=q1(2)*q2(3)-q1(3)*q2(2)
        qperp(2)=q1(3)*q2(1)-q1(1)*q2(3)
        qperp(3)=q1(1)*q2(2)-q1(2)*q2(1)
        do i=1,3
          qperp(i)=qperp(i)/(xnorm1*xnorm2)
        enddo
        qperp(4)=1.d0
      endif
      return
      end





      subroutine boostwdir2(chybst,shybst,chybstmo,xd,xin,xout)
c chybstmo = chybst-1; if it can be computed analytically it improves
c the numerical accuracy
      implicit none
      real*8 chybst,shybst,chybstmo,xd(1:3),xin(0:3),xout(0:3)
      real*8 tmp,en,pz
      integer i
c
      if(abs(xd(1)**2+xd(2)**2+xd(3)**2-1).gt.1.d-6)then
        write(*,*)'Error #1 in boostwdir2',xd
        stop
      endif
c
      en=xin(0)
      pz=xin(1)*xd(1)+xin(2)*xd(2)+xin(3)*xd(3)
      xout(0)=en*chybst-pz*shybst
      do i=1,3
        xout(i)=xin(i)+xd(i)*(pz*chybstmo-en*shybst)
      enddo
c
      return
      end




      subroutine boostwdir3(chybst,shybst,chybstmo,xd,xxin,xxout)
      implicit none
      real*8 chybst,shybst,chybstmo,xd(1:3),xxin(4),xxout(4)
      real*8 xin(0:3),xout(0:3)
      integer i
c
      do i=1,4
         xin(mod(i,4))=xxin(i)
      enddo
      call boostwdir2(chybst,shybst,chybstmo,xd,xin,xout)
      do i=1,4
         xxout(i)=xout(mod(i,4))
      enddo
c
      return
      end




      subroutine getwedge(p1,p2,pout)
      implicit none
      real*8 p1(4),p2(4),pout(4)

      pout(1)=p1(2)*p2(3)-p1(3)*p2(2)
      pout(2)=p1(3)*p2(1)-p1(1)*p2(3)
      pout(3)=p1(1)*p2(2)-p1(2)*p2(1)
      pout(4)=0d0

      return
      end

