!!
!!  File DD_aux.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!

**********************************************************************
        subroutine DDsetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &                        xmir2,xmx2)
**********************************************************************
*       parameter initalization of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        real*8 xmx2(100)
        integer i

c UV parameters
        deltauv  = xdeltauv 
        muv2     = xmuv2   

c IR parameters
        delta2ir = xdelta2ir
        delta1ir = xdelta1ir
        mir2     = xmir2    

c small mass parameters
        do i=1,100
          mx2(i) = xmx2(i) 
        enddo

c determine CPU precision
        dprec = 1d0
        dres_old = 2d0   
        do i=1,1000
          dprec = dprec/2d0
          dres = exp(log(1d0+dprec))
          if (abs(dres).ge.abs(dres_old)) exit
          dres_old = dres
        enddo
        dprec_dd = dprec*8d0

        end

**********************************************************************
        subroutine DDgetparam(xdeltauv,xmuv2,xdelta2ir,xdelta1ir,
     &                        xmir2,xmx2)
**********************************************************************
*       read parameters of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        real*8 xmx2(100)
        integer i

c UV parameters
        xdeltauv  = deltauv 
        xmuv2     = muv2   

c IR parameters
        xdelta2ir = delta2ir
        xdelta1ir = delta1ir
        xmir2     = mir2    

c small mass parameters
        do i=1,100
          xmx2(i) = mx2(i) 
        enddo

        end

**********************************************************************
        subroutine DDsetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &                       xoutlevel,xoutchannel)
**********************************************************************
*       set mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer xmode34,xmode5,xmode6,xoutlevel,xoutchannel

        cacc       = xcacc
        dacc       = xdacc
        mode34     = xmode34
        mode5      = xmode5
        mode6      = xmode6
        outlevel   = xoutlevel
        outchannel = xoutchannel

        end

**********************************************************************
        subroutine DDgetmode(xcacc,xdacc,xmode34,xmode5,xmode6,
     &                       xoutlevel,xoutchannel)
**********************************************************************
*       read mode of DD library
*---------------------------------------------------------------------
*       9.4.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer xmode34,xmode5,xmode6,xoutlevel,xoutchannel

        xcacc       = cacc
        xdacc       = dacc
        xmode34     = mode34
        xmode5      = mode5
        xmode6      = mode6
        xoutlevel   = outlevel
        xoutchannel = outchannel

        end

**********************************************************************
        subroutine DDsetaccthr(accthr)
**********************************************************************
*       set threshold for accuracy flags in DD library
*       acc > accthr: accuracy flag accflag raised 
*                     (use accthr = 10^-4 - 10^-8 or so)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)
        integer n

        do n=1,nmax
          aimacc(n) = accthr
        enddo

        end

**********************************************************************
        subroutine DDseterrthr(errthr)
**********************************************************************
*       set threshold for error flags in DD library
*       err > errthr: error flag errflag raised
*                     (use errthr = 0.1  - 1 or so)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)
        integer n

        do n=1,nmax
          erracc(n) = errthr
        enddo

        end

**********************************************************************
        subroutine DDgetacc(accrel,accabs,accrel2,accabs2,
     &                      n,rank,xaccflag,xerrflag,id)
**********************************************************************
*       get accuracy estimate for last master call of DD library
*
*       rank       = highest rank calculated by DD for n-point fct.
*       accrel(n)  = relative accuracy of rank <= rmax in n-point fct.
*       accabs(n)  = absolute accuracy 
*                    for coefficients T_ij... (i,j,...=/=0)
*       accrel2(n) = as accrel(n), but relevant for whole tensor
*       accabs2(n) = as accabs(n), but relevant for whole tensor
*
*       accflag = 0,1: accuracy better than aimacc(n) (yes/no=0/1)
*       errflag = 0,1: internal problems in DD       (no/yes=0/1)
*---------------------------------------------------------------------
*       28.3.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)                                        

        real*8 accrel(0:rmax), accabs(0:rmax)
        real*8 accrel2(0:rmax),accabs2(0:rmax)
        integer r,n,rank,xaccflag,xerrflag,id

        n        = nmaster
        rank     = r2master
        xaccflag = accflag
        xerrflag = errflag
        do r=0,rank
          accrel(r)  = resaccrel(id,r)
          accabs(r)  = resaccabs(id,r)
          accrel2(r) = resaccrel2(id,r)
          accabs2(r) = resaccabs2(id,r)
        enddo

        end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        FUNCTION cspen_dd(Z)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SPENCE-FUNKTION KOMPLEX, FREI NACH HOLLIK                     C
C---------------------------------------------------------------------C
C       20.07.83    LAST CHANGED 10.05.89        ANSGAR DENNER        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        COMPLEX*16 cspen_dd,W,SUM,Z,U                                  
        REAL*8 RZ,AZ,A1                                                
c        REAL*8 B(9)/                                                   
        REAL*8 :: B(9) = (/
     1   0.1666666666666666666666666667D0,                             
     2  -0.0333333333333333333333333333D0,                             
     3   0.0238095238095238095238095238D0,                             
     4  -0.0333333333333333333333333333D0,                             
     5   0.0757575757575757575757575758D0,                             
     6  -0.2531135531135531135531135531D0,                             
     7   1.1666666666666666666666666667D0,                             
     8  -7.09215686274509804D0         ,                               
     9  54.97117794486215539D0         /)
c     9  54.97117794486215539D0         /                               
C     BEACHTE:                 B(N)=B2N                                
C     B(1)=1./6.                                                       
C     B(2)=-1./30.                                                     
C     B(3)=1./42.                                                      
C     B(4)=-1./30.                                                     
C     B(5)=5./66.                                                      
C     B(6)=-691./2730.                                                 
C     B(7)=7./6.                                                       
C     B(8)=-3617./510.                                                 
C     B(9)=43867./798.                                                 
C     B(10)=-174611./330.                                              
C     B(11)=854513./138.                                               
C     PI=3.1415926535897932384                                         
C     PI*PI/6.=1.6449..., PI*PI/3=3.28986...                           
C                                                                      
      Z =Z*DCMPLX(1D0)                                                 
      RZ=DREAL(Z)                                                      
      AZ=CDABS(Z)                                                      
      A1=CDABS(1D0-Z)                                                  
C     IF((SNGL(RZ) .EQ. 0.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN
C ---> CHANGED  10.5.89                                                
      IF(AZ .LT. 1D-20) THEN                                           
        cspen_dd=-log(1D0-Z)                                            
        RETURN                                                         
      END IF                                                           
c      IF((SNGL(RZ) .EQ. 1.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN
c ---> changed 5.7.94
       IF( (ABS(RZ-1D0).LT.1D-18) .AND. (ABS(DIMAG(Z)).LT.1D-18) ) THEN
        cspen_dd=1.64493406684822643D0                     
        RETURN                                                         
      END IF                                                           
      IF(RZ.GT.5D-1) GOTO 20                                           
      IF(AZ.GT.1D0) GOTO 10                                            
      W=-log(1D0-Z)                                                  
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 2                                     
      DO 1 K=1,9                                                       
      U=U*W*W/dble(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 2                            
      SUM=SUM+U*B(K)                                                   
 1    CONTINUE                                                         
 2    cspen_dd=SUM                        
      RETURN                                                           
10    W=-log(1D0-1D0/Z)                                              
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 12                                    
                                                                       
      DO 11 K=1,9                                                      
      U=U*W*W/dble(2*K*(2*K+1))                                      
      IF(CDABS(B(K)*U/SUM).LT.1D-20) GOTO 12                           
      SUM=SUM+U*B(K)                                                   
11    CONTINUE                                                         
12    cspen_dd=-SUM-1.64493406684822643D0-.5D0*log(-Z)**2               
      RETURN                                                           
20    IF(A1.GT.1D0) GOTO 30                                            
      W=-log(Z)                                                      
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 22                                    
      DO 21 K=1,9                                                      
      U=U*W*W/dble(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 22                           
      SUM=SUM+U*B(K)                                                   
21    CONTINUE                                                         
22    cspen_dd=-SUM+1.64493406684822643D0-log(Z)*log(1D0-Z)           
      RETURN                                                           
30    W=log(1D0-1D0/Z)                                               
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 32                                    
      DO 31 K=1,9                                                      
      U=U*W*W/dble(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 32                           
      SUM=SUM+U*B(K)                                                   
31    CONTINUE                                                         
32    cspen_dd=SUM+3.28986813369645287D0                      
     *               +.5D0*log(Z-1D0)**2-log(Z)*log(1D0-Z)       
        END                                                            

***********************************************************************
        FUNCTION eta_dd(C1,C2)
***********************************************************************
*       COMPLEX ETA-FUNKTION                                           
*---------------------------------------------------------------------*
*       8.06.90    ANSGAR DENNER                                       
***********************************************************************
        use DD_global
        IMPLICIT   real*8(A-Z)                                        

        COMPLEX*16 eta_dd,C1,C2                          
        REAL*8     PI,IM1,IM2,IM12                                     

        PI     = 4D0*DATAN(1D0)                                        
        IM1    = DIMAG(C1)                                             
        IM2    = DIMAG(C2)                                             
        IM12   = DIMAG(C1*C2)                                          

        if (((IM1.eq.0d0).and.(DREAL(C1).lt.0d0)).or.
     &      ((IM2.eq.0d0).and.(DREAL(C2).lt.0d0)).or.
     &      ((IM12.eq.0d0).and.(DREAL(C1*C2).lt.0d0))) then

          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'eta function on cut !!!'
            write(outchannel,*) 'C1    = ',C1
            write(outchannel,*) 'C2    = ',C2
            write(outchannel,*) 'C1*C2 = ',C1*C2
            call DD_debugoutput()
          endif
          stopflag = min(-4,stopflag)
        endif
                                                                       
        IF(IM1.LT.0D0.AND.IM2.LT.0D0.AND.IM12.GT.0D0) THEN             
            eta_dd = DCMPLX(0D0,2D0*PI)  
        ELSE IF (IM1.GT.0D0.AND.IM2.GT.0D0.AND.IM12.LT.0D0) THEN       
            eta_dd = DCMPLX(0D0,-2D0*PI)              
        ELSE                                                           
            eta_dd = DCMPLX(0D0)                           
        END IF                                                         
        END                                                            

***********************************************************************
        function sqe_dd(a,b,c)
***********************************************************************
*       Solution of quadratic equation a*x^2 + b*x + c = 0
*----------------------------------------------------------------------
*       9.4.08  Stefan Dittmaier                                       
***********************************************************************
        use DD_global
        implicit real*8 (a-z)                                        

        complex*16 a,b,c,sqe_dd,x1,x2

        if ((a.eq.(0d0,0d0)).and.(b.eq.(0d0,0d0))) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'sqe_dd: a=b=0 not allowed'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          sqe_dd = 0d0
          stopflag = min(-7,stopflag)
        elseif ((a.eq.(0d0,0d0)).and.(b.ne.(0d0,0d0))) then
          sqe_dd = -c/b
        else
          x1=(-b+sqrt(b**2-4d0*a*c))/2d0/a
          x2=(-b-sqrt(b**2-4d0*a*c))/2d0/a
          if (abs(x1).gt.abs(x2)) then
             sqe_dd=x1
          else
             sqe_dd=x2
          endif
        endif

        end                                                            

***********************************************************************
        function xdet_dd(A,n)
***********************************************************************
*       determinant of complex nxn-matrix A
*----------------------------------------------------------------------
*       10.6.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        complex*16 A(n,n),Q(n,n),R(n,n),detQ,xdet_dd
        integer i

        call xQRdecomp_dd(A,Q,R,detQ,n)

        xdet_dd = detQ*r(n,n)
        do i=1,n-1
          xdet_dd = xdet_dd*r(i,i)
        enddo

        end

***********************************************************************
        subroutine xinverse_dd(A,iA,detA,n)
***********************************************************************
*       inverse of complex nxn-matrix A
*----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        complex*16 A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n),detQ,w,detA
        integer i,j,k

        call xQRdecomp_dd(A,Q,R,detQ,n)

        detA = detQ*R(n,n)
        do i=1,n-1
          detA = detA*R(i,i)
        enddo

c invert R
        do i=n,1,-1
          do j=1,n
            if (j.lt.i) then
              iR(i,j) = 0d0
            else
              w = 0d0
              do k=i+1,n
                w = w + R(i,k)*iR(k,j)
              enddo
              if (i.eq.j) then
                iR(i,j) = (1d0-w)/R(i,i)
              else
                iR(i,j) = -w/R(i,i)
              endif
            endif
          enddo
        enddo

c inverse of A
        do i=1,n
          do j=1,n
            iA(i,j) = 0d0
            do k=1,n
              iA(i,j) = iA(i,j) + iR(i,k)*dconjg(Q(j,k))
            enddo
          enddo
        enddo

        end

***********************************************************************
        subroutine xQRdecomp_dd(A,Q,R,detQ,n)
***********************************************************************
*       QR-decomposition of complex nxn-matrix A 
*       using Householder matrices
*       Q = orthogonal matrix, R = upper triangle matrix
*----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        complex*16 A(n,n),Q(n,n),R(n,n),w(n),wr(n),qw(n),detQ,aux,kap
        complex*16 rmax
        integer i,j,k,imax

        detQ = 1d0

c*** start with R=A and Q=unit matrix 
        do i=1,n
          do j=1,n
            r(i,j) = a(i,j)
            if (i.eq.j) then
              q(i,j) = 1d0
            else
              q(i,j) = 0d0
            endif
          enddo
        enddo

c*** n-1 steps of decomposition
        do 200 i=1,n-1

c exchange rows such that |r(i,i)| is maximal
          imax = i
          rmax = r(i,i)
          do j=i+1,n
            if (abs(r(j,i)).gt.abs(rmax)) then
              imax = j
              rmax = r(j,i)
            endif
          enddo
          if (imax.ne.i) then
            do j=1,n
              aux       = r(i,j)
              r(i,j)    = r(imax,j)
              r(imax,j) = aux
              aux       = q(j,i)
              q(j,i)    = q(j,imax)
              q(j,imax) = aux
            enddo
            detQ = -detQ
          endif
c calculate R and Q if r(i,i) is non-zero
          if (r(i,i).eq.(0d0,0d0)) goto 200
          sig2 = 0d0
          do j=i,n
            sig2 = sig2 + abs(r(j,i))**2
          enddo
          sig = sqrt(sig2)
          kap   = -r(i,i)/abs(r(i,i))*sig
          wnorm = sqrt(2d0*sig*(sig+abs(r(i,i))))
          do j=i,n
            w(j) = r(j,i)/wnorm
          enddo
          w(i) = w(i) - kap/wnorm

          do j=i,n
            wr(j) = 0d0
            do k=i,n
              wr(j) = wr(j) + dconjg(w(k))*r(k,j)
            enddo
          enddo
          do j=i,n
            if (j.eq.i) then
              r(j,i) = r(j,i) - 2d0*w(j)*wr(i)
            else
              r(j,i) = 0d0
            endif
            do k=i+1,n
              r(j,k) = r(j,k) - 2d0*w(j)*wr(k)
            enddo
          enddo

c calculate matrix Q
          do j=1,n
            qw(j) = 0d0
            do k=i,n
              qw(j) = qw(j) + q(j,k)*w(k)
            enddo
          enddo
          do j=1,n
            do k=i,n
              q(j,k) = q(j,k) - 2d0*dconjg(w(k))*qw(j)
            enddo
          enddo
          detQ = -detQ

200     continue

        end

***********************************************************************
        function det_dd(A,n)
***********************************************************************
*       determinant of real nxn-matrix A
*----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        real*8 A(n,n),Q(n,n),R(n,n)
        integer i

        call QRdecomp_dd(A,Q,R,detQ,n)

        det_dd = detQ*R(n,n)
        do i=1,n-1
          det_dd = det_dd*R(i,i)
        enddo

        end

***********************************************************************
        subroutine inverse_dd(A,iA,detA,n)
***********************************************************************
*       inverse of real nxn-matrix A
*----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        real*8 A(n,n),Q(n,n),R(n,n),iA(n,n),iR(n,n)
        integer i,j,k

        call QRdecomp_dd(A,Q,R,detQ,n)

        detA = detQ*R(n,n)
        do i=1,n-1
          detA = detA*R(i,i)
        enddo

c invert R
        do i=n,1,-1
          do j=1,n
            if (j.lt.i) then
              iR(i,j) = 0d0
            else
              w = 0d0
              do k=i+1,n
                w = w + R(i,k)*iR(k,j)
              enddo
              if (i.eq.j) then
                iR(i,j) = (1d0-w)/R(i,i)
              else
                iR(i,j) = -w/R(i,i)
              endif
            endif
          enddo
        enddo

c inverse of A
        do i=1,n
          do j=1,n
            iA(i,j) = 0d0
            do k=1,n
              iA(i,j) = iA(i,j) + iR(i,k)*Q(j,k)
            enddo
          enddo
        enddo

        end

***********************************************************************
        subroutine QRdecomp_dd(A,Q,R,detQ,n)
***********************************************************************
*       QR-decomposition of real nxn-matrix A 
*       using Householder matrices
*       Q = orthogonal matrix, R = upper triangle matrix
*----------------------------------------------------------------------
*       8.1.04 Stefan Dittmaier
***********************************************************************
        implicit real*8 (a-z)
        integer n
        real*8 A(n,n),Q(n,n),R(n,n)
        real*8 w(n),wr(n),qw(n)
        integer i,j,k,imax

        detQ = 1d0

c*** start with R=A and Q=unit matrix 
        do i=1,n
          do j=1,n
            r(i,j) = a(i,j)
            if (i.eq.j) then
              q(i,j) = 1d0
            else
              q(i,j) = 0d0
            endif
          enddo
        enddo

c*** n-1 steps of decomposition
        do 200 i=1,n-1

c exchange rows such that |r(imax,i)| is maximal
          imax = i
          rmax = r(i,i)
          do j=i+1,n
            if (abs(r(j,i)).gt.abs(rmax)) then
              imax = j
              rmax = r(j,i)
            endif
          enddo
          if (imax.ne.i) then
            do j=1,n
              aux       = r(i,j)
              r(i,j)    = r(imax,j)
              r(imax,j) = aux
              aux       = q(j,i)
              q(j,i)    = q(j,imax)
              q(j,imax) = aux
            enddo
            detQ = -detQ
          endif
c calculate R and Q if r(i,i) is non-zero
          if (r(i,i).eq.0d0) goto 200
          sig2 = 0d0
          do j=i,n
            sig2 = sig2 + r(j,i)**2
          enddo
          sig = sqrt(sig2)
          kap   = -sign(1d0,r(i,i))*sig
          wnorm = sqrt(2d0*sig*(sig+abs(r(i,i))))
          do j=i,n
            w(j) = r(j,i)/wnorm
          enddo
          w(i) = w(i) - kap/wnorm

          do j=i,n
            wr(j) = 0d0
            do k=i,n
              wr(j) = wr(j) + w(k)*r(k,j)
            enddo
          enddo
          do j=i,n
            if (j.eq.i) then
              r(j,i) = r(j,i) - 2d0*w(j)*wr(i)
            else
              r(j,i) = 0d0
            endif
            do k=i+1,n
              r(j,k) = r(j,k) - 2d0*w(j)*wr(k)
            enddo
          enddo

c calculate matrix Q
          do j=1,n
            qw(j) = 0d0
            do k=i,n
              qw(j) = qw(j) + q(j,k)*w(k)
            enddo
          enddo
          do j=1,n
            do k=i,n
              q(j,k) = q(j,k) - 2d0*w(k)*qw(j)
            enddo
          enddo
          detQ = -detQ

200     continue

        end

***********************************************************************
        subroutine DD_debugoutput()
***********************************************************************
*       write out debug info
*----------------------------------------------------------------------
*       8.3.13 Stefan Dittmaier
***********************************************************************
        use DD_global
        implicit real*8 (a-z)
        integer i

        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 'DD debug info:'
          write(outchannel,*) 'Last call by ',s_DDin
          write(outchannel,*) 'Real input parameters:   ',nr_DDin
          do i=1,nr_DDin
            write(outchannel,'(g26.16)') r_DDin(i)  
          enddo
          write(outchannel,*) 'Complex input parameters:',nc_DDin
          do i=1,nc_DDin
            write(outchannel,'(g26.16,3x,g26.16)') c_DDin(i)  
          enddo
          write(outchannel,*) 'Integer input parameters:',ni_DDin
          do i=1,ni_DDin
            write(outchannel,*) i_DDin(i)  
          enddo
          if (cout.eq.coutmax) call DDlastmessage()
          cout = cout+1
        endif

        end

**********************************************************************
        subroutine TN_dd_dummy(N,r2)
**********************************************************************
*       N-point coefficients of rank r <= r2
*
*       DUMMY CALL for initialization
*---------------------------------------------------------------------
*       10.9.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer r,r2,i,k,N,m,id,nid(0:nmax-1)

          id = 0

          if (id.eq.0) then
            do i=0,2**N-1
              r2_aux(i)     = -1
              r2_new_aux(i) = -1
              do r=0,r2
                resaccrel(i,r)  = 0d0
                resaccabs(i,r)  = 0d0
                resaccrel2(i,r) = 0d0
                resaccabs2(i,r) = 0d0
              enddo
            enddo
            nmaster   = N
            r2master  = r2
            accflag   = 0
            errflag   = 0
            stopflag  = 0
          endif

        if (N.gt.nmax) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'TN_dd_dummy: N > nmax'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        else

c set identifiers for lower-point integrals
          m = 0
          do k=0,nmax-1
            if (mod(id,2**(k+1))/2**k.eq.0) then
              nid(m) = id + 2**k
              m=m+1
            endif
            if (m.eq.N) goto 205
          enddo
205       continue

        endif

        end

**********************************************************************
        subroutine DDsetcoutmax(ccoutmax)
**********************************************************************
*       set maximal number of DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

        integer ccoutmax

        coutmax = ccoutmax

        end

**********************************************************************
        subroutine DDresetcout()
**********************************************************************
*       reset counter for DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

        cout = 0

        end

**********************************************************************
        subroutine DDsetcout_on(ccout_on)
**********************************************************************
*       reset counter for DD messages
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

        logical ccout_on

        cout_on = ccout_on

        end

**********************************************************************
        subroutine DDlastmessage()
**********************************************************************
*       last DD message
*---------------------------------------------------------------------
*       23.10.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

        write(outchannel,*) 
        write(outchannel,*) 'cout = ',cout
        write(outchannel,*) '==================================' 
        write(outchannel,*) '== further DD output suppressed ==' 
        write(outchannel,*) '==================================' 

        end

