C     Version 3 CTEQ distribution function in a parametrized form.

C   By: H.L. Lai, J. Botts, J. Huston, J.G. Morfin, J.F. Owens, J. Qiu,
C       W.K. Tung & H. Weerts;  Preprint MSU-HEP/41024, CTEQ 404 

C   This file contains three versions of the same CTEQ3 parton distributions: 
C 
C Two "front-end" subprograms:    
C     FUNCTION Ctq3Pd (Iset, Iparton, X, Q, Irt) 
C         returns the PROBABILITY density for a GIVEN flavor;
C     SUBROUTINE Ctq3Pds(Iset, Pdf, XX, QQ, Irt)
C         returns an array of MOMENTUM densities for ALL flavors;
C One lower-level subprogram:
C     FUNCTION Ctq3df (Iset, Iprtn, XX, QQ, Irt)
C         returns the MOMENTUM density of a GIVEN valence or sea distribution.

C      One supplementary function to return the QCD lambda parameter 
C      concerning these distributions is also included (see below). 

C     Although DOUBLE PRECISION is used, conversion to SINGLE PRECISION
C     is straightforward by removing the 
C     Implicit Double Precision statements. 

C     Since this is an initial distribution of version 3, it is
C     useful for the authors to maintain a record of the distribution
C     list in case there are revisions or corrections.
C     In the interest of maintaining the integrity of this package,
C     please do not freely distribute this program package; instead, refer
C     any interested colleagues to direct their request for a copy to:
C     Lai@cteq11.pa.msu.edu or Tung@msupa.pa.msu.edu.

C   If you have detailed questions concerning these CTEQ3 distributions, 
C   or if you find problems/bugs using this initial distribution, direct 
C   inquires to Hung-Liang Lai or Wu-Ki Tung.

C     -------------------------------------------
C     Detailed instructions follow.

C     Name convention for CTEQ distributions:  CTEQnSx  where
C        n : version number                      (currently n = 3)
C        S : factorization scheme label: = [M L D] for [MS-bar LO DIS] 
c               resp.
C        x : special characteristics, if any
C        (e.g. S(F) for singular (flat) small-x, L for "LEP lambda value")
C        (not applicable to CTEQ3 since only three standard sets are given.)

C    Explanation of functional arguments:

C    Iset is the set label; in this version, Iset = 1, 2, 3 
C                           correspond to the following CTEQ global fits:

C          cteq3M  : best fit in the MS-bar scheme 
C          cteq3L  : best fit in Leading order QCD
C          cteq3D  : best fit in the DIS scheme

C   Iprtn  is the parton label (6, 5, 4, 3, 2, 1, 0, -1, ......, -6)
C                          for (t, b, c, s, d, u, g, u_bar, ..., t_bar)
C  *** WARNING: We use the parton label 2 as D-quark, and 1 as U-quark which 
C               might be different with your labels.

C   X, Q are the usual x, Q; 
C   Irt is a return error code (see individual modules for explanation).
C       
C     ---------------------------------------------

C  Since the QCD Lambda value for the various sets are needed more often than
C  the other parameters in most applications, a special function
C     Wlamd3 (Iset, Iorder, Neff)                    is provided
C  which returns the lambda value for Neff = 4,5,6 effective flavors as well as
C  the order these values pertain to.

C     ----------------------------------------------
C     The range of (x, Q) used in this round of global analysis is, approxi-
C     mately,  0.01 < x < 0.75 ; and 4 GeV^2 < Q^2 < 400 GeV^2 for fixed target
C     experiments and 0.0001 < x < 0.1 from HERA data.

C    The range of (x, Q) used in the reparametrization of the QCD evolved
C    parton distributions is 10E-6 < x < 1 ; 1.6 GeV < Q < 10 TeV.  The 
C    functional form of this parametrization is:

C      A0 * x^A1 * (1-x)^A2 * (1 + A3 * x^A4) * [log(1+1/x)]^A5

C   with the A'coefficients being smooth functions of Q.  For heavy quarks,
C   a threshold factor is applied to A0 which simulates the proper Q-dependence
C   of the QCD evolution in that region according to the renormalization
C   scheme defined in Collins-Tung, Nucl. Phys. B278, 934 (1986).

C   Since this function is positive definite and smooth, it provides sensible
C   extrapolations of the parton distributions if they are called beyond
C   the original range in an application. There is no artificial boundaries
C   or sharp cutoff's.
C    ------------------------------------------------

      FUNCTION Ctq3Pd (Iset, Iparton, X, Q, Irt)

C   This function returns the CTEQ parton distributions f^Iset_Iprtn/proton
C   --- the PROBABILITY density

C   (Iset, Iparton, X, Q): explained above;

C    Irt : return error code: see module Ctq3df for explanation.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      Ifl = Iparton
      JFL = ABS(Ifl)
C                                                             Valence
      IF (Ifl.Eq.1 .or. Ifl.Eq.2) THEN
        VL = Ctq3df(Iset, Ifl, X, Q, Irt)
      ELSE
        VL = 0.
      ENDIF
C                                                             Sea
      SEA = Ctq3df (Iset, -JFL, X, Q, Irt)
C                                              Full (probability) Distribution
      Ctq3pd = (VL + SEA) / X
      
      Return
C                         *************************
      END
C
C
      SUBROUTINE Ctq3Pds(Iset, Pdf, X, Q, Irt)

C   This function returns the CTEQ parton distributions xf^Iset_Iprtn/proton
C   --- the Momentum density in array form
c
C    (Iset, X, Q): explained in header comment lines;

C     Irt : return error code -- cumulated over flavors: 
C           see module Ctq3df for explanation on individual flavors.
C     Pdf (Iparton);  
C         Iparton = -6, -5, ...0, 1, 2 ... 6
C               has the same meaning as explained in the header comment lines.
    
      Implicit Double Precision (A-H, O-Z)
      Dimension Pdf (-6:6)

      Irt=0
      do 10 I=-6,2
         if(I.le.0) then
            Pdf(I) = Ctq3df(Iset,I,X,Q,Irt1)
            Pdf(-I)= Pdf(I)
         else
            Pdf(I) = Ctq3df(Iset,I,X,Q,Irt1) + Pdf(-I)
         endif
         Irt=Irt+Irt1
  10  Continue

      Return
C                         *************************
      End

      FUNCTION Ctq3df (Iset, Iprtn, XX, QQ, Irt)

C            Returns xf(x,Q) -- the momentum fraction distribution !!
C            Returns valence and sea rather than combined flavor distr.

C            Iset : PDF set label

C            Iprtn  : Parton label:   2, 1 = d_ and u_ valence
C                                     0 = gluon
C                            -1, ... -6 = u, d, s, c, b, t sea quarks

C            XX  : Bjorken-x
C            QQ  : scale parameter "Q"
C      Irt : Return code
C      0 : no error
C      1 : parametrization is slightly negative; reset to 0.0.
C          (This condition happens rarely -- only for large x where the 
C          absolute value of the parton distribution is extremely small.) 

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      PARAMETER (D0=0D0, D1=1D0, D2=2D0, D3=3D0, D4=4D0, D10=1D1)
      Parameter (Nst = 3)

      DIMENSION
     >   Iord(Nst), Isch(Nst), Nqrk(Nst),Alm(Nst)
     > , Vlm(4:6,Nst), Qms(4:6, Nst)
     > , Xmn(Nst), Qmn(Nst), Qmx(Nst)

c                                          --------- CTEQ3M
c
      DATA 
     >  Isch(1), Iord(1), Nqrk(1), Alm(1) /  1,  2,  6, .239  / 
     >  (Vlm(I,1), I=4,6) / .239,    .158,     .063   /
     >  (Qms(I,1), I=4,6) / 1.60,   5.00,  180.0 /
     >  Xmn(1), Qmn(1), Qmx(1) /  1.E-6,  1.60,  1.E4  /

c                                          --------- CTEQ3L
c
      DATA 
     >  Isch(2), Iord(2), Nqrk(2), Alm(2) /  1,  1,  6, .177  / 
     >  (Vlm(I,2), I=4,6) / .177,    .132,     .066   /
     >  (Qms(I,2), I=4,6) / 1.60,   5.00,  180.0 /
     >  Xmn(2), Qmn(2), Qmx(2) /  1.E-6,  1.60,  1.E4  /

c                                          --------- CTEQ3D
c
      DATA 
     >  Isch(3), Iord(3), Nqrk(3), Alm(3) /  1,  2,  6, .247  / 
     >  (Vlm(I,3), I=4,6) / .247,    .164,     .066   /
     >  (Qms(I,3), I=4,6) / 1.60,   5.00,  180.0 /
     >  Xmn(3), Qmn(3), Qmx(3) /  1.E-6,  1.60,  1.E4  /


      Data Ist, Lp, Qsto / 0, -10, 1.2345 /

      save Ist, Lp, Qsto
      save SB, SB2, SB3

      X  = XX
      Irt = 0
      if(Iset.eq.Ist .and. Qsto.eq.QQ) then
C                                             if only change is in x:
        if (Iprtn.eq.Lp) goto 100
C                         if change in flv is within "light" partons:
        if (Iprtn.ge.-3 .and. Lp.ge.-3) goto 501
      endif

      Ip = abs(Iprtn)
C                                                  Set up Qi for SB
      If (Ip .GE. 4) then
         If (QQ .LE. Qms(Ip, Iset)) Then
           Ctq3df = 0.0
           Return
         Endif
         Qi = Qms(ip, Iset)
      Else
         Qi = Qmn(Iset)
      Endif
C                   Use "standard lambda" of parametrization program
      Alam = Alm (Iset)

      SBL = LOG(QQ/Alam) / LOG(Qi/Alam)
      SB = LOG (SBL)
      SB2 = SB*SB
      SB3 = SB2*SB

 501  Iflv = 3 - Iprtn

      Goto (1,2,3, 311) Iset

 1    Goto(11,12,13,14,15,16,17,18,19)Iflv    
c   Ifl =   2
  11  A0=Exp(-0.7266E+00-0.1584E+01*SB +0.1259E+01*SB2-0.4305E-01*SB3)
      A1= 0.5285E+00-0.3721E+00*SB +0.5150E+00*SB2-0.1697E+00*SB3 
      A2= 0.4075E+01+0.8282E+00*SB -0.4496E+00*SB2+0.2107E+00*SB3 
      A3= 0.3279E+01+0.5066E+01*SB -0.9134E+01*SB2+0.2897E+01*SB3 
      A4= 0.4399E+00-0.5888E+00*SB +0.4802E+00*SB2-0.1664E+00*SB3 
      A5= 0.3678E+00-0.8929E+00*SB +0.1592E+01*SB2-0.5713E+00*SB3 
      goto 100
c   Ifl =   1
  12  A0=Exp( 0.2259E+00+0.1237E+00*SB +0.3035E+00*SB2-0.2935E+00*SB3)
      A1= 0.5085E+00+0.1651E-01*SB -0.3592E-01*SB2+0.2782E-01*SB3 
      A2= 0.3732E+01+0.4901E+00*SB +0.2218E+00*SB2-0.1116E+00*SB3 
      A3= 0.7011E+01-0.6620E+01*SB +0.2557E+01*SB2-0.1360E+00*SB3 
      A4= 0.8969E+00-0.2429E+00*SB +0.1811E+00*SB2-0.6888E-01*SB3 
      A5= 0.8636E-01+0.2558E+00*SB -0.3082E+00*SB2+0.2535E+00*SB3 
      goto 100
c   Ifl =   0
  13  A0=Exp(-0.2318E+00-0.9779E+00*SB -0.3783E+00*SB2+0.1037E-01*SB3)
      A1=-0.2916E+00+0.1754E+00*SB -0.1884E+00*SB2+0.6116E-01*SB3 
      A2= 0.5349E+01+0.7460E+00*SB +0.2319E+00*SB2-0.2622E+00*SB3 
      A3= 0.6920E+01-0.3454E+01*SB +0.2027E+01*SB2-0.7626E+00*SB3 
      A4= 0.1013E+01+0.1423E+00*SB -0.1798E+00*SB2+0.1872E-01*SB3 
      A5=-0.5465E-01+0.2303E+01*SB -0.9584E+00*SB2+0.3098E+00*SB3 
      goto 100
c   Ifl =  -1
  14  A0=Exp(-0.2906E+01-0.1069E+00*SB -0.1055E+01*SB2+0.2496E+00*SB3)
      A1=-0.2875E+00+0.6571E-01*SB -0.1987E-01*SB2-0.1800E-02*SB3 
      A2= 0.9854E+01-0.2715E+00*SB -0.7407E+00*SB2+0.2888E+00*SB3 
      A3= 0.1583E+02-0.7687E+01*SB +0.3428E+01*SB2-0.3327E+00*SB3 
      A4= 0.9763E+00+0.7599E-01*SB -0.2128E+00*SB2+0.6852E-01*SB3 
      A5=-0.8444E-02+0.9434E+00*SB +0.4152E+00*SB2-0.1481E+00*SB3 
      goto 100
c   Ifl =  -2
  15  A0=Exp(-0.2328E+01-0.3061E+01*SB +0.3620E+01*SB2-0.1602E+01*SB3)
      A1=-0.3358E+00+0.3198E+00*SB -0.4210E+00*SB2+0.1571E+00*SB3 
      A2= 0.8478E+01-0.3112E+01*SB +0.5243E+01*SB2-0.2255E+01*SB3 
      A3= 0.1971E+02+0.3389E+00*SB -0.5268E+01*SB2+0.2099E+01*SB3 
      A4= 0.1128E+01-0.4701E+00*SB +0.7779E+00*SB2-0.3506E+00*SB3 
      A5=-0.4708E+00+0.3341E+01*SB -0.3375E+01*SB2+0.1353E+01*SB3 
      goto 100
c   Ifl =  -3
  16  A0=Exp(-0.3780E+01+0.2499E+01*SB -0.4962E+01*SB2+0.1936E+01*SB3)
      A1=-0.2639E+00-0.1575E+00*SB +0.3584E+00*SB2-0.1646E+00*SB3 
      A2= 0.8082E+01+0.2794E+01*SB -0.5438E+01*SB2+0.2321E+01*SB3 
      A3= 0.1811E+02-0.2000E+02*SB +0.1951E+02*SB2-0.6904E+01*SB3 
      A4= 0.9822E+00+0.4972E+00*SB -0.8690E+00*SB2+0.3415E+00*SB3 
      A5= 0.1772E+00-0.6078E+00*SB +0.3341E+01*SB2-0.1473E+01*SB3 
      goto 100
c   Ifl =  -4
  17  A0=SB** 0.1122E+01*Exp(-0.4232E+01-0.1808E+01*SB +0.5348E+00*SB2)
      A1=-0.2824E+00+0.5846E+00*SB -0.7230E+00*SB2+0.2419E+00*SB3 
      A2= 0.5683E+01-0.2948E+01*SB +0.5916E+01*SB2-0.2560E+01*SB3 
      A3= 0.2051E+01+0.4795E+01*SB -0.4271E+01*SB2+0.4174E+00*SB3 
      A4= 0.1737E+00+0.1717E+01*SB -0.1978E+01*SB2+0.6643E+00*SB3 
      A5= 0.8689E+00+0.3500E+01*SB -0.3283E+01*SB2+0.1026E+01*SB3 
      goto 100
c   Ifl =  -5
  18  A0=SB** 0.9906E+00*Exp(-0.1496E+01-0.6576E+01*SB +0.1569E+01*SB2)
      A1=-0.2140E+00-0.6419E-01*SB -0.2741E-02*SB2+0.3185E-02*SB3 
      A2= 0.5781E+01+0.1049E+00*SB -0.3930E+00*SB2+0.5174E+00*SB3 
      A3=-0.9420E+00+0.5511E+00*SB +0.8817E+00*SB2+0.1903E+01*SB3 
      A4= 0.2418E-01+0.4232E-01*SB -0.1244E-01*SB2-0.2365E-01*SB3 
      A5= 0.7664E+00+0.1794E+01*SB -0.4917E+00*SB2-0.1284E+00*SB3 
      goto 100
c   Ifl =  -6
  19  A0=SB** 0.1000E+01*Exp(-0.8460E+01+0.1154E+01*SB +0.8838E+01*SB2)
      A1=-0.4316E-01-0.2976E+00*SB +0.3174E+00*SB2-0.1429E+01*SB3 
      A2= 0.4910E+01+0.2273E+01*SB +0.5631E+01*SB2-0.1994E+02*SB3 
      A3= 0.1190E+02-0.2000E+02*SB -0.2000E+02*SB2+0.1292E+02*SB3 
      A4= 0.5771E+00-0.2552E+00*SB +0.7510E+00*SB2+0.6923E+00*SB3 
      A5= 0.4402E+01-0.1627E+01*SB -0.2085E+01*SB2-0.6737E+01*SB3 
      goto 100

 2    Goto(21,22,23,24,25,26,27,28,29)Iflv    
c   Ifl =   2
  21  A0=Exp( 0.1141E+00+0.4764E+00*SB -0.1745E+01*SB2+0.7728E+00*SB3)
      A1= 0.4275E+00-0.1290E+00*SB +0.3609E+00*SB2-0.1689E+00*SB3 
      A2= 0.3000E+01+0.2946E+01*SB -0.4117E+01*SB2+0.1989E+01*SB3 
      A3=-0.1302E+01+0.2322E+01*SB -0.4258E+01*SB2+0.2109E+01*SB3 
      A4= 0.2586E+01-0.1920E+00*SB -0.3754E+00*SB2+0.2731E+00*SB3 
      A5=-0.2251E+00-0.5374E+00*SB +0.2245E+01*SB2-0.1034E+01*SB3 
      goto 100
c   Ifl =   1
  22  A0=Exp( 0.1907E+00+0.4205E-01*SB +0.2752E+00*SB2-0.3171E+00*SB3)
      A1= 0.4611E+00+0.2331E-01*SB -0.3403E-01*SB2+0.3174E-01*SB3 
      A2= 0.3504E+01+0.5739E+00*SB +0.2676E+00*SB2-0.1553E+00*SB3 
      A3= 0.7452E+01-0.6742E+01*SB +0.2849E+01*SB2-0.1964E+00*SB3 
      A4= 0.1116E+01-0.3435E+00*SB +0.2865E+00*SB2-0.1288E+00*SB3 
      A5= 0.6659E-01+0.2714E+00*SB -0.2688E+00*SB2+0.2763E+00*SB3 
      goto 100
c   Ifl =   0
  23  A0=Exp(-0.7631E+00-0.7241E+00*SB -0.1170E+01*SB2+0.5343E+00*SB3)
      A1=-0.3573E+00+0.3469E+00*SB -0.3396E+00*SB2+0.9188E-01*SB3 
      A2= 0.5604E+01+0.7458E+00*SB -0.5082E+00*SB2+0.1844E+00*SB3 
      A3= 0.1549E+02-0.1809E+02*SB +0.1162E+02*SB2-0.3483E+01*SB3 
      A4= 0.9881E+00+0.1364E+00*SB -0.4421E+00*SB2+0.2051E+00*SB3 
      A5=-0.9505E-01+0.3259E+01*SB -0.1547E+01*SB2+0.2918E+00*SB3 
      goto 100
c   Ifl =  -1
  24  A0=Exp(-0.2740E+01-0.7987E-01*SB -0.9015E+00*SB2-0.9872E-01*SB3)
      A1=-0.3909E+00+0.1244E+00*SB -0.4487E-01*SB2+0.1277E-01*SB3 
      A2= 0.9163E+01+0.2823E+00*SB -0.7720E+00*SB2-0.9360E-02*SB3 
      A3= 0.1080E+02-0.3915E+01*SB -0.1153E+01*SB2+0.2649E+01*SB3 
      A4= 0.9894E+00-0.1647E+00*SB -0.9426E-02*SB2+0.2945E-02*SB3 
      A5=-0.3395E+00+0.6998E+00*SB +0.7000E+00*SB2-0.6730E-01*SB3 
      goto 100
c   Ifl =  -2
  25  A0=Exp(-0.2449E+01-0.3513E+01*SB +0.4529E+01*SB2-0.2031E+01*SB3)
      A1=-0.4050E+00+0.3411E+00*SB -0.3669E+00*SB2+0.1109E+00*SB3 
      A2= 0.7470E+01-0.2982E+01*SB +0.5503E+01*SB2-0.2419E+01*SB3 
      A3= 0.1503E+02+0.1638E+01*SB -0.8772E+01*SB2+0.3852E+01*SB3 
      A4= 0.1137E+01-0.1006E+01*SB +0.1485E+01*SB2-0.6389E+00*SB3 
      A5=-0.5299E+00+0.3160E+01*SB -0.3104E+01*SB2+0.1219E+01*SB3 
      goto 100
c   Ifl =  -3
  26  A0=Exp(-0.3640E+01+0.1250E+01*SB -0.2914E+01*SB2+0.8390E+00*SB3)
      A1=-0.3595E+00-0.5259E-01*SB +0.3122E+00*SB2-0.1642E+00*SB3 
      A2= 0.7305E+01+0.9727E+00*SB -0.9788E+00*SB2-0.5193E-01*SB3 
      A3= 0.1198E+02-0.1799E+02*SB +0.2614E+02*SB2-0.1091E+02*SB3 
      A4= 0.9882E+00-0.6101E+00*SB +0.9737E+00*SB2-0.4935E+00*SB3 
      A5=-0.1186E+00-0.3231E+00*SB +0.3074E+01*SB2-0.1274E+01*SB3 
      goto 100
c   Ifl =  -4
  27  A0=SB** 0.1122E+01*Exp(-0.3718E+01-0.1335E+01*SB +0.1651E-01*SB2)
      A1=-0.4719E+00+0.7509E+00*SB -0.8420E+00*SB2+0.2901E+00*SB3 
      A2= 0.6194E+01-0.1641E+01*SB +0.4907E+01*SB2-0.2523E+01*SB3 
      A3= 0.4426E+01-0.4270E+01*SB +0.6581E+01*SB2-0.3474E+01*SB3 
      A4= 0.2683E+00+0.9876E+00*SB -0.7612E+00*SB2+0.1780E+00*SB3 
      A5=-0.4547E+00+0.4410E+01*SB -0.3712E+01*SB2+0.1245E+01*SB3 
      goto 100
c   Ifl =  -5
  28  A0=SB** 0.9838E+00*Exp(-0.2548E+01-0.7660E+01*SB +0.3702E+01*SB2)
      A1=-0.3122E+00-0.2120E+00*SB +0.5716E+00*SB2-0.3773E+00*SB3 
      A2= 0.6257E+01-0.8214E-01*SB -0.2537E+01*SB2+0.2981E+01*SB3 
      A3=-0.6723E+00+0.2131E+01*SB +0.9599E+01*SB2-0.7910E+01*SB3 
      A4= 0.9169E-01+0.4295E-01*SB -0.5017E+00*SB2+0.3811E+00*SB3 
      A5= 0.2402E+00+0.2656E+01*SB -0.1586E+01*SB2+0.2880E+00*SB3 
      goto 100
c   Ifl =  -6
  29  A0=SB** 0.1001E+01*Exp(-0.6934E+01+0.3050E+01*SB -0.6943E+00*SB2)
      A1=-0.1713E+00-0.5167E+00*SB +0.1241E+01*SB2-0.1703E+01*SB3 
      A2= 0.6169E+01+0.3023E+01*SB -0.1972E+02*SB2+0.1069E+02*SB3 
      A3= 0.4439E+01-0.1746E+02*SB +0.1225E+02*SB2+0.8350E+00*SB3 
      A4= 0.5458E+00-0.4586E+00*SB +0.9089E+00*SB2-0.4049E+00*SB3 
      A5= 0.3207E+01-0.3362E+01*SB +0.5877E+01*SB2-0.7659E+01*SB3 
      goto 100

 3    Goto(31,32,33,34,35,36,37,38,39)Iflv    
c   Ifl =   2
  31  A0=Exp( 0.3961E+00+0.4914E+00*SB -0.1728E+01*SB2+0.7257E+00*SB3)
      A1= 0.4162E+00-0.1419E+00*SB +0.3680E+00*SB2-0.1618E+00*SB3 
      A2= 0.3248E+01+0.3028E+01*SB -0.4307E+01*SB2+0.1920E+01*SB3 
      A3=-0.1100E+01+0.2184E+01*SB -0.3820E+01*SB2+0.1717E+01*SB3 
      A4= 0.2082E+01-0.2756E+00*SB +0.3043E+00*SB2-0.1260E+00*SB3 
      A5=-0.4822E+00-0.5706E+00*SB +0.2243E+01*SB2-0.9760E+00*SB3 
      goto 100
c   Ifl =   1
  32  A0=Exp( 0.2148E+00+0.5814E-01*SB +0.2734E+00*SB2-0.2902E+00*SB3)
      A1= 0.4810E+00+0.1657E-01*SB -0.3800E-01*SB2+0.3125E-01*SB3 
      A2= 0.3509E+01+0.3923E+00*SB +0.4010E+00*SB2-0.1932E+00*SB3 
      A3= 0.7055E+01-0.6552E+01*SB +0.3466E+01*SB2-0.5657E+00*SB3 
      A4= 0.1061E+01-0.3453E+00*SB +0.4089E+00*SB2-0.1817E+00*SB3 
      A5= 0.8687E-01+0.2548E+00*SB -0.2967E+00*SB2+0.2647E+00*SB3 
      goto 100
c   Ifl =   0
  33  A0=Exp(-0.4665E+00-0.7554E+00*SB -0.3323E+00*SB2-0.2734E-04*SB3)
      A1=-0.3359E+00+0.2395E+00*SB -0.2377E+00*SB2+0.7059E-01*SB3 
      A2= 0.5451E+01+0.6086E+00*SB +0.8606E-01*SB2-0.1425E+00*SB3 
      A3= 0.1026E+02-0.9352E+01*SB +0.4879E+01*SB2-0.1150E+01*SB3 
      A4= 0.9935E+00-0.5017E-01*SB -0.1707E-01*SB2-0.1464E-02*SB3 
      A5=-0.4160E-01+0.2305E+01*SB -0.1063E+01*SB2+0.3211E+00*SB3 
      goto 100
c   Ifl =  -1
  34  A0=Exp(-0.3323E+01+0.2296E+00*SB -0.1109E+01*SB2+0.2223E+00*SB3)
      A1=-0.3410E+00+0.8847E-01*SB -0.1111E-01*SB2-0.5927E-02*SB3 
      A2= 0.9753E+01-0.5182E+00*SB -0.4670E+00*SB2+0.1921E+00*SB3 
      A3= 0.1977E+02-0.1600E+02*SB +0.9481E+01*SB2-0.1864E+01*SB3 
      A4= 0.9818E+00+0.2839E-02*SB -0.1188E+00*SB2+0.3584E-01*SB3 
      A5=-0.7934E-01+0.1004E+01*SB +0.3704E+00*SB2-0.1220E+00*SB3 
      goto 100
c   Ifl =  -2
  35  A0=Exp(-0.2714E+01-0.2868E+01*SB +0.3700E+01*SB2-0.1671E+01*SB3)
      A1=-0.3893E+00+0.3341E+00*SB -0.3897E+00*SB2+0.1420E+00*SB3 
      A2= 0.8359E+01-0.3267E+01*SB +0.5327E+01*SB2-0.2245E+01*SB3 
      A3= 0.2359E+02-0.5669E+01*SB -0.4602E+01*SB2+0.3153E+01*SB3 
      A4= 0.1106E+01-0.4745E+00*SB +0.7739E+00*SB2-0.3417E+00*SB3 
      A5=-0.5557E+00+0.3433E+01*SB -0.3390E+01*SB2+0.1354E+01*SB3 
      goto 100
c   Ifl =  -3
  36  A0=Exp(-0.3985E+01+0.2855E+01*SB -0.5208E+01*SB2+0.1937E+01*SB3)
      A1=-0.3337E+00-0.1150E+00*SB +0.3691E+00*SB2-0.1709E+00*SB3 
      A2= 0.7968E+01+0.3641E+01*SB -0.6599E+01*SB2+0.2642E+01*SB3 
      A3= 0.1873E+02-0.1999E+02*SB +0.1734E+02*SB2-0.5813E+01*SB3 
      A4= 0.9731E+00+0.5082E+00*SB -0.8780E+00*SB2+0.3231E+00*SB3 
      A5=-0.5542E-01-0.4189E+00*SB +0.3309E+01*SB2-0.1439E+01*SB3 
      goto 100
c   Ifl =  -4
  37  A0=SB** 0.1105E+01*Exp(-0.3952E+01-0.1901E+01*SB +0.5137E+00*SB2)
      A1=-0.3543E+00+0.6055E+00*SB -0.6941E+00*SB2+0.2278E+00*SB3 
      A2= 0.5955E+01-0.2629E+01*SB +0.5337E+01*SB2-0.2300E+01*SB3 
      A3= 0.1933E+01+0.4882E+01*SB -0.3810E+01*SB2+0.2290E+00*SB3 
      A4= 0.1806E+00+0.1655E+01*SB -0.1893E+01*SB2+0.6395E+00*SB3 
      A5= 0.4790E+00+0.3612E+01*SB -0.3152E+01*SB2+0.9684E+00*SB3 
      goto 100
c   Ifl =  -5
  38  A0=SB** 0.9818E+00*Exp(-0.1825E+01-0.7464E+01*SB +0.2143E+01*SB2)
      A1=-0.2604E+00-0.1400E+00*SB +0.1702E+00*SB2-0.8476E-01*SB3 
      A2= 0.6005E+01+0.6275E+00*SB -0.2535E+01*SB2+0.2219E+01*SB3 
      A3=-0.9067E+00+0.1149E+01*SB +0.1974E+01*SB2+0.4716E+01*SB3 
      A4= 0.3915E-01+0.5945E-01*SB -0.9844E-01*SB2+0.2783E-01*SB3 
      A5= 0.5500E+00+0.1994E+01*SB -0.6727E+00*SB2-0.1510E+00*SB3 
      goto 100
c   Ifl =  -6
  39  A0=SB** 0.1002E+01*Exp(-0.8553E+01+0.3793E+00*SB +0.9998E+01*SB2)
      A1=-0.5870E-01-0.2792E+00*SB +0.6526E+00*SB2-0.1984E+01*SB3 
      A2= 0.4716E+01+0.4473E+00*SB +0.1128E+02*SB2-0.1937E+02*SB3 
      A3= 0.1289E+02-0.1742E+02*SB -0.1983E+02*SB2-0.9274E+00*SB3 
      A4= 0.5647E+00-0.2732E+00*SB +0.1074E+01*SB2+0.5981E+00*SB3 
      A5= 0.4390E+01-0.1262E+01*SB -0.9026E+00*SB2-0.9394E+01*SB3 
      goto 100

 311  stop 'This option is not currently supported.'

 100  Ctq3df = A0 *(x**A1) *((D1-x)**A2) *(D1+A3*(x**A4))
     $            *(log(D1+D1/x))**A5

      if(Ctq3df.lt.D0) then
        Ctq3df = D0
        Irt=1
      endif

      Ist = Iset

      Lp  = Iprtn
      Qsto = QQ

      Return
C                                  -----------------------
      ENTRY Wlamd3 (Iset, Iorder, Neff)

C     Returns the EFFECTIVE QCD lambda values for order=Iorder and
C     effective # of flavors = Neff for each of the PDF sets.

      Iorder = Iord (Iset)
      Wlamd3 = VLM  (Neff, Iset)

      RETURN

C                         *************************
      END



