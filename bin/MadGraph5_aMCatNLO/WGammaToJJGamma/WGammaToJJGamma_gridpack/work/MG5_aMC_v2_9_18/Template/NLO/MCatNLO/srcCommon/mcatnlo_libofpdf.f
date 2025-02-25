C From hvq package (FMNR). Proton, photon and electron PDFs are kept
C PDFs from 1999 onwards taken from P. Nason code
C-----------------------------------------------------------------------
C------- START STRUCTURE FUNCTION SECTION -------------------------------
C--------------------------------------------------------------------------

C-------------------------------------------------------------------------
      SUBROUTINE PRNTSF
C     prints details of the structure function sets
C-------------------------------------------------------------------------
      WRITE(*,100)
     #  ' Set     Authors     Lambda_4    Lambda_5_2loop   Scheme'
     # ,'   1     DO I    *    .200 *      .340              MS  '
     # ,'   2     DO II   *    .400 *      .680              MS  '
     # ,'   3     EHLQ  I *    .200 *      .340              MS  '
     # ,'   4     EHLQ II *    .290 *      .490              MS  '
     # ,'   5     DFLM         .160        .101              DI  '
     # ,'   6     DFLM         .260        .173              DI  '
     # ,'   7     DFLM         .360        .250              DI  '
      WRITE(*,100)
     #  '  10     MRSA mod.    .230        .151              MS  '
     # ,'  11     HMRS B       .190        .122              MS  '
     # ,'  12     KMRS B       .190        .122              MS  '
     # ,'  13      MRS B       .135        .083              MS  '
     # ,'  14      MRS B       .160        .101              MS  '
     # ,'  15      MRS B       .200        .130              MS  '
     # ,'  16      MRS B       .235        .155              MS  '
     # ,'  17     MRSS0        .215        .140              MS  '
     # ,'  18     MRSD0        .215        .140              MS  '
     # ,'  19     MRSD-        .215        .140              MS  '
     # ,'  20     MRSA         .230        .151              MS  '
      WRITE(*,100)
     #  '  21     MT S1        .212        .138              DI  '
     # ,'  22     MT B1        .194        .125              DI  '
     # ,'  23     MT B2        .191        .123              DI  '
     # ,'  24     MT E1        .155        .097              DI  '
     # ,'  25     MT S1M       .212        .138              MS  '
     # ,'  26     MT 6 (1/2s)  .237        .156              DI  '
     # ,'  27     MT 6 (1/2s)  .237        .156              MS  '
     # ,'  28     MT LO   *    .144 *      .245              MS  '
      WRITE(*,100)
     #  '  40     DGK PHOTON*  .400*       .680              MS  '
     # ,'  41     ACFGP-MC PH  .200        .130              MS  '
     # ,'  42     AFG-MC PH    .200        .130              MS  '
     # ,'  43     GRV-HO PH    .200        .130              DI_G'
     # ,'  44     LAC1 PH*     .200        .130              MS  '
     # ,'  45     GRS-HO PH    .268        .179              DI_G'
      WRITE(*,100)
     #  '  51     LAC1 EL*     .200        .130              MS  '
     # ,'  52     GRV-G HO EL  .200        .130              DI_G'
     # ,'  53     USER DEF EL                                    '
      WRITE(*,100)
     #  '  61     CTEQ1M       .231        .152              MS  '
     # ,'  62     CTEQ1MS      .231        .152              MS  '
     # ,'  63     CTEQ1ML      .322        .220              MS  '
     # ,'  64     CTEQ1D       .247        .164              DI  '
     # ,'  65     CTEQ1L  *    .168 *      .249              MS  '
     # ,'  66     CTEQ3M       .239        .158              MS  '
     # ,'  67     CTEQ3L  *    .177 *      .263              MS  '
     # ,'  68     CTEQ3D       .247        .164              DI  '
      WRITE(*,100)
     #  '  71     MRSA prime   .231        .152              MS  '
     # ,'  72     MRSG         .255        .170              MS  '
     # ,'  73     MRS105       .158        .0994             MS  '
     # ,'  74     MRS110       .214        .140              MS  '
     # ,'  75     MRS115       .282        .190              MS  '
     # ,'  76     MRS120       .364        .253              MS  '
     # ,'  77     MRS125       .458        .328              MS  '
     # ,'  78     MRS130       .566        .416              MS  '
      WRITE(*,100)
     #  '  81     CTEQ4M       .298        .202              MS  '
     # ,'  82     CTEQ4D       .298        .202              DI  '
     # ,'  83     CTEQ4L *     .298        .202              MS  '
     # ,'  84     CTEQ4A1      .214        .140              MS  '
     # ,'  85     CTEQ4A2      .254        .169              MS  '
     # ,'  86     CTEQ4A4      .346        .239              MS  '
     # ,'  87     CTEQ4A5      .400        .281              MS  '
     # ,'  88     CTEQ4HJ      .298        .202              MS  '
     # ,'  89     CTEQ4LQ      .268        .179              MS  '
      WRITE(*,100)
     #  '  91     MRSR1(1996)  .241        .159              MS  '
     # ,'  92     MRSR2  ..    .344        .237              MS  '
     # ,'  93     MRSR3  ..    .241        .159              MS  '
     # ,'  94     MRSR4  ..    .344        .237              MS  '
     # ,'  95     MRST1(1998)  .321        .220              MS  '
     # ,'  96     MRSTH  ..    .321        .220              MS  '
     # ,'  97     MRSTL  ..    .321        .220              MS  '
     # ,'  98     MRSTM  ..    .247        .164              MS  '
     # ,'  99     MRSTP  ..    .409        .288              MS  '
      WRITE(*,100)
     #  ' 101     CTEQ5M       .329        .226  (as=0.118)  MS  ' 
     # ,' 102     CTEQ5D       .329        .226  (as=0.118)  DI  ' 
     # ,' 103     CTEQ5L       .497        .359  (as=0.127)  MS  ' 
     # ,' 104     CTEQ5HJ      .329        .226  (as=0.118)  MS  ' 
     # ,' 105     CTEQ5HQ      .329        .226  (as=0.118)  MS  ' 
     # ,' 106     CTEQ5F3      Nf=3, L_3=.395    (as=0.106)  MS  ' 
     # ,' 107     CTEQ5F4      Nf=4, L_4=.309    (as=0.112)  MS  ' 
     # ,' 108     CTEQ5M1      .329        .226  (as=0.118)  MS  ' 
     # ,' 109     CTEQ5HQ1     .329        .226  (as=0.118)  MS  ' 
     # ,' 110     CTEQ5M1 (parametrized version)                  '
      WRITE(*,100)
     #  ' 111     MRST99 COR01 .321        .220              MS  '
     # ,' 112     MRSTH  COR02 .321        .220              MS  '
     # ,' 113     MRSTL  COR03 .321        .220              MS  '
     # ,' 114     MRSTM  COR04 .247        .164              MS  '
     # ,' 115     MRSTP  COR05 .409        .288              MS  '
     # ,' 116     MRST99 COR06 .327        .224              MS  '
     # ,' 117     MRST99 COR07 .315        .215              MS  '
     # ,' 118     MRST99 COR08 .321        .220              MS  '
     # ,' 119     MRST99 COR09 .321        .220              MS  '
     # ,' 120     MRST99 COR10 .321        .220              MS  '
     # ,' 121     MRST99 COR11 .321        .220              MS  '
     # ,' 122     MRST99 COR12 .321        .220              MS  '
      WRITE(*,100)
     #  ' 131     CTEQ6M       .326        .226  (as=0.118)  MS  ' 
     # ,' 132     CTEQ6D       .326        .226  (as=0.118)  DI  ' 
     # ,' 133     CTEQ6L       .326        .226  (as=0.118)  MS  ' 
     # ,' 134-173 CTEQ6M1xx    .326        .226  (as=0.118)  MS  ' 
      write(*,100)
     #  ' 181     MRST2001NNLO av.   .290  .196  (as=0.1155) MS  '
     # ,' 182     MRST2001NNLO fast  .290  .196  (as=0.1155) MS  '
     # ,' 183     MRST2001NNLO slow  .290  .196  (as=0.1155) MS  '
     # ,' 184     MRST2001NNLO jet   .326  .226  (as=0.118)  MS  '
     # ,' 185     MRST2001  best fit .347  .239  (as=0.119)  MS  '
     # ,' 186     MRST2001  low as   .313  .214  (as=0.117)  MS  '
     # ,' 187     MRST2001  high as  .382  .267  (as=0.121)  MS  '
     # ,' 188     MRST2001  jet fit  .382  .267  (as=0.121)  MS  '
     # ,' 189     MRST2001lo         .566  .416  (as=0.130)  LO  '
      write(*,100)
     #  ' 191     MRST2002     .359        .249  (as=0.1197) MS  '
     # ,' 192     MRST2002NNLO .289        .195  (as=0.1154) MS  '
     # ,' 200-230 MRS2001E     .347        .239  (as=0.119)  MS  '
      write(*,100) 
     #  '         Alekhin pdf sets'
     #, ' 231     LO  nominal ffn          .418  (as=0.1301) MS  '
     #, ' 232     LO  nominal vfn'
     #, ' 233     LO  mc=1.75 ffn'
     #, ' 234     LO  mc=1.75 vfn'   
     #, ' 235     LO  ss      ffn'   
     #, ' 236     LO  ss      vfn'   
     #, ' 237     NLO  nominal ffn         .215  (as=0.1171) MS  '
     #, ' 238     NLO  nominal vfn'
     #, ' 239     NLO  mc=1.75 ffn'
     #, ' 240     NLO  mc=1.75 vfn'   
     #, ' 241     NLO  ss      ffn'   
     #, ' 242     NLO  ss      vfn'   
     #, ' 243     NNLO  nominal ffn        .182  (as=0.1143) MS  '
     #, ' 244     NNLO  nominal vfn'
     #, ' 245     NNLO  mc=1.75 ffn'
     #, ' 246     NNLO  mc=1.75 vfn'   
     #, ' 247     NNLO  ss      ffn'   
     #, ' 248     NNLO  ss      vfn'   
     #, ' 249     NNLO  slow ev ffn'   
     #, ' 250     NNLO  slow ev vfn'   
     #, ' To get the sets with errorrs, do:'
     #, ' call errsk(i), con i=-15...15. After this, calls to'
     #, ' mlmpdf will return the pdf minus (plus) the variation'
     #, ' if the |ith| parameter'
C ---------------------------------------------------------------------------
      WRITE(*,100)                             
     #  '  PDF sets followed by * are obtained from a 1-loop analysis,'
     # ,'  and the relative values of Lambda_4 refer to 1-loop. '
     # ,'  Lambda is automatically converted to 2-loop for use with '
     # ,'  a 2-loop alpha in the program. The conversion is performed'
     # ,'  in such a way that at a scale of 10 GeV the value of alpha'
     # ,'  is the same. The MSbar subtr. scheme'
     # ,'  is used by default with 1-loop structure functions.'
     # ,'  MT set 26 has SU(3)-violating strange sea distributions'
     # ,'  Morfin and Tung sets labeled 25 and 27 are simply MSbar '
     # ,'  versions of sets 21 and 26, respectively.'
     # ,'  Sets 13-16 are MRS fits of BCDMS data using'
     # ,'  different values of Lambda PHYS REV D43 (91) 3648.'
     # ,'  Sets 17-19 are the new NMC/CCFR fits by MRS (RAL-92-021)'
      WRITE(*,100)                             
     #  '  Set 20: MRSA (Durham preprint, DTP/94/34)'
     # ,'  Set 71: MRSA prime (Durham preprint, DTP/95/14)'
     # ,'  Set 72: MRSG (Durham preprint, DTP/95/14)'
     # ,'  Sets 73-78 are the MRS structure functions '
     # ,'  with variable Lambda. The values of Lambda5 quoted '
     # ,'  here correspond to values of alpha(Mz) of 0.105,0.110,0.115'
     # ,'  0.120,0.125,0.130, which is slightly different from the'
     # ,'  values one would obtain with the usual matching procedure'
     # ,'  from the corresponding value of Lambda4 quoted by MRS'
      WRITE(*,100)
     #  '  Sets 61-65 are the CTEQ1 fits (61=default, 62=sing.gluon,'
     # ,'  63= LEP lambda, 64=DIS scheme, 65=LO fit).'
     # ,'  Sets 81-89 are the CTEQ4 fits, H.L. Lai et al.,'
     # ,'  CTEQ-604, hep-ph/9606399, (81=default, 82=DIS scheme,'
     # ,'  83=leading order, 84-87=variable Lambda, 88=High-et jet fit,'
     # ,'  89=low momentum evolution)'
      WRITE(*,100)                             
     #  '  Set 40 corresponds to photon PDF''s by Drees, Grassie, Kim'
     # ,'  Z.Phys. C28 (1985) 51 and DTP/91/16'
     # ,'  Set 41 corresponds to photon PDF''s Aurenche et al.'
     # ,'  Set 42 corresponds to photon PDF''s Aurenche et al. (1994)'
     # ,'  Set 43 corresponds to photon PDF''s Glueck et al.'
     # ,'  Set 44 corresponds to photon PDF''s Abramowicz et al.'
     # ,'  Set 45 corresponds to photon PDF''s GRS (99)'
     # ,'  Set 51 corresponds to electron with photon LAC1'
     # ,'  Set 52 corresponds to electron with photon GRV-G HO'
     # ,'  Set 53 corresponds to electron (user-defined)'
     # ,'  GRV-G HO photon uses the DIS_gamma scheme, defined'
     # ,'  in Gluck, Reya and Vogt, Phys. Rev. D45(1992)3986.'
 100  FORMAT(1X,A,100(/,1X,A))
      END

      SUBROUTINE PDFPAR(J,IH,XLAM,SCHE,IRET)
      PARAMETER (NPDF=250)
C LAMBDA VALUES (lAMBDA_5FLAVOUR_2LOOP) FOR DIFFERENT PARTON DENSITIES
      IMPLICIT REAL * 8 (A-H,O-Z)
      CHARACTER * 2 SCHE,SCH(NPDF)
      DIMENSION XLA(NPDF)
      DATA SCH/4*'MS',3*'DI',2*'  ',
     # 11*'MS',
     #  4*'DI','MS','DI',2*'MS',2*'  ',
     #  3*'MS',6*' ',
c photon densities
     #  3*'MS','DG','MS','DG',5*'  ',
c electron densities
     #  'MS','DG','**',7*'  ',
c CTEQ1
     #  3*'MS','DI','MS',
c CTEQ3
     #  2*'MS','DI',2*'  ',
c MRSAp, MRSG, MRSalpha
     #  8*'MS',2*'  ',
c CTEQ4
     #  'MS','DI',7*'MS','  ',
c MRSR and MRST
     #  9*'MS','  ',
c CTEQ5
     #  'MS','DI',6*'MS',2*'MS',
c MRST99
     #  12*'MS',8*'  ',
C CTEQ6
     #  'MS','DI','MS',40*'MS',7*'  ',
c MRS2001NNLO
     #  4*'MS',
c MRS2001
     #  4*'MS','LO',' ',
c MRST2002
     #  2*'MS',7*' ',
c MRST2002E
     # 31*'MS',
c Alekhin
     # 20*'MS'/
c
      DATA XLA/
c 1 DO
     # .34D0,.68D0,.34D0,.49D0,
     # .101D0,.173D0,.250D0,2*0.D0,
c 10 MRSA mod
     # .151d0,.122D0,.122D0,.083D0,.101D0,.130D0,.155D0,3*.140d0,.151d0,
c 21 MT S1
     # .138D0,.125D0,.123D0,.097D0,.138d0,2*.156d0,.245d0,2*0.D0,
     #  3*.122D0,6*0.D0,
c 40 photon densities
     #  0.68D0,4*.130D0,0.1793D0,5*0.D0,
c 51 electron densities
     #  2*0.130D0,0.001d0,7*0.D0,
c 61 CTEQ1M
     #  2*0.152D0,0.220D0,0.164D0,0.249D0,
c 66 CTEQ3M
     #  0.158d0,0.263d0,0.164d0,2*0.D0,
c 71 MRSA prime
     #  0.152D0,0.170D0,
c 73 MRSA-alpha dependent
     #  0.09936d0,0.1396d0,0.1903d0,0.2526d0,0.3276d0,0.4162d0,2*0.D0,
c The values given above for the MRSXXX sets are consistent with the 
c alfas(Mz) given by MRS. The values    
c     #  .094d0,0.130d0,0.178d0,0.237d0,0.309d0,0.396d0/
c are on the other hand consistent with the Lambda_4 given by MRS
c CTEQ4
     #  3*0.2018d0,0.1396d0,0.1687d0,
     #  0.2392d0,0.2811d0,0.2018d0,0.1793d0,0.d0,
c 91-94 MRSR
     #  0.159d0,0.237d0,0.159d0,0.237d0,
c 95-99 MRST                       
     #  3*0.220d0,0.164d0,0.288d0,0.d0,
C 101-110 CTEQ5                    
     #  2*0.226d0,0.359d0,2*0.226d0,2*1.d-8,0.226d0,2*0.226d0,
c 111-122 MRST99
     #  3*.220d0,.164d0,.288d0,.224d0,.215d0,5*.220d0,8*0d0,
C 131-173 CTEQ6
     # 43*0.226D0,7*0d0,
C 181-184, MRS2001NNLO
     # 3*0.196d0,0.226d0,
c 185-189, MRS2001
     # 0.239d0,0.214d0,2*0.267d0,0.416d0,0d0,
c 191-192, MRST2002
     # 0.249d0,0.195d0,7*0d0,
c 200-230
     # 31*0.239d0,
c 231-250, Alekhin 20 sets
     # 6*0.418d0,6*0.215d0,8*0.182d0/
      IRET=0       
      IF(ABS(IH).NE.1.AND.IH.NE.4.AND.IH.NE.5)THEN
        WRITE(*,*) ' HADRON TPYE ',IH,' NOT IMPLEMENTED'
        IRET=1
        RETURN
      ENDIF
      IF(J.LT.1.OR.J.GT.NPDF) THEN
        WRITE(*,*) ' PDF SET ',J,' NOT EXISTING'
        IRET=1
        RETURN
      ENDIF
C LAMBDA_QCD, MSbar, 5 FLAVOURS
      XLAM = XLA(J)
C SCHEME
      SCHE = SCH(J)
      IF(XLAM.EQ.0.OR.SCHE.EQ.'  ') THEN
        WRITE(*,*) ' PDF SET ',J,' NOT EXISTING'
        IRET=1
        RETURN
      ENDIF
C CHECK IF HADRON TYPE AND PDF SET ARE CONSISTENT
      IF(
     #  ABS(IH).EQ.1
C It is a proton/antiproton
     # .AND. J.NE.1
C It is not DO I
     # .AND. J.NE.2
C It is not DO II
     # .AND. J.NE.3
C It is not EHLQ I
     # .AND. J.NE.4
C It is not EHLQ II
     # .AND. J.NE.5
C It is not DFLM 160
     # .AND. J.NE.6
C It is not DFLM 260
     # .AND. J.NE.7
C It is not DFLM 360
     # .AND. J.NE.10
C It is not MRSA modified
     # .AND. J.NE.11
C It is not HMRS B
     # .AND. J.NE.12
C It is not KMRS B
     # .AND. J.NE.13
C It is not MRS B135
     # .AND. J.NE.14
C It is not MRS B160
     # .AND. J.NE.15
C It is not MRS B200
     # .AND. J.NE.16
C It is not MRS B235
     # .AND. J.NE.17
C It is not MRS S0
     # .AND. J.NE.18
C It is not MRS D0
     # .AND. J.NE.19
C It is not MRSD-
     # .AND. J.NE.20
C It is not MRSA
     # .AND. J.NE.21
C It is not MT S1
     # .AND. J.NE.22
C It is not MT B1
     # .AND. J.NE.23
C It is not MT B2
     # .AND. J.NE.24
C It is not MT E1
     # .AND. J.NE.25
C It is not MT S1M
     # .AND. J.NE.26
C It is not MT S2
     # .AND. J.NE.27
C It is not MT S2M
     # .AND. J.NE.28
C It is not MT SL
     # .AND. J.NE.61
C It is not CTEQ1M
     # .AND. J.NE.62
C It is not CTEQ1MS
     # .AND. J.NE.63
C It is not CTEQ1ML
     # .AND. J.NE.64
C It is not CTEQ1D
     # .AND. J.NE.65
C It is not CTEQ1L
     # .AND. J.NE.66
C It is not CTEQ3M
     # .AND. J.NE.67
C It is not CTEQ3L
     # .AND. J.NE.68
C It is not CTEQ3D
     # .AND. J.NE.71
C It is not MRSA prime
     # .AND. J.NE.72
C It is not MRSG
     # .AND. J.NE.73
C It is not MRS105
     # .AND. J.NE.74
C It is not MRS110
     # .AND. J.NE.75
C It is not MRS115
     # .AND. J.NE.76
C It is not MRS120
     # .AND. J.NE.77
C It is not MRS125
     # .AND. J.NE.78
C It is not MRS130
     # .AND. J.NE.81
C It is not CTEQ4M
     # .AND. J.NE.82
C It is not CTEQ4D
     # .AND. J.NE.83
C It is not CTEQ4L
     # .AND. J.NE.84
C It is not CTEQ4A1
     # .AND. J.NE.85
C It is not CTEQ4A2
     # .AND. J.NE.86
C It is not CTEQ4A4
     # .AND. J.NE.87
C It is not CTEQ4A5
     # .AND. J.NE.88
C It is not CTEQ4HJ
     # .AND. J.NE.89
C It is not CTEQ4LQ
     # .AND. J.NE.91
C It is not MRSR1
     # .AND. J.NE.92
C It is not MRSR2
     # .AND. J.NE.93
C It is not MRSR3
     # .AND. J.NE.94
C It is not MRSR4
     # .AND. J.NE.95
C It is not MRST1
     # .AND. J.NE.96
C It is not MRSTH
     # .AND. J.NE.97
C It is not MRSTL
     # .AND. J.NE.98
C It is not MRSTM
     # .AND. J.NE.99 
C It is not MRSTP
     # .AND. J.NE.101 
C It is not CTEQ5M
     # .AND. J.NE.102 
C It is not CTEQ5D
     # .AND. J.NE.103 
C It is not CTEQ5L
     # .AND. J.NE.104 
C It is not CTEQ5HJ
     # .AND. J.NE.105 
C It is not CTEQ5HQ
     # .AND. J.NE.106 
C It is not CTEQ5F3
     # .AND. J.NE.107 
C It is not CTEQ5F4
     # .AND. J.NE.108
C It is not CTEQ5M1
     # .AND. J.NE.109
C It is not CTEQ5HQ1
     # .AND. J.NE.110
C It is not CTEQ5M1 parametrized form
     # .AND. J.NE.111 
C It is not MRST991
     # .AND. J.NE.112 
C It is not MRST992
     # .AND. J.NE.113 
C It is not MRST993
     # .AND. J.NE.114 
C It is not MRST994
     # .AND. J.NE.115 
C It is not MRST995
     # .AND. J.NE.116 
C It is not MRST996
     # .AND. J.NE.117 
C It is not MRST997
     # .AND. J.NE.118 
C It is not MRST998
     # .AND. J.NE.119 
C It is not MRST999
     # .AND. J.NE.120 
C It is not MRST9910
     # .AND. J.NE.121 
C It is not MRST9911
     # .AND. J.NE.122 
C It is not MRST9912
     # .AND. J.NE.131
C It is not CTEQ6M
     # .AND. J.NE.132
C It is not CTEQ6D
     # .AND. J.NE.133
C It is not CTEQ6L
     # .AND. (.NOT.(J.GE.134.AND.J.LE.173))
C It is not CTEQ6M1xx
     # .AND. J.NE.181
C It is not MRST2001NNLO av
     # .AND. J.NE.182
C It is not MRST2001NNLO fast
     # .AND. J.NE.183
C It is not MRST2001NNLO slow
     # .AND. J.NE.184
C It is not MRST2001NNLO jet
     # .AND. J.NE.185
C It is not MRST2001 best fit
     # .AND. J.NE.186
C It is not MRST2001 low as
     # .AND. J.NE.187
C It is not MRST2001 high as
     # .AND. J.NE.188
C It is not MRST2001 jet fit
     # .AND. J.NE.189
C It is not MRST2001  lo
     # .AND. J.NE.191
C It is not MRST2002
     # .AND. J.NE.192
C It is not MRST2002NNLO
     # .AND. (.NOT.(J.GE.200.AND.J.LE.230))
C It is not MRST2001Exx
     # .AND. (.NOT.(J.GE.231.AND.J.LE.250)) )
C It is not Alekhinxx
C It is not a proton PDF
     # THEN
         WRITE(*,*) ' PDF SET ',J,' NOT AVAILABLE FOR PROTONS'
         SCHE='XX'
         XLAM=0.0
         IRET=1
         RETURN
      ENDIF
      IF(
     #  ABS(IH).EQ.4
C It is a photon
     # .AND. J.NE.40
C It is not Drees e Grassie
     # .AND. J.NE.41 
C It is not ACFGP
     # .AND. J.NE.42 
C It is not AFG
     # .AND. J.NE.43
C It is not GRV-HO
     # .AND. J.NE.44 
C It is not LAC1
     # .AND. J.NE.45 )
C It is not GRS-HO
C It is not a photon PDF
     # THEN
         WRITE(*,*) ' PDF SET ',J,' NOT AVAILABLE FOR PHOTONS'
         SCHE='XX'
         XLAM=0.0
         IRET=1
         RETURN
      ENDIF
      IF(
     #  ABS(IH).EQ.5
C It is an electron
     # .AND. J.NE.51 
C It is not LAC1
     # .AND. J.NE.52
C It is not GRV-G HO
     # .AND. J.NE.53 )
C It is not USER DEFINED
     # THEN
         WRITE(*,*) ' PDF SET ',J,' NOT AVAILABLE FOR ELECTRONS'
         SCHE='XX'
         XLAM=0.0
         IRET=1
         RETURN
      ENDIF
      END

C--------------------------------------------------
C- STRUCTURE FUNCTION MAIN PROGRAM
C--------------------------------------------------
      SUBROUTINE MLMPDF(NDNS,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF),DISF(13)
      INTEGER IPAR(-6:6)
      DATA IPAR/12,11,10,9,7,8,13,2,1,3,4,5,6/
C Fix to prevent undefined math operations for x=1.
C Assumes that all structure functions vanish for x=1.
C Modified on 7/11/2008 to exclude also x<=0 and x>1
      IF(X.LE.0.OR.X.GE.1) THEN
         DO J=-NF,NF
            FX(J) = 0
         ENDDO
         RETURN
      ENDIF
C
      IH0=IH
      IF(IH.EQ.0) IH0=1
      IF(NDNS.LE.4) THEN
C--DO1,DO2,EHLQ1,EHLQ2
           Q=SQRT(Q2)
           CALL DOEHLQ(X,Q,IH0,NDNS,DISF,NF)
        DO I =-NF,NF
           FX(I) = DISF(IPAR(I)) / X
        ENDDO
      ELSEIF(NDNS.LE.9) THEN
C--DFLM
        ISET=NDNS-4
        CALL DFLM(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.10) THEN
C--MRSA modified
        CALL XMRSA(Q2,X,FX,NF)
      ELSEIF(NDNS.LE.20) THEN
C--MRS,HMRS,KMRS SETS
        ISET=NDNS-10
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.30) THEN
C--MORFIN AND TUNG
        ISET=NDNS-20
        CALL TUNG(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.45) THEN
C--PHOTON PDFS
        ISET=NDNS-40
        IF(ISET.EQ.0) THEN
C--DREES,GRASSIE, KIM 
           CALL PHOPDF(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.1) THEN
C--AURENCHE ET AL
            CALL FONPDF(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.2) THEN
C--AURENCHE 1994
            CALL AFGPDF(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.3) THEN
C--GLUECK NLO
            CALL GRV_PH(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.4) THEN
C--LAC
            CALL XLAC(1,Q2,X,FX,NF)
        ELSEIF(ISET.EQ.5) THEN
C--GRS NLO
            CALL GRS_PH(Q2,X,FX,NF)
        ENDIF
C--ELECTRON PDFS
      ELSEIF(NDNS.LE.53) THEN
        ISET=NDNS-50
        IF(ISET.EQ.1) THEN
            CALL ELPDF_LAC1(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.2) THEN
            CALL ELPDF_GRV(Q2,X,FX,NF)
        ELSEIF(ISET.EQ.3) THEN
            CALL ELPDF_USER(Q2,X,FX,NF)
        ENDIF
      ELSEIF(NDNS.LE.65) THEN
C-- CTEQ1 FITS          
        ISET=NDNS-60
        CALL CTEQ(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.70) THEN
C-- CTEQ3 FITS          
        ISET=NDNS-65
        CALL CTEQ3(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.80) THEN
C-- MRSAP, MRSG AND MRS WITH VARIABLE LAMBDA
        ISET=NDNS-60
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.89) THEN
C-- CTEQ4 FITS          
        ISET=NDNS-80
        CALL CTEQ4(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.99) THEN
C-- MRSR/T sets        
        ISET=NDNS-60
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.110) THEN
C-- CTEQ5 FITS          
        ISET=NDNS-100
        CALL CTEQ5(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.122) THEN
C-- MRST99 sets        
        ISET=NDNS-70
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
      ELSEIF(NDNS.LE.173) THEN
C-- CTEQ6 FITS          
        ISET=NDNS-130
        IF(ISET.GE.4) ISET=ISET-3+100
        CALL CTEQ6(ISET,IH0,Q2,X,FX,NF)
C-- MRSTNNLO (200?)
      ELSEIF(NDNS.LE.184) THEN
        ISET=NDNS-(184-56)
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
C-- MRST2001
      ELSEIF(NDNS.LE.188) THEN
        ISET=NDNS-(188-60) 
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
C-- MRST2001 lo
      ELSEIF(NDNS.EQ.189) THEN
        ISET=NDNS-(189-61) 
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
C-- MRST2002, MRST2002NNLO
      ELSEIF(NDNS.LE.192) THEN
        ISET=NDNS-(192-63) 
        CALL HMRS(ISET,IH0,Q2,X,FX,NF)
C-- MRST2001E
      ELSEIF(NDNS.LE.230) THEN
         ISET=NDNS-(230-94) 
         CALL HMRS(ISET,IH0,Q2,X,FX,NF)
C-- ALEKHIN
      ELSEIF(NDNS.LE.250) THEN
         ISET=NDNS-230 
         CALL ALEKHIN(ISET,X,Q2,FX,NF)
         CALL HADCONV(FX,IH0,NF)
      ELSE
        WRITE(*,*) ' STRUCTURE FUNCTION SET NOT DEFINED , STOP'
        STOP
      ENDIF
      IF(IH.EQ.0) THEN
        FX(1)  = 0.5 * ( FX(1)+FX(2) )
        FX(-1) = 0.5 * ( FX(-1)+FX(-2) )
        FX(2)  = FX(1)
        FX(-2) = FX(-1)
      ENDIF
      END


      subroutine hadconv(fx,ih0,nf)
      implicit none
      integer nf,ih0,j
      real * 4 fx(-nf:nf),tmp
      if(ih0.eq.-1) then
c antiproton
         do j=1,nf
            tmp=fx(j)
            fx(j)=fx(-j)
            fx(-j)=tmp
         enddo
      elseif(ih0.eq.2) then
c neutron
         tmp=fx(1)
         fx(1)=fx(2)
         fx(2)=tmp
         tmp=fx(-1)
         fx(-1)=fx(-2)
         fx(-2)=tmp
      elseif(ih0.eq.0) then
c nucleon
         fx(1)=(fx(1)+fx(2))/2
         fx(2)=fx(1)
         fx(-1)=(fx(-1)+fx(-2))/2
         fx(-2)=fx(-1)
      elseif(ih0.ne.1) then
         write(*,*) ' hadron ',ih0, 'not implemented'
         stop
      endif
      end


C------------------------------------------------------------------------
      SUBROUTINE DOEHLQ(X,SCALE,IDHAD,NSET,DIST,NF)
C     NUCLEON AND PION STRUCTURE FUNCTIONS DIST=X*QRK(X,Q=SCALE)
C
C     IDHAD = TYPE OF HADRON:
C     1 = P   -1 = PBAR  2 = N   -2 = NBAR  38 = PI+  30 = PI-
C
C     NSET = STRUCTURE FUNCTION SET
C          = 1,2 FOR DUKE+OWENS SETS 1,2 (SOFT/HARD GLUE)
C          = 3,4 FOR EICHTEN ET AL SETS 1,2 (NUCLEON ONLY)
C
C   DUKE+OWENS = D.W.DUKE AND J.F.OWENS, PHYS. REV. D30 (1984) 49 (P/N)
C              + J.F.OWENS, PHYS. REV. D30 (1984) 943 (PI+/-)
C   WITH EXTRA SIGNIFICANT FIGURES VIA ED BERGER
C   WARNING....MOMENTUM SUM RULE BADLY VIOLATED ABOVE 1 TEV
C   PION NOT RELIABLE ABOVE SCALE = 50 GEV
C
C   EICHTEN ET AL = E.EICHTEN,I.HINCHLIFFE,K.LANE AND C.QUIGG,
C                   REV. MOD. PHYS. 56 (1984) 579
C   REVISED AS IN   REV. MOD. PHYS. 58 (1986) 1065
C   RELIABLE RANGE : SQRT(5)GEV < SCALE < 10TEV, 1E-4 < X < 1
C
C------------------------------------------------------------------------
      REAL DIST(13),G(2),Q0(4),QL(4),F(5),A(6,5),B(3,6,5,4)
      REAL XQ(6),TX(6),TT(6),TB(6),NEHLQ(8,2),CEHLQ(6,6,2,8,2)
      REAL TBMIN(2),TTMIN(2)
      DATA (((B(I,J,K,1),I=1,3),J=1,6),K=1,5)/
     &3.,0.,0.,.419,.004383,-.007412,
     &3.46,.72432,-.065998,4.4,-4.8644,1.3274,
     &6*0.,1.,
     &0.,0.,.763,-.23696,.025836,4.,.62664,-.019163,
     &0.,-.42068,.032809,6*0.,1.265,-1.1323,.29268,
     &0.,-.37162,-.028977,8.05,1.5877,-.15291,
     &0.,6.3059,-.27342,0.,-10.543,-3.1674,
     &0.,14.698,9.798,0.,.13479,-.074693,
     &-.0355,-.22237,-.057685,6.3494,3.2649,-.90945,
     &0.,-3.0331,1.5042,0.,17.431,-11.255,
     &0.,-17.861,15.571,1.564,-1.7112,.63751,
     &0.,-.94892,.32505,6.,1.4345,-1.0485,
     &9.,-7.1858,.25494,0.,-16.457,10.947,
     &0.,15.261,-10.085/
      DATA (((B(I,J,K,2),I=1,3),J=1,6),K=1,5)/
     &3.,0.,0.,.3743,.013946,-.00031695,
     &3.329,.75343,-.076125,6.032,-6.2153,1.5561,
     &6*0.,1.,0.,
     &0.,.7608,-.2317,.023232,3.83,.62746,-.019155,
     &0.,-.41843,.035972,6*0.,1.6714,-1.9168,.58175,
     &0.,-.27307,-.16392,9.145,.53045,-.76271,
     &0.,15.665,-2.8341,0.,-100.63,44.658,
     &0.,223.24,-116.76,0.,.067368,-.030574,
     &-.11989,-.23293,-.023273,3.5087,3.6554,-.45313,
     &0.,-.47369,.35793,0.,9.5041,-5.4303,
     &0.,-16.563,15.524,.8789,-.97093,.43388,
     &0.,-1.1612,.4759,4.,1.2271,-.25369,
     &9.,-5.6354,-.81747,0.,-7.5438,5.5034,
     &0.,-.59649,.12611/
      DATA (((B(I,J,K,3),I=1,3),J=1,6),K=1,5)/
     &1.,0.,0.,0.4,-0.06212,-0.007109,0.7,0.6478,0.01335,27*0.,
     &0.9,-0.2428,0.1386,0.,-0.2120,0.003671,5.0,0.8673,0.04747,
     &0.,1.266,-2.215,0.,2.382,0.3482,3*0.,
     &0.,0.07928,-0.06134,-0.02212,-0.3785,-0.1088,2.894,9.433,
     &-10.852,0.,5.248,-7.187,0.,8.388,-11.61,3*0.,
     &0.888,-1.802,1.812,0.,-1.576,1.20,3.11,-0.1317,0.5068,
     &6.0,2.801,-12.16,0.,-17.28,20.49,3*0./
      DATA (((B(I,J,K,4),I=1,3),J=1,6),K=1,5)/
     &1.,0.,0.,0.4,-0.05909,-0.006524,0.628,0.6436,0.01451,27*0.,
     &0.90,-0.1417,-0.1740,0.,-0.1697,-0.09623,5.0,-2.474,1.575,
     &0.,-2.534,1.378,0.,0.5621,-0.2701,3*0.,
     &0.,0.06229,-0.04099,-0.0882,-0.2892,-0.1082,1.924,0.2424,
     &2.036,0.,-4.463,5.209,0.,-0.8367,-0.04840,3*0.,
     &0.794,-0.9144,0.5966,0.,-1.237,0.6582,2.89,0.5966,-0.2550,
     &6.0,-3.671,-2.304,0.,-8.191,7.758,3*0./
C...THE FOLLOWING DATA LINES ARE COEFFICIENTS NEEDED IN THE
C...EICHTEN, HINCHLIFFE, LANE, QUIGG PROTON STRUCTURE FUNCTION
C...POWERS OF 1-X IN DIFFERENT CASES
      DATA NEHLQ/3,4,7,5,7,7,7,7,3,4,7,6,7,7,7,7/
C...EXPANSION COEFFICIENTS FOR UP VALENCE QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,1,1),IX=1,6),IT=1,6),NX=1,2)/
     1 7.677E-01,-2.087E-01,-3.303E-01,-2.517E-02,-1.570E-02,-1.000E-04,
     2-5.326E-01,-2.661E-01, 3.201E-01, 1.192E-01, 2.434E-02, 7.620E-03,
     3 2.162E-01, 1.881E-01,-8.375E-02,-6.515E-02,-1.743E-02,-5.040E-03,
     4-9.211E-02,-9.952E-02, 1.373E-02, 2.506E-02, 8.770E-03, 2.550E-03,
     5 3.670E-02, 4.409E-02, 9.600E-04,-7.960E-03,-3.420E-03,-1.050E-03,
     6-1.549E-02,-2.026E-02,-3.060E-03, 2.220E-03, 1.240E-03, 4.100E-04,
     1 2.395E-01, 2.905E-01, 9.778E-02, 2.149E-02, 3.440E-03, 5.000E-04,
     2 1.751E-02,-6.090E-03,-2.687E-02,-1.916E-02,-7.970E-03,-2.750E-03,
     3-5.760E-03,-5.040E-03, 1.080E-03, 2.490E-03, 1.530E-03, 7.500E-04,
     4 1.740E-03, 1.960E-03, 3.000E-04,-3.400E-04,-2.900E-04,-1.800E-04,
     5-5.300E-04,-6.400E-04,-1.700E-04, 4.000E-05, 6.000E-05, 4.000E-05,
     6 1.700E-04, 2.200E-04, 8.000E-05, 1.000E-05,-1.000E-05,-1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,1,2),IX=1,6),IT=1,6),NX=1,2)/
     1 7.237E-01,-2.189E-01,-2.995E-01,-1.909E-02,-1.477E-02, 2.500E-04,
     2-5.314E-01,-2.425E-01, 3.283E-01, 1.119E-01, 2.223E-02, 7.070E-03,
     3 2.289E-01, 1.890E-01,-9.859E-02,-6.900E-02,-1.747E-02,-5.080E-03,
     4-1.041E-01,-1.084E-01, 2.108E-02, 2.975E-02, 9.830E-03, 2.830E-03,
     5 4.394E-02, 5.116E-02,-1.410E-03,-1.055E-02,-4.230E-03,-1.270E-03,
     6-1.991E-02,-2.539E-02,-2.780E-03, 3.430E-03, 1.720E-03, 5.500E-04,
     1 2.410E-01, 2.884E-01, 9.369E-02, 1.900E-02, 2.530E-03, 2.400E-04,
     2 1.765E-02,-9.220E-03,-3.037E-02,-2.085E-02,-8.440E-03,-2.810E-03,
     3-6.450E-03,-5.260E-03, 1.720E-03, 3.110E-03, 1.830E-03, 8.700E-04,
     4 2.120E-03, 2.320E-03, 2.600E-04,-4.900E-04,-3.900E-04,-2.300E-04,
     5-6.900E-04,-8.200E-04,-2.000E-04, 7.000E-05, 9.000E-05, 6.000E-05,
     6 2.400E-04, 3.100E-04, 1.100E-04, 0.000E+00,-2.000E-05,-2.000E-05/
C...EXPANSION COEFFICIENTS FOR DOWN VALENCE QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,2,1),IX=1,6),IT=1,6),NX=1,2)/
     1 3.813E-01,-8.090E-02,-1.634E-01,-2.185E-02,-8.430E-03,-6.200E-04,
     2-2.948E-01,-1.435E-01, 1.665E-01, 6.638E-02, 1.473E-02, 4.080E-03,
     3 1.252E-01, 1.042E-01,-4.722E-02,-3.683E-02,-1.038E-02,-2.860E-03,
     4-5.478E-02,-5.678E-02, 8.900E-03, 1.484E-02, 5.340E-03, 1.520E-03,
     5 2.220E-02, 2.567E-02,-3.000E-05,-4.970E-03,-2.160E-03,-6.500E-04,
     6-9.530E-03,-1.204E-02,-1.510E-03, 1.510E-03, 8.300E-04, 2.700E-04,
     1 1.261E-01, 1.354E-01, 3.958E-02, 8.240E-03, 1.660E-03, 4.500E-04,
     2 3.890E-03,-1.159E-02,-1.625E-02,-9.610E-03,-3.710E-03,-1.260E-03,
     3-1.910E-03,-5.600E-04, 1.590E-03, 1.590E-03, 8.400E-04, 3.900E-04,
     4 6.400E-04, 4.900E-04,-1.500E-04,-2.900E-04,-1.800E-04,-1.000E-04,
     5-2.000E-04,-1.900E-04, 0.000E+00, 6.000E-05, 4.000E-05, 3.000E-05,
     6 7.000E-05, 8.000E-05, 2.000E-05,-1.000E-05,-1.000E-05,-1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,2,2),IX=1,6),IT=1,6),NX=1,2)/
     1 3.578E-01,-8.622E-02,-1.480E-01,-1.840E-02,-7.820E-03,-4.500E-04,
     2-2.925E-01,-1.304E-01, 1.696E-01, 6.243E-02, 1.353E-02, 3.750E-03,
     3 1.318E-01, 1.041E-01,-5.486E-02,-3.872E-02,-1.038E-02,-2.850E-03,
     4-6.162E-02,-6.143E-02, 1.303E-02, 1.740E-02, 5.940E-03, 1.670E-03,
     5 2.643E-02, 2.957E-02,-1.490E-03,-6.450E-03,-2.630E-03,-7.700E-04,
     6-1.218E-02,-1.497E-02,-1.260E-03, 2.240E-03, 1.120E-03, 3.500E-04,
     1 1.263E-01, 1.334E-01, 3.732E-02, 7.070E-03, 1.260E-03, 3.400E-04,
     2 3.660E-03,-1.357E-02,-1.795E-02,-1.031E-02,-3.880E-03,-1.280E-03,
     3-2.100E-03,-3.600E-04, 2.050E-03, 1.920E-03, 9.800E-04, 4.400E-04,
     4 7.700E-04, 5.400E-04,-2.400E-04,-3.900E-04,-2.400E-04,-1.300E-04,
     5-2.600E-04,-2.300E-04, 2.000E-05, 9.000E-05, 6.000E-05, 4.000E-05,
     6 9.000E-05, 1.000E-04, 2.000E-05,-2.000E-05,-2.000E-05,-1.000E-05/
C...EXPANSION COEFFICIENTS FOR UP AND DOWN SEA QUARK DISTRIBUTIONS
      DATA (((CEHLQ(IX,IT,NX,3,1),IX=1,6),IT=1,6),NX=1,2)/
     1 6.870E-02,-6.861E-02, 2.973E-02,-5.400E-03, 3.780E-03,-9.700E-04,
     2-1.802E-02, 1.400E-04, 6.490E-03,-8.540E-03, 1.220E-03,-1.750E-03,
     3-4.650E-03, 1.480E-03,-5.930E-03, 6.000E-04,-1.030E-03,-8.000E-05,
     4 6.440E-03, 2.570E-03, 2.830E-03, 1.150E-03, 7.100E-04, 3.300E-04,
     5-3.930E-03,-2.540E-03,-1.160E-03,-7.700E-04,-3.600E-04,-1.900E-04,
     6 2.340E-03, 1.930E-03, 5.300E-04, 3.700E-04, 1.600E-04, 9.000E-05,
     1 1.014E+00,-1.106E+00, 3.374E-01,-7.444E-02, 8.850E-03,-8.700E-04,
     2 9.233E-01,-1.285E+00, 4.475E-01,-9.786E-02, 1.419E-02,-1.120E-03,
     3 4.888E-02,-1.271E-01, 8.606E-02,-2.608E-02, 4.780E-03,-6.000E-04,
     4-2.691E-02, 4.887E-02,-1.771E-02, 1.620E-03, 2.500E-04,-6.000E-05,
     5 7.040E-03,-1.113E-02, 1.590E-03, 7.000E-04,-2.000E-04, 0.000E+00,
     6-1.710E-03, 2.290E-03, 3.800E-04,-3.500E-04, 4.000E-05, 1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,3,2),IX=1,6),IT=1,6),NX=1,2)/
     1 1.008E-01,-7.100E-02, 1.973E-02,-5.710E-03, 2.930E-03,-9.900E-04,
     2-5.271E-02,-1.823E-02, 1.792E-02,-6.580E-03, 1.750E-03,-1.550E-03,
     3 1.220E-02, 1.763E-02,-8.690E-03,-8.800E-04,-1.160E-03,-2.100E-04,
     4-1.190E-03,-7.180E-03, 2.360E-03, 1.890E-03, 7.700E-04, 4.100E-04,
     5-9.100E-04, 2.040E-03,-3.100E-04,-1.050E-03,-4.000E-04,-2.400E-04,
     6 1.190E-03,-1.700E-04,-2.000E-04, 4.200E-04, 1.700E-04, 1.000E-04,
     1 1.081E+00,-1.189E+00, 3.868E-01,-8.617E-02, 1.115E-02,-1.180E-03,
     2 9.917E-01,-1.396E+00, 4.998E-01,-1.159E-01, 1.674E-02,-1.720E-03,
     3 5.099E-02,-1.338E-01, 9.173E-02,-2.885E-02, 5.890E-03,-6.500E-04,
     4-3.178E-02, 5.703E-02,-2.070E-02, 2.440E-03, 1.100E-04,-9.000E-05,
     5 8.970E-03,-1.392E-02, 2.050E-03, 6.500E-04,-2.300E-04, 2.000E-05,
     6-2.340E-03, 3.010E-03, 5.000E-04,-3.900E-04, 6.000E-05, 1.000E-05/
C...EXPANSION COEFFICIENTS FOR GLUON DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,4,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.482E-01,-9.578E-01, 1.009E-01,-1.051E-01, 3.456E-02,-3.054E-02,
     2-9.627E-01, 5.379E-01, 3.368E-01,-9.525E-02, 1.488E-02,-2.051E-02,
     3 4.300E-01,-8.306E-02,-3.372E-01, 4.902E-02,-9.160E-03, 1.041E-02,
     4-1.925E-01,-1.790E-02, 2.183E-01, 7.490E-03, 4.140E-03,-1.860E-03,
     5 8.183E-02, 1.926E-02,-1.072E-01,-1.944E-02,-2.770E-03,-5.200E-04,
     6-3.884E-02,-1.234E-02, 5.410E-02, 1.879E-02, 3.350E-03, 1.040E-03,
     1 2.948E+01,-3.902E+01, 1.464E+01,-3.335E+00, 5.054E-01,-5.915E-02,
     2 2.559E+01,-3.955E+01, 1.661E+01,-4.299E+00, 6.904E-01,-8.243E-02,
     3-1.663E+00, 1.176E+00, 1.118E+00,-7.099E-01, 1.948E-01,-2.404E-02,
     4-2.168E-01, 8.170E-01,-7.169E-01, 1.851E-01,-1.924E-02,-3.250E-03,
     5 2.088E-01,-4.355E-01, 2.239E-01,-2.446E-02,-3.620E-03, 1.910E-03,
     6-9.097E-02, 1.601E-01,-5.681E-02,-2.500E-03, 2.580E-03,-4.700E-04/
      DATA (((CEHLQ(IX,IT,NX,4,2),IX=1,6),IT=1,6),NX=1,2)/
     1 2.367E+00, 4.453E-01, 3.660E-01, 9.467E-02, 1.341E-01, 1.661E-02,
     2-3.170E+00,-1.795E+00, 3.313E-02,-2.874E-01,-9.827E-02,-7.119E-02,
     3 1.823E+00, 1.457E+00,-2.465E-01, 3.739E-02, 6.090E-03, 1.814E-02,
     4-1.033E+00,-9.827E-01, 2.136E-01, 1.169E-01, 5.001E-02, 1.684E-02,
     5 5.133E-01, 5.259E-01,-1.173E-01,-1.139E-01,-4.988E-02,-2.021E-02,
     6-2.881E-01,-3.145E-01, 5.667E-02, 9.161E-02, 4.568E-02, 1.951E-02,
     1 3.036E+01,-4.062E+01, 1.578E+01,-3.699E+00, 6.020E-01,-7.031E-02,
     2 2.700E+01,-4.167E+01, 1.770E+01,-4.804E+00, 7.862E-01,-1.060E-01,
     3-1.909E+00, 1.357E+00, 1.127E+00,-7.181E-01, 2.232E-01,-2.481E-02,
     4-2.488E-01, 9.781E-01,-8.127E-01, 2.094E-01,-2.997E-02,-4.710E-03,
     5 2.506E-01,-5.427E-01, 2.672E-01,-3.103E-02,-1.800E-03, 2.870E-03,
     6-1.128E-01, 2.087E-01,-6.972E-02,-2.480E-03, 2.630E-03,-8.400E-04/
C...EXPANSION COEFFICIENTS FOR STRANGE SEA QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,5,1),IX=1,6),IT=1,6),NX=1,2)/
     1 4.968E-02,-4.173E-02, 2.102E-02,-3.270E-03, 3.240E-03,-6.700E-04,
     2-6.150E-03,-1.294E-02, 6.740E-03,-6.890E-03, 9.000E-04,-1.510E-03,
     3-8.580E-03, 5.050E-03,-4.900E-03,-1.600E-04,-9.400E-04,-1.500E-04,
     4 7.840E-03, 1.510E-03, 2.220E-03, 1.400E-03, 7.000E-04, 3.500E-04,
     5-4.410E-03,-2.220E-03,-8.900E-04,-8.500E-04,-3.600E-04,-2.000E-04,
     6 2.520E-03, 1.840E-03, 4.100E-04, 3.900E-04, 1.600E-04, 9.000E-05,
     1 9.235E-01,-1.085E+00, 3.464E-01,-7.210E-02, 9.140E-03,-9.100E-04,
     2 9.315E-01,-1.274E+00, 4.512E-01,-9.775E-02, 1.380E-02,-1.310E-03,
     3 4.739E-02,-1.296E-01, 8.482E-02,-2.642E-02, 4.760E-03,-5.700E-04,
     4-2.653E-02, 4.953E-02,-1.735E-02, 1.750E-03, 2.800E-04,-6.000E-05,
     5 6.940E-03,-1.132E-02, 1.480E-03, 6.500E-04,-2.100E-04, 0.000E+00,
     6-1.680E-03, 2.340E-03, 4.200E-04,-3.400E-04, 5.000E-05, 1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,5,2),IX=1,6),IT=1,6),NX=1,2)/
     1 6.478E-02,-4.537E-02, 1.643E-02,-3.490E-03, 2.710E-03,-6.700E-04,
     2-2.223E-02,-2.126E-02, 1.247E-02,-6.290E-03, 1.120E-03,-1.440E-03,
     3-1.340E-03, 1.362E-02,-6.130E-03,-7.900E-04,-9.000E-04,-2.000E-04,
     4 5.080E-03,-3.610E-03, 1.700E-03, 1.830E-03, 6.800E-04, 4.000E-04,
     5-3.580E-03, 6.000E-05,-2.600E-04,-1.050E-03,-3.800E-04,-2.300E-04,
     6 2.420E-03, 9.300E-04,-1.000E-04, 4.500E-04, 1.700E-04, 1.100E-04,
     1 9.868E-01,-1.171E+00, 3.940E-01,-8.459E-02, 1.124E-02,-1.250E-03,
     2 1.001E+00,-1.383E+00, 5.044E-01,-1.152E-01, 1.658E-02,-1.830E-03,
     3 4.928E-02,-1.368E-01, 9.021E-02,-2.935E-02, 5.800E-03,-6.600E-04,
     4-3.133E-02, 5.785E-02,-2.023E-02, 2.630E-03, 1.600E-04,-8.000E-05,
     5 8.840E-03,-1.416E-02, 1.900E-03, 5.800E-04,-2.500E-04, 1.000E-05,
     6-2.300E-03, 3.080E-03, 5.500E-04,-3.700E-04, 7.000E-05, 1.000E-05/
C...EXPANSION COEFFICIENTS FOR CHARM SEA QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,6,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.270E-03,-1.817E-02, 9.590E-03,-6.390E-03, 1.690E-03,-1.540E-03,
     2 5.710E-03,-1.188E-02, 6.090E-03,-4.650E-03, 1.240E-03,-1.310E-03,
     3-3.960E-03, 7.100E-03,-3.590E-03, 1.840E-03,-3.900E-04, 3.400E-04,
     4 1.120E-03,-1.960E-03, 1.120E-03,-4.800E-04, 1.000E-04,-4.000E-05,
     5 4.000E-05,-3.000E-05,-1.800E-04, 9.000E-05,-5.000E-05,-2.000E-05,
     6-4.200E-04, 7.300E-04,-1.600E-04, 5.000E-05, 5.000E-05, 5.000E-05,
     1 8.098E-01,-1.042E+00, 3.398E-01,-6.824E-02, 8.760E-03,-9.000E-04,
     2 8.961E-01,-1.217E+00, 4.339E-01,-9.287E-02, 1.304E-02,-1.290E-03,
     3 3.058E-02,-1.040E-01, 7.604E-02,-2.415E-02, 4.600E-03,-5.000E-04,
     4-2.451E-02, 4.432E-02,-1.651E-02, 1.430E-03, 1.200E-04,-1.000E-04,
     5 1.122E-02,-1.457E-02, 2.680E-03, 5.800E-04,-1.200E-04, 3.000E-05,
     6-7.730E-03, 7.330E-03,-7.600E-04,-2.400E-04, 1.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,6,2),IX=1,6),IT=1,6),NX=1,2)/
     1 9.980E-03,-1.945E-02, 1.055E-02,-6.870E-03, 1.860E-03,-1.560E-03,
     2 5.700E-03,-1.203E-02, 6.250E-03,-4.860E-03, 1.310E-03,-1.370E-03,
     3-4.490E-03, 7.990E-03,-4.170E-03, 2.050E-03,-4.400E-04, 3.300E-04,
     4 1.470E-03,-2.480E-03, 1.460E-03,-5.700E-04, 1.200E-04,-1.000E-05,
     5-9.000E-05, 1.500E-04,-3.200E-04, 1.200E-04,-6.000E-05,-4.000E-05,
     6-4.200E-04, 7.600E-04,-1.400E-04, 4.000E-05, 7.000E-05, 5.000E-05,
     1 8.698E-01,-1.131E+00, 3.836E-01,-8.111E-02, 1.048E-02,-1.300E-03,
     2 9.626E-01,-1.321E+00, 4.854E-01,-1.091E-01, 1.583E-02,-1.700E-03,
     3 3.057E-02,-1.088E-01, 8.022E-02,-2.676E-02, 5.590E-03,-5.600E-04,
     4-2.845E-02, 5.164E-02,-1.918E-02, 2.210E-03,-4.000E-05,-1.500E-04,
     5 1.311E-02,-1.751E-02, 3.310E-03, 5.100E-04,-1.200E-04, 5.000E-05,
     6-8.590E-03, 8.380E-03,-9.200E-04,-2.600E-04, 1.000E-05,-1.000E-05/
C...EXPANSION COEFFICIENTS FOR BOTTOM SEA QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,7,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.010E-03,-1.401E-02, 7.150E-03,-4.130E-03, 1.260E-03,-1.040E-03,
     2 6.280E-03,-9.320E-03, 4.780E-03,-2.890E-03, 9.100E-04,-8.200E-04,
     3-2.930E-03, 4.090E-03,-1.890E-03, 7.600E-04,-2.300E-04, 1.400E-04,
     4 3.900E-04,-1.200E-03, 4.400E-04,-2.500E-04, 2.000E-05,-2.000E-05,
     5 2.600E-04, 1.400E-04,-8.000E-05, 1.000E-04, 1.000E-05, 1.000E-05,
     6-2.600E-04, 3.200E-04, 1.000E-05,-1.000E-05, 1.000E-05,-1.000E-05,
     1 8.029E-01,-1.075E+00, 3.792E-01,-7.843E-02, 1.007E-02,-1.090E-03,
     2 7.903E-01,-1.099E+00, 4.153E-01,-9.301E-02, 1.317E-02,-1.410E-03,
     3-1.704E-02,-1.130E-02, 2.882E-02,-1.341E-02, 3.040E-03,-3.600E-04,
     4-7.200E-04, 7.230E-03,-5.160E-03, 1.080E-03,-5.000E-05,-4.000E-05,
     5 3.050E-03,-4.610E-03, 1.660E-03,-1.300E-04,-1.000E-05, 1.000E-05,
     6-4.360E-03, 5.230E-03,-1.610E-03, 2.000E-04,-2.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,7,2),IX=1,6),IT=1,6),NX=1,2)/
     1 8.980E-03,-1.459E-02, 7.510E-03,-4.410E-03, 1.310E-03,-1.070E-03,
     2 5.970E-03,-9.440E-03, 4.800E-03,-3.020E-03, 9.100E-04,-8.500E-04,
     3-3.050E-03, 4.440E-03,-2.100E-03, 8.500E-04,-2.400E-04, 1.400E-04,
     4 5.300E-04,-1.300E-03, 5.600E-04,-2.700E-04, 3.000E-05,-2.000E-05,
     5 2.000E-04, 1.400E-04,-1.100E-04, 1.000E-04, 0.000E+00, 0.000E+00,
     6-2.600E-04, 3.200E-04, 0.000E+00,-3.000E-05, 1.000E-05,-1.000E-05,
     1 8.672E-01,-1.174E+00, 4.265E-01,-9.252E-02, 1.244E-02,-1.460E-03,
     2 8.500E-01,-1.194E+00, 4.630E-01,-1.083E-01, 1.614E-02,-1.830E-03,
     3-2.241E-02,-5.630E-03, 2.815E-02,-1.425E-02, 3.520E-03,-4.300E-04,
     4-7.300E-04, 8.030E-03,-5.780E-03, 1.380E-03,-1.300E-04,-4.000E-05,
     5 3.460E-03,-5.380E-03, 1.960E-03,-2.100E-04, 1.000E-05, 1.000E-05,
     6-4.850E-03, 5.950E-03,-1.890E-03, 2.600E-04,-3.000E-05, 0.000E+00/
C...EXPANSION COEFFICIENTS FOR TOP SEA QUARK DISTRIBUTION
      DATA (((CEHLQ(IX,IT,NX,8,1),IX=1,6),IT=1,6),NX=1,2)/
     1 4.410E-03,-7.480E-03, 3.770E-03,-2.580E-03, 7.300E-04,-7.100E-04,
     2 3.840E-03,-6.050E-03, 3.030E-03,-2.030E-03, 5.800E-04,-5.900E-04,
     3-8.800E-04, 1.660E-03,-7.500E-04, 4.700E-04,-1.000E-04, 1.000E-04,
     4-8.000E-05,-1.500E-04, 1.200E-04,-9.000E-05, 3.000E-05, 0.000E+00,
     5 1.300E-04,-2.200E-04,-2.000E-05,-2.000E-05,-2.000E-05,-2.000E-05,
     6-7.000E-05, 1.900E-04,-4.000E-05, 2.000E-05, 0.000E+00, 0.000E+00,
     1 6.623E-01,-9.248E-01, 3.519E-01,-7.930E-02, 1.110E-02,-1.180E-03,
     2 6.380E-01,-9.062E-01, 3.582E-01,-8.479E-02, 1.265E-02,-1.390E-03,
     3-2.581E-02, 2.125E-02, 4.190E-03,-4.980E-03, 1.490E-03,-2.100E-04,
     4 7.100E-04, 5.300E-04,-1.270E-03, 3.900E-04,-5.000E-05,-1.000E-05,
     5 3.850E-03,-5.060E-03, 1.860E-03,-3.500E-04, 4.000E-05, 0.000E+00,
     6-3.530E-03, 4.460E-03,-1.500E-03, 2.700E-04,-3.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,8,2),IX=1,6),IT=1,6),NX=1,2)/
     1 4.260E-03,-7.530E-03, 3.830E-03,-2.680E-03, 7.600E-04,-7.300E-04,
     2 3.640E-03,-6.050E-03, 3.030E-03,-2.090E-03, 5.900E-04,-6.000E-04,
     3-9.200E-04, 1.710E-03,-8.200E-04, 5.000E-04,-1.200E-04, 1.000E-04,
     4-5.000E-05,-1.600E-04, 1.300E-04,-9.000E-05, 3.000E-05, 0.000E+00,
     5 1.300E-04,-2.100E-04,-1.000E-05,-2.000E-05,-2.000E-05,-1.000E-05,
     6-8.000E-05, 1.800E-04,-5.000E-05, 2.000E-05, 0.000E+00, 0.000E+00,
     1 7.146E-01,-1.007E+00, 3.932E-01,-9.246E-02, 1.366E-02,-1.540E-03,
     2 6.856E-01,-9.828E-01, 3.977E-01,-9.795E-02, 1.540E-02,-1.790E-03,
     3-3.053E-02, 2.758E-02, 2.150E-03,-4.880E-03, 1.640E-03,-2.500E-04,
     4 9.200E-04, 4.200E-04,-1.340E-03, 4.600E-04,-8.000E-05,-1.000E-05,
     5 4.230E-03,-5.660E-03, 2.140E-03,-4.300E-04, 6.000E-05, 0.000E+00,
     6-3.890E-03, 5.000E-03,-1.740E-03, 3.300E-04,-4.000E-05, 0.000E+00/
      DATA TBMIN,TTMIN/8.1905,7.4474,11.5528,10.8097/
      DATA XOLD,QOLD,IOLD,NOLD/-1.,0.,0,0/
      DATA DMIN,Q0,QL/1.E-15,2*2.,2*2.236,.2,.4,.2,.29/
      DATA IXLOW,IQLOW,IQHIG/0,0,0/
      XMWN=1.-X
      QSCA=ABS(SCALE)
      ISET=MOD(NSET,100)
      IF (QSCA.LT.Q0(ISET)) THEN
        QSCA=Q0(ISET)
        IF(IQLOW.LE.100) THEN
          IQLOW=IQLOW+1
          CALL MWARN('DOEHLQ')
          IF(IQLOW.EQ.100) WRITE(*,*) ' LAST WARNING'
          WRITE(*,*) ' Q SCALE SMALLER THAN ALLOWED, SET TO MINIMUM'
          WRITE(*,*) '*********************************************'
        ENDIF
      ELSEIF (QSCA.GT.1.E4) THEN
        QSCA=Q0(ISET)
        IF(IQHIG.LE.100) THEN
          IQHIG=IQHIG+1
          CALL MWARN('DOEHLQ')
          IF(IQHIG.EQ.100) WRITE(*,*) ' LAST WARNING'
          WRITE(*,*) ' Q SCALE LARGER THAN ALLOWED, SET TO MAXIMUM'
          WRITE(*,*) '*********************************************'
        ENDIF
      ENDIF
      IF(X.LT.1.E-4) THEN
        IF(IXLOW.LE.100) THEN
          IXLOW=IXLOW+1
          CALL MWARN('DOEHLQ')
          IF(IXLOW.EQ.100) WRITE(*,*) ' LAST WARNING'
          WRITE(*,*) ' X VALUE SMALLER THAN ALLOWED (1.E-4)'
          WRITE(*,*) '*********************************************'
        ENDIF
      ENDIF
      IF (QSCA.NE.QOLD.OR.IDHAD.NE.IOLD.OR.NSET.NE.NOLD) THEN
        QOLD=QSCA
        IOLD=IDHAD
        NOLD=NSET
        SS=LOG(QSCA/QL(ISET))
        SMIN=LOG(Q0(ISET)/QL(ISET))
        IF (ISET.LT.3) THEN
          S=LOG(SS/SMIN)
        ELSEIF (ISET.LT.5) THEN
          T=2.*SS
          TMIN=2.*SMIN
          TMAX=2.*LOG(1.E4/QL(ISET))
        ENDIF
        GG=1.
C
        IF (ABS(IDHAD).LT.3) THEN
          IF (ISET.LT.3) THEN
C...........DUKE AND OWENS NUCLEONS
            IP=ISET
            DO 10 I=1,5
            DO 10 J=1,6
   10       A(J,I)=B(1,J,I,IP)+S*(B(2,J,I,IP)+S*B(3,J,I,IP))
            DO 20 K=1,2
            AA=1.+A(2,K)+A(3,K)
   20       G(K)=SPLGAM(AA)/((1.+A(2,K)*A(4,K)/AA)*SPLGAM(A(2,K))
     &          *SPLGAM(1.+A(3,K)))
          ELSE
C...........EHLQ NUCLEONS
            IP=ISET-2
            VT=MAX(-1.,MIN(1.,(2.*T-TMAX-TMIN)/(TMAX-TMIN)))
            WT=VT*VT
C...CHEBYSHEV POLYNOMIALS FOR T EXPANSION
            TT(1)=1.
            TT(2)=VT
            TT(3)=   2.*WT- 1.
            TT(4)=  (4.*WT- 3.)*VT
            TT(5)=  (8.*WT- 8.)*WT+1.
            TT(6)=((16.*WT-20.)*WT+5.)*VT
          ENDIF
        ELSEIF (ISET.LT.3) THEN
C...........DUKE AND OWENS PION
            IP=ISET+2
            DO 30 I=1,5
            DO 30 J=1,6
   30       A(J,I)=B(1,J,I,IP)+S*(B(2,J,I,IP)+S*B(3,J,I,IP))
            AA=1.+A(2,1)+A(3,1)
            G(1)=SPLGAM(AA)/(SPLGAM(A(2,1))*SPLGAM(1.+A(3,1)))
            G(2)=0.
        ENDIF
      ENDIF
C
      IF (ISET.LT.3) THEN
        DO 50 I=1,5
   50   F(I)=A(1,I)*X**A(2,I)*XMWN**A(3,I)*(1.+X*
     &      (A(4,I)+X*(A(5,I)  +  X*A(6,I))))
        F(1)=F(1)*G(1)
        F(2)=F(2)*G(2)
        UPV=F(1)-F(2)
        DNV=F(2)
        SEA=DMIN+F(3)/6.
        STR=SEA
        CHM=DMIN+F(4)
        BTM=DMIN
        GLU=DMIN+F(5)*GG
      ELSE
        IF (X.NE.XOLD) THEN
          XOLD=X
          IF (X.GT.0.1) THEN
            NX=1
            VX=(2.*X-1.1)/0.9
          ELSE
            NX=2
            VX=MAX(-1.,(2.*LOG(X)+11.51293)/6.90776)
          ENDIF
          WX=VX*VX
          TX(1)=1.
          TX(2)=VX
          TX(3)=   2.*WX- 1.
          TX(4)=  (4.*WX- 3.)*VX
          TX(5)=  (8.*WX- 8.)*WX+1.
          TX(6)=((16.*WX-20.)*WX+5.)*VX
        ENDIF
C...CALCULATE STRUCTURE FUNCTIONS
        DO 120 IFL=1,6
        XQSUM=0.
        DO 110 IT=1,6
        DO 110 IX=1,6
  110   XQSUM=XQSUM+CEHLQ(IX,IT,NX,IFL,IP)*TX(IX)*TT(IT)
  120   XQ(IFL)=XQSUM*XMWN**NEHLQ(IFL,IP)
        UPV=XQ(1)
        DNV=XQ(2)
        STR=DMIN+XQ(5)
        CHM=DMIN+XQ(6)
        SEA=DMIN+XQ(3)
        GLU=DMIN+XQ(4)*GG
C...SPECIAL EXPANSION FOR BOTTOM (THRESHOLD EFFECTS)
        IF (NF.LT.5.OR.T.LE.TBMIN(IP)) THEN
          BTM=DMIN
        ELSE
          VT=MAX(-1.,MIN(1.,(2.*T-TMAX-TBMIN(IP))/(TMAX-TBMIN(IP))))
          WT=VT*VT
          TB(1)=1.
          TB(2)=VT
          TB(3)=   2.*WT- 1.
          TB(4)=  (4.*WT- 3.)*VT
          TB(5)=  (8.*WT- 8.)*WT+1.
          TB(6)=((16.*WT-20.)*WT+5.)*VT
          XQSUM=0.
          DO 130 IT=1,6
          DO 130 IX=1,6
  130     XQSUM=XQSUM+CEHLQ(IX,IT,NX,7,IP)*TX(IX)*TB(IT)
          BTM=DMIN+XQSUM*XMWN**NEHLQ(7,IP)
        ENDIF
C...SPECIAL EXPANSION FOR TOP (THRESHOLD EFFECTS)
        TMTOP=2.*LOG(100./30.)
        TPMIN=TTMIN(IP)+TMTOP
C---TMTOP=2.*LOG(TOPMAS/30.)
        TPMAX=TMAX+TMTOP
        IF (NF.LT.6.OR.T.LE.TPMIN) THEN
          TOP=DMIN
        ELSE
          VT=MAX(-1.,MIN(1.,(2.*T-TPMAX-TPMIN)/(TPMAX-TPMIN)))
          WT=VT*VT
          TB(1)=1.
          TB(2)=VT
          TB(3)=   2.*WT- 1.
          TB(4)=  (4.*WT- 3.)*VT
          TB(5)=  (8.*WT- 8.)*WT+1.
          TB(6)=((16.*WT-20.)*WT+5.)*VT
          XQSUM=0.
          DO 150 IT=1,6
          DO 150 IX=1,6
  150     XQSUM=XQSUM+CEHLQ(IX,IT,NX,8,IP)*TX(IX)*TB(IT)
          TOP=DMIN+XQSUM*XMWN**NEHLQ(8,IP)
        ENDIF
      ENDIF
C
      IF (IDHAD.EQ.1) THEN
         DIST(1)=SEA+DNV
         DIST(2)=SEA+UPV
         DIST(7)=SEA
         DIST(8)=SEA
      ELSEIF (IDHAD.EQ.-1) THEN
         DIST(1)=SEA
         DIST(2)=SEA
         DIST(7)=SEA+DNV
         DIST(8)=SEA+UPV
      ELSEIF (IDHAD.EQ.2) THEN
         DIST(1)=SEA+UPV
         DIST(2)=SEA+DNV
         DIST(7)=SEA
         DIST(8)=SEA
      ELSEIF (IDHAD.EQ.-2) THEN
         DIST(1)=SEA
         DIST(2)=SEA
         DIST(7)=SEA+UPV
         DIST(8)=SEA+DNV
      ELSEIF (IDHAD.EQ.3) THEN
         DIST(1)=SEA
         DIST(2)=SEA+UPV
         DIST(7)=SEA+UPV
         DIST(8)=SEA
      ELSEIF (IDHAD.EQ.-3) THEN
         DIST(1)=SEA+UPV
         DIST(2)=SEA
         DIST(7)=SEA
         DIST(8)=SEA+UPV
      ENDIF
         DIST(3)=STR
         DIST(4)=CHM
         DIST(5)=BTM
         DIST(6)=TOP
         DIST(9)=STR
         DIST(10)=CHM
         DIST(11)=BTM
         DIST(12)=TOP
         DIST(13)=GLU
  999 END
C------------------------------------------------------------------------
      FUNCTION SPLGAM(ZINPUT)
      REAL Z,ZINPUT,G,T,RECZSQ
C
C   Gamma function computed by eq. 6.1.40, Abramowitz.
C   B(M) = B2m/(2m *(2m-1)) where B2m is the 2m'th Bernoulli number.
C   HLNTPI = .5*LOG(2.*PI)
C
      REAL B(10)
      DATA B/
     1       0.83333333333333333333E-01,   -0.27777777777777777778E-02,
     1       0.79365079365079365079E-03,   -0.59523809523809523810E-03,
     1       0.84175084175084175084E-03,   -0.19175269175269175269E-02,
     1       0.64102564102564102564E-02,   -0.29550653594771241830E-01,
     1       0.17964437236883057316E0  ,    -1.3924322169059011164E0  /
      DATA HLNTPI/0.91893853320467274178E0/
C
C   Shift argument to large value ( > 20 )
C
      Z=ZINPUT
      SHIFT=1.
   10 IF (Z.LT.20.E0) THEN
         SHIFT = SHIFT*Z
         Z = Z + 1.E0
         GO TO 10
      ENDIF
C
C   Compute asymptotic formula
C
      G = (Z-.5E0)*LOG(Z) - Z + HLNTPI
      T = 1.E0/Z
      RECZSQ = T**2
      DO 20 I = 1,10
         G = G + B(I)*T
         T = T*RECZSQ
   20 CONTINUE
      SPLGAM = EXP(G)/SHIFT
      END
C----- END DUKE-OWENS AND EHLQ -----------------
C-------------------------------------------------------------
C
C-------------------------------------------------------------
C----- START DFLM ------------------------------
      SUBROUTINE  DFLM(IFLAG,IH,Q2,X,FX,NF)
      DIMENSION FX(-NF:NF)
      CHARACTER*2 PART
      DIMENSION PART(-6:6)
      DATA PART/'TB','BB','CB','SB','DB','UB','GL',
     *       'UP','DO','SB','CB','BB','TB'/
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      DO I=-1,NF
        IF(IFLAG.EQ.1) CALL FXDFLM1(X,Q2,PART(I),FX(I*IH0))
        IF(IFLAG.EQ.2) CALL FXDFLM2(X,Q2,PART(I),FX(I*IH0))
        IF(IFLAG.EQ.3) CALL FXDFLM3(X,Q2,PART(I),FX(I*IH0))
      ENDDO
      SEA     =FX(-IH0)
      FX(-2*IH0)=SEA
      FX(IH0)  =FX(IH0)  +SEA
      FX(2*IH0)=FX(2*IH0)+SEA
      DO I=3,NF
        FX(-I*IH0)=FX(I*IH0)
      ENDDO
      DO I=-NF,NF
       FX(I)=FX(I)/X
      ENDDO
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END
C----- END DFLM -----------------
C------------------------------------------------------------
C
C------------------------------------------------------------
C----- START HMRS ------------------------------
      SUBROUTINE  HMRS(MODE,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF)
      REAL*8 DX,DQ,UPV,DOV,SEA,USEA,DSEA,STR,CHR,BOT,GLU
      REAL*8 XMIN,XMAX,QSQMIN,QSQMAX,QSQ
      REAL*8 IXMIN,IXMAX,IQSQMIN,IQSQMAX
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA INI/0/
      IF(INI.GT.0) GO TO 1
        IF(MODE.EQ.10)QSQMIN=0.625D0
        IF(MODE.GT.30.AND.MODE.LE.94) QSQMIN=1.25D0
        IF(MODE.GT.30.AND.MODE.LE.94) QSQMAX=1.D7
        ILXMIN=0                    
        ILXMAX=0
        ILQSQMIN=0
        ILQSQMAX=0
        INI=1
1     CONTINUE
      IF(ABS(IH).GE.3) CALL NOSETP       
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      IF(DX.LT.XMIN) THEN
        IXMIN=IXMIN+1.
        IF(LOG10(IXMIN).GT.ILXMIN) THEN
          WRITE(*,*)' X < XMIN IN STR. FUNCTIONS MORE THAN 10**',
     +  ILXMIN,' TIMES'                          
          ILXMIN=ILXMIN+1
        ENDIF
      ENDIF
      IF(DX.GT.XMAX) THEN
        IXMAX=IXMAX+1.
        IF(LOG10(IXMAX).GT.ILXMAX) THEN
          WRITE(*,*)' X > XMAX IN STR. FUNCTIONS MORE THAN 10**',
     +  ILXMAX,' TIMES'
          ILXMAX=ILXMAX+1
        ENDIF
      ENDIF
      QSQ=DQ**2
      IF(QSQ.LT.QSQMIN) THEN
        IQSQMIN=IQSQMIN+1.
        IF(LOG10(IQSQMIN).GT.ILQSQMIN) THEN
          WRITE(*,*)'Q**2 < MIN Q**2 IN STR. FUNCTIONS MORE THAN 10**',
     +  ILQSQMIN,' TIMES'
          ILQSQMIN=ILQSQMIN+1
        ENDIF
      ENDIF
      IF(QSQ.GT.QSQMAX) THEN
        IQSQMAX=IQSQMAX+1.
        IF(LOG10(IQSQMAX).GT.ILQSQMAX) THEN
          WRITE(*,*)'Q**2 > MAX Q**2 IN STR. FUNCTIONS MORE THAN 10**',
     +  ILQSQMAX,' TIMES'
          ILQSQMAX=ILQSQMAX+1
        ENDIF
      ENDIF
      IF(MODE.LT.7) THEN
        CALL MRSEB(DX,DQ,MODE,UPV,DOV,SEA,STR,CHR,BOT,GLU)
        FX(0)=SNGL(GLU)
        FX(-IH0)=SNGL(SEA)
        FX(-2*IH0)=SNGL(SEA)
        FX(IH0)  =SNGL(UPV+SEA)
        FX(2*IH0)=SNGL(DOV+SEA)
        IF(NF.GE.3) FX(3)=SNGL(STR)
        IF(NF.GE.4) FX(4)=SNGL(CHR)
        IF(NF.GE.5) FX(5)=SNGL(BOT)
        IF(NF.eq.6) FX(6)=0
      ELSEIF(MODE.LE.94) THEN
        IF(MODE.GT.63) THEN
           CALL MRST2001E
     #          (DX,DQ,MODE-64,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.61)THEN
           CALL MRST2002
     #          (DX,DQ,MODE-61,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.60)THEN
           CALL MRST2001lo
     #          (DX,DQ,MODE-60,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.56)THEN
           CALL MRST2001
     #          (DX,DQ,MODE-56,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.52)THEN
          CALL MRST0201127
     #              (DX,DQ,MODE-52,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.39)THEN
          CALL MRS99(DX,DQ,MODE-40,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.34)THEN
          CALL MRS98(DX,DQ,MODE-30,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.30)THEN
          CALL MRS96(DX,DQ,MODE-30,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.GT.12)THEN
          CALL MRSLAM(DX,DQ,MODE-12,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.EQ.12) THEN
          CALL STRC31(DX,DQ,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.EQ.11) THEN
          CALL MRSLAM(DX,DQ,MODE-11,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ELSEIF(MODE.EQ.10) THEN
          IF(Q2.GT.5D0) THEN
            CALL STRC33(DX,DQ,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
          ELSEIF(Q2.LE.5D0) THEN 
            CALL STRC34(DX,DQ,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
          ENDIF
        ELSE         
          CALL MRS92(DX,DQ,MODE,UPV,DOV,USEA,DSEA,STR,CHR,BOT,GLU)
        ENDIF
        FX(0)=SNGL(GLU)
        FX(-IH0)=SNGL(USEA)
        FX(-2*IH0)=SNGL(DSEA)
        FX(IH0)  =SNGL(UPV+USEA)
        FX(2*IH0)=SNGL(DOV+DSEA)
        IF(NF.GE.3) FX(3)=SNGL(STR)
        IF(NF.GE.4) FX(4)=SNGL(CHR)
        IF(NF.GE.5) FX(5)=SNGL(BOT)
        IF(NF.eq.6) FX(6)=0
      ENDIF
      DO I=3,NF
        FX(-I)=FX(I)
      ENDDO
      DO I=-NF,NF
       FX(I)=FX(I)/X
      ENDDO
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END
C
C
      SUBROUTINE XMRSA(Q2,X,FX,NF)
      IMPLICIT REAL*4(A-H,O-Z)
      REAL FX(-NF:NF)
      IMODE=10
      IIH=1
      R2=1.+4.*0.88*X*X/Q2
      R=SQRT(R2)
      XXI=2.*X/(1.+R)
      EMASS2=0.067*X**(-0.37)
      EMASS2=EMASS2*EXP(-Q2/5.)
      FACTOR=Q2/(Q2 + EMASS2)
      IF(FACTOR.LT.0.D0) FACTOR=0.D0
      IF(FACTOR.GT.1.D0) FACTOR=1.D0
      IF(Q2.LT.0.625) THEN
        Q2Z=SQRT(0.625D0)
      ELSE
        Q2Z=Q2
      ENDIF
      CALL HMRS(IMODE,IIH,Q2Z,XXI,FX,NF)
      DO I=-NF,NF
        FX(I)=FACTOR*FX(I)
      ENDDO
      RETURN
      END 
C
C
      SUBROUTINE MRSCHECK(VAL,MODE)
      REAL * 8 VAL
      CHARACTER * 10 NAME(47)
      DATA NAME/'HMRSB','KMRSB','HMRSB135','HMRSB160',
     #          'HMRSB200','HMRSB235','MRSS0','MRSD0','MRSD-',
     #          'MRSA','MRSAP','MRSG','MRS105','MRS110','MRS115',
     #          'MRS120','MRS125','MRS130',2*'EMPTY',
     #          'MRSR1','MRSR2','MRSR3','MRSR4',
     #          'MRST','MRSTH','MRSTL','MRSTM','MRSTP','EMPTY',
     #          'MRST991','MRST992','MRST993','MRST994',
     #          'MRST995','MRST996','MRST997','MRST998',
     #          'MRST999','MRST9910','MRST9911','MRST9912','EMPTY',
     #          'MRSTNNLO1','MRSTNNLO2','MRSTNNLO3','MRSTNNLO4'/
      IF(ABS(VAL-0.00232D0).LT.0.000001) THEN                      
         IMODE = 10
      ELSEIF(ABS(VAL-0.03058D0).LT.0.000001) THEN                      
         IMODE = 1
      ELSEIF(ABS(VAL-0.01727D0).LT.0.000001) THEN
         IMODE = 2
      ELSEIF(ABS(VAL-0.01683D0).LT.0.000001) THEN
         IMODE = 3
      ELSEIF(ABS(VAL-0.01663D0).LT.0.000001) THEN
         IMODE = 4
      ELSEIF(ABS(VAL-0.01601D0).LT.0.000001) THEN
         IMODE = 5
      ELSEIF(ABS(VAL-0.01571D0).LT.0.000001) THEN
         IMODE = 6
      ELSEIF(ABS(VAL-0.01356D0).LT.0.000001) THEN
         IMODE = 7
      ELSEIF(ABS(VAL-0.00527D0).LT.0.000001) THEN
         IMODE = 8
      ELSEIF(ABS(VAL-0.00474D0).LT.0.000001) THEN
         IMODE = 9
      ELSEIF(ABS(VAL-0.00383D0).LT.0.000001) THEN
         IMODE = 10         
      ELSEIF(ABS(VAL-0.00341D0).LT.0.000001) THEN                      
         IMODE = 11         
      ELSEIF(ABS(VAL-0.00269D0).LT.0.000001) THEN                      
         IMODE = 12         
      ELSEIF(ABS(VAL-0.00429D0).LT.0.000001) THEN                      
         IMODE = 13         
      ELSEIF(ABS(VAL-0.00350D0).LT.0.000001) THEN                      
         IMODE = 14         
      ELSEIF(ABS(VAL-0.00294D0).LT.0.000001) THEN                      
         IMODE = 15         
      ELSEIF(ABS(VAL-0.00273D0).LT.0.000001) THEN                      
         IMODE = 16         
      ELSEIF(ABS(VAL-0.00195D0).LT.0.000001) THEN                      
         IMODE = 17         
      ELSEIF(ABS(VAL-0.00145D0).LT.0.000001) THEN                      
         IMODE = 18         
c
      ELSEIF(ABS(VAL-0.00150D0).LT.0.000001) THEN                      
         IMODE = 21
      ELSEIF(ABS(VAL-0.00125D0).LT.0.000001) THEN                      
         IMODE = 22
      ELSEIF(ABS(VAL-0.00181D0).LT.0.000001) THEN                      
         IMODE = 23
      ELSEIF(ABS(VAL-0.00085D0).LT.0.000001) THEN                      
         IMODE = 24         
      ELSEIF(ABS(VAL-0.00561D0).LT.0.000001) THEN                      
         IMODE = 25
      ELSEIF(ABS(VAL-0.00510D0).LT.0.000001) THEN                      
         IMODE = 26
      ELSEIF(ABS(VAL-0.00408D0).LT.0.000001) THEN                      
         IMODE = 27
      ELSEIF(ABS(VAL-0.00586D0).LT.0.000001) THEN                      
         IMODE = 28
      ELSEIF(ABS(VAL-0.00410D0).LT.0.000001) THEN                      
         IMODE = 29          
c
      ELSEIF(ABS(VAL-0.00524D0).LT.0.000001) THEN                      
         IMODE = 31
      ELSEIF(ABS(VAL-0.00497D0).LT.0.000001) THEN                      
         IMODE = 32
      ELSEIF(ABS(VAL-0.00398D0).LT.0.000001) THEN                      
         IMODE = 33
      ELSEIF(ABS(VAL-0.00585D0).LT.0.000001) THEN                      
         IMODE = 34
      ELSEIF(ABS(VAL-0.00384D0).LT.0.000001) THEN                      
         IMODE = 35
      ELSEIF(ABS(VAL-0.00177D0).LT.0.000001) THEN                      
         IMODE = 36
      ELSEIF(ABS(VAL-0.00593D0).LT.0.000001) THEN                      
         IMODE = 37
      ELSEIF(ABS(VAL-0.00541D0).LT.0.000001) THEN                      
         IMODE = 38
      ELSEIF(ABS(VAL-0.91673D0).LT.0.000001) THEN                      
         IMODE = 39
      ELSEIF(ABS(VAL-0.00525D0).LT.0.000001) THEN                      
         IMODE = 40
      ELSEIF(ABS(VAL-0.89447D0).LT.0.000001) THEN                      
         IMODE = 41
      ELSEIF(ABS(VAL-0.00515D0).LT.0.000001) THEN                      
         IMODE = 42
      ELSEIF(ABS(VAL-0.00725D0).LT.0.000001) THEN                      
         IMODE = 44
      ELSEIF(ABS(VAL-0.00734D0).LT.0.000001) THEN                      
         IMODE = 45
      ELSEIF(ABS(VAL-0.00739D0).LT.0.000001) THEN                      
         IMODE = 46
      ELSEIF(ABS(VAL-0.00865D0).LT.0.000001) THEN                      
         IMODE = 47
      ELSE
         WRITE(*,*) ' MRSCHECK: ERROR,'
         WRITE(*,*) ' NO TABLE MATCHING THE ENTRY HAS BEEN FOUND'
         STOP
      ENDIF        
      IF(IMODE.NE.MODE) THEN
         WRITE(*,*) ' MRSCHECK: ERROR,'
         WRITE(*,*) ' MRSCHECK: MODE CORRESPONDS TO ',NAME(MODE)
         WRITE(*,*) ' MRSCHECK: TABLES ARE ',NAME(IMODE)
         STOP
      ENDIF
      WRITE(*,*)' MRSCHECK: MODE ',NAME(MODE)
      END

       SUBROUTINE MRSEB(X,SCALE,MODE,UPV,DNV,SEA,STR,CHM,BOT,GLU)
C***************************************************************C
C                                                               C
C                                                               C
C     NEW VERSIONS !!!! JANUARY 1990  (AS DESCRIBED IN          C
C     "PARTON DISTRIBUTIONS ... " P.N. HARRIMAN, A.D. MARTIN,   C
C     R.G. ROBERTS AND W.J. STIRLING PREPRINT DTP-90-04 )       C
C                                                               C
C         ********* DEBUGGED APRIL 1990********                 C
C                                                               C
C         ****** NOW DOWN TO X=10^-5 **********                 C
C                                                               C
C  MODE 1  CORRESPONDS TO HARRIMAN,                             C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)  WITH LAMBDA4= 190 MEV  C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C    THE FIRST NUMBER IN THE "E" GRID IS  .01969                C
C    THE FIRST NUMBER IN THE "B" GRID IS  .03058                C
C                                                               C
C                                                               C
C                         -*-                                   C
C                                                               C
C    (NOTE THAT X TIMES THE PARTON DISTRIBUTION FUNCTION        C
C    IS RETURNED I.E. G(X) = GLU/X ETC, AND THAT "SEA"          C
C    IS THE LIGHT QUARK SEA I.E. UBAR(X)=DBAR(X)=               C
C      SEA/X FOR A PROTON.  IF IN DOUBT, CHECK THE              C
C    MOMENTUM SUM RULE! NOTE ALSO THAT SCALE=Q IN GEV)          C
C                                                               C
C                         -*-                                   C
C                                                               C
C     (THE RANGE OF APPLICABILITY IS CURRENTLY:                 C
C     10**-5 < X < 1  AND  5 < Q**2 < 1.31 * 10**6              C
C     HIGHER Q**2 VALUES CAN BE SUPPLIED ON REQUEST             C
C     - PROBLEMS, COMMENTS ETC TO WJS@UK.AC.DUR.HEP             C
C                                                               C
C                                                               C
C***************************************************************C
C                                                               C
C     -----  VARIABLE LAMBDA AND GLUONS  ----                   C
C                                                               C
C     NEW VERSIONS !!!! OCTOBER 1990                            C
C     "........................ "  A.D. MARTIN,                 C
C     R.G. ROBERTS AND W.J. STIRLING PREPRINT DTP/90/76 (1990)  C
C        TO BE PUBLISHED IN PHYS  REV D 43 (1991)               C
C                                                               C
C  MODE 2 CORRESPONDS TO  KWIECINSKI,                           C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)                        C
C  WITH LAMBDA(4) = 190 MEV, ETAG = 5.10                        C
C  AND XG,XQ --> CONSTANT AS X--> 0 AT Q0**2   "B0 FIT"         C
C                                                               C
C  MODE 3 CORRESPONDS TO                                        C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)                        C
C  WITH LAMBDA(4) = 135 MEV, ETAG = 4.65                        C
C  AND XG,XQ --> CONSTANT AS X--> 0 AT Q0**2   "B135 FIT"       C
C                                                               C
C  MODE 4 CORRESPONDS TO                                        C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)                        C
C  WITH LAMBDA(4) = 160 MEV, ETAG = 4.25                        C
C  AND XG,XQ --> CONSTANT AS X--> 0 AT Q0**2   "B160 FIT"       C
C                                                               C
C  MODE 5 CORRESPONDS TO                                        C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)                        C
C  WITH LAMBDA(4) = 200 MEV, ETAG = 5.65                        C
C  AND XG,XQ --> CONSTANT AS X--> 0 AT Q0**2   "B200 FIT"       C
C                                                               C
C  MODE 6 CORRESPONDS TO                                        C
C  MARTIN, ROBERTS, STIRLING (BCDMS FIT)                        C
C  WITH LAMBDA(4) = 235 MEV, ETAG = 5.20                        C
C  AND XG,XQ --> CONSTANT AS X--> 0 AT Q0**2   "B235 FIT"       C
C                                                               C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C    THE FIRST NUMBER IN THE "B0"   GRID IS  .01727             C
C    THE FIRST NUMBER IN THE "B135" GRID IS  .01683             C
C    THE FIRST NUMBER IN THE "B160" GRID IS  .01663             C
C    THE FIRST NUMBER IN THE "B200" GRID IS  .01601             C
C    THE FIRST NUMBER IN THE "B235" GRID IS  .01571             C
C                                                               C
C                         -*-                                   C
C                                                               C
C    (NOTE THAT X TIMES THE PARTON DISTRIBUTION FUNCTION        C
C    IS RETURNED I.E. G(X) = GLU/X ETC, AND THAT "SEA"          C
C    IS THE LIGHT QUARK SEA I.E. UBAR(X)=DBAR(X)                C
C    = SEA/X FOR A PROTON.  IF IN DOUBT, CHECK THE              C
C    MOMENTUM SUM RULE! NOTE ALSO THAT SCALE=Q IN GEV)          C
C                                                               C
C                         -*-                                   C
C                                                               C
C     (THE RANGE OF APPLICABILITY IS CURRENTLY:                 C
C     10**-5 < X < 1  AND  5 < Q**2 < 1.31 * 10**6              C
C     HIGHER Q**2 VALUES CAN BE SUPPLIED ON REQUEST             C
C     - PROBLEMS, COMMENTS ETC TO WJS@UK.AC.DUR.HEP             C
C                                                               C
C                                                               C
C***************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(7,nx,19),G(7),XX(nx),XL(NX),N0(7)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,4,5,0,0,5/
      DATA INIT/0/,IMODE/0/
      DATA IQLOW,IXLOW/2*0/
      xsave=x  ! don't let x be altered if it's out of range!!

      IF(INIT.NE.0.AND.MODE.EQ.IMODE) GOTO 10
      INIT=1
      IMODE=MODE
      IF(MODE.EQ.1)
     .  OPEN(UNIT=27,FILE='HMRSB',STATUS='OLD')
      IF(MODE.EQ.2)
     .  OPEN(UNIT=27,FILE='KMRSB',STATUS='OLD')
      IF(MODE.EQ.3)        
     .  OPEN(UNIT=27,FILE='HMRSB135',STATUS='OLD')
      IF(MODE.EQ.4)
     .  OPEN(UNIT=27,FILE='HMRSB160',STATUS='OLD')
      IF(MODE.EQ.5)
     .  OPEN(UNIT=27,FILE='HMRSB200',STATUS='OLD')
      IF(MODE.EQ.6)
     .  OPEN(UNIT=27,FILE='HMRSB235',STATUS='OLD')
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(27,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M)
C 1=UV 2=DV 3=GLUE 4=(UBAR+DBAR)/2 5=CBAR 7=BBAR 6=SBAR
         DO 25 I=1,7
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      close(unit=27)
      CALL MRSCHECK(F(1,1,1),MODE)
      DO 26 J=NTENTH,NX
  26  XL(J) = XX(J)
      DO 30 J=1,ntenth-1
      XL(J)=DLOG10(XX(J))+1.1D0
      DO 30 I=1,6
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  50  FORMAT(7F10.5)
      DO 40 I=1,7
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XL(N+1)) GOTO 70
      A=(XXX-XL(N))/(XL(N+1)-XL(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,7
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      SEA=G(4) ! THIS SEA IS (UBAR+DBAR)/2
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
      x=xsave  !restore x
      RETURN
      END

      SUBROUTINE MRS92(X,SCALE,MODE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
C***************************************************************C
C                                                               C
C                                                               C
C     NEW VERSIONS:  APRIL  1992, MODE 1 IS THE 1990 KMRS(B0)   C
C     SET; MODES 2-4 ARE NEW SETS FITTED TO THE RECENT NMC      C
C     AND CCFR PRELIMINARY STRUCTURE FUNCTION DATA.             C
C     THE THREE NEW SETS HAVE LAMBDA(MSbar,NF=4) = 215 MeV      C
C                                                               C
C     THE REFERENCE IS: A.D. Martin, R.G. Roberts and           C
C     W.J. Stirling, University of Durham preprint DTP/92/16    C
C                                                               C
C        MODE 7 : MRS(S0) (updated B0, Lambda(4) = 215 MeV)      C
C        MODE 8 : MRS(D0) (... but with ubar not= dbar)          C
C        MODE 9 : MRS(D-) (updated B-, ubar not= dbar)           C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C    THE FIRST NUMBER IN THE "7" GRID IS 0.01356                C
C    THE FIRST NUMBER IN THE "8" GRID IS 0.00527                C
C    THE FIRST NUMBER IN THE "9" GRID IS 0.00474                C
C                                                               C
C    NOTE THE EXTRA ARGUMENT IN THIS SUBROUTINE MRSEB,          C
C    TO ALLOW FOR THE POSSIBILITY OF A *** DIFFERENT ***        C
C    UBAR AND DBAR SEA!                                         C
C                                                               C
C                         -*-                                   C
C                                                               C
C    (NOTE THAT X TIMES THE PARTON DISTRIBUTION FUNCTION        C
C    IS RETURNED I.E. G(X) = GLU/X ETC. IF IN DOUBT, CHECK THE  C
C    MOMENTUM SUM RULE! NOTE ALSO THAT SCALE=Q IN GEV)          C
C                                                               C
C                         -*-                                   C
C                                                               C
C     (THE RANGE OF APPLICABILITY IS CURRENTLY:                 C
C     10**-5 < X < 1  AND  5 < Q**2 < 1.31 * 10**6              C
C     HIGHER Q**2 VALUES CAN BE SUPPLIED ON REQUEST             C
C     - PROBLEMS, COMMENTS ETC TO WJS@UK.AC.DUR.HEP             C
C                                                               C
C                                                               C
C***************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47,ntenth=21,nq=20)
      DIMENSION F(8,NX,nq),G(8),XX(NX),XL(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,4,5,0,0,5,5/
      DATA INIT/0/
      save xx,n0,init
      xsave=x  ! don't let x be altered if it's out of range!!
      IF(INIT.NE.0.AND.MODE.EQ.IMODE) GOTO 10
      INIT=1
      IMODE=MODE
      if(mode.eq.7)then
        open(unit=1,file='MRSS0',status='old')
      elseif(mode.eq.8)then
        open(unit=1,file='MRSD0',status='old')
      elseif(mode.eq.9)then
        open(unit=1,file='MRSDS',status='old')
      endif
      DO 20 N=1,nx-1
      DO 20 M=1,nq-1
      READ(1,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      close(1)
      INIT=1
      CALL MRSCHECK(F(1,1,1),MODE)
      do j=ntenth,nx
         xl(j)=xx(j)
      enddo
      DO 31 J=1,NTENTH-1
      XL(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,nq-1
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  continue
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,nq-1
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XL(N+1)) GOTO 70
      A=(XXX-XL(N))/(XL(N+1)-XL(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      g(i)= (1.d0-a)*(1.d0-b)*f(i,n,m)+(1.d0-a)*b*f(i,n,m+1)
     .    + a*(1.d0-b)*f(i,n+1,m)  + a*b*f(i,n+1,m+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          fac=(1.d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
      x=xsave  !restore x
      RETURN
      END
C
      SUBROUTINE STRC31(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "G" FIT -- Feb 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      open(unit=31,file='MRSG',status='old')
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(31,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      close(31)
      CALL MRSCHECK(F(1,1,1),12)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC33(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
                 
C     THIS IS THE NEW  "A" FIT -- May 1994 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      open(unit=33,file='MRSA',status='old')
      DO 20 N=1,nx-1         
      DO 20 M=1,19
      READ(33,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      close(33)
      CALL MRSCHECK(F(1,1,1),10)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC34(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "A" FIT -- May 1994 -- low Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION	F(8,NX,8),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .	      1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .	      1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .	      1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .	   .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .	   .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .	   .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .	   .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,0.625D0,5.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/

      xsave=x  ! don't let x be	altered	if it's	out of range!!

      IF(INIT.NE.0) GOTO 10
      INIT=1
      open(unit=34,file='MRSA2',status='old')
      DO 20 N=1,nx-1
      DO 20 M=1,7
      READ(34,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .		F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR	6=SBAR 8=DBAR
	 DO 25 I=1,8
  25	 F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      close(34)
      CALL MRSCHECK(F(1,1,1),10)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7.or.i.eq.5) GO TO 31
      DO 30 K=1,7
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,7
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN)	QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX)	QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)*2D0
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .	  + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7.or.i.eq.5) GOTO 65
	  FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
	  G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)

      x=xsave  !restore	x

      RETURN
      END
C
      SUBROUTINE MRSLAM(X,SCALE,MODE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
C***************************************************************C
C								C
C MSBAR MSBAR MSBAR MSBAR MSBAR MSBAR MSBAR MSBAR MSBAR MSBAR   C
C								C
C     This is a package for the new MRS variable alpha_S parton C
C     distributions. The minimum Q^2  value is 5 GeV^2          C
C     and the x range is, as before,                            C
C     10^-5 < x < 1. MSbar factorization is used.               C
C     The package reads 7 grids, which are in  separate files.  C  
C     Note that x times the parton                              C
C     distribution is returned, Q is the scale in GeV.          C
C								C
C	MODE=0 for MRS(A')    (Lambda(4) = 0.231)               C
C	MODE=1 for MRS(105)   (Lambda(4) = 0.150)               C
C	MODE=2 for MRS(110)   (Lambda(4) = 0.201)               C
C	MODE=3 for MRS(115)   (Lambda(4) = 0.266)               C
C	MODE=4 for MRS(120)   (Lambda(4) = 0.344)               C
C	MODE=5 for MRS(125)   (Lambda(4) = 0.435)               C
C	MODE=6 for MRS(130)   (Lambda(4) = 0.542)               C
C								C
C         The reference is:                                     C
C         A.D. Martin, R.G. Roberts and W.J. Stirling,          C
C         Phys. Lett. B356 (1995) 89.                           C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C         THE FIRST NUMBER IN THE A'  GRID IS 0.00341           C
C         THE FIRST NUMBER IN THE 105 GRID IS 0.00429           C
C         THE FIRST NUMBER IN THE 110 GRID IS 0.00350           C
C         THE FIRST NUMBER IN THE 115 GRID IS 0.00294           C
C         THE FIRST NUMBER IN THE 120 GRID IS 0.00273           C
C         THE FIRST NUMBER IN THE 125 GRID IS 0.00195           C
C         THE FIRST NUMBER IN THE 130 GRID IS 0.00145           C
C								C
C***************************************************************C
      IMPLICIT REAL*8(A-H,O-Z)
      data init/0/
      IF(INIT.NE.0.AND.MODE.EQ.IMODE) GOTO 10
      INIT=1                          
      IMODE=MODE
      IF(MODE.EQ.0) then
        OPEN(UNIT=30,FILE='MRSAP',STATUS='OLD')
      ELSEIF(MODE.EQ.1) then
        OPEN(UNIT=55,FILE='MRS105',STATUS='OLD')
      ELSEIF(MODE.EQ.2) then      
        OPEN(UNIT=60,FILE='MRS110',STATUS='OLD')
      ELSEIF(MODE.EQ.3) then
        OPEN(UNIT=65,FILE='MRS115',STATUS='OLD')
      ELSEIF(MODE.EQ.4) then
        OPEN(UNIT=70,FILE='MRS120',STATUS='OLD')
      ELSEIF(MODE.EQ.5) then
        OPEN(UNIT=75,FILE='MRS125',STATUS='OLD')
      ELSEIF(MODE.EQ.6) then
        OPEN(UNIT=80,FILE='MRS130',STATUS='OLD')
      ELSE
         WRITE(*,*) ' MRSLAM: UNKNOWN MODE ',MODE
         STOP
      ENDIF
10    Q2=SCALE**2
      IF(MODE.EQ.0) 
     .   CALL STRC30(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.1) 
     .   CALL STRC105(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.2) 
     .   CALL STRC110(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.3) 
     .   CALL STRC115(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.4) 
     .   CALL STRC120(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.5) 
     .   CALL STRC125(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      IF(MODE.EQ.6) 
     .   CALL STRC130(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)
      RETURN
      END
C
      SUBROUTINE STRC30(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE NEW  "Aprime" FIT -- Feb 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(30,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),11)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC105(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.105 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(55,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),13)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C

      SUBROUTINE STRC110(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.110 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(60,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),14)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC115(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.115 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(65,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),15)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC120(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.120 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(70,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),16)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC125(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.125 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(75,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),17)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      SUBROUTINE STRC130(X,SCALE,UPV,DNV,USEA,DSEA,STR,CHM,BOT,GLU)

C     THIS IS THE alphas=0.130 FIT -- May 1995 -- standard Q^2 range

      IMPLICIT REAL*8(A-H,O-Z)
      parameter(nx=47)
      parameter(ntenth=21)
      DIMENSION F(8,NX,20),G(8),XX(NX),N0(8)
      DATA XX/1.d-5,2.d-5,4.d-5,6.d-5,8.d-5,
     .        1.D-4,2.D-4,4.D-4,6.D-4,8.D-4,
     .        1.D-3,2.D-3,4.D-3,6.D-3,8.D-3,
     .        1.D-2,2.D-2,4.D-2,6.D-2,8.D-2,
     .     .1D0,.125D0,.15D0,.175D0,.2D0,.225D0,.25D0,.275D0,
     .     .3D0,.325D0,.35D0,.375D0,.4D0,.425D0,.45D0,.475D0,
     .     .5D0,.525D0,.55D0,.575D0,.6D0,.65D0,.7D0,.75D0,
     .     .8D0,.9D0,1.D0/
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,5.D0,1310720.D0/
      DATA N0/2,5,5,9,0,0,9,9/
      DATA INIT/0/
 
 
      xsave=x
 
      IF(INIT.NE.0) GOTO 10
      INIT=1
      DO 20 N=1,nx-1
      DO 20 M=1,19
      READ(80,50)F(1,N,M),F(2,N,M),F(3,N,M),F(4,N,M),F(5,N,M),F(7,N,M),
     .          F(6,N,M),F(8,N,M)
C 1=UV 2=DV 3=GLUE 4=UBAR 5=CBAR 7=BBAR 6=SBAR 8=DBAR
         DO 25 I=1,8
  25     F(I,N,M)=F(I,N,M)/(1.D0-XX(N))**N0(I)
  20  CONTINUE
      CALL MRSCHECK(F(1,1,1),18)
      DO 31 J=1,NTENTH-1
      XX(J)=DLOG10(XX(J))+1.1D0
      DO 31 I=1,8
      IF(I.EQ.7) GO TO 31
      DO 30 K=1,19
  30  F(I,J,K)=DLOG(F(I,J,K))*F(I,ntenth,K)/DLOG(F(I,ntenth,K))
  31  CONTINUE
  50  FORMAT(8F10.5)
      DO 40 I=1,8
      DO 40 M=1,19
  40  F(I,nx,M)=0.D0
  10  CONTINUE
      IF(X.LT.XMIN) X=XMIN
      IF(X.GT.XMAX) X=XMAX
      QSQ=SCALE**2
      IF(QSQ.LT.QSQMIN) QSQ=QSQMIN
      IF(QSQ.GT.QSQMAX) QSQ=QSQMAX
      XXX=X
      IF(X.LT.1.D-1) XXX=DLOG10(X)+1.1D0
      N=0
  70  N=N+1
      IF(XXX.GT.XX(N+1)) GOTO 70
      A=(XXX-XX(N))/(XX(N+1)-XX(N))
      RM=DLOG(QSQ/QSQMIN)/DLOG(2.D0)
      B=RM-DINT(RM)
      M=1+IDINT(RM)
      DO 60 I=1,8
      G(I)= (1.D0-A)*(1.D0-B)*F(I,N,M)+(1.D0-A)*B*F(I,N,M+1)
     .    + A*(1.D0-B)*F(I,N+1,M)  + A*B*F(I,N+1,M+1)
      IF(N.GE.ntenth) GOTO 65
      IF(I.EQ.7) GOTO 65
          FAC=(1.D0-B)*F(I,ntenth,M)+B*F(I,ntenth,M+1)
          G(I)=FAC**(G(I)/FAC)
  65  CONTINUE
      G(I)=G(I)*(1.D0-X)**N0(I)
  60  CONTINUE
      UPV=G(1)
      DNV=G(2)
      USEA=G(4)
      DSEA=G(8)
      STR=G(6)
      CHM=G(5)
      GLU=G(3)
      BOT=G(7)
 
      x=xsave
 
      RETURN
      END
C
      subroutine mrs96(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C     This is a package for the new MRS(R1,R2,R3,R4) parton     C
C     distributions. There are several important changes from   C
C     earlier MRS packages:                                     C
C       -- the q**2 range is enlarged to 1.25d0 < q**2 < 1d7,   C
C          the x range is still 1d-5 < x < 1d0                  C
C       -- the interpolation routine has been slightly modified C
C       -- the call is now to mrs96() rather than to MRSEB()    C 
C     Note that the grid files which the program reads in       C
C     (mrsr1.dat,...) are now larger and more obviously named.  C
C      								C
C     As before, x times the parton distribution is returned,   C
C     q is the scale in GeV, MSbar factorization is assumed,    C
C     and Lambda(MSbar,nf=4) = 241 MeV for R1 (mode=1)          C
C                            = 344 MeV for R2 (mode=2)          C
C                            = 241 MeV for R3 (mode=3)          C
C                            = 344 MeV for R4 (mode=4)          C
C								C
C         The reference is:                                     C
C         A.D. Martin, R.G. Roberts and W.J. Stirling,          C
C         University of Durham preprint DTP/96/44 (1996)        C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C             >>>>>>>>  CROSS CHECK  <<<<<<<<                   C
C                                                               C
C         THE FIRST NUMBER IN THE R1 GRID IS 0.00150            C
C         THE FIRST NUMBER IN THE R2 GRID IS 0.00125            C
C         THE FIRST NUMBER IN THE R3 GRID IS 0.00181            C
C         THE FIRST NUMBER IN THE R4 GRID IS 0.00085            C
C								C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
      if(mode.eq.1) then
        call mrsr1(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrsr2(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrsr3(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrsr4(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu)
      endif 
      return
      end

      subroutine mrsr1(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save 
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='MRSR1',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),21)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue                       
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrsr2(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save   
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='MRSR2',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),22)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrsr3(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save   
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='MRSR3',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),23)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrsr4(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save   
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='MRSR4',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),24)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end

      subroutine mrs98(x,q,imode,upv,dnv,usea,dsea,str,chm,bot,glu)
C****************************************************************C
C								 C
C     This is a package for the new MRS 1998 parton              C
C     distributions. The format is similar to the previous       C
C     (1996) MRS-R series.                                       C
C								 C
C     As before, x times the parton distribution is returned,    C
C     q is the scale in GeV, MSbar factorization is assumed,     C
C     and Lambda(MSbar,nf=4) is given below for each set.        C
C								 C
C     TEMPORARY NAMING SCHEME:                                   C
C						                 C
C  mode  set    comment             L(4)/MeV  a_s(M_Z)  grid#1   C
C  ----  ---    -------             --------  -------   ------   C
C								 C
C  1     FT08A  central gluon, a_s    300      0.1175   0.00561  C
C  2     FT09A  higher gluon          300      0.1175   0.00510  C
C  3     FT11A  lower gluon           300      0.1175   0.00408  C
C  4     FT24A  lower a_s             229      0.1125   0.00586  C
C  5     FT23A  higher a_s            383      0.1225   0.00410  C
C						                 C
C						                 C
C      The corresponding grid files are called ft08a.dat etc.    C
C						    	  	 C
C      The reference is:                                         C
C      A.D. Martin, R.G. Roberts, W.J. Stirling, R.S Thorne      C
C      Univ. Durham preprint DTP/98/??, hep-ph/??????? (1998)    C
C                                                                C
C      Comments to : W.J.Stirling@durham.ac.uk                   C
C                                                                C
C								 C
C****************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
      mode=imode-4
      if(mode.eq.1) then
        call mrs981(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrs982(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrs983(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrs984(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.5) then
        call mrs985(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      endif 
      return
      end

      subroutine mrs981(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='ft08a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),25)
      do 31 j=1,ntenth-1       
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs982(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='ft09a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),26)
      do 31 j=1,ntenth-1       
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs983(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='ft11a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),27)
      do 31 j=1,ntenth-1       
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      
      subroutine mrs984(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='ft24a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),28)
      do 31 j=1,ntenth-1       
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs985(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='ft23a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),29)
      do 31 j=1,ntenth-1       
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c end of MRS98
c
      subroutine mrs99(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C****************************************************************C
C								 C
C     This is a package for the new **corrected** MRST parton    C
C     distributions. The format is similar to the previous       C
C     (1998) MRST series.                                        C
C								 C
C     NOTE: 7 new sets are added here, corresponding to shifting C
C     the small x HERA data up and down by 2.5%, and by varying  C
C     the charm and strange distributions, and by forcing a      C
C     larger d/u ratio at large x.                               C
C								 C
C     As before, x times the parton distribution is returned,    C
C     q is the scale in GeV, MSbar factorization is assumed,     C
C     and Lambda(MSbar,nf=4) is given below for each set.        C
C								 C
C     NAMING SCHEME:                                             C
C						                 C
C  mode  set    comment             L(4)/MeV  a_s(M_Z)  grid#1   C
C  ----  ---    -------             --------  -------   ------   C
C								 C
C  1     COR01  central gluon, a_s    300      0.1175   0.00524  C
C  2     COR02  higher gluon          300      0.1175   0.00497  C
C  3     COR03  lower gluon           300      0.1175   0.00398  C
C  4     COR04  lower a_s             229      0.1125   0.00585  C
C  5     COR05  higher a_s            383      0.1225   0.00384  C
C  6     COR06  quarks up             303.3    0.1178   0.00497  C
C  7     COR07  quarks down           290.3    0.1171   0.00593  C
C  8     COR08  strange up            300      0.1175   0.00524  C
C  9     COR09  strange down          300      0.1175   0.00524  C
C  10    C0R10  charm up              300      0.1175   0.00525  C
C  11    COR11  charm down            300      0.1175   0.00524  C
C  12    COR12  larger d/u            300      0.1175   0.00515  C
C						                 C
C      The corresponding grid files are called cor01.dat etc.    C
C							  	 C
C      The reference is:                                         C
C      A.D. Martin, R.G. Roberts, W.J. Stirling, R.S Thorne      C
C      Univ. Durham preprint DTP/99/64, hep-ph/9907231 (1999)    C
C                                                                C
C      Comments to : W.J.Stirling@durham.ac.uk                   C
C                                                                C
C								 C
C****************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99
c      if(x.lt.xmin.or.x.gt.xmax)       print 98
      if(mode.eq.1) then
        call mrs991(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrs992(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrs993(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrs994(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.5) then
        call mrs995(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.6) then
        call mrs996(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.7) then
        call mrs997(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.8) then
        call mrs998(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.9) then
        call mrs999(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.10) then
        call mrs9910(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.11) then
        call mrs9911(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.12) then
        call mrs9912(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ')
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ')
      return
      end

      subroutine mrs991(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor01',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),31)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs992(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor02',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),32)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs993(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor03',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),33)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs994(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor04',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),34)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs995(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor05',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),35)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs996(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor06',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(2,1,1),36)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs997(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor07',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),37)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs998(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor08',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,2),38)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs999(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor09',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(4,1,1)*(1d0-xx(1))**n0(4),39)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs9910(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor10',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),40)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs9911(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor11',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(4,1,1)*(1d0-xx(1))**n0(4),41)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
      
      subroutine mrs9912(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,ntenth=23,np=8)
      real*8 f(np,nx,nq+1),qq(nq),xx(nx),g(np),n0(np)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data n0/3,4,5,9,9,9,9,9/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=1,file='cor12',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(1,50)f(1,n,m),f(2,n,m),f(3,n,m),f(4,n,m),
     .		  f(5,n,m),f(7,n,m),f(6,n,m),f(8,n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
	do 25 i=1,np
  25	 f(i,n,m)=f(i,n,m)/(1d0-xx(n))**n0(i)
  20  continue
      call mrscheck(f(1,1,1),42)
      do 31 j=1,ntenth-1
      xx(j)=dlog10(xx(j)/xx(ntenth))+xx(ntenth)
      do 31 i=1,8
      if(i.eq.5.or.i.eq.7) goto 31
      do 30 k=1,nq
  30  f(i,j,k)=dlog10(f(i,j,k)/f(i,ntenth,k))+f(i,ntenth,k)
  31  continue
  50  format(8f10.5)
      do 40 i=1,np
      do 40 m=1,nq
  40  f(i,nx,m)=0d0
      init=1
      close(1)
  10  continue
      if(x.lt.xmin) x=xmin
      if(x.gt.xmax) x=xmax
      if(qsq.lt.qsqmin)	qsq=qsqmin
      if(qsq.gt.qsqmax)	qsq=qsqmax
      xxx=x
      if(x.lt.xx(ntenth)) xxx=dlog10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))
      do 60 i=1,np
      g(i)= (1d0-a)*(1d0-b)*f(i,n,m)   + (1d0-a)*b*f(i,n,m+1)
     .	  +       a*(1d0-b)*f(i,n+1,m) +       a*b*f(i,n+1,m+1)
      if(n.ge.ntenth) goto 65
      if(i.eq.5.or.i.eq.7) goto 65
	  fac=(1d0-b)*f(i,ntenth,m)+b*f(i,ntenth,m+1)
 	  g(i)=fac*10d0**(g(i)-fac)
  65  continue
      g(i)=g(i)*(1d0-x)**n0(i)
  60  continue
      upv=g(1)
      dnv=g(2)
      usea=g(4)
      dsea=g(8)
      str=g(6)
      chm=g(5)
      glu=g(3) 
      bot=g(7)
        x=xsave
        qsq=q2save
      return
      end
c end of MRS99      
C
C----- END HMRS -------------------------------------------------
C------------------------------------------------------------------
C
C--------------------------------------------------------------------
C----- START TUNG AND MORFIN ------------------------------
      SUBROUTINE  TUNG(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF)
      REAL*8 DX,DQ,PDXMT
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      DO I=-2,NF
        IF(I.GT.2) THEN
                FX(I)   =SNGL(PDXMT(ISET,I,DX,DQ,IRT))
                FX(-I)  =FX(I)
        ELSE
                FX(IH0*I)=SNGL(PDXMT(ISET,I,DX,DQ,IRT))
        ENDIF
      ENDDO
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END

C-------------------------------------------------------------
C                                                                 MAY 30 90
      FUNCTION PDXMT (ISET, IPARTON, X, Q, IRT)

C             For ISET = 1, 2 .. , returns sets of Parton Distributions
C             (in the proton) with parton label Iparton (6, 5, ...,0, ...-6)
C             for (t, b, c, s, d, u, g, u-bar, ... t-bar), and kinematic
C             variables (X, Q).   IRT is a return error code.
C
C     Iset =  1, 2, 3, 4 corresponds to the S1, B1, B2, and E1 fits of Morfin-
C             Tung (Fermilab-Pub-90/24, IIT-90/11) to NLO in the DIS scheme.
C
C             5 (Set S1M) corresponds to the same set as 1 (S1) but expressed
C             in the MS-bar scheme.
C
C             All the above sets assume a SU(3)-symmetric sea.

C             6 (Set S2) corresponds to a new set with input strange quark
C             distribution being 1/2 of the non-strange sea quarks
C             (as prefered by some expts).
C
C             7 (Set S2M) is the set S2 in the MS-bar scheme
C
C             8 is currently empty.

C             9 corresponds to a set of LO distributions suitable to be used
C             with LO hard scattering matrix elements.
C
C  The "lambda" parameter (4-flavors) for each parton distribution set can be
C  obtained by making the following FUNCTION call:
C             Alam = Vlambd (Iset, Iorder)
C  where Iset is the (input) set #, Iorder is the (output) order of the fit (1
C  for set 9, 2 for all the others), and Alam is the value of the effective QCD
C  lambda for 4 flavors.

C             Details about the 1 - 5 distributions are
C             given in the above-mentioned preprint.
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      DIMENSION  THRSLD(0:6)

      DATA (THRSLD(I), I=0,6) / 4*0.0, 1.5, 5.0, 90.0 /

      IFL = IPARTON
      JFL = ABS(IFL)
C                                                   Return 0 if below threshold
      IF (Q .LE. THRSLD(JFL)) THEN
        PDXMT = 0.0
        RETURN
      ENDIF
C                                                                       Valence
      IF (IFL .LE. 0) THEN
        VL = 0
      ELSEIF (IFL .LE. 2) THEN
        VL = PDZXMT(ISET, IFL, X, Q, IRT)
      ELSE
        VL = 0
      ENDIF
C                                                                         Sea
      SEA = PDZXMT (ISET, -JFL, X, Q, IRT)

      PDXMT = VL + SEA

      RETURN
C                         *************************
      END

      FUNCTION PDZXMT (IST, LP, XX, QQ, IRT)

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      PARAMETER (D0=0D0, D1=1D0, D2=2D0, D3=3D0, D4=4D0, D10=1D1)
      PARAMETER (NEX = 3, MXFL = 6, NPN = 2, NST = 10)

      DIMENSION
     1 AC(0:NEX, 0:NPN, -MXFL:2, NST), A(0:NEX), T(0:NPN), FX(0:NEX),
     1 ALM(NST), Q02(NST), MEX(NST), MPN(NST), MQRK(NST), Iord(NST)

      DATA MEX, MPN, MQRK / NST*3, NST*2, NST*6 /
C                                          Set S-1:    PDF parameters from /L352
      DATA IORD(1), ALM(1), Q02(1) / 2, 0.212d0, 4.00d0 /
     > (((AC(IEX,IPN,IFL,1), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.34,  0.15,
     >    5.30, -1.96, -0.57,  0.16,  0.43,  1.08, -0.08, -0.02,
     >    0.06, -0.03,  1.62,  0.11,  3.68, -1.94, -0.33,  0.14,
     >    0.53,  0.87, -0.10, -0.01,  0.03,  0.02,  1.88, -0.33,
     >    7.52, -1.34, -2.78,  0.10, -1.13,  2.92,  0.13, -0.04,
     >    0.04, -0.49, -0.99, -0.33,  8.53, -1.55, -1.54,  0.03,
     >   -1.08,  2.02,  0.10, -0.03,  0.39, -0.39, -0.99, -0.33,
     >    8.53, -1.55, -1.54,  0.03, -1.08,  2.02,  0.10, -0.03,
     >    0.39, -0.39, -0.99, -0.33,  8.53, -1.55, -1.54,  0.03,
     >   -1.08,  2.02,  0.10, -0.03,  0.39, -0.39, -3.98, -0.15,
     >    7.46,  0.35,  0.72, -0.06,  0.96,  0.89, -0.63,  0.00,
     >   -0.30, -0.04, -6.28, -0.18,  6.56,  0.65,  2.62,  0.02,
     >    1.40,  1.13, -1.18, -0.03, -0.38, -0.16,-13.08, -0.40,
     >   15.35, -0.43,  8.54,  0.31,-11.83,  3.18, -2.70, -0.12,
     >    4.16, -0.82 /
C                                           Set B1:    PDF parameters from /L212
      DATA IORD(2), ALM(2), Q02(2) / 2, 0.194, 4.00 /
     > (((AC(IEX,IPN,IFL,2), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.30,  0.19,
     >    5.24, -1.81, -0.57,  0.15,  0.44,  1.06, -0.09, -0.02,
     >    0.05, -0.02,  1.59,  0.14,  3.65, -1.81, -0.34,  0.13,
     >    0.53,  0.86, -0.10, -0.01,  0.03,  0.02,  1.48, -0.14,
     >    6.75, -0.50, -2.49, -0.11, -0.54,  2.13,  0.04,  0.03,
     >   -0.15, -0.24, -1.08, -0.13,  8.40, -0.88, -1.33, -0.21,
     >   -0.51,  1.18, -0.03,  0.06,  0.07, -0.05, -1.08, -0.13,
     >    8.39, -0.88, -1.33, -0.21, -0.50,  1.18, -0.03,  0.06,
     >    0.07, -0.05, -1.08, -0.13,  8.39, -0.88, -1.33, -0.21,
     >   -0.50,  1.18, -0.03,  0.06,  0.07, -0.05, -4.22, -0.02,
     >    7.29,  0.90,  0.88, -0.17,  1.08,  0.50, -0.69,  0.03,
     >   -0.39,  0.08, -6.42, -0.09,  6.47,  1.03,  2.67, -0.03,
     >    1.39,  1.00, -1.21, -0.02, -0.42, -0.14,-12.92, -0.36,
     >   15.74, -0.30,  8.33,  0.32,-12.73,  3.35, -2.68, -0.13,
     >    4.51, -0.91 /
C                                             Set B2:  PDF parameters from /L261
      DATA IORD(3), ALM(3), Q02(3) / 2, 0.191, 4.00 /
     > (((AC(IEX,IPN,IFL,3), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.38,  0.18,
     >    5.40, -1.91, -0.59,  0.16,  0.42,  1.11, -0.08, -0.02,
     >    0.06, -0.03,  1.64,  0.09,  3.74, -2.02, -0.33,  0.14,
     >    0.54,  0.88, -0.10, -0.01,  0.03,  0.02,  1.52, -0.72,
     >    7.75, -2.18, -2.71,  0.45, -1.56,  3.75,  0.15, -0.15,
     >    0.16, -0.76, -0.85, -0.82,  9.19, -2.76, -1.43,  0.35,
     >   -0.92,  2.56, -0.03, -0.09,  0.12, -0.40, -0.85, -0.82,
     >    9.19, -2.76, -1.43,  0.35, -0.92,  2.56, -0.03, -0.10,
     >    0.12, -0.40, -0.85, -0.82,  9.19, -2.76, -1.43,  0.35,
     >   -0.92,  2.56, -0.03, -0.10,  0.12, -0.40, -3.74, -0.58,
     >    9.63, -1.09,  0.21,  0.24, -1.13,  2.10, -0.50, -0.07,
     >    0.25, -0.33, -6.07, -0.52,  8.33, -0.52,  2.33,  0.22,
     >    0.28,  1.91, -1.15, -0.07, -0.28, -0.31,-12.08, -0.73,
     >   21.14, -1.92,  7.31,  0.54,-19.17,  4.59, -2.35, -0.18,
     >    6.64, -1.25 /
C                                             Set E1:  PDF parameters from /L152
      DATA IORD(4), ALM(4), Q02(4) / 2, 0.155, 4.00 /
     > (((AC(IEX,IPN,IFL,4), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.43,  0.16,
     >    6.17, -1.94, -0.65,  0.16,  0.43,  1.12, -0.08, -0.02,
     >    0.06, -0.02,  1.69,  0.11,  3.69, -1.99, -0.33,  0.14,
     >    0.54,  0.90, -0.11, -0.01,  0.03,  0.02,  2.11, -0.33,
     >    7.93, -1.51, -3.01,  0.10, -1.40,  3.14,  0.18, -0.04,
     >    0.09, -0.55, -0.84, -0.32,  8.96, -1.70, -1.65,  0.02,
     >   -1.24,  2.15,  0.12, -0.03,  0.45, -0.43, -0.84, -0.32,
     >    8.96, -1.70, -1.65,  0.02, -1.24,  2.15,  0.12, -0.03,
     >    0.45, -0.43, -0.84, -0.32,  8.96, -1.70, -1.65,  0.02,
     >   -1.24,  2.15,  0.12, -0.03,  0.45, -0.43, -3.87, -0.15,
     >    7.83,  0.21,  0.85, -0.07,  1.00,  0.93, -0.73,  0.00,
     >   -0.36, -0.03, -6.09, -0.17,  6.75,  0.54,  2.81,  0.01,
     >    1.74,  1.15, -1.34, -0.03, -0.56, -0.16,-12.56, -0.38,
     >   14.62, -0.41,  8.69,  0.30,-11.27,  3.19, -2.93, -0.12,
     >    4.29, -0.87 /
C                               Set S1M:  PDF parameters from /L352 -- MS-Bar
      DATA IORD(5), ALM(5), Q02(5) / 2, 0.212, 4.00 /
     > (((AC(IEX,IPN,IFL,5), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.75,  0.11,
     >    6.20, -2.35, -1.02,  0.26, -0.41,  1.68,  0.05, -0.06,
     >    0.29, -0.24,  2.03,  0.06,  4.43, -2.35, -0.78,  0.24,
     >   -0.18,  1.52,  0.03, -0.04,  0.22, -0.19,  1.09, -0.24,
     >    5.97, -0.64, -2.41,  0.08, -0.90,  2.71, -0.12,  0.02,
     >   -0.35, -0.20, -0.14, -0.49, 10.24, -2.57, -1.98,  0.02,
     >   -1.43,  2.32,  0.23, -0.02,  0.44, -0.47, -0.14, -0.49,
     >   10.24, -2.57, -1.98,  0.02, -1.44,  2.32,  0.23, -0.02,
     >    0.45, -0.47, -0.15, -0.49, 10.23, -2.57, -1.98,  0.02,
     >   -1.44,  2.32,  0.23, -0.02,  0.45, -0.47, -2.36, -0.49,
     >    9.00, -1.74, -1.42,  0.44, -0.46,  3.93,  0.21, -0.22,
     >    0.29, -1.34, -2.19, -1.07, 11.30, -4.85, -3.86,  1.56,
     >   -7.20, 10.51,  1.57, -0.73,  3.85, -4.36,-24.77,  7.52,
     >  -99.51, 36.02,-23.00,  0.48,-16.45, 16.51, 34.44, -6.26,
     >   97.19,-40.40 /
C                         Set S2 -- 1/2 strange sea;  PDF parameters from /L405

      DATA IORD(6), ALM(6), Q02(6) / 2, 0.237, 4.00 /
     > (((AC(IEX,IPN,IFL,6), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.42,  0.16,
     >    5.40, -1.99, -0.59,  0.17,  0.41,  1.12, -0.08, -0.02,
     >    0.06, -0.03,  1.68,  0.08,  3.75, -2.09, -0.33,  0.15,
     >    0.53,  0.89, -0.10, -0.01,  0.03,  0.02,  0.90, -0.17,
     >    5.27, -0.20, -1.86, -0.10,  0.43,  1.67, -0.09,  0.02,
     >   -0.26, -0.14, -1.48, -0.13,  7.83, -0.38, -0.89, -0.19,
     >   -0.06,  0.68, -0.12,  0.04,  0.01,  0.05, -1.48, -0.13,
     >    7.83, -0.38, -0.89, -0.19, -0.05,  0.68, -0.13,  0.04,
     >    0.00,  0.05, -2.26, -0.15,  7.47, -0.23, -0.90, -0.10,
     >   -0.61,  1.22, -0.06,  0.01,  0.28, -0.16, -4.68, -0.06,
     >    5.55,  1.13,  0.92, -0.12,  1.16,  0.50, -0.62,  0.01,
     >   -0.26,  0.03, -6.83, -0.12,  5.24,  1.19,  2.68, -0.01,
     >    1.14,  0.93, -1.13, -0.03, -0.24, -0.13,-14.41, -0.28,
     >   11.48,  0.65,  9.65,  0.15, -7.50,  1.99, -2.98, -0.06,
     >    2.54, -0.43 /
C                            Set-S2M: PDF parameters from /L405 FILE   -- MS-BAR
      DATA IORD(7), ALM(7), Q02(7) / 2, 0.237, 4.00 /
     > (((AC(IEX,IPN,IFL,7), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.84,  0.12,
     >    6.34, -2.40, -0.97,  0.22, -0.34,  1.53,  0.03, -0.04,
     >    0.25, -0.16,  2.08,  0.02,  4.53, -2.51, -0.66,  0.19,
     >   -0.04,  1.24, -0.02, -0.01,  0.15, -0.05,  0.31, -0.10,
     >    4.18,  0.34, -1.84, -0.10,  0.05,  1.64, -0.06,  0.01,
     >   -0.12, -0.16, -1.13, -0.15,  8.43, -0.64, -1.26, -0.16,
     >   -0.39,  1.01, -0.01,  0.03,  0.05, -0.06, -1.13, -0.15,
     >    8.43, -0.64, -1.26, -0.16, -0.39,  1.01, -0.01,  0.03,
     >    0.05, -0.06, -1.82, -0.18,  7.94, -0.56, -1.40, -0.06,
     >   -0.82,  1.65,  0.09, -0.01,  0.30, -0.31, -3.69, -0.15,
     >    5.72,  0.26, -0.47,  0.04,  0.93,  1.85, -0.10, -0.05,
     >   -0.11, -0.50, -5.06, -0.25,  4.42, -0.14,  0.39,  0.16,
     >    2.38,  2.72, -0.35, -0.08, -0.63, -0.75, -9.92, -0.38,
     >   -1.27, -1.60,  4.60,  0.24,  9.17,  4.40, -1.53, -0.08,
     >   -2.88, -1.08 /
C                                             Set B0:  PDF parameters from /P154
      DATA IORD(8), ALM(8), Q02(8) / 1, 0.144, 4.00 /
     > (((AC(IEX,IPN,IFL,8), IEX=0,3), IPN=0,2), IFL=2,-6,-1)
     > /  1.38,  0.16,
     >    5.40, -1.97, -0.62,  0.19,  0.59,  1.24, -0.10, -0.02,
     >    0.03, -0.05,  1.67,  0.08,  3.75, -2.09, -0.33,  0.17,
     >    0.70,  0.98, -0.13, -0.01,  0.00,  0.02,  1.52, -0.25,
     >    7.01, -0.79, -3.17, -0.01, -0.90,  2.90,  0.25,  0.00,
     >   -0.08, -0.54, -0.81, -0.07,  9.19, -0.89, -1.13, -0.46,
     >    0.35,  0.33, -0.26,  0.16, -0.49,  0.40, -0.81, -0.07,
     >    9.19, -0.89, -1.13, -0.46,  0.35,  0.33, -0.26,  0.16,
     >   -0.49,  0.40, -0.81, -0.07,  9.19, -0.89, -1.13, -0.46,
     >    0.35,  0.33, -0.26,  0.16, -0.49,  0.40, -3.62, -0.06,
     >    8.30,  0.16,  0.03, -0.21, -0.60,  1.26, -0.48,  0.05,
     >    0.25, -0.15, -6.16, -0.11,  6.49,  0.71,  2.37, -0.05,
     >    1.28,  1.37, -1.24, -0.02, -0.41, -0.26,-12.68, -0.35,
     >   14.87, -0.17,  8.36,  0.28,-12.56,  3.39, -2.89, -0.12,
     >    4.75, -0.96 /

      IRT = 0
      IFL = LP

      X  = XX
      Q0 = SQRT (Q02(IST))
      ALAM = ALM(IST)
      SQ = LOG ( LOG(QQ/ALAM) / LOG(Q0/ALAM) )

      FX(0) = EXP(D1)
      FX(1) = X
      FX(2) = 1.- X
      FX(3) = LOG (1.+ 1./X)

      PDF = 1.
      DO 20 IEX = 0, MEX(IST)
        A(IEX) = AC(IEX, 0, IFL, IST)
        DO 21 IPN = 1, MPN(IST)
          A(IEX) = A(IEX) + AC(IEX, IPN, IFL, IST) * SQ **IPN
21      CONTINUE
        PDF = PDF * FX(IEX) **(A(IEX))
   20 CONTINUE

      PDZXMT = PDF / X

      RETURN

      ENTRY VLAMBD (ISET, IORDER)

      IORDER = IORD (ISET)
      VLAMBD = ALM  (ISET)

      RETURN
C                         *************************
      END


C------- END TUNG AND MORFIN ---------------------------------------
C----- START CTEQ1 FITS ------------------------------
      SUBROUTINE  CTEQ(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF) 
      REAL*8 DX,DQ,PDF(-6:2)
C     Pdf(Iprtn), Iprtn = (2,    1,   0,   -1,  -2, ......,    -6)
C                  for (d_val, u_val, g, u_bar, d_bar,  ..., t_bar)
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      CALL CTQPDS(ISET,PDF,DX,DQ,IRT)
c 
      FX(0)=SNGL(PDF(0))
      FX(-IH0)=SNGL(PDF(-1))
      FX(-2*IH0)=SNGL(PDF(-2))
      FX(IH0)  =SNGL(PDF(1)+PDF(-1))
      FX(2*IH0)=SNGL(PDF(2)+PDF(-2))
      IF(NF.GE.3) FX(3)=SNGL(PDF(-3))
      IF(NF.GE.4) FX(4)=SNGL(PDF(-4))
      IF(NF.GE.5) FX(5)=SNGL(PDF(-5))
      IF(NF.eq.6) FX(6)=0
      DO I=3,NF
        FX(-I)=FX(I)
      ENDDO
      DO I=-NF,NF
       FX(I)=FX(I)/X
      ENDDO
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END
C            
      Subroutine CtqPds (Iset, Pdf, XX, QQ, Irt)

C   CTEQ distribution function in a parametrized form.  

C   (No data tables are needed.)

C   The returned function values (in the array Pdf) are the 
C   MOMENTUM FRACTION densities: 

C    Pdf(Iprtn), Iprtn = (2,    1,   0,   -1,  -2, ......,    -6)
C                 for (d_val, u_val, g, u_bar, d_bar,  ..., t_bar)

C  !!! Be aware of our numbering scheme when you declare the dimension
C  !!! of this array in the calling program!!... In particular,
C  !!! the ascending/descending order!! 

C  \\  A parallel (independent) program (not included in this file) in
C  ||  Function form is also available.  There, the function CteqPd returns
C  ||   d, u, g, u_bar, ... etc. INDIVIDUALLY by a parton label parameter;
C  ||  and the function CtqPdf returns d_val, u_val, ... etc. as above.
C  //  See details in that separate file if you are interested.

C Ref.: "CTEQ Parton Distributions and Flavor Dependence of the Sea Quarks"
C     by: J. Botts, J.G. Morfin, J.F. Owens, J. Qiu, W.K. Tung & H. Weerts
C     MSUHEP-92-27, Fermilab-Pub-92/371, FSU-HEP-92-1225, ISU-NP-92-17

C   Since this is an initial distribution, and there may be updates, it is 
C   useful for the authors to maintain a record of the distribution list.
C   Please do not freely distribute this program package; instead, refer any 
C   interested colleague to direct their request for a copy to:
C   Botts@msupa.pa.msu.edu  or  Botts@msupa (bitnet)  or  MSUHEP::Botts

C   If you have any questions concerning these distributions, direct inquires 
C   to Jim Botts or Wu-Ki Tung (username Tung at same E-mail nodes as above).

C$Header: /users/wkt/1hep/0cteq/RCS/CtqPr1B.f,v 1.1 93/02/16 13:09:52 wkt Exp $ 
C$Log:	CtqPr1B.f,v $
c Revision 1.1  93/02/16  13:09:52  wkt
c Initial revision
c 
c Revision 1.2  93/02/14  17:30:21  botts
c The new Faster version.
c Revision 1.1  93/02/08  18:35:25  wkt
c Initial revision

C     Name convention for CTEQ distributions:  CTEQnSx  where
C         n : version number                      (currently n = 1)
C         S : factorization scheme label: = [M D L] for [MS-bar DIS LO]  
C             resp.
C         x : special characteristics, if any 
C                  (e.g. S for singular gluon, L for "LEP lambda value")

C Xx, Qq are the usual x, Q; Irt is a return error code (not implemented).

C --> Iset = 1, 2, 3, 4, 5 correspond to the following CTEQ global fits:
C     cteq1M, cteq1MS, cteq1ML, cteq1D, cteq1L  respectively.

C --> QCD parameters for parton distribution set Iset can be obtained inside
C         the user's program by:
C     Call Prctq2 
C    >        (Iset, Iord, Ischeme, MxFlv,
C    >         Alam4, Alam5, Alam6, Amas4, Amas5, Amas6,
C    >         Xmin, Qini, Qmax, ExpNor)
C     where all but the first argument are output parameters.
C     They should be self-explanary -- see details in next module.

C     The range of (x, Q) used in this round of global analysis is, approxi-
C     mately,  0.01 < x < 0.75 ; and 4 GeV^2 < Q^2 < 400 GeV^2.

C    The range of (x, Q) used in the reparametrization of the QCD evolved
C    parton distributions is 10E-5 < x < 1 ; 2 GeV < Q < 1 TeV.  The  
c    functional form of this parametrization is:

C      A0 * x^A1 * (1-x)^A2 * (1 + A3 * x^A4) * [log(1+1/x)]^A5

C     with the A'coefficients being smooth functions of Q.  

C    Since this function is positive definite and smooth, it provides sensible
C     extrapolations of the parton distributions if they are called beyond
C     the original range in an application. There is no artificial boundaries
C     or sharp cutoff's.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
      dimension pdf(-6:2)

      PARAMETER (D0=0D0, D1=1D0, D2=2D0, D3=3D0, D4=4D0, D10=1D1)
      PARAMETER (Nex = 5, MxFl = 6, Npn = 3, Nst = 30, Nexpt=20)
      Parameter (Nst4 = Nst*4)

      DIMENSION
     >   Iord(Nst), Isch(Nst), Nqrk(Nst),Alm(Nst)
     > , Vlm(4:6,Nst), Qms(4:6, Nst)
     > , Xmn(Nst), Qmn(Nst), Qmx(Nst), Nexp(Nexpt)
     > , Mex(Nst), Mpn(Nst), ExpN(Nexpt, Nst), ExpNor(Nexpt)

c                                                             CTEQ1M
      DATA 
     >  Isch(1), Iord(1), Nqrk(1), Alm(1) /  1,  2,  6,  .152 / 
     >  (Vlm(I,1), I=4,6) / .231,    .152,    .059  /
     >  (Qms(I,1), I=4,6) / 1.50,   5.00,  180.0 /
     >  Xmn(1), Qmn(1), Qmx(1) /  1.E-5,  2.00,  1.E3  /
     >  Mex(1), Mpn(1), Nexp(1) /  5, 3, 8  /
     >  (ExpN(I, 1), I=1,8)
     >  / 0.989, 1.00, 1.02, 0.978, 1.10, 0.972, 0.987, 0.846 /
c                                                             CTEQ1MS
      DATA 
     >  Isch(2), Iord(2), Nqrk(2), Alm(2) /  1,  2,  6, .152  / 
     >  (Vlm(I,2), I=4,6) / .231,    .152,    .059  /
     >  (Qms(I,2), I=4,6) / 1.50,   5.00,  180.0 /
     >  Xmn(2), Qmn(2), Qmx(2) /  1.E-5,  2.00,  1.E3  /
     >  Mex(2), Mpn(2), Nexp(2) /  5, 3, 8  /
     >  (ExpN(I, 2), I=1,8 )
     >  / 0.989, 1.00, 1.02, 0.984, 1.05, 0.891, 0.923, 0.824 /
c                                                             CTEQ1ML
      DATA 
     >  Isch(3), Iord(3), Nqrk(3), Alm(3) /  1,  2,  6, .220  / 
     >  (Vlm(I,3), I=4,6) / .322,    .220,     .088  /
     >  (Qms(I,3), I=4,6) / 1.50,   5.00,  180.0 /
     >  Xmn(3), Qmn(3), Qmx(3) /  1.E-5,  2.00,  1.E3  /
     >  Mex(3), Mpn(3), Nexp(3) /  5, 3, 8 /
     >  (ExpN(I, 3), I=1,8 )
     >  / 0.985, 1.00, 1.01, 0.977, 1.07, 1.31, 1.19, 1.09 /

c                                                             CTEQ1D
      DATA 
     >  Isch(4), Iord(4), Nqrk(4), Alm(4) /  2,  2,  6, .164  / 
     >  (Vlm(I,4), I=4,6) / .247,    .164,    .064  /
     >  (Qms(I,4), I=4,6) / 1.50,   5.00,  180.0 /
     >  Xmn(4), Qmn(4), Qmx(4) /  1.E-5,  2.00,  1.E3  /
     >  Mex(4), Mpn(4), Nexp(4) /  5, 3, 8 /
     >  (ExpN(I, 4), I=1,8 )
     >  / 0.983, 1.00, 1.01, 0.975, 0.964, 1.23, 1.00, 1.12 /
c                                                             CTEQ1L
      DATA 
     >  Isch(5), Iord(5), Nqrk(5), Alm(5) /  1,  1,  6, .125  / 
     >  (Vlm(I,5), I=4,6) / .168,    .125,     .063   /
     >  (Qms(I,5), I=4,6) / 1.50,   5.00,  180.0 /
     >  Xmn(5), Qmn(5), Qmx(5) /  1.E-5,  2.00,  1.E3  /
     >  Mex(5), Mpn(5), Nexp(5) /  5, 3, 8  /
     >  (ExpN(I, 5), I=1,8 )
     >  / 0.982, 1.01, 1.00, 0.972, 0.840, 0.959, 0.930, 0.861 /

      Data ist, lp, qsto, Aln2 / 0, -10, 1.2345, 0.6931 /

      X  = XX
      if(iset.eq.ist.and.xsto.eq.xx.and.qsto.eq.qq) goto 100

      Irt = 0

      Alam = Alm (Iset)

      sta = log(qq/alam)
      stbqm = log(Qmn(iset)/alam)
      sb = log(sta/stbqm)
      SB2 = SB*SB
      SB3 = SB2*SB

      Goto (1, 2, 3, 4, 5), Iset

 1    continue
c   ifl =     2
 11   A0=0.3636E+01*(1.0 + 0.3122E+00*SB+0.1396E+00*SB2+0.4251E+00*SB3)
      A1=0.6930E+00-.2574E-01*SB+0.1047E+00*SB2-.2794E-01*SB3
      A2=0.3195E+01+0.4045E+00*SB-.3737E+00*SB2-.1677E+00*SB3
      A3=0.1009E+00*(1.0 -.1784E+01*SB+0.6263E+00*SB2+0.7337E-01*SB3)
     $  -1.0
      A4=0.2910E+00-.2793E+00*SB+0.6155E-01*SB2+0.5150E-02*SB3
      A5=0.0000E+00+0.3185E+00*SB+0.1953E+00*SB2+0.4184E-01*SB3
      Pdf(2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5

c   ifl =     1
 12   A0=0.2851E+00*(1.0 + 0.3617E+00*SB-.4526E+00*SB2+0.5787E-01*SB3)
      A1=0.2690E+00+0.1104E-01*SB+0.1888E-01*SB2-.1031E-01*SB3
      A2=0.3766E+01+0.7850E+00*SB-.3053E+00*SB2+0.1822E+00*SB3
      A3=0.2865E+02*(1.0 -.9774E+00*SB+0.5958E+00*SB2-.1234E+00*SB3)
     $  -1.0
      A4=0.8230E+00-.3612E+00*SB+0.5520E-01*SB2+0.1571E-01*SB3
      A5=0.0000E+00+0.2145E-01*SB+0.2289E+00*SB2-.4947E-01*SB3
      Pdf(1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5

c   ifl =     0
 13   A0=0.2716E+01*(1.0 -.2092E+01*SB+0.1500E+01*SB2-.3703E+00*SB3)
      A1=-.3100E-01-.7963E+00*SB+0.1129E+01*SB2-.4191E+00*SB3
      A2=0.8015E+01+0.1168E+01*SB-.1625E+01*SB2-.1130E+01*SB3
      A3=0.4813E+02*(1.0 -.4951E+00*SB-.8715E+00*SB2+0.5893E+00*SB3)
     $  -1.0
      A4=0.2773E+01-.6329E+00*SB-.1048E+01*SB2+0.1418E+00*SB3
      A5=0.0000E+00+0.5048E+00*SB+0.2390E+01*SB2-.4159E+00*SB3
      Pdf(0) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5

c   ifl =    -1
 14   A0=0.3085E+00*(1.0 + 0.9422E+00*SB-.2606E+01*SB2+0.1364E+01*SB3)
      A1=0.5000E-02-.6433E+00*SB+0.4980E+00*SB2-.1780E+00*SB3
      A2=0.7490E+01+0.9112E+00*SB-.2047E+01*SB2+0.1456E+01*SB3
      A3=0.1145E-01*(1.0 + 0.4610E+01*SB+0.1699E+01*SB2+0.1296E+00*SB3)
     $  -1.0
      A4=0.6030E+00-.8081E+00*SB+0.9410E+00*SB2-.4458E+00*SB3
      A5=0.0000E+00-.1736E+01*SB+0.2863E+01*SB2-.1268E+01*SB3
      Pdf(-1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -2
 15   A0=0.1324E+00*(1.0 -.1050E+01*SB+0.4844E+00*SB2-.1043E+00*SB3)
      A1=-.1580E+00+0.1672E+00*SB-.4100E+00*SB2+0.1793E+00*SB3
      A2=0.8559E+01-.7351E-01*SB+0.5898E+00*SB2-.2655E+00*SB3
      A3=0.2378E+02*(1.0 -.1108E+00*SB-.1646E-01*SB2+0.1129E-01*SB3)
     $  -1.0
      A4=0.1477E+01+0.3312E-01*SB-.2191E+00*SB2+0.9588E-01*SB3
      A5=0.0000E+00+0.1850E+01*SB-.1481E+01*SB2+0.6222E+00*SB3
      Pdf(-2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -3
 16   A0=0.3208E+00*(1.0 -.4755E+00*SB-.4003E+00*SB2+0.2300E+00*SB3)
      A1=-.3200E-01-.3357E+00*SB+0.3222E-01*SB2+0.5011E-01*SB3
      A2=0.1164E+02+0.1048E+01*SB-.1097E+01*SB2-.4431E+00*SB3
      A3=0.5065E+02*(1.0 + 0.2484E+00*SB-.9235E+00*SB2+0.1935E+00*SB3)
     $  -1.0
      A4=0.3300E+01-.6785E+00*SB+0.5337E+00*SB2-.4035E+00*SB3
      A5=0.0000E+00-.2496E+00*SB+0.3903E+00*SB2+0.1392E+00*SB3
      Pdf(-3) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -4
 17   A0=0.7967E-06*(1.0 + 0.1587E+01*SB+0.1812E+02*SB2-.1333E+02*SB3)
     $ *sqrt(sta - stbqm)
      A1=0.1096E+01-.1236E+01*SB+0.1014E+02*SB2+0.1940E+01*SB3
      A2=0.4366E+00+0.1197E+02*SB-.5471E+00*SB2-.5427E+01*SB3
      A3=0.4650E+03*(1.0 + 0.1310E+02*SB-.1918E+02*SB2+0.6791E+01*SB3)
     $  -1.0
      A4=-.8486E+00+0.7457E+00*SB-.1083E+02*SB2-.1210E+01*SB3
      A5=0.3494E+01-.3511E+01*SB-.1766E+01*SB2+0.3442E+01*SB3
      Pdf(-4) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -5
      if(qq.le.qms(5,iset)) then
         pdf(-5) = 0.0
         pdf(-6) = 0.0
         goto 100
      endif
      stbq5 = log(Qms(5,iset)/alam)
      s5 = log(sta/stbq5)
      s52 = s5*s5
      s53 = s52*s5
 18   A0=0.1713E-03*(1.0 + 0.2562E+02*S5-.2988E+02*S52+0.4798E+01*S53)
     $ *sqrt(sta - stbq5)
      A1=-.5276E-01+0.4105E+00*S5-.1079E+01*S52+0.6278E+00*S53
      A2=0.4515E+01+0.8369E+01*S5-.1192E+02*S52+0.3403E+01*S53
      A3=0.1756E+01*(1.0 + 0.1325E+02*S5-.2997E+02*S52+0.1758E+02*S53)
     $  -1.0
      A4=0.3557E-01+0.4159E+01*S5-.6947E+01*S52+0.2982E+01*S53
      A5=0.2551E+01+0.2168E+01*S5-.5119E+01*S52+0.3739E+01*S53
      Pdf(-5) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -6
      if(qq.le.qms(6,iset)) then
         pdf(-6) = 0.0
         goto 100
      endif
      stbq6 = log(Qms(6,iset)/alam)
      s6 = log(sta/stbq6)
      s62 = s6*s6
      s63 = s62*s6
 19   A0=0.7510E-04*(1.0 + 0.2836E+02*S6-.3000E+02*S62-.2979E+02*S63)
     $ *sqrt(sta - stbq6)
      A1=-.1855E+00+0.4543E+00*S6-.1448E+01*S62+0.2009E-01*S63
      A2=0.6775E+01-.4210E+01*S6-.1221E+01*S62+0.1199E+02*S63
      A3=0.1070E+01*(1.0 + 0.8356E+01*S6-.2992E+02*S62+0.2433E+02*S63)
     $  -1.0
      A4=-.4601E-01+0.4248E+01*S6-.1736E+01*S62+0.1187E+02*S63
      A5=0.2771E+01+0.1382E+01*S6-.4797E+01*S62+0.1273E+01*S63
      Pdf(-6) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      goto 100


 2    continue
c                                                             CTEQ1MS
c   ifl =     2
 21   A0=0.1828E+01*(1.0 -.8698E+00*SB+0.2906E+00*SB2-.2003E-01*SB3)
      A1=0.6060E+00+0.8595E-01*SB-.4934E-01*SB2+0.2221E-01*SB3
      A2=0.3454E+01-.3115E+00*SB+0.1321E+01*SB2-.3490E+00*SB3
      A3=0.2616E+00*(1.0 -.1670E+01*SB+0.2333E+01*SB2+0.7730E-01*SB3)
     $  -1.0
      A4=0.8920E+00-.8500E-02*SB+0.4960E+00*SB2-.4045E-01*SB3
      A5=0.0000E+00+0.1091E+01*SB-.1613E+00*SB2+0.3773E-01*SB3
      Pdf(2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     1
 22   A0=0.2885E+00*(1.0 + 0.3388E+00*SB-.4550E+00*SB2+0.6005E-01*SB3)
      A1=0.2730E+00+0.1198E-01*SB+0.1880E-01*SB2-.1077E-01*SB3
      A2=0.3736E+01+0.7687E+00*SB-.2731E+00*SB2+0.1638E+00*SB3
      A3=0.2741E+02*(1.0 -.9585E+00*SB+0.5925E+00*SB2-.1239E+00*SB3)
     $  -1.0
      A4=0.8040E+00-.3546E+00*SB+0.6123E-01*SB2+0.1086E-01*SB3
      A5=0.0000E+00+0.4277E-01*SB+0.2187E+00*SB2-.4646E-01*SB3
      Pdf(1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     0
 23   A0=0.8416E-01*(1.0 -.1996E+01*SB+0.1903E+01*SB2-.6722E+00*SB3)
      A1=-.4790E+00-.5459E+00*SB+0.1638E+01*SB2-.4342E+00*SB3
      A2=0.5071E+01+0.1470E+01*SB-.2401E+01*SB2+0.1273E+01*SB3
      A3=0.2847E+02*(1.0 + 0.1124E+00*SB-.1338E+01*SB2+0.7115E+00*SB3)
     $  -1.0
      A4=0.4990E+00-.7208E+00*SB+0.3333E-03*SB2-.2354E+00*SB3
      A5=0.0000E+00-.4480E+00*SB+0.3720E+01*SB2-.1838E+01*SB3
      Pdf(0) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -1
 24   A0=0.4378E+00*(1.0 -.1244E+01*SB+0.3278E+01*SB2-.2098E+01*SB3)
      A1=0.3500E-01-.1298E+01*SB+0.1229E+01*SB2-.3665E+00*SB3
      A2=0.6781E+01+0.4078E+01*SB-.9711E+00*SB2-.1536E+01*SB3
      A3=0.1527E-03*(1.0 + 0.1430E+02*SB+0.3000E+02*SB2+0.2771E+02*SB3)
     $  -1.0
      A4=0.3060E+00+0.1011E+01*SB-.2045E+01*SB2+0.9422E+00*SB3
      A5=0.0000E+00-.3205E+01*SB+0.2683E+01*SB2-.1746E+00*SB3
      Pdf(-1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -2
 25   A0=0.7413E-01*(1.0 + 0.1291E+01*SB-.2667E+01*SB2+0.1076E+01*SB3)
      A1=-.2730E+00-.1206E+00*SB+0.1828E+00*SB2-.1001E+00*SB3
      A2=0.7719E+01+0.1537E+01*SB-.6410E+00*SB2-.3920E-01*SB3
      A3=0.1799E+02*(1.0 -.1334E+01*SB+0.1916E+01*SB2-.8878E+00*SB3)
     $  -1.0
      A4=0.1167E+01-.9176E-01*SB+0.5132E+00*SB2-.3460E+00*SB3
      A5=0.0000E+00-.5023E+00*SB+0.1951E+01*SB2-.8427E+00*SB3
      Pdf(-2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -3
 26   A0=0.6551E+00*(1.0 -.5968E-01*SB+0.5621E-02*SB2-.2074E+00*SB3)
      A1=0.2800E-01-.1138E+01*SB+0.1178E+01*SB2-.4425E+00*SB3
      A2=0.7553E+01+0.3996E+01*SB-.4448E+01*SB2+0.1673E+01*SB3
      A3=0.9264E-01*(1.0 -.1760E+01*SB+0.1634E+01*SB2-.4067E+00*SB3)
     $  -1.0
      A4=0.1970E+00+0.5256E+00*SB-.9775E+00*SB2+0.4488E+00*SB3
      A5=0.0000E+00-.3668E+01*SB+0.4757E+01*SB2-.1717E+01*SB3
      Pdf(-3) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -4
 27   A0=0.1486E-03*(1.0 + 0.2107E+01*SB-.1056E+02*SB2+0.1403E+02*SB3)
     $ * sqrt(sta - stbqm)
      A1=0.2115E+00-.1702E+01*SB+0.2571E+01*SB2-.1177E+01*SB3
      A2=0.3533E+01+0.1367E+01*SB-.3397E+01*SB2+0.6260E+01*SB3
      A3=0.1096E+02*(1.0 + 0.9213E+01*SB-.2020E+02*SB2+0.1084E+02*SB3)
     $  -1.0
      A4=0.7041E+00-.7236E+00*SB+0.2766E-01*SB2+0.7352E+00*SB3
      A5=0.3904E+01-.4398E+01*SB+0.7056E+01*SB2-.3722E+01*SB3
      Pdf(-4) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -5
      if(qq.le.qms(5,iset)) then
         pdf(-5) = 0.0
         pdf(-6) = 0.0
         goto 100
      endif
      stbq5 = log(Qms(5,iset)/alam)
      s5 = log(sta/stbq5)
      s52 = s5*s5
      s53 = s52*s5
 28   A0=0.1201E-03*(1.0 + 0.5408E+01*S5-.1489E+02*S52+0.1667E+02*S53)
     $  * sqrt(sta - stbq5)
      A1=0.1420E-01-.1525E+01*S5+0.2408E+01*S52-.1154E+01*S53
      A2=0.4254E+01+0.2836E+01*S5-.6018E+00*S52+0.4133E+00*S53
      A3=0.5696E+01*(1.0 + 0.9451E+01*S5-.2029E+02*S52+0.1033E+02*S53)
     $  -1.0
      A4=0.4775E+00-.6695E+00*S5+0.2747E+00*S52-.1051E+00*S53
      A5=0.3330E+01-.5133E+01*S5+0.6921E+01*S52-.3283E+01*S53
      Pdf(-5) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -6
      if(qq.le.qms(6,iset)) then
         pdf(-6) = 0.0
         goto 100
      endif
      stbq6 = log(Qms(6,iset)/alam)
      s6 = log(sta/stbq6)
      s62 = s6*s6
      s63 = s62*s6
 29   A0=0.7697E-04*(1.0 + 0.2801E+02*S6-.1901E+02*S62-.2880E+02*S63)
     $ *sqrt(sta - stbq6)
      A1=-.2249E+00+0.4432E+00*S6-.1454E+01*S62+0.3509E-01*S63
      A2=0.6642E+01-.2702E+01*S6+0.8229E+01*S62+0.8243E+01*S63
      A3=0.1146E+01*(1.0 + 0.8104E+01*S6-.2998E+02*S62+0.2812E+02*S63)
     $  -1.0
      A4=-.6421E-01+0.4246E+01*S6-.2908E+01*S62+0.9686E-02*S63
      A5=0.2606E+01+0.1261E+01*S6-.4933E+01*S62+0.3476E+00*S63
      Pdf(-6) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      goto 100


 3    continue
c                                                             CTEQ1ML
c   ifl =     2
 31   A0=0.3777E+01*(1.0 + 0.6986E+00*SB-.20655E+01*SB2+.10334E+01*SB3)
      A1=0.7100E+00+.2880E-01*SB-.7930E-01*SB2+0.5600E-01*SB3
      A2=0.3259E+01+0.1508E+01*SB-.3932E+01*SB2+0.20613E+01*SB3
      A3=0.1304E+00*(1.0 -.2016E+00*SB-.30015E+01*SB2+0.19118E+01*SB3)
     $     -1.0
      A4=0.2890E+00-0.4311E+00*SB+0.7387E+00*SB2-.3697E+00*SB3
      A5=0.0000E+00+0.4320E+00*SB+0.2449E+00*SB2-0.6670E-01*SB3
      Pdf(2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     1
 32   A0=0.2780E+00*(1.0 + 0.4355E+00*SB-0.4584E+00*SB2+0.4390E-01*SB3)
      A1=0.2760E+00+0.1420E-01*SB+0.1480E-01*SB2-.9800E-02*SB3
      A2=0.3710E+01+0.8250E+00*SB-.3581E+00*SB2+0.1978E+00*SB3
      A3=0.2928E+02*(1.0 -.10154E+01*SB+0.6037E+00*SB2-.1175E+00*SB3)
     $     -1.0
      A4=0.8070E+00-.3575E+00*SB+0.4920E-01*SB2+0.1584E-01*SB3
      A5=0.0000E+00+0.1860E-01*SB+0.2080E+00*SB2-.450E-01*SB3
      Pdf(1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     0
 33   A0=0.2924E+01*(1.0 -.18916E+01*SB+0.1191E+01*SB2-.2492E+00*SB3)
      A1=0.0000E+00-.9167E+00*SB+0.11147E+01*SB2-.3329E+00*SB3
      A2=0.8529E+01+0.7080E+00*SB-.11345E+01*SB2-.10563E+01*SB3
      A3=0.1420E+03*(1.0 -.15346E+01*SB+0.7261E+00*SB2-.5730E-01*SB3)
     $     -1.0
      A4=0.3396E+01-.11541E+01*SB-.8834E+00*SB2+0.2430E+00*SB3
      A5=0.0000E+00+0.1645E+00*SB+0.19041E+01*SB2+0.1474E+00*SB3
      Pdf(0) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -1
 34   A0=0.3471E+00*(1.0- 0.1753E+00*SB-.9189E+00*SB2+0.6211E+00*SB3)
      A1=0.1900E-01-.4579E+00*SB+0.2112E+00*SB2-.6180E-01*SB3
      A2=0.7301E+01-.17308E+01*SB+.13666E+01*SB2-.6400E-02*SB3
      A3=0.1853E-04*(1.0 -.18260E+02*SB-.2872E+02*SB2-.23456E+02*SB3)
     $     -1.0
      A4=0.4400E+00-.4672E+00*SB+0.6532E+00*SB2-.3222E+00*SB3
      A5=0.0000E+00-.4679E+00*SB+0.10741E+01*SB2-.5663E+00*SB3
      Pdf(-1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -2
 35   A0=0.1702E+00*(1.0 -.1041E+01*SB+0.4064E+00*SB2-.5888E-01*SB3)
      A1=-.9300E-01-.4742E-01*SB-.1959E+00*SB2+0.1039E+00*SB3
      A2=0.9119E+01-.7331E-01*SB+0.3506E+00*SB2-.2081E+00*SB3
      A3=0.2981E+02*(1.0 -.1912E+00*SB-.8947E-02*SB2+0.8805E-02*SB3)
     $     -1.0
      A4=0.1668E+01-.6678E-02*SB-.2894E+00*SB2+0.1221E+00*SB3
      A5=0.0000E+00+0.1245E+01*SB-.7843E+00*SB2+0.3724E+00*SB3
      Pdf(-2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -3
 36   A0=0.3910E+00*(1.0 -.1103E+01*SB+0.5383E+00*SB2-.1083E+00*SB3)
      A1=-.1400E-01-.2471E+00*SB-.8042E-01*SB2+0.7193E-01*SB3
      A2=0.9812E+01-.4860E+01*SB+0.5958E+01*SB2-.2342E+01*SB3
      A3=0.3749E+00*(1.0 -.3569E+01*SB+0.5456E+01*SB2-.2344E+01*SB3)
     $     -1.0
      A4=0.4940E+00+0.2772E+00*SB-.2732E+00*SB2+0.6466E-01*SB3
      A5=0.0000E+00+0.3927E+00*SB-.3216E+00*SB2+0.2164E+00*SB3
      Pdf(-3) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -4
 37   A0=0.3815E-02*(1.0 + 0.2039E+02*SB-.2834E+02*SB2+0.1070E+02*SB3)
     $ * sqrt(sta - stbqm)
      A1=-.2789E-01-.7345E-03*SB-.3251E+00*SB2+0.1946E+00*SB3
      A2=0.3223E+01-.4268E+00*SB+0.4387E+01*SB2-.2401E+01*SB3
      A3=0.3338E-01*(1.0 -.1163E+02*SB+0.2995E+02*SB2-.1471E+02*SB3)
     $     -1.0
      A4=0.3646E+00-.5767E+00*SB+0.6088E+00*SB2-.2514E+00*SB3
      A5=0.1200E+01+0.2178E+00*SB-.4230E+00*SB2+0.4739E+00*SB3
      Pdf(-4) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -5
      if(qq.le.qms(5,iset)) then
         pdf(-5) = 0.0
         pdf(-6) = 0.0
         goto 100
      endif
      stbq5 = log(Qms(5,iset)/alam)
      s5 = log(sta/stbq5)
      s52 = s5*s5
      s53 = s52*s5
 38   A0=0.1666E-02*(1.0 + 0.9518E+01*S5-.4715E+01*S52-.1060E+01*S53)
     $ * sqrt(sta - stbq5)
      A1=-.1231E+00+0.1656E+00*S5-.5219E+00*S52+0.2750E+00*S53
      A2=0.3693E+01+0.4922E+01*S5-.1200E+02*S52+0.7929E+01*S53
      A3=0.1778E+00*(1.0 + 0.3036E+01*S5-.1184E+02*S52+0.7940E+01*S53)
     $     -1.0
      A4=0.5353E+00-.1401E+01*S5+0.1970E+01*S52-.9405E+00*S53
      A5=0.1590E+01+0.1025E+01*S5-.2318E+01*S52+0.1380E+01*S53
      Pdf(-5) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -6
      if(qq.le.qms(6,iset)) then
         pdf(-6) = 0.0
         goto 100
      endif
      stbq6 = log(Qms(6,iset)/alam)
      s6 = log(sta/stbq6)
      s62 = s6*s6
      s63 = s62*s6
 39   A0=0.4319E-03*(1.0 + 0.1100E+02*S6-.9520E+00*S62+0.1434E+02*S63)
     $ * sqrt(sta - stbq6)
      A1=-.2512E+00+0.3554E+00*S6-.4120E+00*S62+0.1328E+00*S63
      A2=0.4764E+01-.3513E+00*S6+0.1199E+02*S62-.8290E+01*S63
      A3=0.8458E-01*(1.0 + 0.2618E+01*S6+0.4407E+01*S62+0.2991E+02*S63)
     $    -1.0
      A4=0.3991E+00-.1363E+01*S6+0.1526E+01*S62-.3179E+01*S63
      A5=0.1981E+01+0.1496E+01*S6-.1501E+01*S62+0.3880E+01*S63
      Pdf(-6) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      goto 100

 4    continue
c                                                             CTEQ1D
c   ifl =     2
 41   A0=0.1634E+01*(1.0 -.8336E+00*SB+0.1640E+00*SB2+0.1530E+00*SB3)
      A1=0.5790E+00+0.8587E-01*SB-.6087E-01*SB2+0.1361E-01*SB3
      A2=0.2839E+01+0.3720E+00*SB+0.5264E+00*SB2+0.3538E-01*SB3
      A3=0.1095E+00*(1.0 -.4830E+00*SB+0.3708E+01*SB2-.6165E+00*SB3)
     $  -1.0
      A4=0.8010E+00-.1432E+00*SB+0.1442E+01*SB2-.1286E+01*SB3
      A5=0.0000E+00+0.1035E+01*SB-.5910E-01*SB2-.1982E+00*SB3
      Pdf(2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     1
 42   A0=0.3535E+00*(1.0 + 0.4352E+00*SB-.2095E+00*SB2-.8455E-02*SB3)
      A1=0.2660E+00-.4096E-03*SB+0.1502E-01*SB2-.1163E-01*SB3
      A2=0.3514E+01+0.8219E+00*SB-.2330E+00*SB2+0.1055E+00*SB3
      A3=0.2200E+02*(1.0 -.9716E+00*SB+0.4552E+00*SB2-.8202E-01*SB3)
     $  -1.0
      A4=0.9000E+00-.3207E+00*SB-.4808E-01*SB2+0.3492E-01*SB3
      A5=0.0000E+00-.6273E-01*SB+0.1497E+00*SB2-.5683E-01*SB3
      Pdf(1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     0
 43   A0=0.2743E+01*(1.0 -.2027E+01*SB+0.1517E+01*SB2-.4145E+00*SB3)
      A1=0.7000E-02-.9431E+00*SB+0.1231E+01*SB2-.4834E+00*SB3
      A2=0.8200E+01+0.1827E+01*SB-.3453E+01*SB2+0.6763E+00*SB3
      A3=0.4975E+02*(1.0 -.2329E+00*SB-.1245E+01*SB2+0.7194E+00*SB3)
     $  -1.0
      A4=0.2387E+01-.4077E+00*SB-.5542E+00*SB2-.9677E-02*SB3
      A5=0.0000E+00+0.2702E+00*SB+0.2389E+01*SB2-.8274E+00*SB3
      Pdf(0) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -1
 44   A0=0.2015E+00*(1.0 -.2133E+00*SB-.6770E+00*SB2+0.5011E+00*SB3)
      A1=-.7700E-01-.7104E-01*SB-.3720E+00*SB2+0.2159E+00*SB3
      A2=0.8008E+01-.2049E+01*SB+0.1800E+01*SB2-.4660E+00*SB3
      A3=0.2923E-05*(1.0 + 0.2327E+02*SB+0.1500E+02*SB2+0.2633E+02*SB3)
     $  -1.0
      A4=0.9020E+00-.9191E+00*SB+0.1104E+01*SB2-.5863E+00*SB3
      A5=0.0000E+00+0.5840E+00*SB-.8720E+00*SB2+0.4234E+00*SB3
      Pdf(-1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5

c   ifl =    -2
 45   A0=0.9117E-01*(1.0 -.4089E+00*SB-.4361E+00*SB2+0.2512E+00*SB3)
      A1=-.2370E+00+0.2492E+00*SB-.3267E+00*SB2+0.1055E+00*SB3
      A2=0.8447E+01+0.6009E+00*SB+0.1003E+01*SB2-.1287E+01*SB3
      A3=0.3106E+02*(1.0 -.3901E-01*SB+0.1443E+00*SB2-.3433E+00*SB3)
     $  -1.0
      A4=0.1629E+01+0.7855E-01*SB-.1573E+00*SB2-.8595E-01*SB3
      A5=0.0000E+00+0.1558E+01*SB-.6295E+00*SB2+0.1847E+00*SB3
      Pdf(-2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -3
 46   A0=0.3997E+00*(1.0 -.1046E+01*SB+0.6194E+00*SB2-.1342E+00*SB3)
      A1=0.2000E-02-.2544E+00*SB-.1958E+00*SB2+0.1458E+00*SB3
      A2=0.9613E+01-.3919E+01*SB+0.9573E+01*SB2-.5623E+01*SB3
      A3=0.3620E+00*(1.0 -.1858E+01*SB+0.8312E+01*SB2-.5900E+01*SB3)
     $  -1.0
      A4=0.3840E+00+0.3572E+00*SB-.1191E+01*SB2+0.7310E+00*SB3
      A5=0.0000E+00+0.3351E+00*SB-.7709E+00*SB2+0.4296E+00*SB3
      Pdf(-3) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -4
 47   A0=0.2156E-03*(1.0 + 0.2879E+02*SB-.2310E+02*SB2+0.9812E+01*SB3)
     $ * sqrt(sta - stbqm)
      A1=0.9086E-01-.1250E+00*SB-.7373E-01*SB2-.2201E-01*SB3
      A2=0.3588E+01+0.4518E+01*SB-.8930E-01*SB2+0.9163E-02*SB3
      A3=0.5216E+01*(1.0 + 0.5912E+00*SB-.4111E+00*SB2+0.7330E+00*SB3)
     $  -1.0
      A4=0.3145E+00+0.1233E+01*SB-.7478E+00*SB2+0.4657E+00*SB3
      A5=0.2723E+01-.4110E+00*SB+0.4868E-01*SB2-.3075E+00*SB3
      Pdf(-4) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -5
      if(qq.le.qms(5,iset)) then
         pdf(-5) = 0.0
         pdf(-6) = 0.0
         goto 100
      endif
      stbq5 = log(Qms(5,iset)/alam)
      s5 = log(sta/stbq5)
      s52 = s5*s5
      s53 = s52*s5
 48   A0=0.7476E-03*(1.0 + 0.1454E+02*S5-.2509E+02*S52+0.1184E+02*S53)
     $ * sqrt(sta - stbq5)
      A1=-.1955E-01-.1712E+00*S5-.1686E+00*S52+0.2339E+00*S53
      A2=0.4616E+01-.6859E+00*S5-.3959E+01*S52+0.5530E+01*S53
      A3=0.9881E+01*(1.0 -.1239E+02*S5+0.2721E+02*S52-.1850E+02*S53)
     $  -1.0
      A4=0.1200E+02-.1133E+02*S5+0.8138E+01*S52+0.1199E+02*S53
      A5=0.2226E+01-.5738E+00*S5+0.5239E+00*S52+0.3825E+00*S53
      Pdf(-5) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -6
      if(qq.le.qms(6,iset)) then
         pdf(-6) = 0.0
         goto 100
      endif
      stbq6 = log(Qms(6,iset)/alam)
      s6 = log(sta/stbq6)
      s62 = s6*s6
      s63 = s62*s6
 49   A0=0.8392E-06*(1.0 + 0.1844E+02*S6-.1110E+02*S62-.2504E+02*S63)
     $ * sqrt(sta - stbq6)
      A1=0.2127E+00-.5602E+00*S6+0.4777E+01*S62-.1014E+02*S63
      A2=0.1229E+01+0.7495E+01*S6-.5024E+01*S62-.1200E+02*S63
      A3=0.2868E+02*(1.0 + 0.7634E+01*S6-.2916E+02*S62+0.2953E+02*S63)
     $  -1.0
      A4=0.5970E+00+0.1138E+01*S6-.1439E+01*S62-.1966E+01*S63
      A5=0.6429E+01-.6673E+00*S6+0.7008E+01*S62-.1157E+02*S63
      Pdf(-6) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      goto 100

 5    continue
c                                                             CTEQ1L
c   ifl =     2
 51   A0=  1.791*(1.0 -0.449*SB-0.445*SB2+  0.401*SB3)
      A1=  0.608+  0.069*SB+  0.005*SB2-0.037*SB3
      A2=  3.470-0.375*SB+  2.267*SB2-1.261*SB3
      A3=  0.315*(1.0 -2.628*SB+  6.481*SB2-3.834*SB3)-1.0
      A4=  1.007-0.732*SB+  1.490*SB2-0.966*SB3
      A5=  0.000+  0.741*SB+  0.563*SB2-0.525*SB3
      Pdf(2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     1
 52   A0=  0.513*(1.0 +   0.032*SB-0.120*SB2+  0.013*SB3)
      A1=  0.276+  0.052*SB+  0.000*SB2-0.006*SB3
      A2=  3.579+  0.763*SB-0.135*SB2+  0.083*SB3
      A3= 17.993*(1.0 -0.725*SB+  0.241*SB2-0.020*SB3)-1.0
      A4=  1.120-0.357*SB+  0.008*SB2+  0.028*SB3
      A5=  0.000+  0.311*SB+  0.029*SB2-0.010*SB3
      Pdf(1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =     0
 53   A0=  2.710*(1.0 -1.773*SB+  0.970*SB2-0.149*SB3)
      A1= -0.010-1.636*SB+  2.087*SB2-0.637*SB3
      A2=  7.174+  2.102*SB-2.209*SB2-0.420*SB3
      A3= 29.904*(1.0 -0.756*SB-0.506*SB2+  0.605*SB3)-1.0
      A4=  2.572-0.437*SB-0.968*SB2+  0.243*SB3
      A5=  0.000-1.776*SB+  4.266*SB2-0.335*SB3
      Pdf(0) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -1
 54   A0=  0.278*(1.0 - 1.022*SB+  0.6457*SB2-0.1824*SB3)
      A1=  0.0862*SB-0.8657*SB2+  0.4185*SB3
      A2= 11.000-1.2809*SB+ 1.2516*SB2+0.061*SB3
      A3= 37.338*(1.0 - 0.9404*SB+  0.2517*SB2+0.1364*SB3)-1.0
      A4=  1.960-  0.3385*SB-0.3422*SB2+0.3653*SB3
      A5=  0.000+1.424*SB-2.7503*SB2+  1.2226*SB3
      Pdf(-1) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -2
 55   A0=  0.154*(1.0 -0.659*SB+  0.005*SB2+  0.061*SB3)
      A1= -0.128+  0.279*SB-0.786*SB2+  0.363*SB3
      A2=  8.649+  0.071*SB+  0.351*SB2-0.051*SB3
      A3= 43.685*(1.0 -0.603*SB+  0.037*SB2+  0.134*SB3)-1.0
      A4=  2.238-0.338*SB-0.199*SB2+  0.157*SB3
      A5=  0.000+  1.681*SB-2.068*SB2+  0.975*SB3
      Pdf(-2) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -3
 56   A0=  0.372*(1.0 -1.939*SB+  1.504*SB2-0.440*SB3)
      A1=  0.009+  0.610*SB-1.387*SB2+  0.579*SB3
      A2= 10.273-4.833*SB+  6.583*SB2-2.633*SB3
      A3=  0.160*(1.0 +  10.325*SB-2.027*SB2+  1.571*SB3)-1.0
      A4=  0.819-1.660*SB+  1.845*SB2-0.829*SB3
      A5=  0.000+  3.558*SB-3.940*SB2+  1.302*SB3
      Pdf(-3) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -4
 57   A0=  (7.5242E-5)*(1.0+22.0905*SB+7.1209*SB2-8.303*SB3)*
     $     sqrt(sta - stbqm)
      A1=  0.125-0.3027*SB+0.1564*SB2-0.091*SB3
      A2=  2.0388+1.2161*SB+11.5296*SB2-8.0659*SB3
      A3=  14.849*(1.0 -2.556*SB+3.5268*SB2-1.6353*SB3)-1.0
      A4=  0.3061-0.0901*SB+0.953*SB2-0.4871*SB3
      A5=  2.7352+0.1811*SB-0.5167*SB2+0.0543*SB3
      Pdf(-4) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -5
      if(qq.le.qms(5,iset)) then
         pdf(-5) = 0.0
         pdf(-6) = 0.0
         goto 100
      endif
      stbq5 = log(Qms(5,iset)/alam)
      s5 = log(sta/stbq5)
      s52 = s5*s5
      s53 = s52*s5
 58   A0=  (3.751E-4)*(1.0 + 21.5993*S5+3.1379*S52-18.8328*S53)*
     $     sqrt(sta - stbq5)
      A1= -0.0256-0.7717*S5+ 1.1499*S52-0.5037*S53
      A2=  4.9241+4.0107*S5-4.7012*S52+0.1097*S53
      A3=  2.842*(1.0 -2.2184*S5+  2.0293*S52-0.6907*S53)-1.0
      A4=  -0.1352+ 0.8753*S5-1.2626*S52+  0.667*S53
      A5=  1.5627-0.4917*S5+ 1.5927*S52-0.351*S53
      Pdf(-5) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      
c   ifl =    -6
      if(qq.le.qms(6,iset)) then
         pdf(-6) = 0.0
         goto 100
      endif
      stbq6 = log(Qms(6,iset)/alam)
      s6 = log(sta/stbq6)
      s62 = s6*s6
      s63 = s62*s6
 59   A0=(2.725E-4)*(1.0 +  18.8497*S6-26.5797*S62-29.0774*S63)*
     $     sqrt(sta - stbq6)
      A1= -0.2204-1.0048*S6+0.9415*S62-0.4274*S63
      A2=  11.034-9.8362*S6-11.1034*S62-9.1977*S63
      A3=  2.084*(1.0 -2.881*S6+1.2778*S62-2.9328*S63)-1.0
      A4= -0.0872+  0.200*S6-1.6187*S62-1.6058*S63
      A5=  0.8684+4.7047*S6-1.4614*S62-5.2309*S63
      Pdf(-6) = A0*(x**A1)*((1.-x)**A2)*(1.+A3*(x**A4))
     $     *(log(1.+1./x))**A5
      goto 100

 100  continue

      do 110 iji = 2,-6,-1
 110     if(pdf(iji).lt.0.0) pdf(iji)=0.0


      Ist = Iset
      Qsto = QQ
      Xsto = xx

      Return
C                                  -----------------------
c      ENTRY WLAMBD (ISET, IORDER)

c      IORDER = IORD (ISET)
c      WLAMBD = ALM  (ISET)
                
c      RETURN
C                                  -----------------------
      Entry PrCtq2 
     >        (Iset, Iordr, Ischeme, MxFlv,
     >         Alam4, Alam5, Alam6, Amas4, Amas5, Amas6,
     >         Xmin, Qini, Qmax, ExpNor)

C                           Return QCD parameters and Fitting parameters
C                           associated with parton distribution set Iset.
C    Iord    : Order Of Fit
C    Ischeme : (0, 1, 2)  for  (LO, MS-bar-NLO, DIS-NLO) resp.
C    MxFlv   : Maximum number of flavors included
C    Alam_i  : i = 4,5,6  Effective lambda for i-flavors 

C    Amas_i  : i = 4,5,6  Mass parameter for flavor i
C    Xmin, Qini, Qmax : self explanary
C    ExpNor(I) : Normalization factor for the experimental data set used in
C                obtaining the best global fit for parton distributions Iset:
C     I = 1,     2,      3,     4,     5,     6,     7,     8
C      BCDMS   NMC90  NMC280  CCFR   E605    WA70   E706   UA6

      Iordr  = Iord (Iset)
      Ischeme= Isch (Iset)
      MxFlv  = Nqrk (Iset)

      Alam4  = Vlm(4,Iset)
      Alam5  = Vlm(5,Iset)
      Alam6  = Vlm(6,Iset)

      Amas4  = Qms(4,Iset)
      Amas5  = Qms(5,Iset)
      Amas6  = Qms(6,Iset)

      Xmin   = Xmn  (Iset)
      Qini   = Qmn  (Iset)
      Qmax   = Qmx  (Iset)

      Do 101 Iexp = 1, Nexp(Iset)
         ExpNor(Iexp) = ExpN(Iexp, Iset)
  101 Continue

      Return
C                         *************************
      END
C--- END CTEQ1 FITS -----------------------------
C--- START CTEQ3 FITS
      SUBROUTINE  CTEQ3(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF) 
      REAL*8 DX,DQ,PDF(-6:6)
C     Pdf(Iprtn), Iprtn = (6, 5, 4, 3, 2, 1, 0, -1, -2, ......,   -6)
C                  for (t, b, c, d, u, g, u_bar, d_bar,  ..., t_bar)
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      CALL CTQ3PDS(ISET,PDF,DX,DQ,IRT)
c 
      DO I=-NF,NF
        FX(I*IH0)=PDF(I)/DX
      ENDDO
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END
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
C--- END CTEQ3 FITS
C----- START CTEQ4 FITS ------------------------------
      SUBROUTINE  CTEQ4(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF) 
      REAL*8 DX,DQ,CTQ4FN
C
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
C The set CTEQ4A3 (iset=6 in the CTEQ convention) is identical to
C the set CTEQ4M, and was not inserted in our package
      IF(ISET.GE.6)ISET=ISET+1
C The function CTQ4FN return the parton distribution inside the proton.
C The division by the factor DX is NOT needed
      FX(0)=SNGL(CTQ4FN(ISET,0,DX,DQ))
      FX(IH0)=SNGL(CTQ4FN(ISET,1,DX,DQ))
      FX(2*IH0)=SNGL(CTQ4FN(ISET,2,DX,DQ))
      FX(-IH0)=SNGL(CTQ4FN(ISET,-1,DX,DQ))
      FX(-2*IH0)=SNGL(CTQ4FN(ISET,-2,DX,DQ))
      MF=NF
      IF(NF.EQ.6) MF=5
      DO I=3,MF
        FX(I)=SNGL(CTQ4FN(ISET,I,DX,DQ))
      ENDDO
      DO I=-MF,-3
        FX(I)=SNGL(CTQ4FN(ISET,I,DX,DQ))
      ENDDO
      IF(NF.EQ.6) THEN
         FX(6)=0
         FX(-6)=0
      ENDIF
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END

C============================================================================
C                CTEQ Parton Distribution Functions: Version 4
C                          June 21, 1996
C
C   By: H.L. Lai, J. Huston, S. Kuhlmann, F. Olness, J. Owens, D. Soper
C       W.K. Tung, H. Weerts
C   Ref: MSUHEP-60426, CTEQ-604, e-Print Archive: hep-ph/9606399
C
C   This package contains 9 sets of CTEQ4 PDF's. Details are:
C ---------------------------------------------------------------------------
C   Iset   PDF      Description             Alpha_s(Mz)  Q0(GeV)  Table_File
C ---------------------------------------------------------------------------
C   1      CTEQ4M   Standard MSbar scheme   0.116        1.6      cteq4m.tbl
C   2      CTEQ4D   Standard DIS scheme     0.116        1.6      cteq4d.tbl
C   3      CTEQ4L   Leading Order           0.116        1.6      cteq4l.tbl
C   4      CTEQ4A1  Alpha_s series          0.110        1.6      cteq4a1.tbl
C   5      CTEQ4A2  Alpha_s series          0.113        1.6      cteq4a2.tbl
C   6      CTEQ4A3  same as CTEQ4M          0.116        1.6      cteq4m.tbl
C   7      CTEQ4A4  Alpha_s series          0.119        1.6      cteq4a4.tbl
C   8      CTEQ4A5  Alpha_s series          0.122        1.6      cteq4a5.tbl
C   9      CTEQ4HJ  High Jet                0.116        1.6      cteq4hj.tbl
C   10     CTEQ4LQ  Low Q0                  0.114        0.7      cteq4lq.tbl
C ---------------------------------------------------------------------------
C   
C   The available applied range is 10^-5 < x < 1 and 1.6 < Q < 10,000 (GeV) 
C   except CTEQ4LQ for which Q starts at a lower value of 0.7 GeV.  
C   The Table_Files are assumed to be in the working directory.
C   
C   The function Ctq4Fn (Iset, Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton] 
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar)
C   
C   For detailed information on the parameters used, e.q. quark masses, 
C   QCD Lambda, ... etc.,  see info lines at the beginning of the 
C   Table_Files.

C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single 
C   precision.
C   
C   If you have detailed questions concerning these CTEQ4 distributions, 
C   or if you find problems/bugs using this package, direct inquires to 
C   Hung-Liang Lai(Lai_H@pa.msu.edu) or Wu-Ki Tung(Tung@pa.msu.edu).
C   
C===========================================================================

      Function Ctq4Fn (Iset, Iparton, X, Q)
      Implicit Double Precision (A-H,O-Z)
      Character Flnm(10)*11
      Common
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719QCDtable /  Alambda, Nfl, Iorder
      Data (Flnm(I), I=1,10)
     > / 'cteq4m', 'cteq4d', 'cteq4l'
     > , 'cteq4a1', 'cteq4a2', 'cteq4m', 'cteq4a4'
     > , 'cteq4a5', 'cteq4hj', 'cteq4lq' /
      Data Isetold, Isetmin, Isetmax / -987, 1, 10 /
      save

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
         If (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
	    Print *, 'Invalid Iset number in Ctq4Fn :', Iset
	    Stop
	 Endif
	 IU= NextUt()
         Open(IU, File=Flnm(Iset), Status='OLD', Err=100)
         Call ReadTbl (IU)
         Close (IU)
	 Isetold=Iset
      Endif

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
	Print *, 'X out of range in Ctq4Fn: ', X
	Stop
      Endif
      If (Q .lt. Alambda) Then
	Print *, 'Q out of range in Ctq4Fn: ', Q
	Stop
      Endif
      If (Iparton .lt. -NfMx .or. Iparton .gt. NfMx) Then
	Print *, 'Iparton out of range in Ctq4Fn: ', Iparton
	Stop
      Endif

      Ctq4Fn = PartonX (Iparton, X, Q)
      if(Ctq4Fn.lt.0.D0)  Ctq4Fn = 0.D0

      Return

 100  Print *, ' Data file ', Flnm(Iset), ' cannot be opened '
     >//'in Ctq4Fn!!'
      Stop
C                             ********************
      End

      Function NextUt()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 50, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUt = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
C

      Subroutine ReadTbl (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      Common 
     > / K719CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719XQrange / Qini, Qmax, Xmin
     > / K719QCDtable /  Alambda, Nfl, Iorder
     > / K719Masstbl / Amass(6)
      
      Read  (Nu, '(A)') Line     
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line 
      Read  (Nu, *) NX,  NT, NfMx

      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (QL(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         QL(Iq) = Log (QL(Iq) /Al)
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage, 
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence) 

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End

      FUNCTION PartonX (IPRTN, X, Q)
C
C   Given the parton distribution function in the array Upd in
C   COMMON / CtqPar1 / , this routine fetches u(fl, x, q) at any value of
C   x and q using Mth-order polynomial interpolation for x and Ln(Q/Lambda).
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      PARAMETER (M= 2, M1 = M + 1)
C
      Common 
     > / K719CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719XQrange / Qini, Qmax, Xmin
      Dimension Fq(M1), Df(M1)
      data ixrange/0/
      data iqmnrng/0/
      data iqmxrng/0/
      save ixrange,iqmnrng,iqmxrng
C
C                                                 Work with Log (Q)
      QG  = LOG (Q/AL)

C                           Find lower end of interval containing X
      JL = -1
      JU = Nx+1
 11   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (X .GT. XV(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif

      Jx = JL - (M-1)/2
      If (X .lt. Xmin) Then
         ixrange=ixrange+1
         if(ixrange.eq.1) Print '(A, 2(1pE12.4))', 
     >     ' WARNING: X < Xmin, extrapolation used; X, Xmin =', X, Xmin
         If (Jx .LT. 0) Jx = 0
      Elseif (Jx .GT. Nx-M) Then
         Jx = Nx - M
      Endif
C                                    Find the interval where Q lies
      JL = -1
      JU = NT+1
 12   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (QG .GT. QL(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 12
      Endif

      Jq = JL - (M-1)/2
      If (Jq .LT. 0) Then
         Jq = 0
         If (Q .lt. Qini)  then
           iqmnrng=iqmnrng+1
           if(iqmnrng.eq.1) Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q < Qini, extrapolation used; Q, Qini =', Q, Qini
         endif
      Elseif (Jq .GT. Nt-M) Then
         Jq = Nt - M
         If (Q .gt. Qmax) then
           iqmxrng=iqmxrng+1
           if(iqmxrng.eq.1) Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q > Qmax, extrapolation used; Q, Qmax =', Q, Qmax
         endif
      Endif

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
C                             Find the off-set in the linear array Upd
      JFL = Ip + NfMx
      J0  = (JFL * (NT+1) + Jq) * (NX+1) + Jx
C
C                                           Now interpolate in x for M1 Q's
      Do 21 Iq = 1, M1
         J1 = J0 + (Nx+1)*(Iq-1) + 1
         Call Polint_dd (XV(Jx), Upd(J1), M1, X, Fq(Iq), Df(Iq))
 21   Continue
C                                          Finish off by interpolating in Q
      Call Polint_dd (QL(Jq), Fq(1), M1, QG, Ftmp, Ddf)

      PartonX = Ftmp
C
      RETURN
C                        ****************************
      END

      SUBROUTINE POLINT_DD (XA,YA,N,X,Y,DY)
 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes" 
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
C--- END CTEQ4 FITS -----------------------------
C
C----- START CTEQ5 FITS ------------------------------
C Cteq5m1 (fitted form) added on mar-23-2001 by SF
c This set seemingly supersedes Cteq5m, which was affected (?) by a bug
c in the evolution code
      SUBROUTINE  CTEQ5(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF)  
      REAL*8 DX,DQ,CTQ5PDF,CTQ5PD,PDFS(-NF:NF)
      DATA INIT/0/ 
C                         
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      IF(ISET.LE.9)THEN
         CALL SETCTQ5(ISET)
         DO I=-NF,NF
            PDFS(I)=CTQ5PDF(I,DX,DQ)
         ENDDO
      ELSEIF(ISET.EQ.10) THEN
         DO I=-NF,NF
            PDFS(I)=CTQ5PD(1,I,DX,DQ,IRET) 
         ENDDO
      ELSE
         CALL NOSETP
      ENDIF
C     
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
C The function CTQ5PDF return the parton distribution inside the proton.
C The division by the factor DX is NOT needed
      FX(0)=SNGL(PDFS(0))
      FX(IH0)=SNGL(PDFS(1))
      FX(2*IH0)=SNGL(PDFS(2))
      FX(-IH0)=SNGL(PDFS(-1))
      FX(-2*IH0)=SNGL(PDFS(-2))
      DO I=3,NF              
        FX(I)=SNGL(PDFS(I))
      ENDDO          
      DO I=-NF,-3
        FX(I)=SNGL(PDFS(I))
      ENDDO          
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END

C============================================================================
C                CTEQ Parton Distribution Functions: Version 5.0
C                             Nov. 1, 1999
C
C   Ref: "GLOBAL QCD ANALYSIS OF PARTON STRUCTURE OF THE NUCLEON:
C         CTEQ5 PPARTON DISTRIBUTIONS"
C
C  hep-ph/9903282; to be published in Eur. Phys. J. C 1999.
C
C  These PDF's use quadratic interpolation of attached tables. A parametrized 
C  version of the same PDF's without external tables is under construction.  
C  They will become available later.
C
C   This package contains 7 sets of CTEQ5 PDF's; plus two updated ones.
C   The undated CTEQ5M1 and CTEQHQ1 use an improved evolution code.
C   Both the original and the updated ones fit current data with comparable
C   accuracy.  The CTEQHQ1 set also involve a different choice of scale,
C   hence differs from CTEQHQ slightly more.  It is preferred over CTEQ5HQ.

C   Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)  Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C   1    CTEQ5M   Standard MSbar scheme   0.118     326   226    cteq5m.tbl
C   2    CTEQ5D   Standard DIS scheme     0.118     326   226    cteq5d.tbl
C   3    CTEQ5L   Leading Order           0.127     192   146    cteq5l.tbl
C   4    CTEQ5HJ  Large-x gluon enhanced  0.118     326   226    cteq5hj.tbl
C   5    CTEQ5HQ  Heavy Quark             0.118     326   226    cteq5hq.tbl
C   6    CTEQ5F3  Nf=3 FixedFlavorNumber  0.106     (Lam3=395)   cteq5f3.tbl
C   7    CTEQ5F4  Nf=4 FixedFlavorNumber  0.112     309   XXX    cteq5f4.tbl
C         --------------------------------------------------------
C   8    CTEQ5M1  Improved CTEQ5M         0.118     326   226    cteq5m1.tbl
C   9    CTEQ5HQ1 Improved CTEQ5HQ        0.118     326   226    ctq5hq1.tbl
C ---------------------------------------------------------------------------
C   
C  The available applied range is 10^-5 << x << 1 and 1.0 << Q << 10,000 (GeV).
C   Lam5 (Lam4, Lam3) represents Lambda value (in MeV) for 5 (4,3) flavors. 
C   The matching alpha_s between 4 and 5 flavors takes place at Q=4.5 GeV,  
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C   
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq5(Iset) 
C   where Iset is the desired PDF specified in the above table.
C   
C   The function Ctq5Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton] 
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C      whereas CTEQ5F3 has, by definition, only 3 flavors and gluon;
C              CTEQ5F4 has only 4 flavors and gluon.
C   
C   For detailed information on the parameters used, e.q. quark masses, 
C   QCD Lambda, ... etc.,  see info lines at the beginning of the 
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single 
C   precision.
C   
C   If you have detailed questions concerning these CTEQ5 distributions, 
C   or if you find problems/bugs using this package, direct inquires to 
C   Hung-Liang Lai(lai@phys.nthu.edu.tw) or Wu-Ki Tung(Tung@pa.msu.edu).
C   
C===========================================================================

      Function Ctq5Pdf (Iparton, X, Q)
      Implicit Double Precision (A-H,O-Z)
      Logical Warn
      Common
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719QCDtable /  Alambda, Nfl, Iorder

      Data Warn /.true./
      save Warn

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
	Print *, 'X out of range in Ctq5Pdf: ', X
	Stop
      Endif
      If (Q .lt. Alambda) Then
	Print *, 'Q out of range in Ctq5Pdf: ', Q
	Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
	     Warn = .false.
	     Print *, 'Warning: Iparton out of range in Ctq5Pdf: '
     >              , Iparton
         Endif
         Ctq5Pdf = 0D0
         Return
      Endif

      Ctq5Pdf = Ctq5partonx (Iparton, X, Q)
      if(Ctq5Pdf.lt.0.D0)  Ctq5Pdf = 0.D0

      Return

C                             ********************
      End

      FUNCTION Ctq5partonx (IPRTN, X, Q)
C                     
C   Given the parton distribution function in the array Upd in
C   COMMON / K719CtqPar1 / , this routine fetches u(fl, x, q) at any value of
C   x and q using Mth-order polynomial interpolation for x and Ln(Q/Lambda).
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      PARAMETER (M= 2, M1 = M + 1)
C
      Logical First
      Common 
     > / K719CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719XQrange / Qini, Qmax, Xmin
      Dimension Fq(M1), Df(M1)
      data ixrange/0/
      data iqmnrng/0/
      data iqmxrng/0/
      save ixrange,iqmnrng,iqmxrng
C

      Data First /.true./
      save First
C                                                 Work with Log (Q)
      QG  = LOG (Q/AL)

C                           Find lower end of interval containing X
      JL = -1
      JU = Nx+1
 11   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (X .GT. XV(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif

      Jx = JL - (M-1)/2
      If (X .lt. Xmin .and. First ) Then
         First = .false.
         Print '(A, 2(1pE12.4))', 
     >     ' WARNING: X << Xmin, extrapolation used; X, Xmin =', X, Xmin
         If (Jx .LT. 0) Jx = 0
      Elseif (Jx .GT. Nx-M) Then
         Jx = Nx - M
      Endif
C                                    Find the interval where Q lies
      JL = -1
      JU = NT+1
 12   If (JU-JL .GT. 1) Then
         JM = (JU+JL) / 2
         If (QG .GT. QL(JM)) Then
            JL = JM
         Else
            JU = JM
         Endif
         Goto 12
      Endif

      Jq = JL - (M-1)/2
      If (Jq .LT. 0) Then
         Jq = 0
         If (Q .lt. Qini)  then
           iqmnrng=iqmnrng+1
           if(iqmnrng.eq.1) Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q < Qini, extrapolation used; Q, Qini =', Q, Qini
         endif
      Elseif (Jq .GT. Nt-M) Then
         Jq = Nt - M
         If (Q .gt. Qmax) then
           iqmxrng=iqmxrng+1
           if(iqmxrng.eq.1) Print '(A, 2(1pE12.4))', 
     >     ' WARNING: Q > Qmax, extrapolation used; Q, Qmax =', Q, Qmax
         endif
      Endif

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
C                             Find the off-set in the linear array Upd
      JFL = Ip + NfMx
      J0  = (JFL * (NT+1) + Jq) * (NX+1) + Jx
C
C                                           Now interpolate in x for M1 Q's
      Do 21 Iq = 1, M1
         J1 = J0 + (Nx+1)*(Iq-1) + 1
         Call Ctq5polint (XV(Jx), Upd(J1), M1, X, Fq(Iq), Df(Iq))
 21   Continue
C                                          Finish off by interpolating in Q
      Call Ctq5polint (QL(Jq), Fq(1), M1, QG, Ftmp, Ddf)

      Ctq5partonx = Ftmp
C
      RETURN
C                        ****************************
      END


      Subroutine SetCtq5 (Iset)
      Implicit Double Precision (A-H,O-Z)
      Parameter (Isetmax=9)
      Character Flnm(Isetmax)*12, Tablefile*40
      Data (Flnm(I), I=1,Isetmax)
     > / 'cteq5m', 'cteq5d', 'cteq5l', 'cteq5hj'
     > , 'cteq5hq', 'cteq5f3', 'cteq5f4'
     > , 'cteq5m1', 'ctq5hq1'  /
      Data Tablefile / 'test.tbl' /
      Data Isetold, Isetmin, Isettest / -987, 1, 911 /
      save

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
	 IU= Nctq5nextun()
         If (Iset .eq. Isettest) then
            Print* ,'Opening ', Tablefile
 21         Open(IU, File=Tablefile, Status='OLD', Err=101)
            GoTo 22
 101        Print*, Tablefile, ' cannot be opened '
            Print*, 'Please input the .tbl file:'
            Read (*,'(A)') Tablefile
            Goto 21
 22         Continue
         ElseIf (Iset.lt.Isetmin .or. Iset.gt.Isetmax) Then
	    Print *, 'Invalid Iset number in SetCtq5 :', Iset
	    Stop
         Else
            Tablefile=Flnm(Iset)
            Open(IU, File=Tablefile, Status='OLD', Err=100)
	 Endif
         Call Ctq5readtbl (IU)
         Close (IU)   
	 Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq5!!'
      Stop
C                             ********************
      End

      Subroutine Ctq5readtbl (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 105, MXQ = 25, MXF = 6)
      PARAMETER (MXPQX = (MXF *2 +2) * MXQ * MXX)
      Common 
     > / K719CtqPar1 / Al, XV(0:MXX), QL(0:MXQ), UPD(MXPQX)
     > / K719CtqPar2 / Nx, Nt, NfMx
     > / K719XQrange / Qini, Qmax, Xmin
     > / K719QCDtable /  Alambda, Nfl, Iorder
     > / K719Masstbl / Amass(6)
      
      Read  (Nu, '(A)') Line     
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line 
      Read  (Nu, *) NX,  NT, NfMx

      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (QL(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         QL(Iq) = Log (QL(Iq) /Al)
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage, 
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence) 

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End

      Function Nctq5nextun()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            Nctq5nextun = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
C

      SUBROUTINE CTQ5POLINT (XA,YA,N,X,Y,DY)
 
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes" 
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END

C   CTEQ5M1 and CTEQ5L Parton Distribution Functions in Parametrized Form
C                             
C               September 15, 1999
C
C   Ref: "GLOBAL QCD ANALYSIS OF PARTON STRUCTURE OF THE NUCLEON:
C         CTEQ5 PPARTON DISTRIBUTIONS"
C   hep-ph/9903282
C
C   The CTEQ5M1 set given here is an updated version of the original CTEQ5M
C     set posted, in the table version, on the Web page of CTEQ.
C     The differences between CTEQ5M and CTEQ5M1 are insignificant for almost
C     all applications. 
C   The improvement is in the QCD evolution which is now more accurate, and
C   which agrees completely with the benchmark work of the HERA 96/97 Workshop.

C   The differences between the parametrized and the corresponding table ver-
C sions (on which it is based) are of similar order as between the two version.
C    
C!! Because accurate parametrizations over a wide range of (x,Q) is hard to
C   obtain, only the most widely used sets CTEQ5M and CTEQ5L are available 
C   in parametrized form for now. 

C   These parametrizations were obtained by Jon Pumplin.
C
C                    ******************************
C  Iset   PDF        Description                 Alpha_s(Mz)  Lam4  Lam5
C ---------------------------------------------------------------------------
C   1    CTEQ5M1  Standard NLO MSbar scheme         0.118     326   226
C   3    CTEQ5L   Leading Order                     0.127     192   146
C ---------------------------------------------------------------------------
C   Note the Qcd-lambda values given for CTEQ5L is for the leading order
C     form of Alpha_s!!  Alpha_s(Mz) gives the absolute calibration.

C  The two Iset value are adopted to agree with the standard table versions.

C   The following user-callable routines are provided:
C 
C     FUNCTION Ctq5Pd (Iset, Iprtn, X, Q, Irt) 
C         returns the PROBABILITY density for a GIVEN flavor;
C
C     FUNCTION Ctq5df (Iset, Iprtn, X, Q, Irt)
C         returns the MOMENTUM density of a GIVEN valence or sea distribution.
C
C     SUBROUTINE Ctq5Pds(Iset, Pdf, X, Q, Irt)
C         returns an array of MOMENTUM densities for ALL flavors;
C
C   The arguments of these routines are as follows: 
C
C   Iset is the set number:  1 for CTEQ5M1 or 3 for CTEQ5L  
C
C   Iprtn  is the parton label (6, 5, 4, 3, 2, 1, 0, -1, ......, -6)
C                          for (t, b, c, s, d, u, g, u_bar, ..., t_bar)
C  *** WARNING: We use the parton label 2 as D-quark and 1 as U-quark, 
C               which might be different from your labels.
C
C   X, Q are the usual x, Q; 
C
C   Irt is an error code: 0 if there was no error; 1 or more if (x,q) was 
C   outside the range of validity of the parametrization.
C       
C  Range of validity:
C  
C     The range of (x, Q) covered by this parametrization of the QCD evolved
C     parton distributions is 1E-6 < x < 1 ; 1.1 GeV < Q < 10 TeV.  Of course,
C     the PDF's are constrained by data only in a subset of that region; and 
C     the assumed DGLAP evolution is unlikely to be valid for all of it either.
C
C     The range of (x, Q) used in the CTEQ5 round of global analysis is 
C     approximately 0.01 < x < 0.75 ; and 4 GeV^2 < Q^2 < 400 GeV^2 for 
C     fixed target experiments; 0.0001 < x < 0.3 from HERA data; and   
C     Q^2 up to 40,000 GeV^2 from Tevatron inclusive Jet data.
C
C   DOUBLE PRECISION is used throughout in these routines, but conversion to 
C   SINGLE PRECISION is possible by removing the Implicit Double Precision statements. 
C
C **************************************************************************

C ********************************************************
      FUNCTION CTQ5PD(ISET, IPARTON, X, Q, IRT)
C ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

c if called at a point (x,q) that is outside the region that was 
c actually parametrized, return a value of 0, and set the error code IRT=1.  
c The user can remove the following IF statement to receive instead an 
c extrapolated value, which may be wildly unphysical.
      if((x .lt. 1.e-6). or. (x .gt. 1.) 
     &	 .or. (q .lt. .99) .or. (q .gt. 10000.)) then
         ctq5pd = 0.d0
         irt = 1
         return
      endif

      irt = 0
      if(iset .eq. 3) then
         ctq5pd = ctq5L(iparton,x,q)
      elseif(iset .eq. 1) then
         ctq5pd = ctq5Mi(iparton,x,q)
      else
         print *,'iset=',iset,' has not been parametrized.' 
	   print '(/A)', 'Use the interpolation-table version instead.'
         stop
      endif

      return
      end

C ********************************************************
      FUNCTION CTQ5DF(ISET, IFL, X, Q, IRT)
C ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      CTQ5DF = X * CTQ5PD(ISET, IPARTON, X, Q, IRT)
        
      RETURN
      END

C ********************************************************
      SUBROUTINE CTQ5PDS(ISET, PDF, X, Q, IRT)
C ********************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION PDF (-6:6)

      IRT = 0

      DO IFL= -6,2
         PDF(IFL) = CTQ5PD(ISET,IFL,X,Q,IRT1)
         IRT = IRT + IRT1

         IF (IFL .LE. -3) THEN
            PDF(-IFL) = PDF(IFL)
         ENDIF

      ENDDO

      RETURN
      END

c --------------------------------------------------------------------------
	double precision function ctq5MI(ifl,x,q)
c Parametrization of cteq5MI parton distribution functions (J. Pumplin 9/99).
c ifl: 1=u,2=d,3=s,4=c,5=b;0=gluon;-1=ubar,-2=dbar,-3=sbar,-4=cbar,-5=bbar.
c --------------------------------------------------------------------------
	implicit double precision (a-h,o-z)
	integer ifl

	ii = ifl
	if(ii .gt. 2) then
	   ii = -ii
	endif

	if(ii .eq. -1) then
	   sum = faux5MI(-1,x,q)
	   ratio = faux5MI(-2,x,q)
	   ctq5MI = sum/(1.d0 + ratio)

	elseif(ii .eq. -2) then
	   sum = faux5MI(-1,x,q)
	   ratio = faux5MI(-2,x,q)
	   ctq5MI = sum*ratio/(1.d0 + ratio)

	elseif(ii .ge. -5) then
	   ctq5MI = faux5MI(ii,x,q)

	else
	   ctq5MI = 0.d0 

	endif

	return
	end

c ---------------------------------------------------------------------
      double precision function faux5MI(ifl,x,q)
c auxiliary function for parametrization of CTEQ5MI (J. Pumplin 9/99).
c ---------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      integer ifl

      parameter (nex=8, nlf=2)
      dimension am(0:nex,0:nlf,-5:2)
      dimension alfvec(-5:2), qmavec(-5:2)
      dimension mexvec(-5:2), mlfvec(-5:2)
      dimension ut1vec(-5:2), ut2vec(-5:2)
      dimension af(0:nex)

      data mexvec( 2) / 8 /
      data mlfvec( 2) / 2 /
      data ut1vec( 2) /  0.5141718E+01 /
      data ut2vec( 2) / -0.1346944E+01 /
      data alfvec( 2) /  0.5260555E+00 /
      data qmavec( 2) /  0.0000000E+00 /
      data (am( 0,k, 2),k=0, 2)
     & /  0.4289071E+01, -0.2536870E+01, -0.1259948E+01 /
      data (am( 1,k, 2),k=0, 2)
     & /  0.9839410E+00,  0.4168426E-01, -0.5018952E-01 /
      data (am( 2,k, 2),k=0, 2)
     & / -0.1651961E+02,  0.9246261E+01,  0.5996400E+01 /
      data (am( 3,k, 2),k=0, 2)
     & / -0.2077936E+02,  0.9786469E+01,  0.7656465E+01 /
      data (am( 4,k, 2),k=0, 2)
     & /  0.3054926E+02,  0.1889536E+01,  0.1380541E+01 /
      data (am( 5,k, 2),k=0, 2)
     & /  0.3084695E+02, -0.1212303E+02, -0.1053551E+02 /
      data (am( 6,k, 2),k=0, 2)
     & / -0.1426778E+02,  0.6239537E+01,  0.5254819E+01 /
      data (am( 7,k, 2),k=0, 2)
     & / -0.1909811E+02,  0.3695678E+01,  0.5495729E+01 /
      data (am( 8,k, 2),k=0, 2)
     & /  0.1889751E-01,  0.5027193E-02,  0.6624896E-03 /

      data mexvec( 1) / 8 /
      data mlfvec( 1) / 2 /
      data ut1vec( 1) /  0.4138426E+01 /
      data ut2vec( 1) / -0.3221374E+01 /
      data alfvec( 1) /  0.4960962E+00 /
      data qmavec( 1) /  0.0000000E+00 /
      data (am( 0,k, 1),k=0, 2)
     & /  0.1332497E+01, -0.3703718E+00,  0.1288638E+00 /
      data (am( 1,k, 1),k=0, 2)
     & /  0.7544687E+00,  0.3255075E-01, -0.4706680E-01 /
      data (am( 2,k, 1),k=0, 2)
     & / -0.7638814E+00,  0.5008313E+00, -0.9237374E-01 /
      data (am( 3,k, 1),k=0, 2)
     & / -0.3689889E+00, -0.1055098E+01, -0.4645065E+00 /
      data (am( 4,k, 1),k=0, 2)
     & /  0.3991610E+02,  0.1979881E+01,  0.1775814E+01 /
      data (am( 5,k, 1),k=0, 2)
     & /  0.6201080E+01,  0.2046288E+01,  0.3804571E+00 /
      data (am( 6,k, 1),k=0, 2)
     & / -0.8027900E+00, -0.7011688E+00, -0.8049612E+00 /
      data (am( 7,k, 1),k=0, 2)
     & / -0.8631305E+01, -0.3981200E+01,  0.6970153E+00 /
      data (am( 8,k, 1),k=0, 2)
     & /  0.2371230E-01,  0.5372683E-02,  0.1118701E-02 /

      data mexvec( 0) / 8 /
      data mlfvec( 0) / 2 /
      data ut1vec( 0) / -0.1026789E+01 /
      data ut2vec( 0) / -0.9051707E+01 /
      data alfvec( 0) /  0.9462977E+00 /
      data qmavec( 0) /  0.0000000E+00 /
      data (am( 0,k, 0),k=0, 2)
     & /  0.1191990E+03, -0.8548739E+00, -0.1963040E+01 /
      data (am( 1,k, 0),k=0, 2)
     & / -0.9449972E+02,  0.1074771E+01,  0.2056055E+01 /
      data (am( 2,k, 0),k=0, 2)
     & /  0.3701064E+01, -0.1167947E-02,  0.1933573E+00 /
      data (am( 3,k, 0),k=0, 2)
     & /  0.1171345E+03, -0.1064540E+01, -0.1875312E+01 /
      data (am( 4,k, 0),k=0, 2)
     & / -0.1014453E+03, -0.5707427E+00,  0.4511242E-01 /
      data (am( 5,k, 0),k=0, 2)
     & /  0.6365168E+01,  0.1275354E+01, -0.4964081E+00 /
      data (am( 6,k, 0),k=0, 2)
     & / -0.3370693E+01, -0.1122020E+01,  0.5947751E-01 /
      data (am( 7,k, 0),k=0, 2)
     & / -0.5327270E+01, -0.9293556E+00,  0.6629940E+00 /
      data (am( 8,k, 0),k=0, 2)
     & /  0.2437513E-01,  0.1600939E-02,  0.6855336E-03 /

      data mexvec(-1) / 8 /
      data mlfvec(-1) / 2 /
      data ut1vec(-1) /  0.5243571E+01 /
      data ut2vec(-1) / -0.2870513E+01 /
      data alfvec(-1) /  0.6701448E+00 /
      data qmavec(-1) /  0.0000000E+00 /
      data (am( 0,k,-1),k=0, 2)
     & /  0.2428863E+02,  0.1907035E+01, -0.4606457E+00 /
      data (am( 1,k,-1),k=0, 2)
     & /  0.2006810E+01, -0.1265915E+00,  0.7153556E-02 /
      data (am( 2,k,-1),k=0, 2)
     & / -0.1884546E+02, -0.2339471E+01,  0.5740679E+01 /
      data (am( 3,k,-1),k=0, 2)
     & / -0.2527892E+02, -0.2044124E+01,  0.1280470E+02 /
      data (am( 4,k,-1),k=0, 2)
     & / -0.1013824E+03, -0.1594199E+01,  0.2216401E+00 /
      data (am( 5,k,-1),k=0, 2)
     & /  0.8070930E+02,  0.1792072E+01, -0.2164364E+02 /
      data (am( 6,k,-1),k=0, 2)
     & / -0.4641050E+02,  0.1977338E+00,  0.1273014E+02 /
      data (am( 7,k,-1),k=0, 2)
     & / -0.3910568E+02,  0.1719632E+01,  0.1086525E+02 /
      data (am( 8,k,-1),k=0, 2)
     & / -0.1185496E+01, -0.1905847E+00, -0.8744118E-03 /

      data mexvec(-2) / 7 /
      data mlfvec(-2) / 2 /
      data ut1vec(-2) /  0.4782210E+01 /
      data ut2vec(-2) / -0.1976856E+02 /
      data alfvec(-2) /  0.7558374E+00 /
      data qmavec(-2) /  0.0000000E+00 /
      data (am( 0,k,-2),k=0, 2)
     & / -0.6216935E+00,  0.2369963E+00, -0.7909949E-02 /
      data (am( 1,k,-2),k=0, 2)
     & /  0.1245440E+01, -0.1031510E+00,  0.4916523E-02 /
      data (am( 2,k,-2),k=0, 2)
     & / -0.7060824E+01, -0.3875283E-01,  0.1784981E+00 /
      data (am( 3,k,-2),k=0, 2)
     & / -0.7430595E+01,  0.1964572E+00, -0.1284999E+00 /
      data (am( 4,k,-2),k=0, 2)
     & / -0.6897810E+01,  0.2620543E+01,  0.8012553E-02 /
      data (am( 5,k,-2),k=0, 2)
     & /  0.1507713E+02,  0.2340307E-01,  0.2482535E+01 /
      data (am( 6,k,-2),k=0, 2)
     & / -0.1815341E+01, -0.1538698E+01, -0.2014208E+01 /
      data (am( 7,k,-2),k=0, 2)
     & / -0.2571932E+02,  0.2903941E+00, -0.2848206E+01 /

      data mexvec(-3) / 7 /
      data mlfvec(-3) / 2 /
      data ut1vec(-3) /  0.4518239E+01 /
      data ut2vec(-3) / -0.2690590E+01 /
      data alfvec(-3) /  0.6124079E+00 /
      data qmavec(-3) /  0.0000000E+00 /
      data (am( 0,k,-3),k=0, 2)
     & / -0.2734458E+01, -0.7245673E+00, -0.6351374E+00 /
      data (am( 1,k,-3),k=0, 2)
     & /  0.2927174E+01,  0.4822709E+00, -0.1088787E-01 /
      data (am( 2,k,-3),k=0, 2)
     & / -0.1771017E+02, -0.1416635E+01,  0.8467622E+01 /
      data (am( 3,k,-3),k=0, 2)
     & / -0.4972782E+02, -0.3348547E+01,  0.1767061E+02 /
      data (am( 4,k,-3),k=0, 2)
     & / -0.7102770E+01, -0.3205337E+01,  0.4101704E+00 /
      data (am( 5,k,-3),k=0, 2)
     & /  0.7169698E+02, -0.2205985E+01, -0.2463931E+02 /
      data (am( 6,k,-3),k=0, 2)
     & / -0.4090347E+02,  0.2103486E+01,  0.1416507E+02 /
      data (am( 7,k,-3),k=0, 2)
     & / -0.2952639E+02,  0.5376136E+01,  0.7825585E+01 /

      data mexvec(-4) / 7 /
      data mlfvec(-4) / 2 /
      data ut1vec(-4) /  0.2783230E+01 /
      data ut2vec(-4) / -0.1746328E+01 /
      data alfvec(-4) /  0.1115653E+01 /
      data qmavec(-4) /  0.1300000E+01 /
      data (am( 0,k,-4),k=0, 2)
     & / -0.1743872E+01, -0.1128921E+01, -0.2841969E+00 /
      data (am( 1,k,-4),k=0, 2)
     & /  0.3345755E+01,  0.3187765E+00,  0.1378124E+00 /
      data (am( 2,k,-4),k=0, 2)
     & / -0.2037615E+02,  0.4121687E+01,  0.2236520E+00 /
      data (am( 3,k,-4),k=0, 2)
     & / -0.4703104E+02,  0.5353087E+01, -0.1455347E+01 /
      data (am( 4,k,-4),k=0, 2)
     & / -0.1060230E+02, -0.1551122E+01, -0.1078863E+01 /
      data (am( 5,k,-4),k=0, 2)
     & /  0.5088892E+02, -0.8197304E+01,  0.8083451E+01 /
      data (am( 6,k,-4),k=0, 2)
     & / -0.2819070E+02,  0.4554086E+01, -0.5890995E+01 /
      data (am( 7,k,-4),k=0, 2)
     & / -0.1098238E+02,  0.2590096E+01, -0.8062879E+01 /

      data mexvec(-5) / 6 /
      data mlfvec(-5) / 2 /
      data ut1vec(-5) /  0.1619654E+02 /
      data ut2vec(-5) / -0.3367346E+01 /
      data alfvec(-5) /  0.5109891E-02 /
      data qmavec(-5) /  0.4500000E+01 /
      data (am( 0,k,-5),k=0, 2)
     & / -0.6800138E+01,  0.2493627E+01, -0.1075724E+01 /
      data (am( 1,k,-5),k=0, 2)
     & /  0.3036555E+01,  0.3324733E+00,  0.2008298E+00 /
      data (am( 2,k,-5),k=0, 2)
     & / -0.5203879E+01, -0.8493476E+01, -0.4523208E+01 /
      data (am( 3,k,-5),k=0, 2)
     & / -0.1524239E+01, -0.3411912E+01, -0.1771867E+02 /
      data (am( 4,k,-5),k=0, 2)
     & / -0.1099444E+02,  0.1320930E+01, -0.2353831E+01 /
      data (am( 5,k,-5),k=0, 2)
     & /  0.1699299E+02, -0.3565802E+02,  0.3566872E+02 /
      data (am( 6,k,-5),k=0, 2)
     & / -0.1465793E+02,  0.2703365E+02, -0.2176372E+02 /

      if(q .le. qmavec(ifl)) then
         faux5MI = 0.d0
         return
      endif

      if(x .ge. 1.d0) then
         faux5MI = 0.d0
         return
      endif

      tmp = log(q/alfvec(ifl))
      if(tmp .le. 0.d0) then
         faux5MI = 0.d0
         return
      endif

      sb = log(tmp)
      sb1 = sb - 1.2d0
      sb2 = sb1*sb1

      do i = 0, nex
         af(i) = 0.d0
         sbx = 1.d0
         do k = 0, mlfvec(ifl)
            af(i) = af(i) + sbx*am(i,k,ifl)
            sbx = sb1*sbx
         enddo
      enddo

      y = -log(x)
      u = log(x/0.00001d0)

      part1 = af(1)*y**(1.d0+0.01d0*af(4))*(1.d0+ af(8)*u)
      part2 = af(0)*(1.d0 - x) + af(3)*x 
      part3 = x*(1.d0-x)*(af(5)+af(6)*(1.d0-x)+af(7)*x*(1.d0-x))
      part4 = ut1vec(ifl)*log(1.d0-x) + 
     &	      AF(2)*log(1.d0+exp(ut2vec(ifl))-x)

      faux5MI = exp(log(x) + part1 + part2 + part3 + part4)

c include threshold factor...
      faux5MI = faux5MI * (1.d0 - qmavec(ifl)/q)

      return
      end
c --------------------------------------------------------------------------
	double precision function ctq5L(ifl,x,q)
c Parametrization of cteq5L parton distribution functions (J. Pumplin 9/99).
c ifl: 1=u,2=d,3=s,4=c,5=b;0=gluon;-1=ubar,-2=dbar,-3=sbar,-4=cbar,-5=bbar.
c --------------------------------------------------------------------------
	implicit double precision (a-h,o-z)
	integer ifl

	ii = ifl
	if(ii .gt. 2) then
	   ii = -ii
	endif

	if(ii .eq. -1) then
	   sum = faux5L(-1,x,q)
	   ratio = faux5L(-2,x,q)
	   ctq5L = sum/(1.d0 + ratio)

	elseif(ii .eq. -2) then
	   sum = faux5L(-1,x,q)
	   ratio = faux5L(-2,x,q)
	   ctq5L = sum*ratio/(1.d0 + ratio)

	elseif(ii .ge. -5) then
	   ctq5L = faux5L(ii,x,q)

	else
	   ctq5L = 0.d0 

	endif

	return
	end

c ---------------------------------------------------------------------
      double precision function faux5L(ifl,x,q)
c auxiliary function for parametrization of CTEQ5L (J. Pumplin 9/99).
c ---------------------------------------------------------------------
      implicit double precision (a-h,o-z)
      integer ifl

      parameter (nex=8, nlf=2)
      dimension am(0:nex,0:nlf,-5:2)
      dimension alfvec(-5:2), qmavec(-5:2)
      dimension mexvec(-5:2), mlfvec(-5:2)
      dimension ut1vec(-5:2), ut2vec(-5:2)
      dimension af(0:nex)

      data mexvec( 2) / 8 /
      data mlfvec( 2) / 2 /
      data ut1vec( 2) /  0.4971265E+01 /
      data ut2vec( 2) / -0.1105128E+01 /
      data alfvec( 2) /  0.2987216E+00 /
      data qmavec( 2) /  0.0000000E+00 /
      data (am( 0,k, 2),k=0, 2)
     & /  0.5292616E+01, -0.2751910E+01, -0.2488990E+01 /
      data (am( 1,k, 2),k=0, 2)
     & /  0.9714424E+00,  0.1011827E-01, -0.1023660E-01 /
      data (am( 2,k, 2),k=0, 2)
     & / -0.1651006E+02,  0.7959721E+01,  0.8810563E+01 /
      data (am( 3,k, 2),k=0, 2)
     & / -0.1643394E+02,  0.5892854E+01,  0.9348874E+01 /
      data (am( 4,k, 2),k=0, 2)
     & /  0.3067422E+02,  0.4235796E+01, -0.5112136E+00 /
      data (am( 5,k, 2),k=0, 2)
     & /  0.2352526E+02, -0.5305168E+01, -0.1169174E+02 /
      data (am( 6,k, 2),k=0, 2)
     & / -0.1095451E+02,  0.3006577E+01,  0.5638136E+01 /
      data (am( 7,k, 2),k=0, 2)
     & / -0.1172251E+02, -0.2183624E+01,  0.4955794E+01 /
      data (am( 8,k, 2),k=0, 2)
     & /  0.1662533E-01,  0.7622870E-02, -0.4895887E-03 /

      data mexvec( 1) / 8 /
      data mlfvec( 1) / 2 /
      data ut1vec( 1) /  0.2612618E+01 /
      data ut2vec( 1) / -0.1258304E+06 /
      data alfvec( 1) /  0.3407552E+00 /
      data qmavec( 1) /  0.0000000E+00 /
      data (am( 0,k, 1),k=0, 2)
     & /  0.9905300E+00, -0.4502235E+00,  0.1624441E+00 /
      data (am( 1,k, 1),k=0, 2)
     & /  0.8867534E+00,  0.1630829E-01, -0.4049085E-01 /
      data (am( 2,k, 1),k=0, 2)
     & /  0.8547974E+00,  0.3336301E+00,  0.1371388E+00 /
      data (am( 3,k, 1),k=0, 2)
     & /  0.2941113E+00, -0.1527905E+01,  0.2331879E+00 /
      data (am( 4,k, 1),k=0, 2)
     & /  0.3384235E+02,  0.3715315E+01,  0.8276930E+00 /
      data (am( 5,k, 1),k=0, 2)
     & /  0.6230115E+01,  0.3134639E+01, -0.1729099E+01 /
      data (am( 6,k, 1),k=0, 2)
     & / -0.1186928E+01, -0.3282460E+00,  0.1052020E+00 /
      data (am( 7,k, 1),k=0, 2)
     & / -0.8545702E+01, -0.6247947E+01,  0.3692561E+01 /
      data (am( 8,k, 1),k=0, 2)
     & /  0.1724598E-01,  0.7120465E-02,  0.4003646E-04 /

      data mexvec( 0) / 8 /
      data mlfvec( 0) / 2 /
      data ut1vec( 0) / -0.4656819E+00 /
      data ut2vec( 0) / -0.2742390E+03 /
      data alfvec( 0) /  0.4491863E+00 /
      data qmavec( 0) /  0.0000000E+00 /
      data (am( 0,k, 0),k=0, 2)
     & /  0.1193572E+03, -0.3886845E+01, -0.1133965E+01 /
      data (am( 1,k, 0),k=0, 2)
     & / -0.9421449E+02,  0.3995885E+01,  0.1607363E+01 /
      data (am( 2,k, 0),k=0, 2)
     & /  0.4206383E+01,  0.2485954E+00,  0.2497468E+00 /
      data (am( 3,k, 0),k=0, 2)
     & /  0.1210557E+03, -0.3015765E+01, -0.1423651E+01 /
      data (am( 4,k, 0),k=0, 2)
     & / -0.1013897E+03, -0.7113478E+00,  0.2621865E+00 /
      data (am( 5,k, 0),k=0, 2)
     & / -0.1312404E+01, -0.9297691E+00, -0.1562531E+00 /
      data (am( 6,k, 0),k=0, 2)
     & /  0.1627137E+01,  0.4954111E+00, -0.6387009E+00 /
      data (am( 7,k, 0),k=0, 2)
     & /  0.1537698E+00, -0.2487878E+00,  0.8305947E+00 /
      data (am( 8,k, 0),k=0, 2)
     & /  0.2496448E-01,  0.2457823E-02,  0.8234276E-03 /

      data mexvec(-1) / 8 /
      data mlfvec(-1) / 2 /
      data ut1vec(-1) /  0.3862583E+01 /
      data ut2vec(-1) / -0.1265969E+01 /
      data alfvec(-1) /  0.2457668E+00 /
      data qmavec(-1) /  0.0000000E+00 /
      data (am( 0,k,-1),k=0, 2)
     & /  0.2647441E+02,  0.1059277E+02, -0.9176654E+00 /
      data (am( 1,k,-1),k=0, 2)
     & /  0.1990636E+01,  0.8558918E-01,  0.4248667E-01 /
      data (am( 2,k,-1),k=0, 2)
     & / -0.1476095E+02, -0.3276255E+02,  0.1558110E+01 /
      data (am( 3,k,-1),k=0, 2)
     & / -0.2966889E+01, -0.3649037E+02,  0.1195914E+01 /
      data (am( 4,k,-1),k=0, 2)
     & / -0.1000519E+03, -0.2464635E+01,  0.1964849E+00 /
      data (am( 5,k,-1),k=0, 2)
     & /  0.3718331E+02,  0.4700389E+02, -0.2772142E+01 /
      data (am( 6,k,-1),k=0, 2)
     & / -0.1872722E+02, -0.2291189E+02,  0.1089052E+01 /
      data (am( 7,k,-1),k=0, 2)
     & / -0.1628146E+02, -0.1823993E+02,  0.2537369E+01 /
      data (am( 8,k,-1),k=0, 2)
     & / -0.1156300E+01, -0.1280495E+00,  0.5153245E-01 /

      data mexvec(-2) / 7 /
      data mlfvec(-2) / 2 /
      data ut1vec(-2) /  0.1895615E+00 /
      data ut2vec(-2) / -0.3069097E+01 /
      data alfvec(-2) /  0.5293999E+00 /
      data qmavec(-2) /  0.0000000E+00 /
      data (am( 0,k,-2),k=0, 2)
     & / -0.6556775E+00,  0.2490190E+00,  0.3966485E-01 /
      data (am( 1,k,-2),k=0, 2)
     & /  0.1305102E+01, -0.1188925E+00, -0.4600870E-02 /
      data (am( 2,k,-2),k=0, 2)
     & / -0.2371436E+01,  0.3566814E+00, -0.2834683E+00 /
      data (am( 3,k,-2),k=0, 2)
     & / -0.6152826E+01,  0.8339877E+00, -0.7233230E+00 /
      data (am( 4,k,-2),k=0, 2)
     & / -0.8346558E+01,  0.2892168E+01,  0.2137099E+00 /
      data (am( 5,k,-2),k=0, 2)
     & /  0.1279530E+02,  0.1021114E+00,  0.5787439E+00 /
      data (am( 6,k,-2),k=0, 2)
     & /  0.5858816E+00, -0.1940375E+01, -0.4029269E+00 /
      data (am( 7,k,-2),k=0, 2)
     & / -0.2795725E+02, -0.5263392E+00,  0.1290229E+01 /

      data mexvec(-3) / 7 /
      data mlfvec(-3) / 2 /
      data ut1vec(-3) /  0.3753257E+01 /
      data ut2vec(-3) / -0.1113085E+01 /
      data alfvec(-3) /  0.3713141E+00 /
      data qmavec(-3) /  0.0000000E+00 /
      data (am( 0,k,-3),k=0, 2)
     & /  0.1580931E+01, -0.2273826E+01, -0.1822245E+01 /
      data (am( 1,k,-3),k=0, 2)
     & /  0.2702644E+01,  0.6763243E+00,  0.7231586E-02 /
      data (am( 2,k,-3),k=0, 2)
     & / -0.1857924E+02,  0.3907500E+01,  0.5850109E+01 /
      data (am( 3,k,-3),k=0, 2)
     & / -0.3044793E+02,  0.2639332E+01,  0.5566644E+01 /
      data (am( 4,k,-3),k=0, 2)
     & / -0.4258011E+01, -0.5429244E+01,  0.4418946E+00 /
      data (am( 5,k,-3),k=0, 2)
     & /  0.3465259E+02, -0.5532604E+01, -0.4904153E+01 /
      data (am( 6,k,-3),k=0, 2)
     & / -0.1658858E+02,  0.2923275E+01,  0.2266286E+01 /
      data (am( 7,k,-3),k=0, 2)
     & / -0.1149263E+02,  0.2877475E+01, -0.7999105E+00 /

      data mexvec(-4) / 7 /
      data mlfvec(-4) / 2 /
      data ut1vec(-4) /  0.4400772E+01 /
      data ut2vec(-4) / -0.1356116E+01 /
      data alfvec(-4) /  0.3712017E-01 /
      data qmavec(-4) /  0.1300000E+01 /
      data (am( 0,k,-4),k=0, 2)
     & / -0.8293661E+00, -0.3982375E+01, -0.6494283E-01 /
      data (am( 1,k,-4),k=0, 2)
     & /  0.2754618E+01,  0.8338636E+00, -0.6885160E-01 /
      data (am( 2,k,-4),k=0, 2)
     & / -0.1657987E+02,  0.1439143E+02, -0.6887240E+00 /
      data (am( 3,k,-4),k=0, 2)
     & / -0.2800703E+02,  0.1535966E+02, -0.7377693E+00 /
      data (am( 4,k,-4),k=0, 2)
     & / -0.6460216E+01, -0.4783019E+01,  0.4913297E+00 /
      data (am( 5,k,-4),k=0, 2)
     & /  0.3141830E+02, -0.3178031E+02,  0.7136013E+01 /
      data (am( 6,k,-4),k=0, 2)
     & / -0.1802509E+02,  0.1862163E+02, -0.4632843E+01 /
      data (am( 7,k,-4),k=0, 2)
     & / -0.1240412E+02,  0.2565386E+02, -0.1066570E+02 /

      data mexvec(-5) / 6 /
      data mlfvec(-5) / 2 /
      data ut1vec(-5) /  0.5562568E+01 /
      data ut2vec(-5) / -0.1801317E+01 /
      data alfvec(-5) /  0.4952010E-02 /
      data qmavec(-5) /  0.4500000E+01 /
      data (am( 0,k,-5),k=0, 2)
     & / -0.6031237E+01,  0.1992727E+01, -0.1076331E+01 /
      data (am( 1,k,-5),k=0, 2)
     & /  0.2933912E+01,  0.5839674E+00,  0.7509435E-01 /
      data (am( 2,k,-5),k=0, 2)
     & / -0.8284919E+01,  0.1488593E+01, -0.8251678E+00 /
      data (am( 3,k,-5),k=0, 2)
     & / -0.1925986E+02,  0.2805753E+01, -0.3015446E+01 /
      data (am( 4,k,-5),k=0, 2)
     & / -0.9480483E+01, -0.9767837E+00, -0.1165544E+01 /
      data (am( 5,k,-5),k=0, 2)
     & /  0.2193195E+02, -0.1788518E+02,  0.9460908E+01 /
      data (am( 6,k,-5),k=0, 2)
     & / -0.1327377E+02,  0.1201754E+02, -0.6277844E+01 /

      if(q .le. qmavec(ifl)) then
         faux5L = 0.d0
         return
      endif

      if(x .ge. 1.d0) then
         faux5L = 0.d0
         return
      endif

      tmp = log(q/alfvec(ifl))
      if(tmp .le. 0.d0) then
         faux5L = 0.d0
         return
      endif

      sb = log(tmp)
      sb1 = sb - 1.2d0
      sb2 = sb1*sb1

      do i = 0, nex
         af(i) = 0.d0
         sbx = 1.d0
         do k = 0, mlfvec(ifl)
            af(i) = af(i) + sbx*am(i,k,ifl)
            sbx = sb1*sbx
         enddo
      enddo

      y = -log(x)
      u = log(x/0.00001d0)

      part1 = af(1)*y**(1.d0+0.01d0*af(4))*(1.d0+ af(8)*u)
      part2 = af(0)*(1.d0 - x) + af(3)*x 
      part3 = x*(1.d0-x)*(af(5)+af(6)*(1.d0-x)+af(7)*x*(1.d0-x))
      part4 = ut1vec(ifl)*log(1.d0-x) + 
     &	      AF(2)*log(1.d0+exp(ut2vec(ifl))-x)

      faux5L = exp(log(x) + part1 + part2 + part3 + part4)

c include threshold factor...
      faux5L = faux5L * (1.d0 - qmavec(ifl)/q)

      return
      end


C--- END CTEQ5 FITS -----
C    
C-- WEIZSAKER AND WILLIAMS DISTRIBUTION
C----------------------------------------------------------------------
      subroutine wwpdf(xs,q2s,ys,wwwgts)
c  this routine extracts a photon from an electron with momentum fraction
c  y>x at a scale q2. The momentum fraction is distributed according to the 
c  Weitzsaker-Williams shape. WWWGT is the probability that a photon with 
c  momentum fraction > x is emitted at the scale q2. The outine needs a 
c  minimum and maximum value for the range within which x is expected to vary
c  in the desired process. We expect xmin=(min q2)/shad and xmax=1-xme2/max(q2)
c  The program is protected against x=1, so xmax=1 is allowed.
C----------------------------------------------------------------------        
      implicit real * 8 (a-h,o-z)                                           
      real*8 ln10,lgx
      real*4 xs,q2s,ys,wwwgts
      dimension z(0:100),f1int(0:100),f2int(0:100),ftot(0:100)
      common/k719wwdata/xwwmin,xwwmax
      parameter (alfaem=1.d0/137)
      parameter (pi=3.14159265358979312D0)
      parameter (aemo2pi=alfaem/(2*pi))
      parameter (xme = 0.511d-3)
      data e0/0/,jseed/1/,nbin/100/             
      x=dble(xs)
      q2=dble(q2s)
c initialization: evaluates integral of the gamma<-e distribution function (WW)
      if(ini.eq.0) then
        ini=1                        
        if(xwwmin.eq.0.d0) xwwmin=1.d-4
        if(xwwmax.eq.0.d0) xwwmax=1.d0 
        zmin=log10(xwwmin)             
        zmax=log10(xwwmax)
        zrange=zmax-zmin
        dz=zrange/nbin
        z(0)=zmin
        f1int(0)=0
        f2int(0)=0
c perform the integral in the variable log10(x)
c It requires the jacobian d(log10(x)) = 1/log(10) d(log(x)) = 1/log(10) dx/x 
        ln10=log(10.d0)
        do i=1,nbin
          z(i)=z(i-1)+dz
          f1int(i)=f1int(i-1)
          f2int(i)=f2int(i-1)
          do j=1,10  
2           xx= random(jseed)*dz+z(i-1) 
            xx=10.d0**(xx)
            if(xx.eq.1.d0) go to 2
            f2=aemo2pi*(1+(1-xx)**2)
            f1=f2*log((1-xx)/xx**2)    
            f1int(i)=f1int(i)+f1/ln10/10.
            f2int(i)=f2int(i)+f2/ln10/10.
c divide by 10 to take the average within the dz integration bin
          enddo                     
        enddo
        xme2=xme**2
      endif
c inverts the integral function
c first find location of x and relative integral of the WW from xmin to x
      lgx=log10(x)
      nmin=0
      nmax=nbin
10    n=(nmax+nmin)/2
      if(nmin.eq.n) then
        nmax=n+1
        go to 20
      endif
      if(lgx.gt.z(n)) then
        nmin=n                      
        go to 10
      elseif(lgx.lt.z(n)) then
        nmax=n     
        go to 10
      elseif(lgx.eq.z(n)) then
        nmin=n
        nmax=n
        go to 20
      endif
20    continue
      xlogq2=log(q2/xme2)
      do i=nmin,nbin
        ftot(i)=f1int(i)+f2int(i)*xlogq2
c protect against  q2/xm2<x2/(1-x) , by forcing ftot(x>xmax)=f(xmax)
        if(ftot(i).lt.ftot(i-1)) ftot(i)=ftot(i-1)                  
      enddo
      ftot1=ftot(nbin)
      ftotx=ftot(nmin)+(lgx-z(nmin))*(ftot(nmax)-ftot(nmin))/dz
      do i=nmin,nbin      
        ftot(i)=(ftot(i)-ftotx)/(ftot(nbin)-ftotx)
      enddo                                       
c now the WW distribution is normalized to 1 over the (x,xmax) range.
c Generate a random number between 0 and 1 and find by linear interpolation the
c value of y such that ftot(y)=rn
      rn=random(jseed)                                           
      nmax=nbin
30    n=(nmax+nmin)/2
      if(nmin.eq.n) then
        rn=z(n)+dz*(rn-ftot(n))/(ftot(n+1)-ftot(n))
	y=10.d0**(rn)
        wwwgt=ftot1-ftotx
        go to 40
      endif              
      if(rn.gt.ftot(n)) then
        nmin=n
        go to 30
      elseif(rn.lt.ftot(n)) then
        nmax=n   
        go to 30
      elseif(rn.eq.ftot(n)) then
        rn=z(n)           
	y=10.d0**(rn)
        wwwgt=ftot1-ftotx
        go to 40
      endif
40    ys=sngl(y)
      wwwgts=sngl(wwwgt)
      end
C DREES AND GRASSIE PHOTON
C--------------------------------------------------------
      SUBROUTINE PHOPDF(Q2,X,FX,NLF)
C PHOTON PDFS           
C--------------------------------------------------------
      REAL FX(-NLF:NLF)
C--------------------------------------------------
C         nf=3 for   1< Q2 <32  GeV2
C         nf=4 for  32< Q2 <200 GeV2
C         nf=5 for 200< Q2 <1D4 GeV2
C--------------------------------------------------
C Thresholds are chosen for consistency with PDFLIB 4.17
      IF(Q2.LT.32.D0)THEN
        NF=3
      ELSEIF(Q2.LT.200.D0)THEN
        NF=4
      ELSE
        NF=5
      ENDIF
      DQ=PHDGQ(X,Q2,NF,1)/X
      UQ=PHDGQ(X,Q2,NF,2)/X
      GL=PHDGG(X,Q2,NF)/X                        
      IF(NLF.GE.1) FX(1)=UQ
      IF(NLF.GE.2) FX(2)=DQ
      IF(NLF.GE.3) FX(3)=DQ
      IF(NLF.GE.4) FX(4)=UQ
      IF(NLF.GE.5) FX(5)=DQ
      FX(0)=GL
      DO I=1,NLF
        FX(-I)=FX(I)
      ENDDO
      END
c*-- Author :    Drees, Grassie, Charchula, modified by Bryan Webber
C ===============================================================
C  DREES & GRASSIE PARAMETRIZATION OF PHOTON STRUCTURE FUNCTION
C
C    PHDGQ(X,Q2,NFL,NCH) - X*QUARK_IN_PHOTON           
C    PHDGG(X,Q2,NFL)     - X*GLUON_IN_PHOTON
C WHERE:                                    
C        (INTEGER) NCH - QUARK CHARGE: 1 FOR 1/3
C                                      2 FOR 2/3
C        (INTEGER) NFL - NUMBER OF QUARK FLAVOURS /3 OR 4/
C                   Q2 - SQUARE OF MOMENTUM Q /IN GEV2/
C                   X  - LONGITUDINAL FRACTION
C  LAMBDA=0.4 GEV
C
C       NFL=3:     1 < Q2 < 50   GEV^2
C       NFL=4:    20 < Q2 < 500  GEV^2
C       NFL=5:   200 < Q2 < 10^4 GEV^2
C
C
C  KRZYSZTOF CHARCHULA  /14.02.1989/
C================================================================
C
C PS. Note that for the case of three flavors, one has to add
C the QPM charm contribution for getting F2.
C
C================================================================
C MODIFIED FOR HERWIG BY BRW 19/4/91
C--- -----------------------------------------------
C        GLUON PART OF THE PHOTON SF
C--- -----------------------------------------------
      FUNCTION PHDGG(X,Q2,NFL)
      IMPLICIT REAL (A-H,P-Z)
      INTEGER NFL
      DIMENSION A(3,4,3),AT(3)
      ALAM2=0.160
      T=LOG(Q2/ALAM2)
C- ---  CHECK WHETHER NFL  HAVE RIGHT VALUES -----
      IF (.NOT.((NFL.EQ.3).OR.(NFL.EQ.4).OR.(NFL.EQ.5))) THEN
 130   WRITE(*,131)
 131   FORMAT('NUMBER OF FLAVOURS(NFL) HAS NOT BEEN SET TO: 3,4 OR 5;'/
     *'          NFL=3 IS ASSUMED')
       NFL=3
      ENDIF
C ------ INITIALIZATION OF PARAMETERS ARRAY -----
      DATA(((A(I,J,K),I=1,3),J=1,4),K=1,3)/
     + -0.20700,-0.19870, 5.11900,
     +  0.61580, 0.62570,-0.27520,
     +  1.07400, 8.35200,-6.99300,
     +  0.00000, 5.02400, 2.29800,
     +    0.8926E-2, 0.05090,-0.23130,
     +    0.659400, 0.27740, 0.13820,
     +    0.476600,-0.39060, 6.54200,
     +    0.019750,-0.32120, 0.51620,
     +  0.031970, -0.618E-2, -0.1216,
     +  1.0180,    0.94760,  0.90470,
     +  0.24610,  -0.60940,  2.6530,
     +  0.027070, -0.010670, 0.2003E-2/
C ------ Q2 DEPENDENCE -----------
      LF=NFL-2
      DO 20 I=1,3
        AT(I)=A(I,1,LF)*T**A(I,2,LF)+A(I,3,LF)*T**(-A(I,4,LF))
 20   CONTINUE
C ------ GLUON DISTRIBUTION -------------
      PHDGG=AT(1)*X**AT(2)*(1.0-X)**AT(3)/137.
      RETURN
      END
*CMZ :-        -26/04/91  13.04.45  by  Federico Carminati
*-- Author :    Drees, Grassie, Charchula, modified by Bryan Webber
C --------------------------------------
C  QUARK PART OF THE PHOTON SF
C --------------------------------------
      FUNCTION PHDGQ(X,Q2,NFL,NCH)
      IMPLICIT REAL (A-H,P-Z)
      INTEGER NFL,NCH
      DIMENSION A(5,4,2,3),AT(5,2),XQPOM(2),E(2)
      COMMON/K719DG/F2
C SQUARE OF LAMBDA=0.4 GEV
      ALAM2=0.160
      T=LOG(Q2/ALAM2)
C
C  CHECK WHETHER NFL AND NCH HAVE RIGHT VALUES
C
      IF(.NOT.((NFL.EQ.3).OR.(NFL.EQ.4).OR.(NFL.EQ.5))) THEN
 110   WRITE(*,111)
 111   FORMAT('NUMBER OF FLAVOURS (NFL) HAS NOT BEEN SET TO: 3,4 OR 5'/
     *'          NFL=3 IS ASSUMED')
       NFL=3
      ENDIF
      IF (.NOT.((NCH.EQ.1).OR.(NCH.EQ.2))) THEN
 120     WRITE(*,121)
 121     FORMAT(' QUARK CHARGE NUMBER (NCH) HAS NOT BEEN SET
     * TO 1 OR 2;'/
     *'           NCH=1 IS ASSUMED')
         NCH=1
      ENDIF
C ------ INITIALIZATION ------
      DATA(((A(I,J,K,1),I=1,5),J=1,4),K=1,2)/
     + 2.28500,  6.07300, -0.42020,-0.08080, 0.05530,
     +-0.01530, -0.81320,  0.01780, 0.63460, 1.13600,
     + 1.3300E3,-41.3100,   0.92160, 1.20800, 0.95120,
     + 4.21900,  3.16500,  0.18000, 0.20300, 0.01160,
     +16.6900,   0.17600, -0.02080,-0.01680,-0.19860,
     +-0.79160,  0.04790,  0.3386E-2,1.35300, 1.10000,
     + 1.0990E3,  1.04700,  4.85300, 1.42600, 1.13600,
     + 4.42800,  0.02500,  0.84040, 1.23900,-0.27790/
        DATA(((A(I,J,K,2),I=1,5),J=1,4),K=1,2)/
     +-0.37110,-0.17170, 0.087660,-0.89150,-0.18160,
     + 1.06100, 0.78150, 0.021970, 0.28570, 0.58660,
     + 4.75800, 1.53500, 0.109600, 2.97300, 2.42100,
     +-0.01500, 0.7067E-2,0.204000, 0.11850, 0.40590,
     +-0.12070,25.00000,-0.012300,-0.09190, 0.020150,
     + 1.07100,-1.64800, 1.162000, 0.79120, 0.98690,
     + 1.97700,-0.015630,0.482400, 0.63970,-0.070360,
     +-0.8625E-2,6.43800,-0.011000, 2.32700, 0.016940/
        DATA(((A(I,J,K,3),I=1,5),J=1,4),K=1,2)/
     +15.80,     2.7420,  0.029170,-0.03420, -0.023020,
     +-0.94640, -0.73320, 0.046570, 0.71960,  0.92290,
     +-0.50,     0.71480, 0.17850,  0.73380,  0.58730,
     +-0.21180,  3.2870,  0.048110, 0.081390,-0.79E-4,
     + 6.7340,  59.880,  -0.3226E-2,-0.03321,   0.10590,
     +-1.0080,  -2.9830,  0.84320,  0.94750,  0.69540,
     +-0.085940, 4.480,   0.36160, -0.31980, -0.66630,
     + 0.076250, 0.96860, 0.1383E-2, 0.021320, 0.36830/
      CF=10.0
C ------- EVALUATION OF PARAMETERS IN Q2 ---------
      E(1)=1.0
      IF (NFL.EQ.3) THEN
        E(2)=9.0
        LF=1
      ELSEIF (NFL.EQ.4) THEN
        E(2)=10.0
        LF=2
      ELSEIF (NFL.EQ.5) THEN
        E(2)=55.0/6.0
        LF=3
      ENDIF
      DO 10 J=1,2
        DO 20 I=1,5
           ATP=A(I,1,J,LF)*T**A(I,2,J,LF)
           AT(I,J)=ATP+A(I,3,J,LF)*T**(-A(I,4,J,LF))
 20     CONTINUE
 10   CONTINUE
      DO 30 J=1,2
       POM1=X*(X*X+(1.0-X)**2)/(AT(1,J)-AT(2,J)*ALOG(1.0-X))
       POM2=AT(3,J)*X**AT(4,J)*(1.0-X)**AT(5,J)
       XQPOM(J)=E(J)*POM1+POM2
 30   CONTINUE
C -------  QUARK DISTRIBUTIONS ----------
      IF (NFL.EQ.3) THEN
         IF (NCH.EQ.2) THEN
           PHDGQ=1.0/6.0*(XQPOM(2)+9.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           PHDGQ=1.0/6.0*(XQPOM(2)-9.0/2.0*XQPOM(1))
         ENDIF
        F2=2.0/9.0*XQPOM(2)+XQPOM(1)
      ELSEIF (NFL.EQ.4) THEN
         IF (NCH.EQ.2) THEN
           PHDGQ=1.0/8.0*(XQPOM(2)+6.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           PHDGQ=1.0/8.0*(XQPOM(2)-6.0*XQPOM(1))
         ENDIF
        F2=5.0/18.0*XQPOM(2)+XQPOM(1)
      ELSEIF (NFL.EQ.5) THEN
         IF (NCH.EQ.2) THEN
           PHDGQ=1.0/10.0*(XQPOM(2)+15.0/2.0*XQPOM(1))
         ELSEIF(NCH.EQ.1) THEN
           PHDGQ=1.0/10.0*(XQPOM(2)-5.0*XQPOM(1))
         ENDIF
        F2=11.0/45.0*XQPOM(2)+XQPOM(1)
      ENDIF
      PHDGQ=PHDGQ/137.
      RETURN
      END
C  END DREES AND GRASSIE PHOTON
C  FONTANNAZ ET AL PHOTON PDF'S
      SUBROUTINE FONPDF(Q2IN,X,FX,NLF)
C                           
C     INTERPOLATION PROGRAM WHICH INTERPOLATES THE GRID "DATAN" AND GIVES THE
C     QUARK AND GLUON DISTRIBUTIONS IN THE REAL PHOTON, AS FUNCTIONS OF X AND Q2
C
C     THE Q2-EVOLUTION IS PERFORMED WITH BLL AP-EQUATIONS AND NF=4. A MASSIVE
C     CHARM DISTRIBUTION (BORROWED FROM GLUCK AND REYA) IS ALSO AVAILABLE.
C
C     THE BOUNDARY CONDITIONS ARE SUCH THAT THE DISTRIBUTION FUNCTIONS ARE GIVEN
C     BY A VDM "ANSATZ" AT Q2=.25 GEV**2.
C
C     THE PROGRAM WORKS FOR  2. GEV**2 < Q2 <5.5E+5   AND .00137 < X < .9986
C
C     THE DISTRIBUTIONS ARE CALCULATED IN THE MSBAR FACTORIZATION SCHEME.
C
C     THE VALUE OF LAMBDA-MSB IS 200 MEV
C
C     THE OUTPUT IS WRITTEN IN THE FILE 'FILEOUT':
C                                UPLUS=X*(U(X,Q2)+UBAR(X,Q2))
C                                DPLUS= ...
C                                SPLUS= ...
C                                CPLUS= ...  (MASSLESS CHARM WITH CPLUS(X,2.)=0)
C                                CPLUM= ...  (MASSIVE CHARM WITH MC=1.5 GEV )
C                                GLU=GLUON(X,Q2)*X
C                                SING=SINGLET(X,Q2)*X
C
C                    F2 = PHOTON STRUCTURE FUNCTION WITHOUT CHARM
C                    F2C=  "        "         "     WITH MASSIVE CHARM
C
      REAL FX(-NLF:NLF)
      DIMENSION Q(7),PAR(30),ZQ(5)
      COMMON/K719FOANEW/DELTA,FLAVOR,GLUCK
      COMMON/K719FOCONV/ IORD,ICONV,OWLAM,OWLAM2,RLAM,RLAM2
      COMMON/K719FOQS/Q2
      COMMON/K719FOQ000/Q02
      COMMON/K719FOQ2DIST/IDQ2
      COMMON/K719FOGFUNC/CALC(8,20,32)
      COMMON/K719FOGAUS32/XI(32),WI(32),NTERMS,XX(33)
      DATA INIT/0/
      EXTERNAL CPLU
      Q2=Q2IN
C INITIALIZATION
      IF(INIT.EQ.0) THEN
        INIT=1
        OPEN(UNIT=12,FILE='DATAN',STATUS='OLD')
C          
C  SET UP FLAGS, I/O FILES, ETC.
C
C  Q2 DEPENDENCE TURNED ON
        IDQ2=2
C
C   STRUCTURE FUNCTIONS CONVENTIONS
C   IORD=0                     LEADING ORDER
C   IORD=1             NEXT TO LEADING ORDER
C
        READ(12,*) PAR
        READ(12,2) CALC
   2    FORMAT(8E15.4)
        CLOSE(UNIT=12)
        IORD=INT(PAR(28))
        GLUCK=PAR(26)
        FLAV=PAR(25)
        NF=INT(FLAV+1.E-7)
        DELTA=PAR(29)
        OWLAM=PAR(1)
        OWLAM2=OWLAM**2
        Q02=PAR(30)
        PI=4.*ATAN(1.)
        PI2=PI**2
        CF=4./3.
        FLAVOR=FLAV
        B0=11.-2.*NF/3.
        B1=102.-38.*NF/3.
        CCOEG=2./9.
        IF(NF.EQ.4) CCOEG=5./18.
        COEG=2.*FLAVOR*CCOEG
        CALL WATE32
        IF(IORD.EQ.0)B1=0.
      ENDIF
C  INITIALIZATION COMPLETED
      XSAVE=X
      IF(X.LT.0.00137)X=0.00137
      IF(X.GT.0.9986)X=0.9986
      ALQ2=ALOG(Q2/OWLAM2) 
      ALFPI= 2. /(B0*ALQ2+B1*ALOG(ALQ2)/B0)
      CALL DIST(X,Q)
      ADD=Q(1)/FLAVOR
      UPLUS=Q(5)+ADD
      DPLUS=-Q(4)+ADD
      SPLUS=-Q(6)+ADD
      CPLUS=-Q(3)+ADD
      SING=Q(1)
      GLU=Q(7)
      CPLUM=CPLU(X,Q2)
      DQ=DPLUS/X
      UQ=UPLUS/X
      SQ=SPLUS/X
      CQ=CPLUM/X
      GL=GLU/X                        
      IF(NLF.GE.1) FX(1)=UQ/2.
      IF(NLF.GE.2) FX(2)=DQ/2.
      IF(NLF.GE.3) FX(3)=SQ/2.
      IF(NLF.GE.4) FX(4)=CQ/2.
      IF(NLF.GE.5) FX(5)=0.
      FX(0)=GL            
      DO I=1,NLF
        FX(-I)=FX(I)
      ENDDO
      X=XSAVE
      END
C
      FUNCTION CPLU(X,Q2)
      CMS=1.5**2
      BETS=1-4.*CMS*X/(1.-X)/Q2
      IF(BETS.LE..0) CPLU=.0
      IF(BETS.LE..0) GO TO 1
      BETA=SQRT(BETS)
      CPLU=(8.*X*(1.-X)-1.-4.*CMS*X*(1.-X)/Q2)*BETA
      CAU=X**2+(1.-X)**2+4.*CMS*X*(1.-3.*X)/Q2-8.*CMS**2*X**2/Q2**2
      CPLU=CPLU+CAU*ALOG((1.+BETA)/(1.-BETA))
      CPLU=3.*(4./9.)*CPLU*X/(3.1415*137.)
  1   RETURN
      END
C
      SUBROUTINE DIST(X,Q)
      DIMENSION Q(7)
      COMMON/K719FOQS/Q2
      COMMON/K719FOQ000/Q02
      COMMON/K719FOANEW/DELTA,FLAVOR,GLUCK
      COMMON/K719FOCONV/ IORD,ICONV,OWLAM,OWLAM2,RLAM,RLAM2
      COMMON/K719FOQ2DIST/IDQ2
      COMMON/K719FOGFUNC/CALC(8,20,32)
      SB=0.
      IF(Q2-Q02) 1,1,2
    2 IF(IDQ2-1) 1,1,3
    3 SB=ALOG(ALOG(Q2/OWLAM2)/ALOG(Q02/OWLAM2))
    1 CONTINUE
      CALL GINTER(8,0,X,SB,Q(7))
      CALL GINTER(7,0,X,SB,SING)
      CALL GINTER(4,0,X,SB,DPLUSNS)
      CALL GINTER(3,0,X,SB,CPLUSNS)
      IF(GLUCK.GT..5) GO TO 7
      CALL GINTER(5,0,X,SB,UPLUSNS)
      CALL GINTER(6,0,X,SB,SPLUSNS)
      Q(3)=CPLUSNS
      Q(4)=DPLUSNS
      Q(5)=UPLUSNS
      Q(6)=SPLUSNS
      GO TO 8
   7  Q(2)=DPLUSNS
C LORSQUE GLUCK=1, LA 4EME COLONNE DE GRILLE CONTIENT QNS
      Q(5)=0.
      Q(4)=0.
      Q(6)=0.
 8    Q(1)=SING
      RETURN
      END
C
      SUBROUTINE GINTER(I,NDRV,X,S,ANS)
      DIMENSION F1(32),F2(32),F3(32)
      COMMON/K719FOGFUNC/GF(8,20,32)
      COMMON/K719FOANEW/DELTA,FLAVOR,GLUCK
      DIMENSION AF(3),AS(3)
      N=3
      IS=S/DELTA+1
      IF(IS.GE.19)PAUSE
      IF(IS.GE.17) IS=17
      IS1=IS+1
      IS2=IS1+1
      DO 1 L=1,32
      KL=L+32*NDRV
      F1(L)=GF(I,IS,KL)
      F2(L)=GF(I,IS1,KL)
      F3(L)=GF(I,IS2,KL)
    1 CONTINUE
      AF(1)=GETFV(X,F1)
      AF(2)=GETFV(X,F2)
      AF(3)=GETFV(X,F3)
      AS(1)=(IS-1)*DELTA
      AS(2)=AS(1)+DELTA
      AS(3)=AS(2)+DELTA
      CALL POLINT(AS,AF,N,S,AANS,DY)
      ANS=AANS
      RETURN
      END

      SUBROUTINE WATE32
C  32 POINT GAUSSIAN QUADRATURE ROUTINE
      DIMENSION X(16),W(16)
      COMMON/K719FOGAUS32/XI(32),WI(32),NTERMS,XX(33)
      NTERMS=32                     
      X(1)=0.048307665687738316235
      X(2)=0.144471961582796493485
      X(3)=0.239287362252137074545
      X(4)=0.331868602282127649780
      X(5)=0.421351276130635345364
      X(6)=0.506899908932229390024
      X(7)=0.587715757240762329041
      X(8)=0.663044266930215200975
      X(9)=0.732182118740289680387
      X(10)=0.794483795967942406963
      X(11)=0.849367613732569970134
      X(12)=0.896321155766052123965
      X(13)=0.934906075937739689171
      X(14)=0.964762255587506430774
      X(15)=0.985611511545268335400
      X(16)=0.997263861849481563545
      W(1)=0.096540088514727800567
      W(2)=0.095638720079274859419
      W(3)=0.093844399080804565639
      W(4)=0.091173878695763884713
      W(5)=0.087652093004403811143
      W(6)=0.083311924226946755222
      W(7)=0.078193895787070306472
      W(8)=0.072345794108848506225
      W(9)=0.065822222776361846838
      W(10)=0.058684093478535547145
      W(11)=0.050998059262376176196
      W(12)=0.042835898022226680657
      W(13)=0.034273862913021433103
      W(14)=0.025392065309262059456
      W(15)=0.016274394730905670605
      W(16)=0.007018610009470096600
      DO 1 I=1,16
      XI(I)=-X(17-I)
      WI(I)=W(17-I)
      XI(I+16)=X(I)
      WI(I+16)=W(I)
    1 CONTINUE
      DO 2 I=1,32
    2 XX(I)=0.5*(XI(I)+1.)
      XX(33)=1.0
      RETURN
      END
C
       FUNCTION GETFV(X,FVL)
C  NOUVEAU PROGRAMME D'INTERPOLATION UTILISANT UNE ROUTINE DE MATH. RECIPES
       DIMENSION FVL(32)
       COMMON/K719FOGAUS32/XI(32),WI(32),NTERMS,XX(33)
       DIMENSION A(4),B(4)
       N=4
       EPS=1.E-7
       XAM=XX(1)-EPS
       XAP=XX(1)+EPS
C      IF(X.LT.XAM) PRINT*,' X = ',X
       IF(X.GT.XAM.AND.X.LT.XAP) GO TO 50
       GO TO 80
   50  Y=FVL(1)
       GO TO 77
   80  IF(X.LT.XX(2)) GO TO 51
       IF(X.GT.XX(30)) GO TO 61
       DO 1 I=3,30
       IF(X.GT.XX(I)) GO TO 1
       A(1)=XX(I-2)
       A(2)=XX(I-1)
       A(3)=XX(I)
       A(4)=XX(I+1)
       B(1)=FVL(I-2)
       B(2)=FVL(I-1)
       B(3)=FVL(I)
       B(4)=FVL(I+1)
       GO TO 70
   1   CONTINUE
  61   A(1)=XX(29)
       A(2)=XX(30)
       A(3)=XX(31)
       A(4)=XX(32)
       B(1)=FVL(29)
       B(2)=FVL(30)
       B(3)=FVL(31)
       B(4)=FVL(32)
       GO  TO 70
  51   A(1)=XX(1)
       A(2)=XX(2)
       A(3)=XX(3)
       A(4)=XX(4)
       B(1)=FVL(1)
       B(2)=FVL(2)
       B(3)=FVL(3)
       B(4)=FVL(4)
  70   CONTINUE
C      IF(X.GT..2.AND.X.LT..8) THEN
             CALL POLINT(A,B,N,X,Y,DY)
C      ELSE
C            CALL RATINT(A,B,N,X,Y,DY)
C      ENDIF
  77   GETFV=Y
       RETURN
       END

      SUBROUTINE RATINT(XA,YA,N,X,Y,DY)
      PARAMETER (NMAX=10,TINY=1.E-25)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      HH=ABS(X-XA(1))
      DO 11 I=1,N
        H=ABS(X-XA(I))
        IF (H.EQ.0.)THEN
          Y=YA(I)
          DY=0.0
          RETURN
        ELSE IF (H.LT.HH) THEN
          NS=I
          HH=H
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)+TINY
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          W=C(I+1)-D(I)
          H=XA(I+M)-X
          T=(XA(I)-X)*D(I)/H
          DD=T-C(I+1)
          IF(DD.EQ.0.)PAUSE
          DD=W/DD
          D(I)=C(I+1)*DD
          C(I)=T*DD
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END

      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
C-- END FONTANNAZ ET AL PHOTON PDF'S
C FONTANNAZ 1994
      SUBROUTINE AFGPDF(Q2IN,X,FX,NLF)
C**************************************************************************
C                       ( 1st of February 1994)
C     This is an interpolation program which reads the files GRPOL and 
C     GRVDM and gives the quark and gluon distributions in real photon 
C     as functions of x and Q**2.
C
C     The Q**2 evolution is a BLL evolution (MSbar scheme) with Nf=4
C     and LAMBDA(MSbar)=.200 Gev.
C
C     A massless charm distribution is generated for Q**2 > 2 Gev**2.
C
C     The distributions are the sum of a pointlike part (PL) and of a
C     Vdm part (VDM):
C                     dist=PL + KA*VDM
C     KA is a factor which can be adjusted ( the default value is KA=1.0).
C     The file GRPOL contains the pointlike part of the distributions.
C     The file GRVDM contains the vdm part (A precise definition of this
C     latter is given in the paper "PARTON DISTRIBUTIONS IN THE PHOTON",
C     Preprint LPTHE Orsay 93-37, by P.Aurenche,M.Fontannaz and J.Ph.Guillet).
C
C     The output of the program is written in the file GETOUT with the 
C     following conventions
C                              UPLUS=x(u+ubar)
C                              DPLUS=x(d+dbar)
C                              SPLUS=x(s+sbar)
C                              CPLUS=x(c+cbar)
C                              SING =UPLUS+DPLUS+SPLUS+CPLUS
C                              GLU  =x*g
C     
C      The interpolation is valid for     2. < Q**2 < 5.5E+5 Gev**2,
C                             and for   .0015<  x   < .99 
C
C      The program also gives the structure function F2:
C                        F2 = q*Cq + g*Cg + Cgam
C      Cq and Cg are the Wilson coeficients and Cgam is the direct term.
C
C      Although the charm quark evolution is massless, the direct term
C      Cgam includes the effects due to the charm quark mass. The charm
C      quark threshold is therefore correctly described at the lowest 
C      ordre in alphastrong (Details are given in the preprint).
C
C      The charm contribution can be set equal to zero with the CHARME flag
C      ( CHARME=0.  -> no charm) ( 27/09/94)
C      
C**************************************************************************
      REAL FX(-NLF:NLF)
      DIMENSION Q(7),PAR(30),PAR2(30),QQ(7)
      COMMON/K719ANEW/DELTA
      COMMON/K719CONV2342/ IORD,ICONV,OWLAM,OWLAM2,RLAM,RLAM2
      COMMON/K719QS/Q2
      COMMON/K719Q000/Q02
      COMMON/K719GFUNC/CALC(8,20,32)
      COMMON/K719GVDM/CELC(8,20,32)
      COMMON/K719GAUS32/XI(32),WI(32),NTERMS,XX(33)
      COMMON/K719CHARM/CM
      DATA INIT/0/
      REAL KA
      Q2=Q2IN
      IF(INIT.EQ.0) THEN
        INIT=1
        OPEN(UNIT=12,FILE='GRPOL',STATUS='OLD')
        OPEN(UNIT=14,FILE='GRVDM',STATUS='OLD')
C
C   SET UP FLAGS, I/O FILES, ETC.
C
C   STRUCTURE FUNCTIONS CONVENTIONS
C   IORD=0                     LEADING ORDER
C   IORD=1             NEXT TO LEADING ORDER
C
        READ(12,*) PAR
        READ(12,2) CALC
        READ(14,*) PAR2
        READ(14,2) CELC
C*****adjustment of the VDM contribution*******************************
        KA=1.
C
C*****mass of the charm quark******************************************
        CM=1.41
C*****CHARME=0. -> no charm contribution ******************************
        CHARME=1.
C
C******The parameters are fixed in the file GRPOL**********************     
C
        IORD=INT(PAR(28)+1.E-7)
        FLAV=PAR(25)
2       FORMAT(8E15.4)
        DELTA=PAR(29)
        OWLAM=PAR(1)
        OWLAM2=OWLAM**2
        Q02=PAR(30)
        PI=4.*ATAN(1.)
        PI2=PI**2
        CF=4./3.
        NF=INT(FLAV+1.E-7)
        IF(NF.EQ.0) NF=3
        FLAVOR=FLOAT(NF)
        B0=11.-2.*NF/3.
        B1=102.-38.*NF/3.
        CCOEG=2./9.
C        IF(NF.EQ.4) CCOEG=5./18.
C        COEG=2.*FLAVOR*CCOEG
        COEG=6.*CCOEG
        CALL WATE32_N
        IF(IORD.EQ.0)B1=0.
      ENDIF
C  INITIALIZATION COMPLETED
      XSAVE=X
      IF(X.LT.0.0015)X=0.0015
      IF(X.GT.0.99)X=0.99
      XTH=1./(1.+4.*CM**2/Q2)
      ALQ2=XLOG(Q2/OWLAM2)
      ALFPI= 2. /(B0*ALQ2+B1*XLOG(ALQ2)/B0)
      CUT=1.     
      IF(CHARME.EQ.0.) CUT=0.
      CUTG=1.
      IF(X.GT.XTH) CUT=0.
      CALL DIST_N(X,Q)
      ADD=Q(1)/FLAVOR
      UPLUS=Q(5)+ADD
      DPLUS=-Q(4)+ADD
      SPLUS=-Q(6)+ADD
      CPLUS=-Q(3)+ADD
      SING=Q(1)
      GLU=Q(7)         
      CALL DIST2_N(X,QQ)
      ADD2=QQ(1)/FLAVOR
      UPLU2=QQ(5)+ADD2
      DPLU2=-QQ(4)+ADD2
      SPLU2=-QQ(6)+ADD2
      CPLU2=-QQ(3)+ADD2
      SING2=QQ(1)
      GLU2=QQ(7)
      UPLUS=UPLUS+UPLU2*KA
      DPLUS=DPLUS+DPLU2*KA
      SPLUS=SPLUS+SPLU2*KA
      CPLUS=CPLUS+CPLU2*KA
      SING=SING+SING2*KA
      GLU=GLU+GLU2*KA
      DQ=DPLUS/X
      UQ=UPLUS/X
      SQ=SPLUS/X
      CQ=CPLUS/X
      GL=GLU/X                        
      IF(NLF.GE.1) FX(1)=UQ/2.
      IF(NLF.GE.2) FX(2)=DQ/2.
      IF(NLF.GE.3) FX(3)=SQ/2.
      IF(NLF.GE.4) FX(4)=CQ/2.
      IF(NLF.GE.5) FX(5)=0.
      FX(0)=GL
      DO I=1,NLF
        FX(-I)=FX(I)
      ENDDO
      X=XSAVE
      RETURN
      END
C
      FUNCTION WCM(X,Q2)
      COMMON/K719CHARM/CM
      CMS=CM**2
      SC=Q2*(1.-X)/X
      BE=4.*CMS/SC
      IF(BE.GE.1.) WCM=0.
      IF(BE.GE.1.) GO TO 1
      SQ=SQRT(1.-BE)
      A1=((1.+SQ)/2.)**2 
      A2=(1.-X)/X
      WCM=(8.*(1.-X)*X-1.)*SQ+(X**2+(1.-X)**2)*XLOG(A1*A2)
      WCM=3.*(4./9.)/(3.1416*137.)*X*WCM
 1    RETURN
      END

      SUBROUTINE DIST_N(X,Q)
      DIMENSION Q(7)
      COMMON/K719QS/Q2
      COMMON/K719Q000/Q02
      COMMON/K719ANEW/DELTA
      COMMON/K719CONV2342/ IORD,ICONV,OWLAM,OWLAM2,RLAM,RLAM2
      SB=0.
      IF(Q2-Q02) 1,1,3
    3 SB=XLOG(XLOG(Q2/OWLAM2)/XLOG(Q02/OWLAM2))
    1 CONTINUE
C      PROGRAM DISTL0.FOR
      CALL GINTER_N(8,0,X,SB,Q(7))
      CALL GINTER_N(7,0,X,SB,SING1)
      CALL GINTER_N(4,0,X,SB,DPLUSNS)
      CALL GINTER_N(3,0,X,SB,CPLUSNS)
      CALL GINTER_N(5,0,X,SB,UPLUSNS)
      CALL GINTER_N(6,0,X,SB,SPLUSNS)
      Q(3)=CPLUSNS
      Q(4)=DPLUSNS
      Q(5)=UPLUSNS
      Q(6)=SPLUSNS
 8    Q(1)=SING1
      RETURN
      END

      SUBROUTINE DIST2_N(X,QQ)
      DIMENSION QQ(7)
      COMMON/K719QS/Q2
      COMMON/K719Q000/Q02
      COMMON/K719ANEW/DELTA
      COMMON/K719CONV2342/ IORD,ICONV,OWLAM,OWLAM2,RLAM,RLAM2
      SB=0.
      IF(Q2-Q02) 1,1,3
    3 SB=XLOG(XLOG(Q2/OWLAM2)/XLOG(Q02/OWLAM2))
    1 CONTINUE
C      PROGRAM DISTL0.FOR
      CALL GINTER2_N(8,0,X,SB,QQ(7))
      CALL GINTER2_N(7,0,X,SB,SING2)
      CALL GINTER2_N(4,0,X,SB,DPLUSNS)
      CALL GINTER2_N(3,0,X,SB,CPLUSNS)
      CALL GINTER2_N(5,0,X,SB,UPLUSNS)
      CALL GINTER2_N(6,0,X,SB,SPLUSNS)
      QQ(3)=CPLUSNS
      QQ(4)=DPLUSNS
      QQ(5)=UPLUSNS
      QQ(6)=SPLUSNS
 8    QQ(1)=SING2
      RETURN
      END
                                                                      
      SUBROUTINE GINTER_N(I,NDRV,X,S,ANS)
      DIMENSION F1(32),F2(32),F3(32)
      COMMON/K719GFUNC/GF(8,20,32)
      COMMON/K719ANEW/DELTA
      DIMENSION AF(3),AS(3)
      N=3
      IS=S/DELTA+1
      IS1=IS+1
      IS2=IS1+1
      DO 1 L=1,32
      KL=L+32*NDRV
      F1(L)=GF(I,IS,KL)
      F2(L)=GF(I,IS1,KL)
      F3(L)=GF(I,IS2,KL)
    1 CONTINUE
      AF(1)=GETFV_N(X,F1)
      AF(2)=GETFV_N(X,F2)
      AF(3)=GETFV_N(X,F3)
      AS(1)=(IS-1)*DELTA
      AS(2)=AS(1)+DELTA
      AS(3)=AS(2)+DELTA
      CALL POLINT_N(AS,AF,N,S,AANS,DY)
      ANS=AANS
      RETURN
      END

      SUBROUTINE GINTER2_N(I,NDRV,X,S,ANS)
      DIMENSION F1(32),F2(32),F3(32)
      COMMON/K719GVDM/GFV(8,20,32)
      COMMON/K719ANEW/DELTA
      DIMENSION AF(3),AS(3)
      N=3
      IS=S/DELTA+1
      IS1=IS+1
      IS2=IS1+1
      DO 1 L=1,32
      KL=L+32*NDRV
      F1(L)=GFV(I,IS,KL)
      F2(L)=GFV(I,IS1,KL)
      F3(L)=GFV(I,IS2,KL)
    1 CONTINUE
      AF(1)=GETFV_N(X,F1)
      AF(2)=GETFV_N(X,F2)
      AF(3)=GETFV_N(X,F3)
      AS(1)=(IS-1)*DELTA
      AS(2)=AS(1)+DELTA
      AS(3)=AS(2)+DELTA
      CALL POLINT_N(AS,AF,N,S,AANS,DY)
      ANS=AANS
      RETURN
      END

      SUBROUTINE WATE32_N
C  32 POINT GAUSSIAN QUADRATURE ROUTINE
      DIMENSION X(16),W(16)
      COMMON/K719GAUS32/XI(32),WI(32),NTERMS,XX(33)
      NTERMS=32
      X(1)=0.048307665687738316235
      X(2)=0.144471961582796493485
      X(3)=0.239287362252137074545
      X(4)=0.331868602282127649780
      X(5)=0.421351276130635345364
      X(6)=0.506899908932229390024
      X(7)=0.587715757240762329041
      X(8)=0.663044266930215200975
      X(9)=0.732182118740289680387
      X(10)=0.794483795967942406963
      X(11)=0.849367613732569970134
      X(12)=0.896321155766052123965
      X(13)=0.934906075937739689171
      X(14)=0.964762255587506430774
      X(15)=0.985611511545268335400
      X(16)=0.997263861849481563545
      W(1)=0.096540088514727800567
      W(2)=0.095638720079274859419
      W(3)=0.093844399080804565639
      W(4)=0.091173878695763884713
      W(5)=0.087652093004403811143
      W(6)=0.083311924226946755222
      W(7)=0.078193895787070306472
      W(8)=0.072345794108848506225
      W(9)=0.065822222776361846838
      W(10)=0.058684093478535547145
      W(11)=0.050998059262376176196
      W(12)=0.042835898022226680657
      W(13)=0.034273862913021433103
      W(14)=0.025392065309262059456
      W(15)=0.016274394730905670605
      W(16)=0.007018610009470096600
      DO 1 I=1,16
      XI(I)=-X(17-I)
      WI(I)=W(17-I)
      XI(I+16)=X(I)
      WI(I+16)=W(I)
    1 CONTINUE
      DO 2 I=1,32
    2 XX(I)=0.5*(XI(I)+1.)
      XX(33)=1.0
      RETURN
      END
       FUNCTION GETFV_N(X,FVL)
C  NOUVEAU PROGRAMME D'INTERPOLATION UTILISANT UNE ROUTINE DE MATH. RECIPES
       DIMENSION FVL(32)
       COMMON/K719GAUS32/XI(32),WI(32),NTERMS,XX(33)
       DIMENSION A(4),B(4)
       N=4
       EPS=1.E-7
       XAM=XX(1)-EPS
       XAP=XX(1)+EPS
C       IF(X.LT.XAM) PRINT*,' X = ',X
       IF(X.GT.XAM.AND.X.LT.XAP) GO TO 50
       GO TO 80
   50  Y=FVL(1)
       GO TO 77
   80  IF(X.LT.XX(2)) GO TO 51
       IF(X.GT.XX(30)) GO TO 61
       DO 1 I=3,30
       IF(X.GT.XX(I)) GO TO 1
       A(1)=XX(I-2)
       A(2)=XX(I-1)
       A(3)=XX(I)
       A(4)=XX(I+1)
       B(1)=FVL(I-2)
       B(2)=FVL(I-1)
       B(3)=FVL(I)
       B(4)=FVL(I+1)
       GO TO 70
   1   CONTINUE
  61   A(1)=XX(29)
       A(2)=XX(30)
       A(3)=XX(31)
       A(4)=XX(32)
       B(1)=FVL(29)
       B(2)=FVL(30)
       B(3)=FVL(31)
       B(4)=FVL(32)
       GO  TO 70
  51   A(1)=XX(1)
       A(2)=XX(2)
       A(3)=XX(3)
       A(4)=XX(4)
       B(1)=FVL(1)
       B(2)=FVL(2)
       B(3)=FVL(3)
       B(4)=FVL(4)
  70   CONTINUE
C 70   IF(X.GT..2.AND.X.LT..8) THEN
             CALL POLINT_N(A,B,N,X,Y,DY)
C      ELSE
C            CALL RATINT_N(A,B,N,X,Y,DY)
C      ENDIF
  77   GETFV_N=Y
       RETURN
       END
 
      SUBROUTINE POLINT_N(XA,YA,N,X,Y,DY)
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1 
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE     
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
c increase the precision of log calls
      function xlog(x)
      implicit none
      real xlog,x
      real * 8 xx
      xx=x
      xlog=log(xx)
      end
C END FONTANNAZ 1994
C
C GLUECK REYA VOGT PHOTON
      SUBROUTINE GRV_PH(Q2,X,FX,NF)
      REAL FX(-NF:NF)
      REAL * 8 DX,DQ,UPQ,DOQ,STR,CHR,BOT,GLU,DUM
      REAL*8 XMIN,XMAX,QSQMIN,QSQMAX,QSQ
      REAL*8 IXMIN,IXMAX,IQSQMIN,IQSQMAX
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,1.D0,0.3D0,1.D6/
      DATA INI/0/
      IF(INI.GT.0) GO TO 1
      ilxmin=0
      ilxmax=0
      ilqsqmin=0
      ilqsqmax=0
      INI=1
1     CONTINUE
      DX=DBLE(X)
      DQ=DBLE(SQRT(Q2))
      if(Dx.lt.xmin) then
        ixmin=ixmin+1.
        if(log10(ixmin).gt.ilxmin) then
          write(*,*)' x < xmin in str. functions more than 10**',
     +  ilxmin,' times'                          
          ilxmin=ilxmin+1
        endif
      endif
      if(Dx.gt.xmax) then
        ixmax=ixmax+1.
        if(log10(ixmax).gt.ilxmax) then
          write(*,*)' x > xmax in str. functions more than 10**',
     +  ilxmax,' times'
          ilxmax=ilxmax+1
        endif
      endif
      qsq=DQ**2
      if(qsq.lt.qsqmin) then
        iqsqmin=iqsqmin+1.
        if(log10(iqsqmin).gt.ilqsqmin) then
          write(*,*)'q**2 < min q**2 in str. functions more than 10**',
     +  ilqsqmin,' times'
          ilqsqmin=ilqsqmin+1
        endif
      endif
      if(qsq.gt.qsqmax) then
        iqsqmax=iqsqmax+1.
        if(log10(iqsqmax).gt.ilqsqmax) then
          write(*,*)'q**2 > max q**2 in str. functions more than 10**',
     +  ilqsqmax,' times'
          ilqsqmax=ilqsqmax+1
        endif
      endif
      CALL GRVGAHO (DX,DQ,UPQ,DOQ,DUM,DUM,STR,CHR,BOT,GLU)
      FX(0)=SNGL(GLU)
      FX(1)=SNGL(UPQ)
      FX(2)=SNGL(DOQ)
      IF(NF.GE.3) FX(3)=SNGL(STR)
      IF(NF.GE.4) FX(4)=SNGL(CHR)
      IF(NF.GE.5) FX(5)=SNGL(BOT)
      IF(NF.eq.6) FX(6)=0
      DO I=1,NF
        FX(-I)=FX(I)
      ENDDO
      DO I=-NF,NF
       FX(I)=FX(I)/X
      ENDDO
      RETURN
      END


       SUBROUTINE GRVGAHO (ZX,ZQ,ZUV,ZDV,ZUB,ZDB,ZSB,ZCB,ZBB,ZGL)
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                 *
*      G R V - P H O T O N - P A R A M E T R I Z A T I O N S      *
*                                                                 *
*                 FOR A DETAILED EXPLANATION SEE :                *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/31             *
*                                                                 *
*    THE OUTPUT IS ALWAYS   1./ ALPHA(EM) * X * PARTON DENSITY    *
*    output modified by HPB to be always    X * PARTON DENSITY    *
*                                                                 *
*   THE PARAMETRIZATIONS ARE FITTED TO THE PARTON DISTRIBUTIONS   *
*   FOR Q ** 2 BETWEEN MU ** 2 (=  0.25 / 0.30  GEV ** 2  IN LO   *
*   / HO) AND  1.E6 GEV ** 2  AND FOR X BETWEEN  1.E-5  AND  1.   *
*                                                                 *
*              HEAVY QUARK THRESHOLDS  Q(H) = M(H) :              *
*         M(C)  =  1.5,  M(B)  =  4.5,  M(T)  =  100  GEV         *
*                                                                 *
*      CORRESPONDING LAMBDA(F) VALUES FOR F ACTIVE FLAVOURS :     *
*      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.153,   LAMBDA(6)  =  0.082  GEV     *
*      HO :   LAMBDA(3)  =  0.248,   LAMBDA(4)  =  0.200,         *
*             LAMBDA(5)  =  0.131,   LAMBDA(6)  =  0.053  GEV     *
*                                                                 *
*      HO DISTRIBUTIONS REFER TO THE DIS(GAMMA) SCHEME, SEE :     *
*              M. GLUECK, E.REYA, A.VOGT: DO-TH 91/26             *
*                                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
       IMPLICIT REAL (A - Y)
      DOUBLE PRECISION
     +        ZX,ZQ,ZUV,ZDV,ZUB,ZDB,ZSB,ZCB,ZBB,ZGL
       DATA ALPHEM/7.29927D-3/
       REAL  X, Q
       X = ZX
       Q = ZQ
       MU2  = 0.3
       LAM2 = 0.248 * 0.248
       Q2 = Q*Q
       S  = ALOG (ALOG(Q2/LAM2) / ALOG(MU2/LAM2))
       SS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
       AL =  0.583
       BE =  0.688
       AK =  0.449 - 0.025 * S  - 0.071 * S2
       BK =  5.060 - 1.116 * SS
       AG =  0.103
       BG =  0.319 + 0.422 * S
       C  =  1.508 + 4.792 * S  - 1.963 * S2
       D  =  1.075 + 0.222 * SS - 0.193 * S2
       E  =  4.147 + 1.131 * S
       ES =  1.661 + 0.874 * S
       UH =  GRVGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZUV = UH * ALPHEM
       ZUB = ZUV
C...X * D = X * DBAR :
       AL =  0.591
       BE =  0.698
       AK =  0.442 - 0.132 * S  - 0.058 * S2
       BK =  5.437 - 1.916 * SS
       AG =  0.099
       BG =  0.311 - 0.059 * S
       C  =  0.800 + 0.078 * S  - 0.100 * S2
       D  =  0.862 + 0.294 * SS - 0.184 * S2
       E  =  4.202 + 1.352 * S
       ES =  1.841 + 0.990 * S
       DH  =  GRVGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZDV = DH * ALPHEM
       ZDB = ZDV
C...X * G :
       AL =  1.161
       BE =  1.591
       AK =  0.530 - 0.742 * SS + 0.025 * S2
       BK =  5.662
       AG =  0.533 - 0.281 * SS + 0.218 * S2
       BG =  0.025 - 0.518 * S  + 0.156 * S2
       C  = -0.282              + 0.209 * S2
       D  =  0.107 + 1.058 * S  - 0.218 * S2
       E  =   0.0  + 2.704 * S
       ES =  3.071 - 0.378 * S
       GH =  GRVGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZGL = GH * ALPHEM
C...X * S = X * SBAR :
       SF =   0.0
       AL =  0.635
       BE =  0.456
       AK =  1.770 - 0.735 * SS - 0.079 * S2
       BK =  3.832
       AG =  0.084 - 0.023 * S
       BG =  0.136
       C  =  2.119 - 0.942 * S  + 0.063 * S2
       D  =  1.271 + 0.076 * S  - 0.190 * S2
       E  =  4.604 + 0.737 * S
       ES =  1.641 + 0.976 * S
       SH =  GRVGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZSB = SH * ALPHEM
C...X * C = X * CBAR :
       SF =  0.820
       AL =  0.926
       BE =  0.152
       AK =  1.142 - 0.175 * S
       BK =  3.276
       AG =  0.504 + 0.317 * S
       BG = -0.433
       C  =  3.334
       D  =  0.398 + 0.326 * S  - 0.107 * S2
       E  =  5.493 + 0.408 * S
       ES =  2.426 + 1.277 * S
       CH =  GRVGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZCB = CH * ALPHEM
C...X * B = X * BBAR :
       SF =  1.297
       AL =  0.969
       BE =  0.266
       AK =  1.953 - 0.391 * S
       BK =  1.657 - 0.161 * S
       AG =  1.076 + 0.034 * S
       BG = -2.015
       C  =  1.662
       D  =  0.353 + 0.016 * S
       E  =  5.713 + 0.249 * S
       ES =  3.456 + 0.673 * S
       BH =  GRVGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       ZBB = BH * ALPHEM
c
       RETURN
       END


       FUNCTION GRVGF (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT REAL (A - Z)
       SX = SQRT (X)
       LX = ALOG (1./X)
       GRVGF  = (X**AK * (AG + BG * SX + C * X**BK)  +  S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END


       FUNCTION GRVGFS (X, S, SF, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT REAL (A - Z)
       IF (S .LE. SF) THEN
          GRVGFS = 0.0
       ELSE
          SX = SQRT (X)
          LX = ALOG (1./X)
          DS = S - SF
          GRVGFS = (DS * X**AK * (AG + BG * SX + C * X**BK) + DS**AL
     1         * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       END IF
       RETURN
       END
C END GLUECK REYA VOGT PHOTON
C
C GLUECK REYA SCHIENBEIN PHOTON
      SUBROUTINE GRS_PH(Q2,X,FX,NF)
      REAL FX(-NF:NF)
      REAL * 8 DX,DQ2,UPH,DPH,SPH,GPH
      REAL*8 XMIN,XMAX,QSQMIN,QSQMAX,QSQ
      REAL*8 IXMIN,IXMAX,IQSQMIN,IQSQMAX
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-5,0.95D0,0.5D0,1.D5/
      DATA INI/0/
      PARAMETER (ALPHAEM=1/137.D0)
      IF(INI.GT.0) GO TO 1
      ilxmin=0
      ilxmax=0
      ilqsqmin=0
      ilqsqmax=0
      INI=1
1     CONTINUE
      DX=DBLE(X)
      DQ2=DBLE(Q2)
      if(Dx.lt.xmin) then
        ixmin=ixmin+1.
        if(log10(ixmin).gt.ilxmin) then
          write(*,*)' x < xmin in str. functions more than 10**',
     +  ilxmin,' times'                          
          ilxmin=ilxmin+1
        endif
      endif
      if(Dx.gt.xmax) then
        ixmax=ixmax+1.
        if(log10(ixmax).gt.ilxmax) then
          write(*,*)' x > xmax in str. functions more than 10**',
     +  ilxmax,' times'
          ilxmax=ilxmax+1
        endif
      endif
      qsq=DQ2
      if(qsq.lt.qsqmin) then
        iqsqmin=iqsqmin+1.
        if(log10(iqsqmin).gt.ilqsqmin) then
          write(*,*)'q**2 < min q**2 in str. functions more than 10**',
     +  ilqsqmin,' times'
          ilqsqmin=ilqsqmin+1
        endif
      endif
      if(qsq.gt.qsqmax) then
        iqsqmax=iqsqmax+1.
        if(log10(iqsqmax).gt.ilqsqmax) then
          write(*,*)'q**2 > max q**2 in str. functions more than 10**',
     +  ilqsqmax,' times'
          ilqsqmax=ilqsqmax+1
        endif
      endif
      CALL GRSGHO (DX,DQ2,UPH,DPH,SPH,GPH)
      FX(0)=SNGL(GPH)
      FX(1)=SNGL(UPH)
      FX(2)=SNGL(DPH)
      IF(NF.GE.3) FX(3)=SNGL(SPH)
      IF(NF.GE.4) FX(4)=0
      IF(NF.GE.5) FX(5)=0
      IF(NF.eq.6) FX(6)=0
      DO I=1,NF
        FX(-I)=FX(I)
      ENDDO
      DO I=-NF,NF
       FX(I)=ALPHAEM*FX(I)/X
      ENDDO
      RETURN
      END
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                   *
*     G R S  -  P H O T O N  - P A R A M E T R I Z A T I O N S      *
*                                                                   *
*                                1999                               *
*                                                                   *
*                  For a detailed explanation see                   *
*                M. Glueck, E. Reya, I. Schienbein :                *
*                   hep-ph/9903337  =  DO-TH 99/03                  *
*                  (To appear in Phys. Rev. D)                      *
*                                                                   *
*   The parametrizations are fitted to the parton distributions     *
*                               for                                 *
*                0.5 GeV**2 =< Q**2 =< 1.E-5 GeV**2                 *
*                               and                                 * 
*                          1.E-5 =< x =< 0.95                       *                  
*   Regions, where the distribution under consideration is neg-     *
*   ligible, were excluded from the fit.                            *
*                                                                   *
*    Leading Order PDF's of the Real(P2=0) and Virtual Photon:      *
*    call GRSGLO (X, Q2, P2, UPH, DPH, SPH, GPH)                    *
*                                                                   *
*    Next-To-Leading Order PDF's of the Real(P2=0) Photon:          *
*    call GRSGHO (X, Q2, UPH, DPH, SPH, GPH)                        *
*                                                                   *
*   INPUT:   X  = Bjorken-x       (between  1.E-5 and 1   )         *
*            Q2 = Scale in GeV**2 (between  0.5   and 1.E5)         *
*            and  in Leading Order:                                 *
*            P2 = Virtuality of the Photon (typically, P2 =< Q2/10) *
*            P2 = 0 : Real Photon                                   *
*                                                                   *
*   OUTPUT:                                                         *
*            Leading Order:                                         *
*            UPH = x u(gamma(P2))(x,Q2)/ALPHA(em) etc               *
*            Next-To-Leading Order (DIS_gamma Scheme):              *
*            UPH = x u(gamma)(x,Q2)/ALPHA(em) etc                   *
*                                                                   *
*            !Always x times the distribution is returned!          *
*                (divided by ALPHA(em) approx. = 1/137)             *
*                                                                   *
*   ALPHAS:                                                         *
*   At Q^2 = MZ^2, alpha_s reads  0.114 (0.125) in NLO (LO); the    *
*   heavy quark thresholds, Qh^2 = mh^2, in the beta function are   *
*                   mc = 1.4 GeV,  mb = 4.5 GeV.                    *
*   Note that the NLO alpha_s running is different from GRV(94).    * 
*                                                                   *
*   Questions, comments etc to: schien@hal1.physik.uni-dortmund.de  *
*   19.03.1999                                                      *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C Leading Order Real and Virtual Photon (Point-Like + Hadronic)
C
C x f(gamma(P2)) / alpha = x f(gamma(P2))_PL / alpha 
C                        + eta G_f^2 x f(Pi^0) + r_f 
C                        = x f(gamma(P2))_PL / alpha 
C                        + x f(gamma(P2))_HAD / alpha 
       SUBROUTINE GRSGLO (X, Q2, P2, UPH, DPH, SPH, GPH)
       IMPLICIT DOUBLE PRECISION (A - Z)
c couplings and eta factor
       Gu2 = 0.836
       Gd2 = 0.250
       Gs2 = 0.543
       Gg2 = 0.543
       eta = 1/(1.+ P2/0.59)**2
c hadronic part:
       call GRSPILO (X, Q2, VAP, GLP, QBP, SBP)
C X U(Pi^0) = X UBAR(Pi^0) = (VAP + 2 QBP)/2 
C X D(Pi^0) = X DBAR(Pi^0) = (VAP + 2 QBP)/2 
C X S = X SBAR = SBP
C X G = GLP
       r = eta * (Gu2-Gd2)/2. * SBP
       UHAD = eta * Gu2 * (VAP + 2. * QBP)/2. - r  
       DHAD = eta * Gd2 * (VAP + 2. * QBP)/2. + r
       SHAD = eta * Gs2 * SBP 
       GHAD = eta * Gg2 * GLP
c point-like part:
       call GRSGLOPL (X, Q2, P2, UPL, DPL, SPL, GPL)
c PL + HAD:
       UPH = UPL + UHAD
       DPH = DPL + DHAD
       SPH = SPL + SHAD
       GPH = GPL + GHAD
       end
C
C Next-To-Leading Order Real(P2=0) Photon (Point-Like + Hadronic)
C x f(gamma) / alpha = x f(gamma)_PL / alpha + G_f^2 x f(Pi^0) + r_f
C                        = x f(gamma)_PL / alpha + x f(gamma)_HAD / alpha 
       SUBROUTINE GRSGHO (X, Q2, UPH, DPH, SPH, GPH)
       IMPLICIT DOUBLE PRECISION (A - Z)
c couplings 
       Gu2 = 0.836
       Gd2 = 0.250
       Gs2 = 0.543
       Gg2 = 0.543
c hadronic part:
       call GRSPIHO (X, Q2, VAP, GLP, QBP, SBP)
       r = (Gu2-Gd2)/2. * SBP
       UHAD = Gu2 * (VAP + 2. * QBP)/2. - r  
       DHAD = Gd2 * (VAP + 2. * QBP)/2. + r
       SHAD = Gs2 * SBP 
       GHAD = Gg2 * GLP
c point-like part:
       call GRSGHOPL (X, Q2, UPL, DPL, SPL, GPL)
c PL + HAD:
       UPH = UPL + UHAD
       DPH = DPL + DHAD
       SPH = SPL + SHAD
       GPH = GPL + GHAD
       end

C Leading Order, Point-Like
       SUBROUTINE GRSGLOPL (X, Q2, P2, UL, DL, SL, GL)
       IMPLICIT DOUBLE PRECISION (A - Z)
       pi = dacos(-1.d0)
       MU2  = 0.26
       LAM2 = 0.204*0.204
       if (P2 .lt. MU2) then
           S = DLOG(DLOG(Q2/LAM2)/DLOG(MU2/LAM2))
       else
           S = DLOG(DLOG(Q2/LAM2)/DLOG(P2/LAM2))
       end if    
       alpq3= 4.*pi/(9.*DLOG(Q2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
            AL =  2.626 
            BE =  0.413 
            AK =  2.137 - 0.310 * DS
            BK = -1.049 + 0.113 * S
            AG = -0.785 + 0.270 * DS 
            BG =  0.650 - 0.146 * S
            C  =  0.252 - 0.065 * DS 
            D  = -0.116 + 0.403 * S - 0.117 * S2
            E  =  6.749 + 2.452 * S - 0.226 * S2
            ES =          1.994 * S - 0.216 * S2
            GA =  0.897
            UL =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
            UL =  UL/alpq3
C...X * D = X * DBAR :
            AL =  2.811
            BE =  0.960 
            AK =  0.914 
            BK =  3.723 - 0.968 * S
            AG =  0.081 - 0.028 * DS 
            BG = -0.048 
            C  =  0.094 - 0.043 * DS  
            D  =  0.059 + 0.263 * S - 0.085 * S2
            E  =  6.808 + 2.239 * S - 0.108 * S2
            ES =  1.225 + 0.594 * S - 0.073 * S2
            GA =  1.084
            DL =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
            DL =  DL/alpq3
C...X * G :
            AL =  2.024
            BE =  0.770 
            AK =  0.081 
            BK =  0.848 
            AG =  0.012 + 0.039 * DS
            BG = -0.056 - 0.044 * S
            C  =  0.043 + 0.031 * S  
            D  =  0.925 + 0.316 * S 
            E  =  3.129 + 2.434 * S - 0.115 * S2
            ES =  1.364 + 1.227 * S - 0.128 * S2
            GA =  1.262
            GL =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
            GL =  GL/alpq3
C...X * S = X * SBAR :
           SL = DL     
       end
C
C Next-to-leading Order, Point-Like
C P2=0 !
       SUBROUTINE GRSGHOPL (X, Q2, UH, DH, SH, GH)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.4
       LAM2 = 0.299*0.299
       S = DLOG(DLOG(Q2/LAM2)/DLOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * U = X * UBAR :
            AL =  2.107 
            BE =  0.970 
            AK =  0.412 - 0.115 * DS
            BK =  4.544 - 0.563 * S
            AG =        - 0.028 * DS  + 0.019 * S2
            BG =  0.263 + 0.137 * S
            C  =  6.726 - 3.264 * DS  - 0.166 * S2
            D  =  1.145               - 0.131 * S2
            E  =  4.122 + 3.170 * S   - 0.598 * S2
            ES =          1.615 * S   - 0.321 * S2
            GA =  1.051
            UH =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
C...X * D = X * DBAR :
            AL =  1.812 
            BE =  0.457 
            AK =  0.416 - 0.173 * DS
            BK =  4.489 - 0.827 * S
            AG =        - 0.010 * DS  + 0.006 * S2
            BG =  0.064 + 0.020 * S
            C  =  1.577 - 0.916 * DS  
            D  =  1.122 - 0.093 * S   - 0.100 * S2
            E  =  5.240 + 1.666 * S   - 0.234 * S2
            ES =          1.284 * S 
            GA =  1.043
            DH =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
C...X * G :
            AL =  1.773 
            BE =  1.666
            AK =  0.844 - 0.820 * DS
            BK =  2.302 - 0.474 * S
            AG =  0.194
            BG = -0.324 + 0.143 * S
            C  =  0.330 - 0.177 * S 
            D  =  0.778 + 0.502 * S - 0.154 * S2
            E  =  2.895 + 1.823 * S - 0.441 * S2
            ES =  2.344 - 0.584 * S
            GA =  0.901
            GH =  F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
C...X * S = X * SBAR :
            SH = DH     
       end
C
C Point-Like Photon 
C
C... GA = alpha; AL = alpha'; BE = beta
C... AK = a; AG = A
C... BK = b; BG = B
C... C = C; D = D; E = E; E' = ES 
       FUNCTION F (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES, GA)
       IMPLICIT DOUBLE PRECISION (A - Z)
       SX = SQRT (X)
       LX = DLOG (1./X)
       F  = (S**GA * X**AK * (AG + BG * SX + C * X**BK)  +  S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END

C The pion is a vdm-like input for the photon
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                   *
*       G R S  -  P I O N  - P A R A M E T R I Z A T I O N S        *
*                                                                   *
*                                1999                               *
*                                                                   *
*                  For a detailed explanation see                   *
*                M. Glueck, E. Reya, I. Schienbein :                *
*                   hep-ph/9903288  =  DO-TH 99/01                  *
*                  (To appear in ................)                  *
*                                                                   *
*   The parametrizations are fitted to the parton distributions     *
*                               for                                 *
*                0.5 GeV**2 =< Q**2 =< 1.E-5 GeV**2                 *
*                               and                                 * 
*                          1.E-5 =< x =< 1.                         *                  
*   Regions, where the distribution under consideration is neg-     *
*   ligible, were excluded from the fit.                            *
*                                                                   *
*                                                                   *
*   INPUT:   X  = Bjorken-x       (between  1.E-5 and 1   )         *
*            Q2 = Scale in GeV**2 (between  0.5   and 1.E5)         *
*                                                                   *
*   OUTPUT:  VAP = VALENCE : VAP = U_VAL(PI+) = DBAR_VAL(PI+) = ... *
*                  N O T THE SUM, I.E., TOTAL VALENCE DENSITY       *
*            QBP = SU(2) SYMMETRIC LIGHT SEA :                      *
*                  QBP = UBAR(PI+) = D(PI+) = ...                   *
*            SBP = STRANGE SEA : SBP = S = SBAR                     *
*            GLP = GLUON                                            *
*                                                                   *
*            Always x times the distribution is returned            *
*                                                                   *
*   ALPHAS:                                                         *
*   At Q^2 = MZ^2, alpha_s reads  0.114 (0.125) in NLO (LO); the    *
*   heavy quark thresholds, Qh^2 = mh^2, in the beta function are   *
*                   mc = 1.4 GeV,  mb = 4.5 GeV.                    *
*   Note that the NLO alpha_s running is different from GRV(94).    * 
*                                                                   *
*   Questions, comments etc to: schien@hal1.physik.uni-dortmund.de  *
*                                                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C X U(Pi^0) = X UBAR(Pi^0) = (VAP + 2 QBP)/2 
C X D(Pi^0) = X DBAR(Pi^0) = (VAP + 2 QBP)/2 
C X S = X SBAR = SBP
C X G = GLP
C
       SUBROUTINE GRSPILO (X, Q2, VAP, GLP, QBP, SBP)
       IMPLICIT DOUBLE PRECISION (A - Z)
       MU2  = 0.26
       LAM2 = 0.204 * 0.204
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * VALENCE :
       NV  =  0.606 + 0.249 * S + 0.005 * S2
       AKV =  0.517 - 0.020 * S
       AGV =  -0.037 - 0.578 * S
       BGV =  0.241 + 0.251 * S
       DV  =  0.383 + 0.624 * S
       VAP =  FVP (X, NV, AKV, AGV, BGV, DV)
C...X * GLUON :
       ALG =  0.504
       BEG =  0.226
       AKG =  2.251 - 1.339 * DS
       BKG =  0.0
       AGG =  2.668 - 1.265 * S  + 0.156 * S2
       BGG =  -1.839 + 0.386 * S
       CG  =  -1.014 + 0.920 * S  - 0.101 * S2
       DG  =  -0.077 + 1.466 * S
       EG  =  1.245 + 1.833 * S
       ESG =  0.510 + 3.844 * S
       GLP =  FGP (X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * QBAR (LIGHT SEA) :
       ALS =  1.147
       BES =  1.241
       AKS =  0.309 - 0.134 * DS
       BKS =  0.893 - 0.264 * DS
       AGS =  0.219 - 0.054 * S  
       BGS =  -0.593 + 0.240 * S
       CS  =  1.100 - 0.452 * S  
       DS  =  3.526 + 0.491 * S
       ES  =  4.521 + 1.583 * S
       ESS =  3.102
       QBP =  FGP (X, S, ALS, BES, AKS, BKS, AGS, BGS, CS, DS, ES, ESS)
C...X * SBAR = X * S :
       ALSTR =   0.823
       BESTR =   0.650
       AKSTR =   1.036 - 0.709 * S
       AGSTR =   -1.245 + 0.713 * S 
       BGSTR =   5.580 - 1.281 * S
       DSTR  =   2.746 - 0.191 * S
       ESTR  =   5.101 + 1.294 * S
       ESSTR =   4.854 - 0.437 * S
       SBP   =   FSP (X, S, ALSTR, BESTR, AKSTR, AGSTR, BGSTR
     #                     , DSTR, ESTR, ESSTR)
       RETURN
       END
C
C
       SUBROUTINE GRSPIHO (X, Q2, VAP, GLP, QBP, SBP)
       IMPLICIT double precision (A - Z)
       MU2  = 0.4
       LAM2 = 0.299 * 0.299
       S  = LOG (LOG(Q2/LAM2) / LOG(MU2/LAM2))
       DS = SQRT (S)
       S2 = S * S
C...X * VALENCE :
       NV  =  0.750 + 0.263 * S - 0.025 * S2
       AKV =  0.560 - 0.034 * S
       AGV =  -0.357 - 0.458 * S
       BGV =  0.427 + 0.220 * S
       DV  =  0.475 + 0.550 * S
       VAP =  FVP (X, NV, AKV, AGV, BGV, DV)
C...X * GLUON :
       ALG =  0.793
       BEG =  1.722
       AKG =  1.418 - 0.215 * DS
       BKG =  0.0
       AGG =  5.392 + 0.553 * S  - 0.385 * S2
       BGG =  -11.928 + 1.844 * S
       CG  =  11.548 - 4.316 * S  + 0.382 * S2
       DG  =  1.347 + 1.135 * S
       EG  =  0.104 + 1.980 * S
       ESG =  2.375 - 0.188 * S
       GLP =  FGP (X, S, ALG, BEG, AKG, BKG, AGG, BGG, CG, DG, EG, ESG)
C...X * QBAR (LIGHT SEA) :
       ALS =  1.118
       BES =  0.457
       AKS =  0.111 - 0.326 * DS
       BKS =  -0.978 - 0.488 * DS
       AGS =  1.035 - 0.295 * S  
       BGS =  -3.008 + 1.165 * S
       CS  =  4.111 - 1.575 * S  
       DS  =  6.192 + 0.705 * S
       ES  =  5.035 + 0.997 * S
       ESS =  1.486 + 1.288 * S
       QBP =  FGP (X, S, ALS, BES, AKS, BKS, AGS, BGS, CS, DS, ES, ESS)
C...X * SBAR = X * S :
       ALSTR =   0.908
       BESTR =   0.812
       AKSTR =   -0.567 - 0.466 * S
       AGSTR =   -2.348 + 1.433 * S 
       BGSTR =   4.403 
       DSTR  =   2.061
       ESTR  =   3.796 + 1.618 * S
       ESSTR =   0.309 + 0.355 * S
       SBP   =   FSP (X, S, ALSTR, BESTR, AKSTR, AGSTR, BGSTR
     #                     , DSTR, ESTR, ESSTR)
       RETURN
       END
C
C PION 
C
c... AK = a; AG = A
c... BK = b; BG = B
c... AL = alpha; BE = beta
c... E' = ES; N = N; C = C; D = D; E = E
c valence
       FUNCTION FVP (X, N, AK, AG, BG, D)
       IMPLICIT double precision (A - Z)
       DX = SQRT (X)
       FVP = N * X**AK * (1.+ AG*DX + BG*X) * (1.- X)**D
       RETURN
       END
C gluon and light sea
       FUNCTION FGP (X, S, AL, BE, AK, BK, AG, BG, C, D, E, ES)
       IMPLICIT double precision (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       FGP = (X**AK * (AG + BG*DX + C*X) * LX**BK + S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX))) * (1.- X)**D
       RETURN
       END
C strange sea
       FUNCTION FSP (X, S, AL, BE, AK, AG, BG, D, E, ES)
       IMPLICIT double precision (A - Z)
       DX = SQRT (X)
       LX = LOG (1./X)
       FSP = (1. + AG*DX + BG*X) / LX**AK * S**AL
     1       * EXP (-E + SQRT (ES * S**BE * LX)) * (1.- X)**D
       RETURN
       END

*********************************************************************
C END GLUECK REYA SCHIENBEIN PHOTON
C
C LAC1 PHOTON
      SUBROUTINE XLAC(MODE,Q2,X,FX,NF)
      REAL FX(-NF:NF)
      REAL*8 XPDF(-6:6)
      REAL*8 XMIN,XMAX,QSQMIN,QSQMAX,QSQ,DX,DQ
      real*8 ixmin,ixmax,iqsqmin,iqsqmax
      DATA XMIN,XMAX,QSQMIN,QSQMAX/1.D-4,1.D0,4.D0,1.D5/
      DATA INI/0/
      IF(INI.GT.0) GO TO 1
      ilxmin=0
      ilxmax=0
      ilqsqmin=0
      ilqsqmax=0
      INI=1
1     CONTINUE
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      if(Dx.lt.xmin) then
        ixmin=ixmin+1.
        if(log10(ixmin).gt.ilxmin) then
          write(*,*)' x < xmin in str. functions more than 10**',
     +  ilxmin,' times'                          
          ilxmin=ilxmin+1
        endif
      endif
      if(Dx.gt.xmax) then
        ixmax=ixmax+1.
        if(log10(ixmax).gt.ilxmax) then
          write(*,*)' x > xmax in str. functions more than 10**',
     +  ilxmax,' times'
          ilxmax=ilxmax+1
        endif
      endif
      qsq=DQ**2
      if(qsq.lt.qsqmin) then
        iqsqmin=iqsqmin+1.
        if(log10(iqsqmin).gt.ilqsqmin) then
          write(*,*)'q**2 < min q**2 in str. functions more than 10**',
     +  ilqsqmin,' times'
          ilqsqmin=ilqsqmin+1
        endif
      endif
      if(qsq.gt.qsqmax) then
        iqsqmax=iqsqmax+1.
        if(log10(iqsqmax).gt.ilqsqmax) then
          write(*,*)'q**2 > max q**2 in str. functions more than 10**',
     +  ilqsqmax,' times'
          ilqsqmax=ilqsqmax+1
        endif
      endif
      IF(MODE.EQ.1)THEN
        CALL LAC1_PH(DX,QSQ,XPDF)
      ELSE
        WRITE(*,*)'Set is not implemented'
        STOP
      ENDIF
      DO I=-NF,NF
       FX(I)=SNGL(XPDF(I))/X
      ENDDO
      RETURN
      END


      SUBROUTINE LAC1_PH(X,Q2,XPDF)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      PARAMETER(IX=100,IQ=7,NARG=2,NFUN=4)
      DOUBLE PRECISION
     +       DBFINT,
     +       XT(IX),Q2T(IQ),ARG(NARG),ENT(IX+IQ),
     +       XPV(IX,IQ,0:NFUN),XPDF(-6:6),XA(6)
      DIMENSION NA(NARG)
      DATA ZEROD/0.D0/
      DATA INIT/0/
C...100 x valuse; in (D-4,.77) log spaced (78 points)
C...              in (.78,.995) lineary spaced (22 points)
      DATA Q2T/4.D0,10.D0,50.D0,1.D2,1.D3,1.D4,1.D5/
      DATA XT/
     &0.1000D-03,0.1123D-03,0.1262D-03,0.1417D-03,0.1592D-03,0.1789D-03,
     &0.2009D-03,0.2257D-03,0.2535D-03,0.2848D-03,0.3199D-03,0.3593D-03,
     &0.4037D-03,0.4534D-03,0.5093D-03,0.5722D-03,0.6427D-03,0.7220D-03,
     &0.8110D-03,0.9110D-03,0.1023D-02,0.1150D-02,0.1291D-02,0.1451D-02,
     &0.1629D-02,0.1830D-02,0.2056D-02,0.2310D-02,0.2594D-02,0.2914D-02,
     &0.3274D-02,0.3677D-02,0.4131D-02,0.4640D-02,0.5212D-02,0.5855D-02,
     &0.6577D-02,0.7388D-02,0.8299D-02,0.9323D-02,0.1047D-01,0.1176D-01,
     &0.1321D-01,0.1484D-01,0.1667D-01,0.1873D-01,0.2104D-01,0.2363D-01,
     &0.2655D-01,0.2982D-01,0.3350D-01,0.3763D-01,0.4227D-01,0.4748D-01,
     &0.5334D-01,0.5992D-01,0.6731D-01,0.7560D-01,0.8493D-01,0.9540D-01,
     &0.1072D+00,0.1204D+00,0.1352D+00,0.1519D+00,0.1706D+00,0.1917D+00,
     &0.2153D+00,0.2419D+00,0.2717D+00,0.3052D+00,0.3428D+00,0.3851D+00,
     &0.4326D+00,0.4859D+00,0.5458D+00,0.6131D+00,0.6887D+00,0.7737D+00,
     &0.7837D+00,0.7937D+00,0.8037D+00,0.8137D+00,0.8237D+00,0.8337D+00,
     &0.8437D+00,0.8537D+00,0.8637D+00,0.8737D+00,0.8837D+00,0.8937D+00,
     &0.9037D+00,0.9137D+00,0.9237D+00,0.9337D+00,0.9437D+00,0.9537D+00,
     &0.9637D+00,0.9737D+00,0.9837D+00,0.9937D+00/
      IF(INIT.NE.0) GOTO 10
      INIT=1
      open(unit=55,file='LAC1',status='old')
200   format(6(1x,f10.2))
300   format(4(1x,f10.2))
      ir=1+(ix-1)/6
      do jq=1,iq
        do jp=0,nfun
          do jx=1,ir-1
             read(55,200)xa(1),xa(2),xa(3),xa(4),xa(5),xa(6)
             do jj=(jx-1)*6+1,jx*6
               xpv(jj,jq,jp)=xa(jj-(jx-1)*6)
             enddo
          enddo
          read(55,200)xa(1),xa(2),xa(3),xa(4)
          do jj=(ir-1)*6+1,ix
            xpv(jj,jq,jp)=xa(jj-(ir-1)*6)
          enddo
        enddo
      enddo
      close(55)
10    continue
      DO  5 IP=-6,6
        XPDF(IP)=ZEROD
 5    CONTINUE
      DO 2 I=1,IX
        ENT(I)=LOG10(XT(I))
  2   CONTINUE
      NA(1)=IX
      NA(2)=IQ
      DO 3 I=1,IQ
        ENT(IX+I)=LOG10(Q2T(I))
   3  CONTINUE
      ARG(1)=LOG10(X)
      ARG(2)=LOG10(Q2)
C
      XPDF(0)=DBFINT(NARG,ARG,NA,ENT,XPV(1,1,0))
      XPDF(1)=DBFINT(NARG,ARG,NA,ENT,XPV(1,1,1))
      XPDF(2)=DBFINT(NARG,ARG,NA,ENT,XPV(1,1,2))
      XPDF(3)=DBFINT(NARG,ARG,NA,ENT,XPV(1,1,3))
      XPDF(4)=DBFINT(NARG,ARG,NA,ENT,XPV(1,1,4))
      DO 21 JF=1,6
        XPDF(-JF)=XPDF(JF)
 21   CONTINUE
      RETURN
      END


      DOUBLE PRECISION FUNCTION DBFINT(NARG,ARG,NA,ENT,TABLE)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
             INTEGER NA(NARG), INDEX(32)
      DOUBLE PRECISION
     +       ARG(NARG),ENT(10),TABLE(10),WEIGHT(32)
      DATA ZEROD/0.D0/ONED/1.D0/
C
           DBFINT =  ZEROD
           IF(NARG .LT. 1  .OR.  NARG .GT. 5)  RETURN
C
           LMAX      =  0
           ISTEP     =  1
           KNOTS     =  1
           INDEX(1)  =  1
           WEIGHT(1) =  ONED
           DO 100    N  =  1, NARG
              X     =  ARG(N)
              NDIM  =  NA(N)
              LOCA  =  LMAX
              LMIN  =  LMAX + 1
              LMAX  =  LMAX + NDIM
              IF(NDIM .GT. 2)  GOTO 10
              IF(NDIM .EQ. 1)  GOTO 100
              H  =  X - ENT(LMIN)
              IF(H .EQ. ZEROD)  GOTO 90
              ISHIFT  =  ISTEP
              IF(X-ENT(LMIN+1) .EQ. ZEROD)  GOTO 21
              ISHIFT  =  0
              ETA     =  H / (ENT(LMIN+1) - ENT(LMIN))
              GOTO 30
   10         LOCB  =  LMAX + 1
   11         LOCC  =  (LOCA+LOCB) / 2
              IF(X-ENT(LOCC))  12, 20, 13
   12         LOCB  =  LOCC
              GOTO 14
   13         LOCA  =  LOCC
   14         IF(LOCB-LOCA .GT. 1)  GOTO 11
              LOCA    =  MIN ( MAX (LOCA,LMIN), LMAX-1 )
              ISHIFT  =  (LOCA - LMIN) * ISTEP
              ETA     =  (X - ENT(LOCA)) / (ENT(LOCA+1) - ENT(LOCA))
              GOTO 30
   20         ISHIFT  =  (LOCC - LMIN) * ISTEP
   21         DO 22  K  =  1, KNOTS
                 INDEX(K)  =  INDEX(K) + ISHIFT
   22            CONTINUE
              GOTO 90
   30         DO 31  K  =  1, KNOTS
                 INDEX(K)         =  INDEX(K) + ISHIFT
                 INDEX(K+KNOTS)   =  INDEX(K) + ISTEP
                 WEIGHT(K+KNOTS)  =  WEIGHT(K) * ETA
                 WEIGHT(K)        =  WEIGHT(K) - WEIGHT(K+KNOTS)
   31            CONTINUE
              KNOTS  =  2*KNOTS
   90         ISTEP  =  ISTEP * NDIM
  100         CONTINUE
           DO 200    K  =  1, KNOTS
              I  =  INDEX(K)
              DBFINT =  DBFINT + WEIGHT(K) * TABLE(I)
  200         CONTINUE
           RETURN
           END
C END LAC1 PHOTON
C-------------------------------------------------------------------
      SUBROUTINE NOSETP
      WRITE(*,*) ' SET OF STRUCTURE FUNCTIONS NOT IMPLEMENTED'
      WRITE(*,*) ' OR'
      WRITE(*,*) ' HADRON TYPE NOT DESCRIBED BY THE REQUESTED SET:'
      WRITE(*,*) ' IH =    1    2    3    -1    -2    -3    4    5'
      WRITE(*,*) ' HAD=    P    N    PI+  PBAR  NBAR  PI-  PH   EL'
      STOP
      END
C
C----------------------------------------------------------------------------
C-------------------------------------------------------------------
C------- ALPHA QCD -------------------------------------
c Program to calculate alfa strong with nf flavours,
c as a function of lambda with 5 flavors.
c The value of alfa is matched at the thresholds q = mq.
c When invoked with nf < 0 it chooses nf as the number of
c flavors with mass less then q.
c
      function alfas(q2,xlam,inf)
      implicit real * 8 (a-h,o-z)
      data olam/0.d0/,pi/3.14159d0/
      data xmb/5.d0/,xmc/1.5d0/
      if(xlam.ne.olam) then
        olam = xlam
        b5  = (33-2*5)/pi/12
        bp5 = (153 - 19*5) / pi / 2 / (33 - 2*5)
        b4  = (33-2*4)/pi/12
        bp4 = (153 - 19*4) / pi / 2 / (33 - 2*4)
        b3  = (33-2*3)/pi/12
        bp3 = (153 - 19*3) / pi / 2 / (33 - 2*3)
        xlc = 2 * log(xmc/xlam)
        xlb = 2 * log(xmb/xlam)
        xllc = log(xlc)
        xllb = log(xlb)
        c45  =  1/( 1/(b5 * xlb) - xllb*bp5/(b5 * xlb)**2 )
     #        - 1/( 1/(b4 * xlb) - xllb*bp4/(b4 * xlb)**2 )
        c35  =  1/( 1/(b4 * xlc) - xllc*bp4/(b4 * xlc)**2 )
     #        - 1/( 1/(b3 * xlc) - xllc*bp3/(b3 * xlc)**2 ) + c45
      endif
      q   = sqrt(q2)
      xlq = 2 * log( q/xlam )
      xllq = log( xlq )
      nf = inf
      if( nf .lt. 0) then
        if( q .gt. xmb ) then
          nf = 5
        elseif( q .gt. xmc ) then
          nf = 4
        else
          nf = 3
        endif
      endif
      if    ( nf .eq. 5 ) then
        alfas = 1/(b5 * xlq) -  bp5/(b5 * xlq)**2 * xllq
      elseif( nf .eq. 4 ) then
        alfas = 1/( 1/(1/(b4 * xlq) - bp4/(b4 * xlq)**2 * xllq) + c45 )
      elseif( nf .eq. 3 ) then
        alfas = 1/( 1/(1/(b3 * xlq) - bp3/(b3 * xlq)**2 * xllq) + c35 )
      else
        print *,'error in alfa: unimplemented # of light flavours',nf
        stop
      endif
      return
      end
c-------------------------------------------
c Program to calculate as with nf flavours
c as a function of lambda with nf flavours
c
      function alfa(q,xlam,nloop,nf)
      implicit real*8(a-h,o-z)
      data pi/3.1415926536d0/
      anf=dfloat(nf)
      b0=11D0-2D0/3D0*anf
      b1=51D0-19D0/3D0*anf
      b2=2857D0-5033D0/9D0*nf+325D0/27D0*anf**2

      t = 2.d0 * log( q/xlam )
      xlt = log( t )
      if (nloop.eq.1) then
      alfa = 4d0*Pi/(b0 * t)
      elseif (nloop.eq.2) then
      alfa = 4d0*Pi/(b0 * t)*(1D0-2D0*b1/b0**2 * xlt/t)
      elseif (nloop.eq.3) then
      alfa = 4d0*Pi/(b0 * t)*(1D0-2D0*b1/b0**2 * xlt/t
     #+4D0*b1**2/b0**4/t**2*((xlt-0.5D0)**2+b2*b0/8D0/b1**2-5D0/4D0))
      else
         write(*,*) ' cannot do ',nloop,' loops in alfa'
         stop
      endif
      return
      end       

c----------------------------------------------------------
c Program to get lambda_nf from as_nf at the scale q
c
      function xlambd(as,q,nloop,nf)
      data pi/3.14159/
      xlp = float(nloop-1)
      b  = (33-2*nf)/pi/12
      bp = (153 - 19*nf) / pi / 2 / (33 - 2*nf) * xlp
      t  = 1/b/as
c-----------------------------------------------------------
c Solve the equation
c
    1 xlt = log(t)
      ot = t
c-----------------------------------------------------------
c Solve the equation
c Value and Derivative of alfa with respect to t
c
      as0  = 1/b/t - bp*xlt/(b*t)**2
      as1  = - 1/b/t**2 -bp/b**2*(1-2*xlt)/t**3
      t  = (as-as0)/as1 + t
      if(abs(ot-t)/ot.gt..00001)goto 1
      xlambd = q/exp(t/2)
      return
      end


      SUBROUTINE MWARN(ROUT)
      CHARACTER*(*) ROUT
      WRITE(*,*) '***********************************************'
      WRITE(*,*) '***** WARNING CALLED FROM ROUTINE ',ROUT,':'
      END


c CTEQ6 START

C----- START CTEQ6 FITS ------------------------------
C Cteq6, added by P. Nason on 4-2-2002
      SUBROUTINE  CTEQ6(ISET,IH,Q2,X,FX,NF)
      REAL FX(-NF:NF)  
      REAL*8 DX,DQ,CTQ6PDF,PDFS(-NF:NF)
      DATA INIT/0/ 
C
      Q=SQRT(Q2)
      DQ=DBLE(Q)
      DX=DBLE(X)
      CALL SETCTQ6(ISET)
      DO I=-NF,NF
         PDFS(I)=CTQ6PDF(I,DX,DQ)
      ENDDO
C                         
      IF(ABS(IH).GE.3) CALL NOSETP
      IH0=IH
      IF(ABS(IH).EQ.2) IH0=ISIGN(1,IH)
C The function CTQ6PDF return the parton distribution inside the proton.
C The division by the factor DX is NOT needed
      FX(0)=SNGL(PDFS(0))
      FX(IH0)=SNGL(PDFS(1))
      FX(2*IH0)=SNGL(PDFS(2))
      FX(-IH0)=SNGL(PDFS(-1))
      FX(-2*IH0)=SNGL(PDFS(-2))
      DO I=3,NF              
        FX(I)=SNGL(PDFS(I))
      ENDDO          
      DO I=-NF,-3
        FX(I)=SNGL(PDFS(I))
      ENDDO          
C...TRANSFORM PROTON INTO NEUTRON
      IF(ABS(IH).EQ.2) THEN
        T=FX(1)
        FX(1)=FX(2)
        FX(2)=T
        T=FX(-1)
        FX(-1)=FX(-2)
        FX(-2)=T
      ENDIF
      END


C============================================================================
C                CTEQ Parton Distribution Functions: Version 6.0
C                             January 24, 2002
C
C   Ref: "New Generation of Parton Distributions with
C         Uncertainties from Global QCD Analysis"
C   By: J. Pumplin, D.R. Stump, J.Huston, H.L. Lai, P. Nadolsky, W.K. Tung
C       hep-ph/0201195
C
C   This package contains 3 standard sets of CTEQ6 PDF's and 40 up/down sets
C   with respect to CTEQ6M PDF's. Details are:
C ---------------------------------------------------------------------------
C  Iset   PDF        Description       Alpha_s(Mz)**Lam4  Lam5   Table_File
C ---------------------------------------------------------------------------
C   1    CTEQ6M   Standard MSbar scheme   0.118     326   226    cteq6m.tbl
C   2    CTEQ6D   Standard DIS scheme     0.118     326   226    cteq6d.tbl
C   3    CTEQ6L   Leading Order           0.118**   326** 226    cteq6l.tbl
C     ------------------------------
C   1xx  CTEQ6M1xx  +/- w.r.t. CTEQ6M     0.118     326   226    cteq6m1xx.tbl
C    (where xx=01--40)
C ---------------------------------------------------------------------------
C   ** ALL fits are obtained by using the same coupling strength \alpha_s(Mz)=0.118;
C   and the NLO running \alpha_s formula.  For the LO fit, the evolution of the PDF
C   and the hard cross sections are calculated at LO.  More detailed discussions are
C   given in hep-ph/0201195.
C
C   The table grids are generated for 10^-6 < x < 1 and 1.3 < Q < 10,000 (GeV).
C   PDF values outside of the above range are returned using extrapolation.
C   Lam5 (Lam4) represents Lambda value (in MeV) for 5 (4) flavors.
C   The matching alpha_s between 4 and 5 flavors takes place at Q=4.5 GeV,
C   which is defined as the bottom quark mass, whenever it can be applied.
C
C   The Table_Files are assumed to be in the working directory.
C
C   Before using the PDF, it is necessary to do the initialization by
C       Call SetCtq6(Iset)
C   where Iset is the desired PDF specified in the above table.
C
C   The function Ctq6Pdf (Iparton, X, Q)
C   returns the parton distribution inside the proton for parton [Iparton]
C   at [X] Bjorken_X and scale [Q] (GeV) in PDF set [Iset].
C   Iparton  is the parton label (5, 4, 3, 2, 1, 0, -1, ......, -5)
C                            for (b, c, s, d, u, g, u_bar, ..., b_bar),
C
C   For detailed information on the parameters used, e.q. quark masses,
C   QCD Lambda, ... etc.,  see info lines at the beginning of the
C   Table_Files.
C
C   These programs, as provided, are in double precision.  By removing the
C   "Implicit Double Precision" lines, they can also be run in single
C   precision.
C
C   If you have detailed questions concerning these CTEQ6 distributions,
C   or if you find problems/bugs using this package, direct inquires to
C   Pumplin@pa.msu.edu or Tung@pa.msu.edu.
C
C===========================================================================

      Function Ctq6Pdf (Iparton, X, Q)
      Implicit Double Precision (A-H,O-Z)
      Logical Warn
      Common
     > / K720CtqPar2 / Nx, Nt, NfMx
     > / K720QCDtable /  Alambda, Nfl, Iorder

      Data Warn /.true./
      save Warn

      If (X .lt. 0D0 .or. X .gt. 1D0) Then
        Print *, 'X out of range in Ctq6Pdf: ', X
        Stop
      Endif
      If (Q .lt. Alambda) Then
        Print *, 'Q out of range in Ctq6Pdf: ', Q
        Stop
      Endif
      If ((Iparton .lt. -NfMx .or. Iparton .gt. NfMx)) Then
         If (Warn) Then
C        put a warning for calling extra flavor.
             Warn = .false.
             Print *, 'Warning: Iparton out of range in Ctq6Pdf: '
     >              , Iparton
         Endif
         Ctq6Pdf = 0D0
         Return
      Endif

      Ctq6Pdf = PartonX6 (Iparton, X, Q)
      if(Ctq6Pdf.lt.0.D0)  Ctq6Pdf = 0.D0

      Return

C                             ********************
      End

      Subroutine SetCtq6 (Iset)
      Implicit Double Precision (A-H,O-Z)
      Parameter (Isetmax0=3)
      Character Flnm(Isetmax0)*6, nn*3, Tablefile*40
      Data (Flnm(I), I=1,Isetmax0)
     > / 'cteq6m', 'cteq6d', 'cteq6l' /
      Data Isetold, Isetmin0, Isetmin1, Isetmax1 /-987,1,101,140/
      save

C             If data file not initialized, do so.
      If(Iset.ne.Isetold) then
         IU= NextUn6()
         If (Iset.ge.Isetmin0 .and. Iset.le.Isetmax0) Then
            Tablefile=Flnm(Iset)
         Elseif (Iset.ge.Isetmin1 .and. Iset.le.Isetmax1) Then
            write(nn,'(I3)') Iset
            Tablefile=Flnm(1)//nn
         Else
            Print *, 'Invalid Iset number in SetCtq6 :', Iset
            Stop
         Endif
         write(*,*) 'Cteq6, set=',iset,'  file ',Tablefile
         Open(IU, File=Tablefile, Status='OLD', Err=100)
 21      Call ReadTbl6 (IU)
         Close (IU)
         Isetold=Iset
      Endif
      Return

 100  Print *, ' Data file ', Tablefile, ' cannot be opened '
     >//'in SetCtq6!!'
      Stop
C                             ********************
      End

      Subroutine ReadTbl6 (Nu)
      Implicit Double Precision (A-H,O-Z)
      Character Line*80
      PARAMETER (MXX = 96, MXQ = 20, MXF = 5)
      PARAMETER (MXPQX = (MXF + 3) * MXQ * MXX)
      Common
     > / K720CtqPar1 / Al, XV(0:MXX), TV(0:MXQ), UPD(MXPQX)
     > / K720CtqPar2 / Nx, Nt, NfMx
     > / K720XQrange / Qini, Qmax, Xmin
     > / K720QCDtable /  Alambda, Nfl, Iorder
     > / K720Masstbl / Amass(6)

      Read  (Nu, '(A)') Line
      Read  (Nu, '(A)') Line
      Read  (Nu, *) Dr, Fl, Al, (Amass(I),I=1,6)
      Iorder = Nint(Dr)
      Nfl = Nint(Fl)
      Alambda = Al

      Read  (Nu, '(A)') Line
      Read  (Nu, *) NX,  NT, NfMx

      Read  (Nu, '(A)') Line
      Read  (Nu, *) QINI, QMAX, (TV(I), I =0, NT)

      Read  (Nu, '(A)') Line
      Read  (Nu, *) XMIN, (XV(I), I =0, NX)

      Do 11 Iq = 0, NT
         TV(Iq) = Log(Log (TV(Iq) /Al))
   11 Continue
C
C                  Since quark = anti-quark for nfl>2 at this stage,
C                  we Read  out only the non-redundent data points
C     No of flavors = NfMx (sea) + 1 (gluon) + 2 (valence)

      Nblk = (NX+1) * (NT+1)
      Npts =  Nblk  * (NfMx+3)
      Read  (Nu, '(A)') Line
      Read  (Nu, *, IOSTAT=IRET) (UPD(I), I=1,Npts)

      Return
C                        ****************************
      End

      Function NextUn6()
C                                 Returns an unallocated FORTRAN i/o unit.
      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUn6 = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End
C

      SUBROUTINE POLINT6 (XA,YA,N,X,Y,DY)

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C                                        Adapted from "Numerical Recipes"
      PARAMETER (NMAX=10)
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END

      Function PartonX6 (IPRTN, XX, QQ)

c  Given the parton distribution function in the array U in
c  COMMON / PEVLDT / , this routine interpolates to find
c  the parton distribution at an arbitray point in x and q.
c
      Implicit Double Precision (A-H,O-Z)

      Parameter (MXX = 96, MXQ = 20, MXF = 5)
      Parameter (MXQX= MXQ * MXX,   MXPQX = MXQX * (MXF+3))

      Common
     > / K720CtqPar1 / Al, XV(0:MXX), TV(0:MXQ), UPD(MXPQX)
     > / K720CtqPar2 / Nx, Nt, NfMx
     > / K720XQrange / Qini, Qmax, Xmin

      Dimension fvec(4), fij(4)
      Dimension xvpow(0:mxx)
      Data OneP / 1.00001 /
      Data xpow / 0.3d0 /       !**** choice of interpolation variable
      Data nqvec / 4 /
      Data ientry / 0 /
      Save ientry,xvpow

c store the powers used for interpolation on first call...
      if(ientry .eq. 0) then
         ientry = 1

         xvpow(0) = 0D0
         do i = 1, nx
            xvpow(i) = xv(i)**xpow
         enddo
      endif

      X = XX
      Q = QQ
      tt = log(log(Q/Al))

c      -------------    find lower end of interval containing x, i.e.,
c                       get jx such that xv(jx) .le. x .le. xv(jx+1)...
      JLx = -1
      JU = Nx+1
 11   If (JU-JLx .GT. 1) Then
         JM = (JU+JLx) / 2
         If (X .Ge. XV(JM)) Then
            JLx = JM
         Else
            JU = JM
         Endif
         Goto 11
      Endif
C                     Ix    0   1   2      Jx  JLx         Nx-2     Nx
C                           |---|---|---|...|---|-x-|---|...|---|---|
C                     x     0  Xmin               x                 1
C
      If     (JLx .LE. -1) Then
        Print '(A,1pE12.4)', 'Severe error: x <= 0 in PartonX6! x = ', x
        Stop
      ElseIf (JLx .Eq. 0) Then
         Jx = 0
      Elseif (JLx .LE. Nx-2) Then

C                For interrior points, keep x in the middle, as shown above
         Jx = JLx - 1
      Elseif (JLx.Eq.Nx-1 .or. x.LT.OneP) Then

C                  We tolerate a slight over-shoot of one (OneP=1.00001),
C              perhaps due to roundoff or whatever, but not more than that.
C                                      Keep at least 4 points >= Jx
         Jx = JLx - 2
      Else
        Print '(A,1pE12.4)', 'Severe error: x > 1 in PartonX6! x = ', x
        Stop
      Endif
C          ---------- Note: JLx uniquely identifies the x-bin; Jx does not.

C                       This is the variable to be interpolated in
      ss = x**xpow

      If (JLx.Ge.2 .and. JLx.Le.Nx-2) Then

c     initiation work for "interior bins": store the lattice points in s...
      svec1 = xvpow(jx)
      svec2 = xvpow(jx+1)
      svec3 = xvpow(jx+2)
      svec4 = xvpow(jx+3)

      s12 = svec1 - svec2
      s13 = svec1 - svec3
      s23 = svec2 - svec3
      s24 = svec2 - svec4
      s34 = svec3 - svec4

      sy2 = ss - svec2
      sy3 = ss - svec3

c constants needed for interpolating in s at fixed t lattice points...
      const1 = s13/s23
      const2 = s12/s23
      const3 = s34/s23
      const4 = s24/s23
      s1213 = s12 + s13
      s2434 = s24 + s34
      sdet = s12*s34 - s1213*s2434
      tmp = sy2*sy3/sdet
      const5 = (s34*sy2-s2434*sy3)*tmp/s12
      const6 = (s1213*sy2-s12*sy3)*tmp/s34

      EndIf

c         --------------Now find lower end of interval containing Q, i.e.,
c                          get jq such that qv(jq) .le. q .le. qv(jq+1)...
      JLq = -1
      JU = NT+1
 12   If (JU-JLq .GT. 1) Then
         JM = (JU+JLq) / 2
         If (tt .GE. TV(JM)) Then
            JLq = JM
         Else
            JU = JM
         Endif
         Goto 12
       Endif

      If     (JLq .LE. 0) Then
         Jq = 0
      Elseif (JLq .LE. Nt-2) Then
C                                  keep q in the middle, as shown above
         Jq = JLq - 1
      Else
C                         JLq .GE. Nt-1 case:  Keep at least 4 points >= Jq.
        Jq = Nt - 3

      Endif
C                                   This is the interpolation variable in Q

      If (JLq.GE.1 .and. JLq.LE.Nt-2) Then
c                                        store the lattice points in t...
      tvec1 = Tv(jq)
      tvec2 = Tv(jq+1)
      tvec3 = Tv(jq+2)
      tvec4 = Tv(jq+3)

      t12 = tvec1 - tvec2
      t13 = tvec1 - tvec3
      t23 = tvec2 - tvec3
      t24 = tvec2 - tvec4
      t34 = tvec3 - tvec4

      ty2 = tt - tvec2
      ty3 = tt - tvec3

      tmp1 = t12 + t13
      tmp2 = t24 + t34

      tdet = t12*t34 - tmp1*tmp2

      EndIf


c get the pdf function values at the lattice points...

      If (Iprtn .GE. 3) Then
         Ip = - Iprtn
      Else
         Ip = Iprtn
      EndIf
      jtmp = ((Ip + NfMx)*(NT+1)+(jq-1))*(NX+1)+jx+1

      Do it = 1, nqvec

         J1  = jtmp + it*(NX+1)

       If (Jx .Eq. 0) Then
C                          For the first 4 x points, interpolate x^2*f(x,Q)
C                           This applies to the two lowest bins JLx = 0, 1
C            We can not put the JLx.eq.1 bin into the "interrior" section
C                           (as we do for q), since Upd(J1) is undefined.
         fij(1) = 0
         fij(2) = Upd(J1+1) * XV(1)**2
         fij(3) = Upd(J1+2) * XV(2)**2
         fij(4) = Upd(J1+3) * XV(3)**2
C
C                 Use Polint6 which allows x to be anywhere w.r.t. the grid

         Call Polint6 (XVpow(0), Fij(1), 4, ss, Fx, Dfx)

         If (x .GT. 0D0)  Fvec(it) =  Fx / x**2
C                                              Pdf is undefined for x.eq.0
       ElseIf  (JLx .Eq. Nx-1) Then
C                                                This is the highest x bin:

        Call Polint6 (XVpow(Nx-3), Upd(J1), 4, ss, Fx, Dfx)

        Fvec(it) = Fx

       Else
C                       for all interior points, use Jon's in-line function
C                              This applied to (JLx.Ge.2 .and. JLx.Le.Nx-2)
         sf2 = Upd(J1+1)
         sf3 = Upd(J1+2)

         g1 =  sf2*const1 - sf3*const2
         g4 = -sf2*const3 + sf3*const4

         Fvec(it) = (const5*(Upd(J1)-g1)
     &               + const6*(Upd(J1+3)-g4)
     &               + sf2*sy3 - sf3*sy2) / s23

       Endif

      enddo
C                                   We now have the four values Fvec(1:4)
c     interpolate in t...

      If (JLq .LE. 0) Then
C                         1st Q-bin, as well as extrapolation to lower Q
        Call Polint6 (TV(0), Fvec(1), 4, tt, ff, Dfq)

      ElseIf (JLq .GE. Nt-1) Then
C                         Last Q-bin, as well as extrapolation to higher Q
        Call Polint6 (TV(Nt-3), Fvec(1), 4, tt, ff, Dfq)
      Else
C                         Interrior bins : (JLq.GE.1 .and. JLq.LE.Nt-2)
C       which include JLq.Eq.1 and JLq.Eq.Nt-2, since Upd is defined for
C                         the full range QV(0:Nt)  (in contrast to XV)
        tf2 = fvec(2)
        tf3 = fvec(3)

        g1 = ( tf2*t13 - tf3*t12) / t23
        g4 = (-tf2*t34 + tf3*t24) / t23

        h00 = ((t34*ty2-tmp2*ty3)*(fvec(1)-g1)/t12
     &    +  (tmp1*ty2-t12*ty3)*(fvec(4)-g4)/t34)

        ff = (h00*ty2*ty3/tdet + tf2*ty3 - tf3*ty2) / t23
      EndIf

      PartonX6 = ff

      Return
C                                       ********************
      End

c CTEQ6 END


c BEGIN MRSTNNLO (2002) 
      subroutine mrst0201127
     #           (x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C  This is a package for the MRST 2002 NNLO parton distributionsC
C  Reference: A.D. Martin, R.G. Roberts, W.J. Stirling and      C
C  R.S. Thorne, hep-ph/0201127                                  C
C                                                               C
C  There are 4 pdf sets corresponding to mode = 1, 2, 3, 4      C
C                                                               C
C  Mode=1 gives the default set with Lambda(MSbar,nf=4) = 0.235 C
C  corresponding to alpha_s(M_Z) of 0.1155                      C
C  This set is the `average' of the slow and fast evolutions    C 
C  This set reads a grid whose first number is 0.00725          C
C                                                               C
C  Mode=2 gives the set with Lambda(MSbar,nf=4) = 0.235         C
C  corresponding to alpha_s(M_Z) of 0.1155                      C
C  This set is the fast evolution                               C 
C  This set reads a grid whose first number is 0.00734          C
C                                                               C
C  Mode=3 gives the set with Lambda(MSbar,nf=4) = 0.235         C
C  corresponding to alpha_s(M_Z) of 0.1155                      C
C  This set is the slow evolution                               C 
C  This set reads a grid whose first number is 0.00739          C
C                                                               C
C  Mode=4 gives the set MRSTNNLOJ which gives better agreement  C
C  with the Tevatron inclusive jet data but has unattractive    C
C  gluon behaviour at large x (see discussion in paper)         C
C  This set has Lambda(MSbar,nf=4) = 0.267(alpha_s(M_Z) =0.1180 C 
C  This set reads a grid whose first number is 0.00865          C
C                                                               C
C   This subroutine uses an improved interpolation procedure    C 
C   for extracting values of the pdf's from the grid            C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99,q2
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
      if(mode.eq.1) then
        call mrst10201127(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrst20201127(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrst30201127(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrst40201127(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu)
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ','q2= ',e10.5)
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ','x= ',e10.5)
      return
      end

      subroutine mrst10201127(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
c        write(*,*) ' mrstnnlo 1'
        open(unit=33,file='vnvalf1155',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      call mrscheck(f1(1,1),44)
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe1(nx,nq,xxl,qql,f1,cc1)
      call jeppe1(nx,nq,xxl,qql,f2,cc2)
      call jeppe1(nx,nq,xxl,qql,f3,cc3)
      call jeppe1(nx,nq,xxl,qql,f4,cc4)
      call jeppe1(nx,nq,xxl,qql,f6,cc6)
      call jeppe1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end
 
      subroutine mrst20201127(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
c        write(*,*) ' mrstnnlo 2'
        open(unit=33,file='vnvalf1155a',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      call mrscheck(f1(1,1),45)
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe1(nx,nq,xxl,qql,f1,cc1)
      call jeppe1(nx,nq,xxl,qql,f2,cc2)
      call jeppe1(nx,nq,xxl,qql,f3,cc3)
      call jeppe1(nx,nq,xxl,qql,f4,cc4)
      call jeppe1(nx,nq,xxl,qql,f6,cc6)
      call jeppe1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine mrst30201127(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
c        write(*,*) ' mrstnnlo 3'
        open(unit=33,file='vnvalf1155b',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      call mrscheck(f1(1,1),46)
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe1(nx,nq,xxl,qql,f1,cc1)
      call jeppe1(nx,nq,xxl,qql,f2,cc2)
      call jeppe1(nx,nq,xxl,qql,f3,cc3)
      call jeppe1(nx,nq,xxl,qql,f4,cc4)
      call jeppe1(nx,nq,xxl,qql,f6,cc6)
      call jeppe1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine mrst40201127(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
c        write(*,*) ' mrstnnlo 4'
        open(unit=33,file='vnvalf1180j',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      call mrscheck(f1(1,1),47)
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe1(nx,nq,xxl,qql,f1,cc1)
      call jeppe1(nx,nq,xxl,qql,f2,cc2)
      call jeppe1(nx,nq,xxl,qql,f3,cc3)
      call jeppe1(nx,nq,xxl,qql,f4,cc4)
      call jeppe1(nx,nq,xxl,qql,f6,cc6)
      call jeppe1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine jeppe1(nx,my,xx,yy,ff,cc)
      implicit real*8(a-h,o-z)
      PARAMETER(NNX=49,MMY=37)
      dimension xx(nx),yy(my),ff(nx,my),ff1(NNX,MMY),ff2(NNX,MMY),
     xff12(NNX,MMY),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
     xcl(16),cc(nx,my,4,4),iwt(16,16)

      data iwt/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     x		  -3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0,
     x		  2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0,
     x		  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
     x		  0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1,
     x		  0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1,
     x		  -3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0,
     x		  9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2,
     x		  -6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2,
     x		  2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0,
     x		  -6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1,
     x		  4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1/


      do 42 m=1,my
      dx=xx(2)-xx(1)
      ff1(1,m)=(ff(2,m)-ff(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff1(nx,m)=(ff(nx,m)-ff(nx-1,m))/dx
      do 41 n=2,nx-1
      ff1(n,m)=polderiv(xx(n-1),xx(n),xx(n+1),ff(n-1,m),ff(n,m),
     xff(n+1,m))
   41 continue
   42 continue

      do 44 n=1,nx
      dy=yy(2)-yy(1)
      ff2(n,1)=(ff(n,2)-ff(n,1))/dy
      dy=yy(my)-yy(my-1)
      ff2(n,my)=(ff(n,my)-ff(n,my-1))/dy
      do 43 m=2,my-1
      ff2(n,m)=polderiv(yy(m-1),yy(m),yy(m+1),ff(n,m-1),ff(n,m),
     xff(n,m+1))
   43 continue
   44 continue

      do 46 m=1,my
      dx=xx(2)-xx(1)
      ff12(1,m)=(ff2(2,m)-ff2(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff12(nx,m)=(ff2(nx,m)-ff2(nx-1,m))/dx
      do 45 n=2,nx-1
      ff12(n,m)=polderiv(xx(n-1),xx(n),xx(n+1),ff2(n-1,m),ff2(n,m),
     xff2(n+1,m))
   45 continue
   46 continue

      do 53 n=1,nx-1
      do 52 m=1,my-1
      d1=xx(n+1)-xx(n)
      d2=yy(m+1)-yy(m)
      d1d2=d1*d2

      yy0(1)=ff(n,m)
      yy0(2)=ff(n+1,m)
      yy0(3)=ff(n+1,m+1)
      yy0(4)=ff(n,m+1)

      yy1(1)=ff1(n,m)
      yy1(2)=ff1(n+1,m)
      yy1(3)=ff1(n+1,m+1)
      yy1(4)=ff1(n,m+1)

      yy2(1)=ff2(n,m)
      yy2(2)=ff2(n+1,m)
      yy2(3)=ff2(n+1,m+1)
      yy2(4)=ff2(n,m+1)

      yy12(1)=ff12(n,m)
      yy12(2)=ff12(n+1,m)
      yy12(3)=ff12(n+1,m+1)
      yy12(4)=ff12(n,m+1)

      do 47 k=1,4
      z(k)=yy0(k)
      z(k+4)=yy1(k)*d1
      z(k+8)=yy2(k)*d2
      z(k+12)=yy12(k)*d1d2
   47 continue

      do 49 l=1,16
      xxd=0.
      do 48 k=1,16
      xxd=xxd+iwt(k,l)*z(k)
   48 continue
      cl(l)=xxd
   49 continue
      l=0
      do 51 k=1,4
      do 50 j=1,4
      l=l+1
      cc(n,m,k,j)=cl(l)
   50 continue
   51 continue
   52 continue
   53 continue
      return
      end

      subroutine jeppe2(x,y,nx,my,xx,yy,cc,z)
      implicit real*8(a-h,o-z)
      dimension xx(nx),yy(my),cc(nx,my,4,4)      

      n=locx(xx,nx,x)
      m=locx(yy,my,y)

      t=(x-xx(n))/(xx(n+1)-xx(n))
      u=(y-yy(m))/(yy(m+1)-yy(m))

      z=0.
      do 1 l=4,1,-1
      z=t*z+((cc(n,m,l,4)*u+cc(n,m,l,3))*u
     .       +cc(n,m,l,2))*u+cc(n,m,l,1)
    1 continue
      return
      end

      integer function locx(xx,nx,x)
      implicit real*8(a-h,o-z)
      dimension xx(nx)
      if(x.le.xx(1)) then
      locx=1
      return
      endif
      if(x.ge.xx(nx)) then 
      locx=nx-1  
      return
      endif
      ju=nx+1
      jl=0
    1 if((ju-jl).le.1) go to 2
      jm=(ju+jl)/2
      if(x.ge.xx(jm)) then
      jl=jm
      else
      ju=jm
      endif
      go to 1
    2 locx=jl
      return
      end


      real*8 function  polderiv(x1,x2,x3,y1,y2,y3)
      implicit real*8(a-h,o-z)
      polderiv=(x3*x3*(y1-y2)-2.0*x2*(x3*(y1-y2)+x1*
     .(y2-y3))+x2*x2*(y1-y3)+x1*x1*(y2-y3))/((x1-x2)*(x1-x3)*(x2-x3))
      return
      end
c END MRSTNNLO (2002) 


      subroutine mrst2001E(x,q,n,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C  This is a package for the new MRST 2001 "NLO parton          C
C  distributions with errors" package, which allows estimates   C
C  of the uncertainties for given physical quantities according C
C  to the Hessian approach                                      C     
C  Reference: A.D. Martin, R.G. Roberts, W.J. Stirling and      C
C  R.S. Thorne, hep-ph/0211080                                  C
C                                                               C
C  There are 30 pdf "extremum" sets ("+" and "-" sets for each  C
C  of the 15 eigenvectors in parameter space) corresponding to  C
C  n = 1, ..,30 and a central "best fit" set given by n = 0.    C
C  The best fit set is very close to the previous MRST2001 set. C
C                                                               C
C  For a given physical quantity sigma(n) calculated with set n C
C  the prediction with error is therefore                       C
C                                                               C
C sigma(0) +- 1/2 sqrt[sum_i=1,15 {sigma(2i-1) - sigma(2i)}^2 ] C
C                                                               C
C  All 31 sets have Lambda(MSbar,nf=4) = 323 MeV corresponding  C
C  to alpha_s(M_Z) = 0.119                                      C
C                                                               C
C  The 31 grids are concatenated in mrst01E_hessian.dat - the   C
C  first row of which is                                        C
C                                                               C
C   0.00959   0.00189 -10.10634   0.85204   0.00000  ...        C
C                                                               C
C   This subroutine uses an improved interpolation procedure    C 
C   for extracting values of the pdf's from the grid            C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99,q2
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
      call mrst2001EE(n,x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ','q2= ',e10.5)
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ','x= ',e10.5)
      return
      end

      subroutine mrst2001EE(i,x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nhess=30,nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),
     .       f4(nx,nq),f5(nx,nq),f6(nx,nq),
     .       f7(nx,nq),f8(nx,nq),
     .       fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),
     .cc1(0:nhess,nx,nq,4,4),cc2(0:nhess,nx,nq,4,4),
     .cc3(0:nhess,nx,nq,4,4),cc4(0:nhess,nx,nq,4,4),
     .cc6(0:nhess,nx,nq,4,4),cc8(0:nhess,nx,nq,4,4),
     .ccc(0:nhess,nx,nqc,4,4),ccb(0:nhess,nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10

      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

        open(unit=33,file='mrst2001E_hessian',status='old')
        do j=0,nhess

        do n=1,nx-1
        do m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
        enddo
        enddo

      do m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
      enddo

      call jeppe3001E1(j,nx,nq,xxl,qql,f1,cc1)
      call jeppe3001E1(j,nx,nq,xxl,qql,f2,cc2)
      call jeppe3001E1(j,nx,nq,xxl,qql,f3,cc3)
      call jeppe3001E1(j,nx,nq,xxl,qql,f4,cc4)
      call jeppe3001E1(j,nx,nq,xxl,qql,f6,cc6)
      call jeppe3001E1(j,nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe3001E1(j,nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe3001E1(j,nx,nqb,xxl,qqlb,fb,ccb)

      enddo

      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe3001E2(i,xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe3001E2(i,xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe3001E2(i,xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end
 



      subroutine jeppe3001E1(i,nx,my,xx,yy,ff,cc)
      implicit real*8(a-h,o-z)
      parameter(nhess=30,nnx=49,mmy=37)
      dimension xx(nx),yy(my),ff(nnx,mmy),ff1(nnx,mmy),ff2(nnx,mmy),
     xff12(nnx,mmy),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
     xcl(16),cc(0:nhess,nx,my,4,4),iwt(16,16)

      data iwt/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     x		  -3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0,
     x		  2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0,
     x		  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
     x		  0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1,
     x		  0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1,
     x		  -3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0,
     x		  9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2,
     x		  -6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2,
     x		  2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0,
     x		  -6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1,
     x		  4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1/


      do 42 m=1,my
      dx=xx(2)-xx(1)
      ff1(1,m)=(ff(2,m)-ff(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff1(nx,m)=(ff(nx,m)-ff(nx-1,m))/dx
      do 41 n=2,nx-1
      ff1(n,m)=polderiv2001E(xx(n-1),xx(n),xx(n+1),ff(n-1,m),ff(n,m),
     xff(n+1,m))
   41 continue
   42 continue

      do 44 n=1,nx
      dy=yy(2)-yy(1)
      ff2(n,1)=(ff(n,2)-ff(n,1))/dy
      dy=yy(my)-yy(my-1)
      ff2(n,my)=(ff(n,my)-ff(n,my-1))/dy
      do 43 m=2,my-1
      ff2(n,m)=polderiv2001E(yy(m-1),yy(m),yy(m+1),ff(n,m-1),ff(n,m),
     xff(n,m+1))
   43 continue
   44 continue

      do 46 m=1,my
      dx=xx(2)-xx(1)
      ff12(1,m)=(ff2(2,m)-ff2(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff12(nx,m)=(ff2(nx,m)-ff2(nx-1,m))/dx
      do 45 n=2,nx-1
      ff12(n,m)=polderiv2001E(xx(n-1),xx(n),xx(n+1),ff2(n-1,m),ff2(n,m),
     xff2(n+1,m))
   45 continue
   46 continue

      do 53 n=1,nx-1
      do 52 m=1,my-1
      d1=xx(n+1)-xx(n)
      d2=yy(m+1)-yy(m)
      d1d2=d1*d2

      yy0(1)=ff(n,m)
      yy0(2)=ff(n+1,m)
      yy0(3)=ff(n+1,m+1)
      yy0(4)=ff(n,m+1)

      yy1(1)=ff1(n,m)
      yy1(2)=ff1(n+1,m)
      yy1(3)=ff1(n+1,m+1)
      yy1(4)=ff1(n,m+1)

      yy2(1)=ff2(n,m)
      yy2(2)=ff2(n+1,m)
      yy2(3)=ff2(n+1,m+1)
      yy2(4)=ff2(n,m+1)

      yy12(1)=ff12(n,m)
      yy12(2)=ff12(n+1,m)
      yy12(3)=ff12(n+1,m+1)
      yy12(4)=ff12(n,m+1)

      do 47 k=1,4
      z(k)=yy0(k)
      z(k+4)=yy1(k)*d1
      z(k+8)=yy2(k)*d2
      z(k+12)=yy12(k)*d1d2
   47 continue

      do 49 l=1,16
      xxd=0.
      do 48 k=1,16
      xxd=xxd+iwt(k,l)*z(k)
   48 continue
      cl(l)=xxd
   49 continue
      l=0
      do 51 k=1,4
      do 50 j=1,4
      l=l+1
      cc(i,n,m,k,j)=cl(l)
   50 continue
   51 continue
   52 continue
   53 continue
      return
      end

      subroutine jeppe3001E2(i,x,y,nx,my,xx,yy,cc,z)
      implicit real*8(a-h,o-z)
      parameter(nhess=30)
      dimension xx(nx),yy(my),cc(0:nhess,nx,my,4,4)      

      n=locx2001E(xx,nx,x)
      m=locx2001E(yy,my,y)

      t=(x-xx(n))/(xx(n+1)-xx(n))
      u=(y-yy(m))/(yy(m+1)-yy(m))

      z=0.
      do 1 l=4,1,-1
      z=t*z+((cc(i,n,m,l,4)*u+cc(i,n,m,l,3))*u
     .       +cc(i,n,m,l,2))*u+cc(i,n,m,l,1)
    1 continue
      return
      end

      integer function locx2001E(xx,nx,x)
      implicit real*8(a-h,o-z)
      dimension xx(nx)
      if(x.le.xx(1)) then
      locx2001E=1
      return
      endif
      if(x.ge.xx(nx)) then 
      locx2001E=nx-1  
      return
      endif
      ju=nx+1
      jl=0
    1 if((ju-jl).le.1) go to 2
      jm=(ju+jl)/2
      if(x.ge.xx(jm)) then
      jl=jm
      else
      ju=jm
      endif
      go to 1
    2 locx2001E=jl
      return
      end


      real*8 function  polderiv2001E(x1,x2,x3,y1,y2,y3)
      implicit real*8(a-h,o-z)
      polderiv2001E=(x3*x3*(y1-y2)-2.0*x2*(x3*(y1-y2)+x1*
     .(y2-y3))+x2*x2*(y1-y3)+x1*x1*(y2-y3))/((x1-x2)*(x1-x3)*(x2-x3))
      return
      end



      subroutine mrst2002(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C  This is a package for the new MRST 2002 updated NLO and      C
C  NNLO parton distributions.                                   C 
C  Reference: A.D. Martin, R.G. Roberts, W.J. Stirling and      C
C  R.S. Thorne, hep-ph/0211080                                  C
C                                                               C
C  There are 2 pdf sets corresponding to mode = 1, 2            C
C                                                               C
C  Mode=1 gives the NLO set with alpha_s(M_Z,NLO) = 0.1197      C  
C  This set reads a grid whose first number is 0.00949          C
C                                                               C
C  Mode=2 gives the NNLO set with alpha_s(M_Z,NNLO) = 0.1154    C
C  This set reads a grid whose first number is 0.00685          C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99,q2
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
      if(mode.eq.1) then
        call mrst2002_1(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrst2002_2(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ','q2= ',e10.5)
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ','x= ',e10.5)
      return
      end

      subroutine mrst2002_1(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='mrst2002nlo',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2002_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2002_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2002_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2002_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2002_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2002_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2002_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2002_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2002_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2002_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine mrst2002_2(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='mrst2002nnlo',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2002_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2002_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2002_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2002_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2002_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2002_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2002_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2002_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2002_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2002_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2002_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end
      subroutine jeppe2002_1(nx,my,xx,yy,ff,cc)
      implicit real*8(a-h,o-z)
      parameter(nnx=49,mmy=37)
      dimension xx(nx),yy(my),ff(nnx,mmy),ff1(nnx,mmy),ff2(nnx,mmy),
     xff12(nnx,mmy),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
     xcl(16),cc(nx,my,4,4),iwt(16,16)

      data iwt/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     x		  -3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0,
     x		  2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0,
     x		  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
     x		  0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1,
     x		  0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1,
     x		  -3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0,
     x		  9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2,
     x		  -6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2,
     x		  2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0,
     x		  -6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1,
     x		  4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1/


      do 42 m=1,my
      dx=xx(2)-xx(1)
      ff1(1,m)=(ff(2,m)-ff(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff1(nx,m)=(ff(nx,m)-ff(nx-1,m))/dx
      do 41 n=2,nx-1
      ff1(n,m)=pold2002(xx(n-1),xx(n),xx(n+1),ff(n-1,m),ff(n,m),
     xff(n+1,m))
   41 continue
   42 continue

      do 44 n=1,nx
      dy=yy(2)-yy(1)
      ff2(n,1)=(ff(n,2)-ff(n,1))/dy
      dy=yy(my)-yy(my-1)
      ff2(n,my)=(ff(n,my)-ff(n,my-1))/dy
      do 43 m=2,my-1
      ff2(n,m)=pold2002(yy(m-1),yy(m),yy(m+1),ff(n,m-1),ff(n,m),
     xff(n,m+1))
   43 continue
   44 continue

      do 46 m=1,my
      dx=xx(2)-xx(1)
      ff12(1,m)=(ff2(2,m)-ff2(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff12(nx,m)=(ff2(nx,m)-ff2(nx-1,m))/dx
      do 45 n=2,nx-1
      ff12(n,m)=pold2002(xx(n-1),xx(n),xx(n+1),ff2(n-1,m),ff2(n,m),
     xff2(n+1,m))
   45 continue
   46 continue

      do 53 n=1,nx-1
      do 52 m=1,my-1
      d1=xx(n+1)-xx(n)
      d2=yy(m+1)-yy(m)
      d1d2=d1*d2

      yy0(1)=ff(n,m)
      yy0(2)=ff(n+1,m)
      yy0(3)=ff(n+1,m+1)
      yy0(4)=ff(n,m+1)

      yy1(1)=ff1(n,m)
      yy1(2)=ff1(n+1,m)
      yy1(3)=ff1(n+1,m+1)
      yy1(4)=ff1(n,m+1)

      yy2(1)=ff2(n,m)
      yy2(2)=ff2(n+1,m)
      yy2(3)=ff2(n+1,m+1)
      yy2(4)=ff2(n,m+1)

      yy12(1)=ff12(n,m)
      yy12(2)=ff12(n+1,m)
      yy12(3)=ff12(n+1,m+1)
      yy12(4)=ff12(n,m+1)

      do 47 k=1,4
      z(k)=yy0(k)
      z(k+4)=yy1(k)*d1
      z(k+8)=yy2(k)*d2
      z(k+12)=yy12(k)*d1d2
   47 continue

      do 49 l=1,16
      xxd=0.
      do 48 k=1,16
      xxd=xxd+iwt(k,l)*z(k)
   48 continue
      cl(l)=xxd
   49 continue
      l=0
      do 51 k=1,4
      do 50 j=1,4
      l=l+1
      cc(n,m,k,j)=cl(l)
   50 continue
   51 continue
   52 continue
   53 continue
      return
      end

      subroutine jeppe2002_2(x,y,nx,my,xx,yy,cc,z)
      implicit real*8(a-h,o-z)
      dimension xx(nx),yy(my),cc(nx,my,4,4)      

      n=locx2002(xx,nx,x)
      m=locx2002(yy,my,y)

      t=(x-xx(n))/(xx(n+1)-xx(n))
      u=(y-yy(m))/(yy(m+1)-yy(m))

      z=0.
      do 1 l=4,1,-1
      z=t*z+((cc(n,m,l,4)*u+cc(n,m,l,3))*u
     .       +cc(n,m,l,2))*u+cc(n,m,l,1)
    1 continue
      return
      end

      integer function locx2002(xx,nx,x)
      implicit real*8(a-h,o-z)
      dimension xx(nx)
      if(x.le.xx(1)) then
      locx2002=1
      return
      endif
      if(x.ge.xx(nx)) then 
      locx2002=nx-1  
      return
      endif
      ju=nx+1
      jl=0
    1 if((ju-jl).le.1) go to 2
      jm=(ju+jl)/2
      if(x.ge.xx(jm)) then
      jl=jm
      else
      ju=jm
      endif
      go to 1
    2 locx2002=jl
      return
      end


      real*8 function  pold2002(x1,x2,x3,y1,y2,y3)
      implicit real*8(a-h,o-z)
      pold2002=(x3*x3*(y1-y2)-2.0*x2*(x3*(y1-y2)+x1*
     .(y2-y3))+x2*x2*(y1-y3)+x1*x1*(y2-y3))/((x1-x2)*(x1-x3)*(x2-x3))
      return
      end


      subroutine mrst2001lo(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C  This is a package for the new MRST 2001 LO parton            C
C  distributions.                                               C     
C  Reference: A.D. Martin, R.G. Roberts, W.J. Stirling and      C
C  R.S. Thorne, hep-ph/0201xxx                                  C
C                                                               C
C  There is 1 pdf set corresponding to mode = 1                 C
C                                                               C
C  Mode=1 gives the default set with Lambda(MSbar,nf=4) = 0.220 C
C  corresponding to alpha_s(M_Z) of 0.130                       C
C  This set reads a grid whose first number is 0.02868          C
C                                                               C
C   This subroutine uses an improved interpolation procedure    C 
C   for extracting values of the pdf's from the grid            C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99,q2
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
      if(mode.eq.1) then
        call mrst2001lo1(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ','q2= ',e10.5)
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ','x= ',e10.5)
      return
      end

      subroutine mrst2001lo1(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='lo2002',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2001lo1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2001lo1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2001lo1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2001lo1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2001lo1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2001lo1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2001lo1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2001lo1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2001lo2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2001lo2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2001lo2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end
 
c      subroutine jeppe1(nx,my,xx,yy,ff,cc)
c      implicit real*8(a-h,o-z)
c      dimension xx(nx),yy(my),ff(nx,my),ff1(nx,my),ff2(nx,my),
c     xff12(nx,my),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
c     xcl(16),cc(nx,my,4,4),iwt(16,16)

      subroutine jeppe2001lo1(nx,my,xx,yy,ff,cc)
      implicit real*8(a-h,o-z)
      PARAMETER(NNX=49,MMY=37)
      dimension xx(nx),yy(my),ff(nx,my),ff1(NNX,MMY),ff2(NNX,MMY),
     xff12(NNX,MMY),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
     xcl(16),cc(nx,my,4,4),iwt(16,16)

      data iwt/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     x		  -3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0,
     x		  2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0,
     x		  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
     x		  0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1,
     x		  0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1,
     x		  -3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0,
     x		  9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2,
     x		  -6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2,
     x		  2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0,
     x		  -6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1,
     x		  4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1/


      do 42 m=1,my
      dx=xx(2)-xx(1)
      ff1(1,m)=(ff(2,m)-ff(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff1(nx,m)=(ff(nx,m)-ff(nx-1,m))/dx
      do 41 n=2,nx-1
      ff1(n,m)=polderiv2001lo(xx(n-1),xx(n),xx(n+1),ff(n-1,m),
     xff(n,m),ff(n+1,m))
   41 continue
   42 continue

      do 44 n=1,nx
      dy=yy(2)-yy(1)
      ff2(n,1)=(ff(n,2)-ff(n,1))/dy
      dy=yy(my)-yy(my-1)
      ff2(n,my)=(ff(n,my)-ff(n,my-1))/dy
      do 43 m=2,my-1
      ff2(n,m)=polderiv2001lo(yy(m-1),yy(m),yy(m+1),ff(n,m-1),
     xff(n,m),ff(n,m+1))
   43 continue
   44 continue

      do 46 m=1,my
      dx=xx(2)-xx(1)
      ff12(1,m)=(ff2(2,m)-ff2(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff12(nx,m)=(ff2(nx,m)-ff2(nx-1,m))/dx
      do 45 n=2,nx-1
      ff12(n,m)=polderiv2001lo(xx(n-1),xx(n),xx(n+1),ff2(n-1,m),
     xff2(n,m),ff2(n+1,m))
   45 continue
   46 continue

      do 53 n=1,nx-1
      do 52 m=1,my-1
      d1=xx(n+1)-xx(n)
      d2=yy(m+1)-yy(m)
      d1d2=d1*d2

      yy0(1)=ff(n,m)
      yy0(2)=ff(n+1,m)
      yy0(3)=ff(n+1,m+1)
      yy0(4)=ff(n,m+1)

      yy1(1)=ff1(n,m)
      yy1(2)=ff1(n+1,m)
      yy1(3)=ff1(n+1,m+1)
      yy1(4)=ff1(n,m+1)

      yy2(1)=ff2(n,m)
      yy2(2)=ff2(n+1,m)
      yy2(3)=ff2(n+1,m+1)
      yy2(4)=ff2(n,m+1)

      yy12(1)=ff12(n,m)
      yy12(2)=ff12(n+1,m)
      yy12(3)=ff12(n+1,m+1)
      yy12(4)=ff12(n,m+1)

      do 47 k=1,4
      z(k)=yy0(k)
      z(k+4)=yy1(k)*d1
      z(k+8)=yy2(k)*d2
      z(k+12)=yy12(k)*d1d2
   47 continue

      do 49 l=1,16
      xxd=0.
      do 48 k=1,16
      xxd=xxd+iwt(k,l)*z(k)
   48 continue
      cl(l)=xxd
   49 continue
      l=0
      do 51 k=1,4
      do 50 j=1,4
      l=l+1
      cc(n,m,k,j)=cl(l)
   50 continue
   51 continue
   52 continue
   53 continue
      return
      end

      subroutine jeppe2001lo2(x,y,nx,my,xx,yy,cc,z)
      implicit real*8(a-h,o-z)
      dimension xx(nx),yy(my),cc(nx,my,4,4)      

      n=locx2001lo(xx,nx,x)
      m=locx2001lo(yy,my,y)

      t=(x-xx(n))/(xx(n+1)-xx(n))
      u=(y-yy(m))/(yy(m+1)-yy(m))

      z=0.
      do 1 l=4,1,-1
      z=t*z+((cc(n,m,l,4)*u+cc(n,m,l,3))*u
     .       +cc(n,m,l,2))*u+cc(n,m,l,1)
    1 continue
      return
      end

      integer function locx2001lo(xx,nx,x)
      implicit real*8(a-h,o-z)
      dimension xx(nx)
      if(x.le.xx(1)) then
      locx2001lo=1
      return
      endif
      if(x.ge.xx(nx)) then 
      locx2001lo=nx-1  
      return
      endif
      ju=nx+1
      jl=0
    1 if((ju-jl).le.1) go to 2
      jm=(ju+jl)/2
      if(x.ge.xx(jm)) then
      jl=jm
      else
      ju=jm
      endif
      go to 1
    2 locx2001lo=jl
      return
      end


      real*8 function  polderiv2001lo(x1,x2,x3,y1,y2,y3)
      implicit real*8(a-h,o-z)
      polderiv2001lo=(x3*x3*(y1-y2)-2.0*x2*(x3*(y1-y2)+x1*
     .(y2-y3))+x2*x2*(y1-y3)+x1*x1*(y2-y3))/((x1-x2)*(x1-x3)*(x2-x3))
      return
      end


      subroutine mrst2001(x,q,mode,upv,dnv,usea,dsea,str,chm,bot,glu)
C***************************************************************C
C								C
C  This is a package for the new MRST 2001 NLO parton           C
C  distributions.                                               C     
C  Reference: A.D. Martin, R.G. Roberts, W.J. Stirling and      C
C  R.S. Thorne, hep-ph/0110215                                  C
C                                                               C
C  There are 4 pdf sets corresponding to mode = 1, 2, 3, 4      C
C                                                               C
C  Mode=1 gives the default set with Lambda(MSbar,nf=4) = 0.323 C
C  corresponding to alpha_s(M_Z) of 0.119                       C
C  This set reads a grid whose first number is 0.00927          C
C                                                               C
C  Mode=2 gives the set with Lambda(MSbar,nf=4) = 0.290         C
C  corresponding to alpha_s(M_Z) of 0.117                       C
C  This set reads a grid whose first number is 0.00953          C
C                                                               C
C  Mode=3 gives the set with Lambda(MSbar,nf=4) = 0.362         C
C  corresponding to alpha_s(M_Z) of 0.121                       C
C  This set reads a grid whose first number is 0.00889          C
C                                                               C
C  Mode=4 gives the set MRST2001J which gives better agreement  C
C  with the Tevatron inclusive jet data but has unattractive    C
C  gluon behaviour at large x (see discussion in paper)         C
C  This set has Lambda(MSbar,nf=4) = 0.353(alpha_s(M_Z) = 0.121 C 
C  This set reads a grid whose first number is 0.00826          C
C                                                               C
C   This subroutine uses an improved interpolation procedure    C 
C   for extracting values of the pdf's from the grid            C
C                                                               C
C         Comments to : W.J.Stirling@durham.ac.uk               C
C                                                               C
C***************************************************************C
      implicit real*8(a-h,o-z)
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      q2=q*q
c      if(q2.lt.qsqmin.or.q2.gt.qsqmax) print 99,q2
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
      if(mode.eq.1) then
        call mrst2001_1(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.2) then
        call mrst2001_2(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.3) then
        call mrst2001_3(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu) 
      elseif(mode.eq.4) then
        call mrst2001_4(x,q2,upv,dnv,usea,dsea,str,chm,bot,glu)
      endif 
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ','q2= ',e10.5)
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ','x= ',e10.5)
      return
      end

      subroutine mrst2001_1(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='alf119',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2001_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2001_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2001_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2001_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2001_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2001_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2001_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2001_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end
 
      subroutine mrst2001_2(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='alf117',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2001_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2001_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2001_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2001_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2001_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2001_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2001_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2001_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine mrst2001_3(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='alf121',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2001_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2001_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2001_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2001_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2001_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2001_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2001_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2001_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine mrst2001_4(x,qsq,upv,dnv,usea,dsea,str,chm,bot,glu)
      implicit real*8(a-h,o-z)
      parameter(nx=49,nq=37,np=8,nqc0=2,nqb0=11,nqc=35,nqb=26)
      real*8 f1(nx,nq),f2(nx,nq),f3(nx,nq),f4(nx,nq),f5(nx,nq),
     .f6(nx,nq),f7(nx,nq),f8(nx,nq),fc(nx,nqc),fb(nx,nqb)
      real*8 qq(nq),xx(nx),cc1(nx,nq,4,4),cc2(nx,nq,4,4),
     .cc3(nx,nq,4,4),cc4(nx,nq,4,4),cc6(nx,nq,4,4),cc8(nx,nq,4,4),
     .ccc(nx,nqc,4,4),ccb(nx,nqb,4,4)
      real*8 xxl(nx),qql(nq),qqlc(nqc),qqlb(nqb)
      data xx/1d-5,2d-5,4d-5,6d-5,8d-5,
     .	      1d-4,2d-4,4d-4,6d-4,8d-4,
     .	      1d-3,2d-3,4d-3,6d-3,8d-3,
     .	      1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .	   .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .	   .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .	   .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .	   .8d0,.9d0,1d0/
      data qq/1.25d0,1.5d0,2d0,2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7/
      data xmin,xmax,qsqmin,qsqmax/1d-5,1d0,1.25d0,1d7/
      data init/0/
      save
      xsave=x
      q2save=qsq
      if(init.ne.0) goto 10
        open(unit=33,file='j121',status='old')
        do 20 n=1,nx-1
        do 20 m=1,nq
        read(33,50)f1(n,m),f2(n,m),f3(n,m),f4(n,m),
     .		  f5(n,m),f7(n,m),f6(n,m),f8(n,m)
c notation: 1=uval 2=val 3=glue 4=usea 5=chm 6=str 7=btm 8=dsea
  20  continue
      do 40 m=1,nq
      f1(nx,m)=0.d0
      f2(nx,m)=0.d0
      f3(nx,m)=0.d0
      f4(nx,m)=0.d0
      f5(nx,m)=0.d0
      f6(nx,m)=0.d0
      f7(nx,m)=0.d0
      f8(nx,m)=0.d0
  40  continue
      do n=1,nx
      xxl(n)=dlog(xx(n))
      enddo
      do m=1,nq
      qql(m)=dlog(qq(m))
      enddo

      call jeppe2001_1(nx,nq,xxl,qql,f1,cc1)
      call jeppe2001_1(nx,nq,xxl,qql,f2,cc2)
      call jeppe2001_1(nx,nq,xxl,qql,f3,cc3)
      call jeppe2001_1(nx,nq,xxl,qql,f4,cc4)
      call jeppe2001_1(nx,nq,xxl,qql,f6,cc6)
      call jeppe2001_1(nx,nq,xxl,qql,f8,cc8)

      emc2=2.045
      emb2=18.5

      do 44 m=1,nqc
      qqlc(m)=qql(m+nqc0)
      do 44 n=1,nx
      fc(n,m)=f5(n,m+nqc0)
   44 continue
      qqlc(1)=dlog(emc2)
      call jeppe2001_1(nx,nqc,xxl,qqlc,fc,ccc)

      do 45 m=1,nqb
      qqlb(m)=qql(m+nqb0)
      do 45 n=1,nx
      fb(n,m)=f7(n,m+nqb0)
   45 continue
      qqlb(1)=dlog(emb2)
      call jeppe2001_1(nx,nqb,xxl,qqlb,fb,ccb)


      init=1
   10 continue
      
      xlog=dlog(x)
      qsqlog=dlog(qsq)

      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc1,upv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc2,dnv)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc3,glu)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc4,usea)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc6,str)
      call jeppe2001_2(xlog,qsqlog,nx,nq,xxl,qql,cc8,dsea)

      chm=0.d0
      if(qsq.gt.emc2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqc,xxl,qqlc,ccc,chm)
      endif

      bot=0.d0
      if(qsq.gt.emb2) then 
      call jeppe2001_2(xlog,qsqlog,nx,nqb,xxl,qqlb,ccb,bot)
      endif

      x=xsave
      qsq=q2save
      return
   50 format(8f10.5)
      end

      subroutine jeppe2001_1(nx,my,xx,yy,ff,cc)
      implicit real*8(a-h,o-z)
      parameter(nnx=49,mmy=37)
      dimension xx(nx),yy(my),ff(nnx,mmy),ff1(nnx,mmy),ff2(nnx,mmy),
     xff12(nnx,mmy),yy0(4),yy1(4),yy2(4),yy12(4),z(16),wt(16,16),
     xcl(16),cc(nx,my,4,4),iwt(16,16)

      data iwt/1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
     x		  -3,0,0,3,0,0,0,0,-2,0,0,-1,0,0,0,0,
     x		  2,0,0,-2,0,0,0,0,1,0,0,1,0,0,0,0,
     x		  0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,
     x		  0,0,0,0,-3,0,0,3,0,0,0,0,-2,0,0,-1,
     x		  0,0,0,0,2,0,0,-2,0,0,0,0,1,0,0,1,
     x		  -3,3,0,0,-2,-1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,-3,3,0,0,-2,-1,0,0,
     x		  9,-9,9,-9,6,3,-3,-6,6,-6,-3,3,4,2,1,2,
     x		  -6,6,-6,6,-4,-2,2,4,-3,3,3,-3,-2,-1,-1,-2,
     x		  2,-2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
     x		  0,0,0,0,0,0,0,0,2,-2,0,0,1,1,0,0,
     x		  -6,6,-6,6,-3,-3,3,3,-4,4,2,-2,-2,-2,-1,-1,
     x		  4,-4,4,-4,2,2,-2,-2,2,-2,-2,2,1,1,1,1/


      do 42 m=1,my
      dx=xx(2)-xx(1)
      ff1(1,m)=(ff(2,m)-ff(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff1(nx,m)=(ff(nx,m)-ff(nx-1,m))/dx
      do 41 n=2,nx-1
      ff1(n,m)=polderiv2001(xx(n-1),xx(n),xx(n+1),ff(n-1,m),ff(n,m),
     xff(n+1,m))
   41 continue
   42 continue

      do 44 n=1,nx
      dy=yy(2)-yy(1)
      ff2(n,1)=(ff(n,2)-ff(n,1))/dy
      dy=yy(my)-yy(my-1)
      ff2(n,my)=(ff(n,my)-ff(n,my-1))/dy
      do 43 m=2,my-1
      ff2(n,m)=polderiv2001(yy(m-1),yy(m),yy(m+1),ff(n,m-1),ff(n,m),
     xff(n,m+1))
   43 continue
   44 continue

      do 46 m=1,my
      dx=xx(2)-xx(1)
      ff12(1,m)=(ff2(2,m)-ff2(1,m))/dx
      dx=xx(nx)-xx(nx-1)
      ff12(nx,m)=(ff2(nx,m)-ff2(nx-1,m))/dx
      do 45 n=2,nx-1
      ff12(n,m)=polderiv2001(xx(n-1),xx(n),xx(n+1),ff2(n-1,m),ff2(n,m),
     xff2(n+1,m))
   45 continue
   46 continue

      do 53 n=1,nx-1
      do 52 m=1,my-1
      d1=xx(n+1)-xx(n)
      d2=yy(m+1)-yy(m)
      d1d2=d1*d2

      yy0(1)=ff(n,m)
      yy0(2)=ff(n+1,m)
      yy0(3)=ff(n+1,m+1)
      yy0(4)=ff(n,m+1)

      yy1(1)=ff1(n,m)
      yy1(2)=ff1(n+1,m)
      yy1(3)=ff1(n+1,m+1)
      yy1(4)=ff1(n,m+1)

      yy2(1)=ff2(n,m)
      yy2(2)=ff2(n+1,m)
      yy2(3)=ff2(n+1,m+1)
      yy2(4)=ff2(n,m+1)

      yy12(1)=ff12(n,m)
      yy12(2)=ff12(n+1,m)
      yy12(3)=ff12(n+1,m+1)
      yy12(4)=ff12(n,m+1)

      do 47 k=1,4
      z(k)=yy0(k)
      z(k+4)=yy1(k)*d1
      z(k+8)=yy2(k)*d2
      z(k+12)=yy12(k)*d1d2
   47 continue

      do 49 l=1,16
      xxd=0.
      do 48 k=1,16
      xxd=xxd+iwt(k,l)*z(k)
   48 continue
      cl(l)=xxd
   49 continue
      l=0
      do 51 k=1,4
      do 50 j=1,4
      l=l+1
      cc(n,m,k,j)=cl(l)
   50 continue
   51 continue
   52 continue
   53 continue
      return
      end

      subroutine jeppe2001_2(x,y,nx,my,xx,yy,cc,z)
      implicit real*8(a-h,o-z)
      dimension xx(nx),yy(my),cc(nx,my,4,4)      

      n=locx2001(xx,nx,x)
      m=locx2001(yy,my,y)

      t=(x-xx(n))/(xx(n+1)-xx(n))
      u=(y-yy(m))/(yy(m+1)-yy(m))

      z=0.
      do 1 l=4,1,-1
      z=t*z+((cc(n,m,l,4)*u+cc(n,m,l,3))*u
     .       +cc(n,m,l,2))*u+cc(n,m,l,1)
    1 continue
      return
      end

      integer function locx2001(xx,nx,x)
      implicit real*8(a-h,o-z)
      dimension xx(nx)
      if(x.le.xx(1)) then
      locx2001=1
      return
      endif
      if(x.ge.xx(nx)) then 
      locx2001=nx-1  
      return
      endif
      ju=nx+1
      jl=0
    1 if((ju-jl).le.1) go to 2
      jm=(ju+jl)/2
      if(x.ge.xx(jm)) then
      jl=jm
      else
      ju=jm
      endif
      go to 1
    2 locx2001=jl
      return
      end


      real*8 function  polderiv2001(x1,x2,x3,y1,y2,y3)
      implicit real*8(a-h,o-z)
      polderiv2001=(x3*x3*(y1-y2)-2.0*x2*(x3*(y1-y2)+x1*
     .(y2-y3))+x2*x2*(y1-y3)+x1*x1*(y2-y3))/((x1-x2)*(x1-x3)*(x2-x3))
      return
      end


      subroutine errsk(j)
      integer jval,j
      data jval/0/
      jval=j
      return
      entry errgk(j)
      j=jval
      end

      subroutine alekhin(ndns,xs,qsqs,fxs,nf)
      implicit none
      integer ndns,nf,jpar,l,j
      real * 4 xs,qsqs,fxs(-nf:nf)
      real * 8 x,qsq
      integer npdf,npar
      integer np,nvar
      parameter(np=9,nvar=15)
      real*8 pdfs(np),dpdfs(np,nvar)
      integer kord,iset,kset,kschem
      if(ndns.le.6) then
         kord=1
      elseif(ndns.le.12) then
         kord=2
      else
         kord=3
      endif
      iset=ndns-(kord-1)*6
      if(iset.le.2) then
         kset=0
      elseif(iset.le.4) then
         kset=1
      elseif(iset.le.6) then
         kset=2
      elseif(iset.le.8) then
         kset=3
      endif
      kschem=mod(ndns+1,2)
      x=xs
      qsq=qsqs
      call a02(x,qsq,pdfs,dpdfs,NPDF,NPAR,KORD,KSCHEM,KSET)
      call errgk(jpar)
      if(abs(jpar).gt.npar) then
         write(*,*) ' Alekhin PDF''s: max',npar,' parameters, got',jpar
         stop
      endif
      if(jpar.gt.0) then
         do l=1,npdf
            pdfs(l)=pdfs(l)+dpdfs(l,jpar)
         enddo
      elseif(jpar.lt.0) then
         jpar=abs(jpar)
         do l=1,npdf
            pdfs(l)=pdfs(l)-dpdfs(l,jpar)
         enddo
      endif
      do j=-nf,nf
         fxs(j)=0
      enddo
      fxs(0)=pdfs(3)/x
      fxs(1)=( pdfs(1)+pdfs(4) )/x
      fxs(-1)=pdfs(4)
      fxs(2)=( pdfs(2)+pdfs(6) )/x
      fxs(-2)=pdfs(6)/x
      fxs(3)=pdfs(5)/x
      fxs(-3)=fxs(3)
      if(npdf.gt.6.and.nf.ge.4) then
         fxs(4)=pdfs(7)/x
         fxs(-4)=fxs(4)
      endif
      if(npdf.gt.7.and.nf.ge.5) then
         fxs(5)=pdfs(8)/x
         fxs(-5)=fxs(5)
      endif
      if(npdf.gt.8.and.nf.ge.6) then
         fxs(6)=pdfs(9)/x
         fxs(-6)=fxs(6)
      endif
      end

      subroutine a02(x,qsq,PDFS,DPDFS,NPDF,NPAR,KORD,KSCHEM,KSET)
c--------------------
c     This is a package for the parton distributions with account 
c     of their experimental (stat+syst) and theoretical uncertainties. 
c     The q**2 range is 2.5d0 < q**2 < 5.6d7, the x range is 1d-7 < x < 1d0. 
c     The grid and interpolation routines are cloned from the MRS's ones.
C
c  Input parameters:
c        KORD=1 -- the LO PDFs
c        KORD=2 -- the NLO PDFs
c        KORD=3 -- the NNLO PDFs
C      
c        KSCHEM=0 -- the fixed-flavor-number (FFN) scheme 
c        KSCHEM=1 -- the variable-flavor-number (VFN) scheme
C
c        KSET=0 -- nominal PDFs
c        KSET=1 -- PDFs with mass of c-quark increased from 1.5 to 1.75 GeV
c        KSET=2 -- PDFs with the strange sea suppression factor increased from 
c                  0.42 to 0.52
c        KSET=3 -- PDFs with the choice B (slow evolution) for the NNLO kernel 
c                  (used with KORD=2 only)
c
c  Output parameters:
c     The array PDFS contains parton distributions times x:
c        PDF(1) -- valence u-quarks 
c        PDF(2) -- valence d-quarks
c        PDF(3) -- gluons 
c        PDF(4) -- sea u-quarks 
c        PDF(5) -- s-quarks 
c        PDF(6) -- sea d-quarks 
c        PDF(7) -- c-quarks
c        PDF(8) -- b-quarks
c        PDF(9) -- t-quarks
c     NPDF is the number of PDFs returned (NPDF=6 for the FFN PDFs and 9 for 
c     the VFN ones).
c     Output array DPDFS(ipdf,ipar) contains derivatives of the PDFs on the 
c     fitted parameters with the number of the parameters returned in NPAR.
c     These derivatives are transformed to the orthonormal basis of 
c     eigenvectors of the parameters error matrix. For this reason the 
c     variation of the PDFs in the derivatives directions can be performed 
c     independently. For example the dispersion of the i-th PDF can be stored 
c     in DELPDF using the code 
c
c-----------------
c          DELPDF=0.
c          do k=1,npar
c            DELPDF=DELPDF+dpdfs(i,k)**2
c          end do
c-----------------
c     and its random value is stored in RPDF using the code 
c-----------------
c          RPDF=pdfs(i)          
c          do k=1,npar
c            s=0.
c            do k=1,96
c              s=s+(2*rndm(xxx)-1)/sqrt(32.)
c            end do
c            RPDF=RPDF+s*dpdfs(i,k)
c          end do
c-----------------
c          
c         Reference: hep-ph/0211096
c      
c         Comments to: alekhin@sirius.ihep.su                      
c                                                               
      implicit real*8(a-h,o-z)
      parameter(nxb=59,nq=37,ntenth=33,np=9,nvar=15)
      real*4 f(np,nxb,nq+1),qq(nq),xx(nxb),xx0(nxb),n0(np)
      real*8 pdfs(np),dpdfs(np,nvar)
      real*4 df(nvar,np,nxb,nq+1)
      data xx0/1d-7,2d-7,4d-7,6d-7,8d-7,
     .        1d-6,2d-6,4d-6,6d-6,8d-6,
     .        1d-5,2d-5,4d-5,6d-5,8d-5,
     .        1d-4,2d-4,4d-4,6d-4,8d-4,
     .        1d-3,2d-3,4d-3,6d-3,8d-3,
     .        1d-2,1.4d-2,2d-2,3d-2,4d-2,6d-2,8d-2,
     .     .1d0,.125d0,.15d0,.175d0,.2d0,.225d0,.25d0,.275d0,
     .     .3d0,.325d0,.35d0,.375d0,.4d0,.425d0,.45d0,.475d0,
     .     .5d0,.525d0,.55d0,.575d0,.6d0,.65d0,.7d0,.75d0,
     .     .8d0,.9d0,1d0/
      data qq/2.5d0,3.2d0,4d0,5d0,6.4d0,8d0,1d1,
     .        1.2d1,1.8d1,2.6d1,4d1,6.4d1,1d2,
     .        1.6d2,2.4d2,4d2,6.4d2,1d3,1.8d3,3.2d3,5.6d3,1d4,
     .        1.8d4,3.2d4,5.6d4,1d5,1.8d5,3.2d5,5.6d5,1d6,
     .        1.8d6,3.2d6,5.6d6,1d7,1.8d7,3.2d7,5.6d7/
      data xmin,xmax,qsqmin,qsqmax/1d-7,1d0,2.5d0,5.6d7/
      data n0/3,4,5,9,9,9,9,9,9/
      data KORDS,KSCHEMS,KSETS /-1,-1,-1/
      data init /0/

c I/O channel to read the data
      data nport/1/
c put in your local address of the PDFs files in LOCDIR
      character locdir*41
      data locdir /' '/
      character *1 pdford(3)
      data pdford/'1','2','3'/
      character * 3 pdfschem(0:1)
      data pdfschem /'ffn','vfn'/
      character *3 pdfset(0:3)
      data pdfset /'   ','_mc','_ss','_kr'/

      if (init.eq.0) then 
        do n=1,ntenth-1
          xx(n)=log10(xx0(n)/xx0(ntenth))+xx0(ntenth)
        end do
        do n=ntenth,nxb
          xx(n)=xx0(n)
        end do
      init=1
      end if

      if (kschem.eq.0) then 
        npdf=6
      else 
        npdf=9
      end if
      npar=nvar

      if(kords.eq.kord.and.kschems.eq.kschem.and.ksets.eq.kset) goto 10

      kords=kord
      kschems=kschem
      ksets=kset      

      write(*,*) 'a02.pdfs_'//pdford(kord)//'_'
     /     //pdfschem(kschem)//pdfset(kset)
      open(unit=nport,status='old',err=199
     ,    ,file='a02.pdfs_'//pdford(kord)//'_'
     /     //pdfschem(kschem)//pdfset(kset))
      do n=1,nxb-1
        do m=1,nq
          read(nport,100) (f(i,n,m),i=1,npdf)
          do i=1,npdf
            f(i,n,m)=f(i,n,m)/(1d0-xx0(n))**n0(i)
          end do
        end do
      end do
      close(unit=nport)
  100 format (12f11.5)

      open(unit=nport,status='old'
     ,    ,file='a02.dpdfs_'//pdford(kord)//'_'
     /                              //pdfschem(kschem))
      do n=1,nxb-1
        do m=1,nq
          do i=1,npdf 
            read (nport,*) (df(k,i,n,m),k=1,npar)
            do k=1,npar
              df(k,i,n,m)=df(k,i,n,m)/(1d0-xx0(n))**n0(i)
            end do
          end do
        end do
      end do
      close(unit=nport)

      do i=1,npdf
        do m=1,nq
          f(i,nxb,m)=0d0
          do k=1,npar
            df(k,i,nxb,m)=0d0
          end do
        end do
      end do

  10  continue

c      if(qsq.lt.qsqmin.or.qsq.gt.qsqmax) print 99,qsq
c      if(x.lt.xmin.or.x.gt.xmax)       print 98,x
  99  format('  WARNING:  Q^2 VALUE IS OUT OF RANGE   ')
  98  format('  WARNING:   X  VALUE IS OUT OF RANGE   ')

      x=max(x,xmin)
      x=min(x,xmax)
      qsq=max(qsq,qsqmin)
      qsq=min(qsq,qsqmax)
      xxx=x
      if(x.lt.xx(ntenth)) xxx=log10(x/xx(ntenth))+xx(ntenth)
      n=0
  70  n=n+1
      if(xxx.gt.xx(n+1)) goto 70
      a=(xxx-xx(n))/(xx(n+1)-xx(n))
      m=0
  80  m=m+1
      if(qsq.gt.qq(m+1)) goto 80
      b=(qsq-qq(m))/(qq(m+1)-qq(m))

      do i=1,npdf
        pdfs(i)= (1d0-a)*(1d0-b)*f(i,n,m) + (1d0-a)*b*f(i,n,m+1)
     .    +       a*(1d0-b)*f(i,n+1,m) + a*b*f(i,n+1,m+1)
        do k=1,npar
          dpdfs(i,k)=(1d0-a)*(1d0-b)*df(k,i,n,m)+(1d0-a)*b*df(k,i,n,m+1)
     .    +       a*(1d0-b)*df(k,i,n+1,m) + a*b*df(k,i,n+1,m+1)
        end do
        pdfs(i)=pdfs(i)*(1d0-x)**n0(i)
        do k=1,npar
          dpdfs(i,k)=dpdfs(i,k)*(1d0-x)**n0(i)
        end do
      end do

      return

 199  print *,'The PDF set is inavailable'
      return

      end

