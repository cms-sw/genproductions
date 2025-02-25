      SUBROUTINE MP_HELAS_CALLS_AMPB_1(P,NHEL,H,IC)
C     
      USE POLYNOMIAL_CONSTANTS
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NEXTERNAL
      PARAMETER (NEXTERNAL=5)
      INTEGER    NCOMB
      PARAMETER (NCOMB=48)

      INTEGER NBORNAMPS
      PARAMETER (NBORNAMPS=8)
      INTEGER    NLOOPS, NLOOPGROUPS, NCTAMPS
      PARAMETER (NLOOPS=71, NLOOPGROUPS=47, NCTAMPS=116)
      INTEGER    NLOOPAMPS
      PARAMETER (NLOOPAMPS=187)
      INTEGER    NWAVEFUNCS,NLOOPWAVEFUNCS
      PARAMETER (NWAVEFUNCS=26,NLOOPWAVEFUNCS=143)
      REAL*16     ZERO
      PARAMETER (ZERO=0.0E0_16)
      COMPLEX*32     IZERO
      PARAMETER (IZERO=CMPLX(0.0E0_16,0.0E0_16,KIND=16))
C     These are constants related to the split orders
      INTEGER    NSO, NSQUAREDSO, NAMPSO
      PARAMETER (NSO=1, NSQUAREDSO=1, NAMPSO=2)
C     
C     ARGUMENTS
C     
      REAL*16 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
      INTEGER H
C     
C     LOCAL VARIABLES
C     
      INTEGER I,J,K
      COMPLEX*32 COEFS(MAXLWFSIZE,0:VERTEXMAXCOEFS-1,MAXLWFSIZE)
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'mp_coupl_same_name.inc'

      INTEGER GOODHEL(NCOMB)
      LOGICAL GOODAMP(NSQUAREDSO,NLOOPGROUPS)
      COMMON/FILTERS/GOODAMP,GOODHEL

      INTEGER SQSO_TARGET
      COMMON/SOCHOICE/SQSO_TARGET

      LOGICAL UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE,CT_REQ_SO_DONE
     $ ,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE,MP_LOOP_REQ_SO_DONE
     $ ,CTCALL_REQ_SO_DONE,FILTER_SO
      COMMON/SO_REQS/UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE
     $ ,CT_REQ_SO_DONE,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE
     $ ,MP_LOOP_REQ_SO_DONE,CTCALL_REQ_SO_DONE,FILTER_SO

      COMPLEX*32 AMP(NBORNAMPS)
      COMMON/MP_AMPS/AMP
      COMPLEX*32 W(20,NWAVEFUNCS)
      COMMON/MP_W/W

      COMPLEX*32 WL(MAXLWFSIZE,0:LOOPMAXCOEFS-1,MAXLWFSIZE,
     $ -1:NLOOPWAVEFUNCS)
      COMPLEX*32 PL(0:3,-1:NLOOPWAVEFUNCS)
      COMMON/MP_WL/WL,PL

      COMPLEX*32 AMPL(3,NCTAMPS)
      COMMON/MP_AMPL/AMPL

C     
C     ----------
C     BEGIN CODE
C     ----------

C     The target squared split order contribution is already reached
C      if true.
      IF (FILTER_SO.AND.MP_CT_REQ_SO_DONE) THEN
        GOTO 1001
      ENDIF

      CALL MP_VXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))
      CALL MP_IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL MP_VXXXXX(P(0,3),MDL_MW,NHEL(3),+1*IC(3),W(1,3))
      CALL MP_VXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL MP_OXXXXX(P(0,5),ZERO,NHEL(5),+1*IC(5),W(1,5))
      CALL MP_FFV1_2(W(1,2),W(1,1),GC_5,ZERO,ZERO,W(1,6))
      CALL MP_VVV1_3(W(1,4),W(1,3),GC_25,MDL_MW,MDL_WW,W(1,7))
C     Amplitude(s) for born diagram with ID 1
      CALL MP_FFV2_0(W(1,6),W(1,5),W(1,7),GC_11,AMP(1))
      CALL MP_FFV2_1(W(1,5),W(1,3),GC_11,ZERO,ZERO,W(1,8))
C     Amplitude(s) for born diagram with ID 2
      CALL MP_FFV1_0(W(1,6),W(1,8),W(1,4),GC_1,AMP(2))
      CALL MP_FFV1_1(W(1,5),W(1,4),GC_2,ZERO,ZERO,W(1,9))
C     Amplitude(s) for born diagram with ID 3
      CALL MP_FFV2_0(W(1,6),W(1,9),W(1,3),GC_11,AMP(3))
      CALL MP_FFV1_1(W(1,5),W(1,1),GC_5,ZERO,ZERO,W(1,10))
      CALL MP_FFV2_2(W(1,2),W(1,3),GC_11,ZERO,ZERO,W(1,11))
C     Amplitude(s) for born diagram with ID 4
      CALL MP_FFV1_0(W(1,11),W(1,10),W(1,4),GC_2,AMP(4))
      CALL MP_FFV1_2(W(1,2),W(1,4),GC_1,ZERO,ZERO,W(1,12))
C     Amplitude(s) for born diagram with ID 5
      CALL MP_FFV2_0(W(1,12),W(1,10),W(1,3),GC_11,AMP(5))
C     Amplitude(s) for born diagram with ID 6
      CALL MP_FFV2_0(W(1,2),W(1,10),W(1,7),GC_11,AMP(6))
C     Amplitude(s) for born diagram with ID 7
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),GC_5,AMP(7))
C     Amplitude(s) for born diagram with ID 8
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),GC_5,AMP(8))
      CALL MP_FFV2_1(W(1,5),W(1,7),GC_11,ZERO,ZERO,W(1,13))
C     Counter-term amplitude(s) for loop diagram number 9
      CALL MP_R2_QQ_1_0(W(1,6),W(1,13),R2_QQQ,AMPL(1,1))
C     Counter-term amplitude(s) for loop diagram number 10
      CALL MP_FFV2_0(W(1,6),W(1,5),W(1,7),R2_BXTW,AMPL(1,2))
      CALL MP_FFV1_2(W(1,6),W(1,4),GC_1,ZERO,ZERO,W(1,14))
C     Counter-term amplitude(s) for loop diagram number 11
      CALL MP_R2_QQ_1_0(W(1,14),W(1,8),R2_QQQ,AMPL(1,3))
      CALL MP_FFV1_1(W(1,8),W(1,4),GC_1,ZERO,ZERO,W(1,15))
C     Counter-term amplitude(s) for loop diagram number 12
      CALL MP_R2_QQ_1_0(W(1,6),W(1,15),R2_QQQ,AMPL(1,4))
C     Counter-term amplitude(s) for loop diagram number 13
      CALL MP_FFV1_0(W(1,6),W(1,8),W(1,4),R2_DDA,AMPL(1,5))
C     Counter-term amplitude(s) for loop diagram number 14
      CALL MP_FFV2_0(W(1,14),W(1,5),W(1,3),R2_BXTW,AMPL(1,6))
C     Counter-term amplitude(s) for loop diagram number 16
      CALL MP_FFV2_0(W(1,6),W(1,9),W(1,3),R2_BXTW,AMPL(1,7))
      CALL MP_FFV2_1(W(1,9),W(1,3),GC_11,ZERO,ZERO,W(1,16))
C     Counter-term amplitude(s) for loop diagram number 18
      CALL MP_R2_QQ_1_0(W(1,6),W(1,16),R2_QQQ,AMPL(1,8))
      CALL MP_FFV2_1(W(1,10),W(1,3),GC_11,ZERO,ZERO,W(1,17))
C     Counter-term amplitude(s) for loop diagram number 19
      CALL MP_R2_QQ_1_0(W(1,12),W(1,17),R2_QQQ,AMPL(1,9))
C     Counter-term amplitude(s) for loop diagram number 20
      CALL MP_FFV2_0(W(1,12),W(1,10),W(1,3),R2_BXTW,AMPL(1,10))
C     Counter-term amplitude(s) for loop diagram number 21
      CALL MP_FFV2_0(W(1,2),W(1,10),W(1,7),R2_BXTW,AMPL(1,11))
      CALL MP_FFV1_1(W(1,10),W(1,4),GC_2,ZERO,ZERO,W(1,18))
C     Counter-term amplitude(s) for loop diagram number 23
      CALL MP_FFV2_0(W(1,2),W(1,18),W(1,3),R2_BXTW,AMPL(1,12))
C     Counter-term amplitude(s) for loop diagram number 25
      CALL MP_FFV1_0(W(1,2),W(1,17),W(1,4),R2_DDA,AMPL(1,13))
C     Counter-term amplitude(s) for loop diagram number 26
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,14))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,15))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,16))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,17))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,18))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQT,AMPL(1,19))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQB_1EPS,AMPL(2,20))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),UV_GQQG_1EPS,AMPL(2,21))
      CALL MP_FFV1_0(W(1,12),W(1,8),W(1,1),R2_GQQ,AMPL(1,22))
C     Counter-term amplitude(s) for loop diagram number 28
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,23))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,24))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,25))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,26))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,27))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQT,AMPL(1,28))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQB_1EPS,AMPL(2,29))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),UV_GQQG_1EPS,AMPL(2,30))
      CALL MP_FFV1_0(W(1,2),W(1,13),W(1,1),R2_GQQ,AMPL(1,31))
C     Counter-term amplitude(s) for loop diagram number 30
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,32))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,33))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,34))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,35))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,36))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQT,AMPL(1,37))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQB_1EPS,AMPL(2,38))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),UV_GQQG_1EPS,AMPL(2,39))
      CALL MP_FFV1_0(W(1,2),W(1,15),W(1,1),R2_GQQ,AMPL(1,40))
C     Counter-term amplitude(s) for loop diagram number 36
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,41))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,42))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,43))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,44))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,45))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQT,AMPL(1,46))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQB_1EPS,AMPL(2,47))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),UV_GQQG_1EPS,AMPL(2,48))
      CALL MP_FFV1_0(W(1,2),W(1,16),W(1,1),R2_GQQ,AMPL(1,49))
      CALL MP_FFV1_2(W(1,12),W(1,1),GC_5,ZERO,ZERO,W(1,19))
C     Counter-term amplitude(s) for loop diagram number 38
      CALL MP_R2_QQ_1_0(W(1,19),W(1,8),R2_QQQ,AMPL(1,50))
      CALL MP_FFV1_1(W(1,8),W(1,1),GC_5,ZERO,ZERO,W(1,20))
C     Counter-term amplitude(s) for loop diagram number 39
      CALL MP_R2_QQ_1_0(W(1,12),W(1,20),R2_QQQ,AMPL(1,51))
C     Counter-term amplitude(s) for loop diagram number 41
      CALL MP_FFV2_0(W(1,19),W(1,5),W(1,3),R2_BXTW,AMPL(1,52))
C     Counter-term amplitude(s) for loop diagram number 49
      CALL MP_FFV1_0(W(1,2),W(1,20),W(1,4),R2_DDA,AMPL(1,53))
      CALL MP_FFV1_1(W(1,9),W(1,1),GC_5,ZERO,ZERO,W(1,21))
C     Counter-term amplitude(s) for loop diagram number 55
      CALL MP_FFV2_0(W(1,2),W(1,21),W(1,3),R2_BXTW,AMPL(1,54))
      CALL MP_FFV2_2(W(1,6),W(1,3),GC_11,ZERO,ZERO,W(1,22))
C     Counter-term amplitude(s) for loop diagram number 59
      CALL MP_R2_QQ_1_0(W(1,22),W(1,9),R2_QQQ,AMPL(1,55))
C     Counter-term amplitude(s) for loop diagram number 60
      CALL MP_FFV1_0(W(1,22),W(1,5),W(1,4),R2_UUA,AMPL(1,56))
C     Counter-term amplitude(s) for loop diagram number 61
      CALL MP_R2_QQ_1_0(W(1,11),W(1,18),R2_QQQ,AMPL(1,57))
      CALL MP_FFV1_2(W(1,11),W(1,4),GC_2,ZERO,ZERO,W(1,23))
C     Counter-term amplitude(s) for loop diagram number 62
      CALL MP_R2_QQ_1_0(W(1,23),W(1,10),R2_QQQ,AMPL(1,58))
C     Counter-term amplitude(s) for loop diagram number 63
      CALL MP_FFV1_0(W(1,11),W(1,10),W(1,4),R2_UUA,AMPL(1,59))
      CALL MP_FFV2_2(W(1,12),W(1,3),GC_11,ZERO,ZERO,W(1,24))
C     Counter-term amplitude(s) for loop diagram number 64
      CALL MP_R2_QQ_1_0(W(1,24),W(1,10),R2_QQQ,AMPL(1,60))
      CALL MP_FFV2_2(W(1,2),W(1,7),GC_11,ZERO,ZERO,W(1,25))
C     Counter-term amplitude(s) for loop diagram number 65
      CALL MP_R2_QQ_1_0(W(1,25),W(1,10),R2_QQQ,AMPL(1,61))
C     Counter-term amplitude(s) for loop diagram number 66
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,62))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,63))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,64))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,65))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,66))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQT,AMPL(1,67))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQB_1EPS,AMPL(2,68))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),UV_GQQG_1EPS,AMPL(2,69))
      CALL MP_FFV1_0(W(1,11),W(1,9),W(1,1),R2_GQQ,AMPL(1,70))
C     Counter-term amplitude(s) for loop diagram number 69
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,71))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,72))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,73))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,74))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,75))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQT,AMPL(1,76))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,77))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),UV_GQQG_1EPS,AMPL(2,78))
      CALL MP_FFV1_0(W(1,23),W(1,5),W(1,1),R2_GQQ,AMPL(1,79))
C     Counter-term amplitude(s) for loop diagram number 70
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,80))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,81))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,82))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,83))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,84))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQT,AMPL(1,85))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,86))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),UV_GQQG_1EPS,AMPL(2,87))
      CALL MP_FFV1_0(W(1,24),W(1,5),W(1,1),R2_GQQ,AMPL(1,88))
C     Counter-term amplitude(s) for loop diagram number 71
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,89))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,90))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,91))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,92))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,93))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQT,AMPL(1,94))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQB_1EPS,AMPL(2,95))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),UV_GQQG_1EPS,AMPL(2,96))
      CALL MP_FFV1_0(W(1,25),W(1,5),W(1,1),R2_GQQ,AMPL(1,97))
      CALL MP_FFV1_2(W(1,11),W(1,1),GC_5,ZERO,ZERO,W(1,26))
C     Counter-term amplitude(s) for loop diagram number 72
      CALL MP_R2_QQ_1_0(W(1,26),W(1,9),R2_QQQ,AMPL(1,98))
C     Counter-term amplitude(s) for loop diagram number 73
      CALL MP_R2_QQ_1_0(W(1,11),W(1,21),R2_QQQ,AMPL(1,99))
C     Counter-term amplitude(s) for loop diagram number 75
      CALL MP_FFV1_0(W(1,26),W(1,5),W(1,4),R2_UUA,AMPL(1,100))
C     At this point, all CT amps needed for (QCD=4), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 2000

      GOTO 1001
 2000 CONTINUE
      MP_CT_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

