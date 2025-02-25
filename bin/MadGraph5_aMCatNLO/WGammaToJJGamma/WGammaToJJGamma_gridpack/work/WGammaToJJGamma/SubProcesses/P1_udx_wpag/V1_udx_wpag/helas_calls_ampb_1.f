      SUBROUTINE HELAS_CALLS_AMPB_1(P,NHEL,H,IC)
C     
C     Modules
C     
      USE POLYNOMIAL_CONSTANTS
C     
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
      PARAMETER (NWAVEFUNCS=26,NLOOPWAVEFUNCS=136)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      REAL*16     MP__ZERO
      PARAMETER (MP__ZERO=0.0E0_16)
C     These are constants related to the split orders
      INTEGER    NSO, NSQUAREDSO, NAMPSO
      PARAMETER (NSO=1, NSQUAREDSO=1, NAMPSO=2)
C     
C     ARGUMENTS
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
      INTEGER H
C     
C     LOCAL VARIABLES
C     
      INTEGER I,J,K
      COMPLEX*16 COEFS(MAXLWFSIZE,0:VERTEXMAXCOEFS-1,MAXLWFSIZE)

      LOGICAL DUMMYFALSE
      DATA DUMMYFALSE/.FALSE./
C     
C     GLOBAL VARIABLES
C     
      INCLUDE 'coupl.inc'
      INCLUDE 'mp_coupl.inc'

      INTEGER HELOFFSET
      INTEGER GOODHEL(NCOMB)
      LOGICAL GOODAMP(NSQUAREDSO,NLOOPGROUPS)
      COMMON/FILTERS/GOODAMP,GOODHEL,HELOFFSET

      LOGICAL CHECKPHASE
      LOGICAL HELDOUBLECHECKED
      COMMON/INIT/CHECKPHASE, HELDOUBLECHECKED

      INTEGER SQSO_TARGET
      COMMON/SOCHOICE/SQSO_TARGET

      LOGICAL UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE,CT_REQ_SO_DONE
     $ ,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE,MP_LOOP_REQ_SO_DONE
     $ ,CTCALL_REQ_SO_DONE,FILTER_SO
      COMMON/SO_REQS/UVCT_REQ_SO_DONE,MP_UVCT_REQ_SO_DONE
     $ ,CT_REQ_SO_DONE,MP_CT_REQ_SO_DONE,LOOP_REQ_SO_DONE
     $ ,MP_LOOP_REQ_SO_DONE,CTCALL_REQ_SO_DONE,FILTER_SO

      INTEGER I_SO
      COMMON/I_SO/I_SO
      INTEGER I_LIB
      COMMON/I_LIB/I_LIB

      COMPLEX*16 AMP(NBORNAMPS)
      COMMON/AMPS/AMP
      COMPLEX*16 W(20,NWAVEFUNCS)
      COMMON/W/W

      COMPLEX*16 WL(MAXLWFSIZE,0:LOOPMAXCOEFS-1,MAXLWFSIZE,
     $ -1:NLOOPWAVEFUNCS)
      COMPLEX*16 PL(0:3,-1:NLOOPWAVEFUNCS)
      COMMON/WL/WL,PL

      COMPLEX*16 AMPL(3,NCTAMPS)
      COMMON/AMPL/AMPL

C     
C     ----------
C     BEGIN CODE
C     ----------

C     The target squared split order contribution is already reached
C      if true.
      IF (FILTER_SO.AND.CT_REQ_SO_DONE) THEN
        GOTO 1001
      ENDIF

      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL OXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))
      CALL VXXXXX(P(0,3),MDL_MW,NHEL(3),+1*IC(3),W(1,3))
      CALL VXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL VXXXXX(P(0,5),ZERO,NHEL(5),+1*IC(5),W(1,5))
      CALL FFV2_2(W(1,1),W(1,3),GC_11,ZERO,ZERO,W(1,6))
      CALL FFV1_1(W(1,2),W(1,4),GC_1,ZERO,ZERO,W(1,7))
C     Amplitude(s) for born diagram with ID 1
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),GC_5,AMP(1))
      CALL FFV1_1(W(1,2),W(1,5),GC_5,ZERO,ZERO,W(1,8))
C     Amplitude(s) for born diagram with ID 2
      CALL FFV1_0(W(1,6),W(1,8),W(1,4),GC_1,AMP(2))
      CALL FFV1_2(W(1,1),W(1,4),GC_2,ZERO,ZERO,W(1,9))
      CALL FFV2_1(W(1,2),W(1,3),GC_11,ZERO,ZERO,W(1,10))
C     Amplitude(s) for born diagram with ID 3
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),GC_5,AMP(3))
C     Amplitude(s) for born diagram with ID 4
      CALL FFV2_0(W(1,9),W(1,8),W(1,3),GC_11,AMP(4))
      CALL FFV1_2(W(1,1),W(1,5),GC_5,ZERO,ZERO,W(1,11))
C     Amplitude(s) for born diagram with ID 5
      CALL FFV1_0(W(1,11),W(1,10),W(1,4),GC_2,AMP(5))
C     Amplitude(s) for born diagram with ID 6
      CALL FFV2_0(W(1,11),W(1,7),W(1,3),GC_11,AMP(6))
      CALL VVV1_2(W(1,4),W(1,3),GC_25,MDL_MW,MDL_WW,W(1,12))
C     Amplitude(s) for born diagram with ID 7
      CALL FFV2_0(W(1,11),W(1,2),W(1,12),GC_11,AMP(7))
C     Amplitude(s) for born diagram with ID 8
      CALL FFV2_0(W(1,1),W(1,8),W(1,12),GC_11,AMP(8))
      CALL FFV1_2(W(1,6),W(1,5),GC_5,ZERO,ZERO,W(1,13))
C     Counter-term amplitude(s) for loop diagram number 9
      CALL R2_QQ_1_0(W(1,13),W(1,7),R2_QQQ,AMPL(1,1))
      CALL FFV1_1(W(1,7),W(1,5),GC_5,ZERO,ZERO,W(1,14))
C     Counter-term amplitude(s) for loop diagram number 10
      CALL R2_QQ_1_0(W(1,6),W(1,14),R2_QQQ,AMPL(1,2))
C     Counter-term amplitude(s) for loop diagram number 11
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,3))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,4))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,5))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,6))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,7))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQT,AMPL(1,8))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQB_1EPS,AMPL(2,9))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),UV_GQQG_1EPS,AMPL(2,10))
      CALL FFV1_0(W(1,6),W(1,7),W(1,5),R2_GQQ,AMPL(1,11))
      CALL FFV1_2(W(1,6),W(1,4),GC_1,ZERO,ZERO,W(1,15))
C     Counter-term amplitude(s) for loop diagram number 13
      CALL R2_QQ_1_0(W(1,15),W(1,8),R2_QQQ,AMPL(1,12))
      CALL FFV1_1(W(1,8),W(1,4),GC_1,ZERO,ZERO,W(1,16))
C     Counter-term amplitude(s) for loop diagram number 14
      CALL R2_QQ_1_0(W(1,6),W(1,16),R2_QQQ,AMPL(1,13))
C     Counter-term amplitude(s) for loop diagram number 15
      CALL FFV1_0(W(1,6),W(1,8),W(1,4),R2_DDA,AMPL(1,14))
C     Counter-term amplitude(s) for loop diagram number 16
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,15))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,16))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,17))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,18))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,19))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQT,AMPL(1,20))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,21))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),UV_GQQG_1EPS,AMPL(2,22))
      CALL FFV1_0(W(1,15),W(1,2),W(1,5),R2_GQQ,AMPL(1,23))
C     Counter-term amplitude(s) for loop diagram number 19
      CALL FFV1_0(W(1,13),W(1,2),W(1,4),R2_DDA,AMPL(1,24))
      CALL FFV2_2(W(1,9),W(1,3),GC_11,ZERO,ZERO,W(1,17))
C     Counter-term amplitude(s) for loop diagram number 22
      CALL R2_QQ_1_0(W(1,17),W(1,8),R2_QQQ,AMPL(1,25))
C     Counter-term amplitude(s) for loop diagram number 23
      CALL FFV2_0(W(1,9),W(1,8),W(1,3),R2_BXTW,AMPL(1,26))
C     Counter-term amplitude(s) for loop diagram number 24
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,27))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,28))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,29))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,30))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,31))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQT,AMPL(1,32))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,33))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),UV_GQQG_1EPS,AMPL(2,34))
      CALL FFV1_0(W(1,17),W(1,2),W(1,5),R2_GQQ,AMPL(1,35))
      CALL FFV1_2(W(1,9),W(1,5),GC_5,ZERO,ZERO,W(1,18))
C     Counter-term amplitude(s) for loop diagram number 27
      CALL FFV2_0(W(1,18),W(1,2),W(1,3),R2_BXTW,AMPL(1,36))
      CALL FFV2_2(W(1,11),W(1,3),GC_11,ZERO,ZERO,W(1,19))
C     Counter-term amplitude(s) for loop diagram number 30
      CALL R2_QQ_1_0(W(1,19),W(1,7),R2_QQQ,AMPL(1,37))
C     Counter-term amplitude(s) for loop diagram number 31
      CALL FFV2_0(W(1,11),W(1,7),W(1,3),R2_BXTW,AMPL(1,38))
C     Counter-term amplitude(s) for loop diagram number 32
      CALL FFV2_0(W(1,11),W(1,2),W(1,12),R2_BXTW,AMPL(1,39))
      CALL FFV1_2(W(1,11),W(1,4),GC_2,ZERO,ZERO,W(1,20))
C     Counter-term amplitude(s) for loop diagram number 34
      CALL FFV2_0(W(1,20),W(1,2),W(1,3),R2_BXTW,AMPL(1,40))
C     Counter-term amplitude(s) for loop diagram number 36
      CALL FFV1_0(W(1,19),W(1,2),W(1,4),R2_DDA,AMPL(1,41))
C     Counter-term amplitude(s) for loop diagram number 37
      CALL FFV2_0(W(1,1),W(1,14),W(1,3),R2_BXTW,AMPL(1,42))
      CALL FFV2_2(W(1,1),W(1,12),GC_11,ZERO,ZERO,W(1,21))
C     Counter-term amplitude(s) for loop diagram number 41
      CALL R2_QQ_1_0(W(1,21),W(1,8),R2_QQQ,AMPL(1,43))
C     Counter-term amplitude(s) for loop diagram number 42
      CALL FFV2_0(W(1,1),W(1,8),W(1,12),R2_BXTW,AMPL(1,44))
C     Counter-term amplitude(s) for loop diagram number 43
      CALL FFV2_0(W(1,1),W(1,16),W(1,3),R2_BXTW,AMPL(1,45))
C     Counter-term amplitude(s) for loop diagram number 47
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,46))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,47))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,48))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,49))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,50))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQT,AMPL(1,51))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQB_1EPS,AMPL(2,52))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),UV_GQQG_1EPS,AMPL(2,53))
      CALL FFV1_0(W(1,21),W(1,2),W(1,5),R2_GQQ,AMPL(1,54))
C     Counter-term amplitude(s) for loop diagram number 59
      CALL R2_QQ_1_0(W(1,18),W(1,10),R2_QQQ,AMPL(1,55))
      CALL FFV1_1(W(1,10),W(1,5),GC_5,ZERO,ZERO,W(1,22))
C     Counter-term amplitude(s) for loop diagram number 60
      CALL R2_QQ_1_0(W(1,9),W(1,22),R2_QQQ,AMPL(1,56))
C     Counter-term amplitude(s) for loop diagram number 61
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,57))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,58))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,59))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,60))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,61))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQT,AMPL(1,62))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQB_1EPS,AMPL(2,63))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),UV_GQQG_1EPS,AMPL(2,64))
      CALL FFV1_0(W(1,9),W(1,10),W(1,5),R2_GQQ,AMPL(1,65))
      CALL FFV2_1(W(1,8),W(1,3),GC_11,ZERO,ZERO,W(1,23))
C     Counter-term amplitude(s) for loop diagram number 63
      CALL R2_QQ_1_0(W(1,9),W(1,23),R2_QQQ,AMPL(1,66))
C     Counter-term amplitude(s) for loop diagram number 64
      CALL R2_QQ_1_0(W(1,20),W(1,10),R2_QQQ,AMPL(1,67))
      CALL FFV1_1(W(1,10),W(1,4),GC_2,ZERO,ZERO,W(1,24))
C     Counter-term amplitude(s) for loop diagram number 65
      CALL R2_QQ_1_0(W(1,11),W(1,24),R2_QQQ,AMPL(1,68))
C     Counter-term amplitude(s) for loop diagram number 66
      CALL FFV1_0(W(1,11),W(1,10),W(1,4),R2_UUA,AMPL(1,69))
      CALL FFV2_1(W(1,7),W(1,3),GC_11,ZERO,ZERO,W(1,25))
C     Counter-term amplitude(s) for loop diagram number 67
      CALL R2_QQ_1_0(W(1,11),W(1,25),R2_QQQ,AMPL(1,70))
      CALL FFV2_1(W(1,2),W(1,12),GC_11,ZERO,ZERO,W(1,26))
C     Counter-term amplitude(s) for loop diagram number 68
      CALL R2_QQ_1_0(W(1,11),W(1,26),R2_QQQ,AMPL(1,71))
C     Counter-term amplitude(s) for loop diagram number 69
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,72))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,73))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,74))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,75))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,76))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQT,AMPL(1,77))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQB_1EPS,AMPL(2,78))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),UV_GQQG_1EPS,AMPL(2,79))
      CALL FFV1_0(W(1,1),W(1,24),W(1,5),R2_GQQ,AMPL(1,80))
C     Counter-term amplitude(s) for loop diagram number 71
      CALL FFV1_0(W(1,1),W(1,22),W(1,4),R2_UUA,AMPL(1,81))
C     Counter-term amplitude(s) for loop diagram number 75
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,82))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,83))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,84))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,85))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,86))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQT,AMPL(1,87))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQB_1EPS,AMPL(2,88))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),UV_GQQG_1EPS,AMPL(2,89))
      CALL FFV1_0(W(1,1),W(1,25),W(1,5),R2_GQQ,AMPL(1,90))
C     Counter-term amplitude(s) for loop diagram number 77
      CALL FFV1_0(W(1,1),W(1,23),W(1,4),R2_UUA,AMPL(1,91))
C     Counter-term amplitude(s) for loop diagram number 78
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,92))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,93))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,94))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,95))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,96))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQT,AMPL(1,97))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQB_1EPS,AMPL(2,98))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),UV_GQQG_1EPS,AMPL(2,99))
      CALL FFV1_0(W(1,1),W(1,26),W(1,5),R2_GQQ,AMPL(1,100))
C     At this point, all CT amps needed for (QCD=4), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 2000

      GOTO 1001
 2000 CONTINUE
      CT_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

