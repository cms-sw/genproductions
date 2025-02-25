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
      PARAMETER (NBORNAMPS=6)
      INTEGER    NLOOPS, NLOOPGROUPS, NCTAMPS
      PARAMETER (NLOOPS=90, NLOOPGROUPS=50, NCTAMPS=108)
      INTEGER    NLOOPAMPS
      PARAMETER (NLOOPAMPS=198)
      INTEGER    NWAVEFUNCS,NLOOPWAVEFUNCS
      PARAMETER (NWAVEFUNCS=26,NLOOPWAVEFUNCS=185)
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

      CALL OXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))
      CALL VXXXXX(P(0,3),MDL_MZ,NHEL(3),+1*IC(3),W(1,3))
      CALL VXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL VXXXXX(P(0,5),ZERO,NHEL(5),+1*IC(5),W(1,5))
      CALL FFV2_5_1(W(1,1),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,6))
      CALL FFV1_2(W(1,2),W(1,4),GC_2,ZERO,ZERO,W(1,7))
C     Amplitude(s) for born diagram with ID 1
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),GC_5,AMP(1))
      CALL FFV1_2(W(1,2),W(1,5),GC_5,ZERO,ZERO,W(1,8))
C     Amplitude(s) for born diagram with ID 2
      CALL FFV1_0(W(1,8),W(1,6),W(1,4),GC_2,AMP(2))
      CALL FFV1_1(W(1,1),W(1,4),GC_2,ZERO,ZERO,W(1,9))
      CALL FFV2_5_2(W(1,2),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,10))
C     Amplitude(s) for born diagram with ID 3
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),GC_5,AMP(3))
C     Amplitude(s) for born diagram with ID 4
      CALL FFV2_5_0(W(1,8),W(1,9),W(1,3),-GC_21,GC_23,AMP(4))
      CALL FFV1_1(W(1,1),W(1,5),GC_5,ZERO,ZERO,W(1,11))
C     Amplitude(s) for born diagram with ID 5
      CALL FFV1_0(W(1,10),W(1,11),W(1,4),GC_2,AMP(5))
C     Amplitude(s) for born diagram with ID 6
      CALL FFV2_5_0(W(1,7),W(1,11),W(1,3),-GC_21,GC_23,AMP(6))
      CALL FFV1P0_3(W(1,2),W(1,1),GC_5,ZERO,ZERO,W(1,12))
C     Counter-term amplitude(s) for loop diagram number 7
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZADOWN,AMPL(1,1)
     $ )
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZADOWN,AMPL(1,2)
     $ )
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZADOWN,AMPL(1,3)
     $ )
      CALL FFV1P0_3(W(1,2),W(1,9),GC_5,ZERO,ZERO,W(1,13))
C     Counter-term amplitude(s) for loop diagram number 13
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),R2_GGZDOWN,AMPL(1,4))
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),R2_GGZDOWN,AMPL(1,5))
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),R2_GGZDOWN,AMPL(1,6))
      CALL FFV1P0_3(W(1,7),W(1,1),GC_5,ZERO,ZERO,W(1,14))
C     Counter-term amplitude(s) for loop diagram number 15
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),R2_GGZDOWN,AMPL(1,7))
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),R2_GGZDOWN,AMPL(1,8))
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),R2_GGZDOWN,AMPL(1,9))
C     Counter-term amplitude(s) for loop diagram number 17
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZAUP,AMPL(1,10))
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZAUP,AMPL(1,11))
      CALL FFV1_1(W(1,6),W(1,5),GC_5,ZERO,ZERO,W(1,15))
C     Counter-term amplitude(s) for loop diagram number 23
      CALL R2_QQ_1_0(W(1,7),W(1,15),R2_QQQ,AMPL(1,12))
      CALL FFV1_2(W(1,7),W(1,5),GC_5,ZERO,ZERO,W(1,16))
C     Counter-term amplitude(s) for loop diagram number 24
      CALL R2_QQ_1_0(W(1,16),W(1,6),R2_QQQ,AMPL(1,13))
C     Counter-term amplitude(s) for loop diagram number 25
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,14))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,15))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,16))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,17))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,18))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQT,AMPL(1,19))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQB_1EPS,AMPL(2,20))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),UV_GQQG_1EPS,AMPL(2,21))
      CALL FFV1_0(W(1,7),W(1,6),W(1,5),R2_GQQ,AMPL(1,22))
      CALL FFV1_1(W(1,6),W(1,4),GC_2,ZERO,ZERO,W(1,17))
C     Counter-term amplitude(s) for loop diagram number 27
      CALL R2_QQ_1_0(W(1,8),W(1,17),R2_QQQ,AMPL(1,23))
      CALL FFV1_2(W(1,8),W(1,4),GC_2,ZERO,ZERO,W(1,18))
C     Counter-term amplitude(s) for loop diagram number 28
      CALL R2_QQ_1_0(W(1,18),W(1,6),R2_QQQ,AMPL(1,24))
C     Counter-term amplitude(s) for loop diagram number 29
      CALL FFV1_0(W(1,8),W(1,6),W(1,4),R2_UUA,AMPL(1,25))
C     Counter-term amplitude(s) for loop diagram number 30
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,26))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,27))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,28))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,29))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,30))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQT,AMPL(1,31))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQB_1EPS,AMPL(2,32))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),UV_GQQG_1EPS,AMPL(2,33))
      CALL FFV1_0(W(1,2),W(1,17),W(1,5),R2_GQQ,AMPL(1,34))
C     Counter-term amplitude(s) for loop diagram number 33
      CALL FFV1_0(W(1,2),W(1,15),W(1,4),R2_UUA,AMPL(1,35))
      CALL FFV1_1(W(1,9),W(1,5),GC_5,ZERO,ZERO,W(1,19))
C     Counter-term amplitude(s) for loop diagram number 36
      CALL R2_QQ_1_0(W(1,10),W(1,19),R2_QQQ,AMPL(1,36))
      CALL FFV1_2(W(1,10),W(1,5),GC_5,ZERO,ZERO,W(1,20))
C     Counter-term amplitude(s) for loop diagram number 37
      CALL R2_QQ_1_0(W(1,20),W(1,9),R2_QQQ,AMPL(1,37))
C     Counter-term amplitude(s) for loop diagram number 38
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,38))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,39))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,40))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,41))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,42))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQT,AMPL(1,43))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQB_1EPS,AMPL(2,44))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),UV_GQQG_1EPS,AMPL(2,45))
      CALL FFV1_0(W(1,10),W(1,9),W(1,5),R2_GQQ,AMPL(1,46))
      CALL FFV2_5_1(W(1,9),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,21))
C     Counter-term amplitude(s) for loop diagram number 40
      CALL R2_QQ_1_0(W(1,8),W(1,21),R2_QQQ,AMPL(1,47))
      CALL FFV2_5_2(W(1,8),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,22))
C     Counter-term amplitude(s) for loop diagram number 41
      CALL R2_QQ_1_0(W(1,22),W(1,9),R2_QQQ,AMPL(1,48))
C     Counter-term amplitude(s) for loop diagram number 42
      CALL FFV2_5_0(W(1,8),W(1,9),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,49))
C     Counter-term amplitude(s) for loop diagram number 43
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,50))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,51))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,52))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,53))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,54))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQT,AMPL(1,55))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQB_1EPS,AMPL(2,56))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),UV_GQQG_1EPS,AMPL(2,57))
      CALL FFV1_0(W(1,2),W(1,21),W(1,5),R2_GQQ,AMPL(1,58))
C     Counter-term amplitude(s) for loop diagram number 46
      CALL FFV2_5_0(W(1,2),W(1,19),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,59))
C     Counter-term amplitude(s) for loop diagram number 49
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),-R2_GGZDOWN,AMPL(1,60))
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),-R2_GGZDOWN,AMPL(1,61))
      CALL FFV1_1(W(1,11),W(1,4),GC_2,ZERO,ZERO,W(1,23))
C     Counter-term amplitude(s) for loop diagram number 51
      CALL R2_QQ_1_0(W(1,10),W(1,23),R2_QQQ,AMPL(1,62))
      CALL FFV1_2(W(1,10),W(1,4),GC_2,ZERO,ZERO,W(1,24))
C     Counter-term amplitude(s) for loop diagram number 52
      CALL R2_QQ_1_0(W(1,24),W(1,11),R2_QQQ,AMPL(1,63))
C     Counter-term amplitude(s) for loop diagram number 53
      CALL FFV1_0(W(1,10),W(1,11),W(1,4),R2_UUA,AMPL(1,64))
      CALL FFV2_5_1(W(1,11),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,25))
C     Counter-term amplitude(s) for loop diagram number 54
      CALL R2_QQ_1_0(W(1,7),W(1,25),R2_QQQ,AMPL(1,65))
      CALL FFV2_5_2(W(1,7),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,26))
C     Counter-term amplitude(s) for loop diagram number 55
      CALL R2_QQ_1_0(W(1,26),W(1,11),R2_QQQ,AMPL(1,66))
C     Counter-term amplitude(s) for loop diagram number 56
      CALL FFV2_5_0(W(1,7),W(1,11),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,67))
C     Counter-term amplitude(s) for loop diagram number 58
      CALL FFV2_5_0(W(1,2),W(1,23),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,68))
C     Counter-term amplitude(s) for loop diagram number 60
      CALL FFV1_0(W(1,2),W(1,25),W(1,4),R2_UUA,AMPL(1,69))
C     Counter-term amplitude(s) for loop diagram number 61
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,70))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,71))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,72))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,73))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,74))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQT,AMPL(1,75))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,76))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),UV_GQQG_1EPS,AMPL(2,77))
      CALL FFV1_0(W(1,24),W(1,1),W(1,5),R2_GQQ,AMPL(1,78))
C     Counter-term amplitude(s) for loop diagram number 63
      CALL FFV1_0(W(1,20),W(1,1),W(1,4),R2_UUA,AMPL(1,79))
C     Counter-term amplitude(s) for loop diagram number 67
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,80))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,81))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,82))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,83))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,84))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQT,AMPL(1,85))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQB_1EPS,AMPL(2,86))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),UV_GQQG_1EPS,AMPL(2,87))
      CALL FFV1_0(W(1,26),W(1,1),W(1,5),R2_GQQ,AMPL(1,88))
C     Counter-term amplitude(s) for loop diagram number 69
      CALL FFV2_5_0(W(1,16),W(1,1),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,89))
C     Counter-term amplitude(s) for loop diagram number 74
      CALL FFV2_5_0(W(1,18),W(1,1),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,90))
C     Counter-term amplitude(s) for loop diagram number 76
      CALL FFV1_0(W(1,22),W(1,1),W(1,4),R2_UUA,AMPL(1,91))
C     Counter-term amplitude(s) for loop diagram number 85
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),-R2_GGZDOWN,AMPL(1,92))
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),-R2_GGZDOWN,AMPL(1,93))
C     Counter-term amplitude(s) for loop diagram number 87
      CALL R2_GGVV_0(W(1,5),W(1,12),W(1,3),W(1,4),R2_GGZAUP,AMPL(1,94))
C     Counter-term amplitude(s) for loop diagram number 93
      CALL R2_GGZ_0(W(1,5),W(1,13),W(1,3),-R2_GGZDOWN,AMPL(1,95))
C     Counter-term amplitude(s) for loop diagram number 95
      CALL R2_GGZ_0(W(1,5),W(1,14),W(1,3),-R2_GGZDOWN,AMPL(1,96))
C     At this point, all CT amps needed for (QCD=4), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 2000

      GOTO 1001
 2000 CONTINUE
      CT_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

