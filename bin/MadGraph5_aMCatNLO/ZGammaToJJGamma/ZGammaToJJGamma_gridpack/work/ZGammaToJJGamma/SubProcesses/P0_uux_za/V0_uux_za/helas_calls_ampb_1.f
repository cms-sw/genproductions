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
      PARAMETER (NEXTERNAL=4)
      INTEGER    NCOMB
      PARAMETER (NCOMB=24)
      INTEGER NBORNAMPS
      PARAMETER (NBORNAMPS=2)
      INTEGER    NLOOPS, NLOOPGROUPS, NCTAMPS
      PARAMETER (NLOOPS=8, NLOOPGROUPS=8, NCTAMPS=6)
      INTEGER    NLOOPAMPS
      PARAMETER (NLOOPAMPS=14)
      INTEGER    NWAVEFUNCS,NLOOPWAVEFUNCS
      PARAMETER (NWAVEFUNCS=8,NLOOPWAVEFUNCS=20)
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
      CALL VXXXXX(P(0,3),MDL_MZ,NHEL(3),+1*IC(3),W(1,3))
      CALL VXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL FFV2_5_2(W(1,1),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,5))
C     Amplitude(s) for born diagram with ID 1
      CALL FFV1_0(W(1,5),W(1,2),W(1,4),GC_2,AMP(1))
      CALL FFV1_2(W(1,1),W(1,4),GC_2,ZERO,ZERO,W(1,6))
C     Amplitude(s) for born diagram with ID 2
      CALL FFV2_5_0(W(1,6),W(1,2),W(1,3),-GC_21,GC_23,AMP(2))
      CALL FFV1_1(W(1,2),W(1,4),GC_2,ZERO,ZERO,W(1,7))
C     Counter-term amplitude(s) for loop diagram number 3
      CALL R2_QQ_1_0(W(1,5),W(1,7),R2_QQQ,AMPL(1,1))
C     Counter-term amplitude(s) for loop diagram number 4
      CALL FFV1_0(W(1,5),W(1,2),W(1,4),R2_UUA,AMPL(1,2))
      CALL FFV2_5_1(W(1,2),W(1,3),-GC_21,GC_23,ZERO,ZERO,W(1,8))
C     Counter-term amplitude(s) for loop diagram number 5
      CALL R2_QQ_1_0(W(1,6),W(1,8),R2_QQQ,AMPL(1,3))
C     Counter-term amplitude(s) for loop diagram number 6
      CALL FFV2_5_0(W(1,6),W(1,2),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1,4)
     $ )
C     Counter-term amplitude(s) for loop diagram number 7
      CALL FFV1_0(W(1,1),W(1,8),W(1,4),R2_UUA,AMPL(1,5))
C     Counter-term amplitude(s) for loop diagram number 8
      CALL FFV2_5_0(W(1,1),W(1,7),W(1,3),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1,6)
     $ )
C     At this point, all CT amps needed for (QCD=2), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 2000

      GOTO 1001
 2000 CONTINUE
      CT_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

