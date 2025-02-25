      SUBROUTINE LOOP_CT_CALLS_1(P,NHEL,H,IC)
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
      PARAMETER (NWAVEFUNCS=26,NLOOPWAVEFUNCS=181)
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
      IF (FILTER_SO.AND.CTCALL_REQ_SO_DONE) THEN
        GOTO 1001
      ENDIF

C     CutTools call for loop numbers 1,57
      CALL LOOP_2(7,12,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,1)
C     CutTools call for loop numbers 2,9
      CALL LOOP_2(6,13,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,2)
C     CutTools call for loop numbers 3
      CALL LOOP_3(4,6,7,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,3)
C     CutTools call for loop numbers 4
      CALL LOOP_3(3,6,8,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,4)
C     CutTools call for loop numbers 5
      CALL LOOP_4(3,4,5,6,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,5)
C     CutTools call for loop numbers 6,60
      CALL LOOP_3(3,5,12,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,6)
C     CutTools call for loop numbers 7
      CALL LOOP_4(3,4,6,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,7)
C     CutTools call for loop numbers 8,51
      CALL LOOP_2(8,14,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,8)
C     CutTools call for loop numbers 10,54
      CALL LOOP_3(4,5,14,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,9)
C     CutTools call for loop numbers 11,52
      CALL LOOP_2(10,16,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,10)
C     CutTools call for loop numbers 12,15
      CALL LOOP_2(9,17,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,11)
C     CutTools call for loop numbers 13
      CALL LOOP_3(4,10,9,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,12)
C     CutTools call for loop numbers 14,58
      CALL LOOP_2(11,18,DCMPLX(ZERO),DCMPLX(ZERO),1,I_SO,13)
C     CutTools call for loop numbers 16
      CALL LOOP_3(3,11,9,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,14)
C     CutTools call for loop numbers 17
      CALL LOOP_4(2,3,4,9,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,15)
C     CutTools call for loop numbers 18,68
      CALL LOOP_3(2,3,16,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,16)
C     CutTools call for loop numbers 19
      CALL LOOP_4(2,4,3,9,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,17)
C     CutTools call for loop numbers 20,65
      CALL LOOP_3(2,4,18,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,18)
C     CutTools call for loop numbers 21,53
      CALL LOOP_3(1,10,8,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,19)
C     CutTools call for loop numbers 22
      CALL LOOP_4(1,4,5,10,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,20)
C     CutTools call for loop numbers 23
      CALL LOOP_4(1,4,10,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,21)
C     CutTools call for loop numbers 24,30,56,62
      CALL LOOP_3(1,5,17,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,22)
C     CutTools call for loop numbers 25,28,43,45,71,72,79,80
      CALL LOOP_3(1,3,20,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),3,I_SO
     $ ,23)
C     CutTools call for loop numbers 26,59
      CALL LOOP_3(1,11,7,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,24)
C     CutTools call for loop numbers 27
      CALL LOOP_4(1,3,5,11,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,25)
C     CutTools call for loop numbers 29
      CALL LOOP_4(1,3,11,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,26)
C     CutTools call for loop numbers 31,33,73,75
      CALL LOOP_4(1,3,4,21,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),4,I_SO,27)
C     CutTools call for loop numbers 32,35,74,77
      CALL LOOP_4(1,4,3,21,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),4,I_SO,28)
C     CutTools call for loop numbers 34,36,76,78
      CALL LOOP_4(1,3,21,4,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),4,I_SO,29)
C     CutTools call for loop numbers 37,44,63,69
      CALL LOOP_3(1,2,13,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO),2,I_SO
     $ ,30)
C     CutTools call for loop numbers 38
      CALL LOOP_4(1,4,2,7,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,31)
C     CutTools call for loop numbers 39
      CALL LOOP_4(1,2,7,4,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,32)
C     CutTools call for loop numbers 40
      CALL LOOP_5(1,2,5,4,3,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,33)
C     CutTools call for loop numbers 41
      CALL LOOP_5(1,3,5,2,4,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,34)
C     CutTools call for loop numbers 42
      CALL LOOP_4(1,2,8,3,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,35)
C     CutTools call for loop numbers 46
      CALL LOOP_4(1,3,2,8,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,36)
C     CutTools call for loop numbers 47
      CALL LOOP_5(1,2,5,3,4,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,37)
C     CutTools call for loop numbers 48
      CALL LOOP_5(1,3,2,5,4,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,38)
C     CutTools call for loop numbers 49
      CALL LOOP_5(1,3,4,2,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,39)
C     CutTools call for loop numbers 50
      CALL LOOP_5(1,4,3,2,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,40)
C     CutTools call for loop numbers 55
      CALL LOOP_4(1,5,4,10,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,41)
C     CutTools call for loop numbers 61
      CALL LOOP_4(1,5,3,11,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,42)
C     CutTools call for loop numbers 64
      CALL LOOP_4(1,2,4,7,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,43)
C     CutTools call for loop numbers 66
      CALL LOOP_5(1,2,3,4,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,44)
C     CutTools call for loop numbers 67
      CALL LOOP_4(1,2,3,8,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),3,I_SO,45)
C     CutTools call for loop numbers 70
      CALL LOOP_5(1,2,4,3,5,DCMPLX(ZERO),DCMPLX(ZERO),DCMPLX(ZERO)
     $ ,DCMPLX(ZERO),DCMPLX(ZERO),4,I_SO,46)
C     CutTools call for loop numbers 81,82,89,90
      CALL LOOP_3(1,3,20,DCMPLX(MDL_MT),DCMPLX(MDL_MT),DCMPLX(MDL_MT)
     $ ,3,I_SO,47)
C     CutTools call for loop numbers 83,85
      CALL LOOP_4(1,3,4,21,DCMPLX(MDL_MT),DCMPLX(MDL_MT),DCMPLX(MDL_MT)
     $ ,DCMPLX(MDL_MT),4,I_SO,48)
C     CutTools call for loop numbers 84,87
      CALL LOOP_4(1,4,3,21,DCMPLX(MDL_MT),DCMPLX(MDL_MT),DCMPLX(MDL_MT)
     $ ,DCMPLX(MDL_MT),4,I_SO,49)
C     CutTools call for loop numbers 86,88
      CALL LOOP_4(1,3,21,4,DCMPLX(MDL_MT),DCMPLX(MDL_MT),DCMPLX(MDL_MT)
     $ ,DCMPLX(MDL_MT),4,I_SO,50)
C     At this point, all reductions needed for (QCD=4), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 5000

      GOTO 1001
 5000 CONTINUE
      CTCALL_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

