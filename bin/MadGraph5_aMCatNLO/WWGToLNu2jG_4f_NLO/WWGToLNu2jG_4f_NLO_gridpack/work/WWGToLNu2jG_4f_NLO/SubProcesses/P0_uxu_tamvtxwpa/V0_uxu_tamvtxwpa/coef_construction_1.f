      SUBROUTINE COEF_CONSTRUCTION_1(P,NHEL,H,IC)
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
      PARAMETER (NEXTERNAL=6)
      INTEGER    NCOMB
      PARAMETER (NCOMB=96)
      INTEGER NBORNAMPS
      PARAMETER (NBORNAMPS=32)
      INTEGER    NLOOPS, NLOOPGROUPS, NCTAMPS
      PARAMETER (NLOOPS=92, NLOOPGROUPS=29, NCTAMPS=70)
      INTEGER    NLOOPAMPS
      PARAMETER (NLOOPAMPS=162)
      INTEGER    NWAVEFUNCS,NLOOPWAVEFUNCS
      PARAMETER (NWAVEFUNCS=73,NLOOPWAVEFUNCS=132)
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
      IF (FILTER_SO.AND.LOOP_REQ_SO_DONE) THEN
        GOTO 1001
      ENDIF

C     Coefficient construction for loop diagram with ID 33
      CALL FFV1L2P0_3(PL(0,0),W(1,19),GC_5,ZERO,ZERO,PL(0,1),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,1))
      CALL FFV1L3_1(PL(0,1),W(1,28),GC_5,ZERO,ZERO,PL(0,2),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1,2))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,2),1,4,1,1,1,71,H)
C     Coefficient construction for loop diagram with ID 34
      CALL FFV2L2_1(PL(0,0),W(1,8),GC_11,ZERO,ZERO,PL(0,3),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,3))
      CALL FFV1L2P0_3(PL(0,3),W(1,19),GC_5,ZERO,ZERO,PL(0,4),COEFS)
      CALL UPDATE_WL_1_0(WL(1,0,1,3),4,COEFS,4,4,WL(1,0,1,4))
      CALL FFV1L3_1(PL(0,4),W(1,18),GC_5,ZERO,ZERO,PL(0,5),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,4),4,COEFS,4,4,WL(1,0,1,5))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,5),2,4,2,1,1,72,H)
C     Coefficient construction for loop diagram with ID 35
      CALL FFV1L3_2(PL(0,0),W(1,2),GC_5,ZERO,ZERO,PL(0,6),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,6))
      CALL FFV2L1_2(PL(0,6),W(1,5),GC_11,ZERO,ZERO,PL(0,7),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1,7))
      CALL FFV1L1P0_3(PL(0,7),W(1,28),GC_5,ZERO,ZERO,PL(0,8),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,8))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,8),2,4,3,1,1,73,H)
C     Coefficient construction for loop diagram with ID 36
      CALL FFV2L1_2(PL(0,7),W(1,8),GC_11,ZERO,ZERO,PL(0,9),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,9))
      CALL FFV1L1P0_3(PL(0,9),W(1,18),GC_5,ZERO,ZERO,PL(0,10),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,9),4,COEFS,4,4,WL(1,0,1,10))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,10),3,4,4,1,1,74,H)
C     Coefficient construction for loop diagram with ID 37
      CALL FFV1L1P0_3(PL(0,0),W(1,24),GC_5,ZERO,ZERO,PL(0,11),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,11))
      CALL FFV1L3_2(PL(0,11),W(1,29),GC_5,ZERO,ZERO,PL(0,12),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,11),4,COEFS,4,4,WL(1,0,1,12))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,12),1,4,5,1,1,75,H)
C     Coefficient construction for loop diagram with ID 38
      CALL FFV1L3_1(PL(0,1),W(1,30),GC_5,ZERO,ZERO,PL(0,13),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1,13))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,13),1,4,1,1,1,76,H)
C     Coefficient construction for loop diagram with ID 39
      CALL FFV1L3_1(PL(0,0),W(1,1),GC_5,ZERO,ZERO,PL(0,14),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,14))
      CALL FFV2L2_1(PL(0,14),W(1,8),GC_11,ZERO,ZERO,PL(0,15),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,14),4,COEFS,4,4,WL(1,0,1,15))
      CALL FFV1L2P0_3(PL(0,15),W(1,29),GC_5,ZERO,ZERO,PL(0,16),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,15),4,COEFS,4,4,WL(1,0,1,16))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,16),2,4,6,1,1,77,H)
C     Coefficient construction for loop diagram with ID 40
      CALL FFV1L1P0_3(PL(0,0),W(1,1),GC_5,ZERO,ZERO,PL(0,17),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,17))
      CALL FFV1L3_2(PL(0,17),W(1,19),GC_5,ZERO,ZERO,PL(0,18),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,17),4,COEFS,4,4,WL(1,0,1,18))
      CALL FFV2L1_2(PL(0,18),W(1,31),GC_11,ZERO,ZERO,PL(0,19),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,18),4,COEFS,4,4,WL(1,0,1,19))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,19),2,4,7,1,1,78,H)
C     Coefficient construction for loop diagram with ID 41
      CALL FFV1L3_1(PL(0,1),W(1,32),GC_5,ZERO,ZERO,PL(0,20),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1,20))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,20),1,4,1,1,1,79,H)
C     Coefficient construction for loop diagram with ID 42
      CALL FFV1L2_1(PL(0,14),W(1,6),GC_2,ZERO,ZERO,PL(0,21),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,14),4,COEFS,4,4,WL(1,0,1,21))
      CALL FFV2L2_1(PL(0,21),W(1,8),GC_11,ZERO,ZERO,PL(0,22),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,22))
      CALL FFV1L2P0_3(PL(0,22),W(1,19),GC_5,ZERO,ZERO,PL(0,23),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,22),4,COEFS,4,4,WL(1,0,1,23))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,23),3,4,8,1,1,80,H)
C     Coefficient construction for loop diagram with ID 43
      CALL FFV1L2_1(PL(0,0),W(1,6),GC_1,ZERO,ZERO,PL(0,24),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,24))
      CALL FFV1L2P0_3(PL(0,24),W(1,19),GC_5,ZERO,ZERO,PL(0,25),COEFS)
      CALL UPDATE_WL_1_0(WL(1,0,1,24),4,COEFS,4,4,WL(1,0,1,25))
      CALL FFV1L3_1(PL(0,25),W(1,24),GC_5,ZERO,ZERO,PL(0,26),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,25),4,COEFS,4,4,WL(1,0,1,26))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,26),2,4,9,1,1,81,H)
C     Coefficient construction for loop diagram with ID 44
      CALL FFV1L2_1(PL(0,15),W(1,6),GC_1,ZERO,ZERO,PL(0,27),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,15),4,COEFS,4,4,WL(1,0,1,27))
      CALL FFV1L2P0_3(PL(0,27),W(1,19),GC_5,ZERO,ZERO,PL(0,28),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,27),4,COEFS,4,4,WL(1,0,1,28))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,28),3,4,10,1,1,82,H)
C     Coefficient construction for loop diagram with ID 45
      CALL FFV2L1_2(PL(0,18),W(1,33),GC_11,ZERO,ZERO,PL(0,29),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,18),4,COEFS,4,4,WL(1,0,1,29))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,29),2,4,7,1,1,83,H)
C     Coefficient construction for loop diagram with ID 46
      CALL FFV1L3_1(PL(0,1),W(1,34),GC_5,ZERO,ZERO,PL(0,30),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,1),4,COEFS,4,4,WL(1,0,1,30))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,30),1,4,1,1,1,84,H)
C     Coefficient construction for loop diagram with ID 47
      CALL FFV1L3_2(PL(0,11),W(1,35),GC_5,ZERO,ZERO,PL(0,31),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,11),4,COEFS,4,4,WL(1,0,1,31))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,31),1,4,5,1,1,85,H)
C     Coefficient construction for loop diagram with ID 48
      CALL FFV1L2P0_3(PL(0,15),W(1,35),GC_5,ZERO,ZERO,PL(0,32),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,15),4,COEFS,4,4,WL(1,0,1,32))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,32),2,4,6,1,1,86,H)
C     Coefficient construction for loop diagram with ID 49
      CALL FFV2L2_1(PL(0,0),W(1,5),GC_11,ZERO,ZERO,PL(0,33),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,33))
      CALL FFV1L2P0_3(PL(0,33),W(1,25),GC_5,ZERO,ZERO,PL(0,34),COEFS)
      CALL UPDATE_WL_1_0(WL(1,0,1,33),4,COEFS,4,4,WL(1,0,1,34))
      CALL FFV1L3_1(PL(0,34),W(1,24),GC_5,ZERO,ZERO,PL(0,35),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,34),4,COEFS,4,4,WL(1,0,1,35))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,35),2,4,11,1,1,87,H)
C     Coefficient construction for loop diagram with ID 50
      CALL FFV2L2_1(PL(0,15),W(1,5),GC_11,ZERO,ZERO,PL(0,36),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,15),4,COEFS,4,4,WL(1,0,1,36))
      CALL FFV1L2P0_3(PL(0,36),W(1,25),GC_5,ZERO,ZERO,PL(0,37),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,36),4,COEFS,4,4,WL(1,0,1,37))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,37),3,4,12,1,1,88,H)
C     Coefficient construction for loop diagram with ID 51
      CALL FFV1L3_2(PL(0,11),W(1,36),GC_5,ZERO,ZERO,PL(0,38),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,11),4,COEFS,4,4,WL(1,0,1,38))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,38),1,4,5,1,1,89,H)
C     Coefficient construction for loop diagram with ID 52
      CALL FFV2L1_2(PL(0,6),W(1,12),GC_11,ZERO,ZERO,PL(0,39),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1,39))
      CALL FFV1L1P0_3(PL(0,39),W(1,24),GC_5,ZERO,ZERO,PL(0,40),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,39),4,COEFS,4,4,WL(1,0,1,40))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,40),2,4,13,1,1,90,H)
C     Coefficient construction for loop diagram with ID 53
      CALL FFV1L2P0_3(PL(0,15),W(1,36),GC_5,ZERO,ZERO,PL(0,41),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,15),4,COEFS,4,4,WL(1,0,1,41))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,41),2,4,6,1,1,91,H)
C     Coefficient construction for loop diagram with ID 54
      CALL FFV1L3_2(PL(0,17),W(1,2),GC_5,ZERO,ZERO,PL(0,42),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,17),4,COEFS,4,4,WL(1,0,1,42))
      CALL FFV2L1_2(PL(0,42),W(1,12),GC_11,ZERO,ZERO,PL(0,43),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,43))
      CALL FFV2L1_2(PL(0,43),W(1,8),GC_11,ZERO,ZERO,PL(0,44),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,43),4,COEFS,4,4,WL(1,0,1,44))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,44),3,4,14,1,1,92,H)
C     Coefficient construction for loop diagram with ID 55
      CALL FFV1L1P0_3(PL(0,7),W(1,30),GC_5,ZERO,ZERO,PL(0,45),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,45))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,45),2,4,3,1,1,93,H)
C     Coefficient construction for loop diagram with ID 56
      CALL FFV1L1_2(PL(0,6),W(1,6),GC_2,ZERO,ZERO,PL(0,46),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,6),4,COEFS,4,4,WL(1,0,1,46))
      CALL FFV2L1_2(PL(0,46),W(1,5),GC_11,ZERO,ZERO,PL(0,47),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,47))
      CALL FFV1L1P0_3(PL(0,47),W(1,24),GC_5,ZERO,ZERO,PL(0,48),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,47),4,COEFS,4,4,WL(1,0,1,48))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,48),3,4,15,1,1,94,H)
C     Coefficient construction for loop diagram with ID 57
      CALL FFV2L1_2(PL(0,42),W(1,5),GC_11,ZERO,ZERO,PL(0,49),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,49))
      CALL FFV2L1_2(PL(0,49),W(1,31),GC_11,ZERO,ZERO,PL(0,50),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,49),4,COEFS,4,4,WL(1,0,1,50))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,50),3,4,16,1,1,95,H)
C     Coefficient construction for loop diagram with ID 58
      CALL FFV1L1P0_3(PL(0,7),W(1,32),GC_5,ZERO,ZERO,PL(0,51),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,51))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,51),2,4,3,1,1,96,H)
C     Coefficient construction for loop diagram with ID 59
      CALL FFV2L1_2(PL(0,49),W(1,8),GC_11,ZERO,ZERO,PL(0,52),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,49),4,COEFS,4,4,WL(1,0,1,52))
      CALL FFV1L1_2(PL(0,52),W(1,6),GC_2,ZERO,ZERO,PL(0,53),COEFS)
      CALL UPDATE_WL_3_1(WL(1,0,1,52),4,COEFS,4,4,WL(1,0,1,53))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,53),4,4,17,1,1,97,H)
C     Coefficient construction for loop diagram with ID 60
      CALL FFV1L1_2(PL(0,42),W(1,6),GC_2,ZERO,ZERO,PL(0,54),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,54))
      CALL FFV2L1_2(PL(0,54),W(1,5),GC_11,ZERO,ZERO,PL(0,55),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,55))
      CALL FFV2L1_2(PL(0,55),W(1,8),GC_11,ZERO,ZERO,PL(0,56),COEFS)
      CALL UPDATE_WL_3_1(WL(1,0,1,55),4,COEFS,4,4,WL(1,0,1,56))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,56),4,4,18,1,1,98,H)
C     Coefficient construction for loop diagram with ID 61
      CALL FFV1L1_2(PL(0,7),W(1,6),GC_1,ZERO,ZERO,PL(0,57),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,57))
      CALL FFV1L1P0_3(PL(0,57),W(1,24),GC_5,ZERO,ZERO,PL(0,58),COEFS)
      CALL UPDATE_WL_3_0(WL(1,0,1,57),4,COEFS,4,4,WL(1,0,1,58))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,58),3,4,19,1,1,99,H)
C     Coefficient construction for loop diagram with ID 62
      CALL FFV1L1_2(PL(0,49),W(1,6),GC_1,ZERO,ZERO,PL(0,59),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,49),4,COEFS,4,4,WL(1,0,1,59))
      CALL FFV2L1_2(PL(0,59),W(1,8),GC_11,ZERO,ZERO,PL(0,60),COEFS)
      CALL UPDATE_WL_3_1(WL(1,0,1,59),4,COEFS,4,4,WL(1,0,1,60))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,60),4,4,20,1,1,100,H)
C     Coefficient construction for loop diagram with ID 63
      CALL FFV2L1_2(PL(0,49),W(1,33),GC_11,ZERO,ZERO,PL(0,61),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,49),4,COEFS,4,4,WL(1,0,1,61))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,61),3,4,16,1,1,101,H)
C     Coefficient construction for loop diagram with ID 64
      CALL FFV1L1P0_3(PL(0,7),W(1,34),GC_5,ZERO,ZERO,PL(0,62),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,7),4,COEFS,4,4,WL(1,0,1,62))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,62),2,4,3,1,1,102,H)
C     Coefficient construction for loop diagram with ID 65
      CALL FFV1L1P0_3(PL(0,0),W(1,18),GC_5,ZERO,ZERO,PL(0,63),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,63))
      CALL FFV1L3_2(PL(0,63),W(1,37),GC_5,ZERO,ZERO,PL(0,64),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,64))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,64),1,4,21,1,1,103,H)
C     Coefficient construction for loop diagram with ID 66
      CALL FFV1L2P0_3(PL(0,0),W(1,2),GC_5,ZERO,ZERO,PL(0,65),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,65))
      CALL FFV1L3_1(PL(0,65),W(1,18),GC_5,ZERO,ZERO,PL(0,66),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,65),4,COEFS,4,4,WL(1,0,1,66))
      CALL FFV1L2_1(PL(0,66),W(1,38),GC_2,ZERO,ZERO,PL(0,67),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0,1,67))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,67),2,4,22,1,1,104,H)
C     Coefficient construction for loop diagram with ID 67
      CALL FFV2_5L2_1(PL(0,66),W(1,39),-GC_21,GC_23,ZERO,ZERO,PL(0,68)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0,1,68))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,68),2,4,22,1,1,105,H)
C     Coefficient construction for loop diagram with ID 68
      CALL FFV2_5L2_1(PL(0,66),W(1,40),-GC_21,GC_23,ZERO,ZERO,PL(0,69)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0,1,69))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,69),2,4,22,1,1,106,H)
C     Coefficient construction for loop diagram with ID 69
      CALL FFV1L2_1(PL(0,66),W(1,41),GC_2,ZERO,ZERO,PL(0,70),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0,1,70))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,70),2,4,22,1,1,107,H)
C     Coefficient construction for loop diagram with ID 70
      CALL FFV2_5L2_1(PL(0,66),W(1,42),-GC_21,GC_23,ZERO,ZERO,PL(0,71)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,66),4,COEFS,4,4,WL(1,0,1,71))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,71),2,4,22,1,1,108,H)
C     Coefficient construction for loop diagram with ID 71
      CALL FFV1L3_2(PL(0,63),W(1,43),GC_5,ZERO,ZERO,PL(0,72),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,72))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,72),1,4,21,1,1,109,H)
C     Coefficient construction for loop diagram with ID 72
      CALL FFV1L3_2(PL(0,63),W(1,44),GC_5,ZERO,ZERO,PL(0,73),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,73))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,73),1,4,21,1,1,110,H)
C     Coefficient construction for loop diagram with ID 73
      CALL FFV1L3_2(PL(0,63),W(1,45),GC_5,ZERO,ZERO,PL(0,74),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,74))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,74),1,4,21,1,1,111,H)
C     Coefficient construction for loop diagram with ID 74
      CALL FFV1L3_2(PL(0,63),W(1,46),GC_5,ZERO,ZERO,PL(0,75),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,75))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,75),1,4,21,1,1,112,H)
C     Coefficient construction for loop diagram with ID 75
      CALL FFV1L3_2(PL(0,63),W(1,47),GC_5,ZERO,ZERO,PL(0,76),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,63),4,COEFS,4,4,WL(1,0,1,76))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,76),1,4,21,1,1,113,H)
C     Coefficient construction for loop diagram with ID 76
      CALL FFV1L2P0_3(PL(0,21),W(1,37),GC_5,ZERO,ZERO,PL(0,77),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,77))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,77),2,4,23,1,1,114,H)
C     Coefficient construction for loop diagram with ID 77
      CALL FFV1L3_2(PL(0,17),W(1,25),GC_5,ZERO,ZERO,PL(0,78),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,17),4,COEFS,4,4,WL(1,0,1,78))
      CALL FFV1L1_2(PL(0,78),W(1,38),GC_2,ZERO,ZERO,PL(0,79),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0,1,79))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,79),2,4,24,1,1,115,H)
C     Coefficient construction for loop diagram with ID 78
      CALL FFV2_5L1_2(PL(0,78),W(1,39),-GC_21,GC_23,ZERO,ZERO,PL(0,80)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0,1,80))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,80),2,4,24,1,1,116,H)
C     Coefficient construction for loop diagram with ID 79
      CALL FFV2_5L1_2(PL(0,78),W(1,40),-GC_21,GC_23,ZERO,ZERO,PL(0,81)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0,1,81))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,81),2,4,24,1,1,117,H)
C     Coefficient construction for loop diagram with ID 80
      CALL FFV1L1_2(PL(0,78),W(1,41),GC_2,ZERO,ZERO,PL(0,82),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0,1,82))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,82),2,4,24,1,1,118,H)
C     Coefficient construction for loop diagram with ID 81
      CALL FFV2_5L1_2(PL(0,78),W(1,42),-GC_21,GC_23,ZERO,ZERO,PL(0,83)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,78),4,COEFS,4,4,WL(1,0,1,83))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,83),2,4,24,1,1,119,H)
C     Coefficient construction for loop diagram with ID 82
      CALL FFV1L1_2(PL(0,54),W(1,38),GC_2,ZERO,ZERO,PL(0,84),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,84))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,84),3,4,25,1,1,120,H)
C     Coefficient construction for loop diagram with ID 83
      CALL FFV2_5L1_2(PL(0,54),W(1,39),-GC_21,GC_23,ZERO,ZERO,PL(0,85)
     $ ,COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,85))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,85),3,4,25,1,1,121,H)
C     Coefficient construction for loop diagram with ID 84
      CALL FFV1L1_2(PL(0,42),W(1,48),GC_2,ZERO,ZERO,PL(0,86),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,86))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,86),2,4,26,1,1,122,H)
C     Coefficient construction for loop diagram with ID 85
      CALL FFV2_5L1_2(PL(0,42),W(1,49),-GC_21,GC_23,ZERO,ZERO,PL(0,87)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,87))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,87),2,4,26,1,1,123,H)
C     Coefficient construction for loop diagram with ID 86
      CALL FFV1L1_2(PL(0,42),W(1,50),GC_2,ZERO,ZERO,PL(0,88),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,88))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,88),2,4,26,1,1,124,H)
C     Coefficient construction for loop diagram with ID 87
      CALL FFV2_5L1_2(PL(0,42),W(1,51),-GC_21,GC_23,ZERO,ZERO,PL(0,89)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,89))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,89),2,4,26,1,1,125,H)
C     Coefficient construction for loop diagram with ID 88
      CALL FFV1L1_2(PL(0,42),W(1,52),GC_2,ZERO,ZERO,PL(0,90),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,90))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,90),2,4,26,1,1,126,H)
C     Coefficient construction for loop diagram with ID 89
      CALL FFV2_5L1_2(PL(0,42),W(1,53),-GC_21,GC_23,ZERO,ZERO,PL(0,91)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,91))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,91),2,4,26,1,1,127,H)
C     Coefficient construction for loop diagram with ID 90
      CALL FFV1L1_2(PL(0,42),W(1,38),GC_2,ZERO,ZERO,PL(0,92),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,92))
      CALL FFV1L1_2(PL(0,92),W(1,6),GC_2,ZERO,ZERO,PL(0,93),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,92),4,COEFS,4,4,WL(1,0,1,93))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,93),3,4,27,1,1,128,H)
C     Coefficient construction for loop diagram with ID 91
      CALL FFV2_5L1_2(PL(0,42),W(1,39),-GC_21,GC_23,ZERO,ZERO,PL(0,94)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,94))
      CALL FFV1L1_2(PL(0,94),W(1,6),GC_2,ZERO,ZERO,PL(0,95),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,94),4,COEFS,4,4,WL(1,0,1,95))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,95),3,4,27,1,1,129,H)
C     Coefficient construction for loop diagram with ID 92
      CALL FFV1L2P0_3(PL(0,21),W(1,43),GC_5,ZERO,ZERO,PL(0,96),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,96))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,96),2,4,23,1,1,130,H)
C     Coefficient construction for loop diagram with ID 93
      CALL FFV1L2P0_3(PL(0,21),W(1,44),GC_5,ZERO,ZERO,PL(0,97),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,97))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,97),2,4,23,1,1,131,H)
C     Coefficient construction for loop diagram with ID 94
      CALL FFV2_5L1_2(PL(0,54),W(1,40),-GC_21,GC_23,ZERO,ZERO,PL(0,98)
     $ ,COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,98))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,98),3,4,25,1,1,132,H)
C     Coefficient construction for loop diagram with ID 95
      CALL FFV2_5L1_2(PL(0,42),W(1,40),-GC_21,GC_23,ZERO,ZERO,PL(0,99)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,99))
      CALL FFV1L1_2(PL(0,99),W(1,6),GC_2,ZERO,ZERO,PL(0,100),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,99),4,COEFS,4,4,WL(1,0,1,100))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,100),3,4,27,1,1,133,H)
C     Coefficient construction for loop diagram with ID 96
      CALL FFV1L2P0_3(PL(0,21),W(1,45),GC_5,ZERO,ZERO,PL(0,101),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,101))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,101),2,4,23,1,1,134,H)
C     Coefficient construction for loop diagram with ID 97
      CALL FFV1L1_2(PL(0,42),W(1,54),GC_2,ZERO,ZERO,PL(0,102),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,102))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,102),2,4,26,1,1,135,H)
C     Coefficient construction for loop diagram with ID 98
      CALL FFV2_5L1_2(PL(0,42),W(1,55),-GC_21,GC_23,ZERO,ZERO,PL(0,103)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,103))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,103),2,4,26,1,1,136,H)
C     Coefficient construction for loop diagram with ID 99
      CALL FFV2_5L1_2(PL(0,42),W(1,57),-GC_21,GC_23,ZERO,ZERO,PL(0,104)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,104))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,104),2,4,26,1,1,137,H)
C     Coefficient construction for loop diagram with ID 100
      CALL FFV1L1_2(PL(0,42),W(1,58),GC_2,ZERO,ZERO,PL(0,105),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,105))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,105),2,4,26,1,1,138,H)
C     Coefficient construction for loop diagram with ID 101
      CALL FFV2_5L1_2(PL(0,42),W(1,59),-GC_21,GC_23,ZERO,ZERO,PL(0,106)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,106))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,106),2,4,26,1,1,139,H)
C     Coefficient construction for loop diagram with ID 102
      CALL FFV1L1_2(PL(0,54),W(1,41),GC_2,ZERO,ZERO,PL(0,107),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,107))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,107),3,4,25,1,1,140,H)
C     Coefficient construction for loop diagram with ID 103
      CALL FFV2_5L1_2(PL(0,54),W(1,42),-GC_21,GC_23,ZERO,ZERO,PL(0,108)
     $ ,COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,54),4,COEFS,4,4,WL(1,0,1,108))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,108),3,4,25,1,1,141,H)
C     Coefficient construction for loop diagram with ID 104
      CALL FFV1L1_2(PL(0,42),W(1,61),GC_2,ZERO,ZERO,PL(0,109),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,109))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,109),2,4,26,1,1,142,H)
C     Coefficient construction for loop diagram with ID 105
      CALL FFV2_5L1_2(PL(0,42),W(1,62),-GC_21,GC_23,ZERO,ZERO,PL(0,110)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,110))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,110),2,4,26,1,1,143,H)
C     Coefficient construction for loop diagram with ID 106
      CALL FFV1L1_2(PL(0,42),W(1,41),GC_2,ZERO,ZERO,PL(0,111),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,111))
      CALL FFV1L1_2(PL(0,111),W(1,6),GC_2,ZERO,ZERO,PL(0,112),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,111),4,COEFS,4,4,WL(1,0,1,112))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,112),3,4,27,1,1,144,H)
C     Coefficient construction for loop diagram with ID 107
      CALL FFV2_5L1_2(PL(0,42),W(1,42),-GC_21,GC_23,ZERO,ZERO,PL(0,113)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,113))
      CALL FFV1L1_2(PL(0,113),W(1,6),GC_2,ZERO,ZERO,PL(0,114),COEFS)
      CALL UPDATE_WL_2_1(WL(1,0,1,113),4,COEFS,4,4,WL(1,0,1,114))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,114),3,4,27,1,1,145,H)
C     Coefficient construction for loop diagram with ID 108
      CALL FFV1L2P0_3(PL(0,21),W(1,46),GC_5,ZERO,ZERO,PL(0,115),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,115))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,115),2,4,23,1,1,146,H)
C     Coefficient construction for loop diagram with ID 109
      CALL FFV1L2P0_3(PL(0,21),W(1,47),GC_5,ZERO,ZERO,PL(0,116),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,21),4,COEFS,4,4,WL(1,0,1,116))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,116),2,4,23,1,1,147,H)
C     Coefficient construction for loop diagram with ID 110
      CALL FFV2_5L1_2(PL(0,42),W(1,64),-GC_21,GC_23,ZERO,ZERO,PL(0,117)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,117))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,117),2,4,26,1,1,148,H)
C     Coefficient construction for loop diagram with ID 111
      CALL FFV1L1_2(PL(0,42),W(1,66),GC_2,ZERO,ZERO,PL(0,118),COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,118))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,118),2,4,26,1,1,149,H)
C     Coefficient construction for loop diagram with ID 112
      CALL FFV2_5L1_2(PL(0,42),W(1,67),-GC_21,GC_23,ZERO,ZERO,PL(0,119)
     $ ,COEFS)
      CALL UPDATE_WL_1_1(WL(1,0,1,42),4,COEFS,4,4,WL(1,0,1,119))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,119),2,4,26,1,1,150,H)
C     Coefficient construction for loop diagram with ID 113
      CALL FFV1L2P0_3(PL(0,0),W(1,25),GC_5,ZERO,ZERO,PL(0,120),COEFS)
      CALL UPDATE_WL_0_0(WL(1,0,1,0),4,COEFS,4,4,WL(1,0,1,120))
      CALL FFV1L3_1(PL(0,120),W(1,68),GC_5,ZERO,ZERO,PL(0,121),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,121))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,121),1,4,28,1,1,151,H)
C     Coefficient construction for loop diagram with ID 114
      CALL FFV1L3_1(PL(0,120),W(1,69),GC_5,ZERO,ZERO,PL(0,122),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,122))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,122),1,4,28,1,1,152,H)
C     Coefficient construction for loop diagram with ID 115
      CALL FFV1L3_1(PL(0,120),W(1,70),GC_5,ZERO,ZERO,PL(0,123),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,123))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,123),1,4,28,1,1,153,H)
C     Coefficient construction for loop diagram with ID 116
      CALL FFV1L3_1(PL(0,120),W(1,71),GC_5,ZERO,ZERO,PL(0,124),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,124))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,124),1,4,28,1,1,154,H)
C     Coefficient construction for loop diagram with ID 117
      CALL FFV1L3_1(PL(0,120),W(1,72),GC_5,ZERO,ZERO,PL(0,125),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,125))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,125),1,4,28,1,1,155,H)
C     Coefficient construction for loop diagram with ID 118
      CALL FFV1L3_1(PL(0,120),W(1,73),GC_5,ZERO,ZERO,PL(0,126),COEFS)
      CALL UPDATE_WL_0_1(WL(1,0,1,120),4,COEFS,4,4,WL(1,0,1,126))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,126),1,4,28,1,1,156,H)
C     Coefficient construction for loop diagram with ID 119
      CALL FFV1L1P0_3(PL(0,46),W(1,68),GC_5,ZERO,ZERO,PL(0,127),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,127))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,127),2,4,29,1,1,157,H)
C     Coefficient construction for loop diagram with ID 120
      CALL FFV1L1P0_3(PL(0,46),W(1,69),GC_5,ZERO,ZERO,PL(0,128),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,128))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,128),2,4,29,1,1,158,H)
C     Coefficient construction for loop diagram with ID 121
      CALL FFV1L1P0_3(PL(0,46),W(1,70),GC_5,ZERO,ZERO,PL(0,129),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,129))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,129),2,4,29,1,1,159,H)
C     Coefficient construction for loop diagram with ID 122
      CALL FFV1L1P0_3(PL(0,46),W(1,71),GC_5,ZERO,ZERO,PL(0,130),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,130))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,130),2,4,29,1,1,160,H)
C     Coefficient construction for loop diagram with ID 123
      CALL FFV1L1P0_3(PL(0,46),W(1,72),GC_5,ZERO,ZERO,PL(0,131),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,131))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,131),2,4,29,1,1,161,H)
C     Coefficient construction for loop diagram with ID 124
      CALL FFV1L1P0_3(PL(0,46),W(1,73),GC_5,ZERO,ZERO,PL(0,132),COEFS)
      CALL UPDATE_WL_2_0(WL(1,0,1,46),4,COEFS,4,4,WL(1,0,1,132))
      CALL CREATE_LOOP_COEFS(WL(1,0,1,132),2,4,29,1,1,162,H)
C     At this point, all loop coefficients needed for (QCD=2), i.e. of
C      split order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 4000

      GOTO 1001
 4000 CONTINUE
      LOOP_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

