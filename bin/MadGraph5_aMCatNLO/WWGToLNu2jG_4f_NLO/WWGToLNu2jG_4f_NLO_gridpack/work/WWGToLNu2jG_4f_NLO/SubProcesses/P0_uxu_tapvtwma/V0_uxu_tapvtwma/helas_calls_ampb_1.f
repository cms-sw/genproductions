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
      PARAMETER (NWAVEFUNCS=73,NLOOPWAVEFUNCS=133)
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
      CALL IXXXXX(P(0,3),MDL_MTA,NHEL(3),-1*IC(3),W(1,3))
      CALL OXXXXX(P(0,4),ZERO,NHEL(4),+1*IC(4),W(1,4))
      CALL VXXXXX(P(0,5),MDL_MW,NHEL(5),+1*IC(5),W(1,5))
      CALL VXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,6))
      CALL FFV1P0_3(W(1,2),W(1,1),GC_2,ZERO,ZERO,W(1,7))
      CALL FFV2_3(W(1,3),W(1,4),GC_11,MDL_MW,MDL_WW,W(1,8))
C     Amplitude(s) for born diagram with ID 1
      CALL VVVV2_0(W(1,7),W(1,6),W(1,5),W(1,8),GC_27,AMP(1))
      CALL VVV1_3(W(1,7),W(1,5),GC_25,MDL_MW,MDL_WW,W(1,9))
C     Amplitude(s) for born diagram with ID 2
      CALL VVV1_0(W(1,6),W(1,9),W(1,8),GC_25,AMP(2))
      CALL FFV2_5_3(W(1,2),W(1,1),-GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,10))
C     Amplitude(s) for born diagram with ID 3
      CALL VVVV5_0(W(1,6),W(1,5),W(1,8),W(1,10),GC_26,AMP(3))
      CALL VVV1_2(W(1,5),W(1,10),GC_7,MDL_MW,MDL_WW,W(1,11))
C     Amplitude(s) for born diagram with ID 4
      CALL VVV1_0(W(1,6),W(1,11),W(1,8),GC_25,AMP(4))
      CALL VVV1_3(W(1,6),W(1,5),GC_25,MDL_MW,MDL_WW,W(1,12))
C     Amplitude(s) for born diagram with ID 5
      CALL VVV1_0(W(1,7),W(1,12),W(1,8),GC_25,AMP(5))
C     Amplitude(s) for born diagram with ID 6
      CALL VVV1_0(W(1,12),W(1,8),W(1,10),GC_7,AMP(6))
      CALL FFV1_2(W(1,3),W(1,6),-GC_25,MDL_MTA,ZERO,W(1,13))
C     Amplitude(s) for born diagram with ID 7
      CALL FFV2_0(W(1,13),W(1,4),W(1,9),GC_11,AMP(7))
      CALL FFV2_1(W(1,4),W(1,10),GC_28,ZERO,ZERO,W(1,14))
C     Amplitude(s) for born diagram with ID 8
      CALL FFV2_0(W(1,13),W(1,14),W(1,5),GC_11,AMP(8))
C     Amplitude(s) for born diagram with ID 9
      CALL FFV2_0(W(1,13),W(1,4),W(1,11),GC_11,AMP(9))
      CALL FFV2_1(W(1,4),W(1,5),GC_11,MDL_MTA,ZERO,W(1,15))
C     Amplitude(s) for born diagram with ID 10
      CALL FFV1_0(W(1,13),W(1,15),W(1,7),-GC_25,AMP(10))
C     Amplitude(s) for born diagram with ID 11
      CALL FFV2_4_0(W(1,13),W(1,15),W(1,10),GC_21,GC_24,AMP(11))
      CALL FFV1_2(W(1,3),W(1,7),-GC_25,MDL_MTA,ZERO,W(1,16))
C     Amplitude(s) for born diagram with ID 12
      CALL FFV1_0(W(1,16),W(1,15),W(1,6),-GC_25,AMP(12))
      CALL FFV2_4_2(W(1,3),W(1,10),GC_21,GC_24,MDL_MTA,ZERO,W(1,17))
C     Amplitude(s) for born diagram with ID 13
      CALL FFV1_0(W(1,17),W(1,15),W(1,6),-GC_25,AMP(13))
C     Amplitude(s) for born diagram with ID 14
      CALL FFV2_0(W(1,16),W(1,4),W(1,12),GC_11,AMP(14))
C     Amplitude(s) for born diagram with ID 15
      CALL FFV2_0(W(1,17),W(1,4),W(1,12),GC_11,AMP(15))
C     Amplitude(s) for born diagram with ID 16
      CALL FFV2_0(W(1,3),W(1,14),W(1,12),GC_11,AMP(16))
      CALL FFV2_1(W(1,1),W(1,5),GC_11,ZERO,ZERO,W(1,18))
      CALL FFV1_2(W(1,2),W(1,6),GC_2,ZERO,ZERO,W(1,19))
C     Amplitude(s) for born diagram with ID 17
      CALL FFV2_0(W(1,19),W(1,18),W(1,8),GC_11,AMP(17))
      CALL FFV2_3(W(1,2),W(1,18),GC_11,MDL_MW,MDL_WW,W(1,20))
C     Amplitude(s) for born diagram with ID 18
      CALL VVV1_0(W(1,6),W(1,20),W(1,8),GC_25,AMP(18))
      CALL FFV1_1(W(1,18),W(1,6),GC_1,ZERO,ZERO,W(1,21))
C     Amplitude(s) for born diagram with ID 19
      CALL FFV2_0(W(1,2),W(1,21),W(1,8),GC_11,AMP(19))
C     Amplitude(s) for born diagram with ID 20
      CALL FFV2_0(W(1,13),W(1,4),W(1,20),GC_11,AMP(20))
      CALL FFV1_1(W(1,1),W(1,6),GC_2,ZERO,ZERO,W(1,22))
      CALL FFV1P0_3(W(1,2),W(1,22),GC_2,ZERO,ZERO,W(1,23))
C     Amplitude(s) for born diagram with ID 21
      CALL VVV1_0(W(1,23),W(1,5),W(1,8),GC_25,AMP(21))
      CALL FFV2_5_3(W(1,2),W(1,22),-GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,24))
C     Amplitude(s) for born diagram with ID 22
      CALL VVV1_0(W(1,5),W(1,8),W(1,24),GC_7,AMP(22))
      CALL FFV2_1(W(1,22),W(1,5),GC_11,ZERO,ZERO,W(1,25))
C     Amplitude(s) for born diagram with ID 23
      CALL FFV2_0(W(1,2),W(1,25),W(1,8),GC_11,AMP(23))
      CALL FFV2_2(W(1,3),W(1,5),GC_11,ZERO,ZERO,W(1,26))
C     Amplitude(s) for born diagram with ID 24
      CALL FFV2_0(W(1,26),W(1,4),W(1,24),GC_28,AMP(24))
C     Amplitude(s) for born diagram with ID 25
      CALL FFV1_0(W(1,3),W(1,15),W(1,23),-GC_25,AMP(25))
C     Amplitude(s) for born diagram with ID 26
      CALL FFV2_4_0(W(1,3),W(1,15),W(1,24),GC_21,GC_24,AMP(26))
      CALL FFV1P0_3(W(1,19),W(1,1),GC_2,ZERO,ZERO,W(1,27))
C     Amplitude(s) for born diagram with ID 27
      CALL VVV1_0(W(1,27),W(1,5),W(1,8),GC_25,AMP(27))
      CALL FFV2_5_3(W(1,19),W(1,1),-GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,28))
C     Amplitude(s) for born diagram with ID 28
      CALL VVV1_0(W(1,5),W(1,8),W(1,28),GC_7,AMP(28))
C     Amplitude(s) for born diagram with ID 29
      CALL FFV2_0(W(1,26),W(1,4),W(1,28),GC_28,AMP(29))
C     Amplitude(s) for born diagram with ID 30
      CALL FFV1_0(W(1,3),W(1,15),W(1,27),-GC_25,AMP(30))
C     Amplitude(s) for born diagram with ID 31
      CALL FFV2_4_0(W(1,3),W(1,15),W(1,28),GC_21,GC_24,AMP(31))
      CALL FFV2_1(W(1,1),W(1,12),GC_11,ZERO,ZERO,W(1,29))
C     Amplitude(s) for born diagram with ID 32
      CALL FFV2_0(W(1,2),W(1,29),W(1,8),GC_11,AMP(32))
      CALL FFV2_2(W(1,19),W(1,8),GC_11,ZERO,ZERO,W(1,30))
C     Counter-term amplitude(s) for loop diagram number 33
      CALL R2_QQ_1_0(W(1,30),W(1,18),R2_QQQ,AMPL(1,1))
C     Counter-term amplitude(s) for loop diagram number 34
      CALL FFV2_0(W(1,19),W(1,18),W(1,8),R2_BXTW,AMPL(1,2))
      CALL FFV2_2(W(1,2),W(1,8),GC_11,ZERO,ZERO,W(1,31))
C     Counter-term amplitude(s) for loop diagram number 35
      CALL R2_QQ_1_0(W(1,31),W(1,21),R2_QQQ,AMPL(1,3))
C     Counter-term amplitude(s) for loop diagram number 36
      CALL FFV2_0(W(1,2),W(1,21),W(1,8),R2_BXTW,AMPL(1,4))
      CALL FFV1_2(W(1,31),W(1,6),GC_1,ZERO,ZERO,W(1,32))
C     Counter-term amplitude(s) for loop diagram number 37
      CALL R2_QQ_1_0(W(1,32),W(1,18),R2_QQQ,AMPL(1,5))
      CALL VVV1_2(W(1,6),W(1,8),GC_25,MDL_MW,MDL_WW,W(1,33))
C     Counter-term amplitude(s) for loop diagram number 38
      CALL FFV2_0(W(1,2),W(1,18),W(1,33),R2_BXTW,AMPL(1,6))
      CALL FFV2_2(W(1,2),W(1,33),GC_11,ZERO,ZERO,W(1,34))
C     Counter-term amplitude(s) for loop diagram number 39
      CALL R2_QQ_1_0(W(1,34),W(1,18),R2_QQQ,AMPL(1,7))
C     Counter-term amplitude(s) for loop diagram number 41
      CALL FFV1_0(W(1,31),W(1,18),W(1,6),R2_DDA,AMPL(1,8))
      CALL FFV2_3(W(1,13),W(1,4),GC_11,MDL_MW,MDL_WW,W(1,35))
C     Counter-term amplitude(s) for loop diagram number 43
      CALL FFV2_0(W(1,2),W(1,18),W(1,35),R2_BXTW,AMPL(1,9))
      CALL FFV2_2(W(1,2),W(1,35),GC_11,ZERO,ZERO,W(1,36))
C     Counter-term amplitude(s) for loop diagram number 44
      CALL R2_QQ_1_0(W(1,36),W(1,18),R2_QQQ,AMPL(1,10))
C     Counter-term amplitude(s) for loop diagram number 45
      CALL R2_QQ_1_0(W(1,31),W(1,25),R2_QQQ,AMPL(1,11))
C     Counter-term amplitude(s) for loop diagram number 46
      CALL FFV2_0(W(1,2),W(1,25),W(1,8),R2_BXTW,AMPL(1,12))
C     Counter-term amplitude(s) for loop diagram number 47
      CALL FFV2_0(W(1,31),W(1,22),W(1,5),R2_BXTW,AMPL(1,13))
C     Counter-term amplitude(s) for loop diagram number 49
      CALL FFV2_0(W(1,30),W(1,1),W(1,5),R2_BXTW,AMPL(1,14))
C     Counter-term amplitude(s) for loop diagram number 51
      CALL R2_QQ_1_0(W(1,31),W(1,29),R2_QQQ,AMPL(1,15))
C     Counter-term amplitude(s) for loop diagram number 52
      CALL FFV2_0(W(1,2),W(1,29),W(1,8),R2_BXTW,AMPL(1,16))
C     Counter-term amplitude(s) for loop diagram number 53
      CALL FFV2_0(W(1,31),W(1,1),W(1,12),R2_BXTW,AMPL(1,17))
C     Counter-term amplitude(s) for loop diagram number 55
      CALL FFV2_0(W(1,32),W(1,1),W(1,5),R2_BXTW,AMPL(1,18))
C     Counter-term amplitude(s) for loop diagram number 57
      CALL FFV2_0(W(1,34),W(1,1),W(1,5),R2_BXTW,AMPL(1,19))
C     Counter-term amplitude(s) for loop diagram number 64
      CALL FFV2_0(W(1,36),W(1,1),W(1,5),R2_BXTW,AMPL(1,20))
      CALL FFV2_1(W(1,18),W(1,8),GC_11,ZERO,ZERO,W(1,37))
C     Counter-term amplitude(s) for loop diagram number 65
      CALL R2_QQ_1_0(W(1,19),W(1,37),R2_QQQ,AMPL(1,21))
C     Counter-term amplitude(s) for loop diagram number 66
      CALL FFV1_0(W(1,2),W(1,37),W(1,6),R2_UUA,AMPL(1,22))
      CALL VVV1P0_1(W(1,5),W(1,8),GC_25,ZERO,ZERO,W(1,38))
C     Counter-term amplitude(s) for loop diagram number 67
      CALL FFV1_0(W(1,2),W(1,22),W(1,38),R2_UUA,AMPL(1,23))
      CALL VVV1_3(W(1,5),W(1,8),GC_7,MDL_MZ,MDL_WZ,W(1,39))
C     Counter-term amplitude(s) for loop diagram number 68
      CALL FFV2_5_0(W(1,2),W(1,22),W(1,39),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,24))
      CALL FFV2_3(W(1,26),W(1,4),GC_28,MDL_MZ,MDL_WZ,W(1,40))
C     Counter-term amplitude(s) for loop diagram number 69
      CALL FFV2_5_0(W(1,2),W(1,22),W(1,40),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,25))
      CALL FFV1P0_3(W(1,3),W(1,15),-GC_25,ZERO,ZERO,W(1,41))
C     Counter-term amplitude(s) for loop diagram number 70
      CALL FFV1_0(W(1,2),W(1,22),W(1,41),R2_UUA,AMPL(1,26))
      CALL FFV2_4_3(W(1,3),W(1,15),GC_21,GC_24,MDL_MZ,MDL_WZ,W(1,42))
C     Counter-term amplitude(s) for loop diagram number 71
      CALL FFV2_5_0(W(1,2),W(1,22),W(1,42),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,27))
      CALL FFV2_2(W(1,31),W(1,5),GC_11,ZERO,ZERO,W(1,43))
C     Counter-term amplitude(s) for loop diagram number 72
      CALL R2_QQ_1_0(W(1,43),W(1,22),R2_QQQ,AMPL(1,28))
      CALL FFV1_2(W(1,2),W(1,38),GC_2,ZERO,ZERO,W(1,44))
C     Counter-term amplitude(s) for loop diagram number 73
      CALL R2_QQ_1_0(W(1,44),W(1,22),R2_QQQ,AMPL(1,29))
      CALL FFV2_5_2(W(1,2),W(1,39),-GC_21,GC_23,ZERO,ZERO,W(1,45))
C     Counter-term amplitude(s) for loop diagram number 74
      CALL R2_QQ_1_0(W(1,45),W(1,22),R2_QQQ,AMPL(1,30))
      CALL FFV2_5_2(W(1,2),W(1,40),-GC_21,GC_23,ZERO,ZERO,W(1,46))
C     Counter-term amplitude(s) for loop diagram number 75
      CALL R2_QQ_1_0(W(1,46),W(1,22),R2_QQQ,AMPL(1,31))
      CALL FFV1_2(W(1,2),W(1,41),GC_2,ZERO,ZERO,W(1,47))
C     Counter-term amplitude(s) for loop diagram number 76
      CALL R2_QQ_1_0(W(1,47),W(1,22),R2_QQQ,AMPL(1,32))
      CALL FFV2_5_2(W(1,2),W(1,42),-GC_21,GC_23,ZERO,ZERO,W(1,48))
C     Counter-term amplitude(s) for loop diagram number 77
      CALL R2_QQ_1_0(W(1,48),W(1,22),R2_QQQ,AMPL(1,33))
C     Counter-term amplitude(s) for loop diagram number 78
      CALL FFV1_0(W(1,19),W(1,1),W(1,38),R2_UUA,AMPL(1,34))
C     Counter-term amplitude(s) for loop diagram number 79
      CALL FFV2_5_0(W(1,19),W(1,1),W(1,39),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,35))
C     Counter-term amplitude(s) for loop diagram number 80
      CALL FFV2_5_0(W(1,19),W(1,1),W(1,40),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,36))
C     Counter-term amplitude(s) for loop diagram number 81
      CALL FFV1_0(W(1,19),W(1,1),W(1,41),R2_UUA,AMPL(1,37))
C     Counter-term amplitude(s) for loop diagram number 82
      CALL FFV2_5_0(W(1,19),W(1,1),W(1,42),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,38))
      CALL VVV1P0_1(W(1,5),W(1,33),GC_25,ZERO,ZERO,W(1,49))
C     Counter-term amplitude(s) for loop diagram number 85
      CALL FFV1_0(W(1,2),W(1,1),W(1,49),R2_UUA,AMPL(1,39))
      CALL VVV1_3(W(1,5),W(1,33),GC_7,MDL_MZ,MDL_WZ,W(1,50))
C     Counter-term amplitude(s) for loop diagram number 86
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,50),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,40))
      CALL VVVV2P0_1(W(1,6),W(1,5),W(1,8),GC_27,ZERO,ZERO,W(1,51))
C     Counter-term amplitude(s) for loop diagram number 87
      CALL FFV1_0(W(1,2),W(1,1),W(1,51),R2_UUA,AMPL(1,41))
      CALL VVVV5_4(W(1,6),W(1,5),W(1,8),GC_26,MDL_MZ,MDL_WZ,W(1,52))
C     Counter-term amplitude(s) for loop diagram number 88
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,52),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,42))
      CALL VVV1P0_1(W(1,12),W(1,8),GC_25,ZERO,ZERO,W(1,53))
C     Counter-term amplitude(s) for loop diagram number 89
      CALL FFV1_0(W(1,2),W(1,1),W(1,53),R2_UUA,AMPL(1,43))
      CALL VVV1_3(W(1,12),W(1,8),GC_7,MDL_MZ,MDL_WZ,W(1,54))
C     Counter-term amplitude(s) for loop diagram number 90
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,54),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,44))
C     Counter-term amplitude(s) for loop diagram number 93
      CALL FFV1_0(W(1,43),W(1,1),W(1,6),R2_UUA,AMPL(1,45))
C     Counter-term amplitude(s) for loop diagram number 94
      CALL FFV1_0(W(1,44),W(1,1),W(1,6),R2_UUA,AMPL(1,46))
C     Counter-term amplitude(s) for loop diagram number 95
      CALL FFV1_0(W(1,45),W(1,1),W(1,6),R2_UUA,AMPL(1,47))
C     Counter-term amplitude(s) for loop diagram number 98
      CALL FFV1_0(W(1,46),W(1,1),W(1,6),R2_UUA,AMPL(1,48))
      CALL VVV1P0_1(W(1,5),W(1,35),GC_25,ZERO,ZERO,W(1,55))
C     Counter-term amplitude(s) for loop diagram number 99
      CALL FFV1_0(W(1,2),W(1,1),W(1,55),R2_UUA,AMPL(1,49))
      CALL VVV1_3(W(1,5),W(1,35),GC_7,MDL_MZ,MDL_WZ,W(1,56))
C     Counter-term amplitude(s) for loop diagram number 100
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,56),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,50))
      CALL FFV2_2(W(1,13),W(1,5),GC_11,ZERO,ZERO,W(1,57))
      CALL FFV2_3(W(1,57),W(1,4),GC_28,MDL_MZ,MDL_WZ,W(1,58))
C     Counter-term amplitude(s) for loop diagram number 101
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,58),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,51))
      CALL FFV1P0_3(W(1,13),W(1,15),-GC_25,ZERO,ZERO,W(1,59))
C     Counter-term amplitude(s) for loop diagram number 102
      CALL FFV1_0(W(1,2),W(1,1),W(1,59),R2_UUA,AMPL(1,52))
      CALL FFV2_4_3(W(1,13),W(1,15),GC_21,GC_24,MDL_MZ,MDL_WZ,W(1,60))
C     Counter-term amplitude(s) for loop diagram number 103
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,60),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,53))
      CALL FFV1_1(W(1,15),W(1,6),-GC_25,MDL_MTA,ZERO,W(1,61))
      CALL FFV1P0_3(W(1,3),W(1,61),-GC_25,ZERO,ZERO,W(1,62))
C     Counter-term amplitude(s) for loop diagram number 106
      CALL FFV1_0(W(1,2),W(1,1),W(1,62),R2_UUA,AMPL(1,54))
      CALL FFV2_4_3(W(1,3),W(1,61),GC_21,GC_24,MDL_MZ,MDL_WZ,W(1,63))
C     Counter-term amplitude(s) for loop diagram number 107
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,63),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,55))
C     Counter-term amplitude(s) for loop diagram number 110
      CALL FFV1_0(W(1,47),W(1,1),W(1,6),R2_UUA,AMPL(1,56))
C     Counter-term amplitude(s) for loop diagram number 111
      CALL FFV1_0(W(1,48),W(1,1),W(1,6),R2_UUA,AMPL(1,57))
      CALL FFV2_2(W(1,3),W(1,12),GC_11,ZERO,ZERO,W(1,64))
      CALL FFV2_3(W(1,64),W(1,4),GC_28,MDL_MZ,MDL_WZ,W(1,65))
C     Counter-term amplitude(s) for loop diagram number 112
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,65),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,58))
      CALL FFV2_1(W(1,4),W(1,12),GC_11,MDL_MTA,ZERO,W(1,66))
      CALL FFV1P0_3(W(1,3),W(1,66),-GC_25,ZERO,ZERO,W(1,67))
C     Counter-term amplitude(s) for loop diagram number 113
      CALL FFV1_0(W(1,2),W(1,1),W(1,67),R2_UUA,AMPL(1,59))
      CALL FFV2_4_3(W(1,3),W(1,66),GC_21,GC_24,MDL_MZ,MDL_WZ,W(1,68))
C     Counter-term amplitude(s) for loop diagram number 114
      CALL FFV2_5_0(W(1,2),W(1,1),W(1,68),-R2_DDZ_V2,R2_DDZ_V3,AMPL(1
     $ ,60))
      CALL FFV1_1(W(1,1),W(1,38),GC_2,ZERO,ZERO,W(1,69))
C     Counter-term amplitude(s) for loop diagram number 115
      CALL R2_QQ_1_0(W(1,19),W(1,69),R2_QQQ,AMPL(1,61))
      CALL FFV2_5_1(W(1,1),W(1,39),-GC_21,GC_23,ZERO,ZERO,W(1,70))
C     Counter-term amplitude(s) for loop diagram number 116
      CALL R2_QQ_1_0(W(1,19),W(1,70),R2_QQQ,AMPL(1,62))
      CALL FFV2_5_1(W(1,1),W(1,40),-GC_21,GC_23,ZERO,ZERO,W(1,71))
C     Counter-term amplitude(s) for loop diagram number 117
      CALL R2_QQ_1_0(W(1,19),W(1,71),R2_QQQ,AMPL(1,63))
      CALL FFV1_1(W(1,1),W(1,41),GC_2,ZERO,ZERO,W(1,72))
C     Counter-term amplitude(s) for loop diagram number 118
      CALL R2_QQ_1_0(W(1,19),W(1,72),R2_QQQ,AMPL(1,64))
      CALL FFV2_5_1(W(1,1),W(1,42),-GC_21,GC_23,ZERO,ZERO,W(1,73))
C     Counter-term amplitude(s) for loop diagram number 119
      CALL R2_QQ_1_0(W(1,19),W(1,73),R2_QQQ,AMPL(1,65))
C     Counter-term amplitude(s) for loop diagram number 120
      CALL FFV1_0(W(1,2),W(1,69),W(1,6),R2_UUA,AMPL(1,66))
C     Counter-term amplitude(s) for loop diagram number 121
      CALL FFV1_0(W(1,2),W(1,70),W(1,6),R2_UUA,AMPL(1,67))
C     Counter-term amplitude(s) for loop diagram number 122
      CALL FFV1_0(W(1,2),W(1,71),W(1,6),R2_UUA,AMPL(1,68))
C     Counter-term amplitude(s) for loop diagram number 123
      CALL FFV1_0(W(1,2),W(1,72),W(1,6),R2_UUA,AMPL(1,69))
C     Counter-term amplitude(s) for loop diagram number 124
      CALL FFV1_0(W(1,2),W(1,73),W(1,6),R2_UUA,AMPL(1,70))
C     At this point, all CT amps needed for (QCD=2), i.e. of split
C      order ID=1, are computed.
      IF(FILTER_SO.AND.SQSO_TARGET.EQ.1) GOTO 2000

      GOTO 1001
 2000 CONTINUE
      CT_REQ_SO_DONE=.TRUE.
 1001 CONTINUE
      END

