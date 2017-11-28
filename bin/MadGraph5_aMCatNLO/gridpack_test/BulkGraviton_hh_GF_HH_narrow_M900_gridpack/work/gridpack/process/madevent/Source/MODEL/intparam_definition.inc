ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C     Parameters that should not be recomputed event by event.
C     
      IF(READLHA) THEN

        G = 2 * DSQRT(AS*PI)  ! for the first init

        MDL_CW = SQRT(DCMPLX(1.000000D+00-MDL_SW2))

        MDL_SQRT__SW2 = SQRT(DCMPLX(MDL_SW2))

        MDL_SW = MDL_SQRT__SW2

        MDL_YE = 0.000000D+00

        MDL_YM = 0.000000D+00

        MDL_YU = 0.000000D+00

        MDL_CU = MDL_CLIGHT

        MDL_CC = MDL_CLIGHT

        MDL_CF3 = MDL_CTOP

        MDL_YD = 0.000000D+00

        MDL_YS = 0.000000D+00

        MDL_CKM11 = 1.000000D+00

        MDL_CKM12 = 0.000000D+00

        MDL_CKM13 = 0.000000D+00

        MDL_CKM21 = 0.000000D+00

        MDL_CKM22 = 1.000000D+00

        MDL_CKM23 = 0.000000D+00

        MDL_CKM31 = 0.000000D+00

        MDL_CKM32 = 0.000000D+00

        MDL_CKM33 = 1.000000D+00

        MDL_KAPPA = (3.830000D+00*MDL_KTILDA)/MDL_MGR

        MDL_MH__EXP__2 = MDL_MH**2

        MDL_SQRT__2 = SQRT(DCMPLX(2.000000D+00))

        MDL_MH__EXP__4 = MDL_MH**4

        MDL_MT__EXP__4 = MDL_MT**4

        MDL_MT__EXP__2 = MDL_MT**2

        MDL_MH__EXP__12 = MDL_MH**12

        MDL_MW__EXP__12 = MDL_MW**12

        MDL_MH__EXP__10 = MDL_MH**10

        MDL_MW__EXP__10 = MDL_MW**10

        MDL_MH__EXP__8 = MDL_MH**8

        MDL_MW__EXP__8 = MDL_MW**8

        MDL_MH__EXP__6 = MDL_MH**6

        MDL_MW__EXP__6 = MDL_MW**6

        MDL_MW__EXP__4 = MDL_MW**4

        MDL_MW__EXP__2 = MDL_MW**2

        MDL_COMPLEXI = DCMPLX(0.000000D+00,1.000000D+00)

        MDL_CW__EXP__2 = MDL_CW**2

        MDL_SW__EXP__2 = MDL_SW**2

        MDL_CONJG__CKM11 = CONJG(DCMPLX(MDL_CKM11))

        MDL_CONJG__CKM12 = CONJG(DCMPLX(MDL_CKM12))

        MDL_CONJG__CKM13 = CONJG(DCMPLX(MDL_CKM13))

        MDL_CONJG__CKM21 = CONJG(DCMPLX(MDL_CKM21))

        MDL_CONJG__CKM22 = CONJG(DCMPLX(MDL_CKM22))

        MDL_CONJG__CKM23 = CONJG(DCMPLX(MDL_CKM23))

        MDL_CONJG__CKM31 = CONJG(DCMPLX(MDL_CKM31))

        MDL_CONJG__CKM32 = CONJG(DCMPLX(MDL_CKM32))

        MDL_CONJG__CKM33 = CONJG(DCMPLX(MDL_CKM33))

        MDL_AEW = 1.000000D+00/AEWM1

        MDL_SQRT__AEW = SQRT(DCMPLX(MDL_AEW))

        MDL_EE = 2.000000D+00*MDL_SQRT__AEW*SQRT(DCMPLX(PI))

        MDL_G1 = MDL_EE/MDL_CW

        MDL_GW = MDL_EE/MDL_SW

        MDL_V = (2.000000D+00*MDL_MW*MDL_SW)/MDL_EE

        MDL_V__EXP__2 = MDL_V**2

        MDL_LAM = MDL_MH__EXP__2/(2.000000D+00*MDL_V__EXP__2)

        MDL_YB = (MDL_YMB*MDL_SQRT__2)/MDL_V

        MDL_YC = (MDL_YMC*MDL_SQRT__2)/MDL_V

        MDL_YT = (MDL_YMT*MDL_SQRT__2)/MDL_V

        MDL_YTAU = (MDL_YMTAU*MDL_SQRT__2)/MDL_V

        MDL_MUH = SQRT(DCMPLX(MDL_LAM*MDL_V__EXP__2))

        MDL_EE__EXP__2 = MDL_EE**2

        MDL_AH = (4.700000D+01*MDL_EE__EXP__2*(1.000000D+00-(2.000000D
     $   +00*MDL_MH__EXP__4)/(9.870000D+02*MDL_MT__EXP__4)-(1.400000D
     $   +01*MDL_MH__EXP__2)/(7.050000D+02*MDL_MT__EXP__2)+(2.130000D
     $   +02*MDL_MH__EXP__12)/(2.634632D+07*MDL_MW__EXP__12)
     $   +(5.000000D+00*MDL_MH__EXP__10)/(1.197560D+05*MDL_MW__EXP__10)
     $   +(4.100000D+01*MDL_MH__EXP__8)/(1.809500D+05*MDL_MW__EXP__8)
     $   +(8.700000D+01*MDL_MH__EXP__6)/(6.580000D+04*MDL_MW__EXP__6)
     $   +(5.700000D+01*MDL_MH__EXP__4)/(6.580000D+03*MDL_MW__EXP__4)
     $   +(3.300000D+01*MDL_MH__EXP__2)/(4.700000D+02*MDL_MW__EXP__2)))
     $   /(7.200000D+01*PI**2*MDL_V)

        MDL_GW__EXP__2 = MDL_GW**2

        MDL_MUH__EXP__2 = MDL_MUH**2

      ENDIF
C     
C     Parameters that should be recomputed at an event by even basis.
C     
      AS = G**2/4/PI

      MDL_SQRT__AS = SQRT(DCMPLX(AS))

      MDL_G__EXP__2 = G**2

C     
C     Definition of the EW coupling used in the write out of aqed
C     
      GAL(1) = 3.5449077018110318D0 / DSQRT(AEWM1)
      GAL(2) = 1D0

