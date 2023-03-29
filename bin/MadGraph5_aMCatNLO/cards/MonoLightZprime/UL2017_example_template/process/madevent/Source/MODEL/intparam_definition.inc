ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

C     Parameters that should not be recomputed event by event.
C     
      IF(READLHA) THEN

        G = 2 * DSQRT(AS*PI)  ! for the first init

        MDL_YE = 0.000000D+00

        MDL_YM = 0.000000D+00

        MDL_YU = 0.000000D+00

        MDL_YD = 0.000000D+00

        MDL_YS = 0.000000D+00

        MDL_COS__CABI = COS(MDL_CABI)

        MDL_CKM1X1 = MDL_COS__CABI

        MDL_SIN__CABI = SIN(MDL_CABI)

        MDL_CKM1X2 = MDL_SIN__CABI

        MDL_CKM1X3 = 0.000000D+00

        MDL_CKM2X1 = -MDL_SIN__CABI

        MDL_CKM2X2 = MDL_COS__CABI

        MDL_CKM2X3 = 0.000000D+00

        MDL_CKM3X1 = 0.000000D+00

        MDL_CKM3X2 = 0.000000D+00

        MDL_CKM3X3 = 1.000000D+00

        MDL_MZ__EXP__2 = MDL_MZ**2

        MDL_MZ__EXP__4 = MDL_MZ**4

        MDL_SQRT__2 = SQRT(DCMPLX(2.000000D+00))

        MDL_MH__EXP__2 = MDL_MH**2

        MDL_COMPLEXI = DCMPLX(0.000000D+00,1.000000D+00)

        MDL_CONJG__CKM1X1 = CONJG(DCMPLX(MDL_CKM1X1))

        MDL_CONJG__CKM1X2 = CONJG(DCMPLX(MDL_CKM1X2))

        MDL_CONJG__CKM1X3 = CONJG(DCMPLX(MDL_CKM1X3))

        MDL_CONJG__CKM2X1 = CONJG(DCMPLX(MDL_CKM2X1))

        MDL_CONJG__CKM2X2 = CONJG(DCMPLX(MDL_CKM2X2))

        MDL_CONJG__CKM2X3 = CONJG(DCMPLX(MDL_CKM2X3))

        MDL_CONJG__CKM3X1 = CONJG(DCMPLX(MDL_CKM3X1))

        MDL_CONJG__CKM3X2 = CONJG(DCMPLX(MDL_CKM3X2))

        MDL_CONJG__CKM3X3 = CONJG(DCMPLX(MDL_CKM3X3))

        MDL_AEW = 1.000000D+00/AEWM1

        MDL_MW = SQRT(DCMPLX(MDL_MZ__EXP__2/2.000000D+00
     $   +SQRT(DCMPLX(MDL_MZ__EXP__4/4.000000D+00-(MDL_AEW*PI
     $   *MDL_MZ__EXP__2)/(MDL_GF*MDL_SQRT__2)))))

        MDL_SQRT__AEW = SQRT(DCMPLX(MDL_AEW))

        MDL_EE = 2.000000D+00*MDL_SQRT__AEW*SQRT(DCMPLX(PI))

        MDL_MW__EXP__2 = MDL_MW**2

        MDL_SW2 = 1.000000D+00-MDL_MW__EXP__2/MDL_MZ__EXP__2

        MDL_CW = SQRT(DCMPLX(1.000000D+00-MDL_SW2))

        MDL_SQRT__SW2 = SQRT(DCMPLX(MDL_SW2))

        MDL_SW = MDL_SQRT__SW2

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

