ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MP_COUP3()

      IMPLICIT NONE
      REAL*16 MP__PI
      PARAMETER (MP__PI=3.1415926535897932384626433832795E0_16)
      INCLUDE 'mp_input.inc'
      INCLUDE 'mp_coupl.inc'


      INCLUDE 'model_functions.inc'
      MP__UV_TMASS_1EPS = (MP_COND(CMPLX(MP__MDL_MT,KIND=16),CMPLX(0.00
     $ 0000E+00_16,KIND=16),CMPLX(MP__MDL_COMPLEXI*((MP__MDL_G__EXP__2
     $ )/(1.600000E+01_16*MP__PI**2))*3.000000E+00_16*MP__MDL_CF
     $ *MP__MDL_MT,KIND=16)))
      MP__UVWFCT_B_0_1EPS = MP_COND(CMPLX(MP__MDL_MB,KIND=16)
     $ ,CMPLX(0.000000E+00_16,KIND=16),CMPLX(-((MP__MDL_G__EXP__2)
     $ /(2.000000E+00_16*1.600000E+01_16*MP__PI**2))*3.000000E
     $ +00_16*MP__MDL_CF,KIND=16))
      MP__UVWFCT_G_2_1EPS = MP_COND(CMPLX(MP__MDL_MT,KIND=16)
     $ ,CMPLX(0.000000E+00_16,KIND=16),CMPLX(-((MP__MDL_G__EXP__2)
     $ /(2.000000E+00_16*4.800000E+01_16*MP__PI**2))*4.000000E
     $ +00_16*MP__MDL_TF,KIND=16))
      END
