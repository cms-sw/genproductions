ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE COUP1()

      IMPLICIT NONE
      INCLUDE 'model_functions.inc'

      DOUBLE PRECISION PI, ZERO
      PARAMETER  (PI=3.141592653589793D0)
      PARAMETER  (ZERO=0D0)
      INCLUDE 'input.inc'
      INCLUDE 'coupl.inc'
      REAL*16 MP__PI, MP__ZERO
      PARAMETER (MP__PI=3.1415926535897932384626433832795E0_16)
      PARAMETER (MP__ZERO=0E0_16)
      INCLUDE 'mp_input.inc'
      INCLUDE 'mp_coupl.inc'

      GC_1 = -(MDL_EE*MDL_COMPLEXI)/3.000000D+00
      MP__GC_1 = -(MP__MDL_EE*MP__MDL_COMPLEXI)/3.000000E+00_16
      GC_2 = (2.000000D+00*MDL_EE*MDL_COMPLEXI)/3.000000D+00
      MP__GC_2 = (2.000000E+00_16*MP__MDL_EE*MP__MDL_COMPLEXI)
     $ /3.000000E+00_16
      GC_7 = MDL_CW*MDL_COMPLEXI*MDL_GW
      MP__GC_7 = MP__MDL_CW*MP__MDL_COMPLEXI*MP__MDL_GW
      GC_11 = (MDL_EE*MDL_COMPLEXI)/(MDL_SW*MDL_SQRT__2)
      MP__GC_11 = (MP__MDL_EE*MP__MDL_COMPLEXI)/(MP__MDL_SW
     $ *MP__MDL_SQRT__2)
      GC_21 = -(MDL_CW*MDL_EE*MDL_COMPLEXI)/(2.000000D+00*MDL_SW)
      MP__GC_21 = -(MP__MDL_CW*MP__MDL_EE*MP__MDL_COMPLEXI)/(2.000000E
     $ +00_16*MP__MDL_SW)
      GC_23 = -(MDL_EE*MDL_COMPLEXI*MDL_SW)/(6.000000D+00*MDL_CW)
      MP__GC_23 = -(MP__MDL_EE*MP__MDL_COMPLEXI*MP__MDL_SW)/(6.000000E
     $ +00_16*MP__MDL_CW)
      GC_24 = (MDL_EE*MDL_COMPLEXI*MDL_SW)/(2.000000D+00*MDL_CW)
      MP__GC_24 = (MP__MDL_EE*MP__MDL_COMPLEXI*MP__MDL_SW)/(2.000000E
     $ +00_16*MP__MDL_CW)
      GC_25 = MDL_COMPLEXI*MDL_GW*MDL_SW
      MP__GC_25 = MP__MDL_COMPLEXI*MP__MDL_GW*MP__MDL_SW
      GC_26 = -2.000000D+00*MDL_CW*MDL_COMPLEXI*MDL_GW__EXP__2*MDL_SW
      MP__GC_26 = -2.000000E+00_16*MP__MDL_CW*MP__MDL_COMPLEXI
     $ *MP__MDL_GW__EXP__2*MP__MDL_SW
      GC_27 = MDL_COMPLEXI*MDL_GW__EXP__2*MDL_SW__EXP__2
      MP__GC_27 = MP__MDL_COMPLEXI*MP__MDL_GW__EXP__2
     $ *MP__MDL_SW__EXP__2
      GC_28 = (MDL_CW*MDL_EE*MDL_COMPLEXI)/(2.000000D+00*MDL_SW)
     $ +(MDL_EE*MDL_COMPLEXI*MDL_SW)/(2.000000D+00*MDL_CW)
      MP__GC_28 = (MP__MDL_CW*MP__MDL_EE*MP__MDL_COMPLEXI)/(2.000000E
     $ +00_16*MP__MDL_SW)+(MP__MDL_EE*MP__MDL_COMPLEXI*MP__MDL_SW)
     $ /(2.000000E+00_16*MP__MDL_CW)
      END
