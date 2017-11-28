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
      GC_53 = MDL_COMPLEXI*MDL_KAPPA*MDL_MUH__EXP__2-3.000000D+00
     $ *MDL_COMPLEXI*MDL_KAPPA*MDL_LAM*MDL_V__EXP__2
      GC_10 = MDL_COMPLEXI*MDL_KAPPA
      GC_11 = -(MDL_CG*MDL_COMPLEXI*MDL_KAPPA)
      END
