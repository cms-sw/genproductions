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
      GC_74 = (MDL_CLL1X1*MDL_EE*MDL_COMPLEXI*MDL_KL)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_83 = (MDL_CLQ1X1*MDL_EE*MDL_COMPLEXI*MDL_KL)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_84 = (MDL_CLQ1X2*MDL_EE*MDL_COMPLEXI*MDL_KL)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_86 = (MDL_CLQ2X1*MDL_EE*MDL_COMPLEXI*MDL_KL)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_87 = (MDL_CLQ2X2*MDL_EE*MDL_COMPLEXI*MDL_KL)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_92 = (MDL_CRL1X1*MDL_EE*MDL_COMPLEXI*MDL_KR)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_101 = (MDL_CRQ1X1*MDL_EE*MDL_COMPLEXI*MDL_KR)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_102 = (MDL_CRQ1X2*MDL_EE*MDL_COMPLEXI*MDL_KR)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_104 = (MDL_CRQ2X1*MDL_EE*MDL_COMPLEXI*MDL_KR)/(MDL_SW
     $ *MDL_SQRT__2)
      GC_105 = (MDL_CRQ2X2*MDL_EE*MDL_COMPLEXI*MDL_KR)/(MDL_SW
     $ *MDL_SQRT__2)
      END
