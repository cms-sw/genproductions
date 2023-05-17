ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE COUP2()

      IMPLICIT NONE
      INCLUDE 'model_functions.inc'

      DOUBLE PRECISION PI, ZERO
      PARAMETER  (PI=3.141592653589793D0)
      PARAMETER  (ZERO=0D0)
      INCLUDE 'input.inc'
      INCLUDE 'coupl.inc'
      GC_5 = MDL_COMPLEXI*G
      R2_QQQ = MDL_LHV*MDL_COMPLEXI*MDL_G__EXP__2*(MDL_NCOL__EXP__2
     $ -1.000000D+00)/(3.200000D+01*PI**2*MDL_NCOL)
      R2_DDA = (-(MDL_EE*MDL_COMPLEXI)/3.000000D+00)
     $ *MDL_R2MIXEDFACTOR_FIN_
      R2_UUA = (2.000000D+00*(MDL_EE*MDL_COMPLEXI)/3.000000D+00)
     $ *MDL_R2MIXEDFACTOR_FIN_
      R2_DDZ_V2 = (-(MDL_CW*MDL_EE*MDL_COMPLEXI)/(2.000000D+00*MDL_SW))
     $ *MDL_R2MIXEDFACTOR_FIN_
      R2_DDZ_V3 = (-(MDL_EE*MDL_COMPLEXI*MDL_SW)/(6.000000D+00*MDL_CW))
     $ *MDL_R2MIXEDFACTOR_FIN_
      R2_BXTW = ((MDL_CKM33*MDL_EE*MDL_COMPLEXI)/(MDL_SW*MDL_SQRT__2))
     $ *MDL_R2MIXEDFACTOR_FIN_
      END
