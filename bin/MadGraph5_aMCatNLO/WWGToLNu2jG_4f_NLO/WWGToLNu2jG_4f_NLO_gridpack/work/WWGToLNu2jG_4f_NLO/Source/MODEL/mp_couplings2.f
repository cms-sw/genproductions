ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE MP_COUP2()

      IMPLICIT NONE
      INCLUDE 'model_functions.inc'
      REAL*16 MP__PI, MP__ZERO
      PARAMETER (MP__PI=3.1415926535897932384626433832795E0_16)
      PARAMETER (MP__ZERO=0E0_16)
      INCLUDE 'mp_input.inc'
      INCLUDE 'mp_coupl.inc'

      MP__GC_5 = MP__MDL_COMPLEXI*MP__G
      MP__R2_QQQ = MP__MDL_LHV*MP__MDL_COMPLEXI*MP__MDL_G__EXP__2
     $ *(MP__MDL_NCOL__EXP__2-1.000000E+00_16)/(3.200000E+01_16*MP__PI
     $ **2*MP__MDL_NCOL)
      MP__R2_DDA = (-(MP__MDL_EE*MP__MDL_COMPLEXI)/3.000000E+00_16)
     $ *MP__MDL_R2MIXEDFACTOR_FIN_
      MP__R2_UUA = (2.000000E+00_16*(MP__MDL_EE*MP__MDL_COMPLEXI)
     $ /3.000000E+00_16)*MP__MDL_R2MIXEDFACTOR_FIN_
      MP__R2_DDZ_V2 = (-(MP__MDL_CW*MP__MDL_EE*MP__MDL_COMPLEXI)
     $ /(2.000000E+00_16*MP__MDL_SW))*MP__MDL_R2MIXEDFACTOR_FIN_
      MP__R2_DDZ_V3 = (-(MP__MDL_EE*MP__MDL_COMPLEXI*MP__MDL_SW)
     $ /(6.000000E+00_16*MP__MDL_CW))*MP__MDL_R2MIXEDFACTOR_FIN_
      MP__R2_BXTW = ((MP__MDL_CKM33*MP__MDL_EE*MP__MDL_COMPLEXI)
     $ /(MP__MDL_SW*MP__MDL_SQRT__2))*MP__MDL_R2MIXEDFACTOR_FIN_
      END
