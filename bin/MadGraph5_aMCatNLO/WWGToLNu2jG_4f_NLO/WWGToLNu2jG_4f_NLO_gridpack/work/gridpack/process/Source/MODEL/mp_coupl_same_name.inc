ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL*16 G
      COMMON/MP_STRONG/ G

      COMPLEX*32 GAL(2)
      COMMON/MP_WEAK/ GAL

      COMPLEX*32 MU_R
      COMMON/MP_RSCALE/ MU_R


      REAL*16 MDL_MW,MDL_MZ,MDL_MB,MDL_MTA,MDL_MH,MDL_MT

      COMMON/MP_MASSES/ MDL_MW,MDL_MZ,MDL_MB,MDL_MTA,MDL_MH,MDL_MT


      REAL*16 MDL_WT,MDL_WZ,MDL_WH,MDL_WW

      COMMON/MP_WIDTHS/ MDL_WT,MDL_WZ,MDL_WH,MDL_WW


      COMPLEX*32 GC_5,R2_QQQ,R2_DDA,R2_UUA,R2_DDZ_V2,R2_DDZ_V3,R2_BXTW
     $ ,GC_1,GC_2,GC_7,GC_11,GC_21,GC_23,GC_24,GC_25,GC_26,GC_27,GC_28

      COMMON/MP_COUPLINGS/ GC_5,R2_QQQ,R2_DDA,R2_UUA,R2_DDZ_V2
     $ ,R2_DDZ_V3,R2_BXTW,GC_1,GC_2,GC_7,GC_11,GC_21,GC_23,GC_24,GC_25
     $ ,GC_26,GC_27,GC_28


