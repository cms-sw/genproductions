ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL*16 G
      COMMON/MP_STRONG/ G

      COMPLEX*32 GAL(2)
      COMMON/MP_WEAK/ GAL

      COMPLEX*32 MU_R
      COMMON/MP_RSCALE/ MU_R


      REAL*16 MDL_MT,MDL_MH,MDL_MTA,MDL_MW,MDL_MZ

      COMMON/MP_MASSES/ MDL_MT,MDL_MH,MDL_MTA,MDL_MW,MDL_MZ


      REAL*16 MDL_WZ,MDL_WW,MDL_WT,MDL_WH

      COMMON/MP_WIDTHS/ MDL_WZ,MDL_WW,MDL_WT,MDL_WH


      COMPLEX*32 GC_5,R2_BXUW,R2_BXCW,R2_CXDW,R2_CXSW,R2_UXBW,GC_11
     $ ,GC_12,GC_13,GC_14,GC_17,GC_41

      COMMON/MP_COUPLINGS/ GC_5,R2_BXUW,R2_BXCW,R2_CXDW,R2_CXSW
     $ ,R2_UXBW,GC_11,GC_12,GC_13,GC_14,GC_17,GC_41


