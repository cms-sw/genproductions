ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL*16 MP__G
      COMMON/MP_STRONG/ MP__G

      COMPLEX*32 MP__GAL(2)
      COMMON/MP_WEAK/ MP__GAL

      COMPLEX*32 MP__MU_R
      COMMON/MP_RSCALE/ MP__MU_R


      REAL*16 MP__MDL_MT,MP__MDL_MH,MP__MDL_MTA,MP__MDL_MW,MP__MDL_MZ

      COMMON/MP_MASSES/ MP__MDL_MT,MP__MDL_MH,MP__MDL_MTA,MP__MDL_MW
     $ ,MP__MDL_MZ


      REAL*16 MP__MDL_WZ,MP__MDL_WW,MP__MDL_WT,MP__MDL_WH

      COMMON/MP_WIDTHS/ MP__MDL_WZ,MP__MDL_WW,MP__MDL_WT,MP__MDL_WH


      COMPLEX*32 MP__GC_5,MP__R2_BXUW,MP__R2_BXCW,MP__R2_CXDW
     $ ,MP__R2_CXSW,MP__R2_UXBW,MP__GC_11,MP__GC_12,MP__GC_13
     $ ,MP__GC_14,MP__GC_17,MP__GC_41

      COMMON/MP_COUPLINGS/ MP__GC_5,MP__R2_BXUW,MP__R2_BXCW
     $ ,MP__R2_CXDW,MP__R2_CXSW,MP__R2_UXBW,MP__GC_11,MP__GC_12
     $ ,MP__GC_13,MP__GC_14,MP__GC_17,MP__GC_41


