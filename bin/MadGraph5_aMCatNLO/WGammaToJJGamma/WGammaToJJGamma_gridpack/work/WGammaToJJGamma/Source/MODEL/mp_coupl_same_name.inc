ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      REAL*16 G
      COMMON/MP_STRONG/ G

      COMPLEX*32 GAL(2)
      COMMON/MP_WEAK/ GAL

      COMPLEX*32 MU_R
      COMMON/MP_RSCALE/ MU_R


      REAL*16 MDL_MZ,MDL_MT,MDL_MTA,MDL_MH,MDL_MW

      COMMON/MP_MASSES/ MDL_MZ,MDL_MT,MDL_MTA,MDL_MH,MDL_MW


      REAL*16 MDL_WW,MDL_WZ,MDL_WT,MDL_WH

      COMMON/MP_WIDTHS/ MDL_WW,MDL_WZ,MDL_WT,MDL_WH


      COMPLEX*32 GC_4,GC_5,R2_GQQ,R2_QQQ,UV_GQQG_1EPS,UV_GQQB_1EPS
     $ ,UVWFCT_G_2_1EPS,R2_DDA,R2_UUA,R2_BXTW,UV_GQQT,UVWFCT_G_2,GC_1
     $ ,GC_2,GC_11,GC_25

      COMMON/MP_COUPLINGS/ GC_4,GC_5,R2_GQQ,R2_QQQ,UV_GQQG_1EPS
     $ ,UV_GQQB_1EPS,UVWFCT_G_2_1EPS,R2_DDA,R2_UUA,R2_BXTW,UV_GQQT
     $ ,UVWFCT_G_2,GC_1,GC_2,GC_11,GC_25


