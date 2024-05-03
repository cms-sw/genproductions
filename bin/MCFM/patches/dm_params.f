
      double precision xmass,medmass,dm_lam
      double precision medwidth
      double precision gdm,g_dmx,g_dmq,g_dmg
      logical effective_th
      logical incbsmdm
      character*6 dm_mediator 
      double precision dmL(5),dmR(5)
      logical yukawa_scal
      common/yuk_scal/yukawa_scal
      common/dm_params/xmass,medmass,dm_lam,medwidth
      common/dm_coup/dmL,dmR
      common/dm_med/dm_mediator
      common/bsmdm/incbsmdm
      common/effec_dm/effective_th
      common/dm_g/gdm,g_dmx,g_dmq,g_dmg
