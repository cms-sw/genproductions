      subroutine init_couplings
      implicit none
      include 'nlegborn.h'
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'pwhg_flg.h'
      include 'pwhg_math.h'
      include 'pwhg_physpar.h'      
      include 'pwhg_par.h'      
      include 'minnlo_flg.h'
      real * 8 masswindow_low,masswindow_high,wmasslow,wmasshigh
      real * 8 powheginput
      external powheginput
      real*8 osWmass,osZmass
      logical verbose
      parameter(verbose=.true.)
      integer i,j

      flg_minnlo=powheginput('#minnlo').eq.1
      flg_uubornonly=powheginput('#uubornonly').eq.1
c$$$      if (flg_uubornonly) then
c$$$         flg_bornonly = .true.
c$$$      endif
c     If minlo is not present in the input card, but minnlo is,
c     then set also flg_minlo to true, so that we are sure all is done
c     properly in setlocalscales and for all other occurencies of
c     flg_minlo.
      if(flg_minnlo) flg_minlo=.true.
      
      
      if(powheginput("#par_isrtinycsi").gt.0) 
     $     par_isrtinycsi = powheginput("#par_isrtinycsi")

      if(powheginput("#par_fsrtinycsi").gt.0) 
     $     par_fsrtinycsi = powheginput("#par_fsrtinycsi")

      if(powheginput("#par_isrtinyy").gt.0) 
     $     par_isrtinyy = powheginput("#par_isrtinyy")

      if(powheginput("#par_fsrtinyy").gt.0) 
     $     par_fsrtinyy = powheginput("#par_fsrtinyy")

      flg_withdamp=.true.
      flg_bornzerodamp=.true.
      if(powheginput('#withdamp').eq.0) then
         flg_withdamp=.false.
         flg_bornzerodamp=.false.
      endif

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_Wmass = powheginput("#Wmass")
      if (ph_Wmass.le.0d0) ph_Wmass  = 80.379d0
      ph_Wwidth = powheginput("#Wwidth")
      if (ph_Wwidth.le.0d0) ph_Wwidth =  2.085d0
      ! ph_alphaem = powheginput("#alphaem")
      ! if (ph_alphaem.le.0d0) ph_alphaem = 1d0/128.89d0
      ph_Zmass = powheginput("#Zmass")
      if (ph_Zmass.le.0d0) ph_Zmass  = 91.1876d0     
      ph_Zwidth = powheginput("#Zwidth")
      if (ph_Zwidth.le.0d0) ph_Zwidth =  2.4952d0
      ph_gmu = powheginput("#gmu")
      if (ph_gmu.le.0) ph_gmu = 1.1663787d-5
      
      if(verbose) then
            write(*,*) '*************************************'
            write(*,*) 'Using GF,mZ,mW input scheme'
            write(*,*) 'input Z mass = ',ph_Zmass
            write(*,*) 'input Z width = ',ph_Zwidth
            write(*,*) 'input W mass = ',ph_Wmass
            write(*,*) 'input W width = ',ph_Wwidth
            write(*,*) 'input gmu = ',ph_gmu
            write(*,*) '*************************************'
      endif
            
c     correct masses fixed-width -> width-dependent scheme
      osWmass= ph_Wmass
      osWwidth= ph_Wwidth
      ph_Wmass= osWmass/sqrt(1.d0+(osWwidth/osWmass)**2)
      ph_Wwidth= osWwidth/sqrt(1.d0+(osWwidth/osWmass)**2)
      
      osZmass= ph_Zmass
      osZwidth= ph_Zwidth
      ph_Zmass= osZmass/sqrt(1.d0+(osZwidth/osZmass)**2)
      ph_Zwidth= osZwidth/sqrt(1.d0+(osZwidth/osZmass)**2)
      
c     CAVEAT: 
      ph_CKM(1,1) = powheginput("#CKM_Vud")
      if (ph_CKM(1,1).le.0d0) ph_CKM(1,1)=0.975d0 
      ph_CKM(1,2) = powheginput("#CKM_Vus")
      if (ph_CKM(1,2).le.0d0) ph_CKM(1,2)=0.222d0 
      ph_CKM(1,3) = powheginput("#CKM_Vub")
      if (ph_CKM(1,3).le.0d0) ph_CKM(1,3)=1d-10
      ph_CKM(2,1) = powheginput("#CKM_Vcd")
      if (ph_CKM(2,1).le.0d0) ph_CKM(2,1)=0.222d0 
      ph_CKM(2,2) = powheginput("#CKM_Vcs")
      if (ph_CKM(2,2).le.0d0)  ph_CKM(2,2)=0.975d0 
      ph_CKM(2,3) = powheginput("#CKM_Vcb")
      if (ph_CKM(2,3).le.0d0) ph_CKM(2,3)=1d-10
      ph_CKM(3,1) = powheginput("#CKM_Vtd")
      if (ph_CKM(3,1).le.0d0) ph_CKM(3,1)=1d-10
      ph_CKM(3,2) = powheginput("#CKM_Vts")
      if (ph_CKM(3,2).le.0d0) ph_CKM(3,2)=1d-10
      ph_CKM(3,3) = powheginput("#CKM_Vtb")
      if (ph_CKM(3,3).le.0d0) ph_CKM(3,3)=1d0

c     number of light flavors
      st_nlight = 5

c Masses of light leptons for W decays:
      physpar_ml(1)=0.51099891d-3
      physpar_ml(2)=0.1056583668d0
      physpar_ml(3)=1.77684d0     

      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   DEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ph_sthw2 = abs(1d0-(ph_Wmass/ph_Zmass)**2)
      ph_sthw = sqrt(ph_sthw2)
      ph_cthw = sqrt(1-ph_sthw2)
      ph_Zmass2 = ph_Zmass**2
      ph_Wmass2 = ph_Wmass**2



c     set mass windows around W-mass peak in unit of ph_Wwidth
c     It is used in the generation of the Born phase space
      wmasslow = powheginput("#min_W_mass")
      wmasshigh = powheginput("#max_W_mass")
      if(wmasslow.gt.0) then
         ph_Wmass2low=wmasslow**2
      else
C     masswindow is an optonal  parameter passed by the user
C     the default vale is 30 
         masswindow_low = powheginput("#masswindow_low")
         if(masswindow_low.lt.0d0) masswindow_low=30d0
         ph_Wmass2low=max(0d0,ph_Wmass-masswindow_low*ph_Wwidth)
         ph_Wmass2low= ph_Wmass2low**2
      endif
      if(wmasshigh.gt.0) then
         ph_Wmass2high=wmasshigh**2
      else
         masswindow_high = powheginput("#masswindow_high")
         if(masswindow_high.lt.0d0) masswindow_high=30d0
         ph_Wmass2high=ph_Wmass+masswindow_high*ph_Wwidth
         ph_Wmass2high= min(kn_sbeams,ph_Wmass2high**2)
      endif

      ph_WmWw = ph_Wmass * ph_wwidth

      ph_unit_e = sqrt(ph_gmu * 8d0/sqrt(2d0) * ph_Wmass**2 * ph_sthw2)
      ph_alphaem = ph_unit_e*ph_unit_e/4d0/pi

      if(verbose) then
      write(*,*) '*************************************'
      write(*,*) 'Z mass = ',ph_Zmass
      write(*,*) 'Z width = ',ph_Zwidth
      write(*,*) 'W mass = ',ph_Wmass
      write(*,*) 'W width = ',ph_Wwidth
      write(*,*) '1/alphaem = ',1d0/ph_alphaem
      write(*,*) 'alphaem = ',ph_alphaem
      write(*,*) 'sthw2 = ',ph_sthw2
      write(*,*) '(unit_e)^2 = ',ph_unit_e**2   
      write(*,*) '(g_w)^2 = ',ph_unit_e*ph_unit_e/ph_sthw2   
      write(*,*) 'CKM matrix' 
      do i=1,3
         write(*,*) (ph_CKM(i,j),j=1,3)
      enddo
      write(*,*) '*************************************'
      write(*,*)
      write(*,*) '*************************************'
      write(*,*) sqrt(ph_Wmass2low),'< M_W <',sqrt(ph_Wmass2high)
      write(*,*) '*************************************'
      endif
      end





