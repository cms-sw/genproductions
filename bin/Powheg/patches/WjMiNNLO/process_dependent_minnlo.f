      subroutine init_masses_Dterms()
      use internal_parameters, pi_hoppet => pi, cf_hoppet => cf, ca_hoppet => ca, tf_hoppet => tf
      implicit none
      include 'PhysPars.h'
      call set_masses(ph_zmass)
      end

c DUMMY routine for bmass used in setlocalscales.f for Higgs
      subroutine bmass_in_minlo(bfact,alphas)
      implicit none
      double precision bfact, alphas, dummy
      dummy = 1337
      end
c DUMMY routine for bmass used in setlocalscales.f for Higgs
c DUMMY routine for bmass used in setlocalscales.f for Higgs
      function get_M_for_init_Dterms()
      implicit none
      double precision get_M_for_init_Dterms
      include 'PhysPars.h'
      get_M_for_init_Dterms = ph_wmass
      end
c DUMMY routine for bmass used in setlocalscales.f for Higgs

      subroutine get_B_V1_V2(pborn_UUB,msqB,msqV1,msqV2)
      implicit none
      include 'nlegborn.h'
      integer, parameter :: nflav=5
      double precision msqB(-nflav:nflav,-nflav:nflav), msqV1(-nflav:nflav,-nflav:nflav), msqV2(-nflav:nflav,-nflav:nflav)
      double precision pborn_UUB(0:3,nlegborn-1)
      integer process
      msqB(:,:)  = 0d0
      msqV1(:,:) = 0d0
      msqV2(:,:) = 0d0
      call M2_DY_v(pborn_UUB,msqB,msqV1,msqV2)

      end

c     matrix elements to test UUB projection (blank for DY)
      subroutine uub_for_minnlo(pphy,iborn,amp2)
      implicit none
      include 'pwhg_math.h'
      include 'pwhg_st.h'
      include 'PhysPars.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      real* 8 amp2
      integer nlegs, nflav
      parameter (nlegs=4)
      integer j, k, bflav(1:nlegs), iborn
      real *8 pphy(0:3,nlegs), s, s34
      parameter (nflav=5)
      double precision msqb(-nflav:nflav,-nflav:nflav), msqV1(-nflav:nflav,-nflav:nflav), msqV2(-nflav:nflav,-nflav:nflav)
      real *8, parameter :: zeta3=1.2020569031595942853997381615114d0
      
c$$$      ! build the born squared amplitude
c$$$      do j=-nflav,nflav
c$$$         if (j==0) cycle
c$$$         k=-j
c$$$         bflav(1) = j
c$$$         bflav(2) = k
c$$$         bflav(3) = flst_born(3,1)
c$$$         bflav(4) = flst_born(4,1)
c$$$         call compborn_uub(pphy,bflav,msqb(j,k))
c$$$         amp2 = amp2 + msqb(j,k)
c$$$      end do
c$$$
      write(*,*) 'uub_for_minnlo: not coded for Z production'
      call exit(-1)

      end



      subroutine M2_DY_v(pphy,msqb,msqv1,msqv2)
      implicit none
      include 'pwhg_math.h'
      include 'pwhg_st.h'
      include 'PhysPars.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer nlegs, nflav
      parameter (nlegs=4)
      integer j, k, bflav(1:nlegs), x
      real *8 pphy(0:3,nlegs), s, s34
      parameter (nflav=5)
      double precision msqb(-nflav:nflav,-nflav:nflav), msqV1(-nflav:nflav,-nflav:nflav), msqV2(-nflav:nflav,-nflav:nflav)
      double precision jmapping(12), kmapping(12)
      real *8, parameter :: zeta3=1.2020569031595942853997381615114d0
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode   

      msqb = 0d0
      jmapping = (/-4,-4,-4,-2,-2,-2, 1, 1, 3, 3, 5, 5/)
      kmapping = (/ 1, 3, 5, 1, 3, 5,-2,-4,-2,-4,-2,-4/)
      ! build the born squared amplitude
      do x=1,12
         if(idvecbos.eq.-24) then ! W^- case
            j = jmapping(x)
            k = kmapping(x)
         elseif(idvecbos.eq.24) then ! W^+ case
            j = -jmapping(x)
            k = -kmapping(x)
         else
            write(*,*) 'ERROR: idvecbos can only be -24:W+ or 24:W- '
            call exit(1)
         endif
         bflav(1) = j
         bflav(2) = k
         bflav(3) = flst_born(3,1)
         bflav(4) = flst_born(4,1)
         call compborn_uub(pphy,bflav,msqb(j,k))
      end do
      
      ! now build the one and two loop corrections
      do j=-nflav,nflav
         do k=-nflav,nflav
            msqv1(j,k)=msqb(j,k)*cf*( -8d0 + 7d0/6d0*pi**2)
            msqv2(j,k)=msqb(j,k)*(-57433d0/972d0+281d0/162d0*pi**2
     &           +22d0/27d0*pi**4+1178d0/27d0*zeta3)
         enddo
      enddo

      end


      subroutine compborn_uub(p,bflav,born)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
c -*- Fortran -*-
c      character *2 flav(-5:5)
      real * 8 charge(-5:5)
c      data (charge(ijkh),ijkh=-5,5) 
c      data (flav(ijkh),ijkh=-5,5) 
c      data flav
c     #     /'b~','c~','s~','u~','d~','g','d','u','s','c','b'/
      data charge
     #     / 0.33333333333333333333d0, !   1d0/3
     #      -0.66666666666666666667d0, !  -2d0/3
     #       0.33333333333333333333d0, !   1d0/3 
     #      -0.66666666666666666667d0, !   -2d0/3
     #       0.33333333333333333333d0, !   1d0/3 
     #       0d0,                      !   0d0   
     #      -0.33333333333333333333d0, !   -1d0/3
     #       0.66666666666666666667d0, !   2d0/3   
     #      -0.33333333333333333333d0, !   -1d0/3
     #       0.66666666666666666667d0, !   2d0/3 
     #      -0.33333333333333333333d0/ !   -1d0/3
c      include 'QuarkFlavs.h'
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegborn-1)
      real * 8 p(0:3,nleg)
      integer bflav(nleg)
      real * 8 amp2,born
      integer ferm_type(nleg)
      integer i,j
      real * 8 ferm_charge(nleg)
c     vector boson id and decay
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode   

      if (abs(bflav(3)).le.6.or.abs(bflav(4)).le.6) then
         write(*,*) 'born_ampsq: ERROR in flavor assignement'
         stop
      endif
 
c     i is the flavour index of first incoming parton
c     j is the flavour index of second incoming parton
c     with the convention:
c     
c      -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6                    
c      t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t                    
      
      i = bflav(1)
      j = bflav(2)
      ferm_charge(1) = charge(i)
      ferm_charge(2) = charge(j)
      ferm_type(1) = i/abs(i)
      ferm_type(2) = j/abs(j)


c     antilepton-neutrino from W decay
      ferm_type(3) = bflav(3)/abs(bflav(3))
      ferm_charge(3) = ferm_type(3)*(-1d0)
      ferm_type(4) = -ferm_type(3)
      ferm_charge(4) = 0d0

      
      if(idvecbos.eq.24) then
         call q_aqp_to_al_vl(p,ferm_type,ferm_charge,
     $        amp2)
      elseif(idvecbos.eq.-24) then
         call q_aqp_to_l_avl(p,ferm_type,ferm_charge,
     $        amp2)
      else
         write(*,*) 'ERROR: this subroutine deals only with W+ or W- '
         call exit(1)
      endif

      if(mod(abs(i),2).eq.0) then
         born=amp2*ph_CKM(abs(i)/2,(abs(j)+1)/2)**2
      elseif(mod(abs(i),2).eq.1) then   
         born=amp2*ph_CKM(abs(j)/2,(abs(i)+1)/2)**2
      endif

      end


c this subroutine compute the Born amplitude for the process
c q(p1) qp(p2) -> W(p3+p4)   con W -> l(p3) vl(p4) 
c NUMERICALLY, with the bra/ket formalism, not by squaring the analytic 
c amplitude
c It gets the matrix pphy with all the momenta and gives   
c the amplitude squared (amp2) averaged over initial 
c polarization     
c
c         q  --->-----
c                     |
c                     |            l
c                     |          /  
c         qp ---<-----/\/\/\/\/\/
c                        W      \
c                                \ vl
c     ferm_type = 1 fermion
c     ferm_type = -1 antifermion
c     fermion_charge = +2/3, -1/3, -2/3, +1/3

      subroutine q_aqp_to_al_vl(pphy,fermion_type,fermion_charge,
     $     amp2)
   
      implicit none
c the nleg 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=4)
      integer fermion_type(nleg)
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      real * 8 p1(0:3),p2(0:3)
      include 'pwhg_st.h'
      include 'pwhg_math.h'
      include 'PhysPars.h'
      real * 8 p34
      real * 8 dotp,tmp
      complex * 16 ccdotp
      complex*16 psi1(2,-1:1),psi2(2,-1:1),psi3(2,-1:1),psi4(2,-1:1)
      complex*16 jlep(0:3),jqua(0:3)
      complex*16 jlep_dot_jqua
      integer mu,i
      real * 8 p(0:3,nleg)
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)
      complex *16 prop34w

      if ((fermion_type(3).ne.-1).and.(fermion_type(4).ne.1)) then
         write(*,*) 'ERROR: this subroutine deals only with W+ decay'
         stop
      endif
     

c  copy to local variables
      do i=1,nleg
         do mu=0,3
            p(mu,i) = pphy(mu,i)
         enddo
         ferm_charge(i) = fermion_charge(i)
         ferm_type(i) = fermion_type(i)
      enddo
      
c     exchance particle 1 and 2
      if (ferm_type(1).eq.-1) then
         if (ferm_type(2).eq.1) then
            call exchange_momenta(p(0,1),p(0,2))
            tmp = ferm_charge(1)
            ferm_charge(1)=ferm_charge(2)
            ferm_charge(2)=tmp
            tmp = ferm_type(1)
            ferm_type(1)=ferm_type(2)
            ferm_type(2)=tmp
         else
            write(*,*) 'Error in the type of the quark 1-2'
            stop
         endif
      endif

      
c     fake momenta:
c     for bosons always outgoing
c     for fermions along fermionic current
      do mu=0,3
         p1(mu) = ferm_type(1)*p(mu,1)
         p2(mu) = ferm_type(2)*p(mu,2)
      enddo

      p34=dotp(p(0,3),p(0,4))
      
c     W propagator
      prop34w=1d0/dcmplx(2d0*p34-ph_Wmass2,ph_WmWw)
c     bra and ket are built with physical momenta 

c     q
      call ket(p(0,1),ferm_type(1),psi1)
c     qp
      call bra(p(0,2),ferm_type(2),psi2)
c     l
      call ket(p(0,3),ferm_type(3),psi3)
c     vl
      call bra(p(0,4),ferm_type(4),psi4)
      
c     leptonic current
      call bra_gamma_ket(psi4,psi3,-1,jlep)
      
c     quark current
      call bra_gamma_ket(psi2,psi1,-1,jqua)
      


      amp2=0d0

      
      jlep_dot_jqua = ccdotp(jlep,jqua)*prop34w
            
      amp2 = amp2 + jlep_dot_jqua *
     $     DCONJG(jlep_dot_jqua)       
            
            
                 
      amp2 = amp2*(ph_unit_e/ph_sthw)**4/4d0 
c     1/4 from average over initial-state polarization
c     1/nc^2 * nc = 1/nc from average over initial-state colors and sum over 
c     quark colors 
      amp2=  amp2/4d0/nc 
      
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine q_aqp_to_l_avl(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
c the 5 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=4)
      integer fermion_type(nleg),i
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)

       if ((fermion_type(3).ne.1).and.(fermion_type(4).ne.-1)) then
         write(*,*) 'ERROR: this subroutine deals only with W- decay'
         stop
      endif

      do i=1,nleg
      
         ferm_charge(i) = -fermion_charge(i)
         ferm_type(i) = -fermion_type(i)
      enddo
            
      
      call q_aqp_to_al_vl(pphy,ferm_type,ferm_charge,
     #     amp2)

      end
