


      subroutine qqb_dm_monojet_v(p,msq) 
!----- routine for calcuating the virtual corrections to monojet production 
      implicit none 
      include 'constants.f' 
      include 'dm_params.f' 
      include 'scheme.f' 
      include 'qcdcouple.f'
      include 'epinv.f' 
      include 'nflav.f' 
      double precision  qqbg(2),qbqg(2)
      double precision qbgq(2),qgqb(2)
      double precision gqqb(2),gqbq(2) 
      double precision  qqbg_nf(2),qbqg_nf(2)
      double precision qbgq_nf(2),qgqb_nf(2)
      double precision gqqb_nf(2),gqbq_nf(2) 
      double precision qqbg_sum(nf),qbqg_sum(nf) 
      double precision qgqb_sum(nf),qbgq_sum(nf) 
      double precision gqqb_sum(nf),gqbq_sum(nf) 
      double precision p(mxpart,4),msq(-nf:nf,-nf:nf) 
      integer j,k 
      double precision msq0(-nf:nf,-nf:nf),fac,subuv 
      logical check_QED 
      common/check_QED/check_QED 
      double precision fac_dm,s34
      double complex cprop
      double precision propsq
      logical first 
      data first /.true./
      save first

      check_QED=.false. 
      
!      if(check_QED) then 
!------ make factor into photon propogator for testing 
!         s34=2d0*(p(3,4)*p(4,4)-p(3,3)*p(4,3)
!     &        -p(3,2)*p(4,2)-p(3,1)*p(4,1))
!         fac_dm=esq**2/s34**2 
!         do j=1,nf 
!            dmL(j)=Q(j)
!            dmR(j)=Q(j) 
!         enddo
!      else
!         fac_dm=one/dm_lam**4
!      endif


      
!------ Fix Med-width 
!      if((medwidth.eq.1d0).and.(first)) then 
!         medwidth=medmass/8d0/pi 
!      elseif((medwidth.eq.0d0).and.(first)) then
!         medwidth=medmass/3d0
!      endif
!      if(first.and.(effective_th.eqv..false.)) then 
!         write(6,*) 'automatically set medwidth ',medwidth 
!      endif

      if(effective_th) then 
!--------- effective theory pre-factor
         fac_dm=one/dm_lam**4
      else
!-------- full theory => for V,A and PS is simply g_dmx**2*g_dmq**2/prop(34)**2 
!-------- for S and G need to be more careful, but G happens elsewhere and S can be done 
!-------- in special routine
         s34=(p(3,4)+p(4,4))**2
     &        -(p(3,1)+p(4,1))**2
     &        -(p(3,2)+p(4,2))**2
     &        -(p(3,3)+p(4,3))**2         
         cprop=cone/Dcmplx((s34-medmass**2),medmass*medwidth)
         propsq=cdabs(cprop)**2 
         fac_dm=propsq*g_dmq**2*g_dmx**2
      endif

      

      scheme='dred' 
      msq(:,:)=0d0 
      qqbg_sum(:)=0d0 
      qbqg_sum(:)=0d0
      qgqb_sum(:)=0d0 
      qbgq_sum(:)=0d0
      gqqb_sum(:)=0d0 
      gqbq_sum(:)=0d0
      qgqb_nf(:)=0d0 
      qbgq_nf(:)=0d0 
      gqqb_nf(:)=0d0 
      gqbq_nf(:)=0d0 
      qqbg_nf(:)=0d0  
      qbqg_nf(:)=0d0 
!------ lowest order 

      call qqb_dm_monojet(p,msq0) 
!------ UV counterterm contains the finite renormalization to arrive at the MS bar scheme (V,A check for S) 

      subuv=ason2pi*xn*(epinv*(11d0-2d0*dble(nflav)/xn)-1d0)/6d0

      fac=8d0*cf*xnsq*gsq*fac_dm
    

      if(dm_mediator.eq.'vector') then 
         call qqb_dm_monojet_v_Vamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_v_Vamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_v_Vamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_v_Vamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_v_Vamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_v_Vamps(p,5,1,2,3,4,gqbq)
         if(first) first=.false.
      elseif(dm_mediator.eq.'axvect') then 
         call qqb_dm_monojet_v_Axamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_v_Axamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_v_Axamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_v_Axamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_v_Axamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_v_Axamps(p,5,1,2,3,4,gqbq)       
!------ nf axial pieces 
         call qqb_dm_monojet_nf_ax(p,1,2,5,3,4,qgqb_nf)
         call qqb_dm_monojet_nf_ax(p,5,2,1,3,4,qbgq_nf)
         call qqb_dm_monojet_nf_ax(p,1,5,2,3,4,qqbg_nf)  
         call qqb_dm_monojet_nf_ax(p,2,5,1,3,4,qbqg_nf)
         call qqb_dm_monojet_nf_ax(p,2,1,5,3,4,gqqb_nf)  
         call qqb_dm_monojet_nf_ax(p,5,1,2,3,4,gqbq_nf)  
         if(first) first=.false.
      elseif(dm_mediator.eq.'scalar') then 
         call qqb_dm_monojet_v_Samps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_v_Samps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_v_Samps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_v_Samps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_v_Samps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_v_Samps(p,5,1,2,3,4,gqbq)
         fac=fac/4d0
         
         if(first) then 
            first=.false.
            call set_scalar_coups
         endif
      elseif(dm_mediator.eq.'pseudo') then 
         call qqb_dm_monojet_v_PSamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_v_PSamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_v_PSamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_v_PSamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_v_PSamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_v_PSamps(p,5,1,2,3,4,gqbq)
         fac=fac/4d0
         if(first) then 
            first=.false.
            call set_scalar_coups
            call check_dmAxC
         endif
   
      elseif(dm_mediator.eq.'gluonO') then 
         if(first) first=.false.
         call gg_dm_monojet_v(p,msq) 
         return 
      endif

      

     
!----- note nf pieces are linear in couplings since 
!----- only come from tree 
      
      do j=1,nf
         qqbg_sum(j)=qqbg_sum(j)
     &        +dmL(j)**2*qqbg(1)
     &        +dmR(j)**2*qqbg(2)
     &        +dmL(j)*qqbg_nf(1)+dmR(j)*qqbg_nf(2)
         qbqg_sum(j)=qbqg_sum(j)
     &        +dmL(j)**2*qbqg(1)
     &        +dmR(j)**2*qbqg(2)
     &        +dmL(j)*qbqg_nf(1)+dmR(j)*qbqg_nf(2)
         qgqb_sum(j)=qgqb_sum(j)
     &        +dmL(j)**2*qgqb(1)
     &        +dmR(j)**2*qgqb(2)
     &        +dmL(j)*qgqb_nf(1)+dmR(j)*qgqb_nf(2)
         qbgq_sum(j)=qbgq_sum(j)
     &        +dmL(j)**2*qbgq(1)
     &        +dmR(j)**2*qbgq(2)
     &        +dmL(j)*qbgq_nf(1)+dmR(j)*qbgq_nf(2)
         gqqb_sum(j)=gqqb_sum(j)
     &        +dmL(j)**2*gqqb(1)
     &        +dmR(j)**2*gqqb(2)
     &        +dmL(j)*gqqb_nf(1)+dmR(j)*gqqb_nf(2)
         gqbq_sum(j)=gqbq_sum(j)
     &        +dmL(j)**2*gqbq(1)
     &        +dmR(j)**2*gqbq(2)
     &        +dmL(j)*gqqb_nf(1)+dmR(j)*gqqb_nf(2)
      enddo
      
    

      do j=-nf,nf 
         do k=-nf,nf 
           if( (j .ne. 0) .and. (k .ne. 0) .and. (j .ne. -k)) goto 20

           if((j.eq.0).and.(k.eq.0)) then 
              msq(j,k)=0d0 
           elseif((j.gt.0).and.(k.lt.0)) then 
              msq(j,k)=aveqq*qqbg_sum(j)*fac 
     &            -subuv*msq0(j,k)
           elseif((j.gt.0).and.(k.eq.0)) then 
              msq(j,k)=aveqg*qgqb_sum(j)*fac
     &            -subuv*msq0(j,k)
           elseif((j.eq.0).and.(k.gt.0)) then 
              msq(j,k)=aveqg*gqqb_sum(k)*fac
     &            -subuv*msq0(j,k)
           elseif((j.eq.0).and.(k.lt.0)) then 
              msq(j,k)=aveqg*gqbq_sum(abs(k))*fac 
     &            -subuv*msq0(j,k)
           elseif((j.lt.0).and.(k.gt.0)) then 
              msq(j,k)=aveqq*qbqg_sum(k)*fac
     &            -subuv*msq0(j,k)
           elseif((j.lt.0).and.(k.eq.0)) then 
              msq(j,k)=aveqg*qbgq_sum(abs(j))*fac
     &            -subuv*msq0(j,k)
           endif

         
 20        continue 
        enddo
      enddo

    
      return 
      end 


