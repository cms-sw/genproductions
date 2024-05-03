


      subroutine qqb_dm_monophot_v(p,msq) 
!----- routine for calcuating the virtual corrections to monojet production 
      implicit none 
      include 'constants.f' 
      include 'dm_params.f' 
      include 'scheme.f' 
      include 'ewcharge.f' 
      include 'ewcouple.f'
      double precision  qqbg(2),qbqg(2)
      double precision qqbg_sum(nf),qbqg_sum(nf) 
      double precision p(mxpart,4),msq(-nf:nf,-nf:nf) 
      integer j,k 
      double precision fac
      double complex cprop
      double precision propsq
  
      logical check_QED 
      common/check_QED/check_QED 
      double precision fac_dm,s34
      logical first 
      data first /.true./
      save first 


      check_QED=.false. 
      
      if(check_QED) then 
!------ make factor into photon propogator for testing 
         s34=2d0*(p(3,4)*p(4,4)-p(3,3)*p(4,3)
     &        -p(3,2)*p(4,2)-p(3,1)*p(4,1))
         fac_dm=esq**2/s34**2 
         do j=1,nf 
            dmL(j)=Q(j)
            dmR(j)=Q(j) 
         enddo
      else
         fac_dm=one/dm_lam**4
      endif

      
      if(effective_th) then 
         fac_dm=one/dm_lam**4
      else
!--------full theory => for V,A and PS is simply g_dmx**2*g_dmq**2/prop(34)**2 
!--------for S and G need to be more careful, but G happens elsewhere and S can be done 
!--------in special routine
         s34=(p(3,4)+p(4,4))**2
     &        -(p(3,1)+p(4,1))**2
     &        -(p(3,2)+p(4,2))**2-(p(3,3)+p(4,3))**2         
         cprop=cone/Dcmplx((s34-medmass**2),medmass*medwidth)
         propsq=cdabs(cprop)**2 
         fac_dm=propsq*g_dmq**2*g_dmx**2
      endif

      scheme='dred' 
      msq(:,:)=0d0 
      qqbg_sum(:)=0d0 
      qbqg_sum(:)=0d0
      fac=16d0*cf*xn*esq*fac_dm
     
      if(dm_mediator.eq.'vector') then 
         call qqb_dm_monophot_v_Vamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monophot_v_Vamps(p,2,5,1,3,4,qbqg)
      elseif(dm_mediator.eq.'axvect') then 
         call qqb_dm_monophot_v_Axamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monophot_v_Axamps(p,2,5,1,3,4,qbqg)
         if(first) then 
            first = .false. 
            call check_dmAxC
         endif       
      elseif(dm_mediator.eq.'scalar') then 
         call qqb_dm_monophot_v_Samps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monophot_v_Samps(p,2,5,1,3,4,qbqg)
         fac=fac/4d0
         if(first) then 
            first=.false.
            call set_scalar_coups
         endif
      elseif(dm_mediator.eq.'pseudo') then 
         call qqb_dm_monophot_v_PSamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monophot_v_PSamps(p,2,5,1,3,4,qbqg)
         if(first) then 
            first = .false. 
            call set_scalar_coups      
            call check_dmAxC
         endif       
         fac=fac/4d0
      endif


      
      
      do j=1,nf
         qqbg_sum(j)=qqbg_sum(j)
     &        +dmL(j)**2*qqbg(1)
     &        +dmR(j)**2*qqbg(2)
         qbqg_sum(j)=qbqg_sum(j)
     &        +dmL(j)**2*qbqg(1)
     &        +dmR(j)**2*qbqg(2)
      enddo
      
    

      do j=-nf,nf 
         do k=-nf,nf 
           if( (j .ne. 0) .and. (k .ne. 0) .and. (j .ne. -k)) goto 20

           if((j.gt.0).and.(k.lt.0)) then 
              msq(j,k)=aveqq*qqbg_sum(j)*fac*Q(j)**2
           elseif((j.lt.0).and.(k.gt.0)) then 
              msq(j,k)=aveqq*qbqg_sum(k)*fac*Q(k)**2
           endif

         
 20        continue 
        enddo
      enddo

    
      return 
      end 


