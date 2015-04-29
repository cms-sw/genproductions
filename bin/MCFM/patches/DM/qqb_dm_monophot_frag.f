
      subroutine qqb_dm_monophot_frag(p,msq) 
      implicit none 
      include 'constants.f'
      include 'dm_params.f'
      include 'qcdcouple.f'
      include 'frag.f'
      double precision p(mxpart,4),msq(-nf:nf,-nf:nf)
      double complex qqbg(2,2,2,2),qbqg(2,2,2,2)
      double complex qgqb(2,2,2,2),qbgq(2,2,2,2)
      double complex gqqb(2,2,2,2),gqbq(2,2,2,2)
      double precision qqbg_sum(nf),qbqg_sum(nf)
      double precision qgqb_sum(nf),qbgq_sum(nf)
      double precision gqbq_sum(nf),gqqb_sum(nf)
      double precision fac 
      integer h2,h3,h4 
      integer j,k,i
      double complex cprop
      double precision propsq
  
      double precision s34 
      logical check_QED 
      common/check_QED/check_QED 
      double precision fsq,D(0:5)
      common/D/D
      logical first 
      data first /.true./
      save first 
      
      do j=-nf,nf
      do k=-nf,nf
      msq(j,k)=0d0
      enddo
      enddo
      
      fsq=frag_scale**2
c---- Generate array D(j) corresponding to MCFM notation 0=gluon 1=up 2=down ....
        do i=0,5
           D(i)=0d0
           if     (fragset .eq. 'BFGset_I') then
              call get_frag(z_frag,fsq,1,i,D(i))   
           elseif (fragset .eq. 'BFGsetII') then  
              call get_frag(z_frag,fsq,2,i,D(i))   
           elseif (fragset .eq. 'GdRG__LO') then 
            call GGdR_frag(z_frag,i,D(i),0)
           else
              write(6,*) 'Unrecognized fragmentation set name: ',fragset
              stop        
           endif
        enddo


      check_QED=.false. 

!      if(check_QED) then 
!------ make factor into photon propogator for testing 
!         s34=2d0*(p(3,4)*p(4,4)-p(3,3)*p(4,3)
!     &        -p(3,2)*p(4,2)-p(3,1)*p(4,1))
!         fac=gsq*V*esq**2/s34**2*4d0
!         do j=1,nf 
!            dmL(j)=Q(j)
!            dmR(j)=Q(j) 
!         enddo
!      else
!         fac=4d0/dm_lam**4*gsq*V
!      endif


      if(effective_th) then 
         fac=4d0/dm_lam**4*gsq*V
      else
!--------full theory => for V,A and PS is simply g_dmx**2*g_dmq**2/prop(34)**2 
!--------for S and G need to be more careful, but G happens elsewhere and S can be done 
!--------in special routine
         s34=(p(3,4)+p(4,4))**2
     &        -(p(3,1)+p(4,1))**2
     &        -(p(3,2)+p(4,2))**2-(p(3,3)+p(4,3))**2         
         cprop=cone/Dcmplx((s34-medmass**2),medmass*medwidth)
         propsq=cdabs(cprop)**2 
         fac=4d0*gsq*V*propsq*g_dmq**2*g_dmx**2
      endif
      

      qqbg_sum(:)=0d0
      qbqg_sum(:)=0d0 
      qgqb_sum(:)=0d0 
      qbgq_sum(:)=0d0 
      gqqb_sum(:)=0d0 
      gqbq_sum(:)=0d0 

      msq(:,:)=0
!------- wrapper for separte operators
      if(dm_mediator.eq.'vector') then 
         call qqb_dm_monojet_Vamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_Vamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_Vamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_Vamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_Vamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_Vamps(p,5,1,2,3,4,gqbq)
      elseif(dm_mediator.eq.'axvect') then 
         call qqb_dm_monojet_Axamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_Axamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_Axamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_Axamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_Axamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_Axamps(p,5,1,2,3,4,gqbq)
         if(first) then 
            first = .false. 
            call check_dmAxC
         endif       
      elseif(dm_mediator.eq.'scalar') then
         call qqb_dm_monojet_Samps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_Samps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_Samps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_Samps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_Samps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_Samps(p,5,1,2,3,4,gqbq)
         fac=fac/4d0
          if(first) then 
            first=.false.
            call set_scalar_coups
         endif

      elseif(dm_mediator.eq.'pseudo') then 
         call qqb_dm_monojet_PSamps(p,1,2,5,3,4,qgqb)  
         call qqb_dm_monojet_PSamps(p,5,2,1,3,4,qbgq)
         call qqb_dm_monojet_PSamps(p,1,5,2,3,4,qqbg)  
         call qqb_dm_monojet_PSamps(p,2,5,1,3,4,qbqg)
         call qqb_dm_monojet_PSamps(p,2,1,5,3,4,gqqb)  
         call qqb_dm_monojet_PSamps(p,5,1,2,3,4,gqbq)
         fac=fac/4d0
         if(first) then 
            first = .false. 
            call check_dmAxC
         endif       

      endif

     

      do j=1,nf
      do h2=1,2
         do h3=1,2 
            do h4=1,2 
               qqbg_sum(j)=qqbg_sum(j)
     &              +cdabs(dmL(j)*qqbg(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*qqbg(2,h2,h3,h4))**2
               qbqg_sum(j)=qbqg_sum(j)
     &              +cdabs(dmL(j)*qbqg(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*qbqg(2,h2,h3,h4))**2
               qgqb_sum(j)=qgqb_sum(j)
     &              +cdabs(dmL(j)*qgqb(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*qgqb(2,h2,h3,h4))**2
               qbgq_sum(j)=qbgq_sum(j)
     &              +cdabs(dmL(j)*qbgq(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*qbgq(2,h2,h3,h4))**2
               gqqb_sum(j)=gqqb_sum(j)
     &              +cdabs(dmL(j)*gqqb(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*gqqb(2,h2,h3,h4))**2
               gqbq_sum(j)=gqbq_sum(j)
     &              +cdabs(dmL(j)*gqbq(1,h2,h3,h4))**2
     &              +cdabs(dmR(j)*gqbq(2,h2,h3,h4))**2
            enddo
         enddo
      enddo
      enddo

!      write(6,*) qqbg_sum 
!      pause

!---- compare to z 
!      call spinoru(5,p,za,zb) 
!      call zgamps2(1,2,4,3,5,za,zb,ztes)
!      write(6,*) ztes 
!      zt2=0d0 
!      do j=1,2
!         do k=1,2 
!            zt2=zt2+ztes(j,k)
!         enddo
!      enddo
!      write(6,*) zt2,qqbg
!      pause

!     call writeout(p)
!--- compare to paddy 
!      call pjfox_dm(2,p,pjf_msq) 
!      write(6,*) 'my qbqg ',qbqg 
!      write(6,*) 'my qqbg_sum', qqbg 
!      write(6,*) 'paddys ',pjf_msq 
!      write(6,*) 'ratio ',qqbg/pjf_msq 
!      pause

      do j=-nf,nf 
         do k=-nf,nf 
           if( (j .ne. 0) .and. (k .ne. 0) .and. (j .ne. -k)) goto 20

           if((j.eq.0).and.(k.eq.0)) then 
              msq(j,k)=0d0 
           elseif((j.gt.0).and.(k.lt.0)) then 
              msq(j,k)=aveqq*qqbg_sum(j)*fac*D(0) 
           elseif((j.gt.0).and.(k.eq.0)) then 
              msq(j,k)=aveqg*qgqb_sum(j)*fac*D(j)
           elseif((j.eq.0).and.(k.gt.0)) then 
              msq(j,k)=aveqg*gqqb_sum(k)*fac*D(k)
           elseif((j.eq.0).and.(k.lt.0)) then 
              msq(j,k)=aveqg*gqbq_sum(abs(k))*fac*D(-k) 
           elseif((j.lt.0).and.(k.gt.0)) then 
              msq(j,k)=aveqq*qbqg_sum(k)*fac*D(0)
           elseif((j.lt.0).and.(k.eq.0)) then 
              msq(j,k)=aveqg*qbgq_sum(abs(j))*fac*D(-j)
           endif


 20        continue 
        enddo
      enddo

     
      return 
      end 
