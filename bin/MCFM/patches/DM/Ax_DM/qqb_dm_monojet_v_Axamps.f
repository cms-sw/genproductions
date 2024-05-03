
      subroutine qqb_dm_monojet_v_Axamps(p,i1,i2,i3,i4,i5,qgqb) 
      implicit none 
!----- combined colour and Born intefered amplitudes as a function of quark line helicity 
!------ default is q(i1)+g(i2)+qb(i3)+x(i4)+x(i5) 
      include 'constants.f' 
      include 'qcdcouple.f' 
!      include 'zprods_decl.f'
      double precision qgqb(2) 
      integer i1,i2,i3,i4,i5 
      double precision p(mxpart,4) 
      integer h1,h2,h3,h4 
      double complex amp_tree(2,2,2,2),amp_lc(2,2,2,2),amp_slc(2,2,2,2) 
!      double precision s45
!------ tree
      call qqb_dm_monojet_Axamps(p,i1,i2,i3,i4,i5,amp_tree)  
!------ leading colour      
       call qqb_dm_monojet_lc_Axamps(p,i1,i2,i3,i4,i5,amp_lc) 
!------ subleading colour (swap i2 and i3) 
      call qqb_dm_monojet_slc_Axamps(p,i1,i3,i2,i4,i5,amp_slc)

!------ interfere with born as function of quark line helicity 
  
  
!      call spinoru(5,p,za,zb)
!      write(6,*) A51(i1,i2,i3,i4,i5,za,zb)
!      write(6,*) amp_lc(2,2,1,2)/(cone*s45)
!      pause 
      qgqb(:)=0d0 
      do h1=1,2 
         do h2=1,2
            do h3=1,2 
               do h4=1,2 
!                  if(h3+h4.eq.3) then
!        write(6,*) h1,h2,h3,h4,amp_tree(h1,h2,h3,h4),amp_lc(h1,h2,h3,h4)
!                  endif
                  qgqb(h1)=qgqb(h1)
     &                 +ason2pi*Dble(Dconjg(amp_tree(h1,h2,h3,h4))*
     &                 (amp_lc(h1,h2,h3,h4)
     &                 +amp_slc(h1,h2,h3,h4)/xnsq))
               enddo
            enddo
         enddo
      enddo

!      write(6,*) 
!      write(6,*) i1,i2,i3,i4,i5




!      do h1=1,2 
!         do h3=1,2 
!            do h4=1,2 
!               if(h3.ne.h4) then 
!                  write(6,*) h1,h3,h4,
!     &             '1',amp_tree(h1,1,h3,h4)/(s45*cone)
!     &  ,amp_lc(h1,1,h3,h4)/(s45*cone),
!     &  Dble(Dconjg(amp_tree(h1,1,h3,h4))*amp_lc(h1,1,h3,h4))/s45**2,
!     &            '2',amp_tree(h1,2,h3,h4)/(s45*cone)
!     &  ,amp_lc(h1,2,h3,h4)/(s45*cone),
!     &  Dble(Dconjg(amp_tree(h1,2,h3,h4))*amp_lc(h1,1,h3,h4))/s45**2

!     &    ason2pi*Dble(Dconjg(amp_tree(h1,1,h3,h4))*amp_lc(h1,1,h3,h4))
!     &                 /s45**2+
!     &     ason2pi*Dble(Dconjg(amp_tree(h1,2,h3,h4))*amp_lc(h1,2,h3,h4))
!     &                 /s45**2 
!               endif
!            enddo
!         enddo
!      enddo
      
      return 
      end 

      
