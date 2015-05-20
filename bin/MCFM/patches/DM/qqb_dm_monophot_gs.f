      subroutine qqb_dm_monophot_gs(p,msq)
c---Matrix element SUBTRACTION squared averaged over initial colors and spins
c     q(-p1)+qbar(-p2) -->  e-(p3)+e+(p4))+a(p5)+g(p6)
      implicit none 
      include 'constants.f'
      include 'ptilde.f'
      include 'qqgg.f'
      include 'frag.f'
      include 'ewcharge.f'
       
      include 'phot_dip.f'
      integer j,k,nd

      double precision p(mxpart,4),msq(maxd,-nf:nf,-nf:nf)
      double precision msq16_2(-nf:nf,-nf:nf),msq26_1(-nf:nf,-nf:nf),
     . sub16_2(4),sub26_1(4),dummyv(-nf:nf,-nf:nf),dsubv
      double precision sub56_1,sub56_2,msq56_1(-nf:nf,-nf:nf),
     . msq56_2(-nf:nf,-nf:nf)
      external qqb_dm_monophot,donothing_gvec
      external qqb_dm_monojet

      if(frag) then
         ndmax=3
      else
         ndmax=2
      endif

      do j=1,mxpart
         phot_dip(j)=.false.
      enddo

c---- calculate both initial-initial dipoles
c---- note that we do not require the gg dipoles, so the v-type
c---- entries are left as dummies
      call dips(1,p,1,6,2,sub16_2,dsubv,msq16_2,dummyv,
     . qqb_dm_monophot,donothing_gvec)
      call dips(2,p,2,6,1,sub26_1,dsubv,msq26_1,dummyv,
     . qqb_dm_monophot,donothing_gvec)
      
      if (frag) then 
         call dipsfrag(3,p,5,6,2,sub56_2,msq56_2,qqb_dm_monojet)
         phot_dip(3)=.true.
      endif
    


      do j=-nf,nf
      do k=-nf,nf

      do nd=1,ndmax
      msq(nd,j,k)=0d0
      enddo

      if  ((j .eq. 0) .and. (k .eq. 0)) then
         goto 20
      elseif  ((j .gt. 0) .and. (k .eq. -j)
     .        .or.(j .lt. 0) .and. (k .eq. -j)) then
         msq(1,j,k)=2d0*cf*sub16_2(qq)*msq16_2(j,k)
         msq(2,j,k)=2d0*cf*sub26_1(qq)*msq26_1(j,k)
      elseif ((j .ne. 0) .and. (k .eq. 0)) then
         msq(2,j,k)=2d0*tr*sub26_1(qg)*msq26_1(j,-j)
         if(frag) then 
            msq(3,j,k)=Q(j)**2*sub56_2*msq56_2(j,k) 
         endif
      elseif ((j .eq. 0) .and. (k .ne. 0)) then
         msq(1,j,k)=2d0*tr*sub16_2(qg)*msq16_2(-k,k)         
         if(frag) then 
            msq(3,j,k)=Q(k)**2*sub56_2*msq56_2(j,k) 
         endif
         
      endif
 20   continue

      enddo
      enddo

   
      return      
      end


