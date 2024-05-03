      subroutine gg_dm_monojet_gs(p,msq)
************************************************************************
*     Author: J.M. Campbell                                            *
*     March, 2003.                                                      *
************************************************************************
c---Matrix element SUBTRACTION squared averaged over initial colors and spins
c     g(-p1)+g(-p2) -->  g+ parton(p5) + parton(p6)
c                           |
c                            -->b(p3)+bbar(p4)

      implicit none 
      include 'constants.f'
      include 'ptilde.f'
      include 'qqgg.f'
      include 'nflav.f'
      integer j,k,nd
c --- remember: nd will count the dipoles
      
      double precision p(mxpart,4),msq(maxd,-nf:nf,-nf:nf)
      double precision 
     & msq15_2(-nf:nf,-nf:nf),msq25_1(-nf:nf,-nf:nf),
     & msq16_2(-nf:nf,-nf:nf),msq26_1(-nf:nf,-nf:nf),
     & msq15_6(-nf:nf,-nf:nf),msq26_5(-nf:nf,-nf:nf),
     & msq16_5(-nf:nf,-nf:nf),msq25_6(-nf:nf,-nf:nf),
     & msq56_1v(-nf:nf,-nf:nf),msq56_2v(-nf:nf,-nf:nf),
     & msq26_5v(-nf:nf,-nf:nf),msq26_1v(-nf:nf,-nf:nf),
     & msq15_6v(-nf:nf,-nf:nf),msq16_2v(-nf:nf,-nf:nf),
     & msq16_5v(-nf:nf,-nf:nf),msq25_6v(-nf:nf,-nf:nf),
     & msq15_2v(-nf:nf,-nf:nf),msq25_1v(-nf:nf,-nf:nf),
     & dummy(-nf:nf,-nf:nf),
     & sub15_2(4),sub25_1(4),sub16_2(4),sub26_1(4),
     & sub15_6(4),sub16_5(4),sub25_6(4),sub26_5(4),
     & sub56_1(4),sub56_2(4),sub56_1v,sub56_2v,
     & sub26_5v,sub26_1v,sub16_5v,sub16_2v,sub15_2v,sub15_6v,sub25_6v,
     & sub25_1v
      external gg_dm_monojet,gg_dm_monojet_gvec
      ndmax=6

c--- calculate all the initial-initial dipoles
      call dips(1,p,1,5,2,sub15_2,sub15_2v,msq15_2,msq15_2v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(2,p,2,5,1,sub25_1,sub25_1v,msq25_1,msq25_1v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(3,p,1,6,2,sub16_2,sub16_2v,msq16_2,msq16_2v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(4,p,2,6,1,sub26_1,sub26_1v,msq26_1,msq26_1v,
     . gg_dm_monojet,gg_dm_monojet_gvec)

c--- now the basic initial final ones
      call dips(5,p,1,5,6,sub15_6,sub15_6v,msq15_6,msq15_6v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
c--- called for final initial the routine only supplies new values for
c--- sub... and sub...v and msqv
      call dips(5,p,5,6,1,sub56_1,sub56_1v,dummy,msq56_1v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(5,p,1,6,5,sub16_5,sub16_5v,msq16_5,msq16_5v,
     . gg_dm_monojet,gg_dm_monojet_gvec)

      call dips(6,p,2,6,5,sub26_5,sub26_5v,msq26_5,msq26_5v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(6,p,5,6,2,sub56_2,sub56_2v,dummy,msq56_2v,
     . gg_dm_monojet,gg_dm_monojet_gvec)
      call dips(6,p,2,5,6,sub25_6,sub25_6v,msq25_6,msq25_6v,
     . gg_dm_monojet,gg_dm_monojet_gvec)

      do j=-nf,nf
      do k=-nf,nf      
      do nd=1,ndmax
        msq(nd,j,k)=0d0
      enddo
      enddo
      enddo


      do j=-nf,nf
      do k=-nf,nf
c--- do only q-qb and qb-q cases      
      if (  ((j .gt. 0).and.(k .lt. 0))
     . .or. ((j .lt. 0).and.(k .gt. 0))) then
      msq(1,j,k)=-msq15_2(j,k)*sub15_2(qq)/xn
      msq(2,j,k)=-msq25_1(j,k)*sub25_1(qq)/xn
      msq(3,j,k)=-msq16_2(j,k)*sub16_2(qq)/xn
      msq(4,j,k)=-msq26_1(j,k)*sub26_1(qq)/xn
      msq(5,j,k)=xn*(
     .  +msq15_6(j,k)*(sub15_6(qq)+0.5d0*sub56_1(gg))
     .  +0.5d0*msq56_1v(j,k)*sub56_1v
     .  +msq16_5(j,k)*(sub16_5(qq)+0.5d0*sub56_1(gg))
     .  +0.5d0*msq56_1v(j,k)*sub56_1v)
      msq(6,j,k)=xn*(
     .  (msq26_5(j,k)*(sub26_5(qq)+0.5d0*sub56_2(gg))
     .   +0.5d0*msq56_2v(j,k)*sub56_2v)
     . +(msq25_6(j,k)*(sub25_6(qq)+0.5d0*sub56_2(gg))
     .   +0.5d0*msq56_2v(j,k)*sub56_2v))

c--- note statistical factor of one half for two gluons in the final state
      do nd=1,ndmax
        msq(nd,j,k)=half*msq(nd,j,k)
      enddo

      elseif ((k .eq. 0).and. (j .ne. 0)) then
c--- q-g and qb-g cases
      msq(1,j,k)=(aveqg/avegg)*(
     . msq15_2(0,0)*sub15_2(gq)+msq15_2v(0,0)*sub15_2v)
      msq(2,j,k)=2d0*tr*(msq25_1(j,-5)+msq25_1(j,-4)+msq25_1(j,-3)
     .                  +msq25_1(j,-2)+msq25_1(j,-1)+msq25_1(j,+1)
     .                  +msq25_1(j,+2)+msq25_1(j,+3)+msq25_1(j,+4)
     .                  +msq25_1(j,+5))*sub25_1(qg)
      msq(3,j,k)=xn*msq16_2(j,k)*sub16_2(qq)
      msq(4,j,k)=xn*(msq26_1(j,k)*sub26_1(gg)+msq26_1v(j,k)*sub26_1v)
      msq(5,j,k)=-msq16_5(j,k)*(sub16_5(qq)+sub56_1(qq))/xn
      msq(6,j,k)=xn*(msq26_5(j,k)*sub26_5(gg)+msq26_5v(j,k)*sub26_5v
     .              +msq26_5(j,k)*sub56_2(qq))

      elseif ((j .eq. 0).and.(k.ne.0)) then
c--- g-q and g-qb cases
      msq(1,j,k)=2d0*tr*(msq15_2(-5,k)+msq15_2(-4,k)+msq15_2(-3,k)
     .                  +msq15_2(-2,k)+msq15_2(-1,k)+msq15_2(+1,k)
     .                  +msq15_2(+2,k)+msq15_2(+3,k)+msq15_2(+4,k)
     .                  +msq15_2(+5,k))*sub15_2(qg)
      msq(2,j,k)=(aveqg/avegg)*(
     . msq25_1(0,0)*sub25_1(gq)+msq25_1v(0,0)*sub25_1v)
      msq(3,j,k)=xn*(msq16_2(j,k)*sub16_2(gg)+msq16_2v(j,k)*sub16_2v)
      msq(4,j,k)=xn*msq26_1(j,k)*sub26_1(qq)
      msq(5,j,k)=xn*(msq16_5(j,k)*sub16_5(gg)+msq16_5v(j,k)*sub16_5v
     .              +msq16_5(j,k)*sub56_1(qq))
      msq(6,j,k)=-msq26_5(j,k)*(sub26_5(qq)+sub56_2(qq))/xn

      elseif ((j .eq. 0).and.(k .eq. 0)) then
c--- g-g case
c--- first set of subtractions take care of gg->qbq, second set gg->gg;
c--- note g,g = 1,2 and qb=5, q=6 so (15),(25)-->q and (16),(26)-->qb
      msq(1,j,k)=(msq15_2(+1,k)+msq15_2(+2,k)+msq15_2(+3,k)
     .           +msq15_2(+4,k)+msq15_2(+5,k))*sub15_2(qg)*2d0*tr
      msq(2,j,k)=(msq25_1(k,+1)+msq25_1(k,+2)+msq25_1(k,+3)
     .           +msq25_1(k,+4)+msq25_1(k,+5))*sub25_1(qg)*2d0*tr
      msq(3,j,k)=(msq16_2(-5,k)+msq16_2(-4,k)+msq16_2(-3,k)
     .           +msq16_2(-2,k)+msq16_2(-1,k))*sub16_2(qg)*2d0*tr
      msq(4,j,k)=(msq26_1(k,-5)+msq26_1(k,-4)+msq26_1(k,-3)
     .           +msq26_1(k,-2)+msq26_1(k,-1))*sub26_1(qg)*2d0*tr
      msq(5,j,k)=dfloat(nflav)*half*(
     .+msq16_5(j,k)*sub56_1(gq)-msq56_1v(j,k)*sub56_1v)
      msq(6,j,k)=dfloat(nflav)*half*(
     .+msq26_5(j,k)*sub56_2(gq)-msq56_2v(j,k)*sub56_2v)
      msq(1,j,k)=msq(1,j,k)+half*xn*(
     . msq15_2(j,k)*sub15_2(gg)+msq15_2v(j,k)*sub15_2v)
      msq(2,j,k)=msq(2,j,k)+half*xn*(
     . msq25_1(j,k)*sub25_1(gg)+msq25_1v(j,k)*sub25_1v)
      msq(3,j,k)=msq(3,j,k)+half*xn*(
     . msq16_2(j,k)*sub16_2(gg)+msq16_2v(j,k)*sub16_2v)
      msq(4,j,k)=msq(4,j,k)+half*xn*(
     . msq26_1(j,k)*sub26_1(gg)+msq26_1v(j,k)*sub26_1v)
      msq(5,j,k)=msq(5,j,k)+half*xn*(
     . msq15_6(j,k)*sub15_6(gg)+msq15_6v(j,k)*sub15_6v
     .+msq16_5(j,k)*sub16_5(gg)+msq16_5v(j,k)*sub16_5v
     .+msq16_5(j,k)*sub56_1(gg)+msq56_1v(j,k)*sub56_1v)
      msq(6,j,k)=msq(6,j,k)+half*xn*(
     . msq25_6(j,k)*sub25_6(gg)+msq25_6v(j,k)*sub25_6v
     .+msq26_5(j,k)*sub26_5(gg)+msq26_5v(j,k)*sub26_5v
     .+msq26_5(j,k)*sub56_2(gg)+msq56_2v(j,k)*sub56_2v)
      endif
     
      
      enddo
      enddo
c      endif

c      return
c--- Start of the 4Q contribution

      do j=-nf,nf
      do k=-nf,nf

      if ((j .gt. 0) .and. (k .gt. 0)) then
c--- Q Q - different flavours
        if (j .ne. k) then
        msq(1,j,k)=msq(1,j,k)+(xn-1d0/xn)
     .  *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(4,j,k)=msq(4,j,k)+(xn-1d0/xn)
     .  *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        else
c--- Q Q - same flavours
        msq(1,j,k)=msq(1,j,k)+half*(xn-1d0/xn)
     .  *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(2,j,k)=msq(2,j,k)+half*(xn-1d0/xn)
     .  *(msq25_1(j,0)*sub25_1(gq)+msq25_1v(j,0)*sub25_1v)
        msq(3,j,k)=msq(3,j,k)+half*(xn-1d0/xn)
     .  *(msq16_2(0,k)*sub16_2(gq)+msq16_2v(0,k)*sub16_2v)
        msq(4,j,k)=msq(4,j,k)+half*(xn-1d0/xn)
     .  *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        endif

       elseif ((j .lt. 0).and.(k .lt. 0)) then
        if (j .ne. k) then
c--- QBAR QBAR - different flavours
        msq(1,j,k)=msq(1,j,k)+(xn-1d0/xn)
     .    *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(4,j,k)=msq(4,j,k)+(xn-1d0/xn)
     .    *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        else
c--- QBAR QBAR - same flavours     
        msq(1,j,k)=msq(1,j,k)+half*(xn-1d0/xn)
     .  *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(2,j,k)=msq(2,j,k)+half*(xn-1d0/xn)
     .  *(msq25_1(j,0)*sub25_1(gq)+msq25_1v(j,0)*sub25_1v)
        msq(3,j,k)=msq(3,j,k)+half*(xn-1d0/xn)
     .  *(msq16_2(0,k)*sub16_2(gq)+msq16_2v(0,k)*sub16_2v)
        msq(4,j,k)=msq(4,j,k)+half*(xn-1d0/xn)
     .  *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        endif

      elseif ((j .gt. 0).and.(k .lt. 0)) then
c--- Q QBAR
        if (j .eq. -k) then
        msq(1,j,k)=msq(1,j,k)+(xn-1d0/xn)
     .    *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(4,j,k)=msq(4,j,k)+(xn-1d0/xn)
     .    *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        msq(6,j,k)=msq(6,j,k)+2d0*tr*dfloat(nflav)
     .    *(msq26_5(j,k)*sub56_2(gq)-msq56_2v(j,k)*sub56_2v)
        else 
        msq(1,j,k)=msq(1,j,k)+(xn-1d0/xn)
     .    *(msq15_2(0,k)*sub15_2(gq)+msq15_2v(0,k)*sub15_2v)
        msq(4,j,k)=msq(4,j,k)+(xn-1d0/xn)
     .    *(msq26_1(j,0)*sub26_1(gq)+msq26_1v(j,0)*sub26_1v)
        endif
c--- QBAR Q
      elseif ((j .lt. 0).and.(k .gt. 0)) then
      if (j .eq. -k) then
      msq(2,j,k)=msq(2,j,k)+(xn-1d0/xn)
     .  *(msq25_1(j,0)*sub25_1(gq)+msq25_1v(j,0)*sub25_1v)
      msq(3,j,k)=msq(3,j,k)+(xn-1d0/xn)
     .  *(msq16_2(0,k)*sub16_2(gq)+msq16_2v(0,k)*sub16_2v)
      msq(6,j,k)=msq(6,j,k)+2d0*tr*dfloat(nflav)
     . *(msq26_5(j,k)*sub56_2(gq)-msq56_2v(j,k)*sub56_2v)
      else 
      msq(2,j,k)=msq(2,j,k)+(xn-1d0/xn)
     .  *(msq25_1(j,0)*sub25_1(gq)+msq25_1v(j,0)*sub25_1v)
      msq(3,j,k)=msq(3,j,k)+(xn-1d0/xn)
     .  *(msq16_2(0,k)*sub16_2(gq)+msq16_2v(0,k)*sub16_2v)

      endif
      endif


      enddo
      enddo

      return
      end

