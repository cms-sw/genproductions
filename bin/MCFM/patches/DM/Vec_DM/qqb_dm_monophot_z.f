      subroutine qqb_dm_monophot_z(p,z)
      implicit none
      include 'constants.f'
      include 'qcdcouple.f'
      include 'scale.f'
      include 'PR_new.f'
      include 'agq.f'
      integer is
      double precision z,xl12,p(mxpart,4),dot,ii_qq,ii_qg,tempqq,tempqg

      xl12=log(two*dot(p,1,2)/musq)
c----contributions for one leg

      do is=1,3
      tempqq=+ason2pi*cf*ii_qq(z,xl12,is)
      tempqg=+ason2pi*tr*ii_qg(z,xl12,is)

      Q1(q,q,a,is)=tempqq
      Q2(a,a,q,is)=tempqq
      Q1(a,a,q,is)=tempqq
      Q2(q,q,a,is)=tempqq

      Q2(q,g,q,is)=tempqg
      Q2(a,g,q,is)=tempqg
      Q2(q,g,a,is)=tempqg
      Q2(a,g,a,is)=tempqg
      Q1(q,g,q,is)=tempqg
      Q1(a,g,q,is)=tempqg
      Q1(q,g,a,is)=tempqg
      Q1(a,g,a,is)=tempqg
      enddo
      return
      end
