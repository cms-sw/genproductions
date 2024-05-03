
      subroutine qqb_dm_monojet_nf_ax(p,i1,i2,i3,i4,i5,qgqb)
      implicit none 
      include 'constants.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f'
      include 'sprods_com.f'
      include 'qcdcouple.f'
      logical first 
      data first /.true./
      save first
    
      double precision p(mxpart,4),q(mxpart,4)
      integer i1,i2,i3,i4,i5
      double precision qgqb(2)
      double complex amp_tree(2,2,2,2),amp(2,2,2,2) 
      integer h1,h2,h3,h4
      double complex L1
      double precision bp,beta,s34,s123
      double complex sc123
!========= amplitudes for the axial part of q(i1)+g(i2)+qb(i3)+x(i4)+~x(i5) (triange loop) 
      integer j 
      double precision nffac
      qgqb(:)=0d0 
      amp(:,:,:,:)=czip
      if(xmass.gt.1d-8) then 
!--------- generate massless phase space 
      call gen_masslessvecs(p,q,i4,i5)
!--------- generate spinors 
      call spinoru(5,q,za,zb)
      else
!-------- massless dm can use usual spinoru
         call spinoru(5,p,za,zb)          
      endif

!bp = 1/2(1+beta) 
! beta = dsqrt(1-4xmass**2/s34) 
      s34=Dble(za(i4,i5)*zb(i5,i4))
      beta=dsqrt(1d0-4d0*xmass**2/s34) 
      bp=0.5d0*(one+beta)

      sc123=Dcmplx(s(i1,i2)+s(i2,i3)+s(i3,i1),0d0)
      s123=s(i1,i2)+s(i2,i3)+s(i3,i1)
!====== amplitudes : helicity conserving 
      amp(1,1,1,2)=(L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i4)*
     -     zb(i5,i3))/sc123 - 
     -  (2*bp*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i4)*
     -     zb(i5,i3))/sc123  
      amp(1,2,1,2)= (L1(-s(i1,i3),-s123)*za(i1,i4)*zb(i3,i2)*
     -     zb(i5,i2))/sc123 - 
     -  (2*bp*L1(-s(i1,i3),-s123)*za(i1,i4)*zb(i3,i2)*
     -     zb(i5,i2))/sc123
      amp(2,1,1,2)= -((L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i4)*
     -       zb(i5,i1))/sc123)
     -   + (2*bp*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i4)*
     -     zb(i5,i1))/sc123
      amp(2,2,1,2)= -((L1(-s(i1,i3),-s123)*za(i3,i4)*zb(i2,i1)*
     -       zb(i5,i2))/sc123)
     -   + (2*bp*L1(-s(i1,i3),-s123)*za(i3,i4)*zb(i2,i1)*
     -     zb(i5,i2))/sc123
      amp(1,1,2,1)= -((L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i5)*
     -       zb(i4,i3))/sc123)
     -   + (2*bp*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i5)*
     -     zb(i4,i3))/sc123
      amp(2,1,2,1)=(L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i5)*
     -     zb(i4,i1))/sc123 - 
     -  (2*bp*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i5)*
     -     zb(i4,i1))/sc123
      amp(1,2,2,1)= -((L1(-s(i1,i3),-s123)*za(i1,i5)*zb(i3,i2)*
     -       zb(i4,i2))/sc123)
     -   + (2*bp*L1(-s(i1,i3),-s123)*za(i1,i5)*zb(i3,i2)*
     -     zb(i4,i2))/sc123
      amp(2,2,2,1)=  (L1(-s(i1,i3),-s123)*za(i3,i5)*zb(i2,i1)*
     -     zb(i4,i2))/sc123 - 
     -  (2*bp*L1(-s(i1,i3),-s123)*za(i3,i5)*zb(i2,i1)*
     -     zb(i4,i2))/sc123

!======amplitudes : helicity violating 
      amp(1,1,1,1)=-((xmass*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i4)*
     -       zb(i4,i3))/
     -     (sc123*zb(i5,i4)))
     -   - (xmass*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i5)*
     -     zb(i5,i3))/
     -   (sc123*zb(i5,i4))
      amp(1,2,1,1)= -((xmass*L1(-s(i1,i3),-s123)*za(i1,i4)*zb(i3,i2)*
     -       zb(i4,i2))/
     -     (sc123*zb(i5,i4)))
     -   - (xmass*L1(-s(i1,i3),-s123)*za(i1,i5)*zb(i3,i2)*
     -     zb(i5,i2))/
     -   (sc123*zb(i5,i4))
      amp(2,1,1,1)=(xmass*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i4)*
     -     zb(i4,i1))/
     -   (sc123*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i5)*
     -     zb(i5,i1))/
     -   (sc123*zb(i5,i4))
      amp(2,2,1,1)=(xmass*L1(-s(i1,i3),-s123)*za(i3,i4)*zb(i2,i1)*
     -     zb(i4,i2))/
     -   (sc123*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i3),-s123)*za(i3,i5)*zb(i2,i1)*
     -     zb(i5,i2))/
     -   (sc123*zb(i5,i4))
      amp(1,1,2,2)=-((xmass*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i4)*
     -       zb(i4,i3))/
     -     (sc123*za(i4,i5)))
     -   - (xmass*L1(-s(i1,i3),-s123)*za(i1,i2)*za(i2,i5)*
     -     zb(i5,i3))/
     -   (sc123*za(i4,i5))
      amp(2,1,2,2)= (xmass*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i4)*
     -     zb(i4,i1))/
     -   (sc123*za(i4,i5)) + 
     -  (xmass*L1(-s(i1,i3),-s123)*za(i2,i3)*za(i2,i5)*
     -     zb(i5,i1))/
     -   (sc123*za(i4,i5))
      amp(2,2,2,2)=(xmass*L1(-s(i1,i3),-s123)*za(i3,i4)*zb(i2,i1)*
     -     zb(i4,i2))/
     -   (sc123*za(i4,i5)) + 
     -  (xmass*L1(-s(i1,i3),-s123)*za(i3,i5)*zb(i2,i1)*
     -     zb(i5,i2))/
     -   (sc123*za(i4,i5))

      call qqb_dm_monojet_Axamps(p,i1,i2,i3,i4,i5,amp_tree) 

!---- bulid amplitudes (sum over nf) 
      do h1=1,2
         do h2=1,2
            do h3=1,2
               do h4=1,2
                  qgqb(h1)=qgqb(h1)-
     & ason2pi/xn*Dble(Dconjg(amp_tree(h1,h2,h3,h4))*amp(h1,h2,h3,h4))
               enddo
            enddo
         enddo
      enddo

      nffac=0d0
!---- now sum over active flavors 
      do j=1,nf 
        if(first) then 
           first=.false. 
           call check_dmAxC
        endif 
        nffac=nffac+0.5d0*(dmL(j)-dmR(j))
      enddo

      
      qgqb(1)=nffac*qgqb(1)
      qgqb(2)=nffac*qgqb(2)
      
      return 
      end 
