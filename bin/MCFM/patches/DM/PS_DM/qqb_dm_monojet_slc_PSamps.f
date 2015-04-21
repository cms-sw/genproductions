

      subroutine qqb_dm_monojet_slc_PSamps(p,i1,i2,i3,i4,i5,amp) 
      implicit none
      include 'constants.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f' 
      include 'scale.f'
      include 'epinv.f'
!----- SubLEADING COLOR 
!----- fills amplitude for q qb g chi,chib 
      double complex amp(2,2,2,2) 
      double complex amp_tree(2,2,2,2) 
      double precision p(mxpart,4),q(mxpart,4) 
      integer i1,i2,i3,i4,i5 
      integer h1,h2,h3,h4
      double complex Lsm1
      double precision s12,s23,s123,s34
      double complex lnrat
      double precision s(mxpart,mxpart)
      double complex vfac,ffac,rat(2,2),amp_dec(2,2) 
      integer j,k
      double precision bp,beta
      double precision uvsub
      double precision s13


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
      s(:,:)=0d0
      do j=1,5 
         do k=1,5
            s(j,k)=Dble(za(j,k)*zb(k,j))
         enddo
      enddo

!------ basis integrals 
!      l12=lnrat(musq,-s(i1,i2))
!      l23=lnrat(musq,-s(i2,i3))

      s123=s(i1,i2)+s(i2,i3)+s(i1,i3)
      s12=s(i1,i2) ! s12 = s_qqb
      s13=s(i1,i3) 
      s23=s(i2,i3)
!------ univerisal v and f pieces
      vfac=-epinv**2-(epinv*lnrat(musq,-s12))
     &-(0.5d0*lnrat(musq,-s12)**2)
      ffac=-Lsm1(-s23,-s123,-s12,-s123)-Lsm1(-s13,-s123,-s12,-s123)
      uvsub=-1.5d0*epinv
!------helicity dependent rational pieces 
      
      rat(1,1)=0.5d0*(s13+s23)/(zb(i3,i2)*zb(i1,i3))
      rat(2,2)=0.5d0*(s13+s23)/(za(i3,i2)*za(i1,i3))
      rat(1,2)=czip
      rat(2,1)=czip
      
!      write(6,*) 'sub'
!      write(6,*) atreet,cdabs(rt2*atreet) 
!      write(6,*) vfac+ffac,cdabs(two*(vfac+ffac))
!      write(6,*) rat(1,1)*rt2*two,cdabs(rat(1,1)*rt2*two)
!      write(6,*) cdabs(rt2*two*((vfac+ffac)*atreet+rat(1,1)))
!      pause 

!====== scalar decay 
      s34=Dble(za(i4,i5)*zb(i5,i4))
      beta=dsqrt(1d0-4d0*xmass**2/s34) 
      bp=0.5d0*(one+beta)
      
      call dm_Pscal_decay(i4,i5,za,zb,bp,amp_dec) 
!======= tree (note swap on gluon quark in these amplitudes) 
      call qqb_dm_monojet_PSamps(p,i1,i3,i2,i4,i5,amp_tree) 
     
      do h1=1,2 
         do h2=1,2 
            do h3=1,2 
               do h4=1,2 
               amp(h1,h2,h3,h4)=(vfac+ffac+uvsub)*amp_tree(h1,h2,h3,h4)
     &                 +rat(h1,h2)*amp_dec(h3,h4) 
               enddo
            enddo
         enddo
      enddo
      
      return 
      end 
