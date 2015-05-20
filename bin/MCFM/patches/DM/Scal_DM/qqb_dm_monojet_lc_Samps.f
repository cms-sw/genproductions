

      subroutine qqb_dm_monojet_lc_Samps(p,i1,i2,i3,i4,i5,amp) 
      implicit none
      include 'constants.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f' 
      include 'scale.f'
      include 'epinv.f'
!----- LEADING COLOR 
!----- fills amplitude for q g qb chi,chib 
      double complex amp(2,2,2,2) 
      double complex amp_tree(2,2,2,2) 
      double precision p(mxpart,4),q(mxpart,4) 
      integer i1,i2,i3,i4,i5 
      integer h1,h2,h3,h4
      double complex Lsm1
      double precision s23,s123,s34
      double complex lnrat
      double precision s(mxpart,mxpart)
      double complex vfac,ffac,rat(2,2),amp_dec(2,2) 
      integer j,k
      double precision uvsub
      double precision s13,beta,bp
      
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
      s13=s(i1,i2) ! s13 = s_qg 
      s23=s(i2,i3) ! s23 = s_gqb
!------ univerisal v and f pieces
      vfac=-2d0*epinv**2+epinv*(-lnrat(musq,-s13) 
     &     - lnrat(musq,-s23))
     & -0.5d0*(lnrat(musq,-s13)**2+lnrat(musq,-s23)**2)

      ffac=-Lsm1(-s23,-s123,-s13,-s123)
!----- additional uv-counterterm from vertex renorm 
      uvsub=-1.5d0*epinv

!------helicity dependent rational pieces 
      
      rat(1,1)=0.5d0*(s13+s23)/(zb(i2,i1)*zb(i3,i2))
      rat(2,2)=0.5d0*(s13+s23)/(za(i2,i1)*za(i3,i2))
      rat(1,2)=czip
      rat(2,1)=czip

!====== scalar decay 
      s34=Dble(za(i4,i5)*zb(i5,i4))
      beta=dsqrt(1d0-4d0*xmass**2/s34) 
      bp=0.5d0*(one+beta)
      
      call dm_scal_decay(i4,i5,za,zb,bp,amp_dec) 
!======= tree (note swap on gluon quark in these amplitudes) 
      call qqb_dm_monojet_Samps(p,i1,i2,i3,i4,i5,amp_tree) 
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
