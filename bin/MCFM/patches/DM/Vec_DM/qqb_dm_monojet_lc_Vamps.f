      subroutine qqb_dm_monojet_lc_Vamps(p,i1,i2,i3,i4,i5,amp) 
      implicit none
      include 'constants.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f' 
      include 'sprods_com.f' 
      include 'scale.f'
      include 'epinv.f'
!----- LEADING COLOR 
!----- fills amplitude for q g qb chi,chib 
      double complex amp(2,2,2,2),Vamp(2,2,2,2),Famp(2,2,2,2) 
      double complex amp_tree(2,2,2,2) 
      double precision p(mxpart,4),q(mxpart,4) 
      integer i1,i2,i3,i4,i5 
      integer h1,h2,h3,h4
!      double complex l12,l23,L0,Lsm1,L1
      double complex Lsm1,L1,L0
      double complex lnrat
      double complex vfac(2,2)

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

!------ basis integrals 
!      l12=lnrat(musq,-s(i1,i2))
!      l23=lnrat(musq,-s(i2,i3))

!----- use same notation as Born 
!----- split into Vamp and Famp with amp=Vamp+Famp 

!------ Vamp piece universial 
!      vfac= 
!     & -(epinv**2+epinv*l12+0.5d0*l12**2)
!     & -(epinv**2+epinv*l23+0.5d0*l23**2)
!     & -2d0*(epinv+l23)-4d0      
!     & +0.5d0*(epinv+l23)+1d0

      vfac(2,2)=  -3d0 - 2*epinv**2 - epinv*lnrat(musq,-s(i1,i2)) - 
     -  0.5*lnrat(musq,-s(i1,i2))**2 - 
     -  epinv*lnrat(musq,-s(i2,i3)) - 
     -  0.5*lnrat(musq,-s(i2,i3))**2 - 
     -  1.5*(epinv + lnrat(musq,-s(i2,i3)))
      vfac(2,1)= -3d0 - 2*epinv**2 - epinv*lnrat(musq,-s(i1,i2)) - 
     -  0.5*lnrat(musq,-s(i1,i2))**2 - 
     -  1.5*(epinv + lnrat(musq,-s(i1,i2))) - 
     -  epinv*lnrat(musq,-s(i2,i3)) - 
     -  0.5*lnrat(musq,-s(i2,i3))**2
      vfac(1,2)=-3d0 - 2*epinv**2 - epinv*lnrat(musq,-s(i1,i2)) - 
     -  0.5*lnrat(musq,-s(i1,i2))**2 - 
     -  1.5*(epinv + lnrat(musq,-s(i1,i2))) - 
     -  epinv*lnrat(musq,-s(i2,i3)) - 
     -     0.5*lnrat(musq,-s(i2,i3))**2
      vfac(1,1)=-3d0 - 2*epinv**2 - epinv*lnrat(musq,-s(i1,i2)) - 
     -  0.5*lnrat(musq,-s(i1,i2))**2 - 
     -  epinv*lnrat(musq,-s(i2,i3)) - 
     -  0.5*lnrat(musq,-s(i2,i3))**2 - 
     -  1.5*(epinv + lnrat(musq,-s(i2,i3)))
    
      call qqb_dm_monojet_Vamps(p,i1,i2,i3,i4,i5,amp_tree)
      do h1=1,2
         do h2=1,2 
            do h3=1,2 
               do h4=1,2 
                  
                  Vamp(h1,h2,h3,h4)=amp_tree(h1,h2,h3,h4)*vfac(h1,h2)
               enddo
            enddo
         enddo
      enddo

!      write(6,*) 'amp_tree ',amp_tree(2,2,1,2)/s(i4,i5)
!      call A51(i1,i2,i3,i4,i5,za,zb) 
!      write(6,*) ' my imp ', Vamp(2,2,1,2)/s(i4,i5)
    
     

!      pause 
!------ F pieces ==

!===== helicity conserving amplitudes 
      Famp(1,1,1,2)= -(L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -      za(i1,i2)*za(i1,i4)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)) - (Lsm1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i2,i4)*
     -     zb(i5,i3))/zb(i2,i1) - 
     -  (L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i5,i3))/
     -   (zb(i2,i1)*zb(i3,i2)) - 
     -  (Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i5,i3))/
     -   (zb(i2,i1)*zb(i3,i2)) - 
     -  (L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i3,i1)**2*zb(i5,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2))
      Famp(1,2,1,2)= -((Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -        -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -       za(i1,i4)*zb(i5,i2))/za(i2,i3)) - 
     -  (L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)) - 
     -  (Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)) - 
     -  (L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i4)*zb(i3,i1)*zb(i5,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) - (L1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i1,i3)**2*
     -     za(i2,i4)*zb(i3,i2)*zb(i5,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3))
      Famp(2,1,1,2)= (Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i4)*zb(i5,i1))/zb(i3,i2) + 
     -  (L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)) + 
     -  (Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)) + 
     -  (L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i3,i1)**2*zb(i5,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)) + (L1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i2,i3)*
     -     za(i3,i4)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2))
      Famp(2,2,1,2)=(L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)) + 
     -  (Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)) + 
     -  (L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i4)*zb(i2,i1)*zb(i5,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) + (L1(-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i1,i3)**2*
     -     za(i3,i4)*zb(i3,i1)*zb(i5,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) + (Lsm1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i3,i4)*
     -     zb(i5,i2))/za(i1,i2)

      Famp(1,1,2,1)=-(L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -      za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)) - (Lsm1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i2,i5)*
     -     zb(i4,i3))/zb(i2,i1) - 
     -  (L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i4,i3))/
     -   (zb(i2,i1)*zb(i3,i2)) - 
     -  (Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i4,i3))/
     -   (zb(i2,i1)*zb(i3,i2)) - 
     -  (L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i3,i1)**2*zb(i4,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2))
      Famp(1,2,2,1)=-((Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -        -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -       za(i1,i5)*zb(i4,i2))/za(i2,i3)) - 
     -  (L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)) - 
     -  (Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)) - 
     -  (L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i5)*zb(i3,i1)*zb(i4,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) - (L1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i1,i3)**2*
     -     za(i2,i5)*zb(i3,i2)*zb(i4,i3))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3))
      Famp(2,1,2,1)=(Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i5)*zb(i4,i1))/zb(i3,i2) + 
     -  (L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i4,i1))/
     -   (zb(i2,i1)*zb(i3,i2)) + 
     -  (Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i4,i1))/
     -   (zb(i2,i1)*zb(i3,i2)) + 
     -  (L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i3,i1)**2*zb(i4,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)) + (L1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i2,i3)*
     -     za(i3,i5)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2))
      Famp(2,2,2,1)=(L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i4,i1))/
     -   (za(i1,i2)*za(i2,i3)) + 
     -  (Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i4,i1))/
     -   (za(i1,i2)*za(i2,i3)) + 
     -  (L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i5)*zb(i2,i1)*zb(i4,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) + (L1(-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i1,i3)**2*
     -     za(i3,i5)*zb(i3,i1)*zb(i4,i1))/
     -   (2.*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)) + (Lsm1(-s(i1,i2),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i2,i3),
     -      -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i3,i5)*
     -     zb(i4,i2))/za(i1,i2)

!=========== Helicity violating amplitudes 
      
      Famp(1,1,1,1)=(xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) -
     &     s(i2,i3))*
     -     za(i1,i2)*za(i1,i4)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i4)*zb(i4,i3))/(zb(i2,i1)*zb(i5,i4)) + 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i3,i1)**2*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i5)*zb(i5,i3))/(zb(i2,i1)*zb(i5,i4)) - 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4))
      Famp(1,2,1,1)=(xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3)
     &     - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i4,i2))/(za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i4)*zb(i3,i2)*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i5,i2))/(za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i5)*zb(i3,i2)*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4))
      Famp(2,1,1,1)=-((xmass*Lsm1(-s(i2,i3),
     -        -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i1,i2),
     -        -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i2,i4)*
     -       zb(i4,i1))/(zb(i3,i2)*zb(i5,i4))) - 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i3,i1)**2*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i3)*za(i3,i4)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i5)*zb(i5,i1))/(zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i3,i1)**2*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i3)*za(i3,i5)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*
     -     zb(i3,i2)*zb(i5,i4))
      Famp(2,2,1,1)=-((xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3)
     &     - s(i2,i3))*
     -       za(i1,i3)*za(i3,i4)*zb(i4,i1))/
     -     (za(i1,i2)*za(i2,i3)*zb(i5,i4))) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i4,i1))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i4)*zb(i2,i1)*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i4,i2))/(za(i1,i2)*zb(i5,i4)) + 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i5)*zb(i2,i1)*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*zb(i5,i4)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i5,i2))/(za(i1,i2)*zb(i5,i4))
      
      Famp(1,1,2,2)=-(xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) 
     &     - s(i2,i3))*
     -      za(i1,i2)*za(i1,i4)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i4)*zb(i4,i3))/(za(i4,i5)*zb(i2,i1)) - 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i3,i1)**2*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i5)*zb(i5,i3))/(za(i4,i5)*zb(i2,i1)) + 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) 
      Famp(1,2,2,2)=-((xmass*Lsm1(-s(i2,i3),
     -        -s(i1,i2) - s(i1,i3) - s(i2,i3),-s(i1,i2),
     -        -s(i1,i2) - s(i1,i3) - s(i2,i3))*za(i1,i4)*
     -       zb(i4,i2))/(za(i2,i3)*za(i4,i5))) - 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i4)*zb(i4,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i4)*zb(i3,i1)*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i4)*zb(i3,i2)*zb(i4,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i5)*zb(i5,i2))/(za(i2,i3)*za(i4,i5)) + 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i1,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i1,i5)*zb(i3,i1)*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i5)*zb(i3,i2)*zb(i5,i3))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5))
      Famp(2,1,2,2)=(xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) 
     &     - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i4)*zb(i4,i1))/(za(i4,i5)*zb(i3,i2)) + 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i3,i1)**2*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) + 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i3)*za(i3,i4)*zb(i3,i1)**2*zb(i4,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i5)*zb(i5,i1))/(za(i4,i5)*zb(i3,i2)) - 
     -  (xmass*L0(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*Lsm1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (za(i4,i5)*zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i3,i1)**2*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*L1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i2,i3)*za(i3,i5)*zb(i3,i1)**2*zb(i5,i2))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i4,i5)*
     -     zb(i2,i1)*zb(i3,i2))
      Famp(2,2,2,2)=(xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) 
     &     - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i4,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i4)*zb(i4,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i4)*zb(i2,i1)*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) + 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i3,i4)*zb(i3,i1)*zb(i4,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) + 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i4)*zb(i4,i2))/(za(i1,i2)*za(i4,i5)) - 
     -  (xmass*L0(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)*za(i3,i5)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i4,i5)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i2,i5)*zb(i2,i1)*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) - 
     -  (xmass*L1(-s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i1,i3)**2*za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (2d0*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i2,i3)*za(i4,i5)) - 
     -  (xmass*Lsm1(-s(i1,i2),-s(i1,i2) - s(i1,i3) - s(i2,i3),
     -      -s(i2,i3),-s(i1,i2) - s(i1,i3) - s(i2,i3))*
     -     za(i3,i5)*zb(i5,i2))/(za(i1,i2)*za(i4,i5))


!----- bulid final amplitude 
      do h1=1,2
         do h2=1,2 
            do h3=1,2 
               do h4=1,2 
                  amp(h1,h2,h3,h4)=Vamp(h1,h2,h3,h4)+Famp(h1,h2,h3,h4) 
               enddo
            enddo
         enddo
      enddo

!      call A51(i1,i2,i3,i4,i5,za,zb) 

!      write(6,*) 'my V*tree ',Vamp(2,2,1,2)/s(i4,i5)
!      write(6,*) 'my F ',Famp(2,2,1,2)/s(i4,i5) 
!3      pause
      
      return 
      end 
