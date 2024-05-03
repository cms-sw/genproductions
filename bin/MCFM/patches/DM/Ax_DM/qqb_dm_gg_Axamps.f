      subroutine qqb_dm_gg_Axamps(p,i1,i2,i3,i4,i5,i6,msq1,msq2,msqsl) 
      implicit none 
      include 'dm_params.f' 
      include 'constants.f'
      include 'zprods_decl.f'
!----- vector amplitude for 
!----- q(i1)+g(i2)+g(i3)+qb(i4)+x(i5)+x(i6) 
      double precision p(mxpart,4) 
!-----fills amplitude for q g qb chi,chib 
      double precision q(mxpart,4)
      integer i1,i2,i3,i4,i5,i6
!------and gluon helicity is summed over 
!------returns amp squared. 
      integer h1,h2,h3,h4,h5
      double complex amp_hconsx1(2,2,2,2,2),amp_hconsx2(2,2,2,2,2)
      double complex amp_hconsQED(2,2,2,2,2)
!------ index refers to helicit of qqb (sum over everyting else) 
      double precision msq1(2),msq2(2),msqsl(2)
           
      if(xmass.gt.1d-8) then 
!--------- generate massless phase space 
      call gen_masslessvecs(p,q,i5,i6)
!--------- generate spinors 
      call spinoru(6,q,za,zb)
      else
!-------- massless dm can use usual spinoru
         call spinoru(6,p,za,zb)       
      endif

!======helicity amplitudes
      call dm_gg_helamps_Ax(i1,i2,i3,i4,i5,i6,za,zb,amp_hconsx1)
!======= other colour ordering, swap gluons 
      call dm_gg_helamps_Ax(i1,i3,i2,i4,i5,i6,za,zb,amp_hconsx2) 
      do h1=1,2
         do h2=1,2
            do h3=1,2
               do h4=1,2 
                  do h5=1,2
               amp_hconsQED(h1,h2,h3,h4,h5)=amp_hconsx1(h1,h2,h3,h4,h5) 
     &                +amp_hconsx2(h1,h3,h2,h4,h5) 
             enddo
          enddo
       enddo
      enddo
      enddo
      
      msq1(:)=0d0 
      msq2(:)=0d0 
      msqsl(:)=0d0 
!------ msq 
      do h1=1,2
         do h2=1,2
            do h3=1,2 
               do h4=1,2
                  do h5=1,2 
              msq1(h1)=msq1(h1)+cdabs(amp_hconsx1(h1,h2,h3,h4,h5))**2
              msq2(h1)=msq2(h1)+cdabs(amp_hconsx2(h1,h2,h3,h4,h5))**2
              msqsl(h1)=msqsl(h1)+cdabs(amp_hconsQED(h1,h2,h3,h4,h5))**2
                  enddo
               enddo
            enddo
         enddo
      enddo
!----- include -1/N_c^2 for subleading piece 
      do h1=1,2
         msqsl(h1)=-one/9d0*msqsl(h1)
      enddo
 
      return 
      end

      
      subroutine dm_gg_helamps_Ax(i1,i2,i3,i4,i5,i6,za,zb,amp) 
      implicit none 
      include 'dm_params.f' 
      include 'constants.f'
      include 'zprods_decl.f'
      include 'sprods_com.f'
!----- vector amplitude for 
!----- q(i1)+g(i2)+g(i3)+qb(i4)+x(i5)+x(i6) 
!      double precision p(mxpart,4) 
!-----fills amplitude for q g qb chi,chib 
      double complex amp(2,2,2,2,2) 
      integer i1,i2,i3,i4,i5,i6
!------and gluon helicity is summed over 
!------returns amp squared. 
      double precision bp,beta,s34
    
         
!     bp = 1/2(1+beta) 
!     beta = dsqrt(1-4xmass**2/s34) 
      s34=s(i5,i6)
      beta=dsqrt(1d0-4d0*xmass**2/s34) 
      bp=0.5d0*(one+beta)


      amp(:,:,:,:,:)=czip
!------- helicity conserving amplitudes       
      amp(2,1,1,1,2)=(za(i2,i6)*zb(i5,i1))/(zb(i3,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i2,i6)*zb(i5,i1))/(zb(i3,i2)*zb(i4,i3)) + 
     -  (za(i3,i6)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i3,i6)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (za(i4,i6)*zb(i4,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i4,i6)*zb(i4,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3))
      amp(2,2,2,1,2)=(za(i1,i4)*za(i4,i6)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (2*bp*za(i1,i4)*za(i4,i6)*zb(i5,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (za(i2,i4)*za(i4,i6)*zb(i5,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (2*bp*za(i2,i4)*za(i4,i6)*zb(i5,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (za(i4,i6)*zb(i5,i3))/(za(i1,i2)*za(i2,i3)) - 
     -  (2*bp*za(i4,i6)*zb(i5,i3))/(za(i1,i2)*za(i2,i3))
      amp(2,1,2,1,2)=-((za(i2,i4)*za(i4,i6)*zb(i3,i1)*zb(i5,i1))/
     -     (s(i2,i3)*za(i3,i4)*zb(i2,i1))) + 
     -  (2*bp*za(i2,i4)*za(i4,i6)*zb(i3,i1)*zb(i5,i1))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (za(i1,i2)*za(i4,i6)*zb(i3,i1)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (2*bp*za(i1,i2)*za(i4,i6)*zb(i3,i1)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (za(i2,i4)**2*za(i2,i6)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (2*bp*za(i2,i4)**2*za(i2,i6)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (za(i2,i4)**2*za(i4,i6)*zb(i4,i3)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (2*bp*za(i2,i4)**2*za(i4,i6)*zb(i4,i3)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (za(i2,i3)*za(i4,i6)*zb(i3,i1)**2*zb(i5,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (2*bp*za(i2,i3)*za(i4,i6)*zb(i3,i1)**2*zb(i5,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1))
      amp(2,2,1,1,2)=(za(i1,i3)**2*za(i4,i6)*zb(i2,i1)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (2*bp*za(i1,i3)**2*za(i4,i6)*zb(i2,i1)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (za(i1,i3)*za(i3,i6)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i1,i3)*za(i3,i6)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (za(i1,i3)*za(i4,i6)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i1,i3)*za(i4,i6)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (za(i3,i4)*za(i3,i6)*zb(i3,i2)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (2*bp*za(i3,i4)*za(i3,i6)*zb(i3,i2)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (za(i3,i4)*za(i4,i6)*zb(i4,i2)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (2*bp*za(i3,i4)*za(i4,i6)*zb(i4,i2)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (za(i1,i3)*za(i2,i3)*za(i4,i6)*zb(i2,i1)*zb(i5,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (2*bp*za(i1,i3)*za(i2,i3)*za(i4,i6)*zb(i2,i1)*zb(i5,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (za(i2,i3)*za(i3,i6)*zb(i3,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i2,i3)*za(i3,i6)*zb(i3,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (za(i2,i3)*za(i4,i6)*zb(i4,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i2,i3)*za(i4,i6)*zb(i4,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3))
!------ current flip : quarks 
      amp(1,1,1,1,2)=(za(i3,i6)*zb(i5,i4))/(zb(i2,i1)*zb(i3,i2)) - 
     -  (2*bp*za(i3,i6)*zb(i5,i4))/(zb(i2,i1)*zb(i3,i2)) + 
     -  (za(i1,i6)*zb(i4,i1)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i1,i6)*zb(i4,i1)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (za(i2,i6)*zb(i4,i2)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i2,i6)*zb(i4,i2)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3))
      amp(1,1,2,1,2)= -((za(i1,i6)*za(i2,i3)*zb(i3,i1)*zb(i5,i3))/
     -     (s(i2,i3)*za(i3,i4)*zb(i2,i1))) + 
     -  (2*bp*za(i1,i6)*za(i2,i3)*zb(i3,i1)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (za(i2,i3)*za(i2,i6)*zb(i3,i2)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (2*bp*za(i2,i3)*za(i2,i6)*zb(i3,i2)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (za(i1,i6)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i5,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (2*bp*za(i1,i6)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i5,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (za(i1,i6)*za(i2,i4)*zb(i3,i1)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (2*bp*za(i1,i6)*za(i2,i4)*zb(i3,i1)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (za(i1,i2)*za(i1,i6)*zb(i3,i1)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (2*bp*za(i1,i2)*za(i1,i6)*zb(i3,i1)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (za(i2,i4)*za(i2,i6)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (2*bp*za(i2,i4)*za(i2,i6)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (za(i1,i2)*za(i2,i6)*zb(i3,i1)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (2*bp*za(i1,i2)*za(i2,i6)*zb(i3,i1)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (za(i1,i6)*za(i2,i4)**2*zb(i4,i3)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (2*bp*za(i1,i6)*za(i2,i4)**2*zb(i4,i3)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4))
      amp(1,2,1,1,2)=-((za(i1,i6)*za(i2,i3)*zb(i4,i2)**2*zb(i5,i2))/
     -     (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3))) + 
     -  (2*bp*za(i1,i6)*za(i2,i3)*zb(i4,i2)**2*zb(i5,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (za(i1,i3)**2*za(i1,i6)*zb(i2,i1)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (2*bp*za(i1,i3)**2*za(i1,i6)*zb(i2,i1)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (za(i1,i3)**2*za(i3,i6)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (2*bp*za(i1,i3)**2*za(i3,i6)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (za(i1,i3)*za(i1,i6)*zb(i4,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i1,i3)*za(i1,i6)*zb(i4,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (za(i1,i6)*za(i3,i4)*zb(i4,i2)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (2*bp*za(i1,i6)*za(i3,i4)*zb(i4,i2)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3))
      amp(1,2,2,1,2)=(za(i1,i6)*zb(i5,i2))/(za(i2,i3)*za(i3,i4)) - 
     -  (2*bp*za(i1,i6)*zb(i5,i2))/(za(i2,i3)*za(i3,i4)) + 
     -  (za(i1,i3)*za(i1,i6)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (2*bp*za(i1,i3)*za(i1,i6)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (za(i1,i4)*za(i1,i6)*zb(i5,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (2*bp*za(i1,i4)*za(i1,i6)*zb(i5,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4))

!----- current flip : dm 
      amp(2,1,1,2,1)=-((za(i2,i5)*zb(i6,i1))/(zb(i3,i2)*zb(i4,i3))) + 
     -  (2*bp*za(i2,i5)*zb(i6,i1))/(zb(i3,i2)*zb(i4,i3)) - 
     -  (za(i3,i5)*zb(i3,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i3,i5)*zb(i3,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (za(i4,i5)*zb(i4,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i4,i5)*zb(i4,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) 
      amp(2,1,2,2,1)=(za(i2,i4)*za(i4,i5)*zb(i3,i1)*zb(i6,i1))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (2*bp*za(i2,i4)*za(i4,i5)*zb(i3,i1)*zb(i6,i1))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (za(i1,i2)*za(i4,i5)*zb(i3,i1)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (2*bp*za(i1,i2)*za(i4,i5)*zb(i3,i1)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (za(i2,i4)**2*za(i2,i5)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (2*bp*za(i2,i4)**2*za(i2,i5)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) - 
     -  (za(i2,i4)**2*za(i4,i5)*zb(i4,i3)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (2*bp*za(i2,i4)**2*za(i4,i5)*zb(i4,i3)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (za(i2,i3)*za(i4,i5)*zb(i3,i1)**2*zb(i6,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (2*bp*za(i2,i3)*za(i4,i5)*zb(i3,i1)**2*zb(i6,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1))
      amp(2,2,1,2,1)=-((za(i1,i3)**2*za(i4,i5)*zb(i2,i1)*zb(i6,i1))/
     -     (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2))) + 
     -  (2*bp*za(i1,i3)**2*za(i4,i5)*zb(i2,i1)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (za(i1,i3)*za(i3,i5)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i1,i3)*za(i3,i5)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (za(i1,i3)*za(i4,i5)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i1,i3)*za(i4,i5)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (za(i3,i4)*za(i3,i5)*zb(i3,i2)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (2*bp*za(i3,i4)*za(i3,i5)*zb(i3,i2)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (za(i3,i4)*za(i4,i5)*zb(i4,i2)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (2*bp*za(i3,i4)*za(i4,i5)*zb(i4,i2)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (za(i1,i3)*za(i2,i3)*za(i4,i5)*zb(i2,i1)*zb(i6,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (2*bp*za(i1,i3)*za(i2,i3)*za(i4,i5)*zb(i2,i1)*zb(i6,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (za(i2,i3)*za(i3,i5)*zb(i3,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i2,i3)*za(i3,i5)*zb(i3,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) + 
     -  (za(i2,i3)*za(i4,i5)*zb(i4,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i2,i3)*za(i4,i5)*zb(i4,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3))
      amp(2,2,2,2,1)=-((za(i1,i4)*za(i4,i5)*zb(i6,i1))/
     -     (za(i1,i2)*za(i2,i3)*za(i3,i4))) + 
     -  (2*bp*za(i1,i4)*za(i4,i5)*zb(i6,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (za(i2,i4)*za(i4,i5)*zb(i6,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (2*bp*za(i2,i4)*za(i4,i5)*zb(i6,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (za(i4,i5)*zb(i6,i3))/(za(i1,i2)*za(i2,i3)) + 
     -  (2*bp*za(i4,i5)*zb(i6,i3))/(za(i1,i2)*za(i2,i3))

!------ current flip : dm and quark 
      amp(1,1,1,2,1)=-((za(i3,i5)*zb(i6,i4))/(zb(i2,i1)*zb(i3,i2))) + 
     -  (2*bp*za(i3,i5)*zb(i6,i4))/(zb(i2,i1)*zb(i3,i2)) - 
     -  (za(i1,i5)*zb(i4,i1)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i1,i5)*zb(i4,i1)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (za(i2,i5)*zb(i4,i2)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) + 
     -  (2*bp*za(i2,i5)*zb(i4,i2)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3))
      amp(1,1,2,2,1)=(za(i1,i5)*za(i2,i3)*zb(i3,i1)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (2*bp*za(i1,i5)*za(i2,i3)*zb(i3,i1)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) + 
     -  (za(i2,i3)*za(i2,i5)*zb(i3,i2)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (2*bp*za(i2,i3)*za(i2,i5)*zb(i3,i2)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (za(i1,i5)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i6,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (2*bp*za(i1,i5)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i6,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (za(i1,i5)*za(i2,i4)*zb(i3,i1)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (2*bp*za(i1,i5)*za(i2,i4)*zb(i3,i1)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (2*bp*za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (za(i2,i4)*za(i2,i5)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (2*bp*za(i2,i4)*za(i2,i5)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)) - 
     -  (za(i1,i2)*za(i2,i5)*zb(i3,i1)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) + 
     -  (2*bp*za(i1,i2)*za(i2,i5)*zb(i3,i1)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)) - 
     -  (za(i1,i5)*za(i2,i4)**2*zb(i4,i3)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)) + 
     -  (2*bp*za(i1,i5)*za(i2,i4)**2*zb(i4,i3)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4))
      amp(1,2,1,2,1)=(za(i1,i5)*za(i2,i3)*zb(i4,i2)**2*zb(i6,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (2*bp*za(i1,i5)*za(i2,i3)*zb(i4,i2)**2*zb(i6,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) - 
     -  (za(i1,i3)**2*za(i1,i5)*zb(i2,i1)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (2*bp*za(i1,i3)**2*za(i1,i5)*zb(i2,i1)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (za(i1,i3)**2*za(i3,i5)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) - 
     -  (2*bp*za(i1,i3)**2*za(i3,i5)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)) + 
     -  (za(i1,i3)*za(i1,i5)*zb(i4,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (2*bp*za(i1,i3)*za(i1,i5)*zb(i4,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)) - 
     -  (za(i1,i5)*za(i3,i4)*zb(i4,i2)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)) + 
     -  (2*bp*za(i1,i5)*za(i3,i4)*zb(i4,i2)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3))
      amp(1,2,2,2,1)=-((za(i1,i5)*zb(i6,i2))/(za(i2,i3)*za(i3,i4))) + 
     -  (2*bp*za(i1,i5)*zb(i6,i2))/(za(i2,i3)*za(i3,i4)) - 
     -  (za(i1,i3)*za(i1,i5)*zb(i6,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (2*bp*za(i1,i3)*za(i1,i5)*zb(i6,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) - 
     -  (za(i1,i4)*za(i1,i5)*zb(i6,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)) + 
     -  (2*bp*za(i1,i4)*za(i1,i5)*zb(i6,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4))


!======= HELICITY VIOLATING AMPLITUDES 
      
!------ 
      amp(1,1,1,1,1)=-((xmass*za(i3,i5)*zb(i5,i4))/
     -     (zb(i2,i1)*zb(i3,i2)*zb(i6,i5))) - 
     -  (xmass*za(i1,i5)*zb(i4,i1)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i2,i5)*zb(i4,i2)*zb(i5,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i3,i6)*zb(i6,i4))/(zb(i2,i1)*zb(i3,i2)*zb(i6,i5)) - 
     -  (xmass*za(i1,i6)*zb(i4,i1)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i2,i6)*zb(i4,i2)*zb(i6,i4))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5))
      amp(1,1,2,1,1)= (xmass*za(i1,i5)*za(i2,i3)*zb(i3,i1)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) + 
     -  (xmass*za(i2,i3)*za(i2,i5)*zb(i3,i2)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i5)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i5,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    + (xmass*za(i1,i5)*za(i2,i4)*zb(i3,i1)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    + (xmass*za(i2,i4)*za(i2,i5)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i2,i5)*zb(i3,i1)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    - (xmass*za(i1,i5)*za(i2,i4)**2*zb(i4,i3)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    + (xmass*za(i1,i6)*za(i2,i3)*zb(i3,i1)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) + 
     -  (xmass*za(i2,i3)*za(i2,i6)*zb(i3,i2)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i6)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i6,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    + (xmass*za(i1,i6)*za(i2,i4)*zb(i3,i1)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i1,i6)*zb(i3,i1)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    + (xmass*za(i2,i4)*za(i2,i6)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i2,i6)*zb(i3,i1)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    - (xmass*za(i1,i6)*za(i2,i4)**2*zb(i4,i3)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
      amp(1,2,1,1,1)=(xmass*za(i1,i5)*za(i2,i3)*zb(i4,i2)**2*zb(i5,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i1,i3)**2*za(i1,i5)*zb(i2,i1)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i1,i3)**2*za(i3,i5)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i1,i3)*za(i1,i5)*zb(i4,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i1,i5)*za(i3,i4)*zb(i4,i2)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    + (xmass*za(i1,i6)*za(i2,i3)*zb(i4,i2)**2*zb(i6,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i1,i3)**2*za(i1,i6)*zb(i2,i1)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i1,i3)**2*za(i3,i6)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i1,i3)*za(i1,i6)*zb(i4,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i1,i6)*za(i3,i4)*zb(i4,i2)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
      amp(2,1,1,1,1)= -((xmass*za(i2,i5)*zb(i5,i1))/
     -     (zb(i3,i2)*zb(i4,i3)*zb(i6,i5))) - 
     -  (xmass*za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i4,i5)*zb(i4,i1)*zb(i5,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i2,i6)*zb(i6,i1))/(zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i3,i6)*zb(i3,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i4,i6)*zb(i4,i1)*zb(i6,i1))/
     -   (zb(i2,i1)*zb(i3,i2)*zb(i4,i3)*zb(i6,i5))
      amp(1,2,2,1,1)=-((xmass*za(i1,i5)*zb(i5,i2))/
     -     (za(i2,i3)*za(i3,i4)*zb(i6,i5))) - 
     -  (xmass*za(i1,i3)*za(i1,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i1,i4)*za(i1,i5)*zb(i5,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i1,i6)*zb(i6,i2))/(za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i1,i3)*za(i1,i6)*zb(i6,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i1,i4)*za(i1,i6)*zb(i6,i4))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5))
      amp(2,1,2,1,1)=(xmass*za(i2,i4)*za(i4,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i4,i5)*zb(i3,i1)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    + (xmass*za(i2,i4)**2*za(i2,i5)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    - (xmass*za(i2,i4)**2*za(i4,i5)*zb(i4,i3)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    + (xmass*za(i2,i3)*za(i4,i5)*zb(i3,i1)**2*zb(i5,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    + (xmass*za(i2,i4)*za(i4,i6)*zb(i3,i1)*zb(i6,i1))/
     -   (s(i2,i3)*za(i3,i4)*zb(i2,i1)*zb(i6,i5)) - 
     -  (xmass*za(i1,i2)*za(i4,i6)*zb(i3,i1)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
     -    + (xmass*za(i2,i4)**2*za(i2,i6)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    - (xmass*za(i2,i4)**2*za(i4,i6)*zb(i4,i3)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*zb(i6,i5))
     -    + (xmass*za(i2,i3)*za(i4,i6)*zb(i3,i1)**2*zb(i6,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*zb(i2,i1)*zb(i6,i5))
      amp(2,2,1,1,1)=-((xmass*za(i1,i3)**2*za(i4,i5)*zb(i2,i1)*
     &     zb(i5,i1))/
     -     (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -       zb(i6,i5))) + (xmass*za(i1,i3)*za(i3,i5)*zb(i3,i2)*
     -     zb(i5,i1))/(s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) + 
     -  (xmass*za(i1,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i3,i4)*za(i3,i5)*zb(i3,i2)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i3,i4)*za(i4,i5)*zb(i4,i2)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i1,i3)*za(i2,i3)*za(i4,i5)*zb(i2,i1)*zb(i5,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i2,i3)*za(i3,i5)*zb(i3,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) + 
     -  (xmass*za(i2,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i1,i3)**2*za(i4,i6)*zb(i2,i1)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i1,i3)*za(i3,i6)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) + 
     -  (xmass*za(i1,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) - 
     -  (xmass*za(i3,i4)*za(i3,i6)*zb(i3,i2)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i3,i4)*za(i4,i6)*zb(i4,i2)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*zb(i4,i3)*zb(i6,i5))
     -    - (xmass*za(i1,i3)*za(i2,i3)*za(i4,i6)*zb(i2,i1)*zb(i6,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*zb(i6,i5))
     -    + (xmass*za(i2,i3)*za(i3,i6)*zb(i3,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5)) + 
     -  (xmass*za(i2,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*zb(i4,i3)*zb(i6,i5))
      amp(2,2,2,1,1)= -((xmass*za(i1,i4)*za(i4,i5)*zb(i5,i1))/
     -     (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5))) - 
     -  (xmass*za(i2,i4)*za(i4,i5)*zb(i5,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i4,i5)*zb(i5,i3))/(za(i1,i2)*za(i2,i3)*zb(i6,i5)) - 
     -  (xmass*za(i1,i4)*za(i4,i6)*zb(i6,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i2,i4)*za(i4,i6)*zb(i6,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*zb(i6,i5)) - 
     -  (xmass*za(i4,i6)*zb(i6,i3))/(za(i1,i2)*za(i2,i3)*zb(i6,i5))

!----- spin flip on dm 
      amp(1,1,1,2,2)=-((xmass*za(i3,i5)*zb(i5,i4))/
     -     (za(i5,i6)*zb(i2,i1)*zb(i3,i2))) - 
     -  (xmass*za(i1,i5)*zb(i4,i1)*zb(i5,i4))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i2,i5)*zb(i4,i2)*zb(i5,i4))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i3,i6)*zb(i6,i4))/(za(i5,i6)*zb(i2,i1)*zb(i3,i2)) - 
     -  (xmass*za(i1,i6)*zb(i4,i1)*zb(i6,i4))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i2,i6)*zb(i4,i2)*zb(i6,i4))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3))
      amp(1,1,2,2,2)= (xmass*za(i1,i5)*za(i2,i3)*zb(i3,i1)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) + 
     -  (xmass*za(i2,i3)*za(i2,i5)*zb(i3,i2)*zb(i5,i3))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i5)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i5,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*za(i5,i6))
     -    + (xmass*za(i1,i5)*za(i2,i4)*zb(i3,i1)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i1,i5)*zb(i3,i1)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i5,i6)*zb(i2,i1))
     -    + (xmass*za(i2,i4)*za(i2,i5)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i2,i5)*zb(i3,i1)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i5,i6)*zb(i2,i1))
     -    - (xmass*za(i1,i5)*za(i2,i4)**2*zb(i4,i3)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*za(i5,i6))
     -    + (xmass*za(i1,i6)*za(i2,i3)*zb(i3,i1)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) + 
     -  (xmass*za(i2,i3)*za(i2,i6)*zb(i3,i2)*zb(i6,i3))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i6)*za(i2,i3)*za(i2,i4)*zb(i4,i3)*zb(i6,i3))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*za(i5,i6))
     -    + (xmass*za(i1,i6)*za(i2,i4)*zb(i3,i1)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i1,i6)*zb(i3,i1)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i5,i6)*zb(i2,i1))
     -    + (xmass*za(i2,i4)*za(i2,i6)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i2,i6)*zb(i3,i1)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i5,i6)*zb(i2,i1))
     -    - (xmass*za(i1,i6)*za(i2,i4)**2*zb(i4,i3)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i3,i4)*za(i5,i6))
      amp(1,2,1,2,2)=(xmass*za(i1,i5)*za(i2,i3)*zb(i4,i2)**2*zb(i5,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*zb(i4,i3))
     -    - (xmass*za(i1,i3)**2*za(i1,i5)*zb(i2,i1)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*za(i5,i6))
     -    + (xmass*za(i1,i3)**2*za(i3,i5)*zb(i3,i2)*zb(i5,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*za(i5,i6))
     -    + (xmass*za(i1,i3)*za(i1,i5)*zb(i4,i2)*zb(i5,i4))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i5)*za(i3,i4)*zb(i4,i2)**2*zb(i5,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*zb(i4,i3))
     -    + (xmass*za(i1,i6)*za(i2,i3)*zb(i4,i2)**2*zb(i6,i2))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*zb(i4,i3))
     -    - (xmass*za(i1,i3)**2*za(i1,i6)*zb(i2,i1)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*za(i5,i6))
     -    + (xmass*za(i1,i3)**2*za(i3,i6)*zb(i3,i2)*zb(i6,i4))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*za(i5,i6))
     -    + (xmass*za(i1,i3)*za(i1,i6)*zb(i4,i2)*zb(i6,i4))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i6)*za(i3,i4)*zb(i4,i2)**2*zb(i6,i4))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*zb(i4,i3))
      amp(2,1,1,2,2)=  -((xmass*za(i2,i5)*zb(i5,i1))/
     -     (za(i5,i6)*zb(i3,i2)*zb(i4,i3))) - 
     -  (xmass*za(i3,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i4,i5)*zb(i4,i1)*zb(i5,i1))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i2,i6)*zb(i6,i1))/(za(i5,i6)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i3,i6)*zb(i3,i1)*zb(i6,i1))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3)) - 
     -  (xmass*za(i4,i6)*zb(i4,i1)*zb(i6,i1))/
     -   (za(i5,i6)*zb(i2,i1)*zb(i3,i2)*zb(i4,i3))
      amp(2,2,1,2,2)= -((xmass*za(i1,i3)**2*za(i4,i5)*zb(i2,i1)*
     &     zb(i5,i1))/
     -     (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -       za(i5,i6))) + 
     -  (xmass*za(i1,i3)*za(i3,i5)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i1,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i3,i5)*zb(i3,i2)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*
     -     zb(i4,i3)) - (xmass*za(i3,i4)*za(i4,i5)*zb(i4,i2)**2*
     -     zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*
     -     zb(i4,i3)) - (xmass*za(i1,i3)*za(i2,i3)*za(i4,i5)*
     -     zb(i2,i1)*zb(i5,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i5,i6)) + (xmass*za(i2,i3)*za(i3,i5)*zb(i3,i2)*
     -     zb(i5,i2))/(s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i2,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i3)**2*za(i4,i6)*zb(i2,i1)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i5,i6)) + (xmass*za(i1,i3)*za(i3,i6)*zb(i3,i2)*
     -     zb(i6,i1))/(s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i1,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i3,i6)*zb(i3,i2)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*
     -     zb(i4,i3)) - (xmass*za(i3,i4)*za(i4,i6)*zb(i4,i2)**2*
     -     zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i5,i6)*
     -     zb(i4,i3)) - (xmass*za(i1,i3)*za(i2,i3)*za(i4,i6)*
     -     zb(i2,i1)*zb(i6,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i1,i2)*
     -     za(i5,i6)) + (xmass*za(i2,i3)*za(i3,i6)*zb(i3,i2)*
     -     zb(i6,i2))/(s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i2,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3))
      amp(2,1,2,2,2)=(xmass*za(i2,i4)*za(i4,i5)*zb(i3,i1)*zb(i5,i1))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i4,i5)*zb(i3,i1)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i5,i6)*zb(i2,i1)) + 
     -  (xmass*za(i2,i4)**2*za(i2,i5)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i3,i4)*za(i5,i6)) - 
     -  (xmass*za(i2,i4)**2*za(i4,i5)*zb(i4,i3)*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i3,i4)*za(i5,i6)) + 
     -  (xmass*za(i2,i3)*za(i4,i5)*zb(i3,i1)**2*zb(i5,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i5,i6)*zb(i2,i1)) + 
     -  (xmass*za(i2,i4)*za(i4,i6)*zb(i3,i1)*zb(i6,i1))/
     -   (s(i2,i3)*za(i3,i4)*za(i5,i6)*zb(i2,i1)) - 
     -  (xmass*za(i1,i2)*za(i4,i6)*zb(i3,i1)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i5,i6)*zb(i2,i1)) + 
     -  (xmass*za(i2,i4)**2*za(i2,i6)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i3,i4)*za(i5,i6)) - 
     -  (xmass*za(i2,i4)**2*za(i4,i6)*zb(i4,i3)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i3,i4)*za(i5,i6)) + 
     -  (xmass*za(i2,i3)*za(i4,i6)*zb(i3,i1)**2*zb(i6,i3))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i5,i6)*zb(i2,i1))
      amp(2,2,1,2,2)=-((xmass*za(i1,i3)**2*za(i4,i5)*zb(i2,i1)*
     -       zb(i5,i1))/
     -     (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -       za(i1,i2)*za(i5,i6))) + 
     -  (xmass*za(i1,i3)*za(i3,i5)*zb(i3,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i1,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i3,i5)*zb(i3,i2)*zb(i4,i2)*
     -     zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i4,i5)*zb(i4,i2)**2*zb(i5,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i3)*za(i2,i3)*za(i4,i5)*zb(i2,i1)*
     -     zb(i5,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i1,i2)*za(i5,i6)) + 
     -  (xmass*za(i2,i3)*za(i3,i5)*zb(i3,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i2,i3)*za(i4,i5)*zb(i4,i2)*zb(i5,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i3)**2*za(i4,i6)*zb(i2,i1)*zb(i6,i1))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i1,i2)*za(i5,i6)) + 
     -  (xmass*za(i1,i3)*za(i3,i6)*zb(i3,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i1,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i1))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i3,i6)*zb(i3,i2)*zb(i4,i2)*
     -     zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i3,i4)*za(i4,i6)*zb(i4,i2)**2*zb(i6,i1))/
     -   (s(i2,i3)*(s(i2,i3) + s(i2,i4) + s(i3,i4))*
     -     za(i5,i6)*zb(i4,i3)) - 
     -  (xmass*za(i1,i3)*za(i2,i3)*za(i4,i6)*zb(i2,i1)*
     -     zb(i6,i2))/
     -   (s(i2,i3)*(s(i1,i2) + s(i1,i3) + s(i2,i3))*
     -     za(i1,i2)*za(i5,i6)) + 
     -  (xmass*za(i2,i3)*za(i3,i6)*zb(i3,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3)) + 
     -  (xmass*za(i2,i3)*za(i4,i6)*zb(i4,i2)*zb(i6,i2))/
     -   (s(i2,i3)*za(i1,i2)*za(i5,i6)*zb(i4,i3))
      amp(2,2,2,2,2)=  -((xmass*za(i1,i4)*za(i4,i5)*zb(i5,i1))/
     -     (za(i1,i2)*za(i2,i3)*za(i3,i4)*za(i5,i6))) - 
     -  (xmass*za(i2,i4)*za(i4,i5)*zb(i5,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*za(i5,i6)) - 
     -  (xmass*za(i4,i5)*zb(i5,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i5,i6)) - 
     -  (xmass*za(i1,i4)*za(i4,i6)*zb(i6,i1))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*za(i5,i6)) - 
     -  (xmass*za(i2,i4)*za(i4,i6)*zb(i6,i2))/
     -   (za(i1,i2)*za(i2,i3)*za(i3,i4)*za(i5,i6)) - 
     -  (xmass*za(i4,i6)*zb(i6,i3))/
     -   (za(i1,i2)*za(i2,i3)*za(i5,i6))


      return 
      end 


      
