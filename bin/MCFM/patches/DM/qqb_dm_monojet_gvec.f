      subroutine qqb_dm_monojet_gvec(p,n,in,msq)
C*********************************************************************** 
c     Author: R.K. Ellis                                               *
c     September, 1999.                                                 *
c     Matrix element for Z production                                  *
c     averaged over initial colours and spins                          *
c     contracted with the vector n(mu) (orthogonal to p5)              *
c     u(-p1)+dbar(-p2)--> g(p5)+ Z^+(l(p3)+a(p4))   
!----- CW MODIFIED FOR DM                    *
C*********************************************************************** 
      implicit none
      include 'constants.f'
      include 'qcdcouple.f'
      include 'sprods_com.f'
      include 'nflav.f'
      include 'dm_params.f' 
      integer j,k,in
C--in is the label of the parton dotted with n
      double precision msq(-nf:nf,-nf:nf),p(mxpart,4)
      double precision fac,n(4)
!      double complex prop
!      double precision ans1,ans2 
      double precision qqbg(2),qbqg(2)
      double precision gqqb(2),gqbq(2)
      double precision qgqb(2),qbgq(2)
      double complex cprop
      double precision propsq,s34,fac_dm
      logical first 
      data first /.true./ 
      save first 

      do j=-nf,nf
      do k=-nf,nf
      msq(j,k)=0d0
      enddo
      enddo

      if(dm_mediator.eq.'gluonO') then 
         call gg_dm_monojet_gvec(p,n,in,msq) 
         return
      endif

      call dotem(5,p,s)
      

C-----Protect from photon pole by cutting off at some value about 10 GeV
c      if (s(3,4) .lt. 4d0*mbsq) return

!      if(check_QED) then 
!         dm_lam=dsqrt(s(3,4)/esq)
!         do j=1,nf 
!            dmL(j)=Q(j) 
!            dmR(j)=Q(j) 
!         enddo
!      endif

      if(effective_th) then 
!--------- effective theory pre-factor
         fac_dm=one/dm_lam**4
      else
!-------- full theory => for V,A and PS is simply g_dmx**2*g_dmq**2/prop(34)**2 
!-------- for S and G need to be more careful, but G happens elsewhere and S can be done 
!-------- in special routine
         s34=(p(3,4)+p(4,4))**2
     &        -(p(3,1)+p(4,1))**2-(p(3,2)+p(4,2))**2-(p(3,3)+p(4,3))**2
         cprop=cone/Dcmplx((s34-medmass**2),medmass*medwidth)
         propsq=cdabs(cprop)**2 
         fac_dm=propsq*g_dmq**2*g_dmx**2
      endif

      

      fac=16d0*cf*xn*gsq*fac_dm

!      prop=s(3,4)/dcmplx((s(3,4)-zmass**2),zmass*zwidth)

c      p1p2(:,:)=0d0

      gqqb(:)=0d0
      gqbq(:)=0d0
      qgqb(:)=0d0
      qbgq(:)=0d0
      qqbg(:)=0d0
      qbqg(:)=0d0


      if(dm_mediator.eq.'vector') then 
         if (in .eq. 1) then
            call dmmonojn(5,2,1,p,n,gqqb)
            call dmmonojn(2,5,1,p,n,gqbq)
!     p1p2(0,-1)=-aveqg*fac*dmmonojn(5,2,1,p,n)
!     p1p2(0,+1)=-aveqg*fac*dmmonojn(2,5,1,p,n)
         elseif (in .eq. 2) then
            call dmmonojn(1,5,2,p,n,qgqb) 
            call dmmonojn(5,1,2,p,n,qbgq)
!     p1p2(+1,0)=-aveqg*fac*dmmonojn(1,5,2,p,n)
!     p1p2(-1,0)=-aveqg*fac*dmmonojn(5,1,2,p,n)
         elseif (in .eq. 5) then      
            call dmmonojn(2,1,5,p,n,qbqg)
            call dmmonojn(1,2,5,p,n,qqbg)
!     p1p2(-1,1)=+aveqq*fac*dmmonojn(2,1,5,p,n)
!     p1p2(1,-1)=+aveqq*fac*dmmonojn(1,2,5,p,n)
         endif
      elseif(dm_mediator.eq.'axvect') then 
         if (in .eq. 1) then
            call dmmonojn_ax(5,2,1,p,n,gqqb)
            call dmmonojn_ax(2,5,1,p,n,gqbq)
!     p1p2(0,-1)=-aveqg*fac*dmmonojn(5,2,1,p,n)
!     p1p2(0,+1)=-aveqg*fac*dmmonojn(2,5,1,p,n)
         elseif (in .eq. 2) then
            call dmmonojn_ax(1,5,2,p,n,qgqb) 
            call dmmonojn_ax(5,1,2,p,n,qbgq)
!     p1p2(+1,0)=-aveqg*fac*dmmonojn(1,5,2,p,n)
!     p1p2(-1,0)=-aveqg*fac*dmmonojn(5,1,2,p,n)
         elseif (in .eq. 5) then      
            call dmmonojn_ax(2,1,5,p,n,qbqg)
            call dmmonojn_ax(1,2,5,p,n,qqbg)
            if(first) then 
            first = .false. 
            call check_dmAxC
         endif         
!     p1p2(-1,1)=+aveqq*fac*dmmonojn(2,1,5,p,n)
!      p1p2(1,-1)=+aveqq*fac*dmmonojn(1,2,5,p,n)
         endif
      elseif(dm_mediator.eq.'scalar') then 
         if (in .eq. 1) then
            call dmmonojn_scal(5,2,1,p,n,gqqb)
            call dmmonojn_scal(2,5,1,p,n,gqbq)
!     p1p2(0,-1)=-aveqg*fac*dmmonojn(5,2,1,p,n)
!     p1p2(0,+1)=-aveqg*fac*dmmonojn(2,5,1,p,n)
         elseif (in .eq. 2) then
            call dmmonojn_scal(1,5,2,p,n,qgqb) 
            call dmmonojn_scal(5,1,2,p,n,qbgq)
!     p1p2(+1,0)=-aveqg*fac*dmmonojn(1,5,2,p,n)
!     p1p2(-1,0)=-aveqg*fac*dmmonojn(5,1,2,p,n)
         elseif (in .eq. 5) then      
            call dmmonojn_scal(2,1,5,p,n,qbqg)
            call dmmonojn_scal(1,2,5,p,n,qqbg)
!     p1p2(-1,1)=+aveqq*fac*dmmonojn(2,1,5,p,n)
!     p1p2(1,-1)=+aveqq*fac*dmmonojn(1,2,5,p,n)
         endif
         fac=fac/4d0
         
         if(first) then 
            first=.false.
            call set_scalar_coups
         endif
      elseif(dm_mediator.eq.'pseudo') then 
         if (in .eq. 1) then
            call dmmonojn_Pscal(5,2,1,p,n,gqqb)
            call dmmonojn_Pscal(2,5,1,p,n,gqbq)
!     p1p2(0,-1)=-aveqg*fac*dmmonojn(5,2,1,p,n)
!     p1p2(0,+1)=-aveqg*fac*dmmonojn(2,5,1,p,n)
         elseif (in .eq. 2) then
            call dmmonojn_Pscal(1,5,2,p,n,qgqb) 
            call dmmonojn_Pscal(5,1,2,p,n,qbgq)
!     p1p2(+1,0)=-aveqg*fac*dmmonojn(1,5,2,p,n)
!     p1p2(-1,0)=-aveqg*fac*dmmonojn(5,1,2,p,n)
         elseif (in .eq. 5) then      
            call dmmonojn_Pscal(2,1,5,p,n,qbqg)
            call dmmonojn_Pscal(1,2,5,p,n,qqbg)
!     p1p2(-1,1)=+aveqq*fac*dmmonojn(2,1,5,p,n)
!     p1p2(1,-1)=+aveqq*fac*dmmonojn(1,2,5,p,n)
         endif
         if(first) then 
            first = .false. 
            call check_dmAxC
         endif
         fac=fac/4d0
      endif

         
     
      do j=-nflav,nflav
      do k=-nflav,nflav
      if( j .ne. 0 .and. k .ne. 0 .and. j .ne. -k) goto 19

      if     ((j .eq. 0) .and. (k .eq. 0)) then
          msq(j,k)=0d0
      elseif ((j .gt. 0) .and. (k .lt. 0)) then
          msq(j,k)=+aveqq*fac*(dabs(dmL(j))**2*qqbg(1)
     .              +dabs(dmR(j))**2*qqbg(2))
      elseif ((j .lt. 0) .and. (k .gt. 0)) then
          msq(j,k)=+aveqq*fac*(dabs(dmL(-j))**2*qbqg(1)
     .              +dabs(dmR(-j))**2*qbqg(2))
      elseif ((j .gt. 0) .and. (k .eq. 0)) then
          msq(j,k)=+aveqg*fac*(dabs(dmL(j))**2*qgqb(1)
     .              +dabs(dmR(j))**2*qgqb(2))
      elseif ((j .lt. 0) .and. (k .eq. 0)) then
          msq(j,k)=+aveqg*fac*(dabs(dmL(-j))**2*qbgq(1)
     .              +dabs(dmR(-j))**2*qbgq(2))
      elseif ((j .eq. 0) .and. (k .gt. 0)) then
          msq(j,k)=+aveqg*fac*(dabs(dmL(k))**2*gqqb(1)
     .              +dabs(dmR(k))**2*gqqb(2))
      elseif ((j .eq. 0) .and. (k .lt. 0)) then
          msq(j,k)=+aveqg*fac*(dabs(dmL(-k))**2*gqbq(1)
     .              +dabs(dmR(-k))**2*gqbq(2))
      endif

   
   19 continue
      enddo
      enddo
      
      return
      end
 

      subroutine dmmonojn(j1,j2,j5,p,n1,qqbg) 
      implicit none 
      include 'constants.f' 
      include 'sprods_com.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f'
!----- ME squared for q(j1)+qb(j2)+x(j3)+x~(j4)+g(j5) X n_nu 
      integer j1,j2,j5,j3,j4 
      double precision n1(4),p(mxpart,4),q(mxpart,4) 
      double complex amp(2,2,2),zab(mxpart,mxpart),zba(mxpart,mxpart)
      double precision qqbg(2) 
!      double precision z1jetn
!----- return as funciton of quark line helicity 
      integer h1,h2,h3

      j3=3
      j4=4 

      call checkndotp(p,n1,j5)

      if(xmass.gt.1d-8) then 
!---------generate massless phase space 
         call gen_masslessvecs(p,q,j3,j4)
!---------generate spinors 
         call spinoru(5,q,za,zb)
         call spinork(5,q,zab,zba,n1)          
      else
!--------massless dm can use usual spinoru
         call spinoru(5,p,za,zb)      
         call spinork(5,p,zab,zba,n1) 

      endif

      
      do h1=1,5 
         do h2=1,5
            s(h1,h2)=Dble(za(h1,h2)*zb(h2,h1))
         enddo
      enddo


!------ bulid amplitudes 
      
!------- helicity conserving 
      
      amp(1,1,2)= (-2*za(j2,j3)*zab(j1,j1)*zb(j4,j1))/s(j1,j5) + 
     -  (2*za(j2,j3)*zab(j2,j2)*zb(j4,j1))/s(j2,j5) - 
     -  (2*za(j3,j5)*zab(j2,j5)*zb(j4,j1))/s(j2,j5) + 
     -  (2*za(j2,j3)*zab(j5,j1)*zb(j5,j4))/s(j1,j5)
      amp(2,1,2)=(2*za(j1,j3)*zab(j1,j1)*zb(j4,j2))/s(j1,j5) - 
     -  (2*za(j3,j5)*zab(j1,j5)*zb(j4,j2))/s(j1,j5) - 
     -  (2*za(j1,j3)*zab(j2,j2)*zb(j4,j2))/s(j2,j5) + 
     -  (2*za(j1,j3)*zab(j5,j2)*zb(j5,j4))/s(j2,j5)
      amp(1,2,1)=(-2*za(j2,j4)*zab(j1,j1)*zb(j3,j1))/s(j1,j5) + 
     -  (2*za(j2,j4)*zab(j2,j2)*zb(j3,j1))/s(j2,j5) - 
     -  (2*za(j4,j5)*zab(j2,j5)*zb(j3,j1))/s(j2,j5) + 
     -  (2*za(j2,j4)*zab(j5,j1)*zb(j5,j3))/s(j1,j5)
      amp(2,2,1)=(2*za(j1,j4)*zab(j1,j1)*zb(j3,j2))/s(j1,j5) - 
     -  (2*za(j4,j5)*zab(j1,j5)*zb(j3,j2))/s(j1,j5) - 
     -  (2*za(j1,j4)*zab(j2,j2)*zb(j3,j2))/s(j2,j5) + 
     -  (2*za(j1,j4)*zab(j5,j2)*zb(j5,j3))/s(j2,j5)   

!------ helicity violating 
      amp(1,1,1)=(2*xmass*za(j2,j3)*zab(j1,j1)*zb(j3,j1))/
     -   (s(j1,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j3)*zab(j2,j2)*zb(j3,j1))/
     -   (s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j3,j5)*zab(j2,j5)*zb(j3,j1))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j4)*zab(j1,j1)*zb(j4,j1))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j2,j4)*zab(j2,j2)*zb(j4,j1))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j4,j5)*zab(j2,j5)*zb(j4,j1))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j3)*zab(j5,j1)*zb(j5,j3))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j2,j4)*zab(j5,j1)*zb(j5,j4))/
     -   (s(j1,j5)*zb(j4,j3))
      amp(2,1,1)= (-2*xmass*za(j1,j3)*zab(j1,j1)*zb(j3,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j3,j5)*zab(j1,j5)*zb(j3,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j1,j3)*zab(j2,j2)*zb(j3,j2))/
     -   (s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j1,j4)*zab(j1,j1)*zb(j4,j2))/
     -   (s(j1,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j4,j5)*zab(j1,j5)*zb(j4,j2))/
     -   (s(j1,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j1,j4)*zab(j2,j2)*zb(j4,j2))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j1,j3)*zab(j5,j2)*zb(j5,j3))/
     -   (s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j1,j4)*zab(j5,j2)*zb(j5,j4))/
     -   (s(j2,j5)*zb(j4,j3))
      amp(1,2,2)=(-2*xmass*za(j2,j3)*zab(j1,j1)*zb(j3,j1))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j2,j3)*zab(j2,j2)*zb(j3,j1))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j3,j5)*zab(j2,j5)*zb(j3,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j2,j4)*zab(j1,j1)*zb(j4,j1))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j4)*zab(j2,j2)*zb(j4,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j4,j5)*zab(j2,j5)*zb(j4,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j2,j3)*zab(j5,j1)*zb(j5,j3))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j4)*zab(j5,j1)*zb(j5,j4))/
     -   (s(j1,j5)*za(j3,j4))
      amp(2,2,2)=(2*xmass*za(j1,j3)*zab(j1,j1)*zb(j3,j2))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j3,j5)*zab(j1,j5)*zb(j3,j2))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j3)*zab(j2,j2)*zb(j3,j2))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j4)*zab(j1,j1)*zb(j4,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j4,j5)*zab(j1,j5)*zb(j4,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j1,j4)*zab(j2,j2)*zb(j4,j2))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j1,j3)*zab(j5,j2)*zb(j5,j3))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j4)*zab(j5,j2)*zb(j5,j4))/
     -   (s(j2,j5)*za(j3,j4))

      
       qqbg(:)=0d0 
       do h1=1,2 
          do h2=1,2 
             do h3=1,2 
                 qqbg(h1)=qqbg(h1)+cdabs(0.25d0*amp(h1,h2,h3))**2
              enddo
           enddo
        enddo

!        write(6,*) j1,j2,j5
!        write(6,*) one/16d0*cdabs(amp(2,2,1))**2/s(j3,j4)**2 
!     &/z1jetn(j1,j2,j5,p,n1) 
!        write(6,*) one/16d0*cdabs(amp(2,1,2))**2/s(j3,j4)**2 
!     &/z1jetn(j2,j1,j5,p,n1) 
!        pause 

      return 
      end 


!      double precision function z1jetn(j1,j2,j5,p,n)
!      implicit none 
C---calculates the amplitude squared for the process 
c   q(p1)+qbar(p2) --> Z(l(p3)+a(p4))+g(p5)
c   contracted with the vector n(mu) 
c   before spin/color average
c---overall factor of 16 gs**2*gw**4*xw**2*CF*xn removed
c--note QED propagator included.
!      include 'constants.f'
!      include 'sprods_com.f'

!      integer j1,j2,j3,j4,j5
!      double precision n(4),p(mxpart,4),nDn,nDp1,nDp2,nDp3,test
!      j3=3
!      j4=4

!      nDp1=n(4)*p(j1,4)-n(3)*p(j1,3)-n(2)*p(j1,2)-n(1)*p(j1,1)
!      nDp2=n(4)*p(j2,4)-n(3)*p(j2,3)-n(2)*p(j2,2)-n(1)*p(j2,1)
!      nDp3=n(4)*p(j3,4)-n(3)*p(j3,3)-n(2)*p(j3,2)-n(1)*p(j3,1)
!      nDn=n(4)**2-n(3)**2-n(2)**2-n(1)**2

!      call checkndotp(p,n,j5)

!      z1jetn=((nDp1*s(j2,j3)/s(j1,j5)-nDp2*s(j1,j4)/s(j2,j5))**2
!     . +two*(s(j2,j3)*nDp1/s(j1,j5)-s(j1,j4)*nDp2/s(j2,j5))*(nDp2+nDp3)
!     . -(s(j1,4)-s(j2,3))**2*s(j3,j4)*nDn/(four*s(j1,j5)*s(j2,j5))
!     . +(nDp2+nDp3)**2)/s(j3,j4)**2

!      return
!      end




      subroutine dmmonojn_ax(j1,j2,j5,p,n1,qqbg) 
      implicit none 
      include 'constants.f' 
      include 'sprods_com.f' 
      include 'dm_params.f' 
      include 'zprods_decl.f'
!----- ME squared for q(j1)+qb(j2)+x(j3)+x~(j4)+g(j5) X n_nu 
      integer j1,j2,j5,j3,j4 
      double precision n1(4),p(mxpart,4),q(mxpart,4) 
      double complex amp(2,2,2),zab(mxpart,mxpart),zba(mxpart,mxpart)
      double precision qqbg(2) 
!      double precision z1jetn
!----- return as funciton of quark line helicity 
      integer h1,h2,h3
      double precision bp,beta

      j3=3
      j4=4 

      call checkndotp(p,n1,j5)

      if(xmass.gt.1d-8) then 
!---------generate massless phase space 
         call gen_masslessvecs(p,q,j3,j4)
!---------generate spinors 
         call spinoru(5,q,za,zb)
         call spinork(5,q,zab,zba,n1)          
      else
!--------massless dm can use usual spinoru
         call spinoru(5,p,za,zb)      
         call spinork(5,p,zab,zba,n1) 

      endif

      
      do h1=1,5 
         do h2=1,5
            s(h1,h2)=Dble(za(h1,h2)*zb(h2,h1)) 
         enddo
      enddo

!bp = 1/2(1+beta) 
! beta = dsqrt(1-4xmass**2/s34) 
      beta=dsqrt(1d0-4d0*xmass**2/s(j3,j4))
      bp=0.5d0*(one+beta)



!------ bulid amplitudes 
      
!------- helicity conserving 
      
      amp(1,1,2)= (-2*za(j2,j4)*zab(j1,j1)*zb(j3,j1))/s(j1,j5) + 
     -  (4*bp*za(j2,j4)*zab(j1,j1)*zb(j3,j1))/s(j1,j5) + 
     -  (2*za(j2,j4)*zab(j2,j2)*zb(j3,j1))/s(j2,j5) - 
     -  (4*bp*za(j2,j4)*zab(j2,j2)*zb(j3,j1))/s(j2,j5) - 
     -  (2*za(j4,j5)*zab(j2,j5)*zb(j3,j1))/s(j2,j5) + 
     -  (4*bp*za(j4,j5)*zab(j2,j5)*zb(j3,j1))/s(j2,j5) + 
     -  (2*za(j2,j4)*zab(j5,j1)*zb(j5,j3))/s(j1,j5) - 
     -  (4*bp*za(j2,j4)*zab(j5,j1)*zb(j5,j3))/s(j1,j5)
      amp(2,1,2)=(2*za(j1,j4)*zab(j1,j1)*zb(j3,j2))/s(j1,j5) - 
     -  (4*bp*za(j1,j4)*zab(j1,j1)*zb(j3,j2))/s(j1,j5) - 
     -  (2*za(j4,j5)*zab(j1,j5)*zb(j3,j2))/s(j1,j5) + 
     -  (4*bp*za(j4,j5)*zab(j1,j5)*zb(j3,j2))/s(j1,j5) - 
     -  (2*za(j1,j4)*zab(j2,j2)*zb(j3,j2))/s(j2,j5) + 
     -  (4*bp*za(j1,j4)*zab(j2,j2)*zb(j3,j2))/s(j2,j5) + 
     -  (2*za(j1,j4)*zab(j5,j2)*zb(j5,j3))/s(j2,j5) - 
     -  (4*bp*za(j1,j4)*zab(j5,j2)*zb(j5,j3))/s(j2,j5)
      amp(1,2,1)=(2*za(j2,j3)*zab(j1,j1)*zb(j4,j1))/s(j1,j5) - 
     -  (4*bp*za(j2,j3)*zab(j1,j1)*zb(j4,j1))/s(j1,j5) - 
     -  (2*za(j2,j3)*zab(j2,j2)*zb(j4,j1))/s(j2,j5) + 
     -  (4*bp*za(j2,j3)*zab(j2,j2)*zb(j4,j1))/s(j2,j5) + 
     -  (2*za(j3,j5)*zab(j2,j5)*zb(j4,j1))/s(j2,j5) - 
     -  (4*bp*za(j3,j5)*zab(j2,j5)*zb(j4,j1))/s(j2,j5) - 
     -  (2*za(j2,j3)*zab(j5,j1)*zb(j5,j4))/s(j1,j5) + 
     -  (4*bp*za(j2,j3)*zab(j5,j1)*zb(j5,j4))/s(j1,j5)
      amp(2,2,1)=(-2*za(j1,j3)*zab(j1,j1)*zb(j4,j2))/s(j1,j5) + 
     -  (4*bp*za(j1,j3)*zab(j1,j1)*zb(j4,j2))/s(j1,j5) + 
     -  (2*za(j3,j5)*zab(j1,j5)*zb(j4,j2))/s(j1,j5) - 
     -  (4*bp*za(j3,j5)*zab(j1,j5)*zb(j4,j2))/s(j1,j5) + 
     -  (2*za(j1,j3)*zab(j2,j2)*zb(j4,j2))/s(j2,j5) - 
     -  (4*bp*za(j1,j3)*zab(j2,j2)*zb(j4,j2))/s(j2,j5) - 
     -  (2*za(j1,j3)*zab(j5,j2)*zb(j5,j4))/s(j2,j5) + 
     -  (4*bp*za(j1,j3)*zab(j5,j2)*zb(j5,j4))/s(j2,j5)

!------ helicity violating 
      amp(1,1,1)=(2*xmass*za(j2,j3)*zab(j1,j1)*zb(j3,j1))/(s(j1,j5)*
     &     zb(j4,j3)) - 
     -  (2*xmass*za(j2,j3)*zab(j2,j2)*zb(j3,j1))/(s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j3,j5)*zab(j2,j5)*zb(j3,j1))/(s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j2,j4)*zab(j1,j1)*zb(j4,j1))/(s(j1,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j4)*zab(j2,j2)*zb(j4,j1))/(s(j2,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j4,j5)*zab(j2,j5)*zb(j4,j1))/(s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j3)*zab(j5,j1)*zb(j5,j3))/(s(j1,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j2,j4)*zab(j5,j1)*zb(j5,j4))/(s(j1,j5)*zb(j4,j3))
      amp(2,1,1)=(-2*xmass*za(j1,j3)*zab(j1,j1)*zb(j3,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j3,j5)*zab(j1,j5)*zb(j3,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j1,j3)*zab(j2,j2)*zb(j3,j2))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j1,j4)*zab(j1,j1)*zb(j4,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j4,j5)*zab(j1,j5)*zb(j4,j2))/
     -   (s(j1,j5)*zb(j4,j3)) + 
     -  (2*xmass*za(j1,j4)*zab(j2,j2)*zb(j4,j2))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j1,j3)*zab(j5,j2)*zb(j5,j3))/
     -   (s(j2,j5)*zb(j4,j3)) - 
     -  (2*xmass*za(j1,j4)*zab(j5,j2)*zb(j5,j4))/
     -   (s(j2,j5)*zb(j4,j3))
      amp(1,2,2)=(2*xmass*za(j2,j3)*zab(j1,j1)*zb(j3,j1))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j3)*zab(j2,j2)*zb(j3,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j3,j5)*zab(j2,j5)*zb(j3,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j2,j4)*zab(j1,j1)*zb(j4,j1))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j4)*zab(j2,j2)*zb(j4,j1))/
     -   (s(j2,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j4,j5)*zab(j2,j5)*zb(j4,j1))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j3)*zab(j5,j1)*zb(j5,j3))/
     -   (s(j1,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j2,j4)*zab(j5,j1)*zb(j5,j4))/
     -   (s(j1,j5)*za(j3,j4))
      amp(2,2,2)=(-2*xmass*za(j1,j3)*zab(j1,j1)*zb(j3,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j3,j5)*zab(j1,j5)*zb(j3,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j1,j3)*zab(j2,j2)*zb(j3,j2))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j4)*zab(j1,j1)*zb(j4,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j4,j5)*zab(j1,j5)*zb(j4,j2))/
     -   (s(j1,j5)*za(j3,j4)) + 
     -  (2*xmass*za(j1,j4)*zab(j2,j2)*zb(j4,j2))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j3)*zab(j5,j2)*zb(j5,j3))/
     -   (s(j2,j5)*za(j3,j4)) - 
     -  (2*xmass*za(j1,j4)*zab(j5,j2)*zb(j5,j4))/
     -   (s(j2,j5)*za(j3,j4))

      
       qqbg(:)=0d0 
       do h1=1,2 
          do h2=1,2 
             do h3=1,2 
                 qqbg(h1)=qqbg(h1)+cdabs(0.25d0*amp(h1,h2,h3))**2
              enddo
           enddo
        enddo

!        write(6,*) j1,j2,j5
!        write(6,*) one/16d0*cdabs(amp(2,2,1))**2/s(j3,j4)**2 
!     &/z1jetn(j1,j2,j5,p,n1) 
!        write(6,*) one/16d0*cdabs(amp(2,1,2))**2/s(j3,j4)**2 
!     &/z1jetn(j2,j1,j5,p,n1) 
!        pause 

      return 
      end 
