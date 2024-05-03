      
      subroutine qqb_dm_qqb_Samps(p,i1,i2,i3,i4,za,zb,amp)
      implicit none 
!---------- amplitudes for q(i1)+Q(i2)+Qb(i3)+q(i4) 
!---------- with i1 i4 coupling to DM 
      include 'dm_params.f' 
      include 'constants.f' 
      include 'zprods_decl.f'
      double precision p(mxpart,4),q(mxpart,4) 
      integer i1,i2,i3,i4
      double complex amp(2,2) 
      double precision s(mxpart,mxpart) 
      integer h1,h2 
      double precision fac 
    

!====== fac w.r.t. vector !--- note in gg we defined fac as 
!------- at ME level, here we are at amp so sqrt(fac) 
      fac=sqrt(1d0) 

      if(xmass.gt.1d-8) then 
!---------generate massless phase space 
         call gen_masslessvecs(p,q,3,4)
!---------generate spinors 
         call spinoru(6,q,za,zb)
      else
!--------massless dm can use usual spinoru
         call spinoru(6,p,za,zb)       
      endif
      
      do h1=1,6
         do h2=1,6 
            s(h1,h2)=Dble(za(h1,h2)*zb(h2,h1))
         enddo
      enddo  

      

      amp(1,1)= (za(i1,i2)*(za(i1,i4)*zb(i3,i1) + za(i2,i4)*zb(i3,i2)))/
     -   ((s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i2,i3)*zb(i3,i2)) - 
     -  (za(i2,i4)*(-(za(i1,i2)*zb(i3,i2)) + za(i1,i4)*zb(i4,i3)))/
     -   ((s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i2,i3)*zb(i3,i2))
      amp(1,2)=(za(i1,i3)*(za(i1,i4)*zb(i2,i1) - za(i3,i4)*zb(i3,i2)))/
     -   ((s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i2,i3)*zb(i3,i2)) - 
     -  (za(i3,i4)*(za(i1,i3)*zb(i3,i2) + za(i1,i4)*zb(i4,i2)))/
     -   ((s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i2,i3)*zb(i3,i2))
      amp(2,1)= ((za(i2,i3)*zb(i3,i1) + za(i2,i4)*zb(i4,i1))*zb(i4,i3))/
     -   ((s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i2,i3)*zb(i3,i2)) - 
     -  (zb(i3,i1)*(za(i1,i2)*zb(i4,i1) - za(i2,i3)*zb(i4,i3)))/
     -   ((s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i2,i3)*zb(i3,i2))
      amp(2,2)=((-(za(i2,i3)*zb(i2,i1)) + za(i3,i4)*zb(i4,i1))
     &     *zb(i4,i2))/
     -   ((s(i2,i3) + s(i2,i4) + s(i3,i4))*za(i2,i3)*zb(i3,i2)) - 
     -  (zb(i2,i1)*(za(i1,i3)*zb(i4,i1) + za(i2,i3)*zb(i4,i2)))/
     -   ((s(i1,i2) + s(i1,i3) + s(i2,i3))*za(i2,i3)*zb(i3,i2))

      do h1=1,2
         do h2=1,2 
            amp(h1,h2)=fac*amp(h1,h2) 
         enddo
      enddo


      return 
      end 
