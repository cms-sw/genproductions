!---- simple routine to read in standard lhe event and to calculate p_med
!====== copy over to new lhe file which we will stick decay onto
      subroutine readlhe_infile(nlf,nout,nomore,pm,idmed)
      implicit none
      include 'hepeup.f'  
      include 'heprup.f'
      logical nomore 
      integer unit
      integer nout
      double precision pm(4)
      integer nlf,idmed
      integer i,j,nu
      character*20 tag
      character*8 tag2
      integer ios
      integer idmedx
      common/idmedx/idmedx
      
      read(nlf,'(a)') tag
      write(nout,'(a)') tag 
      read(nlf,*,iostat=ios) nup,idprup,xwgtup,scalup,aqedup,aqcdup
!      write(6,*)      tag
!      write(6,*)      nup+2,idprup,xwgtup,scalup,aqedup,aqcdup
      if(ios.ne.0) then 
         nomore=.true. 
         return 
      endif
!===== note taht nup is increased by 2 for new decays 
      write(nout,210,iostat=ios)nup+2,idprup,xwgtup,scalup,aqedup,aqcdup
      
      do i=1,nup
         read(nlf,*) idup(i),istup(i),mothup(1,i),
     & mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     & vtimup(i),spinup(i)
         spinup(i)=1.00
!===========check if we have the mediator
         if(idup(i).eq.idmed) then
            idmedx=i
!======== change istup to 2
            istup(i)=2
            do nu=1,4
               pm(nu)=pup(nu,i)
            enddo
         endif
         write(nout,220) idup(i),istup(i),mothup(1,i),
     & mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     & vtimup(i),spinup(i)

      enddo
 
 !     write(6,*) ph(4),ph(1),ph(2),ph(3)
 !    &     ,dsqrt(ph(4)**2-ph(3)**2-ph(2)**2-ph(1)**2)
      read(nlf,'(a)') tag2     
      return

 210  format(1p,2(i6),4(e12.5))
 220  format(1p,i8,5(1x,i5),5(1x,e16.9),1x,e12.5,1x,e10.3)

      end
