 
      subroutine read_lhe_event_init(nlf,*) 
      implicit none 
      include 'heprup.f'
      include 'hepeup.f'
      integer nlf,ipr
      character*6 gumpf
      character*7 istring
 22   continue
      read(nlf,'(a)',err=333) gumpf
      write(6,*) gumpf
   !   pause
      if(gumpf.eq.'<init>') then
         goto 11
      else
         goto 22
      endif

      read(nlf,'(a)',err=333) istring
 11   continue
      write(6,*) 'ello'
      read(nlf,*,err=333) idbmup(1),idbmup(2),ebmup(1),ebmup(2),
     &     pdfgup(1),pdfgup(2),pdfsup(1),pdfsup(2),idwtup,nprup
      do 100 ipr=1,nprup
         read(nlf,*,err=333) xsecup(ipr),xerrup(ipr),xmaxup(ipr),
     &        lprup(ipr)
 100     continue 
         read(nlf,'(a)',err=333) istring
         
 110     format(1p,2(1x,i8),2(1x,e12.5),6(1x,i6))
 120     format(1p,3(1x,e12.5),1x,i6)
     
         return 
 333     return 1 
         end
      
