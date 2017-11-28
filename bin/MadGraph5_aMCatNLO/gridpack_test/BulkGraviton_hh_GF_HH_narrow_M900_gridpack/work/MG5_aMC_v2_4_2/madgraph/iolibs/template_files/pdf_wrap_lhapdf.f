      subroutine pdfwrap
      implicit none
C
C     INCLUDE
C
      include 'pdf.inc'
      include '../alfas.inc'
      real*8 zmass
      data zmass/91.188d0/
      Character*250 LHAPath
      character*20 parm(20)
      double precision value(20)
      real*8 alphasPDF
      external alphasPDF


c-------------------
c     START THE CODE
c-------------------      

c     initialize the pdf set
      call FindPDFPath(LHAPath)
c      CALL SetPDFPath(LHAPath)
      value(1)=lhaid
      parm(1)='DEFAULT'
      call pdfset(parm,value)
      call GetOrderAs(nloop)
      nloop=nloop+1  
      asmz=alphasPDF(zmass)
      
      return
      end
 

      subroutine FindPDFPath(LHAPath)
c********************************************************************
c generic subroutine to open the table files in the right directories
c********************************************************************
      implicit none
c
      Character LHAPath*250,up*3
      data up/'../'/
      logical exists
      integer i


c     first try in the current directory
      LHAPath='./PDFsets'
      Inquire(File=LHAPath, exist=exists)
      if(exists)return
      %(cluster_specific_path)s
      do i=1,6
         LHAPath=up//LHAPath
         Inquire(File=LHAPath, exist=exists)
         if(exists)return
      enddo
      LHAPath='lib/PDFsets'
      Inquire(File=LHAPath, exist=exists)
      if(exists)return
      do i=1,6
         LHAPath=up//LHAPath
         Inquire(File=LHAPath, exist=exists)
         if(exists)return
      enddo
      print*,'Could not find PDFsets directory, quitting'
      stop
      
      return
      end

