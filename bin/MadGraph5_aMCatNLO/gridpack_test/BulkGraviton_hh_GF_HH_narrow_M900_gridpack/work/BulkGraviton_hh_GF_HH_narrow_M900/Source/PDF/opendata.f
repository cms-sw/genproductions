      Integer Function NextUnopen()
c********************************************************************
C     Returns an unallocated FORTRAN i/o unit.
c********************************************************************

      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUnopen = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End



      subroutine OpenData(Tablefile)
c********************************************************************
c generic subroutine to open the table files in the right directories
c********************************************************************
      implicit none
c
      Character Tablefile*(*),up*3,lib*4,dir*8,tempname*250
      data up,lib,dir/'../','lib/','Pdfdata/'/
      Integer IU,NextUnopen,i
      External NextUnopen
      common/IU/IU
c
c--   start
c
      IU=NextUnopen()

c     First try system wide (for cluster if define)
      
            tempname='/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.2.1/share/LHAPDF'//Tablefile
            open(IU,file=tempname,status='old',ERR=1)
            return
 1          tempname='/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.2.1/share/LHAPDF/Pdfdata/'//Tablefile
            open(IU,file=tempname,status='old',ERR=2)
            return
 2          tempname='/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.2.1/share/LHAPDF/lhapdf'//Tablefile
            open(IU,file=tempname,status='old',ERR=3)
            return            
 3          tempname='/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.2.1/share/LHAPDF/../lhapdf/pdfsets/'//Tablefile
            open(IU,file=tempname,status='old',ERR=4)
            return              
 4          tempname='/cvmfs/cms.cern.ch/slc6_amd64_gcc481/external/lhapdf/6.2.1/share/LHAPDF/../lhapdf/pdfsets/6.1/'//Tablefile
            open(IU,file=tempname,status='old',ERR=5)
            return  
            

c     Then try in the current directory (for cluster use)
 5    tempname=Tablefile
      open(IU,file=tempname,status='old',ERR=10)
      return

 10   tempname=up//Tablefile
      open(IU,file=tempname,status='old',ERR=20)
      return

c     then try PdfData directory
 20   tempname=dir//Tablefile
      open(IU,file=tempname,status='old',ERR=30)
      return

 30   tempname=lib//tempname
      open(IU,file=tempname,status='old',ERR=40)

 40   continue
      do i=0,6
         open(IU,file=tempname,status='old',ERR=50)
         return
 50      tempname=up//tempname
         if (i.eq.6)then
            write(*,*) 'Error: PDF file ',Tablefile,' not found'
            stop
         endif
      enddo

      print*,'table for the pdf NOT found!!!'
      
      return
      end

