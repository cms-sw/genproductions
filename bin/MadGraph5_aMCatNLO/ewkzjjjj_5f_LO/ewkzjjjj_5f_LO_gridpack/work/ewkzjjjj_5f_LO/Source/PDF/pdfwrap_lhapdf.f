      SUBROUTINE PDFWRAP
      IMPLICIT NONE
C     
C     INCLUDE
C     
      INCLUDE 'pdf.inc'
      INCLUDE '../alfas.inc'
      REAL*8 ZMASS
      DATA ZMASS/91.188D0/
      CHARACTER*250 LHAPATH
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VALUE(20)
      REAL*8 ALPHASPDF
      EXTERNAL ALPHASPDF


C     -------------------
C     START THE CODE
C     -------------------      

C     initialize the pdf set
      CALL FINDPDFPATH(LHAPATH)
C     CALL SetPDFPath(LHAPath)
      VALUE(1)=LHAID
      PARM(1)='DEFAULT'
      CALL PDFSET(PARM,VALUE)
      CALL GETORDERAS(NLOOP)
      NLOOP=NLOOP+1
      ASMZ=ALPHASPDF(ZMASS)

      RETURN
      END


      SUBROUTINE FINDPDFPATH(LHAPATH)
C     *****************************************************************
C     ***
C     generic subroutine to open the table files in the right
C      directories
C     *****************************************************************
C     ***
      IMPLICIT NONE
C     
      CHARACTER LHAPATH*250,UP*3
      DATA UP/'../'/
      LOGICAL EXISTS
      INTEGER I, POS
      CHARACTER*300  TEMPNAME2
      CHARACTER*300 PATH  ! path of the executable
      INTEGER FINE2
      CHARACTER*30  UPNAME  ! sequence of ../

C     first try in the current directory
      LHAPATH='./PDFsets'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN

      LHAPATH='/cvmfs/cms.cern.ch/slc7_amd64_gcc10/external/lhapdf'
     $ //'/6.4.0-68defff11ffd434c73727d03802bfb85/share/LHAPDF/PDFsets'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      LHAPATH='/cvmfs/cms.cern.ch/slc7_amd64_gcc10/external/lhapdf'
     $ //'/6.4.0-68defff11ffd434c73727d03802bfb85/share/LHAPDF/..'
     $ //'/lhapdf/pdfsets/6.1/'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      LHAPATH='/cvmfs/cms.cern.ch/slc7_amd64_gcc10/external/lhapdf'
     $ //'/6.4.0-68defff11ffd434c73727d03802bfb85/share/LHAPDF/..'
     $ //'/lhapdf/pdfsets/'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      LHAPATH='/cvmfs/cms.cern.ch/slc7_amd64_gcc10/external/lhapdf'
     $ //'/6.4.0-68defff11ffd434c73727d03802bfb85/share/LHAPDF'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      LHAPATH='./PDFsets'

      DO I=1,6
        LHAPATH=UP//LHAPATH
        INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
        IF(EXISTS)RETURN
      ENDDO

C     
C     getting the path of the executable
C     
      CALL GETARG(0,PATH)  !path is the PATH to the madevent executable (either global or from launching directory)
      POS = INDEX(PATH,'/',.TRUE.)
      PATH = PATH(:POS)
      FINE2=INDEX(PATH,' ')-1


C     
C     check path from the executable
C     
      LHAPATH='lib/PDFsets'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      UPNAME='../../../../../../../'
      DO I=1,6
        TEMPNAME2=PATH(:FINE2)//UPNAME(:3*I)//LHAPATH
C       LHAPath=up//LHAPath
        INQUIRE(FILE=TEMPNAME2, EXIST=EXISTS)
        IF(EXISTS)THEN
          LHAPATH = TEMPNAME2
          RETURN
        ENDIF
      ENDDO
      PRINT*,'Could not find PDFsets directory, quitting'
      STOP

      RETURN
      END


