      SUBROUTINE SB_SF_001(P1,ANS)
C     
C     Generated by MadGraph5_aMC@NLO v. 2.9.9, 2022-02-25
C     By the MadGraph5_aMC@NLO Development Team
C     Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
C     
C     RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C     AND HELICITIES
C     FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL-1)
C     
C     Process: u~ u > ta- vt~ w+ a WEIGHTED<=8 [ all = QCD ]
C     Process: c~ c > ta- vt~ w+ a WEIGHTED<=8 [ all = QCD ]
C     spectators: 1 2 

C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INCLUDE 'nexternal.inc'
      INTEGER     NCOMB
      PARAMETER ( NCOMB=  96 )
      INTEGER    THEL
      PARAMETER (THEL=NCOMB*4)
      INTEGER NGRAPHS
      PARAMETER (NGRAPHS=   32)
C     
C     ARGUMENTS 
C     
      REAL*8 P1(0:3,NEXTERNAL-1),ANS
C     
C     LOCAL VARIABLES 
C     
      INTEGER IHEL,IDEN,J
      REAL*8 B_SF_001
      INTEGER IDEN_VALUES(4)
      DATA IDEN_VALUES /36, 36, 36, 36/
C     
C     GLOBAL VARIABLES
C     
      LOGICAL GOODHEL(NCOMB,4)
      COMMON /C_GOODHEL/ GOODHEL
      DOUBLE PRECISION SAVEMOM(NEXTERNAL-1,2)
      COMMON/TO_SAVEMOM/SAVEMOM
      LOGICAL CALCULATEDBORN
      COMMON/CCALCULATEDBORN/CALCULATEDBORN
      INTEGER NFKSPROCESS
      COMMON/C_NFKSPROCESS/NFKSPROCESS
C     ----------
C     BEGIN CODE
C     ----------
      IDEN=IDEN_VALUES(NFKSPROCESS)
      IF (CALCULATEDBORN) THEN
        DO J=1,NEXTERNAL-1
          IF (SAVEMOM(J,1).NE.P1(0,J) .OR. SAVEMOM(J,2).NE.P1(3,J))
     $      THEN
            CALCULATEDBORN=.FALSE.
            WRITE(*,*) 'Error in sb_sf: momenta not the same in the'
     $       //' born'
            STOP
          ENDIF
        ENDDO
      ELSE
        WRITE(*,*) 'Error in sb_sf: color_linked borns should be'
     $   //' called only with calculatedborn = true'
        STOP
      ENDIF
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL,NFKSPROCESS)) THEN
          ANS=ANS+B_SF_001(P1,IHEL)
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      END


      REAL*8 FUNCTION B_SF_001(P,HELL)
C     
C     Generated by MadGraph5_aMC@NLO v. 2.9.9, 2022-02-25
C     By the MadGraph5_aMC@NLO Development Team
C     Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
C     RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C     FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL-1)

C     Process: u~ u > ta- vt~ w+ a WEIGHTED<=8 [ all = QCD ]
C     Process: c~ c > ta- vt~ w+ a WEIGHTED<=8 [ all = QCD ]
C     spectators: 1 2 

C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER     NGRAPHS
      PARAMETER ( NGRAPHS = 32 )
      INTEGER NCOLOR1, NCOLOR2
      PARAMETER (NCOLOR1=1, NCOLOR2=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1 = (0D0,1D0))
      INCLUDE 'nexternal.inc'
      INCLUDE 'born_nhel.inc'
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL-1)
      INTEGER HELL
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      REAL*8  CF(NCOLOR2,NCOLOR1)
      COMPLEX*16 ZTEMP, AMP(NGRAPHS), JAMP1(NCOLOR1), JAMP2(NCOLOR2)
      COMPLEX*16 TMP_JAMP(0)
C     
C     GLOBAL VARIABLES
C     
      DOUBLE COMPLEX SAVEAMP(NGRAPHS,MAX_BHEL)
      COMMON/TO_SAVEAMP/SAVEAMP
      LOGICAL CALCULATEDBORN
      COMMON/CCALCULATEDBORN/CALCULATEDBORN
C     
C     COLOR DATA
C     
      DATA (CF(I,  1),I=  1,  1) /3.000000000000000D+00/
C     ----------
C     BEGIN CODE
C     ----------
      IF (.NOT. CALCULATEDBORN) THEN
        WRITE(*,*) 'Error in b_sf: color_linked borns should be called'
     $   //' only with calculatedborn = true'
        STOP
      ELSEIF (CALCULATEDBORN) THEN
        DO I=1,NGRAPHS
          AMP(I)=SAVEAMP(I,HELL)
        ENDDO
      ENDIF
      JAMP1(1) = AMP(1)+AMP(2)+AMP(3)+AMP(4)+AMP(5)+AMP(6)+AMP(7)
     $ +AMP(8)+AMP(9)+AMP(10)+AMP(11)+AMP(12)+AMP(13)+AMP(14)+AMP(15)
     $ +AMP(16)+AMP(17)+AMP(18)+AMP(19)+AMP(20)+AMP(21)+AMP(22)+AMP(23)
     $ +AMP(24)+AMP(25)+AMP(26)+AMP(27)+AMP(28)+AMP(29)+AMP(30)+AMP(31)
     $ +AMP(32)
      JAMP2(1) = (-1.333333333333333D+00)*AMP(1)+(-1.333333333333333D
     $ +00)*AMP(2)+(-1.333333333333333D+00)*AMP(3)+(
     $ -1.333333333333333D+00)*AMP(4)+(-1.333333333333333D+00)*AMP(5)
     $ +(-1.333333333333333D+00)*AMP(6)+(-1.333333333333333D+00)*AMP(7)
     $ +(-1.333333333333333D+00)*AMP(8)+(-1.333333333333333D+00)*AMP(9)
     $ +(-1.333333333333333D+00)*AMP(10)+(-1.333333333333333D+00)
     $ *AMP(11)+(-1.333333333333333D+00)*AMP(12)+(-1.333333333333333D
     $ +00)*AMP(13)+(-1.333333333333333D+00)*AMP(14)+(
     $ -1.333333333333333D+00)*AMP(15)+(-1.333333333333333D+00)*AMP(16)
     $ +(-1.333333333333333D+00)*AMP(17)+(-1.333333333333333D+00)
     $ *AMP(18)+(-1.333333333333333D+00)*AMP(19)+(-1.333333333333333D
     $ +00)*AMP(20)+(-1.333333333333333D+00)*AMP(21)+(
     $ -1.333333333333333D+00)*AMP(22)+(-1.333333333333333D+00)*AMP(23)
     $ +(-1.333333333333333D+00)*AMP(24)+(-1.333333333333333D+00)
     $ *AMP(25)+(-1.333333333333333D+00)*AMP(26)+(-1.333333333333333D
     $ +00)*AMP(27)+(-1.333333333333333D+00)*AMP(28)+(
     $ -1.333333333333333D+00)*AMP(29)+(-1.333333333333333D+00)*AMP(30)
     $ +(-1.333333333333333D+00)*AMP(31)+(-1.333333333333333D+00)
     $ *AMP(32)
      B_SF_001 = 0.D0
      DO I = 1, NCOLOR1
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR2
          ZTEMP = ZTEMP + CF(J,I)*JAMP2(J)
        ENDDO
        B_SF_001 =B_SF_001+ZTEMP*DCONJG(JAMP1(I))
      ENDDO
      END



