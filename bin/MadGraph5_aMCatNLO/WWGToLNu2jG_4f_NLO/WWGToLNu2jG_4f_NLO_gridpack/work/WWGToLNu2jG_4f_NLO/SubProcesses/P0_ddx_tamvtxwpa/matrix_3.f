      SUBROUTINE SMATRIX_3(P,ANS)
C     
C     Generated by MadGraph5_aMC@NLO v. 2.9.9, 2022-02-25
C     By the MadGraph5_aMC@NLO Development Team
C     Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
C     
C     Returns amplitude squared summed/avg over colors
C     and helicities
C     for the point in phase space P(0:3,NEXTERNAL)
C     
C     Process: d g > ta- vt~ w+ a d WEIGHTED<=9 [ all = QCD ]
C     Process: s g > ta- vt~ w+ a s WEIGHTED<=9 [ all = QCD ]
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INCLUDE 'nexternal.inc'
      INTEGER     NCOMB
      PARAMETER ( NCOMB=192)
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL),ANS
      DOUBLE PRECISION       WGT_ME_BORN,WGT_ME_REAL
      COMMON /C_WGT_ME_TREE/ WGT_ME_BORN,WGT_ME_REAL
C     
C     LOCAL VARIABLES 
C     
      INTEGER IHEL,IDEN,I,T_IDENT(NCOMB)
      REAL*8 MATRIX_3
      REAL*8 T,T_SAVE(NCOMB)
      SAVE T_SAVE,T_IDENT
      INTEGER NHEL(NEXTERNAL,NCOMB)
      DATA (NHEL(I,   1),I=1,7) / 1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,   2),I=1,7) / 1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,   3),I=1,7) / 1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,   4),I=1,7) / 1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,   5),I=1,7) / 1,-1,-1, 1, 0,-1,-1/
      DATA (NHEL(I,   6),I=1,7) / 1,-1,-1, 1, 0,-1, 1/
      DATA (NHEL(I,   7),I=1,7) / 1,-1,-1, 1, 0, 1,-1/
      DATA (NHEL(I,   8),I=1,7) / 1,-1,-1, 1, 0, 1, 1/
      DATA (NHEL(I,   9),I=1,7) / 1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  10),I=1,7) / 1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  11),I=1,7) / 1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  12),I=1,7) / 1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  13),I=1,7) / 1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  14),I=1,7) / 1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  15),I=1,7) / 1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  16),I=1,7) / 1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  17),I=1,7) / 1,-1,-1,-1, 0,-1,-1/
      DATA (NHEL(I,  18),I=1,7) / 1,-1,-1,-1, 0,-1, 1/
      DATA (NHEL(I,  19),I=1,7) / 1,-1,-1,-1, 0, 1,-1/
      DATA (NHEL(I,  20),I=1,7) / 1,-1,-1,-1, 0, 1, 1/
      DATA (NHEL(I,  21),I=1,7) / 1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  22),I=1,7) / 1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  23),I=1,7) / 1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  24),I=1,7) / 1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  25),I=1,7) / 1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  26),I=1,7) / 1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  27),I=1,7) / 1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  28),I=1,7) / 1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  29),I=1,7) / 1,-1, 1, 1, 0,-1,-1/
      DATA (NHEL(I,  30),I=1,7) / 1,-1, 1, 1, 0,-1, 1/
      DATA (NHEL(I,  31),I=1,7) / 1,-1, 1, 1, 0, 1,-1/
      DATA (NHEL(I,  32),I=1,7) / 1,-1, 1, 1, 0, 1, 1/
      DATA (NHEL(I,  33),I=1,7) / 1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  34),I=1,7) / 1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  35),I=1,7) / 1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  36),I=1,7) / 1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  37),I=1,7) / 1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  38),I=1,7) / 1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  39),I=1,7) / 1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  40),I=1,7) / 1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  41),I=1,7) / 1,-1, 1,-1, 0,-1,-1/
      DATA (NHEL(I,  42),I=1,7) / 1,-1, 1,-1, 0,-1, 1/
      DATA (NHEL(I,  43),I=1,7) / 1,-1, 1,-1, 0, 1,-1/
      DATA (NHEL(I,  44),I=1,7) / 1,-1, 1,-1, 0, 1, 1/
      DATA (NHEL(I,  45),I=1,7) / 1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  46),I=1,7) / 1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  47),I=1,7) / 1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  48),I=1,7) / 1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  49),I=1,7) / 1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  50),I=1,7) / 1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  51),I=1,7) / 1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  52),I=1,7) / 1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  53),I=1,7) / 1, 1,-1, 1, 0,-1,-1/
      DATA (NHEL(I,  54),I=1,7) / 1, 1,-1, 1, 0,-1, 1/
      DATA (NHEL(I,  55),I=1,7) / 1, 1,-1, 1, 0, 1,-1/
      DATA (NHEL(I,  56),I=1,7) / 1, 1,-1, 1, 0, 1, 1/
      DATA (NHEL(I,  57),I=1,7) / 1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  58),I=1,7) / 1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  59),I=1,7) / 1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  60),I=1,7) / 1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  61),I=1,7) / 1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  62),I=1,7) / 1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  63),I=1,7) / 1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  64),I=1,7) / 1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  65),I=1,7) / 1, 1,-1,-1, 0,-1,-1/
      DATA (NHEL(I,  66),I=1,7) / 1, 1,-1,-1, 0,-1, 1/
      DATA (NHEL(I,  67),I=1,7) / 1, 1,-1,-1, 0, 1,-1/
      DATA (NHEL(I,  68),I=1,7) / 1, 1,-1,-1, 0, 1, 1/
      DATA (NHEL(I,  69),I=1,7) / 1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  70),I=1,7) / 1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  71),I=1,7) / 1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  72),I=1,7) / 1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  73),I=1,7) / 1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  74),I=1,7) / 1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  75),I=1,7) / 1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  76),I=1,7) / 1, 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  77),I=1,7) / 1, 1, 1, 1, 0,-1,-1/
      DATA (NHEL(I,  78),I=1,7) / 1, 1, 1, 1, 0,-1, 1/
      DATA (NHEL(I,  79),I=1,7) / 1, 1, 1, 1, 0, 1,-1/
      DATA (NHEL(I,  80),I=1,7) / 1, 1, 1, 1, 0, 1, 1/
      DATA (NHEL(I,  81),I=1,7) / 1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  82),I=1,7) / 1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  83),I=1,7) / 1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  84),I=1,7) / 1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  85),I=1,7) / 1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  86),I=1,7) / 1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  87),I=1,7) / 1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  88),I=1,7) / 1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  89),I=1,7) / 1, 1, 1,-1, 0,-1,-1/
      DATA (NHEL(I,  90),I=1,7) / 1, 1, 1,-1, 0,-1, 1/
      DATA (NHEL(I,  91),I=1,7) / 1, 1, 1,-1, 0, 1,-1/
      DATA (NHEL(I,  92),I=1,7) / 1, 1, 1,-1, 0, 1, 1/
      DATA (NHEL(I,  93),I=1,7) / 1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  94),I=1,7) / 1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  95),I=1,7) / 1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  96),I=1,7) / 1, 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  97),I=1,7) /-1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  98),I=1,7) /-1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  99),I=1,7) /-1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 100),I=1,7) /-1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 101),I=1,7) /-1,-1,-1, 1, 0,-1,-1/
      DATA (NHEL(I, 102),I=1,7) /-1,-1,-1, 1, 0,-1, 1/
      DATA (NHEL(I, 103),I=1,7) /-1,-1,-1, 1, 0, 1,-1/
      DATA (NHEL(I, 104),I=1,7) /-1,-1,-1, 1, 0, 1, 1/
      DATA (NHEL(I, 105),I=1,7) /-1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 106),I=1,7) /-1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 107),I=1,7) /-1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 108),I=1,7) /-1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 109),I=1,7) /-1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 110),I=1,7) /-1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 111),I=1,7) /-1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 112),I=1,7) /-1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 113),I=1,7) /-1,-1,-1,-1, 0,-1,-1/
      DATA (NHEL(I, 114),I=1,7) /-1,-1,-1,-1, 0,-1, 1/
      DATA (NHEL(I, 115),I=1,7) /-1,-1,-1,-1, 0, 1,-1/
      DATA (NHEL(I, 116),I=1,7) /-1,-1,-1,-1, 0, 1, 1/
      DATA (NHEL(I, 117),I=1,7) /-1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 118),I=1,7) /-1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 119),I=1,7) /-1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 120),I=1,7) /-1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 121),I=1,7) /-1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 122),I=1,7) /-1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 123),I=1,7) /-1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 124),I=1,7) /-1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 125),I=1,7) /-1,-1, 1, 1, 0,-1,-1/
      DATA (NHEL(I, 126),I=1,7) /-1,-1, 1, 1, 0,-1, 1/
      DATA (NHEL(I, 127),I=1,7) /-1,-1, 1, 1, 0, 1,-1/
      DATA (NHEL(I, 128),I=1,7) /-1,-1, 1, 1, 0, 1, 1/
      DATA (NHEL(I, 129),I=1,7) /-1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 130),I=1,7) /-1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 131),I=1,7) /-1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 132),I=1,7) /-1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 133),I=1,7) /-1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 134),I=1,7) /-1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 135),I=1,7) /-1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 136),I=1,7) /-1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 137),I=1,7) /-1,-1, 1,-1, 0,-1,-1/
      DATA (NHEL(I, 138),I=1,7) /-1,-1, 1,-1, 0,-1, 1/
      DATA (NHEL(I, 139),I=1,7) /-1,-1, 1,-1, 0, 1,-1/
      DATA (NHEL(I, 140),I=1,7) /-1,-1, 1,-1, 0, 1, 1/
      DATA (NHEL(I, 141),I=1,7) /-1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 142),I=1,7) /-1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 143),I=1,7) /-1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 144),I=1,7) /-1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 145),I=1,7) /-1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 146),I=1,7) /-1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 147),I=1,7) /-1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 148),I=1,7) /-1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 149),I=1,7) /-1, 1,-1, 1, 0,-1,-1/
      DATA (NHEL(I, 150),I=1,7) /-1, 1,-1, 1, 0,-1, 1/
      DATA (NHEL(I, 151),I=1,7) /-1, 1,-1, 1, 0, 1,-1/
      DATA (NHEL(I, 152),I=1,7) /-1, 1,-1, 1, 0, 1, 1/
      DATA (NHEL(I, 153),I=1,7) /-1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 154),I=1,7) /-1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 155),I=1,7) /-1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 156),I=1,7) /-1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 157),I=1,7) /-1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 158),I=1,7) /-1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 159),I=1,7) /-1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 160),I=1,7) /-1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 161),I=1,7) /-1, 1,-1,-1, 0,-1,-1/
      DATA (NHEL(I, 162),I=1,7) /-1, 1,-1,-1, 0,-1, 1/
      DATA (NHEL(I, 163),I=1,7) /-1, 1,-1,-1, 0, 1,-1/
      DATA (NHEL(I, 164),I=1,7) /-1, 1,-1,-1, 0, 1, 1/
      DATA (NHEL(I, 165),I=1,7) /-1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 166),I=1,7) /-1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 167),I=1,7) /-1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 168),I=1,7) /-1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 169),I=1,7) /-1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 170),I=1,7) /-1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 171),I=1,7) /-1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 172),I=1,7) /-1, 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 173),I=1,7) /-1, 1, 1, 1, 0,-1,-1/
      DATA (NHEL(I, 174),I=1,7) /-1, 1, 1, 1, 0,-1, 1/
      DATA (NHEL(I, 175),I=1,7) /-1, 1, 1, 1, 0, 1,-1/
      DATA (NHEL(I, 176),I=1,7) /-1, 1, 1, 1, 0, 1, 1/
      DATA (NHEL(I, 177),I=1,7) /-1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 178),I=1,7) /-1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 179),I=1,7) /-1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 180),I=1,7) /-1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 181),I=1,7) /-1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 182),I=1,7) /-1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 183),I=1,7) /-1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 184),I=1,7) /-1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 185),I=1,7) /-1, 1, 1,-1, 0,-1,-1/
      DATA (NHEL(I, 186),I=1,7) /-1, 1, 1,-1, 0,-1, 1/
      DATA (NHEL(I, 187),I=1,7) /-1, 1, 1,-1, 0, 1,-1/
      DATA (NHEL(I, 188),I=1,7) /-1, 1, 1,-1, 0, 1, 1/
      DATA (NHEL(I, 189),I=1,7) /-1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 190),I=1,7) /-1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 191),I=1,7) /-1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 192),I=1,7) /-1, 1, 1,-1, 1, 1, 1/
      LOGICAL GOODHEL(NCOMB)
      DATA GOODHEL/NCOMB*.FALSE./
      INTEGER NTRY
      DATA NTRY/0/
      DATA IDEN/96/
C     ----------
C     BEGIN CODE
C     ----------
      NTRY=NTRY+1
      ANS = 0D0
      DO IHEL=1,NCOMB
        IF (GOODHEL(IHEL) .OR. NTRY .LT. 2) THEN
          IF (NTRY.LT.2) THEN
C           for the first ps-point, check for helicities that give
C           identical matrix elements
            T=MATRIX_3(P ,NHEL(1,IHEL))
            T_SAVE(IHEL)=T
            T_IDENT(IHEL)=-1
            DO I=1,IHEL-1
              IF (T.EQ.0D0) EXIT
              IF (T_SAVE(I).EQ.0D0) CYCLE
              IF (ABS(T/T_SAVE(I)-1D0) .LT. 1D-12) THEN
C               WRITE (*,*) 'FOUND IDENTICAL',T,IHEL,T_SAVE(I),I
                T_IDENT(IHEL) = I
              ENDIF
            ENDDO
          ELSE
            IF (T_IDENT(IHEL).GT.0) THEN
C             if two helicity states are identical, dont recompute
              T=T_SAVE(T_IDENT(IHEL))
              T_SAVE(IHEL)=T
            ELSE
              T=MATRIX_3(P ,NHEL(1,IHEL))
              T_SAVE(IHEL)=T
            ENDIF
          ENDIF
C         add to the sum of helicities
          ANS=ANS+T
          IF (T .NE. 0D0 .AND. .NOT. GOODHEL(IHEL)) THEN
            GOODHEL(IHEL)=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      ANS=ANS/DBLE(IDEN)
      WGT_ME_REAL=ANS
      END


      REAL*8 FUNCTION MATRIX_3(P,NHEL)
C     
C     Generated by MadGraph5_aMC@NLO v. 2.9.9, 2022-02-25
C     By the MadGraph5_aMC@NLO Development Team
C     Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
C     
C     Returns amplitude squared summed/avg over colors
C     for the point with external lines W(0:6,NEXTERNAL)
C     
C     Process: d g > ta- vt~ w+ a d WEIGHTED<=9 [ all = QCD ]
C     Process: s g > ta- vt~ w+ a s WEIGHTED<=9 [ all = QCD ]
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INTEGER    NGRAPHS
      PARAMETER (NGRAPHS=83)
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER (NWAVEFUNCS=30, NCOLOR=1)
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)
      COMPLEX*16 IMAG1
      PARAMETER (IMAG1=(0D0,1D0))
      INCLUDE 'nexternal.inc'
      INCLUDE 'coupl.inc'
C     
C     ARGUMENTS 
C     
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL)
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,J
      INTEGER IC(NEXTERNAL)
      DATA IC /NEXTERNAL*1/
      REAL*8 CF(NCOLOR,NCOLOR)
      COMPLEX*16 ZTEMP, AMP(NGRAPHS), JAMP(NCOLOR), W(8,NWAVEFUNCS)
      COMPLEX*16 TMP_JAMP(0)
C     
C     COLOR DATA
C     
      DATA (CF(I,  1),I=  1,  1) /4.000000000000000D+00/
C     1 T(2,7,1)
C     ----------
C     BEGIN CODE
C     ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))
      CALL VXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))
      CALL OXXXXX(P(0,3),MDL_MTA,NHEL(3),+1*IC(3),W(1,3))
      CALL IXXXXX(P(0,4),ZERO,NHEL(4),-1*IC(4),W(1,4))
      CALL VXXXXX(P(0,5),MDL_MW,NHEL(5),+1*IC(5),W(1,5))
      CALL VXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,6))
      CALL OXXXXX(P(0,7),ZERO,NHEL(7),+1*IC(7),W(1,7))
      CALL FFV1_2(W(1,1),W(1,2),GC_5,ZERO,ZERO,W(1,8))
      CALL FFV2_3(W(1,4),W(1,3),GC_11,MDL_MW,MDL_WW,W(1,9))
      CALL FFV1_2(W(1,8),W(1,6),GC_1,ZERO,ZERO,W(1,10))
      CALL VVV1P0_1(W(1,9),W(1,5),GC_25,ZERO,ZERO,W(1,11))
C     Amplitude(s) for diagram number 1
      CALL FFV1_0(W(1,10),W(1,7),W(1,11),GC_1,AMP(1))
      CALL VVV1_3(W(1,9),W(1,5),GC_7,MDL_MZ,MDL_WZ,W(1,12))
C     Amplitude(s) for diagram number 2
      CALL FFV2_3_0(W(1,10),W(1,7),W(1,12),GC_21,GC_23,AMP(2))
      CALL FFV1P0_3(W(1,8),W(1,7),GC_1,ZERO,ZERO,W(1,13))
      CALL VVV1_3(W(1,6),W(1,9),GC_25,MDL_MW,MDL_WW,W(1,14))
C     Amplitude(s) for diagram number 3
      CALL VVV1_0(W(1,13),W(1,14),W(1,5),GC_25,AMP(3))
      CALL FFV2_3_3(W(1,8),W(1,7),GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,15))
C     Amplitude(s) for diagram number 4
      CALL VVV1_0(W(1,14),W(1,5),W(1,15),GC_7,AMP(4))
C     Amplitude(s) for diagram number 5
      CALL VVVV2_0(W(1,6),W(1,13),W(1,9),W(1,5),GC_27,AMP(5))
C     Amplitude(s) for diagram number 6
      CALL VVVV5_0(W(1,6),W(1,9),W(1,5),W(1,15),GC_26,AMP(6))
      CALL VVV1_2(W(1,6),W(1,5),GC_25,MDL_MW,MDL_WW,W(1,16))
      CALL FFV2_2(W(1,8),W(1,9),GC_11,ZERO,ZERO,W(1,17))
C     Amplitude(s) for diagram number 7
      CALL FFV2_0(W(1,17),W(1,7),W(1,16),GC_11,AMP(7))
C     Amplitude(s) for diagram number 8
      CALL VVV1_0(W(1,13),W(1,9),W(1,16),GC_25,AMP(8))
C     Amplitude(s) for diagram number 9
      CALL VVV1_0(W(1,9),W(1,16),W(1,15),GC_7,AMP(9))
      CALL FFV2_1(W(1,7),W(1,5),GC_11,ZERO,ZERO,W(1,18))
C     Amplitude(s) for diagram number 10
      CALL FFV1_0(W(1,17),W(1,18),W(1,6),GC_2,AMP(10))
      CALL FFV2_3(W(1,8),W(1,18),GC_11,MDL_MW,MDL_WW,W(1,19))
C     Amplitude(s) for diagram number 11
      CALL VVV1_0(W(1,6),W(1,9),W(1,19),GC_25,AMP(11))
C     Amplitude(s) for diagram number 12
      CALL FFV2_0(W(1,10),W(1,18),W(1,9),GC_11,AMP(12))
      CALL FFV1_1(W(1,7),W(1,6),GC_1,ZERO,ZERO,W(1,20))
C     Amplitude(s) for diagram number 13
      CALL FFV2_0(W(1,17),W(1,20),W(1,5),GC_11,AMP(13))
      CALL FFV1P0_3(W(1,8),W(1,20),GC_1,ZERO,ZERO,W(1,17))
C     Amplitude(s) for diagram number 14
      CALL VVV1_0(W(1,17),W(1,9),W(1,5),GC_25,AMP(14))
      CALL FFV2_3_3(W(1,8),W(1,20),GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,21))
C     Amplitude(s) for diagram number 15
      CALL VVV1_0(W(1,9),W(1,5),W(1,21),GC_7,AMP(15))
      CALL FFV2_1(W(1,3),W(1,5),GC_11,ZERO,ZERO,W(1,8))
      CALL FFV2_3(W(1,4),W(1,8),GC_28,MDL_MZ,MDL_WZ,W(1,22))
C     Amplitude(s) for diagram number 16
      CALL FFV2_3_0(W(1,10),W(1,7),W(1,22),GC_21,GC_23,AMP(16))
C     Amplitude(s) for diagram number 17
      CALL FFV2_0(W(1,4),W(1,8),W(1,21),GC_28,AMP(17))
      CALL FFV1_1(W(1,3),W(1,6),-GC_25,MDL_MTA,ZERO,W(1,23))
      CALL FFV2_3(W(1,4),W(1,23),GC_11,MDL_MW,MDL_WW,W(1,24))
C     Amplitude(s) for diagram number 18
      CALL VVV1_0(W(1,13),W(1,24),W(1,5),GC_25,AMP(18))
C     Amplitude(s) for diagram number 19
      CALL VVV1_0(W(1,24),W(1,5),W(1,15),GC_7,AMP(19))
      CALL FFV2_1(W(1,23),W(1,5),GC_11,ZERO,ZERO,W(1,25))
C     Amplitude(s) for diagram number 20
      CALL FFV2_0(W(1,4),W(1,25),W(1,15),GC_28,AMP(20))
      CALL FFV2_2(W(1,4),W(1,5),GC_11,MDL_MTA,ZERO,W(1,26))
C     Amplitude(s) for diagram number 21
      CALL FFV1_0(W(1,26),W(1,23),W(1,13),-GC_25,AMP(21))
C     Amplitude(s) for diagram number 22
      CALL FFV2_4_0(W(1,26),W(1,23),W(1,15),GC_21,GC_24,AMP(22))
C     Amplitude(s) for diagram number 23
      CALL FFV2_0(W(1,4),W(1,23),W(1,19),GC_11,AMP(23))
      CALL FFV1P0_3(W(1,26),W(1,3),-GC_25,ZERO,ZERO,W(1,19))
C     Amplitude(s) for diagram number 24
      CALL FFV1_0(W(1,10),W(1,7),W(1,19),GC_1,AMP(24))
      CALL FFV2_4_3(W(1,26),W(1,3),GC_21,GC_24,MDL_MZ,MDL_WZ,W(1,27))
C     Amplitude(s) for diagram number 25
      CALL FFV2_3_0(W(1,10),W(1,7),W(1,27),GC_21,GC_23,AMP(25))
      CALL FFV1_2(W(1,26),W(1,6),-GC_25,MDL_MTA,ZERO,W(1,10))
C     Amplitude(s) for diagram number 26
      CALL FFV1_0(W(1,10),W(1,3),W(1,13),-GC_25,AMP(26))
C     Amplitude(s) for diagram number 27
      CALL FFV2_4_0(W(1,10),W(1,3),W(1,15),GC_21,GC_24,AMP(27))
C     Amplitude(s) for diagram number 28
      CALL FFV1_0(W(1,26),W(1,3),W(1,17),-GC_25,AMP(28))
C     Amplitude(s) for diagram number 29
      CALL FFV2_4_0(W(1,26),W(1,3),W(1,21),GC_21,GC_24,AMP(29))
      CALL FFV2_1(W(1,3),W(1,16),GC_11,ZERO,ZERO,W(1,21))
C     Amplitude(s) for diagram number 30
      CALL FFV2_0(W(1,4),W(1,21),W(1,15),GC_28,AMP(30))
      CALL FFV2_2(W(1,4),W(1,16),GC_11,MDL_MTA,ZERO,W(1,17))
C     Amplitude(s) for diagram number 31
      CALL FFV1_0(W(1,17),W(1,3),W(1,13),-GC_25,AMP(31))
C     Amplitude(s) for diagram number 32
      CALL FFV2_4_0(W(1,17),W(1,3),W(1,15),GC_21,GC_24,AMP(32))
      CALL FFV1_2(W(1,1),W(1,6),GC_1,ZERO,ZERO,W(1,15))
      CALL FFV1_1(W(1,7),W(1,2),GC_5,ZERO,ZERO,W(1,13))
      CALL FFV1P0_3(W(1,15),W(1,13),GC_1,ZERO,ZERO,W(1,28))
C     Amplitude(s) for diagram number 33
      CALL VVV1_0(W(1,28),W(1,9),W(1,5),GC_25,AMP(33))
      CALL FFV2_3_3(W(1,15),W(1,13),GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,29))
C     Amplitude(s) for diagram number 34
      CALL VVV1_0(W(1,9),W(1,5),W(1,29),GC_7,AMP(34))
      CALL FFV2_2(W(1,15),W(1,9),GC_11,ZERO,ZERO,W(1,30))
C     Amplitude(s) for diagram number 35
      CALL FFV2_0(W(1,30),W(1,13),W(1,5),GC_11,AMP(35))
C     Amplitude(s) for diagram number 36
      CALL FFV2_0(W(1,4),W(1,8),W(1,29),GC_28,AMP(36))
C     Amplitude(s) for diagram number 37
      CALL FFV1_0(W(1,26),W(1,3),W(1,28),-GC_25,AMP(37))
C     Amplitude(s) for diagram number 38
      CALL FFV2_4_0(W(1,26),W(1,3),W(1,29),GC_21,GC_24,AMP(38))
      CALL FFV1_2(W(1,15),W(1,2),GC_5,ZERO,ZERO,W(1,29))
C     Amplitude(s) for diagram number 39
      CALL FFV1_0(W(1,29),W(1,7),W(1,11),GC_1,AMP(39))
C     Amplitude(s) for diagram number 40
      CALL FFV2_3_0(W(1,29),W(1,7),W(1,12),GC_21,GC_23,AMP(40))
C     Amplitude(s) for diagram number 41
      CALL FFV2_0(W(1,29),W(1,18),W(1,9),GC_11,AMP(41))
C     Amplitude(s) for diagram number 42
      CALL FFV1_0(W(1,30),W(1,18),W(1,2),GC_5,AMP(42))
C     Amplitude(s) for diagram number 43
      CALL FFV2_3_0(W(1,29),W(1,7),W(1,22),GC_21,GC_23,AMP(43))
C     Amplitude(s) for diagram number 44
      CALL FFV1_0(W(1,29),W(1,7),W(1,19),GC_1,AMP(44))
C     Amplitude(s) for diagram number 45
      CALL FFV2_3_0(W(1,29),W(1,7),W(1,27),GC_21,GC_23,AMP(45))
      CALL FFV1P0_3(W(1,1),W(1,13),GC_1,ZERO,ZERO,W(1,29))
C     Amplitude(s) for diagram number 46
      CALL VVV1_0(W(1,29),W(1,14),W(1,5),GC_25,AMP(46))
      CALL FFV2_3_3(W(1,1),W(1,13),GC_21,GC_23,MDL_MZ,MDL_WZ,W(1,30))
C     Amplitude(s) for diagram number 47
      CALL VVV1_0(W(1,14),W(1,5),W(1,30),GC_7,AMP(47))
C     Amplitude(s) for diagram number 48
      CALL VVVV2_0(W(1,6),W(1,29),W(1,9),W(1,5),GC_27,AMP(48))
C     Amplitude(s) for diagram number 49
      CALL VVVV5_0(W(1,6),W(1,9),W(1,5),W(1,30),GC_26,AMP(49))
      CALL FFV2_2(W(1,1),W(1,9),GC_11,ZERO,ZERO,W(1,15))
      CALL FFV2_1(W(1,13),W(1,5),GC_11,ZERO,ZERO,W(1,28))
C     Amplitude(s) for diagram number 50
      CALL FFV1_0(W(1,15),W(1,28),W(1,6),GC_2,AMP(50))
      CALL FFV1_1(W(1,13),W(1,6),GC_1,ZERO,ZERO,W(1,8))
C     Amplitude(s) for diagram number 51
      CALL FFV2_0(W(1,15),W(1,8),W(1,5),GC_11,AMP(51))
C     Amplitude(s) for diagram number 52
      CALL FFV2_0(W(1,1),W(1,28),W(1,14),GC_11,AMP(52))
C     Amplitude(s) for diagram number 53
      CALL FFV1_0(W(1,1),W(1,8),W(1,11),GC_1,AMP(53))
C     Amplitude(s) for diagram number 54
      CALL FFV2_3_0(W(1,1),W(1,8),W(1,12),GC_21,GC_23,AMP(54))
C     Amplitude(s) for diagram number 55
      CALL VVV1_0(W(1,29),W(1,9),W(1,16),GC_25,AMP(55))
C     Amplitude(s) for diagram number 56
      CALL VVV1_0(W(1,9),W(1,16),W(1,30),GC_7,AMP(56))
C     Amplitude(s) for diagram number 57
      CALL FFV2_0(W(1,15),W(1,13),W(1,16),GC_11,AMP(57))
C     Amplitude(s) for diagram number 58
      CALL FFV2_3_0(W(1,1),W(1,8),W(1,22),GC_21,GC_23,AMP(58))
C     Amplitude(s) for diagram number 59
      CALL VVV1_0(W(1,29),W(1,24),W(1,5),GC_25,AMP(59))
C     Amplitude(s) for diagram number 60
      CALL VVV1_0(W(1,24),W(1,5),W(1,30),GC_7,AMP(60))
C     Amplitude(s) for diagram number 61
      CALL FFV2_0(W(1,4),W(1,25),W(1,30),GC_28,AMP(61))
C     Amplitude(s) for diagram number 62
      CALL FFV2_0(W(1,1),W(1,28),W(1,24),GC_11,AMP(62))
C     Amplitude(s) for diagram number 63
      CALL FFV1_0(W(1,26),W(1,23),W(1,29),-GC_25,AMP(63))
C     Amplitude(s) for diagram number 64
      CALL FFV2_4_0(W(1,26),W(1,23),W(1,30),GC_21,GC_24,AMP(64))
C     Amplitude(s) for diagram number 65
      CALL FFV1_0(W(1,10),W(1,3),W(1,29),-GC_25,AMP(65))
C     Amplitude(s) for diagram number 66
      CALL FFV2_4_0(W(1,10),W(1,3),W(1,30),GC_21,GC_24,AMP(66))
C     Amplitude(s) for diagram number 67
      CALL FFV1_0(W(1,1),W(1,8),W(1,19),GC_1,AMP(67))
C     Amplitude(s) for diagram number 68
      CALL FFV2_3_0(W(1,1),W(1,8),W(1,27),GC_21,GC_23,AMP(68))
C     Amplitude(s) for diagram number 69
      CALL FFV2_0(W(1,4),W(1,21),W(1,30),GC_28,AMP(69))
C     Amplitude(s) for diagram number 70
      CALL FFV1_0(W(1,17),W(1,3),W(1,29),-GC_25,AMP(70))
C     Amplitude(s) for diagram number 71
      CALL FFV2_4_0(W(1,17),W(1,3),W(1,30),GC_21,GC_24,AMP(71))
      CALL FFV2_1(W(1,7),W(1,16),GC_11,ZERO,ZERO,W(1,30))
C     Amplitude(s) for diagram number 72
      CALL FFV1_0(W(1,15),W(1,30),W(1,2),GC_5,AMP(72))
      CALL FFV1_1(W(1,18),W(1,2),GC_5,ZERO,ZERO,W(1,30))
C     Amplitude(s) for diagram number 73
      CALL FFV1_0(W(1,15),W(1,30),W(1,6),GC_2,AMP(73))
      CALL FFV1_1(W(1,18),W(1,6),GC_2,ZERO,ZERO,W(1,16))
C     Amplitude(s) for diagram number 74
      CALL FFV1_0(W(1,15),W(1,16),W(1,2),GC_5,AMP(74))
C     Amplitude(s) for diagram number 75
      CALL FFV2_0(W(1,1),W(1,30),W(1,14),GC_11,AMP(75))
      CALL FFV1_1(W(1,20),W(1,2),GC_5,ZERO,ZERO,W(1,14))
C     Amplitude(s) for diagram number 76
      CALL FFV2_0(W(1,15),W(1,14),W(1,5),GC_11,AMP(76))
      CALL FFV2_1(W(1,20),W(1,5),GC_11,ZERO,ZERO,W(1,16))
C     Amplitude(s) for diagram number 77
      CALL FFV1_0(W(1,15),W(1,16),W(1,2),GC_5,AMP(77))
C     Amplitude(s) for diagram number 78
      CALL FFV1_0(W(1,1),W(1,14),W(1,11),GC_1,AMP(78))
C     Amplitude(s) for diagram number 79
      CALL FFV2_3_0(W(1,1),W(1,14),W(1,12),GC_21,GC_23,AMP(79))
C     Amplitude(s) for diagram number 80
      CALL FFV2_3_0(W(1,1),W(1,14),W(1,22),GC_21,GC_23,AMP(80))
C     Amplitude(s) for diagram number 81
      CALL FFV2_0(W(1,1),W(1,30),W(1,24),GC_11,AMP(81))
C     Amplitude(s) for diagram number 82
      CALL FFV1_0(W(1,1),W(1,14),W(1,19),GC_1,AMP(82))
C     Amplitude(s) for diagram number 83
      CALL FFV2_3_0(W(1,1),W(1,14),W(1,27),GC_21,GC_23,AMP(83))
      JAMP(1) = (-1.000000000000000D+00)*AMP(1)+(-1.000000000000000D
     $ +00)*AMP(2)+(-1.000000000000000D+00)*AMP(3)+(
     $ -1.000000000000000D+00)*AMP(4)+(-1.000000000000000D+00)*AMP(5)
     $ +(-1.000000000000000D+00)*AMP(6)+(-1.000000000000000D+00)*AMP(7)
     $ +(-1.000000000000000D+00)*AMP(8)+(-1.000000000000000D+00)*AMP(9)
     $ +(-1.000000000000000D+00)*AMP(10)+(-1.000000000000000D+00)
     $ *AMP(11)+(-1.000000000000000D+00)*AMP(12)+(-1.000000000000000D
     $ +00)*AMP(13)+(-1.000000000000000D+00)*AMP(14)+(
     $ -1.000000000000000D+00)*AMP(15)+(-1.000000000000000D+00)*AMP(16)
     $ +(-1.000000000000000D+00)*AMP(17)+(-1.000000000000000D+00)
     $ *AMP(18)+(-1.000000000000000D+00)*AMP(19)+(-1.000000000000000D
     $ +00)*AMP(20)+(-1.000000000000000D+00)*AMP(21)+(
     $ -1.000000000000000D+00)*AMP(22)+(-1.000000000000000D+00)*AMP(23)
     $ +(-1.000000000000000D+00)*AMP(24)+(-1.000000000000000D+00)
     $ *AMP(25)+(-1.000000000000000D+00)*AMP(26)+(-1.000000000000000D
     $ +00)*AMP(27)+(-1.000000000000000D+00)*AMP(28)+(
     $ -1.000000000000000D+00)*AMP(29)+(-1.000000000000000D+00)*AMP(30)
     $ +(-1.000000000000000D+00)*AMP(31)+(-1.000000000000000D+00)
     $ *AMP(32)+(-1.000000000000000D+00)*AMP(33)+(-1.000000000000000D
     $ +00)*AMP(34)+(-1.000000000000000D+00)*AMP(35)+(
     $ -1.000000000000000D+00)*AMP(36)+(-1.000000000000000D+00)*AMP(37)
     $ +(-1.000000000000000D+00)*AMP(38)+(-1.000000000000000D+00)
     $ *AMP(39)+(-1.000000000000000D+00)*AMP(40)+(-1.000000000000000D
     $ +00)*AMP(41)+(-1.000000000000000D+00)*AMP(42)+(
     $ -1.000000000000000D+00)*AMP(43)+(-1.000000000000000D+00)*AMP(44)
     $ +(-1.000000000000000D+00)*AMP(45)+(-1.000000000000000D+00)
     $ *AMP(46)+(-1.000000000000000D+00)*AMP(47)+(-1.000000000000000D
     $ +00)*AMP(48)+(-1.000000000000000D+00)*AMP(49)+(
     $ -1.000000000000000D+00)*AMP(50)+(-1.000000000000000D+00)*AMP(51)
     $ +(-1.000000000000000D+00)*AMP(52)+(-1.000000000000000D+00)
     $ *AMP(53)+(-1.000000000000000D+00)*AMP(54)+(-1.000000000000000D
     $ +00)*AMP(55)+(-1.000000000000000D+00)*AMP(56)+(
     $ -1.000000000000000D+00)*AMP(57)+(-1.000000000000000D+00)*AMP(58)
     $ +(-1.000000000000000D+00)*AMP(59)+(-1.000000000000000D+00)
     $ *AMP(60)+(-1.000000000000000D+00)*AMP(61)+(-1.000000000000000D
     $ +00)*AMP(62)+(-1.000000000000000D+00)*AMP(63)+(
     $ -1.000000000000000D+00)*AMP(64)+(-1.000000000000000D+00)*AMP(65)
     $ +(-1.000000000000000D+00)*AMP(66)+(-1.000000000000000D+00)
     $ *AMP(67)+(-1.000000000000000D+00)*AMP(68)+(-1.000000000000000D
     $ +00)*AMP(69)+(-1.000000000000000D+00)*AMP(70)+(
     $ -1.000000000000000D+00)*AMP(71)+(-1.000000000000000D+00)*AMP(72)
     $ +(-1.000000000000000D+00)*AMP(73)+(-1.000000000000000D+00)
     $ *AMP(74)+(-1.000000000000000D+00)*AMP(75)+(-1.000000000000000D
     $ +00)*AMP(76)+(-1.000000000000000D+00)*AMP(77)+(
     $ -1.000000000000000D+00)*AMP(78)+(-1.000000000000000D+00)*AMP(79)
     $ +(-1.000000000000000D+00)*AMP(80)+(-1.000000000000000D+00)
     $ *AMP(81)+(-1.000000000000000D+00)*AMP(82)+(-1.000000000000000D
     $ +00)*AMP(83)
      MATRIX_3 = 0.D0
      DO I = 1, NCOLOR
        ZTEMP = (0.D0,0.D0)
        DO J = 1, NCOLOR
          ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
        ENDDO
        MATRIX_3 = MATRIX_3+ZTEMP*DCONJG(JAMP(I))
      ENDDO
      END

