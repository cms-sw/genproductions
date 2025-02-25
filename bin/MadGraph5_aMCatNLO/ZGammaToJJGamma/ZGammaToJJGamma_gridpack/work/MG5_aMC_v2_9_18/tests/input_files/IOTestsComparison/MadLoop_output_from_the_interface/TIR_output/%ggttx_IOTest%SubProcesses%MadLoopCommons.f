      SUBROUTINE JOINPATH(STR1,STR2,PATH)

      CHARACTER*(*) STR1
      CHARACTER*(*) STR2
      CHARACTER*(*) PATH

      INTEGER I,J,K

      I =1
      DO WHILE (I.LE.LEN(STR1))
        IF(STR1(I:I).EQ.' ') GOTO 800
        PATH(I:I) = STR1(I:I)
        I=I+1
      ENDDO
 800  CONTINUE
      J=1
      DO WHILE (J.LE.LEN(STR2))
        IF(STR2(J:J).EQ.' ') GOTO 801
        PATH(I-1+J:I-1+J) = STR2(J:J)
        J=J+1
      ENDDO
 801  CONTINUE
      K=I+J-1
      DO WHILE (K.LE.LEN(PATH))
        PATH(K:K) = ' '
        K=K+1
      ENDDO

      RETURN

      END


      SUBROUTINE INITCOLLIER()
C     
C     INITIALISATION OF COLLIER
C     
C     
C     MODULE
C     
      USE COLLIER
C     
C     CONSTANTS
C     
      CHARACTER(LEN=*) NO_FOLDER
      PARAMETER (NO_FOLDER='')
      CHARACTER(LEN=*) FOLDEROUTPUT
      PARAMETER (FOLDEROUTPUT='COLLIER_output')

C     Force COLLIER to completely reset from scratch
      LOGICAL NORESET
      PARAMETER (NORESET=.FALSE.)
C     
C     LOCAL VARIABLES 
C     
      INTEGER N_CACHES
C     
C     GLOBAL VARIABLES 
C     
      INCLUDE 'MadLoopParams.inc'
C     Now obtain the overall maximum rank, maximum external lines
C     and maximal number of processes.
      INCLUDE 'global_specs.inc'

C     ----------
C     BEGIN CODE
C     ----------

C     Initialize Collier
      IF (.NOT.COLLIERCANOUTPUT) THEN
        CALL INIT_CLL(MAXNEXTERNAL,OVERALLMAXRANK,NO_FOLDER,NORESET)
      ELSE
        CALL INIT_CLL(MAXNEXTERNAL,OVERALLMAXRANK,FOLDEROUTPUT,NORESET)
      ENDIF

C     Set target accuracy, be conservative w.r.t user request
C     Keep in mind that COLLIER has been optimized for 1d-8.
      IF (COLLIERREQUIREDACCURACY.EQ.-1.0D0) THEN
        CALL SETREQACC_CLL(MLSTABTHRES*1.0D-3)
      ELSE
        CALL SETREQACC_CLL(COLLIERREQUIREDACCURACY)
      ENDIF

C     Set COLLIER mode
      CALL SETMODE_CLL(COLLIERMODE)

C     Set the global cache strategy
C     If we use the caches for the poles as well, then it is a total of
C     4 caches per process.
      IF (COLLIERUSECACHEFORPOLES) THEN
        N_CACHES =4*NPROCS
      ELSE
        N_CACHES =NPROCS
      ENDIF
      IF (COLLIERGLOBALCACHE.EQ.-1) THEN
        CALL INITCACHESYSTEM_CLL(N_CACHES,MAXNEXTERNAL)
      ELSEIF(COLLIERGLOBALCACHE.GT.0) THEN
        CALL INITCACHESYSTEM_CLL(N_CACHES,COLLIERGLOBALCACHE)
      ENDIF

C     Make sure to start by first switching off all cache
      IF (COLLIERGLOBALCACHE.NE.0) THEN
        CALL SWITCHOFFCACHESYSTEM_CLL()
      ENDIF

C     Make sure no COLLIER error can interrupt the code
      CALL SWITCHOFFERRSTOP_CLL()

C     Specify below your other custom COLLIER parameter settings 
C     [user_specific_COLLIER_settings]

      END

      SUBROUTINE SET_FORBID_HEL_DOUBLECHECK(ONOFF)
C     
C     Give the possibility to overwrite the value of MadLoopParams.dat
C     for the helicity double checking.
C     Make sure to call this subroutine before the first time you 
C     call MadLoop.
C     
      IMPLICIT NONE
C     
C     ARGUMENT
C     
      LOGICAL ONOFF
C     
C     GLOBAL VARIABLES
C     
      LOGICAL FORBID_HEL_DOUBLECHECK
      DATA FORBID_HEL_DOUBLECHECK/.FALSE./
      COMMON/FORBID_HEL_DOUBLECHECK/FORBID_HEL_DOUBLECHECK
C     ----------
C     BEGIN CODE
C     ----------
      FORBID_HEL_DOUBLECHECK = ONOFF
      END

      SUBROUTINE SETMADLOOPPATH(PATH)

      CHARACTER(512) PATH
      CHARACTER(512) DUMMY
      CHARACTER(512) EPATH  ! path of the executable
      INTEGER POS
      CHARACTER(512) PREFIX,FPATH
      CHARACTER(17) NAMETOCHECK
      PARAMETER (NAMETOCHECK='MadLoopParams.dat')

      LOGICAL ML_INIT
      DATA ML_INIT/.TRUE./
      COMMON/ML_INIT/ML_INIT

      LOGICAL CTINIT,TIRINIT,GOLEMINIT,SAMURAIINIT,NINJAINIT
     $ ,COLLIERINIT
      DATA CTINIT,TIRINIT,GOLEMINIT,SAMURAIINIT,NINJAINIT,COLLIERINIT
     $ /.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      COMMON/REDUCTIONCODEINIT/CTINIT, TIRINIT, GOLEMINIT, SAMURAIINIT
     $ , NINJAINIT, COLLIERINIT


      CHARACTER(512) MLPATH
      DATA MLPATH/'[[NA]]'/
      COMMON/MLPATH/MLPATH

      INTEGER I

C     Just a dummy call for LD to pick up this function
C     when creating the BLHA2 dynamic library
      DUMMY = ' '
      CALL SETPARA2(DUMMY)

      IF (LEN(PATH).GE.4 .AND. PATH(1:4).EQ.'auto') THEN
        IF (MLPATH(1:6).EQ.'[[NA]]') THEN
C         Try to automatically find the path
          PREFIX='./'
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          OPEN(1, FILE=FPATH, ERR=1, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 1        CONTINUE
          CLOSE(1)
          PREFIX='./MadLoop5_resources/'
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          OPEN(1, FILE=FPATH, ERR=2, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 2        CONTINUE
          CLOSE(1)
          PREFIX='../MadLoop5_resources/'
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          OPEN(1, FILE=FPATH, ERR=3, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 3        CONTINUE
          CLOSE(1)
C         
C         Try to automatically find the path from the executable
C          location
C         particularly usefull in gridpack readonly mode
C         
          CALL GETARG(0,PATH)  !path is the PATH to the madevent executable (either global or from launching directory)
          POS = INDEX(PATH,'/',.TRUE.)
          PREFIX = PATH(:POS)
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          WRITE(*,*) 'test', FPATH
          OPEN(1, FILE=FPATH, ERR=4, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 4        CONTINUE
          CLOSE(1)
          PREFIX= PREFIX // '/MadLoop5_resources/'
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          WRITE(*,*) 'test', FPATH
          OPEN(1, FILE=FPATH, ERR=5, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 5        CONTINUE
          CLOSE(1)
          PREFIX= PATH(:POS) // '/../MadLoop5_resources/'
          CALL JOINPATH(PREFIX,NAMETOCHECK,FPATH)
          WRITE(*,*) 'test', FPATH
          OPEN(1, FILE=FPATH, ERR=6, STATUS='OLD',ACTION='READ')
          MLPATH=PREFIX
          GOTO 10
 6        CONTINUE
          CLOSE(1)

C         We could not automatically find the auxiliary files
          WRITE(*,*) '==='
          WRITE(*,*) 'ERROR: MadLoop5 could not automatically find the'
     $     //' file MadLoopParams.dat.'
          WRITE(*,*) '==='
          WRITE(*,*) '(Try using <CALL setMadLoopPath(/my/path)>'
     $     //' (before your first call to MadLoop) in order to set the'
     $     //' directory where this file is located as well as  other'
     $     //' auxiliary files, such as <xxx>_ColorNumFactors.dat,'
     $     //' <xxx>_ColorDenomFactors.dat, etc..)'
          STOP
 10       CONTINUE
          CLOSE(1)
          RETURN
        ENDIF
      ELSE
C       Use the one specified by the user
C       Make sure there is a separator added
        I =1
        DO WHILE (I.LE.LEN(PATH) .AND. PATH(I:I).NE.' ')
          I=I+1
        ENDDO
        IF (PATH(I-1:I-1).NE.'/') THEN
          PATH(I:I) = '/'
        ENDIF
        MLPATH=PATH
      ENDIF

C     Check that the FilePath set is correct
      CALL JOINPATH(MLPATH,NAMETOCHECK,FPATH)
      OPEN(1, FILE=FPATH, ERR=33, STATUS='OLD',ACTION='READ')
      GOTO 11
 33   CONTINUE
      CLOSE(1)
      WRITE(*,*) '==='
      WRITE(*,*) 'ERROR: The MadLoop5 auxiliary files could not be'
     $ //' found in ',MLPATH
      WRITE(*,*) '==='
      STOP
 11   CONTINUE
      CLOSE(1)

      END

      INTEGER FUNCTION SET_RET_CODE_U(MLRED,DOING_QP,STABLE)
C     
C     This functions returns the value of U
C     
C     
C     U == 0
C     Not stable.
C     U == 1
C     Stable with CutTools in double precision.
C     U == 2
C     Stable with PJFry++.
C     U == 3
C     Stable with IREGI.
C     U == 4
C     Stable with Golem95.
C     U == 5
C     Stable with Samurai.
C     U == 6
C     Stable with Ninja in double precision.
C     U == 7
C     Stable with COLLIER.
C     U == 8
C     Stable with Ninja in quadruple precision.
C     U == 9
C     Stable with CutTools in quadruple precision.
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER MLRED
      LOGICAL DOING_QP,STABLE
C     
C     LOCAL VARIABLES
C     
C     
C     FUNCTION
C     
C     
C     BEGIN CODE
C     
      IF(.NOT.STABLE)THEN
        SET_RET_CODE_U=0
        RETURN
      ENDIF
      IF(DOING_QP)THEN
        IF(MLRED.EQ.1)THEN
          SET_RET_CODE_U=9
          RETURN
        ELSEIF(MLRED.EQ.6)THEN
          SET_RET_CODE_U=8
          RETURN
        ELSE
          STOP 'Only CutTools and Ninja can use quardruple precision'
        ENDIF
      ENDIF
      IF(MLRED.GE.1.AND.MLRED.LE.7)THEN
        SET_RET_CODE_U=MLRED
      ELSE
        STOP 'Only CutTools, PJFry++, IREGI, Golem95, Samurai, Ninja'
     $   //' and COLLIER are available'
      ENDIF
      END

      SUBROUTINE DETECT_LOOPLIB(LIBNUM,NLOOPLINE,RANK,COMPLEX_MASS
     $ ,HAS_HEFT_VERTEX,MAX_SPIN_CONNECTED_TO_LOOP,LPASS)
C     
C     DETECT WHICH LOOP LIB PASSED
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER LIBNUM,NLOOPLINE,RANK,MAX_SPIN_CONNECTED_TO_LOOP
C     The argument HAS_HEFT_VERTEX is only to implement correctly
C      CutTools limitation
      LOGICAL COMPLEX_MASS,LPASS,HAS_HEFT_VERTEX
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------
      IF(LIBNUM.EQ.1)THEN
C       CutTools
        CALL DETECT_CUTTOOLS(NLOOPLINE,RANK,COMPLEX_MASS
     $   ,HAS_HEFT_VERTEX,MAX_SPIN_CONNECTED_TO_LOOP,LPASS)
      ELSEIF(LIBNUM.EQ.2)THEN
C       PJFry++
        CALL DETECT_PJFRY(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSEIF(LIBNUM.EQ.3)THEN
C       IREGI
        CALL DETECT_IREGI(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSEIF(LIBNUM.EQ.4)THEN
C       Golem95
        CALL DETECT_GOLEM(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSEIF(LIBNUM.EQ.5)THEN
C       Samurai
        CALL DETECT_SAMURAI(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSEIF(LIBNUM.EQ.6)THEN
C       Ninja 
        CALL DETECT_NINJA(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSEIF(LIBNUM.EQ.7)THEN
C       Collier 
        CALL DETECT_COLLIER(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
      ELSE
        STOP 'Only CutTools, PJFry++, IREGI, Golem95, Samurai, Ninja'
     $   //' and COLLIER are available'
      ENDIF
      RETURN
      END

      SUBROUTINE DETECT_CUTTOOLS(NLOOPLINE,RANK,COMPLEX_MASS
     $ ,HAS_HEFT_VERTEX,MAX_SPIN_CONNECTED_TO_LOOP,LPASS)
C     
C     DETECT whether CUTTOOLS CAN BE USED OR NOT
C     
      IMPLICIT NONE

C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      INTEGER MAX_SPIN_CONNECTED_TO_LOOP
      LOGICAL COMPLEX_MASS,LPASS,HAS_HEFT_VERTEX
C     
C     LOCAL VARIABLES
C     
      INTEGER MAX_RANK
C     ----------
C     BEGIN CODE
C     ----------
      LPASS=.TRUE.
C     The limit of 10 loop lines is just a parameter hardcoded in
C      CutTools sources.
C     It can easily be increased if necessary.
C     Also in the presence of spin2 particles, RANK=NLOOPLINE+1 is not
C      supported,
C     or in general whenever the higher rank doesn't come from the
C      Higgs effective vertex.

      IF (MAX_SPIN_CONNECTED_TO_LOOP.LE.3.AND.HAS_HEFT_VERTEX) THEN
        MAX_RANK = NLOOPLINE+1
      ELSE
        MAX_RANK = NLOOPLINE
      ENDIF

      IF( (RANK.GT.MAX_RANK).OR.(NLOOPLINE.GT.10) ) THEN
        LPASS=.FALSE.
      ENDIF

      RETURN
      END

      SUBROUTINE DETECT_SAMURAI(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     DETECT whether Samurai CAN BE USED OR NOT
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------
      LPASS=.TRUE.
C     The limit of 8 loop lines is just a parameter hardcoded in
C      Samurai sources.
C     It can easily be increased if necessary.
      IF((NLOOPLINE+1.LT.RANK).OR.(NLOOPLINE.GT.8)) THEN
        LPASS=.FALSE.
      ENDIF
      RETURN
      END

      SUBROUTINE DETECT_NINJA(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     Detect whether Ninja can be used or not
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------
      LPASS=.TRUE.
C     The limit of rank 20 is just a parameter hardcoded in Ninja
C      sources.
C     It can easily be increased if necessary.
      IF((NLOOPLINE+1.LT.RANK).OR.(RANK.GE.20)) THEN
        LPASS=.FALSE.
      ENDIF
      RETURN
      END

      SUBROUTINE DETECT_COLLIER(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     Detect whether Collier can be used or not
C     
      USE COLLIER
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
      INTEGER CURRENT_COLLIERMODE
C     
C     GLOBAL VARIABLES
C     
      LOGICAL CTINIT,TIRINIT,GOLEMINIT,SAMURAIINIT,NINJAINIT
     $ ,COLLIERINIT
      COMMON/REDUCTIONCODEINIT/CTINIT, TIRINIT, GOLEMINIT, SAMURAIINIT
     $ , NINJAINIT, COLLIERINIT
      INCLUDE 'MadLoopParams.inc'
C     ----------
C     BEGIN CODE
C     ----------
      IF (.NOT.COLLIERINIT) THEN
        CALL GETMODE_CLL(CURRENT_COLLIERMODE)
      ELSE
        CURRENT_COLLIERMODE = COLLIERMODE
      ENDIF
      LPASS=.TRUE.
      IF (CURRENT_COLLIERMODE.NE.1) THEN
C       The DD branch is used and it has limitations
        IF((NLOOPLINE.GT.6).OR.(RANK.GT.NLOOPLINE)) THEN
          LPASS=.FALSE.
        ENDIF
      ELSE
C       Limitations of the COLI branch are academic.
        LPASS=.TRUE.
      ENDIF
      RETURN
      END

      SUBROUTINE DETECT_PJFRY(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     DETECT whether PJFRY++ CAN BE USED OR NOT
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------
      LPASS=.TRUE.
      IF(NLOOPLINE.LT.RANK.OR.RANK.GT.5.OR.NLOOPLINE.GT.5.OR.COMPLEX_MA
     $SS.OR.NLOOPLINE.EQ.1) THEN
        LPASS=.FALSE.
      ENDIF
      RETURN
      END

      SUBROUTINE DETECT_IREGI(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     DETECT whether IREGI CAN BE USED OR NOT
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------
C     Stability studies show that IREGI is completely unstable at rank
C      7 and above.
      LPASS=.TRUE.
      IF(NLOOPLINE.GE.8.OR.RANK.GE.7)LPASS=.FALSE.
      RETURN
      END

      SUBROUTINE DETECT_GOLEM(NLOOPLINE,RANK,COMPLEX_MASS,LPASS)
C     
C     DETECT whether Golem95 CAN BE USED OR NOT
C     
      IMPLICIT NONE
C     
C     CONSTANTS
C     
C     
C     ARGUMENTS
C     
      INTEGER NLOOPLINE,RANK
      LOGICAL COMPLEX_MASS,LPASS
C     
C     LOCAL VARIABLES
C     
C     
C     GLOBAL VARIABLES
C     
C     ----------
C     BEGIN CODE
C     ----------

      LPASS=.TRUE.
      IF(NLOOPLINE.GE.7.OR.RANK.GE.7.OR.NLOOPLINE.LE.1)LPASS=.FALSE.
      IF(NLOOPLINE.LE.5.AND.RANK.GT.NLOOPLINE+1)LPASS=.FALSE.
      IF(NLOOPLINE.EQ.6.AND.RANK.GT.NLOOPLINE)LPASS=.FALSE.
      RETURN
      END

C     Now some sorting related routines. Only to be used for small 
C     arrays since these are not the most optimized sorting algorithms.

        ! --------------------------------------------------------------------
        ! INTEGER FUNCTION  FindMinimum():
        !    This function returns the location of the minimum in the section
        ! between Start and End.
        ! --------------------------------------------------------------------

      INTEGER FUNCTION  FINDMINIMUM(X, MSTART, MEND)
      IMPLICIT  NONE
      INTEGER MAXNREF_EVALS
      PARAMETER (MAXNREF_EVALS=100)
      DOUBLE PRECISION, DIMENSION(MAXNREF_EVALS), INTENT(IN) :: X
      INTEGER, INTENT(IN)							  :: MSTART, MEND
      INTEGER										  :: MINIMUM
      INTEGER										  :: LOCATION
      INTEGER										  :: I

      MINIMUM  = X(MSTART)  ! assume the first is the min
      LOCATION = MSTART  ! record its position
      DO I = MSTART+1, MEND  ! start with next elements
        IF (X(I) < MINIMUM) THEN  !   if x(i) less than the min?
          MINIMUM  = X(I)  !      Yes, a new minimum found
          LOCATION = I  !      record its position
          END IF
          END DO
          FINDMINIMUM = LOCATION  ! return the position
          END FUNCTION  FINDMINIMUM

            ! --------------------------------------------------------------------
            ! SUBROUTINE  Swap():
            !    This subroutine swaps the values of its two formal arguments.
            ! --------------------------------------------------------------------

          SUBROUTINE  SWAP(A, B)
          IMPLICIT  NONE
          REAL*8,  INTENT(INOUT) :: A, B
          REAL*8                 :: TEMP

          TEMP = A
          A    = B
          B    = TEMP
          END SUBROUTINE  SWAP

            ! --------------------------------------------------------------------
            ! SUBROUTINE  Sort():
            !    This subroutine receives an array x() and sorts it into ascending
            ! order.
            ! --------------------------------------------------------------------

          SUBROUTINE  SORT(X, MSIZE)
          IMPLICIT  NONE
          INTEGER MAXNREF_EVALS
          PARAMETER (MAXNREF_EVALS=100)
          REAL*8, DIMENSION(MAXNREF_EVALS), INTENT(INOUT)  :: X
          INTEGER, INTENT(IN)							   :: MSIZE
          INTEGER										   :: I
          INTEGER										   :: LOCATION
          INTEGER 										   :: FINDMINIMUM
          DO I = 1, MSIZE-1  ! except for the last
            LOCATION = FINDMINIMUM(X, I, MSIZE)  ! find min from this to last
            CALL  SWAP(X(I), X(LOCATION))  ! swap this and the minimum
            END DO
            END SUBROUTINE  SORT

              ! --------------------------------------------------------------------
              ! REAL*8 FUNCTION  Median() :
              !    This function receives an array X of N entries, copies its value
              ! to a local array Temp(), sorts Temp() and computes the median.
              !    The returned value is of REAL type.
              ! --------------------------------------------------------------------

            REAL*8 FUNCTION  MEDIAN(X, N)
            IMPLICIT  NONE
            INTEGER MAXNREF_EVALS
            PARAMETER (MAXNREF_EVALS=100)
            REAL*8, DIMENSION(MAXNREF_EVALS), INTENT(IN)  :: X
            INTEGER, INTENT(IN)                			  :: N
            REAL*8, DIMENSION(MAXNREF_EVALS)              :: TEMP
            INTEGER                                         :: I

            DO I = 1, N  ! make a copy
              TEMP(I) = X(I)
              END DO
              CALL  SORT(TEMP, N)  ! sort the copy
              IF (MOD(N,2) == 0) THEN  ! compute the median
                MEDIAN = (TEMP(N/2) + TEMP(N/2+1)) / 2.0D0
              ELSE
                MEDIAN = TEMP(N/2+1)
                END IF
                END FUNCTION  MEDIAN


