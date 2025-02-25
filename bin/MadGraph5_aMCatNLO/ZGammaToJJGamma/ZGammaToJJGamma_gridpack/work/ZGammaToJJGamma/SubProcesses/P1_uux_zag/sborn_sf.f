      SUBROUTINE SBORN_SF(P_BORN,M,N,WGT)
      IMPLICIT NONE
      INCLUDE 'nexternal.inc'
      DOUBLE PRECISION P_BORN(0:3,NEXTERNAL-1),WGT
      DOUBLE COMPLEX WGT1(2)
      INTEGER M,N

C     b_sf_001 links partons 1 and 2 
      IF ((M.EQ.1 .AND. N.EQ.2).OR.(M.EQ.2 .AND. N.EQ.1)) THEN
        CALL SB_SF_001(P_BORN,WGT)

C       b_sf_002 links partons 1 and 5 
      ELSEIF ((M.EQ.1 .AND. N.EQ.5).OR.(M.EQ.5 .AND. N.EQ.1)) THEN
        CALL SB_SF_002(P_BORN,WGT)

C       b_sf_003 links partons 2 and 5 
      ELSEIF ((M.EQ.2 .AND. N.EQ.5).OR.(M.EQ.5 .AND. N.EQ.2)) THEN
        CALL SB_SF_003(P_BORN,WGT)

      ELSE
        WGT = 0D0
      ENDIF

      RETURN
      END
