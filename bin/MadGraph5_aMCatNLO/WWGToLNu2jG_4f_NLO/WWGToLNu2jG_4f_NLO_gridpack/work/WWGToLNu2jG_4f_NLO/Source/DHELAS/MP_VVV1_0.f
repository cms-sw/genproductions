C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     P(3,1)*Metric(1,2) - P(3,2)*Metric(1,2) - P(2,1)*Metric(1,3) +
C      P(2,3)*Metric(1,3) + P(1,2)*Metric(2,3) - P(1,3)*Metric(2,3)
C     
      SUBROUTINE MP_VVV1_0(V1, V2, V3, COUP,VERTEX)
      IMPLICIT NONE
      COMPLEX*32 CI
      PARAMETER (CI=(0Q0,1Q0))
      COMPLEX*32 COUP
      COMPLEX*32 P1(0:3)
      COMPLEX*32 P2(0:3)
      COMPLEX*32 P3(0:3)
      COMPLEX*32 TMP10
      COMPLEX*32 TMP12
      COMPLEX*32 TMP13
      COMPLEX*32 TMP14
      COMPLEX*32 TMP15
      COMPLEX*32 TMP16
      COMPLEX*32 TMP17
      COMPLEX*32 TMP18
      COMPLEX*32 TMP8
      COMPLEX*32 V1(*)
      COMPLEX*32 V2(*)
      COMPLEX*32 V3(*)
      COMPLEX*32 VERTEX
      P1(0) = V1(1)
      P1(1) = V1(2)
      P1(2) = V1(3)
      P1(3) = V1(4)
      P2(0) = V2(1)
      P2(1) = V2(2)
      P2(2) = V2(3)
      P2(3) = V2(4)
      P3(0) = V3(1)
      P3(1) = V3(2)
      P3(2) = V3(3)
      P3(3) = V3(4)
      TMP10 = (V3(5)*V1(5)-V3(6)*V1(6)-V3(7)*V1(7)-V3(8)*V1(8))
      TMP12 = (V1(5)*V2(5)-V1(6)*V2(6)-V1(7)*V2(7)-V1(8)*V2(8))
      TMP13 = (V3(5)*P1(0)-V3(6)*P1(1)-V3(7)*P1(2)-V3(8)*P1(3))
      TMP14 = (V3(5)*P2(0)-V3(6)*P2(1)-V3(7)*P2(2)-V3(8)*P2(3))
      TMP15 = (P1(0)*V2(5)-P1(1)*V2(6)-P1(2)*V2(7)-P1(3)*V2(8))
      TMP16 = (P3(0)*V2(5)-P3(1)*V2(6)-P3(2)*V2(7)-P3(3)*V2(8))
      TMP17 = (P2(0)*V1(5)-P2(1)*V1(6)-P2(2)*V1(7)-P2(3)*V1(8))
      TMP18 = (P3(0)*V1(5)-P3(1)*V1(6)-P3(2)*V1(7)-P3(3)*V1(8))
      TMP8 = (V3(5)*V2(5)-V3(6)*V2(6)-V3(7)*V2(7)-V3(8)*V2(8))
      VERTEX = COUP*(TMP10*(+CI*(TMP15)-CI*(TMP16))+(TMP12*(-CI*(TMP13)
     $ +CI*(TMP14))+TMP8*(-CI*(TMP17)+CI*(TMP18))))
      END


