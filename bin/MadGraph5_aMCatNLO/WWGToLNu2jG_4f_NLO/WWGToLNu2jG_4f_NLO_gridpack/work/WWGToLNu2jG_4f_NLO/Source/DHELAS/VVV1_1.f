C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     P(3,1)*Metric(1,2) - P(3,2)*Metric(1,2) - P(2,1)*Metric(1,3) +
C      P(2,3)*Metric(1,3) + P(1,2)*Metric(2,3) - P(1,3)*Metric(2,3)
C     
      SUBROUTINE VVV1_1(V2, V3, COUP, M1, W1,V1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP
      REAL*8 M1
      REAL*8 OM1
      COMPLEX*16 P1(0:3)
      COMPLEX*16 P2(0:3)
      COMPLEX*16 P3(0:3)
      COMPLEX*16 TMP13
      COMPLEX*16 TMP14
      COMPLEX*16 TMP15
      COMPLEX*16 TMP16
      COMPLEX*16 TMP19
      COMPLEX*16 TMP20
      COMPLEX*16 TMP8
      COMPLEX*16 V1(8)
      COMPLEX*16 V2(*)
      COMPLEX*16 V3(*)
      REAL*8 W1
      COMPLEX*16 DENOM
      OM1 = 0D0
      IF (M1.NE.0D0) OM1=1D0/M1**2
      P2(0) = V2(1)
      P2(1) = V2(2)
      P2(2) = V2(3)
      P2(3) = V2(4)
      P3(0) = V3(1)
      P3(1) = V3(2)
      P3(2) = V3(3)
      P3(3) = V3(4)
      V1(1) = +V2(1)+V3(1)
      V1(2) = +V2(2)+V3(2)
      V1(3) = +V2(3)+V3(3)
      V1(4) = +V2(4)+V3(4)
      P1(0) = -V1(1)
      P1(1) = -V1(2)
      P1(2) = -V1(3)
      P1(3) = -V1(4)
      TMP13 = (V3(5)*P1(0)-V3(6)*P1(1)-V3(7)*P1(2)-V3(8)*P1(3))
      TMP14 = (V3(5)*P2(0)-V3(6)*P2(1)-V3(7)*P2(2)-V3(8)*P2(3))
      TMP15 = (P1(0)*V2(5)-P1(1)*V2(6)-P1(2)*V2(7)-P1(3)*V2(8))
      TMP16 = (P3(0)*V2(5)-P3(1)*V2(6)-P3(2)*V2(7)-P3(3)*V2(8))
      TMP19 = (P1(0)*P2(0)-P1(1)*P2(1)-P1(2)*P2(2)-P1(3)*P2(3))
      TMP20 = (P1(0)*P3(0)-P1(1)*P3(1)-P1(2)*P3(2)-P1(3)*P3(3))
      TMP8 = (V3(5)*V2(5)-V3(6)*V2(6)-V3(7)*V2(7)-V3(8)*V2(8))
      DENOM = COUP/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 -CI
     $ * W1))
      V1(5)= DENOM*(OM1*P1(0)*(TMP8*(+CI*(TMP19)-CI*(TMP20))+(-CI
     $ *(TMP14*TMP15)+CI*(TMP13*TMP16)))+(TMP8*(-CI*(P2(0))+CI*(P3(0)))
     $ +(V2(5)*(-CI*(TMP13)+CI*(TMP14))+V3(5)*(+CI*(TMP15)-CI*(TMP16)))
     $ ))
      V1(6)= DENOM*(OM1*P1(1)*(TMP8*(+CI*(TMP19)-CI*(TMP20))+(-CI
     $ *(TMP14*TMP15)+CI*(TMP13*TMP16)))+(TMP8*(-CI*(P2(1))+CI*(P3(1)))
     $ +(V2(6)*(-CI*(TMP13)+CI*(TMP14))+V3(6)*(+CI*(TMP15)-CI*(TMP16)))
     $ ))
      V1(7)= DENOM*(OM1*P1(2)*(TMP8*(+CI*(TMP19)-CI*(TMP20))+(-CI
     $ *(TMP14*TMP15)+CI*(TMP13*TMP16)))+(TMP8*(-CI*(P2(2))+CI*(P3(2)))
     $ +(V2(7)*(-CI*(TMP13)+CI*(TMP14))+V3(7)*(+CI*(TMP15)-CI*(TMP16)))
     $ ))
      V1(8)= DENOM*(OM1*P1(3)*(TMP8*(+CI*(TMP19)-CI*(TMP20))+(-CI
     $ *(TMP14*TMP15)+CI*(TMP13*TMP16)))+(TMP8*(-CI*(P2(3))+CI*(P3(3)))
     $ +(V2(8)*(-CI*(TMP13)+CI*(TMP14))+V3(8)*(+CI*(TMP15)-CI*(TMP16)))
     $ ))
      END


