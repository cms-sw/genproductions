C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Metric(1,4)*Metric(2,3) - (Metric(1,3)*Metric(2,4))/2. -
C      (Metric(1,2)*Metric(3,4))/2.
C     
      SUBROUTINE VVVV5_4(V1, V2, V3, COUP, M4, W4,V4)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP
      REAL*8 M4
      REAL*8 OM4
      COMPLEX*16 P4(0:3)
      COMPLEX*16 TMP10
      COMPLEX*16 TMP12
      COMPLEX*16 TMP22
      COMPLEX*16 TMP23
      COMPLEX*16 TMP24
      COMPLEX*16 TMP8
      COMPLEX*16 V1(*)
      COMPLEX*16 V2(*)
      COMPLEX*16 V3(*)
      COMPLEX*16 V4(8)
      REAL*8 W4
      COMPLEX*16 DENOM
      OM4 = 0D0
      IF (M4.NE.0D0) OM4=1D0/M4**2
      V4(1) = +V1(1)+V2(1)+V3(1)
      V4(2) = +V1(2)+V2(2)+V3(2)
      V4(3) = +V1(3)+V2(3)+V3(3)
      V4(4) = +V1(4)+V2(4)+V3(4)
      P4(0) = -V4(1)
      P4(1) = -V4(2)
      P4(2) = -V4(3)
      P4(3) = -V4(4)
      TMP10 = (V3(5)*V1(5)-V3(6)*V1(6)-V3(7)*V1(7)-V3(8)*V1(8))
      TMP12 = (V1(5)*V2(5)-V1(6)*V2(6)-V1(7)*V2(7)-V1(8)*V2(8))
      TMP22 = (V1(5)*P4(0)-V1(6)*P4(1)-V1(7)*P4(2)-V1(8)*P4(3))
      TMP23 = (V2(5)*P4(0)-V2(6)*P4(1)-V2(7)*P4(2)-V2(8)*P4(3))
      TMP24 = (V3(5)*P4(0)-V3(6)*P4(1)-V3(7)*P4(2)-V3(8)*P4(3))
      TMP8 = (V3(5)*V2(5)-V3(6)*V2(6)-V3(7)*V2(7)-V3(8)*V2(8))
      DENOM = COUP/(P4(0)**2-P4(1)**2-P4(2)**2-P4(3)**2 - M4 * (M4 -CI
     $ * W4))
      V4(5)= DENOM*1D0/2D0*(OM4*-P4(0)*(-2D0 * CI*(TMP8*TMP22)+CI
     $ *(TMP10*TMP23+TMP12*TMP24))+(-2D0 * CI*(V1(5)*TMP8)+CI*(V2(5)
     $ *TMP10+V3(5)*TMP12)))
      V4(6)= DENOM*1D0/2D0*(OM4*-P4(1)*(-2D0 * CI*(TMP8*TMP22)+CI
     $ *(TMP10*TMP23+TMP12*TMP24))+(-2D0 * CI*(V1(6)*TMP8)+CI*(V2(6)
     $ *TMP10+V3(6)*TMP12)))
      V4(7)= DENOM*1D0/2D0*(OM4*-P4(2)*(-2D0 * CI*(TMP8*TMP22)+CI
     $ *(TMP10*TMP23+TMP12*TMP24))+(-2D0 * CI*(V1(7)*TMP8)+CI*(V2(7)
     $ *TMP10+V3(7)*TMP12)))
      V4(8)= DENOM*1D0/2D0*(OM4*-P4(3)*(-2D0 * CI*(TMP8*TMP22)+CI
     $ *(TMP10*TMP23+TMP12*TMP24))+(-2D0 * CI*(V1(8)*TMP8)+CI*(V2(8)
     $ *TMP10+V3(8)*TMP12)))
      END


