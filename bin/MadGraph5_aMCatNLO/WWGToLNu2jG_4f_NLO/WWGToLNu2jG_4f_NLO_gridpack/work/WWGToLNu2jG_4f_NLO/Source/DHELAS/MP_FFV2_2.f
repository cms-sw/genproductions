C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
C     Gamma(3,2,-1)*ProjM(-1,1)
C     
      SUBROUTINE MP_FFV2_2(F1, V3, COUP, M2, W2,F2)
      IMPLICIT NONE
      COMPLEX*32 CI
      PARAMETER (CI=(0Q0,1Q0))
      COMPLEX*32 COUP
      COMPLEX*32 F1(*)
      COMPLEX*32 F2(8)
      REAL*16 M2
      COMPLEX*32 P2(0:3)
      COMPLEX*32 V3(*)
      REAL*16 W2
      COMPLEX*32 DENOM
      F2(1) = +F1(1)+V3(1)
      F2(2) = +F1(2)+V3(2)
      F2(3) = +F1(3)+V3(3)
      F2(4) = +F1(4)+V3(4)
      P2(0) = -F2(1)
      P2(1) = -F2(2)
      P2(2) = -F2(3)
      P2(3) = -F2(4)
      DENOM = COUP/(P2(0)**2-P2(1)**2-P2(2)**2-P2(3)**2 - M2 * (M2 -CI
     $ * W2))
      F2(5)= DENOM*CI*(F1(5)*(P2(0)*(V3(5)+V3(8))+(P2(1)*(-1Q0)*(V3(6)
     $ +CI*(V3(7)))+(P2(2)*(+CI*(V3(6))-V3(7))-P2(3)*(V3(5)+V3(8)))))
     $ +F1(6)*(P2(0)*(V3(6)-CI*(V3(7)))+(P2(1)*(-V3(5)+V3(8))+(P2(2)*(
     $ +CI*(V3(5))-CI*(V3(8)))+P2(3)*(-V3(6)+CI*(V3(7)))))))
      F2(6)= DENOM*CI*(F1(5)*(P2(0)*(V3(6)+CI*(V3(7)))+(P2(1)*(-1Q0)
     $ *(V3(5)+V3(8))+(P2(2)*(-1Q0)*(+CI*(V3(5)+V3(8)))+P2(3)*(V3(6)
     $ +CI*(V3(7))))))+F1(6)*(P2(0)*(V3(5)-V3(8))+(P2(1)*(-V3(6)+CI
     $ *(V3(7)))+(P2(2)*(-1Q0)*(+CI*(V3(6))+V3(7))+P2(3)*(V3(5)-V3(8)))
     $ )))
      F2(7)= DENOM*(-CI )* M2*(F1(5)*(-1Q0)*(V3(5)+V3(8))+F1(6)*(-V3(6)
     $ +CI*(V3(7))))
      F2(8)= DENOM*CI * M2*(F1(5)*(V3(6)+CI*(V3(7)))+F1(6)*(V3(5)-V3(8)
     $ ))
      END


