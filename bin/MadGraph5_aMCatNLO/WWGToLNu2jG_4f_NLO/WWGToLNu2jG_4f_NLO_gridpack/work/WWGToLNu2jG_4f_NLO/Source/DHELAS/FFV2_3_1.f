C     This File is Automatically generated by ALOHA 
C     The process calculated in this file is: 
Coup(1) * (Gamma(3,2,-1)*ProjM(-1,1)) + Coup(2) * (Gamma(3,2,-1)*ProjM(-1,1) - 2*Gamma(3,2,-1)*ProjP(-1,1))
C     
      SUBROUTINE FFV2_3_1(F2, V3, COUP1, COUP2, M1, W1,F1)
      IMPLICIT NONE
      COMPLEX*16 CI
      PARAMETER (CI=(0D0,1D0))
      COMPLEX*16 COUP1
      COMPLEX*16 COUP2
      COMPLEX*16 F1(8)
      COMPLEX*16 F2(*)
      REAL*8 M1
      COMPLEX*16 P1(0:3)
      COMPLEX*16 V3(*)
      REAL*8 W1
      COMPLEX*16 DENOM
      F1(1) = +F2(1)+V3(1)
      F1(2) = +F2(2)+V3(2)
      F1(3) = +F2(3)+V3(3)
      F1(4) = +F2(4)+V3(4)
      P1(0) = -F1(1)
      P1(1) = -F1(2)
      P1(2) = -F1(3)
      P1(3) = -F1(4)
      DENOM = 1D0/(P1(0)**2-P1(1)**2-P1(2)**2-P1(3)**2 - M1 * (M1 -CI*
     $  W1))
      F1(5)= DENOM*2D0 * CI*(COUP2*(F2(5)*(P1(0)*(V3(5)-V3(8))+(P1(1)
     $ *(-V3(6)+CI*(V3(7)))+(P1(2)*(-1D0)*(+CI*(V3(6))+V3(7))+P1(3)
     $ *(V3(5)-V3(8)))))+(F2(6)*(P1(0)*(-1D0)*(V3(6)+CI*(V3(7)))+(P1(1)
     $ *(V3(5)+V3(8))+(P1(2)*(+CI*(V3(5)+V3(8)))-P1(3)*(V3(6)+CI*(V3(7)
     $ )))))+M1*(F2(7)*1D0/2D0*(V3(5)+V3(8))+1D0/2D0*(F2(8)*(V3(6)+CI
     $ *(V3(7)))))))+COUP1*M1*(F2(7)*1D0/2D0*(V3(5)+V3(8))+1D0/2D0
     $ *(F2(8)*(V3(6)+CI*(V3(7))))))
      F1(6)= DENOM*(-2D0 * CI)*(COUP2*(F2(5)*(P1(0)*(V3(6)-CI*(V3(7)))
     $ +(P1(1)*(-V3(5)+V3(8))+(P1(2)*(+CI*(V3(5))-CI*(V3(8)))+P1(3)*(
     $ -V3(6)+CI*(V3(7))))))+(F2(6)*(P1(0)*(-1D0)*(V3(5)+V3(8))+(P1(1)
     $ *(V3(6)+CI*(V3(7)))+(P1(2)*(-CI*(V3(6))+V3(7))+P1(3)*(V3(5)
     $ +V3(8)))))+M1*(F2(7)*1D0/2D0*(-V3(6)+CI*(V3(7)))+1D0/2D0*(F2(8)
     $ *(-V3(5)+V3(8))))))+COUP1*M1*(F2(7)*1D0/2D0*(-V3(6)+CI*(V3(7)))
     $ +1D0/2D0*(F2(8)*(-V3(5)+V3(8)))))
      F1(7)= DENOM*CI*(COUP2*(F2(7)*(P1(0)*(-1D0)*(V3(5)+V3(8))+(P1(1)
     $ *(V3(6)-CI*(V3(7)))+(P1(2)*(+CI*(V3(6))+V3(7))+P1(3)*(V3(5)
     $ +V3(8)))))+(F2(8)*(P1(0)*(-1D0)*(V3(6)+CI*(V3(7)))+(P1(1)*(V3(5)
     $ -V3(8))+(P1(2)*(+CI*(V3(5))-CI*(V3(8)))+P1(3)*(V3(6)+CI*(V3(7)))
     $ )))+M1*(F2(5)*2D0*(-V3(5)+V3(8))+2D0*(F2(6)*(V3(6)+CI*(V3(7)))))
     $ ))+COUP1*(F2(7)*(P1(0)*(-1D0)*(V3(5)+V3(8))+(P1(1)*(V3(6)-CI
     $ *(V3(7)))+(P1(2)*(+CI*(V3(6))+V3(7))+P1(3)*(V3(5)+V3(8)))))
     $ +F2(8)*(P1(0)*(-1D0)*(V3(6)+CI*(V3(7)))+(P1(1)*(V3(5)-V3(8))
     $ +(P1(2)*(+CI*(V3(5))-CI*(V3(8)))+P1(3)*(V3(6)+CI*(V3(7))))))))
      F1(8)= DENOM*(-CI)*(COUP2*(F2(7)*(P1(0)*(V3(6)-CI*(V3(7)))+(P1(1)
     $ *(-1D0)*(V3(5)+V3(8))+(P1(2)*(+CI*(V3(5)+V3(8)))+P1(3)*(V3(6)
     $ -CI*(V3(7))))))+(F2(8)*(P1(0)*(V3(5)-V3(8))+(P1(1)*(-1D0)*(V3(6)
     $ +CI*(V3(7)))+(P1(2)*(+CI*(V3(6))-V3(7))+P1(3)*(V3(5)-V3(8)))))
     $ +M1*(F2(5)*2D0*(-V3(6)+CI*(V3(7)))+2D0*(F2(6)*(V3(5)+V3(8))))))
     $ +COUP1*(F2(7)*(P1(0)*(V3(6)-CI*(V3(7)))+(P1(1)*(-1D0)*(V3(5)
     $ +V3(8))+(P1(2)*(+CI*(V3(5)+V3(8)))+P1(3)*(V3(6)-CI*(V3(7))))))
     $ +F2(8)*(P1(0)*(V3(5)-V3(8))+(P1(1)*(-1D0)*(V3(6)+CI*(V3(7)))
     $ +(P1(2)*(+CI*(V3(6))-V3(7))+P1(3)*(V3(5)-V3(8)))))))
      END


