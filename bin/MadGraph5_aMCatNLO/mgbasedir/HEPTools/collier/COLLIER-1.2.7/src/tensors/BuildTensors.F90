!!
!!  File BuildTensors.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *************************
!  *  module BuildTensors  *
!  *    by Lars Hofer      *
!  *************************
!
!  global variables:
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module BuildTensors

  use reductionTN
  use InitTensors

  implicit none



contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorA_list(TA,TAuv,TAerr,CA,CAuv,CAerr,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorA_list(TA,TAuv,TAerr,CoefsA,CoefsAuv,CoefsAerr,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: CoefsA(0:rmax/2), CoefsAuv(0:rmax/2)
    double precision, intent(in) :: CoefsAerr(0:rmax)
    double complex, intent(out) :: TA(RtS(rmax)), TAuv(RtS(rmax))
    double precision :: TAerr(0:rmax)
    double complex :: CA
    integer :: mu1,mu2,nsum,mu,nu,r,a,cnt

    TA = 0d0
    TA(1) = CoefsA(0)
    TAerr = CoefsAerr(0)

    do nsum=1,rmax/2
      CA = CoefsA(nsum)
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TA(AddGtab(1,nu)) = TA(AddGtab(1,nu)) + CA*CFtab(1,nu)
      end do
    end do

    if (calcUV_ten) then
      TAuv = 0d0
      TAuv(1) = CoefsAuv(0)

      do nsum=1,rmax/2
        CA = CoefsAuv(nsum)
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TAuv(AddGtab(1,nu)) = TAuv(AddGtab(1,nu)) + CA*CFtab(1,nu)
        end do
      end do

    end if


  end subroutine CalcTensorA_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorA(TA,TAuv,TAerr,CA,CAuv,CAerr,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorA(TA,TAuv,TAerr,CoefsA,CoefsAuv,CoefsAerr,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: CoefsA(0:rmax/2), CoefsAuv(0:rmax/2)
    double precision, intent(in) :: CoefsAerr(0:rmax)
    double complex, intent(out) :: TA(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TAuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TAerr(0:rmax)
    double complex :: TA_aux(RtS(rmax)), TAuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorA_list(TA_aux,TAuv_aux,TAerr,CoefsA,CoefsAuv,CoefsAerr,rmax)

    do mu=1,RtS(rmax)
      TA(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TA_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TAuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TAuv_aux(mu)
      end do
    end if

  end subroutine CalcTensorA





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorB_list(TB,TBuv,TBerr,CoefsB,CoefsBuv,CoefsBerr,mom,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorB_list(TB,TBuv,TBerr,CoefsB,CoefsBuv,CoefsBerr,mom,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: mom(0:3)
    double complex, intent(in) :: CoefsB(0:rmax/2,0:rmax), CoefsBuv(0:rmax/2,0:rmax)
    double precision, intent(in) :: CoefsBerr(0:rmax)
    double complex, intent(out) :: TB(RtS(rmax)), TBuv(RtS(rmax))
    double precision, intent(out) :: TBerr(0:rmax)
    double complex :: MomTen(RtS(rmax)), CB, Pmu
    double precision :: MomMax
    integer :: mu1,mu2,nsum,mu,nu,r,a,cnt


    TB = 0d0
    TB(1) = CoefsB(0,0)
    TBerr = 0d0
    TBerr(0) = CoefsBerr(0)

    do nsum=1,rmax/2
      CB = CoefsB(nsum,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TB(AddGtab(1,nu)) = TB(AddGtab(1,nu)) + CB*CFtab(1,nu)
      end do

    end do

    if (calcUV_ten) then
      TBuv = 0d0
      TBuv(1) = CoefsBuv(0,0)

      do nsum=1,rmax/2
        CB = CoefsBuv(nsum,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TBuv(AddGtab(1,nu)) = TBuv(AddGtab(1,nu)) + CB*CFtab(1,nu)
        end do

      end do
    end if

    MomTen(1) = 1
    do r=1,rmax

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      cnt = mu1
      do mu=0,3
        Pmu = mom(mu)
        do a = mu1-BinomTable(r-1,r+2-mu),mu1-1
          MomTen(cnt)=MomTen(a)*Pmu
          cnt = cnt+1
        end do
      end do

      CB = CoefsB(0,r)
      TB(mu1:mu2) = TB(mu1:mu2) + CB*MomTen(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CB = CoefsB(nsum,r)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TB(AddGtab(mu,nu)) = TB(AddGtab(mu,nu)) + CB*MomTen(mu)*CFtab(mu,nu)
          end do
        end do

      end do     

      if (calcUV_ten) then
        CB = CoefsBuv(0,r)
        TBuv(mu1:mu2) = TBuv(mu1:mu2) + CB*MomTen(mu1:mu2)

        do nsum=1,(rmax-r)/2
          CB = CoefsBuv(nsum,r)
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TBuv(AddGtab(mu,nu)) = TBuv(AddGtab(mu,nu)) + CB*MomTen(mu)*CFtab(mu,nu)
            end do
          end do

        end do

      end if

    end do
       
    MomMax = maxval(abs(mom))
    do r=1,rmax
      TBerr(r) = CoefsBerr(r)*MomMax**r
    end do    


  end subroutine CalcTensorB_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorB(TB,TBuv,TBerr,CoefsB,CoefsBuv,CoefsBerr,mom,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorB(TB,TBuv,TBerr,CoefsB,CoefsBuv,CoefsBerr,mom,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: mom(0:3)
    double complex, intent(in) :: CoefsB(0:rmax/2,0:rmax), CoefsBuv(0:rmax/2,0:rmax)
    double precision, intent(in) :: CoefsBerr(0:rmax)
    double complex, intent(out) :: TB(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TBuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TBerr(0:rmax)
    double complex :: TB_aux(RtS(rmax)), TBuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorB_list(TB_aux,TBuv_aux,TBerr,CoefsB,CoefsBuv,CoefsBerr,mom,rmax)

    do mu=1,RtS(rmax)
      TB(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TB_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TBuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TBuv_aux(mu)
      end do
    end if


  end subroutine CalcTensorB





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorC_list(TC,TCuv,TCerr,CoefsC,CoefsCuv,CoefsCerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorC_list(TC,TCuv,TCerr,CoefsC,CoefsCuv,CoefsCerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,2)
    double complex, intent(in) :: CoefsC(0:rmax/2,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsCuv(0:rmax/2,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsCerr(0:rmax)
    double complex, intent(out) :: TC(RtS(rmax)), TCuv(RtS(rmax))
    double precision, intent(out) :: TCerr(0:rmax)
    double complex :: MomTen(5), CC, Pmu
    double precision :: MomMax
    integer :: IndsCoef(2),mu1,mu2,nsum,mu,nu,i,a,cnt,r0   


    TC = 0d0
    TC(1) = CoefsC(0,0,0)
    TCerr = 0d0
    TCerr(0) = CoefsCerr(0)

    do nsum=1,rmax/2
      CC = CoefsC(nsum,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TC(AddGtab(1,nu)) = TC(AddGtab(1,nu)) + CC*CFtab(1,nu)
      end do

    end do

    TCuv = 0d0
    if (calcUV_ten) then
!      TCuv(1) = CoefsC(0,0,0)

      do nsum=1,rmax/2
        CC = CoefsCuv(nsum,0,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TCuv(AddGtab(1,nu)) = TCuv(AddGtab(1,nu)) + CC*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      IndsCoef = (/ 1,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorC(1,MomTen,IndsCoef)

      IndsCoef = (/ 0,1 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorC(1,MomTen,IndsCoef)
            
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TCerr(r0) = CoefsCerr(r0)*MomMax**r0
      end do

    end if



    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorC
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorC(r,MomTenRec,IndsCoef)

      integer, intent(in) :: r
      double complex, intent(in) :: MomTenRec(RtS(r))
      integer, intent(inout) :: IndsCoef(2)
      double complex :: MomTen(RtS(r+1)), CC, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      CC = CoefsC(0,IndsCoef(1),IndsCoef(2))

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TC(mu1:mu2) = TC(mu1:mu2) + CC*MomTenRec(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CC = CoefsC(nsum,IndsCoef(1),IndsCoef(2))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TC(AddGtab(mu,nu)) = TC(AddGtab(mu,nu)) + CC*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

      if (calcUV_ten) then
!        CC = CoefsCuv(0,IndsCoef(1),IndsCoef(2))

!        TCuv(mu1:mu2) = TCuv(mu1:mu2) + CC*MomTenRec(mu1:mu2)

        do nsum=1,(rmax-r)/2
          CC = CoefsCuv(nsum,IndsCoef(1),IndsCoef(2))
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TCuv(AddGtab(mu,nu)) = TCuv(AddGtab(mu,nu)) + CC*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        do i=1,2
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorC(r+1,MomTen,IndsCoef)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorC


  end subroutine CalcTensorC_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorC(TC,TCuv,TCerr,CoefsC,CoefsCuv,CoefsCerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorC(TC,TCuv,TCerr,CoefsC,CoefsCuv,CoefsCerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,2)
    double complex, intent(in) :: CoefsC(0:rmax/2,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsCuv(0:rmax/2,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsCerr(0:rmax)
    double complex, intent(out) :: TC(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TCuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TCerr(0:rmax)
    double complex :: TC_aux(RtS(rmax)), TCuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorC_list(TC_aux,TCuv_aux,TCerr,CoefsC,CoefsCuv,CoefsCerr,MomVec,rmax)

    do mu=1,RtS(rmax)
      TC(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TC_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TCuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TCuv_aux(mu)
      end do
    else
      TCuv = 0d0
    end if
    

  end subroutine CalcTensorC





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorD_list(TD,TDuv,TDerr,CoefsD,CoefsDuv,CoefsDerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorD_list(TD,TDuv,TDerr,CoefsD,CoefsDuv,CoefsDerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,3)
    double complex, intent(in) :: CoefsD(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsDerr(0:rmax) 
    double complex, intent(out) :: TD(RtS(rmax)), TDuv(RtS(rmax))
    double precision, intent(out) :: TDerr(0:rmax)
    double complex :: MomTen(5), CD, Pmu
    double precision :: MomMax
    integer :: IndsCoef(3),mu1,mu2,nsum,mu,nu,i,a,cnt,r0   

    TD = 0d0
    TD(1) = CoefsD(0,0,0,0)
    TDerr = 0d0
    TDerr(0) = CoefsDerr(0)

    do nsum=1,rmax/2
      CD = CoefsD(nsum,0,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TD(AddGtab(1,nu)) = TD(AddGtab(1,nu)) + CD*CFtab(1,nu)
      end do

    end do

    TDuv = 0d0
    if (calcUV_ten) then
!      TDuv(1) = CoefsDuv(0,0,0,0)

!      do nsum=1,rmax/2
      do nsum=2,rmax/2
        CD = CoefsDuv(nsum,0,0,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TDuv(AddGtab(1,nu)) = TDuv(AddGtab(1,nu)) + CD*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      IndsCoef = (/ 1,0,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorD(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,1,0 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorD(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,1 /)
      MomTen(2:5) = MomVec(0:3,3)
      call AddToTensorD(MomTen,IndsCoef,1)
            
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TDerr(r0) = CoefsDerr(r0)*MomMax**r0
      end do

    end if


  
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorD
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorD(MomTenRec,IndsCoef,r)

      integer, intent(in) :: r
      integer, intent(inout) :: IndsCoef(3)
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CD, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      CD = CoefsD(0,IndsCoef(1),IndsCoef(2),IndsCoef(3))

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TD(mu1:mu2) = TD(mu1:mu2) + CD*MomTenRec(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CD = CoefsD(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TD(AddGtab(mu,nu)) = TD(AddGtab(mu,nu)) + CD*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

      if (calcUV_ten) then
!        CD = CoefsDuv(0,IndsCoef(1),IndsCoef(2),IndsCoef(3))

!        TDuv(mu1:mu2) = TDuv(mu1:mu2) + CD*MomTenRec(mu1:mu2)

!        do nsum=1,(rmax-r)/2
        do nsum=2,(rmax-r)/2
          CD = CoefsDuv(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3))
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TDuv(AddGtab(mu,nu)) = TDuv(AddGtab(mu,nu))  &
                                   + CD*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        do i=1,3
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorD(MomTen,IndsCoef,r+1)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorD

  end subroutine CalcTensorD_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorD(TD,TDuv,TDerr,CoefsD,CoefsDuv,CoefsDerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorD(TD,TDuv,TDerr,CoefsD,CoefsDuv,CoefsDerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,3)
    double complex, intent(in) :: CoefsD(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsDerr(0:rmax) 
    double complex, intent(out) :: TD(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TDuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TDerr(0:rmax)
    double complex :: TD_aux(RtS(rmax)), TDuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorD_list(TD_aux,TDuv_aux,TDerr,CoefsD,CoefsDuv,CoefsDerr,MomVec,rmax)

    do mu=1,RtS(rmax)
      TD(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TD_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TDuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TDuv_aux(mu)
      end do
    else
      TDuv = 0d0
    end if

  end subroutine CalcTensorD





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorE_list(TE,TEuv,TEerr,CoefsE,CoefsEuv,CoefsEerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorE_list(TE,TEuv,TEerr,CoefsE,CoefsEuv,CoefsEerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,4)
    double complex, intent(in) :: CoefsE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsEerr(0:rmax) 
    double complex, intent(out) :: TE(RtS(rmax)), TEuv(RtS(rmax))
    double precision, intent(out) :: TEerr(0:rmax)
    double complex :: MomTen(5), CE, Pmu
    double precision :: MomMax
    integer :: IndsCoef(4),mu1,mu2,nsum,mu,nu,i,a,cnt,r0


    TE = 0d0
    TE(1) = CoefsE(0,0,0,0,0)
    TEerr = 0d0
    TEerr(0) = CoefsEerr(0)

    do nsum=1,rmax/2
      CE = CoefsE(nsum,0,0,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TE(AddGtab(1,nu)) = TE(AddGtab(1,nu)) + CE*CFtab(1,nu)
      end do

    end do


    TEuv = 0d0    
    if (calcUV_ten) then
!      TEuv(1) = CoefsEuv(0,0,0,0,0)

!      do nsum=1,rmax/2
      do nsum=3,rmax/2
        CE = CoefsEuv(nsum,0,0,0,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TEuv(AddGtab(1,nu)) = TEuv(AddGtab(1,nu)) + CE*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      IndsCoef = (/ 1,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorE(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,1,0,0 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorE(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,1,0 /)
      MomTen(2:5) = MomVec(0:3,3)
      call AddToTensorE(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,1 /)
      MomTen(2:5) = MomVec(0:3,4)
      call AddToTensorE(MomTen,IndsCoef,1)
            
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TEerr(r0) = CoefsEerr(r0)*MomMax**r0
      end do

    end if



    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorE
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorE(MomTenRec,IndsCoef,r)

      integer, intent(in) :: r
      integer, intent(inout) :: IndsCoef(4)
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CE, Pmu
      double precision :: CEmax
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      CE = CoefsE(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4))
      CEmax = abs(CE)    ! abs(CoefsE(0,0,0,0,0))
      
      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TE(mu1:mu2) = TE(mu1:mu2) + CE*MomTenRec(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CE = CoefsE(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TE(AddGtab(mu,nu)) = TE(AddGtab(mu,nu)) + CE*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

      if (calcUV_ten) then
!        CE = CoefsEuv(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4))

!        TEuv(mu1:mu2) = TEuv(mu1:mu2) + CE*MomTenRec(mu1:mu2)

!        do nsum=1,(rmax-r)/2
        do nsum=1,(rmax-r)/2
          CE = CoefsEuv(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4))
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TEuv(AddGtab(mu,nu)) = TEuv(AddGtab(mu,nu))  &
                                   + CE*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        do i=1,4
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorE(MomTen,IndsCoef,r+1)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorE

  end subroutine CalcTensorE_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorE(TE,TEuv,TEerr,CoefsE,CoefsEuv,CoefsEerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorE(TE,TEuv,TEerr,CoefsE,CoefsEuv,CoefsEerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,4)
    double complex, intent(in) :: CoefsE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsEerr(0:rmax) 
    double complex, intent(out) :: TE(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TEuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TEerr(0:rmax)
    double complex :: TE_aux(RtS(rmax)), TEuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorE_list(TE_aux,TEuv_aux,TEerr,CoefsE,CoefsEuv,CoefsEerr,MomVec,rmax)

    do mu=1,RtS(rmax)
      TE(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TE_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TEuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TEuv_aux(mu)
      end do
    else
      TEuv = 0d0
    end if


  end subroutine CalcTensorE





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorF_list(TF,TFuv,TFerr,CoefsF,CoefsFuv,CoefsFerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorF_list(TF,TFuv,TFerr,CoefsF,CoefsFuv,CoefsFerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5)
    double complex, intent(in) :: CoefsF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsFerr(0:rmax)
    double complex, intent(out) :: TF(RtS(rmax)), TFuv(RtS(rmax))
    double precision, intent(out) :: TFerr(0:rmax)
    double precision :: MomMax
    double complex :: MomTen(5), CF, Pmu 
    integer :: IndsCoef(5),mu1,mu2,nsum,mu,nu,i,a,cnt,r0

    TF = 0d0
    TF(1) = CoefsF(0,0,0,0,0,0)
    TFerr = 0d0
    TFerr(0) = CoefsFerr(0)

    do nsum=1,rmax/2
      CF = CoefsF(nsum,0,0,0,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TF(AddGtab(1,nu)) = TF(AddGtab(1,nu)) + CF*CFtab(1,nu)
      end do

    end do

    TFuv = 0d0
    if (calcUV_ten) then
!      TFuv(1) = CoefsFuv(0,0,0,0,0,0)

!      do nsum=1,rmax/2
      do nsum=4,rmax/2
        CF = CoefsFuv(nsum,0,0,0,0,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TFuv(AddGtab(1,nu)) = TFuv(AddGtab(1,nu)) + CF*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      IndsCoef = (/ 1,0,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorF(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,1,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorF(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,1,0,0 /)
      MomTen(2:5) = MomVec(0:3,3)
      call AddToTensorF(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,1,0 /)
      MomTen(2:5) = MomVec(0:3,4)
      call AddToTensorF(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,0,1 /)
      MomTen(2:5) = MomVec(0:3,5)
      call AddToTensorF(MomTen,IndsCoef,1)
      
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TFerr(r0) = CoefsFerr(r0)*MomMax**r0
      end do

    end if


  
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorF
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorF(MomTenRec,IndsCoef,r)

      integer, intent(in) :: r
      integer, intent(inout) :: IndsCoef(5)
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CF, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      CF = CoefsF(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5))

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TF(mu1:mu2) = TF(mu1:mu2) + CF*MomTenRec(mu1:mu2) 

      do nsum=1,(rmax-r)/2
        CF = CoefsF(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TF(AddGtab(mu,nu)) = TF(AddGtab(mu,nu)) + CF*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

      if (calcUV_ten) then
!        CF = CoefsFuv(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5))

!        TFuv(mu1:mu2) = TFuv(mu1:mu2) + CF*MomTenRec(mu1:mu2)

!        do nsum=1,(rmax-r)/2
        do nsum=4,(rmax-r)/2
          CF = CoefsFuv(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5))
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TFuv(AddGtab(mu,nu)) = TFuv(AddGtab(mu,nu))  &
                                   + CF*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        do i=1,5
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorF(MomTen,IndsCoef,r+1)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorF

  end subroutine CalcTensorF_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorF(TF,TFuv,TFerr,CoefsF,CoefsFuv,CoefsFerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorF(TF,TFuv,TFerr,CoefsF,CoefsFuv,CoefsFerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5)
    double complex, intent(in) :: CoefsF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsFerr(0:rmax)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TFerr(0:rmax)
    double complex :: TF_aux(RtS(rmax)), TFuv_aux(RtS(rmax)) 
    integer :: mu

    call CalcTensorF_list(TF_aux,TFuv_aux,TFerr,CoefsF,CoefsFuv,CoefsFerr,MomVec,rmax)

    do mu=1,RtS(rmax)
      TF(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TF_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TFuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TFuv_aux(mu)
      end do
    else
      TFuv = 0d0
    end if

  end subroutine CalcTensorF





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFuv_list(TFuv,CoefsFuv,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorFuv_list(TFuv,CoefsFuv,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5)
    double complex, intent(in) :: CoefsFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(RtS(rmax))
    double complex :: MomTen(5), CF, Pmu 
    integer :: IndsCoef(5),mu1,mu2,nsum,mu,nu,i,a,cnt


    TFuv = 0d0
    do nsum=4,rmax/2
      CF = CoefsFuv(nsum,0,0,0,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TFuv(AddGtab(1,nu)) = TFuv(AddGtab(1,nu)) + CF*CFtab(1,nu)
      end do

    end do
        
    if (rmax.gt.8) then

      IndsCoef = (/ 1,0,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorFuv(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,1,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorFuv(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,1,0,0 /)
      MomTen(2:5) = MomVec(0:3,3)
      call AddToTensorFuv(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,1,0 /)
      MomTen(2:5) = MomVec(0:3,4)
      call AddToTensorFuv(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,0,1 /)
      MomTen(2:5) = MomVec(0:3,5)
      call AddToTensorFuv(MomTen,IndsCoef,1)

    end if


  
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorFuv
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorFuv(MomTenRec,IndsCoef,r)

      integer, intent(in) :: r
      integer, intent(inout) :: IndsCoef(5)
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CF, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      do nsum=4,(rmax-r)/2
        CF = CoefsFuv(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TFuv(AddGtab(mu,nu)) = TFuv(AddGtab(mu,nu))  &
                                 + CF*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

        
      if (r.lt.rmax-8) then

        do i=1,5
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorFuv(MomTen,IndsCoef,r+1)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorFuv

  end subroutine CalcTensorFuv_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFuv(TFuv,CoefsFuv,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorFuv(TFuv,CoefsFuv,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5)
    double complex, intent(in) :: CoefsFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: TFuv_aux(RtS(rmax)) 
    integer :: mu

    call CalcTensorFuv_list(TFuv_aux,CoefsFuv,MomVec,rmax)

    do mu=1,RtS(rmax)
      TFuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TFuv_aux(mu)
    end do

  end subroutine CalcTensorFuv




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorG_list(TG,TGuv,TGerr,CoefsG,CoefsGuv,CoefsGerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorG_list(TG,TGuv,TGerr,CoefsG,CoefsGuv,CoefsGerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,6)
    double complex, intent(in) :: CoefsG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsGerr(0:rmax)
    double complex, intent(out) :: TG(RtS(rmax)), TGuv(RtS(rmax))
    double precision, intent(out) :: TGerr(0:rmax)
    double complex :: MomTen(5), CG, Pmu
    double precision :: MomMax
    integer :: IndsCoef(6),mu1,mu2,nsum,mu,nu,i,a,cnt,r0

    
    TG = 0d0
    TG(1) = CoefsG(0,0,0,0,0,0,0)
    TGerr = 0d0
    TGerr(0) = CoefsGerr(0)

    do nsum=1,rmax/2
      CG = CoefsG(nsum,0,0,0,0,0,0)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TG(AddGtab(1,nu)) = TG(AddGtab(1,nu)) + CG*CFtab(1,nu)
      end do

    end do

    TGuv = 0d0
    if (calcUV_ten) then
!      TGuv(1) = CoefsGuv(0,0,0,0,0,0,0)

!      do nsum=1,rmax/2
      do nsum=5,rmax/2
        CG = CoefsGuv(nsum,0,0,0,0,0,0)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TGuv(AddGtab(1,nu)) = TGuv(AddGtab(1,nu)) + CG*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      IndsCoef = (/ 1,0,0,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,1)
      call AddToTensorG(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,1,0,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,2)
      call AddToTensorG(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,1,0,0,0 /)
      MomTen(2:5) = MomVec(0:3,3)
      call AddToTensorG(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,1,0,0 /)
      MomTen(2:5) = MomVec(0:3,4)
      call AddToTensorG(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,0,1,0 /)
      MomTen(2:5) = MomVec(0:3,5)
      call AddToTensorG(MomTen,IndsCoef,1)

      IndsCoef = (/ 0,0,0,0,0,1 /)
      MomTen(2:5) = MomVec(0:3,6)
      call AddToTensorG(MomTen,IndsCoef,1)
       
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TGerr(r0) = CoefsGerr(r0)*MomMax**r0
      end do     

    end if


  
    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorG
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorG(MomTenRec,IndsCoef,r)

      integer :: r
      integer, intent(inout) :: IndsCoef(6)
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CG, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt

      CG = CoefsG(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5),IndsCoef(6))

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TG(mu1:mu2) = TG(mu1:mu2) + CG*MomTenRec(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CG = CoefsG(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5),IndsCoef(6))
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TG(AddGtab(mu,nu)) = TG(AddGtab(mu,nu)) + CG*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

      if (calcUV_ten) then
!        CG = CoefsGuv(0,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5),IndsCoef(6))

!        TGuv(mu1:mu2) = TGuv(mu1:mu2) + CG*MomTenRec(mu1:mu2)

!        do nsum=1,(rmax-r)/2
        do nsum=5,(rmax-r)/2
          CG = CoefsGuv(nsum,IndsCoef(1),IndsCoef(2),IndsCoef(3),IndsCoef(4),IndsCoef(5),IndsCoef(6))
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TGuv(AddGtab(mu,nu)) = TGuv(AddGtab(mu,nu))  &
                                   + CG*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        do i=1,6
          IndsCoef(i) = IndsCoef(i)+1

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorG(MomTen,IndsCoef,r+1)

          IndsCoef(i) = IndsCoef(i)-1
        end do

      end if

    end subroutine AddToTensorG

  end subroutine CalcTensorG_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorG(TG,TGuv,TGerr,CoefsG,CoefsGuv,CoefsGerr,MomVec,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorG(TG,TGuv,TGerr,CoefsG,CoefsGuv,CoefsGerr,MomVec,rmax)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,6)
    double complex, intent(in) :: CoefsG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: CoefsGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: CoefsGerr(0:rmax)
    double complex, intent(out) :: TG(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TGuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TGerr(0:rmax)
    double complex :: TG_aux(RtS(rmax)), TGuv_aux(RtS(rmax))
    integer :: mu

    call CalcTensorG_list(TG_aux,TGuv_aux,TGerr,CoefsG,CoefsGuv,CoefsGerr,MomVec,rmax)

    do mu=1,RtS(rmax)
      TG(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TG_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TGuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TGuv_aux(mu)
      end do
    else
      TGuv = 0d0
    end if


  end subroutine CalcTensorG





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTN_list
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorTN_list(TN,TNuv,TNerr,CoefsN,CoefsNuv,CoefsNerr,MomVec,N,rmax)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,N-1)
    double complex, intent(in) :: CoefsN(NCoefs(rmax,N)),CoefsNuv(NCoefs(rmax,N))
    double precision, intent(in) :: CoefsNerr(0:rmax)
    double complex :: CoefsN_aux(BinomTable(rmax,max(N+rmax-2,0)),0:rmax/2,0:rmax)
    double complex :: CoefsNuv_aux(BinomTable(rmax,max(N+rmax-2,0)),0:rmax/2,0:rmax)
    double complex, intent(out) :: TN(RtS(rmax)), TNuv(RtS(rmax)) 
    double precision, intent(out) :: TNerr(0:rmax)
    double precision :: MomMax
    double complex :: MomTen(5), CN, Pmu
    integer :: IndsCoef(5),mu1,mu2,nsum,mu,nu,ind,a,cnt,r0,n0,i,r

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))

        cnt = cnt+1
        CoefsN_aux(i,n0,r) = CoefsN(cnt)
        CoefsNuv_aux(i,n0,r) = CoefsNuv(cnt)

        end do
      end do
    end do
    

    TN = 0d0
    TN(1) = CoefsN_aux(1,0,0)
    TNerr = 0d0
    TNerr(0) = CoefsNerr(0)

    do nsum=1,rmax/2
      CN = CoefsN_aux(1,nsum,2*nsum)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TN(AddGtab(1,nu)) = TN(AddGtab(1,nu)) + CN*CFtab(1,nu)
      end do

    end do

    TNuv = 0d0
    if (calcUV_ten) then
!      TNuv(1) = CoefsNuv_aux(1,0,0)

!      do nsum=1,rmax/2
      do nsum=max(N-2,1),rmax/2
        CN = CoefsNuv_aux(1,nsum,2*nsum)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          TNuv(AddGtab(1,nu)) = TNuv(AddGtab(1,nu)) + CN*CFtab(1,nu)
        end do

      end do
    end if

        
    if (rmax.gt.0) then

      do ind=1,N-1
        MomTen(2:5) = MomVec(0:3,ind)
        call AddToTensorTN(MomTen,ind,1)
      end do
      
      MomMax = maxval(abs(MomVec))
      do r0=1,rmax
        TNerr(r0) = CoefsNerr(r0)*MomMax**r0
      end do
    end if

  

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorTN
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorTN(MomTenRec,ind,r)

      integer, intent(in) :: r,ind
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: CN, Pmu
      double complex, allocatable :: MomTenIt(:)
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt,nind
      double precision :: CNmax 

      CN = CoefsN_aux(ind,0,r)

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      TN(mu1:mu2) = TN(mu1:mu2) + CN*MomTenRec(mu1:mu2)

      do nsum=1,(rmax-r)/2
        CN = CoefsN_aux(ind,nsum,r+2*nsum)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TN(AddGtab(mu,nu)) = TN(AddGtab(mu,nu)) + CN*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do
   
      if (calcUV_ten) then

        if (N.le.2) then
          CN = CoefsNuv_aux(ind,0,r)
          TNuv(mu1:mu2) = TNuv(mu1:mu2) + CN*MomTenRec(mu1:mu2)
        end if

!        do nsum=1,(rmax-r)/2
        do nsum=max(N-2,1),(rmax-r)/2
          CN = CoefsNuv_aux(ind,nsum,r+2*nsum)
      
          do nu=RtS(nsum-1)+1,RtS(nsum)
            do mu=mu1,mu2
              TNuv(AddGtab(mu,nu)) = TNuv(AddGtab(mu,nu))  &
                                   + CN*MomTenRec(mu)*CFtab(mu,nu)
            end do
          end do

        end do
      end if

        
      if (r.lt.rmax) then

        allocate(MomTenIt(RtS(r+1)))
        do i=1,N-1
          nind = IndsPiProd(0,i,ind,N-1)

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTenIt(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorTN(MomTenIt,nind,r+1)

        end do

      end if

    end subroutine AddToTensorTN

  end subroutine CalcTensorTN_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTN
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorTN(TN,TNuv,TNerr,CoefsN,CoefsNuv,CoefsNerr,MomVec,N,rmax)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,N-1)
    double complex, intent(in) :: CoefsN(NCoefs(rmax,N))
    double complex, Intent(in) :: CoefsNuv(NCoefs(rmax,N))
    double precision, intent(in) :: CoefsNerr(0:rmax)
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TNerr(0:rmax)
    double complex :: TN_aux(RtS(rmax)), TNuv_aux(RtS(rmax))
    double complex :: MomTen(5), CN, Pmu
    integer :: IndsCoef(5),mu1,mu2,nsum,mu,nu,ind,a,cnt

    call CalcTensorTN_list(TN_aux,TNuv_aux,TNerr,CoefsN,CoefsNuv,CoefsNerr,MomVec,N,rmax) 

    do mu=1,RtS(rmax)
      TN(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TN_aux(mu)
    end do

    if (calcUV_ten) then
      do mu=1,RtS(rmax)
        TNuv(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu)) = TNuv_aux(mu)
      end do
    else
      TNuv = 0d0
    end if


  end subroutine CalcTensorTN





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNuv_list
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorTNuv_list(TNuv,CoefsNuv,MomVec,N,rmax)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,N-1)
    double complex, intent(in) :: CoefsNuv(BinomTable(rmax-2*N+4,max(rmax-N+2,0)),N-2:rmax/2,2*N-4:rmax)
    double complex, intent(out) :: TNuv(RtS(rmax)) 
    double complex :: MomTen(5), CN, Pmu
    integer :: IndsCoef(5),mu1,mu2,nsum,mu,nu,ind,a,cnt,r,n0,i


    TNuv = 0d0
    TNuv(RtS(2*N-5)+1) = CoefsNuv(1,N-2,2*N-4)

    do nsum=max(N-2,1),rmax/2
      CN = CoefsNuv(1,nsum,2*nsum)
      
      do nu=RtS(nsum-1)+1,RtS(nsum)
        TNuv(AddGtab(1,nu)) = TNuv(AddGtab(1,nu)) + CN*CFtab(1,nu)
      end do
    end do

        
    if (rmax.gt.2*N-4) then

      do ind=1,N-1
        MomTen(2:5) = MomVec(0:3,ind)
        call AddToTensorTNuv(MomTen,ind,1)
      end do

    end if

  

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !  subroutine AddToTensorTNuv
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    recursive subroutine AddToTensorTNuv(MomTenRec,ind,r)

      integer, intent(in) :: r,ind
      double complex, intent(in) :: MomTenRec(RtS(r))
      double complex :: MomTen(RtS(r+1)), CN, Pmu
      integer :: mu1,mu2,nsum,mu,nu,i,a,cnt,nind
      double precision :: CNmax 

      mu1 = RtS(r-1)+1
      mu2 = RtS(r)

      if (N.le.2) then
        CN = CoefsNuv(ind,0,r)
        TNuv(mu1:mu2) = TNuv(mu1:mu2) + CN*MomTenRec(mu1:mu2)
      end if

      do nsum=max(N-2,1),(rmax-r)/2
        CN = CoefsNuv(ind,nsum,r+2*nsum)
      
        do nu=RtS(nsum-1)+1,RtS(nsum)
          do mu=mu1,mu2
            TNuv(AddGtab(mu,nu)) = TNuv(AddGtab(mu,nu))  &
                                 + CN*MomTenRec(mu)*CFtab(mu,nu)
          end do
        end do

      end do

        
      if (r.lt.rmax-2*N+4) then

        do i=1,N-1
          nind = IndsPiProd(0,i,ind,N-1)

          cnt = mu2+1
          do mu=0,3
            Pmu = MomVec(mu,i)
            do a = mu2-BinomTable(r,r+3-mu)+1,mu2
              MomTen(cnt)=MomTenRec(a)*Pmu
              cnt = cnt+1
            end do
          end do

          call AddToTensorTNuv(MomTen,nind,r+1)

        end do

      end if

    end subroutine AddToTensorTNuv

  end subroutine CalcTensorTNuv_list


end module BuildTensors
