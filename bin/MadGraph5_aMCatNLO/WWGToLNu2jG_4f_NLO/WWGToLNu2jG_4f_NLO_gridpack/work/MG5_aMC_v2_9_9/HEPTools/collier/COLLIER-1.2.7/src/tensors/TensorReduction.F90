!!
!!  File TensorReduction.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ****************************
!  *  module TensorReduction  *
!  *      by Lars Hofer       *
!  ****************************
!
!  functions and subroutines:
!  init_tables, CalcTensorNr, CalcTNrPVco, CalcTNrPVco_nd, CalcTtilde, CalcTgtilde,
!  CalcTensorEr, EreduD0, EreduD1, EreduDr, CalcTensorFr, FreduE0, FreduEr
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module TensorReduction

  use collier_coefs
  use BuildTensors

  implicit none


contains





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFr(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax,CFuv)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorFr(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TFerr(0:rmax)
    double complex :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, allocatable :: TFaux(:,:,:,:), CFuvaux(:,:,:,:,:,:)
    double precision, allocatable :: TFerraux(:)
    double complex :: x(41)
    double complex, allocatable :: fct(:)
    integer :: r,n0,n1,n2,n3,n4,n5,i,rank,bino,cnt
    logical :: nocalc,wrica


    if (tenred_cll.gt.6) then
      if (erroutlev_cll.ge.1) then
        write(nerrout_cll,*) 'inconsistent call of CalcTensorFr'
      end if
      stop 
    endif

    if (use_cache_system) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.6) then
          x(1:15) = MomInv
          x(16:21) = masses2
          do i=1,5
            x(22+(i-1)*4:21+i*4) = MomVec(0:3,i)
          end do   
          rank = rmax

          if (rmax.ge.8) then
            allocate(fct(RtS(rmax)+NCoefs(rmax-8,6)+rmax+1))
            call ReadCache(fct,RtS(rmax)+NCoefs(rmax-8,6)+rmax+1,x,41,10+mode_cll,0,6,rank,nocalc,wrica)
          else 
            allocate(fct(RtS(rmax)+rmax+1))
            call ReadCache(fct,RtS(rmax)+rmax+1,x,41,10+mode_cll,0,6,rank,nocalc,wrica)
          end if
    
          if(nocalc)then
            TFuv = 0d0
            cnt = 1
            do r=0,rmax
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
                    TF(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              end do
              do n0=4,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      do n4=0,r-2*n0-n1-n2-n3
                        n5 = r-2*n0-n1-n2-n3-n4
        
                        CFuv(n0,n1,n2,n3,n4,n5) = fct(cnt)
                        cnt = cnt+1
    
                      end do
                    end do
                  end do
                end do
              end do
              TFerr(r) = real(fct(cnt))
              cnt = cnt+1
            end do
            TFuv = 0d0
            if (rmax.ge.8) call CalcTensorFuv(TFuv,CFuv,MomVec,rmax)   
            return
          endif

          
          
          if(rank.eq.rmax) then

            call CalcTensorFrRed(TF,CFuv,TFerr,MomVec,MomInv,masses2,rank)
          
            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r
                  do n1=0,r-n0
                    do n2=0,r-n0-n1
                      n3 = r-n0-n1-n2
 
                      cnt = cnt+1
                      fct(cnt) = TF(n0,n1,n2,n3)

                    end do
                  end do
                end do
                do n0=4,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
        
                          cnt = cnt+1
                          fct(cnt) = CFuv(n0,n1,n2,n3,n4,n5)
    
                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = TFerr(r)
              end do
   
              if (rank.ge.8) then
                call WriteCache(fct,RtS(rank)+NCoefs(rank-8,6)+rank+1,0,6,rank)
              else
                call WriteCache(fct,RtS(rank)+rank+1,0,6,rank)
              end if

            end if

            TFuv = 0d0
            if (rmax.ge.8) call CalcTensorFuv(TFuv,CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax),MomVec,rmax)
            return
            
            
          else
            allocate(TFaux(0:rank,0:rank,0:rank,0:rank))
            allocate(CFuvaux(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank))
            allocate(TFerraux(0:rank))

            call CalcTensorFrRed(TFaux,CFuvaux,TFerraux,MomVec,MomInv,masses2,rank)
          
            if (wrica) then
              cnt = 0
              deallocate(fct)
              if (rank.ge.8) then
                allocate(fct(RtS(rank)+NCoefs(rank-8,6)+rank+1))
              else
                allocate(fct(RtS(rank)+rank+1))
              end if
              do r=0,rank
                do n0=0,r
                  do n1=0,r-n0
                    do n2=0,r-n0-n1
                      n3 = r-n0-n1-n2
 
                      cnt = cnt+1
                      fct(cnt) = TFaux(n0,n1,n2,n3)

                    end do
                  end do
                end do
                do n0=4,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
        
                          cnt = cnt+1
                          fct(cnt) = CFuvaux(n0,n1,n2,n3,n4,n5)
    
                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = TFerraux(r)
              end do
   
              if (rank.ge.8) then
                call WriteCache(fct,RtS(rank)+NCoefs(rank-8,6)+rank+1,0,6,rank)
              else
                call WriteCache(fct,RtS(rank)+rank+1,0,6,rank)
              end if

            end if

            TF = TFaux(0:rmax,0:rmax,0:rmax,0:rmax)
            TFuv = 0d0
            if (rmax.ge.8) call CalcTensorFuv(TFuv,CFuvaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax),MomVec,rmax)
            TFerr = TFerraux(0:rmax)
            return

          end if
          
!       end if          
      end if
    end if

    allocate(CFuvaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax))
    call CalcTensorFrRed(TF,CFuvaux,TFerr,MomVec,MomInv,masses2,rmax)
    TFuv = 0d0
    if (rmax.ge.8) call CalcTensorFuv(TFuv,CFuvaux,MomVec,rmax)


  end subroutine CalcTensorFr




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFrRed(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax,CFuv)
  !
  !  calculate tensor integral TFr for 6-point functions by direct reduction 
  !  from the tensor integrals TEr for 5-point functions.
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorFrRed(TF,CFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: TFerr(0:rmax)
    double complex,intent(out) :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, allocatable :: TE(:,:,:,:,:), TE_0aux(:,:,:,:)
    double complex, allocatable :: TEuv(:,:,:,:,:), TEuv_0aux(:,:,:,:)
    double precision, allocatable :: TEerr(:,:)
    double precision :: p1max
    double complex, allocatable :: CE(:,:,:,:,:), CEuv(:,:,:,:,:), CE0uv(:,:,:,:,:,:)
    double precision, allocatable :: CEerr(:) 
    double complex :: MomVecE(0:3,4), masses2E(0:4), MomInvE(10)
    double complex :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double complex :: q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,ff(5)
!    double complex :: mxinv(0:5,0:5),mxinvs
    double complex :: mx(0:5,0:5),mx0k(5,5),mx0kinv(5,5),mxinvP(5,0:3)
    double complex :: mxinvPs(0:3)
    double complex :: det,newdet
    double complex :: Smod, Faux(5), elimminf2_coli,chdet,P1ten
    integer :: r,n0,n1,n2,n3,n4,n5,np0,np1,np2,np3,k,i,j,kbest,rmaxE,mu,combi,nid(0:4),bin

    double complex :: mxmx0kinv(5,5)
    double precision :: maxf
    integer        :: jmax
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A18,'(',d25.18,' , ',d25.18,' )')"

    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(masses2(0))
    mm12 = elimminf2_coli(masses2(1))
    mm22 = elimminf2_coli(masses2(2))
    mm32 = elimminf2_coli(masses2(3))
    mm42 = elimminf2_coli(masses2(4))
    mm52 = elimminf2_coli(masses2(5))
    q10  = elimminf2_coli(MomInv(1))
    q21  = elimminf2_coli(MomInv(2))
    q32  = elimminf2_coli(MomInv(3))
    q43  = elimminf2_coli(MomInv(4))
    q54  = elimminf2_coli(MomInv(5))
    q50  = elimminf2_coli(MomInv(6))
    q20  = elimminf2_coli(MomInv(7))
    q31  = elimminf2_coli(MomInv(8))
    q42  = elimminf2_coli(MomInv(9))
    q53  = elimminf2_coli(MomInv(10))
    q40  = elimminf2_coli(MomInv(11))
    q51  = elimminf2_coli(MomInv(12))
    q30  = elimminf2_coli(MomInv(13))
    q41  = elimminf2_coli(MomInv(14))
    q52  = elimminf2_coli(MomInv(15))

    ff(1) = q10+mm02-mm12
    ff(2) = q20+mm02-mm22
    ff(3) = q30+mm02-mm32
    ff(4) = q40+mm02-mm42
    ff(5) = q50+mm02-mm52    

 
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(5,0) = q50 - mm52 + mm02
    mx(0,1) = mx(1,0)
    mx(1,1) = 2d0*q10
    mx(2,1) = q10+q20-q21
    mx(3,1) = q10+q30-q31
    mx(4,1) = q10+q40-q41
    mx(5,1) = q10+q50-q51
    mx(0,2) = mx(2,0)
    mx(1,2) = mx(2,1)
    mx(2,2) = 2d0*q20
    mx(3,2) = q20+q30-q32
    mx(4,2) = q20+q40-q42
    mx(5,2) = q20+q50-q52
    mx(0,3) = mx(3,0)
    mx(1,3) = mx(3,1)
    mx(2,3) = mx(3,2)
    mx(3,3) = 2d0*q30
    mx(4,3) = q30+q40-q43
    mx(5,3) = q30+q50-q53
    mx(0,4) = mx(4,0)
    mx(1,4) = mx(4,1)
    mx(2,4) = mx(4,2)
    mx(3,4) = mx(4,3)
    mx(4,4) = 2d0*q40
    mx(5,4) = q40+q50-q54
    mx(0,5) = mx(5,0)
    mx(1,5) = mx(5,1)
    mx(2,5) = mx(5,2)
    mx(3,5) = mx(5,3)
    mx(4,5) = mx(5,4)
    mx(5,5) = 2d0*q50

! changed 21.06.2018
!    call chinv(6,mx,mxinv)

    ! determine X_(0,5)
!    do j=1,5
!      do i=1,5
!        mx0k(i,j) = mx(i,j-1)
!      end do
!    end do
    do j=1,5
      mx0k(:,j) = mx(1:5,j-1)
    end do

    det = chdet(5,mx0k)
    kbest = 5

    do j=5,2,-1
!      do i=1,5
!        mx0k(i,j) = mx(i,j)
!      end do
      mx0k(:,j) = mx(1:5,j)

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
!    do i=1,5
!      mx0k(i,1) = mx(i,1)
!      mx0k(i,kbest) = mx(i,0)
!    end do
    mx0k(:,1) = mx(1:5,1)
    mx0k(:,kbest) = mx(1:5,0)

! changed 21.06.2018
    call chinv(5,mx0k,mx0kinv,det)

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcTensorFrRed',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q54 = ',q54
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q50 = ',q50
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q53 = ',q53
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q51 = ',q51
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: q52 = ',q52
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm42 = ',mm42
        write(nerrout_coli,fmt10) ' CalcTensorFrRed: mm52 = ',mm52
      end if
      TF = 0d0
      return
    end if


!    do i=1,5
!      mx0kinv(kbest,i) = 0d0
!    end do
    mx0kinv(kbest,:) = 0d0
    
!    mxinvs = sum(mxinv(0:5,0))

    ! build rank5 tensors
    rmaxE = max(rmax-1,0)
    allocate(CE0uv(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(CE(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(CEuv(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(CEerr(0:rmaxE))
    allocate(TE(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:5))
    allocate(TE_0aux(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(TEuv(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:5))
    allocate(TEuv_0aux(0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(TEerr(0:5,0:rmaxE))
    
    MomInvE = SubMomInv(6,0,MomInv)
    masses2E = SubMasses(6,0,masses2)
    call E_main_cll(CE(:,:,:,:,:),CE0uv(:,0,:,:,:,:), &
                    MomInvE(1),MomInvE(2),MomInvE(3),MomInvE(4),MomInvE(5),MomInvE(6), &
                    MomInvE(7),MomInvE(8),MomInvE(9),MomInvE(10),masses2E(0),masses2E(1), &
                    masses2E(2),masses2E(3),masses2E(4),rmaxE,Eerr2=CEerr,id_in=1)                           
    call CalcTensorE(TE_0aux(:,:,:,:),TEuv_0aux(:,:,:,:),TEerr(0,0:),  &
                     CE(:,:,:,:,:),CE0uv(:,0,:,:,:,:),CEerr,SubMomVec(6,0,MomVec),rmaxE)

    ! shift of integration momentum in TE\{0}
    TE(:,:,:,:,0) = 0d0

    do r=0,rmaxE

      do np0=0,r
        do np1=0,r-np0
          do np2=0,r-np0-np1
            do np3=0,r-np0-np1-np2

              P1ten = (-MomVec(0,1))**np0*(-MomVec(1,1))**np1*  &
                      (-MomVec(2,1))**np2*(-MomVec(3,1))**np3

              do n0=0,r-np0-np1-np2-np3
                do n1=0,r-np0-np1-np2-np3-n0
                  do n2=0,r-np0-np1-np2-np3-n0-n1
                    n3 = r-np0-np1-np2-np3-n0-n1-n2

                    combi = BinomTable(n0,n0+np0)*BinomTable(n1,n1+np1)*  &
                            BinomTable(n2,n2+np2)*BinomTable(n3,n3+np3)

                    TE(np0+n0,np1+n1,np2+n2,np3+n3,0) = TE(np0+n0,np1+n1,np2+n2,np3+n3,0)  &
                        + combi*TE_0aux(n0,n1,n2,n3)*P1ten

                  end do
                end do
              end do
            
            end do
          end do 
        end do
      end do

    end do
 
    ! shift of integration momentum in TEuv\{0} coefficients
    do n1=1,rmaxE-6
      do n2=0,rmaxE-n1-6
        do n3=0,rmaxE-n1-n2-6
          do n4=0,rmaxE-n1-n2-n3-6
            do n5=0,rmaxE-n1-n2-n3-n4-6
              n0 = (rmaxE-n1-n2-n3-n4-n5)/2
              CE0uv(0:n0,n1,n2,n3,n4,n5) = -CE0uv(0:n0,n1-1,n2,n3,n4,n5)-CE0uv(0:n0,n1-1,n2+1,n3,n4,n5) &
                                           -CE0uv(0:n0,n1-1,n2,n3+1,n4,n5)-CE0uv(0:n0,n1-1,n2,n3,n4+1,n5)  &
                                           -CE0uv(0:n0,n1-1,n2,n3,n4,n5+1)
            end do
          end do
        end do
      end do
    end do


    p1max = maxval(abs(MomVec(0:3,1)))
    do r=1,rmaxE
      do i=1,r
        TEerr(0,r) = max(TEerr(0,i),TEerr(0,r-i)*p1max**i)
      end do
    end do


    do i=1,5
      MomInvE = SubMomInv(6,i,MomInv)
      masses2E = SubMasses(6,i,masses2)
      call E_main_cll(CE,CEuv,MomInvE(1),MomInvE(2),MomInvE(3),MomInvE(4),MomInvE(5),MomInvE(6), &
                              MomInvE(7),MomInvE(8),MomInvE(9),MomInvE(10),masses2E(0),masses2E(1), &
                              masses2E(2),masses2E(3),masses2E(4),rmaxE,Eerr2=CEerr,id_in=2**i)
      call CalcTensorE(TE(:,:,:,:,i),TEuv(:,:,:,:,i),TEerr(i,0:),  &
                       CE,CEuv,CEerr,SubMomVec(6,i,MomVec),rmaxE)
    end do

    ! UV divergent tensor
    ! calculation of UV-divergent parts      
    ! F_(n0,n1,n2,n3) UV-finite for n0<4
    CFuv(0:min(rmax/2,3),:,:,:,:,:) = 0d0
    ! PV reduction (5.10)
    do r=8,rmax
      do n0=3,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                n5 = r-2*n0-n1-n2-n3-n4
        
                CFuv(n0,n1,n2,n3,n4,n5) = (CE0uv(n0-1,n1,n2,n3,n4,n5)  & 
                                       + 2*masses2(0)*CFuv(n0-1,n1,n2,n3,n4,n5)  &
                                       + ff(1)*CFuv(n0-1,n1+1,n2,n3,n4,n5)  & 
                                       + ff(2)*CFuv(n0-1,n1,n2+1,n3,n4,n5)  & 
                                       + ff(3)*CFuv(n0-1,n1,n2,n3+1,n4,n5)  &
                                       + ff(4)*CFuv(n0-1,n1,n2,n3,n4+1,n5)  &
                                       + ff(5)*CFuv(n0-1,n1,n2,n3,n4,n5+1))/ (2*(r-3))
    
              end do
            end do
          end do
        end do
      end do
    end do

    
    TF = 0d0

    ! scalar coefficient
!    based on (D.3), replaced 21.06.2018

!    TF(0,0,0,0) = -mxinv(0,0)*TE(0,0,0,0,0)
!    do i=1,5
!      TF(0,0,0,0) = TF(0,0,0,0) + mxinv(i,0)*(TE(0,0,0,0,i)-TE(0,0,0,0,0))
!    end do
!    TFerr(0) = max( abs(mxinvs)*TEerr(0,0), &
!                    abs(mxinv(1,0))*TEerr(1,0) , &
!                    abs(mxinv(2,0))*TEerr(2,0) , &
!                    abs(mxinv(3,0))*TEerr(3,0) , &
!                    abs(mxinv(4,0))*TEerr(4,0) , &
!                    abs(mxinv(5,0))*TEerr(5,0) )

! New version for  TF(0,,0,0), 21.06.2018

    maxf = abs(mx(1,0))
    jmax = 1
    do j=2,5
      if (abs(mx(j,0)).gt.maxf) then
         jmax = j
         maxf = abs(mx(j,0))
      end if
    end do

    mxmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    TF(0,0,0,0) =  TE(0,0,0,0,jmax) - TE(0,0,0,0,0)
    do j=1,5
      TF(0,0,0,0) =  TF(0,0,0,0) & 
          - mxmx0kinv(jmax,j) * (TE(0,0,0,0,j) - TE(0,0,0,0,0))
    end do
    TF(0,0,0,0) =  TF(0,0,0,0)/mx(jmax,0)

    TFerr(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*TEerr(0,0), &
                   abs(mxmx0kinv(jmax,1))*TEerr(1,0) , &
                   abs(mxmx0kinv(jmax,2))*TEerr(2,0) , &
                   abs(mxmx0kinv(jmax,3))*TEerr(3,0) , &
                   abs(mxmx0kinv(jmax,4))*TEerr(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*TEerr(5,0) , &
                   TEerr(0,0) , TEerr(jmax,0) )/abs(mx(jmax,0))

!    write(*,*) 'CalcTensorTNrRed TN0 pv', TN(0,0,0,0)
!    write(*,*) 'CalcTensorTNrRed TNerr pv', TNerr(0)

    do mu=0,3
      do i=1,5
        mxinvP(i,mu)=0d0
        do j=1,5
          mxinvP(i,mu) = mxinvP(i,mu) + mx0kinv(j,i)*MomVec(mu,j)
        end do
      end do
      mxinvPs(mu) = sum(mxinvP(1:5,mu))
    end do


    
    
    do r=1,rmax    
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (n0.ge.1) then
              do i=1,5
                Smod = TE(n0-1,n1,n2,n3,i) - TE(n0-1,n1,n2,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,0)*Smod
              end do
            else if (n1.ge.1) then
              do i=1,5
                Smod = TE(n0,n1-1,n2,n3,i) - TE(n0,n1-1,n2,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,1)*Smod
              end do
            else if (n2.ge.1) then
              do i=1,5
                Smod = TE(n0,n1,n2-1,n3,i) - TE(n0,n1,n2-1,n3,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,2)*Smod
              end do
            else
              do i=1,5
                Smod = TE(n0,n1,n2,n3-1,i) - TE(n0,n1,n2,n3-1,0)
                TF(n0,n1,n2,n3) = TF(n0,n1,n2,n3) + mxinvP(i,3)*Smod
              end do
            end if

          end do
        end do
      end do

      TFerr(r) = max(maxval(abs(mxinvPs(0:3)))*TEerr(0,r-1), &
                     maxval(abs(mxinvP(1,0:3)))*TEerr(1,r-1), &
                     maxval(abs(mxinvP(2,0:3)))*TEerr(2,r-1), &
                     maxval(abs(mxinvP(3,0:3)))*TEerr(3,r-1))

    end do

  
  end subroutine CalcTensorFrRed





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorFr_list(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTensorFr_list(TF,TFuv,TFerr,MomVec,MomInv,masses2,rmax)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), masses2(0:5), MomInv(15)
    double complex, intent(out) :: TF(RtS(rmax)), TFuv(RtS(rmax))
    double precision, intent(out) :: TFerr(0:rmax)
    double complex :: TF_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: TFuv_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    integer :: mu

    call CalcTensorFr(TF_aux,TFuv_aux,TFerr,MomVec,MomInv,masses2,rmax)

    do mu=1,RtS(rmax)
      TF(mu) = TF_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
    end do

    if (calcUV_cll) then
      do mu=1,RtS(rmax)
        TFuv(mu) = TFuv_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
      end do
    end if

  end subroutine CalcTensorFr_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNr(TN,TNuv,TNerr,MomVec,MomInv,masses2,rmax,id,CNuv)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive subroutine CalcTensorTNr(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax,id,CNuv)
  
    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out), optional :: CNuv(BinomTable(max(rmax-2*N+4,0),max(rmax-N+2,0)),N-2:rmax/2,2*N-4:rmax)
    double complex :: CNuvaux0(BinomTable(max(rmax-2*N+4,0),max(rmax-N+2,0)),N-2:max(rmax/2,N-2),2*N-4:max(rmax,2*N-4))   
    double precision, intent(out) :: TNerr(0:rmax)
    double complex, allocatable :: TNaux(:,:,:,:),TNuvaux(:,:,:,:)
    double precision, allocatable :: TNerraux(:),CNerr(:)
    double complex :: x(BinomTable(2,N)+N+4*(N-1))
    double complex, allocatable :: fct(:),CN(:),CNuvaux1(:),CNuvaux2(:,:,:)
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,cnt,cntC
    logical :: nocalc,wrica

    if ((use_cache_system).and.(N.ge.tencache)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.N) then
          do i=1,N-1
            x((i-1)*4+1:i*4) = MomVec(0:3,i)
          end do 
          bino = BinomTable(2,N)
          x(4*N-3:bino+4*N-4) = MomInv
          x(4*N-3+bino:bino+5*N-4) = masses2  
          rank = rmax

          if (rmax.ge.2*N-4) then
            allocate(fct(RtS(rmax)+NCoefs(rmax-2*N+4,N)+rmax+1))
            call ReadCache(fct,RtS(rmax)+NCoefs(rmax-2*N+4,N)+rmax+1,x,bino+5*N-4,mode_cll+10,id,N,rank,nocalc,wrica)
          else
            allocate(fct(RtS(rmax)+rmax+1))
            call ReadCache(fct,RtS(rmax)+rmax+1,x,bino+5*N-4,mode_cll+10,id,N,rank,nocalc,wrica)
          end if
          ! write(*,*) N,id, nocalc,wrica, masses2(0)
  
          if(nocalc)then
            cnt = 1
            do r=0,rmax
              do n0=0,r
                do n1=0,r-n0
                  do n2=0,r-n0-n1
                    n3 = r-n0-n1-n2
                    TN(n0,n1,n2,n3) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              end do
              if(id.eq.0) then
                do n0=r/2,N-2,-1
                  do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                    CNuvaux0(i,n0,r) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              else
                do n0=r/2,N-2,-1
                  do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                    CNuv(i,n0,r) = fct(cnt)
                    cnt = cnt+1
                  end do
                end do
              end if
              TNerr(r) = real(fct(cnt))
              cnt = cnt+1
            end do
!            write(*,*) 'nocalc', N, rmax, cnt-1, fct(4)
            if(id.eq.0) then
              if(rmax.ge.2*N-4) then
                call CalcTensorTNuv_list(TNuv,CNuvaux0(1:BinomTable(rmax-2*N+4,max(rmax-N+2,0)), &
                                                       N-2:rmax/2,2*N-4:rmax),MomVec,N,rmax)
              else
                TNuv=0d0
              end if
            end if
            return
          endif

          
          if(rank.eq.rmax) then

            if (N.eq.tenred_cll-1) then
              allocate(CN(NCoefs(rank,N)))
              allocate(CNuvaux1(NCoefs(rank,N)))
              allocate(CNerr(0:rank))
              allocate(TNuvaux(0:rank,0:rank,0:rank,0:rank))
              call TN_cll(CN,CNuvaux1,MomInv,masses2,N,rank,TNerr2=CNerr,id_in=id)
              call CalcTensorTN(TN,TNuv,TNerr,CN,CNuvaux1,CNerr,MomVec,N,rank)
              if((id.ne.0).and.(rmax.ge.2*N-4)) then
                do r=2*N-4,rmax
                  cnt = NCoefs(r-1,N)+1                
                  do n0=r/2,N-2,-1
                    do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                      CNuv(i,n0,r) = CNuvaux1(cnt)
                      cnt = cnt+1
                    end do
                  end do
                end do 
              end if
            else
              if(rank.ge.2*N-4) then
                if(id.eq.0) then
                  call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rank,id,CNuvaux0)                 
                  call CalcTensorTNuv_list(TNuv,CNuvaux0,MomVec,N,rank)
                else 
                  call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rank,id,CNuv)               
                end if
              else
                call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rank,id)
                if(id.eq.0) TNuv=0d0
              end if
            end if

            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r
                  do n1=0,r-n0
                    do n2=0,r-n0-n1
                      n3 = r-n0-n1-n2
                      cnt = cnt+1
                      fct(cnt) = TN(n0,n1,n2,n3)
                    end do
                  end do
                end do
                if(r.ge.2*N-4) then
                  if(N.eq.tenred_cll-1) then
                    do i=NCoefs(r-2*N+3,N)+1,NCoefs(r-2*N+4,N)
                      cnt = cnt+1
                      fct(cnt) = CNuvaux1(i)
                    end do
                  else
                    if(id.eq.0) then
                      do n0=r/2,N-2,-1
                        do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                          cnt = cnt+1
                          fct(cnt) = CNuvaux0(i,n0,r)
                        end do
                      end do
                    else
                      do n0=r/2,N-2,-1
                        do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                          cnt = cnt+1
                          fct(cnt) = CNuv(i,n0,r)
                        end do
                      end do
                    end if
                  end if
                end if

                cnt = cnt+1
                fct(cnt) = TNerr(r)

              end do
!              write(*,*) 'wrica', N, rank, cnt, fct(4)
   
              if(rank.ge.2*N-4) then
                call WriteCache(fct,RtS(rank)+NCoefs(rank-2*N+4,N)+rank+1,id,N,rank)
              else
                call WriteCache(fct,RtS(rank)+rank+1,id,N,rank)
              end if

            end if
            return
            
            
          else  
            allocate(TNaux(0:rank,0:rank,0:rank,0:rank))
            allocate(TNerraux(0:rank))

            if (N.eq.tenred_cll-1) then
              allocate(CN(NCoefs(rank,N)))
              allocate(CNuvaux1(NCoefs(rank,N)))
              allocate(CNerr(0:rank))
              allocate(TNuvaux(0:rank,0:rank,0:rank,0:rank))
              call TN_cll(CN,CNuvaux1,MomInv,masses2,N,rank,TNerr2=CNerr,id_in=id)
              call CalcTensorTN(TNaux,TNuvaux,TNerraux,CN,CNuvaux1,CNerr,MomVec,N,rank)
              if(id.eq.0) then
                TNuv = TNuvaux(0:rmax,0:rmax,0:rmax,0:rmax)
              else if(rmax.ge.2*N-4) then
                do r=2*N-4,rmax
                  cnt = NCoefs(r-1,N)+1                
                  do n0=r/2,N-2,-1
                    do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                      CNuv(i,n0,r) = CNuvaux1(cnt)
                      cnt = cnt+1
                    end do
                  end do
                end do 
              end if
            else
              if(rank.ge.2*N-4) then
                allocate(CNuvaux2(BinomTable(rank-2*N+4,max(rank-N+2,0)),N-2:rank/2,2*N-4:rank))
                call CalcTensorTNrRed(TNaux,TNerraux,MomVec,MomInv,masses2,N,rank,id,CNuvaux2)
                if(id.eq.0) then
                  TNuv = 0d0
                  call CalcTensorTNuv_list(TNuv,CNuvaux2(1:BinomTable(rmax-2*N+4,max(rmax-N+2,0)), &
                                                     N-2:rmax/2,2*N-4:rmax),MomVec,N,rmax)
                else
                  CNuv(1:BinomTable(rmax-2*N+4,max(rmax-N+2,0)),N-2:rmax/2,2*N-4:rmax) = &
                  CNuvaux2(1:BinomTable(rmax-2*N+4,max(rmax-N+2,0)),N-2:rmax/2,2*N-4:rmax)
                end if
              else
                call CalcTensorTNrRed(TNaux,TNerraux,MomVec,MomInv,masses2,N,rank,id)
                if(id.eq.0) TNuv=0d0
              end if
            end if

            if (wrica) then
              cnt = 0
              cntC = 0
              deallocate(fct)
              if(rank.ge.2*N-4) then
                allocate(fct(RtS(rank)+NCoefs(rank-2*N+4,N)+rank+1))
              else
                allocate(fct(RtS(rank)+rank+1))
              end if
              do r=0,rank
                do n0=0,r
                  do n1=0,r-n0
                    do n2=0,r-n0-n1
                      n3 = r-n0-n1-n2
                      cnt = cnt+1
                      fct(cnt) = TNaux(n0,n1,n2,n3)
                    end do
                  end do
                end do
                if(r.ge.2*N-4) then
                  if(N.eq.tenred_cll-1) then
                    do i=NCoefs(r-2*N+3,N)+1,NCoefs(r-2*N+4,N)
                      cnt = cnt+1
                      fct(cnt) = CNuvaux1(i)
                    end do
                  else
                    do n0=r/2,N-2,-1
                      do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
                        cnt = cnt+1
                        fct(cnt) = CNuvaux2(i,n0,r)
                      end do
                    end do
                  end if
                end if

                cnt = cnt+1
                fct(cnt) = TNerraux(r)

              end do
   
              if(rank.ge.2*N-4) then
                call WriteCache(fct,RtS(rank)+NCoefs(rank-2*N+4,N)+rank+1,id,N,rank)
              else
                call WriteCache(fct,RtS(rank)+rank+1,id,N,rank)
              end if

            end if

            TN = TNaux(0:rmax,0:rmax,0:rmax,0:rmax)
            TNerr = TNerraux(0:rmax)
            return

          end if
            
!        end if
      end if
    end if  
    
    if (N.le.tenred_cll-1) then
      allocate(CN(NCoefs(rmax,N)))
      allocate(CNuvaux1(NCoefs(rmax,N)))
      allocate(CNerr(0:rmax))
      call TN_cll(CN,CNuvaux1,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=id)
      call CalcTensorTN(TN,TNuv,TNerr,CN,CNuvaux1,CNerr,MomVec,N,rmax)
      if((id.ne.0).and.(rmax.ge.2*N-4)) then
        do r=2*N-4,rmax
          cnt = NCoefs(r-1,N)+1                
          do n0=r/2,N-2,-1
            do i=1,BinomTable(r-2*n0,max(N+r-2*n0-2,0))
              CNuv(i,n0,r) = CNuvaux1(cnt)
              cnt = cnt+1
            end do
          end do
        end do 
      end if
    else
      if(rmax.ge.2*N-4) then
        if(id.eq.0) then
          call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rmax,id,CNuvaux0)                 
          call CalcTensorTNuv_list(TNuv,CNuvaux0,MomVec,N,rmax)
        else 
          call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rmax,id,CNuv)               
        end if
      else
        call CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rmax,id)
        if(id.eq.0) TNuv=0d0
      end if   
    end if


  end subroutine CalcTensorTNr





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,rmax,id,CNuv)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive subroutine CalcTensorTNrRed(TN,TNerr,MomVec,MomInv,masses2,N,rmax,id,CNuv)
  
    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out), optional :: CNuv(BinomTable(max(rmax-2*N+4,0),max(rmax-N+2,0)),N-2:rmax/2,2*N-4:rmax)
    double precision, intent(out) :: TNerr(0:rmax)
    double complex, allocatable :: TNm1(:,:,:,:,:),TNm1_0aux(:,:,:,:)
    double complex, allocatable :: TNm1uv_0aux(:,:,:,:)
    double complex, allocatable :: CNm1UV_0(:,:,:),CNm1UV_0aux(:,:,:),CNm1UV_i(:,:,:)
    double precision, allocatable :: TNm1err(:,:)
    double precision :: p1max
    double complex :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double complex :: q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52
!    double complex :: mxinv(0:5,0:5),mxinvs
    double complex :: mx(0:5,0:5),mx0k(5,5),mx0kinv(5,5)
    double complex :: det,newdet,ff(N-1)
    double complex :: Smod, elimminf2_coli,chdet,P1ten,mxinvP(5,0:3)
    double complex :: mxinvPs(0:3)
    integer :: r,n0,n1,n2,n3,n4,n5,np0,np1,np2,np3,k,i,j,kbest,rmax_m1,combi,mu,cnt
    integer :: bin,nid(0:5),r0,rBCD,mia,bino_0,bino_i,cind,mia1,mia2,mia3

    double complex :: mxmx0kinv(5,5)
    double precision :: maxf
    integer        :: jmax
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A18,'(',d25.18,' , ',d25.18,' )')"


    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(masses2(0))
    mm12 = elimminf2_coli(masses2(1))
    mm22 = elimminf2_coli(masses2(2))
    mm32 = elimminf2_coli(masses2(3))
    mm42 = elimminf2_coli(masses2(4))
    mm52 = elimminf2_coli(masses2(5))
    q10  = elimminf2_coli(MomInv(1))
    q21  = elimminf2_coli(MomInv(2))
    q32  = elimminf2_coli(MomInv(3))
    q43  = elimminf2_coli(MomInv(4))
    q54  = elimminf2_coli(MomInv(5))
    if (N.le.9) then
      q50  = elimminf2_coli(MomInv((N-6)*N+6))
    else
      q50 = elimminf2_coli(MomInv(4*N+1))
    end if
    q20  = elimminf2_coli(MomInv(N+1))
    q31  = elimminf2_coli(MomInv(N+2))
    q42  = elimminf2_coli(MomInv(N+3))
    q53  = elimminf2_coli(MomInv(N+4))
    if (N.le.7) then
      q40 = elimminf2_coli(MomInv((N-5)*N+5))
      q51 = elimminf2_coli(MomInv((N-5)*N+6))
    else
      q40 = elimminf2_coli(MomInv(3*N+1))
      q51 = elimminf2_coli(MomInv(3*N+2))
    end if
    q30  = elimminf2_coli(MomInv(2*N+1))
    q41  = elimminf2_coli(MomInv(2*N+2))
    q52  = elimminf2_coli(MomInv(2*N+3))
    
    cnt = 1
    do i=1,N-1
      ff(i) = elimminf2_coli(MomInv(cnt)) + mm02  &
            - elimminf2_coli(masses2(i))
      cnt = cnt+N-i
    end do
 
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(5,0) = q50 - mm52 + mm02
    mx(0,1) = mx(1,0)
    mx(1,1) = 2d0*q10
    mx(2,1) = q10+q20-q21
    mx(3,1) = q10+q30-q31
    mx(4,1) = q10+q40-q41
    mx(5,1) = q10+q50-q51
    mx(0,2) = mx(2,0)
    mx(1,2) = mx(2,1)
    mx(2,2) = 2d0*q20
    mx(3,2) = q20+q30-q32
    mx(4,2) = q20+q40-q42
    mx(5,2) = q20+q50-q52
    mx(0,3) = mx(3,0)
    mx(1,3) = mx(3,1)
    mx(2,3) = mx(3,2)
    mx(3,3) = 2d0*q30
    mx(4,3) = q30+q40-q43
    mx(5,3) = q30+q50-q53
    mx(0,4) = mx(4,0)
    mx(1,4) = mx(4,1)
    mx(2,4) = mx(4,2)
    mx(3,4) = mx(4,3)
    mx(4,4) = 2d0*q40
    mx(5,4) = q40+q50-q54
    mx(0,5) = mx(5,0)
    mx(1,5) = mx(5,1)
    mx(2,5) = mx(5,2)
    mx(3,5) = mx(5,3)
    mx(4,5) = mx(5,4)
    mx(5,5) = 2d0*q50

! changed 21.06.2018
!    call chinv(6,mx,mxinv)

    ! determine X_(0,5)
!    do j=1,5
!      do i=1,5
!        mx0k(i,j) = mx(i,j-1)
!      end do
!    end do
    do j=1,5
      mx0k(:,j) = mx(1:5,j-1)
    end do

    det = chdet(5,mx0k)
    kbest = 5

    do j=5,2,-1
!      do i=1,5
!        mx0k(i,j) = mx(i,j)
!      end do
      mx0k(:,j) = mx(1:5,j)

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
!    do i=1,5
!      mx0k(i,1) = mx(i,1)
!      mx0k(i,kbest) = mx(i,0)
!    end do
    mx0k(:,1) = mx(1:5,1)
    mx0k(:,kbest) = mx(1:5,0)

! changed 21.06.2018
    call chinv(5,mx0k,mx0kinv,det)

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcTensorTNrRed',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q54 = ',q54
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q50 = ',q50
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q53 = ',q53
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q51 = ',q51
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: q52 = ',q52
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm42 = ',mm42
        write(nerrout_coli,fmt10) ' CalcTensorTNrRed: mm52 = ',mm52
      end if
      TN = 0d0
      return
    end if

!    do i=1,5
!      mx0kinv(kbest,i) = 0d0
!    end do
    mx0kinv(kbest,:) = 0d0
    
!    mxinvs = sum(mxinv(0:5,0))

    ! determine binaries for F-coefficients
    k=0
    bin = 1
    do while (k.le.5)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do


    rmax_m1 = max(rmax-1,0)
    bino_0 = BinomTable(rmax_m1,N+rmax_m1-2)
    bino_i = BinomTable(rmax_m1,N+rmax_m1-3)
    allocate(TNm1(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1,0:5))
    allocate(TNm1_0aux(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1))
    allocate(TNm1uv_0aux(0:rmax_m1,0:rmax_m1,0:rmax_m1,0:rmax_m1))
    allocate(TNm1err(0:5,0:rmax_m1))
  
    if(rmax_m1.ge.2*N-6) then
      allocate(CNm1UV_0(bino_0,N-3:rmax/2,2*N-6:rmax))
      allocate(CNm1UV_0aux(bino_i,N-3:rmax_m1/2,2*N-6:rmax_m1))
      allocate(CNm1UV_i(bino_i,0:rmax_m1/2,0:rmax_m1))
      call CalcTensorTNr(TNm1_0aux(:,:,:,:),TNm1uv_0aux,TNm1err(0,0:rmax_m1),SubMomVec(N,0,MomVec),SubMomInv(N,0,MomInv), &
                         SubMasses(N,0,masses2),N-1,rmax_m1,nid(0),CNm1UV_0aux)
      do i=1,5    
        call CalcTensorTNr(TNm1(:,:,:,:,i),TNm1uv_0aux,TNm1err(i,0:rmax_m1),SubMomVec(N,i,MomVec),SubMomInv(N,i,MomInv), &
                           SubMasses(N,i,masses2),N-1,rmax_m1,nid(i),CNm1UV_i)
      end do                   
    else
      call CalcTensorTNr(TNm1_0aux(:,:,:,:),TNm1uv_0aux,TNm1err(0,0:rmax_m1),SubMomVec(N,0,MomVec),  &
                         SubMomInv(N,0,MomInv),SubMasses(N,0,masses2),N-1,rmax_m1,nid(0))
      do i=1,5    
        call CalcTensorTNr(TNm1(:,:,:,:,i),TNm1uv_0aux,TNm1err(i,0:rmax_m1),SubMomVec(N,i,MomVec),  &
                           SubMomInv(N,i,MomInv),SubMasses(N,i,masses2),N-1,rmax_m1,nid(i))
      end do
    end if
      
    ! shift of integration momentum in TE\{0}
    TNm1(:,:,:,:,0) = 0d0

    do r=0,rmax_m1

      do np0=0,r
        do np1=0,r-np0
          do np2=0,r-np0-np1
            do np3=0,r-np0-np1-np2

              P1ten = (-MomVec(0,1))**np0*(-MomVec(1,1))**np1*  &
                      (-MomVec(2,1))**np2*(-MomVec(3,1))**np3

              do n0=0,r-np0-np1-np2-np3
                do n1=0,r-np0-np1-np2-np3-n0
                  do n2=0,r-np0-np1-np2-np3-n0-n1
                    n3 = r-np0-np1-np2-np3-n0-n1-n2

                    combi = BinomTable(n0,n0+np0)*BinomTable(n1,n1+np1)*  &
                            BinomTable(n2,n2+np2)*BinomTable(n3,n3+np3)

                    TNm1(np0+n0,np1+n1,np2+n2,np3+n3,0) = TNm1(np0+n0,np1+n1,np2+n2,np3+n3,0)  &
                        + combi*TNm1_0aux(n0,n1,n2,n3)*P1ten

                  end do
                end do
              end do

            end do
          end do 
        end do
      end do

    end do

    p1max = maxval(abs(MomVec(0:3,1)))
    do r=1,rmax_m1
      do i=1,r
        TNm1err(0,r) = max(TNm1err(0,i),TNm1err(0,r-i)*p1max**i)
      end do
    end do    

    TN = 0d0

    ! scalar coefficient
!    based on (D.3), replaced 21.06.2018

!    TN(0,0,0,0) = -mxinv(0,0)*TNm1(0,0,0,0,0)
!    do i=1,5
!      TN(0,0,0,0) = TN(0,0,0,0) + mxinv(i,0)*(TNm1(0,0,0,0,i)-TNm1(0,0,0,0,0))
!    end do
!    TNerr(0) = max( abs(mxinvs)*TNm1err(0,0), &
!                    abs(mxinv(1,0))*TNm1err(1,0) , &
!                    abs(mxinv(2,0))*TNm1err(2,0) , &
!                    abs(mxinv(3,0))*TNm1err(3,0) , &
!                    abs(mxinv(4,0))*TNm1err(4,0) , &
!                    abs(mxinv(5,0))*TNm1err(5,0) )
!
!    write(*,*) 'CalcTensorTNrRed TN0 old', TN(0,0,0,0)
!    write(*,*) 'CalcTensorTNrRed TNerr old', TNerr(0)

! New version for  TN(0,0,0,0), 21.06.2018

    maxf = abs(mx(1,0))
    jmax = 1
    do j=2,5
      if (abs(mx(j,0)).gt.maxf) then
         jmax = j
         maxf = abs(mx(j,0))
      end if
    end do

    mxmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    TN(0,0,0,0) =  TNm1(0,0,0,0,jmax) - TNm1(0,0,0,0,0)
    do j=1,5
      TN(0,0,0,0) =  TN(0,0,0,0) & 
          - mxmx0kinv(jmax,j) * (TNm1(0,0,0,0,j) - TNm1(0,0,0,0,0))
    end do
    TN(0,0,0,0) =  TN(0,0,0,0)/mx(jmax,0)

    TNerr(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*TNm1err(0,0), &
                   abs(mxmx0kinv(jmax,1))*TNm1err(1,0) , &
                   abs(mxmx0kinv(jmax,2))*TNm1err(2,0) , &
                   abs(mxmx0kinv(jmax,3))*TNm1err(3,0) , &
                   abs(mxmx0kinv(jmax,4))*TNm1err(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*TNm1err(5,0) , &
                   TNm1err(0,0) , TNm1err(jmax,0) )/abs(mx(jmax,0))

!    write(*,*) 'CalcTensorTNrRed TN0 pv', TN(0,0,0,0)
!    write(*,*) 'CalcTensorTNrRed TNerr pv', TNerr(0)

    do mu=0,3
      do i=1,5
        mxinvP(i,mu)=0d0
        do j=1,5
          mxinvP(i,mu) = mxinvP(i,mu) + mx0kinv(j,i)*MomVec(mu,j)
        end do
      end do
      mxinvPs(mu) = sum(mxinvP(1:5,mu))
    end do

    ! calculation of UV-divergent parts      
    ! TN UV-finite for n0<N-2
    ! TNuv(:,0:min(rmax/2,N-3),:) = 0d0
    
    ! PV reduction (5.10)
    if(rmax.ge.2*N-4) then
      CNm1UV_0 = 0d0      
      do n0=N-3,rmax_m1/2
        CNm1UV_0(1,n0,2*n0) = CNm1UV_0aux(1,n0,2*n0)
        do r=1,rmax_m1-2*n0
          mia1 = BinomTable(r-1,N+r-3)+1
          mia2 = BinomTable(r,N+r-3)
          mia3 = BinomTable(r,N+r-2) 
          CNm1UV_0(mia1:mia3,n0,r+2*n0) = CNm1UV_0aux(1:mia2,n0,r+2*n0)
        end do
      end do
      do r=2*N-6,rmax_m1
        do n0=N-3,(r-1)/2
          do i=BinomTable(r-2*n0-1,N+r-2*n0-3),1,-1
            CNm1UV_0(i,n0,r) = -CNm1UV_0(i,n0,r-1)
            do j=2,N-1
              CNm1UV_0(i,n0,r) = CNm1UV_0(i,n0,r)  &
                               - CNm1UV_0(AddToCInd(j,i,r-2*n0-1,N-1),n0,r) 
            end do
          end do
        end do
      end do
      CNuv = 0d0
      do r=2*N-4,rmax
        do n0=N-2,r/2
          do cind=1,BinomTable(r-2*n0,N+r-2*n0-2)
        
            CNuv(cind,n0,r) = CNm1UV_0(cind,n0-1,r-2)
            if(n0.ge.N-1) then
              CNuv(cind,n0,r) = CNuv(cind,n0,r) + 2*mm02*CNuv(cind,n0-1,r-2)
              do i=1,N-1
                CNuv(cind,n0,r) = CNuv(cind,n0,r)  &
                                + ff(i)*CNuv(AddToCInd(i,cind,r-2*n0,N-1),n0-1,r-1)
              end do
            end if
            CNuv(cind,n0,r) = CNuv(cind,n0,r)/(2*(r+3-N))
    
          end do
        end do
      end do
    end if

    do r=1,rmax    
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (n0.ge.1) then
              do i=1,5
                Smod = TNm1(n0-1,n1,n2,n3,i) - TNm1(n0-1,n1,n2,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,0)*Smod
              end do
            else if (n1.ge.1) then
              do i=1,5
                Smod = TNm1(n0,n1-1,n2,n3,i) - TNm1(n0,n1-1,n2,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,1)*Smod
              end do
            else if (n2.ge.1) then              
              do i=1,5
                Smod = TNm1(n0,n1,n2-1,n3,i) - TNm1(n0,n1,n2-1,n3,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,2)*Smod
              end do           
            else
              do i=1,5
                Smod = TNm1(n0,n1,n2,n3-1,i) - TNm1(n0,n1,n2,n3-1,0)
                TN(n0,n1,n2,n3) = TN(n0,n1,n2,n3) + mxinvP(i,3)*Smod
              end do
            end if

          end do
        end do
      end do

      TNerr(r) = max(maxval(abs(mxinvPs(0:3)))*TNm1err(0,r-1), &
                     maxval(abs(mxinvP(1,0:3)))*TNm1err(1,r-1), &
                     maxval(abs(mxinvP(2,0:3)))*TNm1err(2,r-1), &
                     maxval(abs(mxinvP(3,0:3)))*TNm1err(3,r-1))

    end do

  
  end subroutine CalcTensorTNrRed





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTensorTNr_list(TN,TNuv,TNerr,MomVec,MomInv,masses2,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcTensorTNr_list(TN,TNuv,TNerr,MomVec,MomInv,masses2,N,rmax)
  
    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,N-1), masses2(0:N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex, intent(out) :: TN(RtS(rmax)),TNuv(RtS(rmax))
    double precision, intent(out) :: TNerr(0:rmax)
    double complex :: TN_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: TNuv_aux(0:rmax,0:rmax,0:rmax,0:rmax)
    integer :: mu

    call CalcTensorTNr(TN_aux,TNuv_aux,TNerr,MomVec,MomInv,masses2,N,rmax,0)

    do mu=1,RtS(rmax)
      TN(mu) = TN_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
    end do

    if (calcUV_cll) then
      do mu=1,RtS(rmax)
        TNuv(mu) = TNuv_aux(LorIndTab(0,mu),LorIndTab(1,mu),LorIndTab(2,mu),LorIndTab(3,mu))
      end do
    end if

  end subroutine CalcTensorTNr_list





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensors(TN,MomInv,N,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTensors(TN,MomVec,MomInv,masses2,N,rmax,writeout,acc)

    integer, intent(in) :: N,rmax,writeout
    double precision, intent(in) :: acc
    double complex, intent(in) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: MomVec(0:3,N-1), MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex :: CoefsN(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex :: CoefsNuv(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex :: check1, check2, fac, Gram(N-1,N-1)
    integer, allocatable :: pinds(:),loinds(:)
    integer :: struc1(0:N-1), struc2(0:N-1)
    integer :: r,n0,i,j,mu,mu0,sgn,tinds(0:3),cnt,nn0,ii
    double precision :: TNerr(0:rmax),TNerr2(0:rmax)

    call CalcTN(CoefsN,CoefsNuv,MomInv,masses2,N,rmax,0,TNerr,TNerr2)


    ! determine Gram matrix
    cnt = 1
    do i=1,N-1
      Gram(i,i) = MomInv(cnt)
      cnt = cnt+N-i
    end do

    cnt = 2
    do i=1,N-1
      do j=i+1,N-1
        Gram(i,j) = (Gram(i,i)+Gram(j,j)-MomInv(cnt))/2d0
        Gram(j,i) = Gram(i,j)
        cnt = cnt+1
      end do
      cnt = cnt+1
    end do


    do r=1,rmax
        
      if (allocated(loinds)) then
        deallocate(loinds)
      end if
      allocate(loinds(r))

      do n0=r/2,0,-1
        
        if (allocated(pinds)) then
          deallocate(pinds)
        end if
        allocate(pinds(r-2*n0))

        do i=1,BinomTable(r-2*n0,r-2*n0+N-2)
          if (r.gt.2*n0) then
            pinds = IndCombisEq(1:r-2*n0,i,r-2*n0,N-1)
          end if

          struc1(1:N-1) = CalcCIndArr(N-1,r-2*n0,i)
          ! contract TN(r) with g(n0)*MomVec(pinds)
          ! --> check1
          check1 = 0d0

          loinds = 0
          do mu=0,4**r-1

            mu0 = mu
            tinds = 0
            do j=1,r
              loinds(j) = mod(mu0,4)
              tinds(loinds(j)) = tinds(loinds(j))+1
              mu0 = mu0/4
            end do

            ! sign of contraction from Minkowski metric
            sgn=(-1)**(r-tinds(0))
            do j=1,n0
              select case (loinds(2*j)-loinds(2*j-1))
                case (0)
                  if (loinds(2*j).ne.0) then
                    sgn = -sgn
                  end if
                case default
                  sgn = 0
              end select
            end do

            if (sgn.ne.0) then

              fac = 1d0
              do j=1,r-2*n0
                fac = fac*MomVec(loinds(2*n0+j),pinds(j))
              end do

              check1 = check1 + sgn*fac*TN(tinds(0),tinds(1),tinds(2),tinds(3))

            end if      
          
          end do

          
          ! calculate contraction directly from coefficients
          ! --> check2
          check2 = 0d0

          struc1(0) = n0
          struc1(1:N-1) = CalcCIndArr(N-1,r-2*n0,i)

          do nn0=r/2,0,-1
            do ii=1,BinomTable(r-2*nn0,r-2*nn0+N-2)

              struc2(0) = nn0
              struc2(1:N-1) = CalcCIndArr(N-1,r-2*nn0,ii)

              check2 = check2 + CoefsN(ii,nn0,r)*  &
                       ContractLoStruc(N-1,struc1,struc2,Gram)

            end do
          end do

          select case (writeout)

            case(1)
              if (abs(check1-check2)/abs(check2).gt.acc) then
                write(ninfout_cll,*) struc1
                write(ninfout_cll,*) 'check1:', check1
                write(ninfout_cll,*) 'check2:', check2
              end if

            case(2)
                write(ninfout_cll,*) struc1
                write(ninfout_cll,*) 'check1:', check1
                write(ninfout_cll,*) 'check2:', check2

          end select

        end do
      end do
    end do

  end subroutine CheckTensors





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  function ContractLoStruc(Nm1,struc1,struc2,Gram)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  recursive function ContractLoStruc(Nm1,struc1,struc2,Gram) result(res)
!
!    integer, intent(in) :: Nm1
!    integer, intent(in) :: struc1(0:Nm1), Struc2(0:Nm1)
!    integer :: struc1aux(0:Nm1), struc2aux(0:Nm1), struc2aux2(0:Nm1)
!    double complex, intent(in) :: Gram(Nm1,Nm1)
!    double complex :: res
!    integer :: i,j,con,sum1,sum2,fac
!
!    sum1 = 2*struc1(0)
!    sum2 = 2*struc2(0)
!    do i=1,Nm1
!      sum1 = sum1 + struc1(i)
!      sum2 = sum2 + struc2(i)
!    end do
!    
!    if (sum1.ne.sum2) then
!      write(*,*) 'error in ContractLoStruc:'
!      write(*,*) 'struc1 and struc2 must be of equal rank!'
!      stop
!    end if
!
!    if (sum1.eq.0) then
!      res = 1d0
!      return
!    end if
!    
!
!    do i=0,Nm1
!      if (struc1(i).ge.1) then
!        con = i
!      end if
!    end do
!
!    res = 0d0
!    struc1aux = struc1
!    struc1aux(con) = struc1aux(con)-1
!
!    if (con.ge.1) then
!
!      ! contract p_con from T1 with g from T2
!      if (struc2(0).ge.1) then
!        struc2aux = struc2
!        struc2aux(0) = struc2aux(0)-1
!        struc2aux(con) = struc2aux(con)+1
!        ! go on contracting recursively
!        ! (factor struc2aux(con) because of symmetrization wrt. g and pi)
!        res = res + struc2aux(con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!      end if
!
!      ! contract p_con from T1 with all the pi from T2
!      do i=1,Nm1
!        if (struc2(i).ge.1) then
!          struc2aux = struc2
!          struc2aux(i) = struc2aux(i)-1
!          ! go on contracting recursively
!          res = res + Gram(i,con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!        end if
!      end do
!
!
!    else
!        
!      ! contract g from T1 with g from T2
!      if (struc2(0).ge.1) then
!        struc2aux = struc2
!        struc2aux(0) = struc2aux(0)-1
!        ! full contraction g^{mu,nu}.g_{mu,nu}
!        ! tensor in D=4 dimensions g*g=4
!        fac = 4
!        do i=0,Nm1
!          ! partial contration g^{mu,nu}.(pi_mu g_{nu,rho})
!          ! or g^{mu,nu}.(g_{mu,rho}g_{nu,sigma})
!          ! factor 2 for mu <--> nu
!          fac = fac + 2*struc2aux(i)
!        end do
!        ! go on contracting recursively
!        res = res + fac*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
!      end if
!
!      ! contract g^{mu,nu} from T1 with pi,pj from T2
!      do i=1,Nm1
!        if (struc2(i).ge.1) then
!          struc2aux = struc2
!          struc2aux(i) = struc2aux(i)-1
!          do j=1,Nm1
!            if (struc2aux(j).ge.1) then
!              struc2aux2 = struc2aux
!              struc2aux2(j) = struc2aux2(j)-1
!              ! go on contracting recursively
!              res = res + Gram(i,j)*ContractLoStruc(Nm1,struc1aux,struc2aux2,Gram)
!            end if
!          end do
!        end if
!      end do
!
!    end if
!
!
!  end function ContractLoStruc





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CompareTensors
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CompareTensors(T1,T2,rmax,writeout,acc)

    integer, intent(in) :: rmax, writeout
    double complex, intent(in) :: T1(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: T2(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: acc
    integer :: r,n0,n1,n2,n3

    do r=0,rmax
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3 = r-n0-n1-n2

            if (writeout.eq.1) then
              if (abs(T1(n0,n1,n2,n3)-T2(n0,n1,n2,n3)).gt.  &
                  acc*abs(T1(n0,n1,n2,n3)+T2(n0,n1,n2,n3))) then
                write(ninfout_cll,*) n0,n1,n2,n3
                write(ninfout_cll,*) T1(n0,n1,n2,n3), T2(n0,n1,n2,n3)
              end if
            end if

          end do
        end do
      end do
    end do

  end subroutine CompareTensors

     
end module TensorReduction

