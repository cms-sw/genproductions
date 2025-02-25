!!
!!  File InitTensors.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *************************
!  *   module InitTensors  *
!  *    by Lars Hofer      *
!  *************************
!
!  global variables:
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module InitTensors

  use Combinatorics

  implicit none

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  global variable CFtab
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  integer, dimension(:,:), allocatable :: CFtab, LorIndTab, AddIndTab, AddGtab
  integer, dimension(:,:,:,:), allocatable :: IndsPiProd
  integer, dimension(:), allocatable :: RtS
  real :: stin, stout, time1, time2, time3
  logical :: calcUV_ten


contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine init_tables2(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_tables2(Nm1,rmax)

    integer, intent(in) :: Nm1,rmax

    call SetRtS(rmax)
    call SetLorIndTab(rmax)
    call SetAddIndTab(rmax)
    call SetAddGtab(rmax)
    call SetCFtab(rmax)
    call SetIndsPiProd(Nm1,rmax)

  end subroutine init_tables2





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRtS(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetRtS(rmax)

    integer, intent(in) :: rmax
    integer :: r

    if (allocated(RtS)) deallocate(RtS)
    allocate(RtS(-1:rmax+1))

    RtS(-1) = 0
    do r=0,rmax+1
      RtS(r) = RtS(r-1) + BinomTable(r,r+3)
    end do

  end subroutine SetRtS





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetLorIndTab(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetLorIndTab(rmax)

    integer, intent(in) :: rmax
    integer :: r,mu0,mu1,mu2,mu3,cnt

    if (allocated(LorIndTab)) deallocate(LorIndTab)
    allocate(LorIndTab(0:3,RtS(rmax)))

    cnt = 1
    do r=0,rmax
      do mu0=r,0,-1
        do mu1=r-mu0,0,-1
          do mu2=r-mu0-mu1,0,-1
            mu3=r-mu0-mu1-mu2

            LorIndTab(0,cnt) = mu0
            LorIndTab(1,cnt) = mu1
            LorIndTab(2,cnt) = mu2
            LorIndTab(3,cnt) = mu3

            cnt = cnt+1

          end do
        end do
      end do
    end do

  end subroutine SetLorIndTab





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAddIndTab(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetAddIndTab(rmax)

    integer, intent(in) :: rmax
    integer :: r,mu,nu,IndMu(0:3),i

    if (allocated(AddIndTab)) deallocate(AddIndTab)
    allocate(AddIndTab(RtS(rmax-1),0:3))

    AddIndTab(1,:) = (/ 2,3,4,5 /)
    do r=1,rmax-1
      do mu=RtS(r-1)+1,RtS(r)
        IndMu = LorIndTab(:,mu)
        do i=0,3 
          IndMu(i) = IndMu(i)+1
          do nu=RtS(r)+1,RtS(r+1)
            if (all(LorIndTab(:,nu).eq.IndMu)) then
              AddIndTab(mu,i) = nu
            end if
          end do
          IndMu(i) = IndMu(i)-1
        end do
      end do
    end do

  end subroutine SetAddIndTab





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAddGtab(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetAddGtab(rmax)

    integer, intent(in) :: rmax
    integer :: r,nsum,mu,nu,munu,IndMu(0:3),IndN(0:3)

    if (rmax.le.1) return

    if (allocated(AddGtab)) deallocate(AddGtab)
    allocate(AddGtab(RtS(rmax),RtS(rmax/2)))

    do nsum=1,rmax/2
      do nu=RtS(nsum-1)+1,RtS(nsum)
        IndN = LorIndTab(:,nu)

        do munu=RtS(2*nsum-1)+1,RtS(2*nsum)
          if (all(LorIndTab(:,munu).eq.2*IndN)) then
            AddGtab(1,nu) = munu 
          end if
        end do
               
        do r=1,rmax-2*nsum
          do mu=RtS(r-1)+1,RtS(r)
            IndMu = LorIndTab(:,mu)

            do munu=RtS(r+2*nsum-1)+1,RtS(r+2*nsum)
               
               if (all(LorIndTab(:,munu).eq.IndMu+2*IndN)) then
                 AddGtab(mu,nu) = munu 
               end if

            end do

          end do
        end do

      end do
    end do

  end subroutine SetAddGtab





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCFtab(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetCFtab(rmax)

    integer, intent(in) :: rmax
    integer :: mu, nu, IndMu(0:3), IndNu(0:3), CF, i, r

    if (allocated(CFtab)) deallocate(CFtab)
    allocate(CFtab(RtS(rmax),2:RtS(rmax/2)))

    do r=0,rmax
      do mu=RtS(r-1)+1,RtS(r)
        IndMu = LorIndTab(:,mu)

        do nu=2,RtS((rmax-r)/2)
          IndNu = LorIndTab(:,nu)
       
          CF = (-1)**(IndNu(1)+IndNu(2)+IndNu(3))
          do i=0,3
            CF = CF*CalcFactorial(IndMu(i)+2*IndNu(i)) &
               /(2**IndNu(i)*CalcFactorial(IndMu(i))*CalcFactorial(IndNu(i)))          
          end do
          CFtab(mu,nu) = CF

        end do
      end do
    end do

  end subroutine SetCFtab




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndsPiProd(Nmax,rmax)
  !
  !  sets the global variable IndsPiProd to the table
  !  IndsPiProd = (CIPP(1,rmax),...,CIPP(Nmax,rmax))
  !  CIPP = CalcIndsPiProd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndsPiProd(Nm1max,rmax)
  
    integer, intent(in) :: Nm1max, rmax
    integer :: strmax, Nm1
    
    strmax = BinomTable(rmax,rmax+Nm1max-1)
    if (allocated(IndsPiProd)) then
      deallocate(IndsPiProd)
    end if
    allocate(IndsPiProd(0:1,Nm1max,strmax,Nm1max))
    IndsPiProd = 0
    
    do Nm1=1,Nm1max
      strmax = BinomTable(rmax,rmax+Nm1-1)
      IndsPiProd(0:1,1:Nm1,1:strmax,Nm1) = CalcIndsPiProd(Nm1,rmax)
    end do
    
  end subroutine SetIndsPiProd
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndsPiProd(Nm1,r)
  !
  !  calculates resulting indices for product of momentum
  !  tensor (p_i1,...,p_ik) with p_j (im,j = 1,...,Nm1).
  !  Further the number of indices equal to j in the final
  !  result is given.
  !
  !  example: Nm1=2, r=3
  !  momentum tensors: 
  !  rank3: 1: p1p1p1,  2: p1p1p2,  3: p1p2p2,  4: p2p2p2
  !  rank4: 1: p1p1p1p1,  2: p1p1p1p2,  3: p1p1p2p2,  
  !         4: p1p2p2p2,  5: p2p2p2p2
  !
  !  IndsPiProd(2,1,0:1) = (2,3),   IndsPiProd(2,2,0:1) = (3,2)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndsPiProd(Nm1,r) result(IndsPiProd)
  
    integer, intent(in) :: Nm1, r
    integer, dimension(:,:,:), allocatable :: IndsPiProd
    integer :: str, i, j, inds(r), indsp1(r+1), pos, cnt

    str = BinomTable(r,r+Nm1-1)
    allocate(IndsPiProd(0:1,1:Nm1,1:str))
    
    do i=1,str
      inds = IndCombisEq(1:r,i,r,Nm1)
      
      pos = 1
      do j=1,Nm1
        
        cnt = 1
        do while (pos.le.r)
          if (inds(pos)>j) then
            exit
          else if (inds(pos).eq.j) then
            cnt = cnt+1
          end if
          pos = pos+1
        end do
        
        indsp1(1:pos-1) = inds(1:pos-1)
        indsp1(pos) = j
        indsp1(pos+1:r+1) = inds(pos:r)

        IndsPiProd(0,j,i) = CalcPosIndCombisEq(Nm1,r+1,indsp1)
        IndsPiProd(1,j,i) = cnt
        
      end do
      
    end do
  
  end function CalcIndsPiProd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCalcUV_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCalcUV_ten()

    CalcUV_ten = .false.

  end subroutine SwitchOffCalcUV_ten





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCalcUV_ten()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCalcUV_ten()

    CalcUV_ten = .true.

  end subroutine SwitchOnCalcUV_ten
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensors_cll(T1,T2,MomVec,MomInv,masses2,N,rmax,Tdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTensors_cll(T1,T2,MomVec,MomInv,masses2,norm,N,rmax,Tdiff)

    integer, intent(in) :: N, rmax
    double complex, intent(in) :: T1(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: T2(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: MomVec(0:3,N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double precision, intent(in) :: norm(0:rmax)    
    double precision, intent(out) :: Tdiff(0:rmax)    
    integer :: r,n0,n1,n2,n3,i,j,flag
    double complex :: diffTN
    double precision :: ratio
    character(len=*),parameter :: fmt1 = "(A8,'(',i2,')  = dcmplx(',d25.18,' , ',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' TNten(',i1,',',i1,',',i1,',',i1,') = (',d23.16,' , ',d23.16,' ), r=',i2)"
    character(len=*),parameter :: fmt3 = &
    "(A7,'(',i1,',',i2,') = dcmplx(',d25.18,' , ',d25.18,' )')"    


    CheckCntten_cll(N) = CheckCntten_cll(N) + 1

!    data CheckCntTN /0/

    flag=1
    if(DiffCntten_cll(N).ge.MaxCheck_cll(N)) flag=0
    if(ncheckout_cll.eq.-1) flag=0

    ratio=0d0

    Tdiff=0d0
    do r=0,rmax
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3=r-n0-n1-n2

            diffTN = T1(n0,n1,n2,n3)-T2(n0,n1,n2,n3)
            Tdiff(r) = max(Tdiff(r),abs(diffTN)/norm(r))
            if ((abs(diffTN).gt.checkacc_cll*norm(r)) .and.(flag.eq.1)) then
              write(ncheckout_cll,*) '***************************************************************************'
              write(ncheckout_cll,'(A15,I2,A16,I10)') 'TNten with N =',N,', difference NO.', DiffCntten_cll(N)+1
              write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
              write(ncheckout_cll,'(A24,I2,A10,I2,A4,I2)') 'TNten integral with N =', N, ' and rank ', r,' of ',rmax
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,*) 'GLOBAL PARAMETERS:'
              write(ncheckout_cll,*) 'mode        ', mode_cll
              write(ncheckout_cll,*) 'muUV2       ', muUV2_cll
              write(ncheckout_cll,*) 'muIR2       ', muIR2_cll
              write(ncheckout_cll,*) 'deltaUV     ', deltaUV_cll
              write(ncheckout_cll,*) 'deltaIR1    ', deltaIR1_cll
              write(ncheckout_cll,*) 'deltaIR2    ', deltaIR2_cll
              write(ncheckout_cll,*) 'nminf       ', nminf_cll
              do i=1,nminf_cll
                write(ncheckout_cll,*) 'minf2       ', i, minf2_cll(i)
              end do
              write(ncheckout_cll,*) 'dprec       ', dprec_cll
              write(ncheckout_cll,*) 'reqacc      ', reqacc_cll
              write(ncheckout_cll,*) 'critacc     ', critacc_cll
              write(ncheckout_cll,*) 'checkacc    ', checkacc_cll
              write(ncheckout_cll,*) 'ErrFlag     ', ErrFlag_cll
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              do i=1,N-1
                do j=0,3
                  write(ncheckout_cll,fmt3) 'MomVec', j, i, MomVec(j,i)
                end do
              end do              
              do i=1,BinomTable(2,N)
                write(ncheckout_cll,fmt1) 'MomInv ', i, MomInv(i)
              end do
              do i=0,N-1
                write(ncheckout_cll,fmt1) 'masses2', i, masses2(i)
              end do
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,T1(0,0,0,0),0
              write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,T2(0,0,0,0),0
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(n0,n1,n2,n3),r
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(n0,n1,n2,n3),r
              if(norm(r).ne.0d0)then
                write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
                ratio=abs(diffTN)/norm(r)
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
              flag=2
            elseif((flag.eq.2).and.(abs(diffTN).gt.ratio*norm(r))) then
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(n0,n1,n2,n3),r
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(n0,n1,n2,n3),r
              if(norm(r).gt.1d-100)then
                write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
                ratio=abs(diffTN)/norm(r)
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
            elseif ((flag.eq.0).and.(abs(diffTN).gt.checkacc_cll*norm(r))) then
              flag=3
            end if

          end do
        end do
      end do
    end do

    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end TNten '
      write(ncheckout_cll,*)
      DiffCntten_cll(N) =  DiffCntten_cll(N) + 1
      if(DiffCntten_cll(N).eq.MaxCheck_cll(N)) then
        write(ncheckout_cll,'((A),I4)') ' Further output for differences in TNten functions suppressed for N =', N   
        write(ncheckout_cll,*)
      endif
    else if (flag.eq.3) then
      DiffCntten_cll(N) =  DiffCntten_cll(N) + 1
    endif

  end subroutine CheckTensors_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensorsList_cll(T1,T2,MomVec,MomInv,masses2,N,rmax,Tdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTensorsList_cll(T1,T2,MomVec,MomInv,masses2,norm,N,rmax,Tdiff)

    integer, intent(in) :: N, rmax
!    double complex, intent(in) :: T1(0:RtS(rmax)), T2(0:RtS(rmax))
    double complex, intent(in) :: T1(RtS(rmax)), T2(RtS(rmax))
    double complex, intent(in) :: MomVec(0:3,N-1)
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double precision, intent(in) :: norm(0:rmax)    
    double precision, intent(out) :: Tdiff(0:rmax)    
    integer :: r,ind,i,j,flag,n0,n1,n2,n3
    double complex :: diffTN
    double precision :: ratio
    character(len=*),parameter :: fmt1 = "(A8,'(',i2,')  = dcmplx(',d25.18,' , ',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' TNten(',i1,',',i1,',',i1,',',i1,') = (',d23.16,' , ',d23.16,' ), r=',i2)"    
    character(len=*),parameter :: fmt3 = &
    "(A7,'(',i1,',',i2,') = dcmplx(',d25.18,' , ',d25.18,' )')"

    CheckCntten_cll(N) = CheckCntten_cll(N) + 1

!    data CheckCntTN /0/

    flag=1
    if(DiffCntten_cll(N).ge.MaxCheck_cll(N)) flag=0
    if(ncheckout_cll.eq.-1) flag=0
    
    ratio=0d0

    Tdiff=0d0
    do r=0,rmax
      do ind=RtS(r-1)+1,RtS(r)

        n0 = LorIndTab(0,ind)
        n1 = LorIndTab(1,ind)
        n2 = LorIndTab(2,ind)
        n3 = LorIndTab(3,ind)

        diffTN = T1(ind)-T2(ind)
        Tdiff(r) = max(Tdiff(r),abs(diffTN)/norm(r))
        
        if ((abs(dreal(diffTN)).gt.checkacc_cll*norm(r).or.abs(dimag(diffTN)).gt.checkacc_cll*norm(r)) .and.(flag.eq.1)) then
          write(ncheckout_cll,*) '***************************************************************************'
          write(ncheckout_cll,'(A15,I2,A16,I10)') 'TNten with N =',N,' difference NO.', DiffCntten_cll(N)+1
          write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
          write(ncheckout_cll,'(A24,I2,A10,I2,A4,I2)') 'TNten integral with N =', N, ' and rank ', r,' of ',rmax
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,*) 'GLOBAL PARAMETERS:'
          write(ncheckout_cll,*) 'mode        ', mode_cll
          write(ncheckout_cll,*) 'muUV2       ', muUV2_cll
          write(ncheckout_cll,*) 'muIR2       ', muIR2_cll
          write(ncheckout_cll,*) 'deltaUV     ', deltaUV_cll
          write(ncheckout_cll,*) 'deltaIR1    ', deltaIR1_cll
          write(ncheckout_cll,*) 'deltaIR2    ', deltaIR2_cll
          write(ncheckout_cll,*) 'nminf       ', nminf_cll
          do i=1,nminf_cll
            write(ncheckout_cll,*) 'minf2       ', i, minf2_cll(i)
          end do
          write(ncheckout_cll,*) 'dprec       ', dprec_cll
          write(ncheckout_cll,*) 'reqacc      ', reqacc_cll
          write(ncheckout_cll,*) 'critacc     ', critacc_cll
          write(ncheckout_cll,*) 'checkacc    ', checkacc_cll
          write(ncheckout_cll,*) 'ErrFlag     ', ErrFlag_cll
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          do i=1,N-1
            do j=0,3
              write(ncheckout_cll,fmt3) 'MomVec', j, i, MomVec(j,i)
            end do
          end do              
          do i=1,BinomTable(2,N)
            write(ncheckout_cll,fmt1) 'MomInv ', i, MomInv(i)
          end do
          do i=0,N-1
            write(ncheckout_cll,fmt1) 'masses2', i, masses2(i)
          end do
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,T1(1),0
          write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,T2(1),0
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(ind),r
          if(norm(r).ne.0d0)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
            ratio=abs(diffTN)/norm(r)
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
          flag=2
        elseif((flag.eq.2).and.(abs(diffTN).gt.ratio*norm(r))) then
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(ind),r
          if(norm(r).gt.1d-100)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
            ratio=abs(diffTN)/norm(r)
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
        elseif ((flag.eq.0).and.(abs(diffTN).gt.checkacc_cll*norm(r))) then
          flag=3
        end if
            
      end do
    end do

    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end TNten '
      write(ncheckout_cll,*)
      DiffCntten_cll(N) =  DiffCntten_cll(N) + 1
      if(DiffCntten_cll(N).eq.MaxCheck_cll(N)) then
        write(ncheckout_cll,*) ' Further output for differences in TNten functions suppressed for N =', N   
        write(ncheckout_cll,*)
      endif
    else if (flag.eq.3) then
      DiffCntten_cll(N) =  DiffCntten_cll(N) + 1
    endif      

  end subroutine CheckTensorsList_cll 
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTenA_cll(T1,T2,mass2,norm,rmax,Tdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTenA_cll(T1,T2,masses2,norm,rmax,Tdiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: T1(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: T2(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: masses2(0:0)
    double precision, intent(in) :: norm(0:rmax)    
    double precision, intent(out) :: Tdiff(0:rmax)    
    integer :: r,n0,n1,n2,n3,i,j,flag
    double complex :: diffTN
    double precision :: ratio
    character(len=*),parameter :: fmt1 = "(A8,'(',i2,')  = dcmplx(',d25.18,',',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' TNten(',i1,',',i1,',',i1,',',i1,') = (',d23.16,' , ',d23.16,' ), r=',i2)"    
    character(len=*),parameter :: fmt3 = &
    "(A7,'(',i1,',',i2,') = dcmplx(',d25.18,' , ',d25.18,' )')"

    CheckCntten_cll(1) = CheckCntten_cll(1) + 1

!    data CheckCntTN /0/

    flag=1
    if(DiffCntten_cll(1).ge.MaxCheck_cll(1)) flag=0
    if(ncheckout_cll.eq.-1) flag=0
    
    ratio=0d0

    Tdiff=0d0
    do r=0,rmax
      do n0=0,r
        do n1=0,r-n0
          do n2=0,r-n0-n1
            n3=r-n0-n1-n2

            diffTN = T1(n0,n1,n2,n3)-T2(n0,n1,n2,n3)
            Tdiff(r) = max(Tdiff(r),abs(diffTN)/norm(r))
            if ((abs(dreal(diffTN)).gt.checkacc_cll*norm(r).or.abs(dimag(diffTN)).gt.checkacc_cll*norm(r)) .and.(flag.eq.1)) then
              write(ncheckout_cll,*) '***************************************************************************'
              write(ncheckout_cll,'(A15,I2,A16,I10)') 'TNten with N =',1,' difference NO.', DiffCntten_cll(1)+1
              write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
              write(ncheckout_cll,'(A24,I2,A10,I2,A4,I2)') 'TNten integral with N =', 1, ' and rank ', r,' of ',rmax
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,*) 'GLOBAL PARAMETERS:'
              write(ncheckout_cll,*) 'mode        ', mode_cll
              write(ncheckout_cll,*) 'muUV2       ', muUV2_cll
              write(ncheckout_cll,*) 'muIR2       ', muIR2_cll
              write(ncheckout_cll,*) 'deltaUV     ', deltaUV_cll
              write(ncheckout_cll,*) 'deltaIR1    ', deltaIR1_cll
              write(ncheckout_cll,*) 'deltaIR2    ', deltaIR2_cll
              write(ncheckout_cll,*) 'nminf       ', nminf_cll
              do i=1,nminf_cll
                write(ncheckout_cll,*) 'minf2       ', i, minf2_cll(i)
              end do
              write(ncheckout_cll,*) 'dprec       ', dprec_cll
              write(ncheckout_cll,*) 'reqacc      ', reqacc_cll
              write(ncheckout_cll,*) 'critacc     ', critacc_cll
              write(ncheckout_cll,*) 'checkacc    ', checkacc_cll
              write(ncheckout_cll,*) 'ErrFlag     ', ErrFlag_cll
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,fmt1) 'masses2', 0, masses2(0)
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,T1(0,0,0,0),0
              write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,T2(0,0,0,0),0
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(n0,n1,n2,n3),r
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(n0,n1,n2,n3),r
              if(norm(r).ne.0d0)then
                write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
                ratio=abs(diffTN)/norm(r)
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
              flag=2
            elseif((flag.eq.2).and.(abs(diffTN).gt.ratio*norm(r))) then
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(n0,n1,n2,n3),r
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(n0,n1,n2,n3),r
              if(norm(r).gt.1d-100)then
                write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
                ratio=abs(diffTN)/norm(r)
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
            elseif ((flag.eq.0).and.(abs(diffTN).gt.checkacc_cll*norm(r))) then
              flag=3
            end if
            
          end do
        end do
      end do
    end do

    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end TNten '
      write(ncheckout_cll,*)
      DiffCntten_cll(1) =  DiffCntten_cll(1) + 1
      if(DiffCntten_cll(1).eq.MaxCheck_cll(1)) then
        write(ncheckout_cll,*) ' Further output for differences in TNten functions suppressed for N =', 1   
        write(ncheckout_cll,*)
      endif
    else if (flag.eq.3) then
      DiffCntten_cll(1) =  DiffCntten_cll(1) + 1
    endif      

  end subroutine CheckTenA_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckTensorsAList_cll(T1,T2,masses2,norm,rmax,Tdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckTenAList_cll(T1,T2,masses2,norm,rmax,Tdiff)

    integer, intent(in) :: rmax
!    double complex, intent(in) :: T1(0:RtS(rmax)), T2(0:RtS(rmax))
    double complex, intent(in) :: T1(RtS(rmax)), T2(RtS(rmax))
    double complex, intent(in) :: masses2(0:0)
    double precision, intent(in) :: norm(0:rmax)
    double precision, intent(out) :: Tdiff(0:rmax)    
    integer :: r,ind,i,j,flag,n0,n1,n2,n3
    double complex :: diffTN
    double precision :: ratio
    character(len=*),parameter :: fmt1 = "(A8,'(',i2,')  = dcmplx(',d25.18,',',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' TNten(',i1,',',i1,',',i1,',',i1,') = (',d23.16,' , ',d23.16,' ), r=',i2)"    
    character(len=*),parameter :: fmt3 = &
    "(A7,'(',i1,',',i2,') = dcmplx(',d25.18,' , ',d25.18,' )')"

    CheckCntten_cll(1) = CheckCntten_cll(1) + 1

!    data CheckCntTN /0/

    flag=1
    if(DiffCntten_cll(1).ge.MaxCheck_cll(1)) flag=0
    if(ncheckout_cll.eq.-1) flag=0
    
    ratio=0d0

    Tdiff=0d0
    do r=0,rmax
      do ind=RtS(r-1)+1,RtS(r)

        n0 = LorIndTab(0,ind)
        n1 = LorIndTab(1,ind)
        n2 = LorIndTab(2,ind)
        n3 = LorIndTab(3,ind)
        diffTN = T1(ind)-T2(ind)
        Tdiff(r) = max(Tdiff(r),abs(diffTN)/norm(r))
        
        if ((abs(dreal(diffTN)).gt.checkacc_cll*norm(r).or.abs(dimag(diffTN)).gt.checkacc_cll*norm(r)) .and.(flag.eq.1)) then
          write(ncheckout_cll,*) '***************************************************************************'
          write(ncheckout_cll,'(A15,I2,A16,I10)') 'TNten with N =',1,' difference NO.', DiffCntten_cll(1)+1
          write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
          write(ncheckout_cll,'(A24,I2,A10,I2,A4,I2)') 'TNten integral with N =', 1, ' and rank ', r,' of ',rmax
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,*) 'GLOBAL PARAMETERS:'
          write(ncheckout_cll,*) 'mode        ', mode_cll
          write(ncheckout_cll,*) 'muUV2       ', muUV2_cll
          write(ncheckout_cll,*) 'muIR2       ', muIR2_cll
          write(ncheckout_cll,*) 'deltaUV     ', deltaUV_cll
          write(ncheckout_cll,*) 'deltaIR1    ', deltaIR1_cll
          write(ncheckout_cll,*) 'deltaIR2    ', deltaIR2_cll
          write(ncheckout_cll,*) 'nminf       ', nminf_cll
          do i=1,nminf_cll
            write(ncheckout_cll,*) 'minf2       ', i, minf2_cll(i)
          end do
          write(ncheckout_cll,*) 'dprec       ', dprec_cll
          write(ncheckout_cll,*) 'reqacc      ', reqacc_cll
          write(ncheckout_cll,*) 'critacc     ', critacc_cll
          write(ncheckout_cll,*) 'checkacc    ', checkacc_cll
          write(ncheckout_cll,*) 'ErrFlag     ', ErrFlag_cll
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,fmt1) 'masses2', 0, masses2(0)
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,T1(1),0
          write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,T2(1),0
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(ind),r
          if(norm(r).ne.0d0)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
            ratio=abs(diffTN)/norm(r)
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
          flag=2
        elseif((flag.eq.2).and.(abs(diffTN).gt.ratio*norm(r))) then
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,T1(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,T2(ind),r
          if(norm(r).gt.1d-100)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm(r)
            ratio=abs(diffTN)/norm(r)
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
        elseif ((flag.eq.0).and.(abs(diffTN).gt.checkacc_cll*norm(r))) then
          flag=3
        end if
            
      end do
    end do

    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end TNten '
      write(ncheckout_cll,*)
      DiffCntten_cll(1) =  DiffCntten_cll(1) + 1
      if(DiffCntten_cll(1).eq.MaxCheck_cll(1)) then
        write(ncheckout_cll,*) ' Further output for differences in TNten functions suppressed for N =', 1   
        write(ncheckout_cll,*)
      endif
    else if (flag.eq.3) then
      DiffCntten_cll(1) =  DiffCntten_cll(1) + 1
    endif      

  end subroutine CheckTenAList_cll  


end module InitTensors
