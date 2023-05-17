!!
!!  File Combinatorics.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  **************************
!  *  module Combinatorics  *
!  *     by Lars Hofer      *
!  **************************
!
!
!  global variables:
!  BinomTable, IndCombis, IndCombisEq, DropIndCombisEq
!
!  functions and subroutines:
!  SetBinomTable, CalcBinomTable, CalcBino, SetIndCombis, CalcIndCombis,
!  SetIndCombisEq, CalcIndCombisEq, SetDropIndCombisEq, CalcDropIndCombisEq, 
!  CalcPosIndCombisEq, CalcFactorial, CalcOrderedCombis, CalcOrderedCombis0
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module Combinatorics

  use collier_global

  implicit none
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  global variables BinomTable, IndCombis, IndCombisEq, DropIndCombisEq
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  integer, dimension(:,:), allocatable :: BinomTable
  integer, dimension(:,:,:,:), allocatable :: IndCombis
  integer, dimension(:,:,:,:), allocatable :: IndCombisEq
  integer, dimension(:,:,:,:,:), allocatable :: DropIndCombisEq
  integer, dimension(:,:,:), allocatable :: CntInds
  integer, allocatable :: AddToCInd(:,:,:,:), DropCInd(:,:,:,:), DropCInd2(:,:,:,:)
  integer, allocatable :: NCoefs(:,:),NCoefsG(:,:)   
  
contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetBinomTable(N)
  !
  !  sets the global variable BinomTable to the value CalcBinomTable(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetBinomTable(N)
    
    integer, intent(in) :: N
    
    if (N<0) then
      write(nerrout_cll,*) N, ' is not a non-negative integer'
      stop
    end if
    
    if (allocated(BinomTable)) then
      deallocate(BinomTable)
    end if
    allocate(BinomTable(0:N,0:N))
    
    BinomTable = CalcBinomTable(N)
  
  end subroutine SetBinomTable



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcBinomTable(N)
  !
  !  calculates the table
  !  BinomTable = (((1,0), (1,1), 0, ..., 0),
  !                ((2,0), (2,1), (2,2), 0, ..., 0),
  !                ....
  !                ((N,0), (N,1), (N,2), ...., (N,N))
  !  of all binomial coefficients (n,k) ("n over k") up to n=N
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcBinomTable(N)
  
    integer, intent(in) :: N
    integer, dimension(0:N,0:N) :: CalcBinomTable
    integer :: i,j

    CalcBinomTable = 0
    do i=0,N
      do j=0,i
        CalcBinomTable(j,i) = CalcBino(i,j)
      end do
    end do
    
  end function CalcBinomTable
    
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcBino(n,k)
  !
  !  calculates binomial coefficient "n over k"
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcBino(n,k) result(bino)
  
    integer, intent(in) :: k,n
    integer bino
  
    if ((k < 0).or.(n < 0).or.(k > n)) then
      write (*,*) 'binomial coefficient ', n, ' over ', k, &
                 & ' not defined'
      stop
    end if
    
    if ((k.eq.0).or.(k.eq.n)) then
      bino = 1
    else
      bino = CalcBino(n-1,k-1) + CalcBino(n-1,k)
    end if
    
  end function CalcBino
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndCombis(N)
  !
  !  sets the global variable IndCombis to the value CalcIndCobis(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndCombis(N)
  
    integer, intent(in) :: N
    integer :: binoMax
    
    if (N<1) then
      write (*,*) N, ' is not a positive integer'
      stop
    end if
    
    ! determine maximum number of index combinations
    if (mod(N,2).eq.0) then
      binoMax = BinomTable(N/2,N)
    else
      binoMax = BinomTable((N-1)/2,N)
    end if
    
    if (allocated(IndCombis)) then
      deallocate(IndCombis)
    end if
    allocate(IndCombis(N,binoMax,N,N))
    
    IndCombis = CalcIndCombis(N)
  
  end subroutine SetIndCombis
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndCombis(N)
  !
  !  calculates the table
  !  CalcIndCombis(N) = ((COC(1,1,0), 0, .... 0),
  !                      (COC(2,1,0), COC(2,2,0), 0, ..., 0),
  !                      ....             
  !                      (COC(N,1,0), COC(N,2,0), ..., COC(N,N,0))
  !  (COC = CalcOrderedCombis)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndCombis(N)
  
    integer, intent(in) :: N
    integer, dimension(:,:,:,:), allocatable :: CalcIndCombis
    integer :: bino, binoMax, i, j
    
    ! determine maximum number of index combinations
    if (mod(N,2).eq.0) then
      binoMax = BinomTable(N/2,N)
    else
      binoMax = BinomTable((N-1)/2,N)
    end if
    
    allocate(CalcIndCombis(N,binoMax,N,N))
    CalcIndCombis = 0
    
    do i=1,N    
      do j=1,i        
        bino = BinomTable(j,i)
        CalcIndCombis(1:j,1:bino,j,i) = CalcOrderedCombis(i,j,0)
      end do      
    end do
    
  end function CalcIndCombis
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetIndCombisEq(N,k)
  !
  !  sets the global variable IndCombisEq to the value 
  !  CalcIndCombisEq(N,k)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetIndCombisEq(N,k)
  
    integer, intent(in) :: N,k
    integer :: binoMax
    
    if (N<1) then
      write (*,*) N, ' is not a positive integer'
      stop
    end if
    
    ! determine maximum number of index combinations
    binoMax = BinomTable(k,N+k-1)
    
    if (allocated(IndCombisEq)) then
      deallocate(IndCombisEq)
    end if
    allocate(IndCombisEq(k,binoMax,k,N))
    
    IndCombisEq = CalcIndCombisEq(N,k)
  
  end subroutine SetIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcIndCombisEq(N,k)
  !
  !  calculates the table
  !  CalcIndCombis(N) = ((COC(1,1,1), 0, .... 0),
  !                      (COC(2,1,1), COC(2,2,1), 0, ..., 0),
  !                      ....             
  !                      (COC(N,1,1), COC(N,2,1), ..., COC(N,k,0))
  !  (COC = CalcOrderedCombis)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcIndCombisEq(N,k)
  
    integer, intent(in) :: N, k
    integer, dimension(:,:,:,:), allocatable :: CalcIndCombisEq
    integer :: bino, i, j
    
    allocate(CalcIndCombisEq(k,BinomTable(k,N+k-1),k,N))
    CalcIndCombisEq = 0
    
    do i=1,N    
      do j=1,k        
        bino = BinomTable(j,i+j-1)
        CalcIndCombisEq(1:j,1:bino,j,i) = CalcOrderedCombis(i,j,1)
      end do      
    end do
    
  end function CalcIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDropIndCombisEq(Nmax,k)
  !
  !  set global variable DropIndCombisEq to
  !  DropIndCombisEq = (((CDICE(2,1,1), 0, ..., 0),
  !                      (CDICE(2,2,1), 0, ..., 0),
  !                      ...
  !                      (CDICE(2,kmax,1),0,..., 0)),
  !                     ((CDICE(3,1,1),CDICE(3,1,2),0,...,0),
  !                      ...
  !                      (CDICE(3,kmax,1),CDICE(3,kmax,2),0,...,0)),
  !                     ((CDICE(Nmax,1,1),...,CDICE(Nmax,kmax,Nmax-1)),
  !                      ...
  !                      ((CDICE(Nmax,kmax,1),...,CDICE(Nmax,kmax,Nmax-1)))
  !  CDICE = CalcDropIndCombisEq
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetDropIndCombisEq(Nmax,kmax)
  
    integer, intent(in) :: Nmax, kmax
    integer :: BinoMax1, BinoMax2, N, k, nd
    
    BinoMax1 = BinomTable(Nmax/2,Nmax)
    BinoMax2 = BinomTable(kmax,Nmax+kmax-2)
    
    if (allocated(DropIndCombisEq)) then
      deallocate(DropIndCombisEq)
    end if
    allocate(DropIndCombisEq(BinoMax2,BinoMax1,Nmax-1,0:kmax,2:Nmax))
    DropIndCombisEq = 0
    
    do N=2,Nmax
      do nd=1,N-1
        DropIndCombisEq(1,1:BinomTable(nd,N),nd,0,n) = 1
        do k=1,kmax
          DropIndCombisEq(1:,1:,nd,k,N) = CalcDropIndCombisEq(N,k,nd)
        end do
      end do
    end do
    
  end subroutine SetDropIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDropIndCombisEq(N,k,nd)
  !
  !  calculates table of elements of IndCombisEq(N,k) which 
  !  survive when elements (i_1,...,i_nd) in (1,...,N) are dropped
  !
  !  example:
  !  IndCombisEq(3,2) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !                        1      2      3      4      5      6
  !
  !  drop one element
  !  --> possible element to be dropped: ((1),(2),(3))
  !                                        1   2   3
  !  ==> CalcDropIndCombisEq(3,2,1) = ((4,5,6),(1,3,6),(1,2,4))
  !
  !  drop two elements 
  !  --> possible elements to be dropped: ((1,2),(1,3),(2,3))
  !                                          1     2     3
  !  ==> CalcDropIndCombisEq(3,2,2) = ((6),(4),(1))
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcDropIndCombisEq(N,k,nd)
  
    integer, intent(in) :: N, k, nd
    integer, dimension(:,:), allocatable :: CalcDropIndCombisEq, inds
    integer :: bino, bino_nd, i, j, m, pos, vals(nd), p, combis_nd
    logical :: contains_j
    
    bino = BinomTable(k,N+k-1)
    bino_nd = BinomTable(k,N+k-nd-1)
    combis_nd = BinomTable(nd,N)
    
    allocate(inds(k,bino))
    inds = IndCombisEq(1:k,1:bino,k,N)
    
    allocate(CalcDropIndCombisEq(bino_nd,combis_nd))
    
    do j=1,combis_nd
      pos = 1
      vals = IndCombis(1:nd,j,nd,N)
    
      do i=1,bino
        contains_j = .false.
        
        do m=1,k
          do p=1,nd
            if (inds(m,i).eq.vals(p)) then
              contains_j = .true.
            end if
          end do
        end do
        
        if (contains_j.eqv..false.) then
          CalcDropIndCombisEq(pos,j) = i
          pos = pos+1
        end if
        
      end do      
    end do
    
  end function CalcDropIndCombisEq
  
  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcPosIndCombisEq(N,r,inds)
  !
  !  determines position of index combination inds 
  !  within IndCombisEq(N,r)
  !
  !  example:
  !  IndCombisEq(3,2) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !  ==> CalcPosIndCombisEq(3,2,(2,3)) = 5
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcPosIndCombisEq(N,r,inds) result(pos)
  
    integer, intent(in) :: N, r, inds(r)
    integer :: k, i1, indsm1(r-1), pos
    
    if (r.eq.1) then
      pos = inds(1)
    
    else
      pos = 0
      i1 = inds(1)
      indsm1 = inds(2:r)-i1+1
      
      do k=1,i1-1
        pos = pos + BinomTable(r-1,N-k+r-1)
      end do
      
      pos = pos + CalcPosIndCombisEq(N-i1+1,r-1,indsm1)
      
    end if
  
  end function CalcPosIndCombisEq

  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcFactorial(n)
  !
  !  calculates the factorial of n
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function CalcFactorial(n) result(fact)
  
    integer, intent(in) :: n
    integer :: fact,i
    
    if (n < 0) then 
      write (*,*) 'factorial not defined for negative integer'
      stop
    end if
    
    fact = 1
    if (n .gt. 1) then
      do i=2,n
        fact = fact * i
      end do
    end if
    
  end function CalcFactorial

  
  
  
    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  functions 
  !  CalcOrderedCombis(n,k,opt)    CalcOrderedCombis0(n,k,opt)
  !
  !  determines all ordered(!) combinations taking 
  !  k different (opt=0) or k not necessarily different (opt=1) 
  !  numbers out of 1,...,n (CalcOrderedCombis)
  !  or out of 0,...,n (CalcOrderedCombis0)
  !
  !  example:
  !  CalcOrderedCombis(3,2,0) = ((1,2), (1,3), (2,3))
  !  CalcOrderedCombis(3,2,1) = ((1,1), (1,2), (1,3), (2,2), (2,3), (3,3))
  !  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  recursive function CalcOrderedCombis(n,k,opt) result(combis)
  
    integer, intent(in) :: n, k, opt
    integer, dimension(:,:), allocatable :: combis
    integer, dimension(:,:), allocatable :: CombiRec
    integer :: i, j, BinoRec, bino, num, lb
    
    bino = BinomTable(k,n+opt*(k-1))
    allocate(combis(k,bino))
    
    if (k.eq.1) then
      ! trivial case k=1
      ! result ((1),(2),(3),...,(n))
      do i=1,bino
        combis(1,i) = i
      end do
    
    else
      ! k-1 numbers out of n
      BinoRec = BinomTable(k-1,n+opt*(k-2))
      allocate(CombiRec(k-1,BinoRec))
      CombiRec = CalcOrderedCombis(n,k-1,opt)
      
      ! add the kth number 
      num = 1
      do i=1,BinoRec
        lb = CombiRec(k-1,i) + 1 - opt
        do j=lb,n
          combis(1:k-1,num) = CombiRec(1:k-1,i)
          combis(k,num)=j
          num = num + 1
        end do
      end do
    end if
    
  end function CalcOrderedCombis
  
  
  
  recursive function CalcOrderedCombis0(n,k,opt) result(combis)
  
    integer, intent(in) :: n, k, opt
    integer, dimension(:,:), allocatable :: combis
    integer, dimension(:,:), allocatable :: CombiRec
    integer :: i, j, bino, BinoRec, num, lb
    
    bino = BinomTable(k,n+1+opt*(k-1))
    allocate(combis(k,bino))
    
    if (k.eq.1) then
      ! trivial case k=1
      ! result ((0),(1),(2),...,(n))
      do i=1,n+1
        combis(1,i) = i-1
      end do
    
    else
      ! k-1 numbers out of n
      BinoRec = BinomTable(k-1,n+1+opt*(k-2))
      allocate(CombiRec(k-1,BinoRec))
      CombiRec = CalcOrderedCombis0(n,k-1,opt)
      
      ! add the kth number 
      num = 1
      do i=1,BinoRec
        lb = CombiRec(k-1,i) + 1 - opt
        do j=lb,n
          combis(1:k-1,num) = CombiRec(1:k-1,i)
          combis(k,num)=j
          num = num + 1
        end do
      end do
    end if
    
  end function CalcOrderedCombis0





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcPermutations(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function CalcPermutations(N)  result(perm)

    integer :: N
    integer, allocatable :: perm(:,:), perm_rec(:,:)
    integer :: i,j,k,cnt

    allocate(perm(N,CalcFactorial(N)))

    if (N.eq.1) then
      perm(1,1) = 1
    
    else
      allocate(perm_rec(N-1,CalcFactorial(N-1)))
      perm_rec = CalcPermutations(N-1)
      
      cnt = 1
      do i=1,CalcFactorial(N-1)
        do j=N,1,-1
          do k=1,N
            if (k.lt.j) then
              perm(k,cnt) = perm_rec(k,i)
            else if (k.eq.j) then
              perm(k,cnt) = N
            else
              perm(k,cnt) = perm_rec(k-1,i)
            end if
          end do
          cnt = cnt+1
        end do
      end do

    end if

  end function CalcPermutations





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcCIndArr
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcCIndArr(Nm1,r,cind) result(arr)

    integer :: Nm1, r, cind, arr(Nm1), i
    integer :: combis(r)    

    if (r.ge.1) then
      combis = IndCombisEq(1:r,cind,r,Nm1)
    end if
    arr = 0
    do i=1,r
      arr(combis(i)) = arr(combis(i))+1
    end do    

  end function CalcCIndArr





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDropCInd(Nm1,r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetDropCInd(Nm1,r)

    integer :: Nm1,r,i,j,k

    if (allocated(DropCInd)) then
      deallocate(DropCInd)
    end if
    allocate(DropCInd(Nm1,BinomTable(r,r+Nm1-1),0:r,Nm1))

    do i=1,Nm1
      DropCInd(1:i,1,0,i) = 1
      do j=1,r
        do k=1,i
          DropCind(k,1:BinomTable(j,j+i-1),j,i) = CalcDropCInd(i,j,k)
        end do
      end do
    end do

  end subroutine SetDropCInd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDropCInd(Nm1,r,k)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcDropCInd(Nm1,r,k) result(dcinds)

    integer :: Nm1,r,k,dcinds(BinomTable(r,r+Nm1-1))
    integer :: cnt,arr(Nm1),i

    cnt = 1
    do i=1,BinomTable(r,r+Nm1-1)
      arr = CalcCIndArr(Nm1,r,i)
      if (arr(k).eq.0) then
!      if (arr(k).ne.0) then
        dcinds(i) = cnt
        cnt = cnt+1
      else
        dcinds(i) = 0
      end if
    end do

  end function CalcDropCInd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDropCInd2(Nm1,r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetDropCInd2(Nm1,r)

    integer :: Nm1,r,i,j,k

    if (allocated(DropCInd2)) then
      deallocate(DropCInd2)
    end if
    allocate(DropCInd2(Nm1,BinomTable(r,r+Nm1-1),0:r,Nm1))

    do i=1,Nm1
      DropCInd2(1:i,1,0,i) = 0
      do j=1,r
        do k=1,i
          DropCind2(k,1:BinomTable(j,j+i-1),j,i) = CalcDropCInd2(i,j,k)
        end do
      end do
    end do

  end subroutine SetDropCInd2





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcDropCInd2(Nm1,r,k)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcDropCInd2(Nm1,r,k) result(dcinds)

    integer :: Nm1,r,k,dcinds(BinomTable(r,r+Nm1-1))
    integer :: cnt,arr(Nm1),i

    cnt = 1
    do i=1,BinomTable(r,r+Nm1-1)
      arr = CalcCIndArr(Nm1,r,i)
      if (arr(k).ne.0) then
        dcinds(i) = cnt
        cnt = cnt+1
      else
        dcinds(i) = 0
      end if
    end do

  end function CalcDropCInd2





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAddToCInd(Nm1,r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetAddToCInd(Nm1,r)

    integer, intent(in) :: Nm1,r
    integer :: i,j,k,m

    if (allocated(AddToCInd)) then
      deallocate(AddToCInd)
    end if
    allocate(AddToCInd(Nm1,BinomTable(r,Nm1+r-1),0:r,Nm1))
    AddToCInd = 0

    do i=1,Nm1
      do j=0,r
        do k=1,BinomTable(j,i+j-1)
          do m=1,i
            AddToCInd(m,k,j,i) = CalcAddToCInd(i,j,k,m)
          end do
        end do
      end do
    end do

  end subroutine SetAddToCInd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcAddToCInd
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcAddToCInd(Nm1,r,cind,k) result(newcind)

    integer :: Nm1,r,k,cind,newcind,i
    integer :: indArr(r),newindArr(r+1)
    logical :: kflag

    if ((r.eq.0).and.(cind.eq.1)) then
      newcind = k
      return
    end if

    indArr = IndCombisEq(1:r,cind,r,Nm1)
    
    kflag = .true.
    do i=1,r
      if (indArr(i).ge.k) then
        newindArr(i+1) = indArr(i)
        if (kflag) then
          newindArr(i) = k
          kflag = .false.
        end if
      else
        newindArr(i) = indArr(i)
      end if
    end do
    if (kflag) then
      newindArr(r+1) = k
    end if

    newcind = CalcPosIndCombisEq(Nm1,r+1,newindArr)
      
  end function CalcAddToCInd





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetNCoefs(N,rmax)
  !
  !  constructs the global table NCoefs
  !  NCoefs(N,rmax) gives the total number of Lorentz-invariant
  !  N-point coefficients of rank r<=rmax
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetNCoefs(N,rmax)

    integer, intent(in) :: N,rmax
    integer :: i,r,n0

    if (allocated(NCoefs)) then
      deallocate(NCoefs)
    end if
    allocate(NCoefs(0:rmax,1:N))

    do r=0,rmax
      NCoefs(r,1) = r/2+1
      do i=2,N
        NCoefs(r,i) = CalcNCoefs(i-1,r)
      end do
    end do

  end subroutine SetNCoefs





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcNCoefs(Nm1,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcNCoefs(Nm1,rmax) result(nc)

    integer, intent(in) :: Nm1,rmax
    integer :: r, n0, nc, bino, i

    nc = 0
    do r=0,rmax
      do n0=0,r/2
        bino = 1
        do i=r-2*n0+1,Nm1+r-2*n0-1
          bino = bino*i
        end do
        nc = nc + bino/CalcFactorial(Nm1-1)
      end do
    end do

  end function CalcNCoefs  





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetNCoefsG(N,rmax)
  !
  !  constructs the global table NCoefsG
  !  NCoefsG(N,rmax) gives the total number of Lorentz-invariant
  !  N-point coefficients of rank r<=rmax+nG
  !  where nG is the number of metric tensors
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetNCoefsG(N,rmax)

    integer, intent(in) :: N,rmax
    integer :: i,r,n0

    if (allocated(NCoefsG)) then
      deallocate(NCoefsG)
    end if
    allocate(NCoefsG(0:rmax,1:N))

    do r=0,rmax
      NCoefsG(r,1) = r/2+1
      do i=2,N
        NCoefsG(r,i) = CalcNCoefsG(i-1,r)
      end do
    end do

  end subroutine SetNCoefsG





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcNCoefsG(Nm1,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcNCoefsG(Nm1,rmax) result(nc)

    integer, intent(in) :: Nm1,rmax
    integer :: r, n0, nc, bino, i

    nc = 1
    do i=1,Nm1+1
      nc = nc * (rmax+i)
    end do
    do i=1,Nm1+1
      nc = nc / i
    end do

  end function CalcNCoefsG


end module Combinatorics
