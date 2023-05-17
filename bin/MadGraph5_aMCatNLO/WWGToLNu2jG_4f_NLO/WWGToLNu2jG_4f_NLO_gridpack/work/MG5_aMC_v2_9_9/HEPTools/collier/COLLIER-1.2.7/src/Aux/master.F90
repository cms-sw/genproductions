!!
!!  File master.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *     module master    *
!  *     by Lars Hofer    *
!  ************************ 
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module master

  use Combinatorics

  implicit none

  integer :: masterN_cll, masterR_cll
  double complex, allocatable :: masterArgs_cll(:)
  character(len=250) :: masterFName_cll

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMasterFname_cll(fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMasterFname_cll(fname)

    character(len=*), intent(in) :: fname

    masterFname_cll = fname

  end subroutine SetMasterFname_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMasterN_cll(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMasterN_cll(N)

    integer, intent(in) :: N

    masterN_cll = N

  end subroutine SetMasterN_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMasterR_cll(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMasterR_cll(rmax)

    integer, intent(in) :: rmax

    masterR_cll = rmax

  end subroutine SetMasterR_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMasterArgs_cll(k,args)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMasterArgs_cll(k,args)

    integer, intent(in) :: k
    double complex, intent(in) :: args(k)

    if (allocated(masterArgs_cll)) then
      deallocate(masterArgs_cll)
    end if
    allocate(masterArgs_cll(k))
    masterArgs_cll = args

  end subroutine SetMasterArgs_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getMasterID_cll(N,args,fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getMasterID_cll(N,args,fname)

    integer, intent(out) :: N
    double complex, allocatable, intent(out) :: args(:)
    character(len=250), intent(out) :: fname

    N = masterN_cll

    allocate(args(BinomTable(2,N)+N))
    args = masterArgs_cll
    fname = masterFname_cll

  end subroutine getMasterID_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getMasterFName_cll(fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getMasterFname_cll(fname)

    character(len=250), intent(out) :: fname

    fname = masterFname_cll

  end subroutine getMasterFname_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getMasterN_cll(N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getMasterN_cll(N)

    integer, intent(out) :: N

    N = masterN_cll

  end subroutine getMasterN_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getMasterID_cll(N,args,fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getMasterArgs_cll(k,args)

    integer, intent(in) :: k
    double complex, intent(out) :: args(k)

    args = masterArgs_cll

  end subroutine getMasterArgs_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getMasterR_cll(rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getMasterR_cll(rmax)

    integer, intent(out) :: rmax

    rmax = masterR_cll

  end subroutine getMasterR_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine WriteMaster_cll(nout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine WriteMaster_cll(nout)

    integer, intent(in) :: nout
    integer :: i,j,N

    character(len=*),parameter :: fmtr = "(A8,i2)"
    character(len=*),parameter :: fmt7 = "(A7,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt9 = "(A9,i2,A6,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt11 = "(A9,i2,A6,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt12 = "(A8,i1,A2,i2,A4,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt26 = "(A26,i2)"
    character(len=*),parameter :: fmt29 = "(A29,i2)"

    write(nout,*) '-----------------------------------------------------------'
    select case(trim(masterFname_cll))
      case ('A_cll')
        write(nout,*) 'master call:  A_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'm02 = ', masterArgs_cll(1)
      case ('B_cll')
        write(nout,*) 'master call:  B_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case ('C_cll')
        write(nout,*) 'MASTER CALL:  C_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(3)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(4)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(5)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(6)
      case ('D_cll')
        write(nout,*) 'MASTER CALL: D_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(6)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(7)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(8)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(9)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(10)
      case('E_cll')
        write(nout,*) 'master call: E_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(10)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(11)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(12)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(13)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(14)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(15)
      case ('F_cll')
        write(nout,*) 'master call: F_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p54 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p50 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p53 = ', masterArgs_cll(10)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(11)
        write(nout,fmt7) 'p51 = ', masterArgs_cll(12)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(13)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(14)
        write(nout,fmt7) 'p52 = ', masterArgs_cll(15)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(16)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(17)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(18)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(19)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(20)
        write(nout,fmt7) 'm52 = ', masterArgs_cll(21)
      case ('G_cll')
        write(nout,*) 'master call: G_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p54 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p65 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p60 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(10)
        write(nout,fmt7) 'p53 = ', masterArgs_cll(11)
        write(nout,fmt7) 'p64 = ', masterArgs_cll(12)
        write(nout,fmt7) 'p50 = ', masterArgs_cll(13)
        write(nout,fmt7) 'p61 = ', masterArgs_cll(14)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(15)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(16)
        write(nout,fmt7) 'p52 = ', masterArgs_cll(17)
        write(nout,fmt7) 'p63 = ', masterArgs_cll(18)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(19)
        write(nout,fmt7) 'p51 = ', masterArgs_cll(20)
        write(nout,fmt7) 'p62 = ', masterArgs_cll(21)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(22)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(23)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(24)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(25)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(26)
        write(nout,fmt7) 'm52 = ', masterArgs_cll(27)
        write(nout,fmt7) 'm62 = ', masterArgs_cll(28)
      case ('TN_cll')
        write(nout,fmt26) 'master call: TN_cll, N = ', masterN_cll
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,BinomTable(2,masterN_cll)
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(i)
        end do
        do i=0,masterN_cll-1
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(BinomTable(2,masterN_cll)+i+1)
        end do
      case ('Aten_cll')
        write(nout,*) 'master call:  Aten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'm02 = ', masterArgs_cll(1)
      case ('Bten_cll')
        write(nout,*) 'master call:  Bten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,1
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,1
          write(nout,fmt11) 'MomInv( ',i,')     = ',masterArgs_cll(4+i)
        end do
        do i=0,1
          write(nout,fmt9) 'masses2(',i,')   = ',masterArgs_cll(6+i)
        end do
      case ('Cten_cll')
        write(nout,*) 'MASTER CALL:  Cten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,2
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,3
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(8+i)
        end do
        do i=0,2
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(12+i)
        end do
      case ('Dten_cll')
        write(nout,*) 'MASTER CALL: Dten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,3
          do j=0,3
            write(nout,fmt12) 'Momvec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,6
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(12+i)
        end do
        do i=0,3
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(19+i)
        end do
      case('Eten_cll')
        write(nout,*) 'master call: Eten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,4
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,10
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(16+i)
        end do
        do i=0,4
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(27+i)
        end do
      case ('Ften_cll')
        write(nout,*) 'master call: Ften_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,5
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,15
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(20+i)
        end do
        do i=0,5
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(36+i)
        end do
      case ('Gten_cll')
        write(nout,*) 'master call: Gten_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        do i=1,6
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,21
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(24+i)
        end do
        do i=0,6
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(46+i)
        end do
      case ('TNten_cll')
        write(nout,fmt29) 'master call: TNten_cll, N = ', masterN_cll
        write(nout,fmtr) 'rmax = ', masterR_cll
        N=masterN_cll
        do i=1,N-1
          do j=0,3
            write(nout,fmt12) 'MomVec(',j,',',i,') = ',masterArgs_cll(4*i-3+j)
          end do
        end do
        do i=1,BinomTable(2,N)
          write(nout,fmt11) 'MomInv( ',i,') = ',masterArgs_cll(4*(N-1)+i)
        end do
        do i=0,N-1
          write(nout,fmt9) 'masses2(',i,') = ',masterArgs_cll(4*(N-1)+BinomTable(2,N)+i+1)
        end do
      case ('A0_cll')
        write(nout,*) 'master call:  A0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'm02 = ', masterArgs_cll(1)
      case ('B0_cll')
        write(nout,*) 'master call:  B0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case ('C0_cll')
        write(nout,*) 'MASTER CALL:  C0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(3)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(4)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(5)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(6)
      case ('D0_cll')
        write(nout,*) 'MASTER CALL: D0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(6)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(7)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(8)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(9)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(10)
      case('E0_cll')
        write(nout,*) 'master call: E0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(10)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(11)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(12)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(13)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(14)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(15)
      case ('F0_cll')
        write(nout,*) 'master call: F0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p54 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p50 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p53 = ', masterArgs_cll(10)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(11)
        write(nout,fmt7) 'p51 = ', masterArgs_cll(12)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(13)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(14)
        write(nout,fmt7) 'p52 = ', masterArgs_cll(15)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(16)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(17)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(18)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(19)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(20)
        write(nout,fmt7) 'm52 = ', masterArgs_cll(21)
      case ('G0_cll')
        write(nout,*) 'master call: G0_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'p21 = ', masterArgs_cll(2)
        write(nout,fmt7) 'p32 = ', masterArgs_cll(3)
        write(nout,fmt7) 'p43 = ', masterArgs_cll(4)
        write(nout,fmt7) 'p54 = ', masterArgs_cll(5)
        write(nout,fmt7) 'p65 = ', masterArgs_cll(6)
        write(nout,fmt7) 'p60 = ', masterArgs_cll(7)
        write(nout,fmt7) 'p20 = ', masterArgs_cll(8)
        write(nout,fmt7) 'p31 = ', masterArgs_cll(9)
        write(nout,fmt7) 'p42 = ', masterArgs_cll(10)
        write(nout,fmt7) 'p53 = ', masterArgs_cll(11)
        write(nout,fmt7) 'p64 = ', masterArgs_cll(12)
        write(nout,fmt7) 'p50 = ', masterArgs_cll(13)
        write(nout,fmt7) 'p61 = ', masterArgs_cll(14)
        write(nout,fmt7) 'p30 = ', masterArgs_cll(15)
        write(nout,fmt7) 'p41 = ', masterArgs_cll(16)
        write(nout,fmt7) 'p52 = ', masterArgs_cll(17)
        write(nout,fmt7) 'p63 = ', masterArgs_cll(18)
        write(nout,fmt7) 'p40 = ', masterArgs_cll(19)
        write(nout,fmt7) 'p51 = ', masterArgs_cll(20)
        write(nout,fmt7) 'p62 = ', masterArgs_cll(21)
        write(nout,fmt7) 'm02  =', masterArgs_cll(22)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(23)
        write(nout,fmt7) 'm22 = ', masterArgs_cll(24)
        write(nout,fmt7) 'm32 = ', masterArgs_cll(25)
        write(nout,fmt7) 'm42 = ', masterArgs_cll(26)
        write(nout,fmt7) 'm52 = ', masterArgs_cll(27)
        write(nout,fmt7) 'm62 = ', masterArgs_cll(28)
      case ('DB0_cll')
        write(nout,*) 'master call:  DB0_cll'
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case ('DB1_cll')
        write(nout,*) 'master call:  DB1_cll'
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case ('DB00_cll')
        write(nout,*) 'master call:  DB00_cll'
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case ('DB_cll')
        write(nout,*) 'master call:  DB_cll'
        write(nout,fmtr) 'rmax = ', masterR_cll
        write(nout,fmt7) 'p10 = ', masterArgs_cll(1)
        write(nout,fmt7) 'm02 = ', masterArgs_cll(2)
        write(nout,fmt7) 'm12 = ', masterArgs_cll(3)
      case default
        write(nout,*) 'master call: ',masterFname_cll
    end select
    write(nout,*) '-----------------------------------------------------------'
!    write(nout,*) 'GLOBAL PARAMETERS:'
!    write(nout,*) 'mode        ', mode_cll
!    write(nout,*) 'muUV        ', muUV_cll
!    write(nout,*) 'muIR        ', muIR_cll
!    write(nout,*) 'deltaUV     ', deltaUV_cll
!    write(nout,*) 'deltaIR1    ', deltaIR1_cll
!    write(nout,*) 'deltaIR2    ', deltaIR2_cll
!    write(nout,*) 'nminf       ', nminf_cll
!    do i=1,nminf_cll
!      write(nout,*) 'minf2       ', i, minf2_cll(i)
!    end do
!    write(nout,*) 'dprec       ', dprec_cll
!    write(nout,*) 'reqacc      ', reqacc_cll
!    write(nout,*) 'critacc     ', critacc_cll
!    write(nout,*) 'checkacc    ', checkacc_cll
!    write(nout,*) 'ErrFlag     ', ErrFlag_cll
!    write(nout,*) '------------------------------------------------------------'

  end subroutine


end module master
