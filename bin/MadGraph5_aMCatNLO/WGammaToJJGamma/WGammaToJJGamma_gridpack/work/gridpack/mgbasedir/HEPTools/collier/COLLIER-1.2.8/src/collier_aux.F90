!!
!!  File collier_aux.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ******************************************
!  *  module collier_aux                    *
!  *     by Lars Hofer and Ansgar Denner    *
!  ****************************************** 
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module collier_aux

  use combinatorics
  use master
  use collier_global
  use coli_aux2
  use coli_stat

  implicit none


contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsA_cll(A,A2,m02,rmax,norm0,Adiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsA_cll(A,A2,m02,rmax,norm0,Adiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: m02
    double complex, intent(in) :: A(0:rmax/2),A2(0:rmax/2)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Adiff(0:rmax)
    double complex :: diffA
    double precision :: norm,ratio
    integer :: n0,i,flag
!    integer, parameter :: noutCheckAmax=50
!    integer, save :: DiffCntA

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' A(',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntA /0/

    CheckCnt_cll(1) = CheckCnt_cll(1) + 1

    flag=1
    if(DiffCnt_cll(1).ge.MaxCheck_cll(1)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0
    ratio=0d0

    Adiff=0d0
    do n0=0,rmax/2
!      norm = min(abs(A(n0)),abs(A2(n0)))
      if (m02.ne.0d0) then
        norm = norm0*abs(m02)**(n0)
      else
        norm = norm0**(n0+1)
      endif
      diffA = A(n0)-A2(n0)
      Adiff(2*n0) = max(Adiff(2*n0),abs(diffA))
      if ((abs(diffA).gt.checkacc_cll*norm).and.(flag.eq.1)) then
        write(ncheckout_cll,*) '*************************************************************************'
        write(ncheckout_cll,*) 'A difference NO.', DiffCnt_cll(1)+1
        write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
        write(ncheckout_cll,'(A21,I2,A4,I2)') 'A integral with rank', 2*n0,' of ',rmax
        write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
        write(ncheckout_cll,*) '------------------------------------------------------------'
        write(ncheckout_cll,fmt1) 'm02=', m02
        write(ncheckout_cll,*) '-------------------------------------------------------------------------'
        write(ncheckout_cll,fmt2) 'COLI:',0,A(0)
        write(ncheckout_cll,fmt2) 'DD  :',0,A2(0)
        write(ncheckout_cll,fmt2) 'COLI:',n0,A(n0)
        write(ncheckout_cll,fmt2) 'DD  :',n0,A2(n0)
        write(ncheckout_cll,*) 'diff:', abs(diffA)/norm
        flag=2
        ratio=abs(diffA)/norm
      elseif((flag.eq.2).and.(abs(diffA).gt.ratio*norm)) then
        write(ncheckout_cll,fmt2) 'COLI:',n0,A(n0)
        write(ncheckout_cll,fmt2) 'DD  :',n0,A2(n0)
        write(ncheckout_cll,*) 'diff:', abs(diffA)/norm
        ratio=abs(diffA)/norm
        write(ncheckout_cll,*) 'COLI:', A(n0)
      elseif ((abs(diffA).gt.checkacc_cll*norm).and.(flag.eq.0)) then
        flag=3
      end if
    end do
    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*)
      write(ncheckout_cll,*)
      DiffCnt_cll(1) =  DiffCnt_cll(1) + 1
      if(DiffCnt_cll(1).eq.MaxCheck_cll(1)) then
        write(ncheckout_cll,*) ' Further output for differences in A functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(1) =  DiffCnt_cll(1) + 1
    end if
    
    
  end subroutine CheckCoefsA_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsB_cll(B,B2,p10,m02,m12,rmax,norm0,Bdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsB_cll(B,B2,p10,m02,m12,rmax,norm0,Bdiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(in) :: B(0:rmax/2,0:rmax),B2(0:rmax/2,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Bdiff(0:rmax)
    double complex :: diffB
    double precision :: norm,ratio
    integer :: r,n0,n1,i,flag
!    integer, parameter :: noutCheckBmax=50
!    integer, save :: DiffCntB

    character(len=*),parameter :: fmt1 = "(A6,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' B(',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntB /0/

    CheckCnt_cll(2) = CheckCnt_cll(2) + 1

    flag=1
    if(DiffCnt_cll(2).ge.MaxCheck_cll(2)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0   
    ratio=0d0

    Bdiff=0d0
    do r=0,rmax
      do n0=0,r/2
        n1 = r-2*n0
!        norm = min(abs(B(n0,n1)),abs(B2(n0,n1)))
        if (max(abs(p10),abs(m02),abs(m12)).ne.0d0) then
          norm = norm0*max(abs(p10),abs(m02),abs(m12))**n0
        else
          norm = norm0*muuv2_cll**n0
        end if
        diffB = B(n0,n1)-B2(n0,n1)
        if (n0.eq.0) Bdiff(r) = max(Bdiff(r),abs(diffB))
        if ((abs(diffB).gt.checkacc_cll*norm).and.(flag.eq.1)) then
          write(ncheckout_cll,*) '*************************************************************************'
          write(ncheckout_cll,*) 'B difference NO.', DiffCnt_cll(2)+1
          write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
          write(ncheckout_cll,'(A21,I2,A4,I2)') 'B integral with rank', r,' of ',rmax
          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
          write(ncheckout_cll,*) '------------------------------------------------------------'
!          write(ncheckout_cll,*) 'n0', n0
!          write(ncheckout_cll,*) 'n1', n1
!          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
          write(ncheckout_cll,fmt1) 'p10=', p10
          write(ncheckout_cll,fmt1) 'm02=', m02
          write(ncheckout_cll,fmt1) 'm12=', m12
          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!                write(ncheckout_cll,*) 'C0_coli:', B(0,0)
!                write(ncheckout_cll,*) 'C0_DD  :', B2(0,0)
          write(ncheckout_cll,fmt2) 'COLI:',0,0,B(0,0)
          write(ncheckout_cll,fmt2) 'DD  :',0,0,B2(0,0)
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,B(n0,n1)
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,B2(n0,n1)
!                write(ncheckout_cll,*) 'COLI:', B(n0,n1)
!                write(ncheckout_cll,*) 'DD  :', B2(n0,n1)
          write(ncheckout_cll,*) 'diff:', abs(diffB)/norm
          flag=2
          ratio=abs(diffB)/norm
        elseif((flag.eq.2).and.(abs(diffB).gt.ratio*norm)) then
           write(ncheckout_cll,fmt2) 'COLI:',n0,n1,B(n0,n1)
           write(ncheckout_cll,fmt2) 'DD  :',n0,n1,B2(n0,n1)
           write(ncheckout_cll,*) 'diff:', abs(diffB)/norm
           ratio=abs(diffB)/norm
        elseif ((abs(diffB).gt.checkacc_cll*norm).and.(flag.eq.0)) then
           flag=3
        end if
      end do
    end do
    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end B'
      write(ncheckout_cll,*)
      DiffCnt_cll(2) =  DiffCnt_cll(2) + 1
      if(DiffCnt_cll(2).eq.MaxCheck_cll(2)) then
        write(ncheckout_cll,*) ' Further output for differences in B functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(2) =  DiffCnt_cll(2) + 1
    end if


  end subroutine CheckCoefsB_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsC_cll(C,C2,p10,p21,p20,m02,m12,m22,rmax,norm0,Cdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsC_cll(C,C2,p10,p21,p20,m02,m12,m22,rmax,norm0,Cdiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double complex, intent(in) :: C(0:rmax/2,0:rmax,0:rmax),C2(0:rmax/2,0:rmax,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Cdiff(0:rmax)
    double complex :: diffC
    double precision :: norm,ratio
    integer :: r,n0,n1,n2,i,flag
!    integer, parameter :: noutCheckCmax=50
!    integer, save :: DiffCntC

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' C(',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntC /0/

    CheckCnt_cll(3) = CheckCnt_cll(3) + 1

    flag=1
    if(DiffCnt_cll(3).ge.MaxCheck_cll(3)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0
    ratio=0d0

    Cdiff = 0d0
    do r=0,rmax
      do n0=0,r/2
        do n1=0,r-2*n0
          n2 = r-2*n0-n1
!          norm = min(abs(C(n0,n1,n2)),abs(C2(n0,n1,n2)))
          norm = norm0/norm0**n0
          diffC = C(n0,n1,n2)-C2(n0,n1,n2)
          if (n0.eq.0) Cdiff(r) = max(Cdiff(r),abs(diffC))
          if ((abs(diffc).gt.checkacc_cll*norm).and.(flag.eq.1)) then
            write(ncheckout_cll,*) '*************************************************************************'
            write(ncheckout_cll,*) 'C difference NO.', DiffCnt_cll(3)+1
            write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
            write(ncheckout_cll,'(A21,I2,A4,I2)') 'C integral with rank', r,' of ',rmax
!            write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!            write(ncheckout_cll,*) 'n0', n0
!            write(ncheckout_cll,*) 'n1', n1
!            write(ncheckout_cll,*) 'n2', n2
            write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
            write(ncheckout_cll,*) '------------------------------------------------------------'
            write(ncheckout_cll,fmt1) 'p10=', p10
            write(ncheckout_cll,fmt1) 'p21=', p21
            write(ncheckout_cll,fmt1) 'p20=', p20
            write(ncheckout_cll,fmt1) 'm02=', m02
            write(ncheckout_cll,fmt1) 'm12=', m12
            write(ncheckout_cll,fmt1) 'm22=', m22
            write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!              write(ncheckout_cll,*) 'C0_coli:', C(0,0,0)
!              write(ncheckout_cll,*) 'C0_DD  :', C2(0,0,0)
            write(ncheckout_cll,fmt2) 'COLI:',0,0,0,C(0,0,0)
            write(ncheckout_cll,fmt2) 'DD  :',0,0,0,C2(0,0,0)
            write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,C(n0,n1,n2)
            write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,C2(n0,n1,n2)
!              write(ncheckout_cll,*) 'COLI:', C(n0,n1,n2)
!              write(ncheckout_cll,*) 'DD  :', C2(n0,n1,n2)
            write(ncheckout_cll,*) 'diff:', abs(diffC)/norm
            flag=2
            ratio=abs(diffC)/norm
          elseif((flag.eq.2).and.(abs(diffC).gt.ratio*norm)) then
            write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,C(n0,n1,n2)
            write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,C2(n0,n1,n2)
            write(ncheckout_cll,*) 'diff:', abs(diffC)/norm
            ratio=abs(diffC)/norm
          elseif ((abs(diffC).gt.checkacc_cll*norm).and.(flag.eq.0)) then
            flag=3
          end if
        end do
      end do
    end do
    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end C'
      write(ncheckout_cll,*)
      DiffCnt_cll(3) =  DiffCnt_cll(3) + 1
      if(DiffCnt_cll(3).eq.MaxCheck_cll(3)) then
        write(ncheckout_cll,*) ' Further output for differences in C functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(3) =  DiffCnt_cll(3) + 1
    endif


  end subroutine CheckCoefsC_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsD_cll(D,D2,p10,p21,p32,p30,p20,p31,  &
  !                 m02,m12,m22,m32,rmax,norm0,Ddiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsD_cll(D,D2,p10,p21,p32,p30,p20,p31,  &
                   m02,m12,m22,m32,rmax,norm0,Ddiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(in) :: D(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: D2(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Ddiff(0:rmax)
    double complex :: diffD
    double precision :: norm,ratio
    integer :: r,n0,n1,n2,n3,i,flag
!    integer, parameter :: noutCheckDmax=50
!    integer, save :: DiffCntD

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' D(',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntD /0/

    CheckCnt_cll(4) = CheckCnt_cll(4) + 1

    flag=1
    if(DiffCnt_cll(4).ge.MaxCheck_cll(4)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    
    ratio=0d0

    Ddiff = 0d0
    do r=0,rmax
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2
!            norm = min(abs(D(n0,n1,n2,n3)),abs(D2(n0,n1,n2,n3)))
            norm = norm0/sqrt(norm0)**n0
            diffD = D(n0,n1,n2,n3)-D2(n0,n1,n2,n3)
            if (n0.eq.0) Ddiff(r) = max(Ddiff(r),abs(diffD))
            if ((abs(diffD).gt.checkacc_cll*norm).and.(flag.eq.1)) then
              write(ncheckout_cll,*) '*************************************************************************'
              write(ncheckout_cll,*) 'D difference NO.', DiffCnt_cll(4)+1
              write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
              write(ncheckout_cll,'(A21,I2,A4,I2)') 'D integral with rank', r,' of ',rmax
!              write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!              write(ncheckout_cll,*) 'n0', n0
!              write(ncheckout_cll,*) 'n1', n1
!              write(ncheckout_cll,*) 'n2', n2
!              write(ncheckout_cll,*) 'n3', n3
              write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
              write(ncheckout_cll,*) '------------------------------------------------------------'
              write(ncheckout_cll,fmt1) 'p10=', p10
              write(ncheckout_cll,fmt1) 'p21=', p21
              write(ncheckout_cll,fmt1) 'p32=', p32
              write(ncheckout_cll,fmt1) 'p30=', p30
              write(ncheckout_cll,fmt1) 'p20=', p20
              write(ncheckout_cll,fmt1) 'p31=', p31
              write(ncheckout_cll,fmt1) 'm02=', m02
              write(ncheckout_cll,fmt1) 'm12=', m12
              write(ncheckout_cll,fmt1) 'm22=', m22
              write(ncheckout_cll,fmt1) 'm32=', m32
              write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!                write(ncheckout_cll,*) 'D0_coli:', D(0,0,0,0)
!                write(ncheckout_cll,*) 'D0_DD  :', D2(0,0,0,0)
              write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,D(0,0,0,0)
              write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,D2(0,0,0,0)
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,D(n0,n1,n2,n3)
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,D2(n0,n1,n2,n3)
!                write(ncheckout_cll,*) 'COLI:', D(n0,n1,n2,n3)
!                write(ncheckout_cll,*) 'DD  :', D2(n0,n1,n2,n3)
              if(norm.ne.0d0)then
                write(ncheckout_cll,*) 'diff:', abs(diffD)/norm
                ratio=abs(diffD)/norm
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
              flag=2
            elseif((flag.eq.2).and.(abs(diffD).gt.ratio*norm)) then
              write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,D(n0,n1,n2,n3)
              write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,D2(n0,n1,n2,n3)
              if(norm.gt.1d-100)then
                write(ncheckout_cll,*) 'diff:', abs(diffD)/norm
                ratio=abs(diffD)/norm
              else
                write(ncheckout_cll,*) 'diff:', 1d50
                ratio=1d50
              endif
            elseif ((abs(diffD).gt.checkacc_cll*norm).and.(flag.eq.0)) then
              flag=3
            end if
          end do
        end do
      end do
    end do
    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end D '
      write(ncheckout_cll,*)
      DiffCnt_cll(4) =  DiffCnt_cll(4) + 1
      if(DiffCnt_cll(4).eq.MaxCheck_cll(4)) then
        write(ncheckout_cll,*) ' Further output for differences in D functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(4) =  DiffCnt_cll(4) + 1
    endif


  end subroutine CheckCoefsD_cll







  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsE_cll(E,E2,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
  !                 m02,m12,m22,m32,m42,rmax,norm0,Ediff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsE_cll(E,E2,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                   m02,m12,m22,m32,m42,rmax,norm0,Ediff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(in) :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: E2(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Ediff(0:rmax)
    double complex :: Gram(4,4),diffE
    double complex :: Ep(BinomTable(rmax,rmax+3)),Ep2(BinomTable(rmax,rmax+3)),diffEp(BinomTable(rmax,rmax+3))
    double precision :: norm,ratio
!    double precision :: norm,ratio,Zmax
    integer :: r,n0,n1,n2,n3,n4,m0,m1,m2,m3,m4,i,flag,flagEc
    integer :: struc1(BinomTable(rmax,rmax+3),0:4), struc2(0:4)
!    integer, parameter :: noutCheckEmax=50
!    integer, save :: DiffCntE

    double complex :: LoCons(0:rmax/2,BinomTable(rmax,rmax+3),0:rmax/2,BinomTable(rmax,rmax+3),0:rmax)
    integer :: i1,i2

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',d25.18,',',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' E(',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"
    character(len=*),parameter :: fmt3 = &
    "(A6,' E*T(',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"
    character(len=*),parameter :: fmt4 = &
    "(A6,'   E(',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntE /0/

    CheckCnt_cll(5) = CheckCnt_cll(5) + 1

    flagEc=1
    if(DiffCntEc_cll.ge.MaxCheckEc_cll) flagEc=0
    if(ncheckout_cll.eq.closed_cll) flagEc=0    
    ratio=0d0

    if(.false.) then               !   E-tensor coefficients are not unique!
    Ediff=0d0
    do r=0,rmax
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              n4 = r-2*n0-n1-n2-n3
!              norm = min(abs(E(n0,n1,n2,n3,n4)),abs(E2(n0,n1,n2,n3,n4)))
              norm = norm0/norm0**(n0/3d0)
              diffE = E(n0,n1,n2,n3,n4)-E2(n0,n1,n2,n3,n4)
              if (n0.eq.0) Ediff(r)=max(Ediff(r),abs(diffE))
              if ((abs(diffE).gt.checkacc_cll*norm).and.(flagEc.eq.1)) then
                write(ncheckout_cll,*) '*************************************************************************'
                write(ncheckout_cll,*) 'E difference NO.', DiffCntEc_cll+1
                write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
                write(ncheckout_cll,'(A21,I2,A4,I2)') 'E integral with rank', r,' of ',rmax
!                write(ncheckout_cll,*) '------------------------------------------------------------------------'
!                write(ncheckout_cll,*) 'n0', n0
!                write(ncheckout_cll,*) 'n1', n1
!                write(ncheckout_cll,*) 'n2', n2
!                write(ncheckout_cll,*) 'n3', n3
!                write(ncheckout_cll,*) 'n4', n4
                write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
                write(ncheckout_cll,*) '------------------------------------------------------------'
                write(ncheckout_cll,fmt1) 'p10=', p10
                write(ncheckout_cll,fmt1) 'p21=', p21
                write(ncheckout_cll,fmt1) 'p32=', p32
                write(ncheckout_cll,fmt1) 'p43=', p43
                write(ncheckout_cll,fmt1) 'p40=', p40
                write(ncheckout_cll,fmt1) 'p20=', p20
                write(ncheckout_cll,fmt1) 'p31=', p31
                write(ncheckout_cll,fmt1) 'p42=', p42
                write(ncheckout_cll,fmt1) 'p30=', p30
                write(ncheckout_cll,fmt1) 'p41=', p41
                write(ncheckout_cll,fmt1) 'm02=', m02
                write(ncheckout_cll,fmt1) 'm12=', m12
                write(ncheckout_cll,fmt1) 'm22=', m22
                write(ncheckout_cll,fmt1) 'm32=', m32
                write(ncheckout_cll,fmt1) 'm42=', m42
                write(ncheckout_cll,*) '-------------------------------------------------------------------------'
                write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,0,E(0,0,0,0,0)
                write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,0,E2(0,0,0,0,0)
                write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,n4,E(n0,n1,n2,n3,n4)
                write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,n4,E2(n0,n1,n2,n3,n4)
                write(ncheckout_cll,*) 'diff:', abs(diffE)/norm
                flagEc=2
                ratio=abs(diffE)/norm
              elseif((flagEc.eq.2).and.(abs(diffE).gt.ratio*norm)) then
                write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,n4,E(n0,n1,n2,n3,n4)
                write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,n4,E2(n0,n1,n2,n3,n4)
                write(ncheckout_cll,*) 'diff:', abs(diffE)/norm
                ratio=abs(diffE)/norm
              elseif ((abs(diffE).gt.checkacc_cll*norm).and.(flagEc.eq.0)) then
                flagEc=3
              end if
            end do
          end do
        end do
      end do
    end do
    if((flagEc.eq.2))then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end Ec'
      write(ncheckout_cll,*)
      DiffCntEc_cll =  DiffCntEc_cll + 1
      if(DiffCntEc_cll.eq.MaxCheckEc_cll) then
        write(ncheckout_cll,*) ' Further output for differences Ec in E functions suppressed '
        write(ncheckout_cll,*)
      endif
    elseif(flagEc.eq.3)then
      DiffCntEc_cll =  DiffCntEc_cll + 1
    endif

    end if

    flag=1
    if(DiffCnt_cll(5).ge.MaxCheck_cll(5)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    
    ratio=0d0

    Gram(1,1) = p10
    Gram(2,2) = p20
    Gram(3,3) = p30
    Gram(4,4) = p40
    Gram(1,2) = (p10+p20-p21)*.5d0
    Gram(1,3) = (p10+p30-p31)*.5d0
    Gram(1,4) = (p10+p40-p41)*.5d0
    Gram(2,3) = (p20+p30-p32)*.5d0
    Gram(2,4) = (p20+p40-p42)*.5d0
    Gram(3,4) = (p30+p40-p43)*.5d0
    Gram(2,1) = Gram(1,2)
    Gram(3,1) = Gram(1,3)
    Gram(4,1) = Gram(1,4)
    Gram(3,2) = Gram(2,3)
    Gram(4,2) = Gram(2,4)
    Gram(4,3) = Gram(3,4)
    !Zmax = maxval(abs(Gram(1:4,1:4)))

    LoCons = LoStrucConts(4,rmax,Gram)

    Ediff=0d0
    do r=0,rmax
      do m0=0,r/2
        struc1(:,0)=m0
        norm = norm0/norm0**((r-m0)/3d0)
        ! norm = Zmax**(r-m0)*norm0*max(1d0,1d0/(norm0**(1d0/3d0)*Zmax))**(r/2)   to large !!
        ! norm = Zmax**(r-m0)*norm0*(1d0/(norm0**(1d0/3d0)*Zmax))**(r/2)
        do i1=1,BinomTable(r-2*m0,3+r-2*m0)
          struc1(i1,1:4)=CalcCIndArr(4,r-2*m0,i1)
          Ep(i1) =0d0 
          Ep2(i1)=0d0 
          do n0=0,r/2
            struc2(0)=n0
            do i2=1,BinomTable(r-2*n0,3+r-2*n0)
              struc2(1:4)=CalcCIndArr(4,r-2*n0,i2)
              Ep(i1) = Ep(i1) &
                  + E(n0,struc2(1),struc2(2),struc2(3),struc2(4))* &
                    LoCons(m0,i1,n0,i2,r)
              Ep2(i1) = Ep2(i1) &
                   + E2(n0,struc2(1),struc2(2),struc2(3),struc2(4))* &
                     LoCons(m0,i1,n0,i2,r)
            enddo
          enddo

          norm = max(norm,min(abs(Ep(i1)),abs(Ep2(i1))))
          diffEp(i1) = Ep(i1)-Ep2(i1)
        enddo 

        do i1=1,BinomTable(r-2*m0,3+r-2*m0)
          Ediff(r) = max(Ediff(r),abs(diffEp(i1))/norm)

          if ((abs(diffEp(i1)).gt.checkacc_cll*norm).and.(flag.eq.1)) then
             write(ncheckout_cll,*) '*************************************************************************'
             write(ncheckout_cll,*) 'E difference NO.', DiffCnt_cll(5)+1
             write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
             write(ncheckout_cll,'(A21,I2,A4,I2)') 'E integral with rank', r,' of ',rmax
             write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
             write(ncheckout_cll,*) '------------------------------------------------------------'
             write(ncheckout_cll,fmt1) 'p10=', p10
             write(ncheckout_cll,fmt1) 'p21=', p21
             write(ncheckout_cll,fmt1) 'p32=', p32
             write(ncheckout_cll,fmt1) 'p43=', p43
             write(ncheckout_cll,fmt1) 'p40=', p40
             write(ncheckout_cll,fmt1) 'p20=', p20
             write(ncheckout_cll,fmt1) 'p31=', p31
             write(ncheckout_cll,fmt1) 'p42=', p42
             write(ncheckout_cll,fmt1) 'p30=', p30
             write(ncheckout_cll,fmt1) 'p41=', p41
             write(ncheckout_cll,fmt1) 'm02=', m02
             write(ncheckout_cll,fmt1) 'm12=', m12
             write(ncheckout_cll,fmt1) 'm22=', m22
             write(ncheckout_cll,fmt1) 'm32=', m32
             write(ncheckout_cll,fmt1) 'm42=', m42
             write(ncheckout_cll,*) '--------------------------------------------------------------------------'
             write(ncheckout_cll,fmt4) 'COLI:',0,0,0,0,0,E(0,0,0,0,0)
             write(ncheckout_cll,fmt4) 'DD  :',0,0,0,0,0,E2(0,0,0,0,0)
             write(ncheckout_cll,fmt3) 'COLI:',struc1(i1,0:4),Ep(i1)
             write(ncheckout_cll,fmt3) 'DD  :',struc1(i1,0:4),Ep2(i1)
             write(ncheckout_cll,*) 'diff :', abs(diffEp(i1))/norm
!             write(ncheckout_cll,*) 'adiff:', abs(diffEp(i1))
!             write(ncheckout_cll,*) 'norm :', norm
!            write(ncheckout_cll,*) 'normo:', norm0/norm0**((r-m0)/3d0)
!            write(ncheckout_cll,*) 'normn:', Zmax**(r-m0)*norm0*(1d0/(norm0**(1d0/3d0)*Zmax))**(r/2)
!            write(ncheckout_cll,*) 'norml:', Zmax**(r-m0)*norm0*max(1d0,(1d0/(norm0**(1d0/3d0)*Zmax))**(r/2))
!             write(ncheckout_cll,*) 'norm0:', norm0
!            write(ncheckout_cll,*) 'Zmax:', Zmax
             flag=2
             ratio=abs(diffEp(i1))/norm
          elseif((flag.eq.2).and.(abs(diffEp(i1)).gt.ratio*norm)) then
!          elseif((flag.eq.2).and.(abs(diffEp(i1)).gt.checkacc_cll*norm)) then
             write(ncheckout_cll,fmt3) 'COLI:',struc1(i1,0:4),Ep(i1)
             write(ncheckout_cll,fmt3) 'DD  :',struc1(i1,0:4),Ep2(i1)
             write(ncheckout_cll,*) 'diff:', abs(diffEp(i1))/norm
             ratio=abs(diffEp(i1))/norm
          elseif ((abs(diffEp(i1)).gt.checkacc_cll*norm).and.(flag.eq.0)) then
             flag=3
          end if
        end do
      end do
    end do
    Ediff=Ediff*norm0

    if((flag.eq.2))then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end E'
      write(ncheckout_cll,*)
      DiffCnt_cll(5) =  DiffCnt_cll(5) + 1
      if(DiffCnt_cll(5).eq.MaxCheck_cll(5)) then
        write(ncheckout_cll,*) ' Further output for differences in E functions suppressed '
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(5) =  DiffCnt_cll(5) + 1
    endif


  end subroutine CheckCoefsE_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsF_cll(F,F2,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                       p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,norm2,Fdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsF_cll(F,F2,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,norm0,Fdiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(in) :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: F2(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: Fdiff(0:rmax)
    double complex :: diffF,Gram(5,5)
    double complex :: Fp(BinomTable(rmax,rmax+4)),Fp2(BinomTable(rmax,rmax+4)),diffFp(BinomTable(rmax,rmax+4))
!    double precision :: norm,ratio,Zmax
    double precision :: norm,ratio
    integer :: r,n0,n1,n2,n3,n4,n5,m0,m1,m2,m3,m4,m5,i,flag,flag2
    integer :: struc1(BinomTable(rmax,rmax+4),0:5), struc2(0:5)
!    integer, parameter :: noutCheckFmax=50
!    integer, save :: DiffCntF

    double complex :: LoCons(0:rmax/2,BinomTable(rmax,rmax+4),0:rmax/2,BinomTable(rmax,rmax+4),0:rmax)
    integer :: i1,i2

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',d25.18,',',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' F(',i1,',',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"
    character(len=*),parameter :: fmt3 = &
    "(A6,' F*T(',i1,',',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"
    character(len=*),parameter :: fmt4 = &
    "(A6,'   F(',i1,',',i1,',',i1,',',i1,',',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntF /0/
    
    CheckCnt_cll(6) = CheckCnt_cll(6) + 1

    flag2=1
    if(DiffCnt_cll(6).ge.MaxCheck_cll(6)) flag2=0    
    if(ncheckout_cll.eq.closed_cll) flag2=0   

    if(.false.)then             ! tensor F coefficients are not unique 
    ratio=0d0

    Fdiff=0d0
    do r=0,rmax
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                n5 = r-2*n0-n1-n2-n3-n4
!                norm = min(abs(F(n0,n1,n2,n3,n4,n5)),abs(F2(n0,n1,n2,n3,n4,n5)))
                norm = norm0/norm0**(n0/4d0)
                diffF = F(n0,n1,n2,n3,n4,n5)-F2(n0,n1,n2,n3,n4,n5)
                if (n0.eq.0) Fdiff(r) = max(Fdiff(r),abs(diffF))
                if ((abs(diffF).gt.checkacc_cll*norm).and.(flag2.eq.1)) then
                  write(ncheckout_cll,*) '*************************************************************************'
                  write(ncheckout_cll,*) 'F difference NO.', DiffCnt_cll(6)+1
                  write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
                  write(ncheckout_cll,'(A21,I2,A4,I2)') 'F integral with rank', r,' of ',rmax
!                  write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!                  write(ncheckout_cll,*) 'n0', n0
!                  write(ncheckout_cll,*) 'n1', n1
!                  write(ncheckout_cll,*) 'n2', n2
!                  write(ncheckout_cll,*) 'n3', n3
!                  write(ncheckout_cll,*) 'n4', n4
!                  write(ncheckout_cll,*) 'n5', n5
                  write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
                  write(ncheckout_cll,*) '------------------------------------------------------------'
                  write(ncheckout_cll,fmt1) 'p10=', p10
                  write(ncheckout_cll,fmt1) 'p21=', p21
                  write(ncheckout_cll,fmt1) 'p32=', p32
                  write(ncheckout_cll,fmt1) 'p43=', p43
                  write(ncheckout_cll,fmt1) 'p54=', p54
                  write(ncheckout_cll,fmt1) 'p50=', p50
                  write(ncheckout_cll,fmt1) 'p20=', p20
                  write(ncheckout_cll,fmt1) 'p31=', p31
                  write(ncheckout_cll,fmt1) 'p42=', p42
                  write(ncheckout_cll,fmt1) 'p53=', p53
                  write(ncheckout_cll,fmt1) 'p40=', p40
                  write(ncheckout_cll,fmt1) 'p51=', p51
                  write(ncheckout_cll,fmt1) 'p30=', p30
                  write(ncheckout_cll,fmt1) 'p41=', p41
                  write(ncheckout_cll,fmt1) 'p52=', p52
                  write(ncheckout_cll,fmt1) 'm02=', m02
                  write(ncheckout_cll,fmt1) 'm12=', m12
                  write(ncheckout_cll,fmt1) 'm22=', m22
                  write(ncheckout_cll,fmt1) 'm32=', m32
                  write(ncheckout_cll,fmt1) 'm42=', m42
                  write(ncheckout_cll,fmt1) 'm52=', m52
                  write(ncheckout_cll,*) '--------------------------------------------------------------------------'
!                write(ncheckout_cll,*) 'F0_coli:', F(0,0,0,0,0,0)
!                write(ncheckout_cll,*) 'F0_DD  :', F2(0,0,0,0,0,0)
                  write(ncheckout_cll,fmt2) 'COLI:',0,0,0,0,0,0,F(0,0,0,0,0,0)
                  write(ncheckout_cll,fmt2) 'DD  :',0,0,0,0,0,0,F2(0,0,0,0,0,0)
                  write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,n4,n5,F(n0,n1,n2,n3,n4,n5)
                  write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,n4,n5,F2(n0,n1,n2,n3,n4,n5)
!                write(ncheckout_cll,*) 'COLI:', F(n0,n1,n2,n3,n4,n5)
!                write(ncheckout_cll,*) 'DD  :', F2(n0,n1,n2,n3,n4,n5)
                  write(ncheckout_cll,*) 'diff:', abs(diffF)/norm
                  flag2=2
                  ratio=abs(diffF)/norm
                elseif((flag2.eq.2).and.(abs(diffF).gt.ratio*norm)) then
                  write(ncheckout_cll,fmt2) 'COLI:',n0,n1,n2,n3,n4,n5,F(n0,n1,n2,n3,n4,n5)
                  write(ncheckout_cll,fmt2) 'DD  :',n0,n1,n2,n3,n4,n5,F2(n0,n1,n2,n3,n4,n5)
                  write(ncheckout_cll,*) 'diff:', abs(diffF)/norm
                  ratio=abs(diffF)/norm
                elseif ((abs(diffF).gt.checkacc_cll*norm).and.(flag.eq.0)) then
                  flag=3
                end if
              end do
            end do
          end do
        end do
      end do
    end do
    end if

    flag=1
    if(DiffCnt_cll(6).ge.MaxCheck_cll(6)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    
    ratio=0d0

    Gram(1,1) = p10
    Gram(2,2) = p20
    Gram(3,3) = p30
    Gram(4,4) = p40
    Gram(5,5) = p50
    Gram(1,2) = (p10+p20-p21)*.5d0
    Gram(1,3) = (p10+p30-p31)*.5d0
    Gram(1,4) = (p10+p40-p41)*.5d0
    Gram(1,5) = (p10+p50-p51)*.5d0
    Gram(2,3) = (p20+p30-p32)*.5d0
    Gram(2,4) = (p20+p40-p42)*.5d0
    Gram(2,5) = (p20+p50-p52)*.5d0
    Gram(3,4) = (p30+p40-p43)*.5d0
    Gram(3,5) = (p30+p50-p53)*.5d0
    Gram(4,5) = (p40+p50-p54)*.5d0
    Gram(2,1) = Gram(1,2)
    Gram(3,1) = Gram(1,3)
    Gram(4,1) = Gram(1,4)
    Gram(5,1) = Gram(1,5)
    Gram(3,2) = Gram(2,3)
    Gram(4,2) = Gram(2,4)
    Gram(5,2) = Gram(2,5)
    Gram(4,3) = Gram(3,4)
    Gram(5,3) = Gram(3,5)
    Gram(5,4) = Gram(4,5)
!   Zmax=maxval(abs(Gram(1:5,1:5)))

    LoCons = LoStrucConts(5,rmax,Gram)

    Fdiff=0d0
    do r=0,rmax
      do m0=0,r/2
        struc1(:,0)=m0
        norm = norm0/norm0**((r-m0)/4d0)
        ! norm = Zmax**(r-m0)*norm0*max(1d0,1d0/(norm0**(1d0/4d0)*Zmax))**(r/2)  too large        
        ! norm = Zmax**(r-m0)*norm0*(1d0/(norm0**(1d0/4d0)*Zmax))**(r/2)
        do i1=1,BinomTable(r-2*m0,4+r-2*m0)
          struc1(i1,1:5)=CalcCIndArr(5,r-2*m0,i1)
          Fp(i1) =0d0 
          Fp2(i1)=0d0 
          do n0=0,r/2
            struc2(0)=n0
            do i2=1,BinomTable(r-2*n0,4+r-2*n0)
              struc2(1:5)=CalcCIndArr(5,r-2*n0,i2)
              Fp(i1) = Fp(i1) &
                  + F(n0,struc2(1),struc2(2),struc2(3),struc2(4),struc2(5))* &
                    LoCons(m0,i1,n0,i2,r)
              Fp2(i1) = Fp2(i1) &
                   + F2(n0,struc2(1),struc2(2),struc2(3),struc2(4),struc2(5))* &
                     LoCons(m0,i1,n0,i2,r)
            enddo
          enddo

          norm = max(norm,min(abs(Fp(i1)),abs(Fp2(i1))))
          diffFp(i1) = Fp(i1)-Fp2(i1)
        enddo

        do i1=1,BinomTable(r-2*m0,4+r-2*m0)
          Fdiff(r) = max(Fdiff(r),abs(diffFp(i1))/norm)
          
          if ((abs(diffFp(i1)).gt.checkacc_cll*norm).and.(flag.eq.1)) then
             write(ncheckout_cll,*) '*************************************************************************'
             write(ncheckout_cll,*) 'F difference NO.', DiffCnt_cll(6)+1
             write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
             write(ncheckout_cll,'(A21,I2,A4,I2)') 'F integral with rank', r,' of ',rmax
             write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
             write(ncheckout_cll,*) '------------------------------------------------------------'
             write(ncheckout_cll,fmt1) 'p10=', p10
             write(ncheckout_cll,fmt1) 'p21=', p21
             write(ncheckout_cll,fmt1) 'p32=', p32
             write(ncheckout_cll,fmt1) 'p43=', p43
             write(ncheckout_cll,fmt1) 'p54=', p54
             write(ncheckout_cll,fmt1) 'p50=', p50
             write(ncheckout_cll,fmt1) 'p20=', p20
             write(ncheckout_cll,fmt1) 'p31=', p31
             write(ncheckout_cll,fmt1) 'p42=', p42
             write(ncheckout_cll,fmt1) 'p53=', p53
             write(ncheckout_cll,fmt1) 'p40=', p40
             write(ncheckout_cll,fmt1) 'p51=', p51
             write(ncheckout_cll,fmt1) 'p30=', p30
             write(ncheckout_cll,fmt1) 'p41=', p41
             write(ncheckout_cll,fmt1) 'p52=', p52
             write(ncheckout_cll,fmt1) 'm02=', m02
             write(ncheckout_cll,fmt1) 'm12=', m12
             write(ncheckout_cll,fmt1) 'm22=', m22
             write(ncheckout_cll,fmt1) 'm32=', m32
             write(ncheckout_cll,fmt1) 'm42=', m42
             write(ncheckout_cll,fmt1) 'm52=', m52
             write(ncheckout_cll,*) '--------------------------------------------------------------------------'
             write(ncheckout_cll,fmt4) 'COLI:',0,0,0,0,0,0,F(0,0,0,0,0,0)
             write(ncheckout_cll,fmt4) 'DD  :',0,0,0,0,0,0,F2(0,0,0,0,0,0)
             write(ncheckout_cll,fmt3) 'COLI:',struc1(i1,0:5),Fp(i1)
             write(ncheckout_cll,fmt3) 'DD  :',struc1(i1,0:5),Fp2(i1)
             write(ncheckout_cll,*) 'diff:', abs(diffFp(i1))/norm
!             write(ncheckout_cll,*) 'adiff:', abs(diffFp(i1))
!             write(ncheckout_cll,*) 'norm:', norm
!            write(ncheckout_cll,*) 'normo:',norm0/norm0**((r-m0)/real(4))
!            write(ncheckout_cll,*) 'normn:',Zmax**(r-m0)*norm0*(1d0/(norm0**(1d0/4d0)*Zmax))**(r/2)
!            write(ncheckout_cll,*) 'norml:',Zmax**(r-m0)*norm0*max(1d0,1d0/(norm0**(1d0/4d0)*Zmax))**(r/2)
!             write(ncheckout_cll,*) 'norm0:', norm0
!             write(ncheckout_cll,*) 'Zmax:', Zmax
             flag=2
             ratio=abs(diffFp(i1))/norm
          elseif((flag.eq.2).and.(abs(diffFp(i1)).gt.ratio*norm)) then
!          elseif((flag.eq.2).and.(abs(diffFp(i1)).gt.checkacc_cll*norm)) then
             write(ncheckout_cll,fmt3) 'COLI:',struc1(i1,0:5),Fp(i1)
             write(ncheckout_cll,fmt3) 'DD  :',struc1(i1,0:5),Fp2(i1)
!             write(ncheckout_cll,fmt3) 'COLI:',struc1(0),struc1(1),struc1(2),  &
!                                               struc1(3),struc1(4),struc1(5),Fp
!             write(ncheckout_cll,fmt3) 'DD  :',struc1(0),struc1(1),struc1(2),  &
!                                               struc1(3),struc1(4),struc1(5),Fp2
             write(ncheckout_cll,*) 'diff:', abs(diffFp(i1))/norm
             ratio=abs(diffFp(i1))/norm
           elseif ((abs(diffFp(i1)).gt.checkacc_cll*norm).and.(flag.eq.0)) then
             flag=3
          end if

        end do
      end do
    end do
    Fdiff = Fdiff*norm0

    if(flag.eq.2)then
      write(ncheckout_cll,*) '***************************************************************************'
      write(ncheckout_cll,*) ' end F'
      write(ncheckout_cll,*)
      DiffCnt_cll(6) =  DiffCnt_cll(6) + 1
      if(DiffCnt_cll(6).eq.MaxCheck_cll(6)) then
        write(ncheckout_cll,*) ' Further output for differences in F functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCnt_cll(6) =  DiffCnt_cll(6) + 1
    endif


  end subroutine CheckCoefsF_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsTN_cll(TN,TN2,MomInv,masses2,N,rmax,norm0,TNdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsTN_cll(TN,TN2,MomInv,masses2,N,rmax,norm0,TNdiff)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex, intent(in) :: TN(NCoefs(rmax,N))
    double complex, intent(in) :: TN2(NCoefs(rmax,N))
    double precision, intent(in) :: norm0
    double precision, intent(out) :: TNdiff(0:rmax)
    double complex :: diffTN,Gram(N-1,N-1)
    double complex :: TNp(BinomTable(rmax,rmax+N-2)),TNp2(BinomTable(rmax,rmax+N-2)),diffTNp(BinomTable(rmax,rmax+N-2))
    double precision :: norm,ratio
!   double precision :: norm,ratio,Zmax
    integer :: ind,i,m0,n0,ncnt,r,ncntr,flag,flag2
!    integer, parameter :: noutCheckTNmax=50
!    integer, save :: DiffCntTN
    double complex, allocatable :: LoCons(:,:,:,:,:)
    integer :: i1,i2
    character(len=*),parameter :: fmt1 = "(A8,'(',i2,')=dcmplx(',d25.18,',',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' TN(',i2,') = (',E23.16,' , ',E23.16,' ), r=',i2)"
    character(len=*),parameter :: fmt3 = &
    "(A6,' TN*T(',i1,',',i2,') = (',E23.16,' , ',E23.16,' ), r=',i2)"
    character(len=*),parameter :: fmt4 = &
    "(A6,' TN(',i2,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntTN /0/

    CheckCnt_cll(N) = CheckCnt_cll(N) + 1

    flag=1

    if(DiffCnt_cll(N).ge.MaxCheck_cll(N)) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    

    ratio=0d0

    TNdiff=0d0
    if (N.le.4) then
!      r = 0
!      do ind=1,NCoefs(rmax,N)
!        if (ind.gt.NCoefs(r,N)) then
!          r = r+1
!        end if

      ncnt=0
      do r=0,rmax
        ncntr=ncnt
        do n0=r/2,0,-1
          do i2=1,BinomTable(r-2*n0,N+r-2*n0-2)
            ncnt= ncnt + 1 
            ind = ncnt

!        norm = min(abs(TN(ind)),abs(TN2(ind)))
        if(N.ne.2) then
          norm = norm0/norm0**(n0/real(N-2))
        else
         if (max(abs(MomInv(1)),abs(masses2(0)),abs(masses2(1))).ne.0d0) then
          norm = norm0*max(abs(MomInv(1)),abs(masses2(0)),abs(masses2(1)))**n0
        else
          norm = norm0*muuv2_cll**n0
        end if
         
        endif
        diffTN = TN(ind)-TN2(ind)
        if (n0.eq.0) TNdiff(r) = max(TNdiff(r),abs(diffTN))
        if ((abs(diffTN).gt.checkacc_cll*norm) .and.(flag.eq.1)) then
          write(ncheckout_cll,*) '***************************************************************************'
          write(ncheckout_cll,'(A12,I2,A16,I10)') 'TN with N =',N,' difference NO.', DiffCnt_cll(N)+1
          write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
          write(ncheckout_cll,'(A21,I2,A10,I2,A4,I2)') 'TN integral with N =', N, ' and rank ', r,' of ',rmax
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
          do i=1,BinomTable(2,N)
            write(ncheckout_cll,fmt1) 'MomInv ', i, MomInv(i)
          end do
          do i=0,N-1
            write(ncheckout_cll,fmt1) 'masses2', i, masses2(i)
          end do
          write(ncheckout_cll,*) '---------------------------------------------------------------------------'
          write(ncheckout_cll,fmt2) 'COLI:',1,TN(1),0
          write(ncheckout_cll,fmt2) 'DD  :',1,TN2(1),0
          write(ncheckout_cll,fmt2) 'COLI:',ind,TN(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',ind,TN2(ind),r
!                write(ncheckout_cll,*) 'COLI:', D(n0,n1,n2,n3)
!                write(ncheckout_cll,*) 'DD  :', D2(n0,n1,n2,n3)
          if(norm.ne.0d0)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm
            ratio=abs(diffTN)/norm
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
          flag=2
        elseif((flag.eq.2).and.(abs(diffTN).gt.ratio*norm)) then
          write(ncheckout_cll,fmt2) 'COLI:',ind,TN(ind),r
          write(ncheckout_cll,fmt2) 'DD  :',ind,TN2(ind),r
          if(norm.gt.1d-100)then
            write(ncheckout_cll,*) 'diff:', abs(diffTN)/norm
            ratio=abs(diffTN)/norm
          else
            write(ncheckout_cll,*) 'diff:', 1d50
            ratio=1d50
          endif
        elseif ((flag.eq.0).and.(abs(diffTN).gt.checkacc_cll*norm)) then
          flag=3
        end if

          end do
        end do

      end do

    else

      Gram = CalcGram(N,MomInv)
!     Zmax = maxval(abs(Gram(1:N-1,1:N-1)))
      allocate(LoCons(0:rmax/2,BinomTable(rmax,rmax+N-2),0:rmax/2,BinomTable(rmax,rmax+N-2),0:rmax))
      LoCons = LoStrucConts(N-1,rmax,Gram)

      ncnt=0
      do r=0,rmax
        ncntr=ncnt
        do m0=r/2,0,-1
          if (r-m0.ne.0) then
          norm = norm0/norm0**((r-m0)/real(N-2))
          ! norm = Zmax**(r-m0)*norm0*(1d0/(norm0**(1d0/real(N-2))*Zmax))**(r/2)
          else 
            norm = norm0
            ! norm = norm0*(1d0/(norm0**(1d0/real(N-2))*Zmax))**(r/2)
          end if
          do i1=1,BinomTable(r-2*m0,N+r-2*m0-2)
            TNp(i1) =0d0 
            TNp2(i1)=0d0
            ncnt = ncntr
            do n0=r/2,0,-1
              do i2=1,BinomTable(r-2*n0,N+r-2*n0-2)
                ncnt= ncnt + 1 
                TNp(i1) = TNp(i1) + TN(ncnt)*LoCons(m0,i1,n0,i2,r)
                TNp2(i1) = TNp2(i1) + TN2(ncnt)*LoCons(m0,i1,n0,i2,r)
              end do
            end do
            norm = max(norm,min(abs(TNp(i1)),abs(TNp2(i1))))
            diffTNp(i1) = TNp(i1)-TNp2(i1)
          enddo

          do i1=1,BinomTable(r-2*m0,N+r-2*m0-2)
            TNdiff(r) = max(TNdiff(r),abs(diffTNp(i1))/norm)

            if ((abs(diffTNp(i1)).gt.checkacc_cll*norm).and.(flag.eq.1)) then
              write(ncheckout_cll,*) '***************************************************************************'
              write(ncheckout_cll,'(A12,I2,A16,I10)') 'TN with N =',N,' difference NO.', DiffCnt_cll(N)+1
              write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
              write(ncheckout_cll,'(A21,I2,A10,I2,A4,I2)') 'TN integral with N =', N, ' and rank ', r, ' of ',rmax
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
              do i=1,BinomTable(2,N)
                write(ncheckout_cll,fmt1) 'MomInv', i, MomInv(i)
              end do
              do i=0,N-1
                write(ncheckout_cll,fmt1) 'masses2', i, masses2(i)
              end do
              write(ncheckout_cll,*) '---------------------------------------------------------------------------'
              write(ncheckout_cll,fmt2) 'COLI:',1,TN(1),0
              write(ncheckout_cll,fmt2) 'DD  :',1,TN2(1),0
              write(ncheckout_cll,fmt3) 'COLI:',m0,i1,TNp(i1),r
              write(ncheckout_cll,fmt3) 'DD  :',m0,i1,TNp2(i1),r
              write(ncheckout_cll,*) 'diff:', abs(diffTNp(i1))/norm
!              write(ncheckout_cll,*) 'norm:', norm
!             write(ncheckout_cll,*) 'normo:',norm0/norm0**((r-m0)/real(N-2))
!              write(ncheckout_cll,*) 'norm0:', norm0
!              write(ncheckout_cll,*) 'Zmax:', Zmax
              flag=2
              ratio=abs(diffTNp(i1))/norm
            elseif((flag.eq.2).and.(abs(diffTNp(i1)).gt.ratio*norm)) then
!            elseif((flag.eq.2).and.(abs(diffTN(i1)).gt.checkacc_cll*norm)) then
              write(ncheckout_cll,fmt3) 'COLI:',m0,i1,TNp(i1),r
              write(ncheckout_cll,fmt3) 'DD  :',m0,i1,TNp2(i1),r
              write(ncheckout_cll,*) 'diff:', abs(diffTNp(i1))/norm
              ratio=abs(diffTNp(i1))/norm
            elseif ((flag.eq.0).and.(abs(diffTNp(i1)).gt.checkacc_cll*norm)) then
              flag=3
            end if

          end do
        end do
      end do
      TNdiff = TNdiff*norm0      
    end if

    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end TN '
      write(ncheckout_cll,*)
      DiffCnt_cll(N) =  DiffCnt_cll(N) + 1
      if(DiffCnt_cll(N).eq.MaxCheck_cll(N)) then
        write(ncheckout_cll,*) ' Further output for differences in TN functions suppressed '   
        write(ncheckout_cll,*)
      endif
    else if (flag.eq.3) then
      DiffCnt_cll(N) =  DiffCnt_cll(N) + 1
    endif


  end subroutine CheckCoefsTN_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsDBr_cll(DB,DB2,p10,m02,m12,r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsDBr_cll(DB,DB2,p10,m02,m12,r)

    integer, intent(in) :: r
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(in) :: DB,DB2
    double complex :: diffB
    double precision :: norm
    integer :: flag
!    integer, parameter :: noutCheckDBmax=50
!    integer, save :: DiffCntDB

    character(len=*),parameter :: fmt1 = "(A5,'dcmplx(',g25.18,',',g25.18,' )')"

!    data DiffCntDB /0/


    CheckCntDB_cll = CheckCntDB_cll + 1

    flag=1
    if(DiffCntDB_cll.ge.MaxCheckDB_cll) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    

    norm = min(abs(DB),abs(DB2))
    diffB = DB-DB2
    if ((abs(diffB).gt.checkacc_cll*norm)) then
      if (flag.eq.1) then
        write(ncheckout_cll,*) '*************************************************************************'
        write(ncheckout_cll,*) 'DB difference NO.', DiffCntDB_cll+1
        write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
        select case (r)
        case (0)
          write(ncheckout_cll,*) 'integral DB0'
        case (1)
          write(ncheckout_cll,*) 'integral DB1'
        case (2)
          write(ncheckout_cll,*) 'integral DB00'
        end select
        write(ncheckout_cll,*) '-------------------------------------------------------------------------'
        write(ncheckout_cll,fmt1) 'p10=', p10
        write(ncheckout_cll,fmt1) 'm02=', m02
        write(ncheckout_cll,fmt1) 'm12=', m12
        write(ncheckout_cll,*) '-------------------------------------------------------------------------'
        write(ncheckout_cll,*) 'COLI:', DB
        write(ncheckout_cll,*) 'DD  :', DB2
        write(ncheckout_cll,*) '*************************************************************************'
        write(ncheckout_cll,*) 
        write(ncheckout_cll,*)
      end if
      DiffCntDB_cll = DiffCntDB_cll + 1
      if(DiffCntDB_cll.eq.MaxCheckDB_cll) then
        write(ncheckout_cll,*) ' Further output for differences in DB functions suppressed '   
        write(ncheckout_cll,*)
      endif
      end if


  end subroutine CheckCoefsDBr_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CheckCoefsDB_cll(DB,DB2,p10,m02,m12,rmax,norm0,DBdiff)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CheckCoefsDB_cll(DB,DB2,p10,m02,m12,rmax,norm0,DBdiff)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(in) :: DB(0:rmax/2,0:rmax),DB2(0:rmax/2,0:rmax)
    double precision, intent(in) :: norm0
    double precision, intent(out) :: DBdiff(0:rmax)
    double complex :: diffDB
    double precision :: norm,ratio
    integer :: r,n0,n1,i
    integer :: flag
!    integer, parameter :: noutCheckDBmax=50
!    integer, save :: DiffCntDB

    character(len=*),parameter :: fmt1 = "(A6,'dcmplx(',d25.18,' ,',d25.18,' )')"
    character(len=*),parameter :: fmt2 = &
    "(A6,' DB(',i1,',',i1,') = (',E23.16,' , ',E23.16,' )')"

!    data DiffCntDB /0/

!    DiffCntDB =  DiffCntDB + 1

    CheckCntDB_cll = CheckCntDB_cll + 1

    flag=1
    if(DiffCntDB_cll.ge.MaxCheckDB_cll) flag=0
    if(ncheckout_cll.eq.closed_cll) flag=0    
    ratio=0d0

    DBdiff=0d0
    do r=0,rmax
      do n0=0,r/2
        n1 = r-2*n0
!        norm = min(abs(DB(n0,n1)),abs(DB2(n0,n1)))
        norm = norm0/norm0**n0
        diffDB = DB(n0,n1)-DB2(n0,n1)
        if (n0.eq.0) DBdiff(r) = max(DBdiff(r),abs(diffDB))
        if ((abs(diffDB).gt.checkacc_cll*norm).and.(flag.eq.1)) then
          write(ncheckout_cll,*) '*************************************************************************'
          write(ncheckout_cll,*) 'DB difference NO.', DiffCntDB_cll+1
          write(ncheckout_cll,*) 'COLI and DD do not agree!    checkacc =', checkacc_cll
          write(ncheckout_cll,'(A21,I2,A4,I2)') 'DB integral with rank', r,' of ',rmax
          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
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
          write(ncheckout_cll,*) '------------------------------------------------------------'
!          write(ncheckout_cll,*) 'n0', n0
!          write(ncheckout_cll,*) 'n1', n1
!          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
          write(ncheckout_cll,fmt1) 'p10=', p10
          write(ncheckout_cll,fmt1) 'm02=', m02
          write(ncheckout_cll,fmt1) 'm12=', m12
          write(ncheckout_cll,*) '-------------------------------------------------------------------------'
!                write(ncheckout_cll,*) 'C0_coli:', DB(0,0)
!                write(ncheckout_cll,*) 'C0_DD  :', DB2(0,0)
          write(ncheckout_cll,fmt2) 'COLI:',0,0,DB(0,0)
          write(ncheckout_cll,fmt2) 'DD  :',0,0,DB2(0,0)
          write(ncheckout_cll,fmt2) 'COLI:',n0,n1,DB(n0,n1)
          write(ncheckout_cll,fmt2) 'DD  :',n0,n1,DB2(n0,n1)
!                write(ncheckout_cll,*) 'COLI:', DB(n0,n1)
!                write(ncheckout_cll,*) 'DD  :', DB2(n0,n1)
          write(ncheckout_cll,*) 'diff:', abs(diffDB)/norm
          flag=2
          ratio=abs(diffDB)/norm
        elseif((flag.eq.2).and.(abs(diffDB).gt.ratio*norm)) then
           write(ncheckout_cll,fmt2) 'COLI:',n0,n1,DB(n0,n1)
           write(ncheckout_cll,fmt2) 'DD  :',n0,n1,DB2(n0,n1)
           write(ncheckout_cll,*) 'diff:', abs(diffDB)/norm
           ratio=abs(diffDB)/norm
        elseif ((abs(diffDB).gt.checkacc_cll*norm).and.(flag.eq.0)) then
           flag=3
        end if
      end do
    end do
    if(flag.eq.2)then
      write(ncheckout_cll,*) '*************************************************************************'
      write(ncheckout_cll,*) ' end B'
      write(ncheckout_cll,*)
      DiffCntDB_cll = DiffCntDB_cll + 1
      if(DiffCntDB_cll.ge.MaxCheckDB_cll) then
        write(ncheckout_cll,*) ' Further output for differences in B functions suppressed '   
        write(ncheckout_cll,*)
      endif
    elseif(flag.eq.3)then
      DiffCntDB_cll =  DiffCntDB_cll + 1
    end if 


  end subroutine CheckCoefsDB_cll





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  subroutine SetErrFlag_cll(err)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine SetErrFlag_cll(err)
!
!    integer, intent(in) :: err
!
!    ErrFlag_cll = err
!
!  end subroutine SetErrFlag_cll
!
!
!
!
!
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  subroutine getErrFlag_cll(err)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine getErrFlag_cll(err)
!
!    integer, intent(out) :: err
!
!    err = ErrFlag_cll
!
!  end subroutine getErrFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine ErrOut_cll(sub,err,flag,nomaster)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Suppression of output must be implemented in calling routines!

  subroutine ErrOut_cll(sub,err,flag,nomaster)

    character(len=*), intent(in) :: sub, err
    logical, intent(out) :: flag
    logical, optional, intent(in) :: nomaster
!    integer, parameter :: maxErrOut=100
  
    flag = .false.
    if (erroutlev_cll.eq.0) return

    ErrCnt_cll = ErrCnt_cll + 1      
    if(nerrout_cll.ne.closed_cll) then
      if (ErrCnt_cll.le.MaxErrOut_cll) then
        write(nerrout_cll,*)
        write(nerrout_cll,*)
        write(nerrout_cll,*)
        write(nerrout_cll,*) '***********************************************************'
        write(nerrout_cll,*) 'ERROR NO.', ErrCnt_cll
        write(nerrout_cll,*) 'in routine: ', trim(sub)
        write(nerrout_cll,*) trim(err)
        flag=.true.
        if (present(nomaster)) then
          if(nomaster) return
        end if
        call WriteMaster_cll(nerrout_cll)
      elseif (ErrCnt_cll.eq.MaxErrOut_cll+1) then
        write(nerrout_cll,*)
        write(nerrout_cll,*)
        write(nerrout_cll,*)
        write(nerrout_cll,*) '***********************************************************'
        write(nerrout_cll,*)
        write(nerrout_cll,*) ' Further output of Errors will be suppressed '
        write(nerrout_cll,*)
      endif
    endif

  end subroutine ErrOut_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CritPointsOut_cll(sub,N,acc,cntr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CritPointsOut_cll(sub,N,acc,cntr)

    character(len=*), intent(in) :: sub
    double precision, intent(in) :: acc
    integer, intent(in) :: N
    integer :: i
    integer(kind=long_int) :: cntr

    write(ncpout_cll,*)
    write(ncpout_cll,*)
    write(ncpout_cll,*)
    write(ncpout_cll,*) '***********************************************************'
    write(ncpout_cll,'(A19,I6)') 'Critical Point NO.', cntr
    if (N.gt.0) then
      write(ncpout_cll,'(A14,A9,A6,I0)') &
          'in integral: ', trim(sub),', N = ',N
    else
      write(ncpout_cll,*) 'in integral: ', trim(sub)
    endif
    write(ncpout_cll,*) 'estimated accuracy: ', acc
    write(ncpout_cll,*) '-----------------------------------------------------------'
    write(ncpout_cll,*) 'GLOBAL PARAMETERS:'
    write(ncpout_cll,*) 'mode        ', mode_cll
    write(ncpout_cll,*) 'muUV2       ', muUV2_cll
    write(ncpout_cll,*) 'muIR2       ', muIR2_cll
    write(ncpout_cll,*) 'deltaUV     ', deltaUV_cll
    write(ncpout_cll,*) 'deltaIR1    ', deltaIR1_cll
    write(ncpout_cll,*) 'deltaIR2    ', deltaIR2_cll
    write(ncpout_cll,*) 'nminf       ', nminf_cll
    do i=1,nminf_cll
      write(ncpout_cll,*) 'minf2       ', i, minf2_cll(i)
    end do
    write(ncpout_cll,*) 'dprec       ', dprec_cll
    write(ncpout_cll,*) 'reqacc      ', reqacc_cll
    write(ncpout_cll,*) 'critacc     ', critacc_cll
    write(ncpout_cll,*) 'checkacc    ', checkacc_cll
    write(ncpout_cll,*) 'ErrFlag     ', ErrFlag_cll
!    write(ncpout_cll,*) '------------------------------------------------------------'
    call WriteMaster_cll(ncpout_cll)

  end subroutine CritPointsOut_cll



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CritPointsOut2_cll(sub,N,acc,cntr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CritPointsOut2_cll(sub,N,acc,cntr)

    character(len=*), intent(in) :: sub
    double precision, intent(in) :: acc
    integer, intent(in) :: N
    integer :: i,cntr

    write(ncpout2_cll,*)
    write(ncpout2_cll,*)
    write(ncpout2_cll,*)
    write(ncpout2_cll,*) '***********************************************************'
    write(ncpout2_cll,'(A19,I6)') 'Critical Point NO.', cntr
    if (N.gt.0) then
      write(ncpout2_cll,'(A14,A9,A6,I0)') &
          'in integral: ', trim(sub),', N = ',N
    else
      write(ncpout2_cll,*) 'in integral: ', trim(sub)
    endif
    write(ncpout2_cll,*) 'estimated accuracy: ', acc
    write(ncpout2_cll,*) '-----------------------------------------------------------'
    write(ncpout2_cll,*) 'GLOBAL PARAMETERS:'
    write(ncpout2_cll,*) 'mode        ', mode_cll
    write(ncpout2_cll,*) 'muUV2       ', muUV2_cll
    write(ncpout2_cll,*) 'muIR2       ', muIR2_cll
    write(ncpout2_cll,*) 'deltaUV     ', deltaUV_cll
    write(ncpout2_cll,*) 'deltaIR1    ', deltaIR1_cll
    write(ncpout2_cll,*) 'deltaIR2    ', deltaIR2_cll
    write(ncpout2_cll,*) 'nminf       ', nminf_cll
    do i=1,nminf_cll
      write(ncpout2_cll,*) 'minf2       ', i, minf2_cll(i)
    end do
    write(ncpout2_cll,*) 'dprec       ', dprec_cll
    write(ncpout2_cll,*) 'reqacc      ', reqacc_cll
    write(ncpout2_cll,*) 'critacc     ', critacc_cll
    write(ncpout2_cll,*) 'checkacc    ', checkacc_cll
    write(ncpout2_cll,*) 'ErrFlag     ', ErrFlag_cll
!    write(ncpout2_cll,*) '------------------------------------------------------------'
    call WriteMaster_cll(ncpout2_cll)

  end subroutine CritPointsOut2_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine PrintStatistics_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PrintStatistics_cll()

    integer :: i
    logical :: infwri

101 format(' #calls ',A9,'          = ',i20)
102 format(' #calls ',A9,' (N =',i2,')  = ',i20)
111 format(' #calls ',A9,'          = ',i20,' or ',F10.5,' %')
112 format(' #calls ',A9,' (N =',i2,')  = ',i20,' or ',F10.5,' %')

    if (.not.Monitoring) then
      if (infoutlev_cll.ge.1) then
        InfCnt_cll = InfCnt_cll + 1      
        write(ninfout_cll,*) 'COLLIER: CritPointsMonitor not initialized'
        write(ninfout_cll,*) '         no statistics available          '
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*) '***********************************************************'
        write(ninfout_cll,*) 'Info-output NO.', InfCnt_cll
        write(ninfout_cll,*) 'in routine: PrintStatistics_cll'
        write(ninfout_cll,*) 'CritPointsMonitor not initialized'
        write(ninfout_cll,*) 'no statistics available          '
      end if
      return
    else if(ncpout_cll.eq.closed_cll) then
      if (infoutlev_cll.ge.1) then
        InfCnt_cll = InfCnt_cll + 1      
        write(ninfout_cll,*) 'COLLIER: Output for critical points switched off'
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*) '***********************************************************'
        write(ninfout_cll,*) 'Info-output NO.', InfCnt_cll
        write(ninfout_cll,*) 'in routine: PrintStatistics_cll'
        write(ninfout_cll,*) 'Output for critical points switched off'
       end if
      return    
    endif

    write(ncpout_cll,90) 
90 format (//' Collier: Summary of critical points:')

    write(ncpout_cll,100) 
100 format (/' Total numbers of calls of COLLIER functions')

    if( PointsCntA_cll.ne.0) then
    write(ncpout_cll,101)  'A_cll',PointsCntA_cll
    endif
    if( PointsCntB_cll.ne.0) then
    write(ncpout_cll,101)  'B_cll',PointsCntB_cll
    endif
    if( PointsCntC_cll.ne.0) then
    write(ncpout_cll,101)  'C_cll',PointsCntC_cll
    endif
    if( PointsCntD_cll.ne.0) then
    write(ncpout_cll,101)  'D_cll',PointsCntD_cll
    endif
    if( PointsCntE_cll.ne.0) then
    write(ncpout_cll,101)  'E_cll',PointsCntE_cll
    endif
    if( PointsCntF_cll.ne.0) then
    write(ncpout_cll,101)  'F_cll',PointsCntF_cll
    endif
    if( PointsCntG_cll.ne.0) then
    write(ncpout_cll,101)  'G_cll',PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0) then
        write(ncpout_cll,102)  'TN_cll',i,PointsCntTN_cll(i)
      endif
    end do

    if( PointsCntAten_cll.ne.0) then
    write(ncpout_cll,101)  'Aten_cll',PointsCntAten_cll
    endif
    if( PointsCntBten_cll.ne.0) then
    write(ncpout_cll,101)  'Bten_cll',PointsCntBten_cll
    endif
    if( PointsCntCten_cll.ne.0) then
    write(ncpout_cll,101)  'Cten_cll',PointsCntCten_cll
    endif
    if( PointsCntDten_cll.ne.0) then
    write(ncpout_cll,101)  'Dten_cll',PointsCntDten_cll
    endif
    if( PointsCntEten_cll.ne.0) then
    write(ncpout_cll,101)  'Eten_cll',PointsCntEten_cll
    endif
    if( PointsCntFten_cll.ne.0) then
    write(ncpout_cll,101)  'Ften_cll',PointsCntFten_cll
    endif
    if( PointsCntGten_cll.ne.0) then
    write(ncpout_cll,101)  'Gten_cll',PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0) then
        write(ncpout_cll,102)  'TNten_cll',i,PointsCntTNten_cll(i)
      endif
    end do

    write(ncpout_cll,110) reqacc_coli
110 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than  reqacc_coli =',Es11.4)
    if(PointsCntDB_cll.ne.0.and.AccPointsCntDB_cll.ne.0) then
    write(ncpout_cll,111)  'DB_cll',AccPointsCntDB_cll,AccPointsCntDB_cll/real(PointsCntDB_cll,kind(1d0))*1d2
    endif
    if(PointsCntA_cll.ne.0.and.AccPointsCntA_cll.ne.0) then
    write(ncpout_cll,111)  'A_cll',AccPointsCntA_cll,AccPointsCntA_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.AccPointsCntB_cll.ne.0) then
    write(ncpout_cll,111)  'B_cll',AccPointsCntB_cll,AccPointsCntB_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.AccPointsCntC_cll.ne.0) then
    write(ncpout_cll,111)  'C_cll',AccPointsCntC_cll,AccPointsCntC_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.AccPointsCntD_cll.ne.0) then
    write(ncpout_cll,111)  'D_cll',AccPointsCntD_cll,AccPointsCntD_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.AccPointsCntE_cll.ne.0) then
    write(ncpout_cll,111)  'E_cll',AccPointsCntE_cll,AccPointsCntE_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.AccPointsCntF_cll.ne.0) then
    write(ncpout_cll,111)  'F_cll',AccPointsCntF_cll,AccPointsCntF_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.AccPointsCntG_cll.ne.0) then
    write(ncpout_cll,111)  'G_cll',AccPointsCntG_cll,AccPointsCntG_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.AccPointsCntTN_cll(i).ne.0) then
        write(ncpout_cll,112)  'TN_cll',i,AccPointsCntTN_cll(i),AccPointsCntTN_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do

    if(PointsCntDBten_cll.ne.0.and.AccPointsCntDBten_cll.ne.0) then
    write(ncpout_cll,111)  'DBten_cll',AccPointsCntDBten_cll,AccPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.AccPointsCntAten_cll.ne.0) then
    write(ncpout_cll,111)  'Aten_cll',AccPointsCntAten_cll,AccPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.AccPointsCntBten_cll.ne.0) then
    write(ncpout_cll,111)  'Bten_cll',AccPointsCntBten_cll,AccPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.AccPointsCntCten_cll.ne.0) then
    write(ncpout_cll,111)  'Cten_cll',AccPointsCntCten_cll,AccPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.AccPointsCntDten_cll.ne.0) then
    write(ncpout_cll,111)  'Dten_cll',AccPointsCntDten_cll,AccPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.AccPointsCntEten_cll.ne.0) then
    write(ncpout_cll,111)  'Eten_cll',AccPointsCntEten_cll,AccPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.AccPointsCntFten_cll.ne.0) then
    write(ncpout_cll,111)  'Ften_cll',AccPointsCntFten_cll,AccPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.AccPointsCntGten_cll.ne.0) then
    write(ncpout_cll,111)  'Gten_cll',AccPointsCntGten_cll,AccPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.AccPointsCntTNten_cll(i).ne.0) then
        write(ncpout_cll,112)  'TNten_cll',i,AccPointsCntTNten_cll(i),AccPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do

    write(ncpout_cll,130) sqrt(reqacc_coli)
130 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than ',  &
        ' sqrt(reqacc_coli) =',Es11.4)
!        ' with an estimated accuracy worse than     '/  &
!        ' sqrt(reqacc_coli) =',Es11.4)
    if(PointsCntDB_cll.ne.0.and.sAccPointsCntDB_cll.ne.0) then
    write(ncpout_cll,111)  'DB_cll',sAccPointsCntDB_cll,sAccPointsCntDB_cll/real(PointsCntDB_cll,kind(1d0))*1d2
    endif
    if(PointsCntA_cll.ne.0.and.sAccPointsCntA_cll.ne.0) then
    write(ncpout_cll,111)  'A_cll',sAccPointsCntA_cll,sAccPointsCntA_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.sAccPointsCntB_cll.ne.0) then
    write(ncpout_cll,111)  'B_cll',sAccPointsCntB_cll,sAccPointsCntB_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.sAccPointsCntC_cll.ne.0) then
    write(ncpout_cll,111)  'C_cll',sAccPointsCntC_cll,sAccPointsCntC_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.sAccPointsCntD_cll.ne.0) then
    write(ncpout_cll,111)  'D_cll',sAccPointsCntD_cll,sAccPointsCntD_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.sAccPointsCntE_cll.ne.0) then
    write(ncpout_cll,111)  'E_cll',sAccPointsCntE_cll,sAccPointsCntE_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.sAccPointsCntF_cll.ne.0) then
    write(ncpout_cll,111)  'F_cll',sAccPointsCntF_cll,sAccPointsCntF_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.sAccPointsCntG_cll.ne.0) then
    write(ncpout_cll,111)  'G_cll',sAccPointsCntG_cll,sAccPointsCntG_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.sAccPointsCntTN_cll(i).ne.0) then
        write(ncpout_cll,112)  'TN_cll',i,sAccPointsCntTN_cll(i),sAccPointsCntTN_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do

    if(PointsCntDBten_cll.ne.0.and.sAccPointsCntDBten_cll.ne.0) then
    write(ncpout_cll,111)  'DBten_cll',sAccPointsCntDBten_cll,sAccPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.sAccPointsCntAten_cll.ne.0) then
    write(ncpout_cll,111)  'Aten_cll',sAccPointsCntAten_cll,sAccPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.sAccPointsCntBten_cll.ne.0) then
    write(ncpout_cll,111)  'Bten_cll',sAccPointsCntBten_cll,sAccPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.sAccPointsCntCten_cll.ne.0) then
    write(ncpout_cll,111)  'Cten_cll',sAccPointsCntCten_cll,sAccPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.sAccPointsCntDten_cll.ne.0) then
    write(ncpout_cll,111)  'Dten_cll',sAccPointsCntDten_cll,sAccPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.sAccPointsCntEten_cll.ne.0) then
    write(ncpout_cll,111)  'Eten_cll',sAccPointsCntEten_cll,sAccPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.sAccPointsCntFten_cll.ne.0) then
    write(ncpout_cll,111)  'Ften_cll',sAccPointsCntFten_cll,sAccPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.sAccPointsCntGten_cll.ne.0) then
    write(ncpout_cll,111)  'Gten_cll',sAccPointsCntGten_cll,sAccPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.sAccPointsCntTNten_cll(i).ne.0) then
        write(ncpout_cll,112)  'TNten_cll',i,sAccPointsCntTNten_cll(i),sAccPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do


    write(ncpout_cll,120) critacc_cll
120 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than  critacc_coli =',Es11.4)
    
    if(PointsCntDB_cll.ne.0.and.CritPointsCntDB_cll.ne.0) then
    write(ncpout_cll,111)  'DB_cll',CritPointsCntDB_cll,CritPointsCntDB_cll/real(PointsCntDB_cll,kind(1d0))*1d2
    endif
    if(PointsCntA_cll.ne.0.and.CritPointsCntA_cll.ne.0) then
    write(ncpout_cll,111)  'A_cll',CritPointsCntA_cll,CritPointsCntA_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.CritPointsCntB_cll.ne.0) then
    write(ncpout_cll,111)  'B_cll',CritPointsCntB_cll,CritPointsCntB_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.CritPointsCntC_cll.ne.0) then
    write(ncpout_cll,111)  'C_cll',CritPointsCntC_cll,CritPointsCntC_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.CritPointsCntD_cll.ne.0) then
    write(ncpout_cll,111)  'D_cll',CritPointsCntD_cll,CritPointsCntD_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.CritPointsCntE_cll.ne.0) then
    write(ncpout_cll,111)  'E_cll',CritPointsCntE_cll,CritPointsCntE_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.CritPointsCntF_cll.ne.0) then
    write(ncpout_cll,111)  'F_cll',CritPointsCntF_cll,CritPointsCntF_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.CritPointsCntG_cll.ne.0) then
    write(ncpout_cll,111)  'G_cll',CritPointsCntG_cll,CritPointsCntG_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.CritPointsCntTN_cll(i).ne.0) then
        write(ncpout_cll,112)  'TN_cll',i,CritPointsCntTN_cll(i),CritPointsCntTN_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do

    if(PointsCntDBten_cll.ne.0.and.CritPointsCntDBten_cll.ne.0) then
    write(ncpout_cll,111)  'DBten_cll',CritPointsCntDBten_cll,CritPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.CritPointsCntAten_cll.ne.0) then
    write(ncpout_cll,111)  'Aten_cll',CritPointsCntAten_cll,CritPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.CritPointsCntBten_cll.ne.0) then
    write(ncpout_cll,111)  'Bten_cll',CritPointsCntBten_cll,CritPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.CritPointsCntCten_cll.ne.0) then
    write(ncpout_cll,111)  'Cten_cll',CritPointsCntCten_cll,CritPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.CritPointsCntDten_cll.ne.0) then
    write(ncpout_cll,111)  'Dten_cll',CritPointsCntDten_cll,CritPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.CritPointsCntEten_cll.ne.0) then
    write(ncpout_cll,111)  'Eten_cll',CritPointsCntEten_cll,CritPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.CritPointsCntFten_cll.ne.0) then
    write(ncpout_cll,111)  'Ften_cll',CritPointsCntFten_cll,CritPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.CritPointsCntGten_cll.ne.0) then
    write(ncpout_cll,111)  'Gten_cll',CritPointsCntGten_cll,CritPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.CritPointsCntTNten_cll(i).ne.0) then
        write(ncpout_cll,112)  'TNten_cll',i,CritPointsCntTNten_cll(i),CritPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do
    write(ncpout_cll,*) 


    if (qopened_check) then
    write(ncheckout_cll,290) 
290 format (//' Collier: Summary of COLI/DD use in mode 3:')

    write(ncheckout_cll,300) 
300 format (/' Total numbers of uses of COLI functions')

    if( PointsCntDB_coli.ne.0) then
    write(ncheckout_cll,111)  'DB_cll',PointsCntDB_coli,      &
        PointsCntDB_coli*1d2/(PointsCntDB_coli+PointsCntDB_dd)
    endif
    if( PointsCntA_coli.ne.0) then
    write(ncheckout_cll,111)  'A_cll',PointsCntA_coli,         & 
        PointsCntA_coli*1d2/(PointsCntA_coli+PointsCntA_dd)
    endif
    if( PointsCntB_coli.ne.0) then
    write(ncheckout_cll,111)  'B_cll',PointsCntB_coli,         &
        PointsCntB_coli*1d2/(PointsCntB_coli+PointsCntB_dd)
    endif
    if( PointsCntC_coli.ne.0) then
    write(ncheckout_cll,111)  'C_cll',PointsCntC_coli,          &
        PointsCntC_coli*1d2/(PointsCntC_coli+PointsCntC_dd)
    endif
    if( PointsCntD_coli.ne.0) then
    write(ncheckout_cll,111)  'D_cll',PointsCntD_coli,          &   
        PointsCntD_coli*1d2/(PointsCntD_coli+PointsCntD_dd)
    endif
    if( PointsCntE_coli.ne.0) then
    write(ncheckout_cll,111)  'E_cll',PointsCntE_coli,          &
        PointsCntE_coli*1d2/(PointsCntE_coli+PointsCntE_dd)
    endif
    if( PointsCntF_coli.ne.0) then
    write(ncheckout_cll,111)  'F_cll',PointsCntF_coli,         &
        PointsCntF_coli*1d2/(PointsCntF_coli+PointsCntF_dd)
    endif
    if( PointsCntG_coli.ne.0) then
    write(ncheckout_cll,111)  'G_cll',PointsCntG_coli,         &
        PointsCntG_coli*1d2/(PointsCntG_coli+PointsCntG_dd)
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_coli(i).ne.0) then
        write(ncheckout_cll,112)  'TN_cll',i,PointsCntTN_coli(i),  &
        PointsCntTN_coli(i)*1d2/(PointsCntTN_coli(i)+PointsCntTN_dd(i))
      endif
    end do

    if( PointsCntAten_coli.ne.0) then
    write(ncheckout_cll,111)  'Aten_cll',PointsCntAten_coli,         & 
        PointsCntAten_coli*1d2/(PointsCntAten_coli+PointsCntAten_dd)
    endif
    if( PointsCntBten_coli.ne.0) then
    write(ncheckout_cll,111)  'Bten_cll',PointsCntBten_coli,         &
        PointsCntBten_coli*1d2/(PointsCntBten_coli+PointsCntBten_dd)
    endif
    if( PointsCntCten_coli.ne.0) then
    write(ncheckout_cll,111)  'Cten_cll',PointsCntCten_coli,          &
        PointsCntCten_coli*1d2/(PointsCntCten_coli+PointsCntCten_dd)
    endif
    if( PointsCntDten_coli.ne.0) then
    write(ncheckout_cll,111)  'Dten_cll',PointsCntDten_coli,          &   
        PointsCntDten_coli*1d2/(PointsCntDten_coli+PointsCntDten_dd)
    endif
    if( PointsCntEten_coli.ne.0) then
    write(ncheckout_cll,111)  'Eten_cll',PointsCntEten_coli,          &
        PointsCntEten_coli*1d2/(PointsCntEten_coli+PointsCntEten_dd)
    endif
    if( PointsCntFten_coli.ne.0) then
    write(ncheckout_cll,111)  'Ften_cll',PointsCntFten_coli,         &
        PointsCntFten_coli*1d2/(PointsCntFten_coli+PointsCntFten_dd)
    endif
    if( PointsCntGten_coli.ne.0) then
    write(ncheckout_cll,111)  'Gten_cll',PointsCntGten_coli,         &
        PointsCntGten_coli*1d2/(PointsCntGten_coli+PointsCntGten_dd)
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_coli(i).ne.0) then
        write(ncheckout_cll,112)  'TNten_cll',i,PointsCntTNten_coli(i),  &
        PointsCntTNten_coli(i)*1d2/(PointsCntTNten_coli(i)+PointsCntTNten_dd(i))
      endif
    end do

    write(ncheckout_cll,210) 
210 format (/' Total numbers of uses of DD functions')

    if( PointsCntDB_dd.ne.0) then
    write(ncheckout_cll,111)  'DB_cll',PointsCntDB_dd,      &
        PointsCntDB_dd*1d2/(PointsCntDB_coli+PointsCntDB_dd)
    endif
    if( PointsCntA_dd.ne.0) then
    write(ncheckout_cll,111)  'A_cll',PointsCntA_dd,        &
        PointsCntA_dd*1d2/(PointsCntA_coli+PointsCntA_dd)
    endif
    if( PointsCntB_dd.ne.0) then
    write(ncheckout_cll,111)  'B_cll',PointsCntB_dd,        &
        PointsCntB_dd*1d2/(PointsCntB_coli+PointsCntB_dd)
    endif
    if( PointsCntC_dd.ne.0) then
    write(ncheckout_cll,111)  'C_cll',PointsCntC_dd,        &
        PointsCntC_dd*1d2/(PointsCntC_coli+PointsCntC_dd)
    endif
    if( PointsCntD_dd.ne.0) then
    write(ncheckout_cll,111)  'D_cll',PointsCntD_dd,        &
        PointsCntD_dd*1d2/(PointsCntD_coli+PointsCntD_dd)
    endif
    if( PointsCntE_dd.ne.0) then
    write(ncheckout_cll,111)  'E_cll',PointsCntE_dd,        &
        PointsCntE_dd*1d2/(PointsCntE_coli+PointsCntE_dd)
    endif
    if( PointsCntF_dd.ne.0) then
    write(ncheckout_cll,111)  'F_cll',PointsCntF_dd,        &
        PointsCntF_dd*1d2/(PointsCntF_coli+PointsCntF_dd)
    endif
    if( PointsCntG_dd.ne.0) then
    write(ncheckout_cll,111)  'G_cll',PointsCntG_dd,        &
        PointsCntG_dd*1d2/(PointsCntG_coli+PointsCntG_dd)
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_dd(i).ne.0) then
        write(ncheckout_cll,112)  'TN_cll',i,PointsCntTN_dd(i),    &
        PointsCntTN_dd(i)*1d2/(PointsCntTN_coli(i)+PointsCntTN_dd(i))
      endif
    end do

    if( PointsCntAten_dd.ne.0) then
    write(ncheckout_cll,111)  'Aten_cll',PointsCntAten_dd,        &
        PointsCntAten_dd*1d2/(PointsCntAten_coli+PointsCntAten_dd)
    endif
    if( PointsCntBten_dd.ne.0) then
    write(ncheckout_cll,111)  'Bten_cll',PointsCntBten_dd,        &
        PointsCntBten_dd*1d2/(PointsCntBten_coli+PointsCntBten_dd)
    endif
    if( PointsCntCten_dd.ne.0) then
    write(ncheckout_cll,111)  'Cten_cll',PointsCntCten_dd,        &
        PointsCntCten_dd*1d2/(PointsCntCten_coli+PointsCntCten_dd)
    endif
    if( PointsCntDten_dd.ne.0) then
    write(ncheckout_cll,111)  'Dten_cll',PointsCntDten_dd,        &
        PointsCntDten_dd*1d2/(PointsCntDten_coli+PointsCntDten_dd)
    endif
    if( PointsCntEten_dd.ne.0) then
    write(ncheckout_cll,111)  'Eten_cll',PointsCntEten_dd,        &
        PointsCntEten_dd*1d2/(PointsCntEten_coli+PointsCntEten_dd)
    endif
    if( PointsCntFten_dd.ne.0) then
    write(ncheckout_cll,111)  'Ften_cll',PointsCntFten_dd,        &
        PointsCntFten_dd*1d2/(PointsCntFten_coli+PointsCntFten_dd)
    endif
    if( PointsCntGten_dd.ne.0) then
    write(ncheckout_cll,111)  'Gten_cll',PointsCntGten_dd,        &
        PointsCntGten_dd*1d2/(PointsCntGten_coli+PointsCntGten_dd)
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_dd(i).ne.0) then
        write(ncheckout_cll,112)  'TNten_cll',i,PointsCntTNten_dd(i),    &
        PointsCntTNten_dd(i)*1d2/(PointsCntTNten_coli(i)+PointsCntTNten_dd(i))
      endif
    end do

    write(ncheckout_cll,220) 
220 format (/' Total numbers of calls of COLI/DD functions')

    if( PointsCntDB_coli+PointsCntDB_dd.ne.0) then
    write(ncheckout_cll,101)  'DB_cll',PointsCntDB_coli+PointsCntDB_dd
    endif
    if( PointsCntA_coli+PointsCntA_dd.ne.0) then
    write(ncheckout_cll,101)  'A_cll',PointsCntA_coli+PointsCntA_dd
    endif
    if( PointsCntB_coli+PointsCntB_dd.ne.0) then
    write(ncheckout_cll,101)  'B_cll',PointsCntB_coli+PointsCntB_dd
    endif
    if( PointsCntC_coli+PointsCntC_dd.ne.0) then
    write(ncheckout_cll,101)  'C_cll',PointsCntC_coli+PointsCntC_dd
    endif
    if( PointsCntD_coli+PointsCntD_dd.ne.0) then
    write(ncheckout_cll,101)  'D_cll',PointsCntD_coli+PointsCntD_dd
    endif
    if( PointsCntE_coli+PointsCntE_dd.ne.0) then
    write(ncheckout_cll,101)  'E_cll',PointsCntE_coli+PointsCntE_dd
    endif
    if( PointsCntF_coli+PointsCntF_dd.ne.0) then
    write(ncheckout_cll,101)  'F_cll',PointsCntF_coli+PointsCntF_dd
    endif
    if( PointsCntG_coli+PointsCntG_dd.ne.0) then
    write(ncheckout_cll,101)  'G_cll',PointsCntG_coli+PointsCntG_dd
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_coli(i)+PointsCntTN_dd(i).ne.0) then
        write(ncheckout_cll,102)  'TN_cll',i,PointsCntTN_coli(i)+PointsCntTN_dd(i)
      endif
    end do

    if( PointsCntDBten_coli+PointsCntDBten_dd.ne.0) then
    write(ncheckout_cll,101)  'DBten_cll',PointsCntDBten_coli+PointsCntDBten_dd
    endif
    if( PointsCntAten_coli+PointsCntAten_dd.ne.0) then
    write(ncheckout_cll,101)  'Aten_cll',PointsCntAten_coli+PointsCntAten_dd
    endif
    if( PointsCntBten_coli+PointsCntBten_dd.ne.0) then
    write(ncheckout_cll,101)  'Bten_cll',PointsCntBten_coli+PointsCntBten_dd
    endif
    if( PointsCntCten_coli+PointsCntCten_dd.ne.0) then
    write(ncheckout_cll,101)  'Cten_cll',PointsCntCten_coli+PointsCntCten_dd
    endif
    if( PointsCntDten_coli+PointsCntDten_dd.ne.0) then
    write(ncheckout_cll,101)  'Dten_cll',PointsCntDten_coli+PointsCntDten_dd
    endif
    if( PointsCntEten_coli+PointsCntEten_dd.ne.0) then
    write(ncheckout_cll,101)  'Eten_cll',PointsCntEten_coli+PointsCntEten_dd
    endif
    if( PointsCntFten_coli+PointsCntFten_dd.ne.0) then
    write(ncheckout_cll,101)  'Ften_cll',PointsCntFten_coli+PointsCntFten_dd
    endif
    if( PointsCntGten_coli+PointsCntGten_dd.ne.0) then
    write(ncheckout_cll,101)  'Gten_cll',PointsCntGten_coli+PointsCntGten_dd
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_coli(i)+PointsCntTNten_dd(i).ne.0) then
        write(ncheckout_cll,102)  'TNten_cll',i,PointsCntTNten_coli(i)+PointsCntTNten_dd(i)
      endif
    end do

    write(ncheckout_cll,320)
320 format (/' Numbers of comparisons of functions between COLI and DD')

    if(CheckCntDB_cll.ne.0) then
    write(ncheckout_cll,101)  'DB_cll',CheckCntDB_cll 
    endif
    do i=1,Nmax_cll
      if(CheckCnt_cll(i).ne.0) then
        write(ncheckout_cll,102)  'TN_cll',i,CheckCnt_cll(i)
      endif
    end do
    do i=1,Nmax_cll
      if(CheckCntten_cll(i).ne.0) then
        write(ncheckout_cll,102)  'TNten_cll',i,CheckCntten_cll(i)
      endif
    end do

    write(ncheckout_cll,330) checkacc_cll
330 format (/' Numbers of calls of COLLIER functions'/  &
        ' with difference between COLI and DD > checkacc_cll =',Es11.4)

    if(DiffCntDB_cll.ne.0) then
    write(ncheckout_cll,111)  'DB_cll',DiffCntDB_cll,  &
        DiffCntDB_cll*1d2/CheckCntDB_cll
    endif
    do i=1,Nmax_cll
      if(DiffCnt_cll(i).ne.0) then
        write(ncheckout_cll,112)  'TN_cll',i,DiffCnt_cll(i),  &
        DiffCnt_cll(i)*1d2/CheckCnt_cll(i)
      endif
    end do
    if(DiffCntEc_cll.ne.0) then
    write(ncheckout_cll,111)  'E_cll',DiffCntEc_cll,  &
        DiffCntEc_cll*1d2/CheckCnt_cll(5)
    endif
    do i=1,Nmax_cll
      if(DiffCntten_cll(i).ne.0) then
        write(ncheckout_cll,112)  'TNten_cll',i,DiffCntten_cll(i),  &
        DiffCntten_cll(i)*1d2/CheckCntten_cll(i)
      endif
    end do

    end if ! qopend_check

    ErrEventCnt(1)           = ErrEventCnt(1)           + 1
    ErrEventCnt(ErrFlag_cll) = ErrEventCnt(ErrFlag_cll) + 1
    AccEventCnt(1)           = AccEventCnt(1)           + 1
    AccEventCnt(AccFlag_cll) = AccEventCnt(AccFlag_cll) + 1

401 format(' #calls all          ','              = ',i20)
411 format(' #calls with errors of level ',i3,'   = ',i20,' or ',F10.5,' %')
421 format(' #events all         ','              = ',i20)
431 format(' #events with errors of level ',i3,'  = ',i20,' or ',F10.5,' %')

    write(nerrout_cll,400)
400 format (/' Numbers of errors in COLI functions')
    if(real(ErrCnt(1),kind(1d0)) /= 0d0) then
    do i=-10,0
      write(nerrout_cll,411)  i,ErrCntcoli(i),ErrCntcoli(i)/real(ErrCnt(1),kind(1d0))*1d2
    end do
    write(nerrout_cll,401)  ErrCnt(1)
    endif

    write(nerrout_cll,405)
405 format (/' Numbers of errors in DD functions')
    if(real(ErrCnt(1),kind(1d0)) /= 0d0) then
    do i=-10,0
      write(nerrout_cll,411)  i,ErrCntdd(i),ErrCntdd(i)/real(ErrCnt(1),kind(1d0))*1d2
    end do
    write(nerrout_cll,401)  ErrCnt(1)
    endif

    write(nerrout_cll,410)
410 format (/' Numbers of errors in COLLIER functions')
    if(real(ErrCnt(1),kind(1d0)) /= 0d0) then
    do i=-10,0
      write(nerrout_cll,411)  i,ErrCnt(i),ErrCnt(i)/real(ErrCnt(1),kind(1d0))*1d2
    end do
    write(nerrout_cll,401)  ErrCnt(1)
    endif

    write(nerrout_cll,415)
415 format (/' Numbers of errors in Events')
    if(real(ErrEventCnt(1),kind(1d0)) /= 0d0) then
    do i=-10,0
      write(nerrout_cll,431)  i,ErrEventCnt(i),ErrEventCnt(i)/real(ErrEventCnt(1),kind(1d0))*1d2
    end do
    write(nerrout_cll,421)  ErrEventCnt(1)
    endif


501 format(' #calls all          ','                = ',i20)
511 format(' #calls with accuracy of level ',i3,'   = ',i20,' or ',F10.5,' %')
521 format(' #events all         ','                = ',i20)
531 format(' #events with accuracy of level ',i3,'  = ',i20,' or ',F10.5,' %')

    write(ncpout_cll,510)
510 format (/' Numbers of COLLIER calls with accuracy levels')
    if(real(AccCnt(1),kind(1d0)) /= 0d0) then
    do i=-2,0
      write(ncpout_cll,511)  i,AccCnt(i),AccCnt(i)/real(AccCnt(1),kind(1d0))*1d2
    end do
    write(ncpout_cll,501)  AccCnt(1)
    endif

    write(ncpout_cll,500)
500 format (/' Numbers of Events with accuracy levels')
    if(real(AccEventCnt(1),kind(1d0)) /= 0d0) then
    do i=-2,0
      write(ncpout_cll,531)  i,AccEventCnt(i),AccEventCnt(i)/real(AccEventCnt(1),kind(1d0))*1d2
    end do
    write(ncpout_cll,521)  AccEventCnt(1)
!   write(ncpout_cll,521)  EventCnt_cll+1
    endif
    write(ncpout_cll,*) 





  end subroutine PrintStatistics_cll


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine PrintStatistics2_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PrintStatistics2_cll()

    integer :: i

101 format(' #calls ',A9,'          = ',i20)
102 format(' #calls ',A9,' (N = ',i1,')  = ',i20)
111 format(' #calls ',A9,'          = ',i20,' or ',F10.5,' %')
112 format(' #calls ',A9,' (N = ',i1,')  = ',i20,' or ',F10.5,' %')

    if (.not.Monitoring) then
      if (infoutlev_cll.ge.1) then
        write(ninfout_cll,*) 'COLLIER: CritPointsMonitor not initialized'
        write(ninfout_cll,*) '         no statistics available          '
      end if
      return
    endif

    if(ncpout2_cll.eq.closed_cll) return

    write(ncpout2_cll,90) 
90 format (//' Collier: Summary of critical points:')

    write(ncpout2_cll,100) 
100 format (/' Total numbers of calls of COLLIER functions')

    if( PointsCntA_cll.ne.0) then
    write(ncpout2_cll,101)  'A_cll',PointsCntA_cll
    endif
    if( PointsCntB_cll.ne.0) then
    write(ncpout2_cll,101)  'B_cll',PointsCntB_cll
    endif
    if( PointsCntC_cll.ne.0) then
    write(ncpout2_cll,101)  'C_cll',PointsCntC_cll
    endif
    if( PointsCntD_cll.ne.0) then
    write(ncpout2_cll,101)  'D_cll',PointsCntD_cll
    endif
    if( PointsCntE_cll.ne.0) then
    write(ncpout2_cll,101)  'E_cll',PointsCntE_cll
    endif
    if( PointsCntF_cll.ne.0) then
    write(ncpout2_cll,101)  'F_cll',PointsCntF_cll
    endif
    if( PointsCntG_cll.ne.0) then
    write(ncpout2_cll,101)  'G_cll',PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0) then
        write(ncpout2_cll,102)  'TN_cll',i,PointsCntTN_cll(i)
      endif
    end do

    if( PointsCntAten_cll.ne.0) then
    write(ncpout2_cll,101)  'Aten_cll',PointsCntAten_cll
    endif
    if( PointsCntBten_cll.ne.0) then
    write(ncpout2_cll,101)  'Bten_cll',PointsCntBten_cll
    endif
    if( PointsCntCten_cll.ne.0) then
    write(ncpout2_cll,101)  'Cten_cll',PointsCntCten_cll
    endif
    if( PointsCntDten_cll.ne.0) then
    write(ncpout2_cll,101)  'Dten_cll',PointsCntDten_cll
    endif
    if( PointsCntEten_cll.ne.0) then
    write(ncpout2_cll,101)  'Eten_cll',PointsCntEten_cll
    endif
    if( PointsCntFten_cll.ne.0) then
    write(ncpout2_cll,101)  'Ften_cll',PointsCntFten_cll
    endif
    if( PointsCntGten_cll.ne.0) then
    write(ncpout2_cll,101)  'Gten_cll',PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0) then
        write(ncpout2_cll,102)  'TNten_cll',i,PointsCntTNten_cll(i)
      endif
    end do

    write(ncpout2_cll,110) reqacc_coli
110 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than  reqacc_coli =',Es11.4)
    if(PointsCntA_cll.ne.0.and.AccPointsCntA_cll.ne.0) then
    write(ncpout2_cll,111)  'A_cll',AccPointsCntA2_cll,AccPointsCntA2_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.AccPointsCntB2_cll.ne.0) then
    write(ncpout2_cll,111)  'B_cll',AccPointsCntB2_cll,AccPointsCntB2_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.AccPointsCntC2_cll.ne.0) then
    write(ncpout2_cll,111)  'C_cll',AccPointsCntC2_cll,AccPointsCntC2_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.AccPointsCntD2_cll.ne.0) then
    write(ncpout2_cll,111)  'D_cll',AccPointsCntD2_cll,AccPointsCntD2_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.AccPointsCntE2_cll.ne.0) then
    write(ncpout2_cll,111)  'E_cll',AccPointsCntE2_cll,AccPointsCntE2_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.AccPointsCntF2_cll.ne.0) then
    write(ncpout2_cll,111)  'F_cll',AccPointsCntF2_cll,AccPointsCntF2_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.AccPointsCntG2_cll.ne.0) then
    write(ncpout2_cll,111)  'G_cll',AccPointsCntG2_cll,AccPointsCntG2_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.AccPointsCntTN2_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TN_cll',i,AccPointsCntTN2_cll(i),AccPointsCntTN2_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do

    if(PointsCntDBten_cll.ne.0.and.AccPointsCntDBten_cll.ne.0) then
    write(ncpout2_cll,111)  'DBten_cll',AccPointsCntDBten_cll,AccPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.AccPointsCntAten_cll.ne.0) then
    write(ncpout2_cll,111)  'Aten_cll',AccPointsCntAten_cll,AccPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.AccPointsCntBten_cll.ne.0) then
    write(ncpout2_cll,111)  'Bten_cll',AccPointsCntBten_cll,AccPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.AccPointsCntCten_cll.ne.0) then
    write(ncpout2_cll,111)  'Cten_cll',AccPointsCntCten_cll,AccPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.AccPointsCntDten_cll.ne.0) then
    write(ncpout2_cll,111)  'Dten_cll',AccPointsCntDten_cll,AccPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.AccPointsCntEten_cll.ne.0) then
    write(ncpout2_cll,111)  'Eten_cll',AccPointsCntEten_cll,AccPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.AccPointsCntFten_cll.ne.0) then
    write(ncpout2_cll,111)  'Ften_cll',AccPointsCntFten_cll,AccPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.AccPointsCntGten_cll.ne.0) then
    write(ncpout2_cll,111)  'Gten_cll',AccPointsCntGten_cll,AccPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.AccPointsCntTNten_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TNten_cll',i,AccPointsCntTNten_cll(i),AccPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do

    write(ncpout2_cll,130) sqrt(reqacc_coli)
130 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than ',  &
        ' sqrt(reqacc_coli) =',Es11.4)
    if(PointsCntA_cll.ne.0.and.sAccPointsCntA_cll.ne.0) then
    write(ncpout2_cll,111)  'A_cll',sAccPointsCntA2_cll,sAccPointsCntA2_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.sAccPointsCntB2_cll.ne.0) then
    write(ncpout2_cll,111)  'B_cll',sAccPointsCntB2_cll,sAccPointsCntB2_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.sAccPointsCntC2_cll.ne.0) then
    write(ncpout2_cll,111)  'C_cll',sAccPointsCntC2_cll,sAccPointsCntC2_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.sAccPointsCntD2_cll.ne.0) then
    write(ncpout2_cll,111)  'D_cll',sAccPointsCntD2_cll,sAccPointsCntD2_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.sAccPointsCntE2_cll.ne.0) then
    write(ncpout2_cll,111)  'E_cll',sAccPointsCntE2_cll,sAccPointsCntE2_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.sAccPointsCntF2_cll.ne.0) then
    write(ncpout2_cll,111)  'F_cll',sAccPointsCntF2_cll,sAccPointsCntF2_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.sAccPointsCntG2_cll.ne.0) then
    write(ncpout2_cll,111)  'G_cll',sAccPointsCntG2_cll,sAccPointsCntG2_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.sAccPointsCntTN2_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TN_cll',i,sAccPointsCntTN2_cll(i),sAccPointsCntTN2_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do

    if(PointsCntDBten_cll.ne.0.and.sAccPointsCntDBten_cll.ne.0) then
    write(ncpout2_cll,111)  'DBten_cll',sAccPointsCntDBten_cll,sAccPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.sAccPointsCntAten_cll.ne.0) then
    write(ncpout2_cll,111)  'Aten_cll',sAccPointsCntAten_cll,sAccPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.sAccPointsCntBten_cll.ne.0) then
    write(ncpout2_cll,111)  'Bten_cll',sAccPointsCntBten_cll,sAccPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.sAccPointsCntCten_cll.ne.0) then
    write(ncpout2_cll,111)  'Cten_cll',sAccPointsCntCten_cll,sAccPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.sAccPointsCntDten_cll.ne.0) then
    write(ncpout2_cll,111)  'Dten_cll',sAccPointsCntDten_cll,sAccPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.sAccPointsCntEten_cll.ne.0) then
    write(ncpout2_cll,111)  'Eten_cll',sAccPointsCntEten_cll,sAccPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.sAccPointsCntFten_cll.ne.0) then
    write(ncpout2_cll,111)  'Ften_cll',sAccPointsCntFten_cll,sAccPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.sAccPointsCntGten_cll.ne.0) then
    write(ncpout2_cll,111)  'Gten_cll',sAccPointsCntGten_cll,sAccPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.sAccPointsCntTNten_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TNten_cll',i,sAccPointsCntTNten_cll(i),sAccPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do

    write(ncpout2_cll,120) critacc_cll
120 format (/' Numbers of calls of COLLIER functions'/  &
        ' with an estimated accuracy worse than  critacc_coli =',Es11.4)
    
    if(PointsCntA_cll.ne.0.and.CritPointsCntA_cll.ne.0) then
    write(ncpout2_cll,111)  'A_cll',CritPointsCntA_cll,CritPointsCntA_cll/real(PointsCntA_cll,kind(1d0))*1d2
    endif
    if(PointsCntB_cll.ne.0.and.CritPointsCntB_cll.ne.0) then
    write(ncpout2_cll,111)  'B_cll',CritPointsCntB_cll,CritPointsCntB_cll/real(PointsCntB_cll,kind(1d0))*1d2
    endif
    if(PointsCntC_cll.ne.0.and.CritPointsCntC2_cll.ne.0) then
    write(ncpout2_cll,111)  'C_cll',CritPointsCntC2_cll,CritPointsCntC2_cll/real(PointsCntC_cll,kind(1d0))*1d2
    endif
    if(PointsCntD_cll.ne.0.and.CritPointsCntD2_cll.ne.0) then
    write(ncpout2_cll,111)  'D_cll',CritPointsCntD2_cll,CritPointsCntD2_cll/real(PointsCntD_cll,kind(1d0))*1d2
    endif
    if(PointsCntE_cll.ne.0.and.CritPointsCntE2_cll.ne.0) then
    write(ncpout2_cll,111)  'E_cll',CritPointsCntE2_cll,CritPointsCntE2_cll/real(PointsCntE_cll,kind(1d0))*1d2
    endif
    if(PointsCntF_cll.ne.0.and.CritPointsCntF2_cll.ne.0) then
    write(ncpout2_cll,111)  'F_cll',CritPointsCntF2_cll,CritPointsCntF2_cll*1d2/PointsCntF_cll
    endif
    if(PointsCntG_cll.ne.0.and.CritPointsCntG2_cll.ne.0) then
    write(ncpout2_cll,111)  'G_cll',CritPointsCntG2_cll,CritPointsCntG2_cll*1d2/PointsCntG_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTN_cll(i).ne.0.and.CritPointsCntTN2_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TN_cll',i,CritPointsCntTN2_cll(i),CritPointsCntTN2_cll(i)*1d2/PointsCntTN_cll(i)
      endif
    end do
    write(ncpout2_cll,*) 

    if(PointsCntDBten_cll.ne.0.and.CritPointsCntDBten_cll.ne.0) then
    write(ncpout2_cll,111)  'DBten_cll',CritPointsCntDBten_cll,CritPointsCntDBten_cll/real(PointsCntDBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntAten_cll.ne.0.and.CritPointsCntAten_cll.ne.0) then
    write(ncpout2_cll,111)  'Aten_cll',CritPointsCntAten_cll,CritPointsCntAten_cll/real(PointsCntAten_cll,kind(1d0))*1d2
    endif
    if(PointsCntBten_cll.ne.0.and.CritPointsCntBten_cll.ne.0) then
    write(ncpout2_cll,111)  'Bten_cll',CritPointsCntBten_cll,CritPointsCntBten_cll/real(PointsCntBten_cll,kind(1d0))*1d2
    endif
    if(PointsCntCten_cll.ne.0.and.CritPointsCntCten_cll.ne.0) then
    write(ncpout2_cll,111)  'Cten_cll',CritPointsCntCten_cll,CritPointsCntCten_cll/real(PointsCntCten_cll,kind(1d0))*1d2
    endif
    if(PointsCntDten_cll.ne.0.and.CritPointsCntDten_cll.ne.0) then
    write(ncpout2_cll,111)  'Dten_cll',CritPointsCntDten_cll,CritPointsCntDten_cll/real(PointsCntDten_cll,kind(1d0))*1d2
    endif
    if(PointsCntEten_cll.ne.0.and.CritPointsCntEten_cll.ne.0) then
    write(ncpout2_cll,111)  'Eten_cll',CritPointsCntEten_cll,CritPointsCntEten_cll/real(PointsCntEten_cll,kind(1d0))*1d2
    endif
    if(PointsCntFten_cll.ne.0.and.CritPointsCntFten_cll.ne.0) then
    write(ncpout2_cll,111)  'Ften_cll',CritPointsCntFten_cll,CritPointsCntFten_cll*1d2/PointsCntFten_cll
    endif
    if(PointsCntGten_cll.ne.0.and.CritPointsCntGten_cll.ne.0) then
    write(ncpout2_cll,111)  'Gten_cll',CritPointsCntGten_cll,CritPointsCntGten_cll*1d2/PointsCntGten_cll
    endif
    do i=1,Nmax_cll
      if(PointsCntTNten_cll(i).ne.0.and.CritPointsCntTNten_cll(i).ne.0) then
        write(ncpout2_cll,112)  'TNten_cll',i,CritPointsCntTNten_cll(i),CritPointsCntTNten_cll(i)*1d2/PointsCntTNten_cll(i)
      endif
    end do
    write(ncpout2_cll,*) 

501 format(' #calls all          ','                = ',i20)
511 format(' #calls with accuracy of level ',i3,'   = ',i20,' or ',F10.5,' %')
521 format(' #events all         ','                = ',i20)
531 format(' #events with accuracy of level ',i3,'  = ',i20,' or ',F10.5,' %')

    write(ncpout2_cll,510)
510 format (/' Numbers of COLLIER calls with accuracy levels')
    do i=-2,0
      write(ncpout2_cll,511)  i,AccCnt(i),AccCnt(i)/real(AccCnt(1),kind(1d0))*1d2
    end do
    write(ncpout2_cll,501)  AccCnt(1)

    write(ncpout2_cll,500)
500 format (/' Numbers of Events with accuracy levels')
    do i=-2,0
      write(ncpout2_cll,531)  i,AccEventCnt(i),AccEventCnt(i)/real(AccEventCnt(1),kind(1d0))*1d2
    end do
    write(ncpout2_cll,521)  AccEventCnt(1)
!   write(ncpout2_cll,521)  EventCnt_cll+1
    write(ncpout2_cll,*) 




  end subroutine PrintStatistics2_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function ContractLoStruc(Nm1,struc1,struc2,Gram)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function ContractLoStruc(Nm1,struc1,struc2,Gram) result(res)

    integer, intent(in) :: Nm1
    integer, intent(in) :: struc1(0:Nm1), Struc2(0:Nm1)
    integer :: struc1aux(0:Nm1), struc2aux(0:Nm1), struc2aux2(0:Nm1)
    double complex, intent(in) :: Gram(Nm1,Nm1)
    double complex :: res
    integer :: i,j,con,sum1,sum2,fac
    logical :: errorwriteflag,eflag

    res = 0d0
    
    sum1 = 2*struc1(0)
    sum2 = 2*struc2(0)
    do i=1,Nm1
      sum1 = sum1 + struc1(i)
      sum2 = sum2 + struc2(i)
    end do
    
    if (sum1.ne.sum2) then
      call SetErrFlag_coli(-10)
      call ErrOut_cll('ContractLoStruc',' inconsistent call',eflag)
      if(eflag) then
        write(nerrout_cll,*) ' ContractLoStruc: Lorentz structures struc1 and struc2 must be of equal rank!'
      end if
      return
    end if

    if (sum1.eq.0) then
      res = 1d0
      return
    end if
    
    con = -1
    do i=0,Nm1
      if (struc1(i).ge.1) then
        con = i
      end if
    end do

    struc1aux = struc1
    struc1aux(con) = struc1aux(con)-1

    if (con.ge.1) then

      ! contract p_con from T1 with g from T2
      if (struc2(0).ge.1) then
        struc2aux = struc2
        struc2aux(0) = struc2aux(0)-1
        struc2aux(con) = struc2aux(con)+1
        ! go on contracting recursively
        ! (factor struc2aux(con) because of symmetrization wrt. g and pi)
        res = res + struc2aux(con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
      end if

      ! contract p_con from T1 with all the pi from T2
      do i=1,Nm1
        if (struc2(i).ge.1) then
          struc2aux = struc2
          struc2aux(i) = struc2aux(i)-1
          ! go on contracting recursively
          res = res + Gram(i,con)*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
        end if
      end do


    else
        
      ! contract g from T1 with g from T2
      if (struc2(0).ge.1) then
        struc2aux = struc2
        struc2aux(0) = struc2aux(0)-1
        ! full contraction g^{mu,nu}.g_{mu,nu}
        ! tensor in D=4 dimensions g*g=4
        fac = 4
        do i=0,Nm1
          ! partial contration g^{mu,nu}.(pi_mu g_{nu,rho})
          ! or g^{mu,nu}.(g_{mu,rho}g_{nu,sigma})
          ! factor 2 for mu <--> nu
          fac = fac + 2*struc2aux(i)
        end do
        ! go on contracting recursively
        res = res + fac*ContractLoStruc(Nm1,struc1aux,struc2aux,Gram)
      end if

      ! contract g^{mu,nu} from T1 with pi,pj from T2
      do i=1,Nm1
        if (struc2(i).ge.1) then
          struc2aux = struc2
          struc2aux(i) = struc2aux(i)-1
          do j=1,Nm1
            if (struc2aux(j).ge.1) then
              struc2aux2 = struc2aux
              struc2aux2(j) = struc2aux2(j)-1
              ! go on contracting recursively
              res = res + Gram(i,j)*ContractLoStruc(Nm1,struc1aux,struc2aux2,Gram)
            end if
          end do
        end if
      end do

    end if


  end function ContractLoStruc






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function LoStrucConts(Nm1,rmax,Gram)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function LoStrucConts(Nm1,rmax,Gram) result (LoCons)

    integer, intent(in) :: Nm1,rmax
    double complex, intent(in) :: Gram(Nm1,Nm1)
    double complex :: LoCons(0:rmax/2,BinomTable(rmax,Nm1+rmax-1),0:rmax/2,BinomTable(rmax,Nm1+rmax-1),0:rmax)
    integer :: struc1(0:Nm1),struc2(0:Nm1),struc2aux(0:Nm1)
    integer :: r,i,j,i1,i2,i1aux,i2aux,i2aux2,n01,n02,con,fac,i20


    LoCons = 0d0
    LoCons(0,1,0,1,0) = 1d0

    do r=1,rmax
      do n01=0,r/2
        struc1(0) = n01
        do i1=1,BinomTable(r-2*n01,Nm1+r-2*n01-1)
          struc1(1:Nm1) = CalcCIndArr(Nm1,r-2*n01,i1)

          con = -1
          do i=0,Nm1
            if (struc1(i).ge.1) then
              con = i
            end if
          end do

          if (con.ge.1) then
            i1aux = DropCInd2(con,i1,r-2*n01,Nm1)

            do n02=0,r/2
              struc2(0) = n02

              do i2=1,BinomTable(r-2*n02,Nm1+r-2*n02-1)
                struc2(1:Nm1) = CalcCIndArr(Nm1,r-2*n02,i2)

                ! contract p_con from T1 with g from T2
                if (struc2(0).ge.1) then
                  ! (factor struc2(con)+1 because of symmetrization wrt. g and pi)
                  i2aux = AddToCInd(con,i2,r-2*n02,Nm1)
                  LoCons(n01,i1,n02,i2,r) = LoCons(n01,i1,n02,i2,r) +  &
                                            (struc2(con)+1)*LoCons(n01,i1aux,n02-1,i2aux,r-1)
                end if

                ! contract p_con from T1 with all the pi from T2
                do i=1,Nm1
                  if (struc2(i).ge.1) then
                    i2aux = DropCInd2(i,i2,r-2*n02,Nm1)
                    LoCons(n01,i1,n02,i2,r) = LoCons(n01,i1,n02,i2,r) +  & 
                                              Gram(i,con)*LoCons(n01,i1aux,n02,i2aux,r-1)
                  end if
                end do

              end do
            end do

          else
 
            do n02=0,r/2
              struc2(0) = n02
              do i2=1,BinomTable(r-2*n02,Nm1+r-2*n02-1)
                struc2(1:Nm1) = CalcCIndArr(Nm1,r-2*n02,i2)           

                ! contract g from T1 with g from T2
                if (struc2(0).ge.1) then
                  ! full contraction g^{mu,nu}.g_{mu,nu}
                  ! tensor in D=4 dimensions g*g=4
                  fac = 2  ! =4-2 to compensate for addational +2 in subsequent loop
                  do i=0,Nm1
                    ! partial contration g^{mu,nu}.(pi_mu g_{nu,rho})
                    ! or g^{mu,nu}.(g_{mu,rho}g_{nu,sigma})
                    ! factor 2 for mu <--> nu
                    fac = fac + 2*struc2(i)
                  end do
                  LoCons(n01,i1,n02,i2,r) = LoCons(n01,i1,n02,i2,r) +  &
                                            fac*LoCons(n01-1,i1,n02-1,i2,r-2)
                end if

                ! contract g^{mu,nu} from T1 with pi,pj from T2
                do i=1,Nm1
                  if (struc2(i).ge.1) then
                    struc2aux = struc2
                    struc2aux(i) = struc2aux(i)-1
                    i2aux = DropCInd2(i,i2,r-2*n02,Nm1) 
                    do j=1,Nm1
                      if (struc2aux(j).ge.1) then
                        i2aux2 =  DropCInd2(j,i2aux,r-2*n02,Nm1)
                        LoCons(n01,i1,n02,i2,r) = LoCons(n01,i1,n02,i2,r) +  &
                                                  Gram(i,j)*LoCons(n01-1,i1,n02,i2aux2,r-2)
                      end if
                    end do
                  end if
                end do

              end do
            end do

          end if

        end do
      end do
    end do

  end function LoStrucConts





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function CalcGram(Nm1,MomInv)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function CalcGram(N,MomInv) result (Gram)

    integer, intent(in) :: N
    double complex, intent(in) :: MomInv(BinomTable(2,N))
    double complex :: MomInvInf(BinomTable(2,N)),elimminf2_coli
    double complex :: Gram(N-1,N-1)
    integer :: i,j,cnt

    
    do i=1,BinomTable(2,N)
      MomInvInf(i) = elimminf2_coli(MomInv(i))
    end do
    
    
    cnt = 1
    do i=1,N/2
      Gram(i,i) = MomInvInf(cnt)
      cnt = cnt+1

      do j=i+1,N-1
        Gram(j-i,j) = MomInvInf(cnt)
        cnt = cnt+1
      end do
      if (cnt.gt.BinomTable(2,N)) exit

      Gram(N-i,N-i) = MomInvInf(cnt)
      cnt = cnt+1

      do j=1,i-1
        Gram(j,N-i+j) = MomInvInf(cnt)
        cnt = cnt+1
      end do
    end do

    do i=1,N-1
      do j=i+1,N-1
        Gram(i,j) = -(Gram(i,j)-Gram(i,i)-Gram(j,j))/2d0
        Gram(j,i) = Gram(i,j)
      end do
    end do

  end function CalcGram

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function DiLog_cll(z,eps)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function DiLog_cll(z,eps) result (DiLog)

    double complex, intent(in) :: z
    double precision, optional, intent(in) :: eps
    double complex :: DiLog,cspenc_coli

    if(present(eps)) then
      DiLog = cspenc_coli(z,eps)
    else
      DiLog = cspenc_coli(z,0d0)
    endif
   
    
  end function DiLog_cll


end module collier_aux




