!!
!!  File coli_aux2.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ***********************
!  *  module coli_aux2   *
!  *   by Lars Hofer     *
!  *********************** 
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "global_coli.h"

module coli_aux2

  use master

  implicit none

  integer :: nerrout_coli=6,ninfout_coli=6,ncpout_coli,nstatsout_coli
  integer :: errflag_coli,ErrCnt_coli,CritPointsCnt_coli,erroutlev_coli
  integer :: MaxErrOut_coli=100
  integer :: stdout_coli=6,mode_coli=0,closed_coli=-999    
  double precision :: dprec_coli, reqacc_coli, critacc_coli

  double precision :: acc_inf, acc_def_B, acc_def_C0, acc_def_D0
  double precision :: acc_req_C, acc_req_D, acc_req_CinD
  double precision :: impest_D=1d1, impest_C=1d1, impest_Dgy=1d2
  integer :: rmax_B, rmax_C, rmax_D
  logical :: InfLev_coli

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !    subroutine initcoli_in_collier
  !    fixing of default values for various variables                       *
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initcoli_in_collier

    implicit   none
!    logical    init
!
!    data init /.false./
!    save init
!    
!    if (init) return
!    init = .true.
    
  ! scale factor for *small* masses masses
    call setminfscale2_coli(1d0)
#ifdef SING
  ! shift of mass-singular squared logarithms
    call setshiftms2_coli(0d0)
#endif

  
  ! estimate of CPU precision 
  !    calacc = dprec_coli
  ! infinitesimal parameter
  !    eps    = dprec_coli/4d0
  ! size of imaginary parts below which explicit ieps take over
  !    impacc = dprec_coli/4d4
  !  done via GetCPUprec_cll
  !  call setprecpars_coli(dprec_coli)

  ! infinitesimal accuracy 
    acc_inf = 1d40

!    acc_min_C = 1d0
!    acc_min_D = 1d0


  end subroutine initcoli_in_collier



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitErrCnt_coli(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitErrCnt_coli(val)

    integer, intent(in) :: val

    ErrCnt_coli = val

  end subroutine InitErrCnt_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCritPointsCnt_coli(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCritPointsCnt_coli(val)

    integer, intent(in) :: val

    CritPointsCnt_coli = val

  end subroutine InitCritPointsCnt_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetErrFlag_coli(err)
  !
  !  -1 Check failed
  !  -4 Argument on cut       (only if CHECK set)
  !  -5 Critical event
  !  -6 No reduction method works
  !  -6 momenta not 4-dimensional
  !  -7 specific numerical problem
  !  
  ! -10 Case not supported/implemented
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetErrFlag_coli(err)

    integer, intent(in) :: err

    ErrFlag_coli = err

  end subroutine SetErrFlag_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getErrFlag_coli(err)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getErrFlag_coli(err)

    integer, intent(out) :: err

    err = ErrFlag_coli

  end subroutine getErrFlag_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine setnerrout_coli(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine setnerrout_coli(nerrout)

    integer, intent(in) :: nerrout

    nerrout_coli = nerrout

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: nerrout_coli set to = ',nerrout_coli
    endif

  end subroutine setnerrout_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine setncpout_coli(ncpout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine setncpout_coli(ncpout)

    integer, intent(in) :: ncpout

    ncpout_coli = ncpout

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: ncpout_coli set to = ',ncpout_coli
    endif

  end subroutine setncpout_coli 





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine setninfout_coli(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine setninfout_coli(ninfout)

    integer, intent(in) :: ninfout

    ninfout_coli = ninfout

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: ninfout_coli set to = ',ninfout_coli
    endif

  end subroutine setninfout_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine setnstatsout_coli(nstatsout) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine setnstatsout_coli(nstatsout)

    integer, intent(in) :: nstatsout

    nstatsout_coli = nstatsout

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: nstatsout_coli set to = ',nstatsout_coli
    endif

  end subroutine setnstatsout_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxErrOut_coli(errmax) 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxErrOut_coli(errmax)

    integer, intent(in) :: errmax

    maxErrOut_coli = errmax

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: maxErrOut_coli set to = ',maxErrOut_coli
    endif

  end subroutine SetMaxErrOut_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetErroutlev_coli(erroutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetErroutlev_coli(erroutlev)

    integer, intent(in) :: erroutlev

    erroutlev_coli = erroutlev
    

  end subroutine SetErroutlev_coli
  
  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine ErrOut_coli(sub,err,flag)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Suppression of output must be implemented in calling routines!

  subroutine ErrOut_coli(sub,err,flag)

    character(len=*), intent(in) :: sub, err
!    integer, parameter :: maxErrOut=100
    logical, intent(out) :: flag
  
    flag = .false.
    if(erroutlev_coli.eq.0) return

    ErrCnt_coli = ErrCnt_coli + 1  
    if (ErrCnt_coli.le.maxErrOut_coli) then
      write(nerrout_coli,*)
      write(nerrout_coli,*)
      write(nerrout_coli,*)
      write(nerrout_coli,*) '***********************************************************'
      write(nerrout_coli,*) 'ERROR NO.', ErrCnt_coli
      write(nerrout_coli,*) 'in routine: ', trim(sub)
      write(nerrout_coli,*) trim(err)
      call WriteMaster_cll(nerrout_coli)
      flag = .true.
    elseif (ErrCnt_coli.eq.maxErrOut_coli+1) then
      write(nerrout_coli,*)
      write(nerrout_coli,*)
      write(nerrout_coli,*)
      write(nerrout_coli,*) '***********************************************************'
      write(nerrout_coli,*)
      write(nerrout_coli,*) ' Further output of Errors will be suppressed '
      write(nerrout_coli,*)
    endif

  end subroutine ErrOut_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CritPointsOut_coli(sub,acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CritPointsOut_coli(sub,acc)

    character(len=*), intent(in) :: sub
    double precision, intent(in) :: acc
    integer :: i

#include "common_coli.h"

    CritPointsCnt_coli = CritPointsCnt_coli + 1    

    write(ncpout_coli,*)
    write(ncpout_coli,*)
    write(ncpout_coli,*)
    write(ncpout_coli,*) '***********************************************************'
    write(ncpout_coli,*) 'Critical Point NO.', CritPointsCnt_coli
    write(ncpout_coli,*) 'in integral: ', trim(sub)
    write(ncpout_coli,*) 'estimated accuracy: ', acc
    write(ncpout_coli,*) '-----------------------------------------------------------'
    write(ncpout_coli,*) 'GLOBAL PARAMETERS:'
    write(ncpout_coli,*) 'muUV2 =     ', muuv2
    write(ncpout_coli,*) 'muIR2 =     ', muir2
#ifdef SING
    write(ncpout_coli,*) 'deltaUV =   ', deltauv
    write(ncpout_coli,*) 'deltaIR1 =  ', delta1ir
    write(ncpout_coli,*) 'deltaIR2 =  ', delta2ir
#endif
    write(ncpout_coli,*) 'nminf =     ', ncoliminf
    do i=1,ncoliminf
      write(ncpout_coli,*) 'minf2 =     ', i, coliminf2(i)
    end do
    write(ncpout_coli,*) 'dprec =     ', dprec_coli
    write(ncpout_coli,*) 'reqacc =    ', reqacc_coli
    write(ncpout_coli,*) 'critacc =   ', critacc_coli
    write(ncpout_coli,*) 'ErrFlag =   ', ErrFlag_coli
!   write(ncpout_coli,*) '------------------------------------------------------------'
    call WriteMaster_cll(ncpout_coli)

  end subroutine CritPointsOut_coli








  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMode_coli(mode)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetMode_coli(mode)

    integer, intent(in) :: mode

    Mode_coli = mode
    if(mode.lt.1) then 
      impest_C = 1d1
      impest_D = 1d1
      impest_Dgy = 1d2
    else
      impest_C = 1d0
      impest_D = 1d0
      impest_Dgy = 1d0
    end if

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: Mode_coli set to = ',Mode_coli
    endif

  end subroutine SetMode_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine setprec_coli(dprec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine setprec_coli(dprec)

    double precision, intent(in) :: dprec

    dprec_coli = dprec

! adapted to precision of D0 and C0
    acc_def_B = dprec_coli
    acc_def_C0 = 1d1*dprec_coli
    acc_def_D0 = 1d1*dprec_coli

    call setprecpars_coli(dprec)

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: dprec_coli set to = ',dprec_coli
    endif

  end subroutine setprec_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAcc_coli(reqacc,critacc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetAcc_coli(reqacc,critacc)

    double precision, intent(in) :: reqacc,critacc

    reqacc_coli = reqacc
    acc_req_C = reqacc
    acc_req_CinD = reqacc/1d1
    acc_req_D = reqacc

    critacc_coli = critacc

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: reqacc_coli set to  = ',reqacc_coli
      write(ninfout_coli,*)  'COLI: critacc_coli set to = ',critacc_coli
    endif

  end subroutine SetAcc_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetReqAcc_coli(reqacc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetReqAcc_coli(reqacc)

    double precision, intent(in) :: reqacc

    reqacc_coli = reqacc
    acc_req_C = reqacc
    acc_req_CinD = reqacc/1d1
    acc_req_D = reqacc

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: reqacc_coli set to  = ',reqacc_coli
    endif

  end subroutine SetReqAcc_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCritAcc_coli(critacc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetCritAcc_coli(critacc)

    double precision, intent(in) :: critacc

    critacc_coli = critacc

    if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
      write(ninfout_coli,*)  'COLI: critacc_coli set to = ',critacc_coli
    endif

  end subroutine SetCritAcc_coli





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRitmax_coli(ritmax_B,ritmax_C,ritmax_D)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetRitmax_coli(ritmax_B,ritmax_C,ritmax_D)

    integer, intent(in) :: ritmax_B,ritmax_C, ritmax_D

    if (ritmax_D.lt.6) then
      if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
        write(ninfout_coli,*) 'COLI: SetRitmax_coli'
        write(ninfout_coli,*) 'rmax_D has to be at least 6 --> it is set to 6'
      end if
      rmax_D = 6
    else
      rmax_D = ritmax_D
      if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
        write(ninfout_coli,*)  'COLI: rmax_D set to = ',rmax_D
      endif
    end if

    if (ritmax_C.le.rmax_D) then
      if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
        write(ninfout_coli,*) 'COLI: SetRitmax_coli'
        write(ninfout_coli,*) 'rmax_C has to be larger than rmax_D --> it is set to rmax_D+1'
      end if
      rmax_C = rmax_D+1
    else
      rmax_C = ritmax_C
      if(InfLev_coli.and.ninfout_coli.ne.closed_cll)then
        write(ninfout_coli,*)  'COLI: rmax_C set to = ',rmax_C
      endif
    end if

    if (ritmax_B.le.rmax_C) then
      if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
        write(ninfout_coli,*) 'COLI: SetRitmax_coli'
        write(ninfout_coli,*) 'rmax_B has to be larger than rmax_C --> it is set to rmax_C+1'
      end if
      rmax_B = rmax_C+1
    else
      rmax_B = ritmax_B
      if(InfLev_coli.and.ninfout_coli.ne.closed_coli)then
        write(ninfout_coli,*)  'COLI: rmax_B set to = ',rmax_B
      endif
    end if


  end subroutine SetRitmax_coli

 
end module coli_aux2
