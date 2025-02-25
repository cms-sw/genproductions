!!
!!  File collier_init.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!#define CritPointsCOLI 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *************************
!  *  module collier_init  *
!  *     by Lars Hofer     *
!  ************************* 
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




module collier_init


  use collier_global
  use collier_aux
  use combinatorics
  use cache
  use coli_aux2
  use coli_stat
  use InitTensors

  implicit none
  interface SetMaxCheck_cll
    module procedure SetMaxCheckN_cll,SetMaxCheckArray_cll
  end interface SetMaxCheck_cll
  interface SetMaxCritPoints_cll
    module procedure SetMaxCritPointsN_cll,SetMaxCritPointsArray_cll
  end interface SetMaxCritPoints_cll
  
  
  character(len=230) :: foldername_cll
!  logical :: qopened_critcoli,qopened_crit,qopened_crit2,qopened_statscoli

contains



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function IsInitialized_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function IsInitialized_cll()

    logical :: IsInitialized_cll

    IsInitialized_cll = initialized_cll

  end function IsInitialized_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Init_cll(Nmax,rmax,folder_name,noreset)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Init_cll(Nmax,rmax,folder_name,noreset)

    integer, intent(in) :: Nmax
    integer, optional, intent(in) :: rmax
    character(len=*), optional, intent(in) :: folder_name
    logical, optional, intent(in) :: noreset
    integer :: mode,erroutlev,infoutlev,nminf,ritmax
    integer :: ritmaxB,ritmaxC,ritmaxD
    double precision :: muUV2, muIR2
    double precision :: deltaUV, deltaIR1, deltaIR2
    double complex, allocatable :: minf2(:)
    double precision :: acc0, acc1, acc2
    integer :: i,tenred,nchan,rmax0
    logical :: reset
    character(len=*),parameter :: fmt90 = "(A40,I10)"
    character(len=*),parameter :: fmt91 = "(A40,Es17.10)"
    character(len=*),parameter :: fmt95 = "(A47,I10)"
    character(len=*),parameter :: fmt98 = &
        "(7x,'minf2_cll(',i2,') = ',Es17.10)"
    character(len=*),parameter :: fmt96 = &
        "(7x,'cache no.',i2,': only internal calls cached')"
    character(len=*),parameter :: fmt97 = &
        "(7x,'cache no.',i2,': external and internal calls cached')"

    if (present(noreset)) then 
      if(noreset) then
        reset = .false.
        if (.not.initialized_cll) then
          reset = .true.
        else
          if (present(folder_name)) then
            if (nofiles_cll) then
              if (len(trim(folder_name)).ne.0) then
                reset = .true.
                call Reset_cll
              end if
            else
              if (len(trim(folder_name)).eq.0) then
                reset = .true.
                call Reset_cll
              end if
            end if
          end if
        end if
      else
        if (initialized_cll) call Reset_cll
        reset = .true.
      end if
    else
      if (initialized_cll) call Reset_cll
      reset = .true.
    end if

    if (reset) then
      if (present(folder_name)) then
        if (len(trim(folder_name)).eq.0) then
          erroutlev = 0
          infoutlev = 0
          nofiles_cll = .true.
        else
          erroutlev = 1
          infoutlev = 2
          foldername_cll = trim(folder_name)
          nofiles_cll = .false.
        end if
      else
        erroutlev = 1
        infoutlev = 2
        foldername_cll = "output_cll"
        nofiles_cll = .false.
      end if
    else
      call GetErrOutLev_cll(erroutlev)
      call GetInfoutlev_cll(infoutlev)
    end if
    
    Nmax_cll = Nmax
    if(present(rmax)) then
      rmax0 = rmax
    else
      rmax0 = Nmax
    end if
    rmax_cll = rmax0
    
    call SetInfOutLev_cll(0)
    call SetErrOutLev_cll(0)
 
    call InitCheckCnt_cll(.not.reset)
    call InitMaxCheck_cll(.not.reset)
    if(.not.reset) then
      if (Monitoring) then
        call InitPointsCnt_cll(.not.reset)
        call initMaxCritPoints_cll(.not.reset)        
      end if
    else
      Monitoring=.false.   
    end if
    
    if (reset) then
      call InitOutChan_cp_cll
      call InitOutChan_cll      !  produces output for nerrout_coli = closed_coli
      
      call InitCheckCntDB_cll
      call InitMaxCheckDB_cll
      
      call SetMaxErrOut_cll(100)
      call SetMaxErrOutCOLI_cll(100)
      call SetMaxErrOutDD_cll(100)
      call SetMaxInfOut_cll(1000)

      if (erroutlev.ge.1) then
! set output-file for potential errors
!       write(*,*) "mkdir -p ",trim(foldername_cll)
!       call execute_command_line('mkdir -p '//trim(foldername_cll))
        call make_dir(trim(foldername_cll))
        
        call InitErrCnt_cll(0)
        call OpenErrOutFile_cll(trim(foldername_cll)//'/ErrOut.cll')
        
        call InitErrCntCOLI_cll()
!      call OpenErrOutFileCOLI_cll(trim(foldername_cll)//'/ErrOut.coli')
!
        call InitErrCntDD_cll()
!      call OpenErrOutFileDD_cll(trim(foldername_cll)//'/ErrOut.dd')
      end if

      if (infoutlev.ge.1) then
        call OpenInfOutFile_cll(trim(foldername_cll)//'/InfOut.cll')
        
! no output from COLI  (comes via COLLIER)
        call SetInfOutlevCOLI_cll(0)
        
        call InitInfCnt_cll(0)
 
      end if
    end if

    ! default values.
    mode = 1

    muUV2 = 1d0
    muIR2 = 1d0

    deltaUV = 0d0
    deltaIR1 = 0d0
    deltaIR2 = 0d0
    
    ritmax = max(14,rmax0+4-Nmax)
    ritmax = max(16,rmax0+4-Nmax)          !  changed 29.07.2019
    ritmaxD = ritmax
    ritmaxC = ritmax+2
    ritmaxB = ritmax+4

    ! initialise COLI
    call initcoli_in_collier()
    ! set global parameters for DD
    call InitGlobalDD_cll(Nmax,ritmax)
    call DDsetcout_on(.false.)

    nminf = 0
    if (allocated(minf2)) then
      deallocate(minf2)
    end if
    allocate(minf2(nminf))

! required accuracy
    acc0 = 1d-8
! critical accuracy
    acc1 = 1d-1  !CritPoints
! check accuracy  
    acc2 = 1d-2   

    ! COLLIER mode
    call SetMode_cll(mode)

    ! set UV and IR parameters
    call SetMuUV2_cll(muUV2)
    call SetMuIR2_cll(muIR2)
    call SetDeltaUV_cll(deltaUV)
    call SetDeltaIR_cll(deltaIR1,deltaIR2)

    ! specify infinitesimal mass regulators
    call Setminf2_cll(nminf,minf2)

    ! CPU precision and accuracy
    call GetCPUprec_cll()
    ! call SetAccuracy_cll(acc0,acc1,acc2)
    call SetReqAcc_cll(acc0)
    call SetCritAcc_cll(acc1)
    call SetCheckAcc_cll(acc2)

    ! set maximum number of rank
    call SetRitmax_cll(ritmax)

    ! initialize Cache-system
    call InitCacheSystem_cll(0,Nmax)

    ! initialize table of binomial coefficients
    call SetBinomTable(rmax0+max(Nmax-2,4))

    ! initialization for tensors
    call SetIndCombisEq(Nmax-1,rmax0)
    call SetAddToCInd(Nmax-1,rmax0)
    call SetDropCInd(Nmax-1,rmax0)
    call SetDropCInd2(Nmax-1,rmax0)
    call init_tables2(Nmax-1,rmax0)
    
!    ! initialize table of binomial coefficients
!    call SetBinomTable(2*Nmax)
!
!    ! initialization for tensors
!    call SetIndCombisEq(Nmax,Nmax)
!    call SetAddToCInd(Nmax,Nmax)
!    call SetDropCInd(Nmax,Nmax)
!    call SetDropCInd2(Nmax,Nmax)
!    call init_tables2(Nmax,Nmax)    

    ! choose if UV poles are calculated completely
    ! call SwitchOffCalcUV_cll()
    call SwitchOnCalcUV_cll()

    ! choose to include IR rational terms
    call SwitchOnIRrational_cll()

    ! choose direct tensor reduction for N>=6
    call SwitchOnTenRed_cll()

    ! initialize counter PS points
    call InitEventCnt_cll()

    ! stop if fatal error occurs
    call SetErrStop_cll(-8)

!    call AddMinf2_cll(dcmplx(0.001d0))

    ! COLLIER has been initialized
    initialized_cll = .true.
    call SetInfOutLev_cll(infoutlev)
    call SetErrOutLev_cll(erroutlev)

    if (reset) then

    ! set standard output for infos
    call WriteIntro_cll(stdout_cll)
    if (infoutlev.ge.1) then
      write(unit=stdout_cll,fmt=*) '                                                          '
      write(unit=stdout_cll,fmt=*) '***********************************************************'
      write(unit=stdout_cll,fmt=*) '                                                           '
      write(unit=stdout_cll,fmt=*) '  COLLIER: information on settings and internal parameters '
      write(unit=stdout_cll,fmt=*) '    is written to the file ',trim(foldername_cll)//'/InfOut.cll'
      write(unit=stdout_cll,fmt=*) '                                                           '
      write(unit=stdout_cll,fmt=*) '***********************************************************'
      write(unit=stdout_cll,fmt=*) '                                                           '

      ! add here all the output for the default initialisation
      write(unit=ninfout_cll,fmt=*) '                                                          '
      write(unit=ninfout_cll,fmt=*) '***********************************************************'
      write(unit=ninfout_cll,fmt=*) '  Default initialization of COLLIER:                     '
      write(unit=ninfout_cll,fmt=fmt90) '    COLLIER mode: mode_cll =           ',mode_cll
      select case (mode_cll)
      case (1)
        write(ninfout_cll,*) '      -> use COLI implementation'
      case (2)
        write(ninfout_cll,*) '      -> use DD implementation'
      case (3)
        write(ninfout_cll,*) '      -> check COLI against DD implementation'
      end select
      write(unit=ninfout_cll,fmt=*) '-----------------------------------------------------------'
      write(unit=ninfout_cll,fmt=*) '    internal parameters:                                   '
      write(unit=ninfout_cll,fmt=fmt90) '    maximal degree:     Nmax         = ',Nmax
      write(unit=ninfout_cll,fmt=fmt91) '    UV scale:           muUV2        = ',muUV2
      write(unit=ninfout_cll,fmt=fmt91) '    IR scale:           muIR2        = ',muIR2
      write(unit=ninfout_cll,fmt=fmt91) '    UV pole:            deltaUV      = ',deltaUV 
      write(unit=ninfout_cll,fmt=fmt91) '    single IR pole:     deltaIR1     = ',deltaIR1 
      write(unit=ninfout_cll,fmt=fmt91) '    double IR pole:     deltaIR2     = ',deltaIR2 
      write(unit=ninfout_cll,fmt=fmt91) '    target precision:   reqacc_cll   = ',reqacc_cll 
      write(unit=ninfout_cll,fmt=fmt91) '    critical precision: critacc_cll  = ',critacc_cll 
      write(unit=ninfout_cll,fmt=fmt91) '    check precision:    checkacc_cll = ',checkacc_cll 
      write(unit=ninfout_cll,fmt=fmt91) '    est. CPU precision: dprec_cll    = ',dprec_cll 
      write(unit=ninfout_cll,fmt=fmt90) '    maximal rank of Bs: ritmaxB      = ',ritmaxB
      write(unit=ninfout_cll,fmt=fmt90) '    maximal rank of Cs: ritmaxC      = ',ritmaxC
      write(unit=ninfout_cll,fmt=fmt90) '    maximal rank of Ds: ritmaxD      = ',ritmaxD
      write(unit=ninfout_cll,fmt=fmt90) '    stop forced for ErrorStop_cll   <= ',ErrorStop_cll
      write(unit=ninfout_cll,fmt=*) '-----------------------------------------------------------'

    if (allocated(minf2_cll)) then
        write(unit=ninfout_cll,fmt=*) '    list of infinitesimal masses:'
      do i=1,size(minf2_cll)
        write(unit=ninfout_cll,fmt=fmt98) i,real(minf2_cll(i))
      end do
    end if
  
         
    if (.not.allocated(minf2_cll)) then
        write(unit=ninfout_cll,fmt=*) '    list of infinitesimal masses cleared'
    end if


      if (IR_rational_terms_cll) then
        write(unit=ninfout_cll,fmt=*) '    IR rational terms switched on'
      else
        write(unit=ninfout_cll,fmt=*) '    IR rational terms switched off'
      end if

      if (CalcUV_cll) then
        write(unit=ninfout_cll,fmt=*) '    UV terms for tensors switched on'
      else
        write(unit=ninfout_cll,fmt=*) '    UV terms for tensors switched off'
      end if

      if (tenred_cll.eq.never_tenred_cll) then
        write(unit=ninfout_cll,fmt=*) '    direct tensor reduction switched off'  
      else
        write(unit=ninfout_cll,fmt=fmt90) '    direct tensor reduction for   N >= ',tenred_cll
      end if

      write(unit=ninfout_cll,fmt=*) '-----------------------------------------------------------'
      if (use_cache_system) then
        write(unit=ninfout_cll,fmt=*) '    cache system switched on'
        write(unit=ninfout_cll,fmt=fmt90) '    initialized caches: ncache_max   = ',ncache_max
        do i=1,ncache_max
          if (cache_mode(i).eq.-1) then
            write(unit=ninfout_cll,fmt=fmt96) i     
          else
            write(unit=ninfout_cll,fmt=fmt97) i
          end if
        end do
      else
        write(unit=ninfout_cll,fmt=*) '    cache system switched off'
      end if

      write(unit=ninfout_cll,fmt=*) '                                                           '
      write(unit=ninfout_cll,fmt=*) '***********************************************************'
      write(unit=ninfout_cll,fmt=*) '                                                           '     
    end if
    
    end if
    
  end subroutine Init_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Reset_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Reset_cll
    logical :: qopened


    if (.not.nofiles_cll) then
      if(ninfout_cll.ne.closed_cll) then
        inquire(ninfout_cll, opened=qopened)
        if(qopened.and.ninfout_cll.ne.stdout_cll) close(unit=ninfout_cll)
      end if

      if(ninfoutcoli_cll.ne.closed_cll) then
        inquire(ninfoutcoli_cll, opened=qopened)
        if(qopened.and.ninfoutcoli_cll.ne.stdout_cll) close(unit=ninfoutcoli_cll)
      end if

      if(nerrout_cll.ne.closed_cll) then
        inquire(nerrout_cll, opened=qopened)
        if(qopened.and.nerrout_cll.ne.stdout_cll) close(unit=nerrout_cll)
      end if

      if(nerroutcoli_cll.ne.closed_cll) then
        inquire(nerroutcoli_cll, opened=qopened)
        if(qopened.and.nerroutcoli_cll.ne.stdout_cll) close(unit=nerroutcoli_cll)
      end if
    
      if(nerroutdd_cll.ne.closed_cll) then
        inquire(nerroutdd_cll, opened=qopened)
        if(qopened.and.nerroutdd_cll.ne.stdout_cll) close(unit=nerroutdd_cll)
      end if
    
      if(ncheckout_cll.ne.closed_cll) then
        inquire(ncheckout_cll, opened=qopened)
        if(qopened.and.ncheckout_cll.ne.stdout_cll) close(unit=ncheckout_cll)
      end if

      if(ncpout_cll.ne.closed_cll) then
        inquire(ncpout_cll, opened=qopened)
        if(qopened.and.ncpout_cll.ne.stdout_cll) close(unit=ncpout_cll)
      end if

      if(ncpout2_cll.ne.closed_cll) then
        inquire(ncpout2_cll, opened=qopened)
        if(qopened.and.ncpout2_cll.ne.stdout_cll) close(unit=ncpout2_cll)
      end if

      if(ncpoutcoli_cll.ne.closed_cll) then
        inquire(ncpoutcoli_cll, opened=qopened)
        if(qopened.and.ncpoutcoli_cll.ne.stdout_cll) close(unit=ncpoutcoli_cll)
      end if

      if(nstatsoutcoli_cll.ne.closed_cll) then
        inquire(nstatsoutcoli_cll, opened=qopened)
        if(qopened.and.nstatsoutcoli_cll.ne.stdout_cll) close(unit=nstatsoutcoli_cll)
      end if
    end if
    
    if(allocated(minf2_cll)) deallocate(minf2_cll)
    if(allocated(PointsCntTN_cll)) deallocate(PointsCntTN_cll)
    if(allocated(CritPointsCntTN_cll)) deallocate(CritPointsCntTN_cll)
    if(allocated(AccPointsCntTN_cll)) deallocate(AccPointsCntTN_cll)
    if(allocated(sAccPointsCntTN_cll)) deallocate(sAccPointsCntTN_cll)
    if(allocated(PointsCntTN2_cll)) deallocate(PointsCntTN2_cll)
    if(allocated(CritPointsCntTN2_cll)) deallocate(CritPointsCntTN2_cll)
    if(allocated(AccPointsCntTN2_cll)) deallocate(AccPointsCntTN2_cll)
    if(allocated(sAccPointsCntTN2_cll)) deallocate(sAccPointsCntTN2_cll)
    if(allocated(PointsCntTNten_cll)) deallocate(PointsCntTNten_cll)
    if(allocated(CritPointsCntTNten_cll)) deallocate(CritPointsCntTNten_cll)
    if(allocated(AccPointsCntTNten_cll)) deallocate(AccPointsCntTNten_cll)
    if(allocated(sAccPointsCntTNten_cll)) deallocate(sAccPointsCntTNten_cll)
    if(allocated(PointsCntTN_coli)) deallocate(PointsCntTN_coli)
    if(allocated(PointsCntTN_dd)) deallocate(PointsCntTN_dd)
    if(allocated(PointsCntTNten_coli)) deallocate(PointsCntTNten_coli)
    if(allocated(PointsCntTNten_dd)) deallocate(PointsCntTNten_dd)
    if(allocated(MaxCheck_cll)) deallocate(MaxCheck_cll)
    if(allocated(CheckCnt_cll)) deallocate(CheckCnt_cll)
    if(allocated(DiffCnt_cll)) deallocate(DiffCnt_cll)
    if(allocated(CheckCntten_cll)) deallocate(CheckCntten_cll)
    if(allocated(DiffCntten_cll)) deallocate(DiffCntten_cll)

  end subroutine Reset_cll


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitEvent_cll(Ncache)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitEvent_cll(Ncache)

    integer, optional, intent(in) :: Ncache
    integer :: nc,errflag

    if (present(Ncache)) then
      nc = Ncache
    else
      nc = 1
    end if

    if (Monitoring) then
      ErrEventCnt(1)           = ErrEventCnt(1)           + 1
      ErrEventCnt(ErrFlag_cll) = ErrEventCnt(ErrFlag_cll) + 1
      AccEventCnt(1)           = AccEventCnt(1)           + 1
      AccEventCnt(AccFlag_cll) = AccEventCnt(AccFlag_cll) + 1
    end if

    if (use_cache_system) then
      call InitCache_cll(nc)
    end if
    call InitErrFlag_cll
    call InitAccFlag_cll
    EventCnt_cll = EventCnt_cll+1

  end subroutine InitEvent_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetVersionNumber_cll(version)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetVersionNumber_cll(version)

    character(len=5) :: version
    
    version = version_cll

  end subroutine GetVersionNumber_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitEventCnt_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitEventCnt_cll()

    logical :: infwri
  
    EventCnt_cll = 0

    if (infoutlev_cll.ge.2) call InfOut_cll('InitEventCnt_cll','phase-space point counter set to zero',infwri)

  end subroutine InitEventCnt_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetEventCnt_cll(event)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetEventCnt_cll(event)

    integer(kind=long_int) :: event
    
    event = EventCnt_cll

  end subroutine GetEventCnt_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMode_cll(mode)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMode_cll(mode)
 
    integer, intent(in) :: mode
    integer :: ritmax
    logical :: infwri
    
    if ((mode.lt.1).or.(mode.gt.3)) then
      write(nerrout_cll,*) 'COLLIER: mode_cll must be set to one of the following values'
      write(nerrout_cll,*) '1: use COLI-implementation'
      write(nerrout_cll,*) '2: use DD-implementation'
      write(nerrout_cll,*) '3: check COLI- against DD-implementation'
      if (ErrorStop_cll.ge.-10) stop

    end if

    mode_cll = mode

    infwri = .false.
    if (infoutlev_cll.ge.1) call InfOut_cll('SetMode_cll','mode_cll set to',infwri)

    select case (mode_cll)
    case (1)
      if (infwri) write(ninfout_cll,*) ' 1 --> use COLI implementation'
      
      if (nerroutcoli_cll.eq.closed_cll) then
        call OpenErrOutFileCOLI_cll(trim(foldername_cll)//'/ErrOut.coli')
      end if
      
    case (2)
      if (infwri) write(ninfout_cll,*) ' 2 --> use DD implementation'
      
      if (nerroutdd_cll.eq.closed_cll) then
        call OpenErrOutFileDD_cll(trim(foldername_cll)//'/ErrOut.dd')
        call GetRitmax_cll(ritmax)
        call SetRitMax_cll(ritmax)
      end if
      
    case (3)
      
      if (infwri) write(ninfout_cll,*) ' 3 --> check COLI against DD implementation'
      
      if (nerroutcoli_cll.eq.closed_cll) then
        call OpenErrOutFileCOLI_cll(trim(foldername_cll)//'/ErrOut.coli')
      end if
      
      if (nerroutdd_cll.eq.closed_cll) then
        call OpenErrOutFileDD_cll(trim(foldername_cll)//'/ErrOut.dd')
        call GetRitmax_cll(ritmax)
        call SetRitMax_cll(ritmax)
      end if
      
      if (ncheckout_cll.eq.closed_cll) then
        call OpenCheckOutFile_cll(trim(foldername_cll)//'/CheckOut.cll')
      endif
      
    end select

  end subroutine SetMode_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetMode_cll(mode)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetMode_cll(mode)

    integer, intent(out) :: mode

    mode = mode_cll

  end subroutine GetMode_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMuUV2_cll(mu2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMuUV2_cll(mu2)
 
    double precision, intent(in) :: mu2
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    integer :: i
    logical :: infwri
    character(len=*),parameter :: fmt11 = "(A11,d25.18)"

    muUV2_cll = mu2

    ! set muuv2 in COLI
    call Setmuuv2_coli(muUV2_cll)

    ! set muv2 in DD
    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_dd,muUV2_cll,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)

    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetMuUV2_cll','UV scale set to',infwri)
    if(infwri) write(ninfout_cll,fmt11) '   muUV2 =', muUV2_cll
    

  end subroutine SetMuUV2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetMuUV2_cll(mu2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetMuUV2_cll(mu2)

    double precision, intent(out) :: mu2

    mu2 = muUV2_cll

  end subroutine GetMuUV2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMuIR2_cll(mu2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMuIR2_cll(mu2)
 
    double precision, intent(in) :: mu2
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    integer :: i
    logical :: infwri
    character(len=*),parameter :: fmt11 = "(A11,d25.18)"

    muIR2_cll = mu2

    ! set muir2 in COLI
    call Setmuir2_coli(muIR2_cll)

    ! set muir2 in DD
    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_cll,xmx2_dd)
    
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetMuIR2_cll','IR scale set to',infwri)
    if(infwri) write(ninfout_cll,fmt11) '   muIR2 =', muIR2_cll

  end subroutine SetMuIR2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetMuIR2_cll(mu2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetMuIR2_cll(mu2)

    double precision, intent(out) :: mu2

    mu2 = muIR2_cll

  end subroutine GetMuIR2_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDeltaUV_cll(delta)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetDeltaUV_cll(delta)

    double precision, intent(in) :: delta
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    integer :: i
    logical :: infwri
    character(len=*),parameter :: fmt13 = "(A13,d25.18)"
#include "COLI/global_coli.h"

#ifdef SING
    DeltaUV_cll = delta
#else
    DeltaUV_cll = 0d0
    if (erroutlev_cll.ge.1) then
      write(nerrout_cll,*) 'preprocessor flag SING = false' 
      write(nerrout_cll,*) 'call of SetDeltaUV_cll without effect'
    end if   
#endif

#ifdef SING
    ! set deltauv in COLI
    call Setdeltauv_coli(DeltaUV_cll)
#endif

    ! set deltauv in DD
    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_cll,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)

    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetDeltaUV_cll','UV pole set to',infwri)
    if(infwri) write(ninfout_cll,fmt13) '    deltaUV =', DeltaUV_cll  

  end subroutine SetDeltaUV_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetDeltaUV_cll(delta)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetDeltaUV_cll(delta)
 
    double precision, intent(out) :: delta

    delta = DeltaUV_cll

  end subroutine GetDeltaUV_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetDeltaIR_cll(delta1,delta2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetDeltaIR_cll(delta1,delta2)
 
    double precision, intent(in) :: delta1,delta2
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    integer :: i
    logical :: infwri
    character(len=*),parameter :: fmt14 = "(A14,d25.18)"
#include "COLI/global_coli.h"

#ifdef SING
    DeltaIR1_cll = delta1
    DeltaIR2_cll = delta2
#else
    DeltaIR1_cll = 0d0
    DeltaIR2_cll = 0d0
    if (erroutlev_cll.ge.1) then
      write(nerrout_cll,*) 'preprocessor flag SING = false'
      write(nerrout_cll,*) 'call of SetDeltaUV_cll without effect'
    end if 
#endif

#ifdef SING
    ! set deltauv in COLI
    call Setdeltair_coli(DeltaIR1_cll,DeltaIR2_cll)
#endif
    ! set deltauv in DD
    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_cll,  &
             DeltaIR1_cll,muIR2_dd,xmx2_dd)

    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetDeltaIR_cll','IR single and double pole set to',infwri)
    if(infwri) then
      write(ninfout_cll,fmt14) '    deltaIR1 =', DeltaIR1_cll
      write(ninfout_cll,fmt14) '    deltaIR2 =', DeltaIR2_cll      
    end if

  end subroutine SetDeltaIR_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetDeltaIR_cll(delta1,delta2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetDeltaIR_cll(delta1,delta2)

    double precision, intent(out) :: delta1,delta2

    delta1 = DeltaIR1_cll
    delta2 = DeltaIR2_cll

  end subroutine GetDeltaIR_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddMinf2_cll(m2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine AddMinf2_cll(m2)

    double complex, intent(in) :: m2
    double complex, allocatable :: minf2_cp(:)
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    double precision :: xmx2(nminf_colidd)
    integer :: i
    logical :: infwri
    character(len=*),parameter :: fmt92 = "(A10,I3,A4,'dcmplx(',d25.18,' ,',d25.18,' )')"

    if(m2.eq.0d0) then
      if (infoutlev_cll.ge.1) call InfOut_cll('AddMinf2_cll','zero cannot be added to list of infinitesimal masses:',infwri)
      return
    end if
    
    if (nminf_cll.eq.0) then
      nminf_cll = 1
      if (allocated(minf2_cll)) then
        deallocate(minf2_cll)
      end if
      allocate(minf2_cll(nminf_cll))

      minf2_cll(1) = m2

    else
      do i=1,nminf_cll
        if (m2.eq.minf2_cll(i)) return
      end do

      allocate(minf2_cp(nminf_cll))
      minf2_cp = minf2_cll

      nminf_cll = nminf_cll+1
      if (allocated(minf2_cll)) then
        deallocate(minf2_cll)
      end if

      allocate(minf2_cll(nminf_cll))
      minf2_cll(1:nminf_cll-1) = minf2_cp
      minf2_cll(nminf_cll) = m2

    end if

    ! add m2 to small masses in COLI
    call Setminf2_coli(minf2_cll(nminf_cll))

    ! add m2 to small masses in DD
    if (nminf_cll.gt.nminf_colidd) then
      if (erroutlev_cll.ge.1) then
        write(nerrout_cll,*) 'COLLIER: more than' , nminf_colidd,' different infinitesimal masses not supported by DD'
      end if
      if (ErrorStop_cll.ge.-10) stop
    end if

    xmx2 = 0d0
    do i=1,nminf_cll
      xmx2(i) = dreal(minf2_cll(i))
    end do

    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
                    DeltaIR1_dd,muIR2_dd,xmx2)
    
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('AddMinf2_cll','added to list of infinitesimal masses:',infwri)
    if(infwri) write(ninfout_cll,fmt92) '    minf2(',nminf_cll,') = ',minf2_cll(nminf_cll) 

  end subroutine AddMinf2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine clearminf2_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine clearminf2_cll

    double precision :: xmx2(nminf_colidd)
    double precision :: DeltaUV_dd,DeltaIR2_dd,DeltaIR1_dd
    double precision :: muUV2_dd,muIR2_dd,xmx2_dd(nminf_colidd)
    integer :: i
    logical :: infwri

    nminf_cll = 0
    if (allocated(minf2_cll)) then
      deallocate(minf2_cll)
    end if

    ! clear list of small masses in COLI
    call clearcoliminf2

    ! clear list of small masses in DD
    xmx2 = 0d0

    call DDgetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
             DeltaIR1_dd,muIR2_dd,xmx2_dd)
    call DDsetparam(DeltaUV_dd,muUV2_dd,DeltaIR2_dd,  &
                    DeltaIR1_dd,muIR2_dd,xmx2)

    if (infoutlev_cll.ge.2) call InfOut_cll('clearminf2_cll','list of infinitesimal masses cleared',infwri)                   


  end subroutine clearminf2_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setminf2_cll(nminf,minf2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setminf2_cll(nminf,minf2)

    integer, intent(in) :: nminf
    double complex, intent(in) :: minf2(nminf)
    integer :: i

    call clearminf2_cll

    do i=1,nminf
      call AddMinf2_cll(minf2(i))
    end do

  end subroutine Setminf2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetNminf_cll(nminf)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetNminf_cll(nminf)

    integer, intent(out) :: nminf

    nminf = nminf_cll

  end subroutine GetNminf_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getminf2_cll(minf2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getminf2_cll(minf2)

    double complex, intent(out) :: minf2(nminf_cll)

    minf2 = minf2_cll

  end subroutine Getminf2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function Getminf2DD_cll(m2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function Getminf2DD_cll(m2)  result(minf2DD)

    double complex :: m2, minf2DD
    integer :: i

    do i=1,nminf_cll
      if (m2.eq.minf2_cll(i)) then
        minf2DD = i*1D-20
        return
      end if
    end do
    minf2DD = m2

  end function Getminf2DD_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function GetNc_cll(N,r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  function GetNc_cll(N,r) result(Nc)
  
    integer :: Nc
    integer :: N,r
    
    if ((N.gt.Nmax_cll).or.(N.lt.1).or.(r.gt.rmaxB_cll).or.(r.lt.1)) then
      if (erroutlev_cll.ge.1) then
        write(nerrout_cll,*) 'GetNc: argument N=',N,' or r=',r,' out of bound'
      end if
      Nc=0
      return
    end if
    Nc = NCoefs(r,N)
    
  end function GetNc_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function GetNt_cll(r)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  function GetNt_cll(r) result(Nt)
  
    integer :: Nt
    integer :: r
    
    if ((r.gt.rmaxB_cll).or.(r.lt.1)) then
      if (erroutlev_cll.ge.1) then
        write(nerrout_cll,*) 'GetNt: argument r=',r,' out of bound'
      end if
      Nt=0
      return
    end if
    Nt = RtS(r)
    
  end function GetNt_cll
  
  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffIRrational_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffIRrational_cll()
  
    logical :: infwri

    IR_rational_terms_cll = .false.
    call unsetirratterms_coli

    if (infoutlev_cll.ge.2) call InfOut_cll('SwitchOffIRrational_cll','IR rational terms switched off in COLI',infwri)


  end subroutine SwitchOffIRrational_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnIRrational_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnIRrational_cll()

    IR_rational_terms_cll = .true.
    call Setirratterms_coli
    if (infoutlev_cll.ge.2) then
      write(ninfout_cll,*) 'COLLIER: IR rational terms switched on'
    end if

  end subroutine SwitchOnIRrational_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCalcUV_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCalcUV_cll()

    CalcUV_cll = .false.
    call SwitchOffCalcUV_ten()

  end subroutine SwitchOffCalcUV_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCalcUV_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCalcUV_cll()

    CalcUV_cll = .true.
    call SwitchOnCalcUV_ten()

  end subroutine SwitchOnCalcUV_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetCalcUV_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetCalcUV_cll(CalcUV)

    logical, intent(out) :: CalcUV

    CalcUV = CalcUV_cll

  end subroutine GetCalcUV_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTenRed_cll(tenred)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTenRed_cll(tenred)

    integer, intent(in) :: tenred
    logical :: infwri,fla
    
    if (tenred.le.5) then
      call ErrOut_cll('SetTenRed_cll','Ntenred cannot be chosen smaller than 6',fla,.true.)
      if (fla) then
        write(nerrout_cll,*) 'Ntenred is set to Ntenred = 6'
      end if
      tenred_cll = 6
    else
      tenred_cll = tenred
    end if
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetTenRed_cll','direct tensor reduction switched on for',infwri)    
    if(infwri)  write(ninfout_cll,*) '    N >= ',tenred

  end subroutine SetTenRed_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnTenRed_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnTenRed_cll()

    logical :: infwri
  
    tenred_cll = 6
    if (infoutlev_cll.ge.2) call InfOut_cll('SwitchOnTenRed_cll','direct tensor reduction switched on for N >= 6',infwri)     

  end subroutine SwitchOnTenRed_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffTenRed_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffTenRed_cll()
  
    logical :: infwri

    tenred_cll = never_tenred_cll
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SwitchOffTenRed_cll','direct tensor reduction switched off',infwri)    

  end subroutine SwitchOffTenRed_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetTenRed_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetTenRed_cll(tenred)

    integer, intent(out) :: tenred

    tenred = tenred_cll

  end subroutine GetTenRed_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnTenArgPerm_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnArgPerm_cll()

    logical :: infwri
  
    argperm_cll = .true.
    if (infoutlev_cll.ge.1) then 
       call InfOut_cll('SwitchOnArgPerm_cll','permutation of tensor-integral arguments switched on in TNten'// &
       & ' Cache will be switched off!',infwri)     
!       if(infwri)  write(ninfout_cache,*) ' Cache will be switched off!'
    endif

    call SwitchOffCacheSystem_cll

  end subroutine SwitchOnArgPerm_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffArgPerm_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffArgPerm_cll()
  
    logical :: infwri

    argperm_cll = .false.
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SwitchOffArgPerm_cll','permutation of tensor-integral arguments switched off',infwri)    

  end subroutine SwitchOffArgPerm_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetArgPerm_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetArgPerm_cll(argperm)

    logical, intent(out) :: argperm

    argperm = argperm_cll

  end subroutine GetArgPerm_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffErrStop_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffErrStop_cll()

    ErrorStop_cll = -20

  end subroutine SwitchOffErrStop_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetErrStop_cll(errstop)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetErrStop_cll(errstop)
    integer :: errstop

    ErrorStop_cll = errstop

  end subroutine SetErrStop_cll



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetErrStop_cll(errstop)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetErrStop_cll(errstop)
    integer :: errstop

    errstop = ErrorStop_cll

  end subroutine GetErrStop_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffFileOutput_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffFileOutput_cll
    logical :: qopened

    if(nofiles_cll) return
    
    if(ninfout_cll.ne.closed_cll) then
      inquire(ninfout_cll, opened=qopened)
      if(qopened.and.ninfout_cll.ne.stdout_cll) close(unit=ninfout_cll)
    end if

    if(ninfoutcoli_cll.ne.closed_cll) then
      inquire(ninfoutcoli_cll, opened=qopened)
      if(qopened.and.ninfoutcoli_cll.ne.stdout_cll) close(unit=ninfoutcoli_cll)
    end if

    if(nerrout_cll.ne.closed_cll) then
      inquire(nerrout_cll, opened=qopened)
      if(qopened.and.nerrout_cll.ne.stdout_cll) close(unit=nerrout_cll)
    end if

    if(nerroutcoli_cll.ne.closed_cll) then
      inquire(nerroutcoli_cll, opened=qopened)
      if(qopened.and.nerroutcoli_cll.ne.stdout_cll) close(unit=nerroutcoli_cll)
    end if
    
    if(nerroutdd_cll.ne.closed_cll) then
      inquire(nerroutdd_cll, opened=qopened)
      if(qopened.and.nerroutdd_cll.ne.stdout_cll) close(unit=nerroutdd_cll)
    end if
    
    if(ncheckout_cll.ne.closed_cll) then
      inquire(ncheckout_cll, opened=qopened)
      if(qopened.and.ncheckout_cll.ne.stdout_cll) close(unit=ncheckout_cll)
    end if

    if(ncpout_cll.ne.closed_cll) then
      inquire(ncpout_cll, opened=qopened)
      if(qopened.and.ncpout_cll.ne.stdout_cll) close(unit=ncpout_cll)
    end if

    if(ncpout2_cll.ne.closed_cll) then
      inquire(ncpout2_cll, opened=qopened)
      if(qopened.and.ncpout2_cll.ne.stdout_cll) close(unit=ncpout2_cll)
    end if

    if(ncpoutcoli_cll.ne.closed_cll) then
      inquire(ncpoutcoli_cll, opened=qopened)
      if(qopened.and.ncpoutcoli_cll.ne.stdout_cll) close(unit=ncpoutcoli_cll)
    end if

    if(nstatsoutcoli_cll.ne.closed_cll) then
      inquire(nstatsoutcoli_cll, opened=qopened)
      if(qopened.and.nstatsoutcoli_cll.ne.stdout_cll) close(unit=nstatsoutcoli_cll)
    end if
    

    nerrout_cp_cll = nerrout_cll
    fname_errout_cp_cll = fname_errout_cll   
    nerroutcoli_cp_cll = nerroutcoli_cll    
    fname_erroutcoli_cp_cll = fname_erroutcoli_cll
    nerroutdd_cp_cll = nerroutdd_cll
    fname_erroutdd_cp_cll = fname_erroutdd_cll
    ninfout_cp_cll = ninfout_cll
    fname_infout_cp_cll = fname_infout_cll
    ninfoutcoli_cp_cll = ninfoutcoli_cll    
    fname_infoutcoli_cp_cll = fname_infoutcoli_cll 
    ncheckout_cp_cll = ncheckout_cll
    fname_checkout_cp_cll = fname_checkout_cll
    ncpoutcoli_cp_cll = ncpoutcoli_cll
    fname_cpoutcoli_cp_cll = fname_cpoutcoli_cll
    ncpout_cp_cll = ncpout_cll
    fname_cpout_cp_cll = fname_cpout_cll
    ncpout2_cp_cll = ncpout2_cll
    fname_cpout2_cp_cll = fname_cpout2_cll
    nstatsoutcoli_cp_cll = nstatsoutcoli_cll    
    fname_statsoutcoli_cp_cll = fname_statsoutcoli_cll 

    nofiles_cll = .true.
    call InitOutChan_cll(.false.)
    

  end subroutine SwitchOffFileOutput_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnFileOutput_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnFileOutput_cll
 
    logical :: qopened
 
    if(.not.nofiles_cll) return
    nofiles_cll = .false.
 
    fname_errout_cll = fname_errout_cp_cll
    if ((nerrout_cp_cll.ne.closed_cll).and.(nerrout_cp_cll.ne.stdout_cll)) then
      inquire(nerrout_cp_cll, opened=qopened)
      if (qopened) then
        call Setnerrout_cll
      else
        call Setnerrout_cll(nerrout_cp_cll)
      end if    
    end if
    
    fname_erroutcoli_cll = fname_erroutcoli_cp_cll
    if ((nerroutcoli_cp_cll.ne.closed_cll).and.(nerroutcoli_cp_cll.ne.stdout_cll)) then
      inquire(nerroutcoli_cp_cll, opened=qopened)
      if (qopened) then
        call Setnerroutcoli_cll
      else
        call Setnerroutcoli_cll(nerroutcoli_cp_cll)
      end if    
    end if    
    
    fname_erroutdd_cll = fname_erroutdd_cp_cll
    if ((nerroutdd_cp_cll.ne.closed_cll).and.(nerroutdd_cp_cll.ne.stdout_cll)) then
      inquire(nerroutdd_cp_cll, opened=qopened)
      if (qopened) then
        call Setnerroutdd_cll
      else
        call Setnerroutdd_cll(nerroutdd_cp_cll)
      end if    
    end if     
 
    fname_infout_cll = fname_infout_cp_cll
    if ((ninfout_cp_cll.ne.closed_cll).and.(ninfout_cp_cll.ne.stdout_cll)) then
      inquire(ninfout_cp_cll, opened=qopened)
      if (qopened) then
        call Setninfout_cll
      else
        call Setninfout_cll(ninfout_cp_cll)
      end if
    end if
 
    fname_infoutcoli_cll = fname_infoutcoli_cp_cll
    if ((ninfoutcoli_cp_cll.ne.closed_cll).and.(ninfoutcoli_cp_cll.ne.stdout_cll)) then
      inquire(ninfoutcoli_cp_cll, opened=qopened)
      if (qopened) then
        call Setninfoutcoli_cll
      else
        call Setninfoutcoli_cll(ninfoutcoli_cp_cll)
      end if
    end if
  
    fname_checkout_cll = fname_checkout_cp_cll
    if ((ncheckout_cp_cll.ne.closed_cll).and.(ncheckout_cp_cll.ne.stdout_cll)) then
      inquire(ncheckout_cp_cll, opened=qopened)
      if (qopened) then
        call Setncheckout_cll
      else
        call Setncheckout_cll(ncheckout_cp_cll)
      end if
    end if
  
    fname_cpoutcoli_cll = fname_cpoutcoli_cp_cll
    if ((ncpoutcoli_cp_cll.ne.closed_cll).and.(ncpoutcoli_cp_cll.ne.stdout_cll)) then
      inquire(ncpoutcoli_cp_cll, opened=qopened)
      if (qopened) then
        call Setncpoutcoli_cll
      else
        call Setncpoutcoli_cll(ncpoutcoli_cp_cll)
      end if
    end if    
  
    fname_cpout_cll = fname_cpout_cp_cll
    if ((ncpout_cp_cll.ne.closed_cll).and.(ncpout_cp_cll.ne.stdout_cll)) then
      inquire(ncpout_cp_cll, opened=qopened)
      if (qopened) then
        call Setncritpointsout_cll
      else
        call Setncritpointsout_cll(ncpout_cp_cll)
      end if
    end if 
  
    fname_cpout2_cll = fname_cpout2_cp_cll
    if ((ncpout2_cp_cll.ne.closed_cll).and.(ncpout2_cp_cll.ne.stdout_cll)) then
      inquire(ncpout2_cp_cll, opened=qopened)
      if (qopened) then
        call Setncritpointsout2_cll
      else
        call Setncritpointsout2_cll(ncpout2_cp_cll)
      end if
    end if
  
    fname_statsoutcoli_cll = fname_statsoutcoli_cp_cll
    if ((nstatsoutcoli_cp_cll.ne.closed_cll).and.(nstatsoutcoli_cp_cll.ne.stdout_cll)) then
      inquire(nstatsoutcoli_cp_cll, opened=qopened)
      if (qopened) then
        call Setnstatsoutcoli_cll
      else
        call Setnstatsoutcoli_cll(nstatsoutcoli_cp_cll)
      end if
    end if    
   

  end subroutine SwitchOnFileOutput_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetInfoutlev_cll(infoutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetInfoutlev_cll(infoutlev)

    integer, intent(in) :: infoutlev

    infoutlev_cll = infoutlev
    call Setinfoutlev_cache(infoutlev)

  end subroutine SetInfoutlev_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetInfoutlev_cll(infoutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetInfoutlev_cll(infoutlev)

    integer, intent(out) :: infoutlev

    infoutlev = infoutlev_cll

  end subroutine GetInfoutlev_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetErroutlev_cll(erroutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetErroutlev_cll(erroutlev)

    integer, intent(in) :: erroutlev
   
    if (erroutlev.eq.0) then
      call DDsetcout_on(.false.)   
    else if(erroutlev.eq.1) then
      if(nerroutdd_cll.ne.closed_cll) then
        call DDsetcout_on(.true.)
      end if
    else
      return
    end if
    erroutlev_cll = erroutlev
    call SetErroutlev_coli(erroutlev)

  end subroutine SetErroutlev_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetErroutlev_cll(erroutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetErroutlev_cll(erroutlev)

    integer, intent(out) :: erroutlev

    erroutlev = erroutlev_cll

  end subroutine Geterroutlev_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitGlobalDD_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitGlobalDD_cll(nmax_cll,ritmax_cll)

    integer, intent(in) :: nmax_cll,ritmax_cll
    integer :: nmax,rmax,rmax2,rmax3,rmax4,rmax5,rmax6
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd

    call Init_DD_global(nmax_cll,ritmax_cll)

    call DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
    call DDsetmode(cacc_dd,dacc_dd,2,0,0,0,outchannel_dd)
    call DDgetglobal(nmax,rmax,rmax2,rmax3,  &
                     rmax4,rmax5,rmax6)

    nmax_DD = nmax
    rmax_DD = rmax
    rmax2_DD = rmax2
    rmax3_DD = rmax3
    rmax4_DD = rmax4
    rmax5_DD = rmax5
    rmax6_DD = rmax6

  end subroutine InitGlobalDD_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetCPUprec_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetCPUprec_cll

    double precision :: dprec, dres_old, dres
    integer :: i
    logical :: infwri

    dprec = 1d0
    dres_old = 2d0    
    do i=1,1000
      dprec = dprec/2d0
      dres = exp(log(1d0+dprec))
      if (abs(dres).ge.abs(dres_old)) exit
      dres_old = dres
    end do
    dprec_cll = dprec*8d0

    call Setprec_coli(dprec_cll)

    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('GetCPUprec_cll','estimator of CPU double precision set to',infwri)    
    if(infwri)  write(ninfout_cll,*) '    dprec =', dprec_cll    

  end subroutine GetCPUprec_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAccuracy(acc0,acc1,acc2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetAccuracy_cll(acc0,acc1,acc2)

    double precision :: acc0, acc1, acc2
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd
    logical :: qopened,infwri
    character(len=*),parameter :: fmt27 = "(A27,Es17.10)"
    character(len=*),parameter :: fmt31 = "(A31,Es17.10)"
    character(len=*),parameter :: fmt33 = "(A33,Es17.10)"
    character(len=*),parameter :: fmt45 = "(A45,Es17.10)"
    
    reqacc_cll = acc0
    sreqacc_cll = sqrt(acc0)
    critacc_cll = acc1
    checkacc_cll = acc2

    call DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
    call DDsetmode(reqacc_cll,reqacc_cll,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
    call SetAcc_coli(acc0,acc1)

    if (infoutlev_cll.ge.2) then
      infwri = .false.
      if (infoutlev_cll.ge.2) call InfOut_cll('SetAccuracy_cll','precisions set to',infwri)    
      if(infwri) write(ninfout_cll,fmt31) 'target precision:    acc0 =', reqacc_cll
      if(infwri) write(ninfout_cll,fmt31) 'critical precision:  acc1 =', critacc_cll
      if(infwri) write(ninfout_cll,fmt31) 'check precision:     acc2 =', checkacc_cll
    end if

    if(ncpout_cll.ne.closed_cll) then
      inquire(ncpout_cll, opened=qopened)
      if(qopened) then
        write(unit=ncpout_cll,fmt=*) '                                                          '
        write(unit=ncpout_cll,fmt=*) '***********************************************************'
        write(unit=ncpout_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpout_cll,fmt=*) '***********************************************************'
        write(unit=ncpout_cll,fmt=*) '                                                          '
      endif
    end if

    if(ncpout2_cll.ne.closed_cll) then
      inquire(ncpout2_cll, opened=qopened)
      if(qopened) then
        write(unit=ncpout2_cll,fmt=*) '                                                          '
        write(unit=ncpout2_cll,fmt=*) '***********************************************************'
        write(unit=ncpout2_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpout2_cll,fmt=*) '***********************************************************'
        write(unit=ncpout2_cll,fmt=*) '                                                          '
      endif
    end if
      
    if(ncpoutcoli_cll.ne.closed_cll) then     
      inquire(ncpoutcoli_cll, opened=qopened)  
      if(qopened) then
        write(unit=ncpoutcoli_cll,fmt=*) '                                                          '
        write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
        write(unit=ncpoutcoli_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
        write(unit=ncpoutcoli_cll,fmt=*) '                                                          '
      endif
    end if
    
    if(ncheckout_cll.ne.closed_cll) then
      inquire(ncheckout_cll, opened=qopened)
      if(qopened) then
        write(unit=ncheckout_cll,fmt=*) '                                                          '
        write(unit=ncheckout_cll,fmt=*) '***********************************************************'
        write(unit=ncheckout_cll,fmt=*) ' check precision set to checkacc =', checkacc_cll 
        write(unit=ncheckout_cll,fmt=*) '***********************************************************'
        write(unit=ncheckout_cll,fmt=*) '                                                          '
      end if
    end if

    if (infoutlev_cll.ge.1.and.critacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetAccuracy_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,fmt33) '     critical precision critacc =', critacc_cll
      if(infwri) write(ninfout_cll,fmt45) '     smaller than required precision reqacc =', reqacc_cll
    end if

    if (infoutlev_cll.ge.1.and.checkacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetAccuracy_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,fmt31) '     check precision checkacc =', checkacc_cll
      if(infwri) write(ninfout_cll,fmt45) '     smaller than required precision reqacc =', reqacc_cll
    end if


  end subroutine SetAccuracy_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetReqAcc_cll(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetReqAcc_cll(acc)

    double precision :: acc
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd
    logical :: infwri
    character(len=*),parameter :: fmt12 = "(A12,Es17.10)"
    character(len=*),parameter :: fmt32 = "(A32,Es17.10)"
    character(len=*),parameter :: fmt43 = "(A43,Es17.10)"
    character(len=*),parameter :: fmt48 = "(A48,Es17.10)"

    reqacc_cll = acc
    sreqacc_cll = sqrt(acc)

    call DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
    call DDsetmode(reqacc_cll,reqacc_cll,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
    call DDsetaccthr(reqacc_cll)
    call SetReqAcc_coli(acc)

    if (infoutlev_cll.ge.2) then
      infwri = .false.
      if (infoutlev_cll.ge.2) call InfOut_cll('SetReqAcc_cll','target precision set to',infwri)    
      if(infwri)  write(ninfout_cll,fmt12) '    reqacc =', reqacc_cll
    end if

    if (infoutlev_cll.ge.1.and.critacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetReqAcc_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,fmt32) '     required precision reqacc =', reqacc_cll
      if(infwri) write(ninfout_cll,fmt48) '     larger than critical precision critacc =', critacc_cll
    end if

    if (infoutlev_cll.ge.1.and.checkacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetReqAcc_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,fmt32) '     required precision reqacc =', reqacc_cll
      if(infwri) write(ninfout_cll,fmt43) '     larger than check precision checkacc =', checkacc_cll
    end if

  end subroutine SetReqAcc_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetReqAcc_cll(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetReqAcc_cll(acc)

    double precision, intent(out) :: acc

    acc = reqacc_cll

  end subroutine GetReqAcc_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCritAcc_cll(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetCritAcc_cll(acc)

    double precision :: acc
    logical :: qopened,infwri
    character(len=*),parameter :: fmt93 = "(A13,Es17.10)"
    character(len=*),parameter :: fmt94 = "(A33,Es17.10)"
    character(len=*),parameter :: fmt95 = "(A45,Es17.10)"
    
    critacc_cll = acc


    call DDseterrthr(critacc_cll)

    call SetCritAcc_coli(acc)

    if(ncpoutcoli_cll.ne.closed_cll) then
      inquire(ncpoutcoli_cll, opened=qopened)
      if(qopened) then
        write(unit=ncpoutcoli_cll,fmt=*) '                                                          '
        write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
        write(unit=ncpoutcoli_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
        write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
      endif
    end if
    if(ncpout_cll.ne.closed_cll) then
      inquire(ncpout_cll, opened=qopened) 
      if(qopened) then
        write(unit=ncpout_cll,fmt=*) '                                                          '
        write(unit=ncpout_cll,fmt=*) '***********************************************************'
        write(unit=ncpout_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpout_cll,fmt=*) '***********************************************************'
        write(unit=ncpout_cll,fmt=*) '                                                          ' 
      endif
    end if
    if(ncpout2_cll.ne.closed_cll) then
      inquire(ncpout2_cll, opened=qopened)
      if(qopened) then
        write(unit=ncpout2_cll,fmt=*) '                                                          '
        write(unit=ncpout2_cll,fmt=*) '***********************************************************'
        write(unit=ncpout2_cll,fmt=*) ' critical precision set to critacc =', critacc_cll 
        write(unit=ncpout2_cll,fmt=*) '***********************************************************'
        write(unit=ncpout2_cll,fmt=*) '                                                          ' 
      endif
    end if
    
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetCritAcc_cll','critical precision set to',infwri)    
    if(infwri)  write(ninfout_cll,fmt93) '    critacc =', critacc_cll

    if (infoutlev_cll.ge.1.and.critacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetCritAcc_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,*) '     critical precision critacc =', critacc_cll
      if(infwri) write(ninfout_cll,*) '     smaller than required precision reqacc =', reqacc_cll
    end if

  end subroutine SetCritAcc_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetCritAcc_cll(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetCritAcc_cll(acc)

    double precision, intent(out) :: acc

    acc = critacc_cll

  end subroutine GetCritAcc_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCheckAcc(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetCheckAcc_cll(acc)

    double precision :: acc
    logical :: qopened,infwri
    character(len=*),parameter :: fmt14 = "(A14,Es17.10)"
    character(len=*),parameter :: fmt30 = "(A31,Es17.10)"
    character(len=*),parameter :: fmt45 = "(A45,Es17.10)"

    checkacc_cll = acc

    if(ncheckout_cll.ne.closed_cll) then
      inquire(ncheckout_cll, opened=qopened)
      if (qopened) then
        write(unit=ncheckout_cll,fmt=*) '                                                          '
        write(unit=ncheckout_cll,fmt=*) '***********************************************************'
        write(unit=ncheckout_cll,fmt=*) ' check precision set to checkacc =', checkacc_cll 
        write(unit=ncheckout_cll,fmt=*) '***********************************************************'
        write(unit=ncheckout_cll,fmt=*) '                                                          '
      end if
    end if
        
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetCheckAcc_cll','check precision set to',infwri)    
    if(infwri)  write(ninfout_cll,fmt14) '    checkacc =', checkacc_cll

    if (infoutlev_cll.ge.1.and.checkacc_cll.lt.reqacc_cll) then
      call InfOut_cll('SetCheckAcc_cll','WARNING',infwri)     
      if(infwri) write(ninfout_cll,fmt30) '     check precision checkacc =', checkacc_cll
      if(infwri) write(ninfout_cll,fmt45) '     smaller than required precision reqacc =', reqacc_cll
    end if


  end subroutine SetCheckAcc_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetCheckAcc_cll(acc)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetCheckAcc_cll(acc)

    double precision, intent(out) :: acc

    acc = checkacc_cll

  end subroutine GetCheckAcc_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRitmax_cll(ritmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetRitmax_cll(ritmax)

    double precision :: acc
    integer, intent(in) :: ritmax
    integer :: ritmaxB,ritmaxC,ritmaxD
    logical :: infwri
    character(len=*),parameter :: fmt12 = "(A12,i3)"
         
    infwri = .false.
    if (infoutlev_cll.ge.2) call InfOut_cll('SetRitmax_cll','maximum rank for expansions set to',infwri)    
    if(infwri)  write(ninfout_cll,fmt12) '    ritmax =', ritmax    

    if (ritmax.lt.max(7,rmax_cll+4-Nmax_cll)) then
      ritmaxD = max(7,rmax_cll+4-Nmax_cll)
      if (infoutlev_cll.ge.1) then
        if (rmax_cll+4-Nmax_cll.gt.7) then
          call InfOut_cll('SetRitmax_cll','ritmax has to be at least rmax_cll + 4 - Nmax_cll'//  &
              ' --> it is set to rmax_cll + 4 - Nmax_cll',infwri)        
        else
          call InfOut_cll('SetRitmax_cll','ritmax has to be at least 7 --> it is set to 7',infwri)
        end if
      end if
    else
      ritmaxD = ritmax
    end if    
    
    ritmax_cll = ritmaxD
    ritmaxC = ritmaxD+2
    ritmaxB = ritmaxD+4
    call SetNCoefs(Nmax_cll,ritmaxB)
    call SetNCoefsG(Nmax_cll,ritmaxB)   
    
    call SetRitmaxBCD_cll(ritmaxB,ritmaxC,ritmaxD)

    call InitGlobalDD_cll(nmax_cll,ritmax_cll)
    call GetReqAcc_cll(acc)
    call SetReqAcc_cll(acc)
    call GetCritAcc_cll(acc)
    call SetCritAcc_cll(acc)
    
  end subroutine SetRitmax_cll
    
  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetRitmaxBCD_cll(ritmax_B,ritmax_C,ritmax_D)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine SetRitmaxBCD_cll(ritmax_B,ritmax_C,ritmax_D)

    integer, intent(in) :: ritmax_B, ritmax_C, ritmax_D
    logical :: infwri

    if (ritmax_D.lt.4) then
      rmaxD_cll = 4    
      if (infoutlev_cll.ge.1) call InfOut_cll('SetRitmaxBCD_cll','ritmax_D has to be at least 4 --> it is set to 4',infwri)    
    else
      rmaxD_cll = ritmax_D
    end if

    if (ritmax_C.le.rmaxD_cll) then
      rmaxC_cll = rmaxD_cll+1
      if (infoutlev_cll.ge.1) call InfOut_cll('SetRitmaxBCD_cll', &
                                              'ritmax_C has to be larger than ritmax_C --> it is set to ritmax_D+1',infwri)        
    else
      rmaxC_cll = ritmax_C
    end if

    if (ritmax_B.le.rmaxC_cll) then
      rmaxB_cll = rmaxC_cll+1
      if (infoutlev_cll.ge.1) call InfOut_cll('SetRitmaxBCD_cll', &
                                              'ritmax_B has to be larger than ritmax_C --> it is set to ritmax_C+1',infwri)     
    else
      rmaxB_cll = ritmax_B
    end if

    ! set maximum rank in COLI
    call SetRitmax_coli(rmaxB_cll,rmaxC_cll,rmaxD_cll)
 

  end subroutine SetRitmaxBCD_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetRitmax_cll(ritmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine GetRitmax_cll(ritmax)

    integer, intent(out) :: ritmax
    
    ritmax = ritmax_cll
    
  end subroutine GetRitmax_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitErrFlag_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitErrFlag_cll()

    call SetErrFlag_cll(0)
    call SetErrFlag_coli(0)
    call SetErrFlag_dd(0)

  end subroutine InitErrFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetErrFlag_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetErrFlag_cll(val)

    integer, intent(in) :: val

    ErrFlag_cll = val
!    call SetErrFlag_coli(val)
!    call SetErrFlag_dd(val)

  end subroutine SetErrFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetErrFlag_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetErrFlag_cll(val)

    integer, intent(out) :: val

    val = ErrFlag_cll

  end subroutine GetErrFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine PropagateErrFlag_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PropagateErrFlag_cll()

    integer :: efcoli,efdd,efcll,ef


  ! error flags in COLI
  !  -1 internal Check failed
  !  -4 argument on cut in log or dilog
  !  -5 crit event (error > critical error)    excluded!
  !  -5 wrong exit of rloop
  !  -6 no reduction method works for C or D
  !  -6 inconsistent input momenta (not 4-dimensional)
  !  -7 specific numerical problem
  !  -9 internal inconsistency (e.g. with cache or max. tensor rank)
  ! -10 case not supported/implemented

  ! default for ErrorStopFlag:  -8


  ! changed AD 31.07.2017

    call GetErrFlag_coli(efcoli)
    call GetErrFlag_dd(efdd)
    call GetErrFlag_cll(efcll)
    ef = min(efcoli,efdd,efcll)

  ! added AD 19.10.2017
    ErrFlag_cll = ef

    if (ef.le.ErrorStop_cll) then
      if (efcoli.eq.ef) then
        write(stdout_cll,*) 'COLLIER: fatal error in COLI: ',efcoli
        write(stdout_cll,*) 'execution of program stopped'
        write(stdout_cll,*) 'error output written to the file ErrOut.coli'
      else if (efdd.eq.ef) then
        write(stdout_cll,*) 'COLLIER: fatal error in DD: ',efdd
        write(stdout_cll,*) 'execution of program stopped'
        write(stdout_cll,*) 'error output written to the file ErrOut.dd'
      else if (efcll.eq.ef) then
        write(stdout_cll,*) 'COLLIER: fatal error in COLLIER: ',efcll
        write(stdout_cll,*) 'execution of program stopped'
        write(stdout_cll,*) 'error output written to the file ErrOut.cll'
      end if
      stop
    end if

    if (Monitoring) then
      ErrCnt(1)           = ErrCnt(1)           + 1
      ErrCntcoli(efcoli)  = ErrCntcoli(efcoli)  + 1
      ErrCntdd(efdd  )    = ErrCntdd(efdd)      + 1
      ErrCnt(ef)          = ErrCnt(ef) + 1
    end if

  end subroutine PropagateErrFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxInfOut_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxInfOut_cll(val)

    integer, intent(in) :: val

    MaxInfOut_cll = val

  end subroutine SetMaxInfOut_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxErrOut_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxErrOut_cll(val)

    integer, intent(in) :: val

    MaxErrOut_cll = val

  end subroutine SetMaxErrOut_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitInfCnt_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitInfCnt_cll(val)

    integer, intent(in) :: val

    InfCnt_cll = val

  end subroutine InitInfCnt_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitErrCnt_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitErrCnt_cll(val)

    integer, intent(in) :: val

    ErrCnt_cll = val

  end subroutine InitErrCnt_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitErrCntCOLI_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitErrCntCOLI_cll()

!    integer, intent(in) :: val
    integer :: val    

    val = 0
    errcntcoli_cll = val
    call InitErrCnt_coli(val)

  end subroutine InitErrCntCOLI_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxErrOutCOLI_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxErrOutCOLI_cll(val)

    integer, intent(in) :: val

    MaxErrOutCOLI_cll = val
    call SetMaxErrOut_coli(val)

  end subroutine SetMaxErrOutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitErrCntDD_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitErrCntDD_cll()

!    integer, intent(in) :: val

!    ErrCntDD_cll = val
    ErrCntDD_cll = 0
    call DDresetcout()

  end subroutine InitErrCntDD_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxErrOutDD_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxErrOutDD_cll(val)

    integer, intent(in) :: val

    MaxErrOutDD_cll = val
    call DDsetcoutmax(val)

  end subroutine SetMaxErrOutDD_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitAccFlag_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitAccFlag_cll()

    call SetAccFlag_cll(0)

  end subroutine InitAccFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetAccFlag_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetAccFlag_cll(val)

    integer, intent(in) :: val

    AccFlag_cll = val

  end subroutine SetAccFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetAccFlag_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetAccFlag_cll(val)

    integer, intent(out) :: val

    val = AccFlag_cll

  end subroutine GetAccFlag_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine PropagateAccFlag_cll(RelErrs,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PropagateAccFlag_cll(RelErrs,rmax)

    integer, intent(in) :: rmax
    double precision, intent(in) :: RelErrs(0:rmax)
    integer :: loaf 
     
    loaf = 0
    if (maxval(RelErrs).gt.reqacc_cll) loaf=-1
    if (maxval(RelErrs).gt.critacc_cll) loaf=-2
    AccFlag_cll = min(AccFlag_cll,loaf)

    if (Monitoring) then
      AccCnt(1)    = AccCnt(1)     + 1
      AccCnt(loaf) = AccCnt(loaf)  + 1
    end if

  end subroutine PropagateAccFlag_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetInfoutlev_coli(infoutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetInfoutlevcoli_cll(infoutlev)

    integer, intent(in) :: infoutlev

    if(infoutlev.eq.0) then
       call unsetinfo_coli
    else
       call OpenInfOutFileCOLI_cll(trim(foldername_cll)//'/InfOut.coli')
       call Setinfo_coli
    endif

  end subroutine SetInfoutlevcoli_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCritPointsCntCOLI_cll(val)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCritPointsCntCOLI_cll(val)

    integer, intent(in) :: val

    CritPointsCntCOLI_cll = val
    call InitCritPointsCnt_coli(val)

  end subroutine InitCritPointsCntCOLI_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitPointsCnt_cll(noreset)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitPointsCnt_cll(noreset)

    logical, optional :: noreset
    integer :: Nold
    integer(kind=long_int), allocatable :: saveCnt(:)


    if (present(noreset)) then
    if (noreset) then
      Nold = size(CheckCnt_cll)
      if (Nold.lt.Nmax_cll) then
        allocate(saveCnt(Nold))

        saveCnt = PointsCntTN_cll  
        deallocate(PointsCntTN_cll)
        allocate(PointsCntTN_cll(Nmax_cll))      
        PointsCntTN_cll(1:Nold) = saveCnt
        PointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = AccPointsCntTN_cll  
        deallocate(AccPointsCntTN_cll)
        allocate(AccPointsCntTN_cll(Nmax_cll))      
        AccPointsCntTN_cll(1:Nold) = saveCnt
        AccPointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = sAccPointsCntTN_cll  
        deallocate(sAccPointsCntTN_cll)
        allocate(sAccPointsCntTN_cll(Nmax_cll))      
        sAccPointsCntTN_cll(1:Nold) = saveCnt
        sAccPointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = CritPointsCntTN_cll  
        deallocate(CritPointsCntTN_cll)
        allocate(CritPointsCntTN_cll(Nmax_cll))      
        CritPointsCntTN_cll(1:Nold) = saveCnt
        CritPointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = AccPointsCntTN2_cll  
        deallocate(AccPointsCntTN2_cll)
        allocate(AccPointsCntTN2_cll(Nmax_cll))      
        AccPointsCntTN2_cll(1:Nold) = saveCnt
        AccPointsCntTN2_cll(Nold+1:Nmax_cll) = 0

        saveCnt = sAccPointsCntTN2_cll  
        deallocate(sAccPointsCntTN2_cll)
        allocate(sAccPointsCntTN2_cll(Nmax_cll))      
        sAccPointsCntTN2_cll(1:Nold) = saveCnt
        sAccPointsCntTN2_cll(Nold+1:Nmax_cll) = 0

        saveCnt = CritPointsCntTN2_cll  
        deallocate(CritPointsCntTN2_cll)
        allocate(CritPointsCntTN2_cll(Nmax_cll))      
        CritPointsCntTN2_cll(1:Nold) = saveCnt
        CritPointsCntTN2_cll(Nold+1:Nmax_cll) = 0

        saveCnt = PointsCntTN_coli  
        deallocate(PointsCntTN_coli)
        allocate(PointsCntTN_coli(Nmax_cll))      
        PointsCntTN_coli(1:Nold) = saveCnt
        PointsCntTN_coli(Nold+1:Nmax_cll) = 0

        saveCnt = PointsCntTN_dd  
        deallocate(PointsCntTN_dd)
        allocate(PointsCntTN_dd(Nmax_cll))      
        PointsCntTN_dd(1:Nold) = saveCnt
        PointsCntTN_dd(Nold+1:Nmax_cll) = 0

        saveCnt = PointsCntTNten_cll  
        deallocate(PointsCntTNten_cll)
        allocate(PointsCntTNten_cll(Nmax_cll))      
        PointsCntTNten_cll(1:Nold) = saveCnt
        PointsCntTNten_cll(Nold+1:Nmax_cll) = 0

        saveCnt = AccPointsCntTNten_cll  
        deallocate(AccPointsCntTNten_cll)
        allocate(AccPointsCntTN_cll(Nmax_cll))      
        AccPointsCntTN_cll(1:Nold) = saveCnt
        AccPointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = sAccPointsCntTNten_cll  
        deallocate(sAccPointsCntTNten_cll)
        allocate(sAccPointsCntTN_cll(Nmax_cll))      
        sAccPointsCntTN_cll(1:Nold) = saveCnt
        sAccPointsCntTN_cll(Nold+1:Nmax_cll) = 0

        saveCnt = CritPointsCntTNten_cll  
        deallocate(CritPointsCntTNten_cll)
        allocate(CritPointsCntTNten_cll(Nmax_cll))      
        CritPointsCntTNten_cll(1:Nold) = saveCnt
        CritPointsCntTNten_cll(Nold+1:Nmax_cll) = 0

        saveCnt = PointsCntTNten_coli  
        deallocate(PointsCntTNten_coli)
        allocate(PointsCntTNten_coli(Nmax_cll))      
        PointsCntTNten_coli(1:Nold) = saveCnt
        PointsCntTNten_coli(Nold+1:Nmax_cll) = 0

        saveCnt = PointsCntTNten_dd  
        deallocate(PointsCntTNten_dd)
        allocate(PointsCntTNten_dd(Nmax_cll))      
        PointsCntTNten_dd(1:Nold) = saveCnt
        PointsCntTNten_dd(Nold+1:Nmax_cll) = 0
        
      end if
    end if
    
    else
      if(allocated(PointsCntTN_cll)) deallocate(PointsCntTN_cll)
      if(allocated(AccPointsCntTN_cll)) deallocate(AccPointsCntTN_cll)
      if(allocated(sAccPointsCntTN_cll)) deallocate(sAccPointsCntTN_cll)
      if(allocated(CritPointsCntTN_cll)) deallocate(CritPointsCntTN_cll)
      if(allocated(AccPointsCntTN2_cll)) deallocate(AccPointsCntTN2_cll)
      if(allocated(sAccPointsCntTN2_cll)) deallocate(sAccPointsCntTN2_cll)
      if(allocated(CritPointsCntTN2_cll)) deallocate(CritPointsCntTN2_cll)
      if(allocated(PointsCntTN_coli)) deallocate(PointsCntTN_coli)
      if(allocated(PointsCntTN_dd)) deallocate(PointsCntTN_dd)
      if(allocated(PointsCntTNten_cll)) deallocate(PointsCntTNten_cll)
      if(allocated(AccPointsCntTNten_cll)) deallocate(AccPointsCntTNten_cll)
      if(allocated(sAccPointsCntTNten_cll)) deallocate(sAccPointsCntTNten_cll)
      if(allocated(CritPointsCntTNten_cll)) deallocate(CritPointsCntTNten_cll)
      if(allocated(PointsCntTNten_coli)) deallocate(PointsCntTNten_coli)
      if(allocated(PointsCntTNten_dd)) deallocate(PointsCntTNten_dd)

      ! re-allocate Counters for Critical Points
      allocate(PointsCntTN_cll(Nmax_cll))
      allocate(AccPointsCntTN_cll(Nmax_cll))
      allocate(sAccPointsCntTN_cll(Nmax_cll))
      allocate(CritPointsCntTN_cll(Nmax_cll))
      allocate(AccPointsCntTN2_cll(Nmax_cll))
      allocate(sAccPointsCntTN2_cll(Nmax_cll))
      allocate(CritPointsCntTN2_cll(Nmax_cll))
      allocate(PointsCntTN_coli(Nmax_cll))
      allocate(PointsCntTN_dd(Nmax_cll))
      allocate(PointsCntTNten_cll(Nmax_cll))
      allocate(AccPointsCntTNten_cll(Nmax_cll))
      allocate(sAccPointsCntTNten_cll(Nmax_cll))
      allocate(CritPointsCntTNten_cll(Nmax_cll))
      allocate(PointsCntTNten_coli(Nmax_cll))
      allocate(PointsCntTNten_dd(Nmax_cll))

      PointsCntA_cll = 0
      PointsCntB_cll = 0
      PointsCntC_cll = 0
      PointsCntD_cll = 0
      PointsCntE_cll = 0
      PointsCntF_cll = 0
      PointsCntG_cll = 0
      PointsCntTN_cll = 0
      PointsCntDB_cll = 0
      AccPointsCntA_cll = 0
      AccPointsCntB_cll = 0
      AccPointsCntC_cll = 0
      AccPointsCntD_cll = 0
      AccPointsCntE_cll = 0
      AccPointsCntF_cll = 0
      AccPointsCntG_cll = 0
      AccPointsCntTN_cll = 0
      AccPointsCntDB_cll = 0
      sAccPointsCntA_cll = 0
      sAccPointsCntB_cll = 0
      sAccPointsCntC_cll = 0
      sAccPointsCntD_cll = 0
      sAccPointsCntE_cll = 0
      sAccPointsCntF_cll = 0
      sAccPointsCntG_cll = 0
      sAccPointsCntTN_cll = 0
      sAccPointsCntDB_cll = 0
      CritPointsCntA_cll = 0
      CritPointsCntB_cll = 0
      CritPointsCntC_cll = 0
      CritPointsCntD_cll = 0
      CritPointsCntE_cll = 0
      CritPointsCntF_cll = 0
      CritPointsCntG_cll = 0
      CritPointsCntTN_cll = 0
      CritPointsCntDB_cll = 0

      AccPointsCntA2_cll = 0
      AccPointsCntB2_cll = 0
      AccPointsCntC2_cll = 0
      AccPointsCntD2_cll = 0
      AccPointsCntE2_cll = 0
      AccPointsCntF2_cll = 0
      AccPointsCntG2_cll = 0
      AccPointsCntTN2_cll = 0
      AccPointsCntDB2_cll = 0
      sAccPointsCntA2_cll = 0
      sAccPointsCntB2_cll = 0
      sAccPointsCntC2_cll = 0
      sAccPointsCntD2_cll = 0
      sAccPointsCntE2_cll = 0
      sAccPointsCntF2_cll = 0
      sAccPointsCntG2_cll = 0
      sAccPointsCntTN2_cll = 0
      sAccPointsCntDB2_cll = 0
      CritPointsCntA2_cll = 0
      CritPointsCntB2_cll = 0
      CritPointsCntC2_cll = 0
      CritPointsCntD2_cll = 0
      CritPointsCntE2_cll = 0
      CritPointsCntF2_cll = 0
      CritPointsCntG2_cll = 0
      CritPointsCntTN2_cll = 0
      CritPointsCntDB2_cll = 0

      PointsCntA_coli = 0
      PointsCntB_coli = 0
      PointsCntC_coli = 0
      PointsCntD_coli = 0
      PointsCntE_coli = 0
      PointsCntF_coli = 0
      PointsCntG_coli = 0
      PointsCntTN_coli = 0
      PointsCntDB_coli = 0

      PointsCntA_dd = 0
      PointsCntB_dd = 0
      PointsCntC_dd = 0
      PointsCntD_dd = 0
      PointsCntE_dd = 0
      PointsCntF_dd = 0
      PointsCntG_dd = 0
      PointsCntTN_dd = 0
      PointsCntDB_dd = 0

      PointsCntAten_cll = 0
      PointsCntBten_cll = 0
      PointsCntCten_cll = 0
      PointsCntDten_cll = 0
      PointsCntEten_cll = 0
      PointsCntFten_cll = 0
      PointsCntGten_cll = 0
      PointsCntTNten_cll = 0
      PointsCntDBten_cll = 0
      AccPointsCntAten_cll = 0
      AccPointsCntBten_cll = 0
      AccPointsCntCten_cll = 0
      AccPointsCntDten_cll = 0
      AccPointsCntEten_cll = 0
      AccPointsCntFten_cll = 0
      AccPointsCntGten_cll = 0
      AccPointsCntTNten_cll = 0
      AccPointsCntDBten_cll = 0
      sAccPointsCntAten_cll = 0
      sAccPointsCntBten_cll = 0
      sAccPointsCntCten_cll = 0
      sAccPointsCntDten_cll = 0
      sAccPointsCntEten_cll = 0
      sAccPointsCntFten_cll = 0
      sAccPointsCntGten_cll = 0
      sAccPointsCntTNten_cll = 0
      sAccPointsCntDBten_cll = 0
      CritPointsCntAten_cll = 0
      CritPointsCntBten_cll = 0
      CritPointsCntCten_cll = 0
      CritPointsCntDten_cll = 0
      CritPointsCntEten_cll = 0
      CritPointsCntFten_cll = 0
      CritPointsCntGten_cll = 0
      CritPointsCntTNten_cll = 0
      CritPointsCntDBten_cll = 0


      PointsCntAten_coli = 0
      PointsCntBten_coli = 0
      PointsCntCten_coli = 0
      PointsCntDten_coli = 0
      PointsCntEten_coli = 0
      PointsCntFten_coli = 0
      PointsCntGten_coli = 0
      PointsCntTNten_coli = 0
      PointsCntDBten_coli = 0

      PointsCntAten_dd = 0
      PointsCntBten_dd = 0
      PointsCntCten_dd = 0
      PointsCntDten_dd = 0
      PointsCntEten_dd = 0
      PointsCntFten_dd = 0
      PointsCntGten_dd = 0
      PointsCntTNten_dd = 0
      PointsCntDBten_dd = 0

      ErrCntcoli = 0
      ErrCntdd = 0
      ErrCnt = 0
      AccCnt = 0
      ErrEventCnt = 0
      AccEventCnt = 0
      
    end if
    
  end subroutine InitPointsCnt_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitMonitoring_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitMonitoring_cll()
  
    logical :: infwri

! changed 29.02.2016 AD
!    if (Monitoring) then
!      if (infoutlev_cll.ge.2) call InfOut_cll('InitMonitoring_cll','CritPointsMonitor already initialized',infwri)   
!      return
!    endif
    
    if (Monitoring) then
      if (infoutlev_cll.ge.2) call InfOut_cll('InitMonitoring_cll','CritPointsMonitor re-initialized',infwri)   
    else
      if (infoutlev_cll.ge.2) call InfOut_cll('InitMonitoring_cll','CritPointsMonitor initialized',infwri) 
    endif

    Monitoring = .true. 

    call InitPointsCnt_cll()
    call initMaxCritPoints_cll
    call initMaxCritPointsDB_cll
    call OpenCritPointsOutFile_cll(trim(foldername_cll)//'/CritPointsOut.cll')

#ifdef CritPoints2
    call OpenCritPointsOutFile2_cll(trim(foldername_cll)//'/CritPointsOut2.cll')
#endif

#ifdef CritPointsCOLI 
    call InitCritPointsCntCOLI_cll(0)
    call OpenCritPointsOutFileCOLI_cll(trim(foldername_cll)//'/CritPointsOut.coli')
#endif

  end subroutine InitMonitoring_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitOutChan_cll(init_stdout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitOutChan_cll(init_stdout)

    logical, optional :: init_stdout
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd
    logical :: stdflag
  
    if(present(init_stdout)) then
      stdflag=init_stdout
    else
      stdflag=.true.
    end if
    
    if(stdflag.or.(nerrout_cll.ne.stdout_cll)) then
      nerrout_cll = closed_cll
      fname_errout_cll = ''
    end if

    if(stdflag.or.(nerroutcoli_cll.ne.stdout_cll)) then    
      nerroutcoli_cll = closed_cll
      call Setnerrout_coli(nerroutcoli_cll)    
      fname_erroutcoli_cll = ''
    end if
      
    if(stdflag.or.(nerroutdd_cll.ne.stdout_cll)) then     
      nerroutdd_cll = closed_cll
      call  DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
      call  DDsetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,nerroutdd_cll)
      call  DDsetcout_on(.false.)
      fname_erroutdd_cll = ''
    end if

    if(stdflag.or.(ninfout_cll.ne.stdout_cll)) then   
      ninfout_cll = closed_cll
      fname_infout_cll = ''
    end if
    
    if(stdflag.or.(ninfoutcoli_cll.ne.stdout_cll)) then
      ninfoutcoli_cll = closed_cll
      call Setninfout_coli(ninfoutcoli_cll)    
      fname_infoutcoli_cll = ''
    end if
    
    if(stdflag.or.(ncheckout_cll.ne.stdout_cll)) then
      ncheckout_cll = closed_cll
      fname_checkout_cll = ''
    end if

    if(stdflag.or.(ncpoutcoli_cll.ne.stdout_cll)) then
      ncpoutcoli_cll = closed_cll
      call Setncpout_coli(ncpoutcoli_cll)
      fname_cpoutcoli_cll = ''
    end if
    
    if(stdflag.or.(ncpout_cll.ne.stdout_cll)) then
      ncpout_cll = closed_cll
      fname_cpout_cll = ''
    end if
    
    if(stdflag.or.(ncpout2_cll.ne.stdout_cll)) then
      ncpout2_cll = closed_cll
      fname_cpout2_cll = ''
    end if
    
    if(stdflag.or.(nstatsoutcoli_cll.ne.stdout_cll)) then
      nstatsoutcoli_cll = closed_cll
      call Setnstatsout_coli(nstatsoutcoli_cll)    
      fname_statsoutcoli_cll = ''
    end if
        
!    qopened_critcoli = .false.
!    qopened_crit = .false.
!    qopened_crit2 = .false.
!    qopened_check = .false.
!    qopened_statscoli = .false.


  end subroutine InitOutChan_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitOutChan_cp_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitOutChan_cp_cll
    
    nerrout_cp_cll = closed_cll
    fname_errout_cp_cll = ''   
    nerroutcoli_cp_cll = closed_cll    
    fname_erroutcoli_cp_cll = ''    
    nerroutdd_cp_cll = closed_cll
    fname_erroutdd_cp_cll = '' 
    ninfout_cp_cll = closed_cll
    fname_infout_cp_cll = ''
    ninfoutcoli_cp_cll = closed_cll    
    fname_infoutcoli_cp_cll = ''
    ncheckout_cp_cll = closed_cll
    fname_checkout_cp_cll = ''
    ncpoutcoli_cp_cll = closed_cll
    fname_cpoutcoli_cp_cll = ''
    ncpout_cp_cll = closed_cll
    fname_cpout_cp_cll = ''
    ncpout2_cp_cll = closed_cll
    fname_cpout2_cp_cll = ''
    nstatsoutcoli_cp_cll = closed_cll  
    fname_statsoutcoli_cp_cll = ''

  end subroutine InitOutChan_cp_cll


  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setninfout_cll(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setninfout_cll(ninfout)

    integer, intent(in), optional :: ninfout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ninfout_cll.ne.closed_cll) then
      inquire(ninfout_cll, opened=qopened)
      if(qopened.and.(ninfout_cll.ne.stdout_cll)) close(unit=ninfout_cll)
    end if
    
    if (present(ninfout)) then
      if (len(trim(fname_infout_cll)).eq.0) then
        call OpenInfOutFile_cll(trim(foldername_cll)//'/InfOut.cll',ninfout)
      else if (ninfout.ne.stdout_cll) then
        inquire(ninfout, opened=qopened)
        if(qopened) close(unit=ninfout)
        ninfout_cll = ninfout
        call Setninfout_cache(ninfout_cll)        
        open(unit=ninfout_cll,file=trim(fname_infout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_infout_cll)).eq.0) then
        call OpenInfOutFile_cll(trim(foldername_cll)//'/InfOut.cll')
      else
        ninfout_cll = findFreeChannel_cll()
        call Setninfout_cache(ninfout_cll)        
        open(unit=ninfout_cll,file=trim(fname_infout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if

  end subroutine Setninfout_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getninfout_cll(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getninfout_cll(ninfout)

    integer, intent(out) :: ninfout

    ninfout = ninfout_cll

  end subroutine Getninfout_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setninfoutcoli_cll(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setninfoutcoli_cll(ninfout)

    integer, intent(in), optional :: ninfout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ninfoutcoli_cll.ne.closed_cll) then
      inquire(ninfoutcoli_cll, opened=qopened)
      if(qopened.and.(ninfoutcoli_cll.ne.stdout_cll)) close(unit=ninfoutcoli_cll)
    end if
    
    if (present(ninfout)) then
      if (len(trim(fname_infoutcoli_cll)).eq.0) then
        call OpenInfOutFileCOLI_cll(trim(foldername_cll)//'/InfOut.coli',ninfout)
      else if (ninfout.ne.stdout_cll) then
        inquire(ninfout, opened=qopened)
        if(qopened) close(unit=ninfout)
        ninfoutcoli_cll = ninfout
        call Setninfout_coli(ninfoutcoli_cll)          
        open(unit=ninfoutcoli_cll,file=trim(fname_infoutcoli_cll),form='formatted',access='sequential', &
                                  position='append',status='old')
      end if
    else
      if (len(trim(fname_infoutcoli_cll)).eq.0) then
        call OpenInfOutFileCOLI_cll(trim(foldername_cll)//'/InfOut.coli')
      else
        ninfoutcoli_cll = findFreeChannel_cll()
        call Setninfout_coli(ninfoutcoli_cll)       
        open(unit=ninfoutcoli_cll,file=trim(fname_infoutcoli_cll),form='formatted',access='sequential', &
                                  position='append',status='old')       
      end if
    end if

  end subroutine Setninfoutcoli_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetninfoutCOLI_cll(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetninfoutCOLI_cll(ninfout)

    integer, intent(out) :: ninfout

    ninfout = ninfoutcoli_cll

  end subroutine GetninfoutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setnerrout_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setnerrout_cll(nerrout)

    integer, intent(in), optional :: nerrout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(nerrout_cll.ne.closed_cll) then
      inquire(nerrout_cll, opened=qopened)
      if(qopened.and.(nerrout_cll.ne.stdout_cll)) close(unit=nerrout_cll)
    end if
    
    if (present(nerrout)) then
      if (len(trim(fname_errout_cll)).eq.0) then
        call OpenErrOutFile_cll(trim(foldername_cll)//'/ErrOut.cll',nerrout)
      else if (nerrout.ne.stdout_cll) then
        inquire(nerrout, opened=qopened)
        if(qopened) close(unit=nerrout)
        nerrout_cll = nerrout        
        open(unit=nerrout_cll,file=trim(fname_errout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_errout_cll)).eq.0) then
        call OpenErrOutFile_cll(trim(foldername_cll)//'/ErrOut.cll')
      else
        nerrout_cll = findFreeChannel_cll()
        open(unit=nerrout_cll,file=trim(fname_errout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if
      

  end subroutine Setnerrout_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getnerrout_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getnerrout_cll(nerrout)

    integer, intent(out) :: nerrout

    nerrout = nerrout_cll

  end subroutine Getnerrout_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetnerroutCOLI_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetnerroutCOLI_cll(nerrout)

    integer, intent(in), optional :: nerrout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(nerroutcoli_cll.ne.closed_cll) then
      inquire(nerroutcoli_cll, opened=qopened)
      if(qopened.and.(nerroutcoli_cll.ne.stdout_cll)) close(unit=nerroutcoli_cll)
    end if
    
    if (present(nerrout)) then
      if (len(trim(fname_erroutcoli_cll)).eq.0) then
        call OpenErrOutFileCOLI_cll(trim(foldername_cll)//'/ErrOut.coli',nerrout)
      else if (nerrout.ne.stdout_cll) then
        inquire(nerrout, opened=qopened)
        if(qopened) close(unit=nerrout)
        nerroutcoli_cll = nerrout 
        call Setnerrout_coli(nerroutcoli_cll)        
        open(unit=nerroutcoli_cll,file=trim(fname_erroutcoli_cll),form='formatted', &
                                  access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_erroutcoli_cll)).eq.0) then
        call OpenErrOutFileCOLI_cll(trim(foldername_cll)//'/ErrOut.cll')
      else
        nerroutcoli_cll = findFreeChannel_cll()
        call Setnerrout_coli(nerroutcoli_cll)       
        open(unit=nerroutcoli_cll,file=trim(fname_erroutcoli_cll),form='formatted', &
                                  access='sequential',position='append',status='old')       
      end if
    end if

  end subroutine SetnerroutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetnerroutCOLI_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetnerroutCOLI_cll(nerrout)

    integer, intent(out) :: nerrout

    nerrout = nerroutcoli_cll

  end subroutine GetnerroutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetnerroutDD_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetnerroutDD_cll(nerrout)

    integer, intent(in), optional :: nerrout
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(nerroutdd_cll.ne.closed_cll) then
      inquire(nerroutdd_cll, opened=qopened)
      if(qopened.and.(nerroutdd_cll.ne.stdout_cll)) close(unit=nerroutdd_cll)
    end if
    
    if (present(nerrout)) then
      if (len(trim(fname_erroutdd_cll)).eq.0) then
        call OpenErrOutFileDD_cll(trim(foldername_cll)//'/ErrOut.dd',nerrout)
      else if (nerrout.ne.stdout_cll) then
        inquire(nerrout, opened=qopened)
        if(qopened) close(unit=nerrout)
        nerroutdd_cll = nerrout
        call  DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
        call  DDsetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,nerroutdd_cll)
        if (erroutlev_cll.gt.0) then
          call  DDsetcout_on(.true.)
        end if
        open(unit=nerroutdd_cll,file=trim(fname_erroutdd_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_erroutdd_cll)).eq.0) then
        call OpenErrOutFileDD_cll(trim(foldername_cll)//'/ErrOut.dd')
      else
        nerroutdd_cll = findFreeChannel_cll()
        call  DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
        call  DDsetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,nerroutdd_cll)
        if (erroutlev_cll.gt.0) then
          call  DDsetcout_on(.true.)
        end if
        open(unit=nerroutdd_cll,file=trim(fname_erroutdd_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if    
      
  end subroutine SetnerroutDD_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetnerroutDD_cll(nerrout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetnerroutDD_cll(nerrout)

    integer, intent(out) :: nerrout

    nerrout = nerroutdd_cll

  end subroutine GetnerroutDD_cll

  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setncheckout_cll(ncheckout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setncheckout_cll(ncheckout)

    integer, intent(in), optional :: ncheckout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ncheckout_cll.ne.closed_cll) then
      inquire(ncheckout_cll, opened=qopened)
      if(qopened.and.(ncheckout_cll.ne.stdout_cll)) close(unit=ncheckout_cll)
    end if
    
    if (present(ncheckout)) then
      if (len(trim(fname_checkout_cll)).eq.0) then
        call OpenCheckOutFile_cll(trim(foldername_cll)//'/CheckOut.cll',ncheckout)
      else if (ncheckout.ne.stdout_cll) then
        inquire(ncheckout, opened=qopened)
        if(qopened) close(unit=ncheckout)
        ncheckout_cll = ncheckout        
        open(unit=ncheckout_cll,file=trim(fname_checkout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_checkout_cll)).eq.0) then
        call OpenCheckOutFile_cll(trim(foldername_cll)//'/CheckOut.cll')
      else
        ncheckout_cll = findFreeChannel_cll()
        open(unit=ncheckout_cll,file=trim(fname_checkout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if
    

  end subroutine Setncheckout_cll

  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getncheckout_cll(ncheckout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getncheckout_cll(ncheckout)

    integer, intent(out) :: ncheckout

    ncheckout = ncheckout_cll

  end subroutine Getncheckout_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setncpoutcoli_cll(ncpout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setncpoutcoli_cll(ncpout)

    integer, intent(in), optional :: ncpout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ncpoutcoli_cll.ne.closed_cll) then
      inquire(ncpoutcoli_cll, opened=qopened)
      if(qopened.and.(ncpoutcoli_cll.ne.stdout_cll)) close(unit=ncpoutcoli_cll)
    end if
    
    if (present(ncpout)) then
      if (len(trim(fname_cpoutcoli_cll)).eq.0) then
        call OpenCritPointsOutFileCOLI_cll(trim(foldername_cll)//'/CritPointsOut.coli',ncpout)
      else if (ncpout.ne.stdout_cll) then
        inquire(ncpout, opened=qopened)
        if(qopened) close(unit=ncpout)
        ncpoutcoli_cll = ncpout
        call Setncpout_coli(ncpoutcoli_cll)
        open(unit=ncpoutcoli_cll,file=trim(fname_cpoutcoli_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_cpoutcoli_cll)).eq.0) then
        call OpenCritPointsOutFileCOLI_cll(trim(foldername_cll)//'/CritPointsOut.coli')
      else
        ncpoutcoli_cll = findFreeChannel_cll()
        call Setncpout_coli(ncpoutcoli_cll)
        open(unit=ncpoutcoli_cll,file=trim(fname_cpoutcoli_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if

  end subroutine Setncpoutcoli_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetncpoutCOLI_cll(ncpout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetncpoutCOLI_cll(ncpout)

    integer, intent(out) :: ncpout

    ncpout = ncpoutcoli_cll

  end subroutine GetncpoutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setnstatsoutcoli_cll(nstatsout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setnstatsoutcoli_cll(nstatsout)

    integer, intent(in), optional :: nstatsout
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(nstatsoutcoli_cll.ne.closed_cll) then
      inquire(nstatsoutcoli_cll, opened=qopened)
      if(qopened.and.(nstatsoutcoli_cll.ne.stdout_cll)) close(unit=nstatsoutcoli_cll)
    end if
    
    if (present(nstatsout)) then
      if (len(trim(fname_statsoutcoli_cll)).eq.0) then
        call OpenStatisticsOutFileCOLI_cll(trim(foldername_cll)//'/StatisticsOut.coli',nstatsout)
      else if (nstatsout.ne.stdout_cll) then
        inquire(nstatsout, opened=qopened)
        if(qopened) close(unit=nstatsout)
        nstatsoutcoli_cll = nstatsout
        call Setnstatsout_coli(nstatsoutcoli_cll)
        open(unit=nstatsoutcoli_cll,file=trim(fname_statsoutcoli_cll),form='formatted',access='sequential', &
                                    position='append',status='old')
      end if
    else
      if (len(trim(fname_statsoutcoli_cll)).eq.0) then
        call OpenStatisticsOutFileCOLI_cll(trim(foldername_cll)//'/StatisticsOut.coli')
      else
        nstatsoutcoli_cll = findFreeChannel_cll()
        call Setnstatsout_coli(nstatsoutcoli_cll)
        open(unit=nstatsoutcoli_cll,file=trim(fname_statsoutcoli_cll),form='formatted',access='sequential', &
                                    position='append',status='old')       
      end if
    end if

  end subroutine Setnstatsoutcoli_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetstatsoutCOLI_cll(nstatsout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetnstatsoutCOLI_cll(nstatsout)

    integer, intent(out) :: nstatsout

    nstatsout = nstatsoutcoli_cll

  end subroutine GetnstatsoutCOLI_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setncritpointsout_cll(ncpout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setncritpointsout_cll(ncpout)

    integer, intent(in), optional :: ncpout
    logical :: qopened
     
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ncpout_cll.ne.closed_cll) then
      inquire(ncpout_cll, opened=qopened)
      if(qopened.and.(ncpout_cll.ne.stdout_cll)) close(unit=ncpout_cll)
    end if
    
    if (present(ncpout)) then
      if (len(trim(fname_cpout_cll)).eq.0) then
        call OpenCritPointsOutFile_cll(trim(foldername_cll)//'/CritPointsOut.cll',ncpout)
      else if (ncpout.ne.stdout_cll) then
        inquire(ncpout, opened=qopened)
        if(qopened) close(unit=ncpout)
        ncpout_cll = ncpout
        open(unit=ncpout_cll,file=trim(fname_cpout_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_cpout_cll)).eq.0) then
        call OpenCritPointsOutFile_cll(trim(foldername_cll)//'/CritPointsOut.cll')
      else
        ncpout_cll = findFreeChannel_cll()
        open(unit=ncpout_cll,file=trim(fname_cpout_cll),form='formatted',access='sequential',position='append',status='old')       
      end if
    end if

  end subroutine Setncritpointsout_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getncritpointsout_cll(ncritpointsout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getncritpointsout_cll(ncritpointsout)

    integer, intent(out) :: ncritpointsout

    ncritpointsout = ncpout_cll

  end subroutine Getncritpointsout_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setncritpointsout_cll(ncpout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setncritpointsout2_cll(ncpout)

    integer, intent(in), optional :: ncpout
    logical :: qopened
     
    ! return if output into files is suppressed
    if (nofiles_cll) return

    if(ncpout2_cll.ne.closed_cll) then
      inquire(ncpout2_cll, opened=qopened)
      if(qopened.and.(ncpout2_cll.ne.stdout_cll)) close(unit=ncpout2_cll)
    end if
    
    if (present(ncpout)) then
      if (len(trim(fname_cpout2_cll)).eq.0) then
        call OpenCritPointsOutFile2_cll(trim(foldername_cll)//'/CritPointsOut2.cll',ncpout)
      else if (ncpout.ne.stdout_cll) then
        inquire(ncpout, opened=qopened)
        if(qopened) close(unit=ncpout)
        ncpout2_cll = ncpout
        open(unit=ncpout2_cll,file=trim(fname_cpout2_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    else
      if (len(trim(fname_cpout2_cll)).eq.0) then
        call OpenCritPointsOutFile2_cll(trim(foldername_cll)//'/CritPointsOut2.cll')
      else
        ncpout2_cll = findFreeChannel_cll()
        open(unit=ncpout2_cll,file=trim(fname_cpout2_cll),form='formatted',access='sequential',position='append',status='old')
      end if
    end if    

  end subroutine Setncritpointsout2_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetOutputFolder_cll(fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetOutputFolder_cll(fname)

    character(len=*), intent(in) :: fname  

    foldername_cll = fname
    fname_errout_cll = ''       
    fname_erroutcoli_cll = ''
    fname_erroutdd_cll = ''
    fname_infout_cll = ''
    fname_infoutcoli_cll = ''
    fname_checkout_cll = ''
    fname_cpoutcoli_cll = ''
    fname_cpout_cll = ''
    fname_cpout2_cll = ''    
    fname_statsoutcoli_cll = '' 
    
!   call execute_command_line('mkdir -p '//trim(foldername_cll))   
    call make_dir(trim(foldername_cll))
    
    call SwitchOffFileOutput_cll
    call SwitchOnFileOutput_cll    
    
  end subroutine SetOutputFolder_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetOutputFolder_cll(fname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetOutputFolder_cll(fname)

    character(len=*), intent(out) :: fname  

    fname = foldername_cll
    
  end subroutine GetOutputFolder_cll

  
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Getncritpointsout_cll(ncritpointsout2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Getncritpointsout2_cll(ncritpointsout2)

    integer, intent(out) :: ncritpointsout2

    ncritpointsout2 = ncpout2_cll

  end subroutine Getncritpointsout2_cll



  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine GetOutChannels_cll(chans)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine GetOutChannels_cll(chans)

    integer, intent(out) :: chans(10)

    call Getninfout_cll(chans(1))
    call Getninfoutcoli_cll(chans(2))
    call Getnerrout_cll(chans(3))
    call GetnerroutCOLI_cll(chans(4))
    call GetnerroutDD_cll(chans(5))
    call Getncheckout_cll(chans(6))
    call Getncritpointsout_cll(chans(7))
    call Getncritpointsout2_cll(chans(8))
    call Getncpoutcoli_cll(chans(9))
    call Getnstatsoutcoli_cll(chans(10))
    

  end subroutine GetOutChannels_cll

  
  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCheckCntDB_cll()
  !i
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCheckCntDB_cll

    CheckCntDB_cll = 0
    DiffCntDB_cll = 0

  end subroutine InitCheckCntDB_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitMaxCheckDB_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitMaxCheckDB_cll

    MaxCheckDB_cll = 50

  end subroutine InitMaxCheckDB_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCheckDB_cll(npoints)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCheckDB_cll(npoints)

    integer, intent(in) :: npoints

    MaxCheckDB_cll = npoints

  end subroutine SetMaxCheckDB_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCheckCnt_cll(noreset)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCheckCnt_cll(noreset)
    logical, optional :: noreset
    integer :: Nold
    integer(kind=long_int), allocatable :: saveCnt(:)


    if (present(noreset).and.noreset) then
      Nold = size(CheckCnt_cll)
      if (Nold.lt.Nmax_cll) then
        allocate(saveCnt(Nold))

        saveCnt = CheckCnt_cll  
        deallocate(CheckCnt_cll)
        allocate(CheckCnt_cll(Nmax_cll))      
        CheckCnt_cll(1:Nold) = saveCnt
        CheckCnt_cll(Nold+1:Nmax_cll) = 0

        saveCnt = DiffCnt_cll
        deallocate(DiffCnt_cll)
        allocate(DiffCnt_cll(Nmax_cll))
        DiffCnt_cll(1:Nold) = saveCnt
        DiffCnt_cll(Nold+1:Nmax_cll) = 0

        saveCnt = CheckCntten_cll
        deallocate(CheckCntten_cll)
        allocate(CheckCntten_cll(Nmax_cll))
        CheckCntten_cll(1:Nold) = saveCnt
        CheckCntten_cll(Nold+1:Nmax_cll) = 0

        saveCnt = DiffCntten_cll
        deallocate(DiffCntten_cll)
        allocate(DiffCntten_cll(Nmax_cll))
        DiffCntten_cll(1:Nold) = saveCnt
        DiffCntten_cll(Nold+1:Nmax_cll) = 0
      end if
    else
      if (allocated(CheckCnt_cll)) deallocate(CheckCnt_cll)
      allocate(CheckCnt_cll(Nmax_cll))
      CheckCnt_cll = 0
      if (allocated(DiffCnt_cll)) deallocate(DiffCnt_cll)
      allocate(DiffCnt_cll(Nmax_cll))
      DiffCnt_cll = 0
      DiffCntEc_cll = 0
      if (allocated(CheckCntten_cll)) deallocate(CheckCntten_cll)
      allocate(CheckCntten_cll(Nmax_cll))
      CheckCntten_cll = 0
      if (allocated(DiffCntten_cll)) deallocate(DiffCntten_cll)
      allocate(DiffCntten_cll(Nmax_cll))
      DiffCntten_cll = 0
    endif

  end subroutine InitCheckCnt_cll

  

  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitMaxCheck_cll(noreset)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitMaxCheck_cll(noreset)
    logical, optional :: noreset
    integer :: Nold
    integer, allocatable :: saveMax(:)

    if (present(noreset).and.noreset) then
      Nold = size(MaxCheck_cll)
      if (Nold.lt.Nmax_cll) then
        allocate(saveMax(Nold))

        saveMax = MaxCheck_cll  
        deallocate(MaxCheck_cll)
        allocate(MaxCheck_cll(Nmax_cll))      
        MaxCheck_cll(1:Nold) = saveMax
        MaxCheck_cll(Nold+1:Nmax_cll) = 50
      end if
      
    else  
      if (allocated(MaxCheck_cll)) deallocate(MaxCheck_cll)
      allocate(MaxCheck_cll(Nmax_cll))
      MaxCheck_cll = 50
    end if
    
    MaxCheckEc_cll = 50

  end subroutine InitMaxCheck_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCheckN_cll(npoints,N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCheckN_cll(npoints,N)

    integer, intent(in) :: npoints,N

    MaxCheck_cll(N) = npoints

  end subroutine SetMaxCheckN_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCheckArray_cll(npoints)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCheckArray_cll(npoints)

    integer, intent(in) :: npoints(Nmax_cll)

    MaxCheck_cll = npoints

  end subroutine SetMaxCheckArray_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitMaxCritPointsDB_cll()
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitMaxCritPointsDB_cll

    noutCritPointsMaxDB_cll = 50

  end subroutine InitMaxCritPointsDB_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCritPointsDB_cll(npoints)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCritPointsDB_cll(npoints)

    integer, intent(in) :: npoints

    noutCritPointsMaxDB_cll = npoints

  end subroutine SetMaxCritPointsDB_cll
  
  

  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitMaxCritPoints_cll(noreset)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitMaxCritPoints_cll(noreset)

    logical, optional :: noreset
    integer :: Nold
    integer, allocatable :: saveMax(:)

    if (present(noreset)) then
    if (noreset) then
      Nold = size(noutCritPointsMax_cll)
      if (Nold.lt.Nmax_cll) then
        allocate(saveMax(Nold))
        saveMax = noutCritPointsMax_cll 
        deallocate(noutCritPointsMax_cll)
        allocate(noutCritPointsMax_cll(Nmax_cll))      
        noutCritPointsMax_cll(1:Nold) = saveMax
        noutCritPointsMax_cll(Nold+1:Nmax_cll) = 50
      end if
    end if
      
    else
      if (allocated(noutCritPointsMax_cll)) deallocate(noutCritPointsMax_cll)
      allocate(noutCritPointsMax_cll(Nmax_cll))
      noutCritPointsMax_cll = 50
    end if

  end subroutine InitMaxCritPoints_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCritPointsN_cll(npoints,N)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCritPointsN_cll(npoints,N)

    integer, intent(in) :: npoints,N

    noutCritPointsMax_cll(N) = npoints

  end subroutine SetMaxCritPointsN_cll




  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetMaxCritPointsArray_cll(npoints)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetMaxCritPointsArray_cll(npoints)

    integer, intent(in) :: npoints(Nmax_cll)

    noutCritPointsMax_cll = npoints

  end subroutine SetMaxCritPointsArray_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InfOut_cll(sub,inf,flag)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Suppression of output must be implemented in calling routines!

  subroutine InfOut_cll(sub,inf,flag)

    character(len=*), intent(in) :: sub, inf
    logical, intent(out) :: flag
!    integer, parameter :: maxErrOut=100
  
    flag = .false.
    if (infoutlev_cll.eq.0) return

    InfCnt_cll = InfCnt_cll + 1      
    if(ninfout_cll.ne.closed_cll) then
      if (InfCnt_cll.le.MaxInfOut_cll) then
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*) '***********************************************************'
        write(ninfout_cll,*) 'Info-output NO.', InfCnt_cll
        write(ninfout_cll,*) 'in routine: ', trim(sub)
        write(ninfout_cll,*) trim(inf)
!        call WriteMaster_cll(nerrout_cll)
        flag=.true.
      elseif (InfCnt_cll.eq.MaxInfOut_cll+1) then
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*)
        write(ninfout_cll,*) '***********************************************************'
        write(ninfout_cll,*)
        write(ninfout_cll,*) ' Further output of information will be suppressed '
        write(ninfout_cll,*)
      endif
    endif

  end subroutine InfOut_cll    



  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function findFreeChannel_cll
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function findFreeChannel_cll()    result(nchan)

    integer :: nchan
    integer :: i
    logical :: qopened
    
    qopened = .true.
    i = 100
    do while (qopened.and.(i.le.1000))
      i=i+1
      inquire(i, opened=qopened)
    end do
    nchan = i

  end function findFreeChannel_cll  
  



  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenErrOutFile_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenErrOutFile_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
    
    ! close channel used so far
    if(nerrout_cll.ne.closed_cll) then
      inquire(nerrout_cll, opened=qopened)
      if(qopened.and.nerrout_cll.ne.stdout_cll) close(unit=nerrout_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      nerrout_cll = nchan
      ! return if output shall be written as standard output
      if (nerrout_cll.eq.stdout_cll) return      
      inquire(nerrout_cll, opened=qopened)
      if(qopened) close(unit=nerrout_cll)
    else
      nerrout_cll = findFreeChannel_cll()
    end if     
    
    ! open file 'filename' as unit nerrout_cll
    fname_errout_cll = trim(filename)
    open(unit=nerrout_cll,file=trim(fname_errout_cll),form='formatted',access='sequential',status='replace')
   
    ! write intro
    call WriteIntro_cll(nerrout_cll)
    call date_and_time(date=da,time=ti)
    write(unit=nerrout_cll,fmt=*) '                                                           '
    write(unit=nerrout_cll,fmt=*) '***********************************************************'
    write(unit=nerrout_cll,fmt=*) '                                                           '
    write(unit=nerrout_cll,fmt=*) '   file containing the error output of COLLIER interface   '
    write(unit=nerrout_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=nerrout_cll,fmt=*) '                                                           '
    write(unit=nerrout_cll,fmt=*) '***********************************************************'
    write(unit=nerrout_cll,fmt=*) '                                                           '

  end subroutine OpenErrOutFile_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenErrOutFileCOLI_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenErrOutFileCOLI_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(nerroutcoli_cll.ne.closed_cll) then
      inquire(nerroutcoli_cll, opened=qopened)
      if(qopened.and.nerroutcoli_cll.ne.stdout_cll) close(unit=nerroutcoli_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      nerroutcoli_cll = nchan
      call Setnerrout_coli(nerroutcoli_cll)      
      ! return if output shall be written as standard output
      if (nerroutcoli_cll.eq.stdout_cll) return      
      inquire(nerroutcoli_cll, opened=qopened)
      if(qopened) close(unit=nerroutcoli_cll)
    else
      nerroutcoli_cll = findFreeChannel_cll()
      call Setnerrout_coli(nerroutcoli_cll)      
    end if 
    
    ! open file 'filename' as unit nerroutcoli_cll
    fname_erroutcoli_cll = trim(filename)
    open(unit=nerroutcoli_cll,file=trim(fname_erroutcoli_cll),form='formatted',access='sequential',status='replace')
   
    ! write intro
    call WriteIntro_cll(nerroutcoli_cll)
    call date_and_time(date=da,time=ti)
    write(unit=nerroutcoli_cll,fmt=*) '                                                           '
    write(unit=nerroutcoli_cll,fmt=*) '***********************************************************'
    write(unit=nerroutcoli_cll,fmt=*) '                                                           '
    write(unit=nerroutcoli_cll,fmt=*) '       file containing the error output of COLI            '
    write(unit=nerroutcoli_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=nerroutcoli_cll,fmt=*) '                                                           '
    write(unit=nerroutcoli_cll,fmt=*) '***********************************************************'
    write(unit=nerroutcoli_cll,fmt=*) '                                                           '

  end subroutine OpenErrOutFileCOLI_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenErrOutFileDD_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenErrOutFileDD_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    integer :: outlevel_dd,outchannel_dd,mode34_dd,mode5_dd,mode6_dd
    double precision :: cacc_dd,dacc_dd
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(nerroutdd_cll.ne.closed_cll) then
      inquire(nerroutdd_cll, opened=qopened)
      if(qopened.and.nerroutdd_cll.ne.stdout_cll) close(unit=nerroutdd_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      nerroutdd_cll = nchan
      call  DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
      call  DDsetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,nerroutdd_cll)
      if (erroutlev_cll.gt.0) then
        call  DDsetcout_on(.true.)
      end if
      ! return if output shall be written as standard output
      if (nerroutdd_cll.eq.stdout_cll) return      
      inquire(nerroutdd_cll, opened=qopened)
      if(qopened) close(unit=nerroutdd_cll)
    else
      nerroutdd_cll = findFreeChannel_cll()
      call  DDgetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,outchannel_dd)
      call  DDsetmode(cacc_dd,dacc_dd,mode34_dd,mode5_dd,mode6_dd,outlevel_dd,nerroutdd_cll)
      if (erroutlev_cll.gt.0) then
        call  DDsetcout_on(.true.)
      end if
    end if 
    
    ! open file 'filename' as unit nerroutdd_cll
    fname_erroutdd_cll = trim(filename)
    open(unit=nerroutdd_cll,file=trim(fname_erroutdd_cll),form='formatted',access='sequential',status='replace')
   
    ! write intro
    call WriteIntro_cll(nerroutdd_cll)
    call date_and_time(date=da,time=ti)
    write(unit=nerroutdd_cll,fmt=*) '                                                           '
    write(unit=nerroutdd_cll,fmt=*) '***********************************************************'
    write(unit=nerroutdd_cll,fmt=*) '                                                           '
    write(unit=nerroutdd_cll,fmt=*) '       file containing the error output of DD              '
    write(unit=nerroutdd_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=nerroutdd_cll,fmt=*) '                                                           '
    write(unit=nerroutdd_cll,fmt=*) '***********************************************************'
    write(unit=nerroutdd_cll,fmt=*) '                                                           '

  end subroutine OpenErrOutFileDD_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenCritPointsOutFileCOLI_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenCritPointsOutFileCOLI_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ncpoutcoli_cll.ne.closed_cll) then
      inquire(ncpoutcoli_cll, opened=qopened)
      if(qopened.and.ncpoutcoli_cll.ne.stdout_cll) close(unit=ncpoutcoli_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ncpoutcoli_cll = nchan
      call Setncpout_coli(ncpoutcoli_cll)      
      ! return if output shall be written as standard output
      if (ncpoutcoli_cll.eq.stdout_cll) return      
      inquire(ncpoutcoli_cll, opened=qopened)
      if(qopened) close(unit=ncpoutcoli_cll)
    else
      ncpoutcoli_cll = findFreeChannel_cll()
      call Setncpout_coli(ncpoutcoli_cll)      
    end if
        
    ! open file 'filename' as unit unit=ncpoutcoli_cll
    fname_cpoutcoli_cll = trim(filename)
    open(unit=ncpoutcoli_cll,file=trim(fname_cpoutcoli_cll),form='formatted',access='sequential',status='replace')
!    qopened_critcoli=.true.
   
    ! write intro
    call WriteIntro_cll(ncpoutcoli_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt=*) '     file containing problematic integrals of COLI         '
    write(unit=ncpoutcoli_cll,fmt=*) '     with errors estimated to be above a given limit       '
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt='(A30,Es15.8)') &
        ' Critical precision: critacc =',critacc_cll 
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '
    write(unit=ncpoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=ncpoutcoli_cll,fmt=*) '                                                           '


  end subroutine OpenCritPointsOutFileCOLI_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenCritPointsOutFile_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenCritPointsOutFile_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ncpout_cll.ne.closed_cll) then
      inquire(ncpout_cll, opened=qopened)
      if(qopened.and.ncpout_cll.ne.stdout_cll) close(unit=ncpout_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ncpout_cll = nchan      
      ! return if output shall be written as standard output
      if (ncpout_cll.eq.stdout_cll) return      
      inquire(ncpout_cll, opened=qopened)
      if(qopened) close(unit=ncpout_cll)
    else
      ncpout_cll = findFreeChannel_cll()      
    end if
    
    ! open file 'filename' as unit ncpout_cll
    fname_cpout_cll = trim(filename)
    open(unit=ncpout_cll,file=trim(fname_cpout_cll),form='formatted',access='sequential',status='replace')
!    qopened_crit=.true.
   
    ! write intro
    call WriteIntro_cll(ncpout_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt=*) '***********************************************************'
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt=*) '     file containing problematic integrals with            '
    write(unit=ncpout_cll,fmt=*) '     errors estimated to be above a given limit            '
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt=*) '***********************************************************'
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt='(A30,Es15.8)') &
        ' Critical precision: critacc =',critacc_cll 
    write(unit=ncpout_cll,fmt=*) '                                                           '
    write(unit=ncpout_cll,fmt=*) '***********************************************************'
    write(unit=ncpout_cll,fmt=*)                                         

  end subroutine OpenCritPointsOutFile_cll

  
  
  

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenCritPointsOutFile2_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenCritPointsOutFile2_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ncpout2_cll.ne.closed_cll) then
      inquire(ncpout2_cll, opened=qopened)
      if(qopened.and.ncpout2_cll.ne.stdout_cll) close(unit=ncpout2_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ncpout2_cll = nchan      
      ! return if output shall be written as standard output
      if (ncpout2_cll.eq.stdout_cll) return      
      inquire(ncpout2_cll, opened=qopened)
      if(qopened) close(unit=ncpout2_cll)
    else
      ncpout2_cll = findFreeChannel_cll()      
    end if
    
    ! open file 'filename' as unit ncpout2_cll
    fname_cpout2_cll = trim(filename)
    open(unit=ncpout2_cll,file=trim(fname_cpout2_cll),form='formatted',access='sequential',status='replace')
!    qopened_crit2=.true.
   
    ! write intro
    call WriteIntro_cll(ncpout2_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt=*) '***********************************************************'
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt=*) '     file containing problematic integrals with            '
    write(unit=ncpout2_cll,fmt=*) '     errors estimated to be above a given limit            '
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt=*) '***********************************************************'
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt='(A30,Es15.8)') &
        ' Critical precision: critacc =',critacc_cll
    write(unit=ncpout2_cll,fmt=*) '                                                           '
    write(unit=ncpout2_cll,fmt=*) '***********************************************************'
    write(unit=ncpout2_cll,fmt=*)                                         
  end subroutine OpenCritPointsOutFile2_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenCheckOutFile_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenCheckOutFile_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ncheckout_cll.ne.closed_cll) then
      inquire(ncheckout_cll, opened=qopened)
      if(qopened.and.ncheckout_cll.ne.stdout_cll) close(unit=ncheckout_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ncheckout_cll = nchan   
      ! return if output shall be written as standard output
      if (ncheckout_cll.eq.stdout_cll) return      
      inquire(ncheckout_cll, opened=qopened)
      if(qopened) close(unit=ncheckout_cll)
    else
      ncheckout_cll = findFreeChannel_cll()
    end if 
        
    ! open file 'filename' as unit ncheckout_cll
    fname_checkout_cll = trim(filename)
    open(unit=ncheckout_cll,file=trim(fname_checkout_cll),form='formatted',access='sequential',status='replace')
    qopened_check=.true.
 
    ! write intro
    call WriteIntro_cll(ncheckout_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ncheckout_cll,fmt=*) '                                                          '
    write(unit=ncheckout_cll,fmt=*) '***********************************************************'
    write(unit=ncheckout_cll,fmt=*) '                                                           '
    write(unit=ncheckout_cll,fmt=*) '       file containing integrals which lead to             '
    write(unit=ncheckout_cll,fmt=*) '           different results in COLI and DD                '
    write(unit=ncheckout_cll,fmt=*) '                                                           '
    write(unit=ncheckout_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ncheckout_cll,fmt=*) '                                                           '
    write(unit=ncheckout_cll,fmt=*) '***********************************************************'
    write(unit=ncheckout_cll,fmt=*) '                                                           '
    write(unit=ncheckout_cll,fmt='(A28,Es15.8)') &
        ' Check precision: checkacc =',checkacc_cll 
    write(unit=ncheckout_cll,fmt=*) '                                                           '
    write(unit=ncheckout_cll,fmt=*) '***********************************************************'
    write(unit=ncheckout_cll,fmt=*) '                                                           '

  end subroutine OpenCheckOutFile_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenInfOutFile_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenInfOutFile_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ninfout_cll.ne.closed_cll) then
      inquire(ninfout_cll, opened=qopened)
      if(qopened.and.ninfout_cll.ne.stdout_cll) close(unit=ninfout_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ninfout_cll = nchan
      call Setninfout_cache(ninfout_cll)      
      ! return if output shall be written as standard output
      if (ninfout_cll.eq.stdout_cll) return      
      inquire(ninfout_cll, opened=qopened)
      if(qopened) close(unit=ninfout_cll)
    else
      ninfout_cll = findFreeChannel_cll()
      call Setninfout_cache(ninfout_cll)      
    end if
    
    ! open file 'filename' as unit ninfout_cll
    fname_infout_cll = trim(filename)
    open(unit=ninfout_cll,file=trim(fname_infout_cll),form='formatted',access='sequential',status='replace')

    ! write intro
    call WriteIntro_cll(ninfout_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ninfout_cll,fmt=*) '                                                          '
    write(unit=ninfout_cll,fmt=*) '***********************************************************'
    write(unit=ninfout_cll,fmt=*) '                                                           '
    write(unit=ninfout_cll,fmt=*) '       file containing the info output of COLLIER          '
    write(unit=ninfout_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ninfout_cll,fmt=*) '                                                           '
    write(unit=ninfout_cll,fmt=*) '***********************************************************'
    write(unit=ninfout_cll,fmt=*) '                                                           '

  end subroutine OpenInfOutFile_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenInfOutFileCOLI_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenInfOutFileCOLI_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) ::nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(ninfoutcoli_cll.ne.closed_cll) then
      inquire(ninfoutcoli_cll, opened=qopened)
      if(qopened.and.ninfoutcoli_cll.ne.stdout_cll) close(unit=ninfoutcoli_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      ninfoutcoli_cll = nchan
      call Setninfout_coli(ninfoutcoli_cll)      
      ! return if output shall be written as standard output
      if (ninfoutcoli_cll.eq.stdout_cll) return      
      inquire(ninfoutcoli_cll, opened=qopened)
      if(qopened) close(unit=ninfoutcoli_cll)
    else
      ninfoutcoli_cll = findFreeChannel_cll()
      call Setninfout_coli(ninfoutcoli_cll)      
    end if
    
    ! open file 'filename' as unit ninfoutcoli_cll
    fname_infoutcoli_cll = trim(filename)
    open(unit=ninfoutcoli_cll,file=trim(fname_infoutcoli_cll),form='formatted',access='sequential')
   
    ! write intro
    call WriteIntro_cll(ninfoutcoli_cll)
    call date_and_time(date=da,time=ti)
    write(unit=ninfoutcoli_cll,fmt=*) '                                                          '
    write(unit=ninfoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=ninfoutcoli_cll,fmt=*) '                                                           '
    write(unit=ninfoutcoli_cll,fmt=*) '          file containing info output of COLI              '
    write(unit=ninfoutcoli_cll,fmt=*) '                                                           '
    write(unit=ninfoutcoli_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=ninfoutcoli_cll,fmt=*) '                                                           '
    write(unit=ninfoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=ninfoutcoli_cll,fmt=*) '                                                           '

  end subroutine OpenInfOutFileCOLI_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine OpenStatisticsOutFileCOLI_cll(filename,nchan)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine OpenStatisticsOutFileCOLI_cll(filename,nchan)

    character(len=*), intent(in) :: filename
    integer, optional, intent(in) :: nchan
    character(len=8) :: da
    character(len=10) :: ti
    logical :: qopened
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
     
    ! close channel used so far
    if(nstatsoutcoli_cll.ne.closed_cll) then
      inquire(nstatsoutcoli_cll, opened=qopened)
      if(qopened.and.nstatsoutcoli_cll.ne.stdout_cll) close(unit=nstatsoutcoli_cll)
    end if

    ! set new channel and close if open
    if (present(nchan)) then
      nstatsoutcoli_cll = nchan
      call Setnstatsout_coli(nstatsoutcoli_cll)      
      ! return if output shall be written as standard output
      if (nstatsoutcoli_cll.eq.stdout_cll) return      
      inquire(nstatsoutcoli_cll, opened=qopened)
      if(qopened) close(unit=nstatsoutcoli_cll)
    else
      nstatsoutcoli_cll = findFreeChannel_cll()
      call Setnstatsout_coli(nstatsoutcoli_cll)      
    end if

    ! open file 'filename' as unit nstatsoutcoli_cll
    fname_statsoutcoli_cll = trim(filename)
    open(unit=nstatsoutcoli_cll,file=trim(fname_statsoutcoli_cll),form='formatted',access='sequential',status='replace')
!    qopened_statscoli=.true.

    ! write intro
    ! call WriteIntro_cll(nstatsoutcoli_cll)
    call date_and_time(date=da,time=ti)
    write(unit=nstatsoutcoli_cll,fmt=*) '                                                           '
    write(unit=nstatsoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=nstatsoutcoli_cll,fmt=*) '                                                           '
    write(unit=nstatsoutcoli_cll,fmt=*) '       file containing statistics of calls in COLI         '
    write(unit=nstatsoutcoli_cll,fmt=*) '       of C and D integral reduction functions             '
    write(unit=nstatsoutcoli_cll,fmt=*) '                                                           '
    write(unit=nstatsoutcoli_cll,fmt=*) '               created  ', da(7:8), '/', da(5:6), '/', da(1:4),  &
                         '  ', ti(1:2), ':', ti(3:4)
    write(unit=nstatsoutcoli_cll,fmt=*) '                                                           '
    write(unit=nstatsoutcoli_cll,fmt=*) '***********************************************************'
    write(unit=nstatsoutcoli_cll,fmt=*) '                                                           '

  end subroutine OpenStatisticsOutFilecoli_cll



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine PrintStatisticscoli_cll(noutch)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PrintStatisticscoli_cll(noutch)

    integer, intent(in) :: noutch

!    if (nout.ne.0) then 
!      call Setnstatsoutcoli_cll(nout)
!     call Setnstatsout_coli(nout)
!     if (nout.ne.6) then
!       call OpenStatisticsOutFileCOLI_cll('output_cll/StatisticsOut.coli')
!     end if
!    end if 
    
    ! return if output into files is suppressed
    if (nofiles_cll) return
    
     call OpenStatisticsOutFileCOLI_cll(trim(foldername_cll)//'/StatisticsOut.coli',noutch)

     call PrintStatistics_coli


  end subroutine PrintStatisticscoli_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine WriteIntro_cll(un)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine WriteIntro_cll(un)

    integer, intent(in) :: un
    write(unit=un,fmt=*) '                                                           '
    write(unit=un,fmt=*) '        *******************************************        '
    write(unit=un,fmt=*) '        *              C O L L I E R              *        '
    write(unit=un,fmt=*) '        *                                         *        '
    write(unit=un,fmt=*) '        *        Complex One-Loop Library         *        ' 
    write(unit=un,fmt=*) '        *      In Extended Regularizations        *        '
    write(unit=un,fmt=*) '        *                                         *        '
    write(unit=un,fmt=*) '        *    by A.Denner, S.Dittmaier, L.Hofer    *        '
    write(unit=un,fmt=*) '        *                                         *        '
!   write(unit=un,fmt=*) '        *              version 1.1.x              *        '
    write(unit=un,fmt=*) '        *              version '//version_cll//'              *        '
    write(unit=un,fmt=*) '        *                                         *        '    
    write(unit=un,fmt=*) '        *******************************************        '
    write(unit=un,fmt=*) '                                                           ' 

  end subroutine WriteIntro_cll

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine make_dir(dirname)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine make_dir(dirname)
    use iso_c_binding, only : c_int
    integer(c_int) :: err
    character(len=*), intent(in) :: dirname

    interface
        function c_mkdir(dirname, mode) bind(c, name='mkdir')
            use iso_c_binding, only : c_char, c_int
            integer(c_int)                    :: c_mkdir
            character(c_char), intent(in)     :: dirname(*)
            integer(c_int), intent(in), value :: mode
        end function
    end interface

    err=c_mkdir(dirname // char(0), 511)

!  err = -1 if directory exists or if it cannot be created!
!   if (err > 0) then
!       write(*,*) 'Directory ',dirname,' could not be created'
!       stop
!   end if
  end subroutine


end module collier_init
