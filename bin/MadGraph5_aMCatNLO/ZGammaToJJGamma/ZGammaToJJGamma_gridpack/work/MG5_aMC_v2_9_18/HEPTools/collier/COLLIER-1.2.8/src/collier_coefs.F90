!!
!!  File collier_coefs.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       *******************************************
!       *              C O L L I E R              * 
!       *                                         *
!       *        Complex One-Loop Library         *        
!       *      In Extended Regularizations        *
!       *                                         *
!       *    by A.Denner, S.Dittmaier, L.Hofer    *
!       *                                         *
!       *******************************************
! 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module collier_coefs

  use combinatorics
  use collier_global
  use collier_init
  use collier_aux
  use reductionTN

  implicit none



  interface B_cll
    module procedure B_main_cll,B_arrays_cll, &
                     B_list_cll,B_arrays_list_cll
  end interface B_cll


  interface C_cll
    module procedure C_main_cll,C_arrays_cll, &
                     C_list_cll,C_arrays_list_cll
  end interface C_cll


  interface D_cll
    module procedure D_main_cll,D_arrays_cll, &
                     D_list_cll,D_arrays_list_cll
  end interface D_cll


  interface E_cll
    module procedure E_main_cll,E_arrays_cll, &
                     E_list_cll,E_arrays_list_cll
  end interface E_cll


  interface F_cll
    module procedure F_main_cll,F_arrays_cll, &
                     F_list_cll,F_arrays_list_cll
  end interface F_cll


  interface G_cll
    module procedure G_main_cll,G_arrays_cll, &
                     G_list_cll,G_arrays_list_cll
  end interface G_cll


  interface TN_cll
    module procedure TN_main_cll,T1_cll
  end interface TN_cll
  

  interface B0_cll
    module procedure B0_main_cll,B0_arrays_cll
  end interface B0_cll


  interface C0_cll
    module procedure C0_main_cll,C0_arrays_cll
  end interface C0_cll


  interface D0_cll
    module procedure D0_main_cll,D0_arrays_cll
  end interface D0_cll


  interface E0_cll
    module procedure E0_main_cll,E0_arrays_cll
  end interface E0_cll


  interface F0_cll
    module procedure F0_main_cll,F0_arrays_cll
  end interface F0_cll


  interface DB0_cll
    module procedure DB0_main_cll,DB0_arrays_cll
  end interface DB0_cll


  interface DB1_cll
    module procedure DB1_main_cll,DB1_arrays_cll
  end interface DB1_cll


  interface DB00_cll
    module procedure DB00_main_cll,DB00_arrays_cll
  end interface DB00_cll
 

  interface DB11_cll
    module procedure DB11_main_cll,DB11_arrays_cll
  end interface DB11_cll
  

  interface DB_cll
    module procedure DB_main_cll,DB_arrays_cll
!                     DB_list_cll,DB_arrays_list_cll
  end interface DB_cll



contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine A_cll(A,Auv,m02,rmax,Aerr,id_in)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine A_cll(A,Auv,m02,rmax,Aerr,id_in)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: m02
    double complex :: mm02
    double complex, intent(out) :: Auv(0:rmax/2), A(0:rmax/2)
    double precision, optional, intent(out) :: Aerr(0:rmax)
    integer, optional, intent(in) :: id_in
    double complex :: A2uv(0:rmax/2), A2(0:rmax/2)
    double complex :: Adduv(0:rmax/2), Add(0:rmax/2)
    double precision :: Aerraux(0:rmax),Adiff(0:rmax)
    double complex :: args(1)
    integer :: n0, i, rank,errflag,id
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD),Aacc(0:rmax)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: norm,norm_coli,norm_dd
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag

    if (1.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('A_cll','Nmax_cll smaller 1',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 1'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('A_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      args(1) = m02
      call SetMasterFname_cll('A_cll')
      call SetMasterN_cll(1)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(1,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcA(A,Auv,m02,rmax,Aerraux)
        if (abs(A(0)).ne.0d0) then
          Aacc=Aerraux/abs(A(0))
        else
          Aacc=0d0
        end if
        if (present(Aerr))  Aerr=Aerraux
        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0

        ! replace small masses by DD-identifiers
        mm02 = getminf2DD_cll(m02)

        rank = rmax
        call A_dd(Add,Adduv,mm02,rank,id)

        do n0=0,rank/2
          A(n0) = Add(n0)
          Auv(n0) = Adduv(n0)
        end do

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        if (present(Aerr)) then
          Aerr(0:rmax) = accabsDD(0:rmax)
        endif
        if (abs(A(0)).ne.0d0) then
          Aacc=accabsDD(0:rmax)/abs(A(0))
        else
          Aacc=0d0
        end if
        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcA(A,Auv,m02,rmax,Aerraux)


        ! calculate loop integral using DD

        id=0

        ! replace small masses by DD-identifiers 
        mm02 = getminf2DD_cll(m02)

        rank = rmax
        call A_dd(Add,Adduv,mm02,rank,id)
        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        
        do n0=0,rank/2
          A2(n0) = Add(n0)
          A2uv(n0) = Adduv(n0)
        end do


        ! cross-check

        norm_coli = abs(A(0))
        if(norm_coli.eq.0d0) norm_coli = muuv2_cll
        norm_dd = abs(A2(0))
        if(norm_coli.eq.0d0) norm_dd = muuv2_cll
        norm = min(norm_coli,norm_dd)

        call CheckCoefsA_cll(A,A2,m02,rmax,norm,Adiff)
        
        if (Aerraux(rmax).lt.accabsDD(rmax)) then
          if (present(Aerr)) Aerr = max(Aerraux,Adiff)         
          Aacc = max(Aerraux/norm_coli,Adiff/norm)
          if (Monitoring) PointsCntA_coli = PointsCntA_coli + 1
        else
          A = A2
          Auv = A2uv
          if (present(Aerr))  Aerr = max(accabsDD(0:rmax),Adiff)  
          Aacc = max(accabsDD(0:rmax)/norm_dd,Adiff/norm)          
          if (Monitoring) PointsCntA_dd = PointsCntA_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)

    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntA_cll =  PointsCntA_cll + 1
      
      if(maxval(Aacc).gt.reqacc_cll) AccPointsCntA_cll =  AccPointsCntA_cll + 1
      if(maxval(Aacc).gt.sreqacc_cll) sAccPointsCntA_cll =  sAccPointsCntA_cll + 1
      
      if(maxval(Aacc).gt.critacc_cll) then
        CritPointsCntA_cll =  CritPointsCntA_cll + 1
        if ( CritPointsCntA_cll.le.noutCritPointsMax_cll(1)) then
          call CritPointsOut_cll('A_cll',0,maxval(Aacc), CritPointsCntA_cll)
          if( CritPointsCntA_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for A_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if

    end if


  end subroutine A_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B_main_cll(B,Buv,p10,m02,m12,rmax,Berr,id_in)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B_main_cll(B,Buv,p10,m02,m12,rmax,Berr,id_in)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: Buv(0:rmax/2,0:rmax)
    double complex, intent(out) :: B(0:rmax/2,0:rmax)
    double precision, optional, intent(out) :: Berr(0:rmax)
    integer, optional, intent(in) :: id_in    
    double complex :: B2uv(0:rmax/2,0:rmax), B2(0:rmax/2,0:rmax)
    double complex :: Bcoliuv(0:rmax,0:rmax) 
    double complex :: Bcoli(0:rmax,0:rmax)
    double complex :: Bdduv(0:rmax,0:rmax) 
    double complex :: Bdd(0:rmax,0:rmax)
    double precision :: Berraux(0:rmax),Bdiff(0:rmax)
    double complex :: args(3)
    integer :: n0,rank,errflag,id,r
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: Bacc(0:rmax),Bacc2(0:rmax),norm,norm_coli,norm_dd
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = m02
      args(3) = m12
      call SetMasterFname_cll('B_cll')
      call SetMasterN_cll(2)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(3,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcB(Bcoli,Bcoliuv,p10,m02,m12,rmax,id,Berraux)

        norm = maxval(abs(Bcoli(0,0:rmax)))
        if (norm.ne.0d0) then
          Bacc = Berraux/norm
        else
          Bacc = Berraux
        end if

        if (present(Berr)) then
          Berr = Berraux
        end if

        if (mflag) call PropagateAccFlag_cll(Bacc,rmax)

        B(0:rmax/2,0:rmax) = Bcoli(0:rmax/2,0:rmax)
        Buv(0:rmax/2,0:rmax) = Bcoliuv(0:rmax/2,0:rmax)

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        rank = rmax
        call B_dd(Bdd,Bdduv,q10,mm02,mm12,rank,id)
        do n0=0,rank/2
          B(n0,0:rank) = Bdd(n0,0:rank)
          Buv(n0,0:rank) = Bdduv(n0,0:rank)
        end do

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        if (present(Berr)) then
          Berr(0:rmax) = accabsDD(0:rmax)
        end if
        
        norm = maxval(abs(B(0,0:rmax)))
        if (norm.ne.0d0) then
          Bacc = accabsDD(0:rmax)/norm
        else
          Bacc = accabsDD(0:rmax)
        end if
        if (mflag) call PropagateAccFlag_cll(Bacc,rmax)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcB(Bcoli,Bcoliuv,p10,m02,m12,rmax,id,Berraux)

        B(0:rmax/2,0:rmax) = Bcoli(0:rmax/2,0:rmax)
        Buv(0:rmax/2,0:rmax) = Bcoliuv(0:rmax/2,0:rmax)
        

        ! calculate loop integral using DD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        rank = rmax
        call B_dd(Bdd,Bdduv,q10,mm02,mm12,rank,0)
        do n0=0,rank/2
          B2(n0,0:rmax) = Bdd(n0,0:rmax)
          B2uv(n0,0:rmax) = Bdduv(n0,0:rmax)
        end do
        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)

        norm_coli = maxval(abs(B(0,0:rmax)))         
        if (norm_coli.eq.0d0) norm_coli = 1d0
        norm_dd = maxval(abs(B2(0,0:rmax)))         
        if (norm_dd.eq.0d0) norm_dd = 1d0
        norm = min(norm_coli,norm_dd)
            
        ! cross-check
        call CheckCoefsB_cll(B,B2,p10,m02,m12,rmax,norm,Bdiff)
        
        if (Berraux(rmax).lt.accabsDD(rmax)) then
          if (present(Berr))  Berr = max(Berraux,Bdiff)
          Bacc = max(Berraux/norm_coli,Bdiff/norm)
          if (Monitoring) PointsCntB_coli =  PointsCntB_coli + 1
        else
          B = B2
          Buv = B2uv
          if (present(Berr))  Berr = max(accabsDD(0:rmax),Bdiff)
          Bacc = max(accabsDD(0:rmax)/norm_dd,Bdiff/norm)
          if (Monitoring) PointsCntB_dd =  PointsCntB_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Bacc,rmax)
        
    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntB_cll =  PointsCntB_cll + 1

      if(maxval(Bacc).gt.reqacc_cll) AccPointsCntB_cll =  AccPointsCntB_cll + 1
      if(maxval(Bacc).gt.sreqacc_cll) sAccPointsCntB_cll =  sAccPointsCntB_cll + 1

      if(maxval(Bacc).gt.critacc_cll) then
        CritPointsCntB_cll =  CritPointsCntB_cll + 1
        if ( CritPointsCntB_cll.le.noutCritPointsMax_cll(2) ) then
          call CritPointsOut_cll('B_cll',0,maxval(Bacc), CritPointsCntB_cll)
          if( CritPointsCntB_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout_cll,*) ' Further output of Critical Points for B_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if

    end if


  end subroutine B_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B_arrays_cll(B,Buv,MomInv,masses2,rmax,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B_arrays_cll(B,Buv,MomInv,masses2,rmax,Berr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: Buv(0:rmax/2,0:rmax)
    double complex, intent(out) :: B(0:rmax/2,0:rmax)
    double precision, optional, intent(out) :: Berr(0:rmax)
    double precision :: Berraux(0:rmax)
    
    if (present(Berr)) then
      call B_main_cll(B,Buv,MomInv(1),masses2(0),masses2(1),rmax,Berr)
    else
      call B_main_cll(B,Buv,MomInv(1),masses2(0),masses2(1),rmax,Berraux)
    end if

  end subroutine B_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B_list_cll(B,Buv,p10,m02,m12,rmax,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B_list_cll(B,Buv,p10,m02,m12,rmax,Berr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(out) :: Buv(1:),B(1:)
    double precision, optional, intent(out) ::  Berr(0:rmax)
    double precision :: Berraux(0:rmax)
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call B_list_checked_cll(B,Buv,p10,m02,m12,rmax,Berr)
  
  end subroutine B_list_cll
 

  subroutine B_list_checked_cll(B,Buv,p10,m02,m12,rmax,Berr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(out) :: Buv(1:NCoefs(rmax,2)),B(1:NCoefs(rmax,2))
    double precision, optional, intent(out) ::  Berr(0:rmax)
    double complex :: Buv_aux(0:rmax/2,0:rmax), B_aux(0:rmax/2,0:rmax)
    double precision :: Berraux(0:rmax)
    integer :: r,n0,n1,cnt

    if (present(Berr)) then
      call B_main_cll(B_aux,Buv_aux,p10,m02,m12,rmax,Berr)
    else
      call B_main_cll(B_aux,Buv_aux,p10,m02,m12,rmax,Berraux)
    end if
    
    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        n1 = r-2*n0

        cnt=cnt+1
        B(cnt) = B_aux(n0,n1)
        Buv = Buv_aux(n0,n1)
      end do
    end do

  end subroutine B_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B_arrays_list_cll(B,Buv,MomInv,masses2,rmax,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B_arrays_list_cll(B,Buv,MomInv,masses2,rmax,Berr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(1),masses2(0:1)
    double precision, optional, intent(out) :: Berr(0:rmax)
    double complex, intent(out) :: Buv(1:),B(1:)
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('B_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call B_arrays_list_checked_cll(B,Buv,MomInv,masses2,rmax,Berr)
  
  end subroutine B_arrays_list_cll
  

  subroutine B_arrays_list_checked_cll(B,Buv,MomInv,masses2,rmax,Berr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(1),masses2(0:1)
    double complex, intent(out) :: Buv(1:NCoefs(rmax,2)),B(1:NCoefs(rmax,2))
    double precision, optional, intent(out) :: Berr(0:rmax)
    double complex :: Buv_aux(0:rmax/2,0:rmax), B_aux(0:rmax/2,0:rmax)
    double precision :: Berraux(0:rmax)
    integer :: r,n0,n1,cnt

    if (present(Berr)) then
      call B_main_cll(B_aux,Buv_aux,MomInv(1),masses2(0),masses2(1),rmax,Berr)
    else     
      call B_main_cll(B_aux,Buv_aux,MomInv(1),masses2(0),masses2(1),rmax,Berraux)
    end if
    
    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        n1 = r-2*n0

        cnt=cnt+1
        B(cnt) = B_aux(n0,n1)
        Buv = Buv_aux(n0,n1)
      end do
    end do

  end subroutine B_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C_main_cll(C,Cuv,p10,p21,p20,m02,m12,m22,rmax,Cerr,id_in,Cerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C_main_cll(C,Cuv,p10,p21,p20,m02,m12,m22,rmax,Cerr,id_in,Cerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double precision :: q10,q21,q20
    double complex :: mm02,mm12,mm22
    double complex, intent(out) :: Cuv(0:rmax/2,0:rmax,0:rmax)
    double complex, intent(out) :: C(0:rmax/2,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    integer, optional, intent(in) :: id_in    
    double complex :: C2uv(0:rmax/2,0:rmax,0:rmax),C2(0:rmax/2,0:rmax,0:rmax)
    double complex :: Ccoliuv(0:rmax,0:rmax,0:rmax),Ccoli(0:rmax,0:rmax,0:rmax)
    double complex :: Cdduv(0:rmax,0:rmax,0:rmax) 
    double complex :: Cdd(0:rmax,0:rmax,0:rmax)
    double precision :: Cerraux(0:rmax),Cerr2aux(0:rmax)
    double complex :: elimcminf2
    double complex args(6)
    integer :: n0,rank,errflag,id
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: Cacc(0:rmax),norm,norm_coli,norm_dd,Cacc2(0:rmax),Cdiff(0:rmax)
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag
    integer :: r,n1,n2

    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if


    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = p21
      args(3) = p20
      args(4) = m02
      args(5) = m12
      args(6) = m22
      call SetMasterFname_cll('C_cll')
      call SetMasterN_cll(3)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(6,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcC(Ccoli,Ccoliuv,p10,p21,p20,m02,m12,m22,rmax,id,Cerraux,Cerr2aux)
        
        norm = abs(Ccoli(0,0,0))
        do r=1,rmax
          do n1=0,r
            n2=r-n1
            norm =  max(norm,abs(Ccoli(0,n1,n2)))
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p20),abs(m02),abs(m12),abs(m22))
          if(norm.ne.0d0) then
            norm=1d0/norm
          else
            norm=1d0/muir2_cll
          end if 
        end if
        
        if (norm.ne.0d0) then
          Cacc = Cerraux/norm
          Cacc2 = Cerr2aux/norm
        else
          Cacc = 0d0
          Cacc2 = 0d0
        end if 

        if (present(Cerr))  Cerr = Cerraux
        if (present(Cerr2)) Cerr2 = Cerr2aux

        if (mflag) call PropagateAccFlag_cll(Cacc,rmax)

        C(0:rmax/2,0:rmax,0:rmax) = Ccoli(0:rmax/2,0:rmax,0:rmax)
        Cuv(0:rmax/2,0:rmax,0:rmax) = Ccoliuv(0:rmax/2,0:rmax,0:rmax)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q20 = dreal(getminf2DD_cll(p20))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)

        rank = rmax
        call C_dd(Cdd,Cdduv,q10,q21,q20,mm02,mm12,mm22,rank,id)
        C(0:rank/2,0:rank,0:rank) = Cdd(0:rank/2,0:rank,0:rank)
        Cuv(0:rank/2,0:rank,0:rank) = Cdduv(0:rank/2,0:rank,0:rank)

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        if(present(Cerr)) Cerr(0:rmax) = accabsDD(0:rmax)
        if(present(Cerr2)) Cerr2(0:rmax) = accabs2DD(0:rmax)

        norm = abs(C(0,0,0))
        do r=1,rmax
          do n1=0,r
            n2=r-n1
            norm =  max(norm,abs(C(0,n1,n2)))
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p20),abs(m02),abs(m12),abs(m22))
          if(norm.ne.0d0) then
            norm=1d0/norm
          else
            norm=1d0/muir2_cll
          end if 
        end if
        if (norm.ne.0d0) then
          Cacc = accabsDD(0:rmax)/norm
          Cacc2 = accabs2DD(0:rmax)/norm
        else
          Cacc(r) = 0d0
          Cacc2(r) = 0d0
        end if
        if (mflag) call PropagateAccFlag_cll(Cacc,rmax)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcC(Ccoli,Ccoliuv,p10,p21,p20,m02,m12,m22,rmax,id,Cerraux,Cerr2aux)
        
        C(0:rmax/2,0:rmax,0:rmax) = Ccoli(0:rmax/2,0:rmax,0:rmax)
        Cuv(0:rmax/2,0:rmax,0:rmax) = Ccoliuv(0:rmax/2,0:rmax,0:rmax) 


        ! calculate loop integral using DD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q20 = dreal(getminf2DD_cll(p20))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)

        rank = rmax
        call C_dd(Cdd,Cdduv,q10,q21,q20,mm02,mm12,mm22,rank,id)
        C2(0:rank/2,0:rank,0:rank) = Cdd(0:rank/2,0:rank,0:rank)
        C2uv(0:rank/2,0:rank,0:rank) = Cdduv(0:rank/2,0:rank,0:rank)

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
         
        ! cross-check
        norm_coli = abs(C(0,0,0))
        norm_dd = abs(C2(0,0,0))
        do r=1,rmax
          do n1=0,r
            n2=r-n1
            norm_coli =  max(norm_coli,abs(C(0,n1,n2)))
            norm_dd =  max(norm_dd,abs(C(0,n1,n2)))
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(p10),abs(p21),abs(p20),abs(m02),abs(m12),abs(m22))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli
          else
            norm_coli=1d0/muir2_cll
          end if 
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(p10),abs(p21),abs(p20),abs(m02),abs(m12),abs(m22))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd
          else
            norm_dd=1d0/muir2_cll
          end if 
        end if
        norm = min(norm_coli,norm_dd)        

        call CheckCoefsC_cll(C,C2,p10,p21,p20,m02,m12,m22,rmax,norm,Cdiff)
        
        
        if (Cerraux(rmax).lt.accabsDD(rmax)) then
          if (present(Cerr))  Cerr = max(Cerraux,Cdiff)
          if (present(Cerr2)) Cerr2 = Cerr2aux
          Cacc = max(Cerraux/norm_coli,Cdiff/norm)
          Cacc2 = Cerr2aux/norm_coli
          if (Monitoring) PointsCntC_coli =  PointsCntC_coli + 1
        else
          C = C2
          Cuv = C2uv
          if (present(Cerr))  Cerr = max(accabsDD(0:rmax),Cdiff)
          if (present(Cerr2)) Cerr2 = accabs2DD(0:rmax)    
          Cacc = max(accabsDD(0:rmax)/norm_dd,Cdiff/norm)
          Cacc2 = accabs2DD(0:rmax)/norm_dd
          if (Monitoring) PointsCntC_dd =  PointsCntC_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Cacc,rmax)

    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntC_cll =  PointsCntC_cll + 1

      if(maxval(Cacc).gt.reqacc_cll) AccPointsCntC_cll =  AccPointsCntC_cll + 1
      if(maxval(Cacc).gt.sreqacc_cll) sAccPointsCntC_cll =  sAccPointsCntC_cll + 1

      if(maxval(Cacc).gt.critacc_cll) then
        CritPointsCntC_cll =  CritPointsCntC_cll + 1
        if ( CritPointsCntC_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut_cll('C_cll',0,maxval(Cacc), CritPointsCntC_cll)
          if( CritPointsCntC_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout_cll,*) ' Further output of Critical Points for C_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if

!        write(ncpout_cll,*) 'Cerr_coli =',Cerraux
!        write(ncpout_cll,*) 'Cerr_dd   =',accabsDD(0:rmax)
!        write(ncpout_cll,*) 'norm_coli =',norm_coli
!        write(ncpout_cll,*) 'norm_dd =',norm_dd
!        write(ncpout_cll,*) 'Cacc_coli =',Cerraux/norm_coli
!        write(ncpout_cll,*) 'Cacc_dd   =',accabsDD(0:rmax)/norm_dd
!        write(ncpout_cll,*) 'Cdiff   =',Cdiff
!        write(ncpout_cll,*) 'norm   =',norm
!        write(ncpout_cll,*) 'Caccdiff   =',Cdiff/norm
!
!        write(ncpout_cll,*) 'Cacc   =',Cacc
!       write(ncpout_cll,*) 'Cacc   =',Cerraux(rmax).lt.accabsDD(rmax)
!

      end if

#ifdef CritPoints2      
      if(maxval(Cacc2).gt.reqacc_cll) AccPointsCntC2_cll =  AccPointsCntC2_cll + 1
      if(maxval(Cacc2).gt.sreqacc_cll) sAccPointsCntC2_cll =  sAccPointsCntC2_cll + 1

      if(maxval(Cacc2).gt.critacc_cll) then
        CritPointsCntC2_cll =  CritPointsCntC2_cll + 1
        if ( CritPointsCntC2_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut2_cll('C_cll',0,maxval(Cacc2), CritPointsCntC2_cll)
          if( CritPointsCntC2_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for C_cll suppressed '   
            write(ncpout2_cll,*)
          endif
        end if
      end if
#endif

    end if


  end subroutine C_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C_arrays_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C_arrays_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(3), masses2(0:2)
    double complex, intent(out) :: Cuv(0:rmax/2,0:rmax,0:rmax)
    double complex, intent(out) :: C(0:rmax/2,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    double precision :: Cerraux(0:rmax),Cerr2aux(0:rmax)
    
    if (present(Cerr)) then
      if (present(Cerr2)) then
        call C_main_cll(C,Cuv,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerr,Cerr2=Cerr2)
      else
        call C_main_cll(C,Cuv,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerr,Cerr2=Cerr2aux)
      end if
    else
      if (present(Cerr2)) then
        call C_main_cll(C,Cuv,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerraux,Cerr2=Cerr2)
      else
        call C_main_cll(C,Cuv,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerraux,Cerr2=Cerr2aux)
      end if
    end if

  end subroutine C_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C_list_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C_list_cll(C,Cuv,p10,p21,p20,m02,m12,m22,rmax,Cerr,Cerr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double complex, intent(out) :: Cuv(:),C(:)
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    logical :: eflag
    
    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call C_list_checked_cll(C,Cuv,p10,p21,p20,m02,m12,m22,rmax,Cerr,Cerr2)
      
  end subroutine C_list_cll

      
  subroutine C_list_checked_cll(C,Cuv,p10,p21,p20,m02,m12,m22,rmax,Cerr,Cerr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    double complex, intent(out) :: Cuv(NCoefs(rmax,3)),C(NCoefs(rmax,3))
    double complex :: Cuv_aux(0:rmax/2,0:rmax,0:rmax)
    double complex :: C_aux(0:rmax/2,0:rmax,0:rmax)
    double precision :: Cerraux(0:rmax),Cerr2aux(0:rmax)
    integer :: r,n0,n1,n2,cnt
    logical :: eflag
    
    if (present(Cerr)) then
      if (present(Cerr2)) then
        call C_main_cll(C_aux,Cuv_aux,p10,p21,p20, &
            m02,m12,m22,rmax,Cerr,Cerr2=Cerr2)
      else
        call C_main_cll(C_aux,Cuv_aux,p10,p21,p20, &
            m02,m12,m22,rmax,Cerr,Cerr2=Cerr2aux)
      end if
    else
      if (present(Cerr2)) then
        call C_main_cll(C_aux,Cuv_aux,p10,p21,p20, &
            m02,m12,m22,rmax,Cerraux,Cerr2=Cerr2)
      else
        call C_main_cll(C_aux,Cuv_aux,p10,p21,p20, &
            m02,m12,m22,rmax,Cerraux,Cerr2=Cerr2aux)
      end if
    end if

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          n2=r-2*n0-n1

          cnt=cnt+1
          C(cnt) = C_aux(n0,n1,n2)
          Cuv(cnt) = Cuv_aux(n0,n1,n2)

        end do
      end do
    end do

  end subroutine C_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C_arrays_list_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C_arrays_list_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(3), masses2(0:2)
    double complex, intent(out) :: Cuv(:),C(:)
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    logical :: eflag
    
    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('C_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call C_arrays_list_checked_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
      
  end subroutine C_arrays_list_cll
      

  subroutine C_arrays_list_checked_cll(C,Cuv,MomInv,masses2,rmax,Cerr,Cerr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(3), masses2(0:2)
    double complex, intent(out) :: Cuv(NCoefs(rmax,3)),C(NCoefs(rmax,3))
    double precision, optional, intent(out) :: Cerr(0:rmax),Cerr2(0:rmax)
    double complex :: Cuv_aux(0:rmax/2,0:rmax,0:rmax)
    double complex :: C_aux(0:rmax/2,0:rmax,0:rmax)
    double precision :: Cerraux(0:rmax),Cerr2aux(0:rmax)
    integer :: r,n0,n1,n2,cnt
    
    if (present(Cerr)) then
      if (present(Cerr2)) then
        call C_main_cll(C_aux,Cuv_aux,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerr,Cerr2=Cerr2)
      else
        call C_main_cll(C_aux,Cuv_aux,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerr,Cerr2=Cerr2aux)
      end if
    else
      if (present(Cerr2)) then
        call C_main_cll(C_aux,Cuv_aux,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerraux,Cerr2=Cerr2)
      else
        call C_main_cll(C_aux,Cuv_aux,MomInv(1),MomInv(2),MomInv(3), &
            masses2(0),masses2(1),masses2(2),rmax,Cerraux,Cerr2=Cerr2aux)
      end if
    end if

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          n2=r-2*n0-n1

          cnt=cnt+1
          C(cnt) = C_aux(n0,n1,n2)
          Cuv(cnt) = Cuv_aux(n0,n1,n2)

        end do
      end do
    end do

  end subroutine C_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D_main_cll(D,Duv,p10,p21,p32,p30,p20,p31,  &
  !                              m02,m12,m22,m32,rmax,Derr,id_in,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D_main_cll(D,Duv,p10,p21,p32,p30,p20,p31,  &
                              m02,m12,m22,m32,rmax,Derr,id_in,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double precision :: q10,q21,q32,q30,q20,q31
    double complex :: mm02,mm12,mm22,mm32
    double complex, intent(out) :: D(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    integer, optional, intent(in) :: id_in
    double complex :: D2uv(0:rmax/2,0:rmax,0:rmax,0:rmax) 
    double complex :: D2(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex :: Dcoliuv(0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: Dcoli(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Ddduv(0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: Ddd(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Derraux(0:rmax),Derr2aux(0:rmax),Ddiff(0:rmax)
    double complex :: elimcminf2
    double complex :: args(10)
    integer :: n0,rank,errflag,id
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD),Dacc(0:rmax),norm,norm_coli,norm_dd,Dacc2(0:rmax)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag
    integer :: r,n1,n2,n3

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = p21
      args(3) = p32
      args(4) = p30
      args(5) = p20
      args(6) = p31
      args(7) = m02
      args(8) = m12
      args(9) = m22
      args(10) = m32
      call SetMasterFname_cll('D_cll')
      call SetMasterN_cll(4)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(10,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH
    
        call CalcD(Dcoli,Dcoliuv,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,id,Derraux,Derr2aux)

        norm = abs(Dcoli(0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              n3=r-n1-n2
              norm =  max(norm,abs(Dcoli(0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p30),abs(p20),abs(p31), &
                    abs(m02),abs(m12),abs(m22),abs(m32))
          if(norm.ne.0d0) then
            norm=1d0/norm**2
          else
            norm=1d0/muir2_cll**2
          end if 
        end if
        if (norm.ne.0d0) then
          Dacc  = Derraux/norm
          Dacc2 = Derr2aux/norm
        else
          Dacc = 0d0
          Dacc2 = 0d0
        end if

        if (present(Derr)) Derr = Derraux
        if (present(Derr2)) Derr2 = Derr2aux

        if (mflag) call PropagateAccFlag_cll(Dacc,rmax)           

        D(0:rmax/2,0:rmax,0:rmax,0:rmax) = Dcoli(0:rmax/2,0:rmax,0:rmax,0:rmax)
        Duv(0:rmax/2,0:rmax,0:rmax,0:rmax) = Dcoliuv(0:rmax/2,0:rmax,0:rmax,0:rmax)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q30 = dreal(getminf2DD_cll(p30))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)

        rank = rmax
!        write(*,*) rank
!        write(*,*) q10,q21
!        write(*,*) q32,q30
!        write(*,*) q20,q31
!        write(*,*) mm02,mm12
!        write(*,*) mm22,mm32
        call D_dd(Ddd,Ddduv,q10,q21,q32,q30,q20,q31,  &
                  mm02,mm12,mm22,mm32,rank,id)   
        D(0:rank/2,0:rank,0:rank,0:rank) = Ddd(0:rank/2,0:rank,0:rank,0:rank)
        Duv(0:rank/2,0:rank,0:rank,0:rank) = Ddduv(0:rank/2,0:rank,0:rank,0:rank)

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        if (present(Derr)) Derr(0:rmax) = accabsDD(0:rmax)
        if (present(Derr2)) Derr2(0:rmax) = accabs2DD(0:rmax)

        norm = abs(D(0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              n3=r-n1-n2
              norm =  max(norm,abs(D(0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p30),abs(p20),abs(p31), &
                    abs(m02),abs(m12),abs(m22),abs(m32))
          if(norm.ne.0d0) then
            norm=1d0/norm**2
          else
            norm=1d0/muir2_cll**2
          end if 
        end if
        if (norm.ne.0d0) then
          Dacc  = accabsDD(0:rmax)/norm
          Dacc2 = accabs2DD(0:rmax)/norm
        else
          Dacc = 0d0
          Dacc2 = 0d0
        end if
        if (mflag) call PropagateAccFlag_cll(Dacc,rmax)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcD(Dcoli,Dcoliuv,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,id,Derraux,Derr2aux)          

        D(0:rmax/2,0:rmax,0:rmax,0:rmax) = Dcoli(0:rmax/2,0:rmax,0:rmax,0:rmax)
        Duv(0:rmax/2,0:rmax,0:rmax,0:rmax) = Dcoliuv(0:rmax/2,0:rmax,0:rmax,0:rmax)

        
        ! calculate loop integral using DD

        id=0

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q30 = dreal(getminf2DD_cll(p30))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)

        rank = rmax
        call D_dd(Ddd,Ddduv,q10,q21,q32,q30,q20,q31,  &
                  mm02,mm12,mm22,mm32,rank,id)   
        do n0=0,rank/2
          D2(n0,0:rank,0:rank,0:rank) = Ddd(n0,0:rank,0:rank,0:rank)
          D2uv(n0,0:rank,0:rank,0:rank) = Ddduv(n0,0:rank,0:rank,0:rank)
        end do
        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        
        norm_coli = abs(D(0,0,0,0))
        norm_dd = abs(D2(0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              n3=r-n1-n2
              norm_coli =  max(norm_coli,abs(D(0,n1,n2,n3)))
              norm_dd =  max(norm_dd,abs(D2(0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(p10),abs(p21),abs(p32),abs(p30),abs(p20),abs(p31), &
                    abs(m02),abs(m12),abs(m22),abs(m32))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**2
          else
            norm_coli=1d0/muir2_cll**2
          end if 
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(p10),abs(p21),abs(p32),abs(p30),abs(p20),abs(p31), &
                    abs(m02),abs(m12),abs(m22),abs(m32))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**2
          else
            norm_dd=1d0/muir2_cll**2
          end if 
        end if
        norm = min(norm_coli,norm_dd)

        ! cross-check
        call CheckCoefsD_cll(D,D2,p10,p21,p32,p30,p20,p31,  &
                   m02,m12,m22,m32,rmax,norm,Ddiff)
        
        
        if (Derraux(rmax).lt.accabsDD(rmax)) then
          if (present(Derr))  Derr = max(Derraux,Ddiff)
          if (present(Derr2)) Derr2 = Derr2aux
          if (norm.ne.0d0) then
            Dacc = max(Derraux/norm_coli,Ddiff/norm)
            Dacc2 = Derr2aux/norm_coli
          else
            Dacc = Ddiff
            Dacc2 = 0d0
          end if         
          if (Monitoring) PointsCntD_coli =  PointsCntD_coli + 1
        else
          D = D2
          Duv = D2uv
          if (present(Derr))  Derr = max(accabsDD(0:rmax),Ddiff)
          if (present(Derr2)) Derr2 = accabs2DD(0:rmax)
          if (norm.ne.0d0) then
            Dacc = max(accabsDD(0:rmax)/norm_dd,Ddiff/norm)
            Dacc2 = accabs2DD(0:rmax)/norm_dd
          else
            Dacc = Ddiff
            Dacc2 = 0d0
          end if
          if (Monitoring) PointsCntD_dd =  PointsCntD_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Dacc,rmax)

    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntD_cll =  PointsCntD_cll + 1

      if(maxval(Dacc).gt.reqacc_cll) AccPointsCntD_cll =  AccPointsCntD_cll + 1
      if(maxval(Dacc).gt.sreqacc_cll) sAccPointsCntD_cll =  sAccPointsCntD_cll + 1

      if(maxval(Dacc).gt.critacc_cll) then
        CritPointsCntD_cll =  CritPointsCntD_cll + 1
        if ( CritPointsCntD_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut_cll('D_cll',0,maxval(Dacc), CritPointsCntD_cll)
          if( CritPointsCntD_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout_cll,*) ' Further output of Critical Points for D_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if


#ifdef CritPoints2
      if(maxval(Dacc2).gt.reqacc_cll) AccPointsCntD2_cll =  AccPointsCntD2_cll + 1
      if(maxval(Dacc2).gt.sreqacc_cll) sAccPointsCntD2_cll =  sAccPointsCntD2_cll + 1

      if(maxval(Dacc2).gt.critacc_cll) then
        CritPointsCntD2_cll =  CritPointsCntD2_cll + 1
        if ( CritPointsCntD2_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut2_cll('D_cll',0,maxval(Dacc2), CritPointsCntD2_cll)
          if( CritPointsCntD2_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for D_cll suppressed '   
            write(ncpout2_cll,*)
          endif
        end if
      end if
#endif

    end if


  end subroutine D_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D_arrays_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D_arrays_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(6), masses2(0:3)
    double complex, intent(out) :: D(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double precision :: Derraux(0:rmax),Derr2aux(0:rmax)
    
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    if (present(Derr)) then
      if (present(Derr2)) then
        call D_main_cll(D,Duv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr,Derr2=Derr2)
      else
        call D_main_cll(D,Duv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr)
      end if
    else
      if (present(Derr2)) then
        call D_main_cll(D,Duv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derraux,Derr2=Derr2)
      else
        call D_main_cll(D,Duv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derraux)
      end if
    end if

  end subroutine D_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D_list_cll(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D_list_cll(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,Derr,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(:),Duv(:)
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call D_list_checked_cll(D,Duv,p10,p21,p32,p30,p20,p31,  &
                              m02,m12,m22,m32,rmax,Derr,Derr2)

  end subroutine D_list_cll


  subroutine D_list_checked_cll(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,Derr,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(NCoefs(rmax,4)),Duv(NCoefs(rmax,4))
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: D_aux(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex :: Duv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: Derraux(0:rmax),Derr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,cnt
    
    if (present(Derr)) then
      if (present(Derr2)) then
        call D_main_cll(D_aux,Duv_aux,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,Derr,Derr2=Derr2)
      else
        call D_main_cll(D_aux,Duv_aux,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,Derr)
      end if
    else
      if (present(Derr2)) then
        call D_main_cll(D_aux,Duv_aux,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,Derraux,Derr2=Derr2)
      else
        call D_main_cll(D_aux,Duv_aux,p10,p21,p32,p30,p20,p31,  &
            m02,m12,m22,m32,rmax,Derraux)
      end if
    end if

    cnt=0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            n3=r-2*n0-n1-n2

            cnt=cnt+1
            D(cnt) = D_aux(n0,n1,n2,n3)
            Duv(cnt) = Duv_aux(n0,n1,n2,n3) 

          end do
        end do
      end do
    end do

  end subroutine D_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D_arrays_list_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D_arrays_list_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(6), masses2(0:3)
    double complex, intent(out) :: D(:),Duv(:)
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('D_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call D_arrays_list_checked_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)

  end subroutine D_arrays_list_cll


  subroutine D_arrays_list_checked_cll(D,Duv,MomInv,masses2,rmax,Derr,Derr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(6), masses2(0:3)
    double complex, intent(out) :: D(NCoefs(rmax,4)),Duv(NCoefs(rmax,4))
    double precision, optional, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: D_aux(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex :: Duv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: Derraux(0:rmax),Derr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,cnt
    
    if (present(Derr)) then
      if (present(Derr2)) then
        call D_main_cll(D_aux,Duv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr,Derr2=Derr2)
      else
        call D_main_cll(D_aux,Duv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr)
      end if
    else
      if (present(Derr2)) then
        call D_main_cll(D_aux,Duv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derraux,Derr2=Derr2)
      else
        call D_main_cll(D_aux,Duv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derraux)
      end if
    end if

    cnt=0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            n3=r-2*n0-n1-n2

            cnt=cnt+1
            D(cnt) = D_aux(n0,n1,n2,n3)
            Duv(cnt) = Duv_aux(n0,n1,n2,n3) 

          end do
        end do
      end do
    end do

  end subroutine D_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E_main_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
  !                       m02,m12,m22,m32,m42,rmax,Eerr,id_in,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine E_main_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                       m02,m12,m22,m32,m42,rmax,Eerr,id_in,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    double precision :: q10,q21,q32,q43,q40,q20,q31,q42,q30,q41
    double complex :: mm02,mm12,mm22,mm32,mm42
    double precision :: Eerraux(0:rmax),Eerr2aux(0:rmax),Ediff(0:rmax)
    integer, optional, intent(in) :: id_in   
    double complex :: E2uv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: E2(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: Edd(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: elimcminf2
    double complex :: args(15)
    integer :: n0,rank,errflag,id
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD),Eacc(0:rmax),norm,norm_coli,norm_dd,Eacc2(0:rmax)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag
    integer :: r,n1,n2,n3,n4

    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = p21
      args(3) = p32
      args(4) = p43
      args(5) = p40
      args(6) = p20
      args(7) = p31
      args(8) = p42
      args(9) = p30
      args(10) = p41
      args(11) = m02
      args(12) = m12
      args(13) = m22
      args(14) = m32
      args(15) = m42
      call SetMasterFname_cll('E_cll')
      call SetMasterN_cll(5)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(15,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
            m02,m12,m22,m32,m42,rmax,id,Eerraux,Eerr2aux)

        norm = abs(E(0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                n4=r-n1-n2-n3
                norm =  max(norm,abs(E(0,n1,n2,n3,n4)))
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p40), &
                      abs(p20),abs(p31),abs(p42),abs(p30),abs(p41), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42))
          if(norm.ne.0d0) then
            norm=1d0/norm**3
          else
            norm=1d0/muir2_cll**3
          end if 
        end if
        if (norm.ne.0d0) then
          Eacc = Eerraux/norm       
          Eacc2 = Eerr2aux/norm
        else
          Eacc = 0d0
          Eacc2 = 0d0
        end if

        if (present(Eerr)) Eerr = Eerraux
        if (present(Eerr2)) Eerr2 = Eerr2aux

        if (mflag) call PropagateAccFlag_cll(Eacc,rmax)            


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        if (rmax.gt.5) then
          call SetErrFlag_cll(-10)
          call ErrOut_cll('E_cll','rank higher than maximum rank implemented in DD library',eflag)
          if(eflag) then
            write(nerrout_cll,*) 'E_cll: 5-point function of rank>5 not implemented in DD library'
          end if
        end if


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q40 = dreal(getminf2DD_cll(p40))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)

        rank = rmax
        call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                  mm02,mm12,mm22,mm32,mm42,rank,id)  
        E(0:rank/2,0:rank,0:rank,0:rank,0:rank) = Edd(0:rank/2,0:rank,0:rank,0:rank,0:rank)
        Euv = 0d0

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        if (present(Eerr)) Eerr(0:rmax) = accabsDD(0:rmax)
        if (present(Eerr2)) Eerr2(0:rmax) = accabs2DD(0:rmax)
        
        norm = abs(E(0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                n4=r-n1-n2-n3
                norm =  max(norm,abs(E(0,n1,n2,n3,n4)))
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p40), &
                      abs(p20),abs(p31),abs(p42),abs(p30),abs(p41), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42))
          if(norm.ne.0d0) then
            norm=1d0/norm**3
          else
            norm=1d0/muir2_cll**3
          end if 
        end if
        if (norm.ne.0d0) then
          Eacc = accabsDD(0:rmax)/norm       
          Eacc2 = accabs2DD(0:rmax)/norm
        else
          Eacc = 0d0
          Eacc2 = 0d0
        end if
        if (mflag) call PropagateAccFlag_cll(Eacc,rmax)

        
      case (3)
        ! cross-check mode
        ! compare results for loop integral


        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral
        call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
            m02,m12,m22,m32,m42,rmax,id,Eerraux,Eerr2aux)           


        ! calculate loop integral
        if (rmax.gt.5) then
          call SetErrFlag_cll(-10)
          call ErrOut_cll('E_cll','rank higher than maximum rank implemented in DD library',eflag)
          if(eflag) then
            write(nerrout_cll,*) 'E_cll: 5-point function of rank>5 not implemented in DD library'
          end if
        end if


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q40 = dreal(getminf2DD_cll(p40))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)

        rank = rmax
        call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                  mm02,mm12,mm22,mm32,mm42,rank,id)  
        E2(0:rank/2,0:rank,0:rank,0:rank,0:rank) = Edd(0:rank/2,0:rank,0:rank,0:rank,0:rank)
        E2uv = 0d0

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)

        norm_coli = abs(E(0,0,0,0,0))
        norm_dd = abs(E2(0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                n4=r-n1-n2-n3
                norm_coli =  max(norm_coli,abs(E(0,n1,n2,n3,n4)))
                norm_dd =  max(norm_dd,abs(E2(0,n1,n2,n3,n4)))
              end do
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p40), &
                      abs(p20),abs(p31),abs(p42),abs(p30),abs(p41), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**3
          else
            norm_coli=1d0/muir2_cll**3
          end if 
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p40), &
                      abs(p20),abs(p31),abs(p42),abs(p30),abs(p41), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**3
          else
            norm_dd=1d0/muir2_cll**3
          end if 
        end if
        norm=min(norm_coli,norm_dd)

        ! cross-check
        call CheckCoefsE_cll(E,E2,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                   m02,m12,m22,m32,m42,rmax,norm,Ediff)
                   

        if (Eerraux(rmax).lt.accabsDD(rmax)) then
          if (present(Eerr))  Eerr = max(Eerraux,Ediff)
          if (present(Eerr2)) Eerr2 = Eerr2aux
          if (norm.ne.0d0) then
            Eacc = max(Eerraux/norm_coli,Ediff/norm)
            Eacc2 = Eerr2aux/norm_coli
          else
            Eacc = Ediff
            Eacc2 = 0d0
          end if
          if (Monitoring) PointsCntE_coli =  PointsCntE_coli + 1
        else
          E = E2
          Euv = E2uv
          if (present(Eerr))  Eerr = max(accabsDD(0:rmax),Ediff)
          if (present(Eerr2)) Eerr2 = accabs2DD(0:rmax)
          if (norm.ne.0d0) then
            Eacc = max(accabsDD(0:rmax)/norm_dd,Ediff/norm)
            Eacc2 = accabs2DD(0:rmax)/norm_dd
          else
            Eacc = Ediff
            Eacc2 = 0d0
          end if
          if (Monitoring) PointsCntE_dd =  PointsCntE_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Eacc,rmax) 

    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntE_cll =  PointsCntE_cll + 1

      if(maxval(Eacc).gt.reqacc_cll) AccPointsCntE_cll =  AccPointsCntE_cll + 1
      if(maxval(Eacc).gt.sreqacc_cll) sAccPointsCntE_cll =  sAccPointsCntE_cll + 1

      if(maxval(Eacc).gt.critacc_cll) then
        CritPointsCntE_cll =  CritPointsCntE_cll + 1
        if ( CritPointsCntE_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut_cll('E_cll',0,maxval(Eacc), CritPointsCntE_cll)
          if( CritPointsCntE_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout_cll,*) ' Further output of Critical Points for E_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if

#ifdef CritPoints2
      if(maxval(Eacc2).gt.reqacc_cll) AccPointsCntE2_cll =  AccPointsCntE2_cll + 1
      if(maxval(Eacc2).gt.sreqacc_cll) sAccPointsCntE2_cll =  sAccPointsCntE2_cll + 1

      if(maxval(Eacc2).gt.critacc_cll) then
        CritPointsCntE2_cll =  CritPointsCntE2_cll + 1
        if ( CritPointsCntE2_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut2_cll('E_cll',0,maxval(Eacc2), CritPointsCntE2_cll)
          if( CritPointsCntE2_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for E_cll suppressed '   
            write(ncpout2_cll,*)
          endif
        end if
      end if
#endif

    end if


  end subroutine E_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E_arrays_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine E_arrays_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(10), masses2(0:4)
    double complex, intent(out) :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    double precision :: Eerraux(0:rmax),Eerr2aux(0:rmax)
    
    if (present(Eerr)) then
      if (present(Eerr2)) then
        call E_main_cll(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr,Eerr2=Eerr2)
      else
        call E_main_cll(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr)
      end if
    else
      if (present(Eerr2)) then
        call E_main_cll(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerraux,Eerr2=Eerr2)
      else
        call E_main_cll(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerraux)
      end if
    end if

  end subroutine E_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E_list_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
  !                                       m02,m12,m22,m32,m42,rmax,Eerr,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine E_list_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
                               m02,m12,m22,m32,m42,rmax,Eerr,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: E(:),Euv(:)
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    logical :: eflag
    
    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if
    
    call E_list_checked_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
                               m02,m12,m22,m32,m42,rmax,Eerr,Eerr2)
  
  end subroutine E_list_cll

  
  subroutine E_list_checked_cll(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
                               m02,m12,m22,m32,m42,rmax,Eerr,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: E(NCoefs(rmax,5)),Euv(NCoefs(rmax,5))
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    double complex :: E_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Euv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Eerraux(0:rmax),Eerr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,cnt

    if (present(Eerr)) then
      if (present(Eerr2)) then
        call E_main_cll(E_aux,Euv_aux,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
            m02,m12,m22,m32,m42,rmax,Eerr,Eerr2=Eerr2)
      else
        call E_main_cll(E_aux,Euv_aux,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
            m02,m12,m22,m32,m42,rmax,Eerr)
      end if
    else
      if (present(Eerr2)) then
        call E_main_cll(E_aux,Euv_aux,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
            m02,m12,m22,m32,m42,rmax,Eerraux,Eerr2=Eerr2)
      else
        call E_main_cll(E_aux,Euv_aux,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,  &
            m02,m12,m22,m32,m42,rmax,Eerraux)
      end if
    end if

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              n4=r-2*n0-n1-n2-n3

              cnt=cnt+1
              E(cnt) = E_aux(n0,n1,n2,n3,n4)
              Euv(cnt) = Euv_aux(n0,n1,n2,n3,n4)

            end do
          end do
        end do
      end do
    end do

  end subroutine E_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E_arrays_list_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine E_arrays_list_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(10), masses2(0:4)
    double complex, intent(out) :: E(:),Euv(:)
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    logical :: eflag
    
    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('E_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call E_arrays_list_checked_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  
  end subroutine E_arrays_list_cll

  
  subroutine E_arrays_list_checked_cll(E,Euv,MomInv,masses2,rmax,Eerr,Eerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(10), masses2(0:4)
    double complex, intent(out) :: E(NCoefs(rmax,5)),Euv(NCoefs(rmax,5))
    double precision, optional, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    double complex :: E_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Euv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Eerraux(0:rmax),Eerr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,cnt
    
    if (present(Eerr)) then
      if (present(Eerr2)) then
        call E_main_cll(E_aux,Euv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr,Eerr2=Eerr2)
      else
        call E_main_cll(E_aux,Euv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr)
      end if
    else
      if (present(Eerr2)) then
        call E_main_cll(E_aux,Euv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerraux,Eerr2=Eerr2)
      else
        call E_main_cll(E_aux,Euv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerraux)
      end if
    end if

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              n4=r-2*n0-n1-n2-n3

              cnt=cnt+1
              E(cnt) = E_aux(n0,n1,n2,n3,n4)
              Euv(cnt) = Euv_aux(n0,n1,n2,n3,n4)

            end do
          end do
        end do
      end do
    end do

  end subroutine E_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F_main_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,id_in,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F_main_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                             p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,id_in,Ferr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Fuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    double precision :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double precision :: q51,q30,q41,q52
    double complex :: mm02,mm12,mm22,mm32,mm42,mm52
    integer, optional, intent(in) :: id_in
    double complex :: F2uv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: F2(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: Fdd(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Ferraux(0:rmax),Ferr2aux(0:rmax),Fdiff(0:rmax)
    double complex :: elimcminf2
    double complex :: args(21)
    integer :: n0,rank,errflag,id
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD),Facc(0:rmax),norm,norm_coli,norm_dd,Facc2(0:rmax)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    integer :: accflagDD,errflagDD,NDD,rankDD
    logical :: mflag,eflag
    integer :: r,n1,n2,n3,n4,n5
   
    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = p21
      args(3) = p32
      args(4) = p43
      args(5) = p54
      args(6) = p50
      args(7) = p20
      args(8) = p31
      args(9) = p42
      args(10) = p53
      args(11) = p40
      args(12) = p51
      args(13) = p30
      args(14) = p41
      args(15) = p52
      args(16) = m02
      args(17) = m12
      args(18) = m22
      args(19) = m32
      args(20) = m42
      args(21) = m52
      call SetMasterFname_cll('F_cll')
      call SetMasterN_cll(6)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(21,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,id,Ferraux,Ferr2aux)

        norm = abs(F(0,0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                do n4=0,r-n1-n2-n3
                  n5=r-n1-n2-n3-n4
                  norm =  max(norm,abs(F(0,n1,n2,n3,n4,n5)))
                end do
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                      abs(p50),abs(p20),abs(p31),abs(p42),abs(p53), &
                      abs(p40),abs(p51),abs(p30),abs(p41),abs(p52), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42),abs(m52))
          if(norm.ne.0d0) then
            norm=1d0/norm**4
          else
            norm=1d0/muir2_cll**4
          end if 
        end if
        if (norm.ne.0d0) then
          Facc = Ferraux/norm       
          Facc2 = Ferr2aux/norm
        else
          Facc = 0d0
          Facc2 = 0d0
        end if

        if (present(Ferr)) Ferr = Ferraux
        if (present(Ferr2)) Ferr2 = Ferr2aux

        if (mflag) call PropagateAccFlag_cll(Facc,rmax)                 


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0
        if (rmax.gt.6) then
          call SetErrFlag_cll(-10)
          call ErrOut_cll('F_cll','rank higher than maximum rank implemented in DD library',eflag)
          if(eflag) then
            write(nerrout_cll,*) 'F_cll: 6-point function of rank>6 not implemented in DD library'
          end if
        end if


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q54 = dreal(getminf2DD_cll(p54))
        q50 = dreal(getminf2DD_cll(p50))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q53 = dreal(getminf2DD_cll(p53))
        q40 = dreal(getminf2DD_cll(p40))
        q51 = dreal(getminf2DD_cll(p51))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        q52 = dreal(getminf2DD_cll(p52))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)
        mm52 = getminf2DD_cll(m52)

        rank = rmax
        call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                  q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,id)  
        F(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank) = Fdd(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank)
        Fuv = 0d0

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)         
        if (present(Ferr)) Ferr(0:rmax) = accabsDD(0:rmax)
        if (present(Ferr2)) Ferr2(0:rmax) = accabs2DD(0:rmax)

        norm = abs(F(0,0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                do n4=0,r-n1-n2-n3
                  n5=r-n1-n2-n3-n4
                  norm =  max(norm,abs(F(0,n1,n2,n3,n4,n5)))
                end do
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                     abs(p50),abs(p20),abs(p31),abs(p42),abs(p53), &
                     abs(p40),abs(p51),abs(p30),abs(p41),abs(p52), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42),abs(m52))
          if(norm.ne.0d0) then
            norm=1d0/norm**4
          else
            norm=1d0/muir2_cll**4
          end if 
        end if
        if (norm.ne.0d0) then
          Facc = accabsDD(0:rmax)/norm       
          Facc2 = accabs2DD(0:rmax)/norm
        else
          Facc = 0d0
          Facc2 = 0d0
        end if
        
        if (mflag) call PropagateAccFlag_cll(Facc,rmax) 

        
      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral
        call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,id,Ferraux,Ferr2aux)
                

        if (rmax.gt.6) then
          call SetErrFlag_cll(-10)
          call ErrOut_cll('F_cll','rank higher than maximum rank implemented in DD library',eflag)
          if(eflag) then
            write(nerrout_cll,*) 'F_cll: 6-point function of rank>6 not implemented in DD library'
          end if
        end if


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q54 = dreal(getminf2DD_cll(p54))
        q50 = dreal(getminf2DD_cll(p50))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q53 = dreal(getminf2DD_cll(p53))
        q40 = dreal(getminf2DD_cll(p40))
        q51 = dreal(getminf2DD_cll(p51))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        q52 = dreal(getminf2DD_cll(p52))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)
        mm52 = getminf2DD_cll(m52)

        id=0
        rank = rmax
        call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                  q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,id)  
        F2(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank) = Fdd(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank)
        F2uv = 0d0

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)

        norm_coli = abs(F(0,0,0,0,0,0))
        norm_dd = abs(F2(0,0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                do n4=0,r-n1-n2-n3
                  n5=r-n1-n2-n3-n4
                  norm_coli =  max(norm_coli,abs(F(0,n1,n2,n3,n4,n5)))
                  norm_dd =  max(norm_dd,abs(F2(0,n1,n2,n3,n4,n5)))
                end do
              end do
            end do
          end do
        end do 
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                      abs(p50),abs(p20),abs(p31),abs(p42),abs(p53), &
                      abs(p40),abs(p51),abs(p30),abs(p41),abs(p52), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42),abs(m52))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**4
          else
            norm_coli=1d0/muir2_cll**4
          end if 
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                      abs(p50),abs(p20),abs(p31),abs(p42),abs(p53), &
                      abs(p40),abs(p51),abs(p30),abs(p41),abs(p52), &
                    abs(m02),abs(m12),abs(m22),abs(m32),abs(m42),abs(m52))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**4
          else
            norm_dd=1d0/muir2_cll**4
          end if 
        end if
        norm = min(norm_coli,norm_dd)

        ! cross-check
        call CheckCoefsF_cll(F,F2,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,norm,Fdiff)
                   
                           
        if (Ferraux(rmax).lt.accabsDD(rmax)) then
          if (present(Ferr))  Ferr = max(Ferraux,Fdiff)
          if (present(Ferr2)) Ferr2 = Ferr2aux
          if (norm.ne.0d0) then
            Facc = max(Ferraux/norm_coli,Fdiff/norm)
            Facc2 = Ferr2aux/norm_coli
          else
            Facc = Fdiff
            Facc2 = 0d0
          end if 
          if (Monitoring) PointsCntF_coli =  PointsCntF_coli + 1
        else
          F = F2
          Fuv = F2uv
          if (present(Ferr))  Ferr = max(accabsDD(0:rmax),Fdiff)
          if (present(Ferr2)) Ferr2 = accabs2DD(0:rmax)
          if (norm.ne.0d0) then
            Facc = max(accabsDD(0:rmax)/norm_dd,Fdiff/norm)
            Facc2 = accabs2DD(0:rmax)/norm_dd
          else
            Facc = Fdiff
            Facc2 = 0d0
          end if 
          if (Monitoring) PointsCntF_dd =  PointsCntF_dd + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Facc,rmax)
        
    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntF_cll =  PointsCntF_cll + 1

      if(maxval(Facc).gt.reqacc_cll) AccPointsCntF_cll =  AccPointsCntF_cll + 1
      if(maxval(Facc).gt.sreqacc_cll) sAccPointsCntF_cll =  sAccPointsCntF_cll + 1

      if(maxval(Facc).gt.critacc_cll) then
        CritPointsCntF_cll =  CritPointsCntF_cll + 1
        if ( CritPointsCntF_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut_cll('F_cll',0,maxval(Facc), CritPointsCntF_cll)
          if( CritPointsCntF_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout_cll,*) ' Further output of Critical Points for F_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if

#ifdef CritPoints2
      if(maxval(Facc2).gt.reqacc_cll) AccPointsCntF2_cll =  AccPointsCntF2_cll + 1
      if(maxval(Facc2).gt.sreqacc_cll) sAccPointsCntF2_cll =  sAccPointsCntF2_cll + 1

      if(maxval(Facc2).gt.critacc_cll) then
        CritPointsCntF2_cll =  CritPointsCntF2_cll + 1
        if ( CritPointsCntF2_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut2_cll('F_cll',0,maxval(Facc2), CritPointsCntF2_cll)
          if( CritPointsCntF2_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for F_cll suppressed '   
            write(ncpout2_cll,*)
          endif
        end if
      end if
#endif

    end if

  end subroutine F_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F_arrays_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F_arrays_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(15), masses2(0:5)
    double complex, intent(out) :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Fuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    double precision :: Ferraux(0:rmax),Ferr2aux(0:rmax)
    
    if (present(Ferr)) then
      if (present(Ferr2)) then
        call F_main_cll(F,Fuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr,Ferr2=Ferr2)
      else
        call F_main_cll(F,Fuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr)
      end if
    else
      if (present(Ferr2)) then
        call F_main_cll(F,Fuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferraux,Ferr2=Ferr2)
      else
        call F_main_cll(F,Fuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferraux)
      end if
    end if
      
  end subroutine F_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F_list_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                               p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F_list_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40, &
      p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,Ferr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: F(:),Fuv(:)
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    logical :: eflag
    
    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call F_list_checked_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40, &
      p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,Ferr2)
      
  end subroutine F_list_cll
      

  subroutine F_list_checked_cll(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40, &
      p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,Ferr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: F(NCoefs(rmax,6)),Fuv(NCoefs(rmax,6))
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    double complex :: F_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Fuv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Ferraux(0:rmax),Ferr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,cnt
    
    if (present(Ferr)) then
      if (present(Ferr2)) then
        call F_main_cll(F_aux,Fuv_aux,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr,Ferr2=Ferr2)
      else
        call F_main_cll(F_aux,Fuv_aux,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferr)
      end if
    else
      if (present(Ferr2)) then
        call F_main_cll(F_aux,Fuv_aux,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferraux,Ferr2=Ferr2)
      else
        call F_main_cll(F_aux,Fuv_aux,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,Ferraux)
      end if
    end if
  
    cnt=0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              do n4=r-2*n0-n1-n2-n3,0,-1
                n5=r-2*n0-n1-n2-n3-n4

                cnt = cnt+1
                F(cnt) = F_aux(n0,n1,n2,n3,n4,n5)
                Fuv(cnt) = Fuv_aux(n0,n1,n2,n3,n4,n5)

              end do
            end do
          end do
        end do
      end do
    end do

  end subroutine F_list_checked_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F_arrays_list_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F_arrays_list_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(15), masses2(0:5)
    double complex, intent(out) :: F(:),Fuv(:)
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    logical :: eflag

    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('F_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call F_arrays_list_checked_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
      
  end subroutine F_arrays_list_cll
      

  subroutine F_arrays_list_checked_cll(F,Fuv,MomInv,masses2,rmax,Ferr,Ferr2)
      
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(15), masses2(0:5)
    double complex, intent(out) :: F(NCoefs(rmax,6)),Fuv(NCoefs(rmax,6))
    double precision, optional, intent(out) ::Ferr(0:rmax),Ferr2(0:rmax)
    double complex :: F_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Fuv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Ferraux(0:rmax),Ferr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,cnt
    
    if (present(Ferr)) then
      if (present(Ferr2)) then
        call F_main_cll(F_aux,Fuv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr,Ferr2=Ferr2)
      else
        call F_main_cll(F_aux,Fuv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr)
      end if
    else
      if (present(Ferr2)) then
        call F_main_cll(F_aux,Fuv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferraux,Ferr2=Ferr2)
      else
        call F_main_cll(F_aux,Fuv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferraux)
      end if
    end if
  
    cnt=0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              do n4=r-2*n0-n1-n2-n3,0,-1
                n5=r-2*n0-n1-n2-n3-n4

                cnt = cnt+1
                F(cnt) = F_aux(n0,n1,n2,n3,n4,n5)
                Fuv(cnt) = Fuv_aux(n0,n1,n2,n3,n4,n5)

              end do
            end do
          end do
        end do
      end do
    end do

  end subroutine F_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine G_main_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
  !                              p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
  !                              m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,id_in,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine G_main_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                              p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                              m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,id_in,Gerr2)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: G(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Guv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double precision :: Gerraux(0:rmax),Gerr2aux(0:rmax)
    double precision :: Gacc(0:rmax), Gacc2(0:rmax),norm,norm_coli,norm_dd
    integer, optional, intent(in) :: id_in
    double complex :: args(28)
    double complex :: elimcminf2
    integer :: errflag,id
    logical :: mflag,eflag
    integer :: r,n1,n2,n3,n4,n5,n6

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      ! set ID of master call
      args(1) = p10
      args(2) = p21
      args(3) = p32
      args(4) = p43
      args(5) = p54
      args(6) = p65
      args(7) = p60
      args(8) = p20
      args(9) = p31
      args(10) = p42
      args(11) = p53
      args(12) = p64
      args(13) = p50
      args(14) = p61
      args(15) = p30
      args(16) = p41
      args(17) = p52
      args(18) = p63
      args(19) = p40
      args(20) = p51
      args(21) = p62
      args(22) = m02
      args(23) = m12
      args(24) = m22
      args(25) = m32
      args(26) = m42
      args(27) = m52
      args(28) = m62
      call SetMasterFname_cll('G_cll')
      call SetMasterN_cll(7)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(28,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcG(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerraux,Gerr2aux)

        norm = abs(G(0,0,0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                do n4=0,r-n1-n2-n3
                  do n5=0,r-n1-n2-n3-n4
                    n6=r-n1-n2-n3-n4-n5
                    norm = max(norm,abs(G(0,n1,n2,n3,n4,n5,n6)))
                  end do
                end do
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                      abs(p65),abs(p60),abs(p20),abs(p31),abs(p42), &
                      abs(p53),abs(p64),abs(p50),abs(p61),abs(p30), &
                      abs(p41),abs(p52),abs(p63),abs(p40),abs(p51), &
                      abs(p62),abs(m02),abs(m12),abs(m22),abs(m32), &
                      abs(m42),abs(m52),abs(m62))
          if(norm.ne.0d0) then
            norm=1d0/norm**5
          else
            norm=1d0/muir2_cll**4
          end if 
        end if
        Gacc = Gerraux/norm       
        Gacc2 = Gerr2aux/norm       

        if (present(Gerr)) Gerr = Gerraux
        if (present(Gerr2)) Gerr2 = Gerr2aux

        if (mflag) call PropagateAccFlag_cll(Gacc,rmax)                


      case (2)
        call SetErrFlag_cll(-10)
        call ErrOut_cll('G_cll','7-point functions not implemented in DD library',eflag)
        if(eflag) then
          write(nerrout_cll,*) 'G_cll: 7-point functions not implemented in DD library'
          write(nerrout_cll,*) 'G_cll: --> use COLI implementation (mode_cll=1)'
        end if


      case (3)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcG(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerraux,Gerr2aux)

        norm = abs(G(0,0,0,0,0,0,0))
        do r=1,rmax
          do n1=0,r
            do n2=0,r-n1
              do n3=0,r-n1-n2
                do n4=0,r-n1-n2-n3
                  do n5=0,r-n1-n2-n3-n4
                    n6=r-n1-n2-n3-n4-n5
                    norm = max(norm,abs(G(0,n1,n2,n3,n4,n5,n6)))
                  end do
                end do
              end do
            end do
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(abs(p10),abs(p21),abs(p32),abs(p43),abs(p54), &
                      abs(p65),abs(p60),abs(p20),abs(p31),abs(p42), &
                      abs(p53),abs(p64),abs(p50),abs(p61),abs(p30), &
                      abs(p41),abs(p52),abs(p63),abs(p40),abs(p51), &
                      abs(p62),abs(m02),abs(m12),abs(m22),abs(m32), &
                      abs(m42),abs(m52),abs(m62))
          if(norm.ne.0d0) then
            norm=1d0/norm**5
          else
            norm=1d0/muir2_cll**4
          end if 
        end if
        Gacc = Gerraux/norm       
        Gacc2 = Gerr2aux/norm       

        if (present(Gerr)) Gerr = Gerraux
        if (present(Gerr2)) Gerr2 = Gerr2aux

        if (mflag) call PropagateAccFlag_cll(Gacc,rmax)                

        call SetErrFlag_cll(-10)
        call ErrOut_cll('G_cll','7-point functions not implemented in DD library',eflag)
        if(eflag) then
          write(nerrout_cll,*) 'G_cll: 7-point functions not implemented in DD library'
          write(nerrout_cll,*) 'G_cll: --> use COLI implementation (mode_cll=1)'
        end if


    end select

    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntG_cll =  PointsCntG_cll + 1

      if(maxval(Gacc).gt.reqacc_cll) AccPointsCntG_cll =  AccPointsCntG_cll + 1
      if(maxval(Gacc).gt.sreqacc_cll) sAccPointsCntG_cll =  sAccPointsCntG_cll + 1

      if(maxval(Gacc).gt.critacc_cll) then
        CritPointsCntG_cll =  CritPointsCntG_cll + 1
        if ( CritPointsCntG_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut_cll('G_cll',0,maxval(Gacc), CritPointsCntG_cll)
          if( CritPointsCntG_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncheckout_cll,*) ' Further output of Critical Points for G_cll suppressed '   
            write(nerrout_cll,*)
          endif
        end if
      end if

#ifdef CritPoints2
      if (mode_cll.ne.2) then      
      if(maxval(Gacc2).gt.reqacc_cll) AccPointsCntG2_cll =  AccPointsCntG2_cll + 1
      if(maxval(Gacc2).gt.sreqacc_cll) sAccPointsCntG2_cll =  sAccPointsCntG2_cll + 1

      if(maxval(Gacc2).gt.critacc_cll) then
        CritPointsCntG2_cll =  CritPointsCntG2_cll + 1
        if ( CritPointsCntG2_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut2_cll('G_cll',0,maxval(Gacc2), CritPointsCntG2_cll)
          if( CritPointsCntG2_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for G_cll suppressed '   
            write(ncpout2_cll,*)
          endif
        end if
      end if
      end if
#endif

    end if


  end subroutine G_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine G_arrays_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine G_arrays_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(21), masses2(0:6)
    double complex, intent(out) :: G(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Guv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double precision :: Gerraux(0:rmax),Gerr2aux(0:rmax)

    if (present(Gerr)) then
      if (present(Gerr2)) then
        call G_main_cll(G,Guv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr,Gerr2=Gerr2)
      else
        call G_main_cll(G,Guv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr)
      end if
    else
      if (present(Gerr2)) then
        call G_main_cll(G,Guv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerraux,Gerr2=Gerr2)
      else
        call G_main_cll(G,Guv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerraux)
      end if
    end if
    
  end subroutine G_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine G_list_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53, &
  !                               p64,p50,p61,p30,p41,p52,p63,p40,p51,p62, &
  !                               m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine G_list_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53, &
                               p64,p50,p61,p30,p41,p52,p63,p40,p51,p62, &
                               m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,Gerr2)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: G(:),Guv(:)
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call G_list_checked_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53, &
                               p64,p50,p61,p30,p41,p52,p63,p40,p51,p62, &
                               m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,Gerr2)

  end subroutine G_list_cll
      

  subroutine G_list_checked_cll(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53, &
                               p64,p50,p61,p30,p41,p52,p63,p40,p51,p62, &
                               m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,Gerr2)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: G(NCoefs(rmax,7))
    double complex, intent(out) :: Guv(NCoefs(rmax,7))
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double complex :: G_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Guv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Gerraux(0:rmax),Gerr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,n6,cnt

    if (present(Gerr)) then
      if (present(Gerr2)) then
        call G_main_cll(G_aux,Guv_aux,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,Gerr,Gerr2=Gerr2)
      else
        call G_main_cll(G_aux,Guv_aux,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,Gerr)
      end if
    else
      if (present(Gerr2)) then
        call G_main_cll(G_aux,Guv_aux,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,Gerraux,Gerr2=Gerr2)
      else
        call G_main_cll(G_aux,Guv_aux,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
            m02,m12,m22,m32,m42,m52,m62,rmax,Gerraux)
      end if
    end if
    
    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              do n4=r-2*n0-n1-n2-n3,0,-1
                do n5=r-2*n0-n1-n2-n3-n4,0,-1
                  n6 = r-2*n0-n1-n2-n3-n4-n5

                  cnt = cnt+1
                  G(cnt) = G_aux(n0,n1,n2,n3,n4,n5,n6)
                  Guv(cnt) = Guv_aux(n0,n1,n2,n3,n4,n5,n6)      

                end do
              end do
            end do
          end do
        end do
      end do
    end do  
    
  end subroutine G_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine G_arrays_list_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine G_arrays_list_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(21), masses2(0:6)
    double complex, intent(out) :: G(:),Guv(:)
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('G_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call G_arrays_list_checked_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)

  end subroutine G_arrays_list_cll


  subroutine G_arrays_list_checked_cll(G,Guv,MomInv,masses2,rmax,Gerr,Gerr2)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(21), masses2(0:6)
    double complex, intent(out) :: G(NCoefs(rmax,7))
    double complex, intent(out) :: Guv(NCoefs(rmax,7))
    double precision, optional, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double complex :: G_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Guv_aux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Gerraux(0:rmax),Gerr2aux(0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,n6,cnt

    if (present(Gerr)) then
      if (present(Gerr2)) then
        call G_main_cll(G_aux,Guv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr,Gerr2=Gerr2)
      else
        call G_main_cll(G_aux,Guv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr)
      end if
    else
      if (present(Gerr)) then
        call G_main_cll(G_aux,Guv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerraux,Gerr2=Gerr2)
      else
        call G_main_cll(G_aux,Guv_aux,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12),  &
            MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18),  &
            MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),  &
            masses2(2),masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerraux)
      end if
    end if

    cnt = 0
    do r=0,rmax
      do n0=r/2,0,-1
        do n1=r-2*n0,0,-1
          do n2=r-2*n0-n1,0,-1
            do n3=r-2*n0-n1-n2,0,-1
              do n4=r-2*n0-n1-n2-n3,0,-1
                do n5=r-2*n0-n1-n2-n3-n4,0,-1
                  n6 = r-2*n0-n1-n2-n3-n4-n5

                  cnt = cnt+1
                  G(cnt) = G_aux(n0,n1,n2,n3,n4,n5,n6)
                  Guv(cnt) = Guv_aux(n0,n1,n2,n3,n4,n5,n6)      

                end do
              end do
            end do
          end do
        end do
      end do
    end do  
    
  end subroutine G_arrays_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine TN_main_cll(TN,TNuv,MomInv,masses2,N,rmax,TNerr,id_in,TNerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine TN_main_cll(TN,TNuv,MomInv,masses2,N,rmax,TNerr,id_in,TNerr2)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomInv(:), masses2(0:)
    double complex, intent(out) :: TN(:)
    double complex, intent(out) :: TNuv(:)
    integer, optional, intent(in) :: id_in
    double precision, optional, intent(out) :: TNerr(0:),TNerr2(0:)
    logical :: eflag

    if (N.eq.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','subroutine called with wrong number of arguments for N=1',eflag,.true.)
      call PropagateErrFlag_cll
      return
    end if    
    if (N.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','argument N larger than Nmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'N        =',N
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= ',N
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call TN_main_checked_cll(TN,TNuv,MomInv,masses2,N,rmax,TNerr,id_in,TNerr2)

  end subroutine TN_main_cll


  subroutine TN_main_checked_cll(TN,TNuv,MomInv,masses2,N,rmax,TNerr,id_in,TNerr2)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex, intent(out) :: TN(NCoefs(rmax,N))
    double complex, intent(out) :: TNuv(NCoefs(rmax,N))
    integer, optional, intent(in) :: id_in
    double precision, optional, intent(out) :: TNerr(0:rmax),TNerr2(0:rmax)
    double precision :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double precision :: q51,q30,q41,q52
    double complex :: mm02,mm12,mm22,mm32,mm42,mm52
    double complex :: TN2(NCoefs(rmax,N)),TN2uv(NCoefs(rmax,N))
    double complex :: Adduv(0:rmax/2), Bdduv(0:rmax,0:rmax)
    double complex :: Cdduv(0:rmax,0:rmax,0:rmax)
    double complex :: Ddduv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Add(0:rmax/2), Bdd(0:rmax,0:rmax)
    double complex :: Cdd(0:rmax,0:rmax,0:rmax)
    double complex :: Ddd(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Edd(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Fdd(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: elimcminf2
    double precision :: TNerraux(0:rmax),TNerr2aux(0:rmax),TNdiff(0:rmax)
    double complex :: args(BinomTable(2,N)+N)
    integer :: n0,n1,n2,n3,n4,n5,r,i,cnt,rank,errflag
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: TNacc(0:rmax),TNacc2(0:rmax),norm,norm_coli,norm_dd
    integer :: accflagDD,errflagDD,rankDD,NDD,id
    logical :: mflag,eflag

!    if (N.eq.1) then
!      call SetErrFlag_cll(-10)
!      call ErrOut_cll('TN_cll','subroutine called with wrong number of arguments for N=1',eflag)
!      return
!    end if    
    
    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      args(1:BinomTable(2,N)) = MomInv
      args(BinomTable(2,N)+1:BinomTable(2,N)+N) = masses2(0:N-1)
      call SetMasterFname_cll('TN_cll')
      call SetMasterN_cll(N)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(BinomTable(2,N)+N,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcTN(TN,TNuv,MomInv,masses2,N,rmax,id,TNerraux,TNerr2aux)

        if (present(TNerr)) TNerr = TNerraux
        if (present(TNerr2)) TNerr2 = TNerr2aux

        norm = abs(TN(1))
        do r=1,rmax
          do i=NCoefs(r,N)-BinomTable(r,r+N-2)+1,NCoefs(r,N)
            norm =  max(norm,abs(TN(i)))
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                 maxval(abs(masses2(0:N-1))))
          if(norm.ne.0d0) then
            norm=1d0/norm**(N-2)
          else
            norm=1d0/muir2_cll**(N-2)
          end if 
        end if
        if (norm.ne.0d0) then
          TNacc  = TNerraux/norm
          TNacc2 = TNerr2aux/norm
        else
          TNacc = 0d0
          TNacc2 = 0d0
        end if

        if (mflag) call PropagateAccFlag_cll(TNacc,rmax)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        select case (N)

          case(2)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
    
            rank = rmax
            call B_dd(Bdd,Bdduv,q10,mm02,mm12,rank,id)

            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                n1=r-2*n0 

                cnt = cnt+1
                TN(cnt) = Bdd(n0,n1)
                TNuv(cnt) = Bdduv(n0,n1)

              end do
            end do


          case(3)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q20 = dreal(getminf2DD_cll(MomInv(3)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
    
            rank = rmax
            call C_dd(Cdd,Cdduv,q10,q21,q20,mm02,mm12,mm22,rank,id)

            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  n2 = r-2*n0-n1

                  cnt = cnt+1
                  TN(cnt) = Cdd(n0,n1,n2)
                  TNuv(cnt) = Cdduv(n0,n1,n2)

                end do
              end do
            end do
            

          case(4)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q30 = dreal(getminf2DD_cll(MomInv(4)))
            q20 = dreal(getminf2DD_cll(MomInv(5)))
            q31 = dreal(getminf2DD_cll(MomInv(6)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))

            rank = rmax
            call D_dd(Ddd,Ddduv,q10,q21,q32,q30,q20,q31,  &
                      mm02,mm12,mm22,mm32,rank,id)

            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    n3 = r-2*n0-n1-n2

                    cnt = cnt+1
                    TN(cnt) = Ddd(n0,n1,n2,n3)
                    TNuv(cnt) = Ddduv(n0,n1,n2,n3)

                  end do
                end do
              end do
            end do


          case(5)

            if (rmax.gt.5) then
              call SetErrFlag_cll(-10)
              call ErrOut_cll('TN_cll','rank higher than maximum rank implemented in DD library',eflag)
              if(eflag) then
                write(nerrout_cll,*) 'TN_cll: 5-point function of rank>5 not implemented in DD library'
              end if
            end if

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q43 = dreal(getminf2DD_cll(MomInv(4)))
            q40 = dreal(getminf2DD_cll(MomInv(5)))
            q20 = dreal(getminf2DD_cll(MomInv(6)))
            q31 = dreal(getminf2DD_cll(MomInv(7)))
            q42 = dreal(getminf2DD_cll(MomInv(8)))
            q30 = dreal(getminf2DD_cll(MomInv(9)))
            q41 = dreal(getminf2DD_cll(MomInv(10)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))
            mm42 = getminf2DD_cll(masses2(4))

            rank = rmax
            call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                      mm02,mm12,mm22,mm32,mm42,rank,id) 
        
            TNuv = 0d0
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    do n3=r-2*n0-n1-n2,0,-1
                      n4 = r-2*n0-n1-n2-n3

                      cnt = cnt+1
                      TN(cnt) = Edd(n0,n1,n2,n3,n4)

                    end do
                  end do
                end do
              end do
            end do


          case(6)

            if (rmax.gt.6) then
              call SetErrFlag_cll(-10)
              call ErrOut_cll('TN_cll','rank higher than maximum rank implemented in DD library',eflag)
              if(eflag) then
                write(nerrout_cll,*) 'TN_cll: 6-point function of rank>6 not implemented in DD library'
              end if
            end if

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q43 = dreal(getminf2DD_cll(MomInv(4)))
            q54 = dreal(getminf2DD_cll(MomInv(5)))
            q50 = dreal(getminf2DD_cll(MomInv(6)))
            q20 = dreal(getminf2DD_cll(MomInv(7)))
            q31 = dreal(getminf2DD_cll(MomInv(8)))
            q42 = dreal(getminf2DD_cll(MomInv(9)))
            q53 = dreal(getminf2DD_cll(MomInv(10)))
            q40 = dreal(getminf2DD_cll(MomInv(11)))
            q51 = dreal(getminf2DD_cll(MomInv(12)))
            q30 = dreal(getminf2DD_cll(MomInv(13)))
            q41 = dreal(getminf2DD_cll(MomInv(14)))
            q52 = dreal(getminf2DD_cll(MomInv(15)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))
            mm42 = getminf2DD_cll(masses2(4))
            mm52 = getminf2DD_cll(masses2(5))

            rank = rmax
            call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                      q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,id)

            TNuv = 0d0
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    do n3=r-2*n0-n1-n2,0,-1
                      do n4=r-2*n0-n1-n2-n3,0,-1
                        n5 = r-2*n0-n1-n2-n3-n4

                        cnt = cnt+1
                        TN(cnt) = Fdd(n0,n1,n2,n3,n4,n5)

                      end do
                    end do
                  end do
                end do
              end do
            end do
            

          case(7:)

            call SetErrFlag_cll(-10)
            call ErrOut_cll('TN_cll','N-point functions not implemented in DD library for N>=7',eflag)
            if(eflag) then
              write(nerrout_cll,*) 'TN_cll: N-point functions not implemented in DD library for N>=7'
              write(nerrout_cll,*) 'TN_cll: --> use COLI implementation (mode_cll=1)'
            end if


        end select

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)        
        
        if (present(TNerr)) TNerr(0:rmax) = accabsDD(0:rmax)
        if (present(TNerr2)) TNerr2(0:rmax) = accabs2DD(0:rmax)
        
        norm = abs(TN(1))
        do r=1,rmax
          do i=NCoefs(r,N)-BinomTable(r,r+N-2)+1,NCoefs(r,N)
            norm =  max(norm,abs(TN(i)))
          end do
        end do
        if (norm.eq.0d0) then
          norm = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                 maxval(abs(masses2(0:N-1))))
          if(norm.ne.0d0) then
            norm=1d0/norm**(N-2)
          else
            norm=1d0/muir2_cll**(N-2)
          end if 
        end if
        if (norm.ne.0d0) then
          TNacc  = accabsDD(0:rmax)/norm
          TNacc2 = accabs2DD(0:rmax)/norm
        else
          TNacc = 0d0
          TNacc2 = 0d0
        end if
        if (mflag) call PropagateAccFlag_cll(TNacc,rmax)
        

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop-integral using COLI
        call CalcTN(TN,TNuv,MomInv,masses2,N,rmax,id,TNerraux,TNerr2aux)


        select case (N)

          case(2)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
     
            ! calculate loop-integral using DD
            rank = rmax
            call B_dd(Bdd,Bdduv,q10,mm02,mm12,rank,id)
            call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)            

            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                n1=r-2*n0
 
                cnt = cnt+1
                TN2(cnt) = Bdd(n0,n1)
                TN2uv(cnt) = Bdduv(n0,n1)

              end do
            end do


          case(3)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q20 = dreal(getminf2DD_cll(MomInv(3)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
     
            ! calculate loop-integral using DD
            rank = rmax
            call C_dd(Cdd,Cdduv,q10,q21,q20,mm02,mm12,mm22,rank,id)
            call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)            

            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  n2 = r-2*n0-n1

                  cnt = cnt+1
                  TN2(cnt) = Cdd(n0,n1,n2)
                  TN2uv(cnt) = Cdduv(n0,n1,n2)

                end do
              end do
            end do


          case(4)

            id=0

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q30 = dreal(getminf2DD_cll(MomInv(4)))
            q20 = dreal(getminf2DD_cll(MomInv(5)))
            q31 = dreal(getminf2DD_cll(MomInv(6)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))
     
            ! calculate loop-integral using DD
            rank = rmax
            call D_dd(Ddd,Ddduv,q10,q21,q32,q30,q20,q31,  &
                      mm02,mm12,mm22,mm32,rank,id)
            call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
                      
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    n3 = r-2*n0-n1-n2
        
                    cnt = cnt+1
                    TN2(cnt) = Ddd(n0,n1,n2,n3)
                    TN2uv(cnt) = Ddduv(n0,n1,n2,n3)

                  end do
                end do
              end do
            end do


          case(5)

            if (rmax.gt.5) then
              call SetErrFlag_cll(-10)
              call ErrOut_cll('TN_cll','rank higher than maximum rank implemented in DD library',eflag)
              if(eflag) then
                write(nerrout_cll,*) 'TN_cll: 5-point function of rank>5 not implemented in DD library'
              end if
            end if

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q43 = dreal(getminf2DD_cll(MomInv(4)))
            q40 = dreal(getminf2DD_cll(MomInv(5)))
            q20 = dreal(getminf2DD_cll(MomInv(6)))
            q31 = dreal(getminf2DD_cll(MomInv(7)))
            q42 = dreal(getminf2DD_cll(MomInv(8)))
            q30 = dreal(getminf2DD_cll(MomInv(9)))
            q41 = dreal(getminf2DD_cll(MomInv(10)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))
            mm42 = getminf2DD_cll(masses2(4))
     
            ! calculate loop-integral using DD
            rank = rmax
            call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                      mm02,mm12,mm22,mm32,mm42,rank,id)
            call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
            
            TN2uv = 0d0
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    do n3=r-2*n0-n1-n2,0,-1
                      n4 = r-2*n0-n1-n2-n3

                      cnt = cnt+1
                      TN2(cnt) = Edd(n0,n1,n2,n3,n4)

                    end do
                  end do
                end do
              end do
            end do


          case(6)

            if (rmax.gt.6) then
              call SetErrFlag_cll(-10)
              call ErrOut_cll('TN_cll','rank higher than maximum rank implemented in DD library',eflag)
              if(eflag) then
                write(nerrout_cll,*) 'TN_cll: 6-point function of rank>6 not implemented in DD library'
              end if
            end if

            ! replace small masses by DD-identifiers    
            q10 = dreal(getminf2DD_cll(MomInv(1)))
            q21 = dreal(getminf2DD_cll(MomInv(2)))
            q32 = dreal(getminf2DD_cll(MomInv(3)))
            q43 = dreal(getminf2DD_cll(MomInv(4)))
            q54 = dreal(getminf2DD_cll(MomInv(5)))
            q50 = dreal(getminf2DD_cll(MomInv(6)))
            q20 = dreal(getminf2DD_cll(MomInv(7)))
            q31 = dreal(getminf2DD_cll(MomInv(8)))
            q42 = dreal(getminf2DD_cll(MomInv(9)))
            q53 = dreal(getminf2DD_cll(MomInv(10)))
            q40 = dreal(getminf2DD_cll(MomInv(11)))
            q51 = dreal(getminf2DD_cll(MomInv(12)))
            q30 = dreal(getminf2DD_cll(MomInv(13)))
            q41 = dreal(getminf2DD_cll(MomInv(14)))
            q52 = dreal(getminf2DD_cll(MomInv(15)))
            mm02 = getminf2DD_cll(masses2(0))
            mm12 = getminf2DD_cll(masses2(1))
            mm22 = getminf2DD_cll(masses2(2))
            mm32 = getminf2DD_cll(masses2(3))
            mm42 = getminf2DD_cll(masses2(4))
            mm52 = getminf2DD_cll(masses2(5))
     
            ! calculate loop-integral using DD
            rank = rmax
            call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                      q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,id)
            call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
                      
            TN2uv = 0d0
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do n1=r-2*n0,0,-1
                  do n2=r-2*n0-n1,0,-1
                    do n3=r-2*n0-n1-n2,0,-1
                      do n4=r-2*n0-n1-n2-n3,0,-1
                        n5 = r-2*n0-n1-n2-n3-n4

                        cnt = cnt+1
                        TN2(cnt) = Fdd(n0,n1,n2,n3,n4,n5)

                      end do
                    end do
                  end do
                end do
              end do
            end do
            

          case(7:)

            call SetErrFlag_cll(-10)
            call ErrOut_cll('TN_cll','N-point functions not implemented in DD library for N>=7',eflag)
            if(eflag) then
              write(nerrout_cll,*) 'TN_cll: N-point functions not implemented in DD library for N>=7'
              write(nerrout_cll,*) 'TN_cll: --> use COLI implementation (mode_cll=1)'
            end if

        end select

        if (N.le.6) then
          norm_coli = abs(TN(1))
          norm_dd = abs(TN2(1))
          do r=1,rmax
            do i=NCoefs(r,N)-BinomTable(r,r+N-2)+1,NCoefs(r,N)
              norm_coli =  max(norm_coli,abs(TN(i)))
              norm_dd =  max(norm_dd,abs(TN2(i)))
            end do
          end do

          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(N-2)
            else
              norm_coli=1d0/muir2_cll**(N-2)
            end if 
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  & 
                   maxval(abs(masses2(0:N-1))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(N-2)
            else
              norm_dd=1d0/muir2_cll**(N-2)
            end if 
          end if
          norm = min(norm_coli,norm_dd)

         ! cross-check
          call CheckCoefsTN_cll(TN,TN2,MomInv,masses2,N,rmax,norm,TNdiff)

          if (TNerraux(rmax).lt.accabsDD(rmax)) then                 
            if (present(TNerr)) TNerr = max(TNerraux,TNdiff)
            if (present(TNerr2)) TNerr2 = TNerr2aux
            if (norm_coli.ne.0d0) then
              TNacc = max(TNerraux/norm_coli,TNdiff/norm)
              TNacc2 = TNerr2aux/norm_coli
            else
              TNacc = TNdiff
              TNacc2 = 0d0
            end if        
            if (Monitoring) PointsCntTN_coli(N) =  PointsCntTN_coli(N) + 1
          else
            TN = TN2
            TNuv = TN2uv
            if (present(TNerr)) TNerr = max(accabsDD(0:rmax),TNdiff)
            if (present(TNerr2)) TNerr2 = accabs2DD(0:rmax)
            if (norm_dd.ne.0d0) then
              TNacc = max(accabsDD(0:rmax)/norm_dd,TNdiff/norm)
              TNacc2 = accabs2DD(0:rmax)/norm_dd
            else
              TNacc = TNdiff
              TNacc2 = 0d0
            end if                       
            if (Monitoring) PointsCntTN_dd(N) =  PointsCntTN_dd(N) + 1
          end if

        else
        
          norm = abs(TN(1))
          do r=1,rmax
            do i=NCoefs(r,N)-BinomTable(r,r+N-2)+1,NCoefs(r,N)
              norm =  max(norm,abs(TN(i)))
            end do
          end do
          if (norm.eq.0d0) then
            norm = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm.ne.0d0) then
              norm=1d0/norm**(N-2)
            else
              norm=1d0/muir2_cll**(N-2)
            end if 
          end if
          if (present(TNerr)) TNerr = TNerraux
          if (present(TNerr2)) TNerr2 = TNerr2aux

          if (norm.ne.0d0) then
            TNacc  = TNerraux/norm
            TNacc2 = TNerr2aux/norm
          else
            TNacc = 0d0
            TNacc2 = 0d0
          end if
          if (Monitoring) PointsCntTN_coli(N) =  PointsCntTN_coli(N) + 1
          
        end if

        if (mflag) call PropagateAccFlag_cll(TNacc,rmax)        
        
    end select
    
    if (mflag) call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntTN_cll(N) =  PointsCntTN_cll(N) + 1

      if(maxval(TNacc).gt.reqacc_cll) AccPointsCntTN_cll(N) = AccPointsCntTN_cll(N) + 1
      if(maxval(TNacc).gt.sreqacc_cll) sAccPointsCntTN_cll(N) = sAccPointsCntTN_cll(N) + 1

      if(maxval(TNacc).gt.critacc_cll) then
        CritPointsCntTN_cll(N) =  CritPointsCntTN_cll(N) + 1
        if ( CritPointsCntTN_cll(N).le.noutCritPointsMax_cll(N) ) then
          call CritPointsOut_cll('TN_cll',N,maxval(TNacc),CritPointsCntTN_cll(N))
          if( CritPointsCntTN_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TN_cll suppressed for N =',N   
            write(ncpout_cll,*)
          endif
        end if
      end if


#ifdef CritPoints2
      if(maxval(TNacc2).gt.reqacc_cll) AccPointsCntTN2_cll(N) = AccPointsCntTN2_cll(N) + 1
      if(maxval(TNacc2).gt.sreqacc_cll) sAccPointsCntTN2_cll(N) = sAccPointsCntTN2_cll(N) + 1

      if(maxval(TNacc2).gt.critacc_cll) then
        CritPointsCntTN2_cll(N) =  CritPointsCntTN2_cll(N) + 1
        if ( CritPointsCntTN2_cll(N).le.noutCritPointsMax_cll(N) ) then
          call CritPointsOut2_cll('TN_cll',N,maxval(TNacc2),CritPointsCntTN2_cll(N))
          if( CritPointsCntTN2_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TN_cll suppressed for N =',N   
            write(ncpout2_cll,*)
          endif
        end if
      end if
#endif
    end if

  end subroutine TN_main_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine T1_cll(A,Auv,masses2,N,rmax,Aerr,id_in)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine T1_cll(A,Auv,masses2,N,rmax,Aerr,id_in)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: masses2(0:0)
    double complex, intent(out) :: A(:)
    double complex, intent(out) :: Auv(:)
    integer, optional, intent(in) :: id_in
    double precision, optional, intent(out) :: Aerr(0:rmax)
    logical :: eflag
    
    if (N.ne.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','subroutine called with inconsistent arguments',eflag)
      return
    end if
    if (N.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','argument N larger than Nmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'N        =',N
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= ',N
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TN_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call T1_checked_cll(A,Auv,masses2,N,rmax,Aerr,id_in)

  end subroutine T1_cll


  subroutine T1_checked_cll(A,Auv,masses2,N,rmax,Aerr,id_in)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: masses2(0:0)
    double complex, intent(out) :: A(NCoefs(rmax,1))
    double complex, intent(out) :: Auv(NCoefs(rmax,1))
    integer, optional, intent(in) :: id_in
    double precision, optional, intent(out) :: Aerr(0:rmax)
    double complex :: mm02
    double complex :: A2(NCoefs(rmax,1)),A2uv(NCoefs(rmax,1))
    double complex :: Adduv(0:rmax/2),Add(0:rmax/2)
    double complex :: elimcminf2
    double precision :: Aerraux(0:rmax),Aerr2aux(0:rmax),Adiff(0:rmax)
    double complex :: args(1),MomInvDummy(0)
    integer :: n0,r,i,cnt,rank,errflag
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: Aacc(0:rmax),norm,norm_coli,norm_dd
    integer :: accflagDD,errflagDD,rankDD,NDD,id
    logical :: mflag,eflag
    
!    if (N.ne.1) then
!      call SetErrFlag_cll(-10)
!      call ErrOut_cll('TN_cll','subroutine called with inconsistent arguments',eflag)
!      return
!    end if
        
    mflag=.true.
    if (present(id_in)) then
      mflag=.false.
      id = id_in
    else
      id = 0
    end if

    if (mflag) then
      args(1) = masses2(0)
      call SetMasterFname_cll('TN_cll')
      call SetMasterN_cll(N)
      call SetMasterR_cll(rmax)
      call SetMasterArgs_cll(1,args)

      call SetTenCache_cll(never_tenred_cll)
    end if


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        call CalcA(A,Auv,masses2(0),rmax,Aerraux)
        if (abs(A(1)).ne.0d0) then
          Aacc=Aerraux/abs(A(1))
        else
          Aacc=0d0
        end if
        if (present(Aerr))  Aerr=Aerraux
        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)        
        

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        id=0
    
        ! replace small masses by DD-identifiers
        mm02 = getminf2DD_cll(masses2(0))

        rank = rmax
        call A_dd(Add,Adduv,mm02,rank,id)

        do n0=0,rank/2
          A(n0+1) = Add(n0)
          Auv(n0+1) = Adduv(n0)
        end do

        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)        
        
        if (present(Aerr)) Aerr(0:rmax) = accabsDD(0:rmax)

        if (abs(A(1)).ne.0d0) then
          Aacc=accabsDD(0:rmax)/abs(A(1))
        else
          Aacc=0d0
        end if
        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcA(A,Auv,masses2(0),rmax,Aerraux)

        ! calculate loop integral using DD        
        id=0

        ! replace small masses by DD-identifiers 
        mm02 = getminf2DD_cll(masses2(0))

        rank = rmax
        call A_dd(Add,Adduv,mm02,rank,id)
        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,id)
        
        do n0=0,rank/2
          A2(n0+1) = Add(n0)
          A2uv(n0+1) = Adduv(n0)
        end do


        ! cross-check

        norm_coli = abs(A(1))
        if(norm_coli.eq.0d0) norm_coli = muuv2_cll
        norm_dd = abs(A2(1))
        if(norm_coli.eq.0d0) norm_dd = muuv2_cll
        norm = min(norm_coli,norm_dd)

        call CheckCoefsA_cll(A,A2,masses2(0),rmax,norm,Adiff)
        
        if (Aerraux(rmax).lt.accabsDD(rmax)) then
          if (present(Aerr)) Aerr = max(Aerraux,Adiff)         
          Aacc = max(Aerraux/norm_coli,Adiff/norm)
          if (Monitoring) PointsCntTN_coli(1) = PointsCntTN_coli(1) + 1
        else
          A = A2
          Auv = A2uv
          if (present(Aerr))  Aerr = max(accabsDD(0:rmax),Adiff)  
          Aacc = max(accabsDD(0:rmax)/norm_dd,Adiff/norm)          
          if (Monitoring) PointsCntTN_dd(1) = PointsCntTN_dd(1) + 1
        end if

        if (mflag) call PropagateAccFlag_cll(Aacc,rmax)

    end select

    if (mflag) call PropagateErrFlag_cll
       
    if (Monitoring) then
      PointsCntTN_cll(1) =  PointsCntTN_cll(1) + 1

      if(maxval(Aacc).gt.reqacc_cll) AccPointsCntTN_cll(1) = AccPointsCntTN_cll(1) + 1
      if(maxval(Aacc).gt.sreqacc_cll) sAccPointsCntTN_cll(1) = sAccPointsCntTN_cll(1) + 1

      if(maxval(Aacc).gt.critacc_cll) then
        CritPointsCntTN_cll(1) =  CritPointsCntTN_cll(1) + 1
        if ( CritPointsCntTN_cll(1).le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TN_cll',N,maxval(Aacc),CritPointsCntTN_cll(1))
          if( CritPointsCntTN_cll(1).eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TN_cll suppressed for N =',1   
            write(ncpout_cll,*)
          endif
        end if
      end if

    end if

  end subroutine T1_checked_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine A0_cll(A0,m02)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine A0_cll(A0,m02)
  
    double complex, intent(in) :: m02
    double complex, intent(out) :: A0
    double complex :: mm02
    double complex :: A2uv(0:0),A2(0:0),A0_coli
    double complex :: Auv(0:0),A(0:0)
    double precision :: Adiff(0:0)
    double complex :: Adduv(0:0) 
    double complex :: Add(0:0)
    double complex :: args(1)
    double precision :: norm
    integer :: errflag

    ! set ID of master call
    args(1) = m02
    call SetMasterFname_cll('A0_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(1,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        A0 = A0_coli(m02)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        mm02 = getminf2DD_cll(m02)

        call A_dd(Add,Adduv,mm02,0,0)
        A0 = Add(0)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        A0 = A0_coli(m02)

        ! replace small masses by DD-identifiers    
        mm02 = getminf2DD_cll(m02)

        call A_dd(Add,Adduv,mm02,0,0)
        A2(0) = Add(0)

        ! cross-check
        A(0) = A0
        norm=max(abs(A(0)),abs(A2(0)))
        call CheckCoefsA_cll(A,A2,m02,0,norm,Adiff)


    end select

    call PropagateErrFlag_cll


  end subroutine A0_cll






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B0_main_cll(B0,p10,m02,m12)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B0_main_cll(B0,p10,m02,m12)
  
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: B0
    double complex :: B2uv(0:0,0:0),B2(0:0,0:0),Bn_coli
    double complex :: Buv(0:0,0:0),B(0:0,0:0)
    double precision :: Bdiff(0:0)
    double complex :: Bdduv(0:0,0:0) 
    double complex :: Bdd(0:0,0:0)
    double complex :: args(3)
    double precision :: norm
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('B0_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(3,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

!        B0uv = 1d0
        B0 = Bn_coli(0,p10,m02,m12)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call B_dd(Bdd,Bdduv,q10,mm02,mm12,0,0)
        use_cache_system=use_cache_system_save
!        B0uv = Bdduv(0,0)
        B0 = Bdd(0,0)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
!        B0uv = 1d0
        B0 = Bn_coli(0,p10,m02,m12)

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call B_dd(Bdd,Bdduv,q10,mm02,mm12,0,0)
        use_cache_system=use_cache_system_save
        B2uv(0,0) = Bdduv(0,0)
        B2(0,0) = Bdd(0,0)

        ! cross-check
        B(0,0) = B0
        norm=max(abs(B(0,0)),abs(B2(0,0)))
        call CheckCoefsB_cll(B,B2,p10,m02,m12,0,norm,Bdiff)


    end select

    call PropagateErrFlag_cll


  end subroutine B0_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine B0_arrays_cll(B0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine B0_arrays_cll(B0,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: B0
    
    call B0_main_cll(B0,MomInv(1),masses2(0),masses2(1))

  end subroutine B0_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C0_main_cll(C0,p10,p21,p20,m02,m12,m22)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C0_main_cll(C0,p10,p21,p20,m02,m12,m22)
  
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double precision :: q10,q21,q20
    double complex :: mm02,mm12,mm22
    double complex, intent(out) :: C0
    double complex :: C(0:0,0:0,0:0),C2(0:0,0:0,0:0),C0_coli,C0dd
    double complex args(6)
    double precision :: Cdiff(0:0)
    double precision :: norm
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = p21
    args(3) = p20
    args(4) = m02
    args(5) = m12
    args(6) = m22
    call SetMasterFname_cll('C0_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(6,args)


    ! write(*,*) 'master call: C_cll'
    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH
        C0 = C0_coli(p10,p21,p20,m02,m12,m22)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q20 = dreal(getminf2DD_cll(p20))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)

        C0 = C0dd(q10,q21,q20,mm02,mm12,mm22,0)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        C0 = C0_coli(p10,p21,p20,m02,m12,m22)


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q20 = dreal(getminf2DD_cll(p20))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)

        C2(0,0,0) = C0dd(q10,q21,q20,mm02,mm12,mm22,0)

        ! cross-check
        C(0,0,0)=C0
        norm=max(abs(C(0,0,0)),abs(C2(0,0,0)))
        call CheckCoefsC_cll(C,C2,p10,p21,p20,m02,m12,m22,0,norm,Cdiff)


    end select

    call PropagateErrFlag_cll


  end subroutine C0_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine C0_arrays_cll(C0,MomInv,masses2,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine C0_arrays_cll(C0,MomInv,masses2)
      
    double complex, intent(in) :: MomInv(3), masses2(0:2)
    double complex, intent(out) :: C0
    
    call C0_main_cll(C0,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2))

  end subroutine C0_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D0_main_cll(D0,p10,p21,p32,p30,p20,p31,  &
  !                            m02,m12,m22,m32)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D0_main_cll(D0,p10,p21,p32,p30,p20,p31,  &
                            m02,m12,m22,m32)
  
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double precision :: q10,q21,q32,q30,q20,q31
    double complex :: mm02,mm12,mm22,mm32
    double complex, intent(out) :: D0
    double complex :: D2(0:0,0:0,0:0,0:0),D0_coli,D0dd
    double complex :: D(0:0,0:0,0:0,0:0)
    double complex :: args(10)
    double precision :: Ddiff(0:0)
    double precision :: norm
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = p21
    args(3) = p32
    args(4) = p30
    args(5) = p20
    args(6) = p31
    args(7) = m02
    args(8) = m12
    args(9) = m22
    args(10) = m32
    call SetMasterFname_cll('D0_cll')
    call SetMasterN_cll(4)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(10,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH
        D0 = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
        

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q30 = dreal(getminf2DD_cll(p30))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)

        D0 = D0dd(q10,q21,q32,q30,q20,q31,mm02,mm12,mm22,mm32,0)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        D0 = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)

        ! calculate loop integral using DD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q30 = dreal(getminf2DD_cll(p30))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)

        D2(0,0,0,0) = D0dd(q10,q21,q32,q30,q20,q31,mm02,mm12,mm22,mm32,0)

        ! cross-check
        D(0,0,0,0)=D0
        norm=max(abs(D(0,0,0,0)),abs(D2(0,0,0,0)))
        call CheckCoefsD_cll(D,D2,p10,p21,p32,p30,p20,p31,  &
                    m02,m12,m22,m32,0,norm,Ddiff)


    end select

    call PropagateErrFlag_cll


  end subroutine D0_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine D0_arrays_cll(D0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine D0_arrays_cll(D0,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(6), masses2(0:3)
    double complex, intent(out) :: D0
    
    call D0_main_cll(D0,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
                       masses2(0),masses2(1),masses2(2),masses2(3))

  end subroutine D0_arrays_cll







  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E0_main_cll(E0,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
  !                m02,m12,m22,m32,m42,Eerr,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine E0_main_cll(E0,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
      m02,m12,m22,m32,m42,Eerr,Eerr2)
  
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: E0
    double precision, optional, intent(out) :: Eerr(0:0),Eerr2(0:0)
    double precision :: Eerraux(0:0),Eerr2aux(0:0),Ediff(0:0)
    double precision :: q10,q21,q32,q43,q40,q20,q31,q42,q30,q41
    double complex :: mm02,mm12,mm22,mm32,mm42
    double complex :: E(0:0,0:0,0:0,0:0,0:0)
    double complex :: Euv(0:0,0:0,0:0,0:0,0:0)
    double complex :: E2uv(0:0,0:0,0:0,0:0,0:0) 
    double complex :: E2(0:0,0:0,0:0,0:0,0:0) 
    double complex :: Edd(0:0,0:0,0:0,0:0,0:0)
    double complex :: elimcminf2
    double complex :: args(15)
    double precision :: norm
    integer, parameter :: rank=0

    ! set ID of master call
    args(1) = p10
    args(2) = p21
    args(3) = p32
    args(4) = p43
    args(5) = p40
    args(6) = p20
    args(7) = p31
    args(8) = p42
    args(9) = p30
    args(10) = p41
    args(11) = m02
    args(12) = m12
    args(13) = m22
    args(14) = m32
    args(15) = m42
    call SetMasterFname_cll('E0_cll')
    call SetMasterN_cll(5)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(15,args)

    call SetTenCache_cll(never_tenred_cll)

    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        if (present(Eerr)) then
          if (present(Eerr2)) then
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                m02,m12,m22,m32,m42,rank,0,Eerr,Eerr2)
          else
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                     m02,m12,m22,m32,m42,rank,0,Eerr,Eerr2aux)
          end if
        else
          if (present(Eerr2)) then
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                m02,m12,m22,m32,m42,rank,0,Eerraux,Eerr2)
          else
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                     m02,m12,m22,m32,m42,rank,0,Eerraux,Eerr2aux)
          end if
        end if
        E0 = E(0,0,0,0,0)

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q40 = dreal(getminf2DD_cll(p40))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)

        call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                  mm02,mm12,mm22,mm32,mm42,rank,0)  
        E0 = Edd(0,0,0,0,0)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral
        if (present(Eerr)) then
          if (present(Eerr2)) then
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                m02,m12,m22,m32,m42,rank,0,Eerr,Eerr2)
          else
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                     m02,m12,m22,m32,m42,rank,0,Eerr,Eerr2aux)
          end if
        else
          if (present(Eerr2)) then
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                m02,m12,m22,m32,m42,rank,0,Eerraux,Eerr2)
          else
            call CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                     m02,m12,m22,m32,m42,rank,0,Eerraux,Eerr2aux)
          end if
        end if
        E0 = E(0,0,0,0,0)

        ! calculate loop integral

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q40 = dreal(getminf2DD_cll(p40))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)

        call E_dd(Edd,q10,q21,q32,q43,q40,q20,q31,q42,q30,q41,  &
                  mm02,mm12,mm22,mm32,mm42,rank,0)  

        E2(0,0,0,0,0) = Edd(0,0,0,0,0)
        E2uv = 0d0

        norm=max(abs(E(0,0,0,0,0)),abs(E2(0,0,0,0,0)))

        ! cross-check
        call CheckCoefsE_cll(E,E2,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                   m02,m12,m22,m32,m42,rank,norm,Ediff)

    end select

    call PropagateErrFlag_cll


  end subroutine E0_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine E0_arrays_cll(E0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine E0_arrays_cll(E0,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(10), masses2(0:4)
    double complex, intent(out) :: E0
    
    call E0_main_cll(E0,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10), &
        masses2(0),masses2(1),masses2(2),masses2(3),masses2(4))

  end subroutine E0_arrays_cll







  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F0_main_cll(F0,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                          p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F0_main_cll(F0,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                             p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,Ferr,Ferr2)
  
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: F0
    double precision, optional, intent(out) :: Ferr(0:0),Ferr2(0:0)
    double precision :: Ferraux(0:0),Ferr2aux(0:0),Fdiff(0:0)
    double precision :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double precision :: q51,q30,q41,q52
    double complex :: mm02,mm12,mm22,mm32,mm42,mm52
    integer, parameter :: rmax=0, rank=0
    double complex :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Fuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: F2uv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: F2(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: Fdd(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: elimcminf2
    double complex :: args(21)
    double precision :: norm

    ! set ID of master call
    args(1) = p10
    args(2) = p21
    args(3) = p32
    args(4) = p43
    args(5) = p54
    args(6) = p50
    args(7) = p20
    args(8) = p31
    args(9) = p42
    args(10) = p53
    args(11) = p40
    args(12) = p51
    args(13) = p30
    args(14) = p41
    args(15) = p52
    args(16) = m02
    args(17) = m12
    args(18) = m22
    args(19) = m32
    args(20) = m42
    args(21) = m52
    call SetMasterFname_cll('F0_cll')
    call SetMasterN_cll(6)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(21,args)

!    write(*,*) 'F_main_cll mode',mode_cll

    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        if (present(Ferr)) then
          if (present(Ferr2)) then
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,0,0,Ferr,Ferr2)
          else
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferr,Ferr2aux)
          endif
        else
          if (present(Ferr2)) then
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferraux,Ferr2)
          else
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferraux,Ferr2aux)
          end if
        endif
        F0 = F(0,0,0,0,0,0)

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q54 = dreal(getminf2DD_cll(p54))
        q50 = dreal(getminf2DD_cll(p50))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q53 = dreal(getminf2DD_cll(p53))
        q40 = dreal(getminf2DD_cll(p40))
        q51 = dreal(getminf2DD_cll(p51))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        q52 = dreal(getminf2DD_cll(p52))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)
        mm52 = getminf2DD_cll(m52)

        call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                  q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,0)  

        F0 = Fdd(0,0,0,0,0,0)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral
        if (present(Ferr)) then
          if (present(Ferr2)) then
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferr,Ferr2)
          else
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferr,Ferr2aux)
          endif
        else
          if (present(Ferr2)) then
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferraux,Ferr2)
          else
            call CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,0,Ferraux,Ferr2aux)
          end if
        end if
        F0 = F(0,0,0,0,0,0)

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        q21 = dreal(getminf2DD_cll(p21))
        q32 = dreal(getminf2DD_cll(p32))
        q43 = dreal(getminf2DD_cll(p43))
        q54 = dreal(getminf2DD_cll(p54))
        q50 = dreal(getminf2DD_cll(p50))
        q20 = dreal(getminf2DD_cll(p20))
        q31 = dreal(getminf2DD_cll(p31))
        q42 = dreal(getminf2DD_cll(p42))
        q53 = dreal(getminf2DD_cll(p53))
        q40 = dreal(getminf2DD_cll(p40))
        q51 = dreal(getminf2DD_cll(p51))
        q30 = dreal(getminf2DD_cll(p30))
        q41 = dreal(getminf2DD_cll(p41))
        q52 = dreal(getminf2DD_cll(p52))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)
        mm22 = getminf2DD_cll(m22)
        mm32 = getminf2DD_cll(m32)
        mm42 = getminf2DD_cll(m42)
        mm52 = getminf2DD_cll(m52)

        call F_dd(Fdd,q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40,  &
                  q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52,rank,0)  
        F2(0,0,0,0,0,0) = Fdd(0,0,0,0,0,0)

        norm=max(abs(F(0,0,0,0,0,0)),abs(F2(0,0,0,0,0,0)))

        ! cross-check
        call CheckCoefsF_cll(F,F2,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
            p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,norm,Fdiff)

    end select

    call PropagateErrFlag_cll


  end subroutine F0_main_cll




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine F0_arrays_cll(F0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine F0_arrays_cll(F0,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(15), masses2(0:5)
    double complex, intent(out) :: F0
    
    call F0_main_cll(F0,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),  &
        MomInv(11),MomInv(12),MomInv(13),MomInv(14),MomInv(15),  &
        masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),masses2(5))

  end subroutine F0_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB0_main_cll(DB0,p10,m02,m12)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB0_main_cll(DB0,p10,m02,m12)
  
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: DB0
    double complex :: DB0dd, DB1dd
    double complex :: DB0_coli
    double complex :: args(3)
    double complex :: DBdduv(0:0,0:0)
    double complex :: DBdd(0:0,0:0)
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('DB0_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(0)
    call SetMasterArgs_cll(3,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        DB0 = DB0_coli(p10,m02,m12)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,0)
        use_cache_system=use_cache_system_save
        DB0 = DBdd(0,0)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        DB0 = DB0_coli(p10,m02,m12)


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,0)
        use_cache_system=use_cache_system_save
        DB0dd = DBdd(0,0)

        ! cross-check
        call CheckCoefsDBr_cll(DB0,DB0dd,p10,m02,m12,0)


    end select

    call PropagateErrFlag_cll


  end subroutine DB0_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB0_arrays_cll(DB0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB0_arrays_cll(DB0,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: DB0
    
    call DB0_main_cll(DB0,MomInv(1),masses2(0),masses2(1))

  end subroutine DB0_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB1_main_cll(DB1,p10,m02,m12)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB1_main_cll(DB1,p10,m02,m12)
  
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: DB1
    double complex :: DB0dd, DB1dd 
    double complex :: DB1_coli
    double complex :: args(3)
    double complex :: DBdduv(0:1,0:1)
    double complex :: DBdd(0:1,0:1)
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('DB1_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(1)
    call SetMasterArgs_cll(3,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        DB1 = DB1_coli(p10,m02,m12)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,1)
        use_cache_system=use_cache_system_save
        DB1 = DBdd(0,1)


      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        DB1 = DB1_coli(p10,m02,m12)

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,1)
        use_cache_system=use_cache_system_save
        DB1dd = DBdd(0,1)

        ! cross-check
        call CheckCoefsDBr_cll(DB1,DB1dd,p10,m02,m12,1)


    end select

    call PropagateErrFlag_cll      


  end subroutine DB1_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB1_arrays_cll(DB1,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB1_arrays_cll(DB1,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: DB1
    
    call DB1_main_cll(DB1,MomInv(1),masses2(0),masses2(1))

  end subroutine DB1_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB00_main_cll(DB00,DB00uv,p10,m02,m12)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB00_main_cll(DB00,DB00uv,p10,m02,m12)
  
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: DB00, DB00uv
    double complex :: DB00dd, DB00dduv
    double complex :: DB00_coli
    double complex :: args(3)
    double complex :: DBdduv(0:2,0:2)
    double complex :: DBdd(0:2,0:2)
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('DB00_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(2)
    call SetMasterArgs_cll(3,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        DB00uv = -1d0/12d0
        DB00 = DB00_coli(p10,m02,m12)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,2)
        use_cache_system=use_cache_system_save
        DB00uv = DBdduv(1,0)
        DB00   = DBdd(1,0)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        DB00uv = -1d0/12d0

        ! calculate loop integral using COLI
        DB00 = DB00_coli(p10,m02,m12)


        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,2)
        use_cache_system=use_cache_system_save
        DB00dduv = DBdduv(1,0)
        DB00dd   = DBdd(1,0)

        ! cross-check
        call CheckCoefsDBr_cll(DB00,DB00dd,p10,m02,m12,2)


    end select

    call PropagateErrFlag_cll


  end subroutine DB00_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB00_arrays_cll(DB00,DB00uv,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB00_arrays_cll(DB00,DB00uv,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: DB00uv,DB00
    
    call DB00_main_cll(DB00,DB00uv,MomInv(1),masses2(0),masses2(1))

  end subroutine DB00_arrays_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB11_main_cll(DB11,p10,m02,m12)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB11_main_cll(DB11,p10,m02,m12)
  
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10,DBerraux(0:2)
    double complex :: mm02,mm12
    double complex, intent(out) :: DB11
    double complex :: DB11dd
    double complex :: args(3)
    double complex :: DBcoliuv(0:1,0:1), DBcoli(0:1,0:2)   
    double complex :: DBdduv(0:2,0:2)
    double complex :: DBdd(0:2,0:2)
    integer :: errflag

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('DB11_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(2)
    call SetMasterArgs_cll(3,args)


    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH
  
        use_cache_system=.false.       
        call CalcDB(DBcoli,DBcoliuv,p10,m02,m12,2,0,DBerraux)
        use_cache_system=use_cache_system_save        
        DB11 = DBcoli(0,2)


      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,2)
        use_cache_system=use_cache_system_save
        DB11 = DBdd(0,2)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        use_cache_system=.false.       
        call CalcDB(DBcoli,DBcoliuv,p10,m02,m12,2,0,DBerraux)
        use_cache_system=use_cache_system_save
        DB11 = DBcoli(0,2)        

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,2)
        use_cache_system=use_cache_system_save
        DB11dd   = DBdd(0,2)

        ! cross-check
        call CheckCoefsDBr_cll(DB11,DB11dd,p10,m02,m12,2)


    end select

    call PropagateErrFlag_cll


  end subroutine DB11_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB11_arrays_cll(DB11,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB11_arrays_cll(DB11,MomInv,masses2)
  
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: DB11
    
    call DB11_main_cll(DB11,MomInv(1),masses2(0),masses2(1))

  end subroutine DB11_arrays_cll

  


  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB_main_cll(DB,DBuv,p10,m02,m12,rmax,DBerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB_main_cll(DB,DBuv,p10,m02,m12,rmax,DBerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p10,m02,m12
    double precision :: q10
    double complex :: mm02,mm12
    double complex, intent(out) :: DBuv(0:rmax/2,0:rmax)
    double complex, intent(out) :: DB(0:rmax/2,0:rmax)
    double precision, optional, intent(out) :: DBerr(0:rmax)
    double precision :: DBerraux(0:rmax),DBdiff(0:rmax)
    double complex :: DB2uv(0:rmax/2,0:rmax), DB2(0:rmax/2,0:rmax)
    double complex :: DBcoliuv(0:rmax/2,0:rmax) 
    double complex :: DBcoli(0:rmax/2,0:rmax)
    double complex :: DB0dd,DB1dd
    double complex :: args(3)
    double complex :: DBdduv(0:rmax,0:rmax)
    double complex :: DBdd(0:rmax,0:rmax)
    double precision :: accrelDD(0:rmax_DD),accabsDD(0:rmax_DD)
    double precision :: accrel2DD(0:rmax_DD),accabs2DD(0:rmax_DD)
    double precision :: DBacc(0:rmax),DBacc2(0:rmax),norm,norm_coli,norm_dd
    integer :: accflagDD,errflagDD,NDD,rankDD
    integer :: n0,rank,errflag,i0,i1,n
    logical :: flag = .true.,eflag

!    write(*,*) 'DB_main in',p10,m02,m12,rmax

    ! set ID of master call
    args(1) = p10
    args(2) = m02
    args(3) = m12
    call SetMasterFname_cll('DB_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(3,args)

    select case (mode_cll)

      case (1)
        ! calculate loop integral using
        ! COLI implementation by AD/LH

        if (present(DBerr)) then 
          call CalcDB(DBcoli,DBcoliuv,p10,m02,m12,rmax,0,DBerr)
        else
          call CalcDB(DBcoli,DBcoliuv,p10,m02,m12,rmax,0,DBerraux)
        end if
        DB(0:rmax/2,0:rmax) = DBcoli(0:rmax/2,0:rmax)
        DBuv(0:rmax/2,0:rmax) = DBcoliuv(0:rmax/2,0:rmax)

      case (2)
        ! calculate loop integral using
        ! DD implementation by SD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        rank = rmax

        ! use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,rank)
        ! use_cache_system=use_cache_system_save

        DB(0:rank/2,0:rank) = DBdd(0:rank/2,0:rank)
        DBuv(0:rank/2,0:rank) = DBdduv(0:rank/2,0:rank)

      case (3)
        ! cross-check mode
        ! compare results for loop integral
        ! from COLI implementation by AD/LH and
        ! from DD implementation by SD

        ! calculate loop integral using COLI
        call CalcDB(DBcoli,DBcoliuv,p10,m02,m12,rmax,0,DBerraux)
        DB(0:rmax/2,0:rmax) = DBcoli(0:rmax/2,0:rmax)
        DBuv(0:rmax/2,0:rmax) = DBcoliuv(0:rmax/2,0:rmax)

        ! calculate loop integral using DD

        ! replace small masses by DD-identifiers    
        q10 = dreal(getminf2DD_cll(p10))
        mm02 = getminf2DD_cll(m02)
        mm12 = getminf2DD_cll(m12)

        rank = rmax

        ! use_cache_system=.false.
        call DB_dd(DBdd,DBdduv,q10,mm02,mm12,rank)
        ! use_cache_system=use_cache_system_save

        DB2(0:rank/2,0:rank) = DBdd(0:rank/2,0:rank)
        DB2uv(0:rank/2,0:rank) = DBdduv(0:rank/2,0:rank)
        call DDgetacc(accrelDD,accabsDD,accrel2DD,accabs2DD,NDD,rankDD,accflagDD,errflagDD,0)

        norm_coli = maxval(abs(DB(0,0:rmax)))         
        norm_dd = maxval(abs(DB2(0,0:rmax)))         
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(p10),abs(m02),abs(m12))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli
          else
            norm_coli=1d0/muir2_cll
          end if 
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(p10),abs(m02),abs(m12))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd
          else
            norm_dd=1d0/muir2_cll
          end if 
        end if
        norm = min(norm_coli,norm_dd)

        ! cross-check
        call CheckCoefsDB_cll(DB,DB2,p10,m02,m12,rank,norm,DBdiff)

        if (DBerraux(rmax).lt.accabsDD(rmax)) then
          if (present(DBerr))  DBerr = max(DBerraux,DBdiff)
          DBacc = max(DBerraux/norm_coli,DBdiff/norm)
          if (Monitoring) PointsCntDB_coli =  PointsCntDB_coli + 1
        else
          DB = DB2
          DBuv = DB2uv
          if (present(DBerr))  DBerr = max(accabsDD(0:rmax),DBdiff)
          DBacc = max(accabsDD(0:rmax)/norm_dd,DBdiff/norm)
          if (Monitoring) PointsCntDB_dd =  PointsCntDB_dd + 1
        end if

        call PropagateAccFlag_cll(DBacc,rmax)
 
    end select

    call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntDB_cll =  PointsCntDB_cll + 1

      if(maxval(DBacc).gt.reqacc_cll) AccPointsCntDB_cll =  AccPointsCntDB_cll + 1
      if(maxval(DBacc).gt.sreqacc_cll) sAccPointsCntDB_cll =  sAccPointsCntDB_cll + 1

      if(maxval(DBacc).gt.critacc_cll) then
        CritPointsCntDB_cll =  CritPointsCntDB_cll + 1
        if ( CritPointsCntDB_cll.le.noutCritPointsMaxDB_cll ) then
          call CritPointsOut_cll('DB_cll',0,maxval(DBacc), CritPointsCntDB_cll)
          if( CritPointsCntDB_cll.eq.noutCritPointsMaxDB_cll) then
            write(ncpout_cll,*) ' Further output of Critical Points for DB_cll suppressed '   
            write(ncpout_cll,*)
          endif
        end if
      end if

    end if


  end subroutine DB_main_cll



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine DB_arrays_cll(DB,DBuv,MomInv,masses2,rmax,DBerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DB_arrays_cll(DB,DBuv,MomInv,masses2,rmax,DBerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(out) :: DBuv(0:rmax/2,0:rmax)
    double complex, intent(out) :: DB(0:rmax/2,0:rmax)
    double precision, optional, intent(out) :: DBerr(0:rmax)
    double precision :: DBerraux(0:rmax)
    
    if (present(DBerr)) then
      call DB_main_cll(DB,DBuv,MomInv(1),masses2(0),masses2(1),rmax,DBerr)
    else
      call DB_main_cll(DB,DBuv,MomInv(1),masses2(0),masses2(1),rmax,DBerraux)
    end if

  end subroutine DB_arrays_cll


end module collier_coefs
