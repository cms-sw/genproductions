!!
!!  File collier_global.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ***************************
!  *  module collier_global  *
!  *      by Lars Hofer      *
!  *************************** 
! 
!  contains the global parameters of COLLIER
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module collier_global

  character(len=5) :: version_cll="1.2.8"
!  character(len=5) :: version_cll=" 1.2 "
  integer, parameter :: long_int = selected_int_kind(18)
  integer :: mode_cll, nminf_cll
  double precision :: muUV2_cll,muIR2_cll,DeltaUV_cll,DeltaIR1_cll,DeltaIR2_cll
  double complex, allocatable :: minf2_cll(:) 
  double precision :: dprec_cll, reqacc_cll, critacc_cll, checkacc_cll, sreqacc_cll
  integer :: nmax_DD,rmax_DD,rmax2_DD,rmax3_DD,rmax4_DD,rmax5_DD,rmax6_DD
  character(len=250) :: filename_errout_cll
  integer(kind=long_int) EventCnt_cll,ErrCnt_cll,InfCnt_cll,CritPointsCntCOLI_cll
  integer(kind=long_int) ErrCntCOLI_cll,ErrCntDD_cll
  integer :: ErrFlag_cll,MaxInfOut_cll,AccFlag_cll
  integer :: MaxErrOut_cll,MaxErrOutCOLI_cll,MaxErrOutDD_cll
  integer :: rmaxB_cll,rmaxC_cll,rmaxD_cll,ritmax_cll
  integer :: infoutlev_cll,erroutlev_cll
  integer :: stdout_cll=6
  integer :: ErrorStop_cll
  logical :: calcUV_cll,IR_rational_terms_cll
  integer :: tenred_cll,never_tenred_cll=999
  logical :: initialized_cll=.false.,nofiles_cll=.false.
  logical :: argperm_cll=.false.
  integer, parameter :: nminf_colidd=100, closed_cll=-999

  integer :: Nmax_cll, rmax_cll
  integer(kind=long_int) :: CritPointsCntA_cll,CritPointsCntB_cll,CritPointsCntC_cll,  &
      CritPointsCntD_cll,CritPointsCntE_cll,CritPointsCntF_cll,         &
      CritPointsCntG_cll,CritPointsCntDB_cll
  integer(kind=long_int) :: AccPointsCntA_cll,AccPointsCntB_cll,AccPointsCntC_cll,  &
      AccPointsCntD_cll,AccPointsCntE_cll,AccPointsCntF_cll,         &
      AccPointsCntG_cll,AccPointsCntDB_cll
  integer(kind=long_int) :: sAccPointsCntA_cll,sAccPointsCntB_cll,sAccPointsCntC_cll,  &
      sAccPointsCntD_cll,sAccPointsCntE_cll,sAccPointsCntF_cll,         &
      sAccPointsCntG_cll,sAccPointsCntDB_cll
  integer(kind=long_int) :: PointsCntA_cll,PointsCntB_cll,PointsCntC_cll,  &
      PointsCntD_cll,PointsCntE_cll,PointsCntF_cll,PointsCntG_cll,PointsCntDB_cll
  integer(kind=long_int) :: CritPointsCntAten_cll,CritPointsCntBten_cll,CritPointsCntCten_cll,  &
      CritPointsCntDten_cll,CritPointsCntEten_cll,CritPointsCntFten_cll,         &
      CritPointsCntGten_cll,CritPointsCntDBten_cll
  integer(kind=long_int) :: AccPointsCntAten_cll,AccPointsCntBten_cll,AccPointsCntCten_cll,  &
      AccPointsCntDten_cll,AccPointsCntEten_cll,AccPointsCntFten_cll,         &
      AccPointsCntGten_cll,AccPointsCntDBten_cll
  integer(kind=long_int) :: sAccPointsCntAten_cll,sAccPointsCntBten_cll,sAccPointsCntCten_cll,  &
      sAccPointsCntDten_cll,sAccPointsCntEten_cll,sAccPointsCntFten_cll,         &
      sAccPointsCntGten_cll,sAccPointsCntDBten_cll
  integer(kind=long_int) :: PointsCntAten_cll,PointsCntBten_cll,PointsCntCten_cll,  &
      PointsCntDten_cll,PointsCntEten_cll,PointsCntFten_cll,PointsCntGten_cll,PointsCntDBten_cll
  integer(kind=long_int), allocatable :: PointsCntTN_cll(:),CritPointsCntTN_cll(:),AccPointsCntTN_cll(:),sAccPointsCntTN_cll(:)
  integer(kind=long_int), allocatable :: PointsCntTNten_cll(:),CritPointsCntTNten_cll(:),  &
      AccPointsCntTNten_cll(:),sAccPointsCntTNten_cll(:)
!  integer(kind=long_int) :: PointsCntTN_cll(10),CritPointsCntTN_cll(10),AccPointsCntTN_cll(10)
  logical :: Monitoring,qopened_check

  integer(kind=long_int) :: CritPointsCntA2_cll,CritPointsCntB2_cll,CritPointsCntC2_cll,  &
      CritPointsCntD2_cll,CritPointsCntE2_cll,CritPointsCntF2_cll,         &
      CritPointsCntG2_cll,CritPointsCntDB2_cll
  integer(kind=long_int) :: AccPointsCntA2_cll,AccPointsCntB2_cll,AccPointsCntC2_cll,  &
      AccPointsCntD2_cll,AccPointsCntE2_cll,AccPointsCntF2_cll,         &
      AccPointsCntG2_cll,AccPointsCntDB2_cll
  integer(kind=long_int) :: sAccPointsCntA2_cll,sAccPointsCntB2_cll,sAccPointsCntC2_cll,  &
      sAccPointsCntD2_cll,sAccPointsCntE2_cll,sAccPointsCntF2_cll,         &
      sAccPointsCntG2_cll,sAccPointsCntDB2_cll
  integer(kind=long_int) :: PointsCntA2_cll,PointsCntB2_cll,PointsCntC2_cll,  &
      PointsCntD2_cll,PointsCntE2_cll,PointsCntF2_cll,PointsCntG2_cll,PointsCntDB2_cll
  integer(kind=long_int), allocatable :: PointsCntTN2_cll(:),CritPointsCntTN2_cll(:),AccPointsCntTN2_cll(:),sAccPointsCntTN2_cll(:)
  
  integer(kind=long_int) :: PointsCntA_coli,PointsCntB_coli,PointsCntDB_coli,PointsCntC_coli,  &
      PointsCntD_coli,PointsCntE_coli,PointsCntF_coli,PointsCntG_coli
  integer(kind=long_int), allocatable :: PointsCntTN_coli(:)
  integer(kind=long_int) :: PointsCntAten_coli,PointsCntBten_coli,PointsCntDBten_coli,PointsCntCten_coli,  &
      PointsCntDten_coli,PointsCntEten_coli,PointsCntFten_coli,PointsCntGten_coli
  integer(kind=long_int), allocatable :: PointsCntTNten_coli(:)
  integer(kind=long_int) :: PointsCntA_dd,PointsCntB_dd,PointsCntDB_dd,PointsCntC_dd,  &
      PointsCntD_dd,PointsCntE_dd,PointsCntF_dd,PointsCntG_dd
  integer(kind=long_int), allocatable :: PointsCntTN_dd(:)
  integer(kind=long_int) :: PointsCntAten_dd,PointsCntBten_dd,PointsCntDBten_dd,PointsCntCten_dd,  &
      PointsCntDten_dd,PointsCntEten_dd,PointsCntFten_dd,PointsCntGten_dd
  integer(kind=long_int), allocatable :: PointsCntTNten_dd(:)

  integer, allocatable :: MaxCheck_cll(:)
  integer(kind=long_int), allocatable :: CheckCntten_cll(:),DiffCntten_cll(:),CheckCnt_cll(:),DiffCnt_cll(:)
  integer :: MaxCheckDB_cll,CheckCntDB_cll,DiffCntDB_cll
  integer :: MaxCheckEc_cll,DiffCntEc_cll

  integer(kind=long_int) :: AccCnt(-2:1), ErrCnt(-10:1), ErrCntcoli(-10:0), ErrCntdd(-10:0)
  integer(kind=long_int) :: AccEventCnt(-2:1), ErrEventCnt(-10:1)

  integer, allocatable :: noutCritPointsMax_cll(:)
  integer :: noutCritPointsMaxDB_cll

  
  integer :: nerrout_cll,nerroutcoli_cll,nerroutdd_cll 
  integer :: ninfout_cll,ninfoutcoli_cll,ncheckout_cll 
  integer :: ncpoutcoli_cll,nstatsoutcoli_cll,ncpout_cll,ncpout2_cll
  character(len=250) :: fname_errout_cll,fname_erroutcoli_cll
  character(len=250) :: fname_erroutdd_cll,fname_cpoutcoli_cll
  character(len=250) :: fname_cpout_cll,fname_cpout2_cll
  character(len=250) :: fname_checkout_cll,fname_infout_cll
  character(len=250) :: fname_infoutcoli_cll,fname_statsoutcoli_cll
  integer :: nerrout_cp_cll,nerroutcoli_cp_cll,nerroutdd_cp_cll 
  integer :: ninfout_cp_cll,ninfoutcoli_cp_cll,ncheckout_cp_cll 
  integer :: ncpoutcoli_cp_cll,nstatsoutcoli_cp_cll,ncpout_cp_cll,ncpout2_cp_cll
  character(len=250) :: fname_errout_cp_cll,fname_erroutcoli_cp_cll
  character(len=250) :: fname_erroutdd_cp_cll,fname_cpoutcoli_cp_cll
  character(len=250) :: fname_cpout_cp_cll,fname_cpout2_cp_cll
  character(len=250) :: fname_checkout_cp_cll,fname_infout_cp_cll
  character(len=250) :: fname_infoutcoli_cp_cll,fname_statsoutcoli_cp_cll
  
end module collier_global
