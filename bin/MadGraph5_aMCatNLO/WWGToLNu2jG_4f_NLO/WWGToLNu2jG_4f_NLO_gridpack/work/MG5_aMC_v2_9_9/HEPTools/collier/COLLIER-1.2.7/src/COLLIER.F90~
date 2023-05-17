!!
!!  File COLLIER.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!       *******************************************
!       *              C O L L I E R              * 
!       *                                         *
!       *        Complex One-Loop Library         *        
!       *      In Extended Regularizations        *
!       *                                         *
!       *    by A.Denner, S.Dittmaier, L.Hofer    *
!       *                                         *
!       *               version 1.2.5             *
!       *                                         *    
!       *******************************************
! 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




module COLLIER

  use collier_init, only : &
      GetVersionNumber_cll, &
      Init_cll, InitEvent_cll, &
      GetNc_cll, GetNt_cll, & 
      SetDeltaUV_cll, SetMuUV2_cll, GetDeltaUV_cll, GetMuUV2_cll, &
      SetDeltaIR_cll, SetMuIR2_cll, GetDeltaIR_cll, GetMuIR2_cll, &
      SetMinf2_cll, AddMinf2_cll, GetNminf_cll, GetMinf2_cll, ClearMinf2_cll, &
      SetMode_cll, GetMode_cll, &
      SetReqAcc_cll, GetReqAcc_cll, SetCritAcc_cll, GetCritAcc_cll, &
      SetCheckAcc_cll, GetCheckAcc_cll, SetAccuracy_cll, &
      SetRitmax_cll, GetRitmax_cll, &
      SetTenRed_cll, GetTenRed_cll, SwitchOnTenRed_cll, SwitchOffTenRed_cll, &
      GetErrFlag_cll, InitErrFlag_cll, SetErrStop_cll, &
      GetErrFlag_coli, &
      GetAccFlag_cll, InitAccFlag_cll, &
      SwitchOffFileOutput_cll, SwitchOnFileOutput_cll, &
      SetOutputFolder_cll, GetOutputFolder_cll, &
      SetnerroutCOLI_cll, SetnerroutDD_cll, Setnerrout_cll, &
      GetnerroutCOLI_cll, GetnerroutDD_cll, Getnerrout_cll, &
      SetMaxErrOutCOLI_cll, SetMaxErrOutDD_cll, SetMaxErrOut_cll, &
      InitErrCntCOLI_cll, InitErrCntDD_cll, InitErrCnt_cll, SetErrOutLev_cll, &
      Setninfout_cll, Getninfout_cll, SetMaxInfOut_cll, SetInfOutLev_cll, &
      Setncheckout_cll, Getncheckout_cll, SetMaxCheck_cll, SetMaxCheckDB_cll, &
      InitCheckCnt_cll, InitCheckCntDB_cll, InitMonitoring_cll, &
      Setncritpointsout_cll, Getncritpointsout_cll, SetMaxCritPoints_cll, SetMaxCritPointsDB_cll, &
      InitCheckCnt_cll, InitCheckCntDB_cll, &
      SwitchOffErrStop_cll, PrintStatisticscoli_cll

  use collier_coefs, only : &
      A_cll, B_cll, C_cll, D_cll, E_cll, F_cll, G_cll, TN_cll, &
      A0_cll, B0_cll, C0_cll, D0_cll, &
      DB_cll, DB0_cll, DB1_cll, DB00_cll, DB11_cll
      
  use collier_tensors, only : &
      Aten_cll, Bten_cll, Cten_cll, Dten_cll, Eten_cll, Ften_cll, Gten_cll, TNten_cll
      
  use collier_aux, only : &
      PrintStatistics_cll, PrintStatistics2_cll

  use cache, only : &
      SetCacheMode_cll, InitCacheSystem_cll, SwitchOnCacheSystem_cll, SwitchOffCacheSystem_cll, &
      SwitchOnCache_cll, SwitchOffCache_cll, SetCacheLevel_cll, AddNewCache_cll, SetNopt_cll

end module COLLIER
