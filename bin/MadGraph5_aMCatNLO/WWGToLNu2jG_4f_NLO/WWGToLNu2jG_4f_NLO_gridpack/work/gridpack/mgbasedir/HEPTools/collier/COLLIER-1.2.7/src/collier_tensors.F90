!!
!!  File collier_tensors.F90 is part of COLLIER
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


module collier_tensors

  use combinatorics
  use collier_global
  use collier_init
  use collier_aux
  use collier_coefs
  use BuildTensors
  use TensorReduction
  use cache
!  use coli_statistics

  implicit none



  interface Aten_cll
    module procedure Aten_main_cll,Aten_list_cll, &
                     Aten_args_cll,Aten_args_list_cll
  end interface Aten_cll


  interface Bten_cll
    module procedure Bten_main_cll,Bten_list_cll, &
                     Bten_args_cll,Bten_args_list_cll
  end interface Bten_cll


  interface Cten_cll
    module procedure Cten_main_cll,Cten_list_cll, &
                     Cten_args_cll,Cten_args_list_cll
  end interface Cten_cll


  interface Dten_cll
    module procedure Dten_main_cll,Dten_list_cll, &
                     Dten_args_cll,Dten_args_list_cll
  end interface Dten_cll


  interface Eten_cll
    module procedure Eten_main_cll,Eten_list_cll, &
                     Eten_args_cll,Eten_args_list_cll
  end interface Eten_cll


  interface Ften_cll
    module procedure Ften_main_cll,Ften_list_cll, &
                     Ften_args_cll,Ften_args_list_cll
  end interface Ften_cll


  interface Gten_cll
    module procedure Gten_main_cll,Gten_list_cll, &
                     Gten_args_cll,Gten_args_list_cll
  end interface Gten_cll


  interface TNten_cll
    module procedure TNten_main_cll,TNten_list_cll, &
                     T1ten_main_cll,T1ten_list_cll
  end interface TNten_cll





contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Aten_cll(TA,TAuv,masses2,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Aten_main_cll(TA,TAuv,masses2,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TAuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(0:rmax,0:rmax,0:rmax,0:rmax), TAuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax),TAerr_aux(0:rmax),TAerr_aux2(0:rmax)
    double complex :: args(1)    
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,n0,n1,n2,n3
    logical :: eflag
    
    if (1.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','Nmax_cll smaller 1',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 1'
      call PropagateErrFlag_cll
      return
    end if
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    args(1) = masses2(0)
    call SetMasterFname_cll('Aten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call CalcTensorA(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TA(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TA2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenA_cll(TA,TA2,masses2,norm,rmax,TAdiff)
      
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntAten_coli =  PointsCntAten_coli + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do         
        if (Monitoring) PointsCntAten_dd =  PointsCntAten_dd + 1
      end if       
    
    else
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TA(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntAten_cll =  PointsCntAten_cll + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntAten_cll = AccPointsCntAten_cll + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntAten_cll = sAccPointsCntAten_cll + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntAten_cll =  CritPointsCntAten_cll + 1
        if ( CritPointsCntAten_cll.le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Aten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Aten_cll(TA,TAuv,masses2,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Aten_list_cll(TA,TAuv,masses2,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(:),TAuv(:)
    double precision, intent(out), optional :: TAerr(0:rmax)
    integer :: r,i
    logical :: eflag

    if (1.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','Nmax_cll smaller 1',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 1'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Aten_list_checked_cll(TA,TAuv,masses2,rmax,TAerr)

  end subroutine Aten_list_cll


  subroutine Aten_list_checked_cll(TA,TAuv,masses2,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(RtS(rmax)),TAuv(RtS(rmax))
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(RtS(rmax)),TAuv2(RtS(rmax))    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax), TAerr_aux(0:rmax), TAerr_aux2(0:rmax)
    double complex :: args(1)
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,i

    args(1) = masses2(0)
    call SetMasterFname_cll('Aten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call CalcTensorA_list(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TA(i)))
          norm_dd = max(norm_dd,abs(TA2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenAList_cll(TA,TA2,masses2,norm,rmax,TAdiff)
      
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntAten_coli =  PointsCntAten_coli + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do         
        if (Monitoring) PointsCntAten_dd =  PointsCntAten_dd + 1
      end if       
    
    else
      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)    
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TA(i)))
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntAten_cll =  PointsCntAten_cll + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntAten_cll = AccPointsCntAten_cll + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntAten_cll = sAccPointsCntAten_cll + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntAten_cll =  CritPointsCntAten_cll + 1
        if ( CritPointsCntAten_cll.le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Aten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Aten_cll(TA,TAuv,m02,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Aten_args_cll(TA,TAuv,m02,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: m02
    double complex, intent(out) :: TA(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TAuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(0:rmax,0:rmax,0:rmax,0:rmax), TAuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax),TAerr_aux(0:rmax),TAerr_aux2(0:rmax)
    double complex :: args(1),masses2(0:0)
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,n0,n1,n2,n3
    logical :: eflag
    
    if (1.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','Nmax_cll smaller 1',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 1'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    args(1) = m02
    masses2(0) = m02
    call SetMasterFname_cll('Aten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)
      call CalcTensorA(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TA(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TA2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenA_cll(TA,TA2,masses2,norm,rmax,TAdiff)
       
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntAten_coli =  PointsCntAten_coli + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntAten_dd =  PointsCntAten_dd + 1         
      end if      
    
    else
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TA(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntAten_cll =  PointsCntAten_cll + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntAten_cll = AccPointsCntAten_cll + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntAten_cll = sAccPointsCntAten_cll + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntAten_cll =  CritPointsCntAten_cll + 1
        if ( CritPointsCntAten_cll.le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Aten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Aten_cll(TA,TAuv,m02,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Aten_args_list_cll(TA,TAuv,m02,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: m02
    double complex, intent(out) :: TA(:),TAuv(:)
    double precision, intent(out), optional :: TAerr(0:)
    integer :: r,i    
    logical :: eflag

    if (1.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','Nmax_cll smaller 1',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 1'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Aten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Aten_args_list_checked_cll(TA,TAuv,m02,rmax,TAerr)

  end subroutine Aten_args_list_cll


  subroutine Aten_args_list_checked_cll(TA,TAuv,m02,rmax,TAerr)

    integer, intent(in) :: rmax
    double complex,intent(in) :: m02
    double complex, intent(out) :: TA(RtS(rmax)),TAuv(RtS(rmax))
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(RtS(rmax)),TAuv2(RtS(rmax))    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax), TAerr_aux(0:rmax), TAerr_aux2(0:rmax)
    double complex :: args(1),masses2(0:0)
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    args(1) = m02
    masses2(0) = m02
    call SetMasterFname_cll('Aten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)
      call CalcTensorA_list(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TA(i)))
          norm_dd = max(norm_dd,abs(TA2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenAList_cll(TA,TA2,masses2,norm,rmax,TAdiff)
      
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntAten_coli =  PointsCntAten_coli + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do         
        if (Monitoring) PointsCntAten_dd =  PointsCntAten_dd + 1
      end if      
    
    else
      call A_cll(CA,CAuv,m02,rmax,CAerr,0)    
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TA(i)))
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntAten_cll =  PointsCntAten_cll + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntAten_cll = AccPointsCntAten_cll + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntAten_cll = sAccPointsCntAten_cll + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntAten_cll =  CritPointsCntAten_cll + 1
        if ( CritPointsCntAten_cll.le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TAten_cll',0,maxval(TAacc),CritPointsCntAten_cll)
          if( CritPointsCntAten_cll.eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TAten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Aten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Bten_main_cll(TB.TBuv,mom,MomInv,masses2,rmax,TBerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Bten_main_cll(TB,TBuv,MomVec,MomInv,masses2,rmax,TBerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,1), MomInv(1), masses2(0:1)
    double complex, intent(out) :: TB(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TBuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TBerr(0:rmax)
    double complex :: TB2(0:rmax,0:rmax,0:rmax,0:rmax), TBuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CB(0:rmax/2,0:rmax), CBuv(0:rmax/2,0:rmax)
    double precision :: CBerr(0:rmax), TBerr_aux(0:rmax), TBerr_aux2(0:rmax)
    double complex :: args(7)
    double precision :: TBdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TBacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
 
    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5) = MomInv(1)
    args(6:7) = masses2(0:)
    call SetMasterFname_cll('Bten_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(7,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)
      call CalcTensorB(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,MomVec(0:,1),rmax)      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)
      call CalcTensorB(TB2,TBuv2,TBerr_aux2,CB,CBuv,CBerr,MomVec(0:,1),rmax)     
            
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TB(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TB2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(real(r)/2)
          else
            norm_coli=muir2_cll**(real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(real(r)/2)
          else
            norm_dd=muir2_cll**(real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TB,TB2,MomVec,MomInv,masses2,norm,2,rmax,TBdiff)
      
      if (TBerr_aux(rmax).lt.TBerr_aux2(rmax)) then
        if (present(TBerr))  TBerr = max(TBerr_aux,TBdiff*norm)
        do r=0,rmax
          TBacc(r) = max(TBerr_aux(r)/norm(r),TBdiff(r))
        end do
        if (Monitoring) PointsCntBten_coli =  PointsCntBten_coli + 1
      else
        TB = TB2
        TBuv = TBuv2        
        if (present(TBerr))  TBerr = max(TBerr_aux2,TBdiff*norm)    
        do r=0,rmax
          TBacc(r) = max(TBerr_aux2(r)/norm(r),TBdiff(r))
        end do         
        if (Monitoring) PointsCntBten_dd =  PointsCntBten_dd + 1
      end if      
      
    else
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)
      call CalcTensorB(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,MomVec(0:,1),rmax)
      if (present(TBerr))  TBerr = TBerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TB(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(real(r)/2)
          else
            norm(r)=muir2_cll**(real(r)/2)
          end if
        end if
        TBacc(r) = TBerr_aux(r)/norm(r)
      end do       
      
    end if

    call PropagateAccFlag_cll(TBacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntBten_cll =  PointsCntBten_cll + 1

      if(maxval(TBacc).gt.reqacc_cll) AccPointsCntBten_cll = AccPointsCntBten_cll + 1
      if(maxval(TBacc).gt.sreqacc_cll) sAccPointsCntBten_cll = sAccPointsCntBten_cll + 1

      if(maxval(TBacc).gt.critacc_cll) then
        CritPointsCntBten_cll =  CritPointsCntBten_cll + 1
        if ( CritPointsCntBten_cll.le.noutCritPointsMax_cll(2) ) then
          call CritPointsOut_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Bten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Bten_list_cll(TB.TBuv,mom,MomInv,masses2,rmax,TBerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Bten_list_cll(TB,TBuv,MomVec,MomInv,masses2,rmax,TBerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,1), MomInv(1), masses2(0:1)
    double complex, intent(out) :: TB(:), TBuv(:)
    double precision, intent(out), optional :: TBerr(0:rmax)
    integer :: r,i    
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Bten_list_checked_cll(TB,TBuv,MomVec,MomInv,masses2,rmax,TBerr)

  end subroutine Bten_list_cll


  subroutine Bten_list_checked_cll(TB,TBuv,MomVec,MomInv,masses2,rmax,TBerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,1), MomInv(1), masses2(0:1)
    double complex, intent(out) :: TB(RtS(rmax)), TBuv(RtS(rmax))
    double precision, intent(out), optional :: TBerr(0:rmax)
    double complex :: TB2(RtS(rmax)), TBuv2(RtS(rmax))    
    double complex :: CB(0:rmax/2,0:rmax), CBuv(0:rmax/2,0:rmax)
    double precision :: CBerr(0:rmax), TBerr_aux(0:rmax), TBerr_aux2(0:rmax) 
    double complex :: args(7)
    double precision :: TBdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TBacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5) = MomInv(1)
    args(6:7) = masses2(0:)
    call SetMasterFname_cll('Bten_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(7,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)
      call CalcTensorB_list(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,MomVec(0:,1),rmax)      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)
      call CalcTensorB_list(TB2,TBuv2,TBerr_aux2,CB,CBuv,CBerr,MomVec(0:,1),rmax)     
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TB(i)))
          norm_dd = max(norm_dd,abs(TB2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(real(r)/2)
          else
            norm_coli=muir2_cll**(real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(real(r)/2)
          else
            norm_dd=muir2_cll**(real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TB,TB2,MomVec,MomInv,masses2,norm,2,rmax,TBdiff)
      
      if (TBerr_aux(rmax).lt.TBerr_aux2(rmax)) then
        if (present(TBerr))  TBerr = max(TBerr_aux,TBdiff*norm)
        do r=0,rmax
          TBacc(r) = max(TBerr_aux(r)/norm(r),TBdiff(r))
        end do
        if (Monitoring) PointsCntBten_coli =  PointsCntBten_coli + 1
      else
        TB = TB2
        TBuv = TBuv2        
        if (present(TBerr))  TBerr = max(TBerr_aux2,TBdiff*norm)    
        do r=0,rmax
          TBacc(r) = max(TBerr_aux2(r)/norm(r),TBdiff(r))
        end do         
        if (Monitoring) PointsCntBten_dd =  PointsCntBten_dd + 1
      end if      
    
    else
      call B_main_cll(CB,CBuv,MomInv(1),masses2(0),masses2(1),rmax,CBerr,0)    
      call CalcTensorB_list(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,MomVec(0:,1),rmax)
      if (present(TBerr))  TBerr = TBerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TB(i)))      
        end do             
        if (norm(r).eq.0d0) then
          norm(r) = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(real(r)/2)
          else
            norm(r)=muir2_cll**(real(r)/2)
          end if
        end if
        TBacc(r) = TBerr_aux(r)/norm(r)
      end do     
      
    end if

    call PropagateAccFlag_cll(TBacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntBten_cll =  PointsCntBten_cll + 1

      if(maxval(TBacc).gt.reqacc_cll) AccPointsCntBten_cll = AccPointsCntBten_cll + 1
      if(maxval(TBacc).gt.sreqacc_cll) sAccPointsCntBten_cll = sAccPointsCntBten_cll + 1

      if(maxval(TBacc).gt.critacc_cll) then
        CritPointsCntBten_cll =  CritPointsCntBten_cll + 1
        if ( CritPointsCntBten_cll.le.noutCritPointsMax_cll(2) ) then
          call CritPointsOut_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Bten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Bten_args_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Bten_args_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3)
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(out) :: TB(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TBuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TBerr(0:rmax)
    double complex :: TB2(0:rmax,0:rmax,0:rmax,0:rmax), TBuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: masses2(0:1),MomInv(1)
    double complex :: CB(0:rmax/2,0:rmax), CBuv(0:rmax/2,0:rmax)
    double precision :: CBerr(0:rmax),TBerr_aux(0:rmax),TBerr_aux2(0:rmax)
    double complex :: args(7)
    double precision :: TBdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TBacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    masses2(0) = m02
    masses2(1) = m12
    MomInv(1) = p10

    ! set ID of master call
    args(1:4) = p1Vec(0:)
    args(5) = p10
    args(6:7) = masses2(0:)
    call SetMasterFname_cll('Bten_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(7,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,p1vec,rmax)      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB(TB2,TBuv2,TBerr_aux2,CB,CBuv,CBerr,p1vec,rmax)     
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TB(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TB2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(real(r)/2)
          else
            norm_coli=muir2_cll**(real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(real(r)/2)
          else
            norm_dd=muir2_cll**(real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TB,TB2,p1vec,MomInv,masses2,norm,2,rmax,TBdiff)
      
      if (TBerr_aux(rmax).lt.TBerr_aux2(rmax)) then
        if (present(TBerr))  TBerr = max(TBerr_aux,TBdiff*norm)
        do r=0,rmax
          TBacc(r) = max(TBerr_aux(r)/norm(r),TBdiff(r))
        end do
        if (Monitoring) PointsCntBten_coli =  PointsCntBten_coli + 1
      else
        TB = TB2
        TBuv = TBuv2        
        if (present(TBerr))  TBerr = max(TBerr_aux2,TBdiff*norm)    
        do r=0,rmax
          TBacc(r) = max(TBerr_aux2(r)/norm(r),TBdiff(r))
        end do         
        if (Monitoring) PointsCntBten_dd =  PointsCntBten_dd + 1
      end if
      
    else
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,p1vec,rmax)
      if (present(TBerr))  TBerr = TBerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TB(n0,n1,n2,n3)))
            end do
          end do
        end do       
        if (norm(r).eq.0d0) then
          norm(r) = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(real(r)/2)
          else
            norm(r)=muir2_cll**(real(r)/2)
          end if
        end if
        TBacc(r) = TBerr_aux(r)/norm(r)
      end do     
      
    end if

    call PropagateAccFlag_cll(TBacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntBten_cll =  PointsCntBten_cll + 1

      if(maxval(TBacc).gt.reqacc_cll) AccPointsCntBten_cll = AccPointsCntBten_cll + 1
      if(maxval(TBacc).gt.sreqacc_cll) sAccPointsCntBten_cll = sAccPointsCntBten_cll + 1

      if(maxval(TBacc).gt.critacc_cll) then
        CritPointsCntBten_cll =  CritPointsCntBten_cll + 1
        if ( CritPointsCntBten_cll.le.noutCritPointsMax_cll(2) ) then
          call CritPointsOut_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Bten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Bten_args_list_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Bten_args_list_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3)
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(out) :: TB(:), TBuv(:)
    double precision, intent(out), optional :: TBerr(0:rmax)
    integer :: r,i    
    logical :: eflag

    if (2.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','Nmax_cll smaller 2',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 2'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Bten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Bten_args_list_checked_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  
  end subroutine Bten_args_list_cll
  

  subroutine Bten_args_list_checked_cll(TB,TBuv,p1vec,p10,m02,m12,rmax,TBerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3)
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(out) :: TB(RtS(rmax)), TBuv(RtS(rmax))
    double precision, intent(out), optional :: TBerr(0:rmax)
    double complex :: TB2(RtS(rmax)), TBuv2(RtS(rmax))    
    double complex :: masses2(0:1),MomInv(1)
    double complex :: CB(0:rmax/2,0:rmax), CBuv(0:rmax/2,0:rmax)
    double precision :: CBerr(0:rmax), TBerr_aux(0:rmax), TBerr_aux2(0:rmax)
    double complex :: args(7)
    double precision :: TBdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TBacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    masses2(0) = m02
    masses2(1) = m12
    MomInv(1) = p10

    ! set ID of master call
    args(1:4) = p1vec(0:)
    args(5) = p10
    args(6:7) = masses2(0:)
    call SetMasterFname_cll('Bten_cll')
    call SetMasterN_cll(2)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(7,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB_list(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,p1vec,rmax)      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB_list(TB2,TBuv2,TBerr_aux2,CB,CBuv,CBerr,p1vec,rmax)     
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TB(i)))
          norm_dd = max(norm_dd,abs(TB2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(real(r)/2)
          else
            norm_coli=muir2_cll**(real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(real(r)/2)
          else
            norm_dd=muir2_cll**(real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TB,TB2,p1vec,MomInv,masses2,norm,2,rmax,TBdiff)      
      
      if (TBerr_aux(rmax).lt.TBerr_aux2(rmax)) then
        if (present(TBerr))  TBerr = max(TBerr_aux,TBdiff*norm)
        do r=0,rmax
          TBacc(r) = max(TBerr_aux(r)/norm(r),TBdiff(r))
        end do
        if (Monitoring) PointsCntBten_coli =  PointsCntBten_coli + 1
      else
        TB = TB2
        TBuv = TBuv2        
        if (present(TBerr))  TBerr = max(TBerr_aux2,TBdiff*norm)    
        do r=0,rmax
          TBacc(r) = max(TBerr_aux2(r)/norm(r),TBdiff(r))
        end do         
        if (Monitoring) PointsCntBten_dd =  PointsCntBten_dd + 1
      end if
      
    else
      call B_main_cll(CB,CBuv,p10,m02,m12,rmax,CBerr,0)
      call CalcTensorB_list(TB,TBuv,TBerr_aux,CB,CBuv,CBerr,p1vec,rmax)
      if (present(TBerr))  TBerr = TBerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TB(i)))      
        end do            
        if (norm(r).eq.0d0) then
          norm(r) = max(abs(MomInv(1)),maxval(abs(masses2(0:1))))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(real(r)/2)
          else
            norm(r)=muir2_cll**(real(r)/2)
          end if
        end if
        TBacc(r) = TBerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TBacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntBten_cll =  PointsCntBten_cll + 1

      if(maxval(TBacc).gt.reqacc_cll) AccPointsCntBten_cll = AccPointsCntBten_cll + 1
      if(maxval(TBacc).gt.sreqacc_cll) sAccPointsCntBten_cll = sAccPointsCntBten_cll + 1

      if(maxval(TBacc).gt.critacc_cll) then
        CritPointsCntBten_cll =  CritPointsCntBten_cll + 1
        if ( CritPointsCntBten_cll.le.noutCritPointsMax_cll(2) ) then
          call CritPointsOut_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TBten_cll',0,maxval(TBacc),CritPointsCntBten_cll)
          if( CritPointsCntBten_cll.eq.noutCritPointsMax_cll(2)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TBten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Bten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Cten_main_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine Cten_main_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,2), MomInv(3), masses2(0:2)
    double complex, intent(out) :: TC(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TCuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TCerr(0:rmax)
    double complex :: TC2(0:rmax,0:rmax,0:rmax,0:rmax), TCuv2(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CC(0:rmax/2,0:rmax,0:rmax), CCuv(0:rmax/2,0:rmax,0:rmax)
    double precision :: CCerr(0:rmax), TCerr_aux(0:rmax), TCerr_aux2(0:rmax), TCacc(0:rmax)
    double complex args(14)
    double precision :: TCdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:11) = MomInv
    args(12:14) = masses2(0:)
    call SetMasterFname_cll('Cten_cll')
    call SetMasterN_cll(3)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(14,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC(TC2,TCuv2,TCerr_aux2,CC,CCuv,CCerr,MomVec,rmax)
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TC(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TC2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(1-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(1-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TC,TC2,MomVec,MomInv,masses2,norm,3,rmax,TCdiff)
      
      if (TCerr_aux(rmax).lt.TCerr_aux2(rmax)) then
        if (present(TCerr))  TCerr = max(TCerr_aux,TCdiff*norm)
        do r=0,rmax
          TCacc(r) = max(TCerr_aux(r)/norm(r),TCdiff(r))
        end do
        if (Monitoring) PointsCntCten_coli =  PointsCntCten_coli + 1
      else
        TC = TC2
        TCuv = TCuv2        
        if (present(TCerr))  TCerr = max(TCerr_aux2,TCdiff*norm)    
        do r=0,rmax
          TCacc(r) = max(TCerr_aux2(r)/norm(r),TCdiff(r))
        end do         
        if (Monitoring) PointsCntCten_dd =  PointsCntCten_dd + 1
      end if      
      
    else
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      if (present(TCerr)) TCerr = TCerr_aux
      norm=0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TC(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(1-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        TCacc(r) = TCerr_aux(r)/norm(r)
      end do 
      
    end if

    call PropagateAccFlag_cll(TCacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntCten_cll =  PointsCntCten_cll + 1

      if(maxval(TCacc).gt.reqacc_cll) AccPointsCntCten_cll = AccPointsCntCten_cll + 1
      if(maxval(TCacc).gt.sreqacc_cll) sAccPointsCntCten_cll = sAccPointsCntCten_cll + 1

      if(maxval(TCacc).gt.critacc_cll) then
        CritPointsCntCten_cll =  CritPointsCntCten_cll + 1
        if ( CritPointsCntCten_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Cten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Cten_list_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine Cten_list_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,2), MomInv(3), masses2(0:2)
    double complex, intent(out) :: TC(:), TCuv(:)
    double precision, intent(out), optional :: TCerr(0:rmax)
    logical :: eflag

    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Cten_list_checked_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)

  end subroutine Cten_list_cll


  subroutine Cten_list_checked_cll(TC,TCuv,MomVec,MomInv,masses2,rmax,TCerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,2), MomInv(3), masses2(0:2)
    double complex, intent(out) :: TC(RtS(rmax)), TCuv(RtS(rmax))
    double precision, intent(out), optional :: TCerr(0:rmax)
    double complex :: TC2(RtS(rmax)), TCuv2(RtS(rmax))    
    double complex :: CC(0:rmax/2,0:rmax,0:rmax), CCuv(0:rmax/2,0:rmax,0:rmax)
    double precision :: CCerr(0:rmax), TCerr_aux(0:rmax), TCerr_aux2(0:rmax)
    double complex :: args(14)
    double precision :: TCdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TCacc(0:rmax)
    integer :: r,i
    logical :: eflag

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:11) = MomInv
    args(12:14) = masses2(0:)
    call SetMasterFname_cll('Cten_cll')
    call SetMasterN_cll(3)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(14,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC_list(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC_list(TC2,TCuv2,TCerr_aux2,CC,CCuv,CCerr,MomVec,rmax)
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TC(i)))
          norm_dd = max(norm_dd,abs(TC2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(1-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(1-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TC,TC2,MomVec,MomInv,masses2,norm,3,rmax,TCdiff)      
      
      if (TCerr_aux(rmax).lt.TCerr_aux2(rmax)) then
        if (present(TCerr))  TCerr = max(TCerr_aux,TCdiff*norm)
        do r=0,rmax
          TCacc(r) = max(TCerr_aux(r)/norm(r),TCdiff(r))
        end do
        if (Monitoring) PointsCntCten_coli =  PointsCntCten_coli + 1
      else
        TC = TC2
        TCuv = TCuv2        
        if (present(TCerr))  TCerr = max(TCerr_aux2,TCdiff*norm)    
        do r=0,rmax
          TCacc(r) = max(TCerr_aux2(r)/norm(r),TCdiff(r))
        end do         
        if (Monitoring) PointsCntCten_dd =  PointsCntCten_dd + 1
      end if
      
    else 
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)
      call CalcTensorC_list(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      if (present(TCerr)) TCerr = TCerr_aux
      norm=0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TC(i)))      
        end do             
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(1-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        TCacc(r) = TCerr_aux(r)/norm(r)
      end do       
      
    end if

    call PropagateAccFlag_cll(TCacc,rmax)
    call PropagateErrFlag_cll
    
    if (Monitoring) then
      PointsCntCten_cll =  PointsCntCten_cll + 1

      if(maxval(TCacc).gt.reqacc_cll) AccPointsCntCten_cll = AccPointsCntCten_cll + 1
      if(maxval(TCacc).gt.sreqacc_cll) sAccPointsCntCten_cll = sAccPointsCntCten_cll + 1

      if(maxval(TCacc).gt.critacc_cll) then
        CritPointsCntCten_cll =  CritPointsCntCten_cll + 1
        if ( CritPointsCntCten_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Cten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Cten_args_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine Cten_args_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3)
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double complex, intent(out) :: TC(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TCuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TCerr(0:rmax)
    double complex :: TC2(0:rmax,0:rmax,0:rmax,0:rmax), TCuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: MomVec(0:3,2), MomInv(3), masses2(0:2)
    double complex :: CC(0:rmax/2,0:rmax,0:rmax), CCuv(0:rmax/2,0:rmax,0:rmax)
    double precision :: CCerr(0:rmax), TCerr_aux(0:rmax), TCerr_aux2(0:rmax)
    double complex :: args(14)
    double precision :: TCdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TCacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p20
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:11) = MomInv
    args(12:14) = masses2(0:)
    call SetMasterFname_cll('Cten_cll')
    call SetMasterN_cll(3)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(14,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC(TC2,TCuv2,TCerr_aux2,CC,CCuv,CCerr,MomVec,rmax)
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TC(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TC2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(1-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(1-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
         norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TC,TC2,MomVec,MomInv,masses2,norm,3,rmax,TCdiff)
      
      if (TCerr_aux(rmax).lt.TCerr_aux2(rmax)) then
        if (present(TCerr))  TCerr = max(TCerr_aux,TCdiff*norm)
        do r=0,rmax
          TCacc(r) = max(TCerr_aux(r)/norm(r),TCdiff(r))
        end do
        if (Monitoring) PointsCntCten_coli =  PointsCntCten_coli + 1
      else
        TC = TC2
        TCuv = TCuv2        
        if (present(TCerr))  TCerr = max(TCerr_aux2,TCdiff*norm)    
        do r=0,rmax
          TCacc(r) = max(TCerr_aux2(r)/norm(r),TCdiff(r))
        end do         
        if (Monitoring) PointsCntCten_dd =  PointsCntCten_dd + 1
      end if
      
    else
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)
      call CalcTensorC(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      if (present(TCerr)) TCerr = TCerr_aux
      norm=0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TC(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(1-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        TCacc(r) = TCerr_aux(r)/norm(r)
      end do      
      
    end if  

    call PropagateAccFlag_cll(TCacc,rmax)
    call PropagateErrFlag_cll
    
    if (Monitoring) then
      PointsCntCten_cll =  PointsCntCten_cll + 1

      if(maxval(TCacc).gt.reqacc_cll) AccPointsCntCten_cll = AccPointsCntCten_cll + 1
      if(maxval(TCacc).gt.sreqacc_cll) sAccPointsCntCten_cll = sAccPointsCntCten_cll + 1

      if(maxval(TCacc).gt.critacc_cll) then
        CritPointsCntCten_cll =  CritPointsCntCten_cll + 1
        if ( CritPointsCntCten_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Cten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Cten_args_list_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine Cten_args_list_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3)
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double complex, intent(out) :: TC(:), TCuv(:)
    double precision, intent(out), optional :: TCerr(0:rmax)
    logical :: eflag

    if (3.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','Nmax_cll smaller 3',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 3'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Cten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call Cten_args_list_checked_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)

  end subroutine Cten_args_list_cll


  subroutine Cten_args_list_checked_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TCerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3)
    double complex, intent(in) :: p10,p21,p20,m02,m12,m22
    double complex, intent(out) :: TC(RtS(rmax)), TCuv(RtS(rmax))
    double precision, intent(out), optional :: TCerr(0:rmax)
    double complex :: TC2(RtS(rmax)), TCuv2(RtS(rmax))    
    double complex :: MomVec(0:3,2), MomInv(3), masses2(0:2)
    double complex :: CC(0:rmax/2,0:rmax,0:rmax), CCuv(0:rmax/2,0:rmax,0:rmax)
    double precision :: CCerr(0:rmax), TCerr_aux(0:rmax), TCerr_aux2(0:rmax)
    double complex :: args(14)
    double precision :: TCdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TCacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p20
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:11) = MomInv
    args(12:14) = masses2(0:)
    call SetMasterFname_cll('Cten_cll')
    call SetMasterN_cll(3)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(14,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC_list(TC,TCuv,TCerr_aux,CC,CCuv,CCerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC_list(TC2,TCuv2,TCerr_aux2,CC,CCuv,CCerr,MomVec,rmax)
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TC(i)))
          norm_dd = max(norm_dd,abs(TC2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(1-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(1-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TC,TC2,MomVec,MomInv,masses2,norm,3,rmax,TCdiff)      
      
      if (TCerr_aux(rmax).lt.TCerr_aux2(rmax)) then
        if (present(TCerr))  TCerr = max(TCerr_aux,TCdiff*norm)
        do r=0,rmax
          TCacc(r) = max(TCerr_aux(r)/norm(r),TCdiff(r))
        end do
        if (Monitoring) PointsCntCten_coli =  PointsCntCten_coli + 1
      else
        TC = TC2
        TCuv = TCuv2        
        if (present(TCerr))  TCerr = max(TCerr_aux2,TCdiff*norm)    
        do r=0,rmax
          TCacc(r) = max(TCerr_aux2(r)/norm(r),TCdiff(r))
        end do         
        if (Monitoring) PointsCntCten_dd =  PointsCntCten_dd + 1
      end if
      
    else
      call C_main_cll(CC,CCuv,MomInv(1),MomInv(2),MomInv(3), &
                      masses2(0),masses2(1),masses2(2),rmax,Cerr2=CCerr,id_in=0)    
      call CalcTensorC_list(TC,TCuv,TCerr,CC,CCuv,CCerr,MomVec,rmax)
      if (present(TCerr)) TCerr = TCerr_aux
      norm=0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TC(i)))      
        end do            
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:3))),maxval(abs(masses2(0:2))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(1-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(1-real(r)/2)
          end if
        end if
        TCacc(r) = TCerr_aux(r)/norm(r)
      end do       
      
    end if

    call PropagateAccFlag_cll(TCacc,rmax)
    call PropagateErrFlag_cll
    
    if (Monitoring) then
      PointsCntCten_cll =  PointsCntCten_cll + 1

      if(maxval(TCacc).gt.reqacc_cll) AccPointsCntCten_cll = AccPointsCntCten_cll + 1
      if(maxval(TCacc).gt.sreqacc_cll) sAccPointsCntCten_cll = sAccPointsCntCten_cll + 1

      if(maxval(TCacc).gt.critacc_cll) then
        CritPointsCntCten_cll =  CritPointsCntCten_cll + 1
        if ( CritPointsCntCten_cll.le.noutCritPointsMax_cll(3) ) then
          call CritPointsOut_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TCten_cll',0,maxval(TCacc),CritPointsCntCten_cll)
          if( CritPointsCntCten_cll.eq.noutCritPointsMax_cll(3)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TCten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Cten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Dten_main_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine Dten_main_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex, intent(out) :: TD(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TDuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TDerr(0:rmax)
    double complex :: CD(0:rmax/2,0:rmax,0:rmax,0:rmax) 
    double complex :: TD2(0:rmax,0:rmax,0:rmax,0:rmax), TDuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: CDerr(0:rmax), TDerr_aux(0:rmax), TDerr_aux2(0:rmax)
    double complex :: args(22)
    double precision :: TDdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TDacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:18) = MomInv
    args(19:22) = masses2(0:)
    call SetMasterFname_cll('Dten_cll')
    call SetMasterN_cll(4)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(22,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD(TD2,TDuv2,TDerr_aux2,CD,CDuv,CDerr,MomVec,rmax)      
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TD(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TD2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(2-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(2-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TD,TD2,MomVec,MomInv,masses2,norm,4,rmax,TDdiff)      
       
      if (TDerr_aux(rmax).lt.TDerr_aux2(rmax)) then
        if (present(TDerr))  TDerr = max(TDerr_aux,TDdiff*norm)
        do r=0,rmax
          TDacc(r) = max(TDerr_aux(r)/norm(r),TDdiff(r))
        end do
        if (Monitoring) PointsCntDten_coli =  PointsCntDten_coli + 1
      else
        TD = TD2
        TDuv = TDuv2        
        if (present(TDerr))  TDerr = max(TDerr_aux2,TDdiff*norm)    
        do r=0,rmax
          TDacc(r) = max(TDerr_aux2(r)/norm(r),TDdiff(r))
        end do         
        if (Monitoring) PointsCntDten_dd =  PointsCntDten_dd + 1
      end if
      
    else
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      if (present(TDerr)) TDerr = TDerr_aux
      norm=0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TD(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(2-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        TDacc(r) = TDerr_aux(r)/norm(r)
      end do       
      
    end if

    call PropagateAccFlag_cll(TDacc,rmax)
    call PropagateErrFlag_cll
    
    if (Monitoring) then
      PointsCntDten_cll =  PointsCntDten_cll + 1

      if(maxval(TDacc).gt.reqacc_cll) AccPointsCntDten_cll = AccPointsCntDten_cll + 1
      if(maxval(TDacc).gt.sreqacc_cll) sAccPointsCntDten_cll = sAccPointsCntDten_cll + 1

      if(maxval(TDacc).gt.critacc_cll) then
        CritPointsCntDten_cll =  CritPointsCntDten_cll + 1
        if ( CritPointsCntDten_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Dten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Dten_list_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine Dten_list_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)
    
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex, intent(out) :: TD(:), TDuv(:)
    double precision, intent(out), optional :: TDerr(0:rmax)
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call Dten_list_checked_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)

  end subroutine Dten_list_cll


  subroutine Dten_list_checked_cll(TD,TDuv,MomVec,MomInv,masses2,rmax,TDerr)
    
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex, intent(out) :: TD(RtS(rmax)), TDuv(RtS(rmax))
    double precision, intent(out), optional :: TDerr(0:rmax)
    double complex :: TD2(RtS(rmax)), TDuv2(RtS(rmax))    
    double complex :: CD(0:rmax/2,0:rmax,0:rmax,0:rmax) 
    double complex :: CDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: CDerr(0:rmax), TDerr_aux(0:rmax), TDerr_aux2(0:rmax)
    double complex :: args(22)
    double precision :: TDdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TDacc(0:rmax)
    integer :: r,i    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:18) = MomInv
    args(19:22) = masses2(0:)
    call SetMasterFname_cll('Dten_cll')
    call SetMasterN_cll(4)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(22,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD_list(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD_List(TD2,TDuv2,TDerr_aux2,CD,CDuv,CDerr,MomVec,rmax)      
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TD(i)))
          norm_dd = max(norm_dd,abs(TD2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(2-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(2-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TD,TD2,MomVec,MomInv,masses2,norm,4,rmax,TDdiff)      
       
      if (TDerr_aux(rmax).lt.TDerr_aux2(rmax)) then
        if (present(TDerr))  TDerr = max(TDerr_aux,TDdiff*norm)
        do r=0,rmax
          TDacc(r) = max(TDerr_aux(r)/norm(r),TDdiff(r))
        end do
        if (Monitoring) PointsCntDten_coli =  PointsCntDten_coli + 1
      else
        TD = TD2
        TDuv = TDuv2        
        if (present(TDerr))  TDerr = max(TDerr_aux2,TDdiff*norm)    
        do r=0,rmax
          TDacc(r) = max(TDerr_aux2(r)/norm(r),TDdiff(r))
        end do         
        if (Monitoring) PointsCntDten_dd =  PointsCntDten_dd + 1
      end if
      
    else    
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD_list(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      if (present(TDerr)) TDerr = TDerr_aux
      norm=0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TD(i)))      
        end do            
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(2-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        TDacc(r) = TDerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TDacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntDten_cll =  PointsCntDten_cll + 1

      if(maxval(TDacc).gt.reqacc_cll) AccPointsCntDten_cll = AccPointsCntDten_cll + 1
      if(maxval(TDacc).gt.sreqacc_cll) sAccPointsCntDten_cll = sAccPointsCntDten_cll + 1

      if(maxval(TDacc).gt.critacc_cll) then
        CritPointsCntDten_cll =  CritPointsCntDten_cll + 1
        if ( CritPointsCntDten_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Dten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Dten_args_cll(TC,TCuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rmax,TDerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Dten_args_cll(TD,TDuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,  &
                       m02,m12,m22,m32,rmax,TDerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3), p3vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: TD(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TDuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TDerr(0:rmax)
    double complex TD2(0:rmax,0:rmax,0:rmax,0:rmax), TDuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex :: CD(0:rmax/2,0:rmax,0:rmax,0:rmax) 
    double complex :: CDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: CDerr(0:rmax), TDerr_aux(0:rmax), TDerr_aux2(0:rmax)
    double complex :: args(22)
    double precision :: TDdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TDacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
   
    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p30
    MomInv(5) = p20
    MomInv(6) = p31
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:18) = MomInv
    args(19:22) = masses2(0:)
    call SetMasterFname_cll('Dten_cll')
    call SetMasterN_cll(4)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(22,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD(TD2,TDuv2,TDerr_aux2,CD,CDuv,CDerr,MomVec,rmax)      
       
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TD(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TD2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(2-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(2-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TD,TD2,MomVec,MomInv,masses2,norm,4,rmax,TDdiff)
             
      if (TDerr_aux(rmax).lt.TDerr_aux2(rmax)) then
        if (present(TDerr))  TDerr = max(TDerr_aux,TDdiff*norm)
        do r=0,rmax
          TDacc(r) = max(TDerr_aux(r)/norm(r),TDdiff(r))
        end do
        if (Monitoring) PointsCntDten_coli =  PointsCntDten_coli + 1
      else
        TD = TD2
        TDuv = TDuv2        
        if (present(TDerr))  TDerr = max(TDerr_aux2,TDdiff*norm)    
        do r=0,rmax
          TDacc(r) = max(TDerr_aux2(r)/norm(r),TDdiff(r))
        end do         
        if (Monitoring) PointsCntDten_dd =  PointsCntDten_dd + 1
      end if
      
    else    
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)

      call CalcTensorD(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      if (present(TDerr)) TDerr = TDerr_aux
      norm=0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TD(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(2-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        TDacc(r) = TDerr_aux(r)/norm(r)
      end do       
      
    end if

    call PropagateAccFlag_cll(TDacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntDten_cll =  PointsCntDten_cll + 1

      if(maxval(TDacc).gt.reqacc_cll) AccPointsCntDten_cll = AccPointsCntDten_cll + 1
      if(maxval(TDacc).gt.sreqacc_cll) sAccPointsCntDten_cll = sAccPointsCntDten_cll + 1

      if(maxval(TDacc).gt.critacc_cll) then
        CritPointsCntDten_cll =  CritPointsCntDten_cll + 1
        if ( CritPointsCntDten_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Dten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Dten_args_list_cll(TD,TDuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,  &
  !                     m02,m12,m22,m32,rmax,TDerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Dten_args_list_cll(TD,TDuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,  &
                       m02,m12,m22,m32,rmax,TDerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3), p3vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: TD(:), TDuv(:)
    double precision, intent(out), optional :: TDerr(0:rmax)
    logical :: eflag

    if (4.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','Nmax_cll smaller 4',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 4'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Dten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call Dten_args_list_checked_cll(TD,TDuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,  &
                       m02,m12,m22,m32,rmax,TDerr)

  end subroutine Dten_args_list_cll


  subroutine Dten_args_list_checked_cll(TD,TDuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,  &
                       m02,m12,m22,m32,rmax,TDerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3), p2vec(0:3), p3vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: TD(RtS(rmax)), TDuv(RtS(rmax))
    double precision, intent(out), optional :: TDerr(0:rmax)
    double complex :: TD2(RtS(rmax)), TDuv2(RtS(rmax))    
    double complex :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex :: CD(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double complex :: CDuv(0:rmax/2,0:rmax,0:rmax,0:rmax)
    double precision :: CDerr(0:rmax), TDerr_aux(0:rmax), TDerr_aux2(0:rmax)
    double complex :: args(22)
    double precision :: TDdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TDacc(0:rmax)
    integer :: r,i    

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p30
    MomInv(5) = p20
    MomInv(6) = p31
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:18) = MomInv
    args(19:22) = masses2(0:)
    call SetMasterFname_cll('Dten_cll')
    call SetMasterN_cll(4)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(22,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD_list(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
      call CalcTensorD_list(TD2,TDuv2,TDerr_aux2,CD,CDuv,CDerr,MomVec,rmax)      
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TD(i)))
          norm_dd = max(norm_dd,abs(TD2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(2-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(2-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TD,TD2,MomVec,MomInv,masses2,norm,4,rmax,TDdiff)
       
      if (TDerr_aux(rmax).lt.TDerr_aux2(rmax)) then
        if (present(TDerr))  TDerr = max(TDerr_aux,TDdiff*norm)
        do r=0,rmax
          TDacc(r) = max(TDerr_aux(r)/norm(r),TDdiff(r))
        end do
        if (Monitoring) PointsCntDten_coli =  PointsCntDten_coli + 1
      else
        TD = TD2
        TDuv = TDuv2        
        if (present(TDerr))  TDerr = max(TDerr_aux2,TDdiff*norm)    
        do r=0,rmax
          TDacc(r) = max(TDerr_aux2(r)/norm(r),TDdiff(r))
        end do         
        if (Monitoring) PointsCntDten_dd =  PointsCntDten_dd + 1
      end if      
      
    else    
      call D_main_cll(CD,CDuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      masses2(0),masses2(1),masses2(2),masses2(3),rmax,Derr2=CDerr,id_in=0)    
    
      call CalcTensorD_list(TD,TDuv,TDerr_aux,CD,CDuv,CDerr,MomVec,rmax)
      if (present(TDerr)) TDerr = TDerr_aux
      norm=0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TD(i)))      
        end do           
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:6))),maxval(abs(masses2(0:3))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(2-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(2-real(r)/2)
          end if
        end if
        TDacc(r) = TDerr_aux(r)/norm(r)
      end do       
      
    end if
 
    call PropagateAccFlag_cll(TDacc,rmax)
    call PropagateErrFlag_cll   

    if (Monitoring) then
      PointsCntDten_cll =  PointsCntDten_cll + 1

      if(maxval(TDacc).gt.reqacc_cll) AccPointsCntDten_cll = AccPointsCntDten_cll + 1
      if(maxval(TDacc).gt.sreqacc_cll) sAccPointsCntDten_cll = sAccPointsCntDten_cll + 1

      if(maxval(TDacc).gt.critacc_cll) then
        CritPointsCntDten_cll =  CritPointsCntDten_cll + 1
        if ( CritPointsCntDten_cll.le.noutCritPointsMax_cll(4) ) then
          call CritPointsOut_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TDten_cll',0,maxval(TDacc),CritPointsCntDten_cll)
          if( CritPointsCntDten_cll.eq.noutCritPointsMax_cll(4)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TDten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Dten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Eten_main_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Eten_main_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex, intent(out) :: TE(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TEuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TEerr(0:rmax)
    double complex :: TE2(0:rmax,0:rmax,0:rmax,0:rmax), TEuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CEerr(0:rmax), TEerr_aux(0:rmax), TEerr_aux2(0:rmax)
    double complex :: args(31)
    double precision :: TEdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TEacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:26) = MomInv
    args(27:31) = masses2
    call SetMasterFname_cll('Eten_cll')
    call SetMasterN_cll(5)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(31,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)                      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE2,TEuv2,TEerr_aux2,CE,CEuv,CEerr,MomVec,rmax)                          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TE(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TE2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(3-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(3-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TE,TE2,MomVec,MomInv,masses2,norm,5,rmax,TEdiff)
       
      if (TEerr_aux(rmax).lt.TEerr_aux2(rmax)) then
        if (present(TEerr))  TEerr = max(TEerr_aux,TEdiff*norm)
        do r=0,rmax
          TEacc(r) = max(TEerr_aux(r)/norm(r),TEdiff(r))
        end do
        if (Monitoring) PointsCntEten_coli =  PointsCntEten_coli + 1
      else
        TE = TE2
        TEuv = TEuv2        
        if (present(TEerr))  TEerr = max(TEerr_aux2,TEdiff*norm)    
        do r=0,rmax
          TEacc(r) = max(TEerr_aux2(r)/norm(r),TEdiff(r))
        end do         
        if (Monitoring) PointsCntEten_dd =  PointsCntEten_dd + 1
      end if      
      
    else
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)
      if (present(TEerr)) TEerr = TEerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TE(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(3-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        TEacc(r) = TEerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TEacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntEten_cll =  PointsCntEten_cll + 1

      if(maxval(TEacc).gt.reqacc_cll) AccPointsCntEten_cll = AccPointsCntEten_cll + 1
      if(maxval(TEacc).gt.sreqacc_cll) sAccPointsCntEten_cll = sAccPointsCntEten_cll + 1

      if(maxval(TEacc).gt.critacc_cll) then
        CritPointsCntEten_cll =  CritPointsCntEten_cll + 1
        if ( CritPointsCntEten_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Eten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Eten_list_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Eten_list_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex, intent(out) :: TE(:), TEuv(:)
    double precision, intent(out), optional :: TEerr(0:rmax)
    integer :: r,i   
    logical :: eflag

    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Eten_list_checked_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)

  end subroutine Eten_list_cll


  subroutine Eten_list_checked_cll(TE,TEuv,MomVec,MomInv,masses2,rmax,TEerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex, intent(out) :: TE(RtS(rmax)), TEuv(RtS(rmax))
    double precision, intent(out), optional :: TEerr(0:rmax)
    double complex :: TE2(RtS(rmax)), TEuv2(RtS(rmax))    
    double complex :: CE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CEerr(0:rmax), TEerr_aux(0:rmax), TEerr_aux2(0:rmax)
    double complex :: args(31)
    double precision :: TEdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TEacc(0:rmax)
    integer :: r,i   
    logical :: eflag

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:26) = MomInv
    args(27:31) = masses2
    call SetMasterFname_cll('Eten_cll')
    call SetMasterN_cll(5)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(31,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE_list(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)                      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE_list(TE2,TEuv2,TEerr_aux2,CE,CEuv,CEerr,MomVec,rmax)                          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TE(i)))
          norm_dd = max(norm_dd,abs(TE2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(3-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(3-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TE,TE2,MomVec,MomInv,masses2,norm,5,rmax,TEdiff)      
       
      if (TEerr_aux(rmax).lt.TEerr_aux2(rmax)) then
        if (present(TEerr))  TEerr = max(TEerr_aux,TEdiff*norm)
        do r=0,rmax
          TEacc(r) = max(TEerr_aux(r)/norm(r),TEdiff(r))
        end do
        if (Monitoring) PointsCntEten_coli =  PointsCntEten_coli + 1
      else
        TE = TE2
        TEuv = TEuv2        
        if (present(TEerr))  TEerr = max(TEerr_aux2,TEdiff*norm)    
        do r=0,rmax
          TEacc(r) = max(TEerr_aux2(r)/norm(r),TEdiff(r))
        end do         
        if (Monitoring) PointsCntEten_dd =  PointsCntEten_dd + 1
      end if
      
    else
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)    
      call CalcTensorE_list(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)
      if (present(TEerr)) TEerr = TEerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TE(i)))      
        end do       
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(3-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        TEacc(r) = TEerr_aux(r)/norm(r)
      end do      
      
    end if
 
    call PropagateAccFlag_cll(TEacc,rmax)
    call PropagateErrFlag_cll   

    if (Monitoring) then
      PointsCntEten_cll =  PointsCntEten_cll + 1

      if(maxval(TEacc).gt.reqacc_cll) AccPointsCntEten_cll = AccPointsCntEten_cll + 1
      if(maxval(TEacc).gt.sreqacc_cll) sAccPointsCntEten_cll = sAccPointsCntEten_cll + 1

      if(maxval(TEacc).gt.critacc_cll) then
        CritPointsCntEten_cll =  CritPointsCntEten_cll + 1
        if ( CritPointsCntEten_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Eten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Eten_args_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,  &
  !                             p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rmax,TEerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Eten_args_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,  &
                               p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rmax,TEerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: TE(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TEuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TEerr(0:rmax)
    double complex :: TE2(0:rmax,0:rmax,0:rmax,0:rmax), TEuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex :: CE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CEerr(0:rmax), TEerr_aux(0:rmax), TEerr_aux2(0:rmax)  
    double complex :: args(31)
    double precision :: TEdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TEacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p40
    MomInv(6) = p20
    MomInv(7) = p31
    MomInv(8) = p42
    MomInv(9) = p30
    MomInv(10) = p41
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42
    
    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:26) = MomInv
    args(27:31) = masses2
    call SetMasterFname_cll('Eten_cll')
    call SetMasterN_cll(5)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(31,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)                      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE2,TEuv2,TEerr_aux2,CE,CEuv,CEerr,MomVec,rmax)                          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TE(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TE2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(3-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(3-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensors_cll(TE,TE2,MomVec,MomInv,masses2,norm,5,rmax,TEdiff)      
       
      if (TEerr_aux(rmax).lt.TEerr_aux2(rmax)) then
        if (present(TEerr))  TEerr = max(TEerr_aux,TEdiff*norm)
        do r=0,rmax
          TEacc(r) = max(TEerr_aux(r)/norm(r),TEdiff(r))
        end do
        if (Monitoring) PointsCntEten_coli =  PointsCntEten_coli + 1
      else
        TE = TE2
        TEuv = TEuv2        
        if (present(TEerr))  TEerr = max(TEerr_aux2,TEdiff*norm)    
        do r=0,rmax
          TEacc(r) = max(TEerr_aux2(r)/norm(r),TEdiff(r))
        end do         
        if (Monitoring) PointsCntEten_dd =  PointsCntEten_dd + 1
      end if
      
    else
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)
      if (present(TEerr)) TEerr = TEerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TE(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(3-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        TEacc(r) = TEerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TEacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntEten_cll =  PointsCntEten_cll + 1

      if(maxval(TEacc).gt.reqacc_cll) AccPointsCntEten_cll = AccPointsCntEten_cll + 1
      if(maxval(TEacc).gt.sreqacc_cll) sAccPointsCntEten_cll = sAccPointsCntEten_cll + 1

      if(maxval(TEacc).gt.critacc_cll) then
        CritPointsCntEten_cll =  CritPointsCntEten_cll + 1
        if ( CritPointsCntEten_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Eten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Eten_args_list_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,  &
  !                             p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rmax)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Eten_args_list_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,  &
                               p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rmax,TEerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: TE(RtS(rmax)), TEuv(RtS(rmax))
    double precision, intent(out), optional :: TEerr(0:rmax)
    double complex :: TE2(RtS(rmax)), TEuv2(RtS(rmax))   
    double complex :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex :: CE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CEerr(0:rmax), TEerr_aux(0:rmax), TEerr_aux2(0:rmax)
    double complex :: args(31)
    double precision :: TEdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TEacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    if (5.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','Nmax_cll smaller 5',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 5'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Eten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Eten_args_list_checked_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec, &
                           p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                           m02,m12,m22,m32,m42,rmax,TEerr)

  end subroutine Eten_args_list_cll


  subroutine Eten_args_list_checked_cll(TE,TEuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,  &
                               p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rmax,TEerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: TE(RtS(rmax)), TEuv(RtS(rmax))
    double precision, intent(out), optional :: TEerr(0:rmax)
    double complex :: TE2(RtS(rmax)), TEuv2(RtS(rmax))   
    double complex :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex :: CE(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CEuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CEerr(0:rmax), TEerr_aux(0:rmax), TEerr_aux2(0:rmax)
    double complex :: args(31)
    double precision :: TEdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TEacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p40
    MomInv(6) = p20
    MomInv(7) = p31
    MomInv(8) = p42
    MomInv(9) = p30
    MomInv(10) = p41
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42    
     
    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:26) = MomInv
    args(27:31) = masses2
    call SetMasterFname_cll('Eten_cll')
    call SetMasterN_cll(5)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(31,args)

    call SetTenCache_cll(tenred_cll-1)

    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)                      
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)
      call CalcTensorE(TE2,TEuv2,TEerr_aux2,CE,CEuv,CEerr,MomVec,rmax)                          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TE(i)))
          norm_dd = max(norm_dd,abs(TE2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_coli.ne.0d0) then
            norm_coli=1d0/norm_coli**(3-real(r)/2)
          else
            norm_coli=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm_dd.ne.0d0) then
            norm_dd=1d0/norm_dd**(3-real(r)/2)
          else
            norm_dd=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTensorsList_cll(TE,TE2,MomVec,MomInv,masses2,norm,5,rmax,TEdiff)      
       
      if (TEerr_aux(rmax).lt.TEerr_aux2(rmax)) then
        if (present(TEerr))  TEerr = max(TEerr_aux,TEdiff*norm)
        do r=0,rmax
          TEacc(r) = max(TEerr_aux(r)/norm(r),TEdiff(r))
        end do
        if (Monitoring) PointsCntEten_coli =  PointsCntEten_coli + 1
      else
        TE = TE2
        TEuv = TEuv2        
        if (present(TEerr))  TEerr = max(TEerr_aux2,TEdiff*norm)    
        do r=0,rmax
          TEacc(r) = max(TEerr_aux2(r)/norm(r),TEdiff(r))
        end do         
        if (Monitoring) PointsCntEten_dd =  PointsCntEten_dd + 1
      end if
      
    else
      call E_main_cll(CE,CEuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                      MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),masses2(0), &
                      masses2(1),masses2(2),masses2(3),masses2(4),rmax,Eerr2=CEerr,id_in=0)   
      call CalcTensorE_list(TE,TEuv,TEerr_aux,CE,CEuv,CEerr,MomVec,rmax)
      if (present(TEerr)) TEerr = TEerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TE(i)))      
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:10))),maxval(abs(masses2(0:4))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(3-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(3-real(r)/2)
          end if
        end if
        TEacc(r) = TEerr_aux(r)/norm(r)
      end do     
      
    end if

    call PropagateAccFlag_cll(TEacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntEten_cll =  PointsCntEten_cll + 1

      if(maxval(TEacc).gt.reqacc_cll) AccPointsCntEten_cll = AccPointsCntEten_cll + 1
      if(maxval(TEacc).gt.sreqacc_cll) sAccPointsCntEten_cll = sAccPointsCntEten_cll + 1

      if(maxval(TEacc).gt.critacc_cll) then
        CritPointsCntEten_cll =  CritPointsCntEten_cll + 1
        if ( CritPointsCntEten_cll.le.noutCritPointsMax_cll(5) ) then
          call CritPointsOut_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TEten_cll',0,maxval(TEacc),CritPointsCntEten_cll)
          if( CritPointsCntEten_cll.eq.noutCritPointsMax_cll(5)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TEten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Eten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Ften_main_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Ften_main_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), MomInv(15), masses2(0:5)
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TFerr(0:rmax)
    double complex :: TF2(0:rmax,0:rmax,0:rmax,0:rmax), TFuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CFerr(0:rmax), TFerr_aux(0:rmax), TFerr_aux2(0:rmax) 
    double complex :: args(41)
    double precision :: TFdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TFacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:35) = MomInv
    args(36:41) = masses2(0:)
    call SetMasterFname_cll('Ften_cll')
    call SetMasterN_cll(6)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(41,args)

    call SetTenCache_cll(tenred_cll-1)


    if (tenred_cll.le.6) then
    
      if (mode_cll.gt.1) call F_dd_dummy(rmax)
             
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorFr(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorFr(TF2,TFuv2,TFerr_aux2,MomVec,MomInv,masses2,rmax)                               
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TF(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TF2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else      
        call CalcTensorFr(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TF(n0,n1,n2,n3)))
              end do
            end do
          end do       
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do      
      
      end if
    
    
    else
       
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF(TF2,TFuv2,TFerr_aux2,CF,CFuv,CFerr,MomVec,rmax)                                
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TF(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TF2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0)
        call CalcTensorF(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)
        if (present(TFerr)) TFerr = TFerr_aux       
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TF(n0,n1,n2,n3)))
              end do
            end do
          end do        
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do        
        
      end if
      
    end if

    call PropagateAccFlag_cll(TFacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntFten_cll =  PointsCntFten_cll + 1

      if(maxval(TFacc).gt.reqacc_cll) AccPointsCntFten_cll = AccPointsCntFten_cll + 1
      if(maxval(TFacc).gt.sreqacc_cll) sAccPointsCntFten_cll = sAccPointsCntFten_cll + 1

      if(maxval(TFacc).gt.critacc_cll) then
        CritPointsCntFten_cll =  CritPointsCntFten_cll + 1
        if ( CritPointsCntFten_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Ften_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Ften_list_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Ften_list_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), MomInv(15), masses2(0:5)
    double complex, intent(out) :: TF(:), TFuv(:)
    double precision, intent(out), optional :: TFerr(0:rmax)
    logical :: eflag

    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Ften_list_checked_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)

  end subroutine Ften_list_cll


  subroutine Ften_list_checked_cll(TF,TFuv,MomVec,MomInv,masses2,rmax,TFerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,5), MomInv(15), masses2(0:5)
    double complex, intent(out) :: TF(RtS(rmax)), TFuv(RtS(rmax))
    double precision, intent(out), optional :: TFerr(0:rmax)
    double complex :: TF2(RtS(rmax)), TFuv2(RtS(rmax))
    double complex :: CF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CFerr(0:rmax), TFerr_aux(0:rmax), TFerr_aux2(0:rmax)
    double complex :: args(41)
    double precision :: TFdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TFacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:35) = MomInv
    args(36:41) = masses2(0:)
    call SetMasterFname_cll('Ften_cll')
    call SetMasterN_cll(6)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(41,args)

    call SetTenCache_cll(tenred_cll-1)

    if (tenred_cll.le.6) then
    
      if (mode_cll.gt.1) call F_dd_dummy(rmax)
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorFr_list(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorFr_list(TF2,TFuv2,TFerr_aux2,MomVec,MomInv,masses2,rmax)                               

        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TF(i)))
            norm_dd = max(norm_dd,abs(TF2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do        
      
        call CheckTensorsList_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else       
        call CalcTensorFr_list(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
             norm(r) = max(norm(r),abs(TF(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do        
        
      end if
      
    else
    
       
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF_list(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF_list(TF2,TFuv2,TFerr_aux2,CF,CFuv,CFerr,MomVec,rmax)                                
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TF(i)))
            norm_dd = max(norm_dd,abs(TF2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensorsList_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0)
        call CalcTensorF_list(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TF(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do        
      
      end if
      
    end if

    call PropagateAccFlag_cll(TFacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntFten_cll =  PointsCntFten_cll + 1

      if(maxval(TFacc).gt.reqacc_cll) AccPointsCntFten_cll = AccPointsCntFten_cll + 1
      if(maxval(TFacc).gt.sreqacc_cll) sAccPointsCntFten_cll = sAccPointsCntFten_cll + 1

      if(maxval(TFacc).gt.critacc_cll) then
        CritPointsCntFten_cll =  CritPointsCntFten_cll + 1
        if ( CritPointsCntFten_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Ften_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Ften_args_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
  !                         p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Ften_args_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
                           p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                           p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: TF(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TFuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TFerr(0:rmax)
    double complex :: TF2(0:rmax,0:rmax,0:rmax,0:rmax), TFuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: MomVec(0:3,5), MomInv(15), masses2(0:5)
    double complex :: CF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CFerr(0:rmax), TFerr_aux(0:rmax), TFerr_aux2(0:rmax)
    double complex :: args(41)
    double precision :: TFdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TFacc(0:rmax)
    integer :: r,n0,n1,n2,n3   
    logical :: eflag

    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomVec(0:,5) = p5vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p54
    MomInv(6) = p50
    MomInv(7) = p20
    MomInv(8) = p31
    MomInv(9) = p42
    MomInv(10) = p53
    MomInv(11) = p40
    MomInv(12) = p51
    MomInv(13) = p30
    MomInv(14) = p41
    MomInv(15) = p52
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42
    masses2(5) = m52

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:35) = MomInv
    args(36:41) = masses2(0:)
    call SetMasterFname_cll('Ften_cll')
    call SetMasterN_cll(6)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(41,args)

    call SetTenCache_cll(tenred_cll-1)

    if (tenred_cll.le.6) then
    
      if (mode_cll.gt.1) call F_dd_dummy(rmax)
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorFr(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorFr(TF2,TFuv2,TFerr_aux2,MomVec,MomInv,masses2,rmax)                               
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TF(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TF2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else       
        call CalcTensorFr(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TF(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do        
        
      end if
      
    else
       
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF(TF2,TFuv2,TFerr_aux2,CF,CFuv,CFerr,MomVec,rmax)                                
          
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TF(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TF2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)         
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else    
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0)
        call CalcTensorF(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TF(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do        
        
      end if
    
    end if
 
    call PropagateAccFlag_cll(TFacc,rmax)
    call PropagateErrFlag_cll   

    if (Monitoring) then
      PointsCntFten_cll =  PointsCntFten_cll + 1

      if(maxval(TFacc).gt.reqacc_cll) AccPointsCntFten_cll = AccPointsCntFten_cll + 1
      if(maxval(TFacc).gt.sreqacc_cll) sAccPointsCntFten_cll = sAccPointsCntFten_cll + 1

      if(maxval(TFacc).gt.critacc_cll) then
        CritPointsCntFten_cll =  CritPointsCntFten_cll + 1
        if ( CritPointsCntFten_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Ften_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Ften_args_list_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
  !                         p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Ften_args_list_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
                           p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                           p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: TF(:),TFuv(:)
    double precision, intent(out), optional :: TFerr(0:rmax)
    logical :: eflag

    if (6.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','Nmax_cll smaller 6',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 6'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Ften_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Ften_args_list_checked_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
                           p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                           p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)

  end subroutine Ften_args_list_cll


  subroutine Ften_args_list_checked_cll(TF,TFuv,p1vec,p2vec,p3vec,p4vec,p5vec,  &
                           p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                           p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,TFerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: TF(RtS(rmax)),TFuv(RtS(rmax))
    double precision, intent(out), optional :: TFerr(0:rmax)
    double complex :: TF2(RtS(rmax)),TFuv2(RtS(rmax))    
    double complex :: MomVec(0:3,5), MomInv(15), masses2(0:5)
    double complex :: CF(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CFuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CFerr(0:rmax), TFerr_aux(0:rmax), TFerr_aux2(0:rmax)
    double complex :: args(41)
    double precision :: TFdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TFacc(0:rmax)
    integer :: r,i    
    logical :: eflag

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomVec(0:,5) = p5vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p54
    MomInv(6) = p50
    MomInv(7) = p20
    MomInv(8) = p31
    MomInv(9) = p42
    MomInv(10) = p53
    MomInv(11) = p40
    MomInv(12) = p51
    MomInv(13) = p30
    MomInv(14) = p41
    MomInv(15) = p52
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42
    masses2(5) = m52

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:35) = MomInv
    args(36:41) = masses2(0:)
    call SetMasterFname_cll('Ften_cll')
    call SetMasterN_cll(6)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(41,args)

    call SetTenCache_cll(tenred_cll-1)

    if (tenred_cll.le.6) then
    
      if (mode_cll.gt.1) call F_dd_dummy(rmax)
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorFr_list(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorFr_list(TF2,TFuv2,TFerr_aux2,MomVec,MomInv,masses2,rmax)                               
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TF(i)))
            norm_dd = max(norm_dd,abs(TF2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do        
      
        call CheckTensorsList_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)        
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else       
        call CalcTensorFr_list(TF,TFuv,TFerr_aux,MomVec,MomInv,masses2,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TF(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do         
        
      end if
      
    else           
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF_list(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)                                     
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0) 
        call CalcTensorF_list(TF2,TFuv2,TFerr_aux2,CF,CFuv,CFerr,MomVec,rmax)                                
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TF(i)))
            norm_dd = max(norm_dd,abs(TF2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(4-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(4-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensorsList_cll(TF,TF2,MomVec,MomInv,masses2,norm,6,rmax,TFdiff)         
       
        if (TFerr_aux(rmax).lt.TFerr_aux2(rmax)) then
          if (present(TFerr))  TFerr = max(TFerr_aux,TFdiff*norm)
          do r=0,rmax
            TFacc(r) = max(TFerr_aux(r)/norm(r),TFdiff(r))
          end do
          if (Monitoring) PointsCntFten_coli =  PointsCntFten_coli + 1
        else
          TF = TF2
          TFuv = TFuv2        
          if (present(TFerr))  TFerr = max(TFerr_aux2,TFdiff*norm)    
          do r=0,rmax
            TFacc(r) = max(TFerr_aux2(r)/norm(r),TFdiff(r))
          end do         
          if (Monitoring) PointsCntFten_dd =  PointsCntFten_dd + 1
        end if
        
      else
        call F_main_cll(CF,CFuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5), &
                        MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11), &
                        MomInv(12),MomInv(13),MomInv(14),MomInv(15),masses2(0),masses2(1), &
                        masses2(2),masses2(3),masses2(4),masses2(5),rmax,Ferr2=CFerr,id_in=0)
        call CalcTensorF_list(TF,TFuv,TFerr_aux,CF,CFuv,CFerr,MomVec,rmax)
        if (present(TFerr)) TFerr = TFerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TF(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:15))),maxval(abs(masses2(0:5))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(4-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(4-real(r)/2)
            end if
          end if
          TFacc(r) = TFerr_aux(r)/norm(r)
        end do       
        
      end if
      
    end if
    
    call PropagateAccFlag_cll(TFacc,rmax)
    call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntFten_cll =  PointsCntFten_cll + 1

      if(maxval(TFacc).gt.reqacc_cll) AccPointsCntFten_cll = AccPointsCntFten_cll + 1
      if(maxval(TFacc).gt.sreqacc_cll) sAccPointsCntFten_cll = sAccPointsCntFten_cll + 1

      if(maxval(TFacc).gt.critacc_cll) then
        CritPointsCntFten_cll =  CritPointsCntFten_cll + 1
        if ( CritPointsCntFten_cll.le.noutCritPointsMax_cll(6) ) then
          call CritPointsOut_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TFten_cll',0,maxval(TFacc),CritPointsCntFten_cll)
          if( CritPointsCntFten_cll.eq.noutCritPointsMax_cll(6)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TFten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Ften_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Gten_main_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Gten_main_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex, intent(out) :: TG(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TGuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TGerr(0:rmax)
    double precision :: TGerr_aux(0:rmax), TGerr_aux2(0:rmax)   
    double complex :: TG2(0:rmax,0:rmax,0:rmax,0:rmax), TGuv2(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CGerr(0:rmax), TGacc(0:rmax)
    double precision :: norm(0:rmax),norm_coli,norm_dd, TGdiff(0:rmax)
    double complex :: args(52)
    integer :: r,n0,n1,n2,n3
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:24) = MomVec(0:,6)
    args(25:45) = MomInv
    args(46:52) = masses2
    call SetMasterFname_cll('Gten_cll')
    call SetMasterN_cll(7)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(52,args)

    call SetTenCache_cll(tenred_cll-1)

    if (tenred_cll.le.7) then
    
      if (mode_cll.gt.1) call TN_dd_dummy(7,rmax)  
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorTNr(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax,0)        
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr(TG2,TGuv2,TGerr_aux2,MomVec,MomInv,masses2,7,rmax,0)         
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TG(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TG2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(5-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(5-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TG,TG2,MomVec,MomInv,masses2,norm,7,rmax,TGdiff)        
       
        if (TGerr_aux(rmax).lt.TGerr_aux2(rmax)) then
          if (present(TGerr))  TGerr = max(TGerr_aux,TGdiff*norm)
          do r=0,rmax
            TGacc(r) = max(TGerr_aux(r)/norm(r),TGdiff(r))
          end do
          if (Monitoring) PointsCntGten_coli =  PointsCntGten_coli + 1
        else
          TG = TG2
          TGuv = TGuv2        
          if (present(TGerr))  TGerr = max(TGerr_aux2,TGdiff*norm)    
          do r=0,rmax
            TGacc(r) = max(TGerr_aux2(r)/norm(r),TGdiff(r))
          end do         
          if (Monitoring) PointsCntGten_dd =  PointsCntGten_dd + 1
        end if
        
      else    
        call CalcTensorTNr(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax,0)        
        if (present(TGerr)) TGerr = TGerr_aux
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TG(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(5-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          TGacc(r) = TGerr_aux(r)/norm(r)
        end do        
        
      end if      
     
    else
      call G_main_cll(CG,CGuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12), &
                      MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18), &
                      MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),masses2(2), &
                      masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr2=CGerr,id_in=0)
      call CalcTensorG(TG,TGuv,TGerr_aux,CG,CGuv,CGerr,MomVec,rmax)
      if (present(TGerr)) TGerr = TGerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TG(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(5-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(5-real(r)/2)
          end if
        end if
        TGacc(r) = TGerr_aux(r)/norm(r)
      end do      
    end if    

    if (Monitoring) then
      PointsCntGten_cll =  PointsCntGten_cll + 1

      if(maxval(TGacc).gt.reqacc_cll) AccPointsCntGten_cll = AccPointsCntGten_cll + 1
      if(maxval(TGacc).gt.sreqacc_cll) sAccPointsCntGten_cll = sAccPointsCntGten_cll + 1

      if(maxval(TGacc).gt.critacc_cll) then
        CritPointsCntGten_cll =  CritPointsCntGten_cll + 1
        if ( CritPointsCntGten_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if  

  end subroutine Gten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Gten_list_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Gten_list_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex, intent(out) :: TG(:),TGuv(:)
    double precision, intent(out), optional :: TGerr(0:rmax)
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Gten_list_checked_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)

  end subroutine Gten_list_cll


  subroutine Gten_list_checked_cll(TG,TGuv,MomVec,MomInv,masses2,rmax,TGerr)

    integer, intent(in) :: rmax
    double complex, intent(in) :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex, intent(out) :: TG(RtS(rmax)),TGuv(RtS(rmax))
    double precision, intent(out), optional :: TGerr(0:rmax)
    double complex :: TG2(RtS(rmax)),TGuv2(RtS(rmax))
    double precision :: TGerr_aux(0:rmax),TGerr_aux2(0:rmax) 
    double complex :: CG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CGerr(0:rmax), TGacc(0:rmax)
    double precision :: norm(0:rmax),norm_coli,norm_dd, TGdiff(0:rmax)
    double complex :: args(52)
    integer :: r,i
    logical :: eflag

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:24) = MomVec(0:,6)
    args(25:45) = MomInv
    args(46:52) = masses2
    call SetMasterFname_cll('Gten_cll')
    call SetMasterN_cll(7)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(52,args)

    call SetTenCache_cll(tenred_cll-1)


    if (tenred_cll.le.7) then
    
      if (mode_cll.gt.1) call TN_dd_dummy(7,rmax)  
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorTNr_list(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax)        
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr_list(TG2,TGuv2,TGerr_aux2,MomVec,MomInv,masses2,7,rmax)         
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)          
            norm_coli = max(norm_coli,abs(TG(i)))
            norm_dd = max(norm_dd,abs(TG2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(5-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(5-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensorsList_cll(TG,TG2,MomVec,MomInv,masses2,norm,7,rmax,TGdiff)        
       
        if (TGerr_aux(rmax).lt.TGerr_aux2(rmax)) then
          if (present(TGerr))  TGerr = max(TGerr_aux,TGdiff*norm)
          do r=0,rmax
            TGacc(r) = max(TGerr_aux(r)/norm(r),TGdiff(r))
          end do
          if (Monitoring) PointsCntGten_coli =  PointsCntGten_coli + 1
        else
          TG = TG2
          TGuv = TGuv2        
          if (present(TGerr))  TGerr = max(TGerr_aux2,TGdiff*norm)    
          do r=0,rmax
            TGacc(r) = max(TGerr_aux2(r)/norm(r),TGdiff(r))
          end do         
          if (Monitoring) PointsCntGten_dd =  PointsCntGten_dd + 1
        end if
        
      else  
        call CalcTensorTNr_list(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax)
        if (present(TGerr)) TGerr = TGerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TG(i)))
          end do      
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(5-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          TGacc(r) = TGerr_aux(r)/norm(r)
        end do             
        
      end if      
            
    else
      call G_main_cll(CG,CGuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12), &
                      MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18), &
                      MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),masses2(2), &
                      masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr2=CGerr,id_in=0)
      call CalcTensorG_list(TG,TGuv,TGerr_aux,CG,CGuv,CGerr,MomVec,rmax)
      if (present(TGerr)) TGerr = TGerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TG(i)))
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(5-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(5-real(r)/2)
          end if
        end if
        TGacc(r) = TGerr_aux(r)/norm(r)
      end do      
    end if

    if (Monitoring) then
      PointsCntGten_cll =  PointsCntGten_cll + 1

      if(maxval(TGacc).gt.reqacc_cll) AccPointsCntGten_cll = AccPointsCntGten_cll + 1
      if(maxval(TGacc).gt.sreqacc_cll) sAccPointsCntGten_cll = sAccPointsCntGten_cll + 1

      if(maxval(TGacc).gt.critacc_cll) then
        CritPointsCntGten_cll =  CritPointsCntGten_cll + 1
        if ( CritPointsCntGten_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout_cll,*)
          endif
        end if
      end if
    end if

  end subroutine Gten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Gten_args_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
  !                         p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
  !                         p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
  !                         m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Gten_args_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
                           p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                           p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                           m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3),p6vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: TG(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TGuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TGerr(0:rmax)
    double complex :: TG2(0:rmax,0:rmax,0:rmax,0:rmax), TGuv2(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: TGerr_aux(0:rmax),TGerr_aux2(0:rmax)   
    double complex :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex :: CG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax) 
    double complex :: CGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CGerr(0:rmax), TGacc(0:rmax)
    double precision :: norm(0:rmax),norm_coli,norm_dd, TGdiff(0:rmax)
    double complex :: args(52)
    integer :: r,n0,n1,n2,n3
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomVec(0:,5) = p5vec
    MomVec(0:,6) = p6vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p54
    MomInv(6) = p65
    MomInv(7) = p60
    MomInv(8) = p20
    MomInv(9) = p31
    MomInv(10) = p42
    MomInv(11) = p53
    MomInv(12) = p64
    MomInv(13) = p50
    MomInv(14) = p61
    MomInv(15) = p30
    MomInv(16) = p41
    MomInv(17) = p52
    MomInv(18) = p63
    MomInv(19) = p40
    MomInv(20) = p51
    MomInv(21) = p62
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42
    masses2(5) = m52
    masses2(6) = m62

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:24) = MomVec(0:,6)
    args(25:45) = MomInv
    args(46:52) = masses2
    call SetMasterFname_cll('Gten_cll')
    call SetMasterN_cll(7)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(52,args)

    call SetTenCache_cll(tenred_cll-1)


    if (tenred_cll.le.7) then
    
      if (mode_cll.gt.1) call TN_dd_dummy(7,rmax)  
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorTNr(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax,0)        
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr(TG2,TGuv2,TGerr_aux2,MomVec,MomInv,masses2,7,rmax,0)         
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TG(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TG2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(5-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(5-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TG,TG2,MomVec,MomInv,masses2,norm,7,rmax,TGdiff)        
       
        if (TGerr_aux(rmax).lt.TGerr_aux2(rmax)) then
          if (present(TGerr))  TGerr = max(TGerr_aux,TGdiff*norm)
          do r=0,rmax
            TGacc(r) = max(TGerr_aux(r)/norm(r),TGdiff(r))
          end do
          if (Monitoring) PointsCntGten_coli =  PointsCntGten_coli + 1
        else
          TG = TG2
          TGuv = TGuv2        
          if (present(TGerr))  TGerr = max(TGerr_aux2,TGdiff*norm)    
          do r=0,rmax
            TGacc(r) = max(TGerr_aux2(r)/norm(r),TGdiff(r))
          end do         
          if (Monitoring) PointsCntGten_dd =  PointsCntGten_dd + 1
        end if
        
      else    
        call CalcTensorTNr(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax,0)        
        if (present(TGerr)) TGerr = TGerr_aux
        norm = 0d0
        do r=0,rmax
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TG(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(5-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          TGacc(r) = TGerr_aux(r)/norm(r)
        end do        
        
      end if      
      
    else
      call G_main_cll(CG,CGuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12), &
                      MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18), &
                      MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),masses2(2), &
                      masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr2=CGerr,id_in=0)
      call CalcTensorG(TG,TGuv,TGerr_aux,CG,CGuv,CGerr,MomVec,rmax)
      if (present(TGerr)) TGerr = TGerr_aux
      norm = 0d0
      do r=0,rmax
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TG(n0,n1,n2,n3)))
            end do
          end do
        end do      
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(5-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(5-real(r)/2)
          end if
        end if
        TGacc(r) = TGerr_aux(r)/norm(r)
      end do      
    end if

    if (Monitoring) then
      PointsCntGten_cll =  PointsCntGten_cll + 1

      if(maxval(TGacc).gt.reqacc_cll) AccPointsCntGten_cll = AccPointsCntGten_cll + 1
      if(maxval(TGacc).gt.sreqacc_cll) sAccPointsCntGten_cll = sAccPointsCntGten_cll + 1

      if(maxval(TGacc).gt.critacc_cll) then
        CritPointsCntGten_cll =  CritPointsCntGten_cll + 1
        if ( CritPointsCntGten_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout_cll,*)
          endif
        end if
      end if
    end if

  end subroutine Gten_args_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Gten_args_list_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
  !                           p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
  !                           p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
  !                           m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Gten_args_list_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
                           p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                           p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                           m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3),p6vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: TG(RtS(rmax)), TGuv(RtS(rmax))
    double precision, intent(out), optional :: TGerr(0:rmax)
    double complex :: TG2(RtS(rmax)), TGuv2(RtS(rmax))
    double precision :: TGerr_aux(0:rmax), TGerr_aux2(0:rmax)   
    double complex :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex :: CG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CGerr(0:rmax), TGacc(0:rmax)
    double precision :: norm(0:rmax), TGdiff(0:rmax), norm_coli, norm_dd
    double complex :: args(52)
    integer :: r,i
    logical :: eflag

    if (7.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','Nmax_cll smaller 7',eflag,.true.)
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= 7'
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('Gten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call Gten_args_list_checked_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
                           p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                           p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                           m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)

  end subroutine Gten_args_list_cll


  subroutine Gten_args_list_checked_cll(TG,TGuv,p1vec,p2vec,p3vec,p4vec,p5vec,p6vec,  &
                           p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                           p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                           m02,m12,m22,m32,m42,m52,m62,rmax,TGerr)
    integer, intent(in) :: rmax
    double complex, intent(in) :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3)
    double complex, intent(in) :: p5vec(0:3),p6vec(0:3)
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: TG(RtS(rmax)), TGuv(RtS(rmax))
    double precision, intent(out), optional :: TGerr(0:rmax)
    double complex :: TG2(RtS(rmax)), TGuv2(RtS(rmax))
    double precision :: TGerr_aux(0:rmax), TGerr_aux2(0:rmax)   
    double complex :: MomVec(0:3,6), MomInv(21), masses2(0:6)
    double complex :: CG(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: CGuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: CGerr(0:rmax), TGacc(0:rmax)
    double precision :: norm(0:rmax), TGdiff(0:rmax), norm_coli, norm_dd
    double complex :: args(52)
    integer :: r,i
    logical :: eflag

    MomVec(0:,1) = p1vec
    MomVec(0:,2) = p2vec
    MomVec(0:,3) = p3vec
    MomVec(0:,4) = p4vec
    MomVec(0:,5) = p5vec
    MomVec(0:,6) = p6vec
    MomInv(1) = p10
    MomInv(2) = p21
    MomInv(3) = p32
    MomInv(4) = p43
    MomInv(5) = p54
    MomInv(6) = p65
    MomInv(7) = p60
    MomInv(8) = p20
    MomInv(9) = p31
    MomInv(10) = p42
    MomInv(11) = p53
    MomInv(12) = p64
    MomInv(13) = p50
    MomInv(14) = p61
    MomInv(15) = p30
    MomInv(16) = p41
    MomInv(17) = p52
    MomInv(18) = p63
    MomInv(19) = p40
    MomInv(20) = p51
    MomInv(21) = p62
    masses2(0) = m02
    masses2(1) = m12
    masses2(2) = m22
    masses2(3) = m32
    masses2(4) = m42
    masses2(5) = m52
    masses2(6) = m62

    ! set ID of master call
    args(1:4) = MomVec(0:,1)
    args(5:8) = MomVec(0:,2)
    args(9:12) = MomVec(0:,3)
    args(13:16) = MomVec(0:,4)
    args(17:20) = MomVec(0:,5)
    args(21:24) = MomVec(0:,6)
    args(25:45) = MomInv
    args(46:52) = masses2
    call SetMasterFname_cll('Gten_cll')
    call SetMasterN_cll(7)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(52,args)

    call SetTenCache_cll(tenred_cll-1)


    if (tenred_cll.le.7) then
     
      if (mode_cll.gt.1) call TN_dd_dummy(7,rmax)  
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorTNr_list(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax)        
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr_list(TG2,TGuv2,TGerr_aux2,MomVec,MomInv,masses2,7,rmax)         
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)          
            norm_coli = max(norm_coli,abs(TG(i)))
            norm_dd = max(norm_dd,abs(TG2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(5-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(5-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensorsList_cll(TG,TG2,MomVec,MomInv,masses2,norm,7,rmax,TGdiff)        
       
        if (TGerr_aux(rmax).lt.TGerr_aux2(rmax)) then
          if (present(TGerr))  TGerr = max(TGerr_aux,TGdiff*norm)
          do r=0,rmax
            TGacc(r) = max(TGerr_aux(r)/norm(r),TGdiff(r))
          end do
          if (Monitoring) PointsCntGten_coli =  PointsCntGten_coli + 1
        else
          TG = TG2
          TGuv = TGuv2        
          if (present(TGerr))  TGerr = max(TGerr_aux2,TGdiff*norm)    
          do r=0,rmax
            TGacc(r) = max(TGerr_aux2(r)/norm(r),TGdiff(r))
          end do         
          if (Monitoring) PointsCntGten_dd =  PointsCntGten_dd + 1
        end if
        
      else  
        call CalcTensorTNr_list(TG,TGuv,TGerr_aux,MomVec,MomInv,masses2,7,rmax)
        if (present(TGerr)) TGerr = TGerr_aux
        norm = 0d0
        do r=0,rmax
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TG(i)))
          end do      
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(5-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(5-real(r)/2)
            end if
          end if
          TGacc(r) = TGerr_aux(r)/norm(r)
        end do             
        
      end if      
      
    else
      call G_main_cll(CG,CGuv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6), &
                      MomInv(7),MomInv(8),MomInv(9),MomInv(10),MomInv(11),MomInv(12), &
                      MomInv(13),MomInv(14),MomInv(15),MomInv(16),MomInv(17),MomInv(18), &
                      MomInv(19),MomInv(20),MomInv(21),masses2(0),masses2(1),masses2(2), &
                      masses2(3),masses2(4),masses2(5),masses2(6),rmax,Gerr2=CGerr,id_in=0)
      call CalcTensorG_list(TG,TGuv,TGerr_aux,CG,CGuv,CGerr,MomVec,rmax)
      if (present(TGerr)) TGerr = TGerr_aux
      norm = 0d0
      do r=0,rmax
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TG(i)))
        end do       
        if (norm(r).eq.0d0) then
          norm(r) = max(maxval(abs(MomInv(1:21))),maxval(abs(masses2(0:6))))
          if(norm(r).ne.0d0) then
            norm(r)=1d0/norm(r)**(5-real(r)/2)
          else
            norm(r)=1d0/muir2_cll**(5-real(r)/2)
          end if
        end if
        TGacc(r) = TGerr_aux(r)/norm(r)
      end do      
    end if

    if (Monitoring) then
      PointsCntGten_cll =  PointsCntGten_cll + 1

      if(maxval(TGacc).gt.reqacc_cll) AccPointsCntGten_cll = AccPointsCntGten_cll + 1
      if(maxval(TGacc).gt.sreqacc_cll) sAccPointsCntGten_cll = sAccPointsCntGten_cll + 1

      if(maxval(TGacc).gt.critacc_cll) then
        CritPointsCntGten_cll =  CritPointsCntGten_cll + 1
        if ( CritPointsCntGten_cll.le.noutCritPointsMax_cll(7) ) then
          call CritPointsOut_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TGten_cll',0,maxval(TGacc),CritPointsCntGten_cll)
          if( CritPointsCntGten_cll.eq.noutCritPointsMax_cll(7)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TGten_cll suppressed'   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine Gten_args_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine TNten_main_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine TNten_main_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,max(N-1,1)), MomInv(:), masses2(0:max(N-1,1))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TNerr(0:rmax)
    logical :: eflag

    if (N.eq.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','subroutine called with wrong number of arguments for N=1',eflag)
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
    
    call TNten_main_checked_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

  end subroutine TNten_main_cll


  subroutine TNten_main_checked_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,max(N-1,1)), MomInv(BinomTable(2,N)), masses2(0:max(N-1,1))
    double complex, intent(out) :: TN(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TNuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TNerr(0:rmax)
    double complex :: TN2(0:rmax,0:rmax,0:rmax,0:rmax), TNuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CN(NCoefs(rmax,N))
    double complex :: CNuv(NCoefs(rmax,N)) 
    double precision :: CNerr(0:rmax), TNerr_aux(0:rmax), TNerr_aux2(0:rmax)
    double complex :: args(4*(N-1)+BinomTable(2,N)+N)
    integer :: i
    double precision :: TNdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TNacc(0:rmax)
    integer :: r,n0,n1,n2,n3    
    logical :: eflag

    if (N.eq.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','subroutine called with wrong number of arguments for N=1',eflag)
      return
    end if

    do i=1,N-1
      args(4*i-3:4*i) = MomVec(0:,i)
    end do
    args(4*(N-1)+1:4*(N-1)+BinomTable(2,N)) = MomInv
    args(4*(N-1)+BinomTable(2,N)+1:4*(N-1)+BinomTable(2,N)+N) = masses2(0:)
    call SetMasterFname_cll('TNten_cll')
    call SetMasterN_cll(N)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(4*(N-1)+BinomTable(2,N)+N,args)

    call SetTenCache_cll(tenred_cll-1)

  
    if (tenred_cll.le.N+1) then
    
      if (mode_cll.gt.1) call TN_dd_dummy(N,rmax)
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call CalcTensorTNr(TN,TNuv,TNerr_aux,MomVec,MomInv,masses2,N,rmax,0)                                    
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr(TN2,TNuv2,TNerr_aux2,MomVec,MomInv,masses2,N,rmax,0)                               
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TN(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TN2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(N-2-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  & 
                   maxval(abs(masses2(0:N-1))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(N-2-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TN,TN2,MomVec,MomInv,masses2,norm,N,rmax,TNdiff)

        if (TNerr_aux(rmax).lt.TNerr_aux2(rmax)) then
          if (present(TNerr))  TNerr = max(TNerr_aux,TNdiff*norm)
          do r=0,rmax
            TNacc(r) = max(TNerr_aux(r)/norm(r),TNdiff(r))
          end do
          if (Monitoring) PointsCntTNten_coli(N) =  PointsCntTNten_coli(N) + 1
        else
          TN = TN2
          TNuv = TNuv2        
          if (present(TNerr))  TNerr = max(TNerr_aux2,TNdiff*norm)    
          do r=0,rmax
            TNacc(r) = max(TNerr_aux2(r)/norm(r),TNdiff(r))
          end do         
          if (Monitoring) PointsCntTNten_dd(N) =  PointsCntTNten_dd(N)  + 1
        end if
        
      else       
        call CalcTensorTNr(TN,TNuv,TNerr_aux,MomVec,MomInv,masses2,N,rmax,0)
        if (present(TNerr)) TNerr = TNerr_aux
        do r=0,rmax
          norm(r)=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TN(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(N-2-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
        end do
        do r=0,rmax
          TNacc(r) = TNerr_aux(r)/norm(r)
        end do        
        
      end if
      
    else
       
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN(TN,TNuv,TNerr_aux,CN,CNuv,CNerr,MomVec,N,rmax)        
      
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN(TN2,TNuv2,TNerr_aux2,CN,CNuv,CNerr,MomVec,N,rmax)                                
      
        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm_coli = max(norm_coli,abs(TN(n0,n1,n2,n3)))
                norm_dd = max(norm_dd,abs(TN2(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(N-2-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  & 
                   maxval(abs(masses2(0:N-1))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(N-2-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do
      
        call CheckTensors_cll(TN,TN2,MomVec,MomInv,masses2,norm,N,rmax,TNdiff)         
       
        if (TNerr_aux(rmax).lt.TNerr_aux2(rmax)) then
          if (present(TNerr))  TNerr = max(TNerr_aux,TNdiff*norm)
          do r=0,rmax
            TNacc(r) = max(TNerr_aux(r)/norm(r),TNdiff(r))
          end do
          if (Monitoring) PointsCntTNten_coli(N) =  PointsCntTNten_coli(N) + 1
        else
          TN = TN2
          TNuv = TNuv2        
          if (present(TNerr))  TNerr = max(TNerr_aux2,TNdiff*norm)    
          do r=0,rmax
            TNacc(r) = max(TNerr_aux2(r)/norm(r),TNdiff(r))
          end do         
          if (Monitoring) PointsCntTNten_dd(N) =  PointsCntTNten_dd(N)  + 1
        end if
        
      else    
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN(TN,TNuv,TNerr_aux,CN,CNuv,CNerr,MomVec,N,rmax)
        if (present(TNerr)) TNerr = TNerr_aux
        do r=0,rmax
          norm(r)=0d0
          do n0=0,r
            do n1=0,r-n0
              do n2=0,r-n0-n1
                n3=r-n0-n1-n2
                norm(r) = max(norm(r),abs(TN(n0,n1,n2,n3)))
              end do
            end do
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(N-2-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
        end do
        do r=0,rmax
          TNacc(r) = TNerr_aux(r)/norm(r)
        end do         
        
      end if
      
    end if

    call PropagateAccFlag_cll(TNacc,rmax)
    call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntTNten_cll(N) =  PointsCntTNten_cll(N) + 1

      if(maxval(TNacc).gt.reqacc_cll) AccPointsCntTNten_cll(N) = AccPointsCntTNten_cll(N) + 1
      if(maxval(TNacc).gt.sreqacc_cll) sAccPointsCntTNten_cll(N) = sAccPointsCntTNten_cll(N) + 1

      if(maxval(TNacc).gt.critacc_cll) then
        CritPointsCntTNten_cll(N) =  CritPointsCntTNten_cll(N) + 1
        if ( CritPointsCntTNten_cll(N).le.noutCritPointsMax_cll(N) ) then
          call CritPointsOut_cll('TNten_cll',N,maxval(TNacc),CritPointsCntTNten_cll(N))
          if( CritPointsCntTNten_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',N   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TNten_cll',N,maxval(TNacc),CritPointsCntTNten_cll(N))
          if( CritPointsCntTNten_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',N   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine TNten_main_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine TNten_list_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine TNten_list_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,max(N-1,1)), MomInv(:), masses2(0:max(N-1,1))
    double complex, intent(out) :: TN(:),TNuv(:)
    double precision, intent(out), optional :: TNerr(0:rmax)
    logical :: eflag
      
    if (N.eq.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','subroutine called with wrong number of arguments for N=1',eflag)
      call PropagateErrFlag_cll
      return
    end if
    if (N.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument N larger than Nmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'N        =',N
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= ',N
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    
    
    call TNten_list_checked_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

  end subroutine TNten_list_cll



  subroutine TNten_list_checked_cll(TN,TNuv,MomVec,MomInv,masses2,N,rmax,TNerr)

    integer, intent(in) :: N,rmax
    double complex, intent(in) :: MomVec(0:3,max(N-1,1)), MomInv(BinomTable(2,N)), masses2(0:max(N-1,1))
    double complex, intent(out) :: TN(RtS(rmax)),TNuv(RtS(rmax))
    double precision, intent(out), optional :: TNerr(0:rmax)
    double complex :: TN2(RtS(rmax)),TNuv2(RtS(rmax))    
    double complex :: CN(NCoefs(rmax,N)),CNuv(NCoefs(rmax,N))
    double precision :: CNerr(0:rmax), TNerr_aux(0:rmax), TNerr_aux2(0:rmax)
    double complex :: args(4*(N-1)+BinomTable(2,N)+N)
    double precision :: TNdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TNacc(0:rmax)
    integer :: r,i
    logical :: eflag
      
!    if (N.eq.1) then
!      call SetErrFlag_cll(-10)
!      call ErrOut_cll('TNten_cll','subroutine called with wrong number of arguments for N=1',eflag)
!      return
!    end if

    do i=1,N-1
      args(4*i-3:4*i) = MomVec(0:,i)
    end do
    args(4*(N-1)+1:4*(N-1)+BinomTable(2,N)) = MomInv
    args(4*(N-1)+BinomTable(2,N)+1:4*(N-1)+BinomTable(2,N)+N) = masses2(0:)
    call SetMasterFname_cll('TNten_cll')
    call SetMasterN_cll(N)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(4*(N-1)+BinomTable(2,N)+N,args)

    call SetTenCache_cll(tenred_cll-1)

  
    if (tenred_cll.le.N+1) then
    
      if (mode_cll.gt.1) call TN_dd_dummy(N,rmax)
      
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1

        call CalcTensorTNr_list(TN,TNuv,TNerr_aux,MomVec,MomInv,masses2,N,rmax)                                    
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call CalcTensorTNr_list(TN2,TNuv2,TNerr_aux2,MomVec,MomInv,masses2,N,rmax)                               

        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TN(i)))
            norm_dd = max(norm_dd,abs(TN2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(N-2-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  & 
                   maxval(abs(masses2(0:N-1))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(N-2-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do        
      
        call CheckTensorsList_cll(TN,TN2,MomVec,MomInv,masses2,norm,N,rmax,TNdiff)        
       
        if (TNerr_aux(rmax).lt.TNerr_aux2(rmax)) then
          if (present(TNerr))  TNerr = max(TNerr_aux,TNdiff*norm)
          do r=0,rmax
            TNacc(r) = max(TNerr_aux(r)/norm(r),TNdiff(r))
          end do
          if (Monitoring) PointsCntTNten_coli(N) =  PointsCntTNten_coli(N) + 1
        else
          TN = TN2
          TNuv = TNuv2        
          if (present(TNerr))  TNerr = max(TNerr_aux2,TNdiff*norm)    
          do r=0,rmax
            TNacc(r) = max(TNerr_aux2(r)/norm(r),TNdiff(r))
          end do         
          if (Monitoring) PointsCntTNten_dd(N) =  PointsCntTNten_dd(N) + 1
        end if
        
      else       
        call CalcTensorTNr_list(TN,TNuv,TNerr_aux,MomVec,MomInv,masses2,N,rmax)
        if (present(TNerr)) TNerr = TNerr_aux

        do r=0,rmax
          norm(r)=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TN(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                maxval(abs(masses2(0:N-1))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(N-2-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(N-2-real(r)/2)
            end if
          end if
        end do
        do r=0,rmax
          TNacc(r) = TNerr_aux(r)/norm(r)
        end do         
        
      end if
      
    else
    
           
      if (mode_cll.eq.3) then
        ! calculate tensor with coefficients from COLI
        mode_cll = 1
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN_list(TN,TNuv,TNerr_aux,CN,CNuv,CNerr,MomVec,N,rmax)        
        ! calculate tensor with coefficients from DD
        mode_cll = 2
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN_list(TN2,TNuv2,TNerr_aux2,CN,CNuv,CNerr,MomVec,N,rmax)                                

        ! comparison --> take better result
        mode_cll = 3
        do r=0,rmax
          norm_coli=0d0
          norm_dd=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm_coli = max(norm_coli,abs(TN(i)))
            norm_dd = max(norm_dd,abs(TN2(i)))
          end do
          if (norm_coli.eq.0d0) then
            norm_coli = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                   maxval(abs(masses2(0:N-1))))
            if(norm_coli.ne.0d0) then
              norm_coli=1d0/norm_coli**(N-2-real(r)/2)
            else
              norm_coli=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          if (norm_dd.eq.0d0) then
            norm_dd = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  & 
                   maxval(abs(masses2(0:N-1))))
            if(norm_dd.ne.0d0) then
              norm_dd=1d0/norm_dd**(N-2-real(r)/2)
            else
              norm_dd=1d0/muir2_cll**(N-2-real(r)/2)
            end if 
          end if
          norm(r) = min(norm_coli,norm_dd)
        end do        
     
        call CheckTensorsList_cll(TN,TN2,MomVec,MomInv,masses2,norm,N,rmax,TNdiff)
       
        if (TNerr_aux(rmax).lt.TNerr_aux2(rmax)) then
          if (present(TNerr))  TNerr = max(TNerr_aux,TNdiff*norm)
          do r=0,rmax
            TNacc(r) = max(TNerr_aux(r)/norm(r),TNdiff(r))
          end do
          if (Monitoring) PointsCntTNten_coli(N) =  PointsCntTNten_coli(N) + 1
        else
          TN = TN2
          TNuv = TNuv2        
          if (present(TNerr))  TNerr = max(TNerr_aux2,TNdiff*norm)    
          do r=0,rmax
            TNacc(r) = max(TNerr_aux2(r)/norm(r),TNdiff(r))
          end do         
          if (Monitoring) PointsCntTNten_dd(N) =  PointsCntTNten_dd(N) + 1
        end if        
    
      else 
        call TN_cll(CN,CNuv,MomInv,masses2,N,rmax,TNerr2=CNerr,id_in=0)
        call CalcTensorTN_list(TN,TNuv,TNerr_aux,CN,CNuv,CNerr,MomVec,N,rmax)

        if (present(TNerr)) TNerr = TNerr_aux

        do r=0,rmax
          norm(r)=0d0
          do i=RtS(r-1)+1,RtS(r)
            norm(r) = max(norm(r),abs(TN(i)))
          end do
          if (norm(r).eq.0d0) then
            norm(r) = max(maxval(abs(MomInv(1:BinomTable(2,N)))),  &
                maxval(abs(masses2(0:N-1))))
            if(norm(r).ne.0d0) then
              norm(r)=1d0/norm(r)**(N-2-real(r)/2)
            else
              norm(r)=1d0/muir2_cll**(N-2-real(r)/2)
            end if
          end if
        end do
        do r=0,rmax
          TNacc(r) = TNerr_aux(r)/norm(r)
        end do         
        
      end if
      
    end if

    call PropagateAccFlag_cll(TNacc,rmax)
    call PropagateErrFlag_cll

    if (Monitoring) then
      PointsCntTNten_cll(N) =  PointsCntTNten_cll(N) + 1

      if(maxval(TNacc).gt.reqacc_cll) AccPointsCntTNten_cll(N) = AccPointsCntTNten_cll(N) + 1
      if(maxval(TNacc).gt.sreqacc_cll) sAccPointsCntTNten_cll(N) = sAccPointsCntTNten_cll(N) + 1

      if(maxval(TNacc).gt.critacc_cll) then
        CritPointsCntTNten_cll(N) =  CritPointsCntTNten_cll(N) + 1
        if ( CritPointsCntTNten_cll(N).le.noutCritPointsMax_cll(N) ) then
          call CritPointsOut_cll('TNten_cll',N,maxval(TNacc),CritPointsCntTNten_cll(N))
          if( CritPointsCntTNten_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',N   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TNten_cll',N,maxval(TNacc),CritPointsCntTNten_cll(N))
          if( CritPointsCntTNten_cll(N).eq.noutCritPointsMax_cll(N)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',N   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine TNten_list_checked_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine T1ten_cll(TA,TAuv,masses2,N,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine T1ten_main_cll(TA,TAuv,masses2,N,rmax,TAerr)

    integer, intent(in) :: rmax,N
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: TAuv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(0:rmax,0:rmax,0:rmax,0:rmax), TAuv2(0:rmax,0:rmax,0:rmax,0:rmax)    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax),TAerr_aux(0:rmax),TAerr_aux2(0:rmax)
    double complex :: args(1)    
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,n0,n1,n2,n3
    logical :: eflag
    
    if (N.ne.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','subroutine called with inconsistent arguments',eflag)
    end if
    if (N.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument N larger than Nmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'N        =',N
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= ',N
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    args(1) = masses2(0)
    call SetMasterFname_cll('TNten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)
      call CalcTensorA(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)          
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm_coli = max(norm_coli,abs(TA(n0,n1,n2,n3)))
              norm_dd = max(norm_dd,abs(TA2(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenA_cll(TA,TA2,masses2,norm,rmax,TAdiff)
!      call CheckTensors_cll(TA,TA2,masses2,norm,1,rmax,TAdiff)
      
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        PointsCntTNten_coli(1) =  PointsCntTNten_coli(1) + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do         
        PointsCntTNten_dd(1) =  PointsCntTNten_dd(1) + 1
      end if       
    
    else
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)
      call CalcTensorA(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              norm(r) = max(norm(r),abs(TA(n0,n1,n2,n3)))
            end do
          end do
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntTNten_cll(1) =  PointsCntTNten_cll(1) + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntTNten_cll(1) = AccPointsCntTNten_cll(1) + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntTNten_cll(1) = sAccPointsCntTNten_cll(1) + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntTNten_cll(1) =  CritPointsCntTNten_cll(1) + 1
        if ( CritPointsCntTNten_cll(1).le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TNten_cll',1,maxval(TAacc),CritPointsCntTNten_cll(1))
          if( CritPointsCntTNten_cll(1).eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',1   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TNten_cll',1,maxval(TAacc),CritPointsCntTNten_cll(1))
          if( CritPointsCntTNten_cll(1).eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',1   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine T1ten_main_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine T1ten_cll(TA,TAuv,masses2,N,rmax,TAerr)
  ! 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine T1ten_list_cll(TA,TAuv,masses2,N,rmax,TAerr)

    integer, intent(in) :: rmax,N
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(:),TAuv(:)
    double precision, intent(out), optional :: TAerr(0:rmax)
    integer :: r,i
    logical :: eflag

    if (N.ne.1) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','subroutine called with inconsistent arguments',eflag)
    end if
    if (N.gt.Nmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument N larger than Nmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'N        =',N
      write(nerrout_cll,*) 'Nmax_cll =',Nmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with Nmax_cll >= ',N
      call PropagateErrFlag_cll
      return
    end if    
    if (rmax.gt.rmax_cll) then
      call SetErrFlag_cll(-10)
      call ErrOut_cll('TNten_cll','argument rmax larger than rmax_cll',eflag,.true.)
      write(nerrout_cll,*) 'rmax     =',rmax
      write(nerrout_cll,*) 'rmax_cll =',rmax_cll
      write(nerrout_cll,*) 'Reinitialize COLLIER with rmax_cll >= ',rmax
      call PropagateErrFlag_cll
      return
    end if    

    call T1ten_list_checked_cll(TA,TAuv,masses2,N,rmax,TAerr)

  end subroutine T1ten_list_cll


  subroutine T1ten_list_checked_cll(TA,TAuv,masses2,N,rmax,TAerr)

    integer, intent(in) :: rmax,N
    double complex,intent(in) :: masses2(0:0)
    double complex, intent(out) :: TA(RtS(rmax)),TAuv(RtS(rmax))
    double precision, intent(out), optional :: TAerr(0:rmax)
    double complex :: TA2(RtS(rmax)),TAuv2(RtS(rmax))    
    double complex :: CA(0:rmax/2), CAuv(0:rmax/2)
    double precision :: CAerr(0:rmax), TAerr_aux(0:rmax), TAerr_aux2(0:rmax)
    double complex :: args(1)
    double precision :: TAdiff(0:rmax),norm(0:rmax),norm_coli,norm_dd,TAacc(0:rmax)
    integer :: r,i
    logical :: eflag

    args(1) = masses2(0)
    call SetMasterFname_cll('TNten_cll')
    call SetMasterN_cll(1)
    call SetMasterR_cll(rmax)
    call SetMasterArgs_cll(1,args)

    call SetTenCache_cll(tenred_cll-1)
    
    if (mode_cll.eq.3) then
      ! calculate tensor with coefficients from COLI
      mode_cll = 1
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)         
      
      ! calculate tensor with coefficients from DD
      mode_cll = 2
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)
      call CalcTensorA_list(TA2,TAuv2,TAerr_aux2,CA,CAuv,CAerr,rmax)
      
      ! comparison --> take better result
      mode_cll = 3
      do r=0,rmax
        norm_coli=0d0
        norm_dd=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm_coli = max(norm_coli,abs(TA(i)))
          norm_dd = max(norm_dd,abs(TA2(i)))
        end do
        if (norm_coli.eq.0d0) then
          norm_coli = abs(masses2(0))
          if(norm_coli.ne.0d0) then
            norm_coli=norm_coli**(1+real(r)/2)
          else
            norm_coli=muuv2_cll**(1+real(r)/2)
          end if
        end if
        if (norm_dd.eq.0d0) then
          norm_dd = abs(masses2(0))
          if(norm_dd.ne.0d0) then
            norm_dd=norm_dd**(1+real(r)/2)
          else
            norm_dd=muuv2_cll**(1+real(r)/2)
          end if
        end if
        norm(r) = min(norm_coli,norm_dd)
      end do
      
      call CheckTenAList_cll(TA,TA2,masses2,norm,rmax,TAdiff)
      
      if (TAerr_aux(rmax).lt.TAerr_aux2(rmax)) then
        if (present(TAerr))  TAerr = max(TAerr_aux,TAdiff*norm)
        do r=0,rmax
          TAacc(r) = max(TAerr_aux(r)/norm(r),TAdiff(r))
        end do
        if (Monitoring) PointsCntTNten_coli(1) =  PointsCntTNten_coli(1) + 1
      else
        TA = TA2
        TAuv = TAuv2        
        if (present(TAerr))  TAerr = max(TAerr_aux2,TAdiff*norm)    
        do r=0,rmax
          TAacc(r) = max(TAerr_aux2(r)/norm(r),TAdiff(r))
        end do         
        if (Monitoring) PointsCntTNten_dd(1) =  PointsCntTNten_dd(1) + 1
      end if       
    
    else
!      call A_cll(CA,CAuv,masses2(0),rmax,CAerr,0)    
      call TN_cll(CA,CAuv,masses2(0:0),1,rmax,CAerr,0)    
      call CalcTensorA_list(TA,TAuv,TAerr_aux,CA,CAuv,CAerr,rmax)
      if (present(TAerr)) TAerr = TAerr_aux
      do r=0,rmax
        norm(r)=0d0
        do i=RtS(r-1)+1,RtS(r)
          norm(r) = max(norm(r),abs(TA(i)))
        end do
        if (norm(r).eq.0d0) then
          norm(r) = abs(masses2(0))
          if(norm(r).ne.0d0) then
            norm(r)=norm(r)**(1+real(r)/2)
          else
            norm(r)=muuv2_cll**(1+real(r)/2)
          end if
        end if
      end do
      do r=0,rmax
        TAacc(r) = TAerr_aux(r)/norm(r)
      end do      
      
    end if

    call PropagateAccFlag_cll(TAacc,rmax)
    call PropagateErrFlag_cll    

    if (Monitoring) then
      PointsCntTNten_cll(1) =  PointsCntTNten_cll(1) + 1

      if(maxval(TAacc).gt.reqacc_cll) AccPointsCntTNten_cll(1) = AccPointsCntTNten_cll(1) + 1
      if(maxval(TAacc).gt.sreqacc_cll) sAccPointsCntTNten_cll(1) = sAccPointsCntTNten_cll(1) + 1

      if(maxval(TAacc).gt.critacc_cll) then
        CritPointsCntTNten_cll(1) =  CritPointsCntTNten_cll(1) + 1
        if ( CritPointsCntTNten_cll(1).le.noutCritPointsMax_cll(1) ) then
          call CritPointsOut_cll('TNten_cll',1,maxval(TAacc),CritPointsCntTNten_cll(1))
          if( CritPointsCntTNten_cll(1).eq.noutCritPointsMax_cll(1)) then
            write(ncpout_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',1   
            write(ncpout_cll,*)
          endif
#ifdef CritPoints2
          call CritPointsOut2_cll('TNten_cll',1,maxval(TAacc),CritPointsCntTNten_cll(1))
          if( CritPointsCntTNten_cll(1).eq.noutCritPointsMax_cll(1)) then
            write(ncpout2_cll,*) ' Further output of Critical Points for TNten_cll suppressed for N =',1   
            write(ncpout2_cll,*)
          endif
#endif
        end if
      end if
    end if

  end subroutine T1ten_list_checked_cll




end module collier_tensors

