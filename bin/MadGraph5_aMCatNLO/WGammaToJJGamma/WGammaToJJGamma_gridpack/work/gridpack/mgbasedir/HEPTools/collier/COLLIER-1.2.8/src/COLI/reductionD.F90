!!
!!  File reductionD.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!
!
!#define Dredtest
!#define Dpvtest
!#define Dpv1otest
!#define Dpv1test
!#define Dpv2test
!#define Dgtest 
!#define Dgmtest 
!#define Dgrtest
!#define Dgytest
!#define Dgxtest
!#define Dgptest
!#define Dgpftest
#define ALWAYSPV           !   default
!#define USED0
!#define PPEXP00
#define Cutrloop           !   default
!#define USEGM     !  needs changes in CalcDred in select etc

!#define TEST
!#define CritPointsCOLI 
#define PVEST2             !   default

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ***********************
!  *  module reductionD  *
!  *    by Lars Hofer    *
!  ***********************
! 
!  functions and subroutines:
!  CalcDuv, CalcDpv, CalcDpv1, CalcDpv2, CalcDg, CalcDgy, CalcDgp, CalcDgr, CalcDgpf, CopyDimp3
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



module globalD

  double complex :: q10,q21,q32,q30,q20,q31,mm02,mm12,mm22,mm32
! double complex :: q1q2,q1q3,q2q3,detZ,Z(3,3),Zadj(3,3),f(3),Zadjf(3)
  double complex :: detZ,Z(3,3),Zadj(3,3),f(3),Zadjf(3)
  double complex :: Zadj2f(3,3,3),Zadj2ff(3,3),Xadj(0:3,0:3),Zadjs(3)
  double complex :: Zadjff,detZmZadjf
  double complex :: mx(0:3,0:3),mxinv(0:3,0:3),Zinv(3,3),detX
  double precision :: q2max,m2max,m2scale,adetZ,fmax,maxZadjf,maxZadjfd,aZadjff
  double precision :: maxZadj2ff,maxXadj,adetX,maxZadj,maxZadj2f,maxZ
  double precision :: fac_g,x_g
  double precision :: fac_gm,x_gm
  double precision :: fac_gy,x_gy,y_gy,v_gy
  double precision :: fac_gp,w_gp
  double precision :: fac_gr
  double precision :: fac_gpf,x_gpf,y_gpf,v_gpf
!  double precision :: pweight(3)
  double precision :: wmaxZadj,wmaxZadjf,wmaxXadj
  double precision :: detZerr,detXerr  !  added 13.09.2022
  double complex, parameter :: undefined_D=1d50
  
end module globalD




module reductionD

  use reductionC

  implicit none

  ! should not be too small since expansion for large expansion parameters are calculated to early
  ! 1d1 to small for gy exp
  ! 10.08.2017 1d2 to small for gy expansion => adapt exit rloop in gy expansion 
  double precision, parameter :: truncfacD = 1d2
!  double precision, parameter :: truncfacD = 1d0

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcD(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,
  !                   rmax,id,Derr1,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcD(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,     &
      rmax,id,Derr1,Derr2)
  
    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr1(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: Daux(:,:,:,:), Duvaux(:,:,:,:), fct(:)
    double precision, allocatable :: Derr1aux(:),Derr2aux(:)
    double complex :: x(10)
    integer :: rank,switch,cnt,n0,n1,n2,n3,r
    logical :: nocalc,wrica

!    write(*,*)  'CalcD in',p10,p21,p32,p30,p20,p31
!    write(*,*)  'CalcD in',m02,m12,m22,m32
!    write(*,*)  'CalcD in',rmax,id

    if (use_cache_system) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.4) then
          x(1)=p10
          x(2)=p21
          x(3)=p32
          x(4)=p30
          x(5)=p20
          x(6)=p31
          x(7)=m02
          x(8)=m12
          x(9)=m22
          x(10)=m32
          rank = rmax
          switch = 0

          if(rmax.ge.3) then
            allocate(fct(NCoefsG(rmax,4)-NCoefs(rmax-2,4)+NCoefs(rmax-3,4)+2*(rmax+1)))
            call ReadCache(fct,NCoefsG(rmax,4)-NCoefs(rmax-2,4)+NCoefs(rmax-3,4)+2*(rmax+1),x,10,1,id,4,rank,nocalc,wrica)
          else if(rmax.eq.2) then
            allocate(fct(NCoefsG(rmax,4)+1+2*(rmax+1)))
            call ReadCache(fct,NCoefsG(rmax,4)+1+2*(rmax+1),x,10,1,id,4,rank,nocalc,wrica)
          else
            allocate(fct(NCoefsG(rmax,4)+2*(rmax+1)))
            call ReadCache(fct,NCoefsG(rmax,4)+2*(rmax+1),x,10,1,id,4,rank,nocalc,wrica)
          end if
    
          if(nocalc)then
            cnt = 0
            Duv(0:min(rmax/2,1),:,:,:) = 0d0
            do r=0,rmax
              do n1=0,r
                do n2=0,r-n1
                  n3=r-n1-n2
                  cnt = cnt+1
                  D(0,n1,n2,n3) = fct(cnt)
                end do
              end do
              do n0=1,(r+1)/2
                do n1=0,r-2*n0+1
                  do n2=0,r-2*n0-n1+1
                    n3=r-2*n0-n1-n2+1
 
                    cnt = cnt+1
                    D(n0,n1,n2,n3) = fct(cnt)
                  end do
                end do
              end do

              do n0=2,(r+1)/2
                do n1=0,r-2*n0+1
                  do n2=0,r-2*n0-n1+1
                    n3=r-2*n0-n1-n2+1
 
                    cnt = cnt+1
                    Duv(n0,n1,n2,n3) = fct(cnt)
                  end do
                end do
              end do
              cnt = cnt+1
              Derr1(r) = real(fct(cnt))
              cnt = cnt+1
              Derr2(r) = real(fct(cnt))
            end do

            return
          end if


          if(rank.eq.rmax) then

            call CalcDred(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,id,Derr1,Derr2)

            if (wrica) then
              cnt = 0
              do r=0,rank
                do n1=0,r
                  do n2=0,r-n1
                    n3 = r-n1-n2
                    cnt = cnt+1
                    fct(cnt) = D(0,n1,n2,n3)
                  end do
                end do
                do n0=1,(r+1)/2
                  do n1=0,r-2*n0+1
                    do n2=0,r-2*n0-n1+1
                      n3 = r-2*n0-n1-n2+1
                      cnt = cnt+1
                      fct(cnt) = D(n0,n1,n2,n3)
                    end do
                  end do
                end do
                do n0=2,(r+1)/2
                  do n1=0,r-2*n0+1
                    do n2=0,r-2*n0-n1+1
                      n3 = r-2*n0-n1-n2+1
                      cnt = cnt+1
                      fct(cnt) = Duv(n0,n1,n2,n3)
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Derr1(r)
                cnt = cnt+1
                fct(cnt) = Derr2(r)
              end do
   
              if(rank.ge.3) then
                call WriteCache(fct,NCoefsG(rank,4)-NCoefs(rank-2,4)+NCoefs(rank-3,4)+2*(rank+1),id,4,rank)
              else if(rank.eq.2) then
                call WriteCache(fct,NCoefsG(rank,4)+1+2*(rank+1),id,4,rank)
              else
                call WriteCache(fct,NCoefsG(rank,4)+2*(rank+1),id,4,rank)
              end if            

            end if

            return
          
          
          else
            allocate(Daux(0:rank,0:rank,0:rank,0:rank))
            allocate(Duvaux(0:rank,0:rank,0:rank,0:rank))
            allocate(Derr1aux(0:rank))
            allocate(Derr2aux(0:rank))

            call CalcDred(Daux,Duvaux,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,id,Derr1aux,Derr2aux)

            if (wrica) then
              cnt = 0
              deallocate(fct)
              if(rank.ge.3) then 
                allocate(fct(NCoefsG(rank,4)-NCoefs(rank-2,4)+NCoefs(rank-3,4)+2*(rank+1)))
              else if(rank.eq.2) then 
                allocate(fct(NCoefsG(rank,4)+1+2*(rank+1)))
              else
                allocate(fct(NCoefsG(rank,4)+2*(rank+1)))
              end if

              do r=0,rank
                do n1=0,r
                  do n2=0,r-n1
                    n3 = r-n1-n2
                    cnt = cnt+1
                    fct(cnt) = Daux(0,n1,n2,n3)
                  end do
                end do
                do n0=1,(r+1)/2
                  do n1=0,r-2*n0+1
                    do n2=0,r-2*n0-n1+1
                      n3 = r-2*n0-n1-n2+1
                      cnt = cnt+1
                      fct(cnt) = Daux(n0,n1,n2,n3)
                    end do
                  end do
                end do
                do n0=2,(r+1)/2
                  do n1=0,r-2*n0+1
                    do n2=0,r-2*n0-n1+1
                      n3 = r-2*n0-n1-n2+1
                      cnt = cnt+1
                      fct(cnt) = Duvaux(n0,n1,n2,n3)
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Derr1aux(r)
                cnt = cnt+1
                fct(cnt) = Derr2aux(r)
              end do
! changed 25.10.18
!             do r=0,rank
!               do n0=0,r/2+1
!                 do n1=0,r-n0
!                   do n2=0,r-n0-n1
!                     n3 = r-n0-n1-n2
!
!                     cnt = cnt+1
!                     fct(cnt) = Daux(n0,n1,n2,n3)

!                   end do
!                 end do
!               end do
!               do n0=2,r/2+1
!                 do n1=0,r-n0
!                   do n2=0,r-n0-n1
!                     n3 = r-n0-n1-n2
!
!                     cnt = cnt+1
!                     fct(cnt) = Duvaux(n0,n1,n2,n3)

!                   end do
!                 end do
!               end do
!               cnt = cnt+1
!               fct(cnt) = Derr1aux(r)
!               cnt = cnt+1
!               fct(cnt) = Derr2aux(r)
!             end do

              if(rank.ge.3) then
                call WriteCache(fct,NCoefsG(rank,4)-NCoefs(rank-2,4)+NCoefs(rank-3,4)+2*(rank+1),id,4,rank)   
              else if(rank.eq.2) then
                call WriteCache(fct,NCoefsG(rank,4)+1+2*(rank+1),id,4,rank)
              else
                call WriteCache(fct,NCoefsG(rank,4)+2*(rank+1),id,4,rank)
              end if            

            end if

            D = Daux(0:rmax,0:rmax,0:rmax,0:rmax)
            Duv = Duvaux(0:rmax,0:rmax,0:rmax,0:rmax)
            Derr1 = Derr1aux(0:rmax)
            Derr2 = Derr2aux(0:rmax)

            deallocate(Daux)
            deallocate(Duvaux)
            deallocate(Derr1aux)
            deallocate(Derr2aux)

            return

!          end if
        end if
      end if
    end if
      
    call CalcDred(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr1,Derr2)

  end subroutine CalcD





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDred(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr1,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDred(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr1,Derr2)

    use globalD
  
    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
!    integer :: scheme_C0(rmax-1:rmax_C),scheme_C1(rmax-1:rmax_C)
!    integer :: scheme_C2(rmax-1:rmax_C),scheme_C3(rmax-1,rmax_C)
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: elimminf2_coli
    double precision, intent(out)  :: Derr1(0:rmax),Derr2(0:rmax)
    double precision :: D0est,Dtyp
    double complex :: D0_coli
!    double complex :: detX
    double complex :: chdet
    integer :: rmaxC,r,rid,n0,n1,n2,n3,g,gy,gp,gr,gm,gpf,i,iexp
    integer :: bin,k,nid(0:3)
    logical :: use_D0
    logical :: use_pv,use_pv2,use_g,use_gy,use_gp,use_gr,use_gm,use_gpf

    integer :: r_alt,Drmethod(0:rmax),DrCalc(0:rmax),DCalc
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex :: D_alt(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Duv_alt(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Derr(0:rmax),Derr_alt(0:rmax),Derr1_alt(0:rmax),Derr2_alt(0:rmax)
    integer :: Drmethod_alt(0:rmax)    

    double precision :: err_pv(0:rmax),err_pv2(0:rmax),err_g(0:rmax),err_gy(0:rmax),err_gp(0:rmax)
    double precision :: err_gr(0:rmax),err_gm(0:rmax),err_gpf(0:rmax)
    double precision :: h_pv,w_pv,v_pv,z_pv,h_pv2,w_pv2,v_pv2,z_pv2,hw_pv2
    double precision :: u_g,z_g,err_g_C,err_g_Cr,err_g_exp
    double precision :: u_gm,z_gm,err_gm_C,err_gm_Cr,err_gm_exp
    double precision :: v1_gy,b_gy,err_gy_C,err_gy_Cr,err_gy_exp
    double precision :: v_gp,v1_gp,z_gp,err_gp_C,err_gp_Cr,err_gp_exp
    double precision :: v1_gpf,b_gpf,err_gpf_C,err_gpf_Cr,err_gpf_exp
    double precision :: x_gr,y_gr,y1_gr,a_gr,err_gr_C,err_gr_Cr,err_gr_exp
    double precision :: err_C0,Cerr_i(0:rmax_C,0:3),err_C(0:rmax_C),err_D0,acc_D,errfac(0:3),err_req_D,err_inf,Cerr2_i(0:rmax_C,0:3)
    double precision :: checkest,norm,Dscale2
!    double precision :: deterr
    logical :: lerr_D0,errorwriteflag

    character(len=*),parameter :: fmt1 = "(A7,'dcmplx(',d25.18,' , ',d25.18,' )')"
    character(len=*),parameter :: fmt10 = "(A17,'(',d25.18,' , ',d25.18,' )')"

#ifdef CritPointsCOLI
    integer, parameter :: MaxCritPointD=50
#else
    integer, parameter :: MaxCritPointD=0
#endif
    integer, save :: CritPointCntD
    integer ncount

    data CritPointCntD /0/

#ifdef Dredtest
    write(*,*) 'CalcDred in',rmax,id,acc_req_D
    write(*,*) 'CalcDred acc_req',acc_req_D,reqacc_coli
    write(*,*) 'CalcDred in',p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
#endif
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! for rank = 0 calculate D0 directly 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use_D0 = .false.  
    if (rmax.eq.0d0) then

    ! rough estimate for D0 to set the scale, to be improved
      Dscale2 = max(abs(p10*p32),abs(p20*p31),abs(p30*p21), &
          abs(m02*m12),abs(m02*m22),abs(m02*m32),           &
          abs(m12*m22),abs(m12*m32),abs(m22*m32))
      if(Dscale2.ne.0d0) then 
        Derr1 = acc_inf/Dscale2
      else
        Derr1 = acc_inf
      end if
      Derr2 = Derr1

      Z(1,1) = 2d0*p10
      Z(2,1) = p10+p20-p21
      Z(3,1) = p10+p30-p31
      Z(1,2) = Z(2,1)
      Z(2,2) = 2d0*p20
      Z(3,2) = p20+p30-p32
      Z(1,3) = Z(3,1)
      Z(2,3) = Z(3,2)
      Z(3,3) = 2d0*p30
      mx(0,0) = 2d0*m02
      mx(1,0) = p10 - m12 + m02
      mx(2,0) = p20 - m22 + m02
      mx(3,0) = p30 - m32 + m02
      mx(0,1) = mx(1,0)
      mx(0,2) = mx(2,0)
      mx(0,3) = mx(3,0)
      mx(1:3,1:3) = Z(1:3,1:3)
      
      adetX = abs(chdet(4,mx))

! changed 16.08.2018
!      if (adetX.gt.dprec_cll*Dscale2**2) then
      if (adetX.gt.dprec_cll*maxval(abs(mx(0,:)))*maxval(abs(mx(1,:)))*maxval(abs(mx(2,:)))*maxval(abs(mx(3,:)))) then
        D(0,0,0,0) = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
        Duv(0,0,0,0) = 0d0
        if (D(0,0,0,0).ne.undefined_D) then         
          Derr1(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
        else
          Derr1(0) = acc_inf*abs(D(0,0,0,0))
        end if
        Derr2(0) = Derr1(0)
        if (Derr1(0).lt.acc_req_D*abs(D(0,0,0,0))) then 
          return
        else
          use_D0 = .true.
        endif
      end if 
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! calculate 3-point functions for rank < rmax 
    ! and corresponding accuracy estimates
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! allocation of C functions
    ! bad estimate of higher C coefficients leads to bad estimates for expansions -> not tried!
    ! do not involve estimate of C0 in extrapolations!    
    rmaxC = max(rmax-1,3)
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,0:3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,0:3))

    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! caution: C_i in first call not properly defined!
    call CalcC(C_i(:,:,:,0),Cuv_i(:,:,:,0),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(0:rmaxC,0),Cerr2_i(0:rmaxC,0))
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(0:rmaxC,1),Cerr2_i(0:rmaxC,1))
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(0:rmaxC,2),Cerr2_i(0:rmaxC,2))
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(0:rmaxC,3),Cerr2_i(0:rmaxC,3))

#ifdef Dredtest
    write(*,*) 'CalcDred Cerr 0 =',Cerr_i(0:rmaxC,0)
    write(*,*) 'CalcDred Cerr 1 =',Cerr_i(0:rmaxC,1)
    write(*,*) 'CalcDred Cerr 2 =',Cerr_i(0:rmaxC,2)
    write(*,*) 'CalcDred Cerr 3 =',Cerr_i(0:rmaxC,3)
    if (abs(C_i(0,0,0,0)).ne.0d0) &
        write(*,*) 'CalcDred Cacc 0 =',Cerr_i(0:rmaxC,0)/abs(C_i(0,0,0,0))
    if (abs(C_i(0,0,0,1)).ne.0d0) &
        write(*,*) 'CalcDred Cacc 1 =',Cerr_i(0:rmaxC,1)/abs(C_i(0,0,0,1))
    if (abs(C_i(0,0,0,2)).ne.0d0) &
        write(*,*) 'CalcDred Cacc 2 =',Cerr_i(0:rmaxC,2)/abs(C_i(0,0,0,2))
    if (abs(C_i(0,0,0,3)).ne.0d0) &
        write(*,*) 'CalcDred Cacc 3 =',Cerr_i(0:rmaxC,3)/abs(C_i(0,0,0,3))
#endif
 
!   acc_C(0:rmaxC)=max((Cerr_i(0:rmaxC,0))/abs(C_i(0,0,0,0)),  & 
!                      (Cerr_i(0:rmaxC,1))/abs(C_i(0,0,0,1)),  & 
!                      (Cerr_i(0:rmaxC,2))/abs(C_i(0,0,0,2)),  & 
!                      (Cerr_i(0:rmaxC,3))/abs(C_i(0,0,0,3))) 


    do i=0,3
! changed 01.07.2015 to avoid bad estimates that excluded expansions
!      errfac(i)=max(Cerr_i(rmaxC,i)/Cerr_i(rmaxC-1,i),sqrt(Cerr_i(rmaxC,i)/Cerr_i(rmaxC-2,i)))
       errfac(i) = 1d0
      do r=rmaxC+1,rmax_C
        Cerr_i(r,i)=Cerr_i(r-1,i)*errfac(i)
      end do
    end do

    do r=0,rmax_C
      err_C(r)=maxval(Cerr_i(r,0:3))
    end do

    err_C0=err_C(0)

#ifdef Dredtest
    write(*,*) 'CalcDred err_C0:',err_C0
    write(*,*) 'CalcDred Cerr 0 =',Cerr_i(0:rmax_C,0)
    write(*,*) 'CalcDred Cerr 1 =',Cerr_i(0:rmax_C,1)
    write(*,*) 'CalcDred Cerr 2 =',Cerr_i(0:rmax_C,2)
    write(*,*) 'CalcDred Cerr 3 =',Cerr_i(0:rmax_C,3)
    write(*,*) 'CalcDred Cerr   =', err_C(0:rmax_C)
#endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! choose reduction scheme
    ! by estimating expected errors
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! eliminate infinitesimal masses
    mm02 = elimminf2_coli(m02)
    mm12 = elimminf2_coli(m12)
    mm22 = elimminf2_coli(m22)
    mm32 = elimminf2_coli(m32)
    q10  = elimminf2_coli(p10)
    q21  = elimminf2_coli(p21)
    q32  = elimminf2_coli(p32)
    q30  = elimminf2_coli(p30)
    q31  = elimminf2_coli(p31)
    q20  = elimminf2_coli(p20)

    ! set mass scales
    q2max = max(abs(q10),abs(q21),abs(q32),abs(q30),abs(q31),abs(q20))
    m2max = max(abs(mm02),abs(mm12),abs(mm22),abs(mm32))
    m2scale = max(q2max,m2max)

    ! Gram and related stuff
!   q1q2 = (q10+q20-q21)/2D0
!   q1q3 = (q10+q30-q31)/2D0
!   q2q3 = (q20+q30-q32)/2D0

    Z(1,1) = 2d0*q10
    Z(2,1) = q10+q20-q21
    Z(3,1) = q10+q30-q31
    Z(1,2) = Z(2,1)
    Z(2,2) = 2d0*q20
    Z(3,2) = q20+q30-q32
    Z(1,3) = Z(3,1)
    Z(2,3) = Z(3,2)
    Z(3,3) = 2d0*q30
!    write(*,*) 'Zn ',Z

    maxZ = maxval(abs(Z))

! changed 21.06.2018
! deterr added 17.01.2019
    call chinve(3,Z,Zinv,detZ,detZerr)
!    detZ = chdet(3,Z)

!  write(*,*) 'detZ = ',detZ/maxZ**3,detZerr,dprec_cll,log(detZerr)/log(10d0)


! added 17.01.2019
    if (detZerr.lt.dprec_cll) detZ = 0d0

#ifdef OBSOLETE
! added 16.08.2018
!    if (abs(detZ).lt.dprec_cll*maxval(abs(Z(1,:)))*maxval(abs(Z(2,:)))*maxval(abs(Z(3,:)))) then
    if (abs(detZ).lt.dprec_cll*max(  &
        abs(Z(1,1)*Z(2,2)*Z(3,3)),abs(Z(1,2)*Z(2,3)*Z(3,1)),   &
        abs(Z(1,1)*Z(2,3)*Z(3,2)),abs(Z(2,2)*Z(1,3)*Z(3,1)),   &
        abs(Z(3,3)*Z(1,2)*Z(2,1))) ) then 
#ifdef Dredtest
    write(*,*) 'detZ set to 0  ',abs(detZ),   &  
        dprec_cll*maxval(abs(Z(1,:)))*maxval(abs(Z(2,:))*maxval(abs(Z(3,:)))),  &
        dprec_cll*max(  &
        abs(Z(1,1)*Z(2,2)*Z(3,3)),abs(Z(1,2)*Z(2,3)*Z(3,1)),   &
        abs(Z(1,1)*Z(2,3)*Z(3,2)),abs(Z(2,2)*Z(1,3)*Z(3,1)),   &
        abs(Z(3,3)*Z(1,2)*Z(2,1))) 
#endif
      detZ = 0d0
    end if
#endif

    if (detZ.ne.0d0) then
!      call chinv(3,Z,Zinv)
      Zadj = Zinv * detZ
    else
!     Zadj(1,1) = 4d0*(q30*q20-q2q3*q2q3)
!     Zadj(2,1) = 4d0*(q1q3*q2q3-q30*q1q2)
!     Zadj(3,1) = 4d0*(q1q2*q2q3-q20*q1q3)
!     Zadj(1,2) = Zadj(2,1)
!     Zadj(2,2) = 4d0*(q10*q30-q1q3*q1q3)
!     Zadj(3,2) = 4d0*(q1q2*q1q3-q10*q2q3)
!     Zadj(1,3) = Zadj(3,1)
!     Zadj(2,3) = Zadj(3,2)
!     Zadj(3,3) = 4d0*(q10*q20-q1q2*q1q2)

      Zadj(1,1) = (Z(3,3)*Z(2,2)-Z(2,3)*Z(2,3))
      Zadj(2,1) = (Z(1,3)*Z(2,3)-Z(3,3)*Z(1,2))
      Zadj(3,1) = (Z(1,2)*Z(2,3)-Z(2,2)*Z(1,3))
      Zadj(1,2) = Zadj(2,1)
      Zadj(2,2) = (Z(1,1)*Z(3,3)-Z(1,3)*Z(1,3))
      Zadj(3,2) = (Z(1,2)*Z(1,3)-Z(1,1)*Z(2,3))
      Zadj(1,3) = Zadj(3,1)
      Zadj(2,3) = Zadj(3,2)
      Zadj(3,3) = (Z(1,1)*Z(2,2)-Z(1,2)*Z(1,2))
    endif
!    write(*,*) 'Zadjn ',Zadj
!    write(*,*) 'detZn ',detZ

#ifdef Dredtest
    Zadjs(1) = q20*(q31+q32-q21) + q31*(q32-q30)   &
         + q30*(q32+q21) - q32*(q32+2d0*q10-q21)
    write(*,*) 'Zadjs(1) ',Zadjs(1), Zadj(1,1)+Zadj(1,2)+Zadj(1,3)
    Zadjs(3) = q20*(q21+q31-q32) + (q31-q21)*(q21-q10)  &
         - 2d0*q30*q21 + q32*(q10+q21)
    write(*,*) 'Zadjs(3) ',Zadjs(3), Zadj(3,1)+Zadj(3,2)+Zadj(3,3)
#endif

    Zadjs(1) = q32*(-2d0*q10 + q20+q30+q31+q21 - q32) &
         + (q21-q31)*(q30-q20)
    Zadjs(2) = q31*(-2d0*q20 + q10+q30+q32+q21 - q31) &
         + (q21-q32)*(q30-q10)
    Zadjs(3) = q21*(-2d0*q30 + q10+q20+q32+q31 - q21) &
         + (q31-q32)*(q20-q10)

#ifdef Dredtest
    write(*,*) 'Zadjs(1) ',Zadjs(1), Zadj(1,1)+Zadj(1,2)+Zadj(1,3)
    write(*,*) 'Zadjs(2) ',Zadjs(2), Zadj(2,1)+Zadj(2,2)+Zadj(2,3)
    write(*,*) 'Zadjs(3) ',Zadjs(3), Zadj(3,1)+Zadj(3,2)+Zadj(3,3)
#endif

    detZmZadjf = -2*q21*q31*q32 + q30*q21*(-q21 + q31 + q32) & 
         + q20*q31*(q21 - q31 + q32) + q10*q32*(q21 + q31 - q32)
         
    adetZ = abs(detZ)
    maxZadj = max(abs(Zadj(1,1)),abs(Zadj(2,1)),abs(Zadj(3,1)),  &
                  abs(Zadj(2,2)),abs(Zadj(3,2)),abs(Zadj(3,3)))

    f(1) = q10+mm02-mm12
    f(2) = q20+mm02-mm22
    f(3) = q30+mm02-mm32
    fmax = max(abs(f(1)),abs(f(2)),abs(f(3)))
      
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(0,1) = mx(1,0)
    mx(0,2) = mx(2,0)
    mx(0,3) = mx(3,0)
    mx(1:3,1:3) = Z(1:3,1:3)
!    write(*,*) 'Xn ',mx

! changed 21.06.2018
! deterr added 17.01.2019
    call chinve(4,mx,mxinv,detX,detXerr)
!    detX = chdet(4,mx)

#ifdef Dredtest
    write(*,*) 'reductionD detX = ',((p20-m22-m02)*(p31-m32-m12))**2
    write(*,*) 'reductionD detX = ',detX,detXerr,dprec_cll/detXerr
#endif

! added 17.01.2019
    if (detXerr.lt.dprec_cll) detX = 0d0

! added 16.08.2018   
! commented out 7.01.2019, IR singular D0 more stable for small finite det
#ifdef OBSOLETE
    if (abs(detX).lt.dprec_cll*max(  &
        abs(mx(0,0)*mx(1,1)*mx(2,2)*mx(3,3)),abs(mx(0,1)*mx(1,2)*mx(2,3)*mx(3,0)),   &
        abs(mx(0,2)*mx(1,3)*mx(2,0)*mx(3,1)),abs(mx(0,3)*mx(1,0)*mx(2,1)*mx(3,2)))   &    
        ) then 
#ifdef Dredtest
      write(*,*) 'detX set to 0  ',abs(detX),     &
          dprec_cll*maxval(abs(mx(0,:)))*maxval(abs(mx(1,:)))    &
          *maxval(abs(mx(2,:))*maxval(abs(mx(3,:)))),  &
      dprec_cll*max(  &
        abs(mx(0,0)*mx(1,1)*mx(2,2)*mx(3,3)),abs(mx(0,1)*mx(1,2)*mx(2,3)*mx(3,0)),   &
        abs(mx(0,2)*mx(1,3)*mx(2,0)*mx(3,1)),abs(mx(0,3)*mx(1,0)*mx(2,1)*mx(3,2)))&
      ,dprec_cll,max(  &
        abs(mx(0,0)*mx(1,1)*mx(2,2)*mx(3,3)),abs(mx(0,1)*mx(1,2)*mx(2,3)*mx(3,0)),   &
        abs(mx(0,2)*mx(1,3)*mx(2,0)*mx(3,1)),abs(mx(0,3)*mx(1,0)*mx(2,1)*mx(3,2)))
      write(*,*) 'detX here ',q20*q20*q31*q31
#endif
!      detX = 0d0
    endif
#endif

    if (detX.ne.0d0.and.maxZ.ne.0d0) then  ! more stable for small detX!?
                                           ! for D(3e5,0,0,0,4e6,-4e-3,0,0,0,0)
!      call chinv(4,mx,mxinv)
      Xadj = mxinv * detX

      Zadjf(1:3) = -Xadj(0,1:3)
    
      Zadj2ff(1:3,1:3) = Xadj(1:3,1:3) - 2d0*mm02*Zadj(1:3,1:3)
    else
      Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
      Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
      Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)

      Zadj2ff(1,1) = -f(2)*f(2)*Z(3,3) - f(3)*f(3)*Z(2,2)  &
                   + 2*f(2)*f(3)*Z(3,2)
      Zadj2ff(2,1) = f(2)*f(1)*Z(3,3) - f(3)*f(1)*Z(2,3)  &
                   - f(2)*f(3)*Z(1,3) + f(3)*f(3)*Z(1,2) 
      Zadj2ff(3,1) = -f(2)*f(1)*Z(3,2) + f(3)*f(1)*Z(2,2)  &
                   + f(2)*f(2)*Z(3,1) - f(3)*f(2)*Z(2,1)
      Zadj2ff(1,2) = Zadj2ff(2,1)
      Zadj2ff(2,2) = -f(1)*f(1)*Z(3,3) - f(3)*f(3)*Z(1,1)  &
                   + 2*f(1)*f(3)*Z(1,3)
      Zadj2ff(3,2) = f(1)*f(1)*Z(2,3) - f(1)*f(2)*Z(1,3)  &
                   - f(3)*f(1)*Z(2,1) + f(3)*f(2)*Z(1,1)
      Zadj2ff(1,3) = Zadj2ff(3,1)
      Zadj2ff(2,3) = Zadj2ff(3,2) 
      Zadj2ff(3,3) = -f(1)*f(1)*Z(2,2) - f(2)*f(2)*Z(1,1)  &
                   + 2*f(1)*f(2)*Z(2,1)

      Xadj(1,1) = 2d0*mm02*Zadj(1,1) + Zadj2ff(1,1)
      Xadj(2,1) = 2d0*mm02*Zadj(2,1) + Zadj2ff(2,1)
      Xadj(3,1) = 2d0*mm02*Zadj(3,1) + Zadj2ff(3,1)
      Xadj(1,2) = Xadj(2,1)
      Xadj(2,2) = 2d0*mm02*Zadj(2,2) + Zadj2ff(2,2)
      Xadj(3,2) = 2d0*mm02*Zadj(3,2) + Zadj2ff(3,2)
      Xadj(1,3) = Xadj(3,1)
      Xadj(2,3) = Xadj(3,2)
      Xadj(3,3) = 2d0*mm02*Zadj(3,3) + Zadj2ff(3,3)
    endif

!   write(*,*) 'Xadjn ',Xadj
!   write(*,*) 'detXn ',detX
!   write(*,*) 'Zadjfn',Zadjf
!   write(*,*) 'Zadj2ffn',Zadj2ff

    maxZadj2ff = max(abs(Zadj2ff(1,1)),abs(Zadj2ff(2,1)),abs(Zadj2ff(3,1)),  &
                     abs(Zadj2ff(2,2)),abs(Zadj2ff(3,2)),abs(Zadj2ff(3,3)))
    maxZadjf = max(abs(Zadjf(1)),abs(Zadjf(2)),abs(Zadjf(3)))
    maxZadjfd = max(maxZadjf,adetZ)
    Zadjff = Zadjf(1)*f(1)+Zadjf(2)*f(2)+Zadjf(3)*f(3)
    aZadjff = abs(Zadjff)
! changed 16.08.2018
!    adetX = abs(2d0*mm02*detZ-Zadjf(1)*f(1)-Zadjf(2)*f(2)-Zadjf(3)*f(3))
    adetX = abs(detX)    

!    write(*,*) 'fs', f(1), f(2), f(3)
!    write(*,*) aZadjff, maxZadjf*fmax

    maxXadj = max(abs(Xadj(1,1)),abs(Xadj(2,1)),abs(Xadj(3,1)),  &
                  abs(Xadj(2,2)),abs(Xadj(3,2)),abs(Xadj(3,3)))

!    write(*,*) 'CalcDred acc_inf=',acc_inf
!    write(*,*) 'CalcDred Derr=',Derr

#ifdef Dredtest
    write(*,*) 'CalcDred adetX ',adetX,maxZadjf,maxXadj,adetZ
    write(*,*) 'CalcDred Zadj2ff',maxZadj2ff
    write(*,*) 'CalcDred maxZadj',maxZadj
    write(*,*) 'CalcDred Zadjf',Zadjf
    write(*,*) 'CalcDred f',f
    write(*,*) 'CalcDred Zadjff',Zadjff
#endif

    Zadj2f = 0d0
    Zadj2f(1,2,1) =  Z(3,2)*f(3) - Z(3,3)*f(2)
    Zadj2f(1,3,1) = -Z(2,2)*f(3) + Z(2,3)*f(2)
    Zadj2f(2,3,1) =  Z(1,2)*f(3) - Z(1,3)*f(2)
    Zadj2f(1,2,2) = -Z(3,1)*f(3) + Z(3,3)*f(1)
    Zadj2f(1,3,2) =  Z(2,1)*f(3) - Z(2,3)*f(1)
    Zadj2f(2,3,2) = -Z(1,1)*f(3) + Z(1,3)*f(1)
    Zadj2f(1,2,3) =  Z(3,1)*f(2) - Z(3,2)*f(1)
    Zadj2f(1,3,3) = -Z(2,1)*f(2) + Z(2,2)*f(1)
    Zadj2f(2,3,3) =  Z(1,1)*f(2) - Z(1,2)*f(1)
    Zadj2f(2,1,1) =  -Zadj2f(1,2,1)
    Zadj2f(3,1,1) =  -Zadj2f(1,3,1)
    Zadj2f(3,2,1) =  -Zadj2f(2,3,1)
    Zadj2f(2,1,2) =  -Zadj2f(1,2,2)
    Zadj2f(3,1,2) =  -Zadj2f(1,3,2)
    Zadj2f(3,2,2) =  -Zadj2f(2,3,2)
    Zadj2f(2,1,3) =  -Zadj2f(1,2,3)
    Zadj2f(3,1,3) =  -Zadj2f(1,3,3)
    Zadj2f(3,2,3) =  -Zadj2f(2,3,3)

    maxZadj2f=maxval(abs(Zadj2f))

!    write(*,*) 'CalcDred Zadj2f',maxZadj2f

!    write(*,*) 'CalcDred m2scale',m2scale

    !   1/sqrt(adetX) seems to describe scale of D0 well
    !   scale ratio between D0 and C0's better described by maximal scale (missing in at least one C0 function) 
    !    m2scale = sqrt(adetX)/q2max

!    write(*,*) 'CalcDred m2scale',m2scale
!    write(*,*) 'CalcDred 1/sX  ',1/sqrt(adetX)

!    Zadj2ff = Zadj2f(:,1,:)*f(1)+Zadj2f(:,2,:)*f(2)+Zadj2f(:,3,:)*f(3)

!    write(*,*) 'CalcDred Zadj2ff',Zadj2ff


    ! quantities for modified error estimates
    ! momentum weights
!    do i = 1,3
!      pweight(i) = max(abs(Z(i,1))/maxval(abs(Z(1:3,1))),  &
!          abs(Z(i,2))/maxval(abs(Z(1:3,2))),               & 
!          abs(Z(i,3))/maxval(abs(Z(1:3,3))))
!    end do

!    wmaxZadj = max(pweight(1)*abs(Zadj(1,1)), &
!        pweight(1)*abs(Zadj(1,2)),pweight(1)*abs(Zadj(1,3)),  &
!        pweight(2)*abs(Zadj(2,1)),pweight(3)*abs(Zadj(3,1)),  &
!        pweight(2)*abs(Zadj(2,3)),pweight(3)*abs(Zadj(3,2)),  &
!        pweight(2)*abs(Zadj(2,2)),pweight(3)*abs(Zadj(3,3)))

!    wmaxZadjf = max(pweight(1)*abs(Zadjf(1)),pweight(2)*abs(Zadjf(2)),  &
!        pweight(3)*abs(Zadjf(3)))

!    wmaxXadj = max(pweight(1)*abs(Xadj(1,1)),   &
!        pweight(1)*abs(Xadj(1,2)),pweight(1)*abs(Xadj(1,3)),  &
!        pweight(2)*abs(Xadj(2,1)),pweight(2)*abs(Xadj(2,3)),  &
!        pweight(3)*abs(Xadj(3,1)),pweight(3)*abs(Xadj(3,2)),  &
!        pweight(2)*abs(Xadj(2,2)),pweight(3)*abs(Xadj(3,3)))
!    wmaxXadj = max(2d0*abs(mm02)*sqrt(adetZ*maxZadj/maxZ),maxZadj2ff*maxZadjf/(maxZadj*fmax))

!    write(*,*) 'CalcDred pweight',pweight(1:3)
!    write(*,*) 'CalcDred wmaxZadj',maxZadj,wmaxZadj
!    write(*,*) 'CalcDred wmaxZadjf',maxZadjf,wmaxZadjf
!    write(*,*) 'CalcDred wmaxZadjf',maxXadj,wmaxXadj

    ! rough estimate for D0 to set the scale, to be improved
     Dscale2 = max(abs(p10*p32),abs(p21*p30),abs(p20*p31),abs(m02*m02), &
              abs(m12*m12),abs(m22*m22),abs(m32*m32))
#ifdef USED0
    D0est = max(abs(D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)),  &
             1d0/Dscale2)
    lerr_D0 = .true.
#else
! changed 09.09.16
     if(Dscale2.ne.0d0) then 
       D0est = 1d0/Dscale2
     else
       D0est = 1d0
     end if
!    if (adetX.ne.0d0) then
!      D0est = 1d0/sqrt(adetX)
!    elseif (m2max.ne.0d0) then
!      D0est = 1d0/m2max**2
!    else if (maxZ.ne.0d0) then
!      D0est = 1d0/maxZ**2
!    else
!      D0est = 1d0 
!    endif
    lerr_D0 = .false.
#endif
    err_inf = acc_inf*D0est
    Dtyp = D0est

#ifdef Dredtest
    if (adetX.ne.0d0) write(*,*) 'D0est',1d0/sqrt(adetX)
    if (m2max.ne.0d0) write(*,*) 'D0est',1d0/m2max**2
    if (maxZ.ne.0d0) write(*,*) 'D0est',1d0/maxZ**2
    write(*,*) 'D0est',D0est
#endif

    DCalc = 0
    DrCalc = 0
    Drmethod = 0
!    Derr1 = err_inf    !  shifted above D0
!    Derr2 = err_inf
    Derr  = err_inf
    acc_D = acc_inf
    DCount(0) = DCount(0)+1

    ! error estimate of D0
    if (adetX.ne.0d0) then
      err_D0 = acc_def_D0*max( D0est, 1d0/sqrt(adetX) )
    else
      err_D0 = acc_def_D0*D0est
    endif     

    err_req_D = acc_req_D * D0est

!   write(*,*) 'CalcDred err_req ',err_req_D,acc_req_D , D0est

    ! estimate accuracy of PV-reduction
    h_pv = real(undefined_D)
    w_pv = real(undefined_D)
    v_pv = real(undefined_D)
    z_pv = real(undefined_D)

!   14.07.2017
!   16.08.2018 changed back, since detZ=0 set above if too small
    if (adetZ.lt.dprec_cll*maxZadjf.or.adetZ.eq.0d0) then
!    if (adetZ.lt.dprec_cll*maxZadjf.or.adetZ.lt.dprec_cll*maxZ**3.or.adetZ.eq.0d0) then
      use_pv = .false.
      err_pv = err_inf
    else
      use_pv = .true.
      if (rmax.eq.0) then
        err_pv(0) = err_D0
      else 
#ifdef PVEST2
        h_pv = sqrt(adetZ/(maxZ*maxZadj))
        w_pv = max((maxZadjf*h_pv/adetZ)**2, abs(mm02)*maxZadj*h_pv/adetZ, aZadjff*maxZadj*(h_pv/adetZ)**2)
        v_pv = maxZadjf*h_pv/adetZ
        z_pv = maxZadj*h_pv/adetZ
#else
        w_pv = max((maxZadjf/adetZ)**2, abs(mm02)*maxZadj/adetZ, aZadjff*maxZadj/adetZ**2)
        v_pv = maxZadjf/adetZ
        z_pv = maxZadj/adetZ
#endif
        if (mod(rmax,2).eq.1) then
          err_pv(rmax) = max( w_pv**((rmax-1)/2) * v_pv * err_D0,  &
                        w_pv**((rmax-1)/2) * z_pv * err_C0, z_pv * err_C(rmax-1) )
#ifdef Dredtest
          write(*,*) 'CalcDred w_pv: ',w_pv,v_pv,z_pv,err_D0,err_C0,rmax

          write(*,*) 'CalcDred err_pv con: ',err_pv(rmax), w_pv**((rmax-1)/2) * v_pv * err_D0,  &
                        w_pv**((rmax-1)/2) * z_pv * err_C0, z_pv * err_C(rmax-1)
#endif
        else
          err_pv(rmax) = max( w_pv**(rmax/2) * err_D0,  &
                        w_pv**(rmax/2-1) * v_pv * z_pv * err_C0, z_pv * err_C(rmax-1) )
#ifdef Dredtest
         write(*,*) 'CalcDred w_pv: ',w_pv,v_pv,z_pv
         write(*,*) 'CalcDred err_pv con: ',err_pv(rmax), w_pv**((rmax)/2) * err_D0,  &
                       w_pv**(rmax/2-1) * v_pv * z_pv * err_C0, z_pv * err_C(rmax-1)
         write(*,*) 'CalcDred err_pv con: ',err_pv(rmax), w_pv**((rmax)/2),err_D0,  &
                       w_pv**(rmax/2-1) * v_pv,err_C0, z_pv,err_C(rmax-1)
#endif
        end if

      ! added 13.09.2022  needed?, if yes implement also in pv2 and C
      ! err_pv(rmax) = max( err_pv(rmax), D0est*dprec_cll/detZerr)
      ! write(*,*) 'err_pv = ',err_pv(rmax), D0est*dprec_cll/detZerr, D0est,dprec_cll/detZerr
        
      end if
    end if


    ! estimate accuracy of alternative PV-reduction
    w_pv2 = real(undefined_D)
    h_pv2 = real(undefined_D)
    hw_pv2 = real(undefined_D)
    v_pv2 = real(undefined_D)
    z_pv2 = real(undefined_D)

!   14.07.2017   
!   16.08.2018 changed back, since detZ=0 set above if too small
    if ((adetZ.lt.dprec_cll*maxZadjf).or.(adetX.lt.dprec_cll*maxval(abs(mx))*adetZ).or.adetZ.eq.0d0.or.adetX.eq.0d0) then
!    if ((adetZ.lt.dprec_cll*maxZadjf).or.(adetX.lt.dprec_cll*maxval(abs(mx))*adetZ).or.  &
!         (adetZ.lt.dprec_cll*maxZ**3).or.adetZ.eq.0d0.or.adetX.eq.0d0) then
      use_pv2 = .false.
      err_pv2 = err_inf
    else
      use_pv2 = .true.
      if (rmax.eq.0) then
        err_pv2(0) = err_D0
      else
        w_pv2 = maxZadjf/adetZ
#ifdef PVEST2
        h_pv2 = sqrt(adetZ/(maxZ*maxZadj))
        hw_pv2 =  w_pv2*h_pv2
#else
        hw_pv2 =  w_pv2 
#endif
        v_pv2 = maxXadj/adetZ
        z_pv2 = adetZ/adetX 

#ifdef Dredtest
        write(*,*) 'CalcDred w_pv2: ',w_pv2,v_pv2,z_pv2
        write(*,*) 'CalcDred w_pv2: ',mod(rmax,2).eq.1
#endif

        if (mod(rmax,2).eq.1) then
! change 21.10.15 for PVEST2
!          err_pv2(rmax) = max( err_D0 * max(w_pv2**rmax,    &
!               w_pv2*v_pv2**((rmax-1)/2) ),  &
!               err_C0 * z_pv2*max(w_pv2**(rmax+1), &
!                                  w_pv2*v_pv2**((rmax-1)/2), & 
!                                  v_pv2**((rmax+1)/2)),  &
!               err_C(rmax-1) * z_pv2 * max(w_pv2,w_pv2**2,v_pv2) )
          err_pv2(rmax) = max( err_D0 * max(hw_pv2**rmax,    &
                               hw_pv2*v_pv2**((rmax-1)/2) ),  &
               err_C0 * z_pv2*max(w_pv2*hw_pv2**(rmax), &
                                  max(1d0,w_pv2)*hw_pv2*v_pv2**((rmax-1)/2), & 
                                  v_pv2**((rmax+1)/2)),  &
               err_C(rmax-1) * z_pv2 * max(hw_pv2,hw_pv2*w_pv2,v_pv2)  )

#ifdef Dredtest
        write(*,*) 'CalcDred err_pv2: ',   &
        err_pv2(rmax) ,     err_D0,err_D0*w_pv2**rmax,err_D0*v_pv2**((rmax-1)/2),    &
                                     err_D0*w_pv2*v_pv2**((rmax-1)/2)
       write(*,*) 'CalcDred err_pv2: '  &
       ,err_pv2(rmax) ,     err_D0 * max(1d0,w_pv2**rmax,v_pv2**((rmax-1)/2),    &
                                   w_pv2*v_pv2**((rmax-1)/2) )  &
                     , err_C0 * z_pv2*max(w_pv2**(rmax+1), &
                               w_pv2*v_pv2**((rmax-1)/2), & 
                               v_pv2**((rmax+1)/2))  &
                    ,  err_C(rmax-1) * max(z_pv2*w_pv2, &
                               z_pv2*w_pv2**2,z_pv2*v_pv2) 
        write(*,*) 'CalcDred err_pv2: ',   &
                       err_C0 * z_pv2*w_pv2**(rmax+1), &
                        err_C0 * z_pv2* w_pv2*v_pv2**((rmax-1)/2), & 
                       err_C0 * z_pv2*  v_pv2**((rmax+1)/2)    ,  &
                       err_C0
#endif

        else
! change 21.10.15 for PVEST2
!          err_pv2(rmax) = max( err_D0 * max(w_pv2**rmax,v_pv2**(rmax/2)),  & 
!                err_C0 * z_pv2 * max(w_pv2**(rmax+1),  &
!                         v_pv2**(rmax/2),w_pv2*v_pv2**(rmax/2)), &
!                err_C(rmax-1) * z_pv2 * max(w_pv2, w_pv2**2, v_pv2) )
          err_pv2(rmax) = max( err_D0 * max(hw_pv2**rmax,v_pv2**(rmax/2)),  & 
                err_C0 * z_pv2 * max(w_pv2*hw_pv2**(rmax),  &
                         v_pv2**(rmax/2),w_pv2*v_pv2**(rmax/2)), &
                err_C(rmax-1) * z_pv2 * max(hw_pv2,hw_pv2*w_pv2, v_pv2) )
                                  

!         write(*,*) 'CalcDred err_pv2: ',   &
!         err_pv2(rmax) , err_D0 * max(1d0,w_pv2**rmax,v_pv2**(rmax/2)),  &
!                        err_C0 * z_pv2 * max(w_pv2**(rmax+1),  &
!                                 v_pv2**(rmax/2),w_pv2*v_pv2**(rmax/2)), &
!                        err_C(rmax-1) * max(1d0,z_pv2*w_pv2,  &
!                                 z_pv2*w_pv2**2,z_pv2*v_pv2)
!         write(*,*) 'CalcDred err_pv2: ',   &
!                        err_C0 * z_pv2 * w_pv2**(rmax+1),  &
!                        err_C0 * z_pv2 *          v_pv2**(rmax/2), &
!                        err_C0 * z_pv2 * w_pv2*v_pv2**(rmax/2), &
!                        err_C0
        end if
      end if
    end if 

    ! scale estimates down to allow trying other methods
    err_pv(rmax)  = err_pv(rmax)/impest_D
    err_pv2(rmax) = err_pv2(rmax)/impest_D     

!    write(*,*) 'CalcDred err_pv: ',err_pv, w_pv**((rmax-1)/2) * v_pv * err_D0,  &
!                        w_pv**((rmax-1)/2) * z_pv * err_C0, z_pv * err_C(rmax-1)

#ifdef Dredtest
    write(*,*) 'CalcDred: err_pv',err_pv(rmax),err_pv2(rmax),err_req_D
    write(*,*) 'CalcDred: acc_pv',err_pv(rmax)/D0est,err_pv2(rmax)/D0est,acc_req_D
#endif

#ifdef TEST
!  use_pv = .false.
!  use_pv2 = .false.
!  use_pv = .true.
!  use_pv2 = .true.
!  err_pv2(rmax) = 1d30
!  err_pv(rmax) = 1d30
#endif


!    Dtyp = real(undefined_D)
    Dtyp = D0est
#ifdef ALWAYSPV
    if(use_pv.or.use_pv2) then
#else
    if (min(err_pv(rmax),err_pv2(rmax)).le.err_req_D) then
#endif
      if (err_pv(rmax).le.err_pv2(rmax)) then

#ifdef Dredtest
        write(*,*) 'CalcDred: call Dpv 1 ',rmax,id,err_pv(rmax)
#endif

        ! use PV-reduction if appropriate
        call CalcDpv1(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr1,Derr2)
#ifdef PVEST2
        Derr = Derr2
#else
        Derr = Derr1
#endif

        DCount(1) = DCount(1)+1
        DrCalc(0:rmax) = DrCalc(0:rmax)+1
        DCalc = DCalc+1
        Drmethod(0:rmax) = 1
!        err_D = err_pv

#ifdef Dredtest
        checkest=Derr(rmax)/err_pv(rmax)
        if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
          write(*,*) 'CalcDred: estimate err_pv imprecise',err_pv(rmax),Derr(rmax)
        end if
#endif

        err_pv=Derr

      else

#ifdef Dredtest
        write(*,*) 'CalcDred: call Dpv2 1',rmax,id,err_pv2(rmax)
#endif

        ! use alternative PV-reduction if appropriate
        call CalcDpv2(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr1,Derr2)
#ifdef PVEST2
        Derr = Derr2
#else
        Derr = Derr1
#endif
        DCount(2) = DCount(2)+1
        DrCalc(0:rmax)=DrCalc(0:rmax)+2
        DCalc = DCalc+2
        Drmethod(0:rmax)=2

#ifdef Dredtest
        checkest=Derr(rmax)/err_pv2(rmax)
        if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
          write(*,*) 'CalcDred: estimate err_pv2 imprecise',err_pv2(rmax),Derr(rmax)
        end if
#endif
        err_pv2=Derr

      end if

#ifndef USED0
      ! refine error estimate for D0
!      D0est =  abs(D(0,0,0,0))
      err_D0 = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
      err_req_D = acc_req_D * abs(D(0,0,0,0))
      lerr_D0 = .true.
#endif

      if (rmax.ge.1) then
        Dtyp =  max(abs(D(0,0,0,0)),        &
               abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
      else
        Dtyp =  abs(D(0,0,0,0))
      end if
      if(Dtyp.eq.0d0) Dtyp = D0est
      err_req_D = acc_req_D * Dtyp


#ifdef Dredtest
      write(*,*) 'CalcDred Derr1 after PV = ',Derr1
!      write(*,*) 'CalcDred Dacc1 after PV = ',Derr1/abs(D(0,0,0,0))
      write(*,*) 'CalcDred Dacc1 after PV = ',Derr1/Dtyp
      write(*,*) 'CalcDred err1_D',Derr1(rmax)
      write(*,*) 'CalcDred Derr2 after PV = ',Derr2
!      write(*,*) 'CalcDred Dacc2 after PV = ',Derr2/abs(D(0,0,0,0))
      write(*,*) 'CalcDred Dacc2 after PV = ',Derr2/Dtyp
      write(*,*) 'CalcDred err2_D',Derr2(rmax)
#endif

!  Derr = Derr2 might lead to imprecise results
      if (Derr1(rmax).lt.err_req_D) then
        DCount(DCalc+DCountoffset0) = DCount(DCalc+DCountoffset0)+1
        return
      end if

    else if (.not.use_D0) then  !  added 14.07.2017 adapted 12.4.2018
      D = 0d0
      Duv = 0d0
      Derr1 = err_inf
      Derr2 = err_inf
    end if

#ifdef Dredtest
!   return
#endif

!    allocate(D_alt(0:rmax,0:rmax,0:rmax,0:rmax))
!    allocate(Duv_alt(0:rmax,0:rmax,0:rmax,0:rmax))
!    allocate(Derr1_alt(0:rmax))
!    allocate(Derr2_alt(0:rmax))
!    allocate(Drmethod_alt(0:rmax))

    ! choose most promising expansion scheme
    ! Gram expansion    
!    if (maxZadjf.ne.0d0) then
    if (maxZadjf.gt.m2scale**3*dprec_cll) then   !  10.07.2017
      x_g = adetZ/maxZadjf
!      u_g = max(1d0,maxZadj2ff/maxZadjf/4d0,abs(mm02)*maxZadj/maxZadjf/4d0)
! 03.03.15   large P counts!
!      u_g = max(1d0,maxZadj2ff/maxZadjf/2d0,abs(mm02)*maxZadj/maxZadjf/2d0)
! 24.04.15  term appear only combined
      u_g = max(1d0,maxXadj/maxZadjf/2d0)
      fac_g = x_g*u_g
      err_g = err_inf
      g = -1
      if (fac_g.ge.1) then
        use_g = .false.
        err_g_exp = err_inf
        err_g_C = err_C(rmax)         ! dummy
        err_g_Cr = real(undefined_D)
        z_g = real(undefined_D)
      else
        use_g = .true.
!       z_g = max(1d0,m2scale*maxZadj/maxZadjf)
        z_g = maxZadj/maxZadjf
        err_g_Cr = max( err_C(rmax), err_C0 * u_g**rmax ) * z_g 
        err_g_C = err_g_Cr
        err_g_exp = u_g**(rmax-1) * Dtyp
      end if
    else
      use_g = .false.
      err_g = err_inf
      g = -1
      err_g_exp = err_inf
      err_g_C = err_C(rmax)         ! dummy
      u_g = real(undefined_D)
      z_g = real(undefined_D)
      err_g_Cr = real(undefined_D)
    endif

#ifdef Dredtest
    write(*,*) 'CalcDred: after Gram pars',use_g,fac_g,x_g,u_g,z_g,err_g_Cr,err_C(rmax),err_C0,err_g_exp
!    write(*,*) 'CalcDred: after Gram pars',adetZ,maxZadjf,maxXadj,maxZ
    write(*,*) 'CalcDred: after Gram pars',err_C(rmax), err_C0 * u_g**rmax 
#endif

#ifdef USEGM
    ! modified Gram expansion    
!   if (aZadjff.ne.0d0) then
    if (aZadjff.gt.m2scale**4*dprec_cll) then   !  10.07.2017
      x_gm = adetZ*fmax/aZadjff
!      u_gm = max(1d0,maxZadj2ff/maxZadjf/4d0,abs(mm02)*maxZadj/maxZadjf/4d0)
! 03.03.15   large P counts!
!      u_gm = max(1d0,maxZadj2ff/maxZadjf/2d0,abs(mm02)*maxZadj/maxZadjf/2d0)
! 24.04.15  term appear only combined
      u_gm = max(1d0,maxXadj/maxZadjf/2d0)
      fac_gm = x_gm*u_gm
      err_gm = err_inf
      gm = -1
      if (fac_gm.ge.1) then
        use_gm = .false.
        err_gm_exp = err_inf
        err_gm_C = err_C(rmax)         ! dummy
      else
        use_gm = .true.
!       z_gm = max(1d0,m2scale*maxZadj/maxZadjf)
        z_gm = maxZadjf/aZadjff
        err_gm_Cr = max( err_C(rmax), err_C0 * u_gm**rmax ) * z_gm 
        err_gm_C = err_gm_Cr
        err_gm_exp = u_gm**(rmax-1) * Dtyp
      end if
    else
      use_gm = .false.
      err_gm = err_inf
      gm = -1
      err_gm_exp = err_inf
      err_gm_C = err_C(rmax)         ! dummy
    endif

#ifdef Dredtest
    write(*,*) 'CalcDred: after mod Gram pars',use_gm,fac_gm,x_gm,u_gm,z_gm,err_gm_Cr,err_C(rmax),err_C0,err_gm_exp
!    write(*,*) 'CalcDred: after mod Gram pars',1d0,maxZadj2ff/maxZadjf,abs(mm02)*maxZadj/maxZadjf
    write(*,*) 'CalcDred: after mod Gram pars',err_C(rmax), err_C0 * u_gm**rmax 
    write(*,*) 'CalcDred: after mod Gram pars',  adetZ,fmax,aZadjff
#endif

#else
      use_gm = .false.
      err_gm = err_inf
      gm = -1 
      err_gm_exp = err_inf
      err_gm_C = err_C(rmax)         ! dummy
#endif

    ! Gram-Cayley expansion
!    if (maxXadj.ne.0d0.and.maxZadj.ne.0) then
    if (maxXadj.gt.m2scale**3*dprec_cll.and.maxZadj.gt.m2scale*dprec_cll) then   !  10.07.2017
      x_gy = maxZadjf/maxXadj
      y_gy = adetZ/maxXadj
      v_gy = maxZadj2f/maxZadj
      v1_gy = max(1d0,v_gy)
      fac_gy = max(x_gy,y_gy)*v1_gy
      err_gy = err_inf
      gy = -1
      if (fac_gy.ge.1) then
        use_gy = .false.
        err_gy_exp = err_inf
        err_gy_C = err_C(rmax+1)         ! dummy
        err_gy_Cr = real(undefined_D)
        b_gy = real(undefined_D)
      else
        use_gy = .true.
!       b_gy = max(1d0,m2scale*maxZadj/maxXadj)
        b_gy = maxZadj/maxXadj
        err_gy_Cr =  max( err_C(rmax) * v1_gy, err_C(rmax+1) ) 
        err_gy_C = err_gy_Cr * b_gy
        err_gy_exp = 1d0 * Dtyp
      end if
    else
      use_gy = .false.
      err_gy = err_inf
      gy = -1
      err_gy_exp = err_inf
      err_gy_C = err_C(rmax+1)         ! dummy
      v1_gy = real(undefined_D)
      b_gy = real(undefined_D)
      err_gy_Cr = real(undefined_D)
    endif


#ifdef Dredtest
    write(*,*) 'CalcDred: after GramCay pars',use_gy,fac_gy,x_gy,y_gy,v_gy,b_gy,err_gy_Cr,err_gy_exp
    if (use_gy) then
      write(*,*)  'CalcDred: after GramCay pars',maxXadj/maxZadj,1/v_gy,1/v_gy*maxXadj/maxZadj,1/v_gy*maxXadj/maxZadj*x_gy
    end if
#endif

    ! expansion in small momenta
!    if (fmax.ne.0d0) then
    if (fmax.gt.m2scale*dprec_cll) then   !  10.07.2017
      w_gp = maxZ/fmax                        ! was q2max
      v_gp = abs(mm02/fmax)
      v1_gp = max(1d0,v_gp)
      fac_gp = w_gp*v1_gp
      err_gp = err_inf
      gp = -1
      if (fac_gp.ge.1d0) then
        use_gp = .false.
        err_gp_exp = err_inf
        err_gp_C = err_C(rmax)  ! dummy
        err_gp_Cr = real(undefined_D)
        z_gp = real(undefined_D)
      else
        use_gp = .true.
!       z_gp = max(1d0,m2scale/fmax)
        z_gp = 1d0/fmax
        err_gp_Cr = max(err_C0 * v_gp**rmax , err_C(rmax)) *  z_gp
        err_gp_C = err_gp_Cr
        err_gp_exp = v1_gp**(rmax-1) * Dtyp
      end if
    else
      use_gp = .false.
      err_gp = err_inf
      gp = -1
      err_gp_exp = err_inf
      err_gp_C = err_C(rmax)  ! dummy
      v1_gp = real(undefined_D)
      v_gp = real(undefined_D)
      z_gp = real(undefined_D)
      err_gp_Cr = real(undefined_D)
    endif

#ifdef Dredtest
    write(*,*) 'CalcDred: after Mom pars',use_gp,fac_gp,w_gp,v_gp,z_gp,err_gp_Cr,err_gp_exp
#endif

    ! reversed Gram expansion
!    if (maxZadjf.ne.0d0.and.maxZadj2f.ne.0d0) then
    if (maxZadjf.gt.m2scale**3*dprec_cll.and.maxZadj2f.gt.m2scale**2*dprec_cll) then   !  10.07.2017
      x_gr = adetZ/maxZadjf
      y_gr = maxZadj/maxZadj2f              ! c*y    c=2
      y1_gr = max(1d0,y_gr)
      a_gr = maxZadj/maxZadjf
      fac_gr = max(x_gr,y_gr)
      err_gr = err_inf
      gr = -1
      if (fac_gr.ge.1.or.2*rmax.gt.rmax_C) then
        use_gr = .false.
        err_gr_exp = err_inf
        err_gr_C = err_C(rmax)   ! dummy
        err_gr_Cr = real(undefined_D)
      else
        use_gr = .true.
        err_gr_Cr = err_C(rmax) 
        err_gr_C = err_gr_Cr * a_gr
        err_gr_exp = y1_gr * Dtyp
      end if
    else
      use_gr = .false.
      err_gr = err_inf
      gr = -1
      err_gr_exp = err_inf
      err_gr_C = err_C(rmax)   ! dummy
      a_gr = real(undefined_D)
      y_gr = real(undefined_D)
      y1_gr = real(undefined_D)
      err_gr_Cr = real(undefined_D)
    endif

#ifdef Dredtest
    write(*,*) 'CalcDred: after revGram pars',use_gr,fac_gr,x_gr,y_gr,y1_gr,a_gr,err_gr_Cr,err_C(rmax),err_C0,err_gr_exp
    write(*,*) 'CalcDred: after revGram pars',err_gr_C,Dtyp
#endif

    ! expansion in small momenta and f's
!  estimates to be confirmed 16.08.2017, r dependence may be different
!  since D_mni... is needed in contrast to Dgy expansion
    if (abs(m02).gt.m2scale*dprec_cll) then 
      x_gpf = fmax/abs(m02)
      y_gpf = maxZ/abs(m02)
      v_gpf = 0d0
      v1_gpf = max(1d0,v_gpf)
      fac_gpf = max(x_gpf,y_gpf)
      err_gpf = err_inf
      gpf = -1
      if (fac_gpf.ge.1) then
        use_gpf = .false.
        err_gpf_exp = err_inf
        err_gpf_C = err_C(rmax+1)         ! dummy
        err_gpf_Cr = real(undefined_D)
        b_gpf = real(undefined_D)
      else
        use_gpf = .true.
        b_gpf = 1d0/abs(m02)
        err_gpf_Cr =  max( err_C(rmax), err_C(rmax+1) ) 
        err_gpf_C = err_gpf_Cr * b_gpf
        err_gpf_exp = 1d0 * Dtyp
      end if
    else
      use_gpf = .false.
      err_gpf = err_inf
      gpf = -1
      err_gpf_exp = err_inf
      err_gpf_C = err_C(rmax+1)         ! dummy
      b_gpf = real(undefined_D)
      err_gpf_Cr = real(undefined_D)
    endif


#ifdef Dredtest
    write(*,*) 'CalcDred: after pf pars',use_gpf,fac_gpf,x_gpf,y_gpf,v_gpf,b_gpf,err_gpf_Cr,err_gpf_exp,err_gpf
    if (use_gpf) then
      write(*,*)  'CalcDred: after pf pars',maxXadj/maxZadj,1/v_gpf,1/v_gpf*maxXadj/maxZadj,1/v_gpf*maxXadj/maxZadj*x_gpf
    end if
#endif

! no method works
    if(use_D0.or.use_pv.or.use_pv2.or.use_g.or.use_gy.or.use_gp.or.use_gr.or.use_gm.or.use_gpf.eqv..false.) then
      call SetErrFlag_coli(-6)
      call ErrOut_coli('CalcDred',' no reduction method works',  &
         errorwriteflag)
!      write(nerrout_coli,'((a))')  '  no reduction method works'
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcDred: p10 = ',p10
        write(nerrout_coli,fmt10) ' CalcDred: p21 = ',p21
        write(nerrout_coli,fmt10) ' CalcDred: p32 = ',p32
        write(nerrout_coli,fmt10) ' CalcDred: p30 = ',p30
        write(nerrout_coli,fmt10) ' CalcDred: p20 = ',p20
        write(nerrout_coli,fmt10) ' CalcDred: p31 = ',p31
        write(nerrout_coli,fmt10) ' CalcDred: m02 = ',m02
        write(nerrout_coli,fmt10) ' CalcDred: m12 = ',m12
        write(nerrout_coli,fmt10) ' CalcDred: m22 = ',m22   
        write(nerrout_coli,fmt10) ' CalcDred: m32 = ',m32   
      end if 
      D = 0d0
      Duv = 0d0
      Derr1 = err_inf
      Derr2 = err_inf

#ifdef Dredtest
      write(*,*) 'CalcDred: exit'
#endif

      return
    endif

#ifdef TEST
! switched off for testing
!    use_g  = .false.
!    use_gy = .false.
!    use_gp = .false.
     use_gr = .false.
!    use_gpf= .false.
!    use_g  = .true.
!    use_gy = .true.
!    use_gp = .true.
!    use_gr = .true.
     write(*,*) 'CalcDgy rmax_D ',rmax_D,rmax,rmax_D-rmax
#endif 

    iexp = 0
    do i=0,rmax_D-rmax

      if (use_g) then
        if (err_g_exp.gt.err_g_C) then
          g = i
          err_g_exp = err_g_exp*fac_g
          err_g_C = max(err_g_Cr,err_C(rmax+g)*z_g*x_g**g)
          err_g(rmax) = max(err_g_exp,err_g_C)
          if(err_g(rmax).lt.err_req_D) then
            iexp = 1
            ! increase g by 2 to account for bad estimates
            g = min(max(g+2,3*g/2),rmax_D-rmax)
            exit
          end if
        end if
      end if

#ifdef Dredtest
!       write(*,*) 'CalcDred: it g',g, err_g_exp, err_g_C,  err_g(rmax) 
!       write(*,*) 'CalcDred: it g',err_g_Cr,err_C(rmax+g)*z_g*x_g**g
!       write(*,*) 'CalcDred: it g',err_C(rmax+g),z_g,x_g
#endif

#ifdef USEGM
      if (use_gm) then
        if (err_gm_exp.gt.err_gm_C) then
          gm = i
          err_gm_exp = err_gm_exp*fac_gm
          err_gm_C = max(err_gm_Cr,err_C(rmax+gm)*z_gm*x_gm**gm)
          err_gm(rmax) = max(err_gm_exp,err_gm_C)
          if(err_gm(rmax).lt.err_req_D) then
            iexp = 7
            ! increase gm by 2 to account for bad estimates
            gm = min(max(gm+2,3*gm/2),rmax_D-rmax)
            exit
          end if
        end if
      end if

#ifdef Dredtest
!      write(*,*) 'CalcDred: it gm',gm, err_gm_exp, err_gm_C,  err_gm(rmax) 
!      write(*,*) 'CalcDred: it gm',err_gm_Cr,err_C(rmax+gm)*z_gm*x_gm**gm
!      write(*,*) 'CalcDred: it gm',err_C(rmax+gm),z_gm,x_gm
#endif
#endif


      if (mod(i,2).eq.1) then

#ifdef Dredtest
       write(*,*) 'CalcDred: it gy',use_gy,err_gy_exp,err_gy_C,err_gy(rmax),err_req_D
#endif

        if (use_gy) then
          if (err_gy_exp.gt.err_gy_C.and.err_gy(rmax).gt.err_req_D) then
            gy = i/2
            err_gy_exp = err_gy_exp*fac_gy
            err_gy_C = b_gy*max(err_gy_Cr,                   &
                max(err_C(rmax+2*gy)*v1_gy,err_C(rmax+2*gy+1))*y_gy**gy,      &          
                max(err_C(rmax+gy)*v1_gy,err_C(rmax+gy+1))*(max(x_gy,v_gy*y_gy))**gy) 
            err_gy(rmax) = max(err_gy_exp,err_gy_C)

#ifdef Dredtest
         write(*,*) 'CalcDred i gy',i,gy,err_gy_exp,err_gy_C,err_gy(rmax) 
         write(*,*) 'CalcDred i ',err_gy_Cr,                   &
               max(err_C(rmax+2*gy)*v1_gy,err_C(rmax+2*gy+1))*y_gy**gy,      &          
               max(err_C(rmax+gy)*v1_gy,err_C(rmax+gy+1))*(max(x_gy,v_gy*y_gy))**gy
          write(*,*) 'CalcDred i ', b_gy*err_C(rmax+2*gy)*v1_gy*y_gy**gy, &
                b_gy*err_C(rmax+2*gy+1)*y_gy**gy
          write(*,*) 'CalcDred i ', & 
                b_gy,err_C(rmax+2*gy+1),y_gy**gy 
          write(*,*) 'CalcDred i ', b_gy*x_gy**gy*err_C(rmax+gy)*v1_gy, &
                b_gy*err_C(rmax+gy+1)*x_gy**gy 
          write(*,*) 'CalcDred i ', & 
                b_gy,err_C(rmax+gy+1),x_gy**gy,x_gy,gy 
#endif

            if(err_gy(rmax).lt.err_req_D) then
              iexp = 2
              ! increase gy by 2 to account for bad estimates
              gy = min(max(gy+2,2*gy),(rmax_D-rmax)/2)
              exit
            end if
          end if
        end if

#ifdef Dredtest
!       write(*,*) 'CalcDred: it gy',i,gy, err_gy_exp,err_gy_C ,err_gy(rmax)
#endif

      end if

!      write(*,*) 'CalcDred bef gp it',err_gp(rmax),err_gp_C,err_req_D

      if (use_gp) then
        if (err_gp_exp.gt.err_gp_C.and.err_gp(rmax).gt.err_req_D) then
          gp = i
          err_gp_exp = err_gp_exp*fac_gp
          err_gp_C = max(err_C(rmax+gp)*z_gp*w_gp**gp,err_gp_Cr)
          err_gp(rmax) = max(err_gp_exp,err_gp_C)
          if(err_gp(rmax).lt.err_req_D) then
            iexp = 3
            ! increase gp by 2 to account for bad estimates
            gp = min(max(gp+2,3*gp/2),rmax_D-rmax)
            exit
          end if
        end if
      end if

!        write(*,*) 'CalcDred: it gp',gp,err_gp, err_gp_C,  err_gp(rmax) 

      if (mod(i,2).eq.1.and.i.le.rmax_C-2*rmax) then

#ifdef Dredtest
!        write(*,*) 'CalcDred: it gr',use_gr,err_gr_exp,err_gr_C,err_gr(rmax),  &
!                                     err_req_D
#endif

        if (use_gr) then
          if (err_gr_exp.gt.err_gr_C.and.err_gr(rmax).gt.err_req_D) then
            gr = i/2
            err_gr_exp = err_gr_exp*fac_gr
            err_gr_C = a_gr*max(err_gr_Cr,                   &
                max(err_C(rmax+gr),err_C(rmax+gr+1)*y_gr)*fac_gr**gr)
            err_gr(rmax) = max(err_gr_exp,err_gr_C)
#ifdef Dredtest
!          write(*,*) 'CalcDred: it gr',gr,err_gr(rmax),err_req_D
#endif
            if(err_gr(rmax).lt.err_req_D) then
              iexp = 4
              ! increase gy by 2 to account for bad estimates
! changed 25.07.14
!             gr = min(max(gr+2,2*gr),(rmax_D-rmax)/2,(rmax_C-2*rmax)/2)
              gr = min(max(gr+2,2*gr),rmax_D-rmax,max(0,(rmax_C-2*rmax)/2))
              exit
            end if
          end if
        end if

#ifdef Dredtest
!      write(*,*) 'CalcDred: it gr',i,gr, err_gr_exp,err_gr_C ,err_gr(rmax)
#endif

      if (mod(i,2).eq.1) then

#ifdef Dredtest
       write(*,*) 'CalcDred: it gy',use_gy,err_gy_exp,err_gy_C,err_gy(rmax),err_req_D
#endif

        if (use_gpf) then
          if (err_gpf_exp.gt.err_gpf_C.and.err_gpf(rmax).gt.err_req_D) then
            gpf = i/2
            err_gpf_exp = err_gpf_exp*fac_gpf
            err_gpf_C = b_gpf*max(err_gpf_Cr,                   &
                max(err_C(rmax+2*gpf)*v1_gpf,err_C(rmax+2*gpf+1))*y_gpf**gpf,      &          
                max(err_C(rmax+gpf)*v1_gpf,err_C(rmax+gpf+1))*(max(x_gpf,v_gpf*y_gpf))**gpf) 
            err_gpf(rmax) = max(err_gpf_exp,err_gpf_C)

#ifdef Dredtest
         write(*,*) 'CalcDred i gpf',i,gpf,err_gpf_exp,err_gpf_C,err_gpf(rmax) 
         write(*,*) 'CalcDred i ',err_gpf_Cr,                   &
               max(err_C(rmax+2*gpf)*v1_gpf,err_C(rmax+2*gpf+1))*y_gpf**gpf,      &          
               max(err_C(rmax+gpf)*v1_gpf,err_C(rmax+gpf+1))*(max(x_gpf,v_gpf*y_gpf))**gpf
          write(*,*) 'CalcDred i ', b_gpf*err_C(rmax+2*gpf)*v1_gpf*y_gpf**gpf, &
                b_gpf*err_C(rmax+2*gpf+1)*y_gpf**gpf
          write(*,*) 'CalcDred i ', & 
                b_gpf,err_C(rmax+2*gpf+1),y_gpf**gpf 
          write(*,*) 'CalcDred i ', b_gpf*x_gpf**gpf*err_C(rmax+gpf)*v1_gpf, &
                b_gpf*err_C(rmax+gpf+1)*x_gpf**gpf 
          write(*,*) 'CalcDred i ', & 
                b_gpf,err_C(rmax+gpf+1),x_gpf**gpf,x_gpf,gpf 
#endif

            if(err_gpf(rmax).lt.err_req_D) then
              iexp = 5
              ! increase gpf by 2 to account for bad estimates
              gpf = min(max(gpf+2,2*gpf),(rmax_D-rmax)/2)
              exit
            end if
          end if
        end if

#ifdef Dredtest
!       write(*,*) 'CalcDred: it gpf',i,gpf, err_gpf_exp,err_gpf_C ,err_gpf(rmax)
#endif

      end if
      end if


    end do

    ! scale estimates down to allow trying other methods
    err_g(rmax)  =  err_g(rmax)/impest_D
    err_gy(rmax) =  err_gy(rmax)/impest_Dgy     
    err_gp(rmax) =  err_gp(rmax)/impest_D
    err_gr(rmax) =  err_gr(rmax)/impest_D     
    err_gm(rmax) =  err_gm(rmax)/impest_D     
    err_gpf(rmax) =  err_gpf(rmax)/impest_D     

#ifdef Dredtest
    write(*,*) 'iexp=',iexp
    write(*,*) 'facexp=',fac_g,fac_gy,fac_gp,fac_gr,fac_gm,fac_gpf
    write(*,*) 'errexp=',err_g_exp,err_gy_exp,err_gp_exp,err_gr_exp,err_gm_exp,err_gpf_exp,err_req_D
    write(*,*) 'errexptot=',i,g,err_g(rmax),gy,err_gy(rmax),gp,err_gp(rmax), &

        gr,err_gr(rmax),gm,err_gm(rmax),gpf,err_gpf(rmax)
    write(*,*) 'accexptot=',i,g,err_g(rmax)/Dtyp,gy,err_gy(rmax)/Dtyp, &
        gp,err_gp(rmax)/Dtyp,gr,err_gr(rmax)/Dtyp,gm,err_gm(rmax)/Dtyp, &
        gpf,err_gpf(rmax)/Dtyp
#endif

! call expansions with estimated order to save CPU time

#ifdef TEST
!     iexp = 0
!     iexp = 5
!     gm = 10
!      iexp = 1
!      g = 10
!     iexp = 4
!     gr = 3
!      iexp = 2
!      gy = 6
#endif

    select case (iexp)

    case (1)
      call CalcDg(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,g,g,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(3) = DCount(3)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+4
      DCalc = DCalc+4
      Drmethod_alt(0:rmax)=4
        
#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_g(rmax)
      if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
        write(*,*) 'CalcDred: estimate err_g imprecise ',err_g(rmax),Derr_alt(rmax)
      end if
#endif

      err_g=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)
              

#ifdef Dredtest
      write(*,*) 'CalcDred after exp g Derr=',Derr,err_req_D
!     write(*,*) 'CalcDred after exp   Dacc=',Derr/abs(D(0,0,0,0))
      write(*,*) 'CalcDred after exp   Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp   method=',Drmethod
#endif

    case (2)
      call CalcDgy(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,gy,gy,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(4) = DCount(4)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+8
      DCalc = DCalc+8
      Drmethod_alt(0:rmax)=8

#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_gy(rmax)
      if(checkest.gt.1d2*impest_Dgy.or.checkest.lt.1d-2*impest_Dgy) then
        write(*,*) 'CalcDred: estimate err_gy imprecise',err_gy(rmax),Derr_alt(rmax),checkest
      end if
#endif
      err_gy=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)

#ifdef Dredtest
      write(*,*) 'CalcDred after exp gy Derr=',Derr,err_req_D
      write(*,*) 'CalcDred after exp    Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp    method=',Drmethod
#endif
#ifdef Dredtest
!      write(*,*) 'after CalcDgy D(1,0,1,0)',D_alt(1,0,1,0),D(1,0,1,0)
#endif


    case (3)
      call CalcDgp(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,gp,gp,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(5) = DCount(5)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+16
      DCalc = DCalc+16
      Drmethod_alt(0:rmax)=16

#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_gp(rmax)
      if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
        write(*,*) 'CalcDred: estimate err_gp imprecise',err_gp(rmax),Derr_alt(rmax)
      end if
#endif
      err_gp=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)

#ifdef Dredtest
      write(*,*) 'CalcDred after exp gp Derr=',Derr,err_req_D
      write(*,*) 'CalcDred after exp    Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp    method=',Drmethod
#endif

    case (4)
      call CalcDgr(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,gr,gr,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(6) = DCount(6)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+32
      DCalc = DCalc+32
      Drmethod_alt(0:rmax)=32

#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_gr(rmax)
      if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
        write(*,*) 'CalcDred: estimate err_gr imprecise',err_gr(rmax),Derr_alt(rmax)
      end if
#endif
      err_gr=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)

#ifdef Dredtest
      write(*,*) 'CalcDred after exp gr Derr=',Derr,err_req_D
      write(*,*) 'CalcDred after exp    Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp    method=',Drmethod
#endif

#ifdef USEGM
    case (7)
      call CalcDgm(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,gm,gm,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(7) = DCount(7)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+64
      DCalc = DCalc+64
      Drmethod_alt(0:rmax)=64

#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_gm(rmax)
      if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
        write(*,*) 'CalcDred: estimate err_gm imprecise',err_gm(rmax),Derr_alt(rmax)
      end if
#endif
      err_gm=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)

#ifdef Dredtest
      write(*,*) 'CalcDred after exp gm Derr=',Derr,err_req_D
      write(*,*) 'CalcDred after exp    Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp    method=',Drmethod
#endif
#endif

    case (5)
      call CalcDgpf(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,gpf,gpf,id,Derr1_alt,Derr2_alt)
#ifdef PVEST2
      Derr_alt = Derr2_alt
#else
      Derr_alt = Derr1_alt
#endif
      DCount(7) = DCount(7)+1
      DrCalc(0:rmax)=DrCalc(0:rmax)+64
      DCalc = DCalc+64
      Drmethod_alt(0:rmax)=64

#ifdef Dredtest
      checkest=Derr_alt(rmax)/err_gpf(rmax)
      if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
        write(*,*) 'CalcDred: estimate err_gpf imprecise',err_gpf(rmax),Derr_alt(rmax),checkest
      end if
#endif
      err_gpf=Derr_alt

      call CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,rmax)

#ifdef Dredtest
      write(*,*) 'CalcDred after exp gpf Derr=',Derr,err_req_D
      write(*,*) 'CalcDred after exp     Dacc=',Derr/Dtyp
      write(*,*) 'CalcDred after exp     method=',Drmethod
#endif

    end select

!   write(*,*) 'CalcDred Calc',DrCalc(rmax)

#ifndef USED0
#ifndef ALWAYSPV
    ! refine error estimate for D0
    if(.not.lerr_D0.and.iexp.ne.0) then
!      D0est =  abs(D(0,0,0,0))
      err_D0 = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
!      err_req_D = acc_req_D * abs(D(0,0,0,0))
      lerr_D0 = .true.
    end if
#endif
#endif

    if (iexp.ne.0) then           !  if added 21.11.2016
      if (rmax.ge.1) then
        Dtyp =  max(abs(D(0,0,0,0)),        &
            abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))  
      else
        Dtyp =  abs(D(0,0,0,0))
      end if
      err_req_D = acc_req_D * Dtyp
    
#ifdef Dredtest
      write(*,*) 'CalcDred ',rmax,Derr1(rmax),err_req_D
#endif

      if (Derr1(rmax).le.err_req_D) then
        DCount(DCalc+DCountoffset0) = DCount(DCalc+DCountoffset0)+1
        return
      end if
    end if

#ifdef TEST
!    return
#endif

#ifdef Dredtest
    write(*,*) 'CalcDred no optimal method'
    write(*,*) 'err_req_D=',err_req_D
    write(*,*) 'err_est=',err_pv(rmax),err_pv2(rmax),err_g(rmax),err_gy(rmax),err_gp(rmax),err_gr(rmax),err_gpf(rmax)
#endif

    ! no method does work optimal
    ! use the least problematic (for each rank)
    do r=rmax,0,-1
     
      if(use_pv.and.mod(DrCalc(r),2).ne.1) then   
    ! estimate accuracy of PV-reduction
        if (use_pv) then

          if (mod(r,2).eq.1) then
            err_pv(r) = max( w_pv**((r-1)/2) * v_pv * err_D0,  &
                w_pv**((r-1)/2) * z_pv * err_C0, z_pv * err_C(r-1) )
            
!            write(*,*) 'CalcDred w_pv: ',w_pv,v_pv,err_D0,r
            
!            write(*,*) 'CalcDred err_pv: ',err_pv(r), w_pv**((r-1)/2) * v_pv * err_D0,  &
!                        w_pv**((r-1)/2) * z_pv * err_C0, z_pv * err_C(r-1)
            
          else if (r.ne.0) then
            err_pv(r) = max( w_pv**(r/2) * err_D0,  &
                w_pv**(r/2-1) * v_pv * z_pv * err_C0, z_pv * err_C(r-1) )
            
!            write(*,*) 'CalcDred err_pv: ',err_pv(r), w_pv**((r)/2) * err_D0,  &
!                        w_pv**(r/2-1) * v_pv * z_pv * err_C0, z_pv * err_C(r-1)

          else
            err_pv(r) = err_D0
          end if
        end if
        ! scale estimates down to allow trying other methods
        err_pv(r)  = err_pv(r)/impest_D
      end if

      if (use_pv2.and.mod(DrCalc(r),4)-mod(DrCalc(r),2).ne.2) then
    ! estimate accuracy of alternative PV-reduction
        if (use_pv2) then

!         write(*,*) 'CalcDred err_pv2',  r,w_pv2,v_pv2,z_pv2,err_D0,err_C0

          if (mod(r,2).eq.1) then
! changed 21.10.15 for PVEST2
!            err_pv2(r) = max( err_D0 * max(w_pv2**r,    &
!                w_pv2*v_pv2**((r-1)/2) ),  &
!                err_C0 * z_pv2* max(w_pv2**(r+1), &
!                                    w_pv2*v_pv2**((r-1)/2), & 
!                                    v_pv2**((r+1)/2)),  &
!                err_C(r-1) * z_pv2 * max(w_pv2,w_pv2**2,v_pv2) )
            err_pv2(r) = max( err_D0 * max(hw_pv2**r,    &
                hw_pv2*v_pv2**((r-1)/2) ),  &
                err_C0 * z_pv2* max(w_pv2*w_pv2**(r), &
                                    hw_pv2*v_pv2**((r-1)/2), & 
                                    w_pv2*hw_pv2*v_pv2**((r-1)/2), & 
                                    v_pv2**((r+1)/2)),  &
                err_C(r-1) * z_pv2 * max(hw_pv2,w_pv2*hw_pv2**2,v_pv2) )
                                  

!           write(*,*) 'CalcDred err_pv2: ',   &
!               err_pv2(r) ,     err_D0,err_D0*w_pv2**r,err_D0*v_pv2**((r-1)/2),    &
!                                     err_D0*w_pv2*v_pv2**((r-1)/2),       &
!                                 err_C0 * z_pv2*w_pv2**(r+1), &
!                                  err_C0 * z_pv2*w_pv2*v_pv2**((r-1)/2), & 
!                                  err_C0 * z_pv2*v_pv2**((r+1)/2),     &
!               err_C(r-1),  err_C(r-1)*z_pv2*w_pv2, &
!                                err_C(r-1)*  z_pv2*w_pv2**2, err_C(r-1)*z_pv2*v_pv2


          else if (r.ne.0) then
! changed 21.10.15 for PVEST2
!            err_pv2(r) = max( err_D0 * max(w_pv2**r,v_pv2**(r/2)),  &
!                err_C0 * z_pv2 * max(w_pv2**(r+1),  &
!                                     v_pv2**(r/2),w_pv2*v_pv2**(r/2)), &
!                err_C(r-1) * z_pv2 * max(w_pv2, w_pv2**2, v_pv2) )
            err_pv2(r) = max( err_D0 * max(hw_pv2**r,v_pv2**(r/2)),  &
                err_C0 * z_pv2 * max(w_pv2*hw_pv2**(r),  &
                                     v_pv2**(r/2),w_pv2*v_pv2**(r/2)), &
                err_C(r-1) * z_pv2 * max(hw_pv2, w_pv2*hw_pv2**2, v_pv2) )
                                  

!         write(*,*) 'CalcDred err_pv2: ',   &
!         err_pv2(r) , err_D0 * max(1d0,w_pv2**r,v_pv2**(r/2)),  &
!                        err_C0 * z_pv2 * max(w_pv2**(r+1),z_pv2*w_pv2,  &
!                                 v_pv2**(r/2),w_pv2*v_pv2**(r/2)), &
!                        err_C(r-1) * max(1d0,z_pv2*w_pv2,  &
!                                 z_pv2*w_pv2**2,z_pv2*v_pv2)

          else
            err_pv2(r) = err_D0
          end if
        end if
        ! scale estimates down to allow trying other methods
        err_pv2(r) = err_pv2(r)/impest_D     
      end if

      if (mod(DrCalc(r),8)-mod(DrCalc(r),4).ne.4.and.use_g) then
      ! estimate accuracy of alternative Gram expansion
        err_g_Cr = max( err_C(r), err_C0 * u_g**r ) * z_g 
        err_g_C = err_g_Cr
        err_g_exp = u_g**(r-1) * Dtyp

      ! determine optimal order of expansion 
        do i=0,rmax_D-r
          g = i
          err_g_exp = err_g_exp*fac_g
          err_g_C = max(err_g_Cr,err_C(r+g)*z_g*x_g**g)
          err_g(r) = max(err_g_exp,err_g_C)
          if (err_g_exp.lt.err_g_C.or.err_g(r).lt.err_req_D) exit
        end do
      ! increase g by 2 to account for bad estimates
        g = min(max(g+2,2*g),rmax_D-r)
      ! scale estimates down to allow trying other methods
        err_g(r)  =  err_g(r)/impest_D
      end if

      if (mod(DrCalc(r),16)-mod(DrCalc(r),8).ne.8.and.use_gy) then
      ! estimate accuracy of alternative Gram expansion
        err_gy_Cr =  max( err_C(r) * v1_gy, err_C(r+1) ) 
        err_gy_C = err_gy_Cr * b_gy
        err_gy_exp = 1d0 * Dtyp

      ! determine optimal order of expansion 
        gy = 0
        do i=0,rmax_D-r
          if (mod(i,2).eq.1) then
            gy = i/2
            err_gy_exp = err_gy_exp*fac_gy
            err_gy_C = b_gy*max(err_gy_Cr,                   &
                max(err_C(r+2*gy)*v1_gy,err_C(r+2*gy+1))*y_gy**gy,      &          
                max(err_C(r+gy)*v1_gy,err_C(r+gy+1))*(max(x_gy,v_gy*y_gy))**gy) 
            err_gy(r) = max(err_gy_exp,err_gy_C)            
            if (err_gy_exp.lt.err_gy_C.or.err_gy(r).lt.err_req_D) exit
          end if
        end do
      ! increase gy to account for bad estimates
        gy = min(max(gy+2,2*gy),(rmax_D-r)/2)
      ! scale estimates down to allow trying other methods
        err_gy(r) =  err_gy(r)/impest_Dgy    
      end if

      if (mod(DrCalc(r),32)-mod(DrCalc(r),16).ne.16.and.use_gp) then
      ! estimate accuracy of small momenta expansion
        err_gp_Cr = max(err_C0*v_gp**r,err_C(r))*z_gp
        err_gp_exp = v1_gp**(r-1) * Dtyp

      ! determine optimal order of expansion 
        do i=0,rmax_D-r
          gp = i
          err_gp_exp = err_gp_exp*fac_gp
          err_gp_C = max(err_C(r+gp)*z_gp*w_gp**gp,err_gp_Cr)
          err_gp(r) = max(err_gp_exp,err_gp_C)
          if (err_gp_exp.lt.err_gp_C.or.err_gp(r).lt.err_req_D) exit
        end do
      ! increase gp to account for bad estimates
        gp = min(max(gp+2,3*gp/2),rmax_D-r)
      ! scale estimates down to allow trying other methods
        err_gp(r) =  err_gp(r)/impest_D
      end if

      if (mod(DrCalc(r),64)-mod(DrCalc(r),32).ne.32.and.use_gr) then
      ! estimate accuracy of alternative Gram expansion
        err_gr_Cr = err_C(r) 
        err_gr_C = err_gr_Cr * a_gr
        err_gr_exp = y1_gr * Dtyp

      ! determine optimal order of expansion 
        gr = 0
        do i=0,min(rmax_D-r,rmax_C-2*r)
          if (mod(i,2).eq.1) then
            gr = i/2
            err_gr_exp = err_gr_exp*fac_gr
            err_gr_C = a_gr*max(err_gr_Cr,                   &
                max(err_C(r+gr),err_C(r+gr+1)*y_gr)*fac_gr**gr)
            err_gr(r) = max(err_gr_exp,err_gr_C)

#ifdef Dgrtest
            write(*,*) 'CalcDgr err_gr',i,gr,err_gr_exp,err_gr_C,err_gr(r),err_req_D
#endif

            if (err_gr_exp.lt.err_gr_C.or.err_gr(r).lt.err_req_D) exit
          end if
        end do
      ! increase gr to account for bad estimates
! changed 28.07.14
!       gr = min(max(gr+2,2*gr),(rmax_D-r)/2,(rmax_C-2*r)/2)
        gr = min(max(gr+2,2*gr),rmax_D-r,max(0,(rmax_C-2*r)/2))
      ! scale estimates down to allow trying other methods
        err_gr(r) =  err_gr(r)/impest_D     

      end if

      if (mod(DrCalc(r),128)-mod(DrCalc(r),64).ne.64.and.use_gpf) then
      ! estimate accuracy of small momenta and f expansion
        err_gpf_Cr =  max( err_C(r) * v1_gpf, err_C(r+1) ) 
        err_gpf_C = err_gpf_Cr * b_gpf
        err_gpf_exp = 1d0 * Dtyp

      ! determine optimal order of expansion 
        gpf = 0
        do i=0,rmax_D-r
          if (mod(i,2).eq.1) then
            gpf = i/2
            err_gpf_exp = err_gpf_exp*fac_gpf
            err_gpf_C = b_gpf*max(err_gpf_Cr,                   &
                max(err_C(r+2*gpf)*v1_gpf,err_C(r+2*gpf+1))*y_gpf**gpf,      &          
                max(err_C(r+gpf)*v1_gpf,err_C(r+gpf+1))*(max(x_gpf,v_gpf*y_gpf))**gpf) 
            err_gpf(r) = max(err_gpf_exp,err_gpf_C)            
            if (err_gpf_exp.lt.err_gpf_C.or.err_gpf(r).lt.err_req_D) exit
          end if
        end do
      ! increase gpf to account for bad estimates
        gpf = min(max(gpf+2,2*gpf),(rmax_D-r)/2)
      ! scale estimates down to allow trying other methods
        err_gpf(r) =  err_gpf(r)/impest_D     
      end if



#ifdef USEGM
      if (mod(DrCalc(r),128)-mod(DrCalc(r),64).ne.64.and.use_gm) then
      ! estimate accuracy of modified Gram expansion
        err_gm_Cr = max( err_C(r), err_C0 * u_gm**r ) * z_gm 
        err_gm_C = err_gm_Cr
        err_gm_exp = u_gm**(r-1) * Dtyp

      ! determine optimal order of expansion 
        do i=0,rmax_D-r
          gm = i
          err_gm_exp = err_gm_exp*fac_gm
          err_gm_C = max(err_gm_Cr,err_C(r+gm)*z_gm*x_gm**gm)
          err_gm(r) = max(err_gm_exp,err_gm_C)
          if (err_gm_exp.lt.err_gm_C.or.err_gm(r).lt.err_req_D) exit
        end do
      ! increase gm by 2 to account for bad estimates
        gm = min(max(gm+2,2*gm),rmax_D-r)
      ! scale estimates down to allow trying other methods
        err_gm(r)  =  err_gm(r)/impest_D
      end if
#endif


#ifdef Dredtest
      write(*,*) 'CalcDred: bef final loop ord methods',r,g,gy,gp,gr,gm,gpf
      write(*,*) 'CalcDred: bef final loop err methods',r,err_pv(r),err_pv2(r), &
          err_g(r),err_gy(r),err_gp(r),err_gr(r),err_gm(r),err_gpf(r)
      write(*,*) 'CalcDred: bef final loop acc methods',r,err_pv(r)/Dtyp,err_pv2(r)/Dtyp,   &
          err_g(r)/Dtyp,err_gy(r)/Dtyp,err_gp(r)/Dtyp, &
          err_gr(r)/Dtyp,err_gm(r)/Dtyp,err_gpf(r)/Dtyp
      write(*,*) 'CalcDred: bef final loop method',r,DrCalc(r),Drmethod(r)
#endif

100   continue   ! try other methods if error larger than expected
      if (min(err_pv(r),err_pv2(r)).le.min(err_g(r),err_gy(r),err_gp(r),err_gr(r),err_gpf(r))       &
          .and.min(err_pv(r),err_pv2(r)).lt.err_inf) then

        if (use_pv.and.err_pv(r).le.err_pv2(r).and.mod(DrCalc(r),2).ne.1) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr1_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr1_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

#ifdef Dredtest
          write(*,*) 'CalcDred: call Dpv 2',r,id
#endif
          
!         write(*,*) 'CalcDred: Dpv r',r,rmax,p10
!         write(*,*) 'CalcDred: Dpv Duv',size(Duv)
!         write(*,*) 'CalcDred: Dpv Duv_alt',size(Duv_alt)
         

          if (r.eq.rmax) then
            call CalcDpv1(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,id,Derr1_alt,Derr2_alt)
          else
            call CalcDpv1(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),  &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(11) = DCount(11)+1
          DrCalc(0:r)=DrCalc(0:r)+1
          DCalc = DCalc+1
          Drmethod_alt(0:r)=1
          checkest=Derr_alt(r)/err_pv(r)
          
#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_pv imprecise',err_pv(r),Derr_alt(r)
          end if
#endif

#ifdef Dredtest
       write(*,*) 'final loop r Dpv D(1,0,1,0)',r,D_alt(1,0,1,0),D(1,0,1,0)
       write(*,*) 'final loop r Dpv Derr',Derr_alt(2),Derr(2)
#endif
          err_pv(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),   &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after pv 2nd try Dmethod_alt=',Drmethod_alt
          write(*,*) 'CalcDred: after pv 2nd try Derr_alt(r)=',Derr_alt
          write(*,*) 'CalcDred: after pv 2nd try Dacc_alt(r)=',Derr_alt/Dtyp
          write(*,*) 'CalcDred: after pv 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after pv 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after pv 2nd try Dacc(r)=',Derr/Dtyp
#endif

#ifdef Dredtest
!       if(r.gt.2)then
!       write(*,*) 'after CalcDpv D(1,0,0,0)',r,D_alt(1,0,0,0),D(1,0,0,0)
!       endif
#endif
          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
          
        elseif (use_pv2.and.err_pv2(r).le.err_pv(r).and.mod(DrCalc(r),4)-mod(DrCalc(r),2).ne.2) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

#ifdef Dredtest
          write(*,*) 'CalcDred: call Dpv2 2',r,id
#endif
          if (r.eq.rmax) then
            call CalcDpv2(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,id,Derr1_alt,Derr2_alt)
          else
            call CalcDpv2(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),  &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if

#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(12) = DCount(12)+1
          DrCalc(0:r)=DrCalc(0:r)+2
          DCalc = DCalc+2
          Drmethod_alt(0:r)=2
          checkest=Derr_alt(r)/err_pv2(r)
          
#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_pv2 imprecise',err_pv2(r),Derr_alt(r)
          end if
#endif
          err_pv2(0:r)=Derr_alt(0:r)

#ifdef Dredtest
       if(r.ge.2) then
       write(*,*) 'final loop r Dpv2 D(1,0,1,0)',r,D_alt(1,0,1,0),D(1,0,1,0)
       write(*,*) 'final loop r Dpv2 Derr',Derr_alt(2),Derr(2)
       endif
#endif
          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),   &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
          
#ifdef Dredtest
          write(*,*) 'CalcDred: after pv 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after pv 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after pv 2nd try Dacc(r)=',Derr/Dtyp
#endif

        end if

      else 

#ifdef Dredtest
        write(*,*) 'CalcDred: explore exps once more'
#endif

        if (use_g.and.err_g(r).le.min(err_gy(r),err_gp(r),err_gr(r),err_gpf(r))        &
            .and.mod(DrCalc(r),8)-mod(DrCalc(r),4).ne.4) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

          if (r.eq.rmax) then
            call CalcDg(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,g,rmax_D,id,Derr1_alt,Derr2_alt)
          else
            call CalcDg(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),  &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,g,rmax_D,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(13) = DCount(13)+1
          DrCalc(0:r)=DrCalc(0:r)+4
          DCalc = DCalc+4
          Drmethod_alt(0:r)=4
          checkest=Derr_alt(r)/err_g(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_g imprecise ',err_g(r),Derr_alt(r)
          end if
#endif
          
          err_g(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),  &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

#ifdef Dredtest
!       if(r.gt.2)then
!       write(*,*) 'after CalcDg D(1,0,1,0)',r,D_alt(1,0,1,0),D(1,0,1,0)
!       endif
#endif

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
          
        else if (use_gy.and.err_gy(r).le.min(err_g(r),err_gp(r),err_gr(r),err_gpf(r))        &
            .and.mod(DrCalc(r),16)-mod(DrCalc(r),8).ne.8) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

          if (r.eq.rmax) then
            call CalcDgy(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gy,(rmax_D)/2,id,Derr1_alt,Derr2_alt)
          else
            call CalcDgy(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),   &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gy,(rmax_D)/2,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(14) = DCount(14)+1
          DrCalc(0:r)=DrCalc(0:r)+8
          DCalc = DCalc+8
          Drmethod_alt(0:r)=8
          checkest=Derr_alt(r)/err_gy(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_Dgy.or.checkest.lt.1d-2*impest_Dgy) then
            write(*,*) 'CalcDred: estimate err_gy imprecise',err_gy(r),Derr_alt(r)
          end if
#endif
          
          err_gy(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),  &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

#ifdef Dredtest
!       if(rmax.ge.3)then
!       write(*,*) 'after CalcDgy D(1,0,0,0)',r,D_alt(1,0,0,0),D(1,0,0,0)
!       endif
#endif

          if(checkest.gt.impest_Dgy.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods

        elseif (use_gp.and.err_gp(r).le.min(err_g(r),err_gy(r),err_gr(r),err_gpf(r))    &
            .and.mod(DrCalc(r),32)-mod(DrCalc(r),16).ne.16) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

          if (r.eq.rmax) then
            call CalcDgp(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gp,rmax_D,id,Derr1_alt,Derr2_alt)
          else
            call CalcDgp(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),   &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gp,rmax_D,id,Derr1_alt(0:r),Derr2_alt(0:r))
          endif
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(15) = DCount(15)+1
          DrCalc(0:r)=DrCalc(0:r)+16
          DCalc = DCalc+16
          Drmethod_alt(0:r)=16
          checkest=Derr_alt(r)/err_gp(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_gp imprecise',err_gp(r),Derr_alt(r)
          end if
#endif
          
          err_gp(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),   &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
          
        elseif (use_gr.and.err_gr(r).le.min(err_g(r),err_gy(r),err_gp(r),err_gpf(r))        &
            .and.mod(DrCalc(r),64)-mod(DrCalc(r),32).ne.32) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

          if (r.eq.rmax) then
            call CalcDgr(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gr,rmax_D,id,Derr1_alt,Derr2_alt)
          else
            call CalcDgr(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),  &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gr,rmax_D,id,Derr1_alt(0:r),Derr2_alt(0:r))
          endif
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(16) = DCount(16)+1
          DrCalc(0:r)=DrCalc(0:r)+32
          DCalc = DCalc+32
          Drmethod_alt(0:r)=32
          checkest=Derr_alt(r)/err_gr(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_gr imprecise',err_gr(r),Derr_alt(r)
          end if
#endif
          
          err_gr(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),    &  
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)
 
          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
          

#ifdef Dredtest
!     if(r.gt.2)then
!       write(*,*) 'CalcDred D(1,0,1,0)',r,D(1,0,1,0)
!     endif
#endif

        else if (use_gpf.and.err_gpf(r).le.min(err_g(r),err_gy(r),err_gp(r),err_gr(r))        &
            .and.mod(DrCalc(r),128)-mod(DrCalc(r),64).ne.64) then

          if (r.eq.rmax) then
            call CalcDgpf(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gpf,(rmax_D)/2,id,Derr1_alt,Derr2_alt)
          else
            call CalcDgpf(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),   &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,gpf,(rmax_D)/2,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(17) = DCount(17)+1
          DrCalc(0:r)=DrCalc(0:r)+64
          DCalc = DCalc+64
          Drmethod_alt(0:r)=64
          checkest=Derr_alt(r)/err_gpf(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_gpf imprecise',err_gpf(r),Derr_alt(r)
          end if
#endif
          
          err_gpf(0:r)=Derr_alt(0:r)
          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),  &
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

#ifdef Dredtest
!       if(rmax.ge.3)then
!       write(*,*) 'after CalcDgpf D(1,0,0,0)',r,D_alt(1,0,0,0),D(1,0,0,0)
!       endif
#endif

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods

#ifdef USEGM
        else if (use_gm.and.err_gm(r).le.min(err_gy(r),err_gp(r),err_gr(r),err_g(r))        &
            .and.mod(DrCalc(r),128)-mod(DrCalc(r),64).ne.64) then

!          deallocate(D_alt)
!          deallocate(Duv_alt)
!          deallocate(Derr_alt)
!          deallocate(Derr2_alt)
!          deallocate(Drmethod_alt)
!          allocate(D_alt(0:r,0:r,0:r,0:r))
!          allocate(Duv_alt(0:r,0:r,0:r,0:r))
!          allocate(Derr_alt(0:r))
!          allocate(Derr2_alt(0:r))
!          allocate(Drmethod_alt(0:r))

          if (r.eq.rmax) then
            call CalcDgm(D_alt,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,g,rmax_D,id,Derr1_alt,Derr2_alt)
          else
            call CalcDgm(D_alt(0:r,0:r,0:r,0:r),Duv_alt(0:r,0:r,0:r,0:r),  &
                p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,r,g,rmax_D,id,Derr1_alt(0:r),Derr2_alt(0:r))
          end if
#ifdef PVEST2
          Derr_alt = Derr2_alt
#else
          Derr_alt = Derr1_alt
#endif
          DCount(17) = DCount(17)+1
          DrCalc(0:r)=DrCalc(0:r)+64
          DCalc = DCalc+64
          Drmethod_alt(0:r)=64
          checkest=Derr_alt(r)/err_gm(r)

#ifdef Dredtest
          if(checkest.gt.1d2*impest_D.or.checkest.lt.1d-2*impest_D) then
            write(*,*) 'CalcDred: estimate err_g imprecise ',err_gm(r),Derr_alt(r)
          end if
#endif
          
          err_gm(0:r)=Derr_alt(0:r)

          call CopyDimp3(D,D_alt(0:r,0:r,0:r,0:r),Derr,Derr_alt(0:r),Derr1,Derr1_alt(0:r),  &  
              Derr2,Derr2_alt(0:r),Drmethod,Drmethod_alt(0:r),rmax,r)

          if (rmax.ge.1) then
            Dtyp =  max(abs(D(0,0,0,0)),        &
                abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))
          else
            Dtyp =  abs(D(0,0,0,0))
          end if
          err_req_D = acc_req_D * Dtyp

#ifdef Dredtest
          write(*,*) 'CalcDred: after exp 2nd try Dmethod=',Drmethod
          write(*,*) 'CalcDred: after exp 2nd try Derr(r)=',Derr
          write(*,*) 'CalcDred: after exp 2nd try Dacc(r)=',Derr/Dtyp
#endif

#ifdef Dredtest
!       if(r.gt.2)then
!       write(*,*) 'after CalcDg D(1,0,1,0)',r,D_alt(1,0,1,0),D(1,0,1,0)
!       endif
#endif

          if(checkest.gt.impest_D.and.Mode_coli.lt.1) goto 100     ! error larger than expected: try other methods
#endif
          
        end if
      end if

#ifndef USED0
#ifndef ALWAYSPV
      ! refine error estimate for D0
      if(.not.lerr_D0) then
!        D0est =  abs(D(0,0,0,0))
        err_D0 = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
!        err_req_D = acc_req_D * abs(D(0,0,0,0))
        lerr_D0 = .true.
      end if
#endif
#endif

#ifdef Dredtest
!       if(r.ge.2) then
!       write(*,*) 'after r CalcDg D(1,0,1,0)',r,D_alt(1,0,1,0),D(1,0,1,0)
!       write(*,*) 'after r CalcDg Derr',Derr_alt(2),Derr(2)
!       endif
#endif


#ifdef Dredtest
      write(*,*) 'CalcDred: final loop err methods',r,err_pv(r),err_pv2(r), &
          err_g(r),err_gy(r),err_gp(r),err_gr(r),err_gm(r),err_gpf(r)
      write(*,*) 'CalcDred: final loop acc methods',r,err_pv(r)/Dtyp,err_pv2(r)/Dtyp,   &
          err_g(r)/Dtyp,err_gy(r)/Dtyp,err_gp(r)/Dtyp, &
          err_gr(r)/Dtyp,err_gm(r)/Dtyp,err_gpf(r)/Dtyp
      write(*,*) 'CalcDred: final loop method',r,DrCalc(r),Drmethod(r)
#endif

    end do

    norm = abs(D(0,0,0,0))

    do r=1,rmax
      do n1=0,rmax
        do n2=0,rmax-n1
          n3 = rmax-n1-n2
          norm =  max(norm,abs(D(0,n1,n2,n3)))
        end do
      end do
    end do
    acc_D = Derr(rmax)/norm

    DCount(DCalc+DCountoffset0) = DCount(DCalc+DCountoffset0)+1

#ifdef Dredtest
    write(*,*) 'CalcDred final err_D=',Derr
    write(*,*) 'CalcDred final acc_D=',Derr/norm,critacc_coli
    write(*,*) 'CalcDred final method_D=',Drmethod
#endif

    if (acc_D.gt.sqrt(reqacc_coli)) then
      DCount(DCalc+DCountoffset3) = DCount(DCalc+DCountoffset3)+1
    end if

    if (acc_D.gt.reqacc_coli) then
      DCount(DCalc+DCountoffset1) = DCount(DCalc+DCountoffset1)+1
    end if

    if (acc_D.gt.critacc_coli) then

      DCount(DCalc+DCountoffset2) = DCount(DCalc+DCountoffset2)+1

#ifdef Dredtest
      write(*,*) 'CritPoint D',critacc_coli,acc_D
      write(*,*) 'CritPoint D',CritPointCntD,MaxCritPointD
#endif      

!      call SetErrFlag_coli(-5)
!      call ErrOut_coli('CalcDred',' critical accuracy not reached',  &
!         errorwriteflag)

#ifdef CritPointsCOLI 
      CritPointCntD = CritPointCntD + 1    

      if (CritPointCntD.le.MaxCritPointD.and.Monitoring) then

        call CritPointsOut_coli('D_coli',acc_D)
        write(ncpout_coli,*) 'arguments of CalcDred_coli:'
        write(ncpout_coli,*) 'rank = ', rmax
        write(ncpout_coli,fmt1) 'p10 = ', p10
        write(ncpout_coli,fmt1) 'p21 = ', p21
        write(ncpout_coli,fmt1) 'p32 = ', p32
        write(ncpout_coli,fmt1) 'p30 = ', p30
        write(ncpout_coli,fmt1) 'p20 = ', p20
        write(ncpout_coli,fmt1) 'p31 = ', p31
        write(ncpout_coli,fmt1) 'm02 = ', m02
        write(ncpout_coli,fmt1) 'm12 = ', m12
        write(ncpout_coli,fmt1) 'm22 = ', m22
        write(ncpout_coli,fmt1) 'm32 = ', m32
        if (CritPointCntD.eq.MaxCritPointD) then
          write(ncpout_coli,*)
          write(ncpout_coli,*)
          write(ncpout_coli,*)
          write(ncpout_coli,*) '***********************************************************'
          write(ncpout_coli,*)
          write(ncpout_coli,*) ' Further output of bad D functions will be suppressed '
        end if
      end if
#endif
    end if

#ifdef Dredtest
       write(*,*) 'CalcDred exit D(1,0,0,0)',r,D_alt(1,0,0,0),D(1,0,0,0)
#endif

  end subroutine CalcDred







  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDuv(Duv,Cuv_0,m02,f,rmax,id)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDuv(Duv,Cuv_0,m02,f,rmax,id)
  
    integer, intent(in) :: rmax,id
    double complex, intent(in) :: m02,f(3)
    double complex, intent(inout) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(in) :: Cuv_0(0:rmax-1,0:rmax-1,0:rmax-1,0:rmax-1)
    integer :: r,n0,n1,n2,n3
        
    ! D_(n0,n1,n2,n3) UV-finite for n0<2
    Duv(0:min(rmax,1),:,:,:) = 0d0
    
    ! PV reduction (5.10)
!    do r=4,rmax
!      do n0=2,r/2
    do r=4,rmax+1
      do n0=max(2,r-rmax),r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2
        
            Duv(n0,n1,n2,n3) = (Cuv_0(n0-1,n1,n2,n3) + 2*m02*Duv(n0-1,n1,n2,n3)  &
                                + f(1)*Duv(n0-1,n1+1,n2,n3)  & 
                                + f(2)*Duv(n0-1,n1,n2+1,n3)  & 
                                + f(3)*Duv(n0-1,n1,n2,n3+1)) / (2*(r-1))
    
          end do
        end do
      end do
    end do
    
  end subroutine CalcDuv





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDpv1(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  !
  !  new version 10.02.2016   (5.10) with (5.11) inserted
  !              14.09.2016    prefactors of C_0 improved
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDpv1(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: C_0(0:rmax-1,0:rmax-1,0:rmax-1,0:rmax-1), Cuv_0(0:rmax-1,0:rmax-1,0:rmax-1,0:rmax-1)
    double complex :: C_i(0:rmax-1,0:rmax-1,0:rmax-1,3), Cuv_i(0:rmax-1,0:rmax-1,0:rmax-1,3)
    double complex :: D_alt(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Cerr_i(0:rmax-1,0:3),Cerr2_i(0:rmax-1,0:3)
!   double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:)
!   double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
!   double complex, allocatable :: D_alt(:,:,:,:)
!   double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3)
    double complex :: D0_coli, elimminf2_coli
 !  double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:)
 !  double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: D00_err(0:rmax),Dij_err(0:rmax),Cij_err(0:rmax-1)
    double precision :: D00_err2(0:rmax),Dij_err2(0:rmax),Cij_err2(0:rmax-1)
    integer :: rmaxC,r,n0,n1,n2,n3,nn0,nn1,nn2,nn3,i,j
    integer :: bin,k,nid(0:3)

!    if (id.eq.0) write(*,*) 'CalcDpv1 in', rmax, id
        
    ! calculation of scalar coefficient
    D(0,0,0,0) = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
    Duv(0,0,0,0) = 0d0

    ! accuracy estimate for D0 function
    ! detX=0 implemented 16.08.2018
    if(adetX.ne.0d0) then
      Derr(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
    else
      Derr(0) = acc_def_D0* abs(D(0,0,0,0))
    end if
    Derr2(0) = Derr(0)

    if (rmax.eq.0) return

    ! allocation of C functions
    rmaxC = rmax-1
    ! rmaxC = max(rmax-1,0)
!   allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
!   allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
!   allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
!   allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
!   allocate(Cerr_i(0:rmaxC,0:3))
!   allocate(Cerr2_i(0:rmaxC,0:3))

    ! allocate arrays for error propagation
!   allocate(D00_err(0:rmax))
!   allocate(Dij_err(0:rmax))
!   allocate(Cij_err(0:rmaxC))
    
!   allocate(D00_err2(0:rmax))
!   allocate(Dij_err2(0:rmax))
!   allocate(Cij_err2(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do


    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0))
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1))
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2))
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3))

#ifdef Dpv1test
    write(*,*) 'CalcDpv1 Cerr_i=',Cerr_i(:,0)
    write(*,*) 'CalcDpv1 Cerr_i=',Cerr_i(:,1)
    write(*,*) 'CalcDpv1 Cerr_i=',Cerr_i(:,2)
    write(*,*) 'CalcDpv1 Cerr_i=',Cerr_i(:,3)
#endif

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
              -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do


    ! calculate Duv
    call CalcDuv(Duv,Cuv_0,mm02,f,rmax,id)

    ! initialization of error propagation

    Dij_err =0d0
    D00_err =0d0
    Dij_err(0) = Derr(0)
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))
    
    Dij_err2 =0d0
    D00_err2 =0d0
    Dij_err2(0) = Derr2(0)
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))
    
#ifdef Dpv1test
    write(*,*) 'CalcDpv1 Cij_err=',Cij_err
    write(*,*) 'CalcDpv1 Dij_err(0)=',Dij_err(0)
    write(*,*) 'CalcDpv1 test :', & 
            (1d0 - (Zadjf(1)+Zadjf(2)+Zadjf(3))/detZ), &
            (detZmZadjf + Zadjs(1)*(mm12-mm02) + Zadjs(2)*(mm22-mm02) &
            + Zadjs(3)*(mm32-mm02) ) /detZ
#endif

!   allocate(D_alt(0:rmax,0:rmax,0:rmax,0:rmax))

    ! PV reduction
    do r=1,rmax

      do n0=r/2,1,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

    ! reduction formula (5.10) with (5.11) inserted for n0 >= 1

            D(n0,n1,n2,n3) = 4*Duv(n0,n1,n2,n3) + detX/detZ*D(n0-1,n1,n2,n3) 

            D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                + (detZmZadjf + Zadjs(1)*(mm12-mm02) + Zadjs(2)*(mm22-mm02) &
                    + Zadjs(3)*(mm32-mm02) ) /detZ * C_0(n0-1,n1,n2,n3)
!               + (1d0 - (Zadjf(1)+Zadjf(2)+Zadjf(3))/detZ)* C_0(n0-1,n1,n2,n3)

            if (n1.ge.1) then
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  - 2*n1*Zadjf(1)/detZ*D(n0,n1-1,n2,n3)
            else            
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  + Zadjf(1)/detZ* C_i(n0-1,n2,n3,1)
            end if

            if (n2.ge.1) then
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  - 2*n2*Zadjf(2)/detZ*D(n0,n1,n2-1,n3)
            else            
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  + Zadjf(2)/detZ * C_i(n0-1,n1,n3,2)
            end if

            if (n3.ge.1) then
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  - 2*n3*Zadjf(3)/detZ*D(n0,n1,n2,n3-1)
            else            
              D(n0,n1,n2,n3) = D(n0,n1,n2,n3) &
                  + Zadjf(3)/detZ * C_i(n0-1,n1,n2,3)
            end if

            D(n0,n1,n2,n3) = D(n0,n1,n2,n3)  / (2*(r-1)) 

          end do
        end do
      end do

    ! reduction formula (5.11) with (5.10) inserted for n0 = 0
!     do n0=(r-1)/2,0,-1
      n0=0
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2
         
            if (n1.ge.1) then
              nn1 = n1-1
              nn2 = n2
              nn3 = n3
              j = 1
            else if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

!            do i=1,3
!              Smod(i) = -C_0(n0,nn1,nn2,nn3)
!            end do
            Smod = 0d0  
          
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D(n0,n1,n2,n3) = (Zadj(1,j)*Smod(1) + Zadj(2,j)*Smod(2)  &
                           + Zadj(3,j)*Smod(3) &
                           - Zadjs(j)*C_0(n0,nn1,nn2,nn3) &
                           - Zadjf(j)*D(n0,nn1,nn2,nn3))/detZ

         end do
        end do
!     end do


      ! determine error from symmetry for n0=0 and n1>1, n2>1 
      Derr(r)=Derr(r-1)
      Derr2(r)=Derr2(r-1)

      ! use error on detZ for r=1 (enters r>1 implicitly)
      ! added 13.09.2022
      if (r==1) then
        Derr(r) = max( Derr(r), &
            max(abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))*dprec_cll/detZerr)
        Derr2(r) = max( Derr2(r), &
            max(abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))*dprec_cll/detZerr)
      endif

!      write(*,*) 'Derr(1) = ',Derr(1),max(abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1))),dprec_cll/detZerr
!      write(*,*) 'CalcDpv1: Derr(r)',r,Derr(r),Derr2(r)

      n0=0
      do n1=0,r-2*n0
        do n2=0,r-2*n0-n1
          n3 = r-2*n0-n1-n2
          if (n1.ge.1.and.n2+n3.ge.1) then
         
            if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

!            do i=1,3
!              Smod(i) = -C_0(n0,nn1,nn2,nn3)
!            end do
            Smod = 0d0
             
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D_alt(n0,n1,n2,n3) = (Zadj(1,j)*Smod(1) + Zadj(2,j)*Smod(2)  &
                           + Zadj(3,j)*Smod(3) &
                           - Zadjs(j)*C_0(n0,nn1,nn2,nn3)  &
                           - Zadjf(j)*D(n0,nn1,nn2,nn3))/detZ
 
            Derr(r)=max(Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))
            Derr2(r)=max(Derr2(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))

#ifdef Dpv1test
!            write(*,*) 'CalcDpv1: errpr',r,Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)), &
!                        D(n0,n1,n2,n3),D_alt(n0,n1,n2,n3),n0,n1,n2,n3
#endif


          end if
        end do
      end do

      if(r.ge.2)then

! estimate using insertions of (5.11) in (5.10)
        D00_err(r) = max(2*abs(m02)*Dij_err(r-2), Cerr_i(r-2,0),    &
              aZadjff/adetZ*Dij_err(r-2),             &
              maxZadjf/adetZ*max(2*D00_err(r-1),Cij_err(r-2)))/(2*(r-1))
      else
        D00_err(r) = 0d0
      end if
      Dij_err(r) = max(maxZadjf*Dij_err(r-1),   &
              maxZadj*max(2*D00_err(r),Cij_err(r-1)))/adetZ

      if(r.ge.2)then
! estimate using insertions of (5.11) in (5.10)
        D00_err2(r) = max(2*abs(m02)*Dij_err2(r-2), Cerr2_i(r-2,0),    &
              aZadjff/adetZ*Dij_err2(r-2),             &
              maxZadjf/adetZ*max(2*D00_err2(r-1),Cij_err2(r-2)))/(2*(r-1))
      else
        D00_err2(r) = 0d0
      end if
      Dij_err2(r) = max(maxZadjf*Dij_err2(r-1),   &
              maxZadj*max(2*D00_err2(r),Cij_err2(r-1)))/sqrt(adetZ*maxZ*maxZadj)

#ifdef Dpv1test
    write(*,*) 'Dij_err(r)', r,Dij_err(r),D00_err(r)
    write(*,*) 'Dij_err_jj',maxZadjf*Dij_err/adetZ
    write(*,*) 'Dij_err_00',maxZadj*D00_err(1:rmax)/adetZ
    write(*,*) 'Dij_err_cc',maxZadj*Cij_err/adetZ
    write(*,*) 'factors',maxZadj/adetZ,maxZadjf/adetZ,maxZadj*maxZ/adetZ
    write(*,*) 'Dij_err2(r)', r,Dij_err2(r),D00_err2(r)
    write(*,*) 'Dij_err2_jj',maxZadjf*Dij_err/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_00',maxZadj*D00_err(1:rmax)/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_cc',maxZadj*Cij_err/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'factors2',maxZadj/sqrt(adetZ*maxZ*maxZadj),maxZadjf/sqrt(adetZ*maxZ*maxZadj)
#endif

    end do
    
      ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
!    do r=rmax+1,2*rmax
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 
          end do
        end do
      end do
    end do

#ifdef Dpv1test
    write(*,*) 'CalcDpv1 Derrsym',Derr
    write(*,*) 'CalcDpv1 Daccsym',Derr/abs(D(0,0,0,0))

    write(*,*) 'Dij_err_jj',maxZadjf*Dij_err/adetZ
    write(*,*) 'Dij_err_00',maxZadj*D00_err(1:rmax)/adetZ
    write(*,*) 'Dij_err_cc',maxZadj*Cij_err/adetZ
    write(*,*) 'CalcDpv1 Dijerr',Dij_err(1:rmax)
    write(*,*) 'CalcDpv1 Dijacc',Dij_err(1:rmax)/abs(D(0,0,0,0))
 
    write(*,*) 'CalcDpv1 Derr2sym',Derr2
    write(*,*) 'CalcDpv1 Dacc2sym',Derr2/abs(D(0,0,0,0))

    write(*,*) 'Dij_err2_jj',maxZadjf*Dij_err2/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_00',maxZadj*D00_err2(1:rmax)/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_cc',maxZadj*Cij_err2/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'CalcDpv1 Dijerr2',Dij_err2(1:rmax)
    write(*,*) 'CalcDpv1 Dijacc2',Dij_err2(1:rmax)/abs(D(0,0,0,0))
 
#endif

    Derr2 = max(Derr2,Dij_err2(0:rmax))
    Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dpv1test
    write(*,*) 'CalcDpv1 D(0,0,0,0) = ',D(0,0,0,0)
    if(rmax.ge.3)then
      write(*,*) 'CalcDpv1 D(0,1,1,1) = ',D(0,1,1,1)
    endif

    write(*,*) 'CalcDpv1 Derr',Derr
    write(*,*) 'CalcDpv1 Dacc',Derr/abs(D(0,0,0,0))
    write(*,*) 'CalcDpv1 Derr2',Derr2
    write(*,*) 'CalcDpv1 Dacc2',Derr2/abs(D(0,0,0,0))
#endif

!    if (id.eq.0) then
!    write(*,*) 'CalcDpv1 Derr ',Derr
!    write(*,*) 'CalcDpv1 Derr2',Derr2
!    end if
    
  end subroutine CalcDpv1


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDpv1o(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDpv1o(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3)
    double complex :: D0_coli, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    integer :: rmaxC,r,n0,n1,n2,n3,nn0,nn1,nn2,nn3,i,j
    integer :: bin,k,nid(0:3)

!    if (id.eq.0) write(*,*) 'CalcDpv1o in', rmax, id
        
    ! calculation of scalar coefficient
    D(0,0,0,0) = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
    Duv(0,0,0,0) = 0d0

    ! accuracy estimate for D0 function
    ! detX=0 implemented 16.08.2018
    if(adetX.ne.0d0) then
      Derr(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
    else
      Derr(0) = acc_def_D0* abs(D(0,0,0,0))
    end if
    Derr2(0) = Derr(0)

    if (rmax.eq.0) return

    ! allocation of C functions
    rmaxC = rmax-1
    ! rmaxC = max(rmax-1,0)
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmax))
    allocate(Dij_err(0:rmax))
    allocate(Cij_err(0:rmaxC))
    
    allocate(D00_err2(0:rmax))
    allocate(Dij_err2(0:rmax))
    allocate(Cij_err2(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do


    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0))
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1))
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2))
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3))

#ifdef Dpv1otest
    write(*,*) 'CalcDpv1o Cerr_i=',Cerr_i(:,0)
    write(*,*) 'CalcDpv1o Cerr_i=',Cerr_i(:,1)
    write(*,*) 'CalcDpv1o Cerr_i=',Cerr_i(:,2)
    write(*,*) 'CalcDpv1o Cerr_i=',Cerr_i(:,3)
#endif

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
              -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do

    
    ! determine inverse Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)
 

!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)

!    Zinv(1,1) = (4d0*q30*q20-q2q3*q2q3)/detZ
!    Zinv(2,1) = (q1q3*q2q3-2d0*q30*q1q2)/detZ
!    Zinv(3,1) = (q1q2*q2q3-2d0*q20*q1q3)/detZ
!    Zinv(1,2) = Zinv(2,1)
!    Zinv(2,2) = (4d0*q10*q30-q1q3*q1q3)/detZ
!    Zinv(3,2) = (q1q2*q1q3-2d0*q10*q2q3)/detZ
!    Zinv(1,3) = Zinv(3,1)
!    Zinv(2,3) = Zinv(3,2)
!    Zinv(3,3) = (4d0*q10*q20-q1q2*q1q2)/detZ
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32

!  commented out 2.9.17
!   Zinv = Zadj/detZ

    ! calculate Duv
    call CalcDuv(Duv,Cuv_0,mm02,f,rmax,id)

    ! initialization of error propagation
!    Zadj=Zinv*detZ    

!    maxZadj = max(abs(Zadj(1,1)),abs(Zadj(2,1)),abs(Zadj(3,1)),  &
!                  abs(Zadj(2,2)),abs(Zadj(3,2)),abs(Zadj(3,3)))

!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)
!    maxZadjf = max(abs(Zadjf(1)),abs(Zadjf(2)),abs(Zadjf(3)))
!
!    aZadjff = abs(Zadjf(1)*f(1)+Zadjf(2)*f(2)+Zadjf(3)*f(3))

!    adetZ = abs(detZ)
!    adetX = abs(2d0*mm02*detZ-Zadjf(1)*f(1)-Zadjf(2)*f(2)-Zadjf(3)*f(3))

    Dij_err =0d0
    D00_err =0d0
    Dij_err(0) = Derr(0)
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))
    
    Dij_err2 =0d0
    D00_err2 =0d0
    Dij_err2(0) = Derr2(0)
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))
    
#ifdef Dpv1otest
    write(*,*) 'CalcDpv1o Cij_err=',Cij_err
    write(*,*) 'CalcDpv1o Dij_err(0)=',Dij_err(0)
#endif

    allocate(D_alt(0:rmax,0:rmax,0:rmax,0:rmax))

    ! PV reduction
    do r=1,rmax

      do n0=r/2,1,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

        ! reduction formula (5.10) for D(r/2,0,0,0)
            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3) + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

          end do
        end do
      end do


!     do n0=(r-1)/2,0,-1
      n0=0
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2
         
            if (n1.ge.1) then
              nn1 = n1-1
              nn2 = n2
              nn3 = n3
              j = 1
            else if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

            do i=1,3
              Smod(i) = -C_0(n0,nn1,nn2,nn3)-f(i)*D(n0,nn1,nn2,nn3)
            end do
          
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D(n0,n1,n2,n3) = Zinv(1,j)*Smod(1) + Zinv(2,j)*Smod(2)  &
                           + Zinv(3,j)*Smod(3)

          end do
        end do
!     end do

      ! determine error from symmetry for n0=0 and n1>1, n2>1 
      Derr(r)=Derr(r-1)
      Derr2(r)=Derr2(r-1)
      n0=0
      do n1=0,r-2*n0
        do n2=0,r-2*n0-n1
          n3 = r-2*n0-n1-n2
          if (n1.ge.1.and.n2+n3.ge.1) then
         
            if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

            do i=1,3
              Smod(i) = -C_0(n0,nn1,nn2,nn3)-f(i)*D(n0,nn1,nn2,nn3)
            end do
          
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D_alt(n0,n1,n2,n3) = Zinv(1,j)*Smod(1) + Zinv(2,j)*Smod(2)  &
                           + Zinv(3,j)*Smod(3)
 
            Derr(r)=max(Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))
            Derr2(r)=max(Derr2(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))

#ifdef Dpv1otest
!            write(*,*) 'CalcDpv: errpr',r,Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)), &
!                        D(n0,n1,n2,n3),D_alt(n0,n1,n2,n3),n0,n1,n2,n3
#endif


          end if
        end do
      end do

      if(r.ge.2)then
! 09.02.2016
! old estimate using insertions of (5.11) in (5.10)
        D00_err(r) = max(2*abs(m02)*Dij_err(r-2), Cerr_i(r-2,0),    &
              aZadjff/adetZ*Dij_err(r-2),             &
              maxZadjf/adetZ*max(2*D00_err(r-1),Cij_err(r-2)))/(2*(r-1))
! new estimate 
!       D00_err(r) = max(2*abs(m02)*Dij_err(r-2), Cerr_i(r-2,0),    &
!             fmax*Dij_err(r-1) )/(2*(r-1))
      else
        D00_err(r) = 0d0
      end if
      Dij_err(r) = max(maxZadjf*Dij_err(r-1),   &
              maxZadj*max(2*D00_err(r),Cij_err(r-1)))/adetZ

      if(r.ge.2)then
! old estimate using insertions of (5.11) in (5.10)
        D00_err2(r) = max(2*abs(m02)*Dij_err2(r-2), Cerr2_i(r-2,0),    &
              aZadjff/adetZ*Dij_err2(r-2),             &
              maxZadjf/adetZ*max(2*D00_err2(r-1),Cij_err2(r-2)))/(2*(r-1))
! new estimate 
!       D00_err2(r) = max(2*abs(m02)*Dij_err2(r-2), Cerr2_i(r-2,0),    &
!             fmax*Dij_err2(r-1)  )/(2*(r-1))
      else
        D00_err2(r) = 0d0
      end if
      Dij_err2(r) = max(maxZadjf*Dij_err2(r-1),   &
              maxZadj*max(2*D00_err2(r),Cij_err2(r-1)))/sqrt(adetZ*maxZ*maxZadj)

#ifdef Dpv1otest
    write(*,*) 'Dij_err(r)', r,Dij_err(r),D00_err(r)
    write(*,*) 'Dij_err_jj',maxZadjf*Dij_err/adetZ
    write(*,*) 'Dij_err_00',maxZadj*D00_err(1:rmax)/adetZ
    write(*,*) 'Dij_err_cc',maxZadj*Cij_err/adetZ
    write(*,*) 'factors',maxZadj/adetZ,maxZadjf/adetZ
    write(*,*) 'Dij_err2(r)', r,Dij_err2(r),D00_err2(r)
    write(*,*) 'Dij_err2_jj',maxZadjf*Dij_err/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_00',maxZadj*D00_err(1:rmax)/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_cc',maxZadj*Cij_err/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'factors2',maxZadj/sqrt(adetZ*maxZ*maxZadj),maxZadjf/sqrt(adetZ*maxZ*maxZadj)
#endif

    end do
    
      ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
!    do r=rmax+1,2*rmax
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 
          end do
        end do
      end do
    end do

#ifdef Dpv1otest
    write(*,*) 'CalcDpv1o Derrsym',Derr
    write(*,*) 'CalcDpv1o Daccsym',Derr/abs(D(0,0,0,0))

    write(*,*) 'Dij_err_jj',maxZadjf*Dij_err/adetZ
    write(*,*) 'Dij_err_00',maxZadj*D00_err(1:rmax)/adetZ
    write(*,*) 'Dij_err_cc',maxZadj*Cij_err/adetZ
    write(*,*) 'CalcDpv1o Dijerr',Dij_err(1:rmax)
    write(*,*) 'CalcDpv1o Dijacc',Dij_err(1:rmax)/abs(D(0,0,0,0))
 
    write(*,*) 'CalcDpv1o Derr2sym',Derr2
    write(*,*) 'CalcDpv1o Dacc2sym',Derr2/abs(D(0,0,0,0))

    write(*,*) 'Dij_err2_jj',maxZadjf*Dij_err2/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_00',maxZadj*D00_err2(1:rmax)/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'Dij_err2_cc',maxZadj*Cij_err2/sqrt(adetZ*maxZ*maxZadj)
    write(*,*) 'CalcDpv1o Dijerr2',Dij_err2(1:rmax)
    write(*,*) 'CalcDpv1o Dijacc2',Dij_err2(1:rmax)/abs(D(0,0,0,0))
 
#endif

    Derr2 = max(Derr2,Dij_err2(0:rmax))
    Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dpv1otest
    write(*,*) 'CalcDpv1o D(0,0,0,0) = ',D(0,0,0,0)
    if(rmax.ge.3)then
      write(*,*) 'CalcDpv1o D(0,1,1,1) = ',D(0,1,1,1)
    endif

    write(*,*) 'CalcDpv1o Derr',Derr
    write(*,*) 'CalcDpv1o Dacc',Derr/abs(D(0,0,0,0))
    write(*,*) 'CalcDpv1o Derr2',Derr2
    write(*,*) 'CalcDpv1o Dacc2',Derr2/abs(D(0,0,0,0))
#endif

!    if (id.eq.0) then
!    write(*,*) 'CalcDpv1o Derr ',Derr
!    write(*,*) 'CalcDpv1o Derr2',Derr2
!    end if
    
  end subroutine CalcDpv1o



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDpv(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDpv(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out)  :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3)
    double complex :: D0_coli, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    integer :: rmaxC,r,n0,n1,n2,n3,nn0,nn1,nn2,nn3,i,j
    integer :: bin,k,nid(0:3)

!    if (id.eq.0) write(*,*) 'CalcDpv in', rmax,id
        
    ! calculation of scalar coefficient
    D(0,0,0,0) = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
    Duv(0,0,0,0) = 0d0

    ! accuracy estimate for D0 function
    ! detX=0 implemented 16.08.2018
    if(adetX.ne.0d0) then
      Derr(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
    else
      Derr(0) = acc_def_D0* abs(D(0,0,0,0))
    end if
    Derr2(0) = Derr(0)

    if (rmax.eq.0) return

    ! allocation of C functions
    rmaxC = rmax-1
    ! rmaxC = max(rmax-1,0)
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmax))
    allocate(Dij_err(0:rmax))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmax))
    allocate(Dij_err2(0:rmax))
    allocate(Cij_err2(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do


    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0))
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1))
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2))
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3))

#ifdef Dpvtest
    write(*,*) 'CalcDpv Cerr_i=',Cerr_i(:,0)
    write(*,*) 'CalcDpv Cerr_i=',Cerr_i(:,1)
    write(*,*) 'CalcDpv Cerr_i=',Cerr_i(:,2)
    write(*,*) 'CalcDpv Cerr_i=',Cerr_i(:,3)
#endif

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do

    
    ! determine inverse Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)
 

!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)

!    Zinv(1,1) = (4d0*q30*q20-q2q3*q2q3)/detZ
!    Zinv(2,1) = (q1q3*q2q3-2d0*q30*q1q2)/detZ
!    Zinv(3,1) = (q1q2*q2q3-2d0*q20*q1q3)/detZ
!    Zinv(1,2) = Zinv(2,1)
!    Zinv(2,2) = (4d0*q10*q30-q1q3*q1q3)/detZ
!    Zinv(3,2) = (q1q2*q1q3-2d0*q10*q2q3)/detZ
!    Zinv(1,3) = Zinv(3,1)
!    Zinv(2,3) = Zinv(3,2)
!    Zinv(3,3) = (4d0*q10*q20-q1q2*q1q2)/detZ
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32

!  commented out 2.9.17
!   Zinv = Zadj/detZ

    ! calculate Duv
    call CalcDuv(Duv,Cuv_0,mm02,f,rmax,id)

    ! initialization of error propagation
!    Zadj=Zinv*detZ    

!    maxZadj = max(abs(Zadj(1,1)),abs(Zadj(2,1)),abs(Zadj(3,1)),  &
!                  abs(Zadj(2,2)),abs(Zadj(3,2)),abs(Zadj(3,3)))

!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)
!    maxZadjf = max(abs(Zadjf(1)),abs(Zadjf(2)),abs(Zadjf(3)))
!
!    aZadjff = abs(Zadjf(1)*f(1)+Zadjf(2)*f(2)+Zadjf(3)*f(3))

!    adetZ = abs(detZ)
!    adetX = abs(2d0*mm02*detZ-Zadjf(1)*f(1)-Zadjf(2)*f(2)-Zadjf(3)*f(3))

    Dij_err =0d0
    D00_err =0d0
    Dij_err(0) = Derr(0)
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))
    
    Dij_err2 =0d0
    D00_err2 =0d0
    Dij_err2(0) = Derr2(0)
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))
    
#ifdef Dpvtest
    write(*,*) 'CalcDpv Cij_err=',Cij_err
    write(*,*) 'CalcDpv Dij_err(0)=',Dij_err(0)
#endif

    allocate(D_alt(0:rmax,0:rmax,0:rmax,0:rmax))

    ! PV reduction
    do r=1,rmax

      if (mod(r,2).eq.0) then
        ! reduction formula (5.10) for D(r/2,0,0,0)
        n0 = r/2       
        D(n0,0,0,0) = (C_0(n0-1,0,0,0) + 2*mm02*D(n0-1,0,0,0) + 4*Duv(n0,0,0,0)  &
                       + f(1)*D(n0-1,1,0,0) + f(2)*D(n0-1,0,1,0)  &
                       + f(3)*D(n0-1,0,0,1)) / (2*(r-1)) 
      end if


      do n0=(r-1)/2,0,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2
         
            if (n1.ge.1) then
              nn1 = n1-1
              nn2 = n2
              nn3 = n3
              j = 1
            else if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

            do i=1,3
              Smod(i) = -C_0(n0,nn1,nn2,nn3)-f(i)*D(n0,nn1,nn2,nn3)
            end do
          
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D(n0,n1,n2,n3) = Zinv(1,j)*Smod(1) + Zinv(2,j)*Smod(2)  &
                           + Zinv(3,j)*Smod(3)

          end do
        end do
      end do

      ! determine error from symmetry for n0=0 and n1>1, n2>1 
      Derr(r)=Derr(r-1)
      Derr2(r)=Derr2(r-1)
      n0=0
      do n1=0,r-2*n0
        do n2=0,r-2*n0-n1
          n3 = r-2*n0-n1-n2
          if (n1.ge.1.and.n2+n3.ge.1) then
         
            if (n2.ge.1) then
              nn1 = n1
              nn2 = n2-1
              nn3 = n3
              j = 2
            else
              nn1 = n1
              nn2 = n2
              nn3 = n3-1
              j = 3
            end if

            do i=1,3
              Smod(i) = -C_0(n0,nn1,nn2,nn3)-f(i)*D(n0,nn1,nn2,nn3)
            end do
          
            if (nn1.ge.1) then
              Smod(1) = Smod(1) - 2*nn1*D(n0+1,nn1-1,nn2,nn3)
            else            
              Smod(1) = Smod(1) + C_i(n0,nn2,nn3,1)
            end if

            if (nn2.ge.1) then
              Smod(2) = Smod(2) - 2*nn2*D(n0+1,nn1,nn2-1,nn3)
            else
              Smod(2) = Smod(2) + C_i(n0,nn1,nn3,2)
            end if

            if (nn3.ge.1) then
              Smod(3) = Smod(3) - 2*nn3*D(n0+1,nn1,nn2,nn3-1)
            else
              Smod(3) = Smod(3) + C_i(n0,nn1,nn2,3)
            end if
          
            D_alt(n0,n1,n2,n3) = Zinv(1,j)*Smod(1) + Zinv(2,j)*Smod(2)  &
                           + Zinv(3,j)*Smod(3)
 
            Derr(r)=max(Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))
            Derr2(r)=max(Derr2(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)))

!            write(*,*) 'CalcDpv: errpr',r,Derr(r),abs(D(n0,n1,n2,n3)-D_alt(n0,n1,n2,n3)), &
!                        D(n0,n1,n2,n3),D_alt(n0,n1,n2,n3),n0,n1,n2,n3


          end if
        end do
      end do

      if(r.ge.2)then
        D00_err(r) = max(abs(m02)*Dij_err(r-2), Cerr_i(r-2,0),    &
              aZadjff/adetZ*Dij_err(r-2),             &
              maxZadjf/adetZ*max(D00_err(r-1),Cij_err(r-2)))
      else
        D00_err(r) = 0d0
      end if
      Dij_err(r) = max(maxZadjf*Dij_err(r-1),   &
              maxZadj*max(D00_err(r),Cij_err(r-1)))/adetZ

      if(r.ge.2)then
        D00_err2(r) = max(abs(m02)*Dij_err2(r-2), Cerr2_i(r-2,0),    &
              aZadjff/adetZ*Dij_err2(r-2),             &
              maxZadjf/adetZ*max(D00_err2(r-1),Cij_err2(r-2)))
      else
        D00_err2(r) = 0d0
      end if
      Dij_err2(r) = max(maxZadjf*Dij_err2(r-1),   &
              maxZadj*max(D00_err2(r),Cij_err2(r-1)))/sqrt(adetZ*maxZ*maxZadj)
    end do
    
      ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
!    do r=rmax+1,2*rmax
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 
          end do
        end do
      end do
    end do

#ifdef Dpvtest
    write(*,*) 'CalcDpv Derrsym',Derr
    write(*,*) 'CalcDpv Daccsym',Derr/abs(D(0,0,0,0))

!    write(*,*) 'Dij_err_jj',maxZadjf*Dij_err/adetZ
!    write(*,*) 'Dij_err_00',maxZadj*D00_err(1:rmax)/adetZ
!    write(*,*) 'Dij_err_cc',maxZadj*Cij_err/adetZ
 
    write(*,*) 'CalcDpv Dijerr',Dij_err(1:rmax)
    write(*,*) 'CalcDpv Dijacc',Dij_err(1:rmax)/abs(D(0,0,0,0))
#endif

    Derr2 = max(Derr2,Dij_err2(0:rmax))
    Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dpvtest
    write(*,*) 'CalcDpv Derr',Derr
    write(*,*) 'CalcDpv Dacc',Derr/abs(D(0,0,0,0))
#endif

    if (id.eq.0) then
    write(*,*) 'CalcDpv Derr ',Derr
    write(*,*) 'CalcDpv Derr2',Derr2
    end if
    
  end subroutine CalcDpv





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDpv2(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  subroutine CalcDpv2(D,Duv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Derr,Derr2)

    use globalD
  
    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out)  :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: D0_coli, elimminf2_coli
    double complex :: Daux(1:rmax/2+1,0:rmax-1,0:rmax-1,0:rmax-1), Smod(3)
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    integer :: rmaxC,r,n0,n1,n2,n3,k
    integer :: bin,nid(0:3)

#ifdef Dpv2test
    write(*,*) 'CalcDpv2 in'
#endif
!    write(*,*) 'CalcDpv2 in', rmax, id

    ! calculation of scalar coefficient
    D(0,0,0,0) = D0_coli(p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
    Duv(0,0,0,0) = 0d0

    ! accuracy estimate for D0 function
    Derr(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )
    Derr2(0) = acc_def_D0*max( abs(D(0,0,0,0)), 1d0/sqrt(adetX) )

    if (rmax.eq.0) return

    ! allocation of C functions
    rmaxC = rmax-1
    ! rmaxC = max(rmax-1,0)
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    
    ! allocate arrays for error propagation
    allocate(D00_err(0:rmax+1))
    allocate(Dij_err(0:rmax))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmax+1))
    allocate(Dij_err2(0:rmax))
    allocate(Cij_err2(0:rmaxC))
   

    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0))
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1))
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2))
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3))

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do

    
    ! determine inverse modified Cayley matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)

    ! calculate Duv
    call CalcDuv(Duv,Cuv_0,mm02,mx(1:3,0),rmax,id)    

    ! initialization of error propagation

!    adetX = abs(chdet(4,mx))
!    maxZadjf=maxval(abs(mxinv(0,1:3)))*adetX
!    maxXadj=maxval(abs(mxinv(1:3,1:3)))*adetX
!    adetZ=abs(mxinv(0,0))*adetX

!    write(*,*) 'CalcDpv adetX ',adetX,maxZadjf,maxXadj,adetZ

    Dij_err =0d0
    D00_err =0d0
    Dij_err(0) = Derr(0)
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Dij_err2 =0d0
    D00_err2 =0d0
    Dij_err2(0) = Derr2(0)
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

!   write(*,*) 'CalcDpv2 Cerr _i0=',Cerr_i(:,0)
!   write(*,*) 'CalcDpv2 Cerr2_i0=',Cerr2_i(:,0)
!   write(*,*) 'CalcDpv2 Cerr _i1=',Cerr_i(:,1)
!   write(*,*) 'CalcDpv2 Cerr2_i1=',Cerr2_i(:,1)
!   write(*,*) 'CalcDpv2 Cerr _i2=',Cerr_i(:,2)
!   write(*,*) 'CalcDpv2 Cerr2_i2=',Cerr2_i(:,2)
!   write(*,*) 'CalcDpv2 Cerr _i3=',Cerr_i(:,3)
!   write(*,*) 'CalcDpv2 Cerr2_i3=',Cerr2_i(:,3)
!   write(*,*) 'CalcDpv2 Cij_err=',Cij_err
!   write(*,*) 'CalcDpv2 Cij_err2=',Cij_err2
#ifdef Dpv2test
    write(*,*) 'CalcDpv2 Dij_err(0)=',Dij_err(0)
    write(*,*) 'CalcDpv2 Dij_acc(0)=',Dij_err(0)/D(0,0,0,0)
    write(*,*) 'CalcDpv2 Cij_err=',Cij_err
#endif
    
    allocate(D_alt(0:rmax,0:rmax,0:rmax,0:rmax))

    ! alternative PV-like reduction
    do r=1,rmax

      do n0=2,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            do k=1,3
              Smod(k) = -C_0(n0-1,n1,n2,n3)
            end do

            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2*n1*D(n0,n1-1,n2,n3)
            else
              Smod(1) = Smod(1) + C_i(n0-1,n2,n3,1)
            end if

            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2*n2*D(n0,n1,n2-1,n3)
            else
              Smod(2) = Smod(2) + C_i(n0-1,n1,n3,2)
            end if

            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2*n3*D(n0,n1,n2,n3-1)
            else
              Smod(3) = Smod(3) + C_i(n0-1,n1,n2,3)
            end if

            Daux(n0,n1,n2,n3) = (D(n0-1,n1,n2,n3) - mxinv(1,0)*Smod(1)  &
                              - mxinv(2,0)*Smod(2) - mxinv(3,0)*Smod(3))/mxinv(0,0)

          end do
        end do
      end do

      
      do n0=1,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (Daux(n0,n1,n2,n3) + 4d0*Duv(n0,n1,n2,n3)  &
                             + C_0(n0-1,n1,n2,n3))/(r-1)/2d0    

          end do
        end do
      end do

!      do n1=0,r-1
!        do n2=0,r-1-n1
!          n3 = r-1-n1-n2
!
!          do k=1,3
!            Smod(k) = -C_0(0,n1,n2,n3)
!          end do
!
!          if (n1.ge.1) then
!            Smod(1) = Smod(1) - 2*n1*D(1,n1-1,n2,n3)
!          else
!            Smod(1) = Smod(1) + C_i(0,n2,n3,1)
!          end if
!
!          if (n2.ge.1) then
!            Smod(2) = Smod(2) - 2*n2*D(1,n1,n2-1,n3)
!          else
!            Smod(2) = Smod(2) + C_i(0,n1,n3,2)
!          end if
!
!          if (n3.ge.1) then
!            Smod(3) = Smod(3) - 2*n3*D(1,n1,n2,n3-1)
!          else
!            Smod(3) = Smod(3) + C_i(0,n1,n2,3)
!          end if
!
!          Daux(1,n1,n2,n3) = (D(0,n1,n2,n3) - mxinv(1,0)*Smod(1)  &
!                             - mxinv(2,0)*Smod(2) - mxinv(3,0)*Smod(3))/mxinv(0,0)
!
!          D(0,n1+1,n2,n3) = mxinv(0,1)*Daux(1,n1,n2,n3)  &
!                          + mxinv(1,1)*Smod(1) + mxinv(2,1)*Smod(2) + mxinv(3,1)*Smod(3)
!          D(0,n1,n2+1,n3) = mxinv(0,2)*Daux(1,n1,n2,n3)  &
!                          + mxinv(1,2)*Smod(1) + mxinv(2,2)*Smod(2) + mxinv(3,2)*Smod(3)
!          D(0,n1,n2,n3+1) = mxinv(0,3)*Daux(1,n1,n2,n3)  &
!                          + mxinv(1,3)*Smod(1) + mxinv(2,3)*Smod(2) + mxinv(3,3)*Smod(3)
!
!        end do
!      end do 
      
      ! calculate D and determine error from symmetry for n0=0 and n1>0, n2>0, n3>0 
      Derr(r)=Derr(r-1)
      Derr2(r)=Derr2(r-1)

      do n1=0,r-1
        do n2=0,r-1-n1
          n3 = r-1-n1-n2

          do k=1,3
            Smod(k) = -C_0(0,n1,n2,n3)
          end do

          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2*n1*D(1,n1-1,n2,n3)
          else
            Smod(1) = Smod(1) + C_i(0,n2,n3,1)
          end if

          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2*n2*D(1,n1,n2-1,n3)
          else
            Smod(2) = Smod(2) + C_i(0,n1,n3,2)
          end if

          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2*n3*D(1,n1,n2,n3-1)
          else
            Smod(3) = Smod(3) + C_i(0,n1,n2,3)
          end if

          Daux(1,n1,n2,n3) = (D(0,n1,n2,n3) - mxinv(1,0)*Smod(1)  &
                             - mxinv(2,0)*Smod(2) - mxinv(3,0)*Smod(3))/mxinv(0,0)

          D(0,n1+1,n2,n3) = mxinv(0,1)*Daux(1,n1,n2,n3)  &
                          + mxinv(1,1)*Smod(1) + mxinv(2,1)*Smod(2) + mxinv(3,1)*Smod(3)
          D(0,n1,n2+1,n3) = mxinv(0,2)*Daux(1,n1,n2,n3)  &
                          + mxinv(1,2)*Smod(1) + mxinv(2,2)*Smod(2) + mxinv(3,2)*Smod(3)
          D_alt(0,n1,n2,n3+1) = mxinv(0,3)*Daux(1,n1,n2,n3)  &
                          + mxinv(1,3)*Smod(1) + mxinv(2,3)*Smod(2) + mxinv(3,3)*Smod(3)

          if(n3.eq.r-1) then
             D(0,0,0,r) = D_alt(0,0,0,r)          

! use error on detX for r=1 (enters r>1 implicitly)
! added 13.09.2022
             if (r==1) then
               Derr(r) = max( Derr(r), &
                   max(abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))*dprec_cll/detXerr)
               Derr2(r) = max( Derr2(r), &
                   max(abs(D(0,1,0,0)),abs(D(0,0,1,0)),abs(D(0,0,0,1)))*dprec_cll/detXerr)
             endif

          else
!             write(*,*) 'errsym=',abs(D(0,n1,n2,n3+1)-D_alt(0,n1,n2,n3+1)),  &
!                     D(0,n1,n2,n3+1),D_alt(0,n1,n2,n3+1)

             Derr(r)=max(Derr(r),abs(D(0,n1,n2,n3+1)-D_alt(0,n1,n2,n3+1)))
             Derr2(r)=max(Derr2(r),abs(D(0,n1,n2,n3+1)-D_alt(0,n1,n2,n3+1)))
          end if

!          write(*,*) 'Da(0,n1,n2,n3)=',n1+1,n2,n3,D(0,n1+1,n2,n3)
!          write(*,*) 'Da(0,n1,n2,n3)=',n1,n2+1,n3,D(0,n1,n2+1,n3)
!          write(*,*) 'Db(0,n1,n2,n3)=',n1,n2,n3+1,D_alt(0,n1,n2,n3+1)

        end do
      end do 

      D00_err(r+1) = max(Cerr_i(r-1,0),adetX/adetZ*Dij_err(r-1),      &
                             maxZadjf/adetZ*max(Cij_err(r-1),2*D00_err(r)))/(2*r)
      Dij_err(r) = max(maxZadjf*max(2*r*D00_err(r+1),Cerr_i(r-1,0)),   &
              maxXadj*max(2*D00_err(r),Cij_err(r-1)))/adetX
      
      D00_err2(r+1) = max(Cerr2_i(r-1,0),adetX/adetZ*Dij_err2(r-1),      &
                             maxZadjf/adetZ*max(Cij_err2(r-1),2*D00_err2(r)))/(2*r)
      Dij_err2(r) = max(maxZadjf*max(2*r*D00_err2(r+1),Cerr2_i(r-1,0)),   &
              maxXadj*max(2*D00_err2(r),Cij_err2(r-1)))/adetX*sqrt(adetZ/(maxZ*maxZadj))
      
#ifdef Dpv2test
      write(*,*) 'CalcDpv2 Cerr_i  ',r-1, Cerr_i(r-1,0)
      write(*,*) 'CalcDpv2 Cij_err ',r-1, Cij_err(r-1)
      write(*,*) 'CalcDpv2 D00_err ',r+1, D00_err(r+1)
      write(*,*) 'CalcDpv2 Dij_err ',r, Dij_err(r)
      write(*,*) 'CalcDpv2 Cerr2_i ',r-1, Cerr2_i(r-1,0)
      write(*,*) 'CalcDpv2 Cij_err2',r-1, Cij_err2(r-1)
      write(*,*) 'CalcDpv2 D00_err2',r+1, D00_err2(r+1)
      write(*,*) 'CalcDpv2 Dij_err2',r, Dij_err2(r)
#endif

    end do

      ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
!    do r=rmax+1,2*rmax
    do r=rmax+1,rmax+1

#ifdef Dpv2test
! pv1 version gets unstable for some cases!
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

!            write(*,*) 'D1(n0+1)',n0,n1,n2,n3
!            write(*,*) 'D1(n0+1)',(C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
!                + 4*Duv(n0,n1,n2,n3)  &
!                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
!                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1))

          end do
        end do
      end do
#endif

! pv2 formulas added 24.01.2016
      do n0=max(2,r-rmax),r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            do k=1,3
              Smod(k) = -C_0(n0-1,n1,n2,n3)
            end do

            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2*n1*D(n0,n1-1,n2,n3)
            else
              Smod(1) = Smod(1) + C_i(n0-1,n2,n3,1)
            end if

            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2*n2*D(n0,n1,n2-1,n3)
            else
              Smod(2) = Smod(2) + C_i(n0-1,n1,n3,2)
            end if

            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2*n3*D(n0,n1,n2,n3-1)
            else
              Smod(3) = Smod(3) + C_i(n0-1,n1,n2,3)
            end if

            Daux(n0,n1,n2,n3) = (D(n0-1,n1,n2,n3) - mxinv(1,0)*Smod(1)  &
                              - mxinv(2,0)*Smod(2) - mxinv(3,0)*Smod(3))/mxinv(0,0)

          end do
        end do
      end do

      
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (Daux(n0,n1,n2,n3) + 4d0*Duv(n0,n1,n2,n3)  &
                             + C_0(n0-1,n1,n2,n3))/(r-1)/2d0    

#ifdef Dpv2test
!            write(*,*) 'D2(n0+1)',n0,n1,n2,n3
!            write(*,*) 'D2(n0+1)',(Daux(n0,n1,n2,n3) + 4d0*Duv(n0,n1,n2,n3)  &
!                             + C_0(n0-1,n1,n2,n3))/(r-1)/2d0
#endif
          end do
        end do
      end do

    end do

#ifdef Dpv2test
    write(*,*) 'CalcDpv2 Derrsym',Derr
    write(*,*) 'CalcDpv2 Daccsym',Derr/abs(D(0,0,0,0))
    write(*,*) 'CalcDpv2 Derr2sym',Derr2
    write(*,*) 'CalcDpv2 Dacc2sym',Derr2/abs(D(0,0,0,0))

    write(*,*) 'CalcDpv2 Dijerr',Dij_err
    write(*,*) 'CalcDpv2 Dijacc',Dij_err/abs(D(0,0,0,0))
#endif
    
    Derr2 = max(Derr2,Dij_err2(0:rmax))
    Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dpv2test
!    write(*,*) 'CalcDpv2 D(0,0,0,0) = ',D(0,0,0,0)
!    if(rmax.ge.2)then
!      write(*,*) 'CalcDpv2 D(1,0,1,0) = ',D(1,0,1,0)
!    endif
!    if(rmax.ge.3)then
!      write(*,*) 'CalcDpv2 D(0,1,1,1) = ',D(0,1,1,1)
!    endif

    write(*,*) 'CalcDpv2 Derr ',Derr
    write(*,*) 'CalcDpv2 Dacc ',Derr/abs(D(0,0,0,0))
    write(*,*) 'CalcDpv2 Derr2',Derr2
    write(*,*) 'CalcDpv2 Dacc2',Derr2/abs(D(0,0,0,0))
#endif

!    write(*,*) 'CalcDpv2 Derr ',Derr
!    write(*,*) 'CalcDpv2 Derr2',Derr2
    
  end subroutine CalcDpv2





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDg(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordg_min,ordg_max,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDg(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordg_min,ordg_max,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,ordg_min,ordg_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: Zadjfj,Zadj2(4), Zadjkl, Xtilde
    double complex, allocatable :: Dexpg(:,:,:,:,:), DuvExpg(:,:,:,:)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3), Skl, DexpgAux
    double complex :: cC0f, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpg(0:1,0:rmax+ordg_min+1,0:ordg_max),truncfacexp
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,k,l,i,j,m,n,g,rg
    integer :: inds0(3), inds(3), inds2(2,4)
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgtest
    write(*,*) 'CalcDg in, ord',rmax,ordg_min,ordg_max
#endif

    ! allocation of C functions
    rmaxC = rmax + ordg_min
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax) = acc_req_CinD
    if (x_g.ne.0d0) then
      do r=rmax+1,rmaxC
        acc_req_Cextra(r)= acc_req_Cextra(r-1)/x_g
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+1:rmaxC) = acc_inf
    end if

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do


    ! calculate adjugated Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)

!    Z(1,1) = 2d0*q10
!    Z(2,1) = q10+q20-q21
!    Z(3,1) = q10+q30-q31
!    Z(1,2) = Z(2,1)
!    Z(2,2) = 2d0*q20
!    Z(3,2) = q20+q30-q32
!    Z(1,3) = Z(3,1)
!    Z(2,3) = Z(3,2)
!    Z(3,3) = 2d0*q30

!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)

!    Zadj(1,1) = (4d0*q30*q20-q2q3*q2q3)
!    Zadj(2,1) = (q1q3*q2q3-2d0*q30*q1q2)
!    Zadj(3,1) = (q1q2*q2q3-2d0*q20*q1q3)
!    Zadj(1,2) = Zadj(2,1)
!    Zadj(2,2) = (4d0*q10*q30-q1q3*q1q3)
!    Zadj(3,2) = (q1q2*q1q3-2d0*q10*q2q3)
!    Zadj(1,3) = Zadj(3,1)
!    Zadj(2,3) = Zadj(3,2)
!    Zadj(3,3) = (4d0*q10*q20-q1q2*q1q2)
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32
      
!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)


    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)
            end if

            if(n3.eq.0) then
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)
            end if

          end do
        end do
      end do
    end do


    ! choose reduction formulas with biggest denominators
    if (abs(Zadjf(1)).ge.max(abs(Zadjf(2)),abs(Zadjf(3)))) then
      j = 1
    else if (abs(Zadjf(2)).ge.max(abs(Zadjf(1)),abs(Zadjf(3)))) then
      j = 2
    else
      j = 3
    end if

    maxZadj = 0d0
    if (abs(Zadj(1,1)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,1))
      k = 1
      l = 1
      inds2 = reshape((/2,2,2,3,3,2,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,2)
      Zadj2(3) = Z(2,3)
      Zadj2(4) = -Z(2,2)
    end if
    if (abs(Zadj(2,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(2,2))
      k = 2
      l = 2
      inds2 = reshape((/1,1,1,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(1,3)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Zadj(3,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(3,3))
      k = 3
      l = 3
      inds2 = reshape((/1,1,1,2,2,1,2,2/),shape(inds2))
      Zadj2(1) = -Z(2,2)
      Zadj2(2) = Z(2,1)
      Zadj2(3) = Z(1,2)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Zadj(1,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,2))
      k = 1
      l = 2
      inds2 = reshape((/2,1,2,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = Z(3,3)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(2,3)
      Zadj2(4) = Z(2,1)
    end if
    if (abs(Zadj(1,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,3))
      k = 1
      l = 3
      inds2 = reshape((/2,1,2,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = -Z(3,2)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(2,2)
      Zadj2(4) = -Z(2,1)
    end if
    if (abs(Zadj(2,3)).gt.maxZadj) then
      k = 2
      l = 3
      inds2 = reshape((/1,1,1,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = Z(3,2)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(1,2)
      Zadj2(4) = Z(1,1)
    end if

    Zadjfj = Zadjf(j)
    Zadjkl = Zadj(k,l)
    Xtilde = Xadj(k,l)

!    write(*,*) 'CalcDg Xtilde n',Xtilde,Xadj(1,1),Xadj(1,2),Xadj(2,2)


    ! allocation of array for det(Z)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpg(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:rmaxExp,0:ordg_max))


    ! calculate Duv
    allocate(DuvExpg(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpg,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpg(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))
     
    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))

    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

#ifdef Dgtest
    write(*,*) 'CalcDg Cij_err = ',Cij_err 
    write(*,*) 'CalcDg C0_err = ', Cerr_i(0,0),Cerr_i(0,1),Cerr_i(0,2),Cerr_i(0,3)
    write(*,*) 'CalcDg C0 = ', C_0(0,0,0,0),C_i(0,0,0,1),C_i(0,0,0,2),C_i(0,0,0,3)
#endif

!    maxZadj = maxval(abs(Zadj))
!    maxZadj2f = maxval(abs(f(inds2(1,:))*Zadj2(:)))

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
    truncfacexp = sqrt(fac_g) * truncfacD
    gtrunc = ordg_max 

! calculate D(n0,n1,n2,n3) up to rank r for n0>0 and up to rank r-1 for n0=0
    rloop: do r=1,rmaxExp

#ifdef Dgtest
      write(*,*) 'CalcDg rloop',r,rmax,gtrunc,rmaxExp
#endif

      if (r.gt.rmax+gtrunc+1) exit rloop

#ifdef Dgtest
      write(*,*) 'CalcDg rloop',r
#endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating
      ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
      ! exploiting eq. (5.40)
      maxDexpg(1,r,0)=0d0
      do n0=r/2,1,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3=r-2*n0-n1-n2

            inds0(1) = n1
            inds0(2) = n2
            inds0(3) = n3

            DexpgAux = 2d0*Zadj(k,l)*C_0(n0-1,n1,n2,n3)  & 
                     + Xtilde*Dexpg(n0-1,n1,n2,n3,0)  &
                     + 4d0*Zadj(k,l)*DuvExpg(n0,n1,n2,n3)

            inds = inds0
            inds(k) = inds(k)+1
            do i=1,3
              DexpgAux = DexpgAux + Zadj(i,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
            end do

            do i=1,3
              inds = inds0
              inds(i) = inds(i)+1
              DexpgAux = DexpgAux - Zadj(k,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
            end do

            do i=1,4
              n = inds2(1,i)
              m = inds2(2,i)

              Skl = f(n)*Shat(n0-1,inds0(1),inds0(2),inds0(3),m)

              inds = inds0
              if (inds(m).ge.1) then
                inds(m) = inds(m)-1
                Skl = Skl - 2d0*f(n)*inds0(m)*Dexpg(n0,inds(1),inds(2),inds(3),0)
                if (inds(n).ge.1) then
                  inds(n) = inds(n)-1
                  Skl = Skl - 4d0*inds0(m)*(inds(n)+1)*Dexpg(n0+1,inds(1),inds(2),inds(3),0)
                end if
              end if
              inds = inds0
              if (inds(n).ge.1) then
                inds(n) = inds(n)-1
                Skl = Skl + 2d0*inds0(n)*Shat(n0,inds(1),inds(2),inds(3),m)  &
                          - 2d0*f(m)*inds0(n)*Dexpg(n0,inds(1),inds(2),inds(3),0)
              end if

              DexpgAux = DexpgAux - Zadj2(i)*Skl

            end do

            Dexpg(n0,n1,n2,n3,0) = DexpgAux/(2d0*Zadjkl)/(2d0*(r-n0))

            if (n0.eq.1) then
              maxDexpg(1,r,0) =  maxDexpg(1,r,0) + abs(Dexpg(n0,n1,n2,n3,0) )
            end if

            if (r-n0.le.rmax) then
              D(n0,n1,n2,n3) = Dexpg(n0,n1,n2,n3,0)
            end if

          end do
        end do
      end do

      ! calculate
      ! D_00ijkl.. --> D_aijkl..
      ! exploiting eq. (5.38)
      maxDexpg(0,r-1,0)=0d0
      do n1=0,r-1
        do n2=0,r-1-n1
          n3 = r-1-n1-n2

          Smod = Shat(0,n1,n2,n3,:)
          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2d0*n1*Dexpg(1,n1-1,n2,n3,0)
          end if
          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2d0*n2*Dexpg(1,n1,n2-1,n3,0)
          end if
          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2d0*n3*Dexpg(1,n1,n2,n3-1,0)
          end if

          Dexpg(0,n1,n2,n3,0) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                              +  Zadj(3,j)*Smod(3))/Zadjfj
          maxDexpg(0,r-1,0) =  maxDexpg(0,r-1,0) + abs(Dexpg(0,n1,n2,n3,0))
          if (r.le.rmax+1) then
            D(0,n1,n2,n3) = Dexpg(0,n1,n2,n3,0)
          end if

#ifdef Dgtest
          if(n0.eq.0.and.n1.eq.0.and.n2.eq.0.and.n3.eq.0) then 
            write(*,*) 'D2(0,0,0,0)= ',0,D(n0,n1,n2,n3),detZ/Zadjfj
!            write(*,*) 'D2(0,0,0,0)= ',Smod
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,j),Zadjfj
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,j)/Zadjfj
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,j)/Zadjfj, &
!               Smod(2)*Zadj(2,j)/Zadjfj, Smod(3)*Zadj(3,j)/Zadjfj
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,j)/Zadjfj+ &
!               Smod(2)*Zadj(2,j)/Zadjfj+ Smod(3)*Zadj(3,j)/Zadjfj
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,1),Zadjf(1)
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,1)/Zadjf(1)
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,1)/Zadjf(1), &
!               Smod(2)*Zadj(2,1)/Zadjfj, Smod(3)*Zadj(3,1)/Zadjf(1)
!             write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,1)/Zadjf(1)+ &
!               Smod(2)*Zadj(2,1)/Zadjfj+ Smod(3)*Zadj(3,1)/Zadjf(1)
!             write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,2),Zadjf(2)
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,2)/Zadjf(2)
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,2)/Zadjf(2), &
!               Smod(2)*Zadj(2,2)/Zadjfj, Smod(3)*Zadj(3,2)/Zadjf(2)
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,2)/Zadjf(2)+ &
!               Smod(2)*Zadj(2,2)/Zadjfj+ Smod(3)*Zadj(3,2)/Zadjf(2)
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,3),Zadjf(3)
!            write(*,*) 'D2(0,0,0,0)= ',Zadj(1:3,3)/Zadjf(3)
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,3)/Zadjf(3), &
!               Smod(2)*Zadj(2,3)/Zadjfj, Smod(3)*Zadj(3,3)/Zadjf(3)
!            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadj(1,3)/Zadjf(3)+ &
!               Smod(2)*Zadj(2,3)/Zadjfj+ Smod(3)*Zadj(3,3)/Zadjf(3)
          end if
#endif

        end do
      end do

#ifdef Dgtest
      write(*,*) 'CalcDg maxDexpg 0',r-1, maxDexpg(0,r-1,0)
#endif

      if(r.le.rmax+1) then
!       Derr(r-1) =  abs(detZ/Zadjfj)*maxDexpg(0,r-1,0)
        Derr(r-1) =  fac_g*maxDexpg(0,r-1,0)
      endif


      ! error propagation from C's
      if(r.gt.1)then
        D00_err(r) = max(Cij_err(r-1),Cij_err(r-2),        &
            max(maxZadj*Cij_err(r-1),maxZadj2f*Cij_err(r-2))/abs(Zadjkl)) &
            /(4*(r-1))
      end if
      Dij_err(r-1)=maxZadj*max(Cij_err(r-1),2*D00_err(r))/abs(Zadjfj)

      if(r.gt.1)then
        D00_err2(r) = max(Cij_err2(r-1),Cij_err2(r-2),        &
            max(maxZadj*Cij_err2(r-1),maxZadj2f*Cij_err2(r-2))/abs(Zadjkl)) &
            /(4*(r-1))

      end if
      Dij_err2(r-1)=maxZadj*max(Cij_err2(r-1),2*D00_err2(r))/abs(Zadjfj)


      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rg = r
      gloop: do g=1,min(gtrunc,r-1)
        rg = rg-1

#ifdef  Dgtest
        write(*,*) 'gloop ',g,rg
#endif

        ! calculating
        ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
        ! exploiting eq. (5.40)
        maxDexpg(1,rg,g) = 0d0
        do n0=rg/2,1,-1
          do n1=0,rg-2*n0
            do n2=0,rg-2*n0-n1
              n3=rg-2*n0-n1-n2

              inds0(1) = n1
              inds0(2) = n2
              inds0(3) = n3

              inds = inds0
              inds(k) = inds(k)+1
              inds(l) = inds(l)+1
              DexpgAux = Xtilde*Dexpg(n0-1,n1,n2,n3,g)  &
                       - detZ*Dexpg(n0-1,inds(1),inds(2),inds(3),g-1)


              do i=1,4
                n = inds2(1,i)
                m = inds2(2,i)

                Skl = 0d0

                inds = inds0
                if (inds(m).ge.1) then
                  inds(m) = inds(m)-1
                  Skl = Skl - 2d0*f(n)*inds0(m)*Dexpg(n0,inds(1),inds(2),inds(3),g)
                  if (inds(n).ge.1) then
                    inds(n) = inds(n)-1
                    Skl = Skl - 4d0*inds0(m)*(inds(n)+1)*Dexpg(n0+1,inds(1),inds(2),inds(3),g)
                  end if
                end if
                inds = inds0
                if (inds(n).ge.1) then
                  inds(n) = inds(n)-1
                  Skl = Skl - 2d0*f(m)*inds0(n)*Dexpg(n0,inds(1),inds(2),inds(3),g)
                end if

                DexpgAux = DexpgAux - Zadj2(i)*Skl

              end do

              Dexpg(n0,n1,n2,n3,g) = DexpgAux/(2d0*Zadjkl)/(2d0*(rg-n0))

               
              if(n0.eq.1) then
                maxDexpg(1,rg,g) =  maxDexpg(1,rg,g) + abs(Dexpg(n0,n1,n2,n3,g))

                if (g.eq.1.and.abs(Dexpg(1,n1,n2,n3,g)).gt.            &
                    truncfacexp*max(1/m2scale,maxDexpg(1,rg,g-1)) .or. &
                    g.ge.2.and.abs(Dexpg(1,n1,n2,n3,g)).gt.            &
                    truncfacexp*maxDexpg(1,rg,g-1)) then

#ifdef Dgtest
                  write(*,*) 'CalcDg exit gloop',n0,n1,n2,n3,g,abs(Dexpg(n0,n1,n2,n3,g)),maxDexpg(1,rg,g-1),truncfacexp
#endif      

                  gtrunc = g-1
                  exit gloop
                end if
              end if

            end do
          end do
        end do

#ifndef PPEXP00
        do n0=rg/2,1,-1
          if (rg-n0.le.rmax) then
            do n1=0,rg-2*n0
              do n2=0,rg-2*n0-n1
                n3=rg-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpg(n0,n1,n2,n3,g)
              end do
            end do
          end if
        end do
#endif
!        write(*,*) 'CalcDg after it1 ',rg

        ! calculate
        ! D_00ijkl.. --> D_aijkl..
        ! exploiting eq. (5.38)

!        write(*,*) 'CalcDg maxDexp',rg-1,g-1,maxDexpg(0,rg-1,g-1)

        maxDexpg(0,rg-1,g) = 0d0
        do n1=0,rg-1
          do n2=0,rg-1-n1
            n3 = rg-1-n1-n2

            Smod = 0d0
            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2d0*n1*Dexpg(1,n1-1,n2,n3,g)
            end if
            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2d0*n2*Dexpg(1,n1,n2-1,n3,g)
            end if
            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2d0*n3*Dexpg(1,n1,n2,n3-1,g)
            end if

            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
            inds(j) = inds(j)+1
            Dexpg(0,n1,n2,n3,g) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                                +  Zadj(3,j)*Smod(3)  &
                                - detZ*Dexpg(0,inds(1),inds(2),inds(3),g-1))/Zadjfj

            maxDexpg(0,rg-1,g) =  maxDexpg(0,rg-1,g) + abs(Dexpg(0,n1,n2,n3,g))

!              if(n1.eq.0.and.n2.eq.1.and.n3.eq.2) then 
!                write(*,*) 'D2(2,3,3)= ',g,Dexpg(0,n1,n2,n3,g)
!                write(*,*) 'D2(2,3,3)= ',Zadj(1,j)*Smod(1)/Zadjfj,  Zadj(2,j)*Smod(2)/Zadjfj,  &
!                                +  Zadj(3,j)*Smod(3)/Zadjfj,  &
!                                - detZ*Dexpg(0,inds(1),inds(2),inds(3),g-1)/Zadjfj
!                write(*,*) 'D2(2,3,3)= ',inds(1),inds(2),inds(3),         &
!                                - detZ/Zadjfj,Dexpg(0,inds(1),inds(2),inds(3),g-1)
!              end if

            if (g.eq.1.and.abs(Dexpg(0,n1,n2,n3,g)).gt.                  &
                truncfacexp*max(1/m2scale**2,maxDexpg(0,rg-1,g-1)) .or.  &
                g.ge.2.and.abs(Dexpg(0,n1,n2,n3,g)).gt.                  &
                truncfacexp*maxDexpg(0,rg-1,g-1)) then

#ifdef Dgtest
                  write(*,*) 'CalcDg exit gloop',0,n1,n2,n3,g,abs(Dexpg(0,n1,n2,n3,g)),maxDexpg(0,rg-1,g-1),truncfacexp
#endif      
              gtrunc = g-1
              exit gloop
            end if

          end do
        end do

        ! error propagation from C's
        if(rg.gt.1)then
!          D00_err(rg) = max( D00_err(rg),                                &
!              max( abs(m02)*Dij_err(rg-2),                               &
!              max( abs(detZ)*Dij_err(rg),abs(Xtilde)*Dij_err(rg-2),      &
!              maxZadj2f*D00_err(rg-1) ) / abs(Zadjkl) )                  &
!              /(4*(rg-1))   )
! 06.05.15 ->
          D00_err(rg) = max( D00_err(rg),                                &
              max( abs(detZ)*Dij_err(rg),abs(Xtilde)*Dij_err(rg-2),      &
              maxZadj2f*D00_err(rg-1) ) / abs(Zadjkl)                   &
              /(4*(rg-1))   )
        end if
        Dij_err(rg-1)=max(Dij_err(rg-1),                &
            max(2*maxZadj*D00_err(rg),abs(detZ)*Dij_err(rg))/abs(Zadjfj) )

        if(rg.gt.1)then
          D00_err2(rg) = max( D00_err2(rg),                                &
              max( abs(detZ)*Dij_err2(rg),abs(Xtilde)*Dij_err2(rg-2),      &
              maxZadj2f*D00_err2(rg-1) ) / abs(Zadjkl)                   &
              /(4*(rg-1))   )
        end if
        Dij_err2(rg-1)=max(Dij_err2(rg-1),                &
            max(2*maxZadj*D00_err2(rg),abs(detZ)*Dij_err2(rg))/abs(Zadjfj) )

#ifdef PPEXP00
        do n0=rg/2,1,-1
          if (rg-n0.le.rmax) then
            do n1=0,rg-2*n0
              do n2=0,rg-2*n0-n1
                n3=rg-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpg(n0,n1,n2,n3,g)
              end do
            end do
          end if
        end do
#endif
!        write(*,*) 'CalcDg after it1 ',rg
        if ((rg.le.rmax+1)) then
          Derr(rg-1) = 0d0
          do n1=0,rg-1
            do n2=0,rg-1-n1
              n3 = rg-1-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpg(0,n1,n2,n3,g)
!              Derr(rg-1)=max(Derr(rg-1),abs(Dexpg(0,n1,n2,n3,g))**2/abs(Dexpg(0,n1,n2,n3,g-1)))
              if(abs(Dexpg(0,n1,n2,n3,g-1)).ne.0d0) then
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpg(0,n1,n2,n3,g))*min(1d0,abs(Dexpg(0,n1,n2,n3,g))/abs(Dexpg(0,n1,n2,n3,g-1))))
              else
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpg(0,n1,n2,n3,g)))
              endif

#ifdef Dgtest
!              write(*,*) 'CalcDg Derr calc',rg-1,Derr(rg-1),n1,n2,n3,abs(Dexpg(0,n1,n2,n3,g)),abs(Dexpg(0,n1,n2,n3,g-1))
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
          ! 3.5.2019: needed?   factor 3 -> 10
#ifdef PVEST2
          if(Dij_err2(rg-1).gt.10d0*Derr(rg-1)) then
#else
          if(Dij_err(rg-1).gt.10d0*Derr(rg-1)) then
#endif
            gtrunc = min(g,gtrunc)
             
#ifdef Dgtest
             write(*,*) 'CalcDg exit err',r,rg-1,g,gtrunc,Dij_err(rg-1),Dij_err2(rg-1),Derr(rg-1)
#endif

          end if

        end if

      end do gloop

#ifdef Dgtest
      write(*,*) 'CalcDg D(0,0,0,0) = ',r,D(0,0,0,0)
    if(r.gt.1)then
!      write(*,*) 'CalcDg D(0,1,0,0) = ',r,D(0,1,0,0)
      write(*,*) 'CalcDg D(0,0,1,0) = ',r,D(0,0,1,0)
    endif
    if(r.gt.2.and.rmax.ge.2)then
      write(*,*) 'CalcDg D(1,0,0,0) = ',r,D(1,0,0,0)
      write(*,*) 'CalcDg D(0,2,0,0) = ',r,D(0,2,0,0)
      write(*,*) 'CalcDg D(0,0,1,1) = ',r,D(0,0,1,1)
!      write(*,*) 'CalcDg D(0,1,1,0) = ',r,D(0,1,1,0)
      write(*,*) 'CalcDg D(0,0,2,0) = ',r,D(0,0,2,0)
    endif
    if(r.gt.3.and.rmax.ge.3)then
      write(*,*) 'CalcDg D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDg D(1,1,0,0) = ',r,D(1,1,0,0)
      write(*,*) 'CalcDg D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDg D(1,0,0,1) = ',r,D(1,0,0,1)
!      write(*,*) 'CalcDg D(1,2,0,0) = ',r,D(1,2,0,0)
      write(*,*) 'CalcDg D(0,3,0,0) = ',r,D(0,3,0,0)
      write(*,*) 'CalcDg D(0,2,1,0) = ',r,D(0,2,1,0)
      write(*,*) 'CalcDg D(0,2,0,1) = ',r,D(0,2,0,1)
      write(*,*) 'CalcDg D(0,0,3,0) = ',r,D(0,0,3,0)
      write(*,*) 'CalcDg D(0,1,1,1) = ',r,D(0,1,1,1)
      write(*,*) 'CalcDg D(0,0,2,1) = ',r,D(0,0,2,1)
    endif
      write(*,*) 'CalcDg Dij_err',r,Dij_err
      write(*,*) 'CalcDg Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDg err',r,Derr
      write(*,*) 'CalcDg acc',r,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dgtest
      write(*,*) 'CalcDg exit r',r,maxval(Derr),maxval(Derr2),acc_req_D*abs(D(0,0,0,0))
#endif

!     if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit   ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then

        if (r.lt.rmax) then
          do rg=r+1,rmax
!          write(*,*) 'CalcDg exit rloop  =',rg,r,rmax
            do n0=0,rg/2
              do n1=0,rg-2*n0
                do n2=0,rg-2*n0-n1
                  D(n0,n1,n2,rg-2*n0-n1-n2)=0d0
                end do
              end do
            end do
          end do
          if(r.le.rmax) then
            do n1=0,r
              do n2=0,r-n1
                D(0,n1,n2,r-n1-n2)=0d0
              end do
            end do
          end if

100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDg',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDg:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDg: p10 = ',p10
            write(nerrout_coli,111)' CalcDg: p21 = ',p21
            write(nerrout_coli,111)' CalcDg: p32 = ',p32
            write(nerrout_coli,111)' CalcDg: p30 = ',p30
            write(nerrout_coli,111)' CalcDg: p20 = ',p20
            write(nerrout_coli,111)' CalcDg: p31 = ',p31
            write(nerrout_coli,111)' CalcDg: m02 = ',m02
            write(nerrout_coli,111)' CalcDg: m12 = ',m12
            write(nerrout_coli,111)' CalcDg: m22 = ',m22
            write(nerrout_coli,111)' CalcDg: m32 = ',m32
          end if
        end if

#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif
        exit rloop
      end if

    end do rloop


    ! reduction formula (5.10) for n0+n1+n2+N3=r, n0>=1 only!!!!!! 
    ! already calculated for rmax+1
!    do r=rmax+1,2*rmax 
#ifdef notneeded
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            write(*,*) 'CalcDg exp rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

            write(*,*) 'CalcDg dir rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

          end do
        end do
      end do
    end do
#endif

#ifdef Dgtest
    write(*,*) 'CalcDg D(0,0,0,0) = ',D(0,0,0,0)
    if(rmax.ge.3)then
      write(*,*) 'CalcDg D(1,0,1,0) = ',D(1,0,1,0)
    endif

    write(*,*) 'CalcDg final err ',Derr
    write(*,*) 'CalcDg final acc ',Derr/abs(D(0,0,0,0))
    write(*,*) 'CalcDg final err2',Derr2
    write(*,*) 'CalcDg final acc2',Derr2/abs(D(0,0,0,0))
#endif      
       
!    write(*,*) 'CalcDg Derr ',Derr
!    write(*,*) 'CalcDg Derr2',Derr2
           
  end subroutine CalcDg


#ifdef USEGM
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgm(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordgm_min,ordgm_max,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgm(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgm_min,ordgm_max,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,ordgm_min,ordgm_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax) 
    double complex :: Zadjfj,Zadj2(4), Zadjkl, Xtilde
    double complex, allocatable :: Dexpgm(:,:,:,:,:), DuvExpgm(:,:,:,:)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:), Cerr2_i(:,:)
    double complex :: Smod(3), Skl, DexpgmAux
    double complex :: cC0f, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpgm(0:1,0:rmax+ordgm_min+1,0:ordgm_max),truncfacexp
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,k,l,i,j,m,n,gm,rgm
    integer :: inds0(3), inds(3), inds2(2,4)
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgmtest
    write(*,*) 'CalcDgm in, ord',rmax,ordgm_min,ordgm_max
#endif

    ! allocation of C functions
    rmaxC = rmax + ordgm_min
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax) = acc_req_CinD
    if (x_gm.ne.0d0) then
      do r=rmax+1,rmaxC
        acc_req_Cextra(r)= acc_req_Cextra(r-1)/x_gm
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+1,rmaxC) = acc_inf
    end if

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do

    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)
            end if

            if(n3.eq.0) then
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)
            end if

          end do
        end do
      end do
    end do


    ! choose reduction formulas with biggest denominators
!    if (abs(Zadjf(1)).ge.max(abs(Zadjf(2)),abs(Zadjf(3)))) then
!      j = 1
!    else if (abs(Zadjf(2)).ge.max(abs(Zadjf(1)),abs(Zadjf(3)))) then
!      j = 2
!    else
!      j = 3
!    end if

    Zadjff = Zadjf(1)*f(1)+  Zadjf(2)*f(2)+ Zadjf(3)*f(3) 

    maxZadj = 0d0
    if (abs(Zadj(1,1)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,1))
      k = 1
      l = 1
      inds2 = reshape((/2,2,2,3,3,2,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,2)
      Zadj2(3) = Z(2,3)
      Zadj2(4) = -Z(2,2)
    end if
    if (abs(Zadj(2,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(2,2))
      k = 2
      l = 2
      inds2 = reshape((/1,1,1,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(1,3)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Zadj(3,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(3,3))
      k = 3
      l = 3
      inds2 = reshape((/1,1,1,2,2,1,2,2/),shape(inds2))
      Zadj2(1) = -Z(2,2)
      Zadj2(2) = Z(2,1)
      Zadj2(3) = Z(1,2)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Zadj(1,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,2))
      k = 1
      l = 2
      inds2 = reshape((/2,1,2,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = Z(3,3)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(2,3)
      Zadj2(4) = Z(2,1)
    end if
    if (abs(Zadj(1,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,3))
      k = 1
      l = 3
      inds2 = reshape((/2,1,2,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = -Z(3,2)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(2,2)
      Zadj2(4) = -Z(2,1)
    end if
    if (abs(Zadj(2,3)).gt.maxZadj) then
      k = 2
      l = 3
      inds2 = reshape((/1,1,1,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = Z(3,2)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(1,2)
      Zadj2(4) = Z(1,1)
    end if

!    Zadjfj = Zadjf(j)
    Zadjkl = Zadj(k,l)
    Xtilde = Xadj(k,l)

!    write(*,*) 'CalcDgm Xtilde n',Xtilde,Xadj(1,1),Xadj(1,2),Xadj(2,2)


    ! allocation of array for det(Z)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpgm(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:rmaxExp,0:ordgm_max))


    ! calculate Duv
    allocate(DuvExpgm(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpgm,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgm(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))
     
    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

#ifdef Dgmtest
    write(*,*) 'CalcDgm Cij_err = ',Cij_err 
    write(*,*) 'CalcDgm C0_err = ', Cerr_i(0,0),Cerr_i(0,1),Cerr_i(0,2),Cerr_i(0,3)
    write(*,*) 'CalcDgm C0 = ', C_0(0,0,0,0),C_i(0,0,0,1),C_i(0,0,0,2),C_i(0,0,0,3)
#endif

!    maxZadj = maxval(abs(Zadj))
!    maxZadj2f = maxval(abs(f(inds2(1,:))*Zadj2(:)))

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
    truncfacexp = sqrt(fac_gm) * truncfacD
    gtrunc = ordgm_max 

! calculate D(n0,n1,n2,n3) up to rank r for n0>0 and up to rank r-1 for n0=0
    rloop: do r=1,rmaxExp

#ifdef Dgmtest
!      write(*,*) 'CalcDgm rloop',r,rmax,gtrunc
#endif

      if (r.gt.rmax+gtrunc+1) exit rloop

#ifdef Dgmtest
      write(*,*) 'CalcDgm rloop',r
#endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating
      ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
      ! exploiting eq. (5.40)
      maxDexpgm(1,r,0)=0d0
      do n0=r/2,1,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3=r-2*n0-n1-n2

            inds0(1) = n1
            inds0(2) = n2
            inds0(3) = n3

            DexpgmAux = 2d0*Zadj(k,l)*C_0(n0-1,n1,n2,n3)  & 
                     + Xtilde*Dexpgm(n0-1,n1,n2,n3,0)  &
                     + 4d0*Zadj(k,l)*DuvExpgm(n0,n1,n2,n3)

            inds = inds0
            inds(k) = inds(k)+1
            do i=1,3
              DexpgmAux = DexpgmAux + Zadj(i,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
            end do

            do i=1,3
              inds = inds0
              inds(i) = inds(i)+1
              DexpgmAux = DexpgmAux - Zadj(k,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
            end do

            do i=1,4
              n = inds2(1,i)
              m = inds2(2,i)

              Skl = f(n)*Shat(n0-1,inds0(1),inds0(2),inds0(3),m)

              inds = inds0
              if (inds(m).ge.1) then
                inds(m) = inds(m)-1
                Skl = Skl - 2d0*f(n)*inds0(m)*Dexpgm(n0,inds(1),inds(2),inds(3),0)
                if (inds(n).ge.1) then
                  inds(n) = inds(n)-1
                  Skl = Skl - 4d0*inds0(m)*(inds(n)+1)*Dexpgm(n0+1,inds(1),inds(2),inds(3),0)
                end if
              end if
              inds = inds0
              if (inds(n).ge.1) then
                inds(n) = inds(n)-1
                Skl = Skl + 2d0*inds0(n)*Shat(n0,inds(1),inds(2),inds(3),m)  &
                          - 2d0*f(m)*inds0(n)*Dexpgm(n0,inds(1),inds(2),inds(3),0)
              end if

              DexpgmAux = DexpgmAux - Zadj2(i)*Skl

            end do

            Dexpgm(n0,n1,n2,n3,0) = DexpgmAux/(2d0*Zadjkl)/(2d0*(r-n0))

            if (n0.eq.1) then
              maxDexpgm(1,r,0) =  maxDexpgm(1,r,0) + abs(Dexpgm(n0,n1,n2,n3,0) )
            end if

            if (r-n0.le.rmax) then
              D(n0,n1,n2,n3) = Dexpgm(n0,n1,n2,n3,0)
            end if

          end do
        end do
      end do

      ! calculate
      ! D_00ijkl.. --> D_aijkl..
      ! exploiting eq. (5.38) contracted with f(j)
      maxDexpgm(0,r-1,0)=0d0
      do n1=0,r-1
        do n2=0,r-1-n1
          n3 = r-1-n1-n2

          Smod = Shat(0,n1,n2,n3,:)
          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2d0*n1*Dexpgm(1,n1-1,n2,n3,0)
          end if
          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2d0*n2*Dexpgm(1,n1,n2-1,n3,0)
          end if
          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2d0*n3*Dexpgm(1,n1,n2,n3-1,0)
          end if

          Dexpgm(0,n1,n2,n3,0) = (Zadjf(1)*Smod(1) +  Zadjf(2)*Smod(2)  &
                              +  Zadjf(3)*Smod(3))/Zadjff
          maxDexpgm(0,r-1,0) =  maxDexpgm(0,r-1,0) + abs(Dexpgm(0,n1,n2,n3,0))
          if (r.le.rmax+1) then
            D(0,n1,n2,n3) = Dexpgm(0,n1,n2,n3,0)
          end if


#ifdef Dgmtest
          if(n0.eq.0.and.n1.eq.0.and.n2.eq.0.and.n3.eq.0) then 
            write(*,*) 'D2(0,0,0,0)= ',0,D(n0,n1,n2,n3),detZ*fmax/Zadjff
            write(*,*) 'D2(0,0,0,0)= ',detZ,fmax,Zadjff
            write(*,*) 'D2(0,0,0,0)= ',Smod
            write(*,*) 'D2(0,0,0,0)= ',Zadjf(1:3),Zadjff
            write(*,*) 'D2(0,0,0,0)= ',Zadjf(1:3)/Zadjff
            write(*,*) 'D2(0,0,0,0)= ',Smod(1)*Zadjf(1)/Zadjff, &
               Smod(2)*Zadjf(2)/Zadjff, Smod(3)*Zadjf(3)/Zadjff
          end if
#endif

        end do
      end do

#ifdef Dgmtest
!      write(*,*) 'CalcDgm maxDexpgm 0',r-1, maxDexpgm(0,r-1,0)
#endif

      if(r.le.rmax+1) then
!       Derr(r-1) =  abs(detZ/Zadjfj)*maxDexpgm(0,r-1,0)
        Derr(r-1) =  fac_gm*maxDexpgm(0,r-1,0)
      endif

      ! error propagation from C's
      if(r.gt.1)then
        D00_err(r) = max(Cij_err(r-1),Cij_err(r-2),        &
            max(maxZadj*Cij_err(r-1),maxZadj2f*Cij_err(r-2))/abs(Zadjkl)) &
            /(4*(r-1))
      end if
      Dij_err(r-1)=maxZadjf*max(Cij_err(r-1),2*D00_err(r))/abs(Zadjff)

      if(r.gt.1)then
        D00_err2(r) = max(Cij_err2(r-1),Cij_err2(r-2),        &
            max(maxZadj*Cij_err2(r-1),maxZadj2f*Cij_err2(r-2))/abs(Zadjkl)) &
            /(4*(r-1))
      end if
      Dij_err2(r-1)=maxZadjf*max(Cij_err2(r-1),2*D00_err2(r))/abs(Zadjff)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rgm = r
      gloop: do gm=1,min(gtrunc,r-1)
        rgm = rgm-1

!        write(*,*) 'gloop ',gm,rgm

        ! calculating
        ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
        ! exploiting eq. (5.40)
        maxDexpgm(1,rgm,gm) = 0d0
        do n0=rgm/2,1,-1
          do n1=0,rgm-2*n0
            do n2=0,rgm-2*n0-n1
              n3=rgm-2*n0-n1-n2

              inds0(1) = n1
              inds0(2) = n2
              inds0(3) = n3

              inds = inds0
              inds(k) = inds(k)+1
              inds(l) = inds(l)+1
              DexpgmAux = Xtilde*Dexpgm(n0-1,n1,n2,n3,gm)  &
                       - detZ*Dexpgm(n0-1,inds(1),inds(2),inds(3),gm-1)


              do i=1,4
                n = inds2(1,i)
                m = inds2(2,i)

                Skl = 0d0

                inds = inds0
                if (inds(m).ge.1) then
                  inds(m) = inds(m)-1
                  Skl = Skl - 2d0*f(n)*inds0(m)*Dexpgm(n0,inds(1),inds(2),inds(3),gm)
                  if (inds(n).ge.1) then
                    inds(n) = inds(n)-1
                    Skl = Skl - 4d0*inds0(m)*(inds(n)+1)*Dexpgm(n0+1,inds(1),inds(2),inds(3),gm)
                  end if
                end if
                inds = inds0
                if (inds(n).ge.1) then
                  inds(n) = inds(n)-1
                  Skl = Skl - 2d0*f(m)*inds0(n)*Dexpgm(n0,inds(1),inds(2),inds(3),gm)
                end if

                DexpgmAux = DexpgmAux - Zadj2(i)*Skl

              end do

              Dexpgm(n0,n1,n2,n3,gm) = DexpgmAux/(2d0*Zadjkl)/(2d0*(rgm-n0))

               
              if(n0.eq.1) then
                maxDexpgm(1,rgm,gm) =  maxDexpgm(1,rgm,gm) + abs(Dexpgm(n0,n1,n2,n3,gm))

                if (gm.eq.1.and.abs(Dexpgm(1,n1,n2,n3,gm)).gt.            &
                    truncfacexp*max(1/m2scale,maxDexpgm(1,rgm,gm-1)) .or. &
                    gm.ge.2.and.abs(Dexpgm(1,n1,n2,n3,gm)).gt.            &
                    truncfacexp*maxDexpgm(1,rgm,gm-1)) then

#ifdef Dgmtest
                  write(*,*) 'CalcDgm exit gloop',n0,n1,n2,n3,gm,abs(Dexpgm(n0,n1,n2,n3,gm)),maxDexpgm(1,rgm,gm-1),truncfacexp
#endif      

                  gtrunc = gm-1
                  exit gloop
                end if
              end if

            end do
          end do
        end do

#ifndef PPEXP00
        do n0=rgm/2,1,-1
          if (rgm-n0.le.rmax) then
            do n1=0,rgm-2*n0
              do n2=0,rgm-2*n0-n1
                n3=rgm-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgm(n0,n1,n2,n3,gm)
              end do
            end do
          end if
        end do
#endif
!        write(*,*) 'CalcDgm after it1 ',rgm

        ! calculate
        ! D_00ijkl.. --> D_aijkl..
        ! exploiting eq. (5.38) contracted with f(j)

!        write(*,*) 'CalcDgm maxDexp',rgm-1,gm-1,maxDexpgm(0,rgm-1,gm-1)

        maxDexpgm(0,rgm-1,gm) = 0d0
        do n1=0,rgm-1
          do n2=0,rgm-1-n1
            n3 = rgm-1-n1-n2

            Smod = 0d0
            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2d0*n1*Dexpgm(1,n1-1,n2,n3,gm)
            end if
            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2d0*n2*Dexpgm(1,n1,n2-1,n3,gm)
            end if
            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2d0*n3*Dexpgm(1,n1,n2,n3-1,gm)
            end if

            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
!            inds(j) = inds(j)+1
            Dexpgm(0,n1,n2,n3,gm) = (Zadjf(1)*Smod(1) +  Zadjf(2)*Smod(2)  &
                +  Zadjf(3)*Smod(3)  &
                - detZ*(          &
                f(1)*Dexpgm(0,inds(1)+1,inds(2),inds(3),gm-1)   &
                +f(2)*Dexpgm(0,inds(1),inds(2)+1,inds(3),gm-1)   &
                +f(3)*Dexpgm(0,inds(1),inds(2),inds(3)+1,gm-1))   &
            )/Zadjff

            maxDexpgm(0,rgm-1,gm) =  maxDexpgm(0,rgm-1,gm) + abs(Dexpgm(0,n1,n2,n3,gm))

!              if(n1.eq.0.and.n2.eq.1.and.n3.eq.2) then 
!                write(*,*) 'D2(2,3,3)= ',gm,Dexpgm(0,n1,n2,n3,gm)
!                write(*,*) 'D2(2,3,3)= ',Zadj(1,j)*Smod(1)/Zadjfj,  Zadj(2,j)*Smod(2)/Zadjfj,  &
!                                +  Zadj(3,j)*Smod(3)/Zadjfj,  &
!                                - detZ*Dexpgm(0,inds(1),inds(2),inds(3),gm-1)/Zadjfj
!                write(*,*) 'D2(2,3,3)= ',inds(1),inds(2),inds(3),         &
!                                - detZ/Zadjfj,Dexpgm(0,inds(1),inds(2),inds(3),gm-1)
!              end if

            if (gm.eq.1.and.abs(Dexpgm(0,n1,n2,n3,gm)).gt.                  &
                truncfacexp*max(1/m2scale**2,maxDexpgm(0,rgm-1,gm-1)) .or.  &
                gm.ge.2.and.abs(Dexpgm(0,n1,n2,n3,gm)).gt.                  &
                truncfacexp*maxDexpgm(0,rgm-1,gm-1)) then

#ifdef Dgmtest
                  write(*,*) 'CalcDgm exit gloop',0,n1,n2,n3,gm,abs(Dexpgm(0,n1,n2,n3,gm)),maxDexpgm(0,rgm-1,gm-1),truncfacexp
#endif      
              gtrunc = gm-1
              exit gloop
            end if

          end do
        end do

        ! error propagation from C's
        if(rgm.gt.1)then
          D00_err(rgm) = max( D00_err(rgm),                                &
              max( abs(m02)*Dij_err(rgm-2),                               &
              max( abs(detZ)*Dij_err(rgm),abs(Xtilde)*Dij_err(rgm-2),      &
              maxZadj2f*D00_err(rgm-1) ) / abs(Zadjkl) )                  &
              /(4*(rgm-1))   )
        end if
        Dij_err(rgm-1)=max(Dij_err(rgm-1),                &
            max(2*maxZadjf*D00_err(rgm),abs(detZ*fmax)*Dij_err(rgm))/abs(Zadjff) )
       if(rgm.gt.1)then
          D00_err2(rgm) = max( D00_err2(rgm),                                &
              max( abs(m02)*Dij_err2(rgm-2),                               &
              max( abs(detZ)*Dij_err2(rgm),abs(Xtilde)*Dij_err2(rgm-2),      &
              maxZadj2f*D00_err2(rgm-1) ) / abs(Zadjkl) )                  &
              /(4*(rgm-1))   )
        end if
        Dij_err2(rgm-1)=max(Dij_err2(rgm-1),                &
            max(2*maxZadjf*D00_err2(rgm),abs(detZ*fmax)*Dij_err2(rgm))/abs(Zadjff) )

#ifdef PPEXP00
        do n0=rgm/2,1,-1
          if (rgm-n0.le.rmax) then
            do n1=0,rgm-2*n0
              do n2=0,rgm-2*n0-n1
                n3=rgm-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgm(n0,n1,n2,n3,gm)
              end do
            end do
          end if
        end do
#endif
!        write(*,*) 'CalcDgm after it1 ',rgm
        if ((rgm.le.rmax+1)) then
          Derr(rgm-1) = 0d0
          do n1=0,rgm-1
            do n2=0,rgm-1-n1
              n3 = rgm-1-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgm(0,n1,n2,n3,gm)
!              Derr(rgm-1)=max(Derr(rgm-1),abs(Dexpgm(0,n1,n2,n3,gm))**2 & 
!              /abs(Dexpgm(0,n1,n2,n3,gm-1)))
              if(abs(Dexpgm(0,n1,n2,n3,gm-1)).ne.0d0) then
                Derr(rgm-1)=max(Derr(rgm-1),abs(Dexpgm(0,n1,n2,n3,gm)) &
                    *min(1d0,abs(Dexpgm(0,n1,n2,n3,gm))/abs(Dexpgm(0,n1,n2,n3,gm-1))))
              else
                Derr(rgm-1)=max(Derr(rgm-1),abs(Dexpgm(0,n1,n2,n3,gm)))
              endif

#ifdef Dgmtest
!              write(*,*) 'CalcDgm Derr calc',rgm-1,Derr(rgm-1),n1,n2,n3,abs(Dexpgm(0,n1,n2,n3,gm)),abs(Dexpgm(0,n1,n2,n3,gm-1))
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
#ifdef PVEST2
          if(Dij_err2(rgm-1).gt.3d0*Derr(rgm-1)) then
#else
          if(Dij_err(rgm-1).gt.3d0*Derr(rgm-1)) then
#endif
            gtrunc = min(gm,gtrunc)
             
#ifdef Dgmtest
             write(*,*) 'CalcDgm exit err',r,rgm-1,gm,gtrunc,Dij_err(rgm-1),Derr(rgm-1)
#endif

          end if

        end if

      end do gloop

#ifdef Dgmtest
      write(*,*) 'CalcDgm D(0,0,0,0) = ',r,D(0,0,0,0)
    if(r.gt.1)then
!      write(*,*) 'CalcDgm D(0,1,0,0) = ',r,D(0,1,0,0)
      write(*,*) 'CalcDgm D(0,0,1,0) = ',r,D(0,0,1,0)
    endif
    if(r.gt.2.and.rmax.ge.2)then
      write(*,*) 'CalcDgm D(1,0,0,0) = ',r,D(1,0,0,0)
      write(*,*) 'CalcDgm D(0,2,0,0) = ',r,D(0,2,0,0)
      write(*,*) 'CalcDgm D(0,0,1,1) = ',r,D(0,0,1,1)
!      write(*,*) 'CalcDgm D(0,1,1,0) = ',r,D(0,1,1,0)
      write(*,*) 'CalcDgm D(0,0,2,0) = ',r,D(0,0,2,0)
    endif
    if(r.gt.3.and.rmax.ge.3)then
      write(*,*) 'CalcDgm D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDgm D(1,1,0,0) = ',r,D(1,1,0,0)
      write(*,*) 'CalcDgm D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDgm D(1,0,0,1) = ',r,D(1,0,0,1)
!      write(*,*) 'CalcDgm D(1,2,0,0) = ',r,D(1,2,0,0)
      write(*,*) 'CalcDgm D(0,3,0,0) = ',r,D(0,3,0,0)
      write(*,*) 'CalcDgm D(0,2,1,0) = ',r,D(0,2,1,0)
      write(*,*) 'CalcDgm D(0,2,0,1) = ',r,D(0,2,0,1)
      write(*,*) 'CalcDgm D(0,0,3,0) = ',r,D(0,0,3,0)
      write(*,*) 'CalcDgm D(0,1,1,1) = ',r,D(0,1,1,1)
      write(*,*) 'CalcDgm D(0,0,2,1) = ',r,D(0,0,2,1)
    endif
      write(*,*) 'CalcDgm Dij_err',r,Dij_err
      write(*,*) 'CalcDgm Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgm err',r,Derr
      write(*,*) 'CalcDgm acc',r,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dgmtest
!      write(*,*) 'CalcDgm exit r',r,maxval(Derr),acc_req_D*abs(D(0,0,0,0))
#endif

!     if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit   ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then

        if (r.lt.rmax) then
          do rgm=r+1,rmax
!            write(*,*) 'CalcDgm exit rloop  =',rgm,r,rmax
            do n0=0,rgm/2
              do n1=0,rgm-2*n0
                do n2=0,rgm-2*n0-n1
                  D(n0,n1,n2,rgm-2*n0-n1-n2)=0d0
                end do
              end do
            end do
          end do
          if(r.le.rmax) then
            do n1=0,r
              do n2=0,r-n1
                D(0,n1,n2,r-n1-n2)=0d0
              end do
            end do
          end if

100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDgm',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDgm:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDgm: p10 = ',p10
            write(nerrout_coli,111)' CalcDgm: p21 = ',p21
            write(nerrout_coli,111)' CalcDgm: p32 = ',p32
            write(nerrout_coli,111)' CalcDgm: p30 = ',p30
            write(nerrout_coli,111)' CalcDgm: p20 = ',p20
            write(nerrout_coli,111)' CalcDgm: p31 = ',p31
            write(nerrout_coli,111)' CalcDgm: m02 = ',m02
            write(nerrout_coli,111)' CalcDgm: m12 = ',m12
            write(nerrout_coli,111)' CalcDgm: m22 = ',m22
            write(nerrout_coli,111)' CalcDgm: m32 = ',m32
          end if
        end if
 
#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif
        exit rloop
      end if

    end do rloop


      ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
!    do r=rmax+1,2*rmax
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 
          end do
        end do
      end do
    end do

#ifdef Dgmtest
!    write(*,*) 'CalcDgm D(0,0,0,0) = ',D(0,0,0,0)
!    if(rmax.ge.3)then
!      write(*,*) 'CalcDgm D(0,1,1,1) = ',D(0,1,1,1)
!    endif

!    write(*,*) 'CalcDgm final err',Derr
!    write(*,*) 'CalcDgm final acc',Derr/abs(D(0,0,0,0))
#endif      
                           
  end subroutine CalcDgm
#endif  


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgr(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordgr_min,ordgr_max,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgr(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgr_min,ordgr_max,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,ordgr_min,ordgr_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: Zadjfj,Zadj2(3,3), Zadjkl, Xtilde
    double complex, allocatable :: Dexpgr(:,:,:,:,:), DuvExpgr(:,:,:,:)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3), Skl, Daux
    double complex :: cC0f, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
!    double precision :: maxDexpgr(0:1,0:rmax+ordgr_min+1,0:ordgr_max),truncfacexp
    double precision :: maxDexpgr(0:1,0:2*(rmax+ordgr_min),0:ordgr_max),truncfacexp
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,k,l,i,j,m,n,g,rg,nt,mt,nn,nnt,nntt
    integer :: inds0(3), inds1(3), inds(3)
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgrtest
    write(*,*) 'CalcDgr in, ord',rmax,ordgr_min,ordgr_max
#endif

    ! allocation of C functions
    rmaxC = 2*rmax + 2*ordgr_min
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax) = acc_req_CinD
    if (fac_gr.ne.0d0) then
      do r=rmax+1,rmaxC
!        acc_req_Cextra(r)= acc_req_Cextra(r-1)/x_g
! 09.03.15
        acc_req_Cextra(r)= acc_req_Cextra(r-1)/fac_gr
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+1:rmaxC) = acc_inf
    end if

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

#ifdef Dgrtest
    write(*,*) 'CalcDgr Cerr 0 =',Cerr_i(0:rmaxC,0)
    write(*,*) 'CalcDgr Cerr 1 =',Cerr_i(0:rmaxC,1)
    write(*,*) 'CalcDgr Cerr 2 =',Cerr_i(0:rmaxC,2)
    write(*,*) 'CalcDgr Cerr 3 =',Cerr_i(0:rmaxC,3)
    write(*,*) 'CalcDgr Cacc 0 =',Cerr_i(0:rmaxC,0)/abs(C_0(0,0,0,0))
    write(*,*) 'CalcDgr Cacc 1 =',Cerr_i(0:rmaxC,1)/abs(C_i(0,0,0,1))
    write(*,*) 'CalcDgr Cacc 2 =',Cerr_i(0:rmaxC,2)/abs(C_i(0,0,0,2))
    write(*,*) 'CalcDgr Cacc 3 =',Cerr_i(0:rmaxC,3)/abs(C_i(0,0,0,3))
#endif

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do


    ! calculate adjugated Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)

!    Z(1,1) = 2d0*q10
!    Z(2,1) = q10+q20-q21
!    Z(3,1) = q10+q30-q31
!    Z(1,2) = Z(2,1)
!    Z(2,2) = 2d0*q20
!    Z(3,2) = q20+q30-q32
!    Z(1,3) = Z(3,1)
!    Z(2,3) = Z(3,2)
!    Z(3,3) = 2d0*q30

!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)

!    Zadj(1,1) = (4d0*q30*q20-q2q3*q2q3)
!    Zadj(2,1) = (q1q3*q2q3-2d0*q30*q1q2)
!    Zadj(3,1) = (q1q2*q2q3-2d0*q20*q1q3)
!    Zadj(1,2) = Zadj(2,1)
!    Zadj(2,2) = (4d0*q10*q30-q1q3*q1q3)
!    Zadj(3,2) = (q1q2*q1q3-2d0*q10*q2q3)
!    Zadj(1,3) = Zadj(3,1)
!    Zadj(2,3) = Zadj(3,2)
!    Zadj(3,3) = (4d0*q10*q20-q1q2*q1q2)
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32
      
!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)


    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)

              if(n0.eq.3.and.r.eq.6) then
!             write(*,*) 'CalcDgr test ',n0,n2,n3,C_i(n0,n2,n3,1),Shat(n0,n1,n2,n3,1)
              endif
      
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)

              if(n0.eq.3.and.r.eq.6) then
!             write(*,*) 'CalcDgr test ',n0,n1,n3,C_i(n0,n1,n3,2),Shat(n0,n1,n2,n3,2)
              endif

            end if

            if(n3.eq.0) then
 
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)

              if(n0.eq.3.and.r.eq.6) then
!            write(*,*) 'CalcDgr test ',n0,n1,n2,C_i(n0,n1,n2,3), Shat(n0,n1,n2,n3,3) 
              endif

            end if

          end do
        end do
      end do
    end do


    ! choose reduction formulas with biggest denominators
    if (abs(Zadjf(1)).ge.max(abs(Zadjf(2)),abs(Zadjf(3)))) then
      j = 1
    else if (abs(Zadjf(2)).ge.max(abs(Zadjf(1)),abs(Zadjf(3)))) then
      j = 2
    else
      j = 3
    end if

    maxZadj2f = 0d0                ! Zadj2f(k,n,l) = Zadf2(k,n,l,m)*f(m)
                                   ! Zadj2(n,m) ==  Zadf2(k,n,l,m)
    if (abs(Zadj2f(1,2,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,1))
      k = 1
      n = 2
      nt = 3
      l = 1
      m = 2
      mt = 3
      Zadj2(2,2) = -Z(3,3)
      Zadj2(2,3) =  Z(3,2)
      Zadj2(3,2) =  Z(2,3)
      Zadj2(3,3) = -Z(2,2)
    end if
    if (abs(Zadj2f(1,2,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,2))
      k = 1
      n = 2
      nt = 3
      l = 2
      m = 3
      mt = 1
      Zadj2(2,1) =  Z(3,3)
      Zadj2(2,3) = -Z(3,1)
      Zadj2(3,1) = -Z(2,3)
      Zadj2(3,3) =  Z(2,1)
!      if(abs(Zadj(n,l)).gt.abs(Zadj(k,l))) then
!        k = 2
!        n = 1
!        nt = 3
!        Zadj2(1,1) = -Z(3,3)
!        Zadj2(1,3) =  Z(3,1)
!        Zadj2(3,1) =  Z(1,3)
!        Zadj2(3,3) = -Z(1,1)
!      endif
    end if
    if (abs(Zadj2f(1,2,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,3))
      k = 1
      n = 2
      nt = 3
      l = 3
      m = 1
      mt = 2
      Zadj2(2,1) = -Z(3,2)
      Zadj2(2,2) =  Z(3,1)
      Zadj2(3,1) =  Z(2,2)
      Zadj2(3,2) = -Z(2,1)
    end if

    if (abs(Zadj2f(1,3,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,1))
      k = 1
      n = 3
      nt = 2
      l = 1
      m = 2
      mt = 3
      Zadj2(3,2) =  Z(2,3)
      Zadj2(3,3) = -Z(2,2)
      Zadj2(2,2) = -Z(3,3)
      Zadj2(2,3) =  Z(3,2)
    end if
    if (abs(Zadj2f(1,3,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,2))
      k = 1
      n = 3
      nt = 2
      l = 2
      m = 3
      mt = 1
      Zadj2(3,1) = -Z(2,3)
      Zadj2(3,3) =  Z(2,1)
      Zadj2(2,1) =  Z(3,3)
      Zadj2(2,3) = -Z(3,1)
    end if
    if (abs(Zadj2f(1,3,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,3))
      k = 1
      n = 3
      nt = 2
      l = 3
      m = 1
      mt = 2
      Zadj2(3,1) =  Z(2,2)
      Zadj2(3,2) = -Z(2,1)
      Zadj2(2,1) = -Z(3,2)
      Zadj2(2,2) =  Z(3,1)
    end if

    if (abs(Zadj2f(2,3,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,1))
      k = 2
      n = 3
      nt = 1
      l = 1
      m = 2
      mt = 3
      Zadj2(3,2) = -Z(1,3)
      Zadj2(3,3) =  Z(1,2)
      Zadj2(1,2) =  Z(3,3)
      Zadj2(1,3) = -Z(3,2)
    end if
    if (abs(Zadj2f(2,3,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,2))
      k = 2
      n = 3
      nt = 1
      l = 2
      m = 3
      mt = 1
      Zadj2(3,1) =  Z(1,3)
      Zadj2(3,3) = -Z(1,1)
      Zadj2(1,1) = -Z(3,3)
      Zadj2(1,3) =  Z(3,1)
    end if
    if (abs(Zadj2f(2,3,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,3))
      k = 2
      n = 3
      nt = 1
      l = 3
      m = 1
      mt = 2
      Zadj2(3,1) = -Z(1,2)
      Zadj2(3,2) =  Z(1,1)
      Zadj2(1,1) =  Z(3,2)
      Zadj2(1,2) = -Z(3,1)
    end if

#ifdef Dgrtest
    write(*,*) 'CalcDgr maxZadj2f ',maxZadj2f,maxval(abs(Zadj2f(:,:,:)))
!    write(*,*) 'CalcDgr Zadj2f ',Zadj2f
!    write(*,*) 'CalcDgr Zadj2f ',Zadj2f(1,1,2),Zadj2f(2,1,1)
#endif

    Zadjfj = Zadjf(j)
    Zadjkl = Zadj(k,l)
!    Xtilde = Xadj(k,l)

!    write(*,*) 'CalcDg Xtilde n',Xtilde,Xadj(1,1),Xadj(1,2),Xadj(2,2)


#ifdef Dgrtest
!    write(*,*) 'CalcDgr k,n,nt,l',k,n,nt,l,m,mt
!    write(*,*) 'CalcDgr pars', maxZadj2f,Zadj2f(k,n,l),Zadj(k,l),maxZadj
!    write(*,*) 'CalcDgr pars', abs(Zadjf(l)),abs(detZ)
!    write(*,*) 'CalcDgr pars',  abs(Zadjf(l)/ maxZadj2f),abs(detZ/maxZadj2f)
#endif

    Zadjfj = Zadjf(j)
    Zadjkl = Zadj(k,l)

    ! allocation of array for expanded D-coefficients
    rmaxExp = rmaxC
    allocate(Dexpgr(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:rmaxExp,0:ordgr_max))

    ! calculate Duv
    allocate(DuvExpgr(0:(rmaxExp+1),0:rmaxExp+1,0:rmaxExp+1,0:rmaxExp+1))

!   if(rmaxexp.ge.16)then
!   write(*,*) 'CalcDgr Cuv_0',Cuv_0(1,3,3,3)
!   endif

    call CalcDuv(DuvExpgr,Cuv_0,mm02,f,rmaxExp+1,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgr(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))
     
    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))
     
    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

#ifdef Dgrtest
    write(*,*) 'CalcDgr Cij_err = ',Cij_err 
    write(*,*) 'CalcDgr C0_err = ', Cerr_i(0,0),Cerr_i(0,1),Cerr_i(0,2),Cerr_i(0,3)
    write(*,*) 'CalcDgr C0 = ', C_0(0,0,0,0),C_i(0,0,0,1),C_i(0,0,0,2),C_i(0,0,0,3)
#endif

!    maxZadj = maxval(abs(Zadj))
!    maxZadj2f = maxval(abs(f(inds2(1,:))*Zadj2(:)))

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
    truncfacexp = sqrt(fac_gr) * truncfacD
    gtrunc = ordgr_max 

! calculate D(n0,n1,n2,n3) up to rank r+n0
    rloop: do r=0,rmaxExp/2

#ifdef Dgrtest
!      write(*,*) 'CalcDgr rloop',r,rmax,gtrunc
#endif

      if (r.gt.rmax+gtrunc) exit rloop

#ifdef Dgrtest
      write(*,*) 'CalcDgr rloop',r,rmaxExp,rmaxC
#endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating
      ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
      ! exploiting eq. (5.40) - (5.53) solved for D_00i1..<ir>...iP
      maxDexpgr(1,r,0)=0d0

! Note r is not the rank!  r= n0+n1+n2+n3    rank=2*n0+n1+n2+n3
      do n0=r,1,-1
        do nn=r-n0,0,-1
          do nnt=r-n0-nn,0,-1
            nntt = r-n0-nn-nnt

#ifdef Dgrtest
!            write(*,*) 'CalcDgr rloop',n0,nn,nnt,nntt,Zadj2f(k,n,l)
#endif

            inds0(n) = nn
            inds0(nt) = nnt
            inds0(k) = nntt
            
#ifdef Dgrtest
            write(*,*) 'CalcDgr inds0',n0,inds0
#endif

            inds1(n) = nn+1
            inds1(nt) = nnt
            inds1(k) = nntt

#ifdef Dgrtest
            write(*,*) 'CalcDgr inds1',n0,inds1
#endif

!            Daux = 0d0
            Daux = -Zadj(k,l)*C_0(n0-1,inds1(1),inds1(2),inds1(3))

!            Daux = 2*Zadj(k,l) * (1+r-2*n0) * Dexpgr(n0,inds1(1),inds1(2),inds1(3),0)

!            inds = inds1
!            inds(k) = inds(k) + 1
!            inds(l) = inds(l) + 1
!            Daux = Daux + detZ * Dexpgr(n0-1,inds(1),inds(2),inds(3),0)
!
!            inds = inds1
!            inds(k) = inds(k) + 1
!            Daux = Daux + Zadjf(l) * Dexpgr(n0-1,inds(1),inds(2),inds(3),0)

#ifdef Dgrtest
            write(*,*) 'CalcDgr C_0 1c',n0-1,inds1(1),inds1(2),inds1(3),C_0(n0-1,inds1(1),inds1(2),inds1(3))
            write(*,*) 'CalcDgr Daux 1c',-Zadj(k,l)*C_0(n0-1,inds1(1),inds1(2),inds1(3))
            write(*,*) 'CalcDgr Daux 1s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif

            inds = inds1
            inds(k) = inds(k)+1
            do i=1,3
              Daux = Daux - Zadj(i,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
#ifdef Dgrtest
            write(*,*) 'CalcDgr Daux 2ci', -Zadj(i,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
#endif
            end do

#ifdef Dgrtest
            write(*,*) 'CalcDgr Daux 2s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif

            do i=1,3
              inds = inds1
              inds(i) = inds(i)+1
              Daux = Daux + Zadj(k,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
#ifdef Dgrtest
              write(*,*) 'CalcDgr Daux 3ci',Zadj(k,l)*Shat(n0-1,inds(1),inds(2),inds(3),i)
#endif
            end do


#ifdef Dgrtest
            write(*,*) 'CalcDgr Daux 3s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif

            Daux = Daux + 2*(nn+1) *Zadj2(n ,m )*Shat(n0,inds0(1),inds0(2),inds0(3),m)  &
                        + 2*(nn+1) *Zadj2(n ,mt)*Shat(n0,inds0(1),inds0(2),inds0(3),mt)


#ifdef Dgrtest
            write(*,*) 'CalcDgr Daux 4ca', 2*(nn+1) *Zadj2(n ,m )*Shat(n0,inds0(1),inds0(2),inds0(3),m) 
            write(*,*) 'CalcDgr Daux 4cb', 2*(nn+1) *Zadj2(n ,mt)*Shat(n0,inds0(1),inds0(2),inds0(3),mt)
            write(*,*) 'CalcDgr Daux 4s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif

!            Daux = Daux - 2*(nn+1)* Zadj2f(k,n,l)*Dexpgr(n0,inds0(1),inds0(2),inds0(3),0) 

            if (nnt.gt.0) then
              inds = inds1
              inds(nt) = inds(nt)-1
              Daux = Daux + 2*nnt*Zadj2(nt,m )*Shat(n0,inds(1),inds(2),inds(3),m)  &
                          + 2*nnt*Zadj2(nt,mt)*Shat(n0,inds(1),inds(2),inds(3),mt)
              Daux = Daux - 2*nnt*Zadj2f(k,nt,l)*Dexpgr(n0,inds(1),inds(2),inds(3),0) 

#ifdef Dgrtest
            write(*,*) 'CalcDgr Daux 5ci', 2*nnt*Zadj2(nt,m )*Shat(n0,inds(1),inds(2),inds(3),m) 
            write(*,*) 'CalcDgr Daux 5ci', 2*nnt*Zadj2(nt,mt)*Shat(n0,inds(1),inds(2),inds(3),mt) 
            write(*,*) 'CalcDgr Daux 5ci', 2*nnt*Zadj2f(k,nt,l)*Dexpgr(n0,inds(1),inds(2),inds(3),0) 
            write(*,*) 'CalcDgr Daux 5cii', 2*nnt*Zadj2(nt,m ),Shat(n0,inds(1),inds(2),inds(3),m) 
            write(*,*) 'CalcDgr Daux 5cii', 2*nnt*Zadj2(nt,mt),Shat(n0,inds(1),inds(2),inds(3),mt) 
            write(*,*) 'CalcDgr Daux 5cii', 2*nnt*Zadj2f(k,nt,l),Dexpgr(n0,inds(1),inds(2),inds(3),0) 
            write(*,*) 'CalcDgr Daux 5s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
            endif


            inds = inds1
            if(m.eq.n) then
              if (inds(n).gt.1) then
                inds(n) = inds(n)-2
                Daux = Daux - 4*(nn+1)*nn * Zadj2(n,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 6c',4*(nn+1)*nn* Zadj2(n,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
                write(*,*) 'CalcDgr Daux 6s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            else
              if (inds(n).gt.0.and.inds(m).gt.0) then
                inds(n) = inds(n)-1
                inds(m) = inds(m)-1
                Daux = Daux - 4*(nn+1)*(inds(m)+1)* Zadj2(n,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 6c',-4*(nn+1)*(inds(m)+1)* Zadj2(n,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
                write(*,*) 'CalcDgr Daux 6s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            endif


            inds = inds1
            if(m.eq.nt) then
              if (inds(nt).gt.1) then
                inds(nt) = inds(nt)-2
                Daux = Daux - 4*nnt*(nnt-1) * Zadj2(nt,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 7c',4*nnt*(nnt-1) * Zadj2(nt,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
                write(*,*) 'CalcDgr Daux 7s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            else
              if (inds(nt).gt.0.and.inds(m).gt.0) then
                inds(nt) = inds(nt)-1
                inds(m) = inds(m)-1
                Daux = Daux - 4*nnt*(inds(m)+1)* Zadj2(nt,m )* Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 7c',4*nnt*(inds(m)+1)* Zadj2(nt,m )* Dexpgr(n0,inds(1),inds(2),inds(3),0)
                write(*,*) 'CalcDgr Daux 7s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            endif


            inds = inds1
            if(mt.eq.n) then
              if (inds(n).gt.1) then
                inds(n) = inds(n)-2
                Daux = Daux - 4*(nn+1)*nn * Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 8c',- 4*(nn+1)*nn * Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0)  &
             , n0+1,inds(1),inds(2),inds(3)
                write(*,*) 'CalcDgr Daux 8s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            else
              if (inds(n).gt.0.and.inds(mt).gt.0) then
                inds(n) = inds(n)-1
                inds(mt) = inds(mt)-1
                Daux = Daux - 4*(nn+1)*(inds(mt)+1)* Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 8c',- 4*(nn+1)*(inds(mt)+1)* Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0)   &
             ,n0+1,inds(1),inds(2),inds(3)
                write(*,*) 'CalcDgr Daux 8s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            endif


            inds = inds1
            if(mt.eq.nt) then
              if (inds(nt).gt.1) then
                inds(nt) = inds(nt)-2
                Daux = Daux - 4*nnt*(nnt-1)  * Zadj2(nt,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 9c', - 4*nnt*(nnt-1)  * Zadj2(nt,mt) * Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
                write(*,*) 'CalcDgr Daux 9s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            else
              if (inds(nt).gt.0.and.inds(mt).gt.0) then
                inds(nt) = inds(nt)-1
                inds(mt) = inds(mt)-1
                Daux = Daux - 4*nnt*(inds(mt)+1) * Zadj2(nt,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
#ifdef Dgrtest
                write(*,*) 'CalcDgr Daux 9c',- 4*nnt*(inds(mt)+1) * Zadj2(nt,mt) * Dexpgr(n0+1,inds(1),inds(2),inds(3),0) 
                write(*,*) 'CalcDgr Daux 9s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif
              endif
            endif

            Dexpgr(n0,inds0(1),inds0(2),inds0(3),0) = Daux/(2*(nn+1)* Zadj2f(k,n,l))

#ifdef Dgrtest
                write(*,*) 'CalcDgr Dexpgr',n0,inds0(1),inds0(2),inds0(3),Dexpgr(n0,inds0(1),inds0(2),inds0(3),0)
#endif

            if (n0.eq.1) then
              maxDexpgr(1,r,0) =  maxDexpgr(1,r,0) + abs(Dexpgr(n0,inds0(1),inds0(2),inds0(3),0) )
            end if

!            if (r+n0.le.rmax) then          !  for fixed rank
            if (r.le.rmax) then
              D(n0,inds0(1),inds0(2),inds0(3)) = Dexpgr(n0,inds0(1),inds0(2),inds0(3),0)
            end if

          end do
        end do
      end do

      ! calculate
      ! D_00ijkl.. --> D_aijkl..
      ! exploiting eq. (5.38)
      maxDexpgr(0,r,0)=0d0
      do n1=0,r
        do n2=0,r-n1
          n3 = r-n1-n2

          Smod = Shat(0,n1,n2,n3,:)
          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2d0*n1*Dexpgr(1,n1-1,n2,n3,0)
          end if
          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2d0*n2*Dexpgr(1,n1,n2-1,n3,0)
          end if
          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2d0*n3*Dexpgr(1,n1,n2,n3-1,0)
          end if

          Dexpgr(0,n1,n2,n3,0) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                              +  Zadj(3,j)*Smod(3))/Zadjfj
          maxDexpgr(0,r,0) =  maxDexpgr(0,r,0) + abs(Dexpgr(0,n1,n2,n3,0))
          if (r.le.rmax) then
            D(0,n1,n2,n3) = Dexpgr(0,n1,n2,n3,0)
!            Derr(r-1) =  abs(detZ/Zadjfj*Dexpgr(0,n1,n2,n3,0))
          end if


#ifdef Dgrtest
          if(r.le.rmax) then
!            write(*,*) 'CalcDgr D(0,n1,n2,n3,0)=',n1,n2,n3,D(0,n1,n2,n3)
          endif

          if(n0.eq.0.and.n1.eq.3.and.n2.eq.0.and.n3.eq.0) then 
            write(*,*) 'Smod(0,3,0,0,1)= ',Shat(0,n1,n2,n3,1)
            write(*,*) 'Smod(0,3,0,0,2)= ',Shat(0,n1,n2,n3,2)
            write(*,*) 'Smod(0,3,0,0,3)= ',Shat(0,n1,n2,n3,3)
            write(*,*) 'D(1,2,0,0)= ',0,Dexpgr(1,2,n2,n3,0)
            write(*,*) 'D(0,3,0,0)= ',0,D(n0,n1,n2,n3)
          end if
#endif

        end do
      end do

#ifdef Dgrtest
!      write(*,*) 'CalcDgr maxDexpgr 0',r-1, maxDexpgr(0,r-1,0)
#endif

      if(r.le.rmax) then
!       Derr(r) =  abs(detZ/Zadjfj)*maxDexpgr(0,r,0)
        Derr(r) =  fac_gr*maxDexpgr(0,r,0)
      endif

      ! error propagation from C's
      if(r.gt.0)then
        D00_err(r+1) = maxZadj*Cij_err(r+1)/(2*maxZadj2f)
      end if
      Dij_err(r)=maxZadj*max(Cij_err(r),2*D00_err(r+1))/abs(Zadjfj)

      if(r.gt.0)then
        D00_err2(r+1) = maxZadj*Cij_err2(r+1)/(2*maxZadj2f)
      end if
      Dij_err2(r)=maxZadj*max(Cij_err2(r),2*D00_err2(r+1))/abs(Zadjfj)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rg = r
      gloop: do g=1,min(gtrunc,r)
        rg = rg-1

#ifdef Dgrtest
        write(*,*) 'CalcDgr: gloop ',r,rg,g
#endif

        ! calculating
        ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
       ! exploiting eq. (5.40) - (5.53) solved for D_00i1..<ir>...iP
        maxDexpgr(1,rg,g) = 0d0
        do n0=rg,1,-1                      !  note rank of tensor = rg+n0
          do nn=rg-n0,0,-1
            do nnt=rg-n0-nn,0,-1
              nntt = rg-n0-nn-nnt
              inds0(n) = nn
              inds0(nt) = nnt
              inds0(k) = nntt
              
              inds1(n) = nn+1
              inds1(nt) = nnt
              inds1(k) = nntt
              
#ifdef Dgrtest
!              write(*,*) 'CalcDgr Daux r inds=',n0,inds0
#endif      

              Daux = 2*Zadj(k,l) * (2+rg-n0) * Dexpgr(n0,inds1(1),inds1(2),inds1(3),g-1)
              
#ifdef Dgrtest
!              write(*,*) 'CalcDgr Daux r1c',2*Zadj(k,l)*(2+rg-n0)* Dexpgr(n0,inds1(1),inds1(2),inds1(3),g-1)
!              write(*,*) 'CalcDgr Daux r1c',2*Zadj(k,l)*(2+rg-n0),Dexpgr(n0,inds1(1),inds1(2),inds1(3),g-1) &
!                   ,n0,inds1(1),inds1(2),inds1(3)
!              write(*,*) 'CalcDgr Daux r1s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      

              if (g.gt.1) then
                inds = inds1
                inds(k) = inds(k) + 1
                inds(l) = inds(l) + 1
                Daux = Daux + detZ * Dexpgr(n0-1,inds(1),inds(2),inds(3),g-2)
                
#ifdef Dgrtest
!                write(*,*) 'CalcDgr Daux r2c',detZ * Dexpgr(n0-1,inds(1),inds(2),inds(3),g-2)
!                write(*,*) 'CalcDgr Daux r2s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
              endif

              inds = inds1
              inds(k) = inds(k) + 1
              Daux = Daux + Zadjf(l) * Dexpgr(n0-1,inds(1),inds(2),inds(3),g-1)
              
#ifdef Dgrtest
!              write(*,*) 'CalcDgr Daux r3c',Zadjf(l)* Dexpgr(n0-1,inds(1),inds(2),inds(3),g-1)
!              write(*,*) 'CalcDgr Daux r3c',Zadjf(l),Dexpgr(n0-1,inds(1),inds(2),inds(3),g-1),n0-1,inds(1),inds(2),inds(3)
!              write(*,*) 'CalcDgr Daux r3s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      

!            Daux = Daux - 2*nn* Zadj2f(k,n,l)*Dexpgr(n0,inds0(1),inds0(2),inds0(3),g) 

              if (nnt.gt.0) then
                inds = inds1
                inds(nt) = inds(nt)-1
                Daux = Daux - 2*nnt*Zadj2f(k,nt,l)*Dexpgr(n0,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!              write(*,*) 'CalcDgr Daux r4c',- 2*nnt*Zadj2f(k,nt,l)*Dexpgr(n0,inds(1),inds(2),inds(3),g) 
!              write(*,*) 'CalcDgr Daux r4c',- 2*nnt,Zadj2f(k,nt,l),Dexpgr(n0,inds(1),inds(2),inds(3),g),n0,inds(1),inds(2),inds(3) 
!              write(*,*) 'CalcDgr Daux r4s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
              endif


              inds = inds1
              if(m.eq.n) then
                if (inds(n).gt.1) then
                  inds(n) = inds(n)-2
                  Daux = Daux - 4*(nn+1)*nn * Zadj2(n,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r6c',4*(nn+1)*nn* Zadj2(n,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r6s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              else
                if (inds(n).gt.0.and.inds(m).gt.0) then
                  inds(n) = inds(n)-1
                  inds(m) = inds(m)-1
                  Daux = Daux - 4*(nn+1)*(inds(m)+1)* Zadj2(n,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r6c',4*(nn+1)*(inds(m)+1)* Zadj2(n,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r6c',n,m,nn,4*(nn+1)*(inds(m)+1),Zadj2(n,m ),Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r6s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              endif
              
              inds = inds1
              if(m.eq.nt) then
                if (inds(nt).gt.1) then
                  inds(nt) = inds(nt)-2
                  Daux = Daux - 4*nnt*(nnt-1) * Zadj2(nt,m ) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r7c',4*nnt*(nnt-1) * Zadj2(nt,m ) *Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r7s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              else
                if (inds(nt).gt.0.and.inds(m).gt.0) then
                  inds(nt) = inds(nt)-1
                  inds(m) = inds(m)-1
                  Daux = Daux - 4*nnt*(inds(m)+1)* Zadj2(nt,m )* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r7c',4*nnt*(inds(m)+1)* Zadj2(nt,m )* Dexpgr(n0,inds(1),inds(2),inds(3),g)
!                  write(*,*) 'CalcDgr Daux r7s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              endif

              inds = inds1
              if(mt.eq.n) then
                if (inds(n).gt.1) then
                  inds(n) = inds(n)-2
                  Daux = Daux - 4*(nn+1)*nn * Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r8c',- 4*(nn+1)*nn * Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r8c',n,mt,nn,- 4*(nn+1)*nn,Zadj2(n ,mt),Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r8s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              else
                if (inds(n).gt.0.and.inds(mt).gt.0) then
                  inds(n) = inds(n)-1
                  inds(mt) = inds(mt)-1
                  Daux = Daux - 4*(nn+1)*(inds(mt)+1)* Zadj2(n ,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r8c',- 4*(nn+1)*(inds(mt)+1)* Zadj2(n ,mt) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g)
!                  write(*,*) 'CalcDgr Daux r8s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              endif

              inds = inds1
              if(mt.eq.nt) then
                if (inds(nt).gt.1) then
                  inds(nt) = inds(nt)-2
                  Daux = Daux - 4*nnt*(nnt-1)  * Zadj2(nt,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r9c', - 4*nnt*(nnt-1)  * Zadj2(nt,mt) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r9s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              else
                if (inds(nt).gt.0.and.inds(mt).gt.0) then
                  inds(nt) = inds(nt)-1
                  inds(mt) = inds(mt)-1
                  Daux = Daux - 4*nnt*(inds(mt)+1) * Zadj2(nt,mt)* Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
#ifdef Dgrtest
!                  write(*,*) 'CalcDgr Daux r9c',- 4*nnt*(inds(mt)+1) * Zadj2(nt,mt) * Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r9c',nt,mt,nnt,- 4*nnt*(inds(mt)+1) , Zadj2(nt,mt) , &
!                          Dexpgr(n0+1,inds(1),inds(2),inds(3),g) 
!                  write(*,*) 'CalcDgr Daux r9s',Daux,Daux/(2*(nn+1)* Zadj2f(k,n,l))
#endif      
                endif
              endif

              Dexpgr(n0,inds0(1),inds0(2),inds0(3),g) = Daux/(2*(nn+1)* Zadj2f(k,n,l))
                             
              if(n0.eq.1) then
                maxDexpgr(1,rg,g) =  maxDexpgr(1,rg,g) + abs(Dexpgr(n0,inds0(1),inds0(2),inds0(3),g))


                if (g.eq.1.and.abs(Dexpgr(1,inds0(1),inds0(2),inds0(3),g)).gt.  &
                    truncfacexp*max(1/m2scale,maxDexpgr(1,rg,g-1)) .or. &
                    g.ge.2.and.abs(Dexpgr(1,inds0(1),inds0(2),inds0(3),g)).gt.  &
                    truncfacexp*maxDexpgr(1,rg,g-1)) then


#ifdef Dgrtest
!                  write(*,*) 'CalcDgr exit gloop',n0,inds0(1),inds0(2),inds0(3),g,rg,  &
!                         abs(Dexpgr(n0,inds0(1),inds0(2),inds0(3),g)),maxDexpgr(1,rg,g-1),truncfacexp
#endif      

                  gtrunc = g-1
                  exit gloop
                end if
              end if

            end do
          end do
        end do

#ifndef PPEXP00
        if (rg.le.rmax) then
          do n0=rg,1,-1
!            if (rg+n0.le.rmax) then             ! for fixed rank!
            if (rg.le.rmax) then
              do n1=0,rg-n0
                do n2=0,rg-n0-n1
                  n3=rg-n0-n1-n2
                  D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgr(n0,n1,n2,n3,g)
                end do
              end do
            end if
          end do
        end if
#endif
!        write(*,*) 'CalcDgr after it1 ',rg

        ! calculate
        ! D_00ijkl.. --> D_aijkl..
        ! exploiting eq. (5.38)

!        write(*,*) 'CalcDgr maxDexp',rg,g-1,maxDexpgr(0,rg,g-1)

        maxDexpgr(0,rg,g) = 0d0
        do n1=0,rg
          do n2=0,rg-n1
            n3 = rg-n1-n2

            Smod = 0d0
            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2d0*n1*Dexpgr(1,n1-1,n2,n3,g)
            end if
            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2d0*n2*Dexpgr(1,n1,n2-1,n3,g)
            end if
            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2d0*n3*Dexpgr(1,n1,n2,n3-1,g)
            end if

            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
            inds(j) = inds(j)+1
            Dexpgr(0,n1,n2,n3,g) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                                +  Zadj(3,j)*Smod(3)  &
                                - detZ*Dexpgr(0,inds(1),inds(2),inds(3),g-1))/Zadjfj

            maxDexpgr(0,rg,g) =  maxDexpgr(0,rg,g) + abs(Dexpgr(0,n1,n2,n3,g))

!              if(n1.eq.0.and.n2.eq.1.and.n3.eq.2) then 
!                write(*,*) 'D2(2,3,3)= ',g,Dexpgr(0,n1,n2,n3,g)
!                write(*,*) 'D2(2,3,3)= ',Zadj(1,j)*Smod(1)/Zadjfj,  Zadj(2,j)*Smod(2)/Zadjfj,  &
!                                +  Zadj(3,j)*Smod(3)/Zadjfj,  &
!                                - detZ*Dexpgr(0,inds(1),inds(2),inds(3),g-1)/Zadjfj
!                write(*,*) 'D2(2,3,3)= ',inds(1),inds(2),inds(3),         &
!                                - detZ/Zadjfj,Dexpgr(0,inds(1),inds(2),inds(3),g-1)
!              end if

            if (g.eq.1.and.abs(Dexpgr(0,n1,n2,n3,g)).gt.            &
                truncfacexp*max(1/m2scale,maxDexpgr(0,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgr(0,n1,n2,n3,g)).gt.           &
                truncfacexp*maxDexpgr(0,rg,g-1)) then

#ifdef Dgrtest
                  write(*,*) 'CalcDgr exit gloop',0,n1,n2,n3,g,abs(Dexpgr(0,n1,n2,n3,g)),maxDexpgr(0,rg,g-1),truncfacexp
#endif      
              gtrunc = g-1
              exit gloop
            end if

          end do
        end do

        ! error propagation from C's
        if(rg.gt.0)then
          D00_err(rg+1) = max( D00_err(rg+1),                   &
              max( maxZadj*(2+rg-2*n0)*D00_err(rg+2),       &
                    abs(detZ)*Dij_err(rg+2),                &
                    maxZadjf*Dij_err(rg+1)                   &
                 ) / (2*maxZadj2f)  ) 
        end if
        Dij_err(rg)=max(Dij_err(rg),                &
            max(2*maxZadj*D00_err(rg+1),abs(detZ)*Dij_err(rg))/abs(Zadjfj) )

        if(rg.gt.0)then
          D00_err2(rg+1) = max( D00_err2(rg+1),                   &
              max( maxZadj*(2+rg-2*n0)*D00_err2(rg+2),       &
                    abs(detZ)*Dij_err2(rg+2),                &
                    maxZadjf*Dij_err2(rg+1)                   &
                 ) / (2*maxZadj2f)  ) 
        end if
        Dij_err2(rg)=max(Dij_err2(rg),                &
            max(2*maxZadj*D00_err2(rg+1),abs(detZ)*Dij_err2(rg))/abs(Zadjfj) )

#ifdef PPEXP00
        if (rg.le.rmax) then
          do n0=rg,1,-1
!            if (rg+n0.le.rmax) then             !  for fixed rank
            if (rg.le.rmax) then
              do n1=0,rg-n0
                do n2=0,rg-n0-n1
                  n3=rg-n0-n1-n2
                  D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgr(n0,n1,n2,n3,g)
                end do
              end do
            end if
          end do
        end if
#endif

        if (rg.le.rmax) then
          Derr(rg) = 0d0
          do n1=0,rg
            do n2=0,rg-n1
              n3 = rg-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgr(0,n1,n2,n3,g)
              if(abs(Dexpgr(0,n1,n2,n3,g-1)).ne.0d0) then
!                Derr(rg)=max(Derr(rg),abs(Dexpgr(0,n1,n2,n3,g))**2/abs(Dexpgr(0,n1,n2,n3,g-1)))
                Derr(rg)=max(Derr(rg),abs(Dexpgr(0,n1,n2,n3,g))*min(1d0,abs(Dexpgr(0,n1,n2,n3,g))/abs(Dexpgr(0,n1,n2,n3,g-1))))
              else
                Derr(rg)=max(Derr(rg),abs(Dexpgr(0,n1,n2,n3,g)))
              endif

#ifdef Dgrtest
!              write(*,*) 'CalcDgr Derr calc',rg,Derr(rg),n1,n2,n3,abs(Dexpgr(0,n1,n2,n3,g)),abs(Dexpgr(0,n1,n2,n3,g-1))
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
#ifdef PVEST2
          if(Dij_err2(rg).gt.3d0*Derr(rg)) then
#else
          if(Dij_err(rg).gt.3d0*Derr(rg)) then
#endif
            gtrunc = min(g,gtrunc)
             
#ifdef Dgrtest
             write(*,*) 'CalcDgr exit err',r,rg,g,gtrunc,Dij_err(rg),Derr(rg)
#endif

          end if

        end if

      end do gloop

#ifdef Dgrtest
      write(*,*) 'CalcDgr D(0,0,0,0) = ',r,D(0,0,0,0)
    if(r.ge.1)then
      write(*,*) 'CalcDgr D(1,0,0,0) = ',r,D(1,0,0,0)
      write(*,*) 'CalcDgr D(0,1,0,0) = ',r,D(0,1,0,0)
      write(*,*) 'CalcDgr D(0,0,1,0) = ',r,D(0,0,1,0)
      write(*,*) 'CalcDgr D(0,0,0,1) = ',r,D(0,0,0,1)
    endif
    if(r.ge.2.and.rmax.ge.2)then
      write(*,*) 'CalcDgr D(1,1,0,0) = ',r,D(1,1,0,0)
      write(*,*) 'CalcDgr D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDgr D(1,0,0,1) = ',r,D(1,0,0,1)
      write(*,*) 'CalcDgr D(0,2,0,0) = ',r,D(0,2,0,0)
!      write(*,*) 'CalcDgr D(0,1,1,0) = ',r,D(0,1,1,0)
      write(*,*) 'CalcDgr D(0,0,2,0) = ',r,D(0,0,2,0)
    endif
    if(r.ge.3.and.rmax.ge.2)then
!      write(*,*) 'CalcDgr D(3,0,0,0) = ',r,D(3,0,0,0)
!      write(*,*) 'CalcDgr D(2,0,1,0) = ',r,D(2,0,1,0)
      write(*,*) 'CalcDgr D(1,2,0,0) = ',r,D(1,2,0,0)
      write(*,*) 'CalcDgr D(1,0,2,0) = ',r,D(1,0,2,0)
      write(*,*) 'CalcDgr D(0,3,0,0) = ',r,D(0,3,0,0)
      write(*,*) 'CalcDgr D(0,2,1,0) = ',r,D(0,2,1,0)
      write(*,*) 'CalcDgr D(0,0,3,0) = ',r,D(0,0,3,0)
      write(*,*) 'CalcDgr D(0,1,1,1) = ',r,D(0,1,1,1)
      write(*,*) 'CalcDgr D(0,0,2,1) = ',r,D(0,0,2,1)
    endif
      write(*,*) 'CalcDgr Dij_err',r,Dij_err
      write(*,*) 'CalcDgr Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgr err',r,Derr
      write(*,*) 'CalcDgr acc',r,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dgrtest
!      write(*,*) 'CalcDgr exit r',r,maxval(Derr),acc_req_D*abs(D(0,0,0,0))
#endif

!      if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit    ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then
        if (r.lt.rmax) then
          do rg=r+1,rmax
            do n0=0,rg/2
              do n1=0,rg-n0
                do n2=0,rg-n0-n1
                  D(n0,n1,n2,rg-n0-n1-n2)=0d0
                enddo
              enddo
            enddo
          enddo

100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDgr',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDgr:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDgr: p10 = ',p10
            write(nerrout_coli,111)' CalcDgr: p21 = ',p21
            write(nerrout_coli,111)' CalcDgr: p32 = ',p32
            write(nerrout_coli,111)' CalcDgr: p30 = ',p30
            write(nerrout_coli,111)' CalcDgr: p20 = ',p20
            write(nerrout_coli,111)' CalcDgr: p31 = ',p31
            write(nerrout_coli,111)' CalcDgr: m02 = ',m02
            write(nerrout_coli,111)' CalcDgr: m12 = ',m12
            write(nerrout_coli,111)' CalcDgr: m22 = ',m22
            write(nerrout_coli,111)' CalcDgr: m32 = ',m32
          end if
        endif
        
#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif
        exit rloop
      end if

    end do rloop


#ifdef Dgrtest
!    write(*,*) 'CalcDgr D(0,0,0,0) = ',D(0,0,0,0)
!    if(rmax.ge.3)then
!      write(*,*) 'CalcDgr D(0,1,1,1) = ',D(0,1,1,1)
!    endif

    write(*,*) 'CalcDgr final err',Derr
    write(*,*) 'CalcDgr final acc',Derr/abs(D(0,0,0,0))
#endif      
       
!    write(*,*) 'CalcDgr Derr ',Derr
!    write(*,*) 'CalcDgr Derr2',Derr2
                    
  end subroutine CalcDgr



! CalcDgx not finished!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgx(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordg_min,ordg_max,id,Derr,Derrr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgx(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgx_min,ordgx_max,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,ordgx_min,ordgx_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex :: Zadjfj,Zadj2(4), Zadjkl, Xtilde
    double complex, allocatable :: Dexpgx(:,:,:,:,:), DuvExpgx(:,:,:,:)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: C_i(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3), Skl, Daux, DexpgAux
    double complex :: cC0f, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpgx(0:1,0:rmax+ordgx_min+1,0:ordgx_max),truncfacexp
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,k,l,i,j,m,n,g,rg,lt,ltt,nl,nlt,nltt
    integer :: inds0(3), inds(3), inds2(2,4)
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgxtest
    write(*,*) 'CalcDgx in, ord',rmax,ordgx_min,ordgx_max
#endif

    ! allocation of C functions
    rmaxC = rmax + ordgx_min
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax) = acc_req_CinD
    if (x_g.ne.0d0) then
      do r=rmax+1,rmaxC
        acc_req_Cextra(r)= acc_req_Cextra(r-1)/x_g
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+1:rmaxC) = acc_inf
    end if

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do


    ! calculate adjugated Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)

!    Z(1,1) = 2d0*q10
!    Z(2,1) = q10+q20-q21
!    Z(3,1) = q10+q30-q31
!    Z(1,2) = Z(2,1)
!    Z(2,2) = 2d0*q20
!    Z(3,2) = q20+q30-q32
!    Z(1,3) = Z(3,1)
!    Z(2,3) = Z(3,2)
!    Z(3,3) = 2d0*q30

!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)

!    Zadj(1,1) = (4d0*q30*q20-q2q3*q2q3)
!    Zadj(2,1) = (q1q3*q2q3-2d0*q30*q1q2)
!    Zadj(3,1) = (q1q2*q2q3-2d0*q20*q1q3)
!    Zadj(1,2) = Zadj(2,1)
!    Zadj(2,2) = (4d0*q10*q30-q1q3*q1q3)
!    Zadj(3,2) = (q1q2*q1q3-2d0*q10*q2q3)
!    Zadj(1,3) = Zadj(3,1)
!    Zadj(2,3) = Zadj(3,2)
!    Zadj(3,3) = (4d0*q10*q20-q1q2*q1q2)
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32
      
!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)


    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)
            end if

            if(n3.eq.0) then
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)
            end if

          end do
        end do
      end do
    end do


    ! choose reduction formulas with biggest denominators
    if (abs(Zadjf(1)).ge.max(abs(Zadjf(2)),abs(Zadjf(3)))) then
      j = 1
    else if (abs(Zadjf(2)).ge.max(abs(Zadjf(1)),abs(Zadjf(3)))) then
      j = 2
    else
      j = 3
    end if

    maxZadj2f = 0d0
    if (abs(Zadj2f(1,2,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,1))
      i = 1
      j = 2
      l = 1
      lt = 2
      ltt = 3
    end if
    if (abs(Zadj2f(1,3,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,1))
      i = 1
      j = 3
      l = 1
      lt = 2
      ltt = 3
    end if
    if (abs(Zadj2f(1,2,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,2))
      i = 1
      j = 2
      l = 2
      lt = 3
      ltt = 1
    end if
    if (abs(Zadj2f(1,3,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,2))
      i = 1
      j = 3
      l = 2
      lt = 3
      ltt = 1
    end if
    if (abs(Zadj2f(1,2,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,2,3))
      i = 1
      j = 2
      l = 3
      lt = 1
      ltt = 2
    end if
    if (abs(Zadj2f(1,3,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(1,3,3))
      i = 1
      j = 3
      l = 3
      lt = 1
      ltt = 2
    end if
    if (abs(Zadj2f(2,3,1)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,1))
      i = 2
      j = 3
      l = 1
      lt = 2
      ltt = 3
    end if
    if (abs(Zadj2f(2,3,2)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,2))
      i = 2
      j = 3
      l = 2
      lt = 3
      ltt = 1
    end if
    if (abs(Zadj2f(2,3,3)).gt.maxZadj2f) then
      maxZadj2f = abs(Zadj2f(2,3,3))
      i = 2
      j = 3
      l = 3
      lt = 1
      ltt = 2
    end if

#ifdef Dgxtest
    write(*,*) 'CalcDgx i,j,l',i,j,l,lt,ltt
    write(*,*) 'CalcDgx pars', maxZadj2f,Zadj2f(i,j,l),Zadj(i,j),maxZadj
    write(*,*) 'CalcDgx pars', abs(Zadjf(j)),abs(Xadj(i,j))
    write(*,*) 'CalcDgx pars',  abs(Zadjf(j)/ maxZadj2f),abs(Xadj(i,j)/maxZadj2f)
#endif

    Zadjfj = Zadjf(j)

    Xtilde = Xadj(k,l)

!    write(*,*) 'CalcDgx Xtilde n',Xtilde,Xadj(1,1),Xadj(1,2),Xadj(2,2)


    ! allocation of array for det(Z)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpgx(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:rmaxExp,0:ordgx_max))


    ! calculate Duv
    allocate(DuvExpgx(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpgx,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgx(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))
     
    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))
     
    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

#ifdef Dgxtest
    write(*,*) 'CalcDgx Cij_err = ',Cij_err 
    write(*,*) 'CalcDgx C0_err = ', Cerr_i(0,0),Cerr_i(0,1),Cerr_i(0,2),Cerr_i(0,3)
    write(*,*) 'CalcDgx C0 = ', C_0(0,0,0,0),C_i(0,0,0,1),C_i(0,0,0,2),C_i(0,0,0,3)
#endif

!    maxZadj = maxval(abs(Zadj))
!    maxZadj2f = maxval(abs(f(inds2(1,:))*Zadj2(:)))

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
    truncfacexp = sqrt(fac_g) * truncfacD
    gtrunc = ordgx_max 

! calculate D(1,n1,n2,n3) up to rank r
! calculate D(0,n1,n2,n3) up to rank r-1
    rloop: do r=1,rmaxExp

#ifdef Dgxtest
!      write(*,*) 'CalcDgx rloop',r,rmax,gtrunc
#endif

      if (r.gt.rmax+gtrunc+1) exit rloop

#ifdef Dgxtest
      write(*,*) 'CalcDgx rloop',r
#endif

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating D_00ijk.. exploiting eq. (5.53)
      maxDexpgx(1,r,0)=0d0
      do nl=r-2,0,-1
        do nlt=r-2-nl,0,-1
          nltt = r-2-nl-nlt
          inds0(l) = nl
          inds0(lt) = nlt
          inds0(ltt) = nltt

          inds(l) = nl+1
          inds(lt) = nlt
          inds(ltt) = nltt
          Daux = Zadj2f(i,j,1)*Shat(0,inds(1),inds(2),inds(3),1)  &
               + Zadj2f(i,j,2)*Shat(0,inds(1),inds(2),inds(3),2)  &
               + Zadj2f(i,j,3)*Shat(0,inds(1),inds(2),inds(3),3)  

          inds = inds0
          inds(l) = inds(l)+1
          Daux = Daux - Zadj(i,j)*(C_0(0,inds(1),inds(2),inds(3))      &
                             +4*DuvExpgx(1,inds(1),inds(2),inds(3))) 

          if (nlt.ge.1) then
            inds(lt) = nlt-1
            Daux = Daux - 2*nlt*Zadj2f(i,j,lt)*Dexpgx(1,inds(1),inds(2),inds(3),0)
          end if
          if (nltt.ge.1) then
            inds(lt) = nlt
            inds(ltt) = nltt-1
            Daux = Daux - 2*nltt*Zadj2f(i,j,ltt)*Dexpgx(1,inds(1),inds(2),inds(3),0)
          end if

          Dexpgx(1,inds0(1),inds0(2),inds0(3),0) = Daux/(2*(nl+1)*Zadj2f(i,j,l))

          maxDexpgx(1,r,0) =  maxDexpgx(1,r,0) + abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),0) )

          if (r.le.rmax) then
            D(1,inds0(1),inds0(2),inds0(3)) = Dexpgx(1,inds0(1),inds0(2),inds0(3),0)
          end if
      
        end do
      end do

      ! calculate
      ! D_00ijkl.. --> D_aijkl..
      ! exploiting eq. (5.38)
      maxDexpgx(0,r-1,0)=0d0
      do n1=0,r-1
        do n2=0,r-1-n1
          n3 = r-1-n1-n2

          Smod = Shat(0,n1,n2,n3,:)
          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2d0*n1*Dexpgx(1,n1-1,n2,n3,0)
          end if
          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2d0*n2*Dexpgx(1,n1,n2-1,n3,0)
          end if
          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2d0*n3*Dexpgx(1,n1,n2,n3-1,0)
          end if

          Dexpgx(0,n1,n2,n3,0) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                              +  Zadj(3,j)*Smod(3))/Zadjfj
          maxDexpgx(0,r-1,0) =  maxDexpgx(0,r-1,0) + abs(Dexpgx(0,n1,n2,n3,0))
          if (r.le.rmax+1) then
            D(0,n1,n2,n3) = Dexpgx(0,n1,n2,n3,0)
          end if


#ifdef Dgxtest
!          if(n0.eq.0.and.n1.eq.0.and.n2.eq.3.and.n3.eq.0) then 
!            write(*,*) 'D2(0,0,3,0)= ',0,D(n0,n1,n2,n3)
!          end if
#endif

        end do
      end do

#ifdef Dgxtest
!      write(*,*) 'CalcDgx maxDexpg 0',r-1, maxDexpg(0,r-1,0)
#endif

      if(r-1.le.rmax) then
!       Derr(r-1) =  abs(detZ/Zadjfj)*maxDexpg(0,r-1,0)
        Derr(r-1) =  fac_g*maxDexpgx(0,r-1,0)
      endif

      ! error propagation from C's
      if(r.gt.1)then
        D00_err(r) = max(Cij_err(r-1),maxZadj/maxZadj2f*Cij_err(r-1))/2d0
      end if
      Dij_err(r-1)=maxZadj*max(Cij_err(r-1),2*D00_err(r))/abs(Zadjfj)

      if(r.gt.1)then
        D00_err2(r) = max(Cij_err2(r-1),maxZadj/maxZadj2f*Cij_err2(r-1))/2d0
      end if
      Dij_err2(r-1)=maxZadj*max(Cij_err2(r-1),2*D00_err2(r))/abs(Zadjfj)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rg = r
      gloop: do g=1,min(gtrunc,r-1)
        rg = rg-1

        write(*,*) 'gloop ',g,rg

        ! calculating D_00ijk.. exploiting eq. (5.53)
        maxDexpgx(1,rg,g) = 0d0
        do nl=rg-2,0,-1
          do nlt=rg-2-nl,0,-1
            nltt = rg-2-nl-nlt
            inds0(l) = nl
            inds0(lt) = nlt
            inds0(ltt) = nltt

            inds = inds0
            inds(l) = inds(l)+1
            Daux = -Xadj(i,j)*Dexpgx(0,inds(1),inds(2),inds(3),g-1)   &
                   +Zadj(i,j)*2*rg*Dexpgx(1,inds(1),inds(2),inds(3),g-1) 

            write(*,*) 'CalcDgx con Xij',-Xadj(i,j)*Dexpgx(0,inds(1),inds(2),inds(3),g-1)/(2*(nl+1)*Zadj2f(i,j,l))  
            write(*,*) 'CalcDgx con Zij',+Zadj(i,j)*2*(1+rg)*Dexpgx(1,inds(1),inds(2),inds(3),g-1)/(2*(nl+1)*Zadj2f(i,j,l))

            inds(i) = inds(i)+1
            Daux = Daux - Zadjfj*Dexpgx(0,inds(1),inds(2),inds(3),g-1)
            write(*,*) 'CalcDgx con Zadj2f', - Zadjfj*Dexpgx(0,inds(1),inds(2),inds(3),g-1)/(2*(nl+1)*Zadj2f(i,j,l))  

            if (nlt.ge.1) then
              inds(l) = nl+1
              inds(lt) = nlt-1
              inds(ltt) = nltt
              Daux = Daux - 2*nlt*Zadj2f(i,j,lt)*Dexpgx(1,inds(1),inds(2),inds(3),g)
            end if
            if (nltt.ge.1) then
              inds(l) = nl+1
              inds(lt) = nlt
              inds(ltt) = nltt-1
              Daux = Daux - 2*nltt*Zadj2f(i,j,ltt)*Dexpgx(1,inds(1),inds(2),inds(3),g)
            end if

            Dexpgx(1,inds0(1),inds0(2),inds0(3),g) = Daux/(2*(nl+1)*Zadj2f(i,j,l))

            maxDexpgx(1,rg,g) =  maxDexpgx(1,rg,g) + abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),g) )
 
            write(*,*) 'CalcDgx gloop 00',g,rg,nl,nlt,nltt,Dexpgx(1,inds0(1),inds0(2),inds0(3),g)


            if (g.eq.1.and.abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),g)).gt.           &
                truncfacexp*max(1/m2scale,maxDexpgx(1,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),g)).gt.           &
                truncfacexp*maxDexpgx(1,rg,g-1)) then

#ifdef Dgxtest
              write(*,*) 'CalcDgx cycle loop',1,inds0(1),inds0(2),inds0(3),g, &
                  abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),g)),abs(Dexpgx(1,inds0(1),inds0(2),inds0(3),g-1)),maxDexpgx(1,rg,g-1)
#endif
              
              gtrunc = g-1
              exit gloop
            end if
       
          end do
        end do

#ifndef PPEXP00
        if (rg.le.rmax) then
          do n1=0,rg-2
            do n2=0,rg-2-n1
              n3=rg-2-n1-n2
              D(1,n1,n2,n3) = D(1,n1,n2,n3) + Dexpgx(1,n1,n2,n3,g)
            end do
          end do
        end if
#endif


        ! calculate
        ! D_00ijkl.. --> D_aijkl..
        ! exploiting eq. (5.38)

!        write(*,*) 'CalcDgx maxDexp',rg-1,g-1,maxDexpg(0,rg-1,g-1)

        maxDexpgx(0,rg-1,g) = 0d0
        do n1=0,rg-1
          do n2=0,rg-1-n1
            n3 = rg-1-n1-n2

            Smod = 0d0
            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2d0*n1*Dexpgx(1,n1-1,n2,n3,g)
            end if
            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2d0*n2*Dexpgx(1,n1,n2-1,n3,g)
            end if
            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2d0*n3*Dexpgx(1,n1,n2,n3-1,g)
            end if

            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
            inds(j) = inds(j)+1
            Dexpgx(0,n1,n2,n3,g) = (Zadj(1,j)*Smod(1) +  Zadj(2,j)*Smod(2)  &
                                +  Zadj(3,j)*Smod(3)  &
                                - detZ*Dexpgx(0,inds(1),inds(2),inds(3),g-1))/Zadjfj

            maxDexpgx(0,rg-1,g) =  maxDexpgx(0,rg-1,g) + abs(Dexpgx(0,n1,n2,n3,g))

!              if(n1.eq.0.and.n2.eq.1.and.n3.eq.2) then 
!                write(*,*) 'D2(2,3,3)= ',g,Dexpg(0,n1,n2,n3,g)
!                write(*,*) 'D2(2,3,3)= ',Zadj(1,j)*Smod(1)/Zadjfj,  Zadj(2,j)*Smod(2)/Zadjfj,  &
!                                +  Zadj(3,j)*Smod(3)/Zadjfj,  &
!                                - detZ*Dexpg(0,inds(1),inds(2),inds(3),g-1)/Zadjfj
!                write(*,*) 'D2(2,3,3)= ',inds(1),inds(2),inds(3),         &
!                                - detZ/Zadjfj,Dexpg(0,inds(1),inds(2),inds(3),g-1)
!              end if

            if (g.eq.1.and.abs(Dexpgx(0,n1,n2,n3,g)).gt.               &
                truncfacexp*max(1/m2scale**2,maxDexpgx(0,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgx(0,n1,n2,n3,g)).gt.              &
                truncfacexp*maxDexpgx(0,rg,g-1)) then

#ifdef Dgxtest
                  write(*,*) 'CalcDgx exit gloop',0,n1,n2,n3,g,abs(Dexpgx(0,n1,n2,n3,g)),maxDexpgx(0,rg-1,g-1),truncfacexp
#endif      
              gtrunc = g-1
              exit gloop
            end if

          end do
        end do

        ! error propagation from C's
        if(rg.gt.1)then
          D00_err(rg) = max( D00_err(rg),                                   &
              max( abs(m02)*Dij_err(rg-2),            &
              max( maxZadjf*Dij_err(rg),abs(Xtilde)*Dij_err(rg-1),         &
              maxZadj*D00_err(rg+1) ) / abs(2d0*maxZadj2f) )                        &
              /(4*(rg-1))   )
        end if
        Dij_err(rg-1)=max(Dij_err(rg-1),                &
            max(2*maxZadj*D00_err(rg),abs(detZ)*Dij_err(rg))/abs(Zadjfj) )

        if(rg.gt.1)then
          D00_err2(rg) = max( D00_err2(rg),                                   &
              max( abs(m02)*Dij_err2(rg-2),            &
              max( maxZadjf*Dij_err2(rg),abs(Xtilde)*Dij_err2(rg-1),         &
              maxZadj*D00_err(rg+1) ) / abs(2d0*maxZadj2f) )                        &
              /(4*(rg-1))   )
        end if
        Dij_err2(rg-1)=max(Dij_err2(rg-1),                &
            max(2*maxZadj*D00_err2(rg),abs(detZ)*Dij_err2(rg))/abs(Zadjfj) )

#ifdef PPEXP00
        if (rg.le.rmax) then
          do n1=0,rg-1
            do n2=0,rg-1-n1
              n3=rg-1-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgx(0,n1,n2,n3,g)
            end do
          end do
        end if
#endif
!        write(*,*) 'CalcDgx after it1 ',rg
        if ((rg.le.rmax+1)) then
          Derr(rg-1) = 0d0
          do n1=0,rg-1
            do n2=0,rg-1-n1
              n3 = rg-1-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgx(0,n1,n2,n3,g)
              if(abs(Dexpgx(0,n1,n2,n3,g-1)).ne.0d0) then
!              Derr(rg-1)=max(Derr(rg-1),abs(Dexpgx(0,n1,n2,n3,g))**2/abs(Dexpgx(0,n1,n2,n3,g-1)))
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpgx(0,n1,n2,n3,g))*min(1d0,abs(Dexpgx(0,n1,n2,n3,g))/abs(Dexpgx(0,n1,n2,n3,g-1))))
              else
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpgx(0,n1,n2,n3,g)))
              endif

#ifdef Dgxtest
!              write(*,*) 'CalcDgx Derr calc',rg-1,Derr(rg-1),n1,n2,n3,abs(Dexpg(0,n1,n2,n3,g)),abs(Dexpg(0,n1,n2,n3,g-1))
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
#ifdef PVEST2
          if(Dij_err2(rg-1).gt.3d0*Derr(rg-1)) then
#else
          if(Dij_err(rg-1).gt.3d0*Derr(rg-1)) then
#endif
            gtrunc = min(g,gtrunc)
             
#ifdef Dgxtest
             write(*,*) 'CalcDgx exit err',r,rg-1,g,gtrunc,Dij_err(rg-1),Derr(rg-1)
#endif

          end if

        end if

      end do gloop

#ifdef Dgxtest
      write(*,*) 'CalcDgx D(0,0,0,0) = ',r,D(0,0,0,0)
    if(r.gt.1)then
      write(*,*) 'CalcDgx D(1,0,0,0) = ',r,D(1,0,0,0)
      write(*,*) 'CalcDgx D(0,1,0,0) = ',r,D(0,1,0,0)
      write(*,*) 'CalcDgx D(0,0,1,0) = ',r,D(0,0,1,0)
    endif
    if(r.gt.2.and.rmax.ge.2)then
      write(*,*) 'CalcDgx D(1,1,0,0) = ',r,D(1,1,0,0)
!      write(*,*) 'CalcDgx D(0,2,0,0) = ',r,D(0,2,0,0)
!      write(*,*) 'CalcDgx D(0,1,1,0) = ',r,D(0,1,1,0)
      write(*,*) 'CalcDgx D(0,0,2,0) = ',r,D(0,0,2,0)
    endif
    if(r.gt.3.and.rmax.ge.2)then
      write(*,*) 'CalcDgx D(1,0,1,0) = ',r,D(1,0,1,0)
      write(*,*) 'CalcDgx D(1,1,0,0) = ',r,D(1,1,0,0)
!      write(*,*) 'CalcDgx D(1,2,0,0) = ',r,D(1,2,0,0)
      write(*,*) 'CalcDgx D(0,3,0,0) = ',r,D(0,3,0,0)
      write(*,*) 'CalcDgx D(0,2,1,0) = ',r,D(0,2,1,0)
      write(*,*) 'CalcDgx D(0,0,3,0) = ',r,D(0,0,3,0)
      write(*,*) 'CalcDgx D(0,1,1,1) = ',r,D(0,1,1,1)
      write(*,*) 'CalcDgx D(0,0,2,1) = ',r,D(0,0,2,1)
    endif
      write(*,*) 'CalcDgx Dij_err',r,Dij_err
      write(*,*) 'CalcDgx Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgx err',r,Derr
      write(*,*) 'CalcDgx acc',r,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

#ifdef Dgxtest
!      write(*,*) 'CalcDgx exit r',r,maxval(Derr),acc_req_D*abs(D(0,0,0,0))
#endif

!      if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit    ! changed 28.01.15
      ! check if target precision already reached
! NEEDS UPDATE
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then
#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif
        do rg=r+1,rmax
        do n0=0,rg/2
        do n1=0,rg-2*n0
        do n2=0,rg-2*n0-n1
          D(n0,n1,n2,rg-2*n0-n1-n2)=0d0
        enddo
        enddo
        enddo
        enddo

        exit rloop

      end if

    end do rloop


#ifdef Dgxtest
!    write(*,*) 'CalcDgx D(0,0,0,0) = ',D(0,0,0,0)
!    if(rmax.ge.3)then
!      write(*,*) 'CalcDgx D(0,1,1,1) = ',D(0,1,1,1)
!    endif

    write(*,*) 'CalcDgx final err',Derr
    write(*,*) 'CalcDgx final acc',Derr/abs(D(0,0,0,0))
#endif      
       
!    write(*,*) 'CalcDgx Derr ',Derr
!    write(*,*) 'CalcDgx Derr2',Derr2
                    
  end subroutine CalcDgx










  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgy(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordgy_min,ordgy_max,id,Derr,Derr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgy(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgy_min,ordgy_max,id,Derr,Derr2)  
  
    use globalD

    integer, intent(in) :: rmax,ordgy_min,ordgy_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex ::Zadj2(4)
    double complex, allocatable :: Dexpgy(:,:,:,:,:), DuvExpgy(:,:,:,:)
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: C_0(:,:,:,:), C_i(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: Cuv_0(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3), Daux, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpgy(0:1,0:rmax+2*ordgy_min,0:ordgy_max),truncfacexp,acc_aux
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,a,b,i,g,rg,m,n,rtrunc
    integer :: inds0(3),inds(3),inds2(2,4),at,bt,k,l,lt,ltt,nl,nlt,nltt
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgytest
    write(*,*) 'CalcDgy in, ord',rmax,ordgy_min,ordgy_max
#endif

    ! allocation of C functions
    rmaxC = rmax + 2*ordgy_min + 1
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax+1) = acc_req_CinD
    acc_aux = acc_req_C
    if (y_gy.ne.0d0) then
      do g=1,ordgy_min
        acc_req_Cextra(rmax+2*g) = acc_req_Cextra(rmax+2*g-2)/y_gy
        acc_req_Cextra(rmax+2*g+1) = acc_req_Cextra(rmax+2*g-1)/y_gy
        acc_aux = acc_aux/max(x_gy,v_gy*y_gy)
        acc_req_Cextra(rmax+g+1) = min(acc_req_Cextra(rmax+g+1),acc_aux)
      end do
    else if(x_gy.ne.0d0) then ! 10.07.2017
      do g=1,ordgy_min
        acc_aux = acc_aux/x_gy
        acc_req_Cextra(rmax+g+1) = acc_aux
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+2:rmax+2*ordgy_min+1) = acc_inf
    end if
   


#ifdef Dgytest
    write(*,*) 'CalcDgy: accreq_Cextra',acc_req_Cextra
#endif

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

#ifdef Dgytest
    write(*,*) 'CalcDgy Cerr 0',Cerr_i(:,0)
    write(*,*) 'CalcDgy Cerr 1',Cerr_i(:,1)
    write(*,*) 'CalcDgy Cerr 2',Cerr_i(:,2)
    write(*,*) 'CalcDgy Cerr 3',Cerr_i(:,3)
#endif


    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do
      

    ! calculate adjugated Gram and Cayley matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)
!
!    Z(1,1) = 2d0*q10
!    Z(2,1) = q10+q20-q21
!    Z(3,1) = q10+q30-q31
!    Z(1,2) = Z(2,1)
!    Z(2,2) = 2d0*q20
!    Z(3,2) = q20+q30-q32
!    Z(1,3) = Z(3,1)
!    Z(2,3) = Z(3,2)
!    Z(3,3) = 2d0*q30
!
!    q1q2 = (q10+q20-q21)
!    q1q3 = (q10+q30-q31)
!    q2q3 = (q20+q30-q32)
!    detZ = 8d0*q10*q30*q20+2D0*q1q2*q1q3*q2q3  &
!     &    -2d0*(q10*q2q3*q2q3+q20*q1q3*q1q3+q30*q1q2*q1q2)
!
!    Zadj(1,1) = (4d0*q30*q20-q2q3*q2q3)
!    Zadj(2,1) = (q1q3*q2q3-2d0*q30*q1q2)
!    Zadj(3,1) = (q1q2*q2q3-2d0*q20*q1q3)
!    Zadj(1,2) = Zadj(2,1)
!    Zadj(2,2) = (4d0*q10*q30-q1q3*q1q3)
!    Zadj(3,2) = (q1q2*q1q3-2d0*q10*q2q3)
!    Zadj(1,3) = Zadj(3,1)
!    Zadj(2,3) = Zadj(3,2)
!    Zadj(3,3) = (4d0*q10*q20-q1q2*q1q2)
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32
!      
!    Zadjf(1) = Zadj(1,1)*f(1)+Zadj(2,1)*f(2)+Zadj(3,1)*f(3)
!    Zadjf(2) = Zadj(1,2)*f(1)+Zadj(2,2)*f(2)+Zadj(3,2)*f(3)
!    Zadjf(3) = Zadj(1,3)*f(1)+Zadj(2,3)*f(2)+Zadj(3,3)*f(3)

!    Xadj(1,1) = 2d0*mm02*Zadj(1,1) - f(2)*f(2)*Z(3,3)  &
!              + 2d0*f(2)*f(3)*Z(2,3) - f(3)*f(3)*Z(2,2)
!    Xadj(2,1) = 2d0*mm02*Zadj(2,1) + f(1)*f(2)*Z(3,3)  &
!              - f(1)*f(3)*Z(2,3) - f(2)*f(3)*Z(1,3) + f(3)*f(3)*Z(2,1)
!    Xadj(3,1) = 2d0*mm02*Zadj(3,1) - f(1)*f(2)*Z(3,2)  &
!              + f(2)*f(2)*Z(3,1) + f(1)*f(3)*Z(2,2) - f(2)*f(3)*Z(1,2)
!    Xadj(1,2) = Xadj(2,1)
!    Xadj(2,2) = 2d0*mm02*Zadj(2,2) - f(1)*f(1)*Z(3,3)  &
!              + 2d0*f(1)*f(3)*Z(1,3) - f(3)*f(3)*Z(1,1)
!    Xadj(3,2) = 2d0*mm02*Zadj(3,2) + f(1)*f(1)*Z(3,2)  &
!              - f(1)*f(2)*Z(3,1) - f(1)*f(3)*Z(2,1) + f(2)*f(3)*Z(1,1)
!    Xadj(1,3) = Xadj(3,1)
!    Xadj(2,3) = Xadj(3,2)
!    Xadj(3,3) = 2d0*mm02*Zadj(3,3) - f(1)*f(1)*Z(2,2)  &
!              + 2d0*f(1)*f(2)*Z(2,1) - f(2)*f(2)*Z(1,1)


    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)
            end if

            if(n3.eq.0) then
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)
            end if

#ifdef Dgytest
          if(n0.eq.0.and.n1.eq.0.and.n2.eq.0.and.n3.eq.1)then
            write(*,*) 'CalcDgy 0 C_0',C_0(n0,n1,n2,n3)
            write(*,*) 'CalcDgy 0 C_1',C_i(n0,n2,n3,1)
            write(*,*) 'CalcDgy 0 C_2',C_i(n0,n1,n3,2)
            write(*,*) 'CalcDgy 0 C_3',C_i(n0,n1,n2,3)
            write(*,*) 'CalcDgy 0 Sh1',Shat(n0,n1,n2,n3,1)
            write(*,*) 'CalcDgy 0 Sh2',Shat(n0,n1,n2,n3,2)
            write(*,*) 'CalcDgy 0 Sh3',Shat(n0,n1,n2,n3,3)
          endif
#endif

          end do
        end do
      end do
    end do

    ! choose reduction formulas with biggest denominators
    maxXadj = 0d0
    if (abs(Xadj(1,1)).gt.maxXadj) then
      maxXadj = abs(Xadj(1,1))
      a = 1
      b = 1
      inds2 = reshape((/2,2,2,3,3,2,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,2)
      Zadj2(3) = Z(2,3)
      Zadj2(4) = -Z(2,2)
    end if
    if (abs(Xadj(2,2)).gt.maxXadj) then
      maxXadj = abs(Xadj(2,2))
      a = 2
      b = 2
      inds2 = reshape((/1,1,1,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = -Z(3,3)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(1,3)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Xadj(3,3)).gt.maxXadj) then
      maxXadj = abs(Xadj(3,3))
      a = 3
      b = 3
      inds2 = reshape((/1,1,1,2,2,1,2,2/),shape(inds2))
      Zadj2(1) = -Z(2,2)
      Zadj2(2) = Z(2,1)
      Zadj2(3) = Z(1,2)
      Zadj2(4) = -Z(1,1)
    end if
    if (abs(Xadj(1,2)).gt.maxXadj) then
      maxXadj = abs(Xadj(1,2))
      a = 1
      b = 2
      inds2 = reshape((/2,1,2,3,3,1,3,3/),shape(inds2))
      Zadj2(1) = Z(3,3)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(2,3)
      Zadj2(4) = Z(2,1)
    end if
    if (abs(Xadj(1,3)).gt.maxXadj) then
      maxXadj = abs(Xadj(1,3))
      a = 1
      b = 3
      inds2 = reshape((/2,1,2,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = -Z(3,2)
      Zadj2(2) = Z(3,1)
      Zadj2(3) = Z(2,2)
      Zadj2(4) = -Z(2,1)
    end if
    if (abs(Xadj(2,3)).gt.maxXadj) then
      a = 2
      b = 3
      inds2 = reshape((/1,1,1,2,3,1,3,2/),shape(inds2))
      Zadj2(1) = Z(3,2)
      Zadj2(2) = -Z(3,1)
      Zadj2(3) = -Z(1,2)
      Zadj2(4) = Z(1,1)
    end if

    maxZadj = 0d0
    if (abs(Zadj(1,1)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,1))
      k = 1
      l = 1
      lt = 2
      ltt = 3
    end if
    if (abs(Zadj(2,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(2,2))
      k = 2
      l = 2
      lt = 1
      ltt = 3
    end if
    if (abs(Zadj(3,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(3,3))
      k = 3
      l = 3
      lt = 1
      ltt = 2
    end if
    if (abs(Zadj(1,2)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,2))
      k = 1
      l = 2
      lt = 1
      ltt = 3
    end if
    if (abs(Zadj(1,3)).gt.maxZadj) then
      maxZadj = abs(Zadj(1,3))
      k = 1
      l = 3
      lt = 1
      ltt = 2
    end if
    if (abs(Zadj(2,3)).gt.maxZadj) then
      k = 2
      l = 3
      lt = 1
      ltt = 2
    end if

#ifdef Dgytest
    write(*,*) 'CalcDgy: Zadj',k,l,Zadj(k,l)
    write(*,*) 'CalcDgy: Xadj',a,b,Xadj(a,b)
#endif

        
    ! allocation of array for det(Z)- and det(X)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpgy(0:max(rmax/2,1),0:rmaxExp-2,0:rmaxExp-2,0:rmaxExp-2,0:ordgy_max))
   

    ! calculate Cuv
    allocate(DuvExpgy(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpgy,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgy(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))

    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

!    maxZadj = maxval(abs(Zadj))
!    maxZadj2f = maxval(abs(f(inds2(1,:))*Zadj2(:)))
!    maxZadjf = maxval(abs(Zadjf))
!    adetZ = abs(detZ)

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
!    truncfacexp = sqrt(max(maxZadjf,adetZ)/maxXadj*max(1d0,maxZadj2f/maxZadj)) * truncfacD
    truncfacexp = sqrt(fac_gy) * truncfacD
    gtrunc = ordgy_max 
    rtrunc = -1

! calculate D(1,n1,n2,n3) up to rank r+2
! calculate D(0,n1,n2,n3) up to rank r  
    rloop: do r=0,rmaxExp-2

!      write(*,*) 'rloop start',r,gtrunc,rmax+2*gtrunc+2

      if (r.gt.rmax+2*gtrunc+2) exit rloop

!      write(*,*) 'rloop start',r

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating D_00ijk.. exploiting eq. (5.49)
      maxDexpgy(1,r,0)=0d0
      do nl=r,0,-1
        do nlt=r-nl,0,-1
          nltt = r-nl-nlt
          inds0(l) = nl
          inds0(lt) = nlt
          inds0(ltt) = nltt

          inds(l) = nl+1
          inds(lt) = nlt
          inds(ltt) = nltt

          Daux = Zadj(k,1)*Shat(0,inds(1),inds(2),inds(3),1)  &
               + Zadj(k,2)*Shat(0,inds(1),inds(2),inds(3),2)  &
               + Zadj(k,3)*Shat(0,inds(1),inds(2),inds(3),3)

          if (nlt.ge.1) then
            inds(lt) = nlt-1
            Daux = Daux - 2*nlt*Zadj(k,lt)*Dexpgy(1,inds(1),inds(2),inds(3),0)
          end if

          if (nltt.ge.1) then
            inds(lt) = nlt
            inds(ltt) = nltt-1
            Daux = Daux - 2*nltt*Zadj(k,ltt)*Dexpgy(1,inds(1),inds(2),inds(3),0)
          end if

          Dexpgy(1,inds0(1),inds0(2),inds0(3),0) = Daux/(2*(nl+1)*Zadj(k,l))

          maxDexpgy(1,r,0) =  maxDexpgy(1,r,0) + abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),0) )

!          if (r+2.le.rmax) then          !  for fixed rank
          if (r+1.le.rmax) then
            D(1,inds0(1),inds0(2),inds0(3)) = Dexpgy(1,inds0(1),inds0(2),inds0(3),0)
          end if
      


        end do
      end do

      ! calculate D_ijkl.. exploiting eq. (5.53)
      maxDexpgy(0,r,0)=0d0
      do n1=0,r
        do n2=0,r-n1
          n3 = r-n1-n2

! Duv added 16.05.14      
!          Daux = (2d0*(1+r)*Dexpgy(1,n1,n2,n3,0) - C_0(0,n1,n2,n3))*Zadj(a,b)
          Daux = (2d0*(1+r)*Dexpgy(1,n1,n2,n3,0)  - 4*DuvExpgy(1,n1,n2,n3)  &
            - C_0(0,n1,n2,n3))*Zadj(a,b)

          Smod = Shat(0,n1,n2,n3,:)

#ifdef Dgytest
          if(n1.eq.0.and.n2.eq.2.and.n3.eq.0)then
            write(*,*) 'CalcDgy 0 Smod',Smod
            write(*,*) 'CalcDgy 0 Daux',Daux
          endif
#endif

          if (n1.ge.1) then
            Smod(1) = Smod(1) - 2d0*n1*Dexpgy(1,n1-1,n2,n3,0)
          end if
          if (n2.ge.1) then
            Smod(2) = Smod(2) - 2d0*n2*Dexpgy(1,n1,n2-1,n3,0)
          end if
          if (n3.ge.1) then
            Smod(3) = Smod(3) - 2d0*n3*Dexpgy(1,n1,n2,n3-1,0)
          end if

#ifdef Dgytest
          if(n1.eq.0.and.n2.eq.2.and.n3.eq.0)then
            write(*,*) 'CalcDgy 0',r,a,b,Zadjf(b)/Xadj(a,b)
            write(*,*) 'CalcDgy 0',k,l,detZ/Zadj(k,l),Zadjf(k)/Zadj(k,l)
            write(*,*) 'CalcDgy 0 line1',r,Daux/Xadj(a,b)
          endif
#endif

          do i=1,4
            n = inds2(1,i)
            m = inds2(2,i)
            Daux = Daux + Zadj2(i)*f(n)*Smod(m)

#ifdef Dgytest
           if(n1.eq.0.and.n2.eq.2.and.n3.eq.0)then
             write(*,*) 'CalcDgy 0 2f',r,i,Zadj2(i)*f(n)*Smod(m)/Xadj(a,b)
           endif
#endif

          end do

          Dexpgy(0,n1,n2,n3,0) = Daux/Xadj(a,b)

#ifdef Dgytest
          if(n1.eq.1.and.n2.eq.1.and.n3.eq.1)then
            write(*,*) 'CalcDgy D_0',r,Dexpgy(0,n1,n2,n3,0) 
          endif
#endif

          maxDexpgy(0,r,0) =  maxDexpgy(0,r,0) + abs(Dexpgy(0,n1,n2,n3,0))
          if (r.le.rmax) then
            D(0,n1,n2,n3) = Dexpgy(0,n1,n2,n3,0)
!            Derr(r) =  abs(maxZadjf/maxXadj*Dexpgy(0,n1,n2,n3,0))
          end if
      
        end do
      end do

      if (r.le.rmax) then
!       Derr(r) =  abs(maxZadjf/Xadj(a,b))*maxDexpgy(0,r,0)
        Derr(r) =  fac_gy*maxDexpgy(0,r,0)
      endif

!      write(*,*) 'CalcDgy r',r,0

      ! error propagation from C's
      D00_err(r+2) = Cij_err(r+1)/2d0 
      Dij_err(r)=max(maxZadj/maxXadj*max(2*(r+1)*D00_err(r+2),Cerr_i(r,0)), &
                 maxZadj2f/maxXadj*max(2*D00_err(r+1),Cij_err(r)))

      D00_err2(r+2) = Cij_err2(r+1)/2d0 
      Dij_err2(r)=max(maxZadj/maxXadj*max(2*(r+1)*D00_err2(r+2),Cerr2_i(r,0)), &
                 maxZadj2f/maxXadj*max(2*D00_err2(r+1),Cij_err2(r)))
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!      write(*,*) 'before gloop',r,gtrunc,min(gtrunc,r/2)

      rg = r
      gloop: do g=1,min(gtrunc,r/2)
        rg = rg-2

! changed 7.08.2023 to avoid NaN => used in Dexpgy(1,n1,n2,n3-1,g) below for r -> r+1
!        if(rg.le.rtrunc) exit gloop
        if(rg.le.rtrunc) then
           do nl=rg,0,-1
              do nlt=rg-nl,0,-1
                 nltt = rg-nl-nlt
                 Dexpgy(1,nl,nlt,nltt,g) = 0d0
              enddo
           enddo
          exit gloop
        endif

!        write(*,*) 'gloop start',r,rg,g

        ! calculating D_00ijk.. exploiting eq. (5.49)
        maxDexpgy(1,rg,g) = 0d0
        do nl=rg,0,-1
          do nlt=rg-nl,0,-1
            nltt = rg-nl-nlt
            inds0(l) = nl
            inds0(lt) = nlt
            inds0(ltt) = nltt

            inds = inds0
            inds(l) = inds(l)+1
            Daux = -Zadjf(k)*Dexpgy(0,inds(1),inds(2),inds(3),g-1)

            inds(k) = inds(k)+1
            Daux = Daux - detZ*Dexpgy(0,inds(1),inds(2),inds(3),g-1)

            if (nlt.ge.1) then
              inds(l) = nl+1
              inds(lt) = nlt-1
              inds(ltt) = nltt
              Daux = Daux - 2*nlt*Zadj(k,lt)*Dexpgy(1,inds(1),inds(2),inds(3),g)
            end if
            if (nltt.ge.1) then
              inds(l) = nl+1
              inds(lt) = nlt
              inds(ltt) = nltt-1
              Daux = Daux - 2*nltt*Zadj(k,ltt)*Dexpgy(1,inds(1),inds(2),inds(3),g)
            end if

            Dexpgy(1,inds0(1),inds0(2),inds0(3),g) = Daux/(2*(nl+1)*Zadj(k,l))

            maxDexpgy(1,rg,g) =  maxDexpgy(1,rg,g) + abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),g) )
!            if (rg+2.le.rmax) then
!              D(1,inds0(1),inds0(2),inds0(3)) = D(1,inds0(1),inds0(2),inds0(3))  &
!                                              + Dexpgy(1,inds0(1),inds0(2),inds0(3),g)
!            end if
 
            
!  10.08.2017  factor 1d1 added for g=1 since first terms can cancel for certain cases
            if (g.eq.1.and.abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),g)).gt.   &
                1d1*truncfacexp*max(1/m2scale,maxDexpgy(1,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),g)).gt.   &
                truncfacexp*maxDexpgy(1,rg,g-1)) then

#ifdef Dgytest
                write(*,*) 'CalcDgy exit gloop',1,inds0(1),inds0(2),inds0(3),g, &
                abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),g)),abs(Dexpgy(1,inds0(1),inds0(2),inds0(3),g-1)),maxDexpgy(1,rg,g-1)
#endif

                gtrunc = g-1
                exit gloop
!               gtrunc = g
!               cycle gloop               ! worsens results !?
              end if
       
          end do
        end do

#ifndef PPEXP00
!          if (rg+2.le.rmax) then        ! for fixed rank
        if (rg+1.le.rmax) then
          do n1=0,rg
            do n2=0,rg-n1
              n3=rg-n1-n2
              D(1,n1,n2,n3) = D(1,n1,n2,n3) + Dexpgy(1,n1,n2,n3,g)
            end do
          end do
        end if
#endif

        ! calculate D_ijkl.. exploiting eq. (5.53)
        maxDexpgy(0,rg,g) = 0d0
        do n1=0,rg
          do n2=0,rg-n1
            n3 = rg-n1-n2
      
            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
            inds(a) = inds(a)+1
            Daux = 2*(1+rg)*Dexpgy(1,n1,n2,n3,g)*Zadj(a,b)  &
                 - Zadjf(b)*Dexpgy(0,inds(1),inds(2),inds(3),g-1)

            Smod = 0d0
            if (n1.ge.1) then
              Smod(1) = Smod(1) - 2d0*n1*Dexpgy(1,n1-1,n2,n3,g)
            end if
            if (n2.ge.1) then
              Smod(2) = Smod(2) - 2d0*n2*Dexpgy(1,n1,n2-1,n3,g)
            end if
            if (n3.ge.1) then
              Smod(3) = Smod(3) - 2d0*n3*Dexpgy(1,n1,n2,n3-1,g)
            end if

            do i=1,4
              n = inds2(1,i)
              m = inds2(2,i)
              Daux = Daux + Zadj2(i)*f(n)*Smod(m)
            end do

            Dexpgy(0,n1,n2,n3,g) = Daux/Xadj(a,b)

            maxDexpgy(0,rg,g) =  maxDexpgy(0,rg,g) + abs(Dexpgy(0,n1,n2,n3,g))

!            if (rg.le.rmax) then
!              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgy(0,n1,n2,n3,g)
!            end if

            if (g.eq.1.and.abs(Dexpgy(0,n1,n2,n3,g)).gt.               &
                truncfacexp*max(1/m2scale**2,maxDexpgy(0,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgy(0,n1,n2,n3,g)).gt.              &
                truncfacexp*maxDexpgy(0,rg,g-1)) then

#ifdef Dgytest
            write(*,*) 'CalcDgy cycle loop',n1,n2,n3,g,abs(Dexpgy(0,n1,n2,n3,g)),abs(Dexpgy(0,n1,n2,n3,g-1)),maxDexpgy(0,rg,g-1)
#endif

              gtrunc = g-1
              exit gloop
!             gtrunc = g
!             cycle gloop
            end if

          end do
        end do

!        write(*,*) 'CalcDgy rg',rg,g


        ! error propagation from C's
        if(rg.gt.1)then
          D00_err(rg+2) = max(D00_err(rg+2),                      &
                              maxZadjf/maxZadj/2d0*Dij_err(rg+1),         &
                              abs(detZ)/maxZadj/2d0*Dij_err(rg+2))
        end if
        Dij_err(rg)=max(Dij_err(rg),maxZadjf/maxXadj*Dij_err(rg+1),     &
               2*(rg+1)*maxZadj/maxXadj*D00_err(rg+2),                  &
               2*maxZadj2f/maxXadj*D00_err(rg+1))

        if(rg.gt.1)then
          D00_err2(rg+2) = max(D00_err2(rg+2),                      &
                              maxZadjf/maxZadj/2d0*Dij_err2(rg+1),         &
                              abs(detZ)/maxZadj/2d0*Dij_err2(rg+2))
        end if
        Dij_err2(rg)=max(Dij_err2(rg),maxZadjf/maxXadj*Dij_err2(rg+1),     &
               2*(rg+1)*maxZadj/maxXadj*D00_err2(rg+2),                  &
               2*maxZadj2f/maxXadj*D00_err2(rg+1))

#ifdef PPEXP00
!          if (rg+2.le.rmax) then          !  for fixed rank
        if (rg+2.le.rmax) then
          do n1=0,rg
            do n2=0,rg-n1
              n3=rg-n1-n2
              D(1,n1,n2,n3) = D(1,n1,n2,n3) + Dexpgy(1,n1,n2,n3,g)
            end do
          end do
        end if
#endif

        if ((rg.le.rmax)) then
          Derr(rg) = 0d0
          do n1=0,rg
            do n2=0,rg-n1
              n3 = rg-n1-n2

#ifdef Dgytest
              if(n1.eq.0.and.n2.eq.4.and.n3.eq.0) then
              write(*,*) 'CalcDgy Derr calc',rg,n1,n2,n3,g,D(0,n1,n2,n3),Dexpgy(0,n1,n2,n3,g),Dexpgy(0,n1,n2,n3,g-1),Derr(rg)
              endif
#endif

              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgy(0,n1,n2,n3,g)
              if(abs(Dexpgy(0,n1,n2,n3,g-1)).ne.0d0) then
!             Derr(rg)=max(Derr(rg),abs(Dexpgy(0,n1,n2,n3,g))**2/abs(Dexpgy(0,n1,n2,n3,g-1)))
                Derr(rg)=max(Derr(rg),abs(Dexpgy(0,n1,n2,n3,g))*min(1d0,abs(Dexpgy(0,n1,n2,n3,g))/abs(Dexpgy(0,n1,n2,n3,g-1))))
              else
                Derr(rg)=max(Derr(rg),abs(Dexpgy(0,n1,n2,n3,g)))
              endif

#ifdef Dgytest
              if(n1.eq.0.and.n2.eq.4.and.n3.eq.0) then
              write(*,*) 'CalcDgy Derr calc',rg,Derr(rg),n1,n2,n3,g,abs(Dexpgy(0,n1,n2,n3,g)),abs(Dexpgy(0,n1,n2,n3,g-1))  &
                  ,D(0,n1,n2,n3),Dexpgy(0,n1,n2,n3,g)
              endif
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
          ! allow for one more term, as each step involves only even or odd ranks
#ifdef PVEST2
          if(Dij_err2(rg).gt.3d0*Derr(rg)) then
#else
          if(Dij_err(rg).gt.3d0*Derr(rg)) then
#endif
            rtrunc = rg
!           gtrunc = min(g,gtrunc)    !  replaced by rtrunc 29.07.19
             
#ifdef Dgytest
            write(*,*) 'CalcDgy exit err',rg,rtrunc,g,gtrunc
            write(*,*) 'CalcDgy exit err',Dij_err(rg),Derr(rg)
#endif
          end if

        end if

      end do gloop

!      write(*,*) 'gloop end',r,g

#ifdef Dgytest

      write(*,*) 'CalcDgy D(1,0,0,0)',r,D(1,0,0,0)
      write(*,*) 'CalcDgy D(0,0,0,0)',r,D(0,0,0,0)
      write(*,*) 'CalcDgy D(0,0,0,1)',r,D(0,0,0,1)
      if (r.ge.2.and.rmax.ge.2) then
      write(*,*) 'CalcDgy D(0,2,0,0)',r,D(0,2,0,0)
      write(*,*) 'CalcDgy D(0,0,0,2)',r,D(0,0,0,2)
      endif
      if (r.ge.3.and.rmax.ge.3)then
      write(*,*) 'CalcDgy D(0,3,0,0)',r,D(0,3,0,0)
      write(*,*) 'CalcDgy D(1,0,0,1)',r,D(1,0,0,1)
      write(*,*) 'CalcDgy D(0,1,0,2)',r,D(0,1,0,2)
      write(*,*) 'CalcDgy D(0,0,0,3)',r,D(0,0,0,3)
      write(*,*) 'CalcDgy D(0,1,1,1)',r,D(0,1,1,1)
      write(*,*) 'CalcDgy D(0,2,1,0)',r,D(0,2,1,0)
      endif
      if (r.ge.4.and.rmax.ge.4)then
      write(*,*) 'CalcDgy D(0,4,0,0)',r,D(0,4,0,0)
      write(*,*) 'CalcDgy D(0,0,4,0)',r,D(0,0,4,0)
      endif

      write(*,*) 'CalcDgy Dij_err',r,Dij_err
      write(*,*) 'CalcDgy Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgy err',r,g,Derr
      write(*,*) 'CalcDgy acc',r,g,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

!     if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit     ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then
        if (r.lt.rmax) then
          do rg=r+1,rmax
            do n1=0,rg
              do n2=0,rg-n1
                D(0,n1,n2,rg-n1-n2)=0d0
              end do
            end do
          end do
          do rg=r+1,rmax
            do n1=0,rg-2
              do n2=0,rg-2-n1
                D(1,n1,n2,rg-2-n1-n2)=0d0
              end do
            end do
          end do
          
100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDgy',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDgy:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDgy: p10 = ',p10
            write(nerrout_coli,111)' CalcDgy: p21 = ',p21
            write(nerrout_coli,111)' CalcDgy: p32 = ',p32
            write(nerrout_coli,111)' CalcDgy: p30 = ',p30
            write(nerrout_coli,111)' CalcDgy: p20 = ',p20
            write(nerrout_coli,111)' CalcDgy: p31 = ',p31
            write(nerrout_coli,111)' CalcDgy: m02 = ',m02
            write(nerrout_coli,111)' CalcDgy: m12 = ',m12
            write(nerrout_coli,111)' CalcDgy: m22 = ',m22
            write(nerrout_coli,111)' CalcDgy: m32 = ',m32
          end if
        end if
 
#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif

        exit rloop

      end if

    end do rloop


    ! calculating D_0000ijk.. exploiting eq. (5.49)
!    do r=4,rmax
!!      do n0=2,rmax/2            !   for fixed rank
!      do n0=2,rmax
    do r=4,rmax+1                 !  includes rmax+1  24.01.16
      do n0=2,max(rmax,r/2)       !  includes rmax+1  24.01.16
        do nl=r-2*n0,0,-1
          do nlt=r-2*n0-nl,0,-1
            nltt = r-2*n0-nl-nlt
            inds0(l) = nl
            inds0(lt) = nlt
            inds0(ltt) = nltt

            inds(l) = nl+1
            inds(lt) = nlt
            inds(ltt) = nltt
            Daux = Zadj(k,1)*Shat(n0-1,inds(1),inds(2),inds(3),1)  &
                 + Zadj(k,2)*Shat(n0-1,inds(1),inds(2),inds(3),2)  &
                 + Zadj(k,3)*Shat(n0-1,inds(1),inds(2),inds(3),3)  &
                 - Zadjf(k)*D(n0-1,inds(1),inds(2),inds(3))

            inds(k) = inds(k)+1
            Daux = Daux - detZ*D(n0-1,inds(1),inds(2),inds(3))
            inds(k) = inds(k)-1

            if (nlt.ge.1) then
              inds(lt) = nlt-1
              Daux = Daux - 2*nlt*Zadj(k,lt)*D(n0,inds(1),inds(2),inds(3))
            end if
            if (nltt.ge.1) then
              inds(lt) = nlt
              inds(ltt) = nltt-1
              Daux = Daux - 2*nltt*Zadj(k,ltt)*D(n0,inds(1),inds(2),inds(3))
            end if

            D(n0,inds0(1),inds0(2),inds0(3)) = Daux/(2*(nl+1)*Zadj(k,l))

          end do
        end do
      end do
    end do

    ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
    ! already calculated for rmax+1 with extension of 24.01.16 above
!    do r=rmax+1,2*rmax
#ifdef notneeded
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            write(*,*) 'CalcDgy exp rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

            write(*,*) 'CalcDgy dir rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

          end do
        end do
      end do
    end do
#endif

#ifdef Dgytest
    if(rmax.ge.2) then
      write(*,*) 'CalcDgy D(1,0,0,0) fin',D(1,0,0,0)
      write(*,*) 'CalcDgy D(0,0,2,0) fin',D(0,0,2,0)
      write(*,*) 'CalcDgy D(0,0,0,2) fin',D(0,0,0,2)
      if(rmax.ge.3) then
         write(*,*) 'CalcDgy D(1,0,1,0) fin',D(1,0,1,0)
         write(*,*) 'CalcDgy D(0,1,1,1) fin',D(0,1,1,1)
         write(*,*) 'CalcDgy D(0,0,3,0) fin',D(0,0,3,0)
      endif
      if(rmax.ge.4) then
         write(*,*) 'CalcDgy D(0,4,0,0) fin',D(0,4,0,0),Derr(4)
         write(*,*) 'CalcDgy D(0,0,4,0) fin',D(0,0,4,0),Derr(4)
      endif
    endif
      
    write(*,*) 'CalcDgy final err',Derr
    write(*,*) 'CalcDgy final acc',Derr/abs(D(0,0,0,0))
#endif

!    write(*,*) 'CalcDgy Derr ',Derr
!    write(*,*) 'CalcDgy Derr2',Derr2

  end subroutine CalcDgy





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgp(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordgp_min,ordgp_max,id,Derr,Derr2
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgp(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgp_min,ordgp_max,id,Derr,Derr2)
  
    use globalD

    integer, intent(in) :: rmax,ordgp_min,ordgp_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: Dexpgp(:,:,:,:,:), DuvExpgp(:,:,:,:)
    double complex, allocatable :: C_0(:,:,:,:), Cuv_0(:,:,:,:), Shat(:,:,:)
    double complex, allocatable :: C_k(:,:,:), Cuv_k(:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod, fk, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpgp(0:1,0:rmax+ordgp_min+1,0:ordgp_max),truncfacexp
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,k,l,g,rg
    integer :: bin,nid(0:3),i
    logical :: errorwriteflag

#ifdef Dgtest
    write(*,*) 'CalcDgp in, ord',rmax,ordgp_min,ordgp_max
#endif
!   write(*,*) 'CalcDgp in, ',rmax,ordgp_min,ordgp_max

    ! calculate adjugated Gram matrix
!    mm02 = elimminf2_coli(m02)
!    mm12 = elimminf2_coli(m12)
!    mm22 = elimminf2_coli(m22)
!    mm32 = elimminf2_coli(m32)
!    q10  = elimminf2_coli(p10)
!    q21  = elimminf2_coli(p21)
!    q32  = elimminf2_coli(p32)
!    q30  = elimminf2_coli(p30)
!    q31  = elimminf2_coli(p31)
!    q20  = elimminf2_coli(p20)
!
!    Z(1,1) = 2d0*q10
!    Z(2,1) = q10+q20-q21
!    Z(3,1) = q10+q30-q31
!    Z(1,2) = Z(2,1)
!    Z(2,2) = 2d0*q20
!    Z(3,2) = q20+q30-q32
!    Z(1,3) = Z(3,1)
!    Z(2,3) = Z(3,2)
!    Z(3,3) = 2d0*q30
!
!    f(1) = q10+mm02-mm12
!    f(2) = q20+mm02-mm22
!    f(3) = q30+mm02-mm32
      

    ! choose reduction formulas with biggest denominators
    if (abs(f(1)).ge.max(abs(f(2)),abs(f(3)))) then
      k = 1
    else if (abs(f(2)).ge.max(abs(f(1)),abs(f(3)))) then
      k = 2
    else
      k = 3
    end if
    fk = f(k)


    ! allocation of C functions
    rmaxC = rmax + ordgp_min
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_k(0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_k(0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))

    ! determine binaries for C-coefficients
    i=0
    bin = 1
    do while (i.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(i) = id+bin
        i = i+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax) =  acc_req_CinD
    if(w_gp.ne.0d0) then
      do r=rmax+1,rmaxC
        acc_req_Cextra(r)= acc_req_Cextra(r-1)/w_gp
      end do
    else ! 10.07.2017
      acc_req_Cextra(rmax+1:rmaxC)=acc_inf
    endif

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    if (k.eq.1) then
      call CalcC(C_k(:,:,:),Cuv_k(:,:,:),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    else if (k.eq.2) then
      call CalcC(C_k(:,:,:),Cuv_k(:,:,:),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    else if (k.eq.3) then
      call CalcC(C_k(:,:,:),Cuv_k(:,:,:),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)
    end if

    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do


    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC))

    do r=0,rmaxC
      do n1=0,r
        do n2=0,r-n1
          n3 = r-n1-n2

          Shat(n1,n2,n3) = -C_0(0,n1,n2,n3)

          if ((k.eq.1).and.(n1.eq.0)) then
            Shat(n1,n2,n3) = Shat(n1,n2,n3) + C_k(0,n2,n3)
          else if ((k.eq.2).and.(n2.eq.0)) then
            Shat(n1,n2,n3) = Shat(n1,n2,n3) + C_k(0,n1,n3)
          else if ((k.eq.3).and.(n3.eq.0)) then
            Shat(n1,n2,n3) = Shat(n1,n2,n3) + C_k(0,n1,n2)
          end if

        end do
      end do
    end do

           

    ! allocation of array for det(Z)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpgp(0:rmaxExp/2,0:rmaxExp,0:rmaxExp,0:rmaxExp,0:ordgp_max))

    ! calculate Duv
    allocate(DuvExpgp(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpgp,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgp(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))

    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0

!   write(*,*) 'Dgp Cerr 0 ',Cerr_i(:,0)
!   write(*,*) 'Dgp Cerr k ',Cerr_i(:,k)

    Cij_err = max(Cerr_i(:,0),Cerr_i(:,k))
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,k))

!    maxZ = maxval(abs(Z))
!    maxZ=2d0*q2max 

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
!    truncfacexp = sqrt(abs(maxZ/abs(fk))) * truncfacD
    truncfacexp = sqrt(fac_gp) * truncfacD
    gtrunc = ordgp_max 

! calculate D(n0,n1,n2,n3) up to rank r for n0>0 and up to rank r-1 for n0=0
    rloop: do r=1,rmaxExp

      if (r.gt.rmax+gtrunc+1) exit rloop

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating
      ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
      ! exploiting eq. (5.63)
      maxDexpgp(1,r,0)=0d0
      do n0=r/2,1,-1
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3=r-2*n0-n1-n2

            Dexpgp(n0,n1,n2,n3,0) = (2d0*DuvExpgp(n0,n1,n2,n3) + C_0(n0-1,n1,n2,n3)  &
               + mm02*Dexpgp(n0-1,n1,n2,n3,0))/((r-n0)+1)/2d0

            if (n0.eq.1) then
              maxDexpgp(1,r,0) =  maxDexpgp(1,r,0) + abs(Dexpgp(n0,n1,n2,n3,0) )
            end if

            if (r-n0.le.rmax) then
              D(n0,n1,n2,n3) = Dexpgp(n0,n1,n2,n3,0)
            end if

          end do
        end do
      end do

#ifdef Dgptest      
      write(*,*) 'CalcDgp 0 D(1,0,1,0)= ',r,D(1,0,1,0)
#endif


      ! calculate
      ! D_00ijkl.. --> D_aijkl..
      ! exploiting eq. (5.62)
      maxDexpgp(0,r-1,0)=0d0
      do n1=0,r-1
        do n2=0,r-1-n1
          n3 = r-1-n1-n2

          Smod = Shat(n1,n2,n3)
          if ((k.eq.1).and.(n1.ge.1)) then
            Smod = Smod - 2d0*n1*Dexpgp(1,n1-1,n2,n3,0)
          else if ((k.eq.2).and.(n2.ge.1)) then
            Smod = Smod - 2d0*n2*Dexpgp(1,n1,n2-1,n3,0)
          else if ((k.eq.3).and.(n3.ge.1)) then
            Smod = Smod - 2d0*n3*Dexpgp(1,n1,n2,n3-1,0)
          end if

          Dexpgp(0,n1,n2,n3,0) = Smod/fk
          maxDexpgp(0,r-1,0) =  maxDexpgp(0,r-1,0) + abs(Dexpgp(0,n1,n2,n3,0))
        
          if (r.le.rmax+1) then
            D(0,n1,n2,n3) = Dexpgp(0,n1,n2,n3,0)
!            Derr(r-1) =  abs(maxZ/fk*Dexpgp(0,n1,n2,n3,0))
          end if

        end do
      end do
   
      if (r.le.rmax+1) then
!       Derr(r-1) =  abs(maxZ/fk)*maxDexpgp(0,r-1,0)
        Derr(r-1) =  fac_gp*maxDexpgp(0,r-1,0)
      endif
 
      ! error propagation from C's
      if(r.gt.1)then
        D00_err(r) = Cij_err(r-2)/(2*r)
      end if
      Dij_err(r-1)=max(Cij_err(r-1),2*D00_err(r))/abs(fk)

      if(r.gt.1)then
        D00_err2(r) = Cij_err2(r-2)/(2*r)
      end if
      Dij_err2(r-1)=max(Cij_err2(r-1),2*D00_err2(r))/abs(fk)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rg = r
      gloop: do g=1,min(gtrunc,r-1)
        rg = rg-1

        ! calculating
        ! D_00(a)0000..00 --> D_00(a)ij00..00 --> D_00(a)ijkl00..00 --> ... --> D_00(a)ijklmn..
        ! exploiting eq. (5.63)
        maxDexpgp(1,rg,g) = 0d0
        do n0=rg/2,1,-1
          do n1=0,rg-2*n0
            do n2=0,rg-2*n0-n1
              n3=rg-2*n0-n1-n2

              Dexpgp(n0,n1,n2,n3,g) = (2d0*mm02*Dexpgp(n0-1,n1,n2,n3,g)  &
               - Z(1,1)*Dexpgp(n0-1,n1+2,n2,n3,g-1) - 2d0*Z(2,1)*Dexpgp(n0-1,n1+1,n2+1,n3,g-1)  &
               - 2d0*Z(3,1)*Dexpgp(n0-1,n1+1,n2,n3+1,g-1) - Z(2,2)*Dexpgp(n0-1,n1,n2+2,n3,g-1)  &
               - 2d0*Z(3,2)*Dexpgp(n0-1,n1,n2+1,n3+1,g-1) - Z(3,3)*Dexpgp(n0-1,n1,n2,n3+2,g-1))  &
               /((rg-n0)+1d0)/4d0

              if(n0.eq.1) then
                maxDexpgp(1,rg,g) =  maxDexpgp(1,rg,g) + abs(Dexpgp(n0,n1,n2,n3,g))

#ifdef Dgptest
                if(n0.eq.1.and.n1.eq.0.and.n2.eq.1.and.n3.eq.0) then
                  write(*,*) 'CalcDgp  Dexp(1,0,1,0,g)',r,rg,g,Dexpgp(1,0,1,0,g)
                  write(*,*) 'CalcDgp  D(1,0,1,0)',r,rg,g,D(1,0,1,0)
                  write(*,*) 'CalcDgp  maxDexpgp(1,rg,g)',r,rg,g,maxDexpgp(1,rg,g)
                  if(g.gt.0) write(*,*) 'CalcDgp  trunc',abs(Dexpgp(n0,n1,n2,n3,g)),   &
                      truncfacexp*maxDexpgp(1,rg,g-1),truncfacexp,maxDexpgp(1,rg,g-1)
                endif
#endif

                if (g.eq.1.and.abs(Dexpgp(1,n1,n2,n3,g)).gt.            &
                    truncfacexp*max(1/m2scale,maxDexpgp(1,rg,g-1)) .or. &
                    g.ge.2.and.abs(Dexpgp(1,n1,n2,n3,g)).gt.            &
                    truncfacexp*maxDexpgp(1,rg,g-1)) then

                  gtrunc = g-1
                  exit gloop
                end if
              end if


            end do
          end do
        end do

#ifndef PPEXP00
        do n0=rg/2,1,-1
          if (rg-n0.le.rmax) then
            do n1=0,rg-2*n0
              do n2=0,rg-2*n0-n1
                n3=rg-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgp(n0,n1,n2,n3,g)
              end do
            end do
          end if
        end do
#endif

        ! calculate
        ! D_00ijkl.. --> D_aijkl..
        ! exploiting eq. (5.62)
        maxDexpgp(0,rg-1,g) = 0d0
        do n1=0,rg-1
          do n2=0,rg-1-n1
            n3 = rg-1-n1-n2

            Smod = -Z(1,k)*Dexpgp(0,n1+1,n2,n3,g-1)  &
                   -Z(2,k)*Dexpgp(0,n1,n2+1,n3,g-1)  &
                   -Z(3,k)*Dexpgp(0,n1,n2,n3+1,g-1)
            if ((k.eq.1).and.(n1.ge.1)) then
              Smod = Smod - 2d0*n1*Dexpgp(1,n1-1,n2,n3,g)
            else if ((k.eq.2).and.(n2.ge.1)) then
              Smod = Smod - 2d0*n2*Dexpgp(1,n1,n2-1,n3,g)
            else if ((k.eq.3).and.(n3.ge.1)) then
              Smod = Smod - 2d0*n3*Dexpgp(1,n1,n2,n3-1,g)
            end if

            Dexpgp(0,n1,n2,n3,g) = Smod/fk
        
            maxDexpgp(0,rg-1,g) =  maxDexpgp(0,rg-1,g) + abs(Dexpgp(0,n1,n2,n3,g))

            if (g.eq.1.and.abs(Dexpgp(0,n1,n2,n3,g)).gt.                  &
                truncfacexp*max(1/m2scale**2,maxDexpgp(0,rg-1,g-1)) .or.  &
                g.ge.2.and.abs(Dexpgp(0,n1,n2,n3,g)).gt.                  &
                truncfacexp*maxDexpgp(0,rg-1,g-1)) then

              gtrunc = g-1
              exit gloop
            end if
        
          end do
        end do

        ! error propagation from C's
        if(rg.gt.1)then
          D00_err(rg) = max(D00_err(rg),max(2*abs(m02)*Dij_err(rg-2),maxZ*Dij_err(rg))/(4*r))
        end if
        Dij_err(rg-1) = max(Dij_err(rg-1),max(2*D00_err(rg),maxZ*Dij_err(rg))/abs(fk))

        if(rg.gt.1)then
          D00_err2(rg) = max(D00_err2(rg),max(2*abs(m02)*Dij_err2(rg-2),maxZ*Dij_err2(rg))/(4*r))
        end if
        Dij_err2(rg-1) = max(Dij_err2(rg-1),max(2*D00_err2(rg),maxZ*Dij_err2(rg))/abs(fk))

#ifdef PPEXP00
        do n0=rg/2,1,-1
          if (rg-n0.le.rmax) then
            do n1=0,rg-2*n0
              do n2=0,rg-2*n0-n1
                n3=rg-2*n0-n1-n2
                D(n0,n1,n2,n3) = D(n0,n1,n2,n3) + Dexpgp(n0,n1,n2,n3,g)
              end do
            end do
          end if
        end do
#endif

        if ((rg.le.rmax+1)) then
          Derr(rg-1) = 0d0
          do n1=0,rg-1
            do n2=0,rg-1-n1
              n3 = rg-1-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgp(0,n1,n2,n3,g)
              if(abs(Dexpgp(0,n1,n2,n3,g-1)).ne.0d0) then
!              Derr(rg-1)=max(Derr(rg-1),abs(Dexpgp(0,n1,n2,n3,g))**2/abs(Dexpgp(0,n1,n2,n3,g-1)))
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpgp(0,n1,n2,n3,g))*min(1d0,abs(Dexpgp(0,n1,n2,n3,g))/abs(Dexpgp(0,n1,n2,n3,g-1))))
              else
                Derr(rg-1)=max(Derr(rg-1),abs(Dexpgp(0,n1,n2,n3,g)))
              end if
            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
#ifdef PVEST2
          if(Dij_err2(rg-1).gt.3d0*Derr(rg-1)) then
#else
          if(Dij_err(rg-1).gt.3d0*Derr(rg-1)) then
#endif
             gtrunc = min(g,gtrunc)
             
#ifdef Dgptest
             write(*,*) 'CalcDgp exit err',r,g,gtrunc
#endif
          end if  
 
        end if

      end do gloop

#ifdef Dgptest
      write(*,*) 'CalcDgp D(0,0,0,0)',r,D(0,0,0,0)
      write(*,*) 'CalcDgp D(0,0,1,0)',r,D(0,0,1,0)
      write(*,*) 'CalcDgp D(1,0,0,0)',r,D(1,0,0,0)
      write(*,*) 'CalcDgp D(1,0,1,0)',r,D(1,0,1,0)
      write(*,*) 'CalcDgp D(0,0,3,0)',r,D(0,0,3,0)

      write(*,*) 'CalcDgp Dij_err',r,Dij_err
      write(*,*) 'CalcDgp Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgp err',r,Derr
      write(*,*) 'CalcDgp acc',r,Derr/abs(D(0,0,0,0))
#endif              

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

!     if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit     ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then
        if (r.lt.rmax) then
          do rg=r+1,rmax
!            write(*,*) 'CalcDg exit rloop  =',rg,r,rmax
            do n0=0,rg/2
              do n1=0,rg-2*n0
                do n2=0,rg-2*n0-n1
                  D(n0,n1,n2,rg-2*n0-n1-n2)=0d0
                end do
              end do
            end do
          end do
          if(r.le.rmax) then
            do n1=0,r
!              do n2=0,rg-n1      ! corrected 11.04.2023 AD
              do n2=0,r-n1
                D(0,n1,n2,r-n1-n2)=0d0
              end do
            end do
          end if
          
100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDgp',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDgp:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDgp: p10 = ',p10
            write(nerrout_coli,111)' CalcDgp: p21 = ',p21
            write(nerrout_coli,111)' CalcDgp: p32 = ',p32
            write(nerrout_coli,111)' CalcDgp: p30 = ',p30
            write(nerrout_coli,111)' CalcDgp: p20 = ',p20
            write(nerrout_coli,111)' CalcDgp: p31 = ',p31
            write(nerrout_coli,111)' CalcDgp: m02 = ',m02
            write(nerrout_coli,111)' CalcDgp: m12 = ',m12
            write(nerrout_coli,111)' CalcDgp: m22 = ',m22
            write(nerrout_coli,111)' CalcDgp: m32 = ',m32
          end if
        end if

#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif
        exit rloop
      end if

    end do rloop

    ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
    ! already calculated for rmax+1
!    do r=rmax+1,2*rmax
#ifdef notneeded
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            write(*,*) 'CalcDgp exp rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

            write(*,*) 'CalcDgp exp rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

          end do
        end do
      end do
    end do
#endif

#ifdef Dgptest
      write(*,*) 'CalcDgp D(1,0,0,0) fin',D(1,0,0,0)
      write(*,*) 'CalcDgp D(1,0,1,0) fin',D(1,0,1,0)
      write(*,*) 'CalcDgp D(0,0,3,0) fin',D(0,0,3,0)
      write(*,*) 'CalcDgp D(0,1,1,1) fin',D(0,1,1,1)

    write(*,*) 'CalcDgp final err',Derr
    write(*,*) 'CalcDgp final acc',Derr/abs(D(0,0,0,0))
#endif                    
   

!    write(*,*) 'CalcDp Derr ',Derr
!    write(*,*) 'CalcDp Derr2',Derr2

  end subroutine CalcDgp




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDgpf(D,Duv,p10,p21,p32,p30,p20,p31,
  !                          m02,m12,m22,m32,rmax,ordgpf_min,ordgpf_max,id,Derr,Derr2)
  !  added by AD  16.08.2017
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDgpf(D,Duv,p10,p21,p32,p30,p20,p31,  &
                          m02,m12,m22,m32,rmax,ordgpf_min,ordgpf_max,id,Derr,Derr2)  
  
    use globalD

    integer, intent(in) :: rmax,ordgpf_min,ordgpf_max,id
    double complex, intent(in) :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
    double complex ::Zadj2(4)
    double complex, allocatable :: Dexpgpf(:,:,:,:,:), DuvExpgpf(:,:,:,:)
    double complex, intent(out) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Derr(0:rmax),Derr2(0:rmax)
    double complex, allocatable :: C_0(:,:,:,:), C_i(:,:,:,:), Shat(:,:,:,:,:)
    double complex, allocatable :: Cuv_0(:,:,:,:), Cuv_i(:,:,:,:)
    double complex, allocatable :: D_alt(:,:,:,:)
    double precision, allocatable :: Cerr_i(:,:),Cerr2_i(:,:)
    double complex :: Smod(3), Daux, elimminf2_coli
    double precision, allocatable :: D00_err(:),Dij_err(:),Cij_err(:),acc_req_Cextra(:)
    double precision, allocatable :: D00_err2(:),Dij_err2(:),Cij_err2(:)
    double precision :: maxDexpgpf(0:1,0:rmax+2*ordgpf_min,0:ordgpf_max),truncfacexp,acc_aux
    double precision :: minZk 
    integer :: rmaxC,rmaxExp,gtrunc,r,n0,n1,n2,n3,a,b,i,j,g,rg,m,n
    integer :: inds0(3),inds(3),inds2(2,4),at,bt,k,l,lt,ltt,nl,nlt,nltt
    integer :: bin,nid(0:3)
    logical :: errorwriteflag

#ifdef Dgpftest
    write(*,*) 'CalcDgpf in, ord',rmax,ordgpf_min,ordgpf_max
#endif

    ! allocation of C functions
    rmaxC = rmax + 2*ordgpf_min + 1
    allocate(C_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(Cuv_0(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC))
    allocate(C_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cuv_i(0:rmaxC,0:rmaxC,0:rmaxC,3))
    allocate(Cerr_i(0:rmaxC,0:3))
    allocate(Cerr2_i(0:rmaxC,0:3))
    allocate(acc_req_Cextra(0:rmaxC))
    
    ! determine binaries for C-coefficients
    k=0
    bin = 1
    do while (k.le.3)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    ! reduce required accuracy of higher rank C's that appear only in expansion by dividing
    ! by estimated suppression factors that are multiplied in expansion
    acc_req_Cextra(0:rmax+1) = acc_req_CinD
    acc_aux = acc_req_C
    if (y_gpf.ne.0d0) then
      do g=1,ordgpf_min
        acc_req_Cextra(rmax+2*g) = acc_req_Cextra(rmax+2*g-2)/y_gpf
        acc_req_Cextra(rmax+2*g+1) = acc_req_Cextra(rmax+2*g-1)/y_gpf
        acc_aux = acc_aux/max(x_gpf,v_gpf*y_gpf)
        acc_req_Cextra(rmax+g+1) = min(acc_req_Cextra(rmax+g+1),acc_aux)
      end do
    else if(x_gpf.ne.0d0) then ! 10.07.2017
      do g=1,ordgpf_min
        acc_aux = acc_aux/x_gpf
        acc_req_Cextra(rmax+g+1) = acc_aux
      end do
    else ! 10.07.2017
       acc_req_Cextra(rmax+2:rmax+2*ordgpf_min+1) = acc_inf
    end if
   


#ifdef Dgpftest
    write(*,*) 'CalcDgpf: accreq_Cextra',acc_req_Cextra
#endif

    call CalcC(C_0(:,0,:,:),Cuv_0(:,0,:,:),p21,p32,p31,m12,m22,m32,rmaxC,nid(0),Cerr_i(:,0),Cerr2_i(:,0),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,1),Cuv_i(:,:,:,1),p20,p32,p30,m02,m22,m32,rmaxC,nid(1),Cerr_i(:,1),Cerr2_i(:,1),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,2),Cuv_i(:,:,:,2),p10,p31,p30,m02,m12,m32,rmaxC,nid(2),Cerr_i(:,2),Cerr2_i(:,2),rmax,acc_req_Cextra)
    call CalcC(C_i(:,:,:,3),Cuv_i(:,:,:,3),p10,p21,p20,m02,m12,m22,rmaxC,nid(3),Cerr_i(:,3),Cerr2_i(:,3),rmax,acc_req_Cextra)

#ifdef Dgpftest
    write(*,*) 'CalcDgpf Cerr 0',Cerr_i(:,0)
    write(*,*) 'CalcDgpf Cerr 1',Cerr_i(:,1)
    write(*,*) 'CalcDgpf Cerr 2',Cerr_i(:,2)
    write(*,*) 'CalcDgpf Cerr 3',Cerr_i(:,3)
#endif


    ! shift of integration momentum in C\{0}
    do n1=1,rmaxC
      do n2=0,rmaxC-n1
        do n3=0,rmaxC-n1-n2
          n0 = (rmaxC-n1-n2-n3)
          C_0(0:n0,n1,n2,n3) = -C_0(0:n0,n1-1,n2,n3)  &
                            -C_0(0:n0,n1-1,n2+1,n3)-C_0(0:n0,n1-1,n2,n3+1)
          Cuv_0(0:n0,n1,n2,n3) = -Cuv_0(0:n0,n1-1,n2,n3)  &
                              -Cuv_0(0:n0,n1-1,n2+1,n3)-Cuv_0(0:n0,n1-1,n2,n3+1)
        end do
      end do
    end do
      



    ! coefficients Shat defined in (5.13)
    allocate(Shat(0:rmaxC,0:rmaxC,0:rmaxC,0:rmaxC,3))

    do r=0,rmaxC
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            Shat(n0,n1,n2,n3,:) = -C_0(n0,n1,n2,n3)

            if(n1.eq.0) then
              Shat(n0,n1,n2,n3,1) = Shat(n0,n1,n2,n3,1) + C_i(n0,n2,n3,1)
            end if

            if(n2.eq.0) then
              Shat(n0,n1,n2,n3,2) = Shat(n0,n1,n2,n3,2) + C_i(n0,n1,n3,2)
            end if

            if(n3.eq.0) then
              Shat(n0,n1,n2,n3,3) = Shat(n0,n1,n2,n3,3) + C_i(n0,n1,n2,3)
            end if

#ifdef Dgpftest
          if(n0.eq.0.and.n1.eq.0.and.n2.eq.0.and.n3.eq.1)then
            write(*,*) 'CalcDgpf 0 C_0',C_0(n0,n1,n2,n3)
            write(*,*) 'CalcDgpf 0 C_1',C_i(n0,n2,n3,1)
            write(*,*) 'CalcDgpf 0 C_2',C_i(n0,n1,n3,2)
            write(*,*) 'CalcDgpf 0 C_3',C_i(n0,n1,n2,3)
            write(*,*) 'CalcDgpf 0 Sh1',Shat(n0,n1,n2,n3,1)
            write(*,*) 'CalcDgpf 0 Sh2',Shat(n0,n1,n2,n3,2)
            write(*,*) 'CalcDgpf 0 Sh3',Shat(n0,n1,n2,n3,3)
          endif
#endif

          end do
        end do
      end do
    end do

    ! choose reduction formulas with smallest expansion terms
    minZk = maxZ
    if (maxval(abs(Z(1,1:3))).le.minZk) then
      minZk = maxval(abs(Z(1,1:3)))
      k = 1
      l = 1
      lt = 2
      ltt = 3 
    end if
    if (maxval(abs(Z(2,1:3))).lt.minZk) then
      minZk = maxval(abs(Z(2,1:3)))
      k = 2
      l = 2
      lt = 3
      ltt = 1 
    end if
    if (maxval(abs(Z(3,1:3))).lt.minZk) then
      minZk = maxval(abs(Z(3,1:3)))
      k = 3
      l = 3
      lt = 1
      ltt = 2 
    end if

#ifdef Dgpftest
    write(*,*) 'CalcDgpf: Z',k, maxval(abs(Z(k,1:3)))
#endif

        
    ! allocation of array for det(Z)- and det(X)-expanded C-coefficients
    rmaxExp = rmaxC+1
    allocate(Dexpgpf(0:max(rmax/2,1),0:rmaxExp-2,0:rmaxExp-2,0:rmaxExp-2,0:ordgpf_max))
   

    ! calculate Cuv
    allocate(DuvExpgpf(0:rmaxExp,0:rmaxExp,0:rmaxExp,0:rmaxExp))
    call CalcDuv(DuvExpgpf,Cuv_0,mm02,f,rmaxExp,id)
    Duv(0:rmax,0:rmax,0:rmax,0:rmax) = DuvExpgpf(0:rmax,0:rmax,0:rmax,0:rmax)

    ! allocate arrays for error propagation
    allocate(D00_err(0:rmaxExp))
    allocate(Dij_err(0:rmaxExp))
    allocate(Cij_err(0:rmaxC))

    allocate(D00_err2(0:rmaxExp))
    allocate(Dij_err2(0:rmaxExp))
    allocate(Cij_err2(0:rmaxC))

    ! initialize accuracy estimates
    Derr = acc_inf
    Dij_err =0d0
    D00_err =0d0
    Cij_err = max(Cerr_i(:,0),Cerr_i(:,1),Cerr_i(:,2),Cerr_i(:,3))

    Derr2 = acc_inf
    Dij_err2 =0d0
    D00_err2 =0d0
    Cij_err2 = max(Cerr2_i(:,0),Cerr2_i(:,1),Cerr2_i(:,2),Cerr2_i(:,3))

    ! truncation of expansion if calculated term larger than truncfacexp * previous term
    ! crucial for expansion parameters between 0.1 and 1 !!!
    truncfacexp = sqrt(fac_gpf) * truncfacD
    gtrunc = ordgpf_max 

! calculate D(1,n1,n2,n3) up to rank r+2
! calculate D(0,n1,n2,n3) up to rank r  
    rloop: do r=0,rmaxExp-2

      if (r.gt.rmax+2*gtrunc+2) exit rloop

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! 0th-order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! calculating D_00ijk.. exploiting eq. (5.71)
      maxDexpgpf(1,r,0)=0d0
      do nl=r,0,-1
        do nlt=r-nl,0,-1
          nltt = r-nl-nlt
          inds0(l) = nl
          inds0(lt) = nlt
          inds0(ltt) = nltt

          inds(l) = nl+1
          inds(lt) = nlt
          inds(ltt) = nltt

          Daux = Shat(0,inds(1),inds(2),inds(3),k) 

          Dexpgpf(1,inds0(1),inds0(2),inds0(3),0) = Daux/(2*(nl+1))

          maxDexpgpf(1,r,0) =  maxDexpgpf(1,r,0) + abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),0) )

!          if (r+2.le.rmax) then          !  for fixed rank
          if (r+1.le.rmax) then
            D(1,inds0(1),inds0(2),inds0(3)) = Dexpgpf(1,inds0(1),inds0(2),inds0(3),0)
          end if
      


        end do
      end do

      ! calculate D_ijkl.. exploiting eq. (5.72)
      maxDexpgpf(0,r,0)=0d0
      do n1=0,r
        do n2=0,r-n1
          n3 = r-n1-n2

          Daux = 2d0*(4+r+r)*Dexpgpf(1,n1,n2,n3,0) - 4*DuvExpgpf(1,n1,n2,n3)  &
            - 2*C_0(0,n1,n2,n3)

#ifdef Dgpftest
          if(n1.eq.0.and.n2.eq.2.and.n3.eq.0)then
            write(*,*) 'CalcDgpf 0 Daux',Daux
          endif
#endif

          Dexpgpf(0,n1,n2,n3,0) = Daux/(2d0*m02)

#ifdef Dgpftest
          if(n1.eq.1.and.n2.eq.1.and.n3.eq.1)then
            write(*,*) 'CalcDgpf D_0',r,Dexpgpf(0,n1,n2,n3,0) 
          endif
#endif

          maxDexpgpf(0,r,0) =  maxDexpgpf(0,r,0) + abs(Dexpgpf(0,n1,n2,n3,0))
          if (r.le.rmax) then
            D(0,n1,n2,n3) = Dexpgpf(0,n1,n2,n3,0)
!            Derr(r) =  abs(maxZadjf/maxXadj*Dexpgpf(0,n1,n2,n3,0))
          end if
      
        end do
      end do

      if (r.le.rmax) then
!       Derr(r) =  abs(maxZadjf/Xadj(a,b))*maxDexpgpf(0,r,0)
        Derr(r) =  fac_gpf*maxDexpgpf(0,r,0)
      endif

      ! error propagation from C's
      D00_err(r+2) = Cij_err(r+1)/2d0 
      Dij_err(r)=1d0/abs(m02)*max((2*r+4)*D00_err(r+2),Cerr_i(r,0))

      D00_err2(r+2) = Cij_err2(r+1)/2d0 
      Dij_err2(r)=1d0/abs(m02)*max((2*r+4)*D00_err2(r+2),Cerr2_i(r,0))
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! higher order coefficients
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      rg = r
      gloop: do g=1,min(gtrunc,r/2)
        rg = rg-2

        ! calculating D_00ijk.. exploiting eq. (5.71)
        maxDexpgpf(1,rg,g) = 0d0
        do nl=rg,0,-1
          do nlt=rg-nl,0,-1
            nltt = rg-nl-nlt
            inds0(l) = nl
            inds0(lt) = nlt
            inds0(ltt) = nltt

            inds = inds0
            inds(l) = inds(l)+1
            Daux = -f(k)*Dexpgpf(0,inds(1),inds(2),inds(3),g-1)

            inds(l) = inds(l)+1
            Daux = Daux - Z(k,l)*Dexpgpf(0,inds(1),inds(2),inds(3),g-1)

            inds(l) =  inds(l)-1
            inds(lt) =  inds(lt)+1
            Daux = Daux - Z(k,lt)*Dexpgpf(0,inds(1),inds(2),inds(3),g-1)

            inds(lt) =  inds(lt)-1
            inds(ltt) =  inds(ltt)+1
            Daux = Daux - Z(k,ltt)*Dexpgpf(0,inds(1),inds(2),inds(3),g-1)

            Dexpgpf(1,inds0(1),inds0(2),inds0(3),g) = Daux/(2*(nl+1))

            maxDexpgpf(1,rg,g) =  maxDexpgpf(1,rg,g) + abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),g) )
!            if (rg+2.le.rmax) then
!              D(1,inds0(1),inds0(2),inds0(3)) = D(1,inds0(1),inds0(2),inds0(3))  &
!                                              + Dexpgpf(1,inds0(1),inds0(2),inds0(3),g)
!            end if
 
            
            if (g.eq.1.and.abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),g)).gt.   &
                1d1*truncfacexp*max(1/m2scale,maxDexpgpf(1,rg,g-1)) .or.      &
                g.ge.2.and.abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),g)).gt.   &
                truncfacexp*maxDexpgpf(1,rg,g-1)) then

#ifdef Dgpftest
                write(*,*) 'CalcDgpf exit gloop',1,inds0(1),inds0(2),inds0(3),g, &
                abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),g)),abs(Dexpgpf(1,inds0(1),inds0(2),inds0(3),g-1)),maxDexpgpf(1,rg,g-1)
#endif

                gtrunc = g-1
                exit gloop
!               gtrunc = g
!               cycle gloop               ! worsens results for Dgy ??
              end if
       
          end do
        end do

#ifndef PPEXP00
!          if (rg+2.le.rmax) then        ! for fixed rank
        if (rg+1.le.rmax) then
          do n1=0,rg
            do n2=0,rg-n1
              n3=rg-n1-n2
              D(1,n1,n2,n3) = D(1,n1,n2,n3) + Dexpgpf(1,n1,n2,n3,g)
            end do
          end do
        end if
#endif

        ! calculate D_ijkl.. exploiting eq. (5.72)
        maxDexpgpf(0,rg,g) = 0d0
        do n1=0,rg
          do n2=0,rg-n1
            n3 = rg-n1-n2
      
            inds(1) = n1
            inds(2) = n2
            inds(3) = n3
            Daux = 2*(4+rg+rg)*Dexpgpf(1,n1,n2,n3,g)

            do i=1,3
            do j=1,3
              inds(i)=inds(i)+1
              inds(j)=inds(j)+1 
              Daux = Daux + Z(i,j)*Dexpgpf(0,inds(1),inds(2),inds(3),g-1)
              inds(i)=inds(i)-1
              inds(j)=inds(j)-1  
            end do
            end do

            Dexpgpf(0,n1,n2,n3,g) = Daux/(2*m02)

            maxDexpgpf(0,rg,g) =  maxDexpgpf(0,rg,g) + abs(Dexpgpf(0,n1,n2,n3,g))

!            if (rg.le.rmax) then
!              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgpf(0,n1,n2,n3,g)
!            end if

            if (g.eq.1.and.abs(Dexpgpf(0,n1,n2,n3,g)).gt.               &
                truncfacexp*max(1/m2scale**2,maxDexpgpf(0,rg,g-1)) .or. &
                g.ge.2.and.abs(Dexpgpf(0,n1,n2,n3,g)).gt.               &
                truncfacexp*maxDexpgpf(0,rg,g-1)) then

#ifdef Dgpftest
            write(*,*) 'CalcDgpf exit gloop',n1,n2,n3,g,abs(Dexpgpf(0,n1,n2,n3,g)),abs(Dexpgpf(0,n1,n2,n3,g-1)),maxDexpgpf(0,rg,g-1)
#endif

              gtrunc = g-1
              exit gloop
!             gtrunc = g
!             cycle gloop
            end if

          end do
        end do

        ! error propagation from C's
        if(rg.gt.1)then
          D00_err(rg+2) = max(D00_err(rg+2),                  &
                              fmax/2d0*Dij_err(rg+1),         &
                              maxZ/2d0*Dij_err(rg+2))
        end if
        Dij_err(rg)=max(Dij_err(rg),maxZ/(2*abs(m02))*Dij_err(rg+2), &
               (2*rg+4)/abs(m02)*D00_err(rg+2))

        if(rg.gt.1)then
          D00_err2(rg+2) = max(D00_err2(rg+2),                     &
                              fmax/2d0*Dij_err2(rg+1),     &
                              maxZ/2d0*Dij_err2(rg+2))
        end if
        Dij_err2(rg)=max(Dij_err2(rg),maxZ/(2*abs(m02))*Dij_err2(rg+2), &
               (2*rg+4)/abs(m02)*D00_err2(rg+2))

#ifdef PPEXP00
        if (rg+2.le.rmax) then
          do n1=0,rg
            do n2=0,rg-n1
              n3=rg-n1-n2
              D(1,n1,n2,n3) = D(1,n1,n2,n3) + Dexpgpf(1,n1,n2,n3,g)
            end do
          end do
        end if
#endif

        if (rg.le.rmax) then
          Derr(rg) = 0d0
          do n1=0,rg
            do n2=0,rg-n1
              n3 = rg-n1-n2
              D(0,n1,n2,n3) = D(0,n1,n2,n3) + Dexpgpf(0,n1,n2,n3,g)
              if(abs(Dexpgpf(0,n1,n2,n3,g-1)).ne.0d0) then
                Derr(rg)=max(Derr(rg),abs(Dexpgpf(0,n1,n2,n3,g))*min(1d0,abs(Dexpgpf(0,n1,n2,n3,g))/abs(Dexpgpf(0,n1,n2,n3,g-1))))
              else
                Derr(rg)=max(Derr(rg),abs(Dexpgpf(0,n1,n2,n3,g)))
              endif

#ifdef Dgpftest
!              write(*,*) 'CalcDgpf Derr calc',rg,Derr(rg),n1,n2,n3,g,abs(Dexpgpf(0,n1,n2,n3,g)),abs(Dexpgpf(0,n1,n2,n3,g-1))
#endif

            end do
          end do

          ! if error from C's larger than error from expansion stop expansion
          ! allow for one more term, as each step involves only even or odd ranks
#ifdef PVEST2
          if(Dij_err2(rg).gt.3d0*Derr(rg)) then
#else
          if(Dij_err(rg).gt.3d0*Derr(rg)) then
#endif
            gtrunc = min(g,gtrunc)
!           gtrunc = min(g+1,gtrunc)
             
#ifdef Dgpftest
            write(*,*) 'CalcDgpf exit err',rg,g,gtrunc
            write(*,*) 'CalcDgpf exit err',Dij_err(rg),Derr(rg)
#endif
          end if

        end if

      end do gloop

#ifdef Dgpftest

      write(*,*) 'CalcDgpf D(1,0,0,0)',r,D(1,0,0,0)
      write(*,*) 'CalcDgpf D(0,0,0,0)',r,D(0,0,0,0)
      write(*,*) 'CalcDgpf D(0,0,0,1)',r,D(0,0,0,1)
      if (r.ge.2.and.rmax.ge.2) then
      write(*,*) 'CalcDgpf D(0,0,0,2)',r,D(0,0,0,2)
      endif
      if (r.ge.3.and.rmax.ge.3)then
      write(*,*) 'CalcDgpf D(1,0,0,1)',r,D(1,0,0,1)
      write(*,*) 'CalcDgpf D(0,1,0,2)',r,D(0,1,0,2)
      write(*,*) 'CalcDgpf D(0,0,0,3)',r,D(0,0,0,3)
      write(*,*) 'CalcDgpf D(0,1,1,1)',r,D(0,1,1,1)
      write(*,*) 'CalcDgpf D(0,2,1,0)',r,D(0,2,1,0)
      endif

      write(*,*) 'CalcDgpf Dij_err',r,Dij_err
      write(*,*) 'CalcDgpf Dij_acc',r,Dij_err/abs(D(0,0,0,0))

      write(*,*) 'CalcDgpf err',r,g,Derr
      write(*,*) 'CalcDgpf acc',r,g,Derr/abs(D(0,0,0,0))
#endif

      Derr2 = max(Derr,Dij_err2(0:rmax))
      Derr = max(Derr,Dij_err(0:rmax))

!     if(maxval(Derr).le.acc_req_D*abs(D(0,0,0,0))) exit     ! changed 28.01.15
      ! check if target precision already reached
#ifdef Cutrloop
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0) then
        if (r.lt.rmax) then
          do rg=r+1,rmax
            do n1=0,rg
              do n2=0,rg-n1
                D(0,n1,n2,rg-n1-n2)=0d0
              end do
            end do
          end do
          do rg=r+1,rmax
            do n1=0,rg-2
              do n2=0,rg-2-n1
                D(1,n1,n2,rg-2-n1-n2)=0d0
              end do
            end do
          end do
          
100       format(((a)))
111       format(a22,2('(',g24.17,',',g24.17,') ':))
          call SetErrFlag_coli(-5)
          call ErrOut_coli('CalcDgpf',' exit rloop for D', &
          errorwriteflag)
          if (errorwriteflag) then
            write(nerrout_coli,100)' CalcDgpf:  exit rloop for D ', &
            '    should not appear'
            write(nerrout_coli,111)' CalcDgpf: p10 = ',p10
            write(nerrout_coli,111)' CalcDgpf: p21 = ',p21
            write(nerrout_coli,111)' CalcDgpf: p32 = ',p32
            write(nerrout_coli,111)' CalcDgpf: p30 = ',p30
            write(nerrout_coli,111)' CalcDgpf: p20 = ',p20
            write(nerrout_coli,111)' CalcDgpf: p31 = ',p31
            write(nerrout_coli,111)' CalcDgpf: m02 = ',m02
            write(nerrout_coli,111)' CalcDgpf: m12 = ',m12
            write(nerrout_coli,111)' CalcDgpf: m22 = ',m22
            write(nerrout_coli,111)' CalcDgpf: m32 = ',m32
          end if
        end if
 
#else
      if(maxval(Derr-acc_req_D*abs(D(0,0,0,0))).le.0d0.and.r.ge.rmax) then
#endif

        exit rloop

      end if

    end do rloop


    ! calculating D_0000ijk.. exploiting eq. (5.71)
    do r=4,rmax+1                 !  includes rmax+1  24.01.16
      do n0=2,max(rmax,r/2)       !  includes rmax+1  24.01.16
        do nl=r-2*n0,0,-1
          do nlt=r-2*n0-nl,0,-1
            nltt = r-2*n0-nl-nlt
            inds0(l) = nl
            inds0(lt) = nlt
            inds0(ltt) = nltt

            inds(l) = nl+1
            inds(lt) = nlt
            inds(ltt) = nltt
            Daux = Shat(n0-1,inds(1),inds(2),inds(3),k)            &
                 - f(k)*D(n0-1,inds(1),inds(2),inds(3))            &
                 - Z(k,1)*D(n0-1,inds(1)+1,inds(2),inds(3))        &
                 - Z(k,2)*D(n0-1,inds(1),inds(2)+1,inds(3))        &  
                 - Z(k,3)*D(n0-1,inds(1),inds(2),inds(3)+1)

            D(n0,inds0(1),inds0(2),inds0(3)) = Daux/(2*(nl+1))

          end do
        end do
      end do
    end do

    ! reduction formula (5.10) for n0+n1+n2+N3=r, n0=1 only!!!!!! 
    ! already calculated for rmax+1 with extension of 24.01.16 above
!    do r=rmax+1,2*rmax
#ifdef notneeded
    do r=rmax+1,rmax+1
      do n0=r-rmax,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            n3 = r-2*n0-n1-n2

            write(*,*) 'CalcDgpf exp rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

            D(n0,n1,n2,n3) = (C_0(n0-1,n1,n2,n3) + 2*mm02*D(n0-1,n1,n2,n3)  &
                + 4*Duv(n0,n1,n2,n3)  &
                + f(1)*D(n0-1,n1+1,n2,n3) + f(2)*D(n0-1,n1,n2+1,n3)  &
                + f(3)*D(n0-1,n1,n2,n3+1)) / (2*(r-1)) 

            write(*,*) 'CalcDgpf dir rmax+1',r,n0,n1,n2,n3, D(n0,n1,n2,n3)

          end do
        end do
      end do
    end do
#endif

#ifdef Dgpftest
      if(rmax.ge.2) then
      write(*,*) 'CalcDgpf D(1,0,0,0) fin',D(1,0,0,0)
      write(*,*) 'CalcDgpf D(0,0,0,2) fin',D(0,0,0,2)
      if(rmax.ge.3) then
      write(*,*) 'CalcDgpf D(1,0,1,0) fin',D(1,0,1,0)
      write(*,*) 'CalcDgpf D(0,1,1,1) fin',D(0,1,1,1)
      endif
      endif

    write(*,*) 'CalcDgpf final err',Derr
    write(*,*) 'CalcDgpf final acc',Derr/abs(D(0,0,0,0))
#endif

!    write(*,*) 'CalcDgpf Derr ',Derr
!    write(*,*) 'CalcDgpf Derr2',Derr2

  end subroutine CalcDgpf





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine CopyDimp3(D,D_alt,Derr,Derr_alt,Derr1,Derr1_alt,Derr2,Derr2_alt,Drmethod,Drmethod_alt,rmax,r_alt)
  
    integer,   intent(in) :: rmax,r_alt
    double complex, intent(inout) :: D(0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(inout) :: Derr(0:rmax),Derr1(0:rmax),Derr2(0:rmax)
    integer, intent(inout) :: Drmethod(0:rmax)
    double complex, intent(in) :: D_alt(0:r_alt,0:r_alt,0:r_alt,0:r_alt)
    double precision, intent(in) :: Derr_alt(0:r_alt),Derr1_alt(0:r_alt),Derr2_alt(0:r_alt)
    integer, intent(in) :: Drmethod_alt(0:r_alt)

    integer :: r,n1,n2,n0

    do r=0,r_alt
      if (Derr_alt(r).lt.Derr(r)) then
        Drmethod(r)=Drmethod_alt(r)
        Derr(r)=Derr_alt(r)
        Derr1(r)=Derr1_alt(r)
        Derr2(r)=Derr2_alt(r)
        forall (n0=0:r/2)
          forall (n1=0:2*r-n0)
            forall (n2=0:r-2*n0-n1)
              D(n0,n1,n2,r-2*n0-n1-n2) = D_alt(n0,n1,n2,r-2*n0-n1-n2)
            end forall
          end forall
        end forall
        forall (n0=1:(r+1)/2)
          forall (n1=0:r+1-2*n0)
            forall (n2=0:r+1-2*n0-n1)
              D(n0,n1,n2,r+1-2*n0-n1-n2) = D_alt(n0,n1,n2,r+1-2*n0-n1-n2)
            end forall
          end forall
        end forall
!       forall (n0=0:r)
!         forall (n1=0:r-n0)
!           forall (n2=0:r-n0-n1)
!             D(n0,n1,n2,r-n0-n1-n2) = D_alt(n0,n1,n2,r-n0-n1-n2)
!           end forall
!         end forall
!       end forall
!        forall (n1=0:r)
!          forall (n2=0:r-n1)
!            forall (n3=0:r-n1-n2)
!              D((r-n1-n2-n3)/2,n1,n2,n3) = D_alt((r-n1-n2-n3)/2,n1,n2,n3)
!            end forall
!          end forall
!        end forall
      end if
    end do

   end subroutine CopyDimp3



end module reductionD

