!!
!!  File DD_global.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!
!!-------------------------------------------------------------------

module DD_global
  integer          :: rmax2,rmax3,rmax4,rmax5,rmax6,nmax,rmax
  integer          :: r2max2,r2max3,r2max4,r2max5,r2max6
  integer (kind=8) :: Ncoefmax2,Ncoefmax3,Ncoefmax4,Ncoefmax5,Ncoefmax6
  integer (kind=8) :: Ncoefmax3_int,Ncoefmax4_int
  integer,allocatable,dimension(:)   :: tid,ntid
  integer (kind=8),allocatable,dimension(:,:) :: BinC

  double precision  :: cacc,dacc
  integer           :: mode34,mode5,mode6,ritmax
  integer           :: outlevel,outchannel,cout,coutmax
  logical           :: cout_on

  double precision, allocatable, dimension(:)   :: aimacc,erracc
  double precision, allocatable, dimension(:,:) :: resaccabs,resaccrel
  double precision, allocatable, dimension(:,:) :: resaccabs2
  double precision, allocatable, dimension(:,:) :: resaccrel2
  integer           :: nmaster,r2master,accflag,errflag,stopflag

  double precision  :: deltauv,muv2,delta2ir,delta1ir,mir2,mx2(100)

  double complex, allocatable, dimension(:)     :: scalint,scalintnew
  double precision, allocatable, dimension(:)   :: scalint_err
  double complex, allocatable, dimension(:,:)   :: auxc
  double precision, allocatable, dimension(:,:) :: auxr
  double precision, allocatable, dimension(:)   :: acc_pave,acc_new
  double precision, allocatable, dimension(:,:) :: accr2_aux
  double precision, allocatable, dimension(:,:) :: accr2_newprelim
  double precision, allocatable, dimension(:,:) :: accr2_new_aux
  double precision, allocatable, dimension(:) :: maxtxij,maxttx0klm
  double precision, allocatable, dimension(:) :: maxttx0ijm,maxtz_nj
  double precision, allocatable, dimension(:) :: maxttz_knlm,ttzff_kl
  integer, allocatable,dimension(:,:) :: auxi
  integer, allocatable,dimension(:) :: r2_aux,r2_new_aux,r2_newprelim
  integer, allocatable,dimension(:) :: qmethod,qmethod_new
  integer            :: nc_DDin,nr_DDin,ni_DDin,i_DDin(100)
  double complex     :: c_DDin(100)
  double precision   :: r_DDin(100)
  character(len=20)  :: s_DDin

  double precision                    :: dprec_dd

end module DD_global


module DD_2pt
  double complex, allocatable, dimension(:,:,:) :: B_cache,Buv_cache
end module DD_2pt

module DD_3pt
  double complex, allocatable, dimension(:,:)   :: C_cache,Cuv_cache
  double complex, allocatable, dimension(:,:)   :: C_new_cache
  double complex, allocatable, dimension(:,:)   :: Cuv_new_cache
  double complex, allocatable, dimension(:,:,:,:,:)   :: ttx2_aux
  double complex, allocatable, dimension(:,:,:) :: x2_aux,tx2_aux
  double precision, allocatable, dimension(:,:) :: Cij_err,C00_err
  double precision, allocatable, dimension(:,:) :: Cij_err2
  double precision, allocatable, dimension(:,:) :: Cij_err_newprelim
  double precision, allocatable, dimension(:,:) :: C00_err_newprelim
  double precision, allocatable, dimension(:,:) :: Cij_err_new
  double precision, allocatable, dimension(:,:) :: C00_err_new
  double precision, allocatable, dimension(:,:,:)     :: z2_aux,tz2_aux
  double precision, allocatable, dimension(:,:,:)     :: z2i_aux
  double precision, allocatable, dimension(:,:,:,:,:) :: ttz2_aux
end module DD_3pt

module DD_4pt
  double complex, allocatable, dimension(:,:)   :: D_cache,Duv_cache
  double precision, allocatable, dimension(:,:) :: Dij_err,D00_err
  double precision, allocatable, dimension(:,:) :: Dij_err_new
  double precision, allocatable, dimension(:,:) :: D00_err_new,Dij_err2
end module DD_4pt

module DD_5pt
  double precision, allocatable, dimension(:,:) :: Eij_err,Eij_err2
end module DD_5pt

module DD_6pt
  double precision, allocatable, dimension(:,:) :: Fij_err,Fij_err2
end module DD_6pt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialization of global DD parameters
!--------------------------------------------------------------
! 18.2.2016 Stefan Dittmaier
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine Init_DD_global(nmax_in,ritmax_in)

  use DD_global
  use DD_2pt
  use DD_3pt
  use DD_4pt
  use DD_5pt
  use DD_6pt

  integer, intent(in) :: nmax_in,ritmax_in
  integer             :: id,n,k
  integer (kind=8)    :: Binomial_DD

  ritmax = ritmax_in

! dimensions for array declarations
  rmax6  = 6
  rmax5  = 5
  rmax4  = ritmax+3
  rmax3  = rmax4+2
  rmax2  = rmax4+4
  rmax   = 2*rmax2

! maximal rank of N-point tensors
!  -> dimensions for arrays in internal cache 
  r2max6  = rmax6
  r2max5  = r2max6-1
!  r2max4  = r2max5-1
!changed SD 28.6.17
  r2max4  = rmax4
  r2max3  = rmax3
  r2max2  = rmax2

! store some  binomial coefficients
  if (allocated(BinC)) then
    deallocate(BinC)
  endif
  allocate(BinC(0:r2max3,0:r2max3))
  do n=0,r2max3
    do k=0,r2max3
      BinC(n,k) = Binomial_DD(n,k)
    enddo
  enddo

  Ncoefmax4_int = Binomial_DD(r2max4+4,4)
  Ncoefmax3_int = Binomial_DD(r2max3+3,3)

  Ncoefmax2 = 2*Binomial_DD(r2max2+2,2) + 2*r2max2 + 13
  Ncoefmax3 = 2*Binomial_DD(r2max3+3,3) + 8*r2max3 + 11
  Ncoefmax4 = 2*Binomial_DD(r2max4+4,4) + 7*r2max4 + 10 ! conservative 
                                                        ! upper limit
  Ncoefmax5 = 2*Binomial_DD(r2max5+5,5) + 6*r2max5 + 9
  Ncoefmax6 = 2*Binomial_DD(r2max6+6,6) + 6*r2max6 + 9

  nmax = nmax_in

! identifiers for N-point functions
  if (allocated(tid)) then
    deallocate(tid,ntid)
  end if
  allocate(tid(0:2**nmax_in-1),ntid(0:nmax_in))
  ntid(0:nmax) = 0
  do id=0,2**nmax-1
    n=0
    do k=0,nmax-1
      if (mod(id,2**(k+1))/2**k.eq.0) n=n+1
    enddo
    ntid(n) = ntid(n)+1
    tid(id) = ntid(n)
  enddo

  if (allocated(aimacc)) then
    deallocate(aimacc,erracc)
  end if
  allocate(aimacc(nmax),erracc(nmax))
  
  if (allocated(resaccabs)) then
    deallocate(resaccabs,resaccrel,resaccabs2,resaccrel2)
  end if
  allocate(resaccabs(0:2**nmax,0:rmax),resaccrel(0:2**nmax,0:rmax))
  allocate(resaccabs2(0:2**nmax,0:rmax),resaccrel2(0:2**nmax,0:rmax))

! arrays depending on nmax and any r2max#

  if (allocated(B_cache)) then
    deallocate(B_cache,Buv_cache)
  end if
  if (nmax.ge.2) then
    allocate(B_cache(ntid(2),0:r2max2,0:r2max2))
    allocate(Buv_cache(ntid(2),0:r2max2,0:r2max2))
  endif

  if (allocated(C_cache)) then
    deallocate(C_cache,Cuv_cache,C_new_cache,Cuv_new_cache)
    deallocate(Cij_err,C00_err,Cij_err2)
    deallocate(Cij_err_newprelim,C00_err_newprelim)
    deallocate(Cij_err_new,C00_err_new)
    deallocate(accr2_aux,accr2_newprelim,accr2_new_aux)
  endif
  if (nmax.ge.3) then
    allocate(C_cache(ntid(3),Ncoefmax3_int))
    allocate(Cuv_cache(ntid(3),Ncoefmax3_int))
    allocate(C_new_cache(ntid(3),Ncoefmax3_int))
    allocate(Cuv_new_cache(ntid(3),Ncoefmax3_int))
    allocate(Cij_err(ntid(3),0:2*r2max3))
    allocate(C00_err(ntid(3),0:2*r2max3))
    allocate(Cij_err2(ntid(3),0:2*r2max3))
    allocate(Cij_err_newprelim(ntid(3),0:2*r2max3))
    allocate(C00_err_newprelim(ntid(3),0:2*r2max3))
    allocate(Cij_err_new(ntid(3),0:2*r2max3))
    allocate(C00_err_new(ntid(3),0:2*r2max3))
    allocate(accr2_aux(ntid(3),0:2*r2max3))
    allocate(accr2_newprelim(ntid(3),0:2*r2max3))
    allocate(accr2_new_aux(ntid(3),0:2*r2max3))
  endif

  if (allocated(D_cache)) then
    deallocate(D_cache,Duv_cache)
    deallocate(Dij_err,D00_err,Dij_err_new,D00_err_new,Dij_err2)
  endif
  if (nmax.ge.4) then
    allocate(D_cache(ntid(4),Ncoefmax4_int))
    allocate(Duv_cache(ntid(4),Ncoefmax4_int))
    allocate(Dij_err(ntid(4),0:2*r2max4))
    allocate(D00_err(ntid(4),0:2*r2max4))
    allocate(Dij_err_new(ntid(4),0:2*r2max4))
    allocate(D00_err_new(ntid(4),0:2*r2max4))
    allocate(Dij_err2(ntid(4),0:2*r2max4))
  endif

  if (allocated(Eij_err)) then
    deallocate(Eij_err,Eij_err2)
  endif
  if (nmax.ge.5) then
    allocate(Eij_err(ntid(5),0:2*r2max5))
    allocate(Eij_err2(ntid(5),0:2*r2max5))
  endif

  if (allocated(Fij_err)) then
    deallocate(Fij_err,Fij_err2)
  endif
  if (nmax.ge.6) then
    allocate(Fij_err(ntid(6),0:2*r2max6))
    allocate(Fij_err2(ntid(6),0:2*r2max6))
  endif

! arrays depending only on nmax

  if (allocated(z2i_aux)) then
    deallocate(z2_aux,tz2_aux,z2i_aux,ttz2_aux)
    deallocate(x2_aux,tx2_aux,ttx2_aux)
  endif
  if (nmax.ge.3) then
    allocate(z2i_aux(ntid(3),2,2),ttz2_aux(ntid(3),2,2,2,2))
    allocate(z2_aux(ntid(3),2,2),tz2_aux(ntid(3),2,2))
    allocate(x2_aux(ntid(3),0:2,0:2),tx2_aux(ntid(3),0:2,0:2))
    allocate(ttx2_aux(ntid(3),0:2,0:2,0:2,0:2))
  endif

  if (allocated(scalint)) then
    deallocate(scalint,scalint_err,scalintnew)
    deallocate(auxc)
    deallocate(auxr)
    deallocate(acc_pave,acc_new)
    deallocate(maxtxij,maxttx0klm,maxttx0ijm)
    deallocate(maxtz_nj,maxttz_knlm,ttzff_kl)
    deallocate(auxi,r2_aux,r2_new_aux,r2_newprelim,qmethod,qmethod_new)
  end if

  allocate(scalint(0:2**nmax),scalint_err(0:2**nmax))
  allocate(scalintnew(0:2**nmax))
  allocate(maxtxij(0:2**nmax),maxttx0klm(0:2**nmax))
  allocate(maxttx0ijm(0:2**nmax))
  allocate(maxtz_nj(0:2**nmax),maxttz_knlm(0:2**nmax))
  allocate(ttzff_kl(0:2**nmax))
  allocate(acc_pave(0:2**nmax),acc_new(0:2**nmax))
  allocate(auxr(0:2**nmax,2))
  allocate(auxc(0:2**nmax,7))
  allocate(auxi(0:2**nmax,6))
  allocate(r2_aux(0:2**nmax),r2_new_aux(0:2**nmax))
  allocate(r2_newprelim(0:2**nmax))
  allocate(qmethod(0:2**nmax),qmethod_new(0:2**nmax))

end subroutine Init_DD_global


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Binomial coefficient
!--------------------------------------------------------------
! 18.2.2016 Stefan Dittmaier
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer (kind=8) function Binomial_DD(n,m)
  integer :: i,n,m

  Binomial_DD = 1
  if ((m.ge.0).and.(m.le.n)) then
    do i=1,m
      Binomial_DD = Binomial_DD*(n+1-i)
      Binomial_DD = Binomial_DD/i
    enddo
  else
    Binomial_DD = 0
  endif

end function Binomial_DD

module DD_statistics

  integer :: dpv_calc_dd,dpv_ok_dd
  integer :: dapv_calc_dd,dapv_ok_dd,dg_calc_dd,dg_ok_dd
  integer :: dg2_calc_dd,dg2_ok_dd,dgc_calc_dd,dgc_ok_dd
  integer :: d_bad_dd

end module DD_statistics
