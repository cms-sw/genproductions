!!
!!  File reductionAB.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module reductionAB  *
!  *    by Lars Hofer     *
!  ************************
!
!  global variables:
!  dprec_coli
! 
!  functions and subroutines:
!  CalcA, CalcB, CalcDB
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




module reductionAB

  use combinatorics
  use cache
  use coli_aux2
#ifdef DETTEST
  use coli_aux3
  use coli_aux4
#endif
  use collier_global


  implicit none

!  double complex, allocatable :: BCDuv_cp(:,:,:,:,:), BCD_cp(:,:,:,:,:)
!  double complex, allocatable :: Euv_cp(:,:,:,:,:,:), E_cp(:,:,:,:,:,:) 
!  double complex, allocatable :: Fuv_cp(:,:,:,:,:,:,:), F_cp(:,:,:,:,:,:,:)
!  double complex, allocatable :: TN_cp(:,:,:,:), TNuv_cp(:,:,:,:), TeN_cp(:,:,:,:,:)
!  integer, allocatable :: rmax_cp(:), scheme_cp(:)


contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcA(A,Auv,m02,rmax,Aerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcA(A,Auv,m02,rmax,Aerr)
  
    integer, intent(in) :: rmax
    double complex, intent(in) :: m02
    double complex, intent(out) :: A(0:rmax/2), Auv(0:rmax/2)
    double precision, optional, intent(out) :: Aerr(0:rmax)
    double complex :: A0_coli, mm02, elimminf2_coli
    double complex :: fac, ccc
    integer :: n0

    if(present(Aerr)) then
      Aerr = 0d0
    end if
    Auv(0) = m02
    A(0) = A0_coli(m02)

    ccc = 2d0
    mm02 = elimminf2_coli(m02)
    do n0=1,rmax/2
      fac = m02/(2*(n0+1))
      ccc = ccc*fac
      Auv(n0) = fac*Auv(n0-1)
      A(n0) = fac*(A(n0-1)+ccc)
    end do
    if(present(Aerr)) then
      Aerr = abs(A(0))*acc_def_B
    end if

  end subroutine CalcA





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcB(B,Buv,p10,m02,m12,rmax,id,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcB(B,Buv,p10,m02,m12,rmax,id,Berr)
  
    integer, intent(in) :: rmax, id
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(inout) :: B(0:rmax,0:rmax), Buv(0:rmax,0:rmax) 
    double precision, optional, intent(out) :: Berr(0:rmax)
    double complex, allocatable :: Baux(:,:), Buvaux(:,:),fct(:)
    double precision, allocatable :: Berraux(:)
    double complex :: x(3)
    integer :: rank,switch,cnt,n0,n1,r
    logical :: nocalc,wrica

    if (use_cache_system) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.2) then
          x(1)=p10
          x(2)=m02
          x(3)=m12
          rank = rmax
          switch = 0

          allocate(fct(2*NCoefsG(rmax,2)+rmax+1))
          call ReadCache(fct,2*NCoefsG(rmax,2)+rmax+1,x,3,1,id,2,rank,nocalc,wrica)    

          if(nocalc)then
            cnt = 0
            do r=0,rmax
              do n0=0,r
                n1 = r-n0
 
                cnt = cnt+1
                B(n0,n1) = fct(cnt)
                cnt = cnt+1
                Buv(n0,n1) = fct(cnt)

              end do
              cnt = cnt+1
              if(present(Berr))then     
                Berr(r) = real(fct(cnt))
              end if
            end do     
            return
          endif

          
          if(rank.eq.rmax) then

            allocate(Berraux(0:rank))
            call CalcBred(B,Buv,p10,m02,m12,rank,id,Berraux)
            if(present(Berr)) Berr=Berraux
 
            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r
                  n1 = r-n0
 
                  cnt = cnt+1
                  fct(cnt) = B(n0,n1)
                  cnt = cnt+1
                  fct(cnt) = Buv(n0,n1)

                end do
                cnt = cnt+1
                fct(cnt) = Berraux(r)
              end do
        
              call WriteCache(fct,2*NCoefsG(rank,2)+rank+1,id,2,rank)

            end if

            return
          
          
          else
            allocate(Baux(0:rank,0:rank))
            allocate(Buvaux(0:rank,0:rank))
            allocate(Berraux(0:rank))

            call CalcBred(Baux,Buvaux,p10,m02,m12,rank,id,Berraux)
 
            if (wrica) then
              cnt = 0
              deallocate(fct)
              allocate(fct(2*NCoefsG(rank,2)+rank+1))
              do r=0,rank
                do n0=0,r
                  n1 = r-n0
 
                  cnt = cnt+1
                  fct(cnt) = Baux(n0,n1)
                  cnt = cnt+1
                  fct(cnt) = Buvaux(n0,n1)

                end do
                cnt = cnt+1
                fct(cnt) = Berraux(r)
              end do
        
              call WriteCache(fct,2*NCoefsG(rank,2)+rank+1,id,2,rank)

            end if

            B(0:rmax,0:rmax) = Baux(0:rmax,0:rmax)
            Buv(0:rmax,0:rmax) = Buvaux(0:rmax,0:rmax)
            if(present(Berr))then
              Berr = Berraux(0:rmax)
            end if
            return
      
          end if 
!        end if
      end if
    end if


    if(present(Berr)) then
      call CalcBred(B,Buv,p10,m02,m12,rmax,id,Berr)
    else
      allocate(Berraux(0:rmax))
      call CalcBred(B,Buv,p10,m02,m12,rmax,id,Berraux)
      deallocate(Berraux)
    end if

  end subroutine CalcB





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcBred(B,Buv,p10,m02,m12,rmax,id,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcBred(B,Buv,p10,m02,m12,rmax,id,Berr)
  
    integer, intent(in) :: rmax, id
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(inout) :: B(0:rmax,0:rmax), Buv(0:rmax,0:rmax) 
    double precision, intent(out)  :: Berr(0:rmax)
!   double complex, allocatable :: A(:), Auv(:)
    double complex :: A(0:rmax-1), Auv(0:rmax-1)
    double complex :: q2, mm02, mm12, f1
    double complex :: Bn_coli,elimminf2_coli
    integer :: rmaxA,n0,n1,r,sgn,k,rid,r0
    logical :: finarg
    double complex, parameter :: cd0 = dcmplx(0d0,0d0)  

    r0 = 0

    ! r=0
    if (r0.eq.0) then
      Buv(0,0) = 1d0
      B(0,0) = Bn_coli(0,p10,m02,m12)
    end if    

    ! calculate B(0,n1)
    sgn = (-1)**r0
    do n1=max(r0,1),rmax
      sgn = -sgn
      Buv(0,n1) = sgn/(n1+1d0)
      B(0,n1) = Bn_coli(n1,p10,m02,m12)
    end do


    ! calculate B(n0,n1) for n0=/=0
    if (rmax.ge.1) then
      
      call elminf2iv_coli(p10,m02,m12,q2,mm02,mm12,finarg)

!      q2 = elimminf2_coli(p10)
!      mm12 = elimminf2_coli(m12)
!      mm02 = elimminf2_coli(m02)
!      finarg=q2.ne.cd0.or.mm12.ne.cd0.or.mm02.ne.cd0

      ! at least one of the invariants p10,m02,m12 different from zero
      if (finarg) then
      
        ! calculation of A-functions
        rmaxA = max(rmax-1,0)
!       allocate(A(0:rmaxA))
!       allocate(Auv(0:rmaxA))
        call CalcA(A,Auv,mm12,2*rmaxA)

        ! p10 non-negligible
        if (abs(q2)/(abs(q2)+abs(mm02+mm12)).gt.dprec_coli) then

          f1 = q2-mm12+mm02

!          do n0=1,rmax/2
!            k=rmax-2*n0
          do r=2,2*rmax
            do n0=max(1,r-rmax),r/2
              k=r-2*n0
              do n1=max(r0-2*n0,0),k
                Buv(n0,n1) = ((-1)**n1*Auv(n0-1) + 2*mm02*Buv(n0-1,n1) &
                    + f1*Buv(n0-1,n1+1))/(2*n0+n1+1)/2
                B(n0,n1) = ((-1)**n1*A(n0-1) + 2*mm02*B(n0-1,n1) &
                    + f1*B(n0-1,n1+1) + 4*Buv(n0,n1))/(2*n0+n1+1)/2  
!                write(*,*) 'CalcBred B(n0,n1)',n0,n1,B(n0,n1)
            end do
            end do
          end do

        ! p10 negligible
        else

!          do n0=1,rmax/2
!            k=rmax-2*n0
          do r=2,2*rmax
            do n0=max(1,r-rmax),r/2
              k=r-2*n0
              do n1=max(r0-2*n0,0),k
             
                Buv(n0,n1) = ((-1)**n1*Auv(n0-1) + mm02*Buv(n0-1,n1))/(n0+n1+1)/2
                B(n0,n1) = ((-1)**n1*A(n0-1) + mm02*B(n0-1,n1) &
                            + 2*Buv(n0,n1))/(n0+n1+1)/2  
              end do
            end do
          end do

        end if

      ! p10=m02=m12=0
      else

        Buv(1:,:) = 0d0
        B(1:,:) = 0d0

      end if
    end if

    Berr(0) = abs(B(0,0))
    do r=1,rmax
      Berr(r) = max(Berr(r-1),abs(B(0,r)))
    end do
    Berr = Berr*acc_def_B

  end subroutine CalcBred




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcDB(DB,DBuv,p10,m02,m12,rmax,id,DBerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine CalcDB(DB,DBuv,p10,m02,m12,rmax,id,DBerr)
  
    integer, intent(in) :: rmax, id
    double complex, intent(in) :: p10,m02,m12
    double complex, intent(inout) :: DB(0:rmax/2,0:rmax), DBuv(0:rmax/2,0:rmax) 
    double precision, optional, intent(out)  :: DBerr(0:rmax)
    double precision  :: Berr(0:rmax)
    double complex :: B(0:rmax,0:rmax), Buv(0:rmax,0:rmax) 
!   double complex, allocatable :: A(:), Auv(:)
    double complex :: A(0:rmax-1), Auv(0:rmax-1)
    double complex :: q2, mm02, mm12, f1
    double complex :: DBn_coli,elimminf2_coli
    integer :: rmaxA,n0,n1,r,k,rid
    logical :: finarg
    double complex, parameter :: cd0 = dcmplx(0d0,0d0)  

!    write(*,*) 'CalcDB in',p10,m02,m12,rmax,id

    ! r=0
    DBuv(0,0) = 0d0
    DB(0,0) = DBn_coli(0,p10,m02,m12)

    call CalcBred(B,Buv,p10,m02,m12,rmax,id,Berr)

    ! calculate DB(0,n1)
    do n1=1,rmax
      DBuv(0,n1) = 0d0
      DB(0,n1) = DBn_coli(n1,p10,m02,m12)
    end do


    ! calculate DB(n0,n1) for n0=/=0
    if (rmax.ge.2) then
      
      call elminf2iv_coli(p10,m02,m12,q2,mm02,mm12,finarg)

!      q2 = elimminf2_coli(p10)
!      mm12 = elimminf2_coli(m12)
!      mm02 = elimminf2_coli(m02)
!      finarg=q2.ne.cd0.or.mm12.ne.cd0.or.mm02.ne.0d0

      ! at least one of the invariants p10,m02,m12 different from zero
      if (q2.ne.0d0.or.mm02.ne.0d0.or.mm12.ne.0d0) then
      
        ! calculation of A-functions
        rmaxA = max(rmax-1,0)
!       allocate(A(0:rmaxA))
!       allocate(Auv(0:rmaxA))
        call CalcA(A,Auv,mm12,2*rmaxA)

        f1 = q2-mm12+mm02

        do r=2,rmax
          do n0=max(1,r-rmax),r/2
            k=r-2*n0
            do n1=0,k
              DBuv(n0,n1) = (Buv(n0-1,n1+1) + 2*mm02*DBuv(n0-1,n1) &
                  + f1*DBuv(n0-1,n1+1))/(2*n0+n1+1)/2
     
              DB(n0,n1) = B(n0-1,n1+1) + 4*DBuv(n0,n1)
              if(mm02.ne.0d0) then
                DB(n0,n1) = DB(n0,n1)  + 2*mm02*DB(n0-1,n1)
              end if
              if(f1.ne.0d0) then
                DB(n0,n1) = DB(n0,n1)  + f1*DB(n0-1,n1+1)
              end if
              
              DB(n0,n1) = DB(n0,n1) /(2*(2*n0+n1+1))
  
            end do
          end do
        end do

      ! p10=m02=m12=0
      else

        DBuv(1:,:) = 0d0
        
        do r=2,rmax
          do n0=max(1,r-rmax),r/2
            k=r-2*n0
            do n1=0,k
              if (.not.IR_rational_terms_cll) then
                DBuv(n0,n1) = Buv(n0-1,n1+1)/(2*n0+n1+1)/2
              end if
              DB(n0,n1) = (B(n0-1,n1+1) + 4*DBuv(n0,n1))/(2*n0+n1+1)/2  
            end do
          end do
        end do
          
      end if
    end if

    DBerr(0) = abs(DB(0,0))
    do r=1,rmax
      DBerr(r) = max(DBerr(r-1),abs(DB(0,r)))
    end do
    DBerr = DBerr*acc_def_B

  end subroutine CalcDB


end module reductionAB
