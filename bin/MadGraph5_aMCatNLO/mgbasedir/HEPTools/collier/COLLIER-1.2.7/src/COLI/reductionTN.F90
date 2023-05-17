!!
!!  File reductionTN.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!#define TNtest

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module reductionTN  *
!  *    by Lars Hofer     *
!  ************************
! 
!  functions and subroutines:
!  CalcTN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





module reductionTN

  use reductionEFG
! use coli_aux3

  implicit none

!  integer, allocatable :: AddToCInd(:,:,:,:), DropCInd(:,:,:,:)

contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTN(TN,TNuv,MomInv,masses2,rmax,id,TNerr,TNerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcTN(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)

    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex, intent(inout) :: TN(NCoefs(rmax,N))
    double complex, intent(inout) :: TNuv(NCoefs(rmax,N))
    double precision, intent(out) :: TNerr(0:rmax),TNerr2(0:rmax)
    double complex, allocatable :: TNaux(:,:,:),TNuvaux(:,:,:) 
    double complex :: B(0:rmax,0:rmax), Buv(0:rmax,0:rmax)
    double complex :: C(0:rmax,0:rmax,0:rmax), Cuv(0:rmax,0:rmax,0:rmax)
    double complex :: D(0:rmax,0:rmax,0:rmax,0:rmax), Duv(0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: x(BinomTable(2,N)+N)
    double complex, allocatable :: fct(:)
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,bino_m2,cnt
    logical :: nocalc,wrica


    select case (N)

      case(1)
        call CalcA(TN,TNuv,masses2(0),rmax,TNerr)
        TNerr2=TNerr
        return

      case(2)
        call CalcB(B,Buv,MomInv(1),masses2(0),masses2(1),rmax,id,TNerr)

        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            n1 = r-2*n0
            TN(cnt) = B(n0,n1)
            cnt = cnt+1
          end do
        end do
        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            n1 = r-2*n0
            TNuv(cnt) = Buv(n0,n1)
            cnt = cnt+1
          end do
        end do
        TNerr2=TNerr
        return

      case(3)
        call CalcC(C,Cuv,MomInv(1),MomInv(2),MomInv(3),  &
                         masses2(0),masses2(1),masses2(2),rmax,id,TNerr,TNerr2)

        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            do n1=r-2*n0,0,-1
              n2 = r-2*n0-n1
              TN(cnt) = C(n0,n1,n2)
              cnt = cnt+1
            end do
          end do
        end do
        TNuv = 0d0
        do r=2,rmax
          cnt = NCoefs(r-1,3)+1
          do n0=r/2,1,-1
            do n1=r-2*n0,0,-1
              n2 = r-2*n0-n1
              TNuv(cnt) = Cuv(n0,n1,n2)
              cnt = cnt+1
            end do
          end do
        end do

!        write(*,*) 'CalcTN 3 TNerr ',TNerr
!        write(*,*) 'CalcTN 3 TNerr2',TNerr2

        return

     

      case(4)
        call CalcD(D,Duv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),MomInv(6),  &
            masses2(0),masses2(1),masses2(2),masses2(3),rmax,id,TNerr,TNerr2)

        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            do n1=r-2*n0,0,-1
              do n2=r-2*n0-n1,0,-1
                n3 = r-2*n0-n1-n2
                TN(cnt) = D(n0,n1,n2,n3)
                cnt = cnt+1
              end do
            end do
          end do
        end do
        TNuv = 0d0
        do r=4,rmax
          cnt = NCoefs(r-1,4)+1
          do n0=r/2,2,-1
            do n1=r-2*n0,0,-1
              do n2=r-2*n0-n1,0,-1
                n3 = r-2*n0-n1-n2
                TNuv(cnt) = Duv(n0,n1,n2,n3)
                cnt = cnt+1
              end do
            end do
          end do
        end do

!        write(*,*) 'CalcTN 4 TNerr ',TNerr
!        write(*,*) 'CalcTN 4 TNerr2',TNerr2

        return

      case(5)
        call CalcE(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
            MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),  &
            masses2(0),masses2(1),masses2(2),masses2(3),masses2(4)  &
            ,rmax,id,TNerr,TNerr2)

        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            do n1=r-2*n0,0,-1
              do n2=r-2*n0-n1,0,-1
                do n3=r-2*n0-n1-n2,0,-1
                  n4 = r-2*n0-n1-n2-n3
                  TN(cnt) = E(n0,n1,n2,n3,n4)
                  cnt = cnt+1
                end do
              end do
            end do
          end do
        end do
        TNuv = 0d0
        do r=6,rmax
          cnt = NCoefs(r-1,5)+1
          do n0=r/2,3,-1
            do n1=r-2*n0,0,-1
              do n2=r-2*n0-n1,0,-1
                do n3=r-2*n0-n1-n2,0,-1
                  n4 = r-2*n0-n1-n2-n3
                  TNuv(cnt) = Euv(n0,n1,n2,n3,n4)
                  cnt = cnt+1
                end do
              end do
            end do
          end do
        end do
        return

      case default
        allocate(TNaux(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)) 
        allocate(TNuvaux(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax))
        call CalcTNint(TNaux,TNuvaux,MomInv,masses2,N,rmax,id,TNerr,TNerr2)
       
        cnt = 1
        do r=0,rmax
          do n0=r/2,0,-1
            do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
              TN(cnt) = TNaux(i,n0,r)
              cnt = cnt+1
            end do
          end do
        end do
        TNuv = 0d0
        do r=2*N-4,rmax
          cnt = NCoefs(r-1,N)+1
          do n0=r/2,N-2,-1
            do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
              TNuv(cnt) = TNuvaux(i,n0,r)
              cnt = cnt+1
            end do
          end do
        end do
        return
                        
    end select


  end subroutine CalcTN





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTNint(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine CalcTNint(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)

    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double complex, intent(inout) :: TN(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax) 
    double complex, intent(inout) :: TNuv(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double precision, intent(out) :: TNerr(0:rmax),TNerr2(0:rmax)
    double complex, allocatable :: TNaux(:,:,:), TNuvaux(:,:,:)
    double complex :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, allocatable :: TNerraux(:),TNerr2aux(:)
    double complex :: x(BinomTable(2,N)+N)
    double complex, allocatable :: fct(:)
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,bino_m2,cnt
    logical :: nocalc,wrica,noten



    if (N.eq.5) then
      call CalcE(E,Euv,MomInv(1),MomInv(2),MomInv(3),MomInv(4),MomInv(5),  &
                       MomInv(6),MomInv(7),MomInv(8),MomInv(9),MomInv(10),  &
                       masses2(0),masses2(1),masses2(2),masses2(3),masses2(4),rmax,id,TNerr,TNerr2)

      do r=0,rmax
        do n0=r/2,0,-1
          cnt = 1
          do n1=r-2*n0,0,-1
            do n2=r-2*n0-n1,0,-1
              do n3=r-2*n0-n1-n2,0,-1
                n4 = r-2*n0-n1-n2-n3
                TN(cnt,n0,r) = E(n0,n1,n2,n3,n4)
                cnt = cnt+1
              end do
            end do
          end do
        end do
      end do
      do r=6,rmax
        do n0=r/2,3,-1
          cnt = 1
          do n1=r-2*n0,0,-1
            do n2=r-2*n0-n1,0,-1
              do n3=r-2*n0-n1-n2,0,-1
                n4 = r-2*n0-n1-n2-n3
                TNuv(cnt,n0,r) = Euv(n0,n1,n2,n3,n4)
                cnt = cnt+1
              end do
            end do
          end do
        end do
      end do
      return
    end if

    if ((use_cache_system).and.(tencache.gt.N)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
        bino = BinomTable(2,N)
        x(1:bino) = MomInv
        x(bino+1:bino+N) = masses2   
        rank = rmax

        if(rmax.ge.2*N-4) then
          allocate(fct(NCoefs(rmax,N)+NCoefs(rmax-2*N+4,N)+2*(rmax+1)))
          call ReadCache(fct,NCoefs(rmax,N)+NCoefs(rmax-2*N+4,N)+2*(rmax+1),x,bino+N,1,id,N,rank,nocalc,wrica)
        else
          allocate(fct(NCoefs(rmax,N)+2*(rmax+1)))
          call ReadCache(fct,NCoefs(rmax,N)+2*(rmax+1),x,bino+N,1,id,N,rank,nocalc,wrica)
        end if
    
        if(nocalc)then
          cnt = 0
          do r=0,rmax
            do n0=r/2,0,-1
              do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
                cnt = cnt+1
                TN(i,n0,r) = fct(cnt)
              end do
            end do
            do n0=r/2,N-2,-1
              do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
                cnt = cnt+1
                TNuv(i,n0,r) = fct(cnt)
              end do
            end do
            cnt = cnt+1
            TNerr(r) = real(fct(cnt))
            cnt = cnt+1
            TNerr2(r) = real(fct(cnt))
          end do
          return
        endif


        if(rank.eq.rmax) then

          call CalcTNred(TN,TNuv,MomInv,masses2,N,rank,id,TNerr,TNerr2)
          
          if (wrica) then
            cnt = 0
            do r=0,rank
              do n0=r/2,0,-1
                do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
 
                  cnt = cnt+1
                  fct(cnt) = TN(i,n0,r)

                end do
              end do
              do n0=r/2,N-2,-1
                do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
 
                  cnt = cnt+1
                  fct(cnt) = TNuv(i,n0,r)

                end do
              end do
              cnt = cnt+1
              fct(cnt) = TNerr(r)
              cnt = cnt+1
              fct(cnt) = TNerr2(r)
            end do
   
            if(rank.ge.2*N-4) then
              call WriteCache(fct,NCoefs(rank,N)+NCoefs(rank-2*N+4,N)+2*(rank+1),id,N,rank)
            else
              call WriteCache(fct,NCoefs(rank,N)+2*(rank+1),id,N,rank)
            end if

          end if

          return

        
        else
          allocate(TNaux(BinomTable(rank,N+rank-2),0:rank/2,0:rank))
          allocate(TNuvaux(BinomTable(rank,N+rank-2),0:rank/2,0:rank))
          allocate(TNerraux(0:rank))
          allocate(TNerr2aux(0:rank))

          call CalcTNred(TNaux,TNuvaux,MomInv,masses2,N,rank,id,TNerraux,TNerr2aux)
          
          if (wrica) then
            cnt = 0
            deallocate(fct)
            if(rank.ge.2*N-4) then
              allocate(fct(NCoefs(rank,N)+NCoefs(rank-2*N+4,N)+2*(rank+1)))
            else
              allocate(fct(NCoefs(rank,N)+2*(rank+1)))
            end if
            do r=0,rank
              do n0=r/2,0,-1
                do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
 
                  cnt = cnt+1
                  fct(cnt) = TNaux(i,n0,r)

                end do
              end do
              do n0=r/2,N-2,-1
                do i=1,BinomTable(r-2*n0,N+r-2*n0-2)
 
                  cnt = cnt+1
                  fct(cnt) = TNuvaux(i,n0,r)

                end do
              end do
              cnt = cnt+1
              fct(cnt) = TNerraux(r)
              cnt = cnt+1
              fct(cnt) = TNerr2aux(r)
            end do
   
            if(rank.ge.2*N-4) then
              call WriteCache(fct,NCoefs(rank,N)+NCoefs(rank-2*N+4,N)+2*(rank+1),id,N,rank)
            else
              call WriteCache(fct,NCoefs(rank,N)+2*(rank+1),id,N,rank)
            end if

          end if

          TN = TNaux(1:BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
          TNuv = TNuvaux(1:BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
          TNerr = TNerraux(0:rmax)
          TNerr2 = TNerr2aux(0:rmax)
          return

        end if
      end if
    end if

    call CalcTNred(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)


  end subroutine CalcTNint





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcTNred(TN,TNuv,MomInv,masses2,N,rmax,id)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine CalcTNred(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)

    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double precision, intent(out) :: TNerr(0:rmax),TNerr2(0:rmax)
    double complex :: q10,q21,q32,q43,q54,q20,q31,q42,q53,q50,q30,q41,q52,q40,q51
    double complex :: mm02,mm12,mm22,mm32,mm42,mm52
!    double complex :: mxinv(0:5,0:5),mxinvs
    double complex :: mx(0:5,0:5),mx0k(5,5),mx0kinv(5,5),ff(N-1)
    double complex :: det,newdet,TNaux,mx0kinvs(5)
    double complex :: zmx0kinv(5,5),zmx0kinvs(5)
    double precision :: maxZ,maxzmx0kinv(5),maxzmx0kinvs
    double complex, intent(inout) :: TN(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex, intent(inout) :: TNuv(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex, allocatable :: TNm1_0(:,:,:), TNm1UV_0(:,:,:), TNm1_i(:,:,:,:)
    double complex, allocatable :: TNm1_0aux(:,:,:), TNm1UV_0aux(:,:,:), TNm1UV_i(:,:,:,:)
    double complex :: S(5), elimminf2_coli,chdet,gramdet
    double precision :: TNm1err(0:5,0:rmax),TNm1err2(0:5,0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,n6,k,i,j,m,nid(0:5),r0,bin,kbest,Nm1,inds(N-1)
    integer :: bino,bino_0,bino_i,bino_m1,bino_m2,cnt,iaux,rmax_m1,shift,cind,rBCD
    integer :: mia1,mia2,mia3
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A18,'(',d25.18,' , ',d25.18,' )')"

#ifdef TNtest
    character(len=*),parameter :: fmt999 = "(' gm(',i1,',',i1,') =  ',d23.16,' , ',d23.16)"
#endif

!    double complex :: Xadjk0,Zadjk(5),mat(4,4),mxmx0kinv(5,5),mxmx0kinvs(5)
    double complex :: mxmx0kinv(5,5)
    double precision :: maxf
    integer        :: jmax

#ifdef TNtest    
    write(*,*) 'CalcTNred in N',N,rmax
    write(*,*) 'CalcTNred in s',MomInv
    write(*,*) 'CalcTNred in m',masses2
#endif

    ! allocation of TN functions
    Nm1 = N-1
    rmax_m1 = max(rmax-1,0)
    bino_0 = BinomTable(rmax_m1,N+rmax_m1-2)
    bino_i = BinomTable(rmax_m1,Nm1+rmax_m1-2)
    allocate(TNm1_0(bino_0,0:rmax_m1/2,0:rmax_m1))
    allocate(TNm1_0aux(bino_i,0:rmax_m1/2,0:rmax_m1))
    allocate(TNm1_i(bino_i,0:rmax_m1/2,0:rmax_m1,5))
    allocate(TNm1UV_0(bino_0,0:rmax_m1/2,0:rmax_m1))
    allocate(TNm1UV_0aux(bino_i,0:rmax_m1/2,0:rmax_m1))
    allocate(TNm1UV_i(bino_i,0:rmax_m1/2,0:rmax_m1,5))

    ! determine binaries for TN-coefficients
    k=0
    bin = 1
    do while (k.le.5)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    call CalcTNint(TNm1_0aux,TNm1UV_0aux,SubMomInv(N,0,MomInv),SubMasses(N,0,masses2),  &
        Nm1,rmax_m1,nid(0),TNerr=TNm1err(0,0:rmax_m1),     &
        TNerr2=TNm1err2(0,0:rmax_m1))
    do k=1,5
      call CalcTNint(TNm1_i(:,:,:,k),TNm1UV_i(:,:,:,k),SubMomInv(N,k,MomInv),  &
          SubMasses(N,k,masses2),Nm1,rmax_m1,nid(k),        &
          TNerr=TNm1err(k,0:rmax_m1),TNerr2=TNm1err2(k,0:rmax_m1))
    end do

    ! shift of integration momentum in TN\{0}
    do n0=0,rmax_m1/2
      TNm1_0(1,n0,2*n0) = TNm1_0aux(1,n0,2*n0)
      do r=1,rmax_m1-2*n0
        mia1 = BinomTable(r-1,N+r-3)+1
        mia2 = BinomTable(r,N+r-3)
        mia3 = BinomTable(r,N+r-2) 
        TNm1_0(mia1:mia3,n0,r+2*n0) = TNm1_0aux(1:mia2,n0,r+2*n0)
      end do
    end do
    do r=1,rmax_m1
      do n0=0,(r-1)/2
        do i=BinomTable(r-2*n0-1,N+r-2*n0-3),1,-1
          TNm1_0(i,n0,r) = -TNm1_0(i,n0,r-1)
          do j=2,N-1 
            TNm1_0(i,n0,r) = TNm1_0(i,n0,r)  &
                           - TNm1_0(AddToCInd(j,i,r-2*n0-1,N-1),n0,r)
          end do
        end do
      end do
    end do
    do n0=N-3,rmax_m1/2
      TNm1UV_0(1,n0,2*n0) = TNm1UV_0aux(1,n0,2*n0)
      do r=1,rmax_m1-2*n0
        mia1 = BinomTable(r-1,N+r-3)+1
        mia2 = BinomTable(r,N+r-3)
        mia3 = BinomTable(r,N+r-2) 
        TNm1UV_0(mia1:mia3,n0,r+2*n0) = TNm1UV_0aux(1:mia2,n0,r+2*n0)
      end do
    end do
    do r=2*N-6,rmax_m1
      do n0=N-3,(r-1)/2
        do i=BinomTable(r-2*n0-1,N+r-2*n0-3),1,-1
          TNm1UV_0(i,n0,r) = -TNm1UV_0(i,n0,r-1)
          do j=2,N-1
            TNm1UV_0(i,n0,r) = TNm1UV_0(i,n0,r)  &
                             - TNm1UV_0(AddToCInd(j,i,r-2*n0-1,N-1),n0,r) 
          end do
        end do
      end do
    end do
    

    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(masses2(0))
    mm12 = elimminf2_coli(masses2(1))
    mm22 = elimminf2_coli(masses2(2))
    mm32 = elimminf2_coli(masses2(3))
    mm42 = elimminf2_coli(masses2(4))
    mm52 = elimminf2_coli(masses2(5))
    q10  = elimminf2_coli(MomInv(1))
    q21  = elimminf2_coli(MomInv(2))
    q32  = elimminf2_coli(MomInv(3))
    q43  = elimminf2_coli(MomInv(4))
    q54  = elimminf2_coli(MomInv(5))
    if (N.le.9) then
      q50  = elimminf2_coli(MomInv((N-6)*N+6))
    else
      q50 = elimminf2_coli(MomInv(4*N+1))
    end if
    q20  = elimminf2_coli(MomInv(N+1))
    q31  = elimminf2_coli(MomInv(N+2))
    q42  = elimminf2_coli(MomInv(N+3))
    q53  = elimminf2_coli(MomInv(N+4))
    if (N.le.7) then
      q40 = elimminf2_coli(MomInv((N-5)*N+5))
      q51 = elimminf2_coli(MomInv((N-5)*N+6))
    else
      q40 = elimminf2_coli(MomInv(3*N+1))
      q51 = elimminf2_coli(MomInv(3*N+2))
    end if
    q30  = elimminf2_coli(MomInv(2*N+1))
    q41  = elimminf2_coli(MomInv(2*N+2))
    q52  = elimminf2_coli(MomInv(2*N+3))
    ! write(*,*) 'q50', q50
    ! write(*,*) 'q20', q20
    ! write(*,*) 'q31', q31
    ! write(*,*) 'q42', q42
    ! write(*,*) 'q53', q53
    ! write(*,*) 'q40', q40
    ! write(*,*) 'q51', q51
    ! write(*,*) 'q30', q30
    ! write(*,*) 'q41', q41
    ! write(*,*) 'q52', q52
    
    cnt = 1
    do i=1,N-1
      ff(i) = elimminf2_coli(MomInv(cnt)) + mm02  &
            - elimminf2_coli(masses2(i))
      cnt = cnt+N-i
    end do
 
    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(5,0) = q50 - mm52 + mm02
    mx(0,1) = mx(1,0)
    mx(1,1) = 2d0*q10
    mx(2,1) = q10+q20-q21
    mx(3,1) = q10+q30-q31
    mx(4,1) = q10+q40-q41
    mx(5,1) = q10+q50-q51
    mx(0,2) = mx(2,0)
    mx(1,2) = mx(2,1)
    mx(2,2) = 2d0*q20
    mx(3,2) = q20+q30-q32
    mx(4,2) = q20+q40-q42
    mx(5,2) = q20+q50-q52
    mx(0,3) = mx(3,0)
    mx(1,3) = mx(3,1)
    mx(2,3) = mx(3,2)
    mx(3,3) = 2d0*q30
    mx(4,3) = q30+q40-q43
    mx(5,3) = q30+q50-q53
    mx(0,4) = mx(4,0)
    mx(1,4) = mx(4,1)
    mx(2,4) = mx(4,2)
    mx(3,4) = mx(4,3)
    mx(4,4) = 2d0*q40
    mx(5,4) = q40+q50-q54
    mx(0,5) = mx(5,0)
    mx(1,5) = mx(5,1)
    mx(2,5) = mx(5,2)
    mx(3,5) = mx(5,3)
    mx(4,5) = mx(5,4)
    mx(5,5) = 2d0*q50

! changed 21.06.2018
!    call chinv(6,mx,mxinv,det)

    ! determine X_(0,5)
!    do j=1,5
!      do i=1,5
!        mx0k(i,j) = mx(i,j-1)
!      end do
!    end do
    do j=1,5
      mx0k(:,j) = mx(1:5,j-1)
    end do

    det = chdet(5,mx0k)
    kbest = 5

#ifdef TNtest    
      write(*,*) 'det',5,det
#endif

    do j=5,2,-1
!      do i=1,5
!        mx0k(i,j) = mx(i,j)
!      end do
      mx0k(:,j) = mx(1:5,j)

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
#ifdef TNtest    
      write(*,*) 'det',j-1,newdet
#endif
        det = newdet
      end if
    
    end do

#ifdef TNtest    
!    kbest=5
    write(*,*) 'kbest',kbest
#endif    
  
!    do i=1,5
!      mx0k(i,1) = mx(i,1)
!      mx0k(i,kbest) = mx(i,0)
!    end do
    mx0k(:,1) = mx(1:5,1)
    mx0k(:,kbest) = mx(1:5,0)

! changed 21.06.2018
    call chinv(5,mx0k,mx0kinv,det)

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcTNred',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcTNred: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcTNred: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcTNred: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcTNred: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcTNred: q54 = ',q54
        write(nerrout_coli,fmt10) ' CalcTNred: q50 = ',q50
        write(nerrout_coli,fmt10) ' CalcTNred: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcTNred: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcTNred: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcTNred: q53 = ',q53
        write(nerrout_coli,fmt10) ' CalcTNred: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcTNred: q51 = ',q51
        write(nerrout_coli,fmt10) ' CalcTNred: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcTNred: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcTNred: q52 = ',q52
        write(nerrout_coli,fmt10) ' CalcTNred: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcTNred: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcTNred: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcTNred: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcTNred: mm42 = ',mm42
        write(nerrout_coli,fmt10) ' CalcTNred: mm52 = ',mm52
      end if
      TN = 0d0
      return
    end if

!    do i=1,5
!      mx0kinv(kbest,i) = 0d0
!    end do
    mx0kinv(kbest,:) = 0d0

!    mxinvs = sum(mxinv(0:5,0))
    do i=1,5
      mx0kinvs(i) = sum(mx0kinv(i,1:5))
    end do

    ! for alternative error estimate
    zmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    do i=1,5
      maxzmx0kinv(i) = maxval(abs(zmx0kinv(1:5,i)))
      zmx0kinvs(i) = sum(zmx0kinv(i,1:5))
    end do

    maxzmx0kinvs = maxval(abs(zmx0kinvs(1:5)))

    maxZ = maxval(abs(mx(1:5,1:5)))

    ! calculation of UV-divergent parts      
    ! TN UV-finite for n0<N-2
    ! TNuv(:,0:min(rmax/2,N-3),:) = 0d0
    
    ! PV reduction (5.10)
    do r=2*N-4,rmax
      do n0=N-2,r/2
        do cind=1,BinomTable(r-2*n0,N+r-2*n0-2)
        
          TNuv(cind,n0,r) = TNm1UV_0(cind,n0-1,r-2) + 2*mm02*TNuv(cind,n0-1,r-2)
          do i=1,Nm1
            TNuv(cind,n0,r) = TNuv(cind,n0,r)  &
                            + ff(i)*TNuv(AddToCInd(i,cind,r-2*n0,N-1),n0-1,r-1)
          end do 
          TNuv(cind,n0,r) = TNuv(cind,n0,r)/(2*(r+3-N))
    
        end do
      end do
    end do

    TN = 0d0

    ! scalar coefficient
!    based on (D.3), replaced 21.06.2018
!
!   TN(1,0,0) = -mxinv(0,0)*TNm1_0(1,0,0)
!   do k=1,5
!     TN(1,0,0) = TN(1,0,0) &
!          + mxinv(k,0)*(TNm1_i(1,0,0,k)-TNm1_0(1,0,0))
!   end do

!   TNerr(0) = max(abs(mxinvs)*TNm1err(0,0), &
!                  abs(mxinv(1,0))*TNm1err(1,0) , &
!                  abs(mxinv(2,0))*TNm1err(2,0) , &
!                  abs(mxinv(3,0))*TNm1err(3,0) , &
!                  abs(mxinv(4,0))*TNm1err(4,0) , & 
!                  abs(mxinv(5,0))*TNm1err(5,0) )

!   TNerr2(0) = max(abs(mxinvs)*TNm1err2(0,0), &
!                  abs(mxinv(1,0))*TNm1err2(1,0) , &
!                  abs(mxinv(2,0))*TNm1err2(2,0) , &
!                  abs(mxinv(3,0))*TNm1err2(3,0) , &
!                  abs(mxinv(4,0))*TNm1err2(4,0) , & 
!                  abs(mxinv(5,0))*TNm1err2(5,0) )

!   write(*,*) 'CalcTNred TN(1,0,0)',TN(1,0,0)

! New version for  TN(1,0,0), 21.06.2018

    maxf = abs(mx(1,0))
    jmax = 1
    do j=2,5
      if (abs(mx(j,0)).gt.maxf) then
         jmax = j
         maxf = abs(mx(j,0))
      end if
    end do

    mxmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    TN(1,0,0) =  TNm1_i(1,0,0,jmax) - TNm1_0(1,0,0)
    do j=1,5
      TN(1,0,0) =  TN(1,0,0) & 
          - mxmx0kinv(jmax,j) * (TNm1_i(1,0,0,j) - TNm1_0(1,0,0))
    end do
    TN(1,0,0) =  TN(1,0,0)/mx(jmax,0)

!   write(*,*) 'CalcTNred TN(1,0,0)',TN(1,0,0)

    TNerr(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*TNm1err(0,0), &
                   abs(mxmx0kinv(jmax,1))*TNm1err(1,0) , &
                   abs(mxmx0kinv(jmax,2))*TNm1err(2,0) , &
                   abs(mxmx0kinv(jmax,3))*TNm1err(3,0) , &
                   abs(mxmx0kinv(jmax,4))*TNm1err(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*TNm1err(5,0) , &
                   TNm1err(0,0) , TNm1err(jmax,0) )/abs(mx(jmax,0))

    TNerr2(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*TNm1err2(0,0), &
                   abs(mxmx0kinv(jmax,1))*TNm1err2(1,0) , &
                   abs(mxmx0kinv(jmax,2))*TNm1err2(2,0) , &
                   abs(mxmx0kinv(jmax,3))*TNm1err2(3,0) , &
                   abs(mxmx0kinv(jmax,4))*TNm1err2(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*TNm1err2(5,0) , &
                   TNm1err2(0,0),TNm1err2(jmax,0))/abs(mx(jmax,0))

#ifdef TNtest
          write(*,*) 'Tnred TN(1,0,0)', TN(1,0,0)
          write(*,*) 'Tnred TNm1_0(1,0,0)', TNm1_0(1,0,0)
          write(*,*) 'Tnred TNm1_i(1,0,0,1)', TNm1_i(1,0,0,1)
          write(*,*) 'Tnred TNm1_i(1,0,0,2)', TNm1_i(1,0,0,2)
          write(*,*) 'Tnred TNm1_i(1,0,0,3)', TNm1_i(1,0,0,3)
          write(*,*) 'Tnred TNm1_i(1,0,0,4)', TNm1_i(1,0,0,4)
          write(*,*) 'Tnred TNm1_i(1,0,0,5)', TNm1_i(1,0,0,5)
#endif

!    Alternative new version, 21.06.2018
!
!    Xadjk0 = -chdet(5,mx0k)
!    TN(1,0,0) = 0d0
!    do m=1,5
!      do i=1,5
!        do j=1,5
!          if(i.lt.kbest.and.j.lt.m) then
!            mat(i,j) = mx(i,j)
!          else if(i.lt.kbest.and.j.gt.m) then
!            mat(i,j-1) = mx(i,j)
!          else if(i.gt.kbest.and.j.lt.m) then
!            mat(i-1,j) = mx(i,j)
!          else if(i.gt.kbest.and.j.gt.m) then
!            mat(i-1,j-1) = mx(i,j)
!          end if
!        end do
!      end do
!       
!      Zadjk(m)  = chdet(4,mat)
!      if(mod(kbest+m,2).eq.1)  Zadjk(m) = - Zadjk(m) 
!      TN(1,0,0) = TN(1,0,0) &
!           - Zadjk(m)/Xadjk0*(TNm1_i(1,0,0,m)-TNm1_0(1,0,0))
!    end do
!
!    TNerr(0) = max(maxval(abs(Zadjk(:)/Xadjk0))*TNm1err(0,0), &
!                   abs(Zadjk(1)/Xadjk0)*TNm1err(1,0) , &
!                   abs(Zadjk(2)/Xadjk0)*TNm1err(2,0) , &
!                   abs(Zadjk(3)/Xadjk0)*TNm1err(3,0) , &
!                   abs(Zadjk(4)/Xadjk0)*TNm1err(4,0) , & 
!                   abs(Zadjk(5)/Xadjk0)*TNm1err(5,0) )
!
!    TNerr2(0) = max(maxval(abs(Zadjk(:)/Xadjk0))*TNm1err2(0,0), &
!                   abs(Zadjk(1)/Xadjk0)*TNm1err2(1,0) , &
!                   abs(Zadjk(2)/Xadjk0)*TNm1err2(2,0) , &
!                   abs(Zadjk(3)/Xadjk0)*TNm1err2(3,0) , &
!                   abs(Zadjk(4)/Xadjk0)*TNm1err2(4,0) , & 
!                   abs(Zadjk(5)/Xadjk0)*TNm1err2(5,0) )


    if (rmax.eq.0) return


    ! formula (7.13) extended to arbitrary N
    do r=0,rmax_m1
      do n0=0,max((r-N+6)/2,0)
        bino_m1 = BinomTable(r-2*n0,N+r-2*n0-2)
        do i=1,bino_m1
          inds = CalcCIndArr(Nm1,r-2*n0,i)
            
          do m=1,5
            S(m) = -TNm1_0(i,n0,r)
          end do

#ifdef TNtest
          write(*,*) 'Tnred s(m)',s
#endif

          if (inds(1).eq.0) then
            S(1) = S(1) + TNm1_i(DropCInd(1,i,r-2*n0,Nm1),n0,r,1)
          end if
          if (inds(2).eq.0) then
            S(2) = S(2) + TNm1_i(DropCInd(2,i,r-2*n0,Nm1),n0,r,2)
          end if
          if (inds(3).eq.0) then
            S(3) = S(3) + TNm1_i(DropCInd(3,i,r-2*n0,Nm1),n0,r,3)
          end if
          if (inds(4).eq.0) then
            S(4) = S(4) + TNm1_i(DropCInd(4,i,r-2*n0,Nm1),n0,r,4)
          end if
          if (inds(5).eq.0) then
            S(5) = S(5) + TNm1_i(DropCInd(5,i,r-2*n0,Nm1),n0,r,5)
          end if

#ifdef TNtest
          write(*,*) 'Tnred s(m)',s
#endif

          do k=1,5
            TNaux = mx0kinv(k,1)*S(1)+mx0kinv(k,2)*S(2) &
                  + mx0kinv(k,3)*S(3)+mx0kinv(k,4)*S(4)+mx0kinv(k,5)*S(5)
            iaux = AddToCInd(k,i,r,N-1)
            TN(iaux,n0,r+1) = TN(iaux,n0,r+1) + (inds(k)+1)*TNaux/(r+1)           
#ifdef TNtest
            write(*,*) 'Tnred comb',k,mx0kinv(k,1)*S(1),mx0kinv(k,2)*S(2) &
                  , mx0kinv(k,3)*S(3),mx0kinv(k,4)*S(4),mx0kinv(k,5)*S(5), TNaux

            if(abs(TNaux).ne.0d0) then
              write(*,*) 'Tnred cancel', k, max(abs(mx0kinv(k,1)*S(1)),abs(mx0kinv(k,2)*S(2)) &
                  , abs(mx0kinv(k,3)*S(3)),abs(mx0kinv(k,4)*S(4)),abs(mx0kinv(k,5)*S(5))) &
                  /abs(TNaux)
            endif
#endif
            
          end do



        end do
      end do

      if (r.le.rmax-1) then  

        TNerr(r+1) = max( maxval(abs(mx0kinvs(1:5)))*TNm1err(0,r), &
                   maxval(abs(mx0kinv(1:5,1)))*TNm1err(1,r) , &
                   maxval(abs(mx0kinv(1:5,2)))*TNm1err(2,r) , &
                   maxval(abs(mx0kinv(1:5,3)))*TNm1err(3,r) , &
                   maxval(abs(mx0kinv(1:5,4)))*TNm1err(4,r) , &
                   maxval(abs(mx0kinv(1:5,5)))*TNm1err(5,r) )

        TNerr2(r+1) = max( abs(maxzmx0kinvs)*TNm1err2(0,r), &
                   abs(maxzmx0kinv(1))*TNm1err2(1,r) , &
                   abs(maxzmx0kinv(2))*TNm1err2(2,r) , &
                   abs(maxzmx0kinv(3))*TNm1err2(3,r) , &
                   abs(maxzmx0kinv(4))*TNm1err2(4,r) , &
                   abs(maxzmx0kinv(5))*TNm1err2(5,r) )/maxZ


        if (Mode_coli.lt.1) then
          gramdet = chdet(5,mx(1:5,1:5))

#ifdef TNtest
          write(*,*)
          do i = 1,5
            do j = 1,5
              write(*,fmt999)  i,j,mx(i,j)
            enddo
          enddo
          write(*,*) 'TNred relGramdet=',gramdet/det
#endif

          if (max(abs(TN(1,0,0)),maxval(abs(TN(1:5,0,1))))*abs(gramdet/det).gt. &
              TNerr(r+1)) then

            TNerr(r+1)= max(TNerr(r+1),   &
                max(abs(TN(1,0,0)),maxval(abs(TN(1:5,0,1))))*abs(gramdet/det) )
            TNerr2(r+1)= max(TNerr2(r+1),   &
                max(abs(TN(1,0,0)),maxval(abs(TN(1:5,0,1))))*abs(gramdet/det) )

            if (abs(gramdet/det).gt.reqacc_coli) then
              call SetErrFlag_coli(-6)
              call ErrOut_coli('CalcTNred',  &
                  'input momenta inconsistent! (not 4-dimensional)',  &
                  errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,fmt10) ' CalcTNred: q10 = ',q10
                write(nerrout_coli,fmt10) ' CalcTNred: q21 = ',q21
                write(nerrout_coli,fmt10) ' CalcTNred: q32 = ',q32
                write(nerrout_coli,fmt10) ' CalcTNred: q43 = ',q43
                write(nerrout_coli,fmt10) ' CalcTNred: q54 = ',q54
                write(nerrout_coli,fmt10) ' CalcTNred: q50 = ',q50
                write(nerrout_coli,fmt10) ' CalcTNred: q20 = ',q10
                write(nerrout_coli,fmt10) ' CalcTNred: q31 = ',q31
                write(nerrout_coli,fmt10) ' CalcTNred: q42 = ',q42
                write(nerrout_coli,fmt10) ' CalcTNred: q53 = ',q53
                write(nerrout_coli,fmt10) ' CalcTNred: q40 = ',q40
                write(nerrout_coli,fmt10) ' CalcTNred: q51 = ',q51
                write(nerrout_coli,fmt10) ' CalcTNred: q30 = ',q30
                write(nerrout_coli,fmt10) ' CalcTNred: q41 = ',q41
                write(nerrout_coli,fmt10) ' CalcTNred: q52 = ',q52
                write(nerrout_coli,fmt10) ' CalcTNred: mm02 = ',mm02
                write(nerrout_coli,fmt10) ' CalcTNred: mm12 = ',mm12
                write(nerrout_coli,fmt10) ' CalcTNred: mm22 = ',mm22   
                write(nerrout_coli,fmt10) ' CalcTNred: mm32 = ',mm32   
                write(nerrout_coli,fmt10) ' CalcTNred: mm42 = ',mm42   
                write(nerrout_coli,fmt10) ' CalcTNred: mm52 = ',mm52   
                write(nerrout_coli,fmt10) ' CalcTNred: gram = ',gramdet/det  
              end if
            end if

          end if

        end if
      end if


#ifdef TNtest
            write(*,*) 'Tnred = ',TN
            write(*,*) 'Tnred mn err',TNm1err(1:5,r)
            write(*,*) 'Tnred coef err', (maxval(abs(mx0kinv(1:5,k))),k=1,5)
            write(*,*) 'Tnred err',TNerr(r+1)
#endif

    end do
                       

  end subroutine CalcTNred





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function SubMasses
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function SubMasses(N,k,masses)

    integer :: N,k,i
    double complex :: masses(0:N-1), SubMasses(0:N-2)

!     write(*,*) 'SubMasses in ', N,k

    if ((k.gt.N-1).or.(k.lt.0)) then
      write(nerrout_coli,*) 'SubMasses:'
      write(nerrout_coli,*) 'inkonsistent argument k', k
      write(nerrout_coli,*) 0, '<= k <=', N-1, 'required!'
      stop
    end if

    do i=0,N-2
      if (i.lt.k) then
        SubMasses(i) = masses(i)
      else
        SubMasses(i) = masses(i+1)
      end if
    end do    

  end function





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function SubMomVec
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function SubMomVec(N,k,MomVec)

    integer :: N,k,i
    double complex :: MomVec(0:3,N-1), SubMomVec(0:3,N-2)

    if ((k.gt.N-1).or.(k.lt.0)) then
      write(nerrout_coli,*) 'SubMomVec:'
      write(nerrout_coli,*) 'inkonsistent argument k', k
      write(nerrout_coli,*) 0, '<= k <=', N-1, 'required!'
      stop
    end if

    if (k.eq.0) then
      do i=1,N-2
        SubMomVec(0:3,i) = MomVec(0:3,i+1)-MomVec(0:3,1)
      end do

    else
      do i=1,N-2
        if (i.lt.k) then
          SubMomVec(0:3,i) = MomVec(0:3,i)
        else
          SubMomVec(0:3,i) = MomVec(0:3,i+1)
        end if
      end do

    end if    

  end function





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function SubMomInv
  !  ******************************************
  !  *    rewritten by Ansgar Denner 25.03.15 *
  !  ******************************************
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function SubMomInv(N,k,MomInv)

    integer, intent(in) :: N,k
    integer :: i,cnt,moms,limit
    double complex :: MomInv(BinomTable(2,N)), SubMomInv(BinomTable(2,N-1))

!     write(*,*) 'SubMomInv in ', N,k

    if ((k.gt.N-1).or.(k.lt.0)) then
      write(nerrout_coli,*) 'SubMomInv:'
      write(nerrout_coli,*) 'inkonsistent argument k', k
      write(nerrout_coli,*) 0, '<= k <=', N-1, 'required!'
      stop
    end if

    if (N.eq.2) return

    cnt = 1
    do moms=1,(N-3)/2
      do i=1,N
        if (i.ne.k+1) then
          if ((k.ge.i).and.(i.ge.k-moms+1).or.(N+k.ge.i).and.(i.ge.N+k-moms+1)) then
            SubMomInv(cnt) = MomInv(N*moms+i)
          else
            SubMomInv(cnt) = MomInv(N*(moms-1)+i)
          end if

          cnt = cnt+1
        end if
      end do
    end do

    if (mod(N,2).eq.1) then
      moms = (N-1)/2
      if(k.le.(N-1)/2) then 
        limit = (N+1)/2
      else
        limit = (N-1)/2
      end if
      do i=1,limit
        if (i.ne.k+1) then
          if ((k.ge.i).and.(i.ge.k-moms+1).or.(N+k.ge.i).and.(i.ge.N+k-moms+1)) then
            SubMomInv(cnt) = MomInv(N*moms+i-(N-1)/2)
          else
            SubMomInv(cnt) = MomInv(N*(moms-1)+i)
          end if

          cnt = cnt+1
        end if
      end do

    else
      do i=1,N
        if (i.ne.k+1) then
          moms = (N/2-1)
          if ((k.ge.i).and.(i.ge.k-moms+1).or.(N+k.ge.i).and.(i.ge.N+k-moms+1)) then
            if (i.gt.N/2) then
              SubMomInv(cnt) = MomInv(N*moms-N/2+i)
            else
              SubMomInv(cnt) = MomInv(N*moms+i)
            endif
          else
            SubMomInv(cnt) = MomInv(N*(moms-1)+i)
          end if

          cnt = cnt+1
        end if
      end do

    end if  


  end function





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  function CalcCIndArr
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  function CalcCIndArr(Nm1,r,cind) result(arr)
!
!    integer :: Nm1, r, cind, arr(Nm1), i
!    integer :: combis(r)    
!
!    if (r.ge.1) then
!      combis = IndCombisEq(1:r,cind,r,Nm1)
!    end if
!    arr = 0
!    do i=1,r
!      arr(combis(i)) = arr(combis(i))+1
!    end do    
!
!  end function CalcCIndArr





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  subroutine SetDropCInd(Nm1,r)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine SetDropCInd(Nm1,r)
!
!    integer :: Nm1,r,i,j,k
!
!    if (allocated(DropCInd)) then
!      deallocate(DropCInd)
!    end if
!    allocate(DropCInd(Nm1,BinomTable(r,r+Nm1-1),0:r,Nm1))
!
!    do i=1,Nm1
!      DropCInd(1:i,1,0,i) = 1
!      do j=1,r
!        do k=1,i
!          DropCind(k,1:BinomTable(j,j+i-1),j,i) = CalcDropCInd(i,j,k)
!        end do
!      end do
!    end do
!
!  end subroutine SetDropCInd





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  function CalcDropCInd(Nm1,r,k)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  function CalcDropCInd(Nm1,r,k) result(dcinds)
!
!    integer :: Nm1,r,k,dcinds(BinomTable(r,r+Nm1-1))
!    integer :: cnt,arr(Nm1),i
!
!    cnt = 1
!    do i=1,BinomTable(r,r+Nm1-1)
!      arr = CalcCIndArr(Nm1,r,i)
!      if (arr(k).eq.0) then
!        dcinds(i) = cnt
!        cnt = cnt+1
!      else
!        dcinds(i) = 0
!      end if
!    end do
!
!  end function CalcDropCInd





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  subroutine SetAddToCInd(Nm1,r)
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  subroutine SetAddToCInd(Nm1,r)
!
!    integer, intent(in) :: Nm1,r
!    integer :: i,j,k,m
!
!    if (allocated(AddToCInd)) then
!      deallocate(AddToCInd)
!    end if
!    allocate(AddToCInd(Nm1,BinomTable(r,Nm1+r-1),0:r,Nm1))
!    AddToCInd = 0
!
!    do i=1,Nm1
!      do j=0,r
!        do k=1,BinomTable(j,i+j-1)
!          do m=1,i
!            AddToCInd(m,k,j,i) = CalcAddToCInd(i,j,k,m)
!          end do
!        end do
!      end do
!    end do
!
!  end subroutine SetAddToCInd





!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  !  function CalcAddToCInd
!  !
!  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  function CalcAddToCInd(Nm1,r,cind,k) result(newcind)
!
!    integer :: Nm1,r,k,cind,newcind,i
!    integer :: indArr(r),newindArr(r+1)
!    logical :: kflag
!
!    if ((r.eq.0).and.(cind.eq.1)) then
!      newcind = k
!      return
!    end if
!
!    indArr = IndCombisEq(1:r,cind,r,Nm1)
!    
!    kflag = .true.
!    do i=1,r
!      if (indArr(i).ge.k) then
!        newindArr(i+1) = indArr(i)
!        if (kflag) then
!          newindArr(i) = k
!          kflag = .false.
!        end if
!      else
!        newindArr(i) = indArr(i)
!      end if
!    end do
!    if (kflag) then
!      newindArr(r+1) = k
!    end if
!
!    newcind = CalcPosIndCombisEq(Nm1,r+1,newindArr)
!      
!  end function CalcAddToCInd


end module reductionTN
