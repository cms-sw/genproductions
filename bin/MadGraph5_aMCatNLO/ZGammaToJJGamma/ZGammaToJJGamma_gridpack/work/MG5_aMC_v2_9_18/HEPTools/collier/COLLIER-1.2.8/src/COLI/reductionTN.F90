!!
!!  File reductionTN.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!#define TNtest
!#define TESTPERM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ************************
!  *  module reductionTN  *
!  *    by Lars Hofer     *
!  ************************
!
!  functions and subroutines:
!  CalcTN, CalcTNint, CalcTNred
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
    integer :: r,n0,n1,n2,n3,n4,i,cnt


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
    integer :: r,n0,n1,n2,n3,n4,i,rank,bino,cnt
    logical :: nocalc,wrica



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
  !  version with permutation of momenta for TN reduction AD 30.03.2023
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine CalcTNred(TN,TNuv,MomInv,masses2,N,rmax,id,TNerr,TNerr2)

    integer, intent(in) :: N,rmax,id
    double complex, intent(in) :: MomInv(BinomTable(2,N)), masses2(0:N-1)
    double precision, intent(out) :: TNerr(0:rmax),TNerr2(0:rmax)
    double complex :: q(0:N-1,0:N-1),mm2(0:N-1)
    double complex :: m6xt(0:5,0:5),m6x0kt(5,5)
    double complex :: m6x(0:5,0:5),m6x0k(5,5),m6x0kinv(5,5)
    double complex :: mx(0:N-1,0:N-1),ff(N-1)
    double complex :: det,newdet,TNaux,m6x0kinvs(5),dett
    double complex :: zm6x0kinv(5,5),zm6x0kinvs(5)
    double precision :: maxZ,maxzm6x0kinv(5),maxzm6x0kinvs
    double complex, intent(inout) :: TN(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex, intent(inout) :: TNuv(BinomTable(rmax,N+rmax-2),0:rmax/2,0:rmax)
    double complex, allocatable :: TNm1_0(:,:,:), TNm1UV_0(:,:,:), TNm1_i(:,:,:,:)
    double complex, allocatable :: TNm1_0aux(:,:,:), TNm1UV_0aux(:,:,:), TNm1UV_i(:,:,:,:)
    double complex :: S(5), elimminf2_coli,chdet,gramdet
    double precision :: TNm1err(0:5,0:rmax),TNm1err2(0:5,0:rmax)
    integer :: r,n0,k,i,j,m,nid(0:N-1),bin,kbest,Nm1,inds(N-1)
    integer :: bino_0,bino_i,bino_m1,iaux,rmax_m1,cind
    integer :: mia1,mia2,mia3
    integer :: pick(1:5),pickt(1:5),Ntry
    integer :: kbestt
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A18,'(',d25.18,' , ',d25.18,' )')"

#ifdef TNtest
    character(len=*),parameter :: fmt999 = "(' gm(',i1,',',i1,') =  ',d23.16,' , ',d23.16)"
#endif

!    double complex :: Xadjk0,Zadjk(5),mat(4,4),m6xm6x0kinv(5,5),m6xm6x0kinvs(5)
    double complex :: m6xm6x0kinv(5,5)
    double precision :: maxf
    integer        :: jmax,l !,i1,i2

#ifdef TNtest
    write(*,*) 'CalcTNred in N',N,rmax
    write(*,*) 'CalcTNred in s',MomInv
    write(*,*) 'CalcTNred in m',masses2
#endif

    ! determine inverse modified Caley matrix
    do i=0,N-1
       mm2(i) = elimminf2_coli(masses2(i))
    enddo

! Rewrite input invariants into matrix
!    do k=1,N*(N-1)/2
!      j = mod(k-1,N)   !  entry j+1
!      i = (k-1)/N+1    !  series i+1
!      i1=max(j,modulo(i+j,N))
!      i2=min(j,modulo(i+j,N))
!      q(i1,i2) = elimminf2_coli(MomInv(k))
!    enddo

    do i= 1,(N-1)/2
       do j=0,N-1-i
          q(j+i,j) = elimminf2_coli(MomInv(N*(i-1)+j+1))
       enddo
       do j=N-i,N-1
          q(j,j+i-N) = elimminf2_coli(MomInv(N*(i-1)+j+1))
       enddo
    enddo
    if((N/2)*2 == N) then
       i = N/2
       do j=0,N/2-1
          q(j+i,j) = elimminf2_coli(MomInv(N*(i-1)+j+1))
       enddo
    endif

    mx(0,0) = 2d0*mm2(0)
    do i=1,N-1
      ff(i) = q(i,0) + mm2(0) - mm2(i)
      mx(i,i) = 2*q(i,0)
      mx(i,0) = ff(i)
      mx(0,i) = ff(i)
      do j=1,i-1
         mx(i,j) = q(i,0) + q(j,0) - q(i,j)
         mx(j,i) = mx(i,j)
      enddo
    end do

#ifdef TESTPERM
! for testing of permutation only

    if (N>6) then
       pick = [2,3,4,5,6]
    else
       pick = [2,3,4,5,1]
    endif
!    pick = [1,2,3,4,5]
!    pick = [2,3,4,5,1]
    l=1

!   write(*,*) 'pick = ',pick

    m6x(0,0) = mx(0,0)
    do i=1,5
       m6x(i,i) = mx(pick(i),pick(i))
       m6x(i,0) = mx(pick(i),0)
       m6x(0,i) = m6x(i,0)
       do j=1,i-1
          m6x(i,j) = mx(pick(i),pick(j))
          m6x(j,i) = m6x(i,j)
       enddo
    end do

! determine X_(0,5)
      do j=1,5
         m6x0k(:,j) = m6x(1:5,j-1)
      end do

      det = chdet(5,m6x0k)
      kbest = 5

#ifdef TNtest
      write(*,*) 'TN det1 = ',l,det
#endif

      do j=5,2,-1
         m6x0k(:,j) = m6x(1:5,j)

         newdet =  chdet(5,m6x0k)
         if (abs(newdet).gt.abs(det)) then
            kbest = j-1
#ifdef TNtest
            write(*,*) 'TN: det',l,j-1,newdet
#endif
            det = newdet
         end if

      end do

#ifdef TNtest
      write(*,*) 'kbest',kbest
#endif

      m6x0k(:,1) = m6x(1:5,1)
      m6x0k(:,kbest) = m6x(1:5,0)

#else
!  this is the default  (no TESTPERM)

    m6xt(0,0) = mx(0,0)
    det = 0d0
! try different subsets of momenta (using cyclic permuations only)
    if(N == 6) then     ! no permutation of 5 momenta
      Ntry = 1
    else
      Ntry = N-1
    endif
    do l=0,Ntry-1
! pick 5 momenta out of the set of N-1 momenta
      pickt=[1+mod(l,N-1),1+mod(l+1,N-1),1+mod(l+2,N-1),1+mod(l+3,N-1),1+mod(l+4,N-1)]

!      write(*,*) 'pickt = ',pickt

! m6xt is 6x6 test matrix according to (7.8)
      do i=1,5
         m6xt(i,i) = mx(pickt(i),pickt(i))
         m6xt(i,0) = mx(pickt(i),0)
         m6xt(0,i) = m6xt(i,0)
         do j=1,i-1
            m6xt(i,j) = mx(pickt(i),pickt(j))
            m6xt(j,i) = m6xt(i,j)
         enddo
      end do


! determine X_(0,k)
      do j=1,5
         m6x0kt(:,j) = m6xt(1:5,j-1)
      end do

      dett = chdet(5,m6x0kt)
      kbestt = 5

#ifdef TNtest
      write(*,*) 'TN det1 = ',l,dett
#endif

      do j=5,2,-1
         m6x0kt(:,j) = m6xt(1:5,j)

         newdet =  chdet(5,m6x0kt)
         if (abs(newdet).gt.abs(dett)) then
            kbestt = j-1
#ifdef TNtest
            write(*,*) 'TN: det',l,j-1,newdet
#endif
            dett = newdet
         end if

      end do

      m6x0kt(:,1) = m6xt(1:5,1)
      m6x0kt(:,kbestt) = m6xt(1:5,0)

! if new permutation has smaller determinant, save values for later
      if (abs(dett).gt.abs(det)) then
         kbest = kbestt
         pick = pickt
         det = dett
         m6x = m6xt
         m6x0k = m6x0kt
      endif
   enddo

#endif /*TESTPERM*/

!    write(*,*) 'det = ',det
!    write(*,*) 'm6x = ',m6x
!    write(*,*) 'm6x0k = ',m6x0k


! changed 21.06.2018
    call chinv(5,m6x0k,m6x0kinv,det)

!    write(*,*) 'm6x0kinv = ',m6x0kinv

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcTNred',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcTNred: q10 = ',q(1,0)
        write(nerrout_coli,fmt10) ' CalcTNred: q21 = ',q(2,1)
        write(nerrout_coli,fmt10) ' CalcTNred: q32 = ',q(3,2)
        write(nerrout_coli,fmt10) ' CalcTNred: q43 = ',q(4,3)
        write(nerrout_coli,fmt10) ' CalcTNred: q54 = ',q(5,4)
        write(nerrout_coli,fmt10) ' CalcTNred: q50 = ',q(5,0)
        write(nerrout_coli,fmt10) ' CalcTNred: q20 = ',q(1,0)
        write(nerrout_coli,fmt10) ' CalcTNred: q31 = ',q(3,1)
        write(nerrout_coli,fmt10) ' CalcTNred: q42 = ',q(4,2)
        write(nerrout_coli,fmt10) ' CalcTNred: q53 = ',q(5,3)
        write(nerrout_coli,fmt10) ' CalcTNred: q40 = ',q(4,0)
        write(nerrout_coli,fmt10) ' CalcTNred: q51 = ',q(5,1)
        write(nerrout_coli,fmt10) ' CalcTNred: q30 = ',q(3,0)
        write(nerrout_coli,fmt10) ' CalcTNred: q41 = ',q(4,1)
        write(nerrout_coli,fmt10) ' CalcTNred: q52 = ',q(5,2)
        write(nerrout_coli,fmt10) ' CalcTNred: mm02 = ',mm2(0)
        write(nerrout_coli,fmt10) ' CalcTNred: mm12 = ',mm2(1)
        write(nerrout_coli,fmt10) ' CalcTNred: mm22 = ',mm2(2)
        write(nerrout_coli,fmt10) ' CalcTNred: mm32 = ',mm2(3)
        write(nerrout_coli,fmt10) ' CalcTNred: mm42 = ',mm2(4)
        write(nerrout_coli,fmt10) ' CalcTNred: mm52 = ',mm2(5)
      end if
      TN = 0d0
      return
    end if

    m6x0kinv(kbest,:) = 0d0

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
    do while (k.le.N-1)
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
      call CalcTNint(TNm1_i(:,:,:,k),TNm1UV_i(:,:,:,k),SubMomInv(N,pick(k),MomInv),  &
          SubMasses(N,pick(k),masses2),Nm1,rmax_m1,nid(pick(k)),        &
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

    do i=1,5
      m6x0kinvs(i) = sum(m6x0kinv(i,1:5))
    end do

    ! for alternative error estimate
    zm6x0kinv = matmul(m6x(1:5,1:5),m6x0kinv)

    do i=1,5
      maxzm6x0kinv(i) = maxval(abs(zm6x0kinv(1:5,i)))
      zm6x0kinvs(i) = sum(zm6x0kinv(i,1:5))
    end do

    maxzm6x0kinvs = maxval(abs(zm6x0kinvs(1:5)))

    maxZ = maxval(abs(m6x(1:5,1:5)))

    ! calculation of UV-divergent parts
    ! TN UV-finite for n0<N-2
    ! TNuv(:,0:min(rmax/2,N-3),:) = 0d0

    ! PV reduction (5.10)
    do r=2*N-4,rmax
      do n0=N-2,r/2
        do cind=1,BinomTable(r-2*n0,N+r-2*n0-2)

          TNuv(cind,n0,r) = TNm1UV_0(cind,n0-1,r-2) + 2*mm2(0)*TNuv(cind,n0-1,r-2)
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
!   TN(1,0,0) = -m6xinv(0,0)*TNm1_0(1,0,0)
!   do k=1,5
!     TN(1,0,0) = TN(1,0,0) &
!          + m6xinv(k,0)*(TNm1_i(1,0,0,k)-TNm1_0(1,0,0))
!   end do

!   TNerr(0) = max(abs(m6xinvs)*TNm1err(0,0), &
!                  abs(m6xinv(1,0))*TNm1err(1,0) , &
!                  abs(m6xinv(2,0))*TNm1err(2,0) , &
!                  abs(m6xinv(3,0))*TNm1err(3,0) , &
!                  abs(m6xinv(4,0))*TNm1err(4,0) , &
!                  abs(m6xinv(5,0))*TNm1err(5,0) )

!   TNerr2(0) = max(abs(m6xinvs)*TNm1err2(0,0), &
!                  abs(m6xinv(1,0))*TNm1err2(1,0) , &
!                  abs(m6xinv(2,0))*TNm1err2(2,0) , &
!                  abs(m6xinv(3,0))*TNm1err2(3,0) , &
!                  abs(m6xinv(4,0))*TNm1err2(4,0) , &
!                  abs(m6xinv(5,0))*TNm1err2(5,0) )

! New version for  TN(1,0,0), 21.06.2018

    maxf = abs(m6x(1,0))
    jmax = 1
    do j=2,5
      if (abs(m6x(j,0)).gt.maxf) then
         jmax = j
         maxf = abs(m6x(j,0))
      end if
    end do

    m6xm6x0kinv = matmul(m6x(1:5,1:5),m6x0kinv)

    TN(1,0,0) =  TNm1_i(1,0,0,jmax) - TNm1_0(1,0,0)
    do j=1,5
      TN(1,0,0) =  TN(1,0,0) &
          - m6xm6x0kinv(jmax,j) * (TNm1_i(1,0,0,j) - TNm1_0(1,0,0))
    end do
    TN(1,0,0) =  TN(1,0,0)/m6x(jmax,0)

!   write(*,*) 'CalcTNred TN(1,0,0)',TN(1,0,0)

    TNerr(0) = max(maxval(abs(m6xm6x0kinv(jmax,:)))*TNm1err(0,0), &
                   abs(m6xm6x0kinv(jmax,1))*TNm1err(1,0) , &
                   abs(m6xm6x0kinv(jmax,2))*TNm1err(2,0) , &
                   abs(m6xm6x0kinv(jmax,3))*TNm1err(3,0) , &
                   abs(m6xm6x0kinv(jmax,4))*TNm1err(4,0) , &
                   abs(m6xm6x0kinv(jmax,5))*TNm1err(5,0) , &
                   TNm1err(0,0) , TNm1err(jmax,0) )/abs(m6x(jmax,0))

    TNerr2(0) = max(maxval(abs(m6xm6x0kinv(jmax,:)))*TNm1err2(0,0), &
                   abs(m6xm6x0kinv(jmax,1))*TNm1err2(1,0) , &
                   abs(m6xm6x0kinv(jmax,2))*TNm1err2(2,0) , &
                   abs(m6xm6x0kinv(jmax,3))*TNm1err2(3,0) , &
                   abs(m6xm6x0kinv(jmax,4))*TNm1err2(4,0) , &
                   abs(m6xm6x0kinv(jmax,5))*TNm1err2(5,0) , &
                   TNm1err2(0,0),TNm1err2(jmax,0))/abs(m6x(jmax,0))

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
!    Xadjk0 = -chdet(5,m6x0k)
!    TN(1,0,0) = 0d0
!    do m=1,5
!      do i=1,5
!        do j=1,5
!          if(i.lt.kbest.and.j.lt.m) then
!            mat(i,j) = m6x(i,j)
!          else if(i.lt.kbest.and.j.gt.m) then
!            mat(i,j-1) = m6x(i,j)
!          else if(i.gt.kbest.and.j.lt.m) then
!            mat(i-1,j) = m6x(i,j)
!          else if(i.gt.kbest.and.j.gt.m) then
!            mat(i-1,j-1) = m6x(i,j)
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

          if (inds(pick(1)).eq.0) then
            S(1) = S(1) + TNm1_i(DropCInd(pick(1),i,r-2*n0,Nm1),n0,r,1)
          end if
          if (inds(pick(2)).eq.0) then
            S(2) = S(2) + TNm1_i(DropCInd(pick(2),i,r-2*n0,Nm1),n0,r,2)
          end if
          if (inds(pick(3)).eq.0) then
            S(3) = S(3) + TNm1_i(DropCInd(pick(3),i,r-2*n0,Nm1),n0,r,3)
          end if
          if (inds(pick(4)).eq.0) then
            S(4) = S(4) + TNm1_i(DropCInd(pick(4),i,r-2*n0,Nm1),n0,r,4)
          end if
          if (inds(pick(5)).eq.0) then
            S(5) = S(5) + TNm1_i(DropCInd(pick(5),i,r-2*n0,Nm1),n0,r,5)
          end if

          do k=1,5
            TNaux = m6x0kinv(k,1)*S(1)+m6x0kinv(k,2)*S(2) &
                  + m6x0kinv(k,3)*S(3)+m6x0kinv(k,4)*S(4)+m6x0kinv(k,5)*S(5)
            iaux = AddToCInd(pick(k),i,r,N-1)
            TN(iaux,n0,r+1) = TN(iaux,n0,r+1) + (inds(pick(k))+1)*TNaux/(r+1)
#ifdef TNtest
            write(*,*) 'Tnred comb',k,m6x0kinv(k,1)*S(1),m6x0kinv(k,2)*S(2) &
                  , m6x0kinv(k,3)*S(3),m6x0kinv(k,4)*S(4),m6x0kinv(k,5)*S(5), TNaux

            if(abs(TNaux).ne.0d0) then
              write(*,*) 'Tnred cancel', k, max(abs(m6x0kinv(k,1)*S(1)),abs(m6x0kinv(k,2)*S(2)) &
                  , abs(m6x0kinv(k,3)*S(3)),abs(m6x0kinv(k,4)*S(4)),abs(m6x0kinv(k,5)*S(5))) &
                  /abs(TNaux)
            endif
#endif
          end do

        end do
      end do

      if (r.le.rmax-1) then

        TNerr(r+1) = max( maxval(abs(m6x0kinvs(1:5)))*TNm1err(0,r), &
                   maxval(abs(m6x0kinv(1:5,1)))*TNm1err(1,r) , &
                   maxval(abs(m6x0kinv(1:5,2)))*TNm1err(2,r) , &
                   maxval(abs(m6x0kinv(1:5,3)))*TNm1err(3,r) , &
                   maxval(abs(m6x0kinv(1:5,4)))*TNm1err(4,r) , &
                   maxval(abs(m6x0kinv(1:5,5)))*TNm1err(5,r) )

        TNerr2(r+1) = max( abs(maxzm6x0kinvs)*TNm1err2(0,r), &
                   abs(maxzm6x0kinv(1))*TNm1err2(1,r) , &
                   abs(maxzm6x0kinv(2))*TNm1err2(2,r) , &
                   abs(maxzm6x0kinv(3))*TNm1err2(3,r) , &
                   abs(maxzm6x0kinv(4))*TNm1err2(4,r) , &
                   abs(maxzm6x0kinv(5))*TNm1err2(5,r) )/maxZ


        if (Mode_coli.lt.1) then
          gramdet = chdet(5,m6x(1:5,1:5))

#ifdef TNtest
          write(*,*)
          do i = 1,5
            do j = 1,5
              write(*,fmt999)  i,j,m6x(i,j)
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
                write(nerrout_coli,fmt10) ' CalcTNred: q10 = ',q(1,0)
                write(nerrout_coli,fmt10) ' CalcTNred: q21 = ',q(2,1)
                write(nerrout_coli,fmt10) ' CalcTNred: q32 = ',q(3,2)
                write(nerrout_coli,fmt10) ' CalcTNred: q43 = ',q(4,3)
                write(nerrout_coli,fmt10) ' CalcTNred: q54 = ',q(5,4)
                write(nerrout_coli,fmt10) ' CalcTNred: q50 = ',q(5,0)
                write(nerrout_coli,fmt10) ' CalcTNred: q20 = ',q(1,0)
                write(nerrout_coli,fmt10) ' CalcTNred: q31 = ',q(3,1)
                write(nerrout_coli,fmt10) ' CalcTNred: q42 = ',q(4,2)
                write(nerrout_coli,fmt10) ' CalcTNred: q53 = ',q(5,3)
                write(nerrout_coli,fmt10) ' CalcTNred: q40 = ',q(4,0)
                write(nerrout_coli,fmt10) ' CalcTNred: q51 = ',q(5,1)
                write(nerrout_coli,fmt10) ' CalcTNred: q30 = ',q(3,0)
                write(nerrout_coli,fmt10) ' CalcTNred: q41 = ',q(4,1)
                write(nerrout_coli,fmt10) ' CalcTNred: q52 = ',q(5,2)
                write(nerrout_coli,fmt10) ' CalcTNred: mm02 = ',mm2(0)
                write(nerrout_coli,fmt10) ' CalcTNred: mm12 = ',mm2(1)
                write(nerrout_coli,fmt10) ' CalcTNred: mm22 = ',mm2(2)
                write(nerrout_coli,fmt10) ' CalcTNred: mm32 = ',mm2(3)
                write(nerrout_coli,fmt10) ' CalcTNred: mm42 = ',mm2(4)
                write(nerrout_coli,fmt10) ' CalcTNred: mm52 = ',mm2(5)
                write(nerrout_coli,fmt10) ' CalcTNred: gram = ',gramdet/det
              end if
            end if

          end if

        end if
      end if


#ifdef TNtest
            write(*,*) 'Tnred = ',TN
            write(*,*) 'Tnred mn err',TNm1err(1:5,r)
            write(*,*) 'Tnred coef err', (maxval(abs(m6x0kinv(1:5,k))),k=1,5)
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


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function PermMomVec
  !  ******************************************
  !  *  Ansgar Denner 27.03.23                *
  !  ******************************************
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function PermMomVec(N,permin,MomVec)

    double complex :: PermMomVec(0:3,N-1)
    integer, intent(in) :: N,permin(1:N-1)
    double complex, intent(in) :: MomVec(0:3,N-1)
    integer :: i,perm(1:N-1)

    perm = permin
    do i=1,N-1
      PermMomVec(0:3,perm(i)) = MomVec(0:3,i)
    end do

  end function


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function PermMasses2
  !  ******************************************
  !  *  Ansgar Denner 27.03.23                *
  !  ******************************************
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function PermMasses2(N,permin,masses2)

    double complex :: PermMasses2(0:N-1)
    integer, intent(in) :: N,permin(1:N-1)
    double complex, intent(in) :: masses2(0:N-1)
    integer :: perm(0:N-1),i

    perm = [0,permin]
    do i=0,N-1
      PermMasses2(perm(i)) = Masses2(i)
    end do

  end function


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function PermMomInv
  !  ******************************************
  !  *  Ansgar Denner 27.03.23                *
  !  ******************************************
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function PermMomInv(N,permin,MomInv)

    double complex :: PermMomInv(N*(N-1)/2)
    integer, intent(in) :: N,permin(1:N-1)
    double complex, intent(in) :: MomInv(N*(N-1)/2)
    integer :: i,j,k,i1,i2,series,entry,perm(0:N-1)
    double complex :: s(0:N-1,0:N-1),perms(0:N-1,0:N-1)

    if (N == 2) then
      PermMomInv = MomInv
      return
    endif

    perm = [0,permin]

! translate to matrix and permute
    do k=1,N*(N-1)/2
      j = mod(k-1,N)   !  entry j+1
      i = (k-1)/N+1    !  series i+1
      i1=max(j,modulo(i+j,N))
      i2=min(j,modulo(i+j,N))
      s(i1,i2) = MomInv(k)
!      write(*,*) 'k,i,j',k,i,j,i1,i2
      perms(max(perm(i2),perm(i1)),min(perm(i1),perm(i2))) = s(i1,i2)
    enddo
    if (mod(N,2).eq.0) then
       i=N/2
       do j=0,(N-1)/2
          i1=min(j,modulo(j+i,N))
          i2=max(j,modulo(j+i,N))
          perms(max(perm(i2),perm(i1)),min(perm(i1),perm(i2))) = s(i2,i1)
       enddo
    endif

!    do i=0,N-1
!    do j=0,i-1
!    write(*,*) 's(i,j): ',i,j,s(i,j),perms(i,j)
!    enddo
!    enddo

! translate back to vector
    do i2=0,N-2
      do i1 = i2+1,N-1
        if(abs(i1-i2)<=N/2) then
          series = abs(i1-i2)
          entry  = min(i1,i2) + 1
        else
          series = N-abs(i1-i2)
          entry  = max(i1,i2) + 1
        endif
!        write(*,*) 'after perm: ',i1,i2,series,entry
        PermMomInv(entry+N*(series-1)) = perms(i1,i2)
      enddo
    enddo

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
