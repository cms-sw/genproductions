!!
!!  File democache.f90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

program democache

  use COLLIER

  implicit none

  double complex :: MomInv6(15), masses2_6(0:5), masses2_6b(0:5)
  double complex :: MomInv5(10), masses2_5(0:4)
  double complex :: MomInv4(6), masses2_4(0:3)
  double complex :: MomInv3(3), masses2_3(0:2)
  double complex :: MomInv2(1), masses2_2(0:1)
  double complex, allocatable :: T1coeff(:),T1coeffuv(:)
  double complex, allocatable :: T2coeff(:),T2coeffuv(:)
  double complex, allocatable :: T3coeff(:),T3coeffuv(:)
  double complex, allocatable :: T4coeff(:),T4coeffuv(:)
  double complex, allocatable :: T5coeff(:),T5coeffuv(:)
  double complex, allocatable :: T6coeff(:),T6coeffuv(:)
  double precision, allocatable :: T1err(:),T2err(:),T3err(:),T4err(:)
  double precision, allocatable :: T5err(:),T6err(:)
  real :: starttime,endtime
  integer :: rank2,rank3,rank4,rank5,rank6,Nmax,rmax
  integer :: i,k,nevents,nloops,icache,mycase

! Nmax = maximal degree N of N-point function
  Nmax = 6
  rmax = 4

! Initialize COLLIER 
! the default directory for diagnostic messages is "output_cll"
  call Init_cll(Nmax,rmax,"output_cll")

! INITIALIZE CACHE nr. "icache" for up to Nmax-point functions
  icache = 1
  call InitCacheSystem_cll(icache,Nmax)


! allocate fields for coefficients
  rank2=3
  rank3=4
  rank4=4
  rank5=4
  rank6=4
  allocate(T2coeff(GetNc_cll(2,rank2)))
  allocate(T2coeffuv(GetNc_cll(2,rank2)))
  allocate(T2err(0:rank2))
  allocate(T3coeff(GetNc_cll(3,rank3)))
  allocate(T3coeffuv(GetNc_cll(3,rank3)))
  allocate(T3err(0:rank3))
  allocate(T4coeff(GetNc_cll(4,rank4)))
  allocate(T4coeffuv(GetNc_cll(4,rank4)))
  allocate(T4err(0:rank4))
  allocate(T5coeff(GetNc_cll(5,rank5)))
  allocate(T5coeffuv(GetNc_cll(5,rank5)))
  allocate(T5err(0:rank5))
  allocate(T6coeff(GetNc_cll(6,rank6)))
  allocate(T6coeffuv(GetNc_cll(6,rank6)))
  allocate(T6err(0:rank6))
  
! set some values for masses
  masses2_6(0) = dcmplx(1d2,-1d0)
  masses2_6(1) = dcmplx(1d2,-1d0)
  masses2_6(2) = dcmplx(0d0,0d0)
  masses2_6(3) = dcmplx(1d1,0d0)
  masses2_6(4) = dcmplx(1d1,0d0)
  masses2_6(5) = dcmplx(0d0,0d0)


!  call InitMonitoring_cll

! issue a bunch of calls of COLLIER for different tensor integrals
! for several "events"

  nevents = 2000

  nloops = 2

  write(*,'(/(a))')  ' *** Demonstration of cache system ***'
  write(*,'(/(a))')  ' NOTE: This example merely illustrates the usage of the cache system.'
  write(*,'((a))')   '       The results are not representative for the performance'
  write(*,'((a))')   '       of Collier, neither wrt to speed nor precision.'
  write(*,'(/a21,i10/)')  ' Number of "Events": ',nevents
  write(*,'(/a39,i4/)')  ' Number of "identical calls/integral": ',nloops

  do mycase=1,4

    if (mycase.eq.1) then
!   use COLI branch
      call SetMode_cll(1) 
!   initialize cache nr. icache for tensor integrals up to N=6
      call InitCacheSystem_cll(icache,6)

      write(*,'(/(a))')  ' COLI branch with cache: '
 
    elseif (mycase.eq.2) then
!   use COLI branch
      call SetMode_cll(1) 
!   switch off cache
      call SwitchOffCacheSystem_cll

      write(*,'(/(a))')  ' COLI branch without cache: '
 
    else if (mycase.eq.3) then
!   use DD branch
      call SetMode_cll(2) 
!   reinitialize cache nr. icache
      call InitCacheSystem_cll(icache,6)

      write(*,'(/(a))')  ' DD branch with cache: '
 
    else if (mycase.eq.4) then
!   use DD branch
      call SetMode_cll(2) 
!   switch off cache
      call SwitchOffCacheSystem_cll

      write(*,'(/(a))')  ' DD branch without cache '
 
    end if

    call cpu_time(starttime)

!    write(*,*) 'start',starttime

    eventloop: do i=1,nevents

! get momenta and invariants (typically from Monte Carlo generator)
      call getinvariants(MomInv6,i)
   
! TELL COLLIER START OF NEW EVENT for cache "icache"
      call InitEvent_cll(icache)
  
! call bunch of tensor integrals or coefficients

! loop to generate identical master calls
      testloop: do k=1,nloops

        call TN_cll(T6coeff,T6coeffuv,MomInv6,masses2_6,6,rank6,T6err)

! pick arguments for tensor integrals of lower rank
        MomInv5 = SubMomInv(6,0,MomInv6)
        masses2_5 = Submasses(6,0,masses2_6)
        call TN_cll(T5coeff,T5coeffuv,MomInv5,masses2_5,5,rank5,T5err)
      
        MomInv4 = SubMomInv(5,0,MomInv5)
        masses2_4 = Submasses(5,0,masses2_5)
        MomInv3 = SubMomInv(4,0,MomInv4)
        masses2_3 = Submasses(4,0,masses2_4)
        call TN_cll(T3coeff,T3coeffuv,MomInv3,masses2_3,3,rank3,T3err)
        call TN_cll(T4coeff,T4coeffuv,MomInv4,masses2_4,4,rank4,T4err)

! change one of the masses
        masses2_6b(0) = (0d0,0d0)
        masses2_6b(1:5) = masses2_6(1:5)
        call TN_cll(T6coeff,T6coeffuv,MomInv6,masses2_6b,6,rank6,T6err)

        MomInv5 = SubMomInv(6,1,MomInv6)
        masses2_5 = Submasses(6,1,masses2_6b)
        MomInv4 = SubMomInv(5,2,MomInv5)
        masses2_4 = Submasses(5,2,masses2_5)
        MomInv3 = SubMomInv(4,0,MomInv4)
        masses2_3 = Submasses(4,0,masses2_4)
        MomInv2 = SubMomInv(3,0,MomInv3)
        masses2_2 = Submasses(3,0,masses2_3)
        call TN_cll(T2coeff,T2coeffuv,MomInv2,masses2_2,2,rank2,T2err)
        call TN_cll(T5coeff,T5coeffuv,MomInv5,masses2_5,5,rank5,T5err)
        call TN_cll(T4coeff,T4coeffuv,MomInv4,masses2_4,4,rank4,T4err)

      end do testloop

    end do eventloop

    call cpu_time(endtime)

    write(*,'(/a29,f10.1/)')  ' Needed CPU time in seconds: ',endtime-starttime

  end do

!  call PrintStatistics_cll

  deallocate(T2coeff,T2coeffuv,T2err)
  deallocate(T3coeff,T3coeffuv,T3err)
  deallocate(T4coeff,T4coeffuv,T4err)
  deallocate(T5coeff,T5coeffuv,T5err)
  deallocate(T6coeff,T6coeffuv,T6err)

contains

  subroutine getinvariants(MomInv6,ievent)

    implicit none
    
    double precision p(1:6,0:3) 
    double complex MomInv6(1:15) 
    double precision :: s(0:5,0:5),fact=47d0/49d0
    integer :: N=6,k,i,newmom,ievent
    save s,fact,N

    newmom=0

    if (ievent.eq.1) then
      newmom=1
! use momentum set from Racoon
      p(1,0) =   -0.9500000000000000D+02
      p(1,1) =    0.0000000000000000D+00
      p(1,2) =    0.0000000000000000D+00
      p(1,3) =    0.9500000000000000D+02
      
      p(2,0) =   -0.9500000000000000D+02
      p(2,1) =    0.0000000000000000D+00
      p(2,2) =    0.0000000000000000D+00
      p(2,3) =   -0.9500000000000000D+02
      
      p(3,0) =    0.2046111001757171d+02
      p(3,1) =    0.1057734233089455D+02
      p(3,2) =   -0.2324961261504543D+01
      p(3,3) =    0.1736005205921753D+02
      
      p(4,0) =    0.3558305163378869D+01
      p(4,1) =    0.1436222934374051d+01
      p(4,2) =   -0.2174258125294355D+01
      p(4,3) =   -0.2423097382091398D+01
      
      p(5,0) =    0.8154540918019539D+02
      p(5,1) =   -0.5230395944682889D+02
      p(5,2) =    0.3083642435466509D+02
      p(5,3) =    0.5443403822581044D+02
      
      p(6,0) =    0.8443517563885433D+02
      p(6,1) =    0.4029039418156027D+02
      p(6,2) =   -0.2633720496786619D+02
      p(6,3) =   -0.6937099290293661d+02
      

    else if (ievent.eq.50) then
      newmom=1
! set with small momentum   1.9 versus 3.0 seconds
      p(1,0) =   -250.0000000000000D0       
      p(1,1) =    0.0000000000000000D+00
      p(1,2) =    0.0000000000000000D+00
      p(1,3) =   -250.0000000000000D0
      
      p(2,0) =   -250.0000000000000D0
      p(2,1) =    0.0000000000000000D+00
      p(2,2) =    0.0000000000000000D+00
      p(2,3) =    250.0000000000000D0
      
      p(3,0) =    239.91697591218110D0
      p(3,1) =   31.640107125154238D0
      p(3,2) =   -116.12928664566658D0
      p(3,3) =   -207.54047252312847D0

      p(4,0) =    10.276659578244022D0
      p(4,1) =    1.2584629023924521D0
      p(4,2) =    10.131487190720421D0
      p(4,3) =   -1.1742957526504370D0

      p(5,0) =    181.18800109198216D0
      p(5,1) =    -35.098898474333737D0
      p(5,2) =    109.26326872331617D0
      p(5,3) =    140.20947604742324D0

      p(6,0) =      68.618363417592718D0
      p(6,1) =    2.2003284467870472D0
      p(6,2) =    -3.2654692683700119D0
      p(6,3) =    68.505292228355671D0

    else if (ievent.eq.100) then
      newmom=1
        p(1,0) = -100.0000000000000d0
        p(1,1) = 0.000000000000000d0
        p(1,2) = 0.000000000000000d0
        p(1,3) = -100.0000000000000d0

        p(2,0) = -100.0000000000000d0
        p(2,1) = 0.000000000000000d0
        p(2,2) = 0.000000000000000d0
        p(2,3) = 100.0000000000000d0

        p(5,0) = 42.13045937773281d0
        p(5,1) = 38.71045260055064d0
        p(5,2) = 16.54764683624944d0
        p(5,3) = 1.628450497276325d0

        p(6,0) = 73.31699208541762d0
        p(6,1) = -66.17895647220249d0
        p(6,2) = 31.53024134467396d0
        p(6,3) = -1.253367244715983d0

        p(4,0) = 69.60509150033207d0
        p(4,1) = 34.42307953372518d0
        p(4,2) = -60.31848303205667d0
        p(4,3) = 4.647683605402023d0

        p(3,0) = 14.94745703651754d0
        p(3,1) = -6.954575662073326d0
        p(3,2) = 12.24059485113326d0
        p(3,3) = -5.022766857962365d0
    else if (ievent.eq.150) then
      newmom=1
      p(1,0) =   -100.0000000000000D0       
      p(1,1) =    0.0000000000000000D+00
      p(1,2) =    0.0000000000000000D+00
      p(1,3) =   -100.0000000000000D0
      
      p(2,0) =   -100.0000000000000D0
      p(2,1) =    0.0000000000000000D+00
      p(2,2) =    0.0000000000000000D+00
      p(2,3) =    100.0000000000000D0
      
      p(3,0) =    72.02905523601244D0
      p(3,1) =   32.33193950844802D0
      p(3,2) =   64.10893732125734D0
      p(3,3) =   5.733641195059672D0

      p(4,0) =    25.07519219892066D0
      p(4,1) =    2.906699172283076D0
      p(4,2) =    -24.65281899347465D0
      p(4,3) =    3.543286526606821D0

      p(5,0) =    60.19609558319355D0
      p(5,1) =    2.282219923475870D0
      p(5,2) =    -59.72688022443283D0
      p(5,3) =    -7.145710205299344D0

      p(6,0) =    42.69965698187335D0
      p(6,1) =    -37.52085860420697D0
      p(6,2) =    20.27076189665014D0
      p(6,3) =    -2.131217516367149D0
    else if (ievent.eq.200) then
      newmom=1
      p(1,0) =   -100.0000000000000D0       
      p(1,1) =    0.0000000000000000D+00
      p(1,2) =    0.0000000000000000D+00
      p(1,3) =   -100.0000000000000D0
      
      p(2,0) =   -100.0000000000000D0
      p(2,1) =    0.0000000000000000D+00
      p(2,2) =    0.0000000000000000D+00
      p(2,3) =    100.0000000000000D0
      
      p(3,0) =    69.63925440091566D0
      p(3,1) =   -68.33683461638704D0
      p(3,2) =   -3.803463998624776D0
      p(3,3) =   -12.85443307731553D0

      p(4,0) =    34.49634785978589D0
      p(4,1) =    21.54290146454439D0
      p(4,2) =    -15.60187514686951D0
      p(4,3) =    21.96549348532329D0

      p(5,0) =    66.20628864972379D0
      p(5,1) =    65.60907499667245D0
      p(5,2) =    -3.500460884404577D0
      p(5,3) =    -8.152834381263276D0

      p(6,0) =    29.65810908957467D0
      p(6,1) =    -18.81514184482980D0
      p(6,2) =    22.90580002989887D0
      p(6,3) =    -0.9582260267444891D0

    else if (ievent.eq.250) then
      newmom=1
        p( 1, 0) =      -159.2798637127597488d0
        p( 1, 1) =         0.0000000000000000d0
        p( 1, 2) =         0.0000000000000000d0
        p( 1, 3) =      -159.2798637127597488d0
        p( 2, 0) =      -237.6261878491357891d0
        p( 2, 1) =         0.0000000000000000d0
        p( 2, 2) =         0.0000000000000000d0
        p( 2, 3) =       237.6261878491357891d0
        p( 3, 0) =       133.6018149124976446d0
        p( 3, 1) =        60.5134748218625731d0
        p( 3, 2) =       112.6312302087531094d0
        p( 3, 3) =       -38.7526811273741174d0
        p( 4, 0) =        69.9256648527542950d0
        p( 4, 1) =        66.6463990867762135d0
        p( 4, 2) =         7.6538437283820429d0
        p( 4, 3) =       -19.7300473909055754d0
        p( 5, 0) =        85.7314709474216841d0
        p( 5, 1) =       -38.6625297933730749d0
        p( 5, 2) =       -63.4432116127613241d0
        p( 5, 3) =       -42.7791164126088006d0
        p( 6, 0) =       107.6471008492219283d0
        p( 6, 1) =       -88.4973441152657188d0
        p( 6, 2) =       -56.8418623243738281d0
        p( 6, 3) =        22.9155207945124531d0


    end if

    if (newmom.eq.1) then
      ! define invariants
      N=6 
      do k=0,N-1
        s(modulo(k+1,N),k) = p(k+1,0)**2
        do i=1,3
          s(modulo(k+1,N),k) = s(modulo(k+1,N),k) - p(k+1,i)**2
        end do
        if(abs(s(modulo(k+1,N),k)).lt.1d-14* abs(p(k+1,0))**2) s(modulo(k+1,N),k) = 0d0
      
        s(k,modulo(k+1,N)) = s(modulo(k+1,N),k)
      end do

      do k=0,N-1
        s(modulo(k+2,N),k) = (p(modulo(k+1,N)+1,0) + p(k+1,0))**2
        do i=1,3
          s(modulo(k+2,N),k) = s(modulo(k+2,N),k) &
              - (p(modulo(k+1,N)+1,i) + p(k+1,i))**2
        end do
        s(k,modulo(k+2,N)) = s(modulo(k+2,N),k)
      end do
      do k=0,(N-1)/2
        s(modulo(k+3,N),k) = (p(modulo(k+2,N)+1,0) + p(modulo(k+1,N)+1,0) + p(k+1,0))**2
        do i=1,3
          s(modulo(k+3,N),k) = s(modulo(k+3,N),k) &
              - (p(modulo(k+2,N)+1,i) + p(modulo(k+1,N)+1,i) + p(k+1,i))**2
        end do
        s(k,modulo(k+3,N)) = s(modulo(k+3,N),k)
      end do
      
      do k=1,N
        MomInv6(k) = s(modulo(k,N),k-1)
      end do
      do k=1,N
        MomInv6(k+N) = s(modulo(k+1,N),k-1)
      end do
      do k=1,N/2
        MomInv6(k+2*N) = s(modulo(k+2,N),k-1)
      end do
      fact = 47d0/97d0  

    else
!     rescale invariants
      fact = 4*fact*(1-fact)

      do k=1,N
        MomInv6(k) = s(modulo(k,N),k-1)*fact
      end do
      do k=1,N
        MomInv6(k+N) = s(modulo(k+1,N),k-1)*fact
      end do
      do k=1,N/2
        MomInv6(k+2*N) = s(modulo(k+2,N),k-1)*fact
      end do
    end if
      
  end subroutine getinvariants
 




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  function SubMasses
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function SubMasses(N,k,masses)

    integer :: N,k,i
    double complex :: masses(0:N-1), SubMasses(0:N-2)

    do i=0,N-2
      if (i.lt.k) then
        SubMasses(i) = masses(i)
      else
        SubMasses(i) = masses(i+1)
      end if
    end do    

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
    double complex :: MomInv(N*(N-1)/2), SubMomInv((N-1)*(N-2)/2)

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
  

end program democache
