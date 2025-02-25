!!
!!  File cache.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ******************
!  *  module cache  *
!  *  by Lars Hofer *
!  *  modified by Ansgar Denner
!  ******************
!
!
!  provides a cache-system for the tensor-integral library COLLIER
!  based on routines by Markus Roth, adapted for use in COLI and DD
!  (in particular the cache now distinguishes between external master-calls
!   of tensor integrals and internal recursive calls)
!
!  HOW TO USE THE COLLIER-CACHE IN A MONTE-CARLO:
!  1) After the initialization of COLLIER put
!     call InitCacheSystem(mnc,Nmax)
!     mnc = number of different caches, 
!     Nmax: N-point tensor integrals are cached for N<=Nmax
!  2) Call InitCache(nc) ONCE for EVERY phase-space point BEFORE evaluating the complete set
!     of tensor integrals. nc is the number of the respective cache. The tensor integrals 
!     have to be evaluated in the same order for every phase-space point!
!    
!
!
!  global variables:
!  NCoefs, use_cache_system, use_cache_system_save, use_cache
! 
!  functions and subroutines:
!  InitCacheSystem_cll, AddNewCache_cll, SetCacheMode_cll, 
!  SwitchOnCacheSystem_cll, SwitchOffCacheSystem_cll, 
!  SwitchOnCache_cll, SwitchOffCache_cll, SetCacheLevel_cll
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!#define MEMLOG

module cache

!  use collier_global
  use combinatorics
 
  implicit none

  ! variables/parameters accessed in COLI/DD
  logical :: use_cache_system=.true.,use_cache_system_save=.true.
  integer, allocatable :: use_cache(:)
  ! internal variables of the cache-system
  integer :: ninfout_cache, infoutlev_cache
  double complex, allocatable :: CacheVals(:), CacheArgs(:), CacheVals_local(:,:),argshash(:)
  integer, allocatable :: Valspointer(:), Argspointer(:)
  integer, allocatable :: CacheLib(:)
  integer, allocatable :: use_cache_cp(:)
  integer, allocatable :: nevent(:), cache_mode(:), cache_mode_cp(:), nopt(:), ncalc(:), catype(:), ncall(:)
  integer, allocatable :: casa(:,:), new_casa(:,:), cara(:,:), new_cara(:,:), CachePoint(:,:,:)
  integer, allocatable :: cava(:,:), new_cava(:,:)
  integer, allocatable :: cachesize(:), new_cachesize(:)
  integer, allocatable :: rankcached(:)
  integer :: ncache_max=0, nmascall_max, id_max, ncache, nmascall, tencache, nval_local, id_local, ncalc_alloc
  integer :: ncache_ext=0, cachesize_alloc
  

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCacheSystem_cll(mnc,Nmax)
  !
  !  initialization of the cache-system:
  !  mnc = number of caches;  mnc<=0 --> cache-system switched off
  !  Nmax: N-point tensor integrals cached for N<=Nmax
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCacheSystem_cll(mnc,Nmax)

    integer, intent(in) :: mnc,Nmax
    integer :: ncache
    logical :: onlyinternal,infwri
    character(len=*),parameter :: fmt90 = "(A15,I3,A28,I3)"

    if (Nmax.le.0) then    
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('InitCacheSystem_cll','Nmax has to be a positive integer',infwri)    
      if(infwri) write(ninfout_cache,*) '--> cache has not been initialized!'    
      return
    end if

    if (mnc.lt.0) then
      use_cache_system = .false.
      use_cache_system_save = use_cache_system
      return
    else if (mnc.eq.0) then
      use_cache_system = .true.
      use_cache_system_save = use_cache_system
      ncache_ext = 0      
      ncache_max = 1
      onlyinternal = .true.
    else
      use_cache_system = .true.
      use_cache_system_save = use_cache_system
      ncache_ext = mnc
      ncache_max = ncache_ext
      onlyinternal = .false.    
    end if  

    if (allocated(use_cache)) then
      deallocate(use_cache)
    end if
    allocate(use_cache(ncache_max))
    use_cache = Nmax

    if (allocated(use_cache_cp)) then
      deallocate(use_cache_cp)
    end if
    allocate(use_cache_cp(ncache_max))
    use_cache_cp = Nmax

    if (allocated(cache_mode)) then
      deallocate(cache_mode)
    end if
    allocate(cache_mode(ncache_max))
    if (onlyinternal) then
      cache_mode = -1
    else
      cache_mode = -99
    end if

    if (allocated(cache_mode_cp)) then
      deallocate(cache_mode_cp)
    end if
    allocate(cache_mode_cp(ncache_max))
    cache_mode_cp = -99

    if (allocated(nevent)) then
      deallocate(nevent)
    end if
    allocate(nevent(ncache_max))
    nevent = 0

    if (allocated(ncalc)) then
      deallocate(ncalc)
    end if
    allocate(ncalc(ncache_max))
    ncalc = 0

    if (allocated(ncall)) then
      deallocate(ncall)
    end if
    allocate(ncall(ncache_max))
    ncall = 0

    if (allocated(cachesize)) then
      deallocate(cachesize)
    end if
    allocate(cachesize(ncache_max))
    cachesize = 0

    if (allocated(new_cachesize)) then
      deallocate(new_cachesize)
    end if
    allocate(new_cachesize(ncache_max))
    new_cachesize = 0

    if (allocated(casa)) then
      deallocate(casa)
    end if
    allocate(casa(1,ncache_max))
    casa = 0

    if (allocated(new_casa)) then
      deallocate(new_casa)
    end if
    allocate(new_casa(1,ncache_max))
    new_casa = 0

    if (allocated(cara)) then
      deallocate(cara)
    end if
    allocate(cara(1,ncache_max))
    cara = 0

    if (allocated(new_cara)) then
      deallocate(new_cara)
    end if
    allocate(new_cara(1,ncache_max))
    new_cara = 0

    if (allocated(cava)) then
      deallocate(cava)
    end if
    allocate(cava(1,ncache_max))
    cava = 0

    if (allocated(new_cava)) then
      deallocate(new_cava)
    end if
    allocate(new_cava(1,ncache_max))
    new_cava = 0

    if (allocated(CachePoint)) then
      deallocate(CachePoint)
    end if
    allocate(CachePoint(0:0,1,ncache_max))
    CachePoint = 0

    if (allocated(nopt)) then
      deallocate(nopt)
    end if
    allocate(nopt(ncache_max))
    nopt = 10

    if (allocated(CacheVals_local)) then
      deallocate(CacheVals_local)
    end if
    allocate(CacheVals_local(1,1))    
    
    ncache = 1
    nmascall = 0
    nmascall_max = 0
    id_local = 0
    id_max = 0
    nval_local = 1
    cachesize = 0
    new_cachesize = 0
    cachesize_alloc = 0

    infwri = .false.
    if (infoutlev_cache.ge.1) call InfOut_cache('InitCacheSystem_cll','cache system initialized',infwri)    
    if(infwri) write(ninfout_cache,fmt90) 'COLLIER-Cache:', mnc, ' caches initialized at level', Nmax

  end subroutine InitCacheSystem_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine AddNewCache_cll(ncache_out,Nmax)
  !
  !  add a new cache:
  !  ncache_out = number assigned to cache (output!)
  !  Nmax: N-point tensor integrals cached for N<=Nmax
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine AddNewCache_cll(ncache_out,Nmax)

    integer, intent(in) :: Nmax
    integer, intent(out) :: ncache_out
    integer :: ncache,s1,s2
    integer, allocatable :: intaux1(:),intaux2(:,:),intaux3(:,:,:)
    logical :: onlyinternal,infwri
    character(len=*),parameter :: fmt90 = "(A15,I3,A28,I3)"

    if (Nmax.le.0) then
      ncache_out = 0
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('InitCacheSystem_cll','Nmax has to be a positive integer',infwri)    
      if(infwri) write(ninfout_cache,*) '--> cache has not been added!'    
      return
    end if

    use_cache_system = .true.
    use_cache_system_save = use_cache_system
    ncache_ext = ncache_ext+1
    ncache_max = ncache_ext
    onlyinternal = .false.
    
    if (allocated(use_cache)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = use_cache
      deallocate(use_cache)
    end if
    allocate(use_cache(ncache_max))
    use_cache(1:ncache_max-1) = intaux1
    use_cache(ncache_max) = Nmax
    
    if (allocated(use_cache_cp)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = use_cache_cp
      deallocate(use_cache_cp)
    end if
    allocate(use_cache_cp(ncache_max))
    use_cache_cp(1:ncache_max-1) = intaux1
    use_cache_cp(ncache_max) = Nmax

    if (allocated(cache_mode)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = cache_mode   
      deallocate(cache_mode)
    end if
    allocate(cache_mode(ncache_max))
    cache_mode(1:ncache_max-1) = intaux1
    cache_mode(ncache_max) = -99

    if (allocated(cache_mode_cp)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = cache_mode_cp   
      deallocate(cache_mode_cp)
    end if
    allocate(cache_mode_cp(ncache_max))
    cache_mode_cp(1:ncache_max-1) = intaux1
    cache_mode_cp(ncache_max) = -99

    if (allocated(nevent)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))    
      intaux1 = nevent      
      deallocate(nevent)
    end if
    allocate(nevent(ncache_max))
    nevent(1:ncache_max-1) = intaux1
    nevent(ncache_max) = 0

    if (allocated(ncalc)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))  
      intaux1 = ncalc   
      deallocate(ncalc)
    end if
    allocate(ncalc(ncache_max))
    ncalc(1:ncache_max-1) = intaux1
    ncalc(ncache_max) = 0

    if (allocated(ncall)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))  
      intaux1 = ncall   
      deallocate(ncall)
    end if
    allocate(ncall(ncache_max))
    ncall(1:ncache_max-1) = intaux1
    ncall(ncache_max) = 0

    if (allocated(cachesize)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = cachesize   
      deallocate(cachesize)
    end if
    allocate(cachesize(ncache_max))
    cachesize(1:ncache_max-1) = intaux1
    cachesize(ncache_max) = 0

    if (allocated(new_cachesize)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = new_cachesize   
      deallocate(new_cachesize)
    end if
    allocate(new_cachesize(ncache_max))
    new_cachesize(1:ncache_max-1) = intaux1
    new_cachesize(ncache_max) = 0

    if (allocated(casa)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(casa,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = casa
      deallocate(casa)
    else
      s1 = 1
    end if
    allocate(casa(s1,ncache_max))
    casa(1:s1,1:ncache_max-1) = intaux2
    casa(1:s1,ncache_max) = 0

    if (allocated(new_casa)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(new_casa,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = new_casa
      deallocate(new_casa)
    else
      s1 = 1
    end if
    allocate(new_casa(s1,ncache_max))
    new_casa(1:s1,1:ncache_max-1) = intaux2
    new_casa(1:s1,ncache_max) = 0

    if (allocated(cara)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(cara,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = cara
      deallocate(cara)
    else
      s1 = 1
    end if
    allocate(cara(s1,ncache_max))
    cara(1:s1,1:ncache_max-1) = intaux2
    cara(1:s1,ncache_max) = 0

    if (allocated(new_cara)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(new_cara,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = new_cara      
      deallocate(new_cara)
    end if
    allocate(new_cara(s1,ncache_max))
    new_cara(1:s1,1:ncache_max-1) = intaux2
    new_cara(1:s1,ncache_max) = 0    

    if (allocated(cava)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(cava,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = cava
      deallocate(cava)
    else
      s1 = 1
    end if
    allocate(cava(s1,ncache_max))
    cava(1:s1,1:ncache_max-1) = intaux2
    cava(1:s1,ncache_max) = 0

    if (allocated(new_cava)) then
      if (allocated(intaux2)) then
        deallocate(intaux2)
      end if
      s1 = size(new_cava,1)
      allocate(intaux2(s1,ncache_max-1))
      intaux2 = new_cava      
      deallocate(new_cava)
    end if
    allocate(new_cava(s1,ncache_max))
    new_cava(1:s1,1:ncache_max-1) = intaux2
    new_cava(1:s1,ncache_max) = 0    

    if (allocated(CachePoint)) then
      if (allocated(intaux3)) then
        deallocate(intaux3)
      end if
      s1 = size(CachePoint,1)-1
      s2 = size(CachePoint,2)
      allocate(intaux3(0:s1,s2,ncache_max-1))
      intaux3 = CachePoint
      deallocate(CachePoint)
    else
      s1 = 0
      s2 = 1
    end if
    allocate(CachePoint(0:s1,s2,ncache_max))
    CachePoint(0:s1,1:s2,1:ncache_max-1) = intaux3
    CachePoint(0:s1,1:s2,ncache_max) = 0

    if (allocated(nopt)) then
      if (allocated(intaux1)) then
        deallocate(intaux1)
      end if
      allocate(intaux1(ncache_max-1))
      intaux1 = nopt
      deallocate(nopt)
    end if
    allocate(nopt(ncache_max))
    nopt(1:ncache_max-1) = intaux1
    nopt(ncache_max) = 10

    infwri = .false.
    ncache_out = ncache_max
    if (infoutlev_cache.ge.1) call InfOut_cache('AddNewCache_cll','new cache added',infwri)    
    if(infwri) write(ninfout_cache,fmt90) 'COLLIER-Cache: cache', ncache_out, ' initialized at level', Nmax

  end subroutine AddNewCache_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setninfout_cache(ninfout)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setninfout_cache(ninfout)

    integer, intent(in) :: ninfout

    ninfout_cache = ninfout

  end subroutine Setninfout_cache





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine Setinfoutlev_cache(infoutlev)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine Setinfoutlev_cache(infoutlev)

    integer, intent(in) :: infoutlev

    infoutlev_cache = infoutlev

  end subroutine Setinfoutlev_cache





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCacheSystem_cll
  !
  !  switches off temporarily the whole cache-system
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCacheSystem_cll

    integer :: i
    logical :: infwri
    
    do i=1,ncache_max
      call SetCacheMode_cll(i,1)
    end do
    
    if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOffCacheSystem_cll','cache system switched off',infwri)    

  end subroutine SwitchOffCacheSystem_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCacheSystem_cll
  !
  !  used to switch on the cache-system again after it had 
  !  been switched off temporarily
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCacheSystem_cll

    integer :: i
    logical :: infwri

    if (ncache_max.le.0) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOnCacheSystem_cll','cache has not been initialized',infwri)    
      if(infwri)  write(ninfout_cache,*) '--> it cannot be switched on' 
    elseif(argperm_cll) then
       if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOnCacheSystem_cll','permutation of tensor arguments switched on',infwri)    
       if(infwri)  write(ninfout_cache,*) '--> cache cannot be switched on'      
    else  
      do i=1,ncache_max
        call SetCacheMode_cll(i,2)
      end do
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOnCacheSystem_cll','cache system switched on',infwri) 
    end if

  end subroutine SwitchOnCacheSystem_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCache_cll(ncache_in)
  !
  !  switches off temporarily the cache ncache_in
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCache_cll(ncache_in)

    integer, intent(in) :: ncache_in
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOffCache_cll','individual cache cannot be switched off',infwri)    
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'
    else      
      call SetCacheMode_cll(ncache_in,1)
      infwri = .false.
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOffCache_cll','individual cache switched off',infwri)       
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'switched off'
    end if

  end subroutine SwitchOffCache_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCache_cll(ncache_in)
  !
  !  used to switch on the cache ncache_in again after it had 
  !  been switched off temporarily
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCache_cll(ncache_in)

    integer, intent(in) :: ncache_in
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOnCache_cll','individual cache cannot be switched on',infwri)       
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'
    else 
      call SetCacheMode_cll(ncache_in,2)
      infwri = .false.
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOnCache_cll','individual cache switched on',infwri)       
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'switched on'      
    end if

  end subroutine SwitchOnCache_cll

  



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCacheSystem0_cll
  !
  !  switches off temporarily the whole cache-system
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCacheSystem0_cll
  
    logical :: infwri

    use_cache_system = .false.
    if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOffCacheSystem0_cll', &
                                              'cache system (+internal cache) switched off',infwri)       

  end subroutine SwitchOffCacheSystem0_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCacheSystem0_cll
  !
  !  used to switch on the cache-system again after it had 
  !  been switched off temporarily
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCacheSystem0_cll

    logical :: infwri
  
    if (ncache_max.le.0) then
       infwri = .false.
       if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOnCacheSystem0_cll','cache has not been initialized',infwri)    
       if(infwri)  write(ninfout_cache,*) '--> it cannot be switched on'         
    else  
      use_cache_system = use_cache_system_save
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOnCacheSystem0_cll', &
                                                'cache system (+internal cache) switched on',infwri)         
    end if

  end subroutine SwitchOnCacheSystem0_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOffCache0_cll(ncache_in)
  !
  !  switches off temporarily the cache ncache_in
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOffCache0_cll(ncache_in)

    integer, intent(in) :: ncache_in
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOffCache0_cll','individual cache cannot been switched off',infwri)    
      if(infwri)  write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'
    else      
      use_cache_cp(ncache_in) = use_cache(ncache_in)
      use_cache(ncache_in) = 0
      infwri = .false.
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOffCache0_cll','individual cache switched off (also internally)',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'switched off'
    end if

  end subroutine SwitchOffCache0_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SwitchOnCache0_cll(ncache_in)
  !
  !  used to switch on the cache ncache_in again after it had 
  !  been switched off temporarily
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SwitchOnCache0_cll(ncache_in)

    integer, intent(in) :: ncache_in
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SwitchOnCache0_cll','individual cache cannot be switched on',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'    
    else 
      use_cache(ncache_in) = use_cache_cp(ncache_in)
      infwri = .false.
      if (infoutlev_cache.ge.2) call InfOut_cache('SwitchOnCache0_cll','individual cache switched on',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'switched on'     
    end if

  end subroutine SwitchOnCache0_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCacheMode_cll(ncache_in,mode_in)
  !
  !  set the mode of cache ncache_in to
  !  1: only internal calls are cached
  !  2: internal and external calls are cached
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetCacheMode_cll(ncache_in,mode_in)

    integer, intent(in) :: ncache_in,mode_in
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SetCacheMode_cll','cache cannot be modified',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'
      
    else if ((mode_in.lt.1).or.(mode_in.gt.2)) then
      if (infoutlev_cache.ge.1) call InfOut_cache('SetCacheMode_cll', &
                              'cache can only be set to mode 1 (internal) or 2 (internal + external)!',infwri) 
    else
      if (mode_in.eq.1) then
        if (cache_mode(ncache_in).ne.-1) then
          cache_mode_cp(ncache_in) = cache_mode(ncache_in)
        end if
        cache_mode(ncache_in) = -1
!        onlyinternal = .true.
!        if (infoutlev_cache.ge.2) then
!          write(ninfout_cache,*) 'COLLIER-Cache: cache no.', ncache_in, 'set to mode 1 (internal)'
!        end if
      else
        cache_mode(ncache_in) = cache_mode_cp(ncache_in)
!        onlyinternal = .false.
!        if (infoutlev_cache.ge.2) then
!          write(ninfout_cache,*) 'COLLIER-Cache: cache no.', ncache_in, 'set to mode 2 (internal + external)'
!        end if
      end if       
    end if

  end subroutine SetCacheMode_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetCacheLevel_cll(ncache_in,Nmax)
  !
  !  can be called after InitCacheSystem to set the level
  !  of an individual cache ncache_in to the value Nmax:
  !  N-point tensor integrals are cache in nache_in for N<=Nmax 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetCacheLevel_cll(ncache_in,Nmax)

    integer, intent(in) :: ncache_in,Nmax
    logical :: infwri
    character(len=*),parameter :: fmt90 = "(A10,I3,A24,I3)"
    character(len=*),parameter :: fmt91 = "(A10,I3,A29)"      

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SetCacheLevel_cll','cache cannot be modified',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'    
    else
      if (cache_mode(ncache_in).ne.-99) then
        infwri = .false.
        if(infoutlev_cache.ge.1) call InfOut_cache('SetCacheLevel_cll','cache-level cannot be modified at this stage',infwri) 
        if(infwri) then
          if(cache_mode(ncache_in).eq.-1) then 
            write(ninfout_cache,fmt91) 'cache no.', ncache_in, 'only used for internal calls'
          else
            write(ninfout_cache,fmt90) 'cache no.', ncache_in, 'already in use at level', use_cache(ncache_in)
          end if
        end if
      else
        use_cache(ncache_in) = Nmax
        use_cache_cp(ncache_in) = Nmax
        infwri = .false.
        if (infoutlev_cache.ge.2) call InfOut_cache('SetCacheLevel_cll','cache level modified',infwri) 
        if(infwri) write(ninfout_cache,*) 'COLLIER-Cache: level of cache no.', ncache_in, 'set to ', Nmax
      end if
    end if

  end subroutine SetCacheLevel_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetTenCache_cll(Nten)
  !
  !  tensors are cached for N>=Nten
  !  coefficients are cached for N<Nten 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetTenCache_cll(Nten)

    integer, intent(in) :: Nten

    tencache = Nten

  end subroutine SetTenCache_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine SetNopt_cll(ncache_in,no)
  !
  !  can be called after InitCacheSystem to set the number
  !  of optimization runs no for an individual cache 
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine SetNopt_cll(ncache_in,no)

    integer, intent(in) :: ncache_in,no
    logical :: infwri

    if ((ncache_in.le.0).or.(ncache_in.gt.ncache_max)) then 
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('SetNopt_cll','optimization level cannot be set',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist!'    
    else if (no.lt.0) then
      if (infoutlev_cache.ge.1) call InfOut_cache('SetNopt_cll','argument no has to be a non-negative integer!',infwri)     
    else
      nopt(ncache_in) = no
      infwri = .false.
      if (infoutlev_cache.ge.2) call InfOut_cache('SetNopt_cll','optimization level set',infwri) 
      if(infwri) write(ninfout_cache,*) 'COLLIER-Cache: number of optimization points for cache no.', &
                                        ncache_in, 'set to ', no
    end if

  end subroutine SetNopt_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitCache_cll(ncache_in)
  !
  !  to be called for each phase-space point before
  !  the evaluation of the tensor-integrals
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitCache_cll(ncache_in)

    integer, intent(in) :: ncache_in
    integer, allocatable :: casa_swap(:,:), cara_swap(:,:), new_casa_swap(:,:) 
    integer, allocatable :: cava_swap(:,:), new_cava_swap(:,:) 
    integer, allocatable :: CachePoint_swap(:,:,:), new_cara_swap(:,:)
    integer :: i,nsize,cargs_alloc
    logical :: infwri
 
    if ((.not.use_cache_system).or.(ncache_max.lt.ncache_in)) then
      infwri = .false.
      if (infoutlev_cache.ge.1) call InfOut_cache('InitCache_cll','cache cannot be initialized for new phase-space point',infwri) 
      if(infwri) write(ninfout_cache,*) 'cache no.', ncache_in, 'does not exist or is switched off!'       
      return
    end if

    ncache = ncache_in
    if (cache_mode(ncache).eq.-1) return

    nevent(ncache) = nevent(ncache)+1
    ! write(*,*) 'COLLIER: cache no.', ncache_in, 'reinitialized for event no.', nevent(ncache)

    if (nevent(ncache).ge.nopt(ncache)+4) then
      ! fully optimized runs
      cache_mode(ncache) = 0

#ifdef MEMLOG
       if(5*new_cachesize(ncache)/4.gt.cachesize(ncache))  & 
            write(*,*) 'InitCache inc 0 ',ncache,nevent(ncache),    &
              5*new_cachesize(ncache)/4,cachesize(ncache),cachesize_alloc
#endif

      cachesize(ncache) = max(cachesize(ncache),5*new_cachesize(ncache)/4)
      deallocate(CacheVals)
      cachesize_alloc = max(cachesize(ncache),cachesize_alloc)
      allocate(CacheVals(0:cachesize_alloc))
!      new_cachesize(ncache) = 0

    else if (nevent(ncache).eq.1) then
      ! initialization run no.1
      cache_mode(ncache) = 1
      ncall(ncache) = 0

    else if (nevent(ncache).eq.2) then
      ! initialization run no.2
      cache_mode(ncache) = 2

      if (mode_cll.ne.2) then
!        WWj:      ncall=1782     nmascall =  84   ncalc < 146
!        nnjj:     ncall=15530    nmascall = 215   ncalc < 913
!        Racoonww: ncall=37425    nmascall = 414   ncalc < 1968
!        VBSwwee:  ncall=2010935  nmascall = 1611  ncalc < 19392
        ncalc_alloc = max(int(sqrt(real(maxval(ncall))*real(nmascall_max)))/4,maxval(ncalc))+10      ! to avoid ncalc_alloc=0 for small ncall
!       typically once increased in runs
      else
!        WWj:      ncall=570      nmascall =  84   ncalc < 219
!        nnjj:     ncall=15530    nmascall = 215   ncalc < 741
!        Racoonww: ncall=5679     nmascall = 414  ncalc < 1344
        ncalc_alloc = max(int(sqrt(real(maxval(ncall))*real(nmascall_max)))/2,maxval(ncalc))+10
!       typically once increased in runs
      end if


      ! ncall counts all calls of TI in first event, 
      ! ncalc countes different calls in following events

      if (allocated(CacheArgs)) then
        deallocate(CacheArgs)
      end if
      ! estimate based on number of arguments N*(N+1)/2 for largest N
      ! divided by sum of inequivalent subintegrals 2^N
      ! -> 2* increase for N=8, 1* increase for N=5,6
      ! argssize = ncalc(ncache) * use_cache(ncache)**2 /2**use_cache(ncache)/2
      if (mode_cll.ne.2) then
!        WWj:      ncall=1782    nmascall =  84  size(cacheargs) < 1206
!        nnjj:     ncall=15530   nmascall = 215  size(cacheargs) < 8221
!        Racoonww: ncall=37425   nmascall = 414  size(cacheargs) < 17712
!        VBSwwee:  ncall=2010935 nmascall = 1611 size(cacheargs) < 637240
!        cargs_alloc = ncall(ncache)/2                ! COLI
        cargs_alloc = int(sqrt(real(ncall(ncache))*real(nmascall_max)))*use_cache(ncache)**2/16 + 100
!       typically once increased in runs
      else
!        WWj:      ncall=570     nmascall =  84  size(cacheargs) < 1362
!        nnjj:     ncall=15530   nmascall = 215  size(cacheargs) < 6668
!        Racoonww: ncall=37425   nmascall = 414  size(cacheargs) < 13796
!        cargs_alloc = 2*ncall(ncache)                ! DD
        cargs_alloc = int(sqrt(real(ncall(ncache))*real(nmascall_max)))*use_cache(ncache)**2/8 + 100
!       typically once increased in runs
      end if
      allocate(CacheArgs(cargs_alloc)) 

#ifdef MEMLOG
      write(*,*) 'size(CacheArgs) = ',ncache,size(CacheArgs),use_cache(ncache),ncall(ncache)
      write(*,*) 'ncalc_alloc     = ',ncache,ncalc_alloc, maxval(ncall),ncall(ncache),ncalc(ncache),nmascall_max
#endif

      if (allocated(Argspointer)) then
        deallocate(Argspointer)
      end if
      allocate(Argspointer(0:ncalc_alloc))

      if (allocated(CacheLib)) then
        deallocate(CacheLib)
      end if
      allocate(CacheLib(ncalc_alloc))

      if (allocated(catype)) then
        deallocate(catype)
      end if
      allocate(catype(ncalc_alloc))

      if (allocated(argshash)) then
        deallocate(argshash)
      end if
      allocate(argshash(ncalc_alloc))

      if (size(casa,1).ne.ncalc_alloc) then
        allocate(casa_swap(size(casa,1),ncache_max))
        casa_swap = casa

        deallocate(casa)
        allocate(casa(ncalc_alloc,ncache_max))
        casa = 0
        nsize = min(size(casa_swap,1),ncalc_alloc)
        casa(1:nsize,1:ncache_max) = casa_swap(1:nsize,1:ncache_max)
      end if

      if (size(cara,1).ne.ncalc_alloc) then
        allocate(cara_swap(size(cara,1),ncache_max))
        cara_swap = cara

        deallocate(cara)
        allocate(cara(ncalc_alloc,ncache_max))
        cara = 0
        nsize = min(size(cara_swap,1),ncalc_alloc)
        cara(1:nsize,1:ncache_max) = cara_swap(1:nsize,1:ncache_max)
      end if

      if (size(cava,1).ne.ncalc_alloc) then
        allocate(cava_swap(size(cava,1),ncache_max))
        cava_swap = cava

        deallocate(cava)
        allocate(cava(ncalc_alloc,ncache_max))
        cava = 0
        nsize = min(size(cava_swap,1),ncalc_alloc)
        cava(1:nsize,1:ncache_max) = cava_swap(1:nsize,1:ncache_max)
      end if

      if ((size(CachePoint,1).le.id_max).or.(size(CachePoint,2).lt.nmascall_max)) then
        allocate(CachePoint_swap(0:size(CachePoint,1)-1,size(CachePoint,2),ncache_max))
        CachePoint_swap = CachePoint
        deallocate(CachePoint)
        allocate(CachePoint(0:id_max,nmascall_max,ncache_max))
        CachePoint = 0
        CachePoint(0:size(CachePoint_swap,1)-1,1:size(CachePoint_swap,2),1:ncache_max) = CachePoint_swap
      end if
          
#ifdef MEMLOG
      write(*,*) 'size(CachePoint)= ',ncache,id_max+1,nmascall_max,ncache_max,(id_max+1)*nmascall_max*ncache_max
#endif

      ncalc(ncache) = 0
      ArgsPointer(0) = 0

    else if (nevent(ncache).eq.3) then
      ! first optimization run
      cache_mode(ncache) = 3

#ifdef MEMLOG
      write(*,*) 'InitCache inc 3 ',ncache,nevent(ncache),    &
             2*cachesize(ncache),cachesize_alloc,ncalc(ncache)
#endif

      if (allocated(CacheLib)) then
        deallocate(CacheLib)
      end if
      if (allocated(CacheArgs)) then
        deallocate(CacheArgs)
      end if
      if (allocated(Argspointer)) then
        deallocate(Argspointer)
      end if
      if (allocated(catype)) then
        deallocate(catype)
      end if
      if (allocated(argshash)) then
        deallocate(argshash)
      end if

      if (allocated(CacheVals)) then
        deallocate(CacheVals)
      end if
      cachesize_alloc = max(2*cachesize(ncache), cachesize_alloc)
      allocate(CacheVals(0:cachesize_alloc))
!      new_cachesize(ncache) = 0

      ncalc_alloc = maxval(ncalc)

      if (allocated(Valspointer)) then
        deallocate(Valspointer)
      end if
      allocate(Valspointer(0:ncalc_alloc))

      if (size(casa,1).ne.ncalc_alloc) then
        allocate(casa_swap(size(casa,1),ncache_max))
        casa_swap = casa

        deallocate(casa)
        allocate(casa(ncalc_alloc,ncache_max))
        casa = 0
        nsize = min(size(casa_swap,1),ncalc_alloc)
        casa(1:nsize,1:ncache_max) = casa_swap(1:nsize,1:ncache_max)
      end if

      if (size(cara,1).ne.ncalc_alloc) then
        allocate(cara_swap(size(cara,1),ncache_max))
        cara_swap = cara

        deallocate(cara)
        allocate(cara(ncalc_alloc,ncache_max))
        cara = 0
        nsize = min(size(cara_swap,1),ncalc_alloc)
        cara(1:nsize,1:ncache_max) = cara_swap(1:nsize,1:ncache_max)
      end if

      if (size(cava,1).ne.ncalc_alloc) then
        allocate(cava_swap(size(cava,1),ncache_max))
        cava_swap = cava

        deallocate(cava)
        allocate(cava(ncalc_alloc,ncache_max))
        cava = 0
        nsize = min(size(cava_swap,1),ncalc_alloc)
        cava(1:nsize,1:ncache_max) = cava_swap(1:nsize,1:ncache_max)
      end if

      if (size(new_casa,1).ne.ncalc_alloc) then
        allocate(new_casa_swap(size(new_casa,1),ncache_max))
        new_casa_swap = new_casa

        deallocate(new_casa)
        allocate(new_casa(ncalc_alloc,ncache_max))
        new_casa = 0
        nsize = min(size(new_casa_swap,1),ncalc_alloc)
        new_casa(1:nsize,1:ncache_max) = new_casa_swap(1:nsize,1:ncache_max)
      end if

      if (size(new_cara,1).ne.ncalc_alloc) then
        allocate(new_cara_swap(size(new_cara,1),ncache_max))
        new_cara_swap = new_cara

        deallocate(new_cara)
        allocate(new_cara(ncalc_alloc,ncache_max))
        new_cara = 0
        nsize = min(size(new_cara_swap,1),ncalc_alloc)
        new_cara(1:nsize,1:ncache_max) = new_cara_swap(1:nsize,1:ncache_max)
      end if

      if (size(new_cava,1).ne.ncalc_alloc) then
        allocate(new_cava_swap(size(new_cava,1),ncache_max))
        new_cava_swap = new_cava

        deallocate(new_cava)
        allocate(new_cava(ncalc_alloc,ncache_max))
        new_cava = 0
        nsize = min(size(new_cava_swap,1),ncalc_alloc)
        new_cava(1:nsize,1:ncache_max) = new_cava_swap(1:nsize,1:ncache_max)
      end if

    else if ((nevent(ncache).ge.4).and.(nevent(ncache).le.nopt(ncache)+3)) then
      ! further optimization runs
      cache_mode(ncache) = 3

#ifdef MEMLOG
       if(2*new_cachesize(ncache).gt.cachesize(ncache)) & 
              write(*,*) 'InitCache inc 4 ',ncache,nevent(ncache),    &
              2*new_cachesize(ncache),cachesize(ncache),cachesize_alloc
#endif

      cachesize(ncache) = max(cachesize(ncache),2*new_cachesize(ncache))
      deallocate(CacheVals)
      cachesize_alloc = max(cachesize(ncache),cachesize_alloc)
      allocate(CacheVals(0:cachesize_alloc))
!      new_cachesize(ncache) = 0

      do i=1,ncalc(ncache)
        if (new_casa(i,ncache).gt.casa(i,ncache)) then
          casa(i,ncache) = new_casa(i,ncache)
        end if
        if (new_cara(i,ncache).lt.cara(i,ncache)) then
          cara(i,ncache) = new_cara(i,ncache)
        end if
        if (new_cava(i,ncache).lt.cava(i,ncache)) then
          cava(i,ncache) = new_cava(i,ncache)
        end if
      end do
      
    end if

    nmascall = 0
    new_casa(:,ncache) = 0
    new_cara(:,ncache) = 0
    new_cava(:,ncache) = 0
    new_cachesize(ncache) = 0
    

  end subroutine InitCache_cll





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine ReadCache(vals,nval,args,narg,lib,id,N,r,nocalc,wrica)
  !
  !  check if the nval N-point coefficients up to rank r 
  !  with the narg arguments args are already cached or have to be cached:
  !
  !  1) if they are cached already:
  !     read them from the cache --> the nval values vals are returned
  !     nocalc = .true. and wrica = .false. is returned
  !  2) if they are not yet cached but are needed more than once und thus should be 
  !     written to the cache:
  !     .nocalc = .false. and wrica = .true. is returned
  !  3) if they are only needed once and thus do not have to be cached:
  !      nocalc = .false. and wrica = .false. is returned
  !
  !  Identical function calls are identified assuming a fixed order of the function calls.
  !  The required rank r up to which the respective coefficients have to be cached is 
  !  optimized during the first nopt(ncache) optimization runs.  
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ReadCache(vals,nval,args,narg,lib,id,N,r,nocalc,wrica)

    integer, intent(in) :: id,lib,N,nval,narg
    integer, intent(inout) :: r
    double complex, intent(in) :: args(narg)
    double complex, intent(out) :: vals(nval) 
    double complex, allocatable :: CacheArgs_aux(:),argshash_aux(:)
    logical, intent(out) :: nocalc,wrica
    logical :: nohit
    integer :: i,j,ncapo
    integer, allocatable :: intaux1(:),intaux2(:,:)
    double complex :: argscheck

    nocalc = .false.
    wrica = .false.

    if (use_cache_system) then
      if ((cache_mode(ncache).eq.-1).and.(lib.eq.2)) return
      
      if (id.eq.0) then
        if (cache_mode(ncache).eq.-1) then
          id_local = max(id_local,2**N-3)
          if (allocated(rankcached)) then
            deallocate(rankcached)
          end if
          allocate(rankcached(0:id_local))
          rankcached = -1
          if (nval.gt.nval_local) nval_local = nval         
          if (allocated(CacheVals_local)) then
            deallocate(CacheVals_local)
          end if
          allocate(CacheVals_local(nval_local,id_local))
          return
        else
          nmascall = nmascall+1
          if (cache_mode(ncache).eq.1) then
            id_max = max(id_max,2**N-3)
          end if
        end if
      end if

      if(use_cache(ncache).ge.N) then

        select case(cache_mode(ncache))
 
          case(0)
            ! fully optimized run

            ncapo = CachePoint(id,nmascall,ncache)
            if (ncapo.eq.0) return
 
            if (casa(ncapo,ncache).gt.1) then
              ! coefficient has to be written or
              ! can be read from cache
              new_casa(ncapo,ncache) = new_casa(ncapo,ncache)+1

              if (new_casa(ncapo,ncache).gt.1) then
                if (r.le.new_cara(ncapo,ncache).and.nval.le.new_cava(ncapo,ncache)) then
                  ! coefficient can be read from cache
                  vals(1:nval) = CacheVals(Valspointer(ncapo):Valspointer(ncapo)+nval-1)
                  nocalc = .true.
                else
                  ! coefficient has to be written to cache          
                  r = max(new_cara(ncapo,ncache),r) 
                  wrica = .true.                 
                end if

              else
                ! coefficient has to be stored in cache for the first time
                r = max(cara(ncapo,ncache),r)          
                wrica = .true.

              end if

            end if


          case(1)
            ! initialization run no.1
            ! determine maximum size of args and id 
            nmascall_max = max(nmascall_max,nmascall)
            ncall(ncache) = ncall(ncache)+1

          case(2)
            ! initialization run no.2
            ! determine # of calculations for each coefficient
            ncapo=0
            nohit = .true.
            argscheck = sum(args(:narg))
            do while (nohit.and.(ncapo.lt.ncalc(ncache)-1))
              ncapo = ncapo+1
              if ((N.eq.catype(ncapo)).and. &
                  (argscheck.eq.argshash(ncapo)) .and. &
                  (lib.eq.CacheLib(ncapo))) then
                nohit = .false.
                do j=1,narg
                  if (args(j).ne.CacheArgs(Argspointer(ncapo)-narg+j)) then
                    nohit = .true.
                  end if
                end do
              end if
            end do

            if (nohit) then
              ncalc(ncache) = ncalc(ncache)+1 

              if(ncalc(ncache).gt.ncalc_alloc) then

                allocate(intaux1(0:2*ncalc_alloc))
                intaux1(0:ncalc_alloc) =  Argspointer(:) 
                intaux1(1+ncalc_alloc:2*ncalc_alloc) = 0
                call move_alloc( from=intaux1, to=Argspointer )

                allocate(intaux1(2*ncalc_alloc))
                intaux1(1:ncalc_alloc) =  catype(:) 
                intaux1(1+ncalc_alloc:2*ncalc_alloc) = 0
                call move_alloc( from=intaux1, to=catype )

                allocate(argshash_aux(2*ncalc_alloc))
                argshash_aux(1:ncalc_alloc) =  argshash(:) 
                argshash_aux(1+ncalc_alloc:2*ncalc_alloc) = 0d0
                call move_alloc( from=argshash_aux, to=argshash )

                allocate(intaux1(2*ncalc_alloc))
                intaux1(1:ncalc_alloc) =  CacheLib(:) 
                intaux1(1+ncalc_alloc:2*ncalc_alloc) = 0
                call move_alloc( from=intaux1, to=CacheLib )

                allocate(intaux2(2*ncalc_alloc,ncache_max))
                intaux2(1:ncalc_alloc,:) = casa(:,:) 
                intaux2(1+ncalc_alloc:2*ncalc_alloc,:) = 0
                call move_alloc( from=intaux2, to=casa )

                allocate(intaux2(2*ncalc_alloc,ncache_max))
                intaux2(1:ncalc_alloc,:) = cara(:,:) 
                intaux2(1+ncalc_alloc:2*ncalc_alloc,:) = 0
                call move_alloc( from=intaux2, to=cara )

                allocate(intaux2(2*ncalc_alloc,ncache_max))
                intaux2(1:ncalc_alloc,:) = cava(:,:) 
                intaux2(1+ncalc_alloc:2*ncalc_alloc,:) = 0
                call move_alloc( from=intaux2, to=cava )

#ifdef MEMLOG
                write(*,*) 'ncalc_alloc increased',ncache,ncalc_alloc,size(argspointer)
#endif

                ncalc_alloc = 2*ncalc_alloc
              endif
 
              Argspointer(ncalc(ncache)) = Argspointer(ncalc(ncache)-1) + narg 

              if(Argspointer(ncalc(ncache)).gt.size(CacheArgs)) then
                allocate(CacheArgs_aux(max(2*size(CacheArgs), Argspointer(ncalc(ncache))))) 
                CacheArgs_aux(1: Argspointer(ncalc(ncache)-1)) = CacheArgs(1:Argspointer(ncalc(ncache)-1))
                call move_alloc( from=CacheArgs_aux, to=CacheArgs )

#ifdef MEMLOG
                write(*,*) 'size(CacheArgs) increased to ',ncache,size(CacheArgs)
#endif

              endif

              casa(ncalc(ncache),ncache) = 1
              cara(ncalc(ncache),ncache) = r
              cava(ncalc(ncache),ncache) = nval
              catype(ncalc(ncache)) = N
              argshash(ncalc(ncache)) = argscheck
              CacheLib(ncalc(ncache)) = lib
              CachePoint(id,nmascall,ncache) = ncalc(ncache)
              CacheArgs(Argspointer(ncalc(ncache)-1)+1:Argspointer(ncalc(ncache))) = args
              cachesize(ncache) = cachesize(ncache) + nval
            else
              casa(ncapo,ncache) = casa(ncapo,ncache)+1
              cara(ncapo,ncache) = max(cara(ncapo,ncache),r)
              cava(ncapo,ncache) = max(cava(ncapo,ncache),nval)
              CachePoint(id,nmascall,ncache) = ncapo
              if (nval.gt.cava(ncapo,ncache)) cachesize(ncache) = cachesize(ncache) + nval
            end if

          case(3)
            ! optimization run
            ! try to maximize casa and minimize cara
            ncapo = CachePoint(id,nmascall,ncache)
            if (ncapo.eq.0) return
!            new_cara(ncapo,ncache) = max(new_cara(nca,ncache),r)

            if (casa(ncapo,ncache).gt.1) then
              ! coefficient has to be written to or can be read from cache
              new_casa(ncapo,ncache) = new_casa(ncapo,ncache)+1

              if (new_casa(ncapo,ncache).gt.1) then
                ! coefficient can be read from cache
!                C-functions: rank <=> nval, r=rbasic
                if (nval.le.new_cava(ncapo,ncache).and.r.le.new_cara(ncapo,ncache)) then
                  vals(1:nval) = CacheVals(Valspointer(ncapo):Valspointer(ncapo)+nval-1)
                  nocalc = .true.
                else
                  ! coefficient has to be written to cache          
                  r = max(new_cara(ncapo,ncache),r)             ! needed for CalcC          
                  wrica = .true.                 
                end if

              else
                ! coefficient has to be stored in cache
                wrica = .true.

              end if      

            end if

 
          case(-1)
            ! cache only internal calls

            if (r.le.rankcached(id)) then
              ! coefficient can be read from cache
              vals(1:nval) = CacheVals_local(1:nval,id)
              nocalc = .true.
            else
              ! coefficient has to be written to cache          
              wrica = .true.                 
            end if

        end select

      end if

    end if

  end subroutine ReadCache





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine WriteCache(vals,nval,id,N,r)
  !
  !  write the nval coefficients vals to the cache
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine WriteCache(vals,nval,id,N,r)

    integer, intent(in) :: nval,id,N,r
    double complex, intent(in) :: vals(nval)
    double complex, allocatable :: CacheVals_aux(:),CacheVals_local_aux(:,:)
    integer :: nvalwri,ncapo,nvalold

    if (use_cache_system.and.(use_cache(ncache).ge.N)) then

      select case(cache_mode(ncache))
 
      case(0)
      ! fully optimized run
        
        ncapo = CachePoint(id,nmascall,ncache)
        new_cara(ncapo,ncache) = r         !  CachePoint newly writte
        new_cava(ncapo,ncache) = nval      !  old values are lost!

        ! enlarge cache size if required
        ! dummy entry CacheVals(0) avoids if statements for first call
        if (new_cachesize(ncache)+nval.gt.cachesize_alloc) then

#ifdef MEMLOG
          write(*,*) 'WriCa cache ext.',ncache,nevent(ncache),   &
            max(5*new_cachesize(ncache)/4,new_cachesize(ncache)+nval),cachesize_alloc 
#endif
          cachesize_alloc=max(5*new_cachesize(ncache)/4,  &
              new_cachesize(ncache)+nval)

          allocate(CacheVals_aux(0:cachesize_alloc)) 
          CacheVals_aux(1:new_cachesize(ncache)) = CacheVals(1:new_cachesize(ncache))
          call move_alloc( from=CacheVals_aux, to=CacheVals )

#ifdef MEMLOG
!         write(*,*) 'WriCa cache extended',ncache,cachesize_alloc
#endif
        end if

        ! write to cache 
        Valspointer(ncapo) = new_cachesize(ncache)+1
        new_cachesize(ncache)=  new_cachesize(ncache) + nval

        CacheVals(Valspointer(ncapo):Valspointer(ncapo)+nval-1) = vals(1:nval)

      case(3)
      ! optimization run
        
        ncapo = CachePoint(id,nmascall,ncache)
        new_cara(ncapo,ncache) = r         !  CachePoint newly written
        new_cava(ncapo,ncache) = nval      !  Old values are lost

        ! enlarge cache size if required
        ! dummy entry CacheVals(0) avoids if statements for first call
        if (new_cachesize(ncache)+nval.gt.cachesize_alloc) then

#ifdef MEMLOG
          write(*,*) 'WriCa cache ext.',ncache,nevent(ncache),   &
            max(2*new_cachesize(ncache),new_cachesize(ncache)+nval),cachesize_alloc 
#endif
          cachesize_alloc=max(2*new_cachesize(ncache),  &
              new_cachesize(ncache)+nval)

          allocate(CacheVals_aux(0:cachesize_alloc)) 
          CacheVals_aux(1:new_cachesize(ncache)) = CacheVals(1:new_cachesize(ncache))
          call move_alloc( from=CacheVals_aux, to=CacheVals )

#ifdef MEMLOG
!         write(*,*) 'WriCa cache extended',ncache,cachesize_alloc
#endif
        end if

        ! write to cache 
        Valspointer(ncapo) = new_cachesize(ncache)+1
        new_cachesize(ncache)=  new_cachesize(ncache) + nval

        CacheVals(Valspointer(ncapo):Valspointer(ncapo)+nval-1) = vals(1:nval)

      case(-1)
        
        rankcached(id) = r

        if (nval.gt.nval_local) then
          ! increase size of cache
          nvalold = nval_local
          allocate(CacheVals_local_aux(nvalold,id_local))    
          CacheVals_local_aux = CacheVals_local
          deallocate(CacheVals_local)
          nval_local = nval
          allocate(CacheVals_local(nval_local,id_local))
          CacheVals_local(1:nvalold,1:id_local) = CacheVals_local_aux(1:nvalold,1:id_local)
        end if

        CacheVals_local(1:nval,id) = vals(1:nval)
      
      end select

    end if

  end subroutine WriteCache





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InfOut_cache(sub,inf,flag)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Suppression of output must be implemented in calling routines!

  subroutine InfOut_cache(sub,inf,flag)

    character(len=*), intent(in) :: sub, inf
    logical, intent(out) :: flag
!    integer, parameter :: maxErrOut=100
  
    flag = .false.
    if (infoutlev_cache.eq.0) return

    InfCnt_cll = InfCnt_cll + 1      
    if(ninfout_cache.ne.-1) then
      if (InfCnt_cll.le.MaxInfOut_cll) then
        write(ninfout_cache,*)
        write(ninfout_cache,*)
        write(ninfout_cache,*)
        write(ninfout_cache,*) '***********************************************************'
        write(ninfout_cache,*) 'Info-output NO.', InfCnt_cll
        write(ninfout_cache,*) 'in routine: ', trim(sub)
        write(ninfout_cache,*) trim(inf)
!        call WriteMaster_cll(nerrout_cll)
        flag=.true.
      elseif (InfCnt_cll.eq.MaxInfOut_cll+1) then
        write(ninfout_cache,*)
        write(ninfout_cache,*)
        write(ninfout_cache,*)
        write(ninfout_cache,*) '***********************************************************'
        write(ninfout_cache,*)
        write(ninfout_cache,*) ' Further output of information will be suppressed '
        write(ninfout_cache,*)
      endif
    endif

  end subroutine InfOut_cache  



end module cache

