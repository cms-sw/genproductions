!!
!!  File demo.f90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

program demo

  use COLLIER

  implicit none

  double complex :: m02,m12,m22,m32,m42,m52
  double complex :: p10,p21,p32,p43,p54,p50,p20,p31
  double complex :: p42,p53,p40,p51,p30,p41,p52
  double complex :: p1vec(0:3),p2vec(0:3),p3vec(0:3),p4vec(0:3),p5vec(0:3)
  double complex :: MomInv(15), masses2(0:5), MomVec(0:3,5)
  double complex, allocatable :: Acoeff(:),Acoeffuv(:)
  double complex, allocatable :: Bcoeff(:,:),Bcoeffuv(:,:)
  double complex, allocatable :: Ccoeff(:,:,:),Ccoeffuv(:,:,:)
  double complex, allocatable :: Dcoeff(:,:,:,:),Dcoeffuv(:,:,:,:)
  double complex, allocatable :: Ecoeff(:,:,:,:,:),Ecoeffuv(:,:,:,:,:)
  double complex, allocatable :: Fcoeff(:,:,:,:,:,:),Fcoeffuv(:,:,:,:,:,:)
  double complex, allocatable :: Cten(:,:,:,:),Ctenuv(:,:,:,:)
  double complex, allocatable :: Dten(:,:,:,:),Dtenuv(:,:,:,:)
  double complex, allocatable :: Eten(:,:,:,:),Etenuv(:,:,:,:)
  double complex, allocatable :: Ften(:,:,:,:),Ftenuv(:,:,:,:)  
  double complex, allocatable :: DBcoeff(:,:),DBcoeffuv(:,:)
  double complex, allocatable :: TNcoeff(:),TNcoeffuv(:)
  double complex, allocatable :: TNten(:),TNtenuv(:)
  double complex :: A0,B0,C0,D0,E0,F0,A0uv,B0uv,DB0,DB1,DB00,DB00uv
  double complex, parameter :: C0uv=(0d0,0d0),D0uv=(0d0,0d0)
  double complex, parameter :: E0uv=(0d0,0d0),F0uv=(0d0,0d0)
  double precision, allocatable :: Aerr(:),Berr(:),Cerr(:),Derr(:)
  double precision, allocatable :: Eerr(:),Ferr(:),DBerr(:)
  double precision :: MuUV2,MUIR2,DeltaUV,DeltaIR1,DeltaIR2
  integer :: rank,Nmax,N,rmax
  integer :: mode,caseN,casei

! Nmax = maximal degree N of N-point function
  Nmax = 6
  rmax = 6

! Initialize COLLIER 
! the default directory for diagnostic messages is "output_cll"
  call Init_cll(Nmax,rmax,"output_cll") 
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! for checks
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call InitCacheSystem_cll(1,Nmax)
  call InitEvent_cll
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

! uncomment to redefine regularization parameters of COLLIER 
! (after initialization)
!  MuUV2 = dcmplx(1d2)
!  call SetMuUV2_cll(MuUV2)
!  MuIR2 = dcmplx(1d2)
!  call SetMuIR2_cll(MuIR2)
!  DeltaUV = dcmplx(1d0)
!  call SetdeltaUV_cll(DeltaUV)
!  DeltaIR1 = dcmplx(1d0)
!  DeltaIR2 = dcmplx(1d0)
!  call SetdeltaIR_cll(DeltaIR1,DeltaIR2)

! uncomment to choose DD instead of COLI library
! call SetMode_cll(2)
! or to perform comparison between COLI and DD
! call setMode_cll(3)

! the specific examples start here

! choose COLLIER mode
  write(*,*) 
  write(*,*)  'Choose mode od COLLIER :' 
  write (*,*) ' 1) use COLI branch'
  write (*,*) ' 2) use DD branch'
  write (*,*) ' 3) use both branches and compare'
  read(*,*) mode


  select case(mode) 

  case(1)

  case(2)

  case(3)

  case default

     write(*,'(/(a))') ' Input does not correspond to available  mode'

  end select

  call SetMode_cll(mode)

  write(*,*) 
  write(*,*)  'Choose type of function:' 
  write (*,*) ' 1) 1-point function'
  write (*,*) ' 2) 2-point function'
  write (*,*) ' 3) 3-point function'
  write (*,*) ' 4) 4-point function'
  write (*,*) ' 5) 5-point function'
  write (*,*) ' 6) 6-point function'
  write (*,*) '12) derivative of 2-point function'
  read(*,*) caseN


  select case(caseN)

! 1-point function calls
  case(1)

    write(*,*) 
    write(*,*)  'Choose test case for 1-point function:'
    write (*,*) ' 1) regular scalar 1-point function'
    write (*,*) '10) 1-point coefficients up to rank 2'
    read(*,*) casei
  
    select case(casei)

! example 1  1
! call of regular scalar 1-point function
    case(1) 

      write(*,'(/(a)/)') ' Calculating regular scalar 1-point function'

      m02 = 4d2

      masses2(0) = m02

! A0 contains the results for the scalar 2-point function
      call A0_cll(A0,m02)  

! writes the result to 'demo_1point_example01.dat'
      call writeresultA0(caseN,casei,A0,masses2(0:0))


  
! example 1 10
! call of tensor 1-point function: coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating 1-point coefficients'
      
      m02 = 4d2

      masses2(0:0) = m02

! allocate fields for tensor coefficients
      rank = 2
      allocate(Acoeff(0:rank/2))
      allocate(Acoeffuv(0:rank/2))
      allocate(Aerr(0:rank))
      

! Acoeff(n0) contains the results for the corresponding tensor coefficent
! Acoeffuv(n0) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Aerr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call A_cll(Acoeff,Acoeffuv,m02,rank,Aerr)  

! equivalent call
      N = 1
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call TN_cll(TNcoeff,TNcoeffuv,masses2(0:2),N,rank,Aerr)      

! writes the result to 'demo_1point_example10.dat'
      call writeresultA(caseN,casei,Acoeff,Acoeffuv,masses2(0:0),rank,Aerr)

      deallocate(Acoeff,Acoeffuv,Aerr,TNcoeff,TNcoeffuv)
      
    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select


! 2-point function calls
  case(2)

    write(*,*) 
    write(*,*)  'Choose test case for 2-point function:'
    write (*,*) ' 1) regular scalar 2-point function'
    write (*,*) ' 2) scalar 2-point function with small masses'
    write (*,*) '10) tensor 2-point function: coefficients up to rank 3'
    read(*,*) casei
  
    select case(casei)

! example 2  1
! call of regular scalar 2-point function
    case(1) 

      write(*,'(/(a)/)') ' Calculating regular scalar 2-point function'

      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! B0 contains the results for the scalar 2-point function
      call B0_cll(B0,p10,m02,m12)  

! equivalent call 
      call B0_cll(B0,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_example01.dat'
      call writeresultB0(caseN,casei,B0,MomInv(1:1),masses2(0:1))


! example 2  2
! call of regular scalar 2-point function with small masses
    case(2) 

      write(*,'(/(a)/)')  &
          ' Calculating scalar 2-point function with small masses'

      p10 = 4d2
      m02 = 1d-4
      m12 = 1d-4

! clears list of regulator masses
      call clearminf2_cll
! adds m12 to list of regulator masses
! these are treated as infinitesimal apart from their appearance
! in IR-singular logarithms, where the actual values are taken
      call addminf2_cll(m12)
   
      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! B0 contains the results for the scalar 2-point function
      call B0_cll(B0,p10,m02,m12)  

! equivalent call 
      call B0_cll(B0,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_example02.dat'
      call writeresultB0(caseN,casei,B0,MomInv(1:1),masses2(0:1))

  
! example 2 10
! call of tensor 2-point function: coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating 2-point coefficients'
      
      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = (/p10/)
      masses2(0:1) = (/m02,m12/)

! allocate fields for tensor coefficients
      rank = 3
      allocate(Bcoeff(0:rank/2,0:rank))
      allocate(Bcoeffuv(0:rank/2,0:rank))
      allocate(Berr(0:rank))
      

! Bcoeff(n0,n1) contains the results for the corresponding tensor coefficent
! Bcoeffuv(n0,n1) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Berr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call B_cll(Bcoeff,Bcoeffuv,p10,m02,m12,rank,Berr)  

! equivalent call 
      call B_cll(Bcoeff,Bcoeffuv,MomInv(1:1),masses2(0:1),rank,Berr)

! equivalent calls, but all coefficients are in 1-dimensional array TNcoeff 
      N = 2
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call B_cll(TNcoeff,TNcoeffuv,p10,m02,m12,rank,Berr)
! or
      call B_cll(TNcoeff,TNcoeffuv,MomInv(1:1),masses2(0:1),rank,Berr)
! or
      call TN_cll(TNcoeff,TNcoeffuv,MomInv(1:1),masses2(0:1),N,rank,Berr)      

! writes the result to 'demo_2point_example10.dat'
      call writeresultB(caseN,casei,Bcoeff,Bcoeffuv,MomInv(1:1),masses2(0:1),rank,Berr)
      
      deallocate(Bcoeff,Bcoeffuv,Berr,TNcoeff,TNcoeffuv)

    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'
      
    end select

! 3-point function calls
  case(3)

    write(*,*) 
    write(*,*)  'Choose test case for 3-point function:'
    write (*,*) ' 1) regular scalar 3-point function'
    write (*,*) ' 2) IR-singular scalar 3-point function'
    write (*,*) ' 3) Mass-singular scalar 3-point function in dimensional regularization'
    write (*,*) ' 4) Mass-singular scalar 3-point function in mass regularization'
    write (*,*) '10) tensor 3-point function: coefficients up to rank 3'
    write (*,*) '20) tensor 3-point function: tensor components up to rank 3'
    read(*,*) casei
  
    select case(casei)

! example 3  1
! call of regular scalar 3-point function
    case(1) 

      write(*,'(/(a)/)') ' Calculating regular scalar 3-point function'

      p10 = 2d2
      p21 = 1d3
      p20 = 2d2
      m02 = 1d2
      m12 = 1d2
      m22 = 1d2

      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! C0 contains the results for the scalar 3-point function
      call C0_cll(C0,p10,p21,p20,m02,m12,m22)  

! equivalent call 
      call C0_cll(C0,MomInv(1:3),masses2(0:2))  

! writes the result  to 'demo_3point_example01.dat'
      call writeresultC0(caseN,casei,C0,MomInv(1:3),masses2(0:2))

       
! example 3  2
! call of IR-singular scalar 3-point function
    case(2) 

      write(*,'(/(a)/)') ' Calculating IR-singular scalar 3-point function'
      write(*,'((a))') ' IR singularity described by log(muir2) and deltair'

      p10 = 1d2
      p21 = 5d2
      p20 = 1d2
      m02 = 0d0
      m12 = 1d2
      m22 = 1d2
    
      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! C0 contains the results for the scalar 3-point function
      call C0_cll(C0,p10,p21,p20,m02,m12,m22)  

! equivalent call 
      call C0_cll(C0,MomInv(1:3),masses2(0:2))  

! writes the result to 'demo_3point_example02.dat' 
      call writeresultC0(caseN,casei,C0,MomInv(1:3),masses2(0:2))

       
! example 3  3
! call of mass-singular scalar 3-point function in dimensional regularization
    case(3) 

      write(*,'(/(a))')  ' Calculating mass-singular scalar 3-point function'
      write(*,'((a)/)') ' in dimensional regularization'
      write(*,'((a))') ' mass-singularity described by log(muir2) and deltair'

      p10 = 0d0
      p21 = 5d2
      p20 = 4d2
      m02 = 0d0
      m12 = 0d0
      m22 = 0d0
      
      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! C0 contains the results for the scalar 3-point function
      call C0_cll(C0,p10,p21,p20,m02,m12,m22)  

! equivalent call 
      call C0_cll(C0,MomInv(1:3),masses2(0:2))  

! writes the result to 'demo_3point_example03.dat' 
      call writeresultC0(caseN,casei,C0,MomInv(1:3),masses2(0:2))

       
! example 3  4
! call of mass-singular scalar 3-point function in mass regularization
    case(4) 

      write(*,'(/(a))') ' Calculating mass-singular scalar 3-point function'
      write(*,'((a)/)') ' in mass regularization'
      write(*,'((a)/)') ' mass-singularity described by log(m12)'

      p10 = 1d-4
      p21 = 5d2
      p20 = 4d2
      m02 = 0d0
      m12 = 1d-4
      m22 = 0d0

! clears list of regulator masses
      call clearminf2_cll
! adds m12 to list of regulator masses
! these are treated as infinitesimal apart from their appearance
! in IR-singular logarithms, where the actual values are taken
      call addminf2_cll(m12)
   
      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! C0 contains the results for the scalar 3-point function
      call C0_cll(C0,p10,p21,p20,m02,m12,m22)  

! equivalent call 
      call C0_cll(C0,MomInv(1:3),masses2(0:2))  

! writes the result to 'demo_3point_example04.dat'
      call writeresultC0(caseN,casei,C0,MomInv(1:3),masses2(0:2))

       

! example 3 10
! call of tensor 3-point function: coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating 3-point coefficients'
      
      p10 = 2d2
      p21 = 1d3
      p20 = 2d2
      m02 = 1d2
      m12 = 1d2
      m22 = 1d2

      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! allocate fields for tensor coefficients
      rank = 3
      allocate(Ccoeff(0:rank/2,0:rank,0:rank))
      allocate(Ccoeffuv(0:rank/2,0:rank,0:rank))
      allocate(Cerr(0:rank))
      

! Ccoeff(n0,n1,n2) contains the results for the corresponding tensor coefficent
! Ccoeffuv(n0,n1,n2) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Cerr(r) is optional and contains an absolute estimate of the error of the 
!         tensor coefficients of rank r (not involving metric tensors)
      call C_cll(Ccoeff,Ccoeffuv,p10,p21,p20,m02,m12,m22,rank,Cerr)  

! equivalent call 
      call C_cll(Ccoeff,Ccoeffuv,MomInv(1:3),masses2(0:2),rank,Cerr)  

! equivalent calls, but all coefficients are in 1-dimensional array TNcoeff 
      N = 3
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call C_cll(TNcoeff,TNcoeffuv,p10,p21,p20,m02,m12,m22,rank,Cerr)
! or
      call C_cll(TNcoeff,TNcoeffuv,MomInv(1:3),masses2(0:2),rank,Cerr)
! or
      call TN_cll(TNcoeff,TNcoeffuv,MomInv(1:3),masses2(0:2),N,rank,Cerr)

! writes the result to 'demo_3point_example10.dat'
      call writeresultC(caseN,casei,Ccoeff,Ccoeffuv,MomInv(1:3),masses2(0:2),rank,Cerr)

      deallocate(Ccoeff,Ccoeffuv,Cerr,TNcoeff,TNcoeffuv)

! example 3 20
! call of tensor 3-point function: tensor components up to rank = 3
    case (20)
      
      write(*,'(/(a))') ' Calculating 3-point tensor components'
      
      p1vec = (/ 15d0,0d0,0d0,5d0 /)
      p2vec = (/ -15d0,0d0,0d0,5d0 /)
      p10 = 2d2
      p21 = 9d2
      p20 = 2d2
      m02 = 1d2
      m12 = 1d2
      m22 = 1d2

      MomVec(0:3,1) = p1vec
      MomVec(0:3,2) = p2vec
      MomInv(1:3) = (/p10,p21,p20/)
      masses2(0:2) = (/m02,m12,m22/)

! allocate fields for tensor coefficients
      N = 3
      rank = 3
      allocate(Cten(0:rank,0:rank,0:rank,0:rank))
      allocate(Ctenuv(0:rank,0:rank,0:rank,0:rank))
      allocate(Cerr(0:rank))

! Cten(n0,n1,n2,n3) contains the results for the corresponding tensor components
! Ctenuv(n0,n1,n2,n3) contains the results for the UV-singular part of the tensor component
! (coefficient of 2/(4-D)=1/eps term)
! Cerr(r) is optional and contains an absolute estimate of the error of the 
!         tensor component of rank r (not involving metric tensors)
      call Cten_cll(Cten,Ctenuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rank,Cerr)  

! equivalent calls
      call Cten_cll(Cten,Ctenuv,MomVec(0:3,1:2),MomInv(1:3),masses2(0:2),rank,Cerr)
! or
      call TNten_cll(Cten,Ctenuv,MomVec(0:3,1:2),MomInv(1:3),masses2(0:2),N,rank,Cerr)

! equivalent calls, but with all components are in 1-dimensional array TNten
      allocate(TNten(GetNt_cll(rank)))
      allocate(TNtenuv(GetNt_cll(rank)))
      call Cten_cll(TNten,TNtenuv,p1vec,p2vec,p10,p21,p20,m02,m12,m22,rank,Cerr)
! or
      call Cten_cll(TNten,TNtenuv,MomVec(0:3,1:2),MomInv(1:3),masses2(0:2),rank,Cerr)
! or
      call TNten_cll(TNten,TNtenuv,MomVec(0:3,1:2),MomInv(1:3),masses2(0:2),N,rank,Cerr) 

! writes the result to 'demo_3point_example20.dat'
      call writeresultCten(caseN,casei,Cten,Ctenuv,MomVec(0:3,1:2),MomInv(1:3),masses2(0:2),rank,Cerr)      

      deallocate(Cten,Ctenuv,Cerr,TNten,TNtenuv)
      
    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select

! scalar 4-point function calls
  case(4)

    write(*,*) 
    write(*,*)  'Choose test case for 4-point function:'
    write (*,*) ' 1) regular scalar 4-point function'
!    write (*,*) ' 2) IR-singular scalar 4-point function'
!    write (*,*) ' 3) Mass-singular scalar 4-point function in dimensional regularization'
!    write (*,*) ' 4) Mass-singular scalar 4-point function in mass regularization'
    write (*,*) '10) tensor 4-point function: coefficients up to rank 3'
    write (*,*) '20) tensor 4-point function: tensor components up to rank 3'
    read(*,*) casei
  
    select case(casei)

! example 4  1
! call of regular scalar 4-point function
    case(1) 

      write(*,'(/(a)/)') ' Calculating regular scalar 4-point function'

      p10 = 1d3
      p21 = 2d3
      p32 = 3d3
      p30 = 1d2
      p20 = 1d4
      p31 = -3d3
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2

      MomInv(1:6) = (/p10,p21,p32,p30,p20,p31/)
      masses2(0:3) = (/m02,m12,m22,m32/)

! D0 contains the results for the scalar 4-point function
      call D0_cll(D0,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)  

! equivalent call 
      call D0_cll(D0,MomInv(1:6),masses2(0:3))  

! writes the result to 'demo_4point_example01.dat'
      call writeresultD0(caseN,casei,D0,MomInv(1:6),masses2(0:3))

       
! example 4 10
! call of 4-point coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating tensor 4-point coefficients'
      
      p10 = 1d3
      p21 = 2d3
      p32 = 3d3
      p30 = 1d2
      p20 = 1d4
      p31 = -3d3
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2

      MomInv(1:6) = (/p10,p21,p32,p30,p20,p31/)
      masses2(0:3) = (/m02,m12,m22,m32/)

! allocate fields for tensor coefficients
      rank = 3
      allocate(Dcoeff(0:rank/2,0:rank,0:rank,0:rank))
      allocate(Dcoeffuv(0:rank/2,0:rank,0:rank,0:rank))
      allocate(Derr(0:rank))
      

! Dcoeff(n0,n1,n2,n3) contains the results for the corresponding tensor coefficent
! Dcoeffuv(n0,n1,n2,n3) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Derr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call D_cll(Dcoeff,Dcoeffuv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,Derr)  

! equivalent call 
      call D_cll(Dcoeff,Dcoeffuv,MomInv(1:6),masses2(0:3),rank,Derr)  

! equivalent calls, but all coefficients are in 1-dimensional array TNcoeff 
      N = 4
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call D_cll(TNcoeff,TNcoeffuv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,Derr)
! or
      call D_cll(TNcoeff,TNcoeffuv,MomInv(1:6),masses2(0:3),rank,Derr)
! or
      call TN_cll(TNcoeff,TNcoeffuv,MomInv(1:6),masses2(0:3),N,rank,Derr)
      
      
! writes the result to 'demo_4point_example10.dat'
      call writeresultD(caseN,casei,Dcoeff,Dcoeffuv,MomInv(1:6),masses2(0:3),rank,Derr)

      deallocate(Dcoeff,Dcoeffuv,Derr,TNcoeff,TNcoeffuv)
       
! example 4 20
! call of 4-point tensor components up to rank = 3
    case (20)
      
      write(*,'(/(a))') ' Calculating 4-point tensor components'
      
      p1vec = (/ 1d2,0d0,0d0,1d2 /)
      p2vec = (/ 2d2,0d0,0d0,0d0 /)
      p3vec = (/ 1d2,5d1,0d0,5d1 /)
      p10 = 0d0
      p21 = 0d0
      p32 = 5d3
      p30 = 5d3
      p20 = 4d4
      p31 = -5d3
      m02 = 1d2
      m12 = 3d2
      m22 = 1d2
      m32 = 3d2

      MomVec(0:3,1) = p1vec
      MomVec(0:3,2) = p2vec
      MomVec(0:3,3) = p3vec
      MomInv(1:6) = (/p10,p21,p32,p30,p20,p31/)
      masses2(0:3) = (/m02,m12,m22,m32/)

! allocate fields for tensor coefficients
      N = 4
      rank = 3
      allocate(Dten(0:rank,0:rank,0:rank,0:rank))
      allocate(Dtenuv(0:rank,0:rank,0:rank,0:rank))
      allocate(Derr(0:rank))
      

! Dten(n0,n1,n2,n3) contains the results for the corresponding tensor component
! Dtenuv(n0,n1,n2,n3) contains the results for the UV-singular part of the tensor component
! (coefficient of 2/(4-D)=1/eps term)
! Derr(r) is optional and contains an absolute estimate of the error of the
!         tensor components of rank r (not involving metric tensors)
      call Dten_cll(Dten,Dtenuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,Derr)  

! equivalent calls
      call Dten_cll(Dten,Dtenuv,MomVec(0:3,1:3),MomInv(1:6),masses2(0:3),rank,Derr)
! or
      call TNten_cll(Dten,Dtenuv,MomVec(0:3,1:3),MomInv(1:6),masses2(0:3),N,rank,Derr)

! equivalent calls, but with all components are in 1-dimensional array TNten
      allocate(TNten(GetNt_cll(rank)))
      allocate(TNtenuv(GetNt_cll(rank)))
      call Dten_cll(TNten,TNtenuv,p1vec,p2vec,p3vec,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rank,Derr)
! or
      call Dten_cll(TNten,TNtenuv,MomVec(0:3,1:3),MomInv(1:6),masses2(0:3),rank,Derr)
! or
      call TNten_cll(TNten,TNtenuv,MomVec(0:3,1:3),MomInv(1:6),masses2(0:3),N,rank,Derr)  

! writes the result to 'demo_4point_example20.dat'
      call writeresultDten(caseN,casei,Dten,Dtenuv,MomVec(0:3,1:3),MomInv(1:6),masses2(0:3),rank,Derr)      

      deallocate(Dten,Dtenuv,Derr,TNten,TNtenuv)
      
    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select

! 5-point function calls
  case(5)

    write(*,*) 
    write(*,*)  'Choose test case for 5-point function:'
    write (*,*) '10) tensor 5-point function: coefficients up to rank 3'
    write (*,*) '20) tensor 5-point function: tensor components of rank 3'
    read(*,*) casei
  
    select case(casei)

       
! example 5 10
! call of 5-point coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating 5-point coefficients'
      
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2
      m42 = 4d2
      masses2(0:4) = (/m02,m12,m22,m32,m42/)

      call getinvariants(5,MomInv(1:10))
      p10 = MomInv(1)
      p21 = MomInv(2)
      p32 = MomInv(3)
      p43 = MomInv(4)
      p40 = MomInv(5)
      p20 = MomInv(6)
      p31 = MomInv(7)
      p42 = MomInv(8)
      p30 = MomInv(9)
      p41 = MomInv(10)

! allocate fields for tensor coefficients
      rank = 3
      allocate(Ecoeff(0:rank/2,0:rank,0:rank,0:rank,0:rank))
      allocate(Ecoeffuv(0:rank/2,0:rank,0:rank,0:rank,0:rank))
      allocate(Eerr(0:rank))
      
! Ecoeff(n0,n1,n2,n3,n4) contains the results for the corresponding tensor coefficent
! Ecoeffuv(n0,n1,n2,n3,n4) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Eerr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call E_cll(Ecoeff,Ecoeffuv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rank,Eerr)  

! equivalent call 
      call E_cll(Ecoeff,Ecoeffuv,MomInv(1:10),masses2(0:4),rank,Eerr)  
 
! equivalent calls, but all coefficients are in 1-dimensional array TNcoeff 
      N = 5
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call E_cll(TNcoeff,TNcoeffuv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rank,Eerr)
! or
      call E_cll(TNcoeff,TNcoeffuv,MomInv(1:10),masses2(0:4),rank,Eerr)
! or
      call TN_cll(TNcoeff,TNcoeffuv,MomInv(1:10),masses2(0:4),N,rank,Eerr)     

! writes the result to 'demo_5point_example10.dat'
      call writeresultE(caseN,casei,Ecoeff,Ecoeffuv,MomInv(1:10),masses2(0:4),rank,Eerr)

      deallocate(Ecoeff,Ecoeffuv,Eerr,TNcoeff,TNcoeffuv)
       
! example 5 20
! call of 5-point tensor components up to rank = 3
    case (20)
      
      write(*,'(/(a))') ' Calculating 5-point tensor components'
      
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2
      m42 = 4d2
      masses2(0:4) = (/m02,m12,m22,m32,m42/)

      call getinvariants(5,MomInv(1:10),MomVec(0:3,1:4))
      p1vec = MomVec(0:3,1)
      p2vec = MomVec(0:3,2)
      p3vec = MomVec(0:3,3)
      p4vec = MomVec(0:3,4)
      p10 = MomInv(1)
      p21 = MomInv(2)
      p32 = MomInv(3)
      p43 = MomInv(4)
      p40 = MomInv(5)
      p20 = MomInv(6)
      p31 = MomInv(7)
      p42 = MomInv(8)
      p30 = MomInv(9)
      p41 = MomInv(10)

! allocate fields for tensor components
      N = 5
      rank = 3
      allocate(Eten(0:rank,0:rank,0:rank,0:rank))
      allocate(Etenuv(0:rank,0:rank,0:rank,0:rank))
      allocate(Eerr(0:rank))
      
! Eten(n0,n1,n2,n3) contains the results for the corresponding tensor component
! Etenuv(n0,n1,n2,n3) contains the results for the UV-singular part of the tensor component
! (coefficient of 2/(4-D)=1/eps term)
! Eerr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call Eten_cll(Eten,Etenuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rank,Eerr)  

! equivalent calls
      call Eten_cll(Eten,Etenuv,MomVec(0:3,1:4),MomInv(1:10),masses2(0:4),rank,Eerr)
! or
      call TNten_cll(Eten,Etenuv,MomVec(0:3,1:4),MomInv(1:10),masses2(0:4),N,rank,Eerr)

! equivalent calls, but with all components are in 1-dimensional array TNten
      allocate(TNten(GetNt_cll(rank)))
      allocate(TNtenuv(GetNt_cll(rank)))
      call Eten_cll(TNten,TNtenuv,p1vec,p2vec,p3vec,p4vec,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12,m22,m32,m42,rank,Eerr)
! or
      call Eten_cll(TNten,TNtenuv,MomVec(0:3,1:4),MomInv(1:10),masses2(0:4),rank,Eerr)
! or
      call TNten_cll(TNten,TNtenuv,MomVec(0:3,1:4),MomInv(1:10),masses2(0:4),N,rank,Eerr)

! writes the result to 'demo_5point_example20.dat'
      call writeresultEten(caseN,casei,Eten,Etenuv,MomVec(0:3,1:4),MomInv(1:10),masses2(0:4),rank,Eerr)
      
      deallocate(Eten,Etenuv,Eerr,TNten,TNtenuv)
      
    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select

! 6-point function calls
  case(6)

    write(*,*) 
    write(*,*)  'Choose test case for 6-point function:'
    write (*,*) '10) tensor 6-point function: coefficients up to rank 3'
    write (*,*) '20) tensor 6-point function: tensor components of rank 3'
    read(*,*) casei
  
    select case(casei)
       
! example 6 10
! call of 6-point coefficients up to rank = 3
    case (10)
      
      write(*,'(/(a))') ' Calculating 6-point coefficients'
      
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2
      m42 = 4d2
      m52 = 5d2
      masses2(0:5) = (/m02,m12,m22,m32,m42,m52/)

      call getinvariants(6,MomInv(1:15))

      p10 = MomInv(1)
      p21 = MomInv(2)
      p32 = MomInv(3)
      p43 = MomInv(4)
      p54 = MomInv(5)
      p50 = MomInv(6)
      p20 = MomInv(7)
      p31 = MomInv(8)
      p42 = MomInv(9)
      p53 = MomInv(10)
      p40 = MomInv(11)
      p51 = MomInv(12)
      p30 = MomInv(13)
      p41 = MomInv(14)
      p52 = MomInv(15)

! allocate fields for tensor coefficients
      rank = 3
      allocate(Fcoeff(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank))
      allocate(Fcoeffuv(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank))
      allocate(Ferr(0:rank))
      
! Fcoeff(n0,n1,n2,n3,n4) contains the results for the corresponding tensor coefficent
! Fcoeffuv(n0,n1,n2,n3,n4) contains the results for the UV-singular part of the tensor coefficent
! (coefficient of 2/(4-D)=1/eps term)
! Ferr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call F_cll(Fcoeff,Fcoeffuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
          p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,Ferr)  

! equivalent call 
      call F_cll(Fcoeff,Fcoeffuv,MomInv(1:15),masses2(0:5),rank,Ferr)  
 
! equivalent calls, but all coefficients are in 1-dimensional array TNcoeff 
      N = 6
      allocate(TNcoeff(GetNc_cll(N,rank)))
      allocate(TNcoeffuv(GetNc_cll(N,rank)))
      call F_cll(TNcoeff,TNcoeffuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
          p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,Ferr)
! or
      call F_cll(TNcoeff,TNcoeffuv,MomInv(1:15),masses2(0:5),rank,Ferr)
! or
      call TN_cll(TNcoeff,TNcoeffuv,MomInv(1:15),masses2(0:5),N,rank,Ferr)          
      

! writes the result to 'demo_6point_example10.dat'
      call writeresultF(caseN,casei,Fcoeff,Fcoeffuv,MomInv(1:15),masses2(0:5),rank,Ferr)

      deallocate(Fcoeff,Fcoeffuv,Ferr,TNcoeff,TNcoeffuv)
       
! example 6 20
! call of 6-point tensor components up to rank = 3
    case (20)
      
      write(*,'(/(a))') ' Calculating 6-point tensor components'
      
      m02 = 1d2
      m12 = 1d2
      m22 = 3d2
      m32 = 3d2
      m42 = 4d2
      m52 = 5d2
      masses2(0:5) = (/m02,m12,m22,m32,m42,m52/)

      call getinvariants(6,MomInv(1:15),MomVec(0:3,1:5))

      p1vec = MomVec(0:3,1)
      p2vec = MomVec(0:3,2)
      p3vec = MomVec(0:3,3)
      p4vec = MomVec(0:3,4)
      p5vec = MomVec(0:3,5)
      p10 = MomInv(1)
      p21 = MomInv(2)
      p32 = MomInv(3)
      p43 = MomInv(4)
      p54 = MomInv(5)
      p50 = MomInv(6)
      p20 = MomInv(7)
      p31 = MomInv(8)
      p42 = MomInv(9)
      p53 = MomInv(10)
      p40 = MomInv(11)
      p51 = MomInv(12)
      p30 = MomInv(13)
      p41 = MomInv(14)
      p52 = MomInv(15)

! allocate fields for tensor coefficients
      N = 6
      rank = 3
      allocate(Ften(0:rank,0:rank,0:rank,0:rank))
      allocate(Ftenuv(0:rank,0:rank,0:rank,0:rank))
      allocate(Ferr(0:rank))
      
! Ften(n0,n1,n2,n3) contains the results for the corresponding tensor component
! Ftenuv(n0,n1,n2,n3) contains the results for the UV-singular part of the tensor component
! (coefficient of 2/(4-D)=1/eps term)
! Ferr(r) is optional and contains an absolute estimate of the error of the
!         tensor coefficients of rank r (not involving metric tensors)
      call Ften_cll(Ften,Ftenuv,p1vec,p2vec,p3vec,p4vec,p5vec,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
          p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,Ferr)  

! equivalent calls
      call Ften_cll(Ften,Ftenuv,MomVec(0:3,1:5),MomInv(1:15),masses2(0:5),rank,Ferr)
! or
      call TNten_cll(Ften,Ftenuv,MomVec(0:3,1:5),MomInv(1:15),masses2(0:5),N,rank,Ferr)

! equivalent calls, but with all components are in 1-dimensional array TNten
      allocate(TNten(GetNt_cll(rank)))
      allocate(TNtenuv(GetNt_cll(rank)))
      call Ften_cll(TNten,TNtenuv,p1vec,p2vec,p3vec,p4vec,p5vec,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
          p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,Ferr)
! or
      call Ften_cll(TNten,TNtenuv,MomVec(0:3,1:5),MomInv(1:15),masses2(0:5),rank,Ferr)
! or
      call TNten_cll(TNten,TNtenuv,MomVec(0:3,1:5),MomInv(1:15),masses2(0:5),N,rank,Ferr) 

! writes the result to 'demo_6point_example10.dat'
      call writeresultFten(caseN,casei,Ften,Ftenuv,MomVec(0:3,1:5),MomInv(1:15),masses2(0:5),rank,Ferr)
      
      deallocate(Ften,Ftenuv,Ferr,TNten,TNtenuv)
      
    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select

! calls for derivatives of scalar 2-point function 
  case(12)

    write(*,*) 
    write(*,*)  'Choose test case for 2-point function derivative:'
    write (*,*) ' 1) scalar 2-point function derivative'
    write (*,*) ' 2) vector 2-point function derivative'
    write (*,*) ' 3) tensor 2-point function derivative'
    write (*,*) ' 4) mass-singular scalar 2-point function derivative'
    write (*,*) '10) tensor 2-point function derivatives up to rank 2'
    read(*,*) casei
  
    select case(casei)

! example 12  1
! call of derivative of scalar 2-point function
    case(1) 

      write(*,'(/(a)/)') ' Calculating derivative of scalar 2-point function'

      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! DB0 contains the results for the derivative of the scalar 2-point function
      call DB0_cll(DB0,p10,m02,m12)  

! equivalent call 
      call DB0_cll(DB0,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_derivative_example01.dat'
      call writeresultDB0(caseN,casei,DB0,MomInv(1:1),masses2(0:1))


! example 12  2
! call of derivative of vector 2-point function coefficient
    case(2) 

      write(*,'(/(a)/)') ' Calculating derivative of vector 2-point function coefficient'

      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! DB1 contains the results for the derivative of the scalar 2-point function
      call DB1_cll(DB1,p10,m02,m12)  

! equivalent call 
      call DB1_cll(DB1,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_derivative_example02.dat'
      rank = 1
      allocate(DBcoeff(0:rank/2,0:rank))
      allocate(DBcoeffuv(0:rank/2,0:rank))
      allocate(DBerr(0:rank)) 
      DBcoeff(0,1) = DB1
      call writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv(1:1),masses2(0:1),1,1)


! example 12  3
! call of derivative of tensor 2-point function coefficient
    case(3) 

      write(*,'(/(a)/)') ' Calculating derivative of tensor 2-point function coefficient'

      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! DB00 contains the results for the derivative of the scalar 2-point function
      call DB00_cll(DB00,DB00uv,p10,m02,m12)  

! equivalent call 
      call DB00_cll(DB00,DB00uv,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_derivative_example03.dat'
      rank = 2
      allocate(DBcoeff(0:rank/2,0:rank))
      allocate(DBcoeffuv(0:rank/2,0:rank))
      allocate(DBerr(0:rank)) 
      DBcoeff(1,0) = DB00
      call writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv(1:1),masses2(0:1),2,2)


! example 12  4
! call of derivative of mass-singular scalar 2-point function
    case(4) 

      write(*,'(/(a))') ' Calculating derivative of mass-singular scalar 2-point function'
      write(*,'((a)/)') ' in mass regularization'
      write(*,'((a)/)') ' mass-singularity described by log(m12)'

      p10 = 0d0
      m02 = 1d-4
      m12 = 1d-4

! clears list of regulator masses
      call clearminf2_cll
! adds m12 to list of regulator masses
! these are treated as infinitesimal apart from their appearance
! in IR-singular logarithms, where the actual values are taken
      call addminf2_cll(m12)
   
      MomInv(1:1) = p10
      masses2(0:1) = (/m02,m12/)

! DB0 contains the results for the scalar 2-point function
      call DB0_cll(DB0,p10,m02,m12)  

! equivalent call 
      call DB0_cll(DB0,MomInv(1:1),masses2(0:1))  

! writes the result to 'demo_2point_example04.dat'
      call writeresultDB0(caseN,casei,DB0,MomInv(1:1),masses2(0:1))

  
! example 12 10
! call of derivatives of tensor 2-point function coefficients up to rank = 2
    case (10)
      
      write(*,'(/(a))') ' Calculating derivatives of tensor 2-point coefficients'
      
      p10 = 1d3
      m02 = 4d2
      m12 = 1d2

      MomInv(1:1) = (/p10/)
      masses2(0:1) = (/m02,m12/)

! allocate fields for tensor coefficients
      rank = 2
      allocate(DBcoeff(0:rank/2,0:rank))
      allocate(DBcoeffuv(0:rank/2,0:rank))
      allocate(DBerr(0:rank))
      

! DBcoeff(n0,n1) contains the results for the corresponding tensor coefficent derivative
! DBcoeffuv(n0,n1) contains the results for the UV-singular part of the tensor coefficent derivative
! (coefficient of 2/(4-D)=1/eps term)
! DBerr(r) is optional and contains an absolute estimate of the error of the derivative of the
!         tensor coefficients of rank r (not involving metric tensors)
      call DB_cll(DBcoeff,DBcoeffuv,p10,m02,m12,rank,DBerr)  

! equivalent call 
      call DB_cll(DBcoeff,DBcoeffuv,MomInv(1:1),masses2(0:1),rank,DBerr)  

! writes the result to 'demo_2point_derivative_example10.dat'
      call writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv(1:1),masses2(0:1),0,rank,Berr)
      
      deallocate(DBcoeff,DBcoeffuv,DBerr)

    case default

      write(*,'(/(a))') ' Input does not correspond to predefined sample'

    end select

  case default

    write(*,'(/(a))') ' Input does not correspond to predefined sample'

  end select

  contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultA(caseN,casei,Acoeff,Acoeffuv,masses2,rank,Aerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultA(caseN,casei,Acoeff,Acoeffuv,masses2,rank,Aerr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: masses2(0:0)
    double complex, intent(in) :: Acoeff(0:rank/2)
    double complex, intent(in) :: Acoeffuv(0:rank/2)
    double precision, optional, intent(in) :: Aerr(0:rank)
    integer :: r,n0,mode
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Acoeff(',i1,') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Aerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  A0 = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt13 = "('  Acoeffuv(',i1,') = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_1point_example00_coli.dat'
      case(2)
        fname = 'demo_1point_example00_dd.dat'
      case(3)
        fname = 'demo_1point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
!    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
!    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 1-point function, example',caseN,casei
    write (50,'((a))')     '                                    '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '               /       \            '
    write (50,'((a))')     '              /         \           '
    write (50,'((a))')     '    0d0  -----        0 |  m02 '
    write (50,'((a))')     '              \         /           '
    write (50,'((a))')     '               \       /            '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '                                    '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt2) '  muUV2   ',muUV2
!    write (50,fmt2) '  muIR2   ',muIR2
    write (50,fmt2) '  deltaUV ',deltaUV
!    write (50,fmt2) '  deltaIR1',deltaIR2
!    write (50,fmt2) '  deltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.gt.0) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  A =   ---------------  \int d^D q f(q)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  A_fin(muUV2) + a_UV*DeltaUV  '
    else
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  A0 =   ---------------  \int d^D q f(q)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '     =  A0_fin(muUV2) + a_UV*DeltaUV  '
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    write (50,'((a))') &
        '              c(epsUV) '
    write (50,'((a))') & 
        '   DeltaUV =  -------- '
    write (50,'((a))') &
        '               epsUV   '
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    write (50,'((a))') '   - by default DeltaUV = 0 '
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do n0=0,rank/2
        write (50,fmt10) n0,Acoeff(n0) 
      end do
      
      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Aerr(r) 
      end do
    else
      write (50,fmt12) Acoeff(0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultA


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultA0(caseN,casei,A0,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultA0(caseN,casei,A0,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: A0
      double complex, intent(in) :: masses2(0:0)
      double complex :: Acoeff(0:0)
      double complex :: Acoeffuv(0:0)
      
      
      Acoeff(0:0) = A0
      Acoeffuv(0:0) = masses2
      call  writeresultA(caseN,casei,Acoeff,Acoeffuv,masses2,0)
 
    end  subroutine writeresultA0


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultB(caseN,casei,Bcoeff,Bcoeffuv,MomInv,masses2,rank,Berr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultB(caseN,casei,Bcoeff,Bcoeffuv,MomInv,masses2,rank,Berr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(in) :: Bcoeff(0:rank/2,0:rank)
    double complex, intent(in) :: Bcoeffuv(0:rank/2,0:rank)
    double precision, optional, intent(in) :: Berr(0:rank)
    integer :: r,n0,n1,mode
    integer,parameter :: rankuv=0
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Bcoeff(',i1,1(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Berr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  B0 = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt13 = "('  Bcoeffuv(',i1,1(',',i1),') = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_2point_example00_coli.dat'
      case(2)
        fname = 'demo_2point_example00_dd.dat'
      case(3)
        fname = 'demo_2point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 2-point function, example',caseN,casei
    write (50,'((a))')     '                  m12               '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '               /   1   \            '
    write (50,'((a))')     '              /         \           '
    write (50,'((a))')     '    p10  -----           -----  p10 '
    write (50,'((a))')     '              \         /           '
    write (50,'((a))')     '               \   0   /            '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '                  m02               '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
!    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV ',deltaUV
    end if
!    write (50,fmt2) '  deltaIR1',deltaIR2
!    write (50,fmt2) '  deltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.gt.0) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  B =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  B_fin(muUV2,muIR2) + a_UV*DeltaUV  '
    else
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  B0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '     =  B0_fin(muUV2,muIR2) + a_UV*DeltaUV  '
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    write (50,'((a))') &
        '              c(epsUV) '
    write (50,'((a))') & 
        '   DeltaUV =  -------- '
    write (50,'((a))') &
        '               epsUV   '
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    write (50,'((a))') '   - by default DeltaUV = 0 '
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          n1=r-2*n0
          write (50,fmt10) n0,n1,Bcoeff(n0,n1) 
        end do
      end do
      
!      do r=0,rank
!        do n0=0,r/2
!          n1=r-2*n0
!          write (50,fmt13) n0,n1,Bcoeffuv(n0,n1) 
!        end do
!      end do
!      
      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Berr(r) 
      end do
    else
      write (50,fmt12) Bcoeff(0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultB


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultB0(caseN,casei,B0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultB0(caseN,casei,B0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: B0
      double complex, intent(in) :: MomInv(1), masses2(0:1)
      double complex :: Bcoeff(0:0,0:0)
      double complex :: Bcoeffuv(0:0,0:0)
      
      
      Bcoeff(0:0,0:0) = B0
      Bcoeffuv(0:0,0:0) = 1d0
      call  writeresultB(caseN,casei,Bcoeff,Bcoeffuv,MomInv,masses2,0)
 
    end  subroutine writeresultB0



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultC(caseN,casei,Ccoeff,Ccoeffuv,MomInv,masses2,rank,Cerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultC(caseN,casei,Ccoeff,Ccoeffuv,MomInv,masses2,rank,Cerr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomInv(3), masses2(0:2)
    double complex, intent(in) :: Ccoeff(0:rank/2,0:rank,0:rank)
    double complex, intent(in) :: Ccoeffuv(0:rank/2,0:rank,0:rank)
    double precision, optional, intent(in) :: Cerr(0:rank)
    integer :: r,n0,n1,n2,mode
    integer,parameter :: rankuv=2 
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Ccoeff(',i1,2(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Cerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  C0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_3point_example00_coli.dat'
      case(2)
        fname = 'demo_3point_example00_dd.dat'
      case(3)
        fname = 'demo_3point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 3-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))')     '                  p21               '
    write (50,'((a))')     '                   |                '
    write (50,'((a))')     '                   |                '
    write (50,'((a))')     '                  / \               '
    write (50,'((a))')     '                 /   \              '
    write (50,'((a))')     '          m12   /1   2\   m22       '
    write (50,'((a))')     '               /       \            '
    write (50,'((a))')     '              /    0    \           '
    write (50,'((a))')     '    p10  ---------------------  p20 '
    write (50,'((a))')     '                  m02               '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  p21     ',MomInv(2)
    write (50,fmt1) '  p20     ',MomInv(3)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    write (50,fmt1) '  m22     ',masses2(2)
    if (rank.ge.2) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.2) then
      write (50,fmt2) '  DeltaUV ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1',deltaIR2
    write (50,fmt2) '  DeltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else if(rank.ge.rankuv) then  
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.gt.0) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.gt.0) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.gt.0) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          do n1=0,r-2*n0
            n2 = r-2*n0-n1
            write (50,fmt10) n0,n1,n2,Ccoeff(n0,n1,n2) 
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Cerr(r) 
      end do
    else
      write (50,fmt12) Ccoeff(0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultC


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultC0(caseN,casei,Ccoeff,Ccoeffuv,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultC0(caseN,casei,C0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: C0
      double complex, intent(in) :: MomInv(3), masses2(0:2)
      double complex :: Ccoeff(0:0,0:0,0:0)
      double complex :: Ccoeffuv(0:0,0:0,0:0)
      
      
      Ccoeff(0:0,0:0,0:0) = C0
      Ccoeffuv(0:0,0:0,0:0) = 0d0
      call  writeresultC(caseN,casei,Ccoeff,Ccoeffuv,MomInv,masses2,0)
 
    end  subroutine writeresultC0



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultCten(caseN,casei,Cten,Ctenuv,MomInv,masses2,rank,Cerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultCten(caseN,casei,Cten,Ctenuv,MomVec,MomInv,masses2,rank,Cerr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomVec(0:3,1:2), MomInv(3), masses2(0:2)
    double complex, intent(in) :: Cten(0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Ctenuv(0:rank,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Cerr(0:rank)
    integer :: r,n0,n1,n2,n3,mode
    integer,parameter :: rankuv=2 
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A17,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A17,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Cten(',i1,3(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Cerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  C0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_3point_example00_coli.dat'
      case(2)
        fname = 'demo_3point_example00_dd.dat'
      case(3)
        fname = 'demo_3point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 3-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))')     '                  p21               '
    write (50,'((a))')     '                   |                '
    write (50,'((a))')     '                   |                '
    write (50,'((a))')     '                  / \               '
    write (50,'((a))')     '                 /   \              '
    write (50,'((a))')     '    m12,p1vec   /1   2\   m22,p2vec '
    write (50,'((a))')     '               /       \            '
    write (50,'((a))')     '              /    0    \           '
    write (50,'((a))')     '    p10  ---------------------  p20 '
    write (50,'((a))')     '                  m02               '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p1vec(0)     ',MomVec(0,1)
    write (50,fmt1) '  p1vec(1)     ',MomVec(1,1)
    write (50,fmt1) '  p1vec(2)     ',MomVec(2,1)
    write (50,fmt1) '  p1vec(3)     ',MomVec(3,1)
    write (50,fmt1) '  p2vec(0)     ',MomVec(0,2)
    write (50,fmt1) '  p2vec(1)     ',MomVec(1,2)
    write (50,fmt1) '  p2vec(2)     ',MomVec(2,2)
    write (50,fmt1) '  p2vec(3)     ',MomVec(3,2)    
    write (50,fmt1) '  p10          ',MomInv(1)
    write (50,fmt1) '  p21          ',MomInv(2)
    write (50,fmt1) '  p20          ',MomInv(3)
    write (50,fmt1) '  m02          ',masses2(0)
    write (50,fmt1) '  m12          ',masses2(1) 
    write (50,fmt1) '  m22          ',masses2(2)
    if (rank.ge.2) then
      write (50,fmt2) '  muUV2        ',muUV2
    end if
    write (50,fmt2) '  muIR2        ',muIR2
    if (rank.ge.2) then
      write (50,fmt2) '  DeltaUV      ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1     ',deltaIR2
    write (50,fmt2) '  DeltaIR2     ',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else if(rank.ge.rankuv) then  
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  C =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  C_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.gt.0) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.gt.0) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.gt.0) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3 = r-n0-n1-n2
              write (50,fmt10) n0,n1,n2,n3,Cten(n0,n1,n2,n3)
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Cerr(r) 
      end do
    else
      write (50,fmt12) Cten(0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultCten
    
    
    

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultD(caseN,casei,Dcoeff,Dcoeffuv,MomInv,masses2,rank,Derr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultD(caseN,casei,Dcoeff,Dcoeffuv,MomInv,masses2,rank,Derr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomInv(6), masses2(0:3)
    double complex, intent(in) :: Dcoeff(0:rank/2,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Dcoeffuv(0:rank/2,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Derr(0:rank)
    integer :: r,n0,n1,n2,n3,mode
    integer,parameter :: rankuv=4
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Dcoeff(',i1,3(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Derr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  D0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_4point_example00_coli.dat'
      case(2)
        fname = 'demo_4point_example00_dd.dat'
      case(3)
        fname = 'demo_4point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 4-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))')     '                  p31                  '
    write (50,'((a))')     '          ------------------           '
    write (50,'((a))')     '         /                   \         '
    write (50,'((a))')     '                  m22                  '
    write (50,'((a))')     '    p21  ---------------------  p32 \  '
    write (50,'((a))')     '              |    2    |            \ '
    write (50,'((a))')     '              |         |             |'
    write (50,'((a))')     '          m12 |1       3| m32         | p20'
    write (50,'((a))')     '              |         |             |'
    write (50,'((a))')     '              |    0    |            / '
    write (50,'((a))')     '    p10  ---------------------  p30 /  '
    write (50,'((a))')     '                  m02                  '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  p21     ',MomInv(2)
    write (50,fmt1) '  p32     ',MomInv(3)
    write (50,fmt1) '  p30     ',MomInv(4)
    write (50,fmt1) '  p20     ',MomInv(5)
    write (50,fmt1) '  p31     ',MomInv(6)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    write (50,fmt1) '  m22     ',masses2(2)
    write (50,fmt1) '  m32     ',masses2(3)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  DeltaUV ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1',deltaIR2
    write (50,fmt2) '  DeltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else if(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.gt.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.gt.0) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.gt.0) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          do n1=0,r-2*n0
            do n2=0,r-2*n0-n1
              n3 = r-2*n0-n1-n2
                write (50,fmt10) n0,n1,n2,n3,Dcoeff(n0,n1,n2,n3) 
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Derr(r) 
      end do
    else
      write (50,fmt12) Dcoeff(0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultD


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultD0(caseN,casei,Dcoeff,Dcoeffuv,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultD0(caseN,casei,D0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: D0
      double complex, intent(in) :: MomInv(6), masses2(0:3)
      double complex :: Dcoeff(0:0,0:0,0:0,0:0)
      double complex :: Dcoeffuv(0:0,0:0,0:0,0:0)
      
      
      Dcoeff(0,0,0,0) = D0
      Dcoeffuv(0,0,0,0) = 0d0
      call  writeresultD(caseN,casei,Dcoeff,Dcoeffuv,MomInv,masses2,0)
 
    end  subroutine writeresultD0
    
    
    

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultDten(caseN,casei,Dten,Dtenuv,MomVec,MomInv,masses2,rank,Derr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultDten(caseN,casei,Dten,Dtenuv,MomVec,MomInv,masses2,rank,Derr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomVec(0:3,3), MomInv(6), masses2(0:3)
    double complex, intent(in) :: Dten(0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Dtenuv(0:rank,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Derr(0:rank)
    integer :: r,n0,n1,n2,n3,mode
    integer,parameter :: rankuv=4
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A17,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A17,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Dten(',i1,3(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Derr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  D0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_4point_example00_coli.dat'
      case(2)
        fname = 'demo_4point_example00_dd.dat'
      case(3)
        fname = 'demo_4point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 4-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))')     '                  p31                  '
    write (50,'((a))')     '          ------------------           '
    write (50,'((a))')     '         /                   \         '
    write (50,'((a))')     '               m22,p2vec               '
    write (50,'((a))')     '    p21  ---------------------  p32 \  '
    write (50,'((a))')     '              |    2    |            \ '
    write (50,'((a))')     '              |         |             |'
    write (50,'((a))')     '    m12,p1vec |1       3| m32,p3vec   | p20'
    write (50,'((a))')     '              |         |             |'
    write (50,'((a))')     '              |    0    |            / '
    write (50,'((a))')     '    p10  ---------------------  p30 /  '
    write (50,'((a))')     '                  m02                  '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p1vec(0)     ',MomVec(0,1)
    write (50,fmt1) '  p1vec(1)     ',MomVec(1,1)
    write (50,fmt1) '  p1vec(2)     ',MomVec(2,1)
    write (50,fmt1) '  p1vec(3)     ',MomVec(3,1)
    write (50,fmt1) '  p2vec(0)     ',MomVec(0,2)
    write (50,fmt1) '  p2vec(1)     ',MomVec(1,2)
    write (50,fmt1) '  p2vec(2)     ',MomVec(2,2)
    write (50,fmt1) '  p2vec(3)     ',MomVec(3,2)
    write (50,fmt1) '  p3vec(0)     ',MomVec(0,3)
    write (50,fmt1) '  p3vec(1)     ',MomVec(1,3)
    write (50,fmt1) '  p3vec(2)     ',MomVec(2,3)
    write (50,fmt1) '  p3vec(3)     ',MomVec(3,3)   
    write (50,fmt1) '  p10          ',MomInv(1)
    write (50,fmt1) '  p21          ',MomInv(2)
    write (50,fmt1) '  p32          ',MomInv(3)
    write (50,fmt1) '  p30          ',MomInv(4)
    write (50,fmt1) '  p20          ',MomInv(5)
    write (50,fmt1) '  p31          ',MomInv(6)
    write (50,fmt1) '  m02          ',masses2(0)
    write (50,fmt1) '  m12          ',masses2(1) 
    write (50,fmt1) '  m22          ',masses2(2)
    write (50,fmt1) '  m32          ',masses2(3)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2        ',muUV2
    end if
    write (50,fmt2) '  muIR2        ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  DeltaUV      ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1     ',deltaIR2
    write (50,fmt2) '  DeltaIR2     ',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else if(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  D =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  D_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.gt.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.gt.0) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.gt.0) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3 = r-n0-n1-n2
                write (50,fmt10) n0,n1,n2,n3,Dten(n0,n1,n2,n3) 
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Derr(r) 
      end do
    else
      write (50,fmt12) Dten(0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultDten
    
    

    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultE(caseN,casei,Ecoeff,Ecoeffuv,MomInv,masses2,rank,Eerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultE(caseN,casei,Ecoeff,Ecoeffuv,MomInv,masses2,rank,Eerr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomInv(10), masses2(0:4)
    double complex, intent(in) :: Ecoeff(0:rank/2,0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Ecoeffuv(0:rank/2,0:rank,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Eerr(0:rank)
    integer :: r,n0,n1,n2,n3,n4,mode
    integer,parameter :: rankuv=6
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Ecoeff(',i1,4(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Eerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  E0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_5point_example00_coli.dat'
      case(2)
        fname = 'demo_5point_example00_dd.dat'
      case(3)
        fname = 'demo_5point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 5-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))') '                      p31                '
    write (50,'((a))') '              ---------^---------        '
    write (50,'((a))') '             /                   \       '
    write (50,'((a))') '                      m22                '
    write (50,'((a))') '       / p21 ---------------------  p32 \  '
    write (50,'((a))') '      /          |    2    \             \ '
    write (50,'((a))') '     /           |         3\ m32        | p42'
    write (50,'((a))') '     |           |1          \           / '
    write (50,'((a))') ' p20 |       m12 |            >---- p43 <  '
    write (50,'((a))') '     |           |           /           \ '
    write (50,'((a))') '     \           |         4/ m42        | p30' 
    write (50,'((a))') '      \          |    0    /             / '
    write (50,'((a))') '       \ p10 ---------------------  p40 /  '
    write (50,'((a))') '                      m02               '
    write (50,'((a))') '             \                   /       '
    write (50,'((a))') '              ---------v---------        '
    write (50,'((a))') '                      p41                '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  p21     ',MomInv(2)
    write (50,fmt1) '  p32     ',MomInv(3)
    write (50,fmt1) '  p43     ',MomInv(4)
    write (50,fmt1) '  p40     ',MomInv(5)
    write (50,fmt1) '  p20     ',MomInv(6)
    write (50,fmt1) '  p31     ',MomInv(7)
    write (50,fmt1) '  p42     ',MomInv(8)
    write (50,fmt1) '  p30     ',MomInv(9)
    write (50,fmt1) '  p41     ',MomInv(10)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    write (50,fmt1) '  m22     ',masses2(2)
    write (50,fmt1) '  m32     ',masses2(3)
    write (50,fmt1) '  m42     ',masses2(4)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1',deltaIR2
    write (50,fmt2) '  DeltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    elseif(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.ge.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.ge.rankuv) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.ge.rankuv) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          do n1=0,r-2*n0
            do n2=0,r-2*n0-n1
              do n3=0,r-2*n0-n1-n2
                n4 = r-2*n0-n1-n2-n3
                write (50,fmt10) n0,n1,n2,n3,n4,Ecoeff(n0,n1,n2,n3,n4) 
              end do
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Eerr(r) 
      end do
    else
      write (50,fmt12) Ecoeff(0,0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultE


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultE0(caseN,casei,Ecoeff,Ecoeffuv,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultE0(caseN,casei,E0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: E0
      double complex, intent(in) :: MomInv(10), masses2(0:4)
      double complex :: Ecoeff(0:0,0:0,0:0,0:0,0:0)
      double complex :: Ecoeffuv(0:0,0:0,0:0,0:0,0:0)
      
      
      Ecoeff(0,0,0,0,0) = E0
      Ecoeffuv(0,0,0,0,0) = 0d0
      call  writeresultE(caseN,casei,Ecoeff,Ecoeffuv,MomInv,masses2,0)
 
    end  subroutine writeresultE0
    
    

    
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultEten(caseN,casei,Eten,Etenuv,MomVec,MomInv,masses2,rank,Eerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultEten(caseN,casei,Eten,Etenuv,MomVec,MomInv,masses2,rank,Eerr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomVec(0:3,4), MomInv(10), masses2(0:4)
    double complex, intent(in) :: Eten(0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Etenuv(0:rank,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Eerr(0:rank)
    integer :: r,n0,n1,n2,n3,mode
    integer,parameter :: rankuv=6
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A17,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A17,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Eten(',i1,3(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Eerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  E0 = ',es23.16,' + i*',es23.16)"

! output file for result 
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_5point_example00_coli.dat'
      case(2)
        fname = 'demo_5point_example00_dd.dat'
      case(3)
        fname = 'demo_5point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 5-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))') '                      p31                '
    write (50,'((a))') '              ---------^---------        '
    write (50,'((a))') '             /                   \       '
    write (50,'((a))') '                  m22,p2vec              '
    write (50,'((a))') '       / p21 ---------------------  p32 \  '
    write (50,'((a))') '      /          |    2    \             \ '
    write (50,'((a))') '     /           |         3\ m32,p3vec  | p42'
    write (50,'((a))') '     |           |1          \           / '
    write (50,'((a))') ' p20 |  m12,p1vec|            >---- p43 <  '
    write (50,'((a))') '     |           |           /           \ '
    write (50,'((a))') '     \           |         4/ m42,p4vec  | p30' 
    write (50,'((a))') '      \          |    0    /             / '
    write (50,'((a))') '       \ p10 ---------------------  p40 /  '
    write (50,'((a))') '                      m02               '
    write (50,'((a))') '             \                   /       '
    write (50,'((a))') '              ---------v---------        '
    write (50,'((a))') '                      p41                '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p1vec(0)     ',MomVec(0,1)
    write (50,fmt1) '  p1vec(1)     ',MomVec(1,1)
    write (50,fmt1) '  p1vec(2)     ',MomVec(2,1)
    write (50,fmt1) '  p1vec(3)     ',MomVec(3,1)
    write (50,fmt1) '  p2vec(0)     ',MomVec(0,2)
    write (50,fmt1) '  p2vec(1)     ',MomVec(1,2)
    write (50,fmt1) '  p2vec(2)     ',MomVec(2,2)
    write (50,fmt1) '  p2vec(3)     ',MomVec(3,2)
    write (50,fmt1) '  p3vec(0)     ',MomVec(0,3)
    write (50,fmt1) '  p3vec(1)     ',MomVec(1,3)
    write (50,fmt1) '  p3vec(2)     ',MomVec(2,3)
    write (50,fmt1) '  p3vec(3)     ',MomVec(3,3)
    write (50,fmt1) '  p4vec(0)     ',MomVec(0,4)
    write (50,fmt1) '  p4vec(1)     ',MomVec(1,4)
    write (50,fmt1) '  p4vec(2)     ',MomVec(2,4)
    write (50,fmt1) '  p4vec(3)     ',MomVec(3,4)    
    write (50,fmt1) '  p10          ',MomInv(1)
    write (50,fmt1) '  p21          ',MomInv(2)
    write (50,fmt1) '  p32          ',MomInv(3)
    write (50,fmt1) '  p43          ',MomInv(4)
    write (50,fmt1) '  p40          ',MomInv(5)
    write (50,fmt1) '  p20          ',MomInv(6)
    write (50,fmt1) '  p31          ',MomInv(7)
    write (50,fmt1) '  p42          ',MomInv(8)
    write (50,fmt1) '  p30          ',MomInv(9)
    write (50,fmt1) '  p41          ',MomInv(10)
    write (50,fmt1) '  m02          ',masses2(0)
    write (50,fmt1) '  m12          ',masses2(1) 
    write (50,fmt1) '  m22          ',masses2(2)
    write (50,fmt1) '  m32          ',masses2(3)
    write (50,fmt1) '  m42          ',masses2(4)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2        ',muUV2
    end if
    write (50,fmt2) '  muIR2        ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV      ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1     ',deltaIR2
    write (50,fmt2) '  DeltaIR2     ',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    elseif(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  E =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  E_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.ge.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.ge.rankuv) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.ge.rankuv) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              write (50,fmt10) n0,n1,n2,n3,Eten(n0,n1,n2,n3) 
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Eerr(r) 
      end do
    else
      write (50,fmt12) Eten(0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultEten
    
    
    
    
    

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultF(caseN,casei,Fcoeff,Fcoeffuv,MomInv,masses2,rank,Ferr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultF(caseN,casei,Fcoeff,Fcoeffuv,MomInv,masses2,rank,Ferr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double complex, intent(in) :: MomInv(15), masses2(0:5)
    double complex, intent(in) :: Fcoeff(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Fcoeffuv(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank)
    double precision, optional, intent(in) :: Ferr(0:rank)
    integer :: r,n0,n1,n2,n3,n4,n5,mode
    integer,parameter :: rankuv=8
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Fcoeff(',i1,5(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Ferr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  F0 = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_6point_example00_coli.dat'
      case(2)
        fname = 'demo_6point_example00_dd.dat'
      case(3)
        fname = 'demo_6point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 6-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))') '                     p42                  '
    write (50,'((a))') '             ---------^---------          '
    write (50,'((a))') '            /                   \         '
    write (50,'((a))') '                     m32                  '
    write (50,'((a))') '      / p32 ---------------------  p43 \  '
    write (50,'((a))') '     /             /  3   \             \ '
    write (50,'((a))') ' p31 |        m22 /2      4\ m42        | p53'
    write (50,'((a))') '     \           /          \           / '
    write (50,'((a))') '      > p21 ----<            >---- p54 <  '
    write (50,'((a))') '     /           \          /           \ '
    write (50,'((a))') ' p20 |        m12 \1      5/ m52        | p40' 
    write (50,'((a))') '     \             \   0  /             / '
    write (50,'((a))') '      \ p10 ---------------------  p50 /  '
    write (50,'((a))') '                     m02                  '
    write (50,'((a))') '            \                   /         '
    write (50,'((a))') '             ---------v---------          '
    write (50,'((a))') '                     p51                  '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  p21     ',MomInv(2)
    write (50,fmt1) '  p32     ',MomInv(3)
    write (50,fmt1) '  p43     ',MomInv(4)
    write (50,fmt1) '  p54     ',MomInv(5)
    write (50,fmt1) '  p50     ',MomInv(6)
    write (50,fmt1) '  p20     ',MomInv(7)
    write (50,fmt1) '  p31     ',MomInv(8)
    write (50,fmt1) '  p42     ',MomInv(9)
    write (50,fmt1) '  p53     ',MomInv(10)
    write (50,fmt1) '  p40     ',MomInv(11)
    write (50,fmt1) '  p51     ',MomInv(12)
    write (50,fmt1) '  p30     ',MomInv(13)
    write (50,fmt1) '  p41     ',MomInv(14)
    write (50,fmt1) '  p52     ',MomInv(15)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    write (50,fmt1) '  m22     ',masses2(2)
    write (50,fmt1) '  m32     ',masses2(3)
    write (50,fmt1) '  m42     ',masses2(4)
    write (50,fmt1) '  m52     ',masses2(5)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1',deltaIR2
    write (50,fmt2) '  DeltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    elseif(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.ge.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.ge.rankuv) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.ge.rankuv) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r/2
          do n1=0,r-2*n0
            do n2=0,r-2*n0-n1
              do n3=0,r-2*n0-n1-n2
                do n4=0,r-2*n0-n1-n2-n3
                  n5 = r-2*n0-n1-n2-n3-n4
                  write (50,fmt10) n0,n1,n2,n3,n4,n5,Fcoeff(n0,n1,n2,n3,n4,n5) 
                end do
              end do
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Ferr(r) 
      end do
    else
      write (50,fmt12) Fcoeff(0,0,0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultF


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultF0(caseN,casei,Fcoeff,Fcoeffuv,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultF0(caseN,casei,F0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: F0
      double complex, intent(in) :: MomInv(15), masses2(0:5)
      double complex :: Fcoeff(0:0,0:0,0:0,0:0,0:0,0:0)
      double complex :: Fcoeffuv(0:0,0:0,0:0,0:0,0:0,0:0)
      
      
      Fcoeff(0,0,0,0,0,0) = F0
      Fcoeffuv(0,0,0,0,0,0) = 0d0
      call  writeresultF(caseN,casei,Fcoeff,Fcoeffuv,MomInv,masses2,0)
 
    end  subroutine writeresultF0
    
    
    
    
    

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultFten(caseN,casei,Ften,Ftenuv,MomVec,MomInv,masses2,rank,Ferr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine writeresultFten(caseN,casei,Ften,Ftenuv,MomVec,MomInv,masses2,rank,Ferr)
    implicit none
   
    integer, intent(in) :: rank,caseN,casei
    double precision, optional, intent(in) :: Ferr(0:rank)
    double complex, intent(in) :: MomVec(0:3,1:5), MomInv(15), masses2(0:5)
    double complex, intent(in) :: Ften(0:rank,0:rank,0:rank,0:rank)
    double complex, intent(in) :: Ftenuv(0:rank,0:rank,0:rank,0:rank)
    integer :: r,n0,n1,n2,n3,mode
    integer,parameter :: rankuv=8
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A17,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A17,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  Ften(',i1,3(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  Ferr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  F0 = ',es23.16,' + i*',es23.16)"

! output file for result 
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_6point_example00_coli.dat'
      case(2)
        fname = 'demo_6point_example00_dd.dat'
      case(3)
        fname = 'demo_6point_example00_comp.dat'      
    end select
    write(fname(20:21),'(i2.2)') casei
 
!    write(*,*) casei,fname

    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a37,i2,i3/)')    ' Result for 6-point function, example',caseN,casei
    write (50,'(a63,i2,i3,a)')  ' The corresponding code can be found in demo.f90 under ''example',caseN,casei,''''
    write (50,'((a))')
    write (50,'((a))') '                     p42                  '
    write (50,'((a))') '             ---------^---------          '
    write (50,'((a))') '            /                   \         '
    write (50,'((a))') '                  m32,p3vec               '
    write (50,'((a))') '      / p32 ---------------------  p43 \  '
    write (50,'((a))') '     /             /  3   \             \ '
    write (50,'((a))') ' p31 |   m22,p2vec/2      4\ m42,p4vec  | p53'
    write (50,'((a))') '     \           /          \           / '
    write (50,'((a))') '      > p21 ----<            >---- p54 <  '
    write (50,'((a))') '     /           \          /           \ '
    write (50,'((a))') ' p20 |   m12,p1vec\1      5/ m52,p5vec  | p40' 
    write (50,'((a))') '     \             \   0  /             / '
    write (50,'((a))') '      \ p10 ---------------------  p50 /  '
    write (50,'((a))') '                     m02                  '
    write (50,'((a))') '            \                   /         '
    write (50,'((a))') '             ---------v---------          '
    write (50,'((a))') '                     p51                  '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p1vec(0)     ',MomVec(0,1)
    write (50,fmt1) '  p1vec(1)     ',MomVec(1,1)
    write (50,fmt1) '  p1vec(2)     ',MomVec(2,1)
    write (50,fmt1) '  p1vec(3)     ',MomVec(3,1)
    write (50,fmt1) '  p2vec(0)     ',MomVec(0,2)
    write (50,fmt1) '  p2vec(1)     ',MomVec(1,2)
    write (50,fmt1) '  p2vec(2)     ',MomVec(2,2)
    write (50,fmt1) '  p2vec(3)     ',MomVec(3,2) 
    write (50,fmt1) '  p3vec(0)     ',MomVec(0,3)
    write (50,fmt1) '  p3vec(1)     ',MomVec(1,3)
    write (50,fmt1) '  p3vec(2)     ',MomVec(2,3)
    write (50,fmt1) '  p3vec(3)     ',MomVec(3,3) 
    write (50,fmt1) '  p4vec(0)     ',MomVec(0,4)
    write (50,fmt1) '  p4vec(1)     ',MomVec(1,4)
    write (50,fmt1) '  p4vec(2)     ',MomVec(2,4)
    write (50,fmt1) '  p4vec(3)     ',MomVec(3,4) 
    write (50,fmt1) '  p5vec(0)     ',MomVec(0,5)
    write (50,fmt1) '  p5vec(1)     ',MomVec(1,5)
    write (50,fmt1) '  p5vec(2)     ',MomVec(2,5)
    write (50,fmt1) '  p5vec(3)     ',MomVec(3,5)     
    write (50,fmt1) '  p10     ',MomInv(1)
    write (50,fmt1) '  p21     ',MomInv(2)
    write (50,fmt1) '  p32     ',MomInv(3)
    write (50,fmt1) '  p43     ',MomInv(4)
    write (50,fmt1) '  p54     ',MomInv(5)
    write (50,fmt1) '  p50     ',MomInv(6)
    write (50,fmt1) '  p20     ',MomINv(7)
    write (50,fmt1) '  p31     ',MomInv(8)
    write (50,fmt1) '  p42     ',MomInv(9)
    write (50,fmt1) '  p53     ',MomInv(10)
    write (50,fmt1) '  p40     ',MomInv(11)
    write (50,fmt1) '  p51     ',MomInv(12)
    write (50,fmt1) '  p30     ',MomInv(13)
    write (50,fmt1) '  p41     ',MomInv(14)
    write (50,fmt1) '  p52     ',MomInv(15)
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    write (50,fmt1) '  m22     ',masses2(2)
    write (50,fmt1) '  m32     ',masses2(3)
    write (50,fmt1) '  m42     ',masses2(4)
    write (50,fmt1) '  m52     ',masses2(5)
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV ',deltaUV
    end if
    write (50,fmt2) '  DeltaIR1',deltaIR2
    write (50,fmt2) '  DeltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rank.eq.0) then
      write (50,'((a))') '         (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F0 =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F0_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    elseif(rank.ge.rankuv) then
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '             i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F_fin(muUV2,muIR2) + a_UV*DeltaUV + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    else
      write (50,'((a))') '        (2*pi*mu)^(4-D)                     '
      write (50,'((a))') '  F =   ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '            i*pi^2                          ' 

      write (50,'((a))')
      write (50,'((a))') '    =  F_fin(muUV2,muIR2) + '//  &
                         ' a_IR2*[DeltaIR2 + DeltaIR1*ln(muIR2)] + a_IR1*DeltaIR1'
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.ge.rankuv) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '               epsUV                   epsIR                  epsIR^2'
    else
      write (50,'((a))') &
          '               c(epsIR)                c(epsIR)'
      write (50,'((a))') & 
          '   DeltaIR1 =  --------,   DeltaIR2 =  --------'  
      write (50,'((a))') &
          '                epsIR                  epsIR^2'
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.ge.rankuv) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1, DeltaIR2'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    if (rank.ge.rankuv) then
      write (50,'((a))') '   - by default DeltaUV = DeltaIR1 = DeltaIR2 = 0 '
    else
      write (50,'((a))') '   - by default DeltaIR1 = DeltaIR2 = 0 '
    end if
    write (50,'((a))') '   - suitable DeltaIR2 can be used to adapt the effective normalization'
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      do r=0,rank
        do n0=0,r
          do n1=0,r-n0
            do n2=0,r-n0-n1
              n3=r-n0-n1-n2
              write (50,fmt10) n0,n1,n2,n3,Ften(n0,n1,n2,n3) 
            end do
          end do
        end do
      end do

      write (50,'(/(a))') ' Error estimates:'
      do r=0,rank
        write (50,fmt11) r,Ferr(r) 
      end do
    else
      write (50,fmt12) Ften(0,0,0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultFten

    
    
    
    

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv,masses2,rankm,rank,DBerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv,masses2,rankm,rank,DBerr)
    implicit none
   
    integer, intent(in) :: rank,rankm,caseN,casei
    double complex, intent(in) :: MomInv(1), masses2(0:1)
    double complex, intent(in) :: DBcoeff(0:rank/2,0:rank)
    double complex, intent(in) :: DBcoeffuv(0:rank/2,0:rank)
    double precision, optional, intent(in) :: DBerr(0:rank)
    integer :: r,n0,n1,mode
    integer,parameter :: rankuv=0
    character(len=99) :: fname
    character(len=*),parameter :: fmt1 = "(A9,' = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt2 = "(A9,' = ',es23.16)"
    character(len=*),parameter :: fmt10 = "('  DBcoeff(',i1,1(',',i1),') = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt11 = "('  DBerr(',i1,') = ',es23.16)"
    character(len=*),parameter :: fmt12 = "('  DB0 = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt13 = "('  DB1 = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt14 = "('  DB00 = ',es23.16,' + i*',es23.16)"
    character(len=*),parameter :: fmt20 = "('  DBcoeffuv(',i1,1(',',i1),') = ',es23.16,' + i*',es23.16)"

! output file for result
    call getMode_cll(mode)
    select case (mode)
      case(1)
        fname = 'demo_2point_derivative_example00_coli.dat'
      case(2)
        fname = 'demo_2point_derivative_example00_dd.dat'
      case(3)
        fname = 'demo_2point_derivative_example00_comp.dat'      
    end select
    write(fname(31:32),'(i2.2)') casei
 
    open(unit=50,file=trim(fname),status='unknown')

    call GetMuUV2_cll(MuUV2)
    call GetMuIR2_cll(MuIR2)
    call GetdeltaUV_cll(DeltaUV)
    call GetdeltaIR_cll(DeltaIR1,DeltaIR2)

    write (50,'(a49,i2,i3/)')    ' Result for 2-point function derivative, example ',caseN,casei
    write (50,'((a))')     '                  m12               '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '               /   1   \            '
    write (50,'((a))')     '              /         \           '
    write (50,'((a))')     '    p10  -----           -----  p10 '
    write (50,'((a))')     '              \         /           '
    write (50,'((a))')     '               \   0   /            '
    write (50,'((a))')     '                -------             '
    write (50,'((a))')     '                  m02               '
    write (50,'((a))')     ''
    write (50,'((a))')     ' Input:'
    write (50,fmt1) '  p10     ',p10
    write (50,fmt1) '  m02     ',masses2(0)
    write (50,fmt1) '  m12     ',masses2(1) 
    if (rank.ge.rankuv) then
      write (50,fmt2) '  muUV2   ',muUV2
    end if
!    write (50,fmt2) '  muIR2   ',muIR2
    if (rank.ge.rankuv) then
      write (50,fmt2) '  deltaUV ',deltaUV
    end if
!    write (50,fmt2) '  deltaIR1',deltaIR2
!    write (50,fmt2) '  deltaIR2',deltaIR1
    write (50,'((a))') ''
    write (50,'((a))') ' Conventions:'
    write (50,'((a))') ''
    if(rankm.eq.0.and.rank.gt.0) then
      write (50,'((a))') '         d    (2*pi*mu)^(4-D)                   '
      write (50,'((a))') '  DB = -----  ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '       d p^2       i*pi^2                       ' 

      write (50,'((a))')
      write (50,'((a))') '    =  DB_fin(muUV2,muIR2) + a_UV*DeltaUV + a_IR1*DeltaIR1 '
    else if (rank.eq.0) then
      write (50,'((a))') '          d    (2*pi*mu)^(4-D)                   '
      write (50,'((a))') '  DB0 = -----  ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '        d p^2       i*pi^2                       ' 

      write (50,'((a))')
      write (50,'((a))') '    =  DB0_fin(muIR2) + a_IR1*DeltaIR1 '
    else if (rank.eq.1.and.rankm.eq.1) then
      write (50,'((a))') '          d    (2*pi*mu)^(4-D)                   '
      write (50,'((a))') '  DB1 = -----  ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '        d p^2       i*pi^2                       ' 

      write (50,'((a))')
      write (50,'((a))') '    =  DB1_fin(muIR2) + a_IR1*DeltaIR1 '
    else if (rank.eq.2.and.rankm.eq.2) then
      write (50,'((a))') '           d    (2*pi*mu)^(4-D)                   '
      write (50,'((a))') '  DB00 = -----  ---------------  \int d^D q f(q,p_i)'
      write (50,'((a))') '         d p^2       i*pi^2                       ' 

      write (50,'((a))')
      write (50,'((a))') '    =  DB0_fin(muUV2,muIR2)) + a_UV*DeltaUV + a_IR1*DeltaIR1 '
    end if
    write (50,'((a))')
    write (50,'((a))') '  where'
    write (50,'((a))') 
    if (rank.ge.2) then
      write (50,'((a))') &
          '              c(epsUV)                c(epsIR)   '
      write (50,'((a))') & 
          '   DeltaUV =  --------,   DeltaIR1 =  --------   '  
      write (50,'((a))') &
          '               epsUV                   epsIR     '
    else
      write (50,'((a))') &
          '               c(epsIR)  '
      write (50,'((a))') & 
          '   DeltaIR1 =  --------  '  
      write (50,'((a))') &
          '                epsIR    '
    end if
    write (50,'((a))')
    write (50,'((a))') '   c(eps) = (4*pi)^eps\Gamma(1+eps),  D = 4 -2*eps '
    write (50,'((a))')
    write (50,'((a))') '  you can freely choose the regularization parameters'
    if (rank.ge.2) then  
      write (50,'((a))') '    of UV origin: muUV2 = mu^2, DeltaUV '
    end if
    write (50,'((a))') '    of IR origin: muIR2 = mu^2, DeltaIR1'
    write (50,'((a))')  
    write (50,'((a))') '  note:' 
    write (50,'((a))') '   - we effectively factor out a factor c(eps) '
    write (50,'((a))') '   - by default DeltaUV = 0 '
    
    write (50,'((a))') ''
    write (50,'((a))') ' Results:'

    if (rank.gt.0) then
      if(rankm.eq.0) then
        do r=rankm,rank
          do n0=0,r/2
            n1=r-2*n0
            write (50,fmt10) n0,n1,DBcoeff(n0,n1) 
          end do
        end do
      else if(rankm.eq.1.and.rank.eq.1) then
        write (50,fmt12) DBcoeff(0,1) 
      else if(rankm.eq.2.and.rank.eq.2) then
        write (50,fmt13) DBcoeff(1,0) 
      else 
        write (*,*) 'writeresultDB: case not supported' 
      end if
      

!      do r=0,rank
!        do n0=0,r/2
!          n1=r-2*n0
!          write (50,fmt13) n0,n1,DBcoeffuv(n0,n1) 
!        end do
!      end do
!      
      if(present(DBerr)) then
        write (50,'(/(a))') ' Error estimates:'
        do r=0,rank
          write (50,fmt11) r,DBerr(r) 
        end do
      end if
    else
      write (50,fmt12) DBcoeff(0,0)
    end if

    write(*,'(/(a),(a)/)') ' The result has been written to the file ' &
        ,trim(fname)

    end subroutine writeresultDB


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine writeresultDB0(caseN,casei,DB0,MomInv,masses2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine writeresultDB0(caseN,casei,DB0,MomInv,masses2)
      implicit none
      
      integer, intent(in) :: caseN,casei
      double complex, intent(in) :: DB0
      double complex, intent(in) :: MomInv(1), masses2(0:1)
      double complex :: DBcoeff(0:0,0:0)
      double complex :: DBcoeffuv(0:0,0:0)
      
      
      DBcoeff(0:0,0:0) = DB0
      DBcoeffuv(0:0,0:0) = 0d0
      call  writeresultDB(caseN,casei,DBcoeff,DBcoeffuv,MomInv,masses2,0,0)
 
    end  subroutine writeresultDB0






  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine getinvariants(N,MomInv,MomVec)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getinvariants(N,MomInv,MomVec)

    implicit none
    
    integer, intent(in) :: N
    double complex, intent(out) :: MomInv(N*(N-1)/2)
    double complex, intent(out), optional :: MomVec(0:3,N-1)
    double precision :: p(1:6,0:3) 
!    double complex :: MomInv6(1:15) 
    double precision :: s(0:5,0:5)
    integer :: k,i

    if (N.gt.6.or.N.lt.3) then
      write(*,*) ' getinvariants: N>6 of N<3 not supported '
    else
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
      
      
      do i=N+1,6
        p(N,0) = p(N,0) +  p(i,0)
        p(N,1) = p(N,1) +  p(i,1)
        p(N,2) = p(N,2) +  p(i,2)
        p(N,3) = p(N,3) +  p(i,3)
      end do
      
      if (present(MomVec)) then
        MomVec(0:3,1) = p(1,0:3)
        do i=2,N-1
          MomVec(0:3,i) = MomVec(0:3,i-1)+p(i,0:3)
        end do
      end if

      ! define invariants
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
      
      do i=0,(N+1)/2-2
        do k=1,N
          MomInv(k+N*i) = s(modulo(k+i,N),k-1)

!          write(*,*) 'mominv',k+N*i,s(modulo(k+i,N),k-1)
        end do
      end do
      if(modulo(N,2).eq.0) then
        do k=1,N/2
          MomInv(k+N*(N/2-1)) = s(modulo(k+N/2-1,N),k-1)
!          write(*,*) 'mominv',k+N*i,s(modulo(k+N/2-1,N),k-1)
        end do
      end if
    end if
      
  end subroutine getinvariants


end program demo
