!!
!!  File reductionEFG.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  *************************
!  *  module reductionEFG  *
!  *    by Lars Hofer      *
!  *************************
! 
!  functions and subroutines:
!  CalcE, CalcF, CalcG
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




module reductionEFG

  use reductionD

  implicit none



contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcE(E,Euv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Eerr)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcE(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                   m02,m12,m22,m32,m42,rmax,id,Eerr,Eerr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double complex, intent(out) :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision :: Eerr(0:rmax),Eerr2(0:rmax)
    double complex, allocatable :: Eaux(:,:,:,:,:), Euvaux(:,:,:,:,:), fct(:)
    double precision, allocatable :: Eerraux(:),Eerr2aux(:)
    double complex :: x(15)
    integer :: rank,switch,cnt,n0,n1,n2,n3,n4,r
    logical :: nocalc,wrica


    if ((use_cache_system).and.(tencache.gt.5)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.5) then
          x(1)=p10
          x(2)=p21
          x(3)=p32
          x(4)=p43
          x(5)=p40
          x(6)=p20
          x(7)=p31
          x(8)=p42 
          x(9)=p30
          x(10)=p41
          x(11)=m02
          x(12)=m12
          x(13)=m22
          x(14)=m32
          x(15)=m42

          rank = rmax

          if (rmax.ge.6) then
            allocate(fct(NCoefs(rmax,5)+NCoefs(rmax-6,5)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,5)+NCoefs(rmax-6,5)+2*(rmax+1),x,15,1,id,5,rank,nocalc,wrica)
          else
            allocate(fct(NCoefs(rmax,5)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,5)+2*(rmax+1),x,15,1,id,5,rank,nocalc,wrica)
          end if
    
          if(nocalc)then
            cnt = 0
            Euv(0:min(rmax/2,2),:,:,:,:) = 0d0
            do r=0,rmax
              do n0=0,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      n4 = r-2*n0-n1-n2-n3
 
                      cnt = cnt+1
                      E(n0,n1,n2,n3,n4) = fct(cnt)
              
                    end do
                  end do
                end do
              end do
              do n0=3,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      n4 = r-2*n0-n1-n2-n3
 
                      cnt = cnt+1
                      Euv(n0,n1,n2,n3,n4) = fct(cnt)
              
                    end do
                  end do
                end do
              end do
              cnt = cnt+1
              Eerr(r) = real(fct(cnt))
              cnt = cnt+1
              Eerr2(r) = real(fct(cnt))
            end do
            return
          endif


          if(rank.eq.rmax) then

            call CalcEred(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                           m02,m12,m22,m32,m42,rank,id,Eerr,Eerr2)

            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        n4 = r-2*n0-n1-n2-n3
 
                        cnt = cnt+1
                        fct(cnt) = E(n0,n1,n2,n3,n4)

                      end do
                    end do
                  end do
                end do
                do n0=3,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        n4 = r-2*n0-n1-n2-n3
 
                        cnt = cnt+1
                        fct(cnt) = Euv(n0,n1,n2,n3,n4)

                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Eerr(r)
                cnt = cnt+1
                fct(cnt) = Eerr2(r)
              end do
      
              if (rank.ge.6) then
                call WriteCache(fct,NCoefs(rank,5)+NCoefs(rank-6,5)+2*(rank+1),id,5,rank)
              else
                call WriteCache(fct,NCoefs(rank,5)+2*(rank+1),id,5,rank)
              end if

            end if

          return
          
          
          else
            allocate(Eaux(0:rank/2,0:rank,0:rank,0:rank,0:rank))
            allocate(Euvaux(0:rank/2,0:rank,0:rank,0:rank,0:rank))
            allocate(Eerraux(0:rank))
            allocate(Eerr2aux(0:rank))

            call CalcEred(Eaux,Euvaux,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                           m02,m12,m22,m32,m42,rank,id,Eerraux,Eerr2aux)

            if (wrica) then
              cnt = 0
              if (rank.ge.6) then
                deallocate(fct)
                allocate(fct(NCoefs(rank,5)+NCoefs(rank-6,5)+2*(rank+1)))
              else
                deallocate(fct)
                allocate(fct(NCoefs(rank,5)+2*(rank+1)))
              end if
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        n4 = r-2*n0-n1-n2-n3
 
                        cnt = cnt+1
                        fct(cnt) = Eaux(n0,n1,n2,n3,n4)

                      end do
                    end do
                  end do
                end do
                do n0=3,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        n4 = r-2*n0-n1-n2-n3
 
                        cnt = cnt+1
                        fct(cnt) = Euvaux(n0,n1,n2,n3,n4)

                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Eerraux(r)
                cnt = cnt+1
                fct(cnt) = Eerr2aux(r)
              end do
      
              if (rank.ge.6) then
                call WriteCache(fct,NCoefs(rank,5)+NCoefs(rank-6,5)+2*(rank+1),id,5,rank)
              else
                call WriteCache(fct,NCoefs(rank,5)+2*(rank+1),id,5,rank)
              end if

            end if

            E = Eaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
            Euv = Euvaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
            Eerr = Eerraux(0:rmax)
            Eerr2 = Eerr2aux(0:rmax)
            return

          end if
            
!        end if
      end if
    end if


    call CalcEred(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                     m02,m12,m22,m32,m42,rmax,id,Eerr,Eerr2)


  end subroutine CalcE





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcEred(E,Euv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32,rmax,id,Eerr,Eerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcEred(E,Euv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
                   m02,m12,m22,m32,m42,rmax,id,Eerr,Eerr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
    double complex, intent(in) :: m02,m12,m22,m32,m42
    double precision, intent(out) :: Eerr(0:rmax),Eerr2(0:rmax)
    double complex :: q10,q21,q32,q43,q40,q20,q31,q42,q30,q41
    double complex :: mm02,mm12,mm22,mm32,mm42
    double complex :: mx(0:4,0:4),mxinv(0:4,0:4),f(4),mxinvs(0:4),detX
    double complex :: zmxinv(4,0:4),zmxinvs(4)
    double precision :: maxZ,maxzmxinv(0:4),maxzmxinvs
    double complex, intent(out) :: E(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Euv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: Euv_aux(0:(rmax+1)/2,0:(rmax+1),0:(rmax+1),0:(rmax+1),0:(rmax+1))
    double complex, allocatable :: D_0(:,:,:,:,:), Duv_0(:,:,:,:,:)
    double complex, allocatable :: D_i(:,:,:,:,:), Duv_i(:,:,:,:,:)
!    double complex :: D_0(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD)
!    double complex :: Duv_0(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD)
!    double complex :: D_i(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,4)
!    double complex :: Duv_i(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,4)
    double complex :: S(0:4), S2(4,4), Eaux(4), Eaux2, Eaux3, elimminf2_coli
    double precision, allocatable :: Derr(:,:),Derr2(:,:)
!    double precision :: Derr(0:4,0:rmaxD)
    integer :: r,n0,n1,n2,n3,n4,i,n,k,nid(0:4),rid,r0,bin,rBCD,rmaxD
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A17,'(',d25.18,' , ',d25.18,' )')"

!    write(*,*) 'CalcEred in',p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
!                   m02,m12,m22,m32,m42,rmax,id

    rmaxD = max(rmax-1,0)
!    rmaxD = rmax
    allocate(D_0(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD))
    allocate(Duv_0(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD))
    allocate(D_i(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,4))
    allocate(Duv_i(0:rmaxD,0:rmaxD,0:rmaxD,0:rmaxD,4))
    allocate(Derr(0:4,0:rmaxD))
    allocate(Derr2(0:4,0:rmaxD))

    r0 = 0
    ! write(*,*) 'CalcE', id, rmax_cp(id)
    ! determine binaries for B-coefficients
    k=0
    bin = 1
    do while (k.le.4)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    call CalcD(D_0(:,0,:,:,:),Duv_0(:,0,:,:,:),p21,p32,p43,p41,p31,p42,  &
        m12,m22,m32,m42,rmaxD,nid(0),Derr(0,0:rmaxD),Derr2(0,0:rmaxD))
    call CalcD(D_i(:,:,:,:,1),Duv_i(:,:,:,:,1),p20,p32,p43,p40,p30,p42,  &
        m02,m22,m32,m42,rmaxD,nid(1),Derr(1,0:rmaxD),Derr2(1,0:rmaxD))
    call CalcD(D_i(:,:,:,:,2),Duv_i(:,:,:,:,2),p10,p31,p43,p40,p30,p41,  &
        m02,m12,m32,m42,rmaxD,nid(2),Derr(2,0:rmaxD),Derr2(2,0:rmaxD))     
    call CalcD(D_i(:,:,:,:,3),Duv_i(:,:,:,:,3),p10,p21,p42,p40,p20,p41,  &
        m02,m12,m22,m42,rmaxD,nid(3),Derr(3,0:rmaxD),Derr2(3,0:rmaxD))
    call CalcD(D_i(:,:,:,:,4),Duv_i(:,:,:,:,4),p10,p21,p32,p30,p20,p31,  &
        m02,m12,m22,m32,rmaxD,nid(4),Derr(4,0:rmaxD),Derr2(4,0:rmaxD))
    
    ! shift of integration momentum in C\{0}
    do n1=1,rmaxD
      do n2=0,rmaxD-n1
        do n3=0,rmaxD-n1-n2
          do n4=0,rmaxD-n1-n2-n3
            n0 = (rmaxD-n1-n2-n3-n4)
            Duv_0(0:n0,n1,n2,n3,n4) = -Duv_0(0:n0,n1-1,n2,n3,n4)-Duv_0(0:n0,n1-1,n2+1,n3,n4) &
                                      -Duv_0(0:n0,n1-1,n2,n3+1,n4)-Duv_0(0:n0,n1-1,n2,n3,n4+1)
            D_0(0:n0,n1,n2,n3,n4) = -D_0(0:n0,n1-1,n2,n3,n4)-D_0(0:n0,n1-1,n2+1,n3,n4) &
                                    -D_0(0:n0,n1-1,n2,n3+1,n4)-D_0(0:n0,n1-1,n2,n3,n4+1)
          end do
        end do
      end do
    end do


    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(m02)
    mm12 = elimminf2_coli(m12)
    mm22 = elimminf2_coli(m22)
    mm32 = elimminf2_coli(m32)
    mm42 = elimminf2_coli(m42)
    q10  = elimminf2_coli(p10)
    q21  = elimminf2_coli(p21)
    q32  = elimminf2_coli(p32)
    q43  = elimminf2_coli(p43)
    q40  = elimminf2_coli(p40)
    q31  = elimminf2_coli(p31)
    q42  = elimminf2_coli(p42)
    q30  = elimminf2_coli(p30)
    q41  = elimminf2_coli(p41)
    q20  = elimminf2_coli(p20)

    f(1) = q10+mm02-mm12
    f(2) = q20+mm02-mm22
    f(3) = q30+mm02-mm32
    f(4) = q40+mm02-mm42   

    mx(0,0) = 2d0*mm02
    mx(1,0) = q10 - mm12 + mm02
    mx(2,0) = q20 - mm22 + mm02
    mx(3,0) = q30 - mm32 + mm02
    mx(4,0) = q40 - mm42 + mm02
    mx(0,1) = mx(1,0)
    mx(0,2) = mx(2,0)
    mx(0,3) = mx(3,0)
    mx(0,4) = mx(4,0)
    mx(1,1) = 2d0*q10
    mx(2,2) = 2d0*q20
    mx(3,3) = 2d0*q30
    mx(4,4) = 2d0*q40
    mx(1,2) = q10+q20-q21
    mx(1,3) = q10+q30-q31
    mx(1,4) = q10+q40-q41
    mx(2,3) = q20+q30-q32
    mx(2,4) = q20+q40-q42
    mx(3,4) = q30+q40-q43
    mx(2,1) = mx(1,2)
    mx(3,1) = mx(1,3)
    mx(4,1) = mx(1,4)
    mx(3,2) = mx(2,3)
    mx(4,2) = mx(2,4)
    mx(4,3) = mx(3,4)

! changed 21.06.2018
    call chinv(5,mx,mxinv,detX)

    if (detX.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcEred',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcEred: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcEred: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcEred: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcEred: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcEred: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcEred: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcEred: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcEred: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcEred: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcEred: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcEred: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcEred: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcEred: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcEred: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcEred: mm42 = ',mm42
      end if
      E = 0d0
      return
    end if

    do i=0,4
      mxinvs(i) = sum(mxinv(i,0:4))
    end do

    ! for alternative error estimate
!    Z(1:4,1:4) = mx(1:4,1:4)
    zmxinv = matmul(mx(1:4,1:4),mxinv(1:4,0:4))

    do i=0,4
      maxzmxinv(i) = maxval(abs(zmxinv(1:4,i)))
    end do

    do i=1,4
      zmxinvs(i) = sum(zmxinv(i,0:4))
    end do

    maxzmxinvs = maxval(abs(zmxinvs(1:4)))

    maxZ = maxval(abs(mx(1:4,1:4)))


    ! calculation of UV-divergent parts      
    ! E_(n0,n1,n2,n3) UV-finite for n0<3
    Euv_aux(0:min((rmax+1)/2,2),:,:,:,:) = 0d0
    Euv(0:min(rmax/2,2),:,:,:,:) = 0d0
    
    ! PV reduction (5.10)
    do r=max(r0,6),rmax+1
      do n0=3,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              n4 = r-2*n0-n1-n2-n3
        
              Euv_aux(n0,n1,n2,n3,n4) = (Duv_0(n0-1,n1,n2,n3,n4)  & 
                                      + 2*m02*Euv_aux(n0-1,n1,n2,n3,n4)  &
                                      + f(1)*Euv_aux(n0-1,n1+1,n2,n3,n4)  & 
                                      + f(2)*Euv_aux(n0-1,n1,n2+1,n3,n4)  & 
                                      + f(3)*Euv_aux(n0-1,n1,n2,n3+1,n4)  &
                                      + f(4)*Euv_aux(n0-1,n1,n2,n3,n4+1)) / (2*(r-2))
              if (r.le.rmax) then
                Euv(n0,n1,n2,n3,n4) = Euv_aux(n0,n1,n2,n3,n4)
              end if 
    
            end do
          end do
        end do
      end do
    end do

!    write(*,*) 'CalcEred E0', -mxinv(0,0) -mxinv(1,0)  -mxinv(2,0) -mxinv(3,0)-mxinv(4,0)
!    write(*,*) 'CalcEred E0', -mxinvs(0)
!    write(*,*) 'CalcEred E0', -mxinvs(0)+mxinv(0,0),mxinv(0,0)

    ! scalar coefficient
    if (r0.eq.0) then
      E = 0d0
!      E(0,0,0,0,0) = -mxinv(0,0)*D_0(0,0,0,0,0)
!      do k=1,4
!        E(0,0,0,0,0) = E(0,0,0,0,0) &
!           + mxinv(k,0)*(D_i(0,0,0,0,k)-D_0(0,0,0,0,0))
!      end do
      E(0,0,0,0,0) = -mxinvs(0)*D_0(0,0,0,0,0)
      do k=1,4
        E(0,0,0,0,0) = E(0,0,0,0,0) + mxinv(k,0)*D_i(0,0,0,0,k)
      end do
    end if

!    Eerr(0) = max( maxval(abs(mxinv(0:4,0)))*Derr(0,0), &
    Eerr(0) = max( abs(mxinvs(0))*Derr(0,0), &
                   abs(mxinv(1,0))*Derr(1,0) , &
                   abs(mxinv(2,0))*Derr(2,0) , &
                   abs(mxinv(3,0))*Derr(3,0) , &
                   abs(mxinv(4,0))*Derr(4,0) )
    Eerr2(0) = max( abs(mxinvs(0))*Derr2(0,0), &
                   abs(mxinv(1,0))*Derr2(1,0) , &
                   abs(mxinv(2,0))*Derr2(2,0) , &
                   abs(mxinv(3,0))*Derr2(3,0) , &
                   abs(mxinv(4,0))*Derr2(4,0) )

!    do k=1,4
!    write(*,*) 'CalcEred En', -mxinv(0,k) -mxinv(1,k)  -mxinv(2,k) -mxinv(3,k)-mxinv(4,k)
!    write(*,*) 'CalcEred En', -mxinvs(k)
!    end do

    ! formula (6.12) and (6.13)
    do r=r0,rmax
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              n4 = r-2*n0-n1-n2-n3
            
              if (n0.gt.0.or.r.le.rmax-1) then
                do n=0,4
!                  S(n) = -D_0(n0,n1,n2,n3,n4)
                  S(n) = 0d0
                end do
              endif

              if (n1.eq.0) then
                if (n0.gt.0.or.r.le.rmax-1) then
                  S(1) = S(1) + D_i(n0,n2,n3,n4,1)
                  S2(:,1) = 0d0
                end if
              else
                if (r.le.rmax-1) then
                  S2(:,1) = -n1*D_0(n0+1,n1-1,n2,n3,n4)
                  if (n2.eq.0) then
                    S2(2,1) = S2(2,1) + n1*D_i(n0+1,n1-1,n3,n4,2)
                  end if
                  if (n3.eq.0) then
                    S2(3,1) = S2(3,1) + n1*D_i(n0+1,n1-1,n2,n4,3)
                  end if
                  if (n4.eq.0) then
                    S2(4,1) = S2(4,1) + n1*D_i(n0+1,n1-1,n2,n3,4)
                  end if
                end if
              end if

              if (n2.eq.0) then
                if (n0.gt.0.or.r.le.rmax-1) then
                  S(2) = S(2) + D_i(n0,n1,n3,n4,2)
                  S2(:,2) = 0d0
                end if
              else
                if (r.le.rmax-1) then
                  S2(:,2) = -n2*D_0(n0+1,n1,n2-1,n3,n4)
                  if (n1.eq.0) then
                    S2(1,2) = S2(1,2) + n2*D_i(n0+1,n2-1,n3,n4,1)
                  end if
                  if (n3.eq.0) then
                    S2(3,2) = S2(3,2) + n2*D_i(n0+1,n1,n2-1,n4,3)
                  end if
                  if (n4.eq.0) then
                    S2(4,2) = S2(4,2) + n2*D_i(n0+1,n1,n2-1,n3,4)
                  end if
                end if
              end if

              if (n3.eq.0) then
                if (n0.gt.0.or.r.le.rmax-1) then
                  S(3) = S(3) + D_i(n0,n1,n2,n4,3)
                  S2(:,3) = 0d0
                end if
              else
                if (r.le.rmax-1) then
                  S2(:,3) = -n3*D_0(n0+1,n1,n2,n3-1,n4)
                  if (n1.eq.0) then
                    S2(1,3) = S2(1,3) + n3*D_i(n0+1,n2,n3-1,n4,1)
                  end if
                  if (n2.eq.0) then
                    S2(2,3) = S2(2,3) + n3*D_i(n0+1,n1,n3-1,n4,2)
                  end if
                  if (n4.eq.0) then
                    S2(4,3) = S2(4,3) + n3*D_i(n0+1,n1,n2,n3-1,4)
                  end if
                end if
              end if

              if (n4.eq.0) then
                if (n0.gt.0.or.r.le.rmax-1) then
                  S(4) = S(4) + D_i(n0,n1,n2,n3,4)
                  S2(:,4) = 0d0
                end if
              else
                if (r.le.rmax-1) then
                  S2(:,4) = -n4*D_0(n0+1,n1,n2,n3,n4-1)
                  if (n1.eq.0) then
                    S2(1,4) = S2(1,4) + n4*D_i(n0+1,n2,n3,n4-1,1)
                  end if
                  if (n2.eq.0) then
                    S2(2,4) = S2(2,4) + n4*D_i(n0+1,n1,n3,n4-1,2)
                  end if
                  if (n3.eq.0) then
                    S2(3,4) = S2(3,4) + n4*D_i(n0+1,n1,n2,n4-1,3)
                  end if
                end if
              end if

              if (r.le.rmax-1) then
                
                ! rational term
                S(0) = S(0) - 4d0*Euv_aux(n0+1,n1,n2,n3,n4)
                
                do k=1,4
                  Eaux(k) = mxinv(0,k)*S(0)+mxinv(1,k)*S(1)+mxinv(2,k)*S(2) &
                          + mxinv(3,k)*S(3)+mxinv(4,k)*S(4) &
                          - mxinvs(k) * D_0(n0,n1,n2,n3,n4) &
                          + ((mxinv(1,k)*mxinv(2,0)-mxinv(2,k)*mxinv(1,0))*(S2(1,2)-S2(2,1)) &
                          +  (mxinv(1,k)*mxinv(3,0)-mxinv(3,k)*mxinv(1,0))*(S2(1,3)-S2(3,1)) &
                          +  (mxinv(1,k)*mxinv(4,0)-mxinv(4,k)*mxinv(1,0))*(S2(1,4)-S2(4,1)) &
                          +  (mxinv(2,k)*mxinv(3,0)-mxinv(3,k)*mxinv(2,0))*(S2(2,3)-S2(3,2)) &
                          +  (mxinv(2,k)*mxinv(4,0)-mxinv(4,k)*mxinv(2,0))*(S2(2,4)-S2(4,2)) &
                          +  (mxinv(3,k)*mxinv(4,0)-mxinv(4,k)*mxinv(3,0))*(S2(3,4)-S2(4,3)))*2
                end do

                E(n0,n1+1,n2,n3,n4) = E(n0,n1+1,n2,n3,n4) + (n1+1)*Eaux(1)/(r+1)
                E(n0,n1,n2+1,n3,n4) = E(n0,n1,n2+1,n3,n4) + (n2+1)*Eaux(2)/(r+1)
                E(n0,n1,n2,n3+1,n4) = E(n0,n1,n2,n3+1,n4) + (n3+1)*Eaux(3)/(r+1)
                E(n0,n1,n2,n3,n4+1) = E(n0,n1,n2,n3,n4+1) + (n4+1)*Eaux(4)/(r+1)

              end if

              if (n0.ge.1) then
                Eaux2 = mxinv(1,0)*S(1)+mxinv(2,0)*S(2) &
                      + mxinv(3,0)*S(3)+mxinv(4,0)*S(4) &
                      - (mxinvs(0)-mxinv(0,0)) * D_0(n0,n1,n2,n3,n4) 
                      
                E(n0,n1,n2,n3,n4) = E(n0,n1,n2,n3,n4) + 2*n0*Eaux2/r
              end if

            end do
          end do
        end do
      end do

      if (r.le.rmax-1) then  
!        Eerr(r+1) = max( maxval(abs(mxinv(0:4,1:4)))*Derr(0,r), &
        Eerr(r+1) = max( maxval(abs(mxinvs(1:4)))*Derr(0,r), &
                   maxval(abs(mxinv(1,1:4)))*Derr(1,r) , &
                   maxval(abs(mxinv(2,1:4)))*Derr(2,r) , &
                   maxval(abs(mxinv(3,1:4)))*Derr(3,r) , &
                   maxval(abs(mxinv(4,1:4)))*Derr(4,r) )
        Eerr2(r+1) = max( abs(maxzmxinvs)*Derr2(0,r), &
                   abs(maxzmxinv(1))*Derr2(1,r) , &
                   abs(maxzmxinv(2))*Derr2(2,r) , &
                   abs(maxzmxinv(3))*Derr2(3,r) , &
                   abs(maxzmxinv(4))*Derr2(4,r) ) /maxZ

!        write(*,*) 'CalcEred   s  ', maxval(abs(mxinvs(1:4)))
!        write(*,*) 'CalcEred   1  ', maxval(abs(mxinv(1,1:4)))
!        write(*,*) 'CalcEred   2  ', maxval(abs(mxinv(2,1:4)))
!        write(*,*) 'CalcEred   3  ', maxval(abs(mxinv(3,1:4)))
!        write(*,*) 'CalcEred   4  ', maxval(abs(mxinv(4,1:4)))
!        write(*,*) 'CalcEred err ',r,Eerr(r+1)
!        if (r.gt.0) write(*,*) 'CalcEred err ',1,Eerr(2)
!        write(*,*) 'CalcEred err ',Derr(0:4,r)
!        write(*,*) 'CalcEred err2',r,Eerr2(r+1)
!        write(*,*) 'CalcEred err2',Derr2(0:4,r)

      end if


    end do

    ! write(*,*) 'E', id, E(0,0,0,0,2)


  end subroutine CalcEred





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,      &
  !                         rmax,id,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcF(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,      &
                         rmax,id,Ferr,Ferr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double complex, intent(out) :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Fuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Ferr(0:rmax),Ferr2(0:rmax)
    double complex, allocatable :: Faux(:,:,:,:,:,:), Fuvaux(:,:,:,:,:,:), fct(:)
    double precision, allocatable :: Ferraux(:),Ferr2aux(:)
    double complex :: x(21)
    integer :: rank,switch,cnt,n0,n1,n2,n3,n4,n5,r
    logical :: nocalc,wrica

!    write(*,*) 'CalcF in'

    if ((use_cache_system).and.(tencache.gt.6)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then 
!        if (use_cache(ncache).ge.6) then
          x(1)=p10
          x(2)=p21
          x(3)=p32
          x(4)=p43
          x(5)=p54
          x(6)=p50
          x(7)=p20
          x(8)=p31
          x(9)=p42
          x(10)=p53
          x(11)=p40
          x(12)=p51
          x(13)=p30
          x(14)=p41
          x(15)=p52
          x(16)=m02
          x(17)=m12
          x(18)=m22
          x(19)=m32
          x(20)=m42
          x(21)=m52

          rank = rmax
 
          if (rmax.ge.8) then
            allocate(fct(NCoefs(rmax,6)+NCoefs(rmax-8,6)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,6)+NCoefs(rmax-8,6)+2*(rmax+1),x,21,1,id,6,rank,nocalc,wrica)
          else
            allocate(fct(NCoefs(rmax,6)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,6)+2*(rmax+1),x,21,1,id,6,rank,nocalc,wrica)
          end if    

          if(nocalc) then
            cnt = 0
            Fuv(0:min(rmax/2,3),:,:,:,:,:) = 0d0
            do r=0,rmax
              do n0=0,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      do n4=0,r-2*n0-n1-n2-n3
                        n5 = r-2*n0-n1-n2-n3-n4
 
                        cnt = cnt+1
                        F(n0,n1,n2,n3,n4,n5) = fct(cnt)
              
                      end do
                    end do
                  end do
                end do
              end do
              do n0=4,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      do n4=0,r-2*n0-n1-n2-n3
                        n5 = r-2*n0-n1-n2-n3-n4
 
                        cnt = cnt+1
                        Fuv(n0,n1,n2,n3,n4,n5) = fct(cnt)
              
                      end do
                    end do
                  end do
                end do
              end do
              cnt = cnt+1
              Ferr(r) = real(fct(cnt))
              cnt = cnt+1
              Ferr2(r) = real(fct(cnt))
            end do    
            return
          endif


          if(rank.eq.rmax) then

            call CalcFred(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                                p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,id,Ferr,Ferr2)

            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
 
                          cnt = cnt+1
                          fct(cnt) = F(n0,n1,n2,n3,n4,n5)

                        end do
                      end do
                    end do
                  end do
                end do
                do n0=4,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
 
                          cnt = cnt+1
                          fct(cnt) = Fuv(n0,n1,n2,n3,n4,n5)

                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Ferr(r)
                cnt = cnt+1
                fct(cnt) = Ferr2(r)
              end do
   
              if (rank.ge.8) then
                call WriteCache(fct,NCoefs(rank,6)+NCoefs(rank-8,6)+2*(rank+1),id,6,rank)
              else
                call WriteCache(fct,NCoefs(rank,6)+2*(rank+1),id,6,rank)
              end if

            end if

            return
          
          
          else
            allocate(Faux(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank))
            allocate(Fuvaux(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank))
            allocate(Ferraux(0:rank))
            allocate(Ferr2aux(0:rank))

            call CalcFred(Faux,Fuvaux,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                                      p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rank,id,Ferraux,Ferr2aux)


            if (wrica) then
              cnt = 0
              deallocate(fct)
              if (rank.ge.8) then
                allocate(fct(NCoefs(rank,6)+NCoefs(rank-8,6)+2*(rank+1)))
              else
                allocate(fct(NCoefs(rank,6)+2*(rank+1)))
              end if
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
 
                          cnt = cnt+1
                          fct(cnt) = Faux(n0,n1,n2,n3,n4,n5)

                        end do
                      end do
                    end do
                  end do
                end do
                do n0=4,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          n5 = r-2*n0-n1-n2-n3-n4
 
                          cnt = cnt+1
                          fct(cnt) = Fuvaux(n0,n1,n2,n3,n4,n5)

                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Ferraux(r)
                cnt = cnt+1
                fct(cnt) = Ferr2aux(r)
              end do
   
              if (rank.ge.8) then
                call WriteCache(fct,NCoefs(rank,6)+NCoefs(rank-8,6)+2*(rank+1),id,6,rank)
              else
                call WriteCache(fct,NCoefs(rank,6)+2*(rank+1),id,6,rank)
              end if

            end if

            F = Faux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
            Fuv = Fuvaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
            Ferr = Ferraux(0:rmax)
            Ferr2 = Ferr2aux(0:rmax)
            return

          end if
            
!        end if
      end if
    end if
    

    call CalcFred(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                        p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,rmax,id,Ferr,Ferr2)
    

  end subroutine CalcF





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcFred(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
  !                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,         &
  !                         rmax,id,Ferr,Ferr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcFred(F,Fuv,p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40,  &
                         p51,p30,p41,p52,m02,m12,m22,m32,m42,m52,         &
                         rmax,id,Ferr,Ferr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p54,p50,p20,p31,p42,p53,p40
    double complex, intent(in) :: p51,p30,p41,p52,m02,m12,m22,m32,m42,m52
    double precision, intent(out) :: Ferr(0:rmax),Ferr2(0:rmax)
    double complex, intent(out) :: F(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Fuv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex :: q10,q21,q32,q43,q54,q50,q20,q31,q42,q53,q40
    double complex :: q51,q30,q41,q52,mm02,mm12,mm22,mm32,mm42,mm52
    double complex :: mx(0:5,0:5),mx0k(5,5),mx0kinv(5,5),ff(5)
    double complex :: det,newdet,mx0kinvs(5)
!    double complex ::  mxinv(0:5,0:5),mxinvs
    double complex :: zmx0kinv(5,5),zmx0kinvs(5)
    double precision :: maxZ,maxzmx0kinv(5),maxzmx0kinvs
    double complex, allocatable :: E_0(:,:,:,:,:,:), E_i(:,:,:,:,:,:)
    double complex, allocatable :: Euv_0(:,:,:,:,:,:), Euv_i(:,:,:,:,:,:)
    double complex :: S(5), Faux(5), elimminf2_coli,chdet,gramdet
    double precision :: Eerr(0:5,0:rmax),Eerr2(0:5,0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,n,k,i,j,nid(0:5),r0,bin,kbest,rmaxE,rBCD
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A17,'(',d25.18,' , ',d25.18,' )')"

    double complex :: mxmx0kinv(5,5)
    double precision :: maxf
    integer        :: jmax

!    write(*,*) 'CalcFred in'

    r0 = 0

    ! allocation of C functions
    rmaxE = max(rmax-1,0)
    allocate(E_0(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(E_i(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,5))
    allocate(Euv_0(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE))
    allocate(Euv_i(0:rmaxE/2,0:rmaxE,0:rmaxE,0:rmaxE,0:rmaxE,5))


    ! determine binaries for B-coefficients
    k=0
    bin = 1
    do while (k.le.5)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do


    call CalcE(E_0(:,0,:,:,:,:),Euv_0(:,0,:,:,:,:),p21,p32,p43,p54,p51,p31,p42,p53,p41,p52,  &
                         m12,m22,m32,m42,m52,rmaxE,nid(0),Eerr=Eerr(0,0:rmaxE),Eerr2=Eerr2(0,0:rmaxE))
    call CalcE(E_i(:,:,:,:,:,1),Euv_i(:,:,:,:,:,1),p20,p32,p43,p54,p50,p30,  &
                         p42,p53,p40,p52,m02,m22,m32,m42,m52,rmaxE,nid(1),Eerr=Eerr(1,0:rmaxE),Eerr2=Eerr2(1,0:rmaxE))
    call CalcE(E_i(:,:,:,:,:,2),Euv_i(:,:,:,:,:,2),p10,p31,p43,p54,p50,p30,  &
                         p41,p53,p40,p51,m02,m12,m32,m42,m52,rmaxE,nid(2),Eerr=Eerr(2,0:rmaxE),Eerr2=Eerr2(2,0:rmaxE))
    call CalcE(E_i(:,:,:,:,:,3),Euv_i(:,:,:,:,:,3),p10,p21,p42,p54,p50,p20,  &
                         p41,p52,p40,p51,m02,m12,m22,m42,m52,rmaxE,nid(3),Eerr=Eerr(3,0:rmaxE),Eerr2=Eerr2(3,0:rmaxE)) 
    call CalcE(E_i(:,:,:,:,:,4),Euv_i(:,:,:,:,:,4),p10,p21,p32,p53,p50,p20,  &
                         p31,p52,p30,p51,m02,m12,m22,m32,m52,rmaxE,nid(4),Eerr=Eerr(4,0:rmaxE),Eerr2=Eerr2(4,0:rmaxE))
    call CalcE(E_i(:,:,:,:,:,5),Euv_i(:,:,:,:,:,5),p10,p21,p32,p43,p40,p20,  &
                         p31,p42,p30,p41,m02,m12,m22,m32,m42,rmaxE,nid(5),Eerr=Eerr(5,0:rmaxE),Eerr2=Eerr2(5,0:rmaxE))

    ! shift of integration momentum in E\{0}
    do n1=1,rmaxE
      do n2=0,rmaxE-n1
        do n3=0,rmaxE-n1-n2
          do n4=0,rmaxE-n1-n2-n3
            do n5=0,rmaxE-n1-n2-n3-n4
              n0 = (rmaxE-n1-n2-n3-n4-n5)/2
              E_0(0:n0,n1,n2,n3,n4,n5) = -E_0(0:n0,n1-1,n2,n3,n4,n5)-E_0(0:n0,n1-1,n2+1,n3,n4,n5) &
                                         -E_0(0:n0,n1-1,n2,n3+1,n4,n5)-E_0(0:n0,n1-1,n2,n3,n4+1,n5)  &
                                         -E_0(0:n0,n1-1,n2,n3,n4,n5+1)
            end do
          end do
        end do
      end do
    end do
    do n1=1,rmaxE-6
      do n2=0,rmaxE-n1-6
        do n3=0,rmaxE-n1-n2-6
          do n4=0,rmaxE-n1-n2-n3-6
            do n5=0,rmaxE-n1-n2-n3-n4-6
              n0 = (rmaxE-n1-n2-n3-n4-n5)/2
              Euv_0(0:n0,n1,n2,n3,n4,n5) = -Euv_0(0:n0,n1-1,n2,n3,n4,n5)-Euv_0(0:n0,n1-1,n2+1,n3,n4,n5) &
                                           -Euv_0(0:n0,n1-1,n2,n3+1,n4,n5)-Euv_0(0:n0,n1-1,n2,n3,n4+1,n5)  &
                                           -Euv_0(0:n0,n1-1,n2,n3,n4,n5+1)
            end do
          end do
        end do
      end do
    end do



    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(m02)
    mm12 = elimminf2_coli(m12)
    mm22 = elimminf2_coli(m22)
    mm32 = elimminf2_coli(m32)
    mm42 = elimminf2_coli(m42)
    mm52 = elimminf2_coli(m52)
    q10  = elimminf2_coli(p10)
    q21  = elimminf2_coli(p21)
    q32  = elimminf2_coli(p32)
    q43  = elimminf2_coli(p43)
    q54  = elimminf2_coli(p54)
    q50  = elimminf2_coli(p50)
    q20  = elimminf2_coli(p20)
    q31  = elimminf2_coli(p31)
    q42  = elimminf2_coli(p42)
    q53  = elimminf2_coli(p53)
    q40  = elimminf2_coli(p40)
    q51  = elimminf2_coli(p51)
    q30  = elimminf2_coli(p30)
    q41  = elimminf2_coli(p41)
    q52  = elimminf2_coli(p52)

    ff(1) = q10+mm02-mm12
    ff(2) = q20+mm02-mm22
    ff(3) = q30+mm02-mm32
    ff(4) = q40+mm02-mm42
    ff(5) = q50+mm02-mm52
 
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

!    call chinv(6,mx,mxinv)

    ! determine X_(0,5)
    do j=1,5
      do i=1,5
        mx0k(i,j) = mx(i,j-1)
      end do
    end do

    ! determine best reduction matrix
    det = chdet(5,mx0k)
    kbest = 5
    
!    write(*,*) 'Fred det',5,det

    do j=5,2,-1
      do i=1,5
        mx0k(i,j) = mx(i,j)
      end do

      newdet =  chdet(5,mx0k)

!      write(*,*) 'Fred det',j-1,newdet

      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do

!    write(*,*) 'Fred kbest',kbest
    
    do i=1,5
      mx0k(i,1) = mx(i,1)
      mx0k(i,kbest) = mx(i,0)
    end do

! changed 21.06.2018
    call chinv(5,mx0k,mx0kinv,det)

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcFred',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcFred: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcFred: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcFred: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcFred: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcFred: q54 = ',q54
        write(nerrout_coli,fmt10) ' CalcFred: q50 = ',q50
        write(nerrout_coli,fmt10) ' CalcFred: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcFred: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcFred: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcFred: q53 = ',q53
        write(nerrout_coli,fmt10) ' CalcFred: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcFred: q51 = ',q51
        write(nerrout_coli,fmt10) ' CalcFred: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcFred: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcFred: q52 = ',q52
        write(nerrout_coli,fmt10) ' CalcFred: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcFred: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcFred: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcFred: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcFred: mm42 = ',mm42
        write(nerrout_coli,fmt10) ' CalcFred: mm52 = ',mm52
      end if
      F = 0d0
      return
    end if

    do i=1,5
      mx0kinv(kbest,i) = 0d0
    end do

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
    ! F_(n0,n1,n2,n3) UV-finite for n0<4
    Fuv(0:min(rmax/2,3),:,:,:,:,:) = 0d0
    
    ! PV reduction (5.10)
    do r=max(r0,8),rmax
      do n0=3,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                n5 = r-2*n0-n1-n2-n3-n4
        
                Fuv(n0,n1,n2,n3,n4,n5) = (Euv_0(n0-1,n1,n2,n3,n4,n5)  & 
                                       + 2*m02*Fuv(n0-1,n1,n2,n3,n4,n5)  &
                                       + ff(1)*Fuv(n0-1,n1+1,n2,n3,n4,n5)  & 
                                       + ff(2)*Fuv(n0-1,n1,n2+1,n3,n4,n5)  & 
                                       + ff(3)*Fuv(n0-1,n1,n2,n3+1,n4,n5)  &
                                       + ff(4)*Fuv(n0-1,n1,n2,n3,n4+1,n5)  &
                                       + ff(5)*Fuv(n0-1,n1,n2,n3,n4,n5+1))/ (2*(r-3))
    
              end do
            end do
          end do
        end do
      end do
    end do

    F = 0d0

    ! scalar coefficient


! version replaced 21.06.2018
!    if (r0.eq.0) then
!      F = 0d0
!      F(0,0,0,0,0,0) = -mxinv(0,0)*E_0(0,0,0,0,0,0)
!      do k=1,5
!        F(0,0,0,0,0,0) = F(0,0,0,0,0,0) &
!             + mxinv(k,0)*(E_i(0,0,0,0,0,k)-E_0(0,0,0,0,0,0))
!      end do
!    end if
!
!    Ferr(0) = max( abs(mxinvs)*Eerr(0,0), &
!                   abs(mxinv(1,0))*Eerr(1,0) , &
!                   abs(mxinv(2,0))*Eerr(2,0) , &
!                   abs(mxinv(3,0))*Eerr(3,0) , &
!                   abs(mxinv(4,0))*Eerr(4,0) , &
!                   abs(mxinv(5,0))*Eerr(5,0) )
!    Ferr2(0) = max( abs(mxinvs)*Eerr2(0,0), &
!                   abs(mxinv(1,0))*Eerr2(1,0) , &
!                   abs(mxinv(2,0))*Eerr2(2,0) , &
!                   abs(mxinv(3,0))*Eerr2(3,0) , &
!                   abs(mxinv(4,0))*Eerr2(4,0) , &
!                   abs(mxinv(5,0))*Eerr2(5,0) )
!
!    write(*,*) 'CalcFred: F(0)',F(0,0,0,0,0,0)
!    write(*,*) 'CalcFred: Ferr(0)',Ferr(0),Ferr2(0)

! New version for  F(0,0,0,0,0,0), 21.06.2018

    maxf = abs(mx(1,0))
    jmax = 1
    do j=2,5
      if (abs(mx(j,0)).gt.maxf) then
        jmax = j
        maxf = abs(mx(j,0))
      end if
    end do

    mxmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    F(0,0,0,0,0,0) =  E_i(0,0,0,0,0,jmax) - E_0(0,0,0,0,0,0)
    do j=1,5
      F(0,0,0,0,0,0) =  F(0,0,0,0,0,0) & 
          - mxmx0kinv(jmax,j) * (E_i(0,0,0,0,0,j) - E_0(0,0,0,0,0,0))
    end do
    F(0,0,0,0,0,0) =  F(0,0,0,0,0,0)/mx(jmax,0)

    Ferr(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*Eerr(0,0), &
                   abs(mxmx0kinv(jmax,1))*Eerr(1,0) , &
                   abs(mxmx0kinv(jmax,2))*Eerr(2,0) , &
                   abs(mxmx0kinv(jmax,3))*Eerr(3,0) , &
                   abs(mxmx0kinv(jmax,4))*Eerr(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*Eerr(5,0) , &
                   Eerr(0,0) , Eerr(jmax,0) )/abs(mx(jmax,0))

    Ferr2(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*Eerr2(0,0), &
                   abs(mxmx0kinv(jmax,1))*Eerr2(1,0) , &
                   abs(mxmx0kinv(jmax,2))*Eerr2(2,0) , &
                   abs(mxmx0kinv(jmax,3))*Eerr2(3,0) , &
                   abs(mxmx0kinv(jmax,4))*Eerr2(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*Eerr2(5,0) , &
                   Eerr2(0,0),Eerr2(jmax,0))/abs(mx(jmax,0))

!    write(*,*) 'CalcFred: F(0) n ',F(0,0,0,0,0,0)
!    write(*,*) 'CalcFred: Ferr(0) n ',Ferr(0)


    ! formula (7.13)
    do r=r0,rmax-1
      do n0=0,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                n5 = r-2*n0-n1-n2-n3-n4
            
                do n=1,5
                  S(n) = -E_0(n0,n1,n2,n3,n4,n5)
                end do

                if (n1.eq.0) then
                  S(1) = S(1) + E_i(n0,n2,n3,n4,n5,1)
                end if
                if (n2.eq.0) then
                  S(2) = S(2) + E_i(n0,n1,n3,n4,n5,2)
                end if
                if (n3.eq.0) then
                  S(3) = S(3) + E_i(n0,n1,n2,n4,n5,3)
                end if
                if (n4.eq.0) then
                  S(4) = S(4) + E_i(n0,n1,n2,n3,n5,4)
                end if
                if (n5.eq.0) then
                  S(5) = S(5) + E_i(n0,n1,n2,n3,n4,5)
                end if

                do k=1,5
                  Faux(k) = mx0kinv(k,1)*S(1)+mx0kinv(k,2)*S(2) &
                          + mx0kinv(k,3)*S(3)+mx0kinv(k,4)*S(4)+mx0kinv(k,5)*S(5)
                end do

                F(n0,n1+1,n2,n3,n4,n5) = F(n0,n1+1,n2,n3,n4,n5) + (n1+1)*Faux(1)/(r+1)
                F(n0,n1,n2+1,n3,n4,n5) = F(n0,n1,n2+1,n3,n4,n5) + (n2+1)*Faux(2)/(r+1)
                F(n0,n1,n2,n3+1,n4,n5) = F(n0,n1,n2,n3+1,n4,n5) + (n3+1)*Faux(3)/(r+1)
                F(n0,n1,n2,n3,n4+1,n5) = F(n0,n1,n2,n3,n4+1,n5) + (n4+1)*Faux(4)/(r+1)
                F(n0,n1,n2,n3,n4,n5+1) = F(n0,n1,n2,n3,n4,n5+1) + (n5+1)*Faux(5)/(r+1)

              end do
            end do
          end do
        end do
      end do

      if (r.le.rmax-1) then  
        Ferr(r+1) = max( maxval(abs(mx0kinvs(1:5)))*Eerr(0,r), &
                   maxval(abs(mx0kinv(1:5,1)))*Eerr(1,r) , &
                   maxval(abs(mx0kinv(1:5,2)))*Eerr(2,r) , &
                   maxval(abs(mx0kinv(1:5,3)))*Eerr(3,r) , &
                   maxval(abs(mx0kinv(1:5,4)))*Eerr(4,r) , &
                   maxval(abs(mx0kinv(1:5,5)))*Eerr(5,r) )
        Ferr2(r+1) = max( abs(maxzmx0kinvs)*Eerr2(0,r), &
                   abs(maxzmx0kinv(1))*Eerr2(1,r) , &
                   abs(maxzmx0kinv(2))*Eerr2(2,r) , &
                   abs(maxzmx0kinv(3))*Eerr2(3,r) , &
                   abs(maxzmx0kinv(4))*Eerr2(4,r) , &
                   abs(maxzmx0kinv(5))*Eerr2(5,r) )/maxZ

!        write(*,*) 'CalcFred Ferr',r, Ferr(r+1) 
!        write(*,*) 'CalcFred Ferr',Eerr(0:5,r) 
!        write(*,*) 'CalcFred Ferr2',r, Ferr2(r+1) 
!        write(*,*) 'CalcFred Ferr2',Eerr2(0:5,r) 
!         write(*,*) 'CalcFred Ferr',maxval(abs(mx0kinv(1:5,1:5))),maxval(abs(mx0kinvs(1:5)))
!         write(*,*) 'CalcFred Ferr',maxval(abs(mx0kinvs(1:5)))*Eerr(0,r)
!         write(*,*) 'CalcFred Ferr',maxval(abs(mx0kinv(1:5,1:5)))*Eerr(0,r), &
!                   maxval(abs(mx0kinv(1:5,1)))*Eerr(1,r) , &
!                   maxval(abs(mx0kinv(1:5,2)))*Eerr(2,r) , &
!                   maxval(abs(mx0kinv(1:5,3)))*Eerr(3,r) , &
!                   maxval(abs(mx0kinv(1:5,4)))*Eerr(4,r) , &
!                   maxval(abs(mx0kinv(1:5,5)))*Eerr(5,r) 
!         write(*,*) 'CalcFred Ferr',Eerr(0,r), &
!                   Eerr(1,r) , &
!                   Eerr(2,r) , &
!                   Eerr(3,r) , &
!                   Eerr(4,r) , &
!                   Eerr(5,r) 
!         write(*,*) 'CalcFred Ferr',maxval(abs(mxinv(1:5,1:5))), &
!                   maxval(abs(mx0kinv(1:5,1))), &
!                   maxval(abs(mx0kinv(1:5,2))), &
!                   maxval(abs(mx0kinv(1:5,3))), &
!                   maxval(abs(mx0kinv(1:5,4))), &
!                   maxval(abs(mx0kinv(1:5,5))) 

        if (Mode_coli.lt.1) then
!          gram= mx(1:5,1:5)         
          gramdet= chdet(5,mx(1:5,1:5))
          
!         write(*,*) 'CalcFred gram=',det

          if (max(abs(F(0,0,0,0,0,0)),abs(F(0,1,0,0,0,0)),abs(F(0,0,1,0,0,0)),  &
              abs(F(0,0,0,1,0,0)),abs(F(0,0,0,0,1,0)),abs(F(0,0,0,0,0,1))      &
              )*abs(gramdet/det).gt. Ferr(r+1)) then
            

!          write(*,*) 'CalcFred Ferr=',r+1,Ferr(r+1)

            Ferr(r+1)=max(Ferr(r+1),max(                   &
                abs(F(0,0,0,0,0,0)),abs(F(0,1,0,0,0,0)),   &
                abs(F(0,0,1,0,0,0)), abs(F(0,0,0,1,0,0)),  &
                abs(F(0,0,0,0,1,0)),abs(F(0,0,0,0,0,1))      &
                )*abs(gramdet/det) )
            
            Ferr2(r+1)=max(Ferr2(r+1),max(                   &
                abs(F(0,0,0,0,0,0)),abs(F(0,1,0,0,0,0)),   &
                abs(F(0,0,1,0,0,0)), abs(F(0,0,0,1,0,0)),  &
                abs(F(0,0,0,0,1,0)),abs(F(0,0,0,0,0,1))      &
                )*abs(gramdet/det) )

!          write(*,*) 'CalcFred ',abs(F(0,0,0,0,0,0)),abs(F(0,1,0,0,0,0)),       &
!              abs(F(0,0,1,0,0,0)), abs(F(0,0,0,1,0,0)),  &
!              abs(F(0,0,0,0,1,0)),abs(F(0,0,0,0,0,1)),abs(gramdet/det)
!          write(*,*) 'CalcFred Ferr=',r+1,Ferr(r+1)

            if (abs(gramdet/det).gt.reqacc_coli) then
              call SetErrFlag_coli(-6)
              call ErrOut_coli('CalcFred', &
                  'input momenta inconsistent! (not 4-dimensional)',  &
                  errorwriteflag)
              if (errorwriteflag) then
                write(nerrout_coli,fmt10) ' CalcFred: q10 = ',q10
                write(nerrout_coli,fmt10) ' CalcFred: q21 = ',q21
                write(nerrout_coli,fmt10) ' CalcFred: q32 = ',q32
                write(nerrout_coli,fmt10) ' CalcFred: q43 = ',q43
                write(nerrout_coli,fmt10) ' CalcFred: q54 = ',q54
                write(nerrout_coli,fmt10) ' CalcFred: q50 = ',q50
                write(nerrout_coli,fmt10) ' CalcFred: q20 = ',q10
                write(nerrout_coli,fmt10) ' CalcFred: q31 = ',q31
                write(nerrout_coli,fmt10) ' CalcFred: q42 = ',q42
                write(nerrout_coli,fmt10) ' CalcFred: q53 = ',q53
                write(nerrout_coli,fmt10) ' CalcFred: q40 = ',q40
                write(nerrout_coli,fmt10) ' CalcFred: q51 = ',q51
                write(nerrout_coli,fmt10) ' CalcFred: q30 = ',q30
                write(nerrout_coli,fmt10) ' CalcFred: q41 = ',q41
                write(nerrout_coli,fmt10) ' CalcFred: q52 = ',q52
                write(nerrout_coli,fmt10) ' CalcFred: mm02 = ',mm02
                write(nerrout_coli,fmt10) ' CalcFred: mm12 = ',mm12
                write(nerrout_coli,fmt10) ' CalcFred: mm22 = ',mm22   
                write(nerrout_coli,fmt10) ' CalcFred: mm32 = ',mm32   
                write(nerrout_coli,fmt10) ' CalcFred: mm42 = ',mm42   
                write(nerrout_coli,fmt10) ' CalcFred: mm52 = ',mm52   
                write(nerrout_coli,fmt10) ' CalcFred: gram = ',gramdet/det  
              end if
            end if
            
          end if
        end if

!        write(*,*) 'CalcFred Ferr',r, Ferr(r+1) 
!        write(*,*) 'CalcFred Ferr',Eerr(0:5,r) 
!        write(*,*) 'CalcFred Ferr2',r, Ferr2(r+1) 
!        write(*,*) 'CalcFred Ferr2',Eerr2(0:5,r) 

      end if

    end do                       

  end subroutine CalcFred




  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcG(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
  !                         p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
  !                         m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerr,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcG(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                         p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                         m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerr,Gerr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double complex, intent(out) :: G(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Guv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double precision, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double complex, allocatable :: Gaux(:,:,:,:,:,:,:), Guvaux(:,:,:,:,:,:,:), fct(:)
    double precision, allocatable :: Gerraux(:),Gerr2aux(:)
    double complex :: x(28)
    integer :: rank,switch,cnt,n0,n1,n2,n3,n4,n5,n6,r,r0
    logical :: nocalc,wrica,noten

    r0 = 0

    if ((use_cache_system).and.(tencache.gt.7)) then
      if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
!        if (use_cache(ncache).ge.7) then 
          x(1) = p10
          x(2) = p21
          x(3) = p32
          x(4) = p43
          x(5) = p54
          x(6) = p65
          x(7) = p60
          x(8) = p20
          x(9) = p31
          x(10) = p42
          x(11) = p53
          x(12) = p64
          x(13) = p50
          x(14) = p61
          x(15) = p30
          x(16) = p41
          x(17) = p52
          x(18) = p63
          x(19) = p40
          x(20) = p51
          x(21) = p62
          x(22) = m02
          x(23) = m12
          x(24) = m22
          x(25) = m32
          x(26) = m42
          x(27) = m52
          x(28) = m62

          rank = rmax

          if (rmax.ge.10) then
            allocate(fct(NCoefs(rmax,7)+NCoefs(rmax-10,7)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,7)+NCoefs(rmax-10,7)+2*(rmax+1),x,28,1,id,7,rank,nocalc,wrica)
          else
            allocate(fct(NCoefs(rmax,7)+2*(rmax+1)))
            call ReadCache(fct,NCoefs(rmax,7)+2*(rmax+1),x,28,1,id,7,rank,nocalc,wrica)
          end if
    
          if(nocalc) then
            cnt = 0
            Guv(0:min(rmax/2,4),:,:,:,:,:,:) = 0d0
            do r=0,rmax
              do n0=0,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      do n4=0,r-2*n0-n1-n2-n3
                        do n5=0,r-2*n0-n1-n2-n3-n4
                          n6 = r-2*n0-n1-n2-n3-n4-n5
 
                          cnt = cnt+1
                          G(n0,n1,n2,n3,n4,n5,n6) = fct(cnt)
              
                        end do
                      end do
                    end do
                  end do
                end do
              end do
              do n0=5,r/2
                do n1=0,r-2*n0
                  do n2=0,r-2*n0-n1
                    do n3=0,r-2*n0-n1-n2
                      do n4=0,r-2*n0-n1-n2-n3
                        do n5=0,r-2*n0-n1-n2-n3-n4
                          n6 = r-2*n0-n1-n2-n3-n4-n5
 
                          cnt = cnt+1
                          Guv(n0,n1,n2,n3,n4,n5,n6) = fct(cnt)
              
                        end do
                      end do
                    end do
                  end do
                end do
              end do
              cnt = cnt+1
              Gerr(r) = real(fct(cnt))
              cnt = cnt+1
              Gerr2(r) = real(fct(cnt))
            end do     
            return
          endif


          if(rank.eq.rmax) then

            call CalcGred(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                                p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                                m02,m12,m22,m32,m42,m52,m62,rank,id,Gerr,Gerr2)

            if (wrica) then
              cnt = 0
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          do n5=0,r-2*n0-n1-n2-n3-n4
                            n6 = r-2*n0-n1-n2-n3-n4-n5
 
                            cnt = cnt+1
                            fct(cnt) = G(n0,n1,n2,n3,n4,n5,n6)

                          end do
                        end do
                      end do
                    end do
                  end do
                end do
                do n0=5,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          do n5=0,r-2*n0-n1-n2-n3-n4
                            n6 = r-2*n0-n1-n2-n3-n4-n5
 
                            cnt = cnt+1
                            fct(cnt) = Guv(n0,n1,n2,n3,n4,n5,n6)

                          end do
                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Gerr(r)
                cnt = cnt+1
                fct(cnt) = Gerr2(r)
              end do

              if (rmax.ge.10) then
                call WriteCache(fct,NCoefs(rank,7)+NCoefs(rank-10,7)+2*(rank+1),id,7,rank)
              else
                call WriteCache(fct,NCoefs(rank,7)+2*(rank+1),id,7,rank)
              end if
      
            end if

            return
          
          
          else
            allocate(Gaux(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank,0:rank))
            allocate(Guvaux(0:rank/2,0:rank,0:rank,0:rank,0:rank,0:rank,0:rank))
            allocate(Gerraux(0:rank))
            allocate(Gerr2aux(0:rank))

            call CalcGred(Gaux,Guvaux,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                                      p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                                      m02,m12,m22,m32,m42,m52,m62,rank,id,Gerraux,Gerr2aux)

            if (wrica) then
              cnt = 0
              if (rmax.ge.10) then
                deallocate(fct)
                allocate(fct(NCoefs(rank,7)+NCoefs(rank-10,7)+2*(rank+1)))
              else
                deallocate(fct)
                allocate(fct(NCoefs(rank,7)+2*(rank+1)))
              end if
              do r=0,rank
                do n0=0,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          do n5=0,r-2*n0-n1-n2-n3-n4
                            n6 = r-2*n0-n1-n2-n3-n4-n5
 
                            cnt = cnt+1
                            fct(cnt) = Gaux(n0,n1,n2,n3,n4,n5,n6)

                          end do
                        end do
                      end do
                    end do
                  end do
                end do
                do n0=5,r/2
                  do n1=0,r-2*n0
                    do n2=0,r-2*n0-n1
                      do n3=0,r-2*n0-n1-n2
                        do n4=0,r-2*n0-n1-n2-n3
                          do n5=0,r-2*n0-n1-n2-n3-n4
                            n6 = r-2*n0-n1-n2-n3-n4-n5
 
                            cnt = cnt+1
                            fct(cnt) = Guvaux(n0,n1,n2,n3,n4,n5,n6)

                          end do
                        end do
                      end do
                    end do
                  end do
                end do
                cnt = cnt+1
                fct(cnt) = Gerraux(r)
                cnt = cnt+1
                fct(cnt) = Gerr2aux(r)
              end do

              if (rmax.ge.10) then
                call WriteCache(fct,NCoefs(rank,7)+NCoefs(rank-10,7)+2*(rank+1),id,7,rank)
              else
                call WriteCache(fct,NCoefs(rank,7)+2*(rank+1),id,7,rank)
              end if
      
            end if
   
            G = Gaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
            Guv = Guvaux(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
            Gerr = Gerraux(0:rmax)
            Gerr2 = Gerr2aux(0:rmax)
            return
            
          end if

!        end if
      end if
    end if


    call CalcGred(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                        p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                        m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerr,Gerr2)


  end subroutine CalcG





  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine CalcGred(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
  !                         p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
  !                         m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerr,Gerr2)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine CalcGred(G,Guv,p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53,  &
                            p64,p50,p61,p30,p41,p52,p63,p40,p51,p62,  &
                            m02,m12,m22,m32,m42,m52,m62,rmax,id,Gerr,Gerr2)

    integer, intent(in) :: rmax,id
    double complex, intent(in) :: p10,p21,p32,p43,p54,p65,p60,p20,p31,p42,p53
    double complex, intent(in) :: p64,p50,p61,p30,p41,p52,p63,p40,p51,p62
    double complex, intent(in) :: m02,m12,m22,m32,m42,m52,m62
    double precision, intent(out) :: Gerr(0:rmax),Gerr2(0:rmax)
    double complex :: q10,q21,q32,q43,q54,q20,q31,q42,q53,q50,q30,q41,q52,q40,q51,q60
    double complex :: mm02,mm12,mm22,mm32,mm42,mm52,mm62
    double complex :: mx(0:5,0:5),mx0k(5,5),mx0kinv(5,5), f(6)
    double complex :: det,newdet,mx0kinvs(5)
!    double complex :: mxinv(0:5,0:5),mxinvs
    double complex :: zmx0kinv(5,5),zmx0kinvs(5)
    double precision :: maxZ,maxzmx0kinv(5),maxzmx0kinvs
    double complex, intent(out) :: G(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, intent(out) :: Guv(0:rmax/2,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax,0:rmax)
    double complex, allocatable :: F_0(:,:,:,:,:,:,:), F_i(:,:,:,:,:,:,:)
    double complex, allocatable :: Fuv_0(:,:,:,:,:,:,:), Fuv_i(:,:,:,:,:,:,:)  
    double complex :: S(5), Gaux(5), elimminf2_coli,chdet,gramdet
    double precision :: Ferr(0:5,0:rmax), Ferr2(0:5,0:rmax)
    integer :: r,n0,n1,n2,n3,n4,n5,n6,n,k,i,j,nid(0:5),r0,bin,kbest,rmaxF,rBCD,up
    logical :: errorwriteflag
    character(len=*),parameter :: fmt10 = "(A17,'(',d25.18,' , ',d25.18,' )')"

    double complex :: mxmx0kinv(5,5)
    double precision :: maxf
    integer        :: jmax

    r0=0

    ! allocation of F functions
    rmaxF = max(rmax-1,0)
    allocate(F_0(0:rmaxF/2,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF))
    allocate(F_i(0:rmaxF/2,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,5))
    allocate(Fuv_0(0:rmaxF/2,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF))
    allocate(Fuv_i(0:rmaxF/2,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,0:rmaxF,5))


    ! determine binaries for F-coefficients
    k=0
    bin = 1
    do while (k.le.5)
      if (mod(id/bin,2).eq.0) then
        nid(k) = id+bin
        k = k+1
      end if
      bin = 2*bin
    end do

    call CalcF(F_0(:,0,:,:,:,:,:),Fuv_0(:,0,:,:,:,:,:),p21,p32,p43,p54,p65,p61,  &
        p31,p42,p53,p64,p51,p62,p41,p52,p63,m12,m22,m32,m42,m52,m62,    &  
        rmaxF,nid(0),Ferr=Ferr(0,0:rmaxF),Ferr2=Ferr2(0,0:rmaxF))
    call CalcF(F_i(:,:,:,:,:,:,1),Fuv_i(:,:,:,:,:,:,1),p20,p32,p43,p54,p65,p60,  &
        p30,p42,p53,p64,p50,p62,p40,p52,p63,m02,m22,m32,m42,m52,m62,    &
        rmaxF,nid(1),Ferr=Ferr(1,0:rmaxF),Ferr2=Ferr2(1,0:rmaxF))
    call CalcF(F_i(:,:,:,:,:,:,2),Fuv_i(:,:,:,:,:,:,2),p10,p31,p43,p54,p65,p60,  &
        p30,p41,p53,p64,p50,p61,p40,p51,p63,m02,m12,m32,m42,m52,m62,    &
        rmaxF,nid(2),Ferr=Ferr(2,0:rmaxF),Ferr2=Ferr2(2,0:rmaxF))
    call CalcF(F_i(:,:,:,:,:,:,3),Fuv_i(:,:,:,:,:,:,3),p10,p21,p42,p54,p65,p60,  &
        p20,p41,p52,p64,p50,p61,p40,p51,p62,m02,m12,m22,m42,m52,m62,    &
        rmaxF,nid(3),Ferr=Ferr(3,0:rmaxF),Ferr2=Ferr2(3,0:rmaxF))
    call CalcF(F_i(:,:,:,:,:,:,4),Fuv_i(:,:,:,:,:,:,4),p10,p21,p32,p53,p65,p60,  &
        p20,p31,p52,p63,p50,p61,p30,p51,p62,m02,m12,m22,m32,m52,m62,    &
        rmaxF,nid(4),Ferr=Ferr(4,0:rmaxF),Ferr2=Ferr2(4,0:rmaxF))
    call CalcF(F_i(:,:,:,:,:,:,5),Fuv_i(:,:,:,:,:,:,5),p10,p21,p32,p43,p64,p60,  &
        p20,p31,p42,p63,p40,p61,p30,p41,p62,m02,m12,m22,m32,m42,m62,    &
        rmaxF,nid(5),Ferr=Ferr(5,0:rmaxF),Ferr2=Ferr2(5,0:rmaxF))

    ! shift of integration momentum in F\{0}
    do n1=1,rmaxF
      do n2=0,rmaxF-n1
        do n3=0,rmaxF-n1-n2
          do n4=0,rmaxF-n1-n2-n3
            do n5=0,rmaxF-n1-n2-n3-n4
              do n6=0,rmaxF-n1-n2-n3-n4-n5
                n0 = (rmaxF-n1-n2-n3-n4-n5-n6)/2
                Fuv_0(0:n0,n1,n2,n3,n4,n5,n6) = -Fuv_0(0:n0,n1-1,n2,n3,n4,n5,n6)  &
                                                -Fuv_0(0:n0,n1-1,n2+1,n3,n4,n5,n6)  &
                                                -Fuv_0(0:n0,n1-1,n2,n3+1,n4,n5,n6)  &
                                                -Fuv_0(0:n0,n1-1,n2,n3,n4+1,n5,n6)  &
                                                -Fuv_0(0:n0,n1-1,n2,n3,n4,n5+1,n6)  &
                                                -Fuv_0(0:n0,n1-1,n2,n3,n4,n5,n6+1)
                F_0(0:n0,n1,n2,n3,n4,n5,n6) = -F_0(0:n0,n1-1,n2,n3,n4,n5,n6)  &
                                              -F_0(0:n0,n1-1,n2+1,n3,n4,n5,n6)  &
                                              -F_0(0:n0,n1-1,n2,n3+1,n4,n5,n6)  &
                                              -F_0(0:n0,n1-1,n2,n3,n4+1,n5,n6)  &
                                              -F_0(0:n0,n1-1,n2,n3,n4,n5+1,n6)  &
                                              -F_0(0:n0,n1-1,n2,n3,n4,n5,n6+1)
              end do
            end do
          end do
        end do
      end do
    end do


    ! determine inverse modified Caley matrix
    mm02 = elimminf2_coli(m02)
    mm12 = elimminf2_coli(m12)
    mm22 = elimminf2_coli(m22)
    mm32 = elimminf2_coli(m32)
    mm42 = elimminf2_coli(m42)
    mm52 = elimminf2_coli(m52)
    mm62 = elimminf2_coli(m62)
    q10  = elimminf2_coli(p10)
    q21  = elimminf2_coli(p21)
    q32  = elimminf2_coli(p32)
    q43  = elimminf2_coli(p43)
    q54  = elimminf2_coli(p54)
    q50  = elimminf2_coli(p50)
    q20  = elimminf2_coli(p20)
    q31  = elimminf2_coli(p31)
    q42  = elimminf2_coli(p42)
    q53  = elimminf2_coli(p53)
    q40  = elimminf2_coli(p40)
    q51  = elimminf2_coli(p51)
    q30  = elimminf2_coli(p30)
    q41  = elimminf2_coli(p41)
    q52  = elimminf2_coli(p52)
    q60  = elimminf2_coli(p60)

    f(1) = q10+mm02-mm12
    f(2) = q20+mm02-mm22
    f(3) = q30+mm02-mm32
    f(4) = q40+mm02-mm42
    f(5) = q50+mm02-mm52
    f(6) = q60+mm02-mm62
 
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

!    call chinv(6,mx,mxinv)

    ! determine X_(0,5)
    do j=1,5
      do i=1,5
        mx0k(i,j) = mx(i,j-1)
      end do
    end do

    det = chdet(5,mx0k)
    kbest = 5

    do j=5,2,-1
      do i=1,5
        mx0k(i,j) = mx(i,j)
      end do

      newdet =  chdet(5,mx0k)
      if (abs(newdet).gt.abs(det)) then          
        kbest = j-1
        det = newdet
      end if
    
    end do
    
    do i=1,5
      mx0k(i,1) = mx(i,1)
      mx0k(i,kbest) = mx(i,0)
    end do

! changed 21.06.2018
    call chinv(5,mx0k,mx0kinv,det)

    if (det.eq.0d0) then
      call SetErrFlag_coli(-7)
      call ErrOut_coli('CalcGred',  &
          'inverse matrix M does not exist',  &
          errorwriteflag)
      if (errorwriteflag) then
        write(nerrout_coli,fmt10) ' CalcGred: q10 = ',q10
        write(nerrout_coli,fmt10) ' CalcGred: q21 = ',q21
        write(nerrout_coli,fmt10) ' CalcGred: q32 = ',q32
        write(nerrout_coli,fmt10) ' CalcGred: q43 = ',q43
        write(nerrout_coli,fmt10) ' CalcGred: q54 = ',q54
        write(nerrout_coli,fmt10) ' CalcGred: q50 = ',q50
        write(nerrout_coli,fmt10) ' CalcGred: q20 = ',q10
        write(nerrout_coli,fmt10) ' CalcGred: q31 = ',q31
        write(nerrout_coli,fmt10) ' CalcGred: q42 = ',q42
        write(nerrout_coli,fmt10) ' CalcGred: q53 = ',q53
        write(nerrout_coli,fmt10) ' CalcGred: q40 = ',q40
        write(nerrout_coli,fmt10) ' CalcGred: q51 = ',q51
        write(nerrout_coli,fmt10) ' CalcGred: q30 = ',q30
        write(nerrout_coli,fmt10) ' CalcGred: q41 = ',q41
        write(nerrout_coli,fmt10) ' CalcGred: q52 = ',q52
        write(nerrout_coli,fmt10) ' CalcGred: mm02 = ',mm02
        write(nerrout_coli,fmt10) ' CalcGred: mm12 = ',mm12
        write(nerrout_coli,fmt10) ' CalcGred: mm22 = ',mm22
        write(nerrout_coli,fmt10) ' CalcGred: mm32 = ',mm32
        write(nerrout_coli,fmt10) ' CalcGred: mm42 = ',mm42
        write(nerrout_coli,fmt10) ' CalcGred: mm52 = ',mm52
      end if
      G = 0d0
      return
    end if

    do i=1,5
      mx0kinv(kbest,i) = 0d0
    end do

!    mxinvs = sum(mxinv(0:5,0))
    do i=1,5
      mx0kinvs(i) = sum(mx0kinv(i,1:5))
    end do

    ! for alternative error estimate
!    Z(1:5,1:5) = mx(1:5,1:5)
    zmx0kinv = matmul(mx(1:5,1:5),mx0kinv) 

    do i=1,5
      maxzmx0kinv(i) = maxval(abs(zmx0kinv(1:5,i)))
      zmx0kinvs(i) = sum(zmx0kinv(i,1:5))
    end do

    maxzmx0kinvs = maxval(abs(zmx0kinvs(1:5)))

    maxZ = maxval(abs(mx(1:5,1:5)))


    ! calculation of UV-divergent parts      
    ! G_(n0,n1,n2,n3) UV-finite for n0<5
    Guv(0:min(rmax/2,4),:,:,:,:,:,:) = 0d0
    
    ! PV reduction (5.10)
    do r=max(r0,10),rmax
      do n0=5,r/2
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                do n5=0,r-2*n0-n1-n2-n3-n4
                  n6 = r-2*n0-n1-n2-n3-n4-n5
        
                  Guv(n0,n1,n2,n3,n4,n5,n6) = (Fuv_0(n0-1,n1,n2,n3,n4,n5,n6)  & 
                                            + 2*m02*Guv(n0-1,n1,n2,n3,n4,n5,n6)  &
                                            + f(1)*Guv(n0-1,n1+1,n2,n3,n4,n5,n6)  & 
                                            + f(2)*Guv(n0-1,n1,n2+1,n3,n4,n5,n6)  & 
                                            + f(3)*Guv(n0-1,n1,n2,n3+1,n4,n5,n6)  &
                                            + f(4)*Guv(n0-1,n1,n2,n3,n4+1,n5,n6)  &
                                            + f(5)*Guv(n0-1,n1,n2,n3,n4,n5+1,n6)  &
                                            + f(6)*Guv(n0-1,n1,n2,n3,n4,n5,n6+1))/ (2*(r-4))
    
                end do
              end do
            end do
          end do
        end do
      end do
    end do

    G=0d0

    ! scalar coefficient

! version replaced 21.06.2018
!    if (r0.eq.0) then
!      G = 0d0
!      G(0,0,0,0,0,0,0) = -mxinv(0,0)*F_0(0,0,0,0,0,0,0)
!      do k=1,5
!        G(0,0,0,0,0,0,0) = G(0,0,0,0,0,0,0) &
!             + mxinv(k,0)*(F_i(0,0,0,0,0,0,k)-F_0(0,0,0,0,0,0,0))
!      end do
!    end if
!
!    Gerr(0) = max( abs(mxinvs)*Ferr(0,0), &
!                   abs(mxinv(1,0))*Ferr(1,0) , &
!                   abs(mxinv(2,0))*Ferr(2,0) , &
!                   abs(mxinv(3,0))*Ferr(3,0) , &
!                   abs(mxinv(4,0))*Ferr(4,0) , &
!                   abs(mxinv(5,0))*Ferr(5,0) )
!    Gerr2(0) = max(abs(mxinvs)*Ferr2(0,0), &
!                   abs(mxinv(1,0))*Ferr2(1,0) , &
!                   abs(mxinv(2,0))*Ferr2(2,0) , &
!                   abs(mxinv(3,0))*Ferr2(3,0) , &
!                   abs(mxinv(4,0))*Ferr2(4,0) , &
!                   abs(mxinv(5,0))*Ferr2(5,0) )
!
!    write(*,*) 'CalcGred: G(0)',G(0,0,0,0,0,0,0)
!    write(*,*) 'CalcGred: Gerr(0)',Gerr(0),Gerr2(0)

! New version for  G(0,0,0,0,0,0,0), 21.06.2018

    maxf = abs(mx(1,0))
    jmax = 1
    do j=2,5
      if (abs(mx(j,0)).gt.maxf) then
         jmax = j
         maxf = abs(mx(j,0))
      end if
    end do

    mxmx0kinv = matmul(mx(1:5,1:5),mx0kinv)

    G(0,0,0,0,0,0,0) =  F_i(0,0,0,0,0,0,jmax) - F_0(0,0,0,0,0,0,0)
    do j=1,5
      G(0,0,0,0,0,0,0) =  G(0,0,0,0,0,0,0) & 
          - mxmx0kinv(jmax,j) * (F_i(0,0,0,0,0,0,j) - F_0(0,0,0,0,0,0,0))
    end do
    G(0,0,0,0,0,0,0) =  G(0,0,0,0,0,0,0)/mx(jmax,0)

    Gerr(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*Ferr(0,0), &
                   abs(mxmx0kinv(jmax,1))*Ferr(1,0) , &
                   abs(mxmx0kinv(jmax,2))*Ferr(2,0) , &
                   abs(mxmx0kinv(jmax,3))*Ferr(3,0) , &
                   abs(mxmx0kinv(jmax,4))*Ferr(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*Ferr(5,0) , &
                   Ferr(0,0) , Ferr(jmax,0) )/abs(mx(jmax,0))

    Gerr2(0) = max(maxval(abs(mxmx0kinv(jmax,:)))*Ferr2(0,0), &
                   abs(mxmx0kinv(jmax,1))*Ferr2(1,0) , &
                   abs(mxmx0kinv(jmax,2))*Ferr2(2,0) , &
                   abs(mxmx0kinv(jmax,3))*Ferr2(3,0) , &
                   abs(mxmx0kinv(jmax,4))*Ferr2(4,0) , & 
                   abs(mxmx0kinv(jmax,5))*Ferr2(5,0) , &
                   Ferr2(0,0),Ferr2(jmax,0))/abs(mx(jmax,0))

    ! formula (7.13) extended to N=7
    do r=r0,rmax-1
      do n0=0,max((r-1)/2,0)
        do n1=0,r-2*n0
          do n2=0,r-2*n0-n1
            do n3=0,r-2*n0-n1-n2
              do n4=0,r-2*n0-n1-n2-n3
                do n5=0,r-2*n0-n1-n2-n3-n4
                  n6 = r-2*n0-n1-n2-n3-n4-n5
            
                  do n=1,5
                    S(n) = -F_0(n0,n1,n2,n3,n4,n5,n6)
                  end do

                  if (n1.eq.0) then
                    S(1) = S(1) + F_i(n0,n2,n3,n4,n5,n6,1)
                  end if
                  if (n2.eq.0) then
                    S(2) = S(2) + F_i(n0,n1,n3,n4,n5,n6,2)
                  end if
                  if (n3.eq.0) then
                    S(3) = S(3) + F_i(n0,n1,n2,n4,n5,n6,3)
                  end if
                  if (n4.eq.0) then
                    S(4) = S(4) + F_i(n0,n1,n2,n3,n5,n6,4)
                  end if
                  if (n5.eq.0) then
                    S(5) = S(5) + F_i(n0,n1,n2,n3,n4,n6,5)
                  end if

                  do k=1,5
                    Gaux(k) = mx0kinv(k,1)*S(1)+mx0kinv(k,2)*S(2) &
                            + mx0kinv(k,3)*S(3)+mx0kinv(k,4)*S(4)+mx0kinv(k,5)*S(5)
                  end do

                  G(n0,n1+1,n2,n3,n4,n5,n6) = G(n0,n1+1,n2,n3,n4,n5,n6) + (n1+1)*Gaux(1)/(r+1)
                  G(n0,n1,n2+1,n3,n4,n5,n6) = G(n0,n1,n2+1,n3,n4,n5,n6) + (n2+1)*Gaux(2)/(r+1)
                  G(n0,n1,n2,n3+1,n4,n5,n6) = G(n0,n1,n2,n3+1,n4,n5,n6) + (n3+1)*Gaux(3)/(r+1)
                  G(n0,n1,n2,n3,n4+1,n5,n6) = G(n0,n1,n2,n3,n4+1,n5,n6) + (n4+1)*Gaux(4)/(r+1)
                  G(n0,n1,n2,n3,n4,n5+1,n6) = G(n0,n1,n2,n3,n4,n5+1,n6) + (n5+1)*Gaux(5)/(r+1)

                end do
              end do
            end do
          end do
        end do
      end do

      if (r.le.rmax-1) then  
!        Gerr(r+1) = max( maxval(abs(mx0kinv(1:5,1:5)))*Ferr(0,r), &
        Gerr(r+1) = max( maxval(abs(mx0kinvs(1:5)))*Ferr(0,r), &
                   maxval(abs(mx0kinv(1:5,1)))*Ferr(1,r) , &
                   maxval(abs(mx0kinv(1:5,2)))*Ferr(2,r) , &
                   maxval(abs(mx0kinv(1:5,3)))*Ferr(3,r) , &
                   maxval(abs(mx0kinv(1:5,4)))*Ferr(4,r) , &
                   maxval(abs(mx0kinv(1:5,5)))*Ferr(5,r) )

        Gerr2(r+1) = max( abs(maxzmx0kinvs)*Ferr2(0,r), &
                   abs(maxzmx0kinv(1))*Ferr2(1,r) , &
                   abs(maxzmx0kinv(2))*Ferr2(2,r) , &
                   abs(maxzmx0kinv(3))*Ferr2(3,r) , &
                   abs(maxzmx0kinv(4))*Ferr2(4,r) , &
                   abs(maxzmx0kinv(5))*Ferr2(5,r) ) /maxZ
      end if

      if (Mode_coli.lt.1) then
!        gram= mx(1:5,1:5)       
        gramdet= chdet(5,mx(1:5,1:5))
        
        if (max(abs(G(0,0,0,0,0,0,0)),abs(G(0,1,0,0,0,0,0)),abs(G(0,0,1,0,0,0,0)),  &
            abs(G(0,0,0,1,0,0,0)),abs(G(0,0,0,0,1,0,0)),abs(G(0,0,0,0,0,1,0)),      &
            abs(G(0,0,0,0,0,0,1)))*abs(gramdet/det).gt. Gerr(r+1)) then


!          write(*,*) 'CalcGred Gerr=',r+1,Gerr(r+1)

          Gerr(r+1)=max(Gerr(r+1),max(                   &
              abs(G(0,0,0,0,0,0,0)),abs(G(0,1,0,0,0,0,0)),   &
              abs(G(0,0,1,0,0,0,0)), abs(G(0,0,0,1,0,0,0)),  &
              abs(G(0,0,0,0,1,0,0)),abs(G(0,0,0,0,0,1,0)),      &
              abs(G(0,0,0,0,0,0,1)))*abs(gramdet/det) )

          Gerr2(r+1)=max(Gerr2(r+1),max(                   &
              abs(G(0,0,0,0,0,0,0)),abs(G(0,1,0,0,0,0,0)),   &
              abs(G(0,0,1,0,0,0,0)), abs(G(0,0,0,1,0,0,0)),  &
              abs(G(0,0,0,0,1,0,0)),abs(G(0,0,0,0,0,1,0)),      &
              abs(G(0,0,0,0,0,0,1)))*abs(gramdet/det) )

!          write(*,*) 'CalcGred ',abs(G(0,0,0,0,0,0)),abs(G(0,1,0,0,0,0)),       &
!              abs(G(0,0,1,0,0,0)), abs(G(0,0,0,1,0,0)),  &
!              abs(G(0,0,0,0,1,0)),abs(G(0,0,0,0,0,1)),abs(gramdet/det)
!          write(*,*) 'CalcGred Gerr=',r+1,Gerr(r+1)

          if (abs(gramdet/det).gt.reqacc_coli) then
            call SetErrFlag_coli(-6)
            call ErrOut_coli('CalcGred', &
                'input momenta inconsistent! (not 4-dimensional)',  &
                errorwriteflag)
            if (errorwriteflag) then
              write(nerrout_coli,fmt10) ' CalcGred: q10 = ',q10
              write(nerrout_coli,fmt10) ' CalcGred: q21 = ',q21
              write(nerrout_coli,fmt10) ' CalcGred: q32 = ',q32
              write(nerrout_coli,fmt10) ' CalcGred: q43 = ',q43
              write(nerrout_coli,fmt10) ' CalcGred: q54 = ',q54
              write(nerrout_coli,fmt10) ' CalcGred: q50 = ',q50
              write(nerrout_coli,fmt10) ' CalcGred: q20 = ',q10
              write(nerrout_coli,fmt10) ' CalcGred: q31 = ',q31
              write(nerrout_coli,fmt10) ' CalcGred: q42 = ',q42
              write(nerrout_coli,fmt10) ' CalcGred: q53 = ',q53
              write(nerrout_coli,fmt10) ' CalcGred: q40 = ',q40
              write(nerrout_coli,fmt10) ' CalcGred: q51 = ',q51
              write(nerrout_coli,fmt10) ' CalcGred: q30 = ',q30
              write(nerrout_coli,fmt10) ' CalcGred: q41 = ',q41
              write(nerrout_coli,fmt10) ' CalcGred: q52 = ',q52
              write(nerrout_coli,fmt10) ' CalcGred: mm02 = ',mm02
              write(nerrout_coli,fmt10) ' CalcGred: mm12 = ',mm12
              write(nerrout_coli,fmt10) ' CalcGred: mm22 = ',mm22   
              write(nerrout_coli,fmt10) ' CalcGred: mm32 = ',mm32   
              write(nerrout_coli,fmt10) ' CalcGred: mm42 = ',mm42   
              write(nerrout_coli,fmt10) ' CalcGred: mm52 = ',mm52   
              write(nerrout_coli,fmt10) ' CalcGred: gram = ',gramdet/det  
            end if
          end if

        end if
      end if

    end do                       

  end subroutine CalcGred



end module reductionEFG
