!!
!!  File coli_stat.F90 is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, see COPYING for details.
!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  **********************
!  *  module coli_stat  *
!  *  by Ansgar Denner  *
!  **********************
! 
!  functions and subroutines:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module coli_stat

  implicit none

#ifdef GM
  integer, parameter :: nmethodC=6, nmethodD=7
#else
!  integer, parameter :: nmethodC=6, nmethodD=6
  integer, parameter :: nmethodC=7, nmethodD=7
#endif
  integer, parameter :: kcountC=2**nmethodC
  integer, parameter :: kcountD=2**nmethodD
  integer, parameter :: CCountoffset0=20
  integer, parameter :: Ccountoffset1=CCountoffset0+kcountC
  integer, parameter :: CCountoffset2=Ccountoffset1+kcountC
  integer, parameter :: CCountoffset3=Ccountoffset2+kcountC
  integer, parameter :: DCountoffset0=20
  integer, parameter :: Dcountoffset1=DCountoffset0+kcountD
  integer, parameter :: DCountoffset2=Dcountoffset1+kcountD
  integer, parameter :: DCountoffset3=Dcountoffset2+kcountD
  integer, parameter :: ncountC=CCountoffset3+2**nmethodC   
  integer, parameter :: ncountD=DCountoffset3+2**nmethodD   
  integer (kind=8) :: CCount(0:ncountC),DCount(0:ncountD)

contains

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  subroutine InitStatisticsa_coli
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine InitStatisticsa_coli   

    CCount = 0
    DCount = 0 

  end subroutine InitStatisticsa_coli


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine PrintStatistics_coli

    use coli_aux2

    integer :: i
    double precision :: normC,normD
    character :: stringC(0:ncountC)*32  !  4*nmethodC
#ifdef GM
    character :: stringD(0:ncountD)*36  !  4*nmethodD
#else
!    character :: stringD(0:ncountD)*24  !  4*nmethodD
    character :: stringD(0:ncountD)*32  !  4*nmethodD
#endif

!    if (nout.ne.0) then 
!      call setnstatsoutcoli_cll(nout)
!     call setnstatsout_coli(nout)
!     if (nout.ne.6) then
!       call OpenStatisticsOutFileCOLI_cll('output_cll/StatisticsOut.coli')
!     end if
!    end if 

    normC = dble(max(CCount(0),1))
    normD = dble(max(DCount(0),1))

!    write(*,*) 'printstat C',CCount
!    write(*,*) 'printstat D',DCount


    stringC(0) = 'total   '
    stringC(2**nmethodC) = 'sum    '
    CCount(2**nmethodC+CCountoffset0) = 0
    CCount(2**nmethodC+CCountoffset1) = 0
    CCount(2**nmethodC+CCountoffset2) = 0
    CCount(2**nmethodC+CCountoffset3) = 0
    do i=1,2**nmethodC-1
      stringC(i)= '                    '
      if (mod(i,2).eq.1) stringC(i)(1:4) = 'pv1 '
      if (mod(i,4)-mod(i,2).eq.2) stringC(i)(5:8) = 'pv2 '
      if (mod(i,8)-mod(i,4).eq.4) stringC(i)(9:12) = ' g '
      if (mod(i,16)-mod(i,8).eq.8) stringC(i)(13:16) = ' gc '
      if (mod(i,32)-mod(i,16).eq.16) stringC(i)(17:20) = ' sm '
      if (mod(i,64)-mod(i,32).eq.32) stringC(i)(21:24) = ' gr '
      if (mod(i,128)-mod(i,64).eq.64) stringC(i)(25:28) = 'smf '
      CCount(2**nmethodC+CCountoffset0)=CCount(2**nmethodC+CCountoffset0)+CCount(i+CCountoffset0)
      CCount(2**nmethodC+CCountoffset1)=CCount(2**nmethodC+CCountoffset1)+CCount(i+CCountoffset1)
      CCount(2**nmethodC+CCountoffset2)=CCount(2**nmethodC+CCountoffset2)+CCount(i+CCountoffset2)
      CCount(2**nmethodC+CCountoffset3)=CCount(2**nmethodC+CCountoffset3)+CCount(i+CCountoffset3)
    end do

    stringD(0) = 'total  '
    stringD(2**nmethodD) = 'sum    '
    DCount(2**nmethodD+DCountoffset0) = 0
    DCount(2**nmethodD+DCountoffset1) = 0
    DCount(2**nmethodD+DCountoffset2) = 0
    DCount(2**nmethodD+DCountoffset3) = 0
    do i=1,2**nmethodD-1
      stringD(i)= '                    '
      if (mod(i,2).eq.1) stringD(i)(1:4) = 'pv1 '
      if (mod(i,4)-mod(i,2).eq.2) stringD(i)(5:8) = 'pv2 '
      if (mod(i,8)-mod(i,4).eq.4) stringD(i)(9:12) = ' g '
      if (mod(i,16)-mod(i,8).eq.8) stringD(i)(13:16) = ' gc '
      if (mod(i,32)-mod(i,16).eq.16) stringD(i)(17:20) = ' sm '
      if (mod(i,64)-mod(i,32).eq.32) stringD(i)(21:24) = ' gr '
      if (mod(i,128)-mod(i,64).eq.64) stringD(i)(25:28) = 'smf '
#ifdef GM
      if (mod(i,256)-mod(i,128).eq.128) stringD(i)(29:32) = ' gm '
#endif
      DCount(2**nmethodD+DCountoffset0)=DCount(2**nmethodD+DCountoffset0)+DCount(i+DCountoffset0)
      DCount(2**nmethodD+DCountoffset1)=DCount(2**nmethodD+DCountoffset1)+DCount(i+DCountoffset1)
      DCount(2**nmethodD+DCountoffset2)=DCount(2**nmethodD+DCountoffset2)+DCount(i+DCountoffset2)
      DCount(2**nmethodD+DCountoffset3)=DCount(2**nmethodD+DCountoffset3)+DCount(i+DCountoffset3)
   end do

    CCount(19)=0
    DCount(19)=0
    do i=1,6
       CCount(19)=CCount(19)+CCount(i)+CCount(10+i)
       DCount(19)=DCount(19)+DCount(i)+DCount(10+i)
    end do

    write(nstatsout_coli,100)
100 format (/' Collier: Numbers for calls of different branches in C and D reduction'/)


    write(nstatsout_coli,300) (CCount(i),dble(CCount(i))/normC*1d2,i=1,nmethodC),    &
        CCount(8),dble(CCount(8))/normC*1d2,     &
        CCount(9),dble(CCount(9))/normC*1d2,     &
        (CCount(i),dble(CCount(i))/normC*1d2,i=11,10+nmethodC),               &
        CCount(19),dble(CCount(19))/normC*1d2,     &
        CCount(0),dble(CCount(0))/normC*1d2

300 format(' #calls C pv1 1 = ',i20,' or ',F10.5,' %'/               &
    &       ' #calls C pv2 1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C g   1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C gc  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C sm  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C gr  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C smf 1 = ',i20,' or ',F10.5,' %'/              &
!    &       ' #calls C gm  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C pvs+1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C pvs 1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C pv1 2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C pv2 2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C g   2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C gc  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C sm  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C gr  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C smf 2 = ',i20,' or ',F10.5,' %'/              &
!    &       ' #calls C gm  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C all m = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls C       = ',i20,' or ',F10.5,' %'/)
 
    do i=1,2**nmethodC
      if (CCount(i+CCountoffset0).ne.0.or.i.eq.2**nmethodC) then
      write(nstatsout_coli,310) stringC(i),CCount(i+CCountoffset0),dble(CCount(i+CCountoffset0))/normC*1d2
      end if
    end do
    write(nstatsout_coli,310)   stringC(0),CCount(0),dble(CCount(0))/normC*1d2

310 format(' #calls C ',a32 ,' = ',i16,' or ',F10.5,' %')

    write(nstatsout_coli,400) (DCount(i),dble(DCount(i))/normD*1d2,i=1,nmethodD),    &
        (DCount(i),dble(DCount(i))/normD*1d2,i=11,10+nmethodD),               &
         DCount(19),dble(DCount(19))/normD*1d2,     &
         DCount(0),dble(DCount(0))/normD*1d2

400 format(/' #calls D pv1 1 = ',i20,' or ',F10.5,' %'/               &
    &       ' #calls D pv2 1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D g   1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D gc  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D sm  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D gr  1 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D smf 1 = ',i20,' or ',F10.5,' %'/              &
#ifdef GM
    &       ' #calls D gm  1 = ',i20,' or ',F10.5,' %'/              &
#endif
    &       ' #calls D pv1 2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D pv2 2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D g   2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D gc  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D sm  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D gr  2 = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D smf 2 = ',i20,' or ',F10.5,' %'/              &
#ifdef GM
    &       ' #calls D gm  2 = ',i20,' or ',F10.5,' %'/              &
#endif
    &       ' #calls D all m = ',i20,' or ',F10.5,' %'/              &
    &       ' #calls D       = ',i20,' or ',F10.5,' %'/)
 

    do i=1,2**nmethodD
      if (DCount(i+DCountoffset0).ne.0.or.i.eq.2**nmethodD) then
        write(nstatsout_coli,410) stringD(i),DCount(i+DCountoffset0),dble(DCount(i+DCountoffset0))/normD*1d2
      end if
    end do
    write(nstatsout_coli,410)   stringD(0),DCount(0),dble(DCount(0))/normD*1d2

#ifdef GM
410 format(' #calls D ',a36 ,' = ',i16,' or ',F10.5,' %')
#else
410 format(' #calls D ',a32 ,' = ',i16,' or ',F10.5,' %')
#endif

    write(nstatsout_coli,110) reqacc_coli
110 format (/' Collier: Numbers for calls of different branches in C and D reduction'/  &
             '          with an accuracy worse than reqacc_coli =',Es11.4/)

    do i=1,2**nmethodC
      if (CCount(i+CCountoffset1).ne.0.or.i.eq.2**nmethodC) then
        write(nstatsout_coli,310) stringC(i),CCount(i+CCountoffset1),dble(CCount(i+CCountoffset1))/normC*1d2
      end if
    end do
    write(nstatsout_coli,310)   stringC(0),CCount(0),dble(CCount(0))/normC*1d2
    write(nstatsout_coli,*) 

    do i=1,2**nmethodD
      if (DCount(i+DCountoffset1).ne.0.or.i.eq.2**nmethodD) then
        write(nstatsout_coli,410) stringD(i),DCount(i+DCountoffset1),dble(DCount(i+DCountoffset1))/normD*1d2
      end if
    end do
    write(nstatsout_coli,410)   stringD(0),DCount(0),dble(DCount(0))/normD*1d2

    write(nstatsout_coli,130) sqrt(reqacc_coli)
130 format (/' Collier: Numbers for calls of different branches in C and D reduction'/  &
             '          with an accuracy worse than sqrt(reqacc_coli) =',Es11.4/)

    do i=1,2**nmethodC
      if (CCount(i+CCountoffset3).ne.0.or.i.eq.2**nmethodC) then
        write(nstatsout_coli,310) stringC(i),CCount(i+CCountoffset3),dble(CCount(i+CCountoffset3))/normC*1d2
      end if
    end do
    write(nstatsout_coli,310)   stringC(0),CCount(0),dble(CCount(0))/normC*1d2
    write(nstatsout_coli,*) 

    do i=1,2**nmethodD
      if (DCount(i+DCountoffset3).ne.0.or.i.eq.2**nmethodD) then
        write(nstatsout_coli,410) stringD(i),DCount(i+DCountoffset3),dble(DCount(i+DCountoffset3))/normD*1d2
      end if
    end do
    write(nstatsout_coli,410)   stringD(0),DCount(0),dble(DCount(0))/normD*1d2


    write(nstatsout_coli,120) critacc_coli
120 format (/' Collier: Numbers for calls of different branches in C and D reduction'/  &
             '          with an accuracy worse than critacc_coli =',Es11.4/)

    do i=1,2**nmethodC
      if (CCount(i+CCountoffset2).ne.0.or.i.eq.2**nmethodC) then
        write(nstatsout_coli,310) stringC(i),CCount(i+CCountoffset2),dble(CCount(i+CCountoffset2))/normC*1d2
      end if
    end do
    write(nstatsout_coli,310)   stringC(0),CCount(0),dble(CCount(0))/normC*1d2
    write(nstatsout_coli,*) 

    do i=1,2**nmethodD
      if (DCount(i+DCountoffset2).ne.0.or.i.eq.2**nmethodD) then
        write(nstatsout_coli,410) stringD(i),DCount(i+DCountoffset2),dble(DCount(i+DCountoffset2))/normD*1d2
      end if 
   end do
    write(nstatsout_coli,410)   stringD(0),DCount(0),dble(DCount(0))/normD*1d2
    write(nstatsout_coli,*) 

  end subroutine PrintStatistics_coli


end module coli_stat
