      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'pwhg_flg.h'
      include 'LesHouches.h'
      include 'pwhg_st.h'
      include 'pwhg_par.h'
      include 'minnlo_flg.h'
      integer i1,i2,i3,i4,i5,i6,k,ii(6)
      equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),
     #  (i4,ii(4)),(i5,ii(5)),(i6,ii(6))
      logical debug
      parameter (debug=.false.)
      integer j,tmp
      real * 8 powheginput
      external powheginput
c     vector boson id and decay
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode
      logical condition
      integer charge3(-6:6)
      data charge3 /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/
      logical flavequiv
      external flavequiv
      flg_minnloproc = 'Z' ! change to 'W' when MiNNLOStuff/setlocalscales.f was changed accordingly
c     check nlegborn. This is only a sanity check while we are TESTING 
c     the code and we change often from one process to the other
      if (nlegborn.ne.5) then
         write(*,*) ' ERROR: set nlegborn to the appropriate value'
         write(*,*) ' for this process in nlegborn.h'
         stop
      endif
      st_bornorder = 1
      call minlo_checks
*********************************************************************
c     index of the first LIGHT coloured parton in the final state
      flst_lightpart=5
*********************************************************************

c******************************************************
c     Choose the process to be implemented
c******************************************************
c    ID of vector boson produced
      idvecbos=powheginput('idvecbos')
c   decay products of the vector boson
      tmp=powheginput('vdecaymode')

      if(idvecbos.eq.24) then
         select case(tmp)
         case (1)
            vdecaymode=-11
         case (2)
            vdecaymode=-13
         case (3)
            vdecaymode=-15
         case default
            write(*,*) 'ERROR: The decay mode you selected' /
     $           /' is not allowed '
            stop
         end select  
         write(*,*) 
         write(*,*) ' POWHEG: W+ plus jet production and decay ' 
         if (vdecaymode.eq.-11) write(*,*) '         to e+ ve '
         if (vdecaymode.eq.-13) write(*,*) '         to mu+ vmu'
         if (vdecaymode.eq.-15) write(*,*) '         to tau+ vtau'
         write(*,*) 
      elseif(idvecbos.eq.-24) then
          select case(tmp)
         case (1)
            vdecaymode= 11
         case (2)
            vdecaymode= 13
         case (3)
            vdecaymode= 15
         case default
            write(*,*) 'ERROR: The decay mode you selected' /
     $           /' is not allowed '
            stop
         end select
         write(*,*) 
         write(*,*) ' POWHEG: W- plus jet production and decay '
         if (vdecaymode.eq.11) write(*,*) '         to e- ve~ '
         if (vdecaymode.eq.13) write(*,*) '         to mu- vmu~'
         if (vdecaymode.eq.15) write(*,*) '         to tau- vtau~'
         write(*,*)    
      else
         write(*,*) 'ERROR: The ID of vector boson you selected' 
     $        //' is not allowed (24: W+ -24: W-)'
         stop
      endif

c     change the LHUPI id of the process according to vector boson id
c     and decay
      lprup(1)=10000+vdecaymode ! 10000+idup of charged decay product of the W
      

c*********************************************************     
      i3=vdecaymode
      if ((idvecbos.eq.24).and.(vdecaymode.lt.0)) then
         i4=-vdecaymode+1
      elseif ((idvecbos.eq.-24).and.(vdecaymode.gt.0)) then
         i4=-(vdecaymode+1)
      endif
c*************************************************************




*********************************************************************
***********            BORN  SUBPROCESSES              ***************
*********************************************************************
  

      flst_nborn=0
      condition=.false.
      do i1=-5,5
         do i2=-5,5
            if (abs(i1).eq.abs(i2)) goto 111
            do i5=-5,5
               condition=.false.
               if ((i1.eq.0).and.(i2.ne.0)) then
                  condition=(charge3(i2)-charge3(i5))
     $                 .eq.(sign(3,idvecbos))    
               endif
               if ((i2.eq.0).and.(i1.ne.0)) then
                  condition=(charge3(i1)-charge3(i5))
     $                 .eq.(sign(3,idvecbos))
               endif
               if (i5.eq.0) then
                condition=(charge3(i1)+charge3(i2))
     $                 .eq.(sign(3,idvecbos))
               endif   
               if(condition) then
                  flst_nborn=flst_nborn+1
                  if(flst_nborn.gt.maxprocborn) goto 998
                  do k=1,nlegborn
                     flst_born(k,flst_nborn)=ii(k)
                  enddo
               endif
            enddo
 111         continue
         enddo
      enddo
      if (debug) then
         write(*,*) ' born processes',flst_nborn
         do j=1,flst_nborn
            write(*,*) (flst_born(k,j),k=1,nlegborn)
         enddo
      endif

*********************************************************************
***********            REAL SUBPROCESSES              ***************
*********************************************************************
      flst_nreal=0

      do i1=-5,5
         do i2=-5,5
            do i5=-5,5
               do i6=-5,5
                  condition=(charge3(i1)+charge3(i2)-charge3(i5)
     $                 -charge3(i6)).eq.(sign(3,idvecbos))  
                  if (condition) then
                     if(i1*i2*i5*i6.ne.0) then
c     4 quarks
                        if (((charge3(i1)
     $                       +charge3(i2)).eq.(sign(3,idvecbos)))
     $                       .and.(i5+i6.eq.0)) then
c     q aqp annihilation
                           goto 10
                         elseif (((charge3(i5)
     $                       +charge3(i6)).eq.(-sign(3,idvecbos)))
     $                       .and.(i1+i2.eq.0)) then
c     q aq annihilation
                           goto 10

                        elseif (((charge3(i1)
     $                          -charge3(i5)).eq.(sign(3,idvecbos)))
     $                          .and.(i2.eq.i6)) then
c     W from upper line     
                           goto 10
                        elseif (((charge3(i2)
     $                          -charge3(i5)).eq.(sign(3,idvecbos)))
     $                          .and.(i1.eq.i6)) then
c     W from lower line 
                           goto 10
                        endif

                     elseif((i1*i2.ne.0).and.(i5.eq.0).and.(i6.eq.0))
     $                       then
c     2 quarks 2 gluons    
                        goto 10
                     elseif((i1*i2.eq.0).and.(i1+i2.eq.0).and.(i5*i6.ne
     $                       .0))then
c     2 gluons 2 quarks
                       goto 10
                    elseif((i1*i2.eq.0).and.(i5*i6.eq.0)) then
c     1 gluons 1 quarks
                       goto 10
                    endif
                  endif
                  goto 11     
 10               continue
                  do j=1,flst_nreal
c     Check that an inequivalent configuration is generated
                     if(flavequiv(nlegreal,flst_real(1,j),ii(1)))
     $                    goto 11
                  enddo
                  flst_nreal=flst_nreal+1
                  if(flst_nreal.gt.maxprocreal) goto 999
                  do k=1,6
                     flst_real(k,flst_nreal)=ii(k)
                  enddo
 11               continue
               enddo
            enddo
         enddo
      enddo
      if (debug) then
            write(*,*) ' real processes',flst_nreal
            do j=1,flst_nreal
               write(*,*) (flst_real(k,j),k=1,nlegreal)
            enddo
         endif
      return
 998  write(*,*) 'init_processes: increase maxprocborn'
 999  write(*,*) 'init_processes: increase maxprocreal'
      stop
      end
 


