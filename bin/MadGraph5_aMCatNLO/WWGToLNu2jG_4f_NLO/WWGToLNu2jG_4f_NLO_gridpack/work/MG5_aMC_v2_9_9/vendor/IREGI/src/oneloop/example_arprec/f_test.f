      subroutine f_main
!  ********************************************************************
!  ********************************************************************
      use mpmodule
      use avh_olo_forIREGI
      implicit none
      integer :: nn
      character(4) :: next
      real(kind(1d0)) :: mu
!
!      call olo_onshell( 1d-6 )
!      call olo_unit( 6 ,'printall' )
      call mpinit(34)
      call olo_precision(34)
      call olo_setting
!
      next = 'next'
      do while (next.eq.'next')
        read(5,*) nn ,mu
        call olo_scale( mu )
        if     (nn.eq.2) then
          call test_2
        elseif (nn.eq.3) then
          call test_3
        else!if(nn.eq.4) then
          call test_4
        endif
        read(5,*) next
      enddo
      end subroutine


      subroutine test_4
!  ********************************************************************
!  ********************************************************************
      use mpmodule
      use avh_olo_forIREGI
      implicit none
      complex(kind(1d0)) :: p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,zdp(0:2)
      type(mp_complex) :: p1_mp,p2_mp,p3_mp,p4_mp,p12_mp,p23_mp
      type(mp_complex) :: m1_mp,m2_mp,m3_mp,m4_mp,zmp(0:2)
!
      read(5,*) p1
      read(5,*) p2
      read(5,*) p3
      read(5,*) p4
      read(5,*) p12
      read(5,*) p23
      read(5,*) m1
      read(5,*) m2
      read(5,*) m3
      read(5,*) m4
      write(6,101) 'p1 : (',real(p1),',',aimag(p1),')'
      write(6,101) 'p2 : (',real(p2),',',aimag(p2),')'
      write(6,101) 'p3 : (',real(p3),',',aimag(p3),')'
      write(6,101) 'p4 : (',real(p4),',',aimag(p4),')'
      write(6,101) 'p12: (',real(p12),',',aimag(p12),')'
      write(6,101) 'p23: (',real(p23),',',aimag(p23),')'
      write(6,101) 'm1 : (',real(m1),',',aimag(m1),')'
      write(6,101) 'm2 : (',real(m2),',',aimag(m2),')'
      write(6,101) 'm3 : (',real(m3),',',aimag(m3),')'
      write(6,101) 'm4 : (',real(m4),',',aimag(m4),')'
  101 format(a6,d31.24,a1,d31.24,a1)
!
      p1_mp =p1
      p2_mp =p2
      p3_mp =p3
      p4_mp =p4
      p12_mp=p12
      p23_mp=p23
      m1_mp =m1
      m2_mp =m2
      m3_mp =m3
      m4_mp =m4
      call olo( zdp ,p1,p2,p3,p4,p12,p23  ,m1,m2,m3,m4 )
      call olo( zmp ,p1_mp,p2_mp,p3_mp,p4_mp,p12_mp,p23_mp &
                    ,m1_mp,m2_mp,m3_mp,m4_mp )
!
      write(6,'(a10)') ' eps^( 0):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(0)
      call mpwrite(6,zmp(0))
      write(6,'(a10)') ' eps^(-1):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(1)
      call mpwrite(6,zmp(1))
      write(6,'(a10)') ' eps^(-2):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(2)
      call mpwrite(6,zmp(2))
      write(6,*)
!
      end subroutine
   
      subroutine test_3
!  ********************************************************************
!  ********************************************************************
      use mpmodule
      use avh_olo_forIREGI
      implicit none
      complex(kind(1d0)) :: p1,p2,p3,m1,m2,m3,zdp(0:2)
      type(mp_complex) :: p1_mp,p2_mp,p3_mp
      type(mp_complex) :: m1_mp,m2_mp,m3_mp,zmp(0:2)
!
      read(5,*) p1
      read(5,*) p2
      read(5,*) p3
      read(5,*) m1
      read(5,*) m2
      read(5,*) m3
      write(6,101) 'p1 : (',real(p1),',',aimag(p1),')'
      write(6,101) 'p2 : (',real(p2),',',aimag(p2),')'
      write(6,101) 'p3 : (',real(p3),',',aimag(p3),')'
      write(6,101) 'm1 : (',real(m1),',',aimag(m1),')'
      write(6,101) 'm2 : (',real(m2),',',aimag(m2),')'
      write(6,101) 'm3 : (',real(m3),',',aimag(m3),')'
  101 format(a6,d31.24,a1,d31.24,a1)
!
      p1_mp=p1
      p2_mp=p2
      p3_mp=p3
      m1_mp=m1
      m2_mp=m2
      m3_mp=m3
      call olo( zdp ,p1,p2,p3 ,m1,m2,m3 )
      call olo( zmp ,p1_mp,p2_mp,p3_mp ,m1_mp,m2_mp,m3_mp )
!
      write(6,'(a10)') ' eps^( 0):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(0)
      call mpwrite(6,zmp(0))
      write(6,'(a10)') ' eps^(-1):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(1)
      call mpwrite(6,zmp(1))
      write(6,'(a10)') ' eps^(-2):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(2)
      call mpwrite(6,zmp(2))
      write(6,*)
!
      end subroutine

      subroutine test_2
!  ********************************************************************
!  ********************************************************************
      use mpmodule
      use avh_olo_forIREGI
      implicit none
      complex(kind(1d0)) :: p1,m1,m2,zdp(0:2)
      type(mp_complex) :: m1_mp,m2_mp,p1_mp,zmp(0:2)
!
      read(5,*) p1
      read(5,*) m1
      read(5,*) m2
      write(6,101) 'p1 : (',real(p1),',',aimag(p1),')'
      write(6,101) 'm1 : (',real(m1),',',aimag(m1),')'
      write(6,101) 'm2 : (',real(m2),',',aimag(m2),')'
  101 format(a6,d31.24,a1,d31.24,a1)
!
      p1_mp = p1
      m1_mp = m1
      m2_mp = m2
      call olo( zdp ,p1 ,m1,m2 )
      call olo( zmp ,p1_mp ,m1_mp,m2_mp )
!
      write(6,'(a10)') ' eps^( 0):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(0)
      call mpwrite(6,zmp(0))
      write(6,'(a10)') ' eps^(-1):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(1)
      call mpwrite(6,zmp(1))
      write(6,'(a10)') ' eps^(-2):'
      write(6,'(a8,2d24.16)') '    olo:',zdp(2)
      call mpwrite(6,zmp(2))
      write(6,*)
!
      end subroutine
