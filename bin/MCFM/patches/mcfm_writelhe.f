c--- Routine to write a generic LHE file for a given MCFM process
      subroutine mcfm_writelhe(pin,xmsq,xfac)
      implicit none
      include 'constants.f'
      include 'plabel.f'
      include 'eventbuffer.f'
      include 'facscale.f'
      include 'qcdcouple.f'
      include 'maxwt.f' 
      include 'montecarlorpp.f'
      include 'hepeup.f'
      include 'process.f'
      include 'heprup.f'
      logical first
      integer ilomomenta,plabeltoPDG,ip_parent(10),id_parent(10),ic,
     & i,j,k,jj,kk,j1,j2,j3,j4,ip,nqcdjets,nqcdstart,nproc,nu,ilen
      double precision p(mxpart,4),xfac,p_parent(10,4),pin(mxpart,4),
     & xmsq(-nf:nf,-nf:nf),xx,mm
      integer icolqi(2),icolai(2),icolgi(2),
     &        icolqf(2),icolaf(2),icolgf(2)

      character*255 runname,outputstring
      common/ilomomenta/ilomomenta      
      common/nqcdjets/nqcdjets,nqcdstart
      common/nproc/nproc
      common/runname/runname
      data first /.true./
      save first 
      data icolqi/ 501, 0   /
      data icolai/ 0  , 502 /
      data icolgi/ 502, 501 /
      data icolqf/ 502, 0   /
      data icolaf/ 0  , 501 /
      data icolgf/ 501, 502 /
      save icolqi,icolai,icolgi,icolqf,icolaf,icolgf
      double precision r1,ran2
c--- work out flavour to use for initial state
c--- (randomly, based on weights passed in xmsq)
      call mcfm_getflavour(xmsq,jj,kk)
      if (jj .eq. 0) then
        idup(1)=21
      else
        idup(1)=jj
      endif
      if (kk .eq. 0) then
        idup(2)=21
      else
        idup(2)=kk
      endif
      istup(1)=-1
      istup(2)=-1

c--- fill simple particle labels for final state
      if(nqcdjets.eq.0) then
         do i=3,ilomomenta
            idup(i)=plabeltoPDG(plabel(i))
            istup(i)=1    
         enddo
      elseif(nqcdjets.eq.1) then 
         if(case.ne.'dm_jet') then 
            write(6,*) 'Check idup assignment in mcfm_writelhe.f' 
            stop
         endif
         do i=3,4
            idup(i)=plabeltoPDG(plabel(i))
            istup(i)=1    
         enddo
         if(jj*kk.ne.0) then 
!===== final state gluon
            idup(5)=21
         else
!===== one of initial state is zero 
            if((jj.ne.0).and.(kk.eq.0)) then 
               idup(5)=jj
            else
               idup(5)=kk 
            endif
         endif
         if(idup(5).eq.0) idup(5)=21
      endif


c--- copy array pin to new array p
      p(:,:)=pin(:,:)

c--- get parent combinations: at the end of this loop there will be ip of them
      call getparents(ip_parent,id_parent) 
      ip=1
      do while (ip_parent(ip) .gt. 0)
        j=ip_parent(ip)
        idup(ilomomenta+ip)=id_parent(ip)
        if     (j .lt. 100) then
          j1=j/10
          j2=mod(j,10)
          if (j2 .eq. 0) j2=10   ! special code: 0 -> 10
          mothup(1,ilomomenta+ip)=1
          mothup(2,ilomomenta+ip)=2
          istup(ilomomenta+ip)=+2
          mothup(:,j1)=ilomomenta+ip
          mothup(:,j2)=ilomomenta+ip
          call fixmasses(p,id_parent(ip),idup(j1),idup(j2),j1,j2)
          p_parent(ip,:)=p(j1,:)+p(j2,:)
        elseif (j .lt. 1000) then
c--- three-particle plots
          j2=(j-j1*100)/10
          j3=mod(j,10)
          if (j3 .eq. 0) j3=10   ! special code: 0 -> 10
          write(6,*) 'Unfinished mcfm_writelhe: j=',j
          stop
        elseif (j .lt. 10000) then
          j1=j/1000
          j2=(j-j1*1000)/100
          j3=(j-j1*1000-j2*100)/10
          j4=mod(j,10)
          if (j4 .eq. 0) j4=10   ! special code: 0 -> 10
          write(6,*) 'Unfinished mcfm_writelhe: j=',j
          stop
        else
          write(6,*) 'Error in ip_parent: ',j
          stop
        endif
        ip=ip+1
      enddo
      ip=ip-1 ! over-counted
c--- now handle color
      if (nqcdjets .eq. 0) then
c---  for no jets in the final state, straightforward
        if     (jj .gt. 0) then
           icolup(1,1)=501
           icolup(2,1)=0
           icolup(1,2)=0
           icolup(2,2)=501
        elseif (jj .lt. 0) then
           icolup(1,1)=0
           icolup(2,1)=501
           icolup(1,2)=501
           icolup(2,2)=0
        else
           icolup(1,1)=501
           icolup(2,1)=502
           icolup(1,2)=502
           icolup(2,2)=501
        endif
      elseif(nqcdjets.eq.1) then 
         if(case.ne.'dm_jet') then 
            write(6,*) case,'not implemented yet in mcfm_writelhe.f'
            stop
         endif
!====== q g => qb
         if((jj.gt.0).and.(kk.eq.0)) then 
            icolup(1,1)=icolqi(1)
            icolup(2,1)=icolqi(2)
            icolup(1,2)=icolgi(1)
            icolup(2,2)=icolgi(2)
            icolup(1,5)=icolqf(1)
            icolup(2,5)=icolqf(2)
         elseif((jj.lt.0).and.(kk.eq.0)) then
!======qb g  => qb
            icolup(1,1)=icolai(1)
            icolup(2,1)=icolai(2)
            icolup(1,2)=icolgi(1)
            icolup(2,2)=icolgi(2)
            icolup(1,5)=icolaf(1)
            icolup(2,5)=icolaf(2)
         elseif((jj.eq.0).and.(kk.gt.0)) then
!=======g q  => q
            icolup(1,1)=icolgi(1)
            icolup(2,1)=icolgi(2)
            icolup(1,2)=icolqi(1)
            icolup(2,2)=icolqi(2)
            icolup(1,5)=icolqf(1)
            icolup(2,5)=icolqf(2)
!======= g qb => qb
         elseif((jj.eq.0).and.(kk.lt.0)) then
            icolup(1,1)=icolgi(1)
            icolup(2,1)=icolgi(2)
            icolup(1,2)=icolai(1)
            icolup(2,2)=icolai(2)
            icolup(1,5)=icolaf(1)
            icolup(2,5)=icolaf(2)
!======= g g  => g special 
        elseif((jj.eq.0).and.(kk.eq.0)) then

!===== have two options with equal weight in Nc=> inf limit so 
!===== assign randomly 
           r1=ran2() 
           if(r1.lt.0.5d0) then 
            icolup(1,1)=502
            icolup(2,1)=501
            icolup(1,2)=501
            icolup(2,2)=511
            icolup(1,5)=502
            icolup(2,5)=511
         else
            icolup(1,1)=502
            icolup(2,1)=501
            icolup(1,2)=511
            icolup(2,2)=502
            icolup(1,5)=511
            icolup(2,5)=501
         endif
       elseif((jj.lt.0).and.(kk.gt.0)) then
!====== q qb => g 
            icolup(1,1)=icolai(1)
            icolup(2,1)=icolai(2)
            icolup(1,2)=icolqi(1)
            icolup(2,2)=icolqi(2)
            icolup(1,5)=icolgf(1)
            icolup(2,5)=icolgf(2)
       elseif((jj.gt.0).and.(kk.lt.0)) then
!====== q qb => g 
            icolup(1,1)=icolqi(1)
            icolup(2,1)=icolqi(2)
            icolup(1,2)=icolai(1)
            icolup(2,2)=icolai(2)
            icolup(1,5)=icolgf(1)
            icolup(2,5)=icolgf(2)
         endif
         mothup(1,5)=1
         mothup(2,5)=2
         istup(5)=1
        
         
      else
        write(6,*) 'LHE output not yet available for nqcdjets=',nqcdjets
        stop
      endif

c--- number of entries to write
      nup=ilomomenta+ip
      
c--- fill momenta
      do i=1,nup
        do nu=1,4
          if     (i .le. 2) then
            xx=-p(i,nu)
          elseif (i .le. ilomomenta) then
            xx=p(i,nu)
          else
            xx=p_parent(i-ilomomenta,nu)
          endif
          pup(nu,i)=xx        
        enddo
        if     (i .le. 2) then
          mm=0d0
        elseif (i .le. ilomomenta) then
          mm=sqrt(max(0d0,p(i,4)**2-p(i,1)**2-p(i,2)**2-p(i,3)**2))
          if (mm .lt. 1d-12) mm=0d0
        else
          mm=sqrt(max(0d0,
     &     +p_parent(i-ilomomenta,4)**2-p_parent(i-ilomomenta,1)**2
     &     -p_parent(i-ilomomenta,2)**2-p_parent(i-ilomomenta,3)**2))
        endif
        pup(5,i)=mm
      enddo

c--- Event weight must be multiplied by xfac to account for
c--- events with negative weight or events with weights that exceed wtmax
      xwgtup=xfac
      xmaxup(1)=wtmax

c--- Miscellaneous info
      idprup=10000+nproc
      scalup=facscale
      aqedup=-1d0
      aqcdup=as

c--- junk entries          
      vtimup(:)=0d0
      spinup(:)=9d0

c--- on the first pass, open unit 84 with the right file name and write header
      if(first) then 
         first=.false. 
         ilen=len_trim(runname) 
         outputstring=runname(1:ilen)//'.lhe'
         open(unit=84,file=outputstring,status='unknown')
         call init_lhe_events(84)
      endif

c--- increment event counter and write out entry to unit 84
      numstored=numstored+1
      call lhefwritev(84)
      
      return
      end
      

c--- Routine to specify parent particles
      subroutine getparents(ip_parent,id_parent)
      implicit none
      include 'nwz.f' 
      include 'process.f' 
      include 'montecarlorpp.f'
      integer j,ip_parent(10),id_parent(10)
      
      ip_parent(:)=0
      
      if ( (case.eq.'HZZ_4l') .or. (case.eq.'HZZ_tb')
     & .or.(case.eq.'HZZint') .or. (case.eq.'ggZZ4l')
     & .or.(case.eq.'HZZH+i') .or. (case.eq.'ZZlept')
     & .or.(case.eq.'ggZZbx')) then 
        ip_parent= (/ 34, 56, (0,j=1,8) /)
        id_parent= (/ z0_pdg, z0_pdg, (0,j=1,8) /)
      endif
      
      if ( (case.eq.'HWW_4l') .or. (case.eq.'HWW_tb')
     & .or.(case.eq.'HWWint') .or. (case.eq.'ggWW4l')
     & .or.(case.eq.'HWWH+i') .or. (case.eq.'WWqqbr')
     & .or.(case.eq.'ggWWbx')) then 
        ip_parent= (/ 34, 56, (0,j=1,8) /)
        id_parent= (/ wp_pdg, wm_pdg, (0,j=1,8) /)
      endif
      
      if (case.eq.'W_only') then 
        ip_parent= (/ 34, (0,j=1,9) /)
        id_parent= (/ wp_pdg*nwz, (0,j=1,9) /)
      endif
      
      if (case.eq.'Z_only') then 
        ip_parent= (/ 34, (0,j=1,9) /)
        id_parent= (/ z0_pdg, (0,j=1,9) /)
      endif
      
      if (case.eq.'ggfus0') then 
        ip_parent= (/ 34, (0,j=1,9) /)
        id_parent= (/ h_pdg, (0,j=1,9) /)
      endif
      
      if ( (case.eq.'WWqqbr') .or. (case.eq.'WWnpol')
     & .or.(case.eq.'WWqqdk')) then 
        ip_parent= (/ 34, 56, (0,j=1,8) /)
        id_parent= (/ wp_pdg, wm_pdg, (0,j=1,8) /)
      endif
      
      if (case.eq.'WZbbar') then 
        ip_parent= (/ 34, 56, (0,j=1,8) /)
        id_parent= (/ wp_pdg*nwz, z0_pdg, (0,j=1,8) /)
      endif
      
      if(case.eq.'dm_jet') then 
         ip_parent = (/ 34 , (0,j=1,9) /)
         id_parent= (/ zprime_pdg, (0,j=1,9) /)
      endif
      return
      end
      

c--- Routine to transform massless momenta to massive momenta,
c--- satisfying constraint that parent particle momentum is unchanged
c--- This is implemented by using "Rodrigo" transformation
c---
c---  p -> momentum array that will be changed
c---  idparent -> PDG code of parent particle
c---  id1,id2 -> PDG codes of daughter particles
c---  i1,id2 -> PDG codes of daughter particles
c---  j1,j2 -> entries in momentum array for daughters
      subroutine fixmasses(p,idparent,id1,id2,j1,j2)
      implicit none
      include 'constants.f'
      include 'montecarlorpp.f'
      include 'masses.f'
      double precision p(mxpart,4),dot,m,beta,p1(4),p2(4)
      integer idparent,id1,id2,j1,j2,imass
      
      if     (idparent .eq. z0_pdg) then
c--- Z parent
        if     ((abs(id1).eq.el_pdg) .and. (abs(id2).eq.el_pdg)) then
          m=mel   ! electrons
        elseif ((abs(id1).eq.ml_pdg) .and. (abs(id2).eq.ml_pdg)) then
          m=mmu   ! muons
        elseif ((abs(id1).eq.tl_pdg) .and. (abs(id2).eq.tl_pdg)) then
          m=mtau  ! taus
        elseif ((abs(id1).eq.bq_pdg) .and. (abs(id2).eq.bq_pdg)) then
          m=mb    ! b-quarks
        elseif ((abs(id1).eq.nel_pdg) .and. (abs(id2).eq.nel_pdg))then
          return  ! massless neutrinos -> nothing to do
        elseif ((abs(id1).eq.nml_pdg) .and. (abs(id2).eq.nml_pdg))then
          return  ! massless neutrinos -> nothing to do
        elseif ((abs(id1).eq.ntl_pdg) .and. (abs(id2).eq.ntl_pdg))then
          return  ! massless neutrinos -> nothing to do
        else
          write(6,*) 'Unexpected Z decay products: id1,id2=',id1,id2
          stop
        endif
        beta=sqrt(max(0d0,1d0-4d0*m**2/(2d0*dot(p,j1,j2))))
        p1(:)=(1d0+beta)/2d0*p(j1,:)+(1d0-beta)/2d0*p(j2,:)
        p2(:)=(1d0+beta)/2d0*p(j2,:)+(1d0-beta)/2d0*p(j1,:)
        p(j1,:)=p1(:)
        p(j2,:)=p2(:)
      elseif (abs(idparent) .eq. wp_pdg) then
c--- W parent
        if     ((abs(id1).eq.el_pdg) .and. (abs(id2).eq.nel_pdg)) then
          m=mel   ! electron
          imass=1
        elseif ((abs(id1).eq.ml_pdg) .and. (abs(id2).eq.nml_pdg)) then
          m=mmu   ! muon
          imass=1
        elseif ((abs(id1).eq.tl_pdg) .and. (abs(id2).eq.ntl_pdg)) then
          m=mtau  ! tau
          imass=1
        elseif ((abs(id1).eq.nel_pdg) .and. (abs(id2).eq.el_pdg)) then
          m=mel   ! electron
          imass=2
        elseif ((abs(id1).eq.nml_pdg) .and. (abs(id2).eq.ml_pdg)) then
          m=mmu   ! muon
          imass=2
        elseif ((abs(id1).eq.ntl_pdg) .and. (abs(id2).eq.tl_pdg)) then
          m=mtau  ! tau
          imass=2
        else
          write(6,*) 'Unexpected W decay products: id1,id2=',id1,id2
          stop
        endif
        beta=m**2/(2d0*dot(p,j1,j2))
        if (imass .eq. 1) then ! massive particle is j1
          p2(:)=(1d0-beta)*p(j2,:)
          p1(:)=p(j1,:)+beta*p(j2,:)
        endif
        if (imass .eq. 2) then ! massive particle is j2
          p1(:)=(1d0-beta)*p(j1,:)
          p2(:)=p(j2,:)+beta*p(j1,:)
        endif
          p(j1,:)=p1(:)
          p(j2,:)=p2(:)
      elseif     (idparent .eq. zprime_pdg) then
c--- Z' parent (DM mediagor) (nothing to do since mass is present in ME and phase space) 
         return
      else
        write(6,*) 'Unexpected parent in fixmasses: idparent=',idparent
        stop
      endif
      
      return
      end
      

c--- Routine to convert MCFM plabel to PDG number
      integer function plabeltoPDG(ch)
      implicit none
      include 'montecarlorpp.f'
      character*2 ch
      
      if     (ch .eq. 'el') then
        plabeltoPDG=el_pdg
      elseif (ch .eq. 'ea') then
        plabeltoPDG=ea_pdg
      elseif (ch .eq. 'ml') then
        plabeltoPDG=ml_pdg
      elseif (ch .eq. 'ma') then
        plabeltoPDG=ma_pdg
      elseif (ch .eq. 'tl') then
        plabeltoPDG=tl_pdg
      elseif (ch .eq. 'ta') then
        plabeltoPDG=ta_pdg
      elseif (ch .eq. 'bq') then
        plabeltoPDG=bq_pdg
      elseif (ch .eq. 'ba') then
        plabeltoPDG=bb_pdg
      elseif (ch .eq. 'nl') then
        plabeltoPDG=nel_pdg
      elseif (ch .eq. 'na') then
        plabeltoPDG=nea_pdg
      elseif (ch .eq. 'nm') then
        plabeltoPDG=nml_pdg
      elseif (ch .eq. 'bm') then
        plabeltoPDG=nma_pdg
      elseif (ch .eq. 'nt') then
        plabeltoPDG=ntl_pdg
      elseif (ch .eq. 'bt') then
        plabeltoPDG=nta_pdg
      elseif (ch .eq. 'pp') then
        plabeltoPDG=g_pdg
      elseif (ch .eq. 'pp') then
        plabeltoPDG=g_pdg
      elseif (ch .eq. 'xm') then
        plabeltoPDG=xm_pdg
      elseif (ch .eq. 'xa') then
        plabeltoPDG=xa_pdg

      else
        write(6,*) 'Unknown plabel in plabeltoPDG: ',ch
        stop
      endif

      return
      end
      

      subroutine mcfm_getflavour(msq,j,k)
      implicit none
      include 'constants.f'
      integer j,k
      double precision msq(-nf:nf,-nf:nf),msqsum,ran2,ptr

c--- total of absolute weights      
      msqsum=0d0
      do j=-nf,nf
      do k=-nf,nf
        msqsum=msqsum+abs(msq(j,k))
      enddo
      enddo
      
c--- random weight between 0 and this total
      ptr=msqsum*ran2()

c--- recover corresponding j,k      
      msqsum=0d0
      do j=-nf,nf
      do k=-nf,nf
        msqsum=msqsum+abs(msq(j,k))
        if (msqsum .ge. ptr) return
      enddo
      enddo

      return
      end
      
      
