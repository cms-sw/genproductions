      subroutine transform_os_spect(p,q,ip,jp,kp,mass_i,mass_j,mass_k,mass_ij,stat)
************************************************************************
*     Authors: Dorival Goncalves & Marco Zaro                           *
*     Given momenta p(nu,nexternal) produce q(nu,external) with the    *
*     momentum qij on shell. The reshuffling is done as in the         *                             
*     final-final CS dipole subtraction. kp is the spectator, ip and   *
*     jp are the the decays products from the possible OS resonance    *
************************************************************************
      implicit none 
      include 'nexternal.inc'
      include 'coupl.inc'
C-----Arguments      
      double precision p(0:3,nexternal),q(0:3,nexternal)
      double precision mass_i,mass_j,mass_k,mass_ij
      integer ip,jp,kp
      integer stat
C-----Local
      integer i,j,nu
      double precision qtot(0:3),qsq,pij(0:3),qij(0:3),qz(0:3),dot,threedot
      double precision lambda_tr,msq_ij,msq_k,msq_i,msq_j,a,b,c,d
      external dot,threedot,lambda_tr
      double precision zero, one, two
      parameter (zero=0d0,one=1d0,two=2d0)
 
C---------------                                                                                   
C     BEGIN CODE                                                                    
C---------------    
      stat=0 ! everything is fine for the moment

      msq_i=mass_i*mass_i
      msq_k=mass_k*mass_k
      msq_j=mass_j*mass_j
      msq_ij=mass_ij*mass_ij

      do nu=0,3
         qtot(nu)=p(nu,ip)+p(nu,jp)+p(nu,kp)
         pij(nu)=p(nu,ip)+p(nu,jp)
      enddo
      qsq=dot(qtot,qtot)

C----- check that the reshuffling can be done; otherwise return stat=1
      if (dsqrt(qsq).lt.mass_ij+mass_k) then
        stat=1
        return
      endif
      
      do nu=0,3
C-----spectator kp            
         q(nu,kp)=dsqrt(lambda_tr(qsq,msq_ij,msq_k))/
     &        dsqrt(lambda_tr(qsq,dot(pij,pij),msq_k))
     &        *(p(nu,kp)-dot(qtot,p(0,kp))/qsq*qtot(nu))
     &        +(qsq+msq_k-msq_ij)/two/qsq*qtot(nu)
         
C-----OS particle
         qij(nu)=qtot(nu)-q(nu,kp)
      enddo
      
C-  i'(or j')  momemntum: chose the one which has mass=0
C 2 i' dot qij is fixed
C assume i' direction is the same as i one
      if (mass_i.eq.0) then
        a = (msq_ij -msq_j) / 2d0 / 
     $   (qij(0) * dsqrt(threedot(p(0,ip),p(0,ip))) -
     $    threedot(p(0,ip),qij))
        do nu = 1, 3
          q(nu,ip) = a * p(nu,ip)
        enddo
        q(0,ip) = dsqrt(threedot(q(0,ip), q(0,ip)))
        do nu = 0, 3
          q(nu,jp) = qij(nu) - q(nu,ip)
        enddo
      else if (mass_j.eq.0) then
        a = (msq_ij -msq_i) / 2d0 / 
     $   (qij(0) * dsqrt(threedot(p(0,jp),p(0,jp))) -
     $    threedot(p(0,jp),qij))
        do nu = 1, 3
          q(nu,jp) = a * p(nu,jp)
        enddo
        q(0,jp) = dsqrt(threedot(q(0,jp), q(0,jp)))
        do nu = 0, 3
          q(nu,ip) = qij(nu) - q(nu,jp)
        enddo
      else 
        write(*,*) 'ERROR IN TRANSFORM_OS_SPECT: I and J both massive', mass_i, mass_j
        stop
      endif

      do nu=0,3
C-----Keeping the rest of the momenta without any changes
         do j=1,nexternal
            if(j.ne.kp .and. j.ne.ip .and. j.ne.jp)then
               q(nu,j)=p(nu,j)
            endif
         enddo
      enddo
      
C-----Consistency checks
C--------mass shell condition for spectator
      if (dabs(dot(q(0,kp),q(0,kp))-dot(p(0,kp),p(0,kp)))
     $ .gt. 1d-4 * max(dot(p(0,kp),p(0,kp)), 1d0)) then
        write(*,*) 'ERROR IN TRANSFORM_OS_SPECT: K NOT KEPT ON SHELL'
        write(*,*) 'MSQ before', dot(p(0,kp),p(0,kp))
        write(*,*) 'MSQ after ', dot(q(0,kp),q(0,kp))
        stop
      endif

      ! check the momenta before returning
      call OS_check_momenta(p, q, ip, jp)

      return     
      end
          

      REAL*8 function lambda_tr(x,y,z)
C-----triangular function
      implicit none
      real*8 x,y,z
      lambda_tr=x**2+y**2+z**2-2d0*x*y-2d0*x*z-2d0*y*z
      return
      end


      double precision function lambda2(a,b,c)
      implicit none
      double precision a,b,c
      if (a.le.0d0 .or. abs(b+c).gt.abs(a) .or. abs(b-c).gt.abs(a)) then
         write (*,*) 'Error #1 in lambda2: inputs not consistent',a,b,c
         stop 1
      endif
      lambda2=sqrt(1d0-(b+c)**2/a**2)*sqrt(1d0-(b-c)**2/a**2)
      return
      end



      subroutine transform_os_final(p,q,ip,jp,mass_i,mass_j,mass_ij,stat)
************************************************************************
*     Authors: Marco Zaro                                              *
*     Given momenta p(nu,nexternal) produce q(nu,external) with the    *
*     momentum qij on shell. The reshuffling is done by compensating   *
*     on all the other final-state particles.                          *
*     As in the case of transform_os_init, the angles in the           *
*     pi+pj rest frame are not changed                                 *
************************************************************************
      implicit none 
      include 'nexternal.inc'
C-----Arguments      
      double precision p(0:3,nexternal), q(0:3,nexternal)
      double precision mass_i, mass_j, mass_ij
      integer ip, jp
      integer stat 
C-----Local
      integer i, j, nu
      double precision qtot(0:3), qsq, pij(0:3), qij(0:3), qi(0:3), qj(0:3)
      double precision preco(0:3), qreco(0:3), ptmp(0:3)
      double precision pcom(0:3,nexternal), qcom(0:3,nexternal), pboost(0:3)
      double precision shat
      double precision etot, ztot
      double precision msq_ij, msq_k, msq_i, msq_j, msq_reco, resc
      double precision a, b
      double precision dot, threedot
      external dot, threedot
      double precision rescale_init
C--------------- 
C     BEGIN CODE                                                                   
C---------------    

      stat = 0
      msq_i=mass_i*mass_i
      msq_j=mass_j*mass_j
      msq_ij=mass_ij*mass_ij

C the center of mass energy
      shat = 2d0 * dot(p(0,1),p(0,2))

C boost the momenta in the partonic C.o.M. frame
      do nu=0,3
        pboost(nu) = p(nu,1) + p(nu,2)
      enddo
      do i=1,nexternal
        call invboostx(p(0,i), pboost, pcom(0,i))
      enddo

c reconstruct the recoil system (all FS particles which are not i and j)
      do nu=0,3
        preco(nu)=0d0
      enddo
      do i=nincoming+1,nexternal
        if (i.eq.jp.or.i.eq.ip) cycle
        do nu=0,3
          preco(nu)=preco(nu)+pcom(nu,i)
        enddo
      enddo

      msq_reco=dot(preco,preco)

c ----reconstruct the momentum pij
      do nu=0,3
        pij(nu)=pcom(nu,ip) + pcom(nu,jp)
      enddo

C check energy conditions
      if (abs(preco(0)-sqrt(shat)/2d0*(1d0-(dot(pij,pij)-msq_reco)/shat))/shat.gt.1d-4) then
          write(*,*) 'ERROR IN TRANSFORM_OS_FINAL: ENERGY #1'
          write(*,*) 'shat', shat, sqrt(shat)
          write(*,*) preco(0),sqrt(shat)/2d0*(1d0-(dot(pij,pij)-msq_reco)/shat)
          stop
      endif
      if (abs(pij(0)-sqrt(shat)/2d0*(1d0+(dot(pij,pij)-msq_reco)/shat))/shat.gt.1d-4) then
          write(*,*) 'ERROR IN TRANSFORM_OS_FINAL: ENERGY #2'
          write(*,*) pij(0),sqrt(shat)/2d0*(1d0-(dot(pij,pij)-msq_reco)/shat)
          stop
      endif

C check if the reshuffling can be done (ie if there is enough energy).
C otherwise, rescale shat and the initial momenta; 
      if (dsqrt(shat).lt.mass_ij+dsqrt(msq_reco)) then
        rescale_init=(dsqrt(msq_reco)+mass_ij)/(dsqrt(msq_reco)+dsqrt(dot(pij,pij)))
        do nu = 0, 3
          pcom(nu,1) = pcom(nu,1)*rescale_init
          pcom(nu,2) = pcom(nu,2)*rescale_init
        enddo
        shat=shat*rescale_init**2
      endif
C alternatively one can put the momenta on the largest invariant mass that can be
C generated (this is discarded for now)
CC      if (dsqrt(shat).lt.mass_ij+dsqrt(msq_reco)) then
CC          stat=1
CC          msq_ij = max(0.99d0 * (dsqrt(shat)-dsqrt(msq_reco))**2, (mass_i+mass_j)**2)
CC      endif

C the reshuffled momenta, qij and qreco, will have energy components
C wchic correspond to qij having the reshuffled mass and qreco keeping
C its invariant mass
      qij(0) = dsqrt(shat)/2d0*(1d0+(msq_ij-msq_reco)/shat)
      qreco(0) = dsqrt(shat)/2d0*(1d0-(msq_ij-msq_reco)/shat)
C the other components have the same direction as pij, preco, and must
C satisfy the mass-shell conditions qij^2 = m_ij^2, qreco^2=m_reco^2
      do nu=1,3
        qij(nu) = pij(nu)/dsqrt(threedot(pij,pij))*dsqrt(qij(0)*qij(0)-msq_ij)
        qreco(nu) = preco(nu)/dsqrt(threedot(preco,preco))*dsqrt(qreco(0)*qreco(0)-msq_reco)
      enddo

C now let us turn to the i and j particles and to the recoiling ones
C *** IJ

c now go to the pij rest frame
      call invboostx(pcom(0,ip), pij, qi)
      call invboostx(pcom(0,jp), pij, qj)

C check that momenta are back to back
      do nu=1,3
        if (dabs(qi(nu)+qj(nu))/(qi(0)+qj(0)).gt.1d-4) then
          write(*,*) 'ERROR IN TRANSFORM_OS_FINAL: QI QJ not B2B'
          write(*,*) 'QI', qi, dsqrt(dot(qi,qi)), mass_i
          write(*,*) 'QJ', qj, dsqrt(dot(qj,qj)), mass_j
          stop
        endif
      enddo

C putting qi, qj on shell amounts in rescaling their spatial 
c  component by a factor 
      resc = dsqrt(
     $ (((msq_ij - msq_i - msq_j)/2d0)**2 - msq_i * msq_j) / 
     $ (threedot(qi,qi) * msq_ij))
      do nu=1,3
        qi(nu) = qi(nu) * resc
        qj(nu) = qj(nu) * resc
      enddo
      qi(0) = dsqrt(msq_i + threedot(qi,qi))
      qj(0) = dsqrt(msq_j + threedot(qj,qj))

c check that the sum of the energies is mij (use a low tolerance here)
      if (dabs((dsqrt(msq_ij) - qi(0) - qj(0))/dsqrt(msq_ij)).gt.1d-2) then
        write(*,*) 'ERROR IN TRANSFORM_OS_FINAL: QI QJ not on shell'
        write(*,*) 'DIFF', dabs(dsqrt(msq_ij) - qi(0) - qj(0)), dabs(dsqrt(msq_ij) - qi(0) - qj(0))/dsqrt(msq_ij)
        write(*,*) 'MIJ', dsqrt(msq_ij), stat
        write(*,*) 'QI', qi, dsqrt(dot(qi,qi)), mass_i
        write(*,*) 'QJ', qj, dsqrt(dot(qj,qj)), mass_j
        stop
      endif

C     boost qi,qj in the lab frame with qij
      call boostx(qi, qij, qcom(0,ip))
      call boostx(qj, qij, qcom(0,jp))

C check that momenta sum to qij
      do nu=0,3
        if (dabs(qcom(nu,ip)+qcom(nu,jp)-qij(nu))/(qij(0)).gt.1d-3) then
          write(*,*) 'ERROR IN TRANSFORM_OS_FINAL: QIJ not conserved',nu
          write(*,*) 'QI', qcom(0,ip), qcom(1,ip), qcom(2,ip), qcom(3,ip), dsqrt(dot(qcom(0,ip),qcom(0,ip)))
          write(*,*) 'QJ', qcom(0,jp), qcom(1,jp), qcom(2,jp), qcom(3,jp), dsqrt(dot(qcom(0,jp),qcom(0,jp)))
          write(*,*) 'QIJ', qij, dsqrt(dot(qij,qij)), dsqrt(msq_ij), stat
          stop
        endif
      enddo

C *** other recoiling particles
C boost them to the preco rest frame and then back to
C the lab frame using qreco
      do i=1, nexternal
        if (i.eq.ip.or.i.eq.jp) cycle
        call invboostx(pcom(0,i), preco, ptmp)
        call boostx(ptmp, qreco, qcom(0,i))
      enddo

C *** finally the initial state particles, do nothing
      do i=1, nincoming
        do nu=0,3
          qcom(nu,i)=pcom(nu,i)
        enddo
      enddo

C *** boost the momenta back to the original frame
      do i=1, nexternal
        call boostx(qcom(0,i),pboost,q(0,i))
      enddo

      ! check the momenta before returning
      call OS_check_momenta(p, q, ip, jp)

      return     
      end


      subroutine transform_os_init(p,q,ip,jp,mass_i,mass_j,mass_ij)
************************************************************************
*     Authors: Marco Zaro                                              *
*     Given momenta p(nu,nexternal) produce q(nu,external) with the    *
*     momentum qij on shell. The reshuffling is done keeping the       * 
*     3 momentum of pi+pj fixed, and changing the energy (the initial  *
*     state momenta are also changed. Furthermore, the angles in the   *
*     pi+pj rest frame are not changed                                 *
************************************************************************
      implicit none 
      include 'nexternal.inc'
C-----Arguments      
      double precision p(0:3,nexternal), q(0:3,nexternal)
      double precision mass_i, mass_j, mass_ij
      integer ip, jp
C-----Local
      integer i, j, nu
      double precision qtot(0:3), qsq, pij(0:3), qij(0:3), qi(0:3), qj(0:3)
      double precision etot, ztot
      double precision msq_ij, msq_k, msq_i, msq_j, resc
      double precision a, b
      double precision dot, threedot
      external dot, threedot
 
C--------------- 
C     BEGIN CODE                                                                    
C---------------    
      msq_i=mass_i*mass_i
      msq_j=mass_j*mass_j
      msq_ij=mass_ij*mass_ij

c ----reconstruct the momentum pij
      do nu=0,3
        pij(nu)=p(nu,ip) + p(nu,jp)
      enddo

c and go to the pij rest frame
      call invboostx(p(0,ip), pij, qi)
      call invboostx(p(0,jp), pij, qj)

C check that momenta are back to back
      do nu=1,3
        if (dabs(qi(nu)+qj(nu))/(qi(0)+qj(0)).gt.1d-4) then
          write(*,*) 'ERROR IN TRANSFORM_OS_INIT: QI QJ not B2B'
          write(*,*) 'QI', qi, dsqrt(dot(qi,qi)), mass_i
          write(*,*) 'QJ', qj, dsqrt(dot(qj,qj)), mass_j
          stop
        endif
      enddo

C putting qi, qj on shell amounts in rescaling their spatial 
c  component by a factor 
      resc = dsqrt(
     $ (((msq_ij - msq_i - msq_j)/2d0)**2 - msq_i * msq_j) / 
     $ (threedot(qi,qi) * msq_ij))
      do nu=1,3
        qi(nu) = qi(nu) * resc
        qj(nu) = qj(nu) * resc
      enddo
      qi(0) = dsqrt(msq_i + threedot(qi,qi))
      qj(0) = dsqrt(msq_j + threedot(qj,qj))

c check that the sum of the energies is mij
      if (dabs((mass_ij - qi(0) - qj(0))/mass_ij).gt.1d-4) then
        write(*,*) 'ERROR IN TRANSFORM_OS_INIT: QI QJ not on shell'
        write(*,*) 'MIJ', mass_ij
        write(*,*) 'QI', qi, dsqrt(dot(qi,qi)), mass_i
        write(*,*) 'QJ', qj, dsqrt(dot(qj,qj)), mass_j
        stop
      endif
      
c ----qij has the same spatial components of pij
      do nu=1,3
        qij(nu) = pij(nu)
      enddo
      qij(0) = dsqrt(msq_ij + threedot(qij,qij))

C     boost qi,qj in the lab frame
      call boostx(qi, qij, q(0,ip))
      call boostx(qj, qij, q(0,jp))

C check that momenta sum to qij
      do nu=0,3
        if (dabs(q(nu,ip)+q(nu,jp)-qij(nu))/(qij(0)).gt.1d-4) then
          write(*,*) 'ERROR IN TRANSFORM_OS_INIT: QIJ not conserved'
          write(*,*) 'QI', q(0,ip), q(1,ip), q(2,ip), q(3,ip)
          write(*,*) 'QJ', q(0,jp), q(1,jp), q(2,jp), q(3,jp)
          write(*,*) 'QIJ', qij
          stop
        endif
      enddo

C-----Keep the rest of the FS momenta without any changes
      etot = 0d0
      ztot = 0d0
      do j=nincoming+1,nexternal
        do nu=0,3
          if(j.ne.ip .and. j.ne.jp)then
            q(nu,j) = p(nu,j)
          endif
        enddo
        etot = etot + q(0,j)
        ztot = ztot + q(3,j)
      enddo

C initial state momenta: one knows the sum and the difference of
C energies (sum of z components of FS momenta)
      q(0,1) = (etot + ztot)/2d0
      q(1,1) = 0d0
      q(2,1) = 0d0
      q(3,1) = dsign(q(0,1), p(3,1)) 

      q(0,2) = (etot - ztot)/2d0
      q(1,2) = 0d0
      q(2,2) = 0d0
      q(3,2) = dsign(q(0,2), p(3,2)) 

      ! check the momenta before returning
      call OS_check_momenta(p, q, ip, jp)

      return     
      end


      subroutine OS_check_momenta(p, q, ip, jp)
      ! performs some consistency checks on the momenta
      implicit none
      include 'nexternal.inc'
      double precision p(0:3,nexternal), q(0:3,nexternal)
      integer ip, jp
      double precision a, b
      integer i, j
      double precision dot

      if (nincoming.ne.2) then
        write(*,*) 'ERROR IN OS_CHECK_MOMENTA:, nincoming != 2 not'//
     $   ' implemented', nincoming
        stop
      endif

C--------mass shell conditions
      if (dabs(dot(q(0,ip),q(0,ip))-dot(p(0,ip),p(0,ip)))
     $ .gt. 1d-3 * max(dot(p(0,ip),p(0,ip)), 1d0)) then
        write(*,*) 'ERROR IN OS_CHECK_MOMENTA: I NOT KEPT ON SHELL'
        write(*,*) 'MSQ before', dot(p(0,ip),p(0,ip))
        write(*,*) 'MSQ after ', dot(q(0,ip),q(0,ip))
        stop
      endif
      if (dabs(dot(q(0,jp),q(0,jp))-dot(p(0,jp),p(0,jp)))
     $ .gt. 1d-3 * max(dot(p(0,jp),p(0,jp)), 1d0)) then
        write(*,*) 'ERROR IN OS_CHECK_MOMENTA: J NOT KEPT ON SHELL'
        write(*,*) 'MSQ before', dot(p(0,jp),p(0,jp))
        write(*,*) 'MSQ after ', dot(q(0,jp),q(0,jp))
        stop
      endif

C--------momentum conservation
      do i = 0,3
        a = 0d0
        b = 0d0
        do j = 1, nexternal
          b = max(b, dabs(q(i,j)))
          if (j.le.nincoming) then
            a = a - q(i,j)
          else
            a = a + q(i,j)
          endif
        enddo
        if (dabs(a)/b.gt.1d-6) then
          write(*,*) 'ERROR IN OS_CHECK_MOMENTA: MOMENTUM CONSERVATION',
     $      i, dabs(a), b
          do j = 1, nexternal
            write(*,*) q(0,j), q(1,j), q(2,j), q(3,j), dsqrt(dot(q(0,j), q(0,j)))
          enddo
          stop
        endif
      enddo

      return
      end


      subroutine transform_os_ident(p,q)
************************************************************************
*     Authors: Marco Zaro                                              *
*     No reshuffling is performed in this case                         *
************************************************************************
      implicit none 
      include 'nexternal.inc'
C-----Arguments      
      double precision p(0:3,nexternal), q(0:3,nexternal)

      integer i,j

      do j = 1, nexternal
        do i = 0, 3
          q(i,j) = p(i,j)
        enddo
      enddo 

      return
      end



      subroutine invboostx(p,q , pboost)
c
c This subroutine performs the Lorentz boost of a four-momentum.  The
c momenta p and q are assumed to be given in the same frame.pboost is
c the momentum p boosted to the q rest frame.  q must be a
c timelike momentum.
c it is the inverse of boostx
c
c input:
c       real    p(0:3)         : four-momentum p in the same frame as q
c       real    q(0:3)         : four-momentum q 
c
c output:
c       real    pboost(0:3)    : four-momentum p in the boosted frame
c
      implicit none
      double precision p(0:3),q(0:3),pboost(0:3),pq,qq,m,lf

      double precision rZero
      parameter( rZero = 0.0d0 )

c#ifdef HELAS_CHECK
c      integer stdo
c      parameter( stdo = 6 )
c      double precision pp
c#endif
c
      qq = q(1)**2+q(2)**2+q(3)**2

c#ifdef HELAS_CHECK
c      if (abs(p(0))+abs(p(1))+abs(p(2))+abs(p(3)).eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : p(0:3) in boostx is zero momentum'
c      endif
c      if (abs(q(0))+qq.eq.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx is zero momentum'
c      endif
c      if (p(0).le.rZero) then
c         write(stdo,*)
c     &        ' helas-warn  : p(0:3) in boostx has not positive energy'
c         write(stdo,*)
c     &        '             : p(0) = ',p(0)
c      endif
c      if (q(0).le.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx has not positive energy'
c         write(stdo,*)
c     &        '             : q(0) = ',q(0)
c      endif
c      pp=p(0)**2-p(1)**2-p(2)**2-p(3)**2
c      if (pp.lt.rZero) then
c         write(stdo,*)
c     &        ' helas-warn  : p(0:3) in boostx is spacelike'
c         write(stdo,*)
c     &        '             : p**2 = ',pp
c      endif
c      if (q(0)**2-qq.le.rZero) then
c         write(stdo,*)
c     &        ' helas-error : q(0:3) in boostx is not timelike'
c         write(stdo,*)
c     &        '             : q**2 = ',q(0)**2-qq
c      endif
c      if (qq.eq.rZero) then
c         write(stdo,*)
c     &   ' helas-warn  : q(0:3) in boostx has zero spacial components'
c      endif
c#endif

      if ( qq.ne.rZero ) then
         pq = p(1)*q(1)+p(2)*q(2)+p(3)*q(3)
         m = dsqrt(max(q(0)**2-qq,1d-99))
         lf = (-(q(0)-m)*pq/qq+p(0))/m
         pboost(0) = (p(0)*q(0)-pq)/m
         pboost(1) =  p(1)-q(1)*lf
         pboost(2) =  p(2)-q(2)*lf
         pboost(3) =  p(3)-q(3)*lf
      else
         pboost(0) = p(0)
         pboost(1) = p(1)
         pboost(2) = p(2)
         pboost(3) = p(3)
      endif
c
      return
      end


      subroutine get_pdf_flux_ratio(p, q, pdf_ratio, flux_ratio)
      ! p is off-shell, q is the reshuffled (on-shell) momentum
      implicit none
      include 'nexternal.inc'
      double precision p(0:3,nexternal), q(0:3,nexternal)
      double precision pdf_ratio, flux_ratio
      include 'run.inc' ! to acces the Bjorken x's
      double precision xbk_save(2)
      double precision tiny
      parameter (tiny=1e-6)
      double precision dot, dlum
      logical samemom
C this is to check if we are doing soft-coll tests
C In this case since PDFs are not initialised, just return 1
      logical softtest,colltest
      common/sctests/softtest,colltest

      pdf_ratio = 0d0
      flux_ratio = 0d0

C if initial momenta are identical, or, just return 1. for both ratios
C This is also the case when one does soft/collinear tests
      samemom = abs((p(0,1)-q(0,1))/(p(0,1)+q(0,1))).lt.tiny.and.
     $    abs((p(0,2)-q(0,2))/(p(0,2)+q(0,2))).lt.tiny
      if (samemom.or.colltest.or.softtest) then
        pdf_ratio = 1d0
        flux_ratio = 1d0
        return
      endif

c the xbk corresponding to the os kinematics are choosen in order
c to have the same ratio as the original ones
      xbk_save(1) = xbk(1)
      xbk_save(2) = xbk(2)

C check for the energy in the P_OS array
      if (q(0,1).gt.ebeam(1).or.q(0,2).gt.ebeam(2)) goto 999

c Reweight parton luminosities
      pdf_ratio = 1d0 / dlum()

      xbk(1) = dsqrt(dot(q(0,1), q(0,2)) / dot(p(0,1), p(0,2))) * xbk(1)
      xbk(2) = dsqrt(dot(q(0,1), q(0,2)) / dot(p(0,1), p(0,2))) * xbk(2)

C restore the bjorken x; in any case flux_ratio is zero, therefore 
C the whole on-shell subtraction term will be set to zero as well
      if (xbk(1).gt.1d0.or.xbk(2).gt.1d0) goto 999

      flux_ratio = (xbk_save(1)*xbk_save(2)) / (xbk(1)*xbk(2))
      pdf_ratio = pdf_ratio * dlum()

  999 continue
C finally, restore the bjorken X's to the original values
      xbk(1) = xbk_save(1)
      xbk(2) = xbk_save(2)
      return
      end

     
      subroutine get_bw_ratio(p, mom_mass, mom_wdth, idau1, idau2, ibw, bw_ratio) 
      ! compute the ratio of BW functions
      ! ibw==0, return 1.
      ! ibw==1, standard BW
      ! ibw==2, running BW
      implicit none
      include 'nexternal.inc'
      double precision p(0:3, nexternal)
      double precision mom_mass, mom_wdth
      integer idau1, idau2, ibw
      double precision bw_ratio

      double precision m2_reco
      double precision sumdot

      m2_reco = sumdot(p(0,idau1),p(0,idau2),1d0)

      if (ibw.eq.0) then
        ! nothing
        bw_ratio = 1d0
      elseif (ibw.eq.1) then
        ! standard BW
        bw_ratio = (mom_mass * mom_wdth)**2 / ((m2_reco - mom_mass**2)**2 + (mom_mass * mom_wdth)**2)
      elseif (ibw.eq.2) then
        ! running BW
        bw_ratio = m2_reco * mom_wdth**2 / ((m2_reco - mom_mass**2)**2 + m2_reco * mom_wdth**2)
      else
        write(*,*) 'ERROR in get_bw_ratio: not implemented', ibw
        stop
      endif

      return
      end



      subroutine OS_resonance_and_decay_reshuffle(npart,ibeta,decay_tree
     $     ,Mbeta,p_after,p_out)
c
c     Authors: Rikkert Frederix
c
cc Given the inputs
c   npart : number of particles before shower (this includes
c     resonances). The number of particles after shower should
c     be one greater than this.
c   ibeta : the label for the resonance which invariant mass needs to
c     be conserved. (Label corresponds to particle in p_after).
c   decay_tree : structure of 1->2 splittings for decays of the
c     resonance 'ibeta'. Arguments are
c       1st argument, 1:2, labels repsective daughter
c       2nd argument, -next:-1, mother particle.
c       3rd argument, 1:2. If 1=2, daughter is final state OR daughter is
c         also s-channel particle but not written in event file. If 1!=2, 1
c         gives internal labeling (which is negative), 2 gives label of the
c         actual resonance in momenta list (which is positive).
c   Mbeta : Inv. mass of beta before shower
c   p_after : momenta after one emission (i.e. after shower)
c this subroutine returns the p_out momenta, determined by reshuffling
c p_after such that the invariant mass of beta becomes the same as Mbeta.
      implicit none
      include 'nexternal.inc'
      integer next
      parameter (next=2*nexternal-3)
      integer npart,decay_tree(2,-next:-1,2),ibeta,i,j,d
      double precision Mbeta,p_after(0:4,next),p_out(0:4
     $     ,next),p_after_b(0:4,next),pbetab(0:3)
      double precision dot
c determine reshuffled momenta of resonance and all particles not part
c of the decay of the resonance (EQ.6).
      call OS_reshuffle_resonance(npart,ibeta,Mbeta,p_after
     $     ,p_out)
c From the resonance momenta and the momenta after shower reshuffle all
c the decay products of the resonace.
c First, boost all decays to restframe of beta (EQ.17-24 and 31).
      do j=1,3
         pbetab(j)=-p_after(j,ibeta)
      enddo
      pbetab(0)=p_after(0,ibeta)
      do i=-1,-next,-1
         do j=1,2
            d=decay_tree(j,i,2)
            if (d.lt.0) cycle ! skip s-channels not in event file
            if (d.eq.0) exit  ! done
            call boostm(p_after(0,d),pbetab,p_after(4,ibeta),p_after_b(0
     $           ,d))
            p_after_b(4,d)=p_after(4,d)
         enddo
      enddo
c loop over the decay_tree to reshuffle all the momenta of the decay
c     products to be consistent with the reshuffled resonance momentum
      call OS_reshuffle_decays(ibeta,decay_tree,p_after_b(0,1),p_out(0,1))
c finally, boost all decays back from beta restframe to c.m. frame
c (EQ.26-27 and 40)
      do i=-1,-next,-1
         do j=1,2
            d=decay_tree(j,i,2)
            if (d.lt.0) cycle ! skip s-channels not in event file
            if (d.eq.0) exit  ! done
            call boostm(p_out(0,d),p_out(0,ibeta),p_out(4,ibeta)
     $           ,p_out(0,d))
         enddo
      enddo
      return
      end



      subroutine OS_reshuffle_decays(ibeta,decay_tree,ppa,ppout)
c Reshuffle the momenta of all the particles in the decay-tree of beta.
      implicit none
      include 'nexternal.inc'
      integer next
      parameter (next=2*nexternal-3)
      integer decay_tree(2,-next:-1,2),ibeta,idec,d1,d2,i,j
      double precision ppa(0:4,next), ppout(0:4,next)
      double precision pa(0:4,-next:next),pout(0:4,-next:next),pa_d(0:4
     $     ,2),pout_d(0:4,2)
c First, fill the all the 'decay' momenta in the 1->2 decays (apart from
c the momentum of beta (hence, stop the loop at -2)). Like in genps_fks,
c these have negative labels in 'pa' and 'pout'. If some of the decays
c of beta are themselves resonances, some of these momenta should be
c identical to momenta already in 'pa'. Simply ignore this here, but
c update the ones in pout below.
      do i = 1, next
        do j = 0, 4
          pa(j, i) = ppa(j, i)
          pout(j, i) = ppout(j, i)
        enddo
      enddo

      do idec=-next,-2
         d1=decay_tree(1,idec,1)
         d2=decay_tree(2,idec,1)
         if (d1.eq.0 .and. d2.eq.0) then
            cycle
         elseif (d1.eq.0 .or. d2.eq.0) then
            write (*,*)'Error #1 in OS_reshuffle_decays: both daughters'/
     $           /' should exist or be absent',idec,d1,d2
            stop 1
         endif
         do j=0,3
            pa(j,idec)=pa(j,d1)+pa(j,d2)
         enddo
         pa(4,idec)=sqrt(pa(0,idec)**2-pa(1,idec)**2-pa(2,idec)**2-pa(3
     $        ,idec)**2)
      enddo
c Now loop over all the 1->2 decays, and do their reshuffling.      
      do idec=-1,-next,-1
         d1=decay_tree(1,idec,1)
         d2=decay_tree(2,idec,1)
         if (d1.eq.0 .or. d2.eq.0) exit ! done when no more decays
         do j=0,4
            pa_d(j,1)=pa(j,d1)
            pa_d(j,2)=pa(j,d2)
         enddo
         if (idec.eq.-1) then   ! decay of beta
            call OS_reshuffle_one_decay(pout(4,ibeta),pa_d,pout_d)
         else
           write (*,*)'Error #2 in OS_reshuffle_decays: not implemented',idec
           stop 1
CC            call OS_reshuffle_decay_of_decay(pout(0,decay_tree(1,idec
CC     $           ,2)),pa_d,pout_d)
         endif
         do j=0,4
            pout(j,d1)=pout_d(j,1)
            pout(j,d2)=pout_d(j,2)
         enddo
c     if some of the decays of beta are themselves resonances to be
c     written in the event file, update those momenta as well.
         do i=1,2
            if (decay_tree(i,idec,1).ne.decay_tree(i,idec,2)) then
               do j=0,4
                  pout(j,decay_tree(i,idec,2))=pout(j,decay_tree(i,idec
     $                 ,1))
               enddo
            endif
         enddo
      enddo
      do i = 1, next
        do j = 0, 4
          ppout(j, i) = pout(j, i)
        enddo
      enddo

      return
      end


      subroutine OS_reshuffle_resonance(npart,ibeta,M,p_a,p_out)
c Given the momenta of beta after shower and the recoil momenta,
c reshuffle the former such that its invariant mass coincides with the
c one from before showering
      implicit none
      include 'nexternal.inc'
      integer npart, ibeta
      integer next
      parameter (next=2*nexternal-3)
      double precision q(0:4),p_rec(0:4),M,p_a(0:4,next),p_out(0:4,next)
      double precision expybst,shybst,chybst,chybstmo,xdir(3)
      integer i,j
      double precision lambda2,rho
      external lambda2,rho
      double precision tiny,vtiny,vvtiny
      parameter (tiny=1d-5,vtiny=1d-8,vvtiny=1d-14)
c determine recoil momentum and c.m. momentum (EQ.2-3)
      if (nincoming.eq.2) then
         do j=0,3
            q(j)=p_a(j,1)+p_a(j,2)
         enddo
         q(4)=q(0)
      else
         do j=0,3
            q(j)=p_a(j,1)
         enddo
         q(4)=q(0)
      endif
c     check that we are in the c.m. frame
      do j=1,3
         if (abs(q(j))/q(0).gt.vvtiny) then
            write (*,*) 'Error #2 in OS_reshuffle_resonance: '/
     $           /'Not in C.M. frame',q
            stop 1
         endif
      enddo
      do j=0,3
         p_rec(j)=q(j)-p_a(j,ibeta)
      enddo
      p_rec(4)=sqrt(max(p_rec(0)**2-p_rec(1)**2-p_rec(2)**2-p_rec(3)**2
     $     ,0d0))
c determine the boost -- EQ.(8):
      if (p_rec(4)/q(0).gt.vtiny) then
         expybst=(p_rec(0)+rho(p_rec))/(2*q(0)*p_rec(4)**2) * (q(4)**2
     $        +p_rec(4)**2-M**2-q(4)**2*lambda2(q(0),p_rec(4),M))
         expybst=1/expybst
      else
         expybst=q(0)*(p_rec(0)+rho(p_rec))/(q(4)**2-M**2)*
     $            (1d0+M**2*p_rec(4)**2/(q(4)**2-M**2)**2)
      endif
      if (expybst.lt.0d0) then
         write (*,*) 'Error #3 in OS_reshuffle_resonance: '/
     $        /'Lorentz boost factor is negative',expybst
         stop 1
      elseif (expybst.lt.vvtiny) then
         write (*,*) 'Error #3 in OS_reshuffle_resonance: '/
     $        /'Lorentz boost too extreme',expybst
         stop 1
      endif
      shybst=(expybst-1/expybst)/2.d0
      chybst=(expybst+1/expybst)/2.d0
      chybstmo=chybst-1.d0
c and the boost direction:
      do j=1,3
         xdir(j)=p_a(j,ibeta)/rho(p_a(0,ibeta))
      enddo
c copy the initial-state momenta as they are
      do i=1,nincoming
        do j=0,3
          p_out(j,i)=p_a(j,i)
        enddo
      enddo
c compute all the new momenta EQ.(6):
      do i=nincoming+1,npart+1
         if (i.eq.ibeta) then
            call boostwdir2(chybst,shybst,chybstmo,xdir,p_rec,
     &           p_out(0,i))
            do j=0,3
               p_out(j,i)=q(j)-p_out(j,i)
            enddo
            p_out(4,i)=sqrt(max(p_out(0,i)**2-p_out(1,i)**2
     $           -p_out(2,i)**2-p_out(3,i)**2,0d0))
         else
c     here we fill all p_out, including the decay products. They will be
c     overwritten later.
            call boostwdir2(chybst,shybst,chybstmo,xdir,p_a(0,i),
     &           p_out(0,i))
            p_out(4,i)=p_a(4,i)
         endif
      enddo
c Check that mass is okay:
      if (abs(p_out(4,ibeta)-M)/(p_out(4,ibeta)+M).gt.tiny) then
         write (*,*) 'Error #1 in OS_reshuffle_resonance: '/
     $        /'masses not identical after reshuffle',p_out(4,ibeta),M
         stop 1
      endif
      return
      end


      subroutine OS_reshuffle_one_decay(M,pa,pout)
c Given the two decay products (after shower) in 'pa', reshuffles them
c to make them consistent with decaying particle with mass 'M' and
c returns them in 'pout'. The two daughter momenta are assumed to be
c back-to-back, i.e., we are in the restframe of their mother.
      implicit none
      double precision M,pa(0:4,2),pout(0:4,2),fac1,fac2
      integer i,j
      double precision lambda2,rho
      external lambda2,rho
      double precision vtiny
      parameter (vtiny=1d-12)

      double precision sumdot
      do j=1,3
         if (abs(pa(j,1)+pa(j,2))/(pa(0,1)+pa(0,2)).gt.vtiny) then
            write (*,*) 'Error #1 in OS_reshuffle_one_decay: '/
     $           /'Not in C.M. frame of mother'
            write (*,*) 'daughter 1:',(pa(i,1),i=0,4)
            write (*,*) 'daughter 1:',(pa(i,2),i=0,4)
            write (*,*) 'sum       :',(pa(i,1)+pa(i,2),i=0,4)
            stop 1
         endif
      enddo
      fac1=(pa(4,1)**2-pa(4,2)**2)/M**2
      fac2=M/2d0*lambda2(M,pa(4,1),pa(4,2))/rho(pa(0,1))
      do j=0,3
         if (j.eq.0) then
            pout(j,1)=M/2d0 *(1d0+fac1)
            pout(j,2)=M/2d0 *(1d0-fac1)
         else
            pout(j,1)=fac2*pa(j,1)
            pout(j,2)=fac2*pa(j,2)
         endif
      enddo
      pout(4,1)=pa(4,1)
      pout(4,2)=pa(4,2)
      return
      end


      subroutine write_momenta(p)
      implicit none
      include 'nexternal.inc'
      double precision p(0:3,nexternal)
      integer i
      do i = 1, nexternal
        write(*,*) i, p(0,i), p(1,i), p(2,i), p(3,i)
      enddo
      return
      end

      subroutine write_momenta4(p)
      implicit none
      include 'nexternal.inc'
      double precision p(0:4,nexternal)
      integer i
      do i = 1, nexternal
        write(*,*) i, p(0,i), p(1,i), p(2,i), p(3,i), p(4,i)
      enddo
      return
      end
