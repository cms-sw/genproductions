!!
!!  File DD_4pt.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!

**********************************************************************
        subroutine D_dd(D,Duv,p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                  r2,id)
**********************************************************************
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       D(i>0,j,k,l) calculated for rank r=r2+1 as well.
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       1.5.2006 Stefan Dittmaier
**********************************************************************

#ifdef collierdd
c LH  global cache system
        use cache
#endif

        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,r2,i,igc,j,jg,jgc,k,l,n,i0,i1,i2,i3,i123,count
        complex*16 D(0:r2,0:r2,0:r2,0:r2),D0dd
        complex*16 Duv(0:r2,0:r2,0:r2,0:r2)
        complex*16 D_new(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv_new(0:r2,0:r2,0:r2,0:r2)
        complex*16 D_newprelim(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv_newprelim(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 m02,m12,m22,m32,f(3),detx3
        complex*16 x3(0:3,0:3),tx3(0:3,0:3),ttx3(0:3,0:3,0:3,0:3)
        complex*16 mat(4,4),mati(4,4)
        real*8 accr(0:rmax4),accrnew(0:rmax4),accrnewprelim(0:rmax4)
        real*8 z3(3,3),tz3(3,3),z3i(3,3),ttz3(3,3,3,3)
        real*8 Dij_err_newprelim(0:2*rmax4),D00_err_newprelim(0:2*rmax4)
        integer id,nid(0:nmax-1),qmethod_newprelim,cnt
        logical gp_ok,g2p_ok,gcp_ok,apv_ok
#ifdef collierdd
c LH: variables for cache system
        complex*16 xcache(10), fct(Ncoefmax4)
        integer n0,n1,n2,n3,n123,cntmax,r2cache
c        logical nocalc/.false./, wrica/.false./
c        save wrica
         logical :: nocalc = .false.
         logical, save :: wrica = .false.
#endif

c          outlevel=2

c set identifiers for lower-point integrals
        n = 0
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            nid(n) = id + 2**k
            n=n+1
          endif
          if (n.eq.4) goto 205
        enddo
205     continue

c store DD debug info
        if (id.eq.0) then
          s_DDin  = 'D_dd'
          nc_DDin = 4
          nr_DDin = 6
          ni_DDin = 2
          r_DDin(1) = p1
          r_DDin(2) = p2
          r_DDin(3) = p3
          r_DDin(4) = p4
          r_DDin(5) = p12
          r_DDin(6) = p23
          c_DDin(1) = m02
          c_DDin(2) = m12
          c_DDin(3) = m22
          c_DDin(4) = m32
          i_DDin(1) = r2
          i_DDin(2) = id
        endif 

c       write(*,*)
c       write(*,*) 'D call ',id
c       write(*,*) ' p1  = ',p1
c       write(*,*) ' p2  = ',p2
c       write(*,*) ' p3  = ',p3
c       write(*,*) ' p4  = ',p4
c       write(*,*) ' p12 = ',p12
c       write(*,*) ' p23 = ',p23
c       write(*,*) ' m02 = ',m02
c       write(*,*) ' m12 = ',m12
c       write(*,*) ' m22 = ',m22
c       write(*,*) ' m32 = ',m32

c initialization for master call
        if (id.eq.0) then
          nmaster   = 4
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
          do i=0,15
            r2_aux(i)     = -1
            r2_new_aux(i) = -1
            do r=0,r2
              resaccrel(i,r)  = 0d0
              resaccabs(i,r)  = 0d0
              resaccrel2(i,r) = 0d0
              resaccabs2(i,r) = 0d0
            enddo
          enddo
        endif

        if (r2.gt.r2max4) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D_dd called for rank r2 =',r2
            write(outchannel,*) 'r2max4 =',r2max4,' too small'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

c initializations 
        expcrit = .2d0
        gcrit   = .2d0
        g2crit  = .2d0
        gccrit  = .2d0
        cutacc  = 1d20
        reqacc  = 20d0  
        accnew  = 1d30
        esterr     = 0d0
        esterr_new = 0d0

c        expcrit = .05d0
c        gcrit   = .05d0
c        g2crit  = .05d0
c        gccrit  = .05d0

#ifdef collierdd
c LH: read global cache
c =====================
        if (use_cache_system) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              xcache(1) = p1
              xcache(2) = p2
              xcache(3) = p3
              xcache(4) = p4
              xcache(5) = p12
              xcache(6) = p23
              xcache(7) = m02
              xcache(8) = m12
              xcache(9) = m22
              xcache(10) = m32

              cntmax = 2*(NCoefs(r2+1,4)-(r2+2)*(r2+3)/2) + 7*r2 + 10

              if (cntmax.gt.NCoefmax4) then
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'NCoefmax4 too small!'
                  if (cout.eq.coutmax) call DDlastmessage()
                  cout = cout+1
                endif
                stopflag = min(-9,stopflag)
              endif

              r2cache = r2
              call ReadCache(fct(1:cntmax),cntmax,xcache,10,2,id,4,
     &                       r2cache,nocalc,wrica)

              if(nocalc)then
                accflag   = nint(dreal(fct(1)))
                errflag   = nint(dreal(fct(2)))
                stopflag  = min(nint(dreal(fct(3))),stopflag)
                cnt = 3

                do r=0,r2              
                  cnt = cnt+1
                  resaccabs(id,r)  = dreal(fct(cnt))
                  cnt = cnt+1
                  resaccrel(id,r)  = dreal(fct(cnt))
                  cnt = cnt+1
                  Dij_err(tid(id),r)    = dreal(fct(cnt))
                  cnt = cnt+1
                  resaccabs2(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  resaccrel2(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  Dij_err2(tid(id),r)   = dreal(fct(cnt))
                  cnt = cnt+1
                  D00_err(tid(id),r)    = dreal(fct(cnt))

                  do n0=0,r+1,2
                    n123 = r-max(n0/2,n0-1)
                    do n1=0,n123
                      do n2=0,n123-n1
                        n3=n123-n1-n2

                        cnt = cnt+1
                        D(n0/2,n1,n2,n3) = fct(cnt)
                        cnt = cnt+1
                        Duv(n0/2,n1,n2,n3) = fct(cnt)

                      end do
                    end do
                  end do
                end do
                return
              endif
          end if
        end if
c =====================
#endif

c read cached information for repeated calls
c-------------------------------------------
        if (r2.le.r2_aux(id)) then
          cnt = 0
c          do 500 r=0,r2_aux(id)+1
          do r=0,r2+1
          outer: do i0=0,r,2
c            if ((i0.eq.0).and.(r.eq.r2_aux(id)+1)) goto 500
            if ((i0.eq.0).and.(r.eq.r2+1)) cycle outer
          do i1=0,r-i0
          do i2=0,r-i0-i1
            i3 = r-i0-i1-i2
            cnt = cnt + 1
            D(i0/2,i1,i2,i3)   = D_cache(tid(id),cnt)
            Duv(i0/2,i1,i2,i3) = Duv_cache(tid(id),cnt)
          end do
          end do
          end do outer
          end do
          return
        endif

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p4
        q12 = (p1+p12-p2)/2d0
        q23 = (p4+p12-p3)/2d0
        q13 = (p1+p4-p23)/2d0

c Gram and related matrices
        z3(1,1) = 2d0*q1
        z3(1,2) = 2d0*q12
        z3(1,3) = 2d0*q13
        z3(2,1) = z3(1,2)
        z3(2,2) = 2d0*q2
        z3(2,3) = 2d0*q23
        z3(3,1) = z3(1,3)
        z3(3,2) = z3(2,3)
        z3(3,3) = 2d0*q3
        call inverse_dd(z3,z3i,detz3,3)
        do i=1,3
        do j=1,3
          tz3(i,j) = z3i(j,i)*detz3
        end do
        end do

        do i=1,3
        do k=i,3
        do j=1,3
        do l=j,3
          if ((i.eq.k).or.(j.eq.l)) then
            ttz3(i,k,j,l) = 0d0
          else
            ttz3(i,k,j,l) = (-1)**(1+i+j+k+l)*z3(6-i-k,6-j-l)
          endif
          ttz3(k,i,j,l) = -ttz3(i,k,j,l) 
          ttz3(i,k,l,j) = -ttz3(i,k,j,l) 
          ttz3(k,i,l,j) = +ttz3(i,k,j,l) 
        end do
        end do
        end do
        end do
 
c Caley and related matrices
        f(1) = q1-m12+m02
        f(2) = q2-m22+m02
        f(3) = q3-m32+m02

        x3(0,0) = 2d0*m02
        do i=1,3
          x3(0,i) = f(i)
          x3(i,0) = f(i)
        do j=1,3
          x3(i,j) = z3(i,j)
        end do
        end do

        do i=1,4
        do j=1,4
          mat(i,j) = x3(i-1,j-1)
        end do
        end do
        call xinverse_dd(mat,mati,detx3,4)
        do i=0,3
        do j=0,3
          tx3(i,j) = mati(j+1,i+1)*detx3
        end do
        end do

        do i=1,3
        do j=1,3
          ttx3(0,i,0,j) = -tz3(i,j)
          ttx3(i,0,j,0) = -tz3(i,j)
          ttx3(0,i,j,0) =  tz3(i,j)
          ttx3(i,0,0,j) =  tz3(i,j)
          ttx3(0,i,j,j) =  0d0
          ttx3(i,0,j,j) =  0d0
          ttx3(j,j,0,i) =  0d0
          ttx3(j,j,i,0) =  0d0
        do k=j+1,3
          ttx3(0,i,j,k) = 0d0
        do n=1,3
          ttx3(0,i,j,k) = ttx3(0,i,j,k) - f(n)*ttz3(n,i,j,k) 
        enddo
          ttx3(0,i,k,j) =  ttx3(0,i,j,k) 
          ttx3(i,0,j,k) = -ttx3(0,i,j,k)
          ttx3(i,0,k,j) =  ttx3(0,i,j,k)
          ttx3(j,k,0,i) =  ttx3(0,i,j,k) 
          ttx3(k,j,0,i) = -ttx3(0,i,j,k) 
          ttx3(j,k,i,0) = -ttx3(0,i,j,k) 
          ttx3(k,j,i,0) =  ttx3(0,i,j,k) 
        end do
        end do
        end do

        qmethod_new(id)   = 0
        qmethod_newprelim = 0

c scalar 4pt integral
c--------------------
        D(0,0,0,0)   = D0dd(p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,0)
        scalint(id)  = D(0,0,0,0)
        scalintnew(id) = 0d0

        scalint_err(id) = 10*dprec_dd*abs(D(0,0,0,0))
     &            *max( 1d0/sqrt(abs(detx3))/abs(D(0,0,0,0)), 1d0 )
        Dij_err(tid(id),0)  = scalint_err(id)
        Dij_err2(tid(id),0) = scalint_err(id)
        D00_err(tid(id),0)  = 0d0
        Duv(0,0,0,0)   = 0d0

c quit if no tensors are needed
        if (r2.le.0) then
          acc     = scalint_err(id)/abs(D(0,0,0,0))
          accr(0) = acc
          goto 599
        endif


c*** PaVe reduction available for any tensor rank
c------------------------------------------------

        call Dpave_dd(D,Duv,p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                  acc,accr,z3,z3i,f,r2,id,nid)

        if (acc.gt.cutacc) acc = cutacc

        if ((acc.lt.dacc).or.(mode34.eq.0)) goto 599

c mode34=2: improvement by expansion in small Gram/kin. determinants
c-------------------------------------------------------------------
        if (mode34.eq.2) then

c some criteria / parameter for expansions
          scale2d  = 1d0/sqrt(abs(D(0,0,0,0)))
c          scale2in = (abs(p1)+abs(p2)+abs(p3)+abs(p4)+abs(p12)+abs(p23)
c     &                +abs(m02)+abs(m12)+abs(m22)+abs(m32) )/10d0
          scale2in = max( abs(p1),abs(p2),abs(p3),abs(p4),abs(p12),
     &                    abs(p23),abs(m02),abs(m12),abs(m22),abs(m32) )
          scale2hi = max(scale2d,scale2in)
          scale2lo = min(scale2d,scale2in)

c determine indices k,l for expansions
          maxtz_kl = 0d0
          k = 1
          l = 1
          do i1=1,3
          do i2=i1,3
            if (abs(tz3(i1,i2)).ge.maxtz_kl) then
              maxtz_kl = max(maxtz_kl,abs(tz3(i1,i2)))
              k = i1
              l = i2
            endif
          enddo
          enddo
          if (abs(tx3(l,0)).lt.abs(tx3(k,0))) then
            i1 = k
            k  = l
            l  = i1
          endif
          maxttx0klm(id)= max(abs(ttx3(0,k,l,1)),abs(ttx3(0,l,k,1)),
     &                        abs(ttx3(0,k,l,2)),abs(ttx3(0,l,k,2)),
     &                        abs(ttx3(0,k,l,3)),abs(ttx3(0,l,k,3)))
          maxttz_knlm(id) = 0d0
          do i1=1,3
          do i2=1,3
            maxttz_knlm(id) = max(maxttz_knlm(id),abs(ttz3(k,i1,l,i2))) 
          enddo
          enddo
          ttzff_kl(id) = abs(tx3(k,l)-2d0*m02*tz3(k,l))

c determine index j=jg for Gram 
          maxtx_0j = 0d0
          do i1=1,3
            if (abs(tx3(0,i1)).ge.maxtx_0j) then
              maxtx_0j = abs(tx3(0,i1))
              jg = i1
            endif
          enddo
          maxtz_nj(id) = 0d0
          do n=1,3
            maxtz_nj(id) = max(maxtz_nj(id),abs(tz3(n,jg)))
          enddo

c determine indices i=igc,j=jgc for Gram/Cayley 
          maxtx3ij = 0d0
          igc = 1
          jgc = 1
          do i1=1,3
          do i2=i1,3
            if (abs(tx3(i1,i2)).ge.maxtx3ij) then
              maxtx3ij = abs(tx3(i1,i2))
              igc = i1
              jgc = i2
            endif
          enddo
          enddo
          if (abs(tx3(0,igc)).lt.abs(tx3(0,jgc))) then
            i1  = igc
            igc = jgc
            jgc = i1
          endif         
          maxttx0ijm(id) = 
     &      max(abs(ttx3(0,igc,jgc,1)),abs(ttx3(0,jgc,igc,1)),
     &          abs(ttx3(0,igc,jgc,2)),abs(ttx3(0,jgc,igc,2)),
     &          abs(ttx3(0,igc,jgc,3)),abs(ttx3(0,jgc,igc,3)))

c check if expansions are appropriate
          c0err = 1d-12/scale2lo
          dmiss = 1d0/scale2lo**2
          prog  = 1d0
          do n=0,3
            prog = max(prog,
     &                 Cij_err(tid(nid(n)),r2-1)/Cij_err(tid(nid(n)),0))
          enddo
          prog  = 10d0*prog**(1d0/(r2-1))
          aa = max( 1d0, maxtx3ij/maxtx_0j, maxttx0klm(id)/maxtz_kl )
          bb = max( 1d0, maxtx3ij/maxtx_0j )

c expansion parameter for Gram 
          gparam = abs(detz3)/maxtx_0j

c relax expansion criteria if PV fails badly
          if (acc.gt.1d0) then
          expcrit = 0.7d0
          gcrit   = 0.7d0
          g2crit  = 0.7d0
          gccrit  = 0.7d0
        endif

c error propagation for Gram up to rank=r2+2
          gp_ok = (gparam.lt.gcrit)
          if (gp_ok) then
            gerr1 = c0err*maxtz_kl/maxtx_0j * prog**r2 * max( 
     &                gparam**(2*r2-2)
     &                  *(maxtx3ij*maxttz_knlm(id)/maxtz_kl**2)**(r2-2)
     &                  *aa*max(aa, scale2hi*maxttz_knlm(id)/maxtz_kl),
     &                (scale2hi*maxttz_knlm(id)/maxtz_kl)**(r2/2)*aa,
     &                max( aa, scale2hi*maxttz_knlm(id)/maxtz_kl )
     &                  *aa**(r2-3)
     &                  *max(aa**2,
     &                       maxttz_knlm(id)*maxtx_0j/maxtz_kl**2*bb) )
            gerr2 = dmiss * gparam**(r2+1) * aa**(r2-2) * bb 
     &                 *max( aa, maxttz_knlm(id)*maxtx_0j/maxtz_kl**2 )
            gerr  = max(gerr1,gerr2) / dmiss      
c            if (gerr/dmiss.gt.reqacc) gp_ok=.false.
          else
            gerr  = 1d10
          endif

c expansion parameter for Gram - version 2
          g2param = abs(detz3)/abs(detx3)*scale2hi
c error propagation for Gram - version 2 up to rank=r2+2
          g2p_ok = (g2param.lt.g2crit)
          if (g2p_ok) then
            g2err1 = c0err * prog**r2 * max( 
     &                 maxtx_0j/abs(detx3)  
     &                  * max( 1d0,(scale2hi*maxtx_0j/abs(detx3))**r2 ),
     &                 g2param**(r2+1)/scale2lo )
            g2err2 = dmiss * g2param**(r2+1)
            g2err  = max( g2err1, g2err2 ) / dmiss      
c            if (g2err/dmiss.gt.reqacc) g2p_ok=.false.
          else
            g2err  = 1d10
          endif

c expansion parameters for Gram/Cayley 
          gcparam = max( abs(detz3/tx3(igc,jgc)), 
     &                   abs(maxtx_0j/tx3(igc,jgc)) )
c error propagation for Gram/Cayley up to rank=r2+2
          gcp_ok  = (gcparam.lt.gccrit)
          if (gcp_ok) then
            gcerr1 = c0err * prog**r2 
     &                 *max(maxtz_kl,maxttx0ijm(id))/maxtx3ij
            gcerr2 = dmiss * gcparam**(r2+1) 
            gcerr  = max( gcerr1, gcerr2 ) / dmiss      
c            if (gcerr/dmiss.gt.reqacc) gcp_ok=.false.
          else
            gcerr  = 1d10
          endif

cxxx
c          write(*,*) 'D call   ',p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
c     &                 r2,id
c           write(*,*) 'scale2d  ',scale2d 
c           write(*,*) 'scale2in ',scale2in
c           write(*,*) 'prog     ',prog
c           write(*,*) 'gparam   ',gparam
c           write(*,*) 'gerr     ',gerr
c           if (gp_ok) write(*,*) '         ',gerr1,gerr2
c           write(*,*) 'gp_ok    ',gp_ok
c           write(*,*) 'g2param  ',g2param
c           write(*,*) 'g2err    ',g2err
c           if (g2p_ok) write(*,*) '         ',g2err1 ,g2err2 
c           write(*,*) 'g2p_ok   ',g2p_ok
c           write(*,*) 'gcparam  ',gcparam
c           write(*,*) 'gcerr    ',gcerr
c           if (gcp_ok) write(*,*) '         ',gcerr1,gcerr2 
c           write(*,*) 'gcp_ok   ',gcp_ok

c find optimal method
            Dij_err_new_max = 1d33
            accnew  = 1d30
            qmethod_new(id) = -1
            apv_ok = .true.
777         continue

            besterr = 1d10
            if (gp_ok)  besterr = min(besterr,gerr)
            if (g2p_ok) besterr = min(besterr,g2err)
            if (gcp_ok) besterr = min(besterr,gcerr)

            if ((.not.apv_ok).and.(besterr.eq.1d10)) goto 599

c decide on expansion
            if (apv_ok) then
c              if (abs(detx3*D(0,0,0,0)**2).gt.1d-8) then
              if (abs(detx3*D(0,0,0,0)**2).gt.1d-16) then
c alternative PaVe reduction
                D_newprelim(0,0,0,0) = D(0,0,0,0)
                qmethod_newprelim = 202
                call Dalpave_dd(D_newprelim,Duv_newprelim,
     &            Dij_err_newprelim,D00_err_newprelim,
     &            p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &            accnewprelim,accrnewprelim,f,detx3,tx3,r2,id,nid)
              else
                accnewprelim = 1d30
                qmethod_newprelim = 0
              endif
              apv_ok = .false.
            elseif (gp_ok.and.(besterr.eq.gerr)) then
c expansion for small Gram determinant
              qmethod_newprelim = 200
              call Dgram_dd(D_newprelim,Duv_newprelim,
     &            Dij_err_newprelim,D00_err_newprelim,
     &            p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &            jg,k,l,accnewprelim,accrnewprelim,
     &            detz3,tz3,ttz3,tx3,f,r2,id,nid)
              esterr = gerr
              gp_ok = .false.
            elseif (g2p_ok.and.(besterr.eq.g2err)) then
c expansion for small Gram determinant - version 2
              qmethod_newprelim = 205
              call Dgram2_dd(D_newprelim,Duv_newprelim,
     &            Dij_err_newprelim,D00_err_newprelim,
     &         p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &         accnewprelim,accrnewprelim,detz3,detx3,tx3,f,r2,id,nid)
              esterr = g2err
              g2p_ok = .false.
            elseif (gcp_ok.and.(besterr.eq.gcerr)) then
c expansion for small Gram and Cayley determinants
              qmethod_newprelim = 201
              call Dgramcayley_dd(D_newprelim,Duv_newprelim,
     &            Dij_err_newprelim,D00_err_newprelim,
     &          p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &          igc,jgc,k,l,accnewprelim,accrnewprelim,
     &          detz3,tz3,ttz3,tx3,f,r2,id,nid)
              esterr = gcerr
              gcp_ok = .false.
            endif

c*** result acceptable
            if (accnewprelim.lt.reqacc) then
              Dij_err_newprelim_max = 0d0
              do r=0,r2
                 Dij_err_newprelim_max = max( Dij_err_newprelim_max,
     &                                        Dij_err_newprelim(r) )
              enddo
              if (Dij_err_newprelim_max.lt.Dij_err_new_max) then
                esterr_new = esterr
                qmethod_new(id) = qmethod_newprelim
                Dij_err_new_max = Dij_err_newprelim_max
                accnew = accnewprelim
                do r=0,r2+1
                  Dij_err_new(tid(id),r) = Dij_err_newprelim(r) 
                  D00_err_new(tid(id),r) = D00_err_newprelim(r) 
                  accrnew(r) = accrnewprelim(r) 
                outer2: do i0=0,r,2
                  if ((i0.eq.0).and.(r.eq.r2+1)) cycle outer2
                do i1=0,r-i0
                do i2=0,r-i0-i1
                  i3 = r-i0-i1-i2
                  D_new(i0/2,i1,i2,i3)   = D_newprelim(i0/2,i1,i2,i3)
                  Duv_new(i0/2,i1,i2,i3) = Duv_newprelim(i0/2,i1,i2,i3)
                end do
                end do
                end do outer2
                end do
              endif
            endif

c*** try other expansions if previous expansion was inappropriate

            if (accnewprelim.gt.dacc) then
              if ((outlevel.gt.0).and.(id.eq.0)
     &            .and.cout_on.and.(cout.le.coutmax)
     &           ) then
                 write(outchannel,*) 'D: new method failed: ',
     &                 qmethod_newprelim,accnewprelim,esterr
              endif
              goto 777
            endif

        endif

599     continue

c*** Final result
c================

        if (accnew.gt.cutacc) accnew = cutacc

        if (accnew.gt.reqacc) then
          qmethod_new(id) = -1
          accnew = 1d31
        endif
        acc_pave(id) = acc
        acc_new(id)  = accnew
        qmethod(id)  = 0
        do r=0,r2
          resaccrel(id,r) = accr(r)
        enddo

        Dij_err_max     = 0d0
        do r=0,r2
          Dij_err_max   = max(Dij_err_max,Dij_err(tid(id),r))
        enddo
        Dij_err_new_max = 0d0
        if (qmethod_new(id).gt.0) then
          do r=0,r2
            Dij_err_new_max= max(Dij_err_new_max,Dij_err_new(tid(id),r))
          enddo
        endif

c        if (accnew.lt.acc) then
        if ((qmethod_new(id).gt.0).and.
     &      (Dij_err_new_max.lt.Dij_err_max)) then

          do r=0,r2+1
            Dij_err(tid(id),r)  = Dij_err_new(tid(id),r) 
            Dij_err2(tid(id),r) = Dij_err(tid(id),r) 
            D00_err(tid(id),r)  = D00_err_new(tid(id),r) 
            if (r.le.r2) resaccrel(id,r) = accrnew(r)
          outer3: do i0=0,r,2
            if ((i0.eq.0).and.(r.eq.r2+1)) cycle outer3
          do i1=0,r-i0
          do i2=0,r-i0-i1
            i3 = r-i0-i1-i2
            D(i0/2,i1,i2,i3)   = D_new(i0/2,i1,i2,i3)
            Duv(i0/2,i1,i2,i3) = Duv_new(i0/2,i1,i2,i3)
          end do
          end do
          end do outer3
          end do
          qmethod(id) = qmethod_new(id)
        endif

c cache information
c==================

        r2_aux(id) = r2
        cnt = 0
        do r=0,r2_aux(id)+1
        outer4: do i0=0,r,2
          if ((i0.eq.0).and.(r.eq.r2_aux(id)+1)) cycle outer4
        do i1=0,r-i0
        do i2=0,r-i0-i1
          i3 = r-i0-i1-i2
          cnt = cnt + 1
          D_cache(tid(id),cnt)   = D(i0/2,i1,i2,i3)
          Duv_cache(tid(id),cnt) = Duv(i0/2,i1,i2,i3)
        end do
        end do
        end do outer4
        end do
        if (cnt.gt.Ncoefmax4_int) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'Ncoefmax4_int too small!'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-9,stopflag)
        endif

c store for checking purposes
c============================
        scalintnew(id) = D_new(0,0,0,0)

c accuracy estimate of master call
c=================================
          count = 1
c normalization to maximal coefficient
          Dmax = abs(D(0,0,0,0))
          do r=1,r2
            i0=0
            i123 = r-i0
            do i1=0,i123
              do i2=0,i123-i1
                i3=i123-i1-i2
                Dmax = max(Dmax,abs(D(i0/2,i1,i2,i3)))
              enddo
            enddo
          enddo
          do r=0,r2
            resaccabs(id,r) = Dmax*resaccrel(id,r)
            resaccabs2(id,r) = Dij_err2(tid(id),r)
            resaccrel2(id,r) = Dij_err2(tid(id),r)/Dmax
            if (resaccrel(id,r).gt.aimacc(4)) accflag = 1
            if (resaccrel(id,r).gt.erracc(4)) errflag = 1
          enddo

#ifdef collierdd
c LH: write global cache
c ======================
        if (use_cache_system) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              if (wrica) then
                fct(1) = accflag
                fct(2) = errflag
                fct(3) = stopflag
                cnt = 3

                do r=0,r2
                  cnt = cnt+1
                  fct(cnt) = resaccabs(id,r)
                  cnt = cnt+1
                  fct(cnt) = resaccrel(id,r)
                  cnt = cnt+1
                  fct(cnt) = Dij_err(tid(id),r)
                  cnt = cnt+1
                  fct(cnt) = resaccabs2(id,r)
                  cnt = cnt+1
                  fct(cnt) = resaccrel2(id,r)
                  cnt = cnt+1
                  fct(cnt) = Dij_err2(tid(id),r)
                  cnt = cnt+1
                  fct(cnt) = D00_err(tid(id),r)

                  do n0=0,r+1,2
                    n123 = r-max(n0/2,n0-1)
                    do n1=0,n123
                      do n2=0,n123-n1
                        n3=n123-n1-n2

                        cnt = cnt+1
                        fct(cnt) = D(n0/2,n1,n2,n3)
                        cnt = cnt+1
                        fct(cnt) = Duv(n0/2,n1,n2,n3)

                      end do
                    end do
                  end do
                end do

                call WriteCache(fct(1:cnt),cnt,id,4,r2)

              end if
          end if
        end if
c ======================
#endif

c test output
c============

        acc    = Dij_err_max/Dmax
        accnew = Dij_err_new_max/Dmax

        if ((outlevel.gt.0).and.(acc.ge.1d0).and.(accnew.ge.1d0))
     &      call DD_debugoutput()
        if ((outlevel.gt.0).and.(id.eq.0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)
            write(outchannel,*)
            write(outchannel,*) '4pt tensor integral id = ',id
            write(outchannel,*) '  ranks up to ',r2
            write(outchannel,*) '  Dacc_pave = ',acc
            if (qmethod_new(id).ne.0)
     &      write(outchannel,*) '  Dacc_new  = ',accnew,esterr_new,
     &               '  method ',qmethod_new(id)
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
        endif
        if ((outlevel.gt.1).and.(id.eq.0).and.(r2.gt.0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            do i=0,3
            write(outchannel,*)
            write(outchannel,*) '3pt tensor integral id = ',nid(i)
            write(outchannel,*) '  ranks up to ',r2_aux(nid(i)),
     &               '  method ',qmethod_new(nid(i))
            write(outchannel,*) '  Cacc_pave = ',acc_pave(nid(i))
            if (qmethod_new(nid(i)).ne.0)
     &      write(outchannel,*) '  Cacc_new  = ',acc_new(nid(i)),
     &               '  method ',qmethod_new(nid(i))
            enddo
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
        endif

        end

**********************************************************************
        subroutine Dpave_dd(D,Duv,p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                      acc,accr,z3,z3i,f,r2,id,nid)
**********************************************************************
*       Passarino-Veltman reduction
*
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       1.5.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,r2,id,nid(0:nmax-1)
        integer del(3,3),i(3),j(3),k,l(3),m,n,i0,i1,i2,i3,i123
        data del/1,0,0,0,1,0,0,0,1/
        complex*16 C0(0:r2-1,0:r2-1,0:r2-1,0:r2-1)
        complex*16 Cuv0(0:r2-1,0:r2-1,0:r2-1,0:r2-1)
        complex*16 C_n(0:r2-1,0:r2-1,0:r2-1)
        complex*16 Cuv_n(0:r2-1,0:r2-1,0:r2-1)
        complex*16 S(3,0:r2-1,0:r2-1,0:r2-1)
        complex*16 D(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv(0:r2,0:r2,0:r2,0:r2)
        complex*16 m02,m12,m22,m32,f(3),Stest
        complex*16 Dtest(0:rmax4,0:rmax4,0:rmax4,3)
        real*8 Cij_err0(0:2*rmax3),Cij_err02(0:2*rmax3),accr(0:rmax4)
        real*8 z3(3,3),z3i(3,3)

c        dpv_calc_dd = dpv_calc_dd + 1

        maxz  = 0d0
        maxzi = 0d0
        do m=1,3
        do n=m,3
          maxz  = max(abs(z3(m,n)),maxz)
          maxzi = max(abs(z3i(m,n)),maxzi)
        enddo
        enddo

        ziff   = 0d0
        maxzif = 0d0
        do m=1,3
        do n=1,3
          ziff   = max(ziff,abs(z3i(m,n)*f(n)*f(m)))
          maxzif = max(maxzif,abs(z3i(m,n)*f(n)))
        enddo
        enddo

        call C0_dd(C0,Cuv0, p2,p3,p23,m12,m22,m32,r2-1,nid(0))

        do n=1,3
          if (n.eq.1) then
            call C_dd(C_n,Cuv_n,p12,p3,p4,m02,m22,m32,r2-1,nid(1))
          elseif (n.eq.2) then
            call C_dd(C_n,Cuv_n,p1,p23,p4,m02,m12,m32,r2-1,nid(2))
          else
            call C_dd(C_n,Cuv_n,p1,p2,p12,m02,m12,m22,r2-1,nid(3))
          endif
          do r=1,r2
          do i1=0,r-1
          do i2=0,r-1-i1
            i3 = r-1-i1-i2
              S(n,i1,i2,i3)   = -C0(0,i1,i2,i3) 
            if ((n.eq.1).and.(i1.eq.0)) 
     &        S(1,i1,i2,i3) = S(1,i1,i2,i3) + C_n(0,i2,i3)
            if ((n.eq.2).and.(i2.eq.0)) 
     &        S(2,i1,i2,i3) = S(2,i1,i2,i3) + C_n(0,i1,i3)
            if ((n.eq.3).and.(i3.eq.0)) 
     &        S(3,i1,i2,i3) = S(3,i1,i2,i3) + C_n(0,i1,i2)
          enddo
          enddo
          enddo
        enddo

c initialization of error propagation
        Cij_err0(0)  = 
     &    max(Cij_err(tid(nid(0)),0),Cij_err(tid(nid(1)),0),
     &        Cij_err(tid(nid(2)),0),Cij_err(tid(nid(3)),0))
        Cij_err02(0) = 
     &    max(Cij_err2(tid(nid(0)),0),Cij_err2(tid(nid(1)),0),
     &        Cij_err2(tid(nid(2)),0),Cij_err2(tid(nid(3)),0))
        Dij_err(tid(id),0)  = scalint_err(id)
        Dij_err2(tid(id),0) = scalint_err(id)
        D00_err(tid(id),0)  = 0d0
        do n=1,r2
          Dij_err(tid(id),n)  = 0d0
          Dij_err2(tid(id),n) = 0d0
          D00_err(tid(id),n)  = 0d0
        enddo

        Dmax    = abs(D(0,0,0,0))
        acc     = scalint_err(id)/abs(D(0,0,0,0))
        accr(0) = acc

c PaVe reduction
        do r=1,r2

c PaVe reduction of D_{...} -- Eq.(5.11)
c NOTE: D_{...} without "0" indices are UV finite !
        do n=1,3
          do i1=0,r-1
          do i2=0,r-1-i1
            i3 = r-1-i1-i2
              S(n,i1,i2,i3) = S(n,i1,i2,i3) - f(n)*D(0,i1,i2,i3)
          enddo
          enddo
        enddo

        do i1=0,r-1
        do i2=0,r-1-i1
          i(1) = i1
          i(2) = i2
          i(3) = r-1-i(1)-i(2)
          do k=3,1,-1
            j(1) = i(1)+del(k,1)
            j(2) = i(2)+del(k,2)
            j(3) = i(3)+del(k,3)
            D(0,j(1),j(2),j(3))   = 0d0
            Duv(0,j(1),j(2),j(3)) = 0d0
            do n=1,3
              l(1) = i(1)-del(n,1)
              l(2) = i(2)-del(n,2)
              l(3) = i(3)-del(n,3)
              D(0,j(1),j(2),j(3))   = D(0,j(1),j(2),j(3)) 
     &                           + z3i(k,n)*S(n,i(1),i(2),i(3))
              if (i(n).ne.0) then
                D(0,j(1),j(2),j(3))   = D(0,j(1),j(2),j(3)) 
     &                     -2d0*z3i(k,n)*i(n)*D(1,l(1),l(2),l(3))
              endif
            enddo
            Dtest(j(1),j(2),j(3),k) = D(0,j(1),j(2),j(3)) 
        end do
        end do
        end do

        Dsym_err = 0d0
        do i1=0,r
        do i2=0,r-i1
        i(1) = i1
        i(2) = i2
        i(3) = r-i1-i2
          if ((i(1).ne.0).and.(i(2).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),1)
     &                       -Dtest(i(1),i(2),i(3),2)))
          endif
          if ((i(1).ne.0).and.(i(3).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),1)
     &                       -Dtest(i(1),i(2),i(3),3)))
          endif
          if ((i(2).ne.0).and.(i(3).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),2)
     &                       -Dtest(i(1),i(2),i(3),3)))
          endif
        end do
        end do

c PaVe reduction of D_{00...} of rank r+1 -- Eq.(5.10)
        do i0=2,r+1,2
        do i1=0,r+1-i0
        do i2=0,r+1-i0-i1
          i3 = r+1-i0-i1-i2
          Duv(i0/2,i1,i2,i3) = 1d0/2d0/r*( 
     &            Cuv0(i0/2-1,i1,i2,i3) + 2d0*m02*Duv(i0/2-1,i1,i2,i3)
     &          + f(1)*Duv(i0/2-1,i1+1,i2,i3) 
     &          + f(2)*Duv(i0/2-1,i1,i2+1,i3) 
     &          + f(3)*Duv(i0/2-1,i1,i2,i3+1) )
          D(i0/2,i1,i2,i3)   = 1d0/2d0/r*( 4d0*Duv(i0/2,i1,i2,i3)
     &          + C0(i0/2-1,i1,i2,i3) + 2d0*m02*D(i0/2-1,i1,i2,i3) 
     &          + f(1)*D(i0/2-1,i1+1,i2,i3) + f(2)*D(i0/2-1,i1,i2+1,i3)
     &          + f(3)*D(i0/2-1,i1,i2,i3+1) )
          end do
          end do
          end do

        Stest = 0d0
        aux   = z3(1,1)

c error propagation for PaVe
        Cij_err0(r)  = 
     &    max(Cij_err(tid(nid(0)),r),Cij_err(tid(nid(1)),r),
     &        Cij_err(tid(nid(2)),r),Cij_err(tid(nid(3)),r)) 
        Cij_err02(r) = 
     &    max(Cij_err2(tid(nid(0)),r),Cij_err2(tid(nid(1)),r),
     &        Cij_err2(tid(nid(2)),r),Cij_err2(tid(nid(3)),r)) 

        if (r.ge.2) then
          D00_err(tid(id),r) = max(Cij_err0(r-2),
     &                             abs(m02)*Dij_err(tid(id),r-2),
     &      maxzif*Cij_err0(r-2),ziff*Dij_err(tid(id),r-2),
     &      maxzif*D00_err(tid(id),r-1))
c          acc = max(acc,D00_err(tid(id),r)/abs(D(1,0,0,0)))
        else
          D00_err(tid(id),1) = 0d0
        endif
        Dij_err(tid(id),r)  = max(Dsym_err,maxzi*Cij_err0(r-1),
     &      maxzif*Dij_err(tid(id),r-1),maxzi*D00_err(tid(id),r))
        Dij_err2(tid(id),r) = max(maxzi*Cij_err02(r-1),
     &      maxzif*Dij_err2(tid(id),r-1),maxzi*D00_err(tid(id),r))
     &    / sqrt(maxz*maxzi)

c find maximal value for |D(0,...)| of rank r
        i0=0
        i123 = r-i0
        do i1=0,i123
          do i2=0,i123-i1
            i3=i123-i1-i2
            Dmax = max(Dmax,abs(D(i0/2,i1,i2,i3)))
          enddo
        enddo
        acc     = max(acc,Dij_err(tid(id),r)/Dmax)
        accr(r) = acc
c        Dij_err(tid(id),r) = Dmax*accr(r)

        enddo

        end

**********************************************************************
        subroutine Dalpave_dd(D,Duv,
     &                      Dij_err_newprelim,D00_err_newprelim,
     &                      p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                      acc,accr,f,detx3,tx3,r2,id,nid)
**********************************************************************
*       Alternative Passarino-Veltman reduction
*
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       9.2.2009 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,r2,nid(0:nmax-1)
        integer del(3,3),i(3),j(3),k,l(3),n,i0,i1,i2,i3,i123,id
        data del/1,0,0,0,1,0,0,0,1/
        complex*16 C0(0:r2-1,0:r2-1,0:r2-1,0:r2-1)
        complex*16 Cuv0(0:r2-1,0:r2-1,0:r2-1,0:r2-1)
        complex*16 C_n(0:r2-1,0:r2-1,0:r2-1)
        complex*16 Cuv_n(0:r2-1,0:r2-1,0:r2-1)
        complex*16 Sh(3,0:r2-1,0:r2-1,0:r2-1,0:r2-1)
        complex*16 D(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 m02,m12,m22,m32,f(3)
        complex*16 detx3,tx3(0:3,0:3)
        complex*16 Dtest(0:rmax4,0:rmax4,0:rmax4,3)
        real*8 Dij_err_aux(0:2*rmax4),D00_err_aux(0:2*rmax4)
        real*8 Dij_err_newprelim(0:2*rmax4),D00_err_newprelim(0:2*rmax4)
        real*8 Cij_err0(0:2*rmax3),accr(0:rmax4)

c        dapv_calc_dd = dapv_calc_dd + 1

        do r=0,rmax4
          Dij_err_aux(r)       = 0d0
          D00_err_aux(r)       = 0d0
          Dij_err_newprelim(r) = 0d0
          D00_err_newprelim(r) = 0d0
        enddo

        call C0_dd(C0,Cuv0, p2,p3,p23,m12,m22,m32,r2-1,nid(0))
        
        do n=1,3
          if (n.eq.1) then
            call C_dd(C_n,Cuv_n,p12,p3,p4,m02,m22,m32,r2-1,nid(1))
          elseif (n.eq.2) then
            call C_dd(C_n,Cuv_n,p1,p23,p4,m02,m12,m32,r2-1,nid(2))
          else
            call C_dd(C_n,Cuv_n,p1,p2,p12,m02,m12,m22,r2-1,nid(3))
          endif
          do r=1,r2
          do i0=0,r-1,2
          do i1=0,r-1-i0
          do i2=0,r-1-i0-i1
            i3 = r-1-i0-i1-i2
              Sh(n,i0/2,i1,i2,i3) = -C0(i0/2,i1,i2,i3) 
            if ((n.eq.1).and.(i1.eq.0)) 
     &        Sh(1,i0/2,i1,i2,i3)=Sh(1,i0/2,i1,i2,i3)+C_n(i0/2,i2,i3)
            if ((n.eq.2).and.(i2.eq.0)) 
     &        Sh(2,i0/2,i1,i2,i3)=Sh(2,i0/2,i1,i2,i3)+C_n(i0/2,i1,i3)
            if ((n.eq.3).and.(i3.eq.0)) 
     &        Sh(3,i0/2,i1,i2,i3)=Sh(3,i0/2,i1,i2,i3)+C_n(i0/2,i1,i2)
          enddo
          enddo
          enddo
          enddo
        enddo

c initialization of error propagation
        Cij_err0(0) = max(Cij_err(tid(nid(0)),0),Cij_err(tid(nid(1)),0),
     &                    Cij_err(tid(nid(2)),0),Cij_err(tid(nid(3)),0))
        acc     = 10*dprec_dd
        accr(0) = acc
        Dij_err_aux(0)       = scalint_err(id)
        Dij_err_newprelim(0) = scalint_err(id) 
        Dmax = abs(D(0,0,0,0))

        maxtx30n = 0d0
        maxtx3kn = 0d0
        do n=1,3
          maxtx30n = max(maxtx30n,abs(tx3(0,n)))
        do k=n,3
          maxtx3kn = max(maxtx3kn,abs(tx3(k,n)))
        enddo
        enddo

        Duv(0,0,0,0) = 0d0

c alternative PaVe reduction
        do r=1,r2

c reduction of D_{00...}
        do i0=0,r-1,2
        do i1=0,r-1-i0
        do i2=0,r-1-i0-i1
        i3   = r-1-i0-i1-i2
        i(1) = i1
        i(2) = i2
        i(3) = i3
c Duv_{00...} from conventional PaVe reduction -- Eq.(5.10)
          if (i0.lt.2) then
            Duv(i0/2+1,i1,i2,i3) = 0d0
          else
            Duv(i0/2+1,i1,i2,i3) = 1d0/dble(2*r)*( 
     &            Cuv0(i0/2,i1,i2,i3) + 2d0*m02*Duv(i0/2,i1,i2,i3) 
     &          + f(1)*Duv(i0/2,i1+1,i2,i3) + f(2)*Duv(i0/2,i1,i2+1,i3) 
     &          + f(3)*Duv(i0/2,i1,i2,i3+1) )
          endif
c alternative PaVe reduction of D_{00...} -- Eq.(5.16)
          D(i0/2+1,i1,i2,i3) = 1d0/dble(2*r)*( 
     &            4d0*Duv(i0/2+1,i1,i2,i3) + C0(i0/2,i1,i2,i3) 
     &          + detx3/tx3(0,0)*D(i0/2,i1,i2,i3) )
          do n=1,3
            l(1) = i1-del(n,1)
            l(2) = i2-del(n,2)
            l(3) = i3-del(n,3)
            D(i0/2+1,i1,i2,i3) = D(i0/2+1,i1,i2,i3) 
     &          - tx3(0,n)/tx3(0,0)/dble(2*r)*Sh(n,i0/2,i1,i2,i3)
            if (i(n).gt.0) then
              D(i0/2+1,i1,i2,i3) = D(i0/2+1,i1,i2,i3)
     &          +tx3(0,n)/tx3(0,0)*i(n)/dble(r)*D(i0/2+1,l(1),l(2),l(3))
            endif
          enddo
        end do
        end do
        end do

c*** Cayley reduction of all D(0,i1,i2,i3) with i1+i2+i3>0 -- Eq.(5.15)
c       compare two versions of D(0,i1,i2,i3):  
c                       D(0,i1+1,i2,i3) vs. D(0,j1,j2+1,j3) 
c       and deduce error estimate from difference
c NOTE: D_{...} with <=2 indices "0" are UV finite !
        do i1=0,r-1
        do i2=0,r-1-i1
        i(1) = i1
        i(2) = i2
        i(3) = r-1-i1-i2
        do k=3,1,-1
          j(1) = i(1)+del(k,1)
          j(2) = i(2)+del(k,2)
          j(3) = i(3)+del(k,3)
          Duv(0,j(1),j(2),j(3)) = 0d0
          D(0,j(1),j(2),j(3))   = tx3(0,k)/detx3*( 
     &          2d0*r*D(1,i(1),i(2),i(3)) - C0(0,i(1),i(2),i(3)) )
          do n=1,3
            l(1) = i(1)-del(n,1)
            l(2) = i(2)-del(n,2)
            l(3) = i(3)-del(n,3)
            D(0,j(1),j(2),j(3)) = D(0,j(1),j(2),j(3)) 
     &                  +tx3(k,n)/detx3*Sh(n,0,i(1),i(2),i(3))
            if (i(n).ne.0) D(0,j(1),j(2),j(3)) = D(0,j(1),j(2),j(3)) 
     &                  -2d0*tx3(k,n)/detx3*i(n)*D(1,l(1),l(2),l(3))
          enddo
          Dtest(j(1),j(2),j(3),k) = D(0,j(1),j(2),j(3)) 
        end do
        end do
        end do

        Dsym_err = 0d0
        do i1=0,r
        do i2=0,r-i1
        i(1) = i1
        i(2) = i2
        i(3) = r-i1-i2
          if ((i(1).ne.0).and.(i(2).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),1)
     &                       -Dtest(i(1),i(2),i(3),2)))
          endif
          if ((i(1).ne.0).and.(i(3).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),1)
     &                       -Dtest(i(1),i(2),i(3),3)))
          endif
          if ((i(2).ne.0).and.(i(3).ne.0)) then
            Dsym_err = max(Dsym_err,abs(Dtest(i(1),i(2),i(3),2)
     &                       -Dtest(i(1),i(2),i(3),3)))
          endif
        end do
        end do

c error propagation for alternative PaVe
        Cij_err0(r) = max(Cij_err(tid(nid(0)),r),Cij_err(tid(nid(1)),r),
     &                    Cij_err(tid(nid(2)),r),Cij_err(tid(nid(3)),r))
        D00_err_aux(r+1) = max( Cij_err0(r-1),
     &    abs(detx3/tx3(0,0))*Dij_err_aux(r-1),
     &    maxtx30n/abs(tx3(0,0))*max(Cij_err0(r-1),D00_err_aux(r)) )
        Dij_err_aux(r) = max(Dsym_err, 
     &    maxtx30n/abs(detx3)*max(D00_err_aux(r+1),Cij_err0(r-1)),
     &    maxtx3kn/abs(detx3)*max(D00_err_aux(r),Cij_err0(r-1)) )

c find maximal value for |D(0,...)| of rank r
        i0=0
        i123 = r-i0
        do i1=0,i123
          do i2=0,i123-i1
            i3=i123-i1-i2
            Dmax = max(Dmax,abs(D(i0/2,i1,i2,i3)))
          enddo
        enddo
        acc     = max(acc,Dij_err_aux(r)/Dmax)

        accr(r) = acc
        Dij_err_newprelim(r) = accr(r)*Dmax
        D00_err_newprelim(r) = D00_err_aux(r)

        enddo

        end

**********************************************************************
        subroutine Dgram_dd(D,Duv,
     &                   Dij_err_newprelim,D00_err_newprelim,
     &                   p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                   j,k,l,acc,accr,detz3,tz3,ttz3,tx3,f,r2,id,nid)
**********************************************************************
*       Expansion for small Gram determinant
*
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       19.7.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,r2,del(3,3),i(3),j,k,l,m,n,i0,i1,i2,i3,i123
        integer rup,rupmax,step,ruplimit
        integer id,nid(0:nmax-1)
        data del/1,0,0,0,1,0,0,0,1/
        double complex, allocatable, dimension(:,:,:,:) :: C0,Cuv0
        double complex, allocatable, dimension(:,:,:) :: C_n,Cuv_n
        double complex, allocatable, dimension(:,:,:,:,:) :: Sh
        complex*16 Dstore(3,0:rmax4)
        complex*16 D(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 m02,m12,m22,m32,f(3)
        complex*16 tx3(0:3,0:3)
        real*8 Dij_err_aux(0:2*rmax4),D00_err_aux(0:2*rmax4)
        real*8 Dij_err_newprelim(0:2*rmax4),D00_err_newprelim(0:2*rmax4)
        real*8 tz3(3,3),ttz3(3,3,3,3)
        real*8 Cij_err0(0:2*rmax3),C00_err0(0:2*rmax3),accr(0:rmax4)
        real*8 Dmax(0:rmax4)

c        dg_calc_dd = dg_calc_dd + 1

        rupmax = ritmax
        if (rupmax.gt.rmax4-1) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &        'Dgram_dd called with rmax4-1 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        do r=0,rmax4
          Dmax(r)              = abs(scalint(id))
          accr(r)              = 0d0
          Dij_err_aux(r)       = 0d0
          D00_err_aux(r)       = 0d0
          Dij_err_newprelim(r) = 0d0
          D00_err_newprelim(r) = 0d0
        enddo

c initializations
c----------------
        rup = -1
        D(0,0,0,0) = 0d0
        accprev1 = 1d30
        accprev2 = 1d30

        step = 2
        ruplimit = r2-2 + step

c start iteration
c----------------
500     continue
        rup = rup+1

        if ((rup.eq.0).or.(rup.gt.ruplimit)) then

          if (rup.gt.ruplimit) ruplimit = min(ruplimit+step,rupmax)

          if (allocated(C0)) deallocate(C0,Cuv0,C_n,Cuv_n,Sh)
          allocate(C0(0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Cuv0(0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(C_n(0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Cuv_n(0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Sh(3,0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))

          call C0_dd(C0,Cuv0,p2,p3,p23,m12,m22,m32,ruplimit,nid(0))

c S functions
          do n=1,3
            if (n.eq.1) then
              call C_dd(C_n,Cuv_n,p12,p3,p4,m02,m22,m32,ruplimit,nid(1))
            elseif (n.eq.2) then
              call C_dd(C_n,Cuv_n,p1,p23,p4,m02,m12,m32,ruplimit,nid(2))
            else
              call C_dd(C_n,Cuv_n,p1,p2,p12,m02,m12,m22,ruplimit,nid(3))
            endif
            do r=0,ruplimit
            do i0=0,r,2
            do i1=0,r-i0
            do i2=0,r-i0-i1
              i3 = r-i0-i1-i2
                Sh(n,i0/2,i1,i2,i3) = -C0(i0/2,i1,i2,i3) 
              if ((n.eq.1).and.(i1.eq.0)) 
     &          Sh(1,i0/2,i1,i2,i3)=Sh(1,i0/2,i1,i2,i3)+C_n(i0/2,i2,i3)
              if ((n.eq.2).and.(i2.eq.0)) 
     &          Sh(2,i0/2,i1,i2,i3)=Sh(2,i0/2,i1,i2,i3)+C_n(i0/2,i1,i3)
              if ((n.eq.3).and.(i3.eq.0)) 
     &          Sh(3,i0/2,i1,i2,i3)=Sh(3,i0/2,i1,i2,i3)+C_n(i0/2,i1,i2)
            enddo
            enddo
            enddo
            enddo
          enddo

c Duv_{00...} from PaVe reduction -- Eq.(5.10)
          do r=0,ruplimit+1
          do i0=0,r,2
          do i1=0,r-i0
          do i2=0,r-i0-i1
          i3 = r-i0-i1-i2
            if (i0.le.2) then
              Duv(i0/2,i1,i2,i3) = 0d0
            else
              Duv(i0/2,i1,i2,i3) = 1d0/2d0/dble(r-1)*( 
     &              Cuv0(i0/2-1,i1,i2,i3) + 2d0*m02*Duv(i0/2-1,i1,i2,i3)
     &            + f(1)*Duv(i0/2-1,i1+1,i2,i3) 
     &            + f(2)*Duv(i0/2-1,i1,i2+1,i3) 
     &            + f(3)*Duv(i0/2-1,i1,i2,i3+1) )
            endif
          enddo
          enddo
          enddo
          enddo

        endif

c initialize higher-rank tensors
        do i0=0,rup+1,2
        do i1=0,rup+1-i0
        do i2=0,rup+1-i0-i1
        i3 = rup+1-i0-i1-i2
          D(i0/2,i1,i2,i3) = 0d0
        enddo
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,r2
            Dstore(1,n) = D(0,n,0,0) 
            Dstore(2,n) = D(0,0,n,0) 
            Dstore(3,n) = D(0,0,0,n) 
          enddo
        endif

c iteration: rup,  step: r
        do r=rup,0,-1

c D_{00...} from iteration Eq.(5.40)
        do i0=2*((r+1)/2),2,-2
        do i1=0,r+1-i0
        do i2=0,r+1-i0-i1
        i3 = r+1-i0-i1-i2
        i(1) = i1
        i(2) = i2
        i(3) = i3
          D(i0/2,i1,i2,i3) = 1d0/2d0/dble(i0+2*i1+2*i2+2*i3)*(
     &      4d0*Duv(i0/2,i1,i2,i3) 
     &      - detz3/tz3(k,l)
     &          *D(i0/2-1,i1+del(k,1)+del(l,1),i2+del(k,2)+del(l,2),
     &                  i3+del(k,3)+del(l,3))
     &      + 2d0*C0(i0/2-1,i1,i2,i3)
crrr terms rearranged - start (see also below under crrr)
crrr     &      + 2d0*m02*D(i0/2-1,i1,i2,i3) 
     &      + tx3(k,l)/tz3(k,l)*D(i0/2-1,i1,i2,i3) 
crrr terms rearranged - end
     &      )
          do n=1,3
            D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &        + 1d0/2d0/dble(i0+2*i1+2*i2+2*i3)*(
     &           tz3(n,l)/tz3(k,l)
     &            *Sh(n,i0/2-1,i1+del(k,1),i2+del(k,2),i3+del(k,3))
     &           - Sh(n,i0/2-1,i1+del(n,1),i2+del(n,2),i3+del(n,3)) )
          enddo
          do n=1,3
          do m=1,3
          if ((k.eq.n).or.(l.eq.m)) cycle
            D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &        + 1d0/2d0/dble(i0+2*i1+2*i2+2*i3)*(
     &        - ttz3(k,n,l,m)/tz3(k,l)*f(n)*Sh(m,i0/2-1,i1,i2,i3) 
crrr     &        + ttz3(k,n,l,m)/tz3(k,l)*f(n)*f(m)*D(i0/2-1,i1,i2,i3) 
     &        )

            if (i(n).ne.0) D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &        - i(n)/dble(i0+2*i1+2*i2+2*i3)*ttz3(k,n,l,m)/tz3(k,l)
     &          *(Sh(m,i0/2,i1-del(1,n),i2-del(2,n),i3-del(3,n))
     &            -f(m)*D(i0/2,i1-del(1,n),i2-del(2,n),i3-del(3,n)))
            if (i(m).ne.0) D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &        + i(m)/dble(i0+2*i1+2*i2+2*i3)*ttz3(k,n,l,m)/tz3(k,l)
     &          *f(n)*D(i0/2,i1-del(1,m),i2-del(2,m),i3-del(3,m))
            if (i(n)*i(m).ne.0) then
              if (n.ne.m) then
                D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &            +2d0*i(n)*i(m)/dble(i0+2*i1+2*i2+2*i3)
     &              *ttz3(k,n,l,m)/tz3(k,l)
     &              *D(i0/2+1,i1-del(1,m)-del(1,n),i2-del(2,m)-del(2,n),
     &                      i3-del(3,m)-del(3,n))
              elseif (i(n).gt.1) then
                D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &            + 2d0*i(n)*(i(n)-1)/dble(i0+2*i1+2*i2+2*i3)
     &              *ttz3(k,n,l,m)/tz3(k,l)
     &              *D(i0/2+1,i1-2*del(1,n),i2-2*del(2,n),i3-2*del(3,n))
              endif
            endif

        end do
        end do
        end do
        end do
        end do

c coefficients D(0,i1,i2,i3) from Eq.(5.38)
        Dmax(r) = 0d0
        i0 = 0
        do i1=0,r
        do i2=0,r-i1
        i3 = r-i0-i1-i2
        i(1) = i1
        i(2) = i2
        i(3) = i3
          D(i0/2,i1,i2,i3) = 0d0
          do n=1,3
            D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &                         - tz3(j,n)*Sh(n,i0/2,i1,i2,i3) 
            if (i(n).ne.0) D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3)
     &        + 2d0*i(n)*tz3(j,n)
     &          *D(i0/2+1,i1-del(1,n),i2-del(2,n),i3-del(3,n))
          enddo
          D(i0/2,i1,i2,i3) = ( D(i0/2,i1,i2,i3)
     &      +detz3*D(i0/2,i1+del(1,j),i2+del(2,j),i3+del(3,j)))/tx3(0,j)
        Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))
        end do
        end do

        enddo

c estimate precision from last improvements
        acc = 1d30
        if (rup.ge.r2) then
          acc = 0d0
c         acc = abs(D(0,0,0,0)/scalint(id)-1d0)
          do n=0,r2
            acc = max(acc,abs(Dstore(1,n)-D(0,n,0,0))/Dmax(n))
            acc = max(acc,abs(Dstore(2,n)-D(0,0,n,0))/Dmax(n))
            acc = max(acc,abs(Dstore(3,n)-D(0,0,0,n))/Dmax(n))
            accr(n) = acc
          enddo
        endif

c error propagation for Gram expansion
          Cij_err0(rup) = 
     &      max(Cij_err(tid(nid(0)),rup),Cij_err(tid(nid(1)),rup),
     &          Cij_err(tid(nid(2)),rup),Cij_err(tid(nid(3)),rup)) 
          C00_err0(rup) = 
     &      max(C00_err(tid(nid(0)),rup),C00_err(tid(nid(1)),rup),
     &          C00_err(tid(nid(2)),rup),C00_err(tid(nid(3)),rup)) 

          do r=rup,0,-1
          if (r.ge.1) then
            D00_err_aux(r+1) = max( 
     &        abs(detz3)/abs(tz3(k,l))*Dij_err_aux(r+1),
     &        Cij_err0(r-1), 
crrr     &        abs(m02)*Dij_err_aux(r-1),
     &        Cij_err0(r), 
     &        max( maxttx0klm(id)*Cij_err0(r-1),
     &             maxttz_knlm(id)*C00_err0(r),
crrr     &             ttzff_kl(id)*Dij_err_aux(r-1),
     &             abs(tx3(k,l))*Dij_err_aux(r-1),
     &             maxttx0klm(id)*D00_err_aux(r) )/abs(tz3(k,l)) )
          endif
          Dij_err_aux(r) = 
     &          max( maxtz_nj(id)*max(Cij_err0(r),D00_err_aux(r+1)),
     &               abs(detz3)*Dij_err_aux(r+1) )/abs(tx3(0,j))
          enddo
          do n=0,r2
            acc     = max(acc,Dij_err_aux(n)/Dmax(n))
            accr(n) = max(accr(n),Dij_err_aux(n)/Dmax(n))
          enddo

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 999
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.dacc).and.(rup.le.rupmax-1)) goto 500

999     continue

c final absolute error
        do r=0,r2
          Dmax(r) = 0d0
          i0=0
          i123 = r-i0
          do i1=0,i123
            do i2=0,i123-i1
              i3=i123-i1-i2
              Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))
            enddo
          enddo
        Dij_err_newprelim(r) = accr(r)*Dmax(r)
        D00_err_newprelim(r) = D00_err_aux(r)
        enddo

        deallocate(C0,Cuv0,C_n,Cuv_n,Sh)

        end

**********************************************************************
        subroutine Dgram2_dd(D,Duv,
     &                   Dij_err_newprelim,D00_err_newprelim,
     &                   p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                   acc,accr,detz3,detx3,tx3,f,r2,id,nid)
**********************************************************************
*       Expansion for small Gram determinant - version 2
*       Limit: |detZ| << |detX|/M^2,  max{|tX_0j|}    (M = typ. scale)
*
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       6.6.2014 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,r2,del(3,3),i(3),ii(3),n,i0,i1,i2,i3,i123
        integer rup,rupmax,step,ruplimit
        integer id,nid(0:nmax-1)
        data del/1,0,0,0,1,0,0,0,1/
        double complex, allocatable, dimension(:,:,:,:) :: C0,Cuv0
        double complex, allocatable, dimension(:,:,:) :: C_n,Cuv_n
        double complex, allocatable, dimension(:,:,:,:,:) :: Sh
        complex*16 Dstore(3,0:rmax4)
        complex*16 D(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 m02,m12,m22,m32,f(3)
        complex*16 detx3,tx3(0:3,0:3)
        real*8 Dij_err_aux(0:2*rmax4),D00_err_aux(0:2*rmax4)
        real*8 Dij_err_newprelim(0:2*rmax4),D00_err_newprelim(0:2*rmax4)
        real*8 Cij_err0(0:2*rmax3),C00_err0(0:2*rmax3),accr(0:rmax4)
        real*8 Dmax(0:rmax4)

c        dg2_calc_dd = dg2_calc_dd + 1

        do r=0,rmax4
          Dmax(r)              = abs(scalint(id))
          accr(r)              = 0d0
          Dij_err_aux(r)       = 0d0
          D00_err_aux(r)       = 0d0
          Dij_err_newprelim(r) = 0d0
          D00_err_newprelim(r) = 0d0
        enddo

        rupmax = ritmax
        if (rupmax.gt.rmax4-1) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &        'Dgram2_dd called with rmax4-1 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c initializations
c----------------
        rup = -1
        accprev1 = 1d30
        accprev2 = 1d30
        maxtx30n = max(abs(tx3(0,1)),abs(tx3(0,2)),abs(tx3(0,3)))

        step = 2
        ruplimit = r2-2 + step

c start iteration
c----------------
500     continue
        rup = rup+1

        if ((rup.eq.0).or.(rup.gt.ruplimit)) then

          if (rup.gt.ruplimit) ruplimit = min(ruplimit+step,rupmax)

          if (allocated(C0)) deallocate(C0,Cuv0,C_n,Cuv_n,Sh)
          allocate(C0(0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Cuv0(0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(C_n(0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Cuv_n(0:ruplimit,0:ruplimit,0:ruplimit))
          allocate(Sh(3,0:ruplimit,0:ruplimit,0:ruplimit,0:ruplimit))

          call C0_dd(  C0,Cuv0,p2,p3,p23,m12,m22,m32,ruplimit,nid(0))

c S functions
          do n=1,3
            if (n.eq.1) then
              call C_dd(C_n,Cuv_n,p12,p3,p4,m02,m22,m32,ruplimit,nid(1))
            elseif (n.eq.2) then
              call C_dd(C_n,Cuv_n,p1,p23,p4,m02,m12,m32,ruplimit,nid(2))
            else
              call C_dd(C_n,Cuv_n,p1,p2,p12,m02,m12,m22,ruplimit,nid(3))
            endif
            do r=0,ruplimit
            do i0=0,2*r,2
            do i1=0,r-i0/2
            do i2=0,r-i0/2-i1
            i3   = r-i0/2-i1-i2
              Sh(n,i0/2,i1,i2,i3) = -C0(i0/2,i1,i2,i3) 
              if ((n.eq.1).and.(i1.eq.0)) 
     &          Sh(1,i0/2,i1,i2,i3)=Sh(1,i0/2,i1,i2,i3)+C_n(i0/2,i2,i3)
              if ((n.eq.2).and.(i2.eq.0)) 
     &          Sh(2,i0/2,i1,i2,i3)=Sh(2,i0/2,i1,i2,i3)+C_n(i0/2,i1,i3)
              if ((n.eq.3).and.(i3.eq.0)) 
     &          Sh(3,i0/2,i1,i2,i3)=Sh(3,i0/2,i1,i2,i3)+C_n(i0/2,i1,i2)
            enddo
            enddo
            enddo
            enddo
          enddo

c Duv_{00...} from PaVe reduction -- Eq.(5.10)
          do r=0,ruplimit+1
          do i0=0,2*r,2
          do i1=0,r-i0/2
          do i2=0,r-i0/2-i1
          i3 = r-i0/2-i1-i2
            if (i0.lt.4) then
              Duv(i0/2,i1,i2,i3) = 0d0
            else
              Duv(i0/2,i1,i2,i3) = 1d0/2d0/dble(i0+i1+i2+i3-1)*( 
     &              Cuv0(i0/2-1,i1,i2,i3) + 2d0*m02*Duv(i0/2-1,i1,i2,i3)
     &            + f(1)*Duv(i0/2-1,i1+1,i2,i3) 
     &            + f(2)*Duv(i0/2-1,i1,i2+1,i3) 
     &            + f(3)*Duv(i0/2-1,i1,i2,i3+1) )
            endif
          enddo
          enddo
          enddo
          enddo

        endif

c initialize higher-rank tensors
        do i0=0,2*rup+2,2
        do i1=0,rup+1-i0/2
        do i2=0,rup+1-i0/2-i1
        i3   = rup+1-i0/2-i1-i2
          D(i0/2,i1,i2,i3) = 0d0
        enddo
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,r2
            Dstore(1,n) = D(0,n,0,0) 
            Dstore(2,n) = D(0,0,n,0) 
            Dstore(3,n) = D(0,0,0,n) 
          enddo
        endif

c iteration: rup,  step: r
        do r=rup,0,-1

c reduction -- Eq.(5.14)
        Dmax(r) = 0d0
        do i0=2*r,0,-2
        do i1=0,r-i0/2
        do i2=0,r-i0/2-i1
        i3   = r-i0/2-i1-i2
        i(1) = i1
        i(2) = i2
        i(3) = i3
          D(i0/2,i1,i2,i3) = detz3/detx3*( 
     &                      2*(i0+i1+i2+i3+1)*D(i0/2+1,i1,i2,i3)  
     &                      - 4d0*Duv(i0/2+1,i1,i2,i3) 
     &                      - C0(i0/2,i1,i2,i3) )
          do n=1,3
            ii(1) = i1-del(n,1)
            ii(2) = i2-del(n,2)
            ii(3) = i3-del(n,3)
            D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &           + tx3(0,n)/detx3*Sh(n,i0/2,i1,i2,i3)
            if (i(n).gt.0) then
              D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3)
     &           - 2d0*tx3(0,n)/detx3*i(n)*D(i0/2+1,ii(1),ii(2),ii(3))
            endif
          enddo
        if (i0.eq.0) Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))

        end do
        end do
        end do
        enddo

c estimate precision from last improvements
        acc = 1d30
        if (rup.ge.r2) then
          acc = 0d0
c         acc = abs(D(0,0,0,0)/scalint(id)-1d0)
          do n=0,r2
            acc = max(acc,abs(Dstore(1,n)-D(0,n,0,0))/Dmax(n))
            acc = max(acc,abs(Dstore(2,n)-D(0,0,n,0))/Dmax(n))
            acc = max(acc,abs(Dstore(3,n)-D(0,0,0,n))/Dmax(n))
            accr(n) = acc
          enddo
        endif

c error propagation for Gram expansion
          do r=0,rup
            Cij_err0(r) = 
     &        max(Cij_err(tid(nid(0)),r),Cij_err(tid(nid(1)),r),
     &            Cij_err(tid(nid(2)),r),Cij_err(tid(nid(3)),r)) 
            C00_err0(r) = 
     &        max(C00_err(tid(nid(0)),r),C00_err(tid(nid(1)),r),
     &            C00_err(tid(nid(2)),r),C00_err(tid(nid(3)),r)) 
          enddo

        do r=rup,0,-1
          D00_err_aux(r) = 
     &             abs(detz3/detx3)*(C00_err0(r))
     &           + maxtx30n/abs(detx3)*(C00_err0(r))
          Dij_err_aux(r) = 
     &             abs(detz3/detx3)*(D00_err_aux(r+2)+Cij_err0(r) )
     &           + maxtx30n/abs(detx3)*(Cij_err0(r)+D00_err_aux(r+1))
          enddo
          do n=0,r2
            acc     = max(acc,Dij_err_aux(n)/Dmax(n))
            accr(n) = max(accr(n),Dij_err_aux(n)/Dmax(n))
          enddo

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 999
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary
        if ((acc.gt.dacc).and.(rup.le.rupmax-1)) goto 500

999     continue
c final absolute error
        do r=0,r2
          Dmax(r) = 0d0
          i0=0
          i123 = r-i0
          do i1=0,i123
            do i2=0,i123-i1
              i3=i123-i1-i2
              Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))
            enddo
          enddo
        Dij_err_newprelim(r) = accr(r)*Dmax(r)
        D00_err_newprelim(r) = D00_err_aux(r)
        enddo

        deallocate(C0,Cuv0,C_n,Cuv_n,Sh)

        end

**********************************************************************
        subroutine Dgramcayley_dd(D,Duv,
     &               Dij_err_newprelim,D00_err_newprelim,
     &               p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &               i,j,k,l,acc,accr,detz3,tz3,ttz3,tx3,f,
     &               r2,id,nid)
**********************************************************************
*       Expansion for small Gram and Cayley determinants
*
*       4-point coefficients  
*       D(i,j,k,l) = D_{0...01...12...23...3}(p1,...,m32)
*                       \___/\___/\___/\___/
*                        2i    j    k    l  indices
*       of rank r=i+j+k+l with r <= r2
*
*       Duv(i,j,k,l) = coeff of 1/eps in D(i,j,k,l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        use DD_3pt
        use DD_statistics
        implicit real*8 (a-z)

c local variables
        integer r,rr,rup,rupmax,r2,del(3,3),i,h(3),j,k,l,m,m1,m2,n
        integer id,nid(0:nmax-1),i0,i1,i2,i3,il,i123,step,ruplimit
        data del/1,0,0,0,1,0,0,0,1/
        double complex, allocatable, dimension(:,:,:,:) :: C0,Cuv0
        double complex, allocatable, dimension(:,:,:) :: C_n,Cuv_n
        double complex, allocatable, dimension(:,:,:,:,:) :: Sh
        complex*16 Dstore(3,0:rmax4)
        complex*16 D(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 Duv(0:rmax4,0:rmax4,0:rmax4,0:rmax4)
        complex*16 m02,m12,m22,m32,f(3)
        complex*16 tx3(0:3,0:3)
        real*8 tz3(3,3),ttz3(3,3,3,3)
        real*8 Dij_err_aux(0:2*rmax4),D00_err_aux(0:2*rmax4)
        real*8 Dij_err_newprelim(0:2*rmax4),D00_err_newprelim(0:2*rmax4)
        real*8 Cij_err0(0:2*rmax3),C00_err0(0:2*rmax3),accr(0:rmax4)
        real*8 Dmax(0:rmax4)

c        dgc_calc_dd = dgc_calc_dd + 1

        do r=0,rmax4
          Dmax(r)              = abs(scalint(id))
          accr(r)              = 0d0
          Dij_err_aux(r)       = 0d0
          D00_err_aux(r)       = 0d0
          Dij_err_newprelim(r) = 0d0
          D00_err_newprelim(r) = 0d0
        enddo

        rupmax = ritmax
        if (rupmax.gt.rmax4-3) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &        'Dgramcayley_dd called with rmax4-3 < ritmax = ',ritmax
            write(outchannel,*) 'This is fatal. Fix it!'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

c initializations
c----------------
        rup = -2
        D(0,0,0,0)   = 0d0
        D(0,1,0,0)   = 0d0
        D(0,0,1,0)   = 0d0
        D(0,0,0,1)   = 0d0
        accprev1 = 1d30
        accprev2 = 1d30

        step = 2
        ruplimit = r2-2 + step

c start iteration
c----------------
500     continue
        rup = rup+2

        if ((rup.eq.0).or.(rup.gt.ruplimit)) then

          if (rup.gt.ruplimit) ruplimit = min(ruplimit+step,rupmax)

          if (allocated(C0)) deallocate(C0,Cuv0,C_n,Cuv_n,Sh)
          allocate(C0(0:ruplimit+2,0:ruplimit+2,
     &                0:ruplimit+2,0:ruplimit+2))
          allocate(Cuv0(0:ruplimit+2,0:ruplimit+2,
     &                  0:ruplimit+2,0:ruplimit+2))
          allocate(C_n(0:ruplimit+2,0:ruplimit+2,0:ruplimit+2))
          allocate(Cuv_n(0:ruplimit+2,0:ruplimit+2,0:ruplimit+2))
          allocate(Sh(3,0:ruplimit+2,0:ruplimit+2,
     &                  0:ruplimit+2,0:ruplimit+2))

          call C0_dd(C0,Cuv0,p2,p3,p23,m12,m22,m32,ruplimit+2,nid(0))

c S functions
          do n=1,3
            if (n.eq.1) then
              call C_dd(C_n,Cuv_n,p12,p3,p4,m02,m22,m32,
     &                  ruplimit+2,nid(1))
            elseif (n.eq.2) then
              call C_dd(C_n,Cuv_n,p1,p23,p4,m02,m12,m32,
     &                  ruplimit+2,nid(2))
            else
              call C_dd(C_n,Cuv_n,p1,p2,p12,m02,m12,m22,
     &                  ruplimit+2,nid(3))
            endif
            do r=0,ruplimit+2
            do i0=0,r,2
            do i1=0,r-i0
            do i2=0,r-i0-i1
            i3 = r-i0-i1-i2
              Sh(n,i0/2,i1,i2,i3) = -C0(i0/2,i1,i2,i3) 
              if ((n.eq.1).and.(i1.eq.0)) 
     &          Sh(1,i0/2,i1,i2,i3)=Sh(1,i0/2,i1,i2,i3)+C_n(i0/2,i2,i3)
              if ((n.eq.2).and.(i2.eq.0)) 
     &          Sh(2,i0/2,i1,i2,i3)=Sh(2,i0/2,i1,i2,i3)+C_n(i0/2,i1,i3)
              if ((n.eq.3).and.(i3.eq.0)) 
     &          Sh(3,i0/2,i1,i2,i3)=Sh(3,i0/2,i1,i2,i3)+C_n(i0/2,i1,i2)
            enddo
            enddo
            enddo
            enddo
          enddo

c error propagation for Gram expansion
          do r=0,ruplimit+2
            Cij_err0(r) = 
     &        max(Cij_err(tid(nid(0)),r),Cij_err(tid(nid(1)),r),
     &            Cij_err(tid(nid(2)),r),Cij_err(tid(nid(3)),r)) 
            C00_err0(r) = 
     &        max(C00_err(tid(nid(0)),r),C00_err(tid(nid(1)),r),
     &            C00_err(tid(nid(2)),r),C00_err(tid(nid(3)),r)) 
          enddo

c Duv_{00...} from PaVe reduction -- Eq.(5.10)
          do r=0,ruplimit+3
          do i0=0,r,2
          do i1=0,r-i0
          do i2=0,r-i0-i1
          i3 = r-i0-i1-i2
            if (i0.le.2) then
              Duv(i0/2,i1,i2,i3) = 0d0
            else
              Duv(i0/2,i1,i2,i3) = 1d0/2d0/dble(r-1)*( 
     &          Cuv0(i0/2-1,i1,i2,i3) + 2d0*m02*Duv(i0/2-1,i1,i2,i3)
     &        + f(1)*Duv(i0/2-1,i1+1,i2,i3) 
     &        + f(2)*Duv(i0/2-1,i1,i2+1,i3) 
     &        + f(3)*Duv(i0/2-1,i1,i2,i3+1) )
            endif
          enddo
          enddo
          enddo
          enddo

        endif

c initialize higher-rank tensors
        do r=rup+2,rup+3
        do i0=0,r,2
        do i1=0,r-i0
        do i2=0,r-i0-i1
        i3 = r-i0-i1-i2
          D(i0/2,i1,i2,i3)   = 0d0
        enddo
        enddo
        enddo
        enddo

c store previous approximation for error estimate
        if (rup.ge.r2) then
          do n=0,r2
            Dstore(1,n) = D(0,n,0,0) 
            Dstore(2,n) = D(0,0,n,0) 
            Dstore(3,n) = D(0,0,0,n) 
          enddo
        endif

c iteration: rup/2,  step: rr/2
        do rr=rup,0,-2
        do r=rr,min(rr+1,rupmax)

c D_{00...} from iteration Eq.(5.40)
        m1 = mod(l  ,3)+1
        m2 = mod(l+1,3)+1
        do i0=2,r+2,2
        do il=r+2-i0,0,-1
        do i1=0,r+2-il-i0
        i2=r+2-il-i0-i1
        h(l)  = il
        h(m1) = i1
        h(m2) = i2

c coefficients D_{00...} from Eq.(5.49)
          D(i0/2,h(1),h(2),h(3)) = 1d0/2d0/dble(il+1)/tz3(k,l)*(
     &      tx3(k,0)*D(i0/2-1,h(1)+del(1,l),h(2)+del(2,l),h(3)+del(3,l))
     &      - detz3*D(i0/2-1,h(1)+del(1,l)+del(1,k),
     &                h(2)+del(2,l)+del(2,k),h(3)+del(3,l)+del(3,k)) )
          do n=1,3
            D(i0/2,h(1),h(2),h(3)) = D(i0/2,h(1),h(2),h(3))
     &        + 1d0/2d0/dble(il+1)/tz3(k,l)*tz3(k,n)
     &          *Sh(n,i0/2-1,h(1)+del(1,l),h(2)+del(2,l),h(3)+del(3,l))
          enddo
          if (h(m1).gt.0) D(i0/2,h(1),h(2),h(3))= D(i0/2,h(1),h(2),h(3))
     &      - 1d0/dble(il+1)/tz3(k,l)*h(m1)*tz3(k,m1)
     &          *D(i0/2,h(1)-del(1,m1)+del(1,l),h(2)-del(2,m1)+del(2,l),
     &                h(3)-del(3,m1)+del(3,l))
          if (h(m2).gt.0) D(i0/2,h(1),h(2),h(3))= D(i0/2,h(1),h(2),h(3))
     &      - 1d0/dble(il+1)/tz3(k,l)*h(m2)*tz3(k,m2)
     &          *D(i0/2,h(1)-del(1,m2)+del(1,l),h(2)-del(2,m2)+del(2,l),
     &                h(3)-del(3,m2)+del(3,l))
        end do
        end do
        end do

c coefficients D(0,i1,i2,i3) from Eq.(5.53)
        Dmax(r) = 0d0
        i0 = 0
        do i1=0,r
        do i2=0,r-i0-i1
        i3 = r-i0-i1-i2
        h(1) = i1
        h(2) = i2
        h(3) = i3
          D(i0/2,i1,i2,i3) = 1d0/tx3(i,j)*(
     &        tz3(i,j)*( 2d0*(1+r)*D(i0/2+1,i1,i2,i3) 
     &                  - 4d0*Duv(i0/2+1,i1,i2,i3) - C0(i0/2,i1,i2,i3) )
     &      + tx3(0,j)*D(i0/2,i1+del(1,i),i2+del(2,i),i3+del(3,i))    )
        do n=1,3
        do m=1,3
        if ((i.eq.n).or.(j.eq.m)) cycle
          D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &      + ttz3(i,n,j,m)/tx3(i,j)*f(n)*Sh(m,i0/2,i1,i2,i3)
          if (h(m).gt.0) D(i0/2,i1,i2,i3) = D(i0/2,i1,i2,i3) 
     &          - 2d0*h(m)*ttz3(i,n,j,m)*f(n)/tx3(i,j)
     &            *D(i0/2+1,i1-del(1,m),i2-del(2,m),i3-del(3,m))
        end do
        end do
        Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))
        end do
        end do

        end do
        end do

c estimate precision from last improvements
        acc = 1d30
        if (rup.ge.r2) then
c         acc = abs(D(0,0,0,0)/scalint(id)-1d0)
          acc = 0d0
          do n=0,r2
            acc = max(acc,abs(Dstore(1,n)-D(0,n,0,0))/Dmax(n))
            acc = max(acc,abs(Dstore(2,n)-D(0,0,n,0))/Dmax(n))
            acc = max(acc,abs(Dstore(3,n)-D(0,0,0,n))/Dmax(n))
            accr(n) = acc
          enddo
        endif

          do r=rup,0,-1
            D00_err_aux(r+2) = max( Cij_err0(r+1),
     &        abs(tx3(k,0))/abs(tz3(k,l))*Dij_err_aux(r+1),
     &        abs(detz3)/abs(tz3(k,l))*Dij_err_aux(r+2) )
            Dij_err_aux(r) = max(
     &        abs(tx3(0,j))*Dij_err_aux(r+1),
     &        abs(tz3(i,j))*max(D00_err_aux(r+2),Cij_err0(r)),
     &        maxttx0ijm(id)*max(Cij_err0(r),D00_err_aux(r+1))
     &                             )/abs(tx3(i,j))
          enddo
          do n=0,r2
            acc     = max(acc,Dij_err_aux(n)/Dmax(n))
            accr(n) = max(accr(n),Dij_err_aux(n)/Dmax(n))
          enddo

c stop if accuracy becomes worse
        if ((rup.ge.r2+3).and.(acc.gt.accprev1).and.
     &      (accprev1.gt.accprev2)) goto 999
        accprev2 = accprev1
        accprev1 = acc

c repeat iteration if necessary 
        if ((acc.gt.dacc).and.(rup.le.rupmax-2)) goto 500

999     continue
c final absolute error
        do r=0,r2
          Dmax(r) = 0d0
          i0=0
          i123 = r-i0
          do i1=0,i123
            do i2=0,i123-i1
              i3=i123-i1-i2
              Dmax(r) = max(Dmax(r),abs(D(i0/2,i1,i2,i3)))
            enddo
          enddo
        Dij_err_newprelim(r) = accr(r)*Dmax(r)
        D00_err_newprelim(r) = D00_err_aux(r)
        enddo

        deallocate(C0,Cuv0,C_n,Cuv_n,Sh)

        end

**********************************************************************
        subroutine D0_dd(D0,Duv0,p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,
     &                   r2,id)
**********************************************************************
*       4-point coefficients D(0)_{...} with unshifted momentum
*---------------------------------------------------------------------
*       10.9.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)

c local variables
        integer r2,r2max,r,i0,i1,i2,i3,i4,i1234,id
        complex*16 D(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv(0:r2,0:r2,0:r2,0:r2)
        complex*16 D0(0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv0(0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 m02,m12,m22,m32

        call D_dd(D,Duv,p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,r2,id)

        r2max = max(r2,0)

        do r=0,r2max
          do i0=0,r+1,2
          i1234 = r-max(i0/2,i0-1)
          i1=0
          do i2=0,i1234-i1
          do i3=0,i1234-i1-i2
            i4 = i1234-i1-i2-i3
            D0(i0/2,0,i2,i3,i4)   = D(i0/2,i2,i3,i4)
            Duv0(i0/2,0,i2,i3,i4) = Duv(i0/2,i2,i3,i4)
          end do
          end do
          do i1=1,i1234
          do i2=0,i1234-i1
          do i3=0,i1234-i1-i2
            i4 = i1234-i1-i2-i3
            D0(i0/2,i1,i2,i3,i4)   = -D0(i0/2,i1-1,i2,i3,i4)
     &         - D0(i0/2,i1-1,i2+1,i3,i4) - D0(i0/2,i1-1,i2,i3+1,i4)
     &         - D0(i0/2,i1-1,i2,i3,i4+1)
            Duv0(i0/2,i1,i2,i3,i4) = -Duv0(i0/2,i1-1,i2,i3,i4)
     &         - Duv0(i0/2,i1-1,i2+1,i3,i4) - Duv0(i0/2,i1-1,i2,i3+1,i4)
     &         - Duv0(i0/2,i1-1,i2,i3,i4+1)
        end do
        end do
        end do
        end do
        end do

        end

**********************************************************************
        function D0dd(q1,q2,q3,q4,q12,q23,m02,m12,m22,m32,ext)
**********************************************************************
*       scalar 4-point function
*
*             q1 \           / q4
*                 \   m02   /
*                  *-------*
*                  |       |
*              m12 |       | m32
*                  |       |
*                  *-------*
*                 /   m22   \
*             q2 /           \ q3
*---------------------------------------------------------------------
*       10.3.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)

        complex*16 D0dd,D0ir_dd,D0coll_dd,D0mmmmzero_dd,D0mmzero_dd
        complex*16 D0mzero_dd,D0massive_dd
        complex*16 m02,m12,m22,m32,m2(0:3)
        real*8 p2(0:3,0:3)
        integer ext,ext0,i,j,k,ip,im,perm,nsoft,ncoll
        logical smallp2(0:3,0:3),smallm2(0:3)
        logical coll(0:3,0:3),soft(0:3,0:3,0:3),onsh(0:3,0:3)

        ext0 = ext

        p2(0,1) = q1
        p2(1,2) = q2
        p2(2,3) = q3
        p2(0,3) = q4
        p2(0,2) = q12
        p2(1,3) = q23
        m2(0) = m02
        m2(1) = m12
        m2(2) = m22
        m2(3) = m32

        perm = 1
50      continue
c determine small parameters
        do i=0,2 
        do j=i+1,3 
          p2(j,i) = p2(i,j)
          smallp2(i,j) = (abs(p2(i,j)).lt.1d-15)
          smallp2(j,i) = smallp2(i,j)
        enddo
        enddo
        do i=0,3 
          smallm2(i) = (abs(m2(i)).lt.1d-15)
        enddo

c determine on-shell momenta
        do i=0,2
        do j=i+1,3
          onsh(i,j) = dcmplx(p2(i,j)).eq.m2(j)
          onsh(j,i) = dcmplx(p2(j,i)).eq.m2(i)
        enddo
        enddo
c determine collinear singularities
        ncoll = 0
        do i=0,2 
        do j=i+1,3 
          coll(i,j) = smallp2(i,j).and.smallm2(i).and.smallm2(j)
          if (coll(i,j).and.(.not.
     &        (((p2(i,j).eq.0d0).and.(m2(i).eq.m2(j))).or.
     &        (onsh(j,i).and.(m2(j).eq.(0d0,0d0))).or.
     &        (onsh(i,j).and.(m2(i).eq.(0d0,0d0))) ))) then
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 
     &        'D0dd: structure of collinear singularity not supported:'
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
          coll(j,i) = coll(i,j) 
          if (coll(i,j)) ncoll = ncoll + 1
        enddo
        enddo
c determine soft singularities
        nsoft = 0
        do i=0,3 
        do j=0,2 
          ip = mod(i+1+j,4)
          im = mod(i+1+mod(j+1,3),4)
          soft(i,ip,im) = (abs(m2(i)).lt.1d-18).and.
     &                    onsh(i,ip).and.onsh(i,im)
          soft(i,im,ip) = soft(i,ip,im) 
          if (soft(i,ip,im)) nsoft = nsoft + 1
        enddo
        enddo

c canonical ordering of propagators
c     -> only coll(i,i+1) and sing(j,j-1,j+1) relevant;
c        neither p2(0,2) nor p2(1,3) small
        if (smallp2(0,2).or.smallp2(1,3).or.onsh(0,2).or.onsh(2,0).or.
     &      onsh(1,3).or.onsh(3,1)) goto 60
        do k=0,3 
          i = mod(k+2,4)
          j = mod(k+1,4)
          if (soft(k,i,j)) goto 60
          j = mod(k+3,4)
          if (soft(k,i,j)) goto 60
        enddo
        
        goto 70

60      continue
c try next permutation of propagators

        if (perm.gt.24) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0dd: singularity structure not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
          D0dd = 0d0
          return
        endif
        perm = perm+1
        call D0args(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &              p2(0,2),p2(1,3),m2(0),m2(1),m2(2),m2(3),perm)
        goto 50
70      continue

c soft singular cases  (with standard ordering of soft sings.)
c--------------------
        if ( (nsoft.eq.4).or.
     &      ((nsoft.eq.3).and.(.not.(soft(3,0,2)))).or.
     &      ((nsoft.eq.2).and.soft(0,1,3).and.(.not.(soft(3,0,2)))).or.
     &      ((nsoft.eq.1).and.soft(0,1,3))) then
          D0dd = D0ir_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                   p2(0,2),p2(1,3),m2(0),m2(2))
          return
        elseif (((nsoft.eq.3).and.(.not.(soft(0,1,3)))).or.
     &      ((nsoft.eq.2).and.soft(1,2,0).and.(.not.(soft(0,1,3)))).or.
     &      ((nsoft.eq.1).and.soft(1,2,0))) then
          D0dd = D0ir_dd(p2(1,2),p2(2,3),p2(0,3),p2(0,1),
     &                   p2(1,3),p2(0,2),m2(1),m2(3))
          return
        elseif (((nsoft.eq.3).and.(.not.(soft(1,2,0)))).or.
     &      ((nsoft.eq.2).and.soft(2,3,1).and.(.not.(soft(1,2,0)))).or.
     &      ((nsoft.eq.1).and.soft(2,3,1))) then
          D0dd = D0ir_dd(p2(2,3),p2(0,3),p2(0,1),p2(1,2),
     &                   p2(0,2),p2(1,3),m2(2),m2(0))
          return
        elseif (((nsoft.eq.3).and.(.not.(soft(2,3,1)))).or.
     &      ((nsoft.eq.2).and.soft(3,0,2).and.(.not.(soft(2,3,1)))).or.
     &      ((nsoft.eq.1).and.soft(3,0,2))) then
          D0dd = D0ir_dd(p2(0,3),p2(0,1),p2(1,2),p2(2,3),
     &                   p2(1,3),p2(0,2),m2(3),m2(1))
          return
c coll. singular cases  (with standard ordering of coll. sings.)
c---------------------
        elseif ( (ncoll.eq.4).or.
     &           ((ncoll.eq.3).and.(.not.coll(3,0))).or.
     &           ((ncoll.eq.2).and.coll(0,1).and.coll(1,2)).or.
     &           ((ncoll.eq.2).and.coll(0,1).and.coll(2,3)).or.
     &           ((ncoll.eq.1).and.coll(0,1)) ) then
          D0dd = D0coll_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                     p2(0,2),p2(1,3),m2(0),m2(1),m2(2),m2(3))
          return
        elseif ( ((ncoll.eq.3).and.(.not.coll(0,1))).or.
     &           ((ncoll.eq.2).and.coll(1,2).and.coll(2,3)).or.
     &           ((ncoll.eq.2).and.coll(1,2).and.coll(3,0)).or.
     &           ((ncoll.eq.1).and.coll(1,2)) ) then
          D0dd = D0coll_dd(p2(1,2),p2(2,3),p2(0,3),p2(0,1),
     &                     p2(1,3),p2(0,2),m2(1),m2(2),m2(3),m2(0))
          return
        elseif ( ((ncoll.eq.3).and.(.not.coll(1,2))).or.
     &           ((ncoll.eq.2).and.coll(2,3).and.coll(3,0)).or.
     &           ((ncoll.eq.2).and.coll(2,3).and.coll(0,1)).or.
     &           ((ncoll.eq.1).and.coll(2,3)) ) then
          D0dd = D0coll_dd(p2(2,3),p2(0,3),p2(0,1),p2(1,2),
     &                     p2(0,2),p2(1,3),m2(2),m2(3),m2(0),m2(1))
          return
        elseif ( ((ncoll.eq.3).and.(.not.coll(2,3))).or.
     &           ((ncoll.eq.2).and.coll(3,0).and.coll(0,1)).or.
     &           ((ncoll.eq.2).and.coll(3,0).and.coll(1,2)).or.
     &           ((ncoll.eq.1).and.coll(3,0)) ) then
          D0dd = D0coll_dd(p2(0,3),p2(0,1),p2(1,2),p2(2,3),
     &                     p2(1,3),p2(0,2),m2(3),m2(0),m2(1),m2(2))

          return
        endif

c regular cases
c--------------
        do i=0,2 
        do j=i+1,3 
          if (smallp2(i,j)) then
            p2(i,j) = 0d0
            p2(j,i) = 0d0
          endif
        enddo
        enddo
        do i=0,3 
          if (smallm2(i)) m2(i) = 0d0
        enddo

c 4 internal masses zero
        if (smallm2(0).and.smallm2(1).and.smallm2(2).and.
     &          smallm2(3)) then
          D0dd = D0mmmmzero_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                         p2(0,2),p2(1,3))
          return
c at least 2 internal masses zero
        elseif (smallm2(0).and.smallm2(1)) then
          D0dd = D0mmzero_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                       p2(0,2),p2(1,3),m2(2),m2(3))
          return
        elseif (smallm2(1).and.smallm2(2)) then
          D0dd = D0mmzero_dd(p2(1,2),p2(2,3),p2(0,3),p2(0,1),
     &                       p2(1,3),p2(0,2),m2(3),m2(0))
          return
        elseif (smallm2(2).and.smallm2(3)) then
          D0dd = D0mmzero_dd(p2(2,3),p2(0,3),p2(0,1),p2(1,2),
     &                       p2(0,2),p2(1,3),m2(0),m2(1))
          return
        elseif (smallm2(3).and.smallm2(0)) then
          D0dd = D0mmzero_dd(p2(0,3),p2(0,1),p2(1,2),p2(2,3),
     &                       p2(1,3),p2(0,2),m2(1),m2(2))
          return
        elseif (smallm2(0).and.smallm2(2)) then
          D0dd = D0mmzero_dd(p2(0,2),p2(1,2),p2(1,3),p2(0,3),
     &                       p2(0,1),p2(2,3),m2(1),m2(3))
          return
        elseif (smallm2(1).and.smallm2(3)) then
          D0dd = D0mmzero_dd(p2(1,3),p2(1,2),p2(0,2),p2(0,3),
     &                       p2(2,3),p2(0,1),m2(2),m2(0))
          return
c 1 internal mass zero
        elseif (smallm2(0)) then
          D0dd = D0mzero_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                       p2(0,2),p2(1,3),m2(1),m2(2),m2(3))
          return
        elseif (smallm2(1)) then
          D0dd = D0mzero_dd(p2(1,2),p2(2,3),p2(0,3),p2(0,1),
     &                       p2(1,3),p2(0,2),m2(2),m2(3),m2(0))
          return
        elseif (smallm2(2)) then
          D0dd = D0mzero_dd(p2(2,3),p2(0,3),p2(0,1),p2(1,2),
     &                       p2(0,2),p2(1,3),m2(3),m2(0),m2(1))
          return
        elseif (smallm2(3)) then
          D0dd = D0mzero_dd(p2(0,3),p2(0,1),p2(1,2),p2(2,3),
     &                       p2(1,3),p2(0,2),m2(0),m2(1),m2(2))
          return
        else
c no internal mass zero
          D0dd = D0massive_dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                        p2(0,2),p2(1,3),m2(0),m2(1),m2(2),m2(3))
        endif

        end

************************************************************************
        function D0ir_dd(p1,p2,p3,p4,p12,p23,xm02,xm32)
************************************************************************
*       soft-singular D0 function 
*
*             p1 \   xm02    / p4          Note:
*                 \  =small /              xm02 can only be non-zero
*                  *-------*               if p1,p4 not small
*                  |       |
*         p1 = m22 |       | m42 = p4
*                  |       |
*                  *-------*
*                 /  xm32   \
*             p2 /           \ p3
*
*       further soft singularities allowed, but must be ordered
*       with increasing labels of masses
*       
*       mass reg.: result of Beenakker/Denner NPB338 (1990) 349
*       dim. reg.: translation via S.D. NPB675 (2003) 447 
*                  + special cases
*-----------------------------------------------------------------------
*       7.2.07 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         

c local variables
        complex*16 D0ir_dd,cspen_dd,Kir,eta_dd,eta3,a1,a2,a3
        complex*16 m22,m32,m42,m2,m3,m4,xm02,xm32
        complex*16 cq12,cq23,cq2,cq3,xs,x2,x3,y,ieps,pre,res
        complex*16 cmp2,cm2,div,div2
        integer qxs,qx2,qx3,qm2,qm4,qy

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))
        eta3(a1,a2,a3) = eta_dd(a1*a2,a3) + eta_dd(a1,a2)

        eps  = 1d-20
        ieps = dcmplx(0d0,eps)
        pi   = 4d0*datan(1d0)

        if (((xm02.ne.(0d0,0d0)).and.((p1.lt.1d-18).or.(p4.lt.1d-18)))
     &      .or.(abs(xm02).gt.1d-18)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0ir_dd: inconsistent choice of xm02:'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        if ((p2.ne.0d0).and.(dcmplx(p2).eq.xm32).and.(p1.eq.0d0).and.
     &      (p4.eq.0d0)) then
          q1  = 0d0
          q2  = 0d0
          q3  = p3
          q4  = p2
          q12 = p23
          q23 = p12
          m22 = 0d0
          m32 = 0d0
          m42 = p2
        elseif ((p4.eq.0d0).and.(p1.ne.0d0)) then
          q1  = p4
          q2  = p3
          q3  = p2
          q4  = p1
          q12 = p12
          q23 = p23
          m22 = p4
          m32 = xm32
          m42 = p1
        else
          q1  = p1
          q2  = p2
          q3  = p3
          q4  = p4
          q12 = p12
          q23 = p23
          m22 = p1
          m32 = xm32
          m42 = p4
        endif

        cq2  = q2+abs(q2)*ieps*1.7d0
        cq3  = q3+abs(q3)*ieps*1.3d0
        cq12 = q12+abs(q12)*ieps
        cq23 = q23+abs(q23)*ieps*2.1d0

        D0ir_dd = 0d0

c soft divergent D0's with coll. sings. in dim. reg.
c===================================================
        if (q1.eq.0d0) then
c 4 soft singularities  =  fully massless case
c---------------------------------------------
*            0\  0  /0
*              *---*
*             0|   |0
*              *---*
*            0/  0  \0
c---------------------------------------------
          if ((q2.eq.0d0).and.(q3.eq.0d0).and.(q4.eq.0d0).and.
     &        (m32.eq.dcmplx(0d0))) then
            D0ir_dd = 2d0/q12/q23*( 2d0*delta2ir
     &        + delta1ir*(log(-mir2/cq12)+log(-mir2/cq23))
     &        + log(-mir2/cq12)*log(-mir2/cq23)-5d0/6d0*pi**2 )
            return
c 3 soft singularities
c---------------------------------------------
*            0\  0  /q4
*              *---*
*             0|   | m42=q4
*              *---*           q4 can be small, 
*            0/  0  \q3=q4     but q4=/=0
c---------------------------------------------
          elseif ((q2.eq.0d0).and.(q3.eq.q4).and.
     &            (m32.eq.dcmplx(0d0))) then
            if (q4.lt.1d-18) m42 = rmp2(q4)
            D0ir_dd = ( 2d0*delta2ir
     &        +2d0*delta1ir*log(-mir2/cq12)+log(-mir2/cq12)**2
     &        +(delta1ir+log(-mir2/cq12))
     &         *(log(-cq12/m42)+2d0*log(m42/(q4-cq23)))
     &        -5d0/6d0*pi**2 )/q12/(q23-q4)
            return
c 2 soft singularities
          elseif (dcmplx(q2).eq.m32) then
c---------------------------------------------
*         0\  0  /q4
*           *---*
*          0|   | m42=q4
*           *---*        
*        q2/ m32 \q3       q2,q4 can be small, 
*            =q2           but q2,q4=/=0
c---------------------------------------------
            if ((q2.ne.0d0).and.(dcmplx(q2).eq.m32).and.
     &          (q4.ne.0d0)) then
              x3   = -Kir( q3,m42,m32,qx3)
              if (q4.lt.1d-18) m42 = rmp2(q4)
              if (q2.lt.1d-18) m32 = rmp2(q2)
              D0ir_dd = ( delta2ir 
     &          + delta1ir/2d0*(log(mir2/m32)+log(mir2/m42))
     &          + (log(mir2/m32)**2+log(mir2/m42)**2)/4d0
     &          + (delta1ir+log(mir2/(q4-cq23)))*log(m32/(q2-cq12))
     &          + (delta1ir+log(mir2/(q2-cq12)))*log(m42/(q4-cq23))
     &          - 2d0*pi**2/3d0 - log(x3)**2 - log(m32/m42)**2/4d0 
     &         )/(q12-q2)/(q23-q4) 
              return
c---------------------------------------------
*         0\  0  /q4
*           *---*
*          0|   | m42=q4      q4 can be small, 
*           *---*             but q4=/=0,
*         0/  0  \q3          and q3=/=q4
c---------------------------------------------
            elseif ((q2.eq.0d0).and.(m32.eq.dcmplx(0d0)).and.
     &              (q3.ne.q4).and.(q4.ne.0d0)) then
              if (q4.lt.1d-18) m42 = rmp2(q4)
              D0ir_dd = ( 3d0/2d0*( delta2ir 
     &            +delta1ir*log(mir2/m42)+log(mir2/m42)**2/2d0 )
     &          + (delta1ir+log(mir2/m42))
     &            *(log((cq3-q4)/cq12)-2d0*log((q4-cq23)/m42))
     &          -2d0*cspen_dd((cq3-cq23)/(q4-cq23))-2d0*pi**2/3d0
     &          +2d0*log(-cq12/m42)*log((q4-cq23)/m42)
     &          -log((q4-cq3)/m42)**2 )/q12/(q23-q4)
              return
c---------------------------------------------
*         0\  0  /0
*           *---*
*          0|   |0            
*           *---*             
*         0/  0  \q3          q3=/=0
c---------------------------------------------
            elseif ((q2.eq.0d0).and.(m32.eq.dcmplx(0d0)).and.
     &              (q3.ne.0d0).and.(q4.eq.0d0)) then
              D0ir_dd = 2d0/q12/q23*( delta2ir 
     &            +delta1ir*log(-mir2/cq3)+log(-mir2/cq3)**2/2d0
     &            +(delta1ir+log(-mir2/cq3))
     &              *(log(cq3/cq12)+log(cq3/cq23))
     &            +log(cq3/cq12)*log(cq3/cq23)-pi**2/3d0
     &            -cspen_dd(1d0-cq3/cq23)-cspen_dd(1d0-cq3/cq12) )
              return
            endif
c 1 soft singularity
c---------------------------------------------
*         0\  0  /q4
*           *---*
*          0|   | m42=q4      
*           *---*             q4 can be small, 
*        q2/ m32 \q3          but q4=/=0
*
*    Note: calculated from [q1=small->MIR2] + difference from dim. reg.
c---------------------------------------------
          elseif ((abs(q2)+abs(m32).gt.1d-18).and.
     &            (dcmplx(q2).ne.m32).and.(q4.ne.0d0) ) then
            D0ir_dd = ( delta2ir/2d0 
     &        +delta1ir*log(mir2/MIR2)/2d0+log(mir2/MIR2)**2/4d0
     &        +(delta1ir+log(mir2/MIR2))*log((m32-cq2)/(m32-cq12))
     &        -2d0*cspen_dd((cq2-cq12)/(m32-cq12)) )/(q12-m32)/(q23-m42)
            goto 100
c---------------------------------------------
*         0\  0  /0 
*           *---*
*          0|   |0            
*           *---*             
*        q2/ m32 \q3          
c---------------------------------------------
          elseif ((abs(q2)+abs(m32).gt.1d-18).and.(dcmplx(q2).ne.m32)
     &              .and.(abs(q3)+abs(m32).gt.1d-18).and.
     &              (dcmplx(q3).ne.m32).and.(q4.eq.0d0)) then
            if (abs(m32).gt.1d-18) then
              xs  = -m32/cq23
              x2  = m32/(m32-cq2)
              x3  = m32/(m32-cq3)
              res = cspen_dd(1d0-x2*x3/xs)
     &              +eta3(1d0/xs,x2,x3)*log(1d0-x2*x3/xs)
            else
              res = pi**2/6d0
            endif
            D0ir_dd = ( res + delta2ir + log(-cq23/mir2)**2/2d0
     &          +delta1ir*log(-mir2/cq23)
     &          +(delta1ir+log(-mir2/cq23))
     &           *(log((m32-cq2)/(m32-cq12))+log((m32-cq3)/(m32-cq12)))
     &          -log((m32-cq2)/(m32-cq3))**2/2d0-pi**2/3d0
     &          -2d0*cspen_dd((cq2-cq12)/(m32-cq12))
     &          -2d0*cspen_dd((cq3-cq12)/(m32-cq12)) )/q23/(q12-m32)
            return
          else
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'D0ir_dd: case not implemented:'
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
        endif

c soft divergent D0's with mass regulators
c=========================================
100     continue

        xs   = -Kir(q23,m22,m42,qxs)
        div  = delta1ir+log(mir2)
        if (qxs.eq.0) then
          pre = xs/sqrt(m22*m42)/(q12-m32)/(1d0-xs**2)
          if (xm02.ne.(0d0,0d0)) div = log(cmp2(xm02))
        else
c         pre = -sign(1d0,q23)/abs(q23-m22-m42)/(q12-m32)
          pre = -1d0/(q23-m22-m42)/(q12-m32)
        endif
        res = 0d0
        if (qxs.eq.0) then
          res = res + 2d0*log(xs)*log(1d0-xs**2)+cspen_dd(xs**2)
        endif 

c*** two soft singularities
        if ((abs(m32).lt.1d-18).and.(q1.eq.q2).and.(q3.eq.q4)) then
          if (m32.eq.(0d0,0d0)) then 
            div2 = delta1ir+log(mir2)
          else
            div2 = log(cmp2(m32))
          endif
          D0ir_dd = -pre*log(xs)*(div+div2-2d0*log(-cq12))
          return
c*** m32 can be neglected
        elseif ((m32.eq.(0d0,0d0)).or.
     &      ((abs(m32).lt.1d-18).and.(abs(q2)+abs(m22).gt.1d-18)
     &                          .and.(abs(q3)+abs(m42).gt.1d-18))) then
          if (abs(m22).gt.1d-18) then
            m2  = sqrt(m22)
            qm2 = 0
          elseif (m22.eq.(0d0,0d0)) then
            m2  = sqrt(mir2)
            qm2 = 1
          else
            m2  = sqrt(cmp2(m22))
            qm2 = 1
          endif
          if (abs(m42).gt.1d-18) then
            m4  = sqrt(m42)
            qm4 = 0
          else
            m4  = sqrt(cmp2(m42))
            qm4 = 1
          endif
          if ((abs(q2).gt.1d-18).or.(qm2.eq.0)) then
            x2  = q2-m22
            qx2 = 0
          elseif (q2.eq.0d0) then
            x2  = cmp2(m22)
            qx2 = 2
          else
            x2  = rmp2(q2)-cmp2(m22)
            qx2 = 2
          endif
          if ((abs(q3).gt.1d-18).or.(qm4.eq.0)) then
            x3  = q3-m42
            qx3 = 0
          elseif (q3.eq.0d0) then
            x3  = -cmp2(m42)
            qx3 = 2
          else
            x3  = rmp2(q3)-cmp2(m42)
            qx3 = 2
          endif
          x2 = x2+abs(x2)*ieps
          x3 = x3+abs(x3)*ieps*1.8d0

          if ((q1.eq.q2).and.(q3.eq.q4)) then
            res = -2d0*log(xs)*(div-log(-cq12))
          elseif (q1.eq.q2) then
            res = res - log(xs)*(log(xs)+div-2d0*log(m4)
     &                           +2d0*log(x3/cq12))
     &                - pi**2/6d0
          elseif (q3.eq.q4) then
            res = res - log(xs)*(log(xs)+div-2d0*log(m2)
     &                           +2d0*log(x2/cq12))
     &                - pi**2/6d0
          else
            y  = m2/m4/x2*x3
            qy = qm2-qm4-qx2+qx3
            res = res + log(xs)*( -log(xs)/2d0-div+log(m2*m4)
     &                            -log(x2/cq12)-log(x3/cq12)   )
     &                + pi**2/6d0 + log(y)**2/2d0
            if (qxs+qy.eq.0) then
              res = res - cspen_dd(xs*y)-(log(xs)+log(y))*log(1d0-xs*y)
            elseif (qxs+qy.lt.0) then
              res = res - pi**2/3d0-(log(xs)-log(y))**2/2d0
              if (cout_on.and.(cout.le.coutmax)) then
                write(outchannel,*) 'D0ir_dd for m32=0 not yet checked'
                write(outchannel,*) '(qxs+qy.lt.0)'
                call DD_debugoutput()
              endif
            endif
            if (qxs-qy.eq.0) then
              res = res - cspen_dd(xs/y)-(log(xs)-log(y))*log(1d0-xs/y)
            elseif (qxs-qy.lt.0) then
              res = res - pi**2/3d0-(log(xs)+log(y))**2/2d0
              if (cout_on.and.(cout.le.coutmax)) then
                write(outchannel,*) 'D0ir_dd for m32=0 not yet checked'
                write(outchannel,*) '(qxs-qy.lt.0)'
                call DD_debugoutput()
              endif
              stopflag = min(-1,stopflag)
            endif
          endif

c*** m32 not negligible
        else
          if (abs(m32).gt.1d-18) then
            m3  = sqrt(m32)
          else
            m3  = sqrt(cmp2(m32))
          endif
          x2 = -Kir(q2,m22,m32,qx2)
          x3 = -Kir(q3,m42,m32,qx3)
          res = res - log(xs)*2d0
     &           *(div/2d0+log(m3/(m32-cq12)))
     &              + pi**2/2d0+log(x2)**2+log(x3)**2        
          if ((qxs+qx2+qx3).eq.0) then
            res = res - cspen_dd(xs*x2*x3)
     &                - (log(xs)+log(x2)+log(x3))*log(1d0-xs*x2*x3)
          endif 
          if ((qxs-qx2+qx3).eq.0) then
            res = res - cspen_dd(xs/x2*x3)
     &                - (log(xs)-log(x2)+log(x3))*log(1d0-xs/x2*x3)
          elseif ((qxs-qx2+qx3).lt.0) then
            res = res + pi**2/6d0 + log(-xs/x2*x3)**2/2d0
     &                - (log(xs)-log(x2)+log(x3))*log(-xs/x2*x3)
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'D0ir_dd for m32=/=0 not yet checked'
              write(outchannel,*) '((qxs-qx2+qx3).lt.0)'
              call DD_debugoutput()
            endif
            stopflag = min(-1,stopflag)
          endif 
          if ((qxs+qx2-qx3).eq.0) then
            res = res - cspen_dd(xs*x2/x3)
     &                - (log(xs)+log(x2)-log(x3))*log(1d0-xs*x2/x3)
          elseif ((qxs+qx2-qx3).lt.0) then
            res = res + pi**2/6d0 + log(-xs*x2/x3)**2/2d0
     &                - (log(xs)+log(x2)-log(x3))*log(-xs*x2/x3)
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'D0ir_dd for m32=/=0 not yet checked'
              write(outchannel,*) '((qxs+qx2-qx3).lt.0)'
              call DD_debugoutput()
            endif
            stopflag = min(-1,stopflag)
          endif 
          if ((qxs-qx2-qx3).eq.0) then
            res = res - cspen_dd(xs/x2/x3)
     &                - (log(xs)-log(x2)-log(x3))*log(1d0-xs/x2/x3)
          elseif ((qxs-qx2-qx3).lt.0) then
            res = res + pi**2/6d0 + log(-xs/x2/x3)**2/2d0
     &                - (log(xs)-log(x2)-log(x3))*log(-xs/x2/x3)
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'D0ir_dd for m32=/=0 not yet checked'
              write(outchannel,*) '((qxs-qx2-qx3).lt.0)'
              call DD_debugoutput()
            endif
            stopflag = min(-1,stopflag)
          endif 
        endif 

        D0ir_dd = D0ir_dd + pre*res

        end

************************************************************************
        function Kir(xz,xm12,xm22,qsing)
************************************************************************
*       Auxiliary function for soft singular integrals
*       z -> z+ieps
*-----------------------------------------------------------------------
*       7.2.07 Stefan Dittmaier
************************************************************************
        use DD_global
        implicit real*8 (a-z)                                         

c local variables
        complex*16 Kir,xm12,xm22,z,m12,m22,m1,m2,root,ieps
        complex*16 cmp2,cm2
        integer qsing

c       rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))

        eps   = 1d-20
        ieps  = dcmplx(0d0,eps)
        qsing = 0

        z = xz+abs(xz)*ieps

c ordering: |m12| <= |m22|
        if (abs(xm12).lt.abs(xm22)) then
          m12 = xm12
          m22 = xm22
        else
          m12 = xm22
          m22 = xm12
        endif

c masses
        if (m12.eq.(0d0,0d0)) then
          m1 = sqrt(mir2)
        elseif (abs(m12).lt.1d-18) then
          m1 = sqrt(cmp2(m12))
        else 
          m1 = sqrt(m12)
        endif
        if (m22.eq.(0d0,0d0)) then
          m2 = sqrt(mir2)
        elseif (abs(m22).lt.1d-18) then
          m2 = sqrt(cmp2(m22))
        else 
          m2 = sqrt(m22)
        endif

        if (abs(z).gt.1d-18) then
          if (abs(m12).gt.1d-18) then
            root = sqrt(1d0-4d0*m1*m2/(z-(m1-m2)**2))
c           Kir  = (1-root)/(1+root)
            Kir  = 4d0*m1*m2/(z-(m1-m2)**2)/(1+root)**2
          elseif (abs(m22).gt.1d-18) then
            if ((abs(m12).gt.0d0).and.(dcmplx(xz).eq.m22)) then
              Kir = dcmplx(0d0,1d0)
            elseif ((abs(m12).eq.0d0).and.(dcmplx(xz).eq.m22)) then
              Kir = 0d0
              if (cout_on.and.(cout.le.coutmax)) then
                write(outchannel,*) 'Singular case in Kir !'
                write(outchannel,*) xz,xm12,xm22
                call DD_debugoutput()
              endif
              stopflag = min(-10,stopflag)
            else
              Kir = m1*sqrt(m22)/(z-m22)
              qsing = 1
            endif
          else 
            Kir = m1*m2/z
            qsing = 2
          endif
        else
          if (abs(m12).gt.1d-18) then
            Kir = -m1/m2-abs(m1/m2)*ieps
          elseif (abs(m22).gt.1d-18) then
            Kir = -m1/m2-abs(m1/m2)*ieps
            qsing = 1
          elseif ((xz.eq.0d0).and.(m12.eq.m22)) then
            Kir = -1d0
            qsing = 0
          else
            Kir = 0d0
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 'Singular case in Kir !'
              write(outchannel,*) xz,xm12,xm22
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
        endif

        end

************************************************************************
        function D0coll_dd(p1,p2,p3,p4,p12,p23,xm12,xm22,xm32,xm42)
************************************************************************
*       collinear-singular D0 function 
*
*             p1 \           / p4
*                 \   m12   /
*                  *-------*
*                  |       |
*              m22 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \
*             p2 /           \ p3
*
*       p1,m12,m22 = small or 0
*
*       further collinear singularities allowed, but must be ordered
*       with increasing labels of masses
*       
*       dim. reg.: translation via S.D. NPB675 (2003) 447 
*                  + special cases
*-----------------------------------------------------------------------
*       17.3.07 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         

c local variables
        complex*16 D0coll_dd,cspen_dd,eta_dd,eta3,sqe_dd,a1,a2,a3,pre
        complex*16 res,xs,x1,x2,x3,x4,x5,x6,cp2,cp3,cp4,cp12,cp23,ieps
        complex*16 m12,m22,m32,m42,xm12,xm22,xm32,xm42,cmp2,cm2,Kir
        integer qx3
        logical smallp2,smallp3,smallp4,smallm3,smallm4
        logical coll23,coll34,coll41

        eta3(a1,a2,a3) = eta_dd(a1*a2,a3) + eta_dd(a1,a2)

c       rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))

        D0coll_dd = 0d0

        eps  = 1d-16
        ieps = dcmplx(0d0,eps) 
        pi   = 4d0*datan(1d0)

        m12 = xm12
        m22 = xm22
        m32 = xm32
        m42 = xm42

        smallp2 = (abs(p2).lt.1d-18)
        smallp3 = (abs(p3).lt.1d-18)
        smallp4 = (abs(p4).lt.1d-18)
        smallm3 = (abs(m32).lt.1d-18)
        smallm4 = (abs(m42).lt.1d-18)

        coll23 = smallp2.and.smallm3
        coll34 = smallp3.and.smallm3.and.smallm4
        coll41 = smallp4.and.smallm4

c 4 coll. singularities
c---------------------- 
        if (coll23.and.coll34.and.coll41) then
c---------------------------------------------
*            0\ m12 /0
*              *---*
*           m12|   |m12
*              *---*
*            0/ m12 \0
c---------------------------------------------
          if ((p1.eq.0d0).and.(p2.eq.0d0).and.(p3.eq.0d0).and.
     &        (p4.eq.0d0).and.(m22.eq.m12).and.(m32.eq.m12).and.
     &         (m42.eq.m12)) then
            m12  = cmp2(m12)
            D0coll_dd = 2d0/p12/p23*( -pi**2/2d0
     &                    +log(-p12/m12-ieps)*log(-p23/m12-ieps) )
            return
          endif

c 3 coll. singularities
c---------------------- 
        elseif (coll23.and.coll34) then
c---------------------------------------------
*            0\ m12 /p4       p4=/=0
*              *---*
*           m12|   |m12       m12 small
*              *---*
*            0/ m12 \0
c---------------------------------------------
          if ((p1.eq.0d0).and.(p2.eq.0d0).and.(p3.eq.0d0).and.
     &        (p4.ne.0d0).and.(m22.eq.m12).and.(m32.eq.m12).and.
     &         (m42.eq.m12)) then
            m12  = cmp2(m12)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            cp4  = p4+abs(p4)*ieps
            D0coll_dd = 2d0/p12/p23*( -log(-m12/cp4)**2/2d0
     &            +log(-m12/cp12)*log(-m12/cp23)-pi**2/6d0
     &            -cspen_dd(1d0-cp4/cp23)-cspen_dd(1d0-cp4/cp12) )
            return
c---------------------------------------------
*          m22\  0  /p4       p4=/=0
*              *---*
*           m22|   | 0        m22 small
*              *---*
*            0/ m22 \m22
c---------------------------------------------
          elseif ((dcmplx(p1).eq.m22).and.(p2.eq.0d0).and.
     &      (dcmplx(p3).eq.m22).and.(p4.ne.0d0).and.(m12.eq.(0d0,0d0))
     &      .and.(m22.eq.m32).and.(m42.eq.(0d0,0d0))) then
            m22  = cmp2(m22)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            cp4  = p4+abs(p4)*ieps
            D0coll_dd = 1d0/p12/p23*( -log(-m22/cp4)**2
     &            +log(-m22/cp12)**2+log(-m22/cp23)**2+pi**2/3d0
     &            +2d0*log(cp4/cp12)*log(cp4/cp23) )
            return
c---------------------------------------------
*          m22\  0  /p4       p4=/=0
*              *---*
*           m22|   | m22      m22 small
*              *---*
*            0/ m22 \0  
c---------------------------------------------
*            0\ m22 /p4       p4=/=0
*              *---*
*           m22|   | 0        m22 small
*              *---*
*            0/ m22 \m22
c---------------------------------------------
          elseif ((p2.eq.0d0).and.(m22.eq.m32).and.
     &            (((dcmplx(p1).eq.m22).and.(p3.eq.0d0).and.
     &              (m12.eq.(0d0,0d0)).and.(m22.eq.m42)) .or.
     &             ((dcmplx(p3).eq.m22).and.(p1.eq.0d0).and.
     &              (m42.eq.(0d0,0d0)).and.(m22.eq.m12)) ) ) then
            m22  = cmp2(m22)
            cp4  = p4+abs(p4)*ieps
            if (m42.eq.(0d0,0d0)) then
              cp12 = p12+abs(p12)*ieps
              cp23 = p23+abs(p23)*ieps
            else
              cp23 = p12+abs(p12)*ieps
              cp12 = p23+abs(p23)*ieps
            endif
            D0coll_dd = 1d0/p12/p23*( log(-m22/cp12)**2
     &            +2d0*log(-m22/cp12)*log(cp4/cp23)
     &            -2d0*cspen_dd(1d0-cp4/cp23) )
            return
          endif

c 2 coll. singularities -- adjacent singularities
c---------------------- 
        elseif (coll23.and.(.not.coll41)) then
          if ((dcmplx(p1).eq.m22).and.(dcmplx(p2).eq.m22).and.
     &        (dcmplx(p3).ne.m42).and.(dcmplx(p4).ne.m42).and.
     &        (m12.eq.0d0).and.(m22.ne.0d0).and.(m32.eq.0d0) )then
c---------------------------------------------
*       p1=m22\  0  /p4       p4=/=m42
*              *---*
*           m22|   |m42       m22 small
*              *---*
*       p2=m22/  0  \p3       p3=/=m42
c---------------------------------------------
            m22  = cmp2(m22)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            cp3  = p3+abs(p3)*ieps
            cp4  = p4+abs(p4)*ieps
            if (abs(m42).gt.1d-18) then
              xs  = -m42/cp12
              x3  = m42/(m42-cp3)
              x4  = m42/(m42-cp4)
              res = cspen_dd(1d0-x3*x4/xs)
     &              +eta3(1d0/xs,x3,x4)*log(1d0-x3*x4/xs)
            else
              res = pi**2/6d0
            endif
            D0coll_dd = ( res + log(-cp12/m22)**2/2d0+pi**2/2d0
     &             +log(-m22/cp12)*( log((m42-cp3)/(m42-cp23))
     &                              +log((m42-cp4)/(m42-cp23))) 
     &             -log((m42-cp3)/(m42-cp4))**2/2d0
     &             +log((m42-cp3)/(m42-cp23))**2
     &             +log((m42-cp4)/(m42-cp23))**2
     &                  )/p12/(p23-m42)
            return
          elseif ((p1.eq.0d0).and.(p2.eq.0d0).and.
     &            (dcmplx(p3).ne.m42).and.(dcmplx(p4).ne.m42).and.
     &            (m22.eq.m12).and.(m32.eq.m12)) then
c---------------------------------------------
*            0\ m12 /p4=/=m42
*              *---*
*           m12|   |m42        m12 small
*              *---*
*            0/ m12 \p3=/=m42
c---------------------------------------------
            m12  = cmp2(m12)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            cp3  = p3+abs(p3)*ieps
            cp4  = p4+abs(p4)*ieps

            if (abs(m42).gt.1d-18) then
              xs  = -m42/cp12
              x3  = m42/(m42-cp3)
              x4  = m42/(m42-cp4)
              res = cspen_dd(1d0-x3*x4/xs)
     &              +eta3(1d0/xs,x3,x4)*log(1d0-x3*x4/xs)
            else
              res = pi**2/6d0
            endif
            D0coll_dd = ( res + log(-cp12/m12)**2/2d0-pi**2/6d0
     &          -log((m42-cp3)/(m42-cp4))**2/2d0
     &          +log(-m12/cp12)*( log((m42-cp3)/(m42-cp23))
     &                           +log((m42-cp4)/(m42-cp23)) )
     &          -2d0*cspen_dd((cp3-cp23)/(m42-cp23))
     &          -2d0*cspen_dd((cp4-cp23)/(m42-cp23)) )/p12/(p23-m42)
            return
          elseif ((p1.eq.0d0).and.(p2.eq.0d0).and.
     &            (m22.eq.m12).and.(m32.eq.m12).and.
     &            (dcmplx(p3).eq.m42).and.(dcmplx(p4).eq.m42)) then
c---------------------------------------------
*            0\ m12 /p4=m42
*              *---*
*           m12|   |m42        m12 small
*              *---*
*            0/ m12 \p3=m42
c---------------------------------------------
            m12  = cmp2(m12)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            D0coll_dd = 2d0/p12/(p23-m42)*( -pi**2/4d0
     &                   +log(-m12/cp12)*log(sqrt(m12*m42)/(m42-cp23)) )
            return
          elseif ((p1.eq.0d0).and.(p2.eq.0d0).and.
     &            (m22.eq.m12).and.(m32.eq.m12).and.
     &             (((dcmplx(p3).ne.m42).and.(dcmplx(p4).eq.m42)).or.
     &              ((dcmplx(p3).eq.m42).and.(dcmplx(p4).ne.m42)))) then
c---------------------------------------------
*            0\ m12 /p4=m42
*              *---*
*           m12|   |m42        m12 small
*              *---*
*            0/ m12 \p3=/=m42
c---------------------------------------------
*            0\ m12 /p4=/=m42
*              *---*
*           m12|   |m42        m12 small
*              *---*
*            0/ m12 \p3=m42
c---------------------------------------------
            m12  = cmp2(m12)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            if (dcmplx(p4).eq.m42) then
              cp3  = p3+abs(p3)*ieps
              cp4  = p4+abs(p4)*ieps
            else
              cp4  = p3+abs(p3)*ieps
              cp3  = p4+abs(p4)*ieps
            endif
            D0coll_dd = ( log(m12/m42)**2*3d0/4d0 
     &       +log(m12/m42)*(log((cp3-m42)/cp12)-2d0*log((m42-cp23)/m42))
     &       -2d0*cspen_dd((cp3-cp23)/(m42-cp23))-5d0*pi**2/12d0
     &       +2d0*log(-cp12/m42)*log((m42-cp23)/m42)
     &       -log((m42-cp3)/m42)**2 )/p12/(p23-m42)
            return
          elseif (((p1.eq.0d0).and.(dcmplx(p2).eq.m22).and.
     &             (m22.eq.m12).and.(m32.eq.(0d0,0d0)).and.
     &             (dcmplx(p3).ne.m42).and.(dcmplx(p4).eq.m42)).or. 
     &            ((p2.eq.0d0).and.(dcmplx(p1).eq.m22).and.
     &             (m22.eq.m32).and.(m12.eq.(0d0,0d0)).and.
     &             (dcmplx(p3).eq.m42).and.(dcmplx(p4).ne.m42))) then
c---------------------------------------------
*            0\ m22 /p4=m42
*              *---*
*           m22|   |m42        m12 small
*              *---*
*       p2=m22/  0  \p3=/=m42
c---------------------------------------------
*       p1=m22\  0  /p4=/=m42
*              *---*
*           m22|   |m42        m32 small
*              *---*
*            0/ m22 \p3=m42
c---------------------------------------------
            m22  = cmp2(m22)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            if (dcmplx(p4).eq.m42) then
              cp3  = p3+abs(p3)*ieps
            else
              cp3  = p4+abs(p4)*ieps
            endif
            D0coll_dd = ( log(m22/m42)**2*3d0/4d0 
     &       +log(m22/m42)*(log((cp3-m42)/cp12)-2d0*log((m42-cp23)/m42))
     &       +log((m42-cp23)/m42)**2-pi**2/12d0
     &       -2d0*log((m42-cp23)/m42)*log((cp3-m42)/cp12) 
     &                  )/p12/(p23-m42)
            return
          elseif ( ((p1.eq.0d0).and.(dcmplx(p2).eq.m22).and.
     &              (dcmplx(p3).ne.m42).and.(dcmplx(p4).ne.m42).and.
     &              (m22.eq.m12).and.(m32.eq.(0d0,0d0))) 
     &        .or. ((p2.eq.0d0).and.(dcmplx(p1).eq.m22).and.
     &              (dcmplx(p3).ne.m42).and.(dcmplx(p4).ne.m42).and.
     &              (m22.eq.m32).and.(m12.eq.(0d0,0d0))) ) then
c---------------------------------------------
*            0\ m22 /p4       p4=/=m42
*              *---*
*           m22|   |m42       m22 small
*              *---*
*          m22/  0  \p3       p3=/=m42
c---------------------------------------------
*          m22\  0  /p4       p4=/=m42
*              *---*
*           m22|   |m42       m22 small
*              *---*
*            0/ m22 \p3       p3=/=m42
c---------------------------------------------
            m22  = cmp2(m22)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            cp3  = p3+abs(p3)*ieps
            cp4  = p4+abs(p4)*ieps

            if (abs(m42).gt.1d-18) then
              xs  = -m42/cp12
              x3  = m42/(m42-cp3)
              x4  = m42/(m42-cp4)
              res = cspen_dd(1d0-x3*x4/xs)
     &              +eta3(1d0/xs,x3,x4)*log(1d0-x3*x4/xs)
            else
              res = pi**2/6d0
            endif
            D0coll_dd = ( res + log(-cp12/m22)**2/2d0+pi**2/6d0
     &          -log((m42-cp3)/(m42-cp4))**2/2d0
     &          +log(-m22/cp12)*( log((m42-cp3)/(m42-cp23))
     &                           +log((m42-cp4)/(m42-cp23)) )
     &                  )/p12/(p23-m42)

            if (p1.eq.0d0) then
              D0coll_dd = D0coll_dd + ( log((m42-cp3)/(m42-cp23))**2
     &          -2d0*cspen_dd((cp4-cp23)/(m42-cp23)) )/p12/(p23-m42)
            else
              D0coll_dd = D0coll_dd + ( log((m42-cp4)/(m42-cp23))**2
     &          -2d0*cspen_dd((cp3-cp23)/(m42-cp23)) )/p12/(p23-m42)
            endif
            return
          endif

c 2 coll. singularities -- sings. on opposite legs
c---------------------- 
        elseif (coll34.and.(.not.coll41)) then
c---------------------------------------------
*            0\  0  /p4       
*              *---*      
*             0|   |0      
*              *---*      
*           p2/  0  \0    p2,p4 not small
*
*       and all cases with mass regulators
c---------------------------------------------
          cp12 = p12+abs(p12)*ieps
          cp23 = p23+abs(p23)*ieps*1.7d0
          cp2  = p2+abs(p2)*ieps*2.1d0
          cp4  = p4+abs(p4)*ieps*3d0
          pre  = 1d0/(p12*p23-p2*p4)
          D0coll_dd = -2d0*pre*( cspen_dd(1d0-cp12*cp23/cp2/cp4)
     &        +eta_dd(cp12/cp2,cp23/cp4)*log(1d0-cp12*cp23/cp2/cp4) )
          if ((p1.eq.0d0).and.(m12.eq.m22)) then
            if (m12.ne.dcmplx(0d0)) then
              m12 = cmp2(m12)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m12/cp2))*(log(cp12/cp2)+log(cp23/cp4))
     &          -2d0*cspen_dd(1d0-cp2/cp12)-log(cp2/cp12)**2/2d0
     &          -2d0*cspen_dd(1d0-cp4/cp23)-log(cp4/cp23)**2/2d0 )
            else
              D0coll_dd = D0coll_dd + pre*(
     &          -(delta1ir+log(-mir2/cp2))*(log(cp12/cp2)+log(cp23/cp4))
     &          -2d0*cspen_dd(1d0-cp2/cp12)-log(cp2/cp12)**2/2d0
     &          -2d0*cspen_dd(1d0-cp4/cp23)-log(cp4/cp23)**2/2d0 )
            endif
          elseif ((p1.ne.0d0).and.(dcmplx(p1).eq.m12).and.
     &            (m22.eq.dcmplx(0d0))) then
              m12 = cmp2(m12)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m12/cp2))*(log(cp12/cp2)+log(cp23/cp4))
     &          +log(cp2/cp12)**2/2d0-log(cp4/cp23)**2/2d0 ) 
          elseif ((p1.ne.0d0).and.(dcmplx(p1).eq.m22).and.
     &            (m12.eq.dcmplx(0d0))) then
              m22 = cmp2(m22)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m22/cp2))*(log(cp12/cp2)+log(cp23/cp4))
     &          -log(cp2/cp12)**2/2d0+log(cp4/cp23)**2/2d0 ) 
          else
            goto 999
          endif
          if ((p3.eq.0d0).and.(m32.eq.m42)) then
            if (m32.ne.dcmplx(0d0)) then
              m32 = cmp2(m32)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m32/cp4))*(log(cp12/cp4)+log(cp23/cp2))
     &          -2d0*cspen_dd(1d0-cp2/cp23)-log(cp2/cp23)**2/2d0
     &          -2d0*cspen_dd(1d0-cp4/cp12)-log(cp4/cp12)**2/2d0 )
            else
              D0coll_dd = D0coll_dd + pre*(
     &          -(delta1ir+log(-mir2/cp4))*(log(cp12/cp4)+log(cp23/cp2))
     &          -2d0*cspen_dd(1d0-cp2/cp23)-log(cp2/cp23)**2/2d0
     &          -2d0*cspen_dd(1d0-cp4/cp12)-log(cp4/cp12)**2/2d0 )
            endif
          elseif ((p3.ne.0d0).and.(dcmplx(p3).eq.m32).and.
     &            (m42.eq.dcmplx(0d0))) then
              m32 = cmp2(m32)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m32/cp4))*(log(cp12/cp4)+log(cp23/cp2))
     &          -log(cp2/cp23)**2/2d0+log(cp4/cp12)**2/2d0 )
          elseif ((p3.ne.0d0).and.(dcmplx(p3).eq.m42).and.
     &            (m32.eq.dcmplx(0d0))) then
              m42 = cmp2(m42)
              D0coll_dd = D0coll_dd + pre*(
     &          -(log(-m42/cp4))*(log(cp12/cp4)+log(cp23/cp2))
     &          +log(cp2/cp23)**2/2d0-log(cp4/cp12)**2/2d0 )
          else
            goto 999
          endif
          return

c 1 coll. singularity
c--------------------   
        elseif (.not.(coll23.or.coll34.or.coll41)) then
          if ((dcmplx(p4).eq.m42).and.(dcmplx(p2).eq.m32)) then
c---------------------------------------------
*            0\ m12 /p4=m42
*              *---*            m12 = small
*           m12|   |m42    
*              *---*      
*       m32=p2/ m32 \p3
c---------------------------------------------
            m12  = cmp2(m12)
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps
            x3   = -Kir(p3,m42,m32,qx3)
            D0coll_dd = ( 2d0*log(sqrt(m12*p4)/(p4-cp23))
     &                       *log(sqrt(m12*p2)/(p2-cp12))
     &                   - pi**2/2d0 - log(x3)**2 )/(p12-p2)/(p23-p4) 
            return
          elseif ( ((dcmplx(p1).eq.m12).and.(m22.eq.(0d0,0d0)).and.
     &              (dcmplx(p2).ne.m32).and.(dcmplx(p4).eq.m42)) .or.
     &             ((dcmplx(p1).eq.m22).and.(m12.eq.(0d0,0d0)).and.
     &              (dcmplx(p2).eq.m32).and.(dcmplx(p4).ne.m42)) ) then
c---------------------------------------------
*       m12=p1\ m12 /p4=m42
*              *---*            m12 = small
*             0|   |m42    
*              *---*      
*     m32=/=p2/ m32 \p3        
c---------------------------------------------
*       m22=p1\  0  /p4=/=m42
*              *---*            m22 = small
*           m22|   |m42    
*              *---*      
*       m32=p2/ m32 \p3       
c---------------------------------------------
            cp3 = p3+abs(p3)*ieps
            if (dcmplx(p4).eq.m42) then
              m12  = cmp2(m12)
              cp12 = p12+abs(p12)*ieps
              cp23 = p23+abs(p23)*ieps
              cp2  = p2+abs(p2)*ieps
            else
              m12  = cmp2(m22)
              cp12 = p23+abs(p23)*ieps
              cp23 = p12+abs(p12)*ieps
              cp2  = p4+abs(p4)*ieps
              m32  = xm42
              m42  = xm32
            endif
            if (abs(m32).gt.1d-18) then
              res = (delta1ir+log(mir2/m12))
     &                *log(sqrt(m12*m42)/(m42-cp23))
     &              -5d0*pi**2/12d0-log((m32-cp2)/(m32-cp12))**2 
              xs  = sqrt(m12*m42)/(m42-cp23)
              x2  = sqrt(m12*m32)/(m32-cp2)
              x3  = -Kir(p3,m42,m32,qx3)
              res = res + pi**2/2d0+log(x2)**2+log(x3)**2            
     &              -log(xs)*(delta1ir+2d0
     &                *log(sqrt(mir2*m32)/(m32-cp12)))
              if (qx3.eq.0) then
                res = res - cspen_dd(xs/x2*x3)
     &                    - (log(xs)-log(x2)+log(x3))*log(1d0-xs/x2*x3)
     &                    - cspen_dd(xs/x2/x3)
     &                    - (log(xs)-log(x2)-log(x3))*log(1d0-xs/x2/x3)
              elseif (qx3.lt.0) then
                res = res + pi**2/6d0 + log(-xs/x2*x3)**2/2d0
     &                    - (log(xs)-log(x2)+log(x3))*log(-xs/x2*x3)
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'case not yet checked: (qx3.lt.0)'
                  call DD_debugoutput()
                endif
                stopflag = min(-1,stopflag)
              elseif (qx3.gt.0) then
                res = res + pi**2/6d0 + log(-xs/x2/x3)**2/2d0
     &                    - (log(xs)-log(x2)-log(x3))*log(-xs/x2/x3)
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'case not yet checked: (qx3.gt.0)'
                  call DD_debugoutput()
                endif
                stopflag = min(-1,stopflag)
              endif 
              D0coll_dd = - res/(cp23-m42)/(cp12-m32)
            else
              xs  = m42/(m42-cp23)
              x2  = -m42/cp2
              x3  = m42/(m42-cp3)
              D0coll_dd = ( log(-m42/cp12)*(2d0*log(xs)+log(m12/m42))
     &         +log(m12/m42)**2/4d0+log(m12/m42)*log(xs/x2)
     &         -log(x2/x3)**2/2d0+log(xs/x2)**2/2d0-log(x2)**2/2d0
     &         -log(xs)*log(x3)
     &         -eta_dd(xs/x2,x3)*log(1d0-xs/x2*x3)
     &         -cspen_dd(1d0-xs/x2*x3)+5d0*pi**2/12d0 
     &         +log(cp2/cp12)**2 )/(cp23-m42)/cp12
            endif 
            return
          elseif ((p1.eq.0d0).and.(m12.eq.m22).and.
     &            ((dcmplx(p4).eq.m42).or.(dcmplx(p2).eq.m32))) then
c---------------------------------------------
*            0\ m12 /p4=m42
*              *---*            m12 = small
*           m12|   |m42    
*              *---*      
*     m32=/=p2/ m32 \p3        
c---------------------------------------------
*            0\ m12 /p4=/=m42
*              *---*            m12 = small
*           m12|   |m42    
*              *---*      
*       m32=p2/ m32 \p3       
c---------------------------------------------
            m12 = cmp2(m12)
            cp3 = p3+abs(p3)*ieps
            if (dcmplx(p4).eq.m42) then
              cp12 = p12+abs(p12)*ieps
              cp23 = p23+abs(p23)*ieps
              cp2  = p2+abs(p2)*ieps
            else
              cp12 = p23+abs(p23)*ieps
              cp23 = p12+abs(p12)*ieps
              cp2  = p4+abs(p4)*ieps
              m32  = xm42
              m42  = xm32
            endif
            if (abs(m32).gt.1d-18) then
              res = (delta1ir+log(mir2/m12))
     &                *log(sqrt(m12*m42)/(m42-cp23))
     &              -pi**2/12d0+2d0*cspen_dd((cp2-cp12)/(m32-cp12)) 
              xs  = sqrt(m12*m42)/(m42-cp23)
              x2  = sqrt(m12*m32)/(m32-cp2)
              x3  = -Kir(p3,m42,m32,qx3)
              res = res + pi**2/2d0+log(x2)**2+log(x3)**2            
     &              -log(xs)*(delta1ir+2d0
     &                *log(sqrt(mir2*m32)/(m32-cp12)))
              if (qx3.eq.0) then
                res = res - cspen_dd(xs/x2*x3)
     &                    - (log(xs)-log(x2)+log(x3))*log(1d0-xs/x2*x3)
     &                    - cspen_dd(xs/x2/x3)
     &                    - (log(xs)-log(x2)-log(x3))*log(1d0-xs/x2/x3)
              elseif (qx3.lt.0) then
                res = res + pi**2/6d0 + log(-xs/x2*x3)**2/2d0
     &                    - (log(xs)-log(x2)+log(x3))*log(-xs/x2*x3)
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'case not yet checked: (qx3.lt.0)'
                  call DD_debugoutput()
                endif
                stopflag = min(-1,stopflag)
              elseif (qx3.gt.0) then
                res = res + pi**2/6d0 + log(-xs/x2/x3)**2/2d0
     &                    - (log(xs)-log(x2)-log(x3))*log(-xs/x2/x3)
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'case not yet checked: (qx3.gt.0)'
                  call DD_debugoutput()
                endif
                stopflag = min(-1,stopflag)
              endif 
              D0coll_dd = - res/(cp23-m42)/(cp12-m32)
            else
              xs  = m42/(m42-cp23)
              x2  = -m42/cp2
              x3  = m42/(m42-cp3)
              D0coll_dd = ( log(-m42/cp12)*(2d0*log(xs)+log(m12/m42))
     &         +log(m12/m42)**2/4d0+log(m12/m42)*log(xs/x2)
     &         -log(x2/x3)**2/2d0+log(xs/x2)**2/2d0-log(x2)**2/2d0
     &         -log(xs)*log(x3)-2d0*cspen_dd(1d0-cp2/cp12)
     &         -eta_dd(xs/x2,x3)*log(1d0-xs/x2*x3)
     &         -cspen_dd(1d0-xs/x2*x3)+pi**2/12d0 )/(cp23-m42)/cp12
            endif 
            return
c---------------------------------------------
*            0\  0  /p4      p4=/=m42
*              *---*      
*             0|   |m42    
*              *---*      
*           p2/ m32 \p3      p2=/=m32
*
*       and all cases with mass regulators
c---------------------------------------------
          else
            cp12 = p12+abs(p12)*ieps
            cp23 = p23+abs(p23)*ieps*1.3d0
            if (smallp2) then
              cp2 = 0d0
            else
              cp2  = p2+abs(p2)*ieps*1.7d0
            endif
            if (smallp3) then
              cp3 = 0d0
            else
              cp3  = p3+abs(p3)*ieps*2.1d0
            endif
            if (smallp4) then
              cp4 = 0d0
            else
              cp4  = p4+abs(p4)*ieps*2.3d0
            endif
            if (smallm3) then
              m32 = 0d0
            elseif (dimag(m32).eq.0d0) then
              m32 = m32-abs(m32)*ieps*2.7d0
            endif
            if (smallm4) then
              m42 = 0d0
            elseif (dimag(m42).eq.0d0) then
              m42 = m42-abs(m42)*ieps*2.9d0
            endif
            x1  = -(m42-cp4)/(m32-cp12)
            x2  = -(m42-cp23)/(m32-cp2)
            x5  = -(m42-cp23)/(m42-cp4)
            x6  = -(m32-cp12)/(m32-cp2)
            pre = 1d0/((p12-m32)*(p23-m42)-(p2-m32)*(p4-m42))
            D0coll_dd = pre*( -log(-x5)*log(-x6) 
     &        - 2d0*cspen_dd(1d0-x2/x1)
     &        - 2d0*eta_dd(-x2,-1d0/x1)*log(1d0-x2/x1) )
            if ((m32.eq.dcmplx(0d0)).and.(m42.eq.dcmplx(0d0))) then
              D0coll_dd = D0coll_dd + pre*( 
     &            log(cp4/cp3)*(log(-x2)-log(-x1)) 
     &          - log(-x2)**2/2d0 + log(-x1)**2/2d0 )
            elseif (m32.eq.dcmplx(0d0)) then
              x3 = -1d0+cp3/m42
              D0coll_dd = D0coll_dd + pre*( 
     &            log(1d0-cp4/m42)*(log(-x2)-log(-x1)) 
     &          - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &          + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) )
            elseif (m42.eq.dcmplx(0d0)) then
              if (dcmplx(p3).ne.xm32) then
                x3 = -m32/(m32-cp3)
                D0coll_dd = D0coll_dd + pre*( 
     &            log(-cp4/(m32-cp3))*(log(-x2)-log(-x1)) 
     &          - log(-x2)**2/2d0 + log(-x1)**2/2d0 
     &          - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &          + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) )
              else
                D0coll_dd = D0coll_dd + pre*( 
     &              log(-cp4/m32)*(log(-x2)-log(-x1)) 
     &            - log(-x2)**2 + log(-x1)**2 )
              endif
            else
              x3 = sqe_dd(m42,m32+m42-cp3,m32)
              x4 = m32/m42/x3
              D0coll_dd = D0coll_dd + pre*( 
     &            log(1d0-cp4/m42)*(log(-x2)-log(-x1)) 
     &          - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &          + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) 
     &          - cspen_dd(1d0-x1*x4) - eta_dd(-x1,-x4)*log(1d0-x1*x4) 
     &          + cspen_dd(1d0-x2*x4) + eta_dd(-x2,-x4)*log(1d0-x2*x4) )
            endif
c different regularizations     
            if (p1.eq.0d0) then
              if ((m12.eq.m22).and.(m12.eq.dcmplx(0d0))) then
                D0coll_dd = D0coll_dd + pre*(
     &              (delta1ir+log(mir2/(m42-cp23)))*(log(-x1)-log(-x2)) 
     &            + 2d0*cspen_dd(1d0+x6) + 2d0*cspen_dd(1d0+x5) )
                return
              elseif ((m12.eq.m22).and.(m12.ne.dcmplx(0d0))) then
                m12 = cmp2(m12)
                D0coll_dd = D0coll_dd + pre*(
     &              log((m42-cp23)/m12)*(log(-x2)-log(-x1)) 
     &            + 2d0*cspen_dd(1d0+x6) + 2d0*cspen_dd(1d0+x5) )
                return
              endif
            elseif ((dcmplx(p1).eq.m12).and.(m22.eq.dcmplx(0d0))) then
              m12 = cmp2(m12)
              D0coll_dd = D0coll_dd + pre*(
     &          log(m12/(m42-cp23))*(log(-x1)-log(-x2)) - log(-x5)**2 )
              return
            elseif ((dcmplx(p1).eq.m22).and.(m12.eq.dcmplx(0d0))) then
              m22 = cmp2(m22)
              D0coll_dd = D0coll_dd + pre*(
     &          log(m22/(m42-cp23))*(log(-x1)-log(-x2)) - log(-x6)**2 ) 
              return
            endif
          endif
        endif

999     continue
        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 'D0coll_dd: case not implemented:'
          call DD_debugoutput()
        endif
        stopflag = min(-10,stopflag)

        end

************************************************************************
        function D0mmmmzero_dd(p1,p2,p3,p4,p12,p23)
************************************************************************
*       D0 function with internal masses zero
*
*             p1 \           / p4       no invariant zero or small !
*                 \    0    /
*                  *-------*
*                  |       |
*                0 |       | 0
*                  |       |
*                  *-------*
*                 /    0    \
*             p2 /           \ p3
*
*-----------------------------------------------------------------------
*       25.3.08 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         

c local variables
        complex*16 D0mmmmzero_dd,cspen_dd,eta_dd,ieps
        complex*16 cp1,cp2,cp3,cp4,cp12,cp23,crlam,caux,rlam
        complex*16 x(2),xe(2),arg(2),arge(2),l(2),le(2)
        integer i

        eps   = 1d-10
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        crit  = 1d-9

        cp1  = p1 +abs(p1) *ieps
        cp2  = p2 +abs(p2) *ieps*2
        cp3  = p3 +abs(p3) *ieps*3
        cp4  = p4 +abs(p4) *ieps*4
        cp12 = p12+abs(p12)*ieps*5
        cp23 = p23+abs(p23)*ieps*6

        aux   = p12*p23-p1*p3-p2*p4
        rlam2 = aux**2-4d0*p1*p2*p3*p4

        if (rlam2.gt.0d0) then
          rlam = sqrt(rlam2)
        else
          rlam = dcmplx(0d0,sqrt(-rlam2))
        endif
        if (abs(aux+rlam).gt.abs(aux-rlam)) then
          x(1) = (aux+rlam)/2d0/p1/p2
          x(2) = p3*p4/p1/p2/x(1)
        else
          x(2) = (aux-rlam)/2d0/p1/p2
          x(1) = p3*p4/p1/p2/x(2)
        endif

        caux  = cp12*cp23-cp1*cp3-cp2*cp4
        crlam = sqrt(caux**2-4d0*cp1*cp2*cp3*cp4)
        if (abs(caux+crlam).gt.abs(caux-crlam)) then
          xe(1) = (caux+crlam)/2d0/cp1/cp2
          xe(2) = cp3*cp4/cp1/cp2/xe(1)
        else
          xe(2) = (caux-crlam)/2d0/cp1/cp2
          xe(1) = cp3*cp4/cp1/cp2/xe(2)
        endif

        if (abs(x(1)-xe(1)).gt.abs(x(1)-xe(2))) then
          caux  = xe(1)
          xe(1) = xe(2)
          xe(2) = caux
        endif

        arg(1)  = 1d0+x(1)*p1/p4
        arge(1) = 1d0+xe(1)*cp1/cp4
        arg(2)  = 1d0+x(2)*p1/p4
        arge(2) = 1d0+xe(2)*cp1/cp4
        l(1)    = p12/p4
        le(1)   = cp12/cp4
        l(2)    = p23/p2
        le(2)   = cp23/cp2

        do i=1,2
          if (abs(dimag(x(i)))/abs(x(i)).lt.crit) x(i) = 
     &       dcmplx( dreal(x(i)),(abs(dimag(x(i)))+abs(x(i))*eps2)
     &                           *sign(1d0,dimag(xe(i))) )
          if (abs(dimag(arg(i)))/abs(arg(i)).lt.crit) arg(i) = 
     &       dcmplx( dreal(arg(i)),(abs(dimag(arg(i)))+abs(arg(i))*eps2)
     &                           *sign(1d0,dimag(arge(i))) )
          if (abs(dimag(l(i)))/abs(l(i)).lt.crit) l(i) = 
     &       dcmplx( dreal(l(i)),(abs(dimag(l(i)))+abs(l(i))*eps2)
     &                           *sign(1d0,dimag(le(i))) )
        enddo

        D0mmmmzero_dd = 0d0
        do i=1,2
          D0mmmmzero_dd = D0mmmmzero_dd - (-1)**i/rlam*( 
     &      +log(arg(i))*( eta_dd(-xe(i),cp1/cp4)
     &                    -eta_dd(-xe(3-i),cp2/cp3) )
     &      +2d0*cspen_dd(arg(i)) + log(-x(i))*(log(l(1))+log(l(2))) )
        enddo

        end

************************************************************************
        function D0mmzero_dd(p1,p2,p3,p4,p12,p23,xm32,xm42)
************************************************************************
*       D0 function with at least two internal masses zero
*
*             p1 \           / p4            p1 not small
*                 \    0    /
*                  *-------*
*                  |       |
*                0 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \
*             p2 /           \ p3
*
*-----------------------------------------------------------------------
*       25.3.08 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         

c local variables
        complex*16 D0mmzero_dd,cspen_dd,eta_dd,sqe_dd
        complex*16 cp1,cp2,cp3,cp4,cp12,cp23,m32,m42,xm32,xm42
        complex*16 x1,x2,x3,x4,x5,x6,ieps,pre
        complex*16 y13,y14,y23,y24,y34

        eps  = 1d-20
        ieps = dcmplx(0d0,eps)

        m32  = xm32-abs(xm32)*ieps
        m42  = xm42-abs(xm42)*ieps*2
        cp1  = p1+abs(p1)*ieps
        cp2  = p2+abs(p2)*ieps*2
        cp3  = p3+abs(p3)*ieps*3
        cp4  = p4+abs(p4)*ieps*4
        cp12 = p12+abs(p12)*ieps*5
        cp23 = p23+abs(p23)*ieps*6

        y13 = m32-cp12
        y14 = m42-cp4
        y23 = m32-cp2
        y24 = m42-cp23
        y34 = m32+m42-cp3
        x1  = sqe_dd(y13*y23+cp1*m32,y14*y23+y13*y24+cp1*y34,
     &            y14*y24+cp1*m42)
        x2  = (y14*y24+cp1*m42)/(y13*y23+cp1*m32)/x1
        pre = 1d0/(y13*y23+cp1*m32)/(x1-x2)

        D0mmzero_dd = 0d0

        if (dcmplx(p4).ne.xm42) then
          x5 = -(m32-cp12)/(m42-cp4)
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &        log(-(m42-cp4)/cp1)*(log(-x2)-log(-x1)) 
     &      + cspen_dd(1d0-x1*x5) + eta_dd(-x1,-x5)*log(1d0-x1*x5) 
     &      - cspen_dd(1d0-x2*x5) - eta_dd(-x2,-x5)*log(1d0-x2*x5) )
        elseif (dcmplx(p12).ne.xm32) then
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &        log(-(m32-cp12)/cp1)*(log(-x2)-log(-x1)) 
     &      + log(-x2)**2/2d0 - log(-x1)**2/2d0 )
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mmzero_dd: case not implemented:'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        if (dcmplx(p23).ne.xm42) then
          x6 = -(m32-cp2)/(m42-cp23)
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &        log(-(m42-cp23)/cp1)*(log(-x2)-log(-x1)) 
     &      + cspen_dd(1d0-x1*x6) + eta_dd(-x1,-x6)*log(1d0-x1*x6) 
     &      - cspen_dd(1d0-x2*x6) - eta_dd(-x2,-x6)*log(1d0-x2*x6) )
        elseif (dcmplx(p2).ne.xm32) then
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &        log(-(m32-cp2)/cp1)*(log(-x2)-log(-x1)) 
     &      + log(-x2)**2/2d0 - log(-x1)**2/2d0 )
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mmzero_dd: case not implemented:'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        if ((m32.eq.dcmplx(0d0)).and.(m42.eq.dcmplx(0d0))) then
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &      - log(cp3/cp1)*(log(-x2)-log(-x1)) 
     &      - log(-x2)**2/2d0 + log(-x1)**2/2d0 )
        elseif (m32.eq.dcmplx(0d0)) then
          x3 = -1d0+cp3/m42
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &      - log(-m42/cp1)*(log(-x2)-log(-x1)) 
     &      - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &      + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) )
        elseif (m42.eq.dcmplx(0d0)) then
          if (dcmplx(p3).ne.xm32) then
            x3 = -m32/(m32-cp3)
            D0mmzero_dd = D0mmzero_dd + pre*( 
     &        - log(-(m32-cp3)/cp1)*(log(-x2)-log(-x1)) 
     &        - log(-x2)**2/2d0 + log(-x1)**2/2d0 
     &        - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &        + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) )
          else
            D0mmzero_dd = D0mmzero_dd + pre*( 
     &        - log(-m32/cp1)*(log(-x2)-log(-x1)) 
     &        - log(-x2)**2 + log(-x1)**2 )
          endif
        else
          x3 = sqe_dd(m42,m32+m42-cp3,m32)
          x4 = m32/m42/x3
          D0mmzero_dd = D0mmzero_dd + pre*( 
     &      - log(-m42/cp1)*(log(-x2)-log(-x1)) 
     &      - cspen_dd(1d0-x1*x3) - eta_dd(-x1,-x3)*log(1d0-x1*x3) 
     &      + cspen_dd(1d0-x2*x3) + eta_dd(-x2,-x3)*log(1d0-x2*x3) 
     &      - cspen_dd(1d0-x1*x4) - eta_dd(-x1,-x4)*log(1d0-x1*x4) 
     &      + cspen_dd(1d0-x2*x4) + eta_dd(-x2,-x4)*log(1d0-x2*x4) )
        endif

        end

************************************************************************
        function checkD0mzero_dd(p1,p2,p3,p4,p12,p23,m22,m32,m42)
************************************************************************
*       D0 function with one internal mass zero
*
*             p1 \           / p4         
*                 \    0    /
*                  *-------*
*                  |       |
*              m22 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \            p12,p23 not small !
*             p2 /           \ p3
*
*       check version using tHV's S3 function
*
*-----------------------------------------------------------------------
*       26.3.08 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         


        complex*16 checkD0mzero_dd,D0mzero_dd,ieps,sqe_dd,S3,eta_dd
        complex*16 m22,m32,m42,m22e,m32e,m42e,pre,caux
        complex*16 y12,y13,y14,y12e,y13e,y14e,y0(6),y0e(6),y1(6)
        complex*16 aa,bb,cc,aae,bbe,cce,b,c,be,ce,arg1(6),arg2(6)
        integer i

        eps   = 1d-13
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        crit  = 1d-9

        if (m22*m32*m42.eq.(0d0,0d0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mzero_dd: 3 masses must be non-zero!'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

        m22e = m22-abs(m22)*ieps
        m32e = m32-abs(m32)*ieps*1.3d0
        m42e = m42-abs(m42)*ieps*3.5d0

        rlam2 = (p2+p3-p23)**2-4d0*p2*p3
        if (rlam2.le.0d0) then
          rlam = 0d0
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mzero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif
        if (p3.eq.0d0) then
          if (p2.ne.p23) then
            alp = p23/(p2-p23)
          else
            alp = 0d0
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 
     &           'D0mzero_dd: p3=0 and p2=p23 not supported'
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
        else
          if (p2-p3-p23.gt.0d0) then
            alp = (p2-p3-p23+rlam)/2d0/p3
          else
            alp = (p2-p3-p23-rlam)/2d0/p3
          endif
        endif

        y12  = m22-p1
        y12e = m22e-p1
        y13  = m32-p12
        y13e = m32e-p12
        y14  = m42-p4
        y14e = m42e-p4


c Note: p2*alp/(1d0+alp) = alp*p3+p23
        aa  = p3*y12+p23*y13/alp-y14*p2/(1d0+alp)
        aae = p3*y12e+p23*y13e/alp-y14e*p2/(1d0+alp)
        bb  = (m32-m42-p3)*y12+(-m22+m42+p23)*y13 
     &        +(m22-m32+(1d0-alp)/(1d0+alp)*p2)*y14
        bbe = (m32e-m42e-p3)*y12e+(-m22e+m42e+p23)*y13e 
     &        +(m22e-m32e+(1d0-alp)/(1d0+alp)*p2)*y14e
        cc  = m42*y12+alp*m42*y13+(-m22-alp*m32+alp*p2/(1d0+alp))*y14
        cce = m42e*y12e+alp*m42e*y13e
     &        +(-m22e-alp*m32e+alp*p2/(1d0+alp))*y14e
        y0(1)  = sqe_dd(aa,bb,cc)
        y0e(1) = sqe_dd(aae,bbe,cce)
        y0(2)  = cc/aa/y0(1)
        y0e(2) = cce/aae/y0e(1)
        if (abs(y0(1)-y0e(1)).gt.abs(y0(1)-y0e(2))) then
          caux   = y0e(1)
          y0e(1) = y0e(2)
          y0e(2) = caux
        endif
        pre = 1d0/aa/(y0(1)-y0(2))
        y0(3)  = 1d0+y0(1)/alp
        y0e(3) = 1d0+y0e(1)/alp
        y0(4)  = 1d0+y0(2)/alp
        y0e(4) = 1d0+y0e(2)/alp
        y0(5)  = (1d0-y0(1))/(1d0+alp)
        y0e(5) = (1d0-y0e(1))/(1d0+alp)
        y0(6)  = (1d0-y0(2))/(1d0+alp)
        y0e(6) = (1d0-y0e(2))/(1d0+alp)

        do i=1,6
          y1(i) = 1d0-1d0/y0(i)
          if (abs(dimag(y1(i)))/abs(y1(i)).lt.crit) y1(i) = 
     &      dcmplx( dreal(y1(i)),(abs(dimag(y1(i)))+abs(y1(i))*eps2)
     &                           *sign(1d0,dimag(1d0-1d0/y0e(i))) )
        enddo

        D0mzero_dd = 0d0
        do i=1,6
          sgn = (-1)**i
          if (i.eq.1) then
            a  = p3
            b  = -p3-m42+m32
            be = -p3-m42e+m32e
            c  = m42
            ce = m42e
          elseif (i.eq.3) then
            a  = p23
            b  = -p23-m22+m42
            be = -p23-m22e+m42e
            c  = m22
            ce = m22e
          elseif (i.eq.5) then
            a  = p2
            b  = -p2-m32+m22
            be = -p2-m32e+m22e
            c  = m32
            ce = m32e
          endif
          D0mzero_dd = D0mzero_dd-pre*sgn*S3(y0(i),a,b,c,y0e(i),a,be,ce)
          arg1(i) = a*y0(i)**2+be*y0(i)+ce
          if (i.ge.5) then
            D0mzero_dd = D0mzero_dd
     &          - pre*sgn*log(y1(i))*eta_dd(arg1(i-4),arg1(i)/arg1(i-4))
          elseif (i.ge.3) then
            D0mzero_dd = D0mzero_dd
     &          - pre*sgn*log(y1(i))*eta_dd(arg1(i-2),arg1(i)/arg1(i-2))
          endif
        enddo

        do i=1,6
          sgn = (-1)**i
          if (i.eq.1) then
            a  = 0d0
            b  = y13-y14
            be = y13e-y14e
            c  = y14
            ce = y14e
          elseif (i.eq.3) then
            b  = y14-y12
            be = y14e-y12e
            c  = y12
            ce = y12e
          elseif (i.eq.5) then
            b  = y12-y13
            be = y12e-y13e
            c  = y13
            ce = y13e
          endif
          D0mzero_dd = D0mzero_dd+pre*sgn*S3(y0(i),a,b,c,y0e(i),a,be,ce)
          arg2(i) = be*y0(i)+ce
          if (i.ge.5) then
            D0mzero_dd = D0mzero_dd
     &          + pre*sgn*log(y1(i))*eta_dd(arg2(i-4),arg2(i)/arg2(i-4))
          elseif (i.ge.3) then
            D0mzero_dd = D0mzero_dd
     &          + pre*sgn*log(y1(i))*eta_dd(arg2(i-2),arg2(i)/arg2(i-2))
          endif
        enddo

        checkD0mzero_dd = D0mzero_dd 

        end

************************************************************************
        function D0mzero_dd(p1,p2,p3,p4,p12,p23,m22,m32,m42)
************************************************************************
*       D0 function with one internal mass zero
*
*             p1 \           / p4         
*                 \    0    /
*                  *-------*
*                  |       |
*              m22 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \            p12,p23 not small !
*             p2 /           \ p3
*
*-----------------------------------------------------------------------
*       27.3.08 Stefan Dittmaier
************************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)                                         


        complex*16 D0mzero_dd,sqe_dd,eta_dd,cspen_dd
        complex*16 m22,m32,m42,m22e,m32e,m42e,pre,caux,ieps,ieps2
        complex*16 y12,y13,y14,y12e,y13e,y14e,x(6),xe(6),xx(6)
        complex*16 aa,bb,cc,aae,bbe,cce,r(2),re(2),arg,arge,etal
        integer i,j

        if (m22*m32*m42.eq.(0d0,0d0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mzero_dd: 3 masses must be non-zero!'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag) 
        endif

        eps   = 1d-13
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        ieps2 = dcmplx(0d0,eps2)
        crit  = 1d-9

        m22e = m22-abs(m22)*ieps
        m32e = m32-abs(m32)*ieps*1.3d0
        m42e = m42-abs(m42)*ieps*3.5d0

        rlam2 = (p2+p3-p23)**2-4d0*p2*p3
        if (rlam2.le.0d0) then
          rlam = 0d0
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'D0mzero_dd: lambda<=0 not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        else
          rlam = sqrt(rlam2)
        endif

        if (p2.eq.0d0) then
          alp = -1d0
        elseif (p3.eq.0d0) then
          if (p2.ne.p23) then
            alp = p23/(p2-p23)
          else
            alp = 0d0
            if (cout_on.and.(cout.le.coutmax)) then
              write(outchannel,*) 
     &           'D0mzero_dd: p3=0 and p2=p23 not supported'
              call DD_debugoutput()
            endif
            stopflag = min(-10,stopflag)
          endif
        else
          sig  = sign(1d0,p2-p3-p23)
          alp1 = (p2-p3-p23+sig*rlam)/2d0/p3
          alp2 = 2d0*p23/(p2-p3-p23+sig*rlam)
          if (abs(alp1-1d0).lt.abs(alp2-1d0)) then
            alp = alp1
          else
            alp = alp2
          endif
c          if (p2-p3-p23.gt.0d0) then
c            alp = (p2-p3-p23+rlam)/2d0/p3
c          else
c            alp = (p2-p3-p23-rlam)/2d0/p3
c          endif
        endif

        y12  = m22-p1
        y12e = m22e-p1
        y13  = m32-p12
        y13e = m32e-p12
        y14  = m42-p4
        y14e = m42e-p4

        aa  = -m42*(y12+alp*y13)+(m22+alp*m32-alp*p3-p23)*y14
        aae = -m42e*(y12e+alp*y13e)+(m22e+alp*m32e-alp*p3-p23)*y14e
        bb  = -(m32+m42-p3)*y12+(m22-m42-2d0*alp*m42-p23)*y13
     &        +(m22+m32+2d0*alp*m32-p2)*y14
        bbe = -(m32e+m42e-p3)*y12e+(m22e-m42e-2d0*alp*m42e-p23)*y13e
     &        +(m22e+m32e+2d0*alp*m32e-p2)*y14e
        cc  = -m32*y12-(-m22+m42+alp*m42+(1d0+alp)*p23/alp)*y13 
     &        +m32*(1d0+alp)*y14 
        cce = -m32e*y12e-(-m22e+m42e+alp*m42e+(1d0+alp)*p23/alp)*y13e 
     &        +m32e*(1d0+alp)*y14e 

        x(1)  = sqe_dd(aa,bb,cc)
        xe(1) = sqe_dd(aae,bbe,cce)
        x(2)  = cc/aa/x(1)
        xe(2) = cce/aae/xe(1)
        if (abs(x(1)-xe(1)).gt.abs(x(1)-xe(2))) then
          caux  = xe(1)
          xe(1) = xe(2)
          xe(2) = caux
        endif
        pre = 1d0/aa/(x(1)-x(2))
        x(3)  = -1d0/(1d0+alp+alp*x(1))
        xe(3) = -1d0/(1d0+alp+alp*xe(1))
        x(4)  = -1d0/(1d0+alp+alp*x(2))
        xe(4) = -1d0/(1d0+alp+alp*xe(2))
        x(5)  = alp+(1d0+alp)/x(1)
        xe(5) = alp+(1d0+alp)/xe(1)
        x(6)  = alp+(1d0+alp)/x(2)
        xe(6) = alp+(1d0+alp)/xe(2)

        D0mzero_dd = 0d0
        do i=1,6
          sgn = (-1)**(i+1)

          xx(i) = x(i)
          if (abs(dimag(xx(i)))/abs(xx(i)).lt.crit) xx(i) = 
     &      dcmplx( dreal(xx(i)),(abs(dimag(xx(i)))+abs(xx(i))*eps2)
     &                              *sign(1d0,dimag(xe(i))) )
          D0mzero_dd = D0mzero_dd + pre*sgn*cspen_dd(1d0+xx(i))

          if (i.eq.1) then
            r(1)  = sqe_dd(m42,m32+m42-p3,m32)
            re(1) = sqe_dd(m42e,m32e+m42e-p3,m32e)
            r(2)  = m32/m42/r(1)
            re(2) = m32e/m42e/re(1)
            if (abs(r(1)-re(1)).gt.abs(r(1)-re(2))) then
              caux  = re(1)
              re(1) = re(2)
              re(2) = caux
            endif
          elseif (i.eq.3) then
            r(1)  = sqe_dd(m22,m42+m22-p23,m42)
            re(1) = sqe_dd(m22e,m42e+m22e-p23,m42e)
            r(2)  = m42/m22/r(1)
            re(2) = m42e/m22e/re(1)
            if (abs(r(1)-re(1)).gt.abs(r(1)-re(2))) then
              caux  = re(1)
              re(1) = re(2)
              re(2) = caux
            endif
          elseif (i.eq.5) then
            r(1)  = sqe_dd(m32,m22+m32-p2,m22)
            re(1) = sqe_dd(m32e,m22e+m32e-p2,m22e)
            r(2)  = m22/m32/r(1)
            re(2) = m22e/m32e/re(1)
            if (abs(r(1)-re(1)).gt.abs(r(1)-re(2))) then
              caux  = re(1)
              re(1) = re(2)
              re(2) = caux
            endif
          endif
          do j=1,2
            arg  = 1d0-x(i)/r(j)
            arge = 1d0-xe(i)/re(j)
            if (abs(dimag(arg)).lt.crit*abs(arg)) arg = 
     &        dcmplx( dreal(arg),(abs(dimag(arg))+abs(arg)*eps2)
     &                              *sign(1d0,dimag(arge)) )
            etal = eta_dd(-xe(i),-1d0/re(j))
            if (etal.ne.(0d0,0d0)) etal = etal*log(arg)
            D0mzero_dd = D0mzero_dd  - pre*sgn*(cspen_dd(arg)+etal)
          enddo

          if (i.le.2) then
            if (dcmplx(p12).ne.m32) then
              arg  = 1d0+x(i)*y14/y13
              arge = 1d0+xe(i)*y14e/y13e
              if (abs(dimag(arg))/abs(arg).lt.crit) arg = 
     &          dcmplx( dreal(arg),(abs(dimag(arg))+abs(arg)*eps2)
     &                                *sign(1d0,dimag(arge)) )
              etal = eta_dd(-xe(i),y14e/y13e)
              if (etal.ne.(0d0,0d0)) etal = etal*log(arg)
              if (p12.ne.0d0) D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log(1d0-p12/m32-ieps2) )
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( cspen_dd(arg)+etal )
            else
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log((m42-p4)/m32-ieps2)
     &             -log(-xx(i))**2/2d0 )
            endif
          elseif (i.le.4) then
            if (dcmplx(p4).ne.m42) then
              arg  = 1d0+x(i)*y12/y14
              arge = 1d0+xe(i)*y12e/y14e
              if (abs(dimag(arg))/abs(arg).lt.crit) arg = 
     &          dcmplx( dreal(arg),(abs(dimag(arg))+abs(arg)*eps2)
     &                                *sign(1d0,dimag(arge)) )
              etal = eta_dd(-xe(i),y12e/y14e)
              if (etal.ne.(0d0,0d0)) etal = etal*log(arg)
              if (p4.ne.0d0) D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log(1d0-p4/m42-ieps2) )
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( cspen_dd(arg)+etal )
            else
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log((m22-p1)/m42-ieps2)
     &             -log(-xx(i))**2/2d0 )
            endif
          elseif (i.le.6) then
            if (dcmplx(p1).ne.m22) then
              arg  = 1d0+x(i)*y13/y12
              arge = 1d0+xe(i)*y13e/y12e
              if (abs(dimag(arg))/abs(arg).lt.crit) arg = 
     &          dcmplx( dreal(arg),(abs(dimag(arg))+abs(arg)*eps2)
     &                                *sign(1d0,dimag(arge)) )
              etal = eta_dd(-xe(i),y13e/y12e)
              if (etal.ne.(0d0,0d0)) etal = etal*log(arg)
              if (p1.ne.0d0) D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log(1d0-p1/m22-ieps2) )
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( cspen_dd(arg)+etal )
            else
              D0mzero_dd = D0mzero_dd  + pre*sgn
     &          *( -log(-xx(i))*log((m32-p12)/m22-ieps2)
     &             -log(-xx(i))**2/2d0 )
            endif
          endif
        enddo

        end

**********************************************************************
        function D0massive_dd(q1,q2,q3,q4,q12,q23,m12,m22,m32,m42)
**********************************************************************
*       scalar 4-point function with all masses =/= 0
*       masses can be complex
*
*             q1 \           / q4
*                 \   m12   /
*                  *-------*
*                  |       |
*              m22 |       | m42
*                  |       |
*                  *-------*
*                 /   m32   \
*             q2 /           \ q3
*---------------------------------------------------------------------
*       2.4.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_4pt
        implicit real*8 (a-z)


        complex*16 D0massive_dd,sqe_dd,cspen_dd,eta_dd,ieps
        complex*16 m12,m22,m32,m42,m2(4),m2e(4),caux,Y(4,4),Ye(4,4)
        complex*16 Ax(3),Bx(3),Cx(3),Axe(3),Bxe(3),Cxe(3)
        complex*16 x(9,2),xx(9,2),xe(9,2),pre,pre1,pre2
        complex*16 arg(0:9),arge(0:9),l(9),argl(9),argle(9),etal
        complex*16 r(9),re(9),s(9,2),se(9,2),rx(9,2),sx(9,2,2)
        real*8 p2(4,4),beta(3)
        integer i,j,k

        if (m12*m22*m32*m42.eq.(0d0,0d0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0massive_dd: 4 masses must be non-zero !'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag)
        endif

c        eps   = 1d-13
        eps   = 1d-12
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        crit  = 1d-9
        critcalc = 1d-9

        p2(1,2) = q1
        p2(2,3) = q2
        p2(3,4) = q3
        p2(1,4) = q4
        p2(1,3) = q12
        p2(2,4) = q23
        m2(1) = m12
        m2(2) = m22
        m2(3) = m32
        m2(4) = m42

        m2e(1) = m2(1)-abs(m2(1))*ieps
        m2e(2) = m2(2)-abs(m2(2))*ieps*1.3d0
        m2e(3) = m2(3)-abs(m2(3))*ieps*2.1d0
        m2e(4) = m2(4)-abs(m2(4))*ieps*3.1d0

        do i=1,3
        do j=i+1,4
          Y(i,j)  = m2(i)+m2(j)-p2(i,j)
          Ye(i,j) = m2e(i)+m2e(j)-p2(i,j)
        enddo
        enddo

c real variables for shifts in Euler transformations
        alp = 0d0
        if ((q23+q3-q2)**2-4d0*q23*q3.gt.0d0) then
          alp = dreal(sqe_dd(dcmplx(q23),dcmplx(-q2+q23+q3),dcmplx(q3)))
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*)'D0massive_dd: complex alp not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        beta(1) = 0d0
        if ((q12+q3-q4)**2-4d0*q12*q3.gt.0d0) then
          beta(1) = dreal(sqe_dd(dcmplx(q3),dcmplx(q12+q3-q4),
     &                           dcmplx(q12)))
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0massive_dd: complex beta(1) not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        beta(2) = 0d0
        if ((q1+q2-q12)**2-4d0*q1*q2.gt.0d0) then
          beta(2) = dreal(sqe_dd(dcmplx(q2),dcmplx(q1+q2-q12),
     &                           dcmplx(q1)))
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0massive_dd: complex beta(2) not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        beta(3) = 0d0
        if ((q23+q4-q1)**2-4d0*q23*q4.gt.0d0) then
          beta(3) = dreal(sqe_dd(dcmplx(q23),dcmplx(q23+q4-q1),
     &                           dcmplx(q4)))
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0massive_dd: complex beta(3) not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif

        if (abs(alp).lt.critcalc) alp = 0d0
        if (abs(alp+1d0).lt.critcalc) alp = -1d0
        do i=1,3
          if (abs(beta(i)).lt.critcalc) beta(i) = 0d0
          if (abs(beta(i)+1d0).lt.critcalc) beta(i) = -1d0
        enddo

c complex solutions of quadratic equations for denominators
        AX(1) = -(2D0*M2(3)*M2(4))-2D0*BETA(1)*M2(3)*M2(4)+2D0*
     &  BETA(1)*M2(4)**2+M2(4)*Y(1,3)+M2(4)*Y(1,4)-M2(4)*Y(3,4)-2D0*
     &  BETA(1)*M2(4)*Y(3,4)-Y(1,4)*Y(3,4)+Y(3,4)**2+BETA(1)*Y(3,
     &  4)**2+2D0*BETA(1)*M2(4)**2*ALP+M2(4)*Y(1,2)*ALP+M2(4)*Y(1,
     &  4)*ALP-M2(4)*Y(2,3)*ALP-BETA(1)*M2(4)*Y(2,3)*ALP-BETA(1)*
     &  M2(4)*Y(2,4)*ALP-Y(1,4)*Y(2,4)*ALP-M2(4)*Y(3,4)*ALP-BETA(1)*
     &  M2(4)*Y(3,4)*ALP+Y(2,4)*Y(3,4)*ALP+BETA(1)*Y(2,4)*Y(3,4)*ALP
        BX(1) = -(4D0*M2(3)*M2(4))-8D0*BETA(1)*M2(3)*M2(4)+2D0*
     &  M2(4)*Y(1,3)-2D0*M2(3)*Y(1,4)+2D0*M2(3)*Y(3,4)+2D0*BETA(1)*
     &  M2(3)*Y(3,4)+2D0*BETA(1)*M2(4)*Y(3,4)-4D0*M2(3)*M2(4)*ALP-
     &  4D0*BETA(1)*M2(3)*M2(4)*ALP+2D0*M2(4)*Y(1,3)*ALP-2D0*
     &  BETA(1)*M2(4)*Y(2,3)*ALP-Y(1,4)*Y(2,3)*ALP+2D0*M2(3)*Y(2,4)*
     &  ALP+2D0*BETA(1)*M2(3)*Y(2,4)*ALP-Y(1,3)*Y(2,4)*ALP+2D0*
     &  BETA(1)*M2(4)*Y(3,4)*ALP+Y(1,2)*Y(3,4)*ALP
        CX(1) = 2D0*M2(3)**2+2D0*BETA(1)*M2(3)**2-2D0*BETA(1)*M2(3)*
     &  M2(4)-M2(3)*Y(1,3)-M2(3)*Y(1,4)-M2(3)*Y(3,4)-2D0*BETA(1)*
     &  M2(3)*Y(3,4)+Y(1,3)*Y(3,4)+BETA(1)*Y(3,4)**2-2D0*BETA(1)*
     &  M2(3)*M2(4)*ALP+M2(3)*Y(1,2)*ALP-M2(3)*Y(1,4)*ALP+M2(3)*Y(2,
     &  3)*ALP+BETA(1)*M2(3)*Y(2,3)*ALP-Y(1,3)*Y(2,3)*ALP+BETA(1)*
     &  M2(3)*Y(2,4)*ALP-M2(3)*Y(3,4)*ALP-BETA(1)*M2(3)*Y(3,4)*ALP+
     &  Y(1,3)*Y(3,4)*ALP-BETA(1)*Y(2,3)*Y(3,4)*ALP+BETA(1)*Y(3,4)**
     &  2*ALP
        AX(2) = -(2D0*BETA(2)*M2(3)**2)-M2(3)*Y(1,3)-M2(3)*Y(1,4)+
     &  M2(3)*Y(2,3)+BETA(2)*M2(3)*Y(2,3)+M2(3)*Y(2,4)+BETA(2)*
     &  M2(3)*Y(2,4)+BETA(2)*M2(3)*Y(3,4)+Y(1,3)*Y(3,4)-Y(2,3)*Y(3,
     &  4)-BETA(2)*Y(2,3)*Y(3,4)-2D0*M2(2)*M2(3)*ALP-2D0*BETA(2)*
     &  M2(2)*M2(3)*ALP+M2(3)*Y(1,2)*ALP-M2(3)*Y(1,4)*ALP-BETA(2)*
     &  M2(3)*Y(2,3)*ALP-Y(1,3)*Y(2,3)*ALP+Y(2,3)**2*ALP+BETA(2)*
     &  Y(2,3)**2*ALP+M2(3)*Y(2,4)*ALP+BETA(2)*M2(3)*Y(2,4)*ALP+
     &  BETA(2)*M2(3)*Y(3,4)*ALP+Y(1,3)*Y(3,4)*ALP-Y(2,3)*Y(3,4)*
     &  ALP-BETA(2)*Y(2,3)*Y(3,4)*ALP
        BX(2) = 4D0*M2(2)*M2(3)+4D0*BETA(2)*M2(2)*M2(3)-2D0*M2(3)*
     &  Y(1,2)-2D0*BETA(2)*M2(3)*Y(2,3)-Y(1,4)*Y(2,3)+2D0*BETA(2)*
     &  M2(3)*Y(2,4)+Y(1,3)*Y(2,4)-2D0*M2(2)*Y(3,4)-2D0*BETA(2)*
     &  M2(2)*Y(3,4)+Y(1,2)*Y(3,4)-4D0*BETA(2)*M2(2)*M2(3)*ALP-2D0*
     &  M2(2)*Y(1,3)*ALP+2D0*M2(2)*Y(2,3)*ALP+2D0*BETA(2)*M2(2)*Y(2,
     &  3)*ALP-Y(1,4)*Y(2,3)*ALP+2D0*BETA(2)*M2(3)*Y(2,4)*ALP+Y(1,
     &  3)*Y(2,4)*ALP-2D0*M2(2)*Y(3,4)*ALP-2D0*BETA(2)*M2(2)*Y(3,4)*
     &  ALP+Y(1,2)*Y(3,4)*ALP
        CX(2) = 2D0*BETA(2)*M2(2)*M2(3)+M2(2)*Y(1,3)-M2(2)*Y(1,4)+
     &  M2(2)*Y(2,3)+BETA(2)*M2(2)*Y(2,3)-Y(1,2)*Y(2,3)-BETA(2)*Y(2,
     &  3)**2-M2(2)*Y(2,4)-BETA(2)*M2(2)*Y(2,4)+Y(1,2)*Y(2,4)+
     &  BETA(2)*Y(2,3)*Y(2,4)-BETA(2)*M2(2)*Y(3,4)+2D0*M2(2)**2*ALP+
     &  2D0*BETA(2)*M2(2)**2*ALP-M2(2)*Y(1,2)*ALP-M2(2)*Y(1,4)*ALP-
     &  BETA(2)*M2(2)*Y(2,3)*ALP-M2(2)*Y(2,4)*ALP-BETA(2)*M2(2)*Y(2,
     &  4)*ALP+Y(1,2)*Y(2,4)*ALP+BETA(2)*Y(2,3)*Y(2,4)*ALP-BETA(2)*
     &  M2(2)*Y(3,4)*ALP
        AX(3) = 2D0*M2(2)*M2(4)+2D0*BETA(3)*M2(2)*M2(4)+M2(2)*Y(1,
     &  3)-M2(2)*Y(1,4)-BETA(3)*M2(2)*Y(2,3)-Y(1,2)*Y(2,3)+BETA(3)*
     &  M2(2)*Y(2,4)+Y(1,2)*Y(2,4)+Y(2,3)*Y(2,4)+BETA(3)*Y(2,3)*Y(2,
     &  4)-Y(2,4)**2-BETA(3)*Y(2,4)**2-M2(2)*Y(3,4)-BETA(3)*M2(2)*
     &  Y(3,4)-2D0*BETA(3)*M2(2)**2*ALP+2D0*M2(2)*M2(4)*ALP+2D0*
     &  BETA(3)*M2(2)*M2(4)*ALP-M2(2)*Y(1,2)*ALP-M2(2)*Y(1,4)*ALP+
     &  M2(2)*Y(2,4)*ALP+2D0*BETA(3)*M2(2)*Y(2,4)*ALP+Y(1,2)*Y(2,4)*
     &  ALP-Y(2,4)**2*ALP-BETA(3)*Y(2,4)**2*ALP
        BX(3) = 4D0*BETA(3)*M2(2)*M2(4)+2D0*M2(4)*Y(1,2)+2D0*M2(4)*
     &  Y(2,3)+2D0*BETA(3)*M2(4)*Y(2,3)-Y(1,4)*Y(2,3)-2D0*M2(4)*Y(2,
     &  4)-2D0*BETA(3)*M2(4)*Y(2,4)+Y(1,3)*Y(2,4)-2D0*BETA(3)*M2(2)*
     &  Y(3,4)-Y(1,2)*Y(3,4)+4D0*M2(2)*M2(4)*ALP+8D0*BETA(3)*M2(2)*
     &  M2(4)*ALP+2D0*M2(4)*Y(1,2)*ALP-2D0*M2(2)*Y(1,4)*ALP-2D0*
     &  BETA(3)*M2(2)*Y(2,4)*ALP-2D0*M2(4)*Y(2,4)*ALP-2D0*BETA(3)*
     &  M2(4)*Y(2,4)*ALP
        CX(3) = -(2D0*M2(4)**2)-2D0*BETA(3)*M2(4)**2+M2(4)*Y(1,3)+
     &  M2(4)*Y(1,4)+BETA(3)*M2(4)*Y(2,3)+BETA(3)*M2(4)*Y(2,4)+
     &  M2(4)*Y(3,4)+BETA(3)*M2(4)*Y(3,4)-Y(1,4)*Y(3,4)-BETA(3)*Y(2,
     &  4)*Y(3,4)+2D0*BETA(3)*M2(2)*M2(4)*ALP-2D0*M2(4)**2*ALP-2D0*
     &  BETA(3)*M2(4)**2*ALP+M2(4)*Y(1,2)*ALP+M2(4)*Y(1,4)*ALP+
     &  M2(4)*Y(2,4)*ALP+2D0*BETA(3)*M2(4)*Y(2,4)*ALP-Y(1,4)*Y(2,4)*
     &  ALP-BETA(3)*Y(2,4)**2*ALP

        AXE(1) = -(2D0*M2E(3)*M2E(4))-2D0*BETA(1)*M2E(3)*M2E(4)+2D0*
     &  BETA(1)*M2E(4)**2+M2E(4)*YE(1,3)+M2E(4)*YE(1,4)-M2E(4)*YE(3,
     &  4)-2D0*BETA(1)*M2E(4)*YE(3,4)-YE(1,4)*YE(3,4)+YE(3,4)**2+
     &  BETA(1)*YE(3,4)**2+2D0*BETA(1)*M2E(4)**2*ALP+M2E(4)*YE(1,2)*
     &  ALP+M2E(4)*YE(1,4)*ALP-M2E(4)*YE(2,3)*ALP-BETA(1)*M2E(4)*
     &  YE(2,3)*ALP-BETA(1)*M2E(4)*YE(2,4)*ALP-YE(1,4)*YE(2,4)*ALP-
     &  M2E(4)*YE(3,4)*ALP-BETA(1)*M2E(4)*YE(3,4)*ALP+YE(2,4)*YE(3,
     &  4)*ALP+BETA(1)*YE(2,4)*YE(3,4)*ALP
        BXE(1) = -(4D0*M2E(3)*M2E(4))-8D0*BETA(1)*M2E(3)*M2E(4)+2D0*
     &  M2E(4)*YE(1,3)-2D0*M2E(3)*YE(1,4)+2D0*M2E(3)*YE(3,4)+2D0*
     &  BETA(1)*M2E(3)*YE(3,4)+2D0*BETA(1)*M2E(4)*YE(3,4)-4D0*
     &  M2E(3)*M2E(4)*ALP-4D0*BETA(1)*M2E(3)*M2E(4)*ALP+2D0*M2E(4)*
     &  YE(1,3)*ALP-2D0*BETA(1)*M2E(4)*YE(2,3)*ALP-YE(1,4)*YE(2,3)*
     &  ALP+2D0*M2E(3)*YE(2,4)*ALP+2D0*BETA(1)*M2E(3)*YE(2,4)*ALP-
     &  YE(1,3)*YE(2,4)*ALP+2D0*BETA(1)*M2E(4)*YE(3,4)*ALP+YE(1,2)*
     &  YE(3,4)*ALP
        CXE(1) = 2D0*M2E(3)**2+2D0*BETA(1)*M2E(3)**2-2D0*BETA(1)*
     &  M2E(3)*M2E(4)-M2E(3)*YE(1,3)-M2E(3)*YE(1,4)-M2E(3)*YE(3,4)-
     &  2D0*BETA(1)*M2E(3)*YE(3,4)+YE(1,3)*YE(3,4)+BETA(1)*YE(3,4)**
     &  2-2D0*BETA(1)*M2E(3)*M2E(4)*ALP+M2E(3)*YE(1,2)*ALP-M2E(3)*
     &  YE(1,4)*ALP+M2E(3)*YE(2,3)*ALP+BETA(1)*M2E(3)*YE(2,3)*ALP-
     &  YE(1,3)*YE(2,3)*ALP+BETA(1)*M2E(3)*YE(2,4)*ALP-M2E(3)*YE(3,
     &  4)*ALP-BETA(1)*M2E(3)*YE(3,4)*ALP+YE(1,3)*YE(3,4)*ALP-
     &  BETA(1)*YE(2,3)*YE(3,4)*ALP+BETA(1)*YE(3,4)**2*ALP
        AXE(2) = -(2D0*BETA(2)*M2E(3)**2)-M2E(3)*YE(1,3)-M2E(3)*
     &  YE(1,4)+M2E(3)*YE(2,3)+BETA(2)*M2E(3)*YE(2,3)+M2E(3)*YE(2,
     &  4)+BETA(2)*M2E(3)*YE(2,4)+BETA(2)*M2E(3)*YE(3,4)+YE(1,3)*
     &  YE(3,4)-YE(2,3)*YE(3,4)-BETA(2)*YE(2,3)*YE(3,4)-2D0*M2E(2)*
     &  M2E(3)*ALP-2D0*BETA(2)*M2E(2)*M2E(3)*ALP+M2E(3)*YE(1,2)*ALP-
     &  M2E(3)*YE(1,4)*ALP-BETA(2)*M2E(3)*YE(2,3)*ALP-YE(1,3)*YE(2,
     &  3)*ALP+YE(2,3)**2*ALP+BETA(2)*YE(2,3)**2*ALP+M2E(3)*YE(2,4)*
     &  ALP+BETA(2)*M2E(3)*YE(2,4)*ALP+BETA(2)*M2E(3)*YE(3,4)*ALP+
     &  YE(1,3)*YE(3,4)*ALP-YE(2,3)*YE(3,4)*ALP-BETA(2)*YE(2,3)*
     &  YE(3,4)*ALP
        BXE(2) = 4D0*M2E(2)*M2E(3)+4D0*BETA(2)*M2E(2)*M2E(3)-2D0*
     &  M2E(3)*YE(1,2)-2D0*BETA(2)*M2E(3)*YE(2,3)-YE(1,4)*YE(2,3)+
     &  2D0*BETA(2)*M2E(3)*YE(2,4)+YE(1,3)*YE(2,4)-2D0*M2E(2)*YE(3,
     &  4)-2D0*BETA(2)*M2E(2)*YE(3,4)+YE(1,2)*YE(3,4)-4D0*BETA(2)*
     &  M2E(2)*M2E(3)*ALP-2D0*M2E(2)*YE(1,3)*ALP+2D0*M2E(2)*YE(2,3)*
     &  ALP+2D0*BETA(2)*M2E(2)*YE(2,3)*ALP-YE(1,4)*YE(2,3)*ALP+2D0*
     &  BETA(2)*M2E(3)*YE(2,4)*ALP+YE(1,3)*YE(2,4)*ALP-2D0*M2E(2)*
     &  YE(3,4)*ALP-2D0*BETA(2)*M2E(2)*YE(3,4)*ALP+YE(1,2)*YE(3,4)*
     &  ALP
        CXE(2) = 2D0*BETA(2)*M2E(2)*M2E(3)+M2E(2)*YE(1,3)-M2E(2)*
     &  YE(1,4)+M2E(2)*YE(2,3)+BETA(2)*M2E(2)*YE(2,3)-YE(1,2)*YE(2,
     &  3)-BETA(2)*YE(2,3)**2-M2E(2)*YE(2,4)-BETA(2)*M2E(2)*YE(2,4)+
     &  YE(1,2)*YE(2,4)+BETA(2)*YE(2,3)*YE(2,4)-BETA(2)*M2E(2)*YE(3,
     &  4)+2D0*M2E(2)**2*ALP+2D0*BETA(2)*M2E(2)**2*ALP-M2E(2)*YE(1,
     &  2)*ALP-M2E(2)*YE(1,4)*ALP-BETA(2)*M2E(2)*YE(2,3)*ALP-M2E(2)*
     &  YE(2,4)*ALP-BETA(2)*M2E(2)*YE(2,4)*ALP+YE(1,2)*YE(2,4)*ALP+
     &  BETA(2)*YE(2,3)*YE(2,4)*ALP-BETA(2)*M2E(2)*YE(3,4)*ALP
        AXE(3) = 2D0*M2E(2)*M2E(4)+2D0*BETA(3)*M2E(2)*M2E(4)+M2E(2)*
     &  YE(1,3)-M2E(2)*YE(1,4)-BETA(3)*M2E(2)*YE(2,3)-YE(1,2)*YE(2,
     &  3)+BETA(3)*M2E(2)*YE(2,4)+YE(1,2)*YE(2,4)+YE(2,3)*YE(2,4)+
     &  BETA(3)*YE(2,3)*YE(2,4)-YE(2,4)**2-BETA(3)*YE(2,4)**2-
     &  M2E(2)*YE(3,4)-BETA(3)*M2E(2)*YE(3,4)-2D0*BETA(3)*M2E(2)**2*
     &  ALP+2D0*M2E(2)*M2E(4)*ALP+2D0*BETA(3)*M2E(2)*M2E(4)*ALP-
     &  M2E(2)*YE(1,2)*ALP-M2E(2)*YE(1,4)*ALP+M2E(2)*YE(2,4)*ALP+
     &  2D0*BETA(3)*M2E(2)*YE(2,4)*ALP+YE(1,2)*YE(2,4)*ALP-YE(2,4)**
     &  2*ALP-BETA(3)*YE(2,4)**2*ALP
        BXE(3) = 4D0*BETA(3)*M2E(2)*M2E(4)+2D0*M2E(4)*YE(1,2)+2D0*
     &  M2E(4)*YE(2,3)+2D0*BETA(3)*M2E(4)*YE(2,3)-YE(1,4)*YE(2,3)-
     &  2D0*M2E(4)*YE(2,4)-2D0*BETA(3)*M2E(4)*YE(2,4)+YE(1,3)*YE(2,
     &  4)-2D0*BETA(3)*M2E(2)*YE(3,4)-YE(1,2)*YE(3,4)+4D0*M2E(2)*
     &  M2E(4)*ALP+8D0*BETA(3)*M2E(2)*M2E(4)*ALP+2D0*M2E(4)*YE(1,2)*
     &  ALP-2D0*M2E(2)*YE(1,4)*ALP-2D0*BETA(3)*M2E(2)*YE(2,4)*ALP-
     &  2D0*M2E(4)*YE(2,4)*ALP-2D0*BETA(3)*M2E(4)*YE(2,4)*ALP
        CXE(3) = -(2D0*M2E(4)**2)-2D0*BETA(3)*M2E(4)**2+M2E(4)*YE(1,
     &  3)+M2E(4)*YE(1,4)+BETA(3)*M2E(4)*YE(2,3)+BETA(3)*M2E(4)*
     &  YE(2,4)+M2E(4)*YE(3,4)+BETA(3)*M2E(4)*YE(3,4)-YE(1,4)*YE(3,
     &  4)-BETA(3)*YE(2,4)*YE(3,4)+2D0*BETA(3)*M2E(2)*M2E(4)*ALP-
     &  2D0*M2E(4)**2*ALP-2D0*BETA(3)*M2E(4)**2*ALP+M2E(4)*YE(1,2)*
     &  ALP+M2E(4)*YE(1,4)*ALP+M2E(4)*YE(2,4)*ALP+2D0*BETA(3)*
     &  M2E(4)*YE(2,4)*ALP-YE(1,4)*YE(2,4)*ALP-BETA(3)*YE(2,4)**2*
     &  ALP

        do i=1,3
          x(3*i,1)  = sqe_dd(ax(i),bx(i),cx(i))
          x(3*i,2)  = cx(i)/ax(i)/x(3*i,1)
          xe(3*i,1) = sqe_dd(axe(i),bxe(i),cxe(i))
          xe(3*i,2) = cxe(i)/axe(i)/xe(3*i,1)
        enddo

c common prefactor
        pre = -1d0/ax(3)/(x(9,1)-x(9,2))
        if (alp.ne.0d0) then
          pre1 = -alp/ax(1)/(x(3,1)-x(3,2))
          if (dreal(pre1/pre).lt.0d0) then
            caux   = x(3,1)
            x(3,1) = x(3,2)
            x(3,2) = caux
          endif
        endif
        if (alp.ne.-1d0) then
          pre2 = (1d0+alp)/ax(2)/(x(6,1)-x(6,2))
          if (dreal(pre2/pre).lt.0d0) then
            caux   = x(6,1)
            x(6,1) = x(6,2)
            x(6,2) = caux
          endif
        endif

        do i=1,3
          if (abs(x(3*i,1)-xe(3*i,1)).gt.abs(x(3*i,1)-xe(3*i,2))) then
            caux      = xe(3*i,1)
            xe(3*i,1) = xe(3*i,2)
            xe(3*i,2) = caux
          endif
        enddo

c remaining zeroes of denominators
        do i=1,3
          x(3*i-1,1)  = 1d0/(beta(i)+(1d0+beta(i))*x(3*i,1))
          x(3*i-1,2)  = 1d0/(beta(i)+(1d0+beta(i))*x(3*i,2))
          xe(3*i-1,1) = 1d0/(beta(i)+(1d0+beta(i))*xe(3*i,1))
          xe(3*i-1,2) = 1d0/(beta(i)+(1d0+beta(i))*xe(3*i,2))
          x(3*i-2,1)  = -x(3*i,2)*x(3*i-1,2)
          x(3*i-2,2)  = -x(3*i,1)*x(3*i-1,1)
          xe(3*i-2,1) = -xe(3*i,2)*xe(3*i-1,2)
          xe(3*i-2,2) = -xe(3*i,1)*xe(3*i-1,1)
        enddo

c inverse zeroes of arguments of logarithms
        S(1,1) = sqe_dd(M2(3),Y(1,3),M2(1))
        S(2,1) = sqe_dd(M2(4),Y(1,4),M2(1))
        S(3,1) = sqe_dd(M2(3),Y(3,4),M2(4))
        S(4,1) = sqe_dd(M2(2),Y(1,2),M2(1))
        S(5,1) = sqe_dd(M2(3),Y(1,3),M2(1))
        S(6,1) = sqe_dd(M2(2),Y(2,3),M2(3))
        S(7,1) = sqe_dd(M2(4),Y(1,4),M2(1))
        S(8,1) = sqe_dd(M2(2),Y(1,2),M2(1))
        S(9,1) = sqe_dd(M2(4),Y(2,4),M2(2))

        Se(1,1) = sqe_dd(M2e(3),Ye(1,3),M2e(1))
        Se(2,1) = sqe_dd(M2e(4),Ye(1,4),M2e(1))
        Se(3,1) = sqe_dd(M2e(3),Ye(3,4),M2e(4))
        Se(4,1) = sqe_dd(M2e(2),Ye(1,2),M2e(1))
        Se(5,1) = sqe_dd(M2e(3),Ye(1,3),M2e(1))
        Se(6,1) = sqe_dd(M2e(2),Ye(2,3),M2e(3))
        Se(7,1) = sqe_dd(M2e(4),Ye(1,4),M2e(1))
        Se(8,1) = sqe_dd(M2e(2),Ye(1,2),M2e(1))
        Se(9,1) = sqe_dd(M2e(4),Ye(2,4),M2e(2))

        S(1,2) = M2(1)/M2(3)/S(1,1)
        S(2,2) = M2(1)/M2(4)/S(2,1)
        S(3,2) = M2(4)/M2(3)/S(3,1)
        S(4,2) = M2(1)/M2(2)/S(4,1)
        S(5,2) = M2(1)/M2(3)/S(5,1)
        S(6,2) = M2(3)/M2(2)/S(6,1)
        S(7,2) = M2(1)/M2(4)/S(7,1)
        S(8,2) = M2(1)/M2(2)/S(8,1)
        S(9,2) = M2(2)/M2(4)/S(9,1)

        Se(1,2) = M2e(1)/M2e(3)/Se(1,1)
        Se(2,2) = M2e(1)/M2e(4)/Se(2,1)
        Se(3,2) = M2e(4)/M2e(3)/Se(3,1)
        Se(4,2) = M2e(1)/M2e(2)/Se(4,1)
        Se(5,2) = M2e(1)/M2e(3)/Se(5,1)
        Se(6,2) = M2e(3)/M2e(2)/Se(6,1)
        Se(7,2) = M2e(1)/M2e(4)/Se(7,1)
        Se(8,2) = M2e(1)/M2e(2)/Se(8,1)
        Se(9,2) = M2e(2)/M2e(4)/Se(9,1)

        do i=1,9
          if (abs(S(i,1)-Se(i,1)).gt.abs(S(i,1)-Se(i,2))) then
            caux    = Se(i,1)
            Se(i,1) = Se(i,2)
            Se(i,2) = caux
          endif
        enddo

        arg(0)  = Y(1,3)-Y(1,4)*(1d0+ALP)+Y(1,2)*ALP
        arge(0) = Ye(1,3)-Ye(1,4)*(1d0+ALP)+Ye(1,2)*ALP
        arg(1)  = 2D0*M2(3)-(1D0+ALP)*Y(3,4)+Y(2,3)*ALP
        arge(1) = 2D0*M2e(3)-(1D0+ALP)*Ye(3,4)+Ye(2,3)*ALP
        arg(2)  = -2D0*(1D0+ALP)*M2(4)+Y(3,4)+Y(2,4)*ALP
        arge(2) = -2D0*(1D0+ALP)*M2e(4)+Ye(3,4)+Ye(2,4)*ALP
        arg(4)  = Y(2,3)-(1D0+ALP)*Y(2,4)+2D0*M2(2)*ALP
        arge(4) = Ye(2,3)-(1D0+ALP)*Ye(2,4)+2D0*M2e(2)*ALP

        m2abs = abs(m2(1))+abs(m2(2))+abs(m2(3))+abs(m2(4))
        if (abs(arg(0)/m2abs).lt.critcalc) arg(0) = 0d0
        if (abs(arg(1)/m2abs).lt.critcalc) arg(1) = 0d0
        if (abs(arg(2)/m2abs).lt.critcalc) arg(2) = 0d0
        if (abs(arg(4)/m2abs).lt.critcalc) arg(4) = 0d0

        arg(3)  = arg(1)
        arg(5)  = arg(1)
        arg(6)  = arg(4)
        arg(7)  = arg(2)
        arg(8)  = arg(4)
        arg(9)  = arg(2)
        arge(3) = arge(1)
        arge(5) = arge(1)
        arge(6) = arge(4)
        arge(7) = arge(2)
        arge(8) = arge(4)
        arge(9) = arge(2)

        D0massive_dd = 0d0

        do 100 j=1,9
        
          if (j.eq.1) then
            if ((alp.eq.0d0).or.(beta(1).eq.0d0)) goto 100
            L(1) = -LOG(M2(3))
            if (arg(j).ne.(0d0,0d0)) then
              argl(1)  = arg(1)
              argle(1) = arge(1)
              R(1)  = -arg(0)/arg(1)
              Re(1) = -arge(0)/arge(1)
            else
              argl(1)  = arg(0)
              argle(1) = arge(0)
            endif
          elseif (j.eq.2) then
            if ((alp.eq.0d0).or.(beta(1).eq.-1d0)) goto 100
            L(2) = -LOG(M2(4))
            if (arg(j).ne.(0d0,0d0)) then
              argl(2)  = arg(2)
              argle(2) = arge(2)
              R(2)  = -arg(0)/arg(2)
              Re(2) = -arge(0)/arge(2)
            else
              argl(2)  = arg(0)
              argle(2) = arge(0)
            endif
          elseif (j.eq.3) then
            if (alp.eq.0d0) goto 100
            L(3) = -LOG(M2(3))
            if (arg(j).ne.(0d0,0d0)) then
              argl(3)  = arg(1)
              argle(3) = arge(1)
              R(3)  = -arg(2)/arg(1)
              Re(3) = -arge(2)/arge(1)
            else
              argl(3)  = arg(2)
              argle(3) = arge(2)
            endif
          elseif (j.eq.4) then
            if ((alp.eq.-1d0).or.(beta(2).eq.0d0)) goto 100
            L(4) = -LOG(M2(2))
            if (arg(j).ne.(0d0,0d0)) then
              argl(4)  = arg(4)
              argle(4) = arge(4)
              R(4)  = -arg(0)/arg(4)
              Re(4) = -arge(0)/arge(4)
            else
              argl(4)  = arg(0)
              argle(4) = arge(0)
            endif
          elseif (j.eq.5) then
            if ((alp.eq.-1d0).or.(beta(2).eq.-1d0)) goto 100
            L(5) = -LOG(M2(3))
            if (arg(j).ne.(0d0,0d0)) then
              argl(5)  = arg(1)
              argle(5) = arge(1)
              R(5)  = -arg(0)/arg(1)
              Re(5) = -arge(0)/arge(1)
            else
              argl(5)  = arg(0)
              argle(5) = arge(0)
            endif
          elseif (j.eq.6) then
            if (alp.eq.-1d0) goto 100
            L(6) = -LOG(M2(2))
            if (arg(j).ne.(0d0,0d0)) then
              argl(6)  = arg(4)
              argle(6) = arge(4)
              R(6)  = -arg(1)/arg(4)
              Re(6) = -arge(1)/arge(4)
            else
              argl(6)  = arg(1)
              argle(6) = arge(1)
            endif
          elseif (j.eq.7) then
            if (beta(3).eq.0d0) goto 100
            L(7) = -LOG(M2(4))
            if (arg(j).ne.(0d0,0d0)) then
              argl(7)  = arg(2)
              argle(7) = arge(2)
              R(7)  = -arg(0)/arg(2)
              Re(7) = -arge(0)/arge(2)
            else
              argl(7)  = arg(0)
              argle(7) = arge(0)
            endif
          elseif (j.eq.8) then
            if (beta(3).eq.-1d0) goto 100
            L(8) = -LOG(M2(2))
            if (arg(j).ne.(0d0,0d0)) then
              argl(8)  = arg(4)
              argle(8) = arge(4)
              R(8)  = -arg(0)/arg(4)
              Re(8) = -arge(0)/arge(4)
            else
              argl(8)  = arg(0)
              argle(8) = arge(0)
            endif
          elseif (j.eq.9) then
            L(9) = -LOG(M2(4))
            if (arg(j).ne.(0d0,0d0)) then
              argl(9)  = arg(2)
              argle(9) = arge(2)
              R(9)  = -arg(4)/arg(2)
              Re(9) = -arge(4)/arge(2)
            else
              argl(9)  = arg(4)
              argle(9) = arge(4)
            endif
          endif

          if (abs(dimag(argl(j)))/abs(argl(j)).lt.crit) then
            l(j) = l(j) + log(
     &         dcmplx( dreal(argl(j)),
     &                 (abs(dimag(argl(j)))+abs(argl(j))*eps2)
     &                 *sign(1d0,dimag(argle(j))) ))
          else
            l(j) = l(j) + log(argl(j))
          endif

        do i=1,2
          sgn = (-1)**(i+1)
          xx(j,i) = x(j,i)
          if (abs(dimag(xx(j,i)))/abs(xx(j,i)).lt.crit) xx(j,i) = 
     &       dcmplx( dreal(xx(j,i)),
     &               (abs(dimag(xx(j,i)))+abs(xx(j,i))*eps2)
     &               *sign(1d0,dimag(xe(j,i))) )
          D0massive_dd = D0massive_dd + sgn*pre*cspen_dd(1d0+xx(j,i)) 

          if (arg(j).ne.(0d0,0d0)) then
            rx(j,i) = 1d0-r(j)*x(j,i)
            if (abs(dimag(rx(j,i)))/abs(rx(j,i)).lt.crit) rx(j,i) = 
     &         dcmplx( dreal(rx(j,i)),
     &                 (abs(dimag(rx(j,i)))+abs(rx(j,i))*eps2)
     &                 *sign(1d0,dimag(-re(j)*xe(j,i))) )
            etal = eta_dd(-xe(j,i),-re(j))

            if (etal.ne.(0d0,0d0)) etal = etal*log(rx(j,i))
            D0massive_dd = D0massive_dd + sgn*pre*(
     &        - l(j)*log(-xx(j,i)) + cspen_dd(rx(j,i)) + etal )
          else
            D0massive_dd = D0massive_dd + sgn*pre*(
     &        - l(j)*log(-xx(j,i)) - log(-xx(j,i))**2/2d0 )
          endif

        do k=1,2
          sx(j,k,i) = 1d0-s(j,k)*x(j,i)
          if (abs(dimag(sx(j,k,i)))/abs(sx(j,k,i)).lt.crit) sx(j,k,i) = 
     &       dcmplx( dreal(sx(j,k,i)),
     &               (abs(dimag(sx(j,k,i)))+abs(sx(j,k,i))*eps2)
     &               *sign(1d0,dimag(-se(j,k)*xe(j,i))) )
          etal = eta_dd(-xe(j,i),-se(j,k))
          if (etal.ne.(0d0,0d0)) etal = etal*log(sx(j,k,i))
          D0massive_dd = D0massive_dd - sgn*pre*(
     &      + cspen_dd(sx(j,k,i)) + etal )

        end do
        end do

100     continue

        end

**********************************************************************
        subroutine D0args(p1,p2,p3,p4,p12,p23,m02,m12,m22,m32,n)
**********************************************************************
*       change arguments of D0 function
*---------------------------------------------------------------------
*       20.3.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)


        complex*16 m02,m12,m22,m32,xm02,xm12,xm22,xm32
        integer n

        xp1 =p1
        xp2 =p2
        xp3 =p3
        xp4 =p4
        xp12=p12
        xp23=p23
        xm02=m02
        xm12=m12
        xm22=m22
        xm32=m32

        if (n.eq.1) then
          return
        elseif (n.eq.2) then
          p2 =xp23
          p4 =xp12
          p12=xp4
          p23=xp2
          m22=xm32
          m32=xm22
        elseif (n.eq.3) then
          p1 =xp12
          p3 =xp23
          p12=xp1
          p23=xp3
          m12=xm22
          m22=xm12
        elseif (n.eq.4) then
          p1 =xp12
          p2 =xp3
          p3 =xp23
          p4 =xp1
          p12=xp4
          p23=xp2
          m12=xm22
          m22=xm32
          m32=xm12
        elseif (n.eq.5) then
          p1 =xp4
          p2 =xp23
          p3 =xp2
          p4 =xp12
          p12=xp1
          p23=xp3
          m12=xm32
          m22=xm12
          m32=xm22
        elseif (n.eq.6) then
          p1 =xp4
          p2 =xp3
          p3 =xp2
          p4 =xp1
          m12=xm32
          m32=xm12
        elseif (n.eq.7) then
          p2 =xp12
          p4 =xp23
          p12=xp2
          p23=xp4
          m02=xm12
          m12=xm02
        elseif (n.eq.8) then
          p1 =xp1
          p2 =xp4
          p3 =xp3
          p4 =xp2
          p12=xp23
          p23=xp12
          m02=xm12
          m12=xm02
          m22=xm32
          m32=xm22
        elseif (n.eq.9) then
          p1 =xp2
          p2 =xp12
          p3 =xp4
          p4 =xp23
          p12=xp1
          p23=xp3
          m02=xm12
          m12=xm22
          m22=xm02
        elseif (n.eq.10) then
          p1 =xp2
          p2 =xp3
          p3 =xp4
          p4 =xp1
          p12=xp23
          p23=xp12
          m02=xm12
          m12=xm22
          m22=xm32
          m32=xm02
        elseif (n.eq.11) then
          p1 =xp23
          p2 =xp4
          p3 =xp12
          p4 =xp2
          p12=xp1
          p23=xp3
          m02=xm12
          m12=xm32
          m22=xm02
          m32=xm22
        elseif (n.eq.12) then
          p1 =xp23
          p2 =xp3
          p3 =xp12
          p4 =xp1
          p12=xp2
          p23=xp4
          m02=xm12
          m12=xm32
          m32=xm02
        elseif (n.eq.13) then
          p1 =xp12
          p2 =xp1
          p3 =xp23
          p4 =xp3
          p12=xp2
          p23=xp4
          m02=xm22
          m12=xm02
          m22=xm12
        elseif (n.eq.14) then
          p1 =xp12
          p2 =xp4
          p3 =xp23
          p4 =xp2
          p12=xp3
          p23=xp1
          m02=xm22
          m12=xm02
          m22=xm32
          m32=xm12
        elseif (n.eq.15) then
          p1 =xp2
          p2 =xp1
          p3 =xp4
          p4 =xp3
          m02=xm22
          m22=xm02
        elseif (n.eq.16) then
          p1 =xp2
          p2 =xp23
          p3 =xp4
          p4 =xp12
          p12=xp3
          p23=xp1
          m02=xm22
          m22=xm32
          m32=xm02
        elseif (n.eq.17) then
          p1 =xp3
          p2 =xp4
          p3 =xp1
          p4 =xp2
          m02=xm22
          m12=xm32
          m22=xm02
          m32=xm12
        elseif (n.eq.18) then
          p1 =xp3
          p2 =xp23
          p3 =xp1
          p4 =xp12
          p12=xp2
          p23=xp4
          m02=xm22
          m12=xm32
          m22=xm12
          m32=xm02
        elseif (n.eq.19) then
          p1 =xp4
          p2 =xp1
          p3 =xp2
          p4 =xp3
          p12=xp23
          p23=xp12
          m02=xm32
          m12=xm02
          m22=xm12
          m32=xm22
        elseif (n.eq.20) then
          p1 =xp4
          p2 =xp12
          p3 =xp2
          p4 =xp23
          p12=xp3
          p23=xp1
          m02=xm32
          m12=xm02
          m32=xm12
        elseif (n.eq.21) then
          p1 =xp23
          p2 =xp1
          p3 =xp12
          p4 =xp3
          p12=xp4
          p23=xp2
          m02=xm32
          m22=xm02
          m32=xm22
        elseif (n.eq.22) then
          p1 =xp23
          p3 =xp12
          p12=xp3
          p23=xp1
          m02=xm32
          m32=xm02
        elseif (n.eq.23) then
          p1 =xp3
          p2 =xp12
          p3 =xp1
          p4 =xp23
          p12=xp4
          p23=xp2
          m02=xm32
          m12=xm22
          m22=xm02
          m32=xm12
        elseif (n.eq.24) then
          p1 =xp3
          p3 =xp1
          p12=xp23
          p23=xp12
          m02=xm32
          m12=xm22
          m22=xm12
          m32=xm02
        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0args: illegal arrangement of D0 arguments'
            call DD_debugoutput()
          endif
          stopflag = min(-9,stopflag) 
        endif

        end

**********************************************************************
        function D0ddfin(q1,q2,q3,q4,q12,q23,m02,m12,m22,m32)
**********************************************************************
*       scalar 4-point function
*       IR (soft and collinear) finite part
*---------------------------------------------------------------------
*       10.3.2008 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)


        complex*16 D0ddfin,D0dd,C0dd,C0,A
        complex*16 m02,m12,m22,m32,m2(0:3)
        real*8 p2(0:3,0:3)
        integer i,j,k,ip,im,i1,i2,i3,perm,nsoft,ncoll
        logical smallp2(0:3,0:3),smallm2(0:3)
        logical coll(0:3,0:3),soft(0:3,0:3,0:3)

        p2(0,1) = q1
        p2(1,2) = q2
        p2(2,3) = q3
        p2(0,3) = q4
        p2(0,2) = q12
        p2(1,3) = q23
        m2(0) = m02
        m2(1) = m12
        m2(2) = m22
        m2(3) = m32

        perm = 1
50      continue
c determine small parameters 
        do i=0,2 
        do j=i+1,3
          p2(j,i) = p2(i,j)
          smallp2(i,j) = (abs(p2(i,j)).lt.1d-15)
          smallp2(j,i) = smallp2(i,j)
        enddo 
        enddo
        do i=0,3
          smallm2(i) = (abs(m2(i)).lt.1d-15)
        enddo

c determine collinear singularities
        ncoll = 0
        do i=0,2
        do j=i+1,3
          coll(i,j) = smallp2(i,j).and.smallm2(i).and.smallm2(j)
          coll(j,i) = coll(i,j)
          if (coll(i,j)) ncoll = ncoll + 1
        enddo
        enddo
c determine soft singularities
        nsoft = 0
        do i=0,3
        do j=0,2
          ip = mod(i+1+j,4)
          im = mod(i+1+mod(j+1,3),4)
          soft(i,ip,im) = (abs(m2(i)).lt.1d-15).and.
     &                    (dcmplx(p2(i,ip)).eq.m2(ip)).and.
     &                    (dcmplx(p2(i,im)).eq.m2(im))
          soft(i,im,ip) = soft(i,ip,im)
          if (soft(i,ip,im)) nsoft = nsoft + 1
        enddo
        enddo

c canonical ordering of propagators
c     -> only coll(i,i+1) and sing(j,j-1,j+1) relevant;
c        p2(0,2) and p2(1,3) not small
        if (smallp2(0,2).or.smallp2(1,3)) goto 60
        do k=0,3
          i = mod(k+2,4)
          j = mod(k+1,4)
          if (soft(k,i,j)) goto 60
          j = mod(k+3,4)
          if (soft(k,i,j)) goto 60
        enddo

        goto 70
c swap propagators i and l and start again
60      continue
        if (perm.gt.24) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 
     &         'D0dd: singularity structure not supported'
            call DD_debugoutput()
          endif
          stopflag = min(-10,stopflag)
        endif
        perm = perm+1
        call D0args(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &              p2(0,2),p2(1,3),m2(0),m2(1),m2(2),m2(3),perm)
        goto 50
70      continue

c subdivergencies in terms of C0's
        D0ddfin = D0dd(p2(0,1),p2(1,2),p2(2,3),p2(0,3),
     &                 p2(0,2),p2(1,3),m2(0),m2(1),m2(2),m2(3),0)

        do i=0,3
          i1 = mod(i+1,4)
          i2 = mod(i+2,4)
          i3 = mod(i+3,4)
          if (soft(i2,i1,i3)) then
            A = 1d0/(p2(i,i2)-m2(i))
          elseif (coll(i1,i2)) then
            A = (p2(i1,i3)-p2(i2,i3))/(
     &            (p2(i1,i3)-m2(i3))*(p2(i ,i2)-m2(i ))
     &           -(p2(i ,i1)-m2(i ))*(p2(i2,i3)-m2(i3)) )
          elseif (coll(i2,i3)) then
            A = (p2(i1,i2)-p2(i1,i3))/(
     &            (p2(i1,i2)-m2(i1))*(p2(i ,i3)-m2(i ))
     &           -(p2(i ,i2)-m2(i ))*(p2(i1,i3)-m2(i1)) )
          else 
            cycle
          endif
          if (i.eq.0) then
            C0 = C0dd(p2(1,2),p2(2,3),p2(1,3),m2(1),m2(2),m2(3),0)
          elseif (i.eq.1) then
            C0 = C0dd(p2(2,0),p2(2,3),p2(3,0),m2(0),m2(2),m2(3),0)
          elseif (i.eq.2) then
            C0 = C0dd(p2(0,1),p2(1,3),p2(3,0),m2(0),m2(1),m2(3),0)
          elseif (i.eq.3) then
            C0 = C0dd(p2(0,1),p2(1,2),p2(2,0),m2(0),m2(1),m2(2),0)
          endif
          D0ddfin = D0ddfin - A*C0
        enddo

        end

