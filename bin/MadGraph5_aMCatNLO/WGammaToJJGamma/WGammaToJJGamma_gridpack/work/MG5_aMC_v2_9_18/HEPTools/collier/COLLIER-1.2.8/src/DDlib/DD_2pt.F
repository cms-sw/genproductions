!!
!!  File DD_2pt.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!

**********************************************************************
        subroutine A_dd(A,Auv,xm02,r,id)
**********************************************************************
*       1-point coefficients  
*
*       A(l)  =  A_{0...0}(m02)  of rank 2l with 2l <= r
*                   \___/
*                    2l indices
*
*       Auv(l) = coefficient of 1/eps in A(l),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer r,n,id,factn1(0:r/2)
        complex*16 A(0:r/2),Auv(0:r/2),xm02,m02
        real*8 sumn(0:r/2)

        if (id.eq.0) then
          nmaster   = 1
          r2master  = r
          accflag   = 0
          errflag   = 0
          stopflag  = 0
        endif

c scalar and vector integrals
        if (abs(xm02).gt.1d-17) then
          m02    = xm02
          A(0)   = m02*(deltauv+log(muv2/m02)+1d0)
          Auv(0) = m02
        else
          m02    = 0d0
          A(0)   = 0d0
          Auv(0) = 0d0
        endif
        factn1(0) = 1
        sumn(0)   = 0d0
        
c tensor coefficients for rank 2l > 0
        do n=1,r/2
          factn1(n)  = (n+1)*factn1(n-1)
          sumn(n)    = sumn(n-1) + 1d0/(n+1)
          A(n)     = (m02/2d0)**n/factn1(n)*( A(0)+m02*sumn(n) )
          Auv(n)   = (m02/2d0)**n/factn1(n)* Auv(0)
        enddo

c accuracy estimate
c==================
        if (id.lt.2**nmaster) then
          do n=0,r/2
            resaccrel(id,2*n)    = dprec_dd
            resaccrel(id,2*n+1)  = 0d0
            resaccabs(id,2*n)    = resaccrel(id,2*n)*abs(A(n))
            resaccabs(id,2*n+1)  = 0d0
            resaccrel2(id,2*n)   = resaccrel(id,2*n)   
            resaccrel2(id,2*n+1) = resaccrel(id,2*n+1) 
            resaccabs2(id,2*n)   = resaccabs(id,2*n)   
            resaccabs2(id,2*n+1) = resaccabs(id,2*n+1) 
          enddo
        endif

        end

**********************************************************************
        subroutine B_dd(B,Buv,xp2,xm02,xm12,r2,id)
**********************************************************************
*       2-point coefficients  
*       B(i,j) = B_{0...01...1}(xp2,xm02,xm12) 
*                   \___/\___/
*                    2i    j  indices
*       rank r=i+j
*       
*                (i,j)
*       r2=0:    (0,0)
*       r2=1:    (2,0), (0,1)
*       r2=2:    (4,0), (2,1), (0,2)
*       ...
*       r2:      (2*r2,0), (2*r2-2,1), ... (0,r2)
*
*       Buv(i,j) = coefficient of 1/eps in B(i,j),  Duv = 4-2*eps
*---------------------------------------------------------------------
*       11.4.2006 Stefan Dittmaier
**********************************************************************

#ifdef collierdd
c LH  global cache system
        use cache
#endif

        use DD_global
        use DD_2pt
        implicit real*8 (a-z)

c local variables
        integer r2,i0,i1,k,l,n,r,qm0,id,id0
        complex*16 ieps,ieps2,y(2),ye(2),f1,f(0:r2,2),sqe_dd,luv
        complex*16 A(0:max(r2,1)),Auv(0:max(r2,1))
        complex*16 B(0:r2,0:r2),Buv(0:r2,0:r2)
        complex*16 m02,m12,xm02,xm12,cm2,cmp2,caux
        complex*16 inteps,z,ze
        logical regp,regm02,regm12

#ifdef collierdd
c LH: variables for cache system
        complex*16 xcache(3), fct(Ncoefmax2)
        integer n0,n1,cnt,cntmax,r2cache
        logical nocalc/.false./, wrica/.false./
        save wrica
#endif

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))
        inteps(z,ze) = dcmplx( dreal(z),(abs(dimag(z))+abs(z)*eps2)
     &                                  *sign(1d0,dimag(ze)) )

        if(r2.lt.0) then
          write(*,*) 'B_dd called with r2<0. Fix it! r2 = ',r2
        endif

        if (id.eq.0) then
          nmaster   = 2
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
          r2_aux(0)     = -1
          r2_new_aux(0) = -1
          do r=0,r2  
            resaccrel(0,r)  = 0d0
            resaccabs(0,r)  = 0d0
            resaccrel2(0,r) = 0d0
            resaccabs2(0,r) = 0d0
          enddo
        endif

        if (r2.gt.rmax2) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'B_dd called for rank r2 =',r2
            write(outchannel,*) 'rmax2 =',rmax2,' too small'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-9,stopflag)
        endif

        crit  = 1d-8
        eps   = 1d-13
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        ieps2 = dcmplx(0d0,eps2)

c initialization for first call
c==============================

        if ((r2_aux(id).eq.-1).or.(id.ge.2**nmaster)) then

#ifdef collierdd
c LH: read global cache
c =====================
        if ((use_cache_system).and.(id.lt.2**nmaster)) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              xcache(1) = xp2
              xcache(2) = xm02
              xcache(3) = xm12

c              cntmax = (r2+1)*(r2+2)+12
              cntmax = 2*NCoefsG(r2,2)+2*r2+13

              if (cntmax.gt.NCoefmax2) then
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'NCoefmax2 too small!'
                  if (cout.eq.coutmax) call DDlastmessage()
                  cout = cout+1
                endif
                stopflag = min(-9,stopflag)
              endif

              r2cache = r2
              call ReadCache(fct(1:cntmax),cntmax,xcache,3,2,id,2,
     &                       r2cache,nocalc,wrica)            

              if(nocalc)then
                            
                auxr(id,1) = dble(fct(1))
                auxi(id,1) = nint(dreal(fct(2)))
                auxc(id,1) = fct(3)
                auxc(id,2) = fct(4)
                auxc(id,3) = fct(5)
                auxc(id,4) = fct(6)
                auxc(id,5) = fct(7)
                auxc(id,6) = fct(8)
                accflag    = nint(dreal(fct(9)))
                errflag    = nint(dreal(fct(10)))
                stopflag   = min(nint(dreal(fct(11))),stopflag)

                cnt = 11
                cnt_int = 0
                r2_aux(id) = r2
                do r=0,r2
                  cnt = cnt+1
                  resaccabs(id,r)  = dreal(fct(cnt))
                  resaccabs2(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  resaccrel(id,r)  = dreal(fct(cnt))
                  resaccrel2(id,r) = dreal(fct(cnt))
                  do n0=0,2*r,2
                    n1=r-n0/2

                    cnt = cnt+1
                    B(n0/2,n1) = fct(cnt)
                    cnt_int = cnt_int+1
                    B_cache(tid(id),n0/2,n1) = B(n0/2,n1)
                    cnt = cnt+1
                    Buv(n0/2,n1) = fct(cnt)
                    Buv_cache(tid(id),n0/2,n1) = Buv(n0/2,n1)

                  end do
                end do
                return
              end if
          end if
        end if
c =====================
#endif

c convert input parameters 
c-------------------------
          regp   = (abs(xp2) .gt.1d-15)
          regm02 = (abs(xm02).gt.1d-15)
          regm12 = (abs(xm12).gt.1d-15)
          p2  = xp2
          m02 = xm02
          m12 = xm12
          if (regp.or.regm02.or.regm12) then
c regular case
            if (.not.regp)   p2  = 0d0
            if (.not.regm02) m02 = 0d0
            if (.not.regm12) m12 = 0d0
            qm0 = 0
          else
c singular case
            if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
            qm0 = 1
          endif

c auxiliary parameters
c---------------------
          f1    = p2-m12+m02
c m0 =/= 0
          if (m02.ne.(0d0,0d0)) then
            luv = deltauv+log(muv2/m02)
            if (p2.eq.0d0) then
              y(1) = 1d20
              if (m02.eq.m12) then
                y(2) = 1d20
              else
                y(2) = m02/(m02-m12)
                if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &            y(2) = y(2) - ieps2*abs(y(2))*sign(1d0,dreal(m02-m12))
              endif
            else
              y(1)  = sqe_dd(dcmplx(p2),-p2-m02+m12,m02)
              y(2)  = m02/p2/y(1)
              ye(1) = sqe_dd(dcmplx(p2),-p2-m02+m12,m02-abs(m02)*ieps)
              ye(2) = (m02-abs(m02)*ieps)/p2/y(1)
              if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
                 caux  = ye(1)
                 ye(1) = ye(2)
                 ye(2) = caux
               endif
               if (abs(dimag(y(1)))/abs(y(1)).lt.crit) 
     &           y(1) = inteps(y(1),ye(1))
               if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &           y(2) = inteps(y(2),ye(2))
            endif
c m0 = 0, p2 =/= 0
          elseif (p2.ne.0d0) then
            luv  = deltauv
            y(1) = 1d0-m12/p2+ieps2*sign(1d0,p2)*abs(1d0-m12/p2)
c m0 = 0, m1 =/= 0
          elseif (m12.ne.(0d0,0d0)) then
            luv  = deltauv
            y(1) = 1d20
          else
c m02 =  m12 = p2 = 0
            luv  = deltauv-delta1ir+log(muv2/mir2)
          endif

        else
c read cached information for repeated calls
c-------------------------------------------
          do r=0,min(r2,r2_aux(id))
          do i0=0,2*r,2
          i1 = r-i0/2
            B(i0/2,i1)   = B_cache(tid(id),i0/2,i1)
            Buv(i0/2,i1) = Buv_cache(tid(id),i0/2,i1)
          end do
          end do
          if (r2.le.r2_aux(id)) return
          p2     = auxr(id,1)
          qm0    = auxi(id,1)
          y(1)   = auxc(id,1) 
          y(2)   = auxc(id,2) 
          m02    = auxc(id,3)
          m12    = auxc(id,4)
          f1     = auxc(id,5)
          luv    = auxc(id,6)
        endif

c calculation of B(0,j) = B_{1...1}
c==================================
c m0 =/= 0
        if (m02.ne.(0d0,0d0)) then
          do k=1,2
          do n=r2_aux(id)+1,r2
            if (abs(y(k)).lt.10d0) then
              if (1d0-1d0/y(k).ne.(0d0,0d0)) then
                f(n,k) = (1d0-y(k)**(n+1))*log(1d0-1d0/y(k))
              else
                f(n,k) = 0d0
              endif
              do l=0,n
                f(n,k) = f(n,k) - y(k)**(n-l)/(l+1d0)
              enddo
            else
              f(n,k) = log(1d0-1d0/y(k))
              if (abs(y(k)).lt.1d20) then
                do l=n+1,n+20
                  f(n,k) = f(n,k) + y(k)**(n-l)/(l+1d0)
                enddo
              endif
            endif
          end do
          end do
c m0 = 0 and ( p2 =/= 0 or m1 =/= 0 )
        elseif ((p2.ne.0d0).or.(m12.ne.(0d0,0d0))) then
          do n=r2_aux(id)+1,r2
            if (dcmplx(p2).eq.m12) then
              f(n,1) = -log(muv2/m12) - 1d0/(n+1d0)
              f(n,2) = - 1d0/(n+1d0)
            else
              if (abs(y(1)).lt.10d0) then
                if (1d0-1d0/y(1).ne.(0d0,0d0)) then
                  f(n,1) = (1d0-y(1)**(n+1))*log(1d0-1d0/y(1))
                else
                  f(n,1) = 0d0
                endif
                do l=0,n
                  f(n,1) = f(n,1) - y(1)**(n-l)/(l+1d0)
                enddo
              else
                f(n,1) = log(1d0-1d0/y(1))
                if (abs(y(1)).lt.1d20) then
                  do l=n+1,n+20
                    f(n,1) = f(n,1) + y(1)**(n-l)/(l+1d0)
                  enddo
                endif
              endif
              f(n,2) = log((m12-p2)/muv2 - ieps2*abs((m12-p2)/muv2)) 
     &                 - 1d0/(n+1d0)
            endif
          enddo
c m02 =  m12 = p2 = 0
        else
          do n=r2_aux(id)+1,r2
            f(n,1) = 0d0
            f(n,2) = 0d0
          enddo
        endif

        do n=r2_aux(id)+1,r2
          B(0,n)   = (-1)**n/(n+1d0)*( luv - f(n,1) - f(n,2) )
          Buv(0,n) = (-1)**n/(n+1d0)
        enddo

c calculation of B(i,j) = B_{0...01...1}
c=======================================

c set identifier for 1-point integrals
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            id0 = id + 2**k
            goto 450
          endif
        enddo
450     continue
        if (id.ge.2**nmaster) id0 = 2**nmaster
        if (r2.gt.0) call A_dd(A(0:r2),Auv(0:r2),xm12,2*r2,id0)

        do r=r2_aux(id)+1,r2
        do i0=2,2*r,2
        i1 = r-i0/2
          if (qm0.eq.1) then
            B(i0/2,i1)   = 0d0
            Buv(i0/2,i1) = 0d0
          else
            Buv(i0/2,i1) = 1d0/2d0/(i0+i1+1)*(
     &          (-1)**i1*Auv(i0/2-1)+2d0*m02*Buv(i0/2-1,i1)
     &          +f1*Buv(i0/2-1,i1+1)                              )
            B(i0/2,i1) = 1d0/2d0/(i0+i1+1)*(
     &          (-1)**i1*A(i0/2-1)+2d0*m02*B(i0/2-1,i1)
     &          +f1*B(i0/2-1,i1+1)                                
     &          +4d0*Buv(i0/2,i1)                         )
          endif
        end do
        end do

c cache information 
        if (id.lt.2**nmaster) then
          do r=r2_aux(id)+1,r2
          do i0=0,2*r,2
          i1 = r-i0/2
            B_cache(tid(id),i0/2,i1)   = B(i0/2,i1) 
            Buv_cache(tid(id),i0/2,i1) = Buv(i0/2,i1) 
          end do
          end do
          r2_aux(id) = r2
          auxr(id,1) = p2
          auxi(id,1) = qm0
          auxc(id,1) = y(1) 
          auxc(id,2) = y(2) 
          auxc(id,3) = m02
          auxc(id,4) = m12
          auxc(id,5) = f1
          auxc(id,6) = luv
        endif

c accuracy estimate
c==================
          Bmax = abs(B(0,0))
          do r=1,r2
            Bmax = max(Bmax,abs(B(0,r)))
          enddo
          do r=0,r2
            resaccrel(id,r)  = dprec_dd
            resaccabs(id,r)  = resaccrel(id,r)*Bmax
            resaccrel2(id,r) = resaccrel(id,r) 
            resaccabs2(id,r) = resaccabs(id,r) 
            if (resaccrel(id,r).gt.aimacc(2)) accflag = 1
            if (resaccrel(id,r).gt.erracc(2)) errflag = 1
          enddo

#ifdef collierdd
c LH: write global cache
c ======================
        if ((use_cache_system).and.(id.lt.2**nmaster)) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              if (wrica) then
                
                fct(1) = dcmplx(auxr(id,1))
                fct(2) = dcmplx(auxi(id,1))
                fct(3) = auxc(id,1)
                fct(4) = auxc(id,2) 
                fct(5) = auxc(id,3)
                fct(6) = auxc(id,4)
                fct(7) = auxc(id,5)
                fct(8) = auxc(id,6)
                fct(9) = accflag  
                fct(10)= errflag  
                fct(11)= stopflag  
                
                cnt = 11
                do r=0,r2
                  cnt = cnt+1
                  fct(cnt) = resaccabs(id,r)
                  cnt = cnt+1
                  fct(cnt) = resaccrel(id,r)
                  do n0=0,2*r,2
                    n1=r-n0/2

                    cnt = cnt+1
                    fct(cnt) = B(n0/2,n1)
                    cnt = cnt+1
                    fct(cnt) = Buv(n0/2,n1)

                  end do
                end do
  
                call WriteCache(fct(1:cnt),cnt,id,2,r2)

              end if
          end if
        end if
c ======================
#endif

        end

**********************************************************************
        subroutine B0_dd(B0,Buv0,p2,m02,m12,r2,id)
**********************************************************************
*       2-point coefficients B(0)_{...} with unshifted momentum 
*---------------------------------------------------------------------
*       21.7.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_2pt
        implicit real*8 (a-z)

c local variables
        integer r,r2,i0,i1,i2,i12,id
        complex*16 B(0:r2,0:r2),B0(0:r2,0:r2,0:r2)
        complex*16 Buv(0:r2,0:r2)
        complex*16 Buv0(0:r2,0:r2,0:r2)
        complex*16 m02,m12

        call B_dd(B,Buv,p2,m02,m12,r2,id)

        do r=0,r2
        do i0=0,2*r,2
          i12 = r-i0/2
          i2  = i12
          B0(i0/2,0,i2)   = B(i0/2,i2)
          Buv0(i0/2,0,i2) = Buv(i0/2,i2)
          do i1=1,i12
            i2 = i12-i1
            B0(i0/2,i1,i2)   = -B0(i0/2,i1-1,i2) - B0(i0/2,i1-1,i2+1)
            Buv0(i0/2,i1,i2) = -Buv0(i0/2,i1-1,i2)-Buv0(i0/2,i1-1,i2+1)
        end do
        end do
        end do

        end

**********************************************************************
        subroutine DB_dd(DB,DBuv,xp2,xm02,xm12,r2)
**********************************************************************
*       momentum derivative of 2-point coefficients  
*       DB(i,j) = DB_{0...01...1}(xp2,xm02,xm12) 
*                     \___/\___/
*                      2i    j  indices
*       rank r=i+j
*       
*                (i,j)
*       r2=0:    (0,0)
*       r2=1:    (2,0), (0,1)
*       r2=2:    (4,0), (2,1), (0,2)
*       ...
*       r2:      (2*r2,0), (2*r2-2,1), ... (0,r2)
*
*       DBuv(i,j) = coefficient of 1/eps_UV in DB(i,j),  Duv = 4-2*eps
*
*       NOTE: No cache system implemented for these functions !
*---------------------------------------------------------------------
*       21.6.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_2pt
        implicit real*8 (a-z)

c local variables
        integer r2,i0,i1,k,l,n,qm0
        complex*16 ieps,ieps2,y(2),ye(2),f1,f(-1:r2,2),g(0:r2,2)
        complex*16 B(0:r2+3,0:r2+3),Buv(0:r2+3,0:r2+3)
        complex*16 DB(0:r2,0:r2),DBuv(0:r2,0:r2)
        complex*16 DB_aux(0:r2+1,0:r2+1),DBuv_aux(0:r2+1,0:r2+1)
        complex*16 m02,m12,xm02,xm12,cm2,cmp2,caux,ma,ma2,root
        complex*16 inteps,z,ze,rlam,sqe_dd
        real*8 Berr(0:r2+2),DBerr(0:r2)
        logical regp,regm02,regm12

        rmp2(rm2) = mx2(nint(rm2*1d20))
        cmp2(cm2) = mx2(nint(dreal(cm2*1d20)))
        inteps(z,ze) = dcmplx( dreal(z),(abs(dimag(z))+abs(z)*eps2)
     &                                  *sign(1d0,dimag(ze)) )

        accflag   = 0
        errflag   = 0
        stopflag  = 0

        crit  = 1d-8
        eps   = dprec_dd
        ieps  = dcmplx(0d0,eps)
        eps2  = 1d-20
        ieps2 = dcmplx(0d0,eps2)
        r2master = r2
        nmaster  = 2

c convert input parameters 
c-------------------------
          regp   = (abs(xp2) .gt.1d-15)
          regm02 = (abs(xm02).gt.1d-15)
          regm12 = (abs(xm12).gt.1d-15)
          p2  = xp2
          m02 = xm02
          m12 = xm12

          if (((m02.eq.(0d0,0d0)).and.(xp2.eq.xm12)).or. 
     &        ((m12.eq.(0d0,0d0)).and.(xp2.eq.xm02)).or.
     &        ((.not.regm02).and.(xp2.eq.xm12).and.regp).or. 
     &        ((.not.regm12).and.(xp2.eq.xm02).and.regp)) then
c soft-singular case (dim. reg. or mass reg.)
            if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
            qm0 = -1
          elseif ((xp2.eq.xm12).or.(xp2.eq.xm02)) then
c regular on-shell case
            if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
            qm0 = 2
            if ((.not.regp).and.regm02) then
              p2  = 0d0
              m12 = 0d0
              qm0 = 3
            elseif ((.not.regp).and.regm12) then
              p2  = 0d0
              m02 = 0d0
              qm0 = 3
            endif
          elseif (regp.or.regm02.or.regm12) then
c regular case
            if (.not.regp)   p2  = 0d0
            if (.not.regm02) m02 = 0d0
            if (.not.regm12) m12 = 0d0
            qm0 = 0
          else
c singular case -- all scales small
            if ((.not.regp).and.(p2.ne.0d0))          p2  = rmp2(p2)
            if ((.not.regm02).and.(m02.ne.(0d0,0d0))) m02 = cmp2(m02)
            if ((.not.regm12).and.(m12.ne.(0d0,0d0))) m12 = cmp2(m12)
            qm0 = 1
          endif

c auxiliary parameters
c---------------------
          scale2  = max(abs(p2),abs(m02),abs(m12))
          f1      = p2-m12+m02
          f(-1,1) = 0d0
          f(-1,2) = 0d0
c m0 =/= 0
          if (m02.ne.(0d0,0d0)) then
            if (p2.eq.0d0) then
              y(1) = 1d20
              if (m02.eq.m12) then
                y(2) = 1d20
                rlam = 0d0
              else
                y(2) = m02/(m02-m12)
                if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &            y(2) = y(2) - ieps2*abs(y(2))*sign(1d0,dreal(m02-m12))
                  rlam = m02-m12
              endif
            else
              y(1)  = sqe_dd(dcmplx(p2),-p2-m02+m12,m02)
              y(2)  = m02/p2/y(1)
              ye(1) = sqe_dd(dcmplx(p2),-p2-m02+m12,m02-abs(m02)*ieps)
              ye(2) = (m02-abs(m02)*ieps)/p2/y(1)
              if (abs(y(1)-ye(1)).gt.abs(y(1)-ye(2))) then
                 caux  = ye(1)
                 ye(1) = ye(2)
                 ye(2) = caux
              endif
              if (abs(dimag(y(1)))/abs(y(1)).lt.crit) 
     &          y(1) = inteps(y(1),ye(1))
              if (abs(dimag(y(2)))/abs(y(2)).lt.crit) 
     &          y(2) = inteps(y(2),ye(2))
              rlam = p2*(y(1)-y(2))
            endif
c m02 = 0, p2 =/= 0
          elseif (p2.ne.0d0) then
            y(1) = 1d0-m12/p2+ieps2*sign(1d0,p2)*abs(1d0-m12/p2)
            y(2) = 0d0
            rlam = p2*(y(1)-y(2))
c m02 = p2 = 0, m12 =/= 0
          elseif ((p2.eq.0d0).and.(m12.ne.(0d0,0d0))) then
            y(1) = 0d0
            y(2) = 1d20
            rlam = m12
          else
c m02 =  m12 = p2 = 0
            rlam = 0d0
          endif

c optimize branching to different methods
c----------------------------------------
        if (qm0.eq.2) then
          if (abs(p2).lt.abs(rlam)) then
            qm0 = 0
          endif
        endif

c DB integrals
c-------------

c p2=m02=m12=rlam=0
        if ((p2.eq.0d0).and.(m02.eq.(0d0,0d0))
     &                 .and.(m12.eq.(0d0,0d0))) then
          do n=0,r2
          do i0=0,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 0d0
            DB(i0/2,i1)   = 0d0
          enddo
          enddo
          do n=0,r2
            DBerr(n) = 0d0
          enddo

c regular case with ( m02=0 or m12=0 ) and p2 not too small
        elseif (((qm0.eq.0).or.(qm0.eq.1)).and.
     &          (abs(p2).gt.abs(rlam)).and.
     &          ((m02.eq.(0d0,0d0)).or.(m12.eq.(0d0,0d0)))) then

          if (qm0.eq.0) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)
          elseif (qm0.eq.1) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),p2,m02,m12,r2,0)
          endif

          DBuv(0,0) = 0d0
          DB(0,0)   = -1d0/p2
          if (m02.ne.(0d0,0d0)) then
            DB(0,0) = DB(0,0)
     &                -m02/p2**2*log(1d0-p2/m02-ieps2*abs(1d0-p2/m02))
          elseif (m12.ne.(0d0,0d0)) then
            DB(0,0) = DB(0,0)
     &                -m12/p2**2*log(1d0-p2/m12-ieps2*abs(1d0-p2/m12))
          endif

          do n=1,r2
            DBuv(0,n) = 0d0
            DB(0,n)   = -1d0/p2*( B(0,n)
     &                           + n/(n+1d0)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1d0/p2*(n-1d0)/(n+1d0)
     &                           *( m02*DB(0,n-2)+2d0*DBuv(1,n-2) )
          do i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4d0*DBuv(i0/2,i1)                    )
          end do
          end do

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2)
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

c regular case 
        elseif (((qm0.eq.0).or.(qm0.eq.1).or.(qm0.eq.3))
     &          .and.(rlam.ne.(0d0,0d0))) then

          if ((qm0.eq.0).or.(qm0.eq.3)) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)
          elseif (qm0.eq.1) then
            call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),p2,m02,m12,r2,0)
          endif

          do k=1,2
          do n=0,r2
            if (abs(y(k)).lt.10d0) then
              if (y(k).ne.(0d0,0d0)) then
                caux = 1d0-1d0/y(k)
              else
                caux = 1d25
              endif
              if ((caux.eq.(0d0,0d0)).or.(y(k).eq.(0d0,0d0))) then
                f(n,k) = 0d0
              else
                f(n,k) = (1d0-y(k)**(n+1))*log(1d0-1d0/y(k))
              endif
              do l=0,n
                f(n,k) = f(n,k) - y(k)**(n-l)/(l+1d0)
              enddo
              if (caux.eq.(0d0,0d0)) then
                g(n,k) = 0d0
              elseif (y(k).eq.(0d0,0d0)) then
                g(n,k) = -1d0/(1d0+n) 
              else
                g(n,k) = (1d0-y(k))*( y(k)*f(n-1,k)
     &                     -y(k)*log(1d0-1d0/y(k))-1d0/(1d0+n) )
              endif
            else
              f(n,k) = log(1d0-1d0/y(k))
              g(n,k) = -1d0/(n+2d0)
              if (abs(y(k)).lt.1d20) then
                do l=n+1,n+20
                  f(n,k) = f(n,k) + y(k)**(n-l)/(l+1d0)
                  g(n,k) = g(n,k) + y(k)**(n-l)/(l+1d0)/(l+2d0)
                enddo
              endif
            endif
          end do
          end do

          do n=0,r2
            DBuv(0,n) = 0d0
            DB(0,n)   = (-1)**n/rlam*( g(n,2) - g(n,1) )
          do i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4d0*DBuv(i0/2,i1)                     )
          end do
          end do

c error estimate
          do n=0,r2
            DBerr(n) = dprec_dd*abs(DB(0,n))
          enddo

c p2=0, m02=m12=/=0, rlam=0
        elseif ((p2.eq.0d0).and.(m02.eq.m12)) then

          call B_dd(B,Buv,xp2,xm02,xm12,r2+3,0)

          do n=0,r2+1
            DBuv_aux(0,n) = 0d0
            do i0=2,2*n,2
              i1 = n-i0/2
              DBuv_aux(i0/2,i1) = 
     &            1d0/2d0/(i0+i1+1)*(2d0*m02*DBuv_aux(i0/2-1,i1)
     &                +Buv(i0/2-1,i1+1) + f1*DBuv_aux(i0/2-1,i1+1) )
            enddo
          enddo

          do n=0,r2+1
            DB_aux(0,n)   = ( -(n+2d0)/(n+1d0)*B(0,n+1) 
     &                      -(n+3d0)/(n+1d0)*B(0,n+2)
     &                      -2d0*DBuv_aux(1,n) )/m02
            do i0=2,2*n,2
              i1 = n-i0/2
              DB_aux(i0/2,i1)   = 
     &           1d0/2d0/(i0+i1+1)*( 2d0*m02*DB_aux(i0/2-1,i1)
     &                  + B(i0/2-1,i1+1) + f1*DB_aux(i0/2-1,i1+1) 
     &                  + 4d0*DBuv_aux(i0/2,i1)                  )
          end do
          end do

          do n=0,r2
            do i0=0,2*n,2
              i1 = n-i0/2
              DBuv(i0/2,i1) = DBuv_aux(i0/2,i1) 
              DB(i0/2,i1)   = DB_aux(i0/2,i1) 
          end do
          end do

c error estimate
          Berr(1) = resaccabs(0,1)
          do n=0,r2
            Berr(n+2)= resaccabs(0,n+2)
            DBerr(n) = max(Berr(n+1),Berr(n+2))/abs(m02)
          enddo

c soft-singular case (rlam=0)
c p2=m02=/=0, m12=small  or  p2=m12=/=0, m02=small
        elseif (qm0.eq.-1) then
          call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)

          DBuv(0,0) = 0d0
          if ((m02.eq.(0d0,0d0)).or.(m12.eq.(0d0,0d0))) then
            DB(0,0)   = -(2d0+delta1ir+log(mir2/p2))/(2d0*p2)
          elseif(xp2.eq.xm12) then
            DB(0,0)   = -(2d0+log(m02/p2))/(2d0*p2)
          elseif(xp2.eq.xm02) then
            DB(0,0)   = -(2d0+log(m12/p2))/(2d0*p2)
          endif

          if (regp.and.(xp2.eq.xm12)) then
            m02 = 0d0
            f1  = 0d0
          elseif (regp.and.(xp2.eq.xm02)) then
            m12 = 0d0
            f1  = 2d0*p2
          endif

          do n=1,r2
            DBuv(0,n) = 0d0
            DB(0,n)   = -1d0/p2*( B(0,n)
     &                           + n/(n+1d0)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1d0/p2*(n-1d0)/(n+1d0)
     &                           *( m02*DB(0,n-2)+2d0*DBuv(1,n-2) )
          do i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4d0*DBuv(i0/2,i1)                    )
          end do
          end do

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2)
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

c regular on-shell case
c p2=m02=/=0, m12=/=small  or  p2=m12=/=0, m02=/=small
        elseif (qm0.eq.2) then

          call B_dd(B(0:r2,0:r2),Buv(0:r2,0:r2),xp2,xm02,xm12,r2,0)

          if (xp2.eq.xm12) then
            ma2 = m02
          else
            ma2 = m12
          endif
          ma   = sqrt(ma2)
          root = sqrt( ma2-4d0*p2-ieps2*abs(ma2-4d0*p2) )      

          DBuv(0,0) = 0d0
          if (root.ne.0d0) then
            DB(0,0)   = ma*(3d0*p2-ma2)/root/p2**2
     &                    *log((ma+root)/2d0/sqrt(p2))
     &                  +(ma2-p2)/2d0/p2**2*log(ma2/p2)-1d0/p2 
          else
            DB(0,0)   = (-4d0+3d0*log(4d0))/(2d0*p2)
          endif

          do n=1,r2
            DBuv(0,n) = 0d0
            DB(0,n)   = -1d0/p2*( B(0,n)
     &                           + n/(n+1d0)*(B(0,n-1)+f1*DB(0,n-1)) )
            if (n.gt.1) DB(0,n) = DB(0,n) 
     &                   - 1d0/p2*(n-1d0)/(n+1d0)
     &                           *( m02*DB(0,n-2)+2d0*DBuv(1,n-2) )
          do i0=2,2*n,2
            i1 = n-i0/2
            DBuv(i0/2,i1) = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DBuv(i0/2-1,i1)
     &                        +Buv(i0/2-1,i1+1) + f1*DBuv(i0/2-1,i1+1) )
            DB(i0/2,i1)   = 1d0/2d0/(i0+i1+1)*( 2d0*m02*DB(i0/2-1,i1)
     &                        +B(i0/2-1,i1+1) + f1*DB(i0/2-1,i1+1) 
     &                        + 4d0*DBuv(i0/2,i1)                    )
          end do
          end do

c error estimate
          Berr(0)  = resaccabs(0,0)
          DBerr(0) = dprec_dd*abs(DB(0,0))
          do n=1,r2
            Berr(n)  = resaccabs(0,n)
            DBerr(n) = max(Berr(n),Berr(n-1),abs(f1)*DBerr(n-1))/abs(p2)
            if (n.gt.1) DBerr(n) = max(DBerr(n),abs(m02/p2)*DBerr(n-2))
          enddo

        else
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'DB_dd: case not implemented'
            write(outchannel,*) 'p2  = ',p2
            write(outchannel,*) 'm02 = ',m02
            write(outchannel,*) 'm12 = ',m12
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        endif

c error estimate
        DBmax = 0d0
        do n=0,r2
          DBmax = max(DBmax,abs(DB(0,n)))
          resaccabs(0,n)  = DBerr(n)
          resaccrel(0,n)  = resaccabs(0,0)/DBmax
          resaccabs2(0,n) = resaccabs(0,n) 
          resaccrel2(0,n) = resaccrel(0,n) 
        enddo

        end

