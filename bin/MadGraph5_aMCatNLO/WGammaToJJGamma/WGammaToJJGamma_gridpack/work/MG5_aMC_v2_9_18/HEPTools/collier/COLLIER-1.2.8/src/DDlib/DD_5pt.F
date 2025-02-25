!!
!!  File DD_5pt.F is part of COLLIER
!!  - A Complex One-Loop Library In Extended Regularizations
!!
!!  Copyright (C) 2015, 2016   
!!  Ansgar Denner, Stefan Dittmaier, Lars Hofer
!!
!!  COLLIER is licenced under the GNU GPL version 3, 
!!  see COPYING for details.
!!

**********************************************************************
        subroutine E_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &                  m02,m12,m22,m32,m42,r2,id)
**********************************************************************
*       5-point coefficients  
*       E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m42)
*                         \___/\___/\___/\___/\___/
*                          2i    j    k    l    m  indices
*       of rank r=i+j+k+l+m with r <= r2
*---------------------------------------------------------------------
*       10.9.2006 Stefan Dittmaier
**********************************************************************

#ifdef collierdd
c LH  global cache system
        use cache
#endif

        use DD_global
        use DD_5pt
        use DD_4pt
        implicit real*8 (a-z)

c local variables
        integer r,r2,i,j,k,m,n,id,nid(0:nmax-1)
        complex*16 E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
        complex*16 m02,m12,m22,m32,m42,xdet_dd,f(4),detx4
        complex*16 x4(0:4,0:4),tx4(0:4,0:4),ttx4(0:4,0:4,0:4,0:4)
        complex*16 dety4,eta4(0:4),mat(5,5),mati(5,5),vec(5)
        complex*16 detm,mat4(4,4),mati4(4,4)
        real*8 z4(4,4),tz4(4,4),z4i(4,4)
        integer i0,i1,i2,i3,i4,i1234
#ifdef collierdd
c LH: variables for cache system
        complex*16 xcache(15), fct(NCoefmax5)
        integer n0,n1,n2,n3,n4,n1234,cnt,cntmax,r2cache
c        logical nocalc/.false./, wrica/.false./
c        save wrica
         logical :: nocalc = .false.
         logical, save :: wrica =.false.
#endif

        if (id.eq.0) then
          do i=0,31
              r2_aux(i)     = -1
            r2_new_aux(i) = -1
            do r=0,r2
              resaccrel(i,r)  = 0d0
              resaccabs(i,r)  = 0d0
              resaccrel2(i,r) = 0d0
              resaccabs2(i,r) = 0d0
            enddo
          enddo
          nmaster   = 5
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
        endif

c store DD debug info
        if (id.eq.0) then
          s_DDin  = 'E_dd'
          nc_DDin = 5
          nr_DDin = 10
          ni_DDin = 2
          r_DDin(1) = p1
          r_DDin(2) = p2
          r_DDin(3) = p3
          r_DDin(4) = p4
          r_DDin(5) = p5
          r_DDin(6) = p12
          r_DDin(7) = p23
          r_DDin(8) = p34
          r_DDin(9) = p45
          r_DDin(10)= p15
          c_DDin(1) = m02
          c_DDin(2) = m12
          c_DDin(3) = m22
          c_DDin(4) = m32
          c_DDin(5) = m42
          i_DDin(1) = r2
          i_DDin(2) = id
        endif

        if (r2.gt.r2max5) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'E_dd called for rank r2 =',r2
            write(outchannel,*) 'r2max5 =',r2max5,' too small'
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        endif

#ifdef collierdd
c LH: read global cache
c =====================
        if ((use_cache_system).and.(tencache.gt.5)) then
          if ((ncache.gt.0).and.(ncache.le.ncache_max)) then
              xcache(1) = p1
              xcache(2) = p2
              xcache(3) = p3
              xcache(4) = p4
              xcache(5) = p5
              xcache(6) = p12
              xcache(7) = p23
              xcache(8) = p34
              xcache(9) = p45
              xcache(10) = p15
              xcache(11) = m02
              xcache(12) = m12
              xcache(13) = m22
              xcache(14) = m32
              xcache(15) = m42

              cntmax = NCoefs(r2,5) + 6*r2 + 9

              if (cntmax.gt.NCoefmax5) then
                if (cout_on.and.(cout.le.coutmax)) then
                  write(outchannel,*) 'NCoefmax5 too small!'
                  if (cout.eq.coutmax) call DDlastmessage()
                  cout = cout+1
                endif
                stopflag = min(-9,stopflag)
              endif

              r2cache = r2
              call ReadCache(fct(1:cntmax),cntmax,xcache,15,2,id,5,
     &                       r2cache,nocalc,wrica)

              if(nocalc)then
                accflag   = nint(dreal(fct(1)))
                errflag   = nint(dreal(fct(2)))
                stopflag  = min(nint(dreal(fct(3))),stopflag)
                cnt = 3

                do r=0,r2
                  cnt = cnt+1
                  if (id.eq.0) resaccabs(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  if (id.eq.0) resaccrel(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  Eij_err(tid(id),r)= dreal(fct(cnt))
                  cnt = cnt+1
                  if (id.eq.0) resaccabs2(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  if (id.eq.0) resaccrel2(id,r) = dreal(fct(cnt))
                  cnt = cnt+1
                  Eij_err2(tid(id),r)= dreal(fct(cnt))
                  do n0=0,r,2
                    n1234 = r-n0
                    do n1=0,n1234
                      do n2=0,n1234-n1
                        do n3=0,n1234-n1-n2
                          n4=n1234-n1-n2-n3

                          cnt = cnt+1
                          E(n0/2,n1,n2,n3,n4) = fct(cnt)

                        end do
                      end do
                    end do
                  end do
                end do
                return
              end if
          end if
        end if
c =====================
#endif

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p45
        q4  = p5
        q12 = (p1+p12-p2)/2d0
        q13 = (p1+p45-p23)/2d0
        q14 = (p1+p5-p15)/2d0
        q23 = (p45+p12-p3)/2d0
        q24 = (p5+p12-p34)/2d0
        q34 = (p5+p45-p4)/2d0

c Gram and related matrices
        z4(1,1) = 2d0*q1
        z4(1,2) = 2d0*q12
        z4(1,3) = 2d0*q13
        z4(1,4) = 2d0*q14
        z4(2,1) = z4(1,2)
        z4(2,2) = 2d0*q2
        z4(2,3) = 2d0*q23
        z4(2,4) = 2d0*q24
        z4(3,1) = z4(1,3)
        z4(3,2) = z4(2,3)
        z4(3,3) = 2d0*q3
        z4(3,4) = 2d0*q34
        z4(4,1) = z4(1,4)
        z4(4,2) = z4(2,4)
        z4(4,3) = z4(3,4)
        z4(4,4) = 2d0*q4
        call inverse_dd(z4,z4i,detz4,4)
        do i=1,4
        do j=1,4
          tz4(i,j) = z4i(j,i)*detz4
        end do
        end do

c Caley and related matrices
        f(1) = q1-m12+m02
        f(2) = q2-m22+m02
        f(3) = q3-m32+m02
        f(4) = q4-m42+m02

        x4(0,0) = 2d0*m02
        do i=1,4
          x4(0,i) = f(i)
          x4(i,0) = f(i)
        do j=1,4
          x4(i,j) = z4(i,j)
        end do
        end do

        do i=1,5
        do j=1,5
          mat(i,j) = x4(i-1,j-1)
        end do
        end do
        call xinverse_dd(mat,mati,detx4,5)
        do i=0,4
        do j=i,4
          tx4(i,j) = mati(j+1,i+1)*detx4
          tx4(j,i) = tx4(i,j) 
        end do
        end do
        
        do i=1,4
        do j=1,4
          mat4(i,j) = z4(i,j)
        end do
        end do
        do k=1,4
        do j=1,4
          mat4(k,j) = f(j)
        enddo
        call xinverse_dd(mat4,mati4,detm,4)
        do j=1,4
          mat4(k,j) = z4(k,j)
          ttx4(k,k,0,j) = 0d0
          ttx4(0,j,k,k) = 0d0
        do i=k+1,4
          ttx4(k,i,0,j) = mati4(j,i)*detm
          ttx4(i,k,0,j) = -ttx4(k,i,0,j)
          ttx4(0,j,k,i) = ttx4(k,i,0,j)
          ttx4(0,j,i,k) = ttx4(i,k,0,j)
        end do
        end do
        end do

c Y matrix and related quantities
        mat(1,1) = 2d0*m02
        mat(2,2) = 2d0*m12
        mat(3,3) = 2d0*m22
        mat(4,4) = 2d0*m32
        mat(5,5) = 2d0*m42
        mat(1,2) = m02+m12-p1
        mat(2,3) = m12+m22-p2
        mat(3,4) = m22+m32-p3
        mat(4,5) = m32+m42-p4
        mat(1,5) = m42+m02-p5
        mat(1,3) = m02+m22-p12
        mat(2,4) = m12+m32-p23
        mat(3,5) = m22+m42-p34
        mat(1,4) = m32+m02-p45
        mat(2,5) = m42+m12-p15
        do i=1,5
        do j=i+1,5
          mat(j,i) = mat(i,j)
        end do
        end do
        dety4 = xdet_dd(mat,5)

        do i=0,4
          do j=1,5
            vec(j)     = mat(j,i+1)
            mat(j,i+1) = 1d0
          enddo
          eta4(i) = xdet_dd(mat,5)/dety4
          do j=1,5
          mat(j,i+1) = vec(j)
          enddo
        enddo

c parameters for error propagation
        maxtxij(id) = 0d0
        do i=0,4
        do j=i,4
          maxtxij(id) = max(maxtxij(id),abs(tx4(i,j))) 
        end do
        end do
        maxttx0ijm(id) = 0d0
        do i=1,4
        do j=i+1,4
        do m=1,4
          maxttx0ijm(id) = max(maxttx0ijm(id),abs(ttx4(i,j,0,m)))
        end do
        end do
        end do
        maxtz_nj(id) = 0d0
        do n=1,4
        do j=n,4
          maxtz_nj(id) = max(maxtz_nj(id),abs(tz4(n,j)))
        end do
        end do

c set identifiers for lower-point integrals
        n = 0
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            nid(n) = id + 2**k
            n=n+1
          endif
          if (n.eq.5) goto 205
        enddo
205     continue

c*** Tensor reduction
c====================
        if (mode5.eq.0) then

          call Ex_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &           m02,m12,m22,m32,m42,z4,eta4,detx4,tx4,ttx4,r2,id,nid)

        elseif (mode5.eq.1) then
          call Ey_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &           m02,m12,m22,m32,m42,eta4,detz4,tz4,dety4,r2,id,nid)
        endif

c test output
c============
        if ((outlevel.gt.10).and.(id.eq.0)) then
          if (cout_on.and.(cout.le.coutmax)) then
            do i=0,4
              write(outchannel,*)
              write(outchannel,*) '4pt tensor integral id = ',nid(i)
              write(outchannel,*) '  Dacc_pave = ',acc_pave(nid(i))
              if (qmethod_new(nid(i)).ne.0)
     &        write(outchannel,*) '  Dacc_new  = ',acc_new(nid(i)),
     &                 '  method ',qmethod_new(nid(i))
            enddo
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
        endif

c accuracy estimate of master call
c=================================
c normalization to maximal coefficient
          Emax = abs(E(0,0,0,0,0))
          do r=1,r2
            i0=0
            i1234 = r-i0
            do i1=0,i1234
              do i2=0,i1234-i1
                do i3=0,i1234-i1-i2
                  i4=i1234-i1-i2-i3
                  Emax = max(Emax,abs(E(i0/2,i1,i2,i3,i4)))
                enddo
              enddo
            enddo
          enddo
          do r=0,r2
            resaccabs(id,r)  = Eij_err(tid(id),r)
            resaccabs2(id,r) = Eij_err2(tid(id),r)
            resaccrel(id,r)  = Eij_err(tid(id),r)/Emax
            resaccrel2(id,r) = Eij_err2(tid(id),r)/Emax
            if (resaccrel(id,r).gt.aimacc(5)) accflag = 1
            if (resaccrel(id,r).gt.erracc(5)) errflag = 1
          enddo

#ifdef collierdd
c LH: write global cache
c ======================
        if ((use_cache_system).and.(tencache.gt.5)) then
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
                  fct(cnt) = Eij_err(tid(id),r)
                  cnt = cnt+1
                  fct(cnt) = resaccabs2(id,r)
                  cnt = cnt+1
                  fct(cnt) = resaccrel2(id,r)
                  cnt = cnt+1
                  fct(cnt) = Eij_err2(tid(id),r)
                  do n0=0,r,2
                    n1234 = r-n0
                    do n1=0,n1234
                      do n2=0,n1234-n1
                        do n3=0,n1234-n1-n2
                          n4=n1234-n1-n2-n3

                          cnt = cnt+1
                          fct(cnt) = E(n0/2,n1,n2,n3,n4)

                        end do
                      end do
                    end do
                  end do
                end do
  
                call WriteCache(fct(1:cnt),cnt,id,5,r2)

              end if
          end if
        end if
c ======================
#endif

        end

**********************************************************************
        subroutine Ex_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &         m02,m12,m22,m32,m42,z4,eta4,detx4,tx4,ttx4,r2,id,nid)
**********************************************************************
*       5-point coefficients  
*       E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m32)
*                         \___/\___/\___/\___/\___/
*                          2i    j    k    l    m  indices
*       of rank r=i+j+k+l+m with r <= r2 <= 5
*
*       Method of A.Denner, S.Dittmaier, 
*                       NPB734 (2006) 62 [hep-ph/0509141], Section 6
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_5pt
        use DD_4pt
        implicit real*8 (a-z)

c local variables
        integer del(4,4),r2,i0,i1,i2,i3,i4,i(4),n,j,k,m,l(4),r
        integer id,id0,nid(0:nmax-1)
        data del/1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1/
        complex*16 E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
        complex*16 Eb(0:4,0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 D0(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                0:max(r2-1,0),0:max(r2-1,0))
        complex*16 Duv0(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                  0:max(r2-1,0),0:max(r2-1,0))
        complex*16 D_1(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                 0:max(r2-1,0))
        complex*16 Duv_1(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                   0:max(r2-1,0))
        complex*16 D_2(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                 0:max(r2-1,0))
        complex*16 Duv_2(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                   0:max(r2-1,0))
        complex*16 D_3(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                 0:max(r2-1,0))
        complex*16 Duv_3(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                   0:max(r2-1,0))
        complex*16 D_4(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                 0:max(r2-1,0))
        complex*16 Duv_4(0:max(r2-1,0),0:max(r2-1,0),0:max(r2-1,0),
     &                   0:max(r2-1,0))
        complex*16 m02,m12,m22,m32,m42,eta4(0:4)
        complex*16 detx4,tx4(0:4,0:4),ttx4(0:4,0:4,0:4,0:4),caux
        real*8 z4(4,4),maxc(0:4),maxzc(0:4)

        id0 = id

        if (r2.gt.5) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'Ex_dd not working up to rank ',r2
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        endif

        call D0_dd(D0,Duv0,   p2, p3, p4,p15,p23,p34,m12,m22,m32,m42,
     &            max(r2-1,0),nid(0))
        call D_dd(D_1,Duv_1,p12, p3, p4, p5,p45,p34,m02,m22,m32,m42,
     &            max(r2-1,0),nid(1))
        call D_dd(D_2,Duv_2, p1,p23, p4, p5,p45,p15,m02,m12,m32,m42,
     &            max(r2-1,0),nid(2))
        call D_dd(D_3,Duv_3, p1, p2,p34, p5,p12,p15,m02,m12,m22,m42,
     &            max(r2-1,0),nid(3))
        call D_dd(D_4,Duv_4, p1, p2, p3,p45,p12,p23,m02,m12,m22,m32,
     &            max(r2-1,0),nid(4))

        E(0,0,0,0,0) = -eta4(0)*D0(0,0,0,0,0)
     &                 -eta4(1)*D_1(0,0,0,0)-eta4(2)*D_2(0,0,0,0)
     &                 -eta4(3)*D_3(0,0,0,0)-eta4(4)*D_4(0,0,0,0)

        Eij_err(tid(id),0) = max(abs(eta4(0))*Dij_err(tid(nid(0)),0),
     &    abs(eta4(1))*Dij_err(tid(nid(1)),0),
     &    abs(eta4(2))*Dij_err(tid(nid(2)),0),
     &    abs(eta4(3))*Dij_err(tid(nid(3)),0),
     &    abs(eta4(4))*Dij_err(tid(nid(4)),0))
        Eij_err2(tid(id),0) = Eij_err(tid(id),0) 

c quantities for error estimate
        maxz4 = 0d0
        do j=1,4
        do n=j,4
          maxz4 = max(maxz4,abs(z4(j,n)))
        enddo
        enddo

        do n=1,4
          maxc(n) = 0d0
          do j=1,4
            maxc(n) = max(maxc(n),abs(tx4(j,n)/detx4))
          enddo
        enddo
        maxc(0) = 0d0
        do j=1,4
          caux = 0d0
          do n=1,4
            caux = caux + tx4(j,n)/detx4
          enddo
          maxc(0) = max(maxc(0),abs(caux))
        enddo

        do n=1,4
          maxzc(n) = 0d0
        do j=1,4
          caux = 0d0
          do m=1,4
            caux = caux + z4(j,m)*tx4(m,n)/detx4/maxz4
          enddo
          maxzc(n) = max(maxzc(n),abs(caux))
        enddo
        enddo
        maxzc(0) = 0d0
        do j=1,4
          caux = 0d0
          do m=1,4
          do n=1,4
            caux = caux + z4(j,m)*tx4(m,n)/detx4/maxz4
          enddo
          enddo
          maxzc(0) = max(maxzc(0),abs(caux))
        enddo

        do r=1,r2

c Ebar(k>0,i1,...) from Eq.(6.12)
        do i0=0,r-1,2
        do i1=0,r-1-i0
        do i2=0,r-1-i0-i1
        do i3=0,r-1-i0-i1-i2
        i4 = r-1-i0-i1-i2-i3
        i(1) = i1
        i(2) = i2
        i(3) = i3
        i(4) = i4
        do k=1,4
          Eb(k,i0,i1,i2,i3,i4) = D0(i0/2,i1,i2,i3,i4)
     &      *(-tx4(k,0)-tx4(k,1)-tx4(k,2)-tx4(k,3)-tx4(k,4))
          if (i1.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &      = Eb(k,i0,i1,i2,i3,i4) + tx4(k,1)*D_1(i0/2,i2,i3,i4)
          if (i2.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &      = Eb(k,i0,i1,i2,i3,i4) + tx4(k,2)*D_2(i0/2,i1,i3,i4)
          if (i3.eq.0) Eb(k,i0,i1,i2,i3,i4)  
     &      = Eb(k,i0,i1,i2,i3,i4) + tx4(k,3)*D_3(i0/2,i1,i2,i4)
          if (i4.eq.0) Eb(k,i0,i1,i2,i3,i4) 
     &      = Eb(k,i0,i1,i2,i3,i4) + tx4(k,4)*D_4(i0/2,i1,i2,i3)
          do j=1,4
            if (i(j).ne.0) then
              l(1) = i1-del(j,1)
              l(2) = i2-del(j,2)
              l(3) = i3-del(j,3)
              l(4) = i4-del(j,4)
              Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4) 
     &          + 2d0*i(j)*( ttx4(k,1,0,j)+ttx4(k,2,0,j)
     &                      +ttx4(k,3,0,j)+ttx4(k,4,0,j) )
     &               *D0(i0/2+1,l(1),l(2),l(3),l(4)) 
              if (l(1).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4)
     &          - 2d0*i(j)*ttx4(k,1,0,j)*D_1(i0/2+1,l(2),l(3),l(4))
              if (l(2).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4)
     &          - 2d0*i(j)*ttx4(k,2,0,j)*D_2(i0/2+1,l(1),l(3),l(4))
              if (l(3).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4)
     &          - 2d0*i(j)*ttx4(k,3,0,j)*D_3(i0/2+1,l(1),l(2),l(4))
              if (l(4).eq.0) Eb(k,i0,i1,i2,i3,i4) = Eb(k,i0,i1,i2,i3,i4)
     &          - 2d0*i(j)*ttx4(k,4,0,j)*D_4(i0/2+1,l(1),l(2),l(3))
            endif
          end do
          end do
        end do
        end do
        end do
        end do

c Ebar(i0>0,i1,...) from Eq.(6.13)
        do i0=0,r-2,2
        do i1=0,r-2-i0
        do i2=0,r-2-i0-i1
        do i3=0,r-2-i0-i1-i2
        i4 = r-2-i0-i1-i2-i3
          Eb(0,i0,i1,i2,i3,i4) = D0(i0/2+1,i1,i2,i3,i4)
     &      *(-tx4(0,1)-tx4(0,2)-tx4(0,3)-tx4(0,4))
          if (i1.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &      = Eb(0,i0,i1,i2,i3,i4) + tx4(0,1)*D_1(i0/2+1,i2,i3,i4)
          if (i2.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &      = Eb(0,i0,i1,i2,i3,i4) + tx4(0,2)*D_2(i0/2+1,i1,i3,i4)
          if (i3.eq.0) Eb(0,i0,i1,i2,i3,i4)  
     &      = Eb(0,i0,i1,i2,i3,i4) + tx4(0,3)*D_3(i0/2+1,i1,i2,i4)
          if (i4.eq.0) Eb(0,i0,i1,i2,i3,i4) 
     &      = Eb(0,i0,i1,i2,i3,i4) + tx4(0,4)*D_4(i0/2+1,i1,i2,i3)
        end do
        end do
        end do
        end do

c E(i0,i1,...) from symmetrization as in Eqs.(6.14) and (6.15)
        do i0=0,r,2
        do i1=0,r-i0
        do i2=0,r-i0-i1
        do i3=0,r-i0-i1-i2
        i4 = r-i0-i1-i2-i3
          E(i0/2,i1,i2,i3,i4) = 0d0
          if (i0.gt.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &                                     + i0*Eb(0,i0-2,i1,i2,i3,i4)
          if (i1.gt.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &                                     + i1*Eb(1,i0,i1-1,i2,i3,i4)
          if (i2.gt.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &                                     + i2*Eb(2,i0,i1,i2-1,i3,i4)
          if (i3.gt.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &                                     + i3*Eb(3,i0,i1,i2,i3-1,i4)
          if (i4.gt.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &                                     + i4*Eb(4,i0,i1,i2,i3,i4-1)
          E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) / (r*detx4)
        end do
        end do
        end do
        end do

c algebraic extra terms according to Eqs.(6.16) and (6.21)
        if (r.eq.5) then
          E(2,1,0,0,0) = E(2,1,0,0,0) - tx4(0,1)/(240d0*detx4)
          E(2,0,1,0,0) = E(2,0,1,0,0) - tx4(0,2)/(240d0*detx4)
          E(2,0,0,1,0) = E(2,0,0,1,0) - tx4(0,3)/(240d0*detx4)
          E(2,0,0,0,1) = E(2,0,0,0,1) - tx4(0,4)/(240d0*detx4)
        endif

        Eij_err(tid(id),r)  = 0d0
        Eij_err2(tid(id),r) = 0d0
        do n=0,4
         Eij_err(tid(id),r)  = max(Eij_err(tid(id),r),
     &                         maxc(n)*Dij_err(tid(nid(n)),r-1))
         Eij_err2(tid(id),r) = max(Eij_err2(tid(id),r),
     &                         maxzc(n)*Dij_err2(tid(nid(n)),r-1))
        enddo

        enddo

        end

**********************************************************************
        subroutine Ey_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &               m02,m12,m22,m32,m42,eta4,detz4,tz4,dety4,r2,id,nid)
**********************************************************************
*       5-point coefficients  
*       E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m32)
*                         \___/\___/\___/\___/\___/
*                          2i    j    k    l    m  indices
*       of rank r=i+j+k+l+m with r <= r2 <= 4
*
*       Method of A.Denner, S.Dittmaier, 
*                 NPB658 (2003) 175 [hep-ph/0212259]
*---------------------------------------------------------------------
*       10.9.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_5pt
        use DD_4pt
        implicit real*8 (a-z)

c local variables
        integer del(4,4),r2,i0,i1,i2,i3,i4,i(4),j,l(4),r
        integer id,id0,nid(0:nmax-1)
        data del/1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1/
        complex*16 E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
        complex*16 D0(0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv0(0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 D_1(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv_1(0:r2,0:r2,0:r2,0:r2)
        complex*16 D_2(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv_2(0:r2,0:r2,0:r2,0:r2)
        complex*16 D_3(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv_3(0:r2,0:r2,0:r2,0:r2)
        complex*16 D_4(0:r2,0:r2,0:r2,0:r2)
        complex*16 Duv_4(0:r2,0:r2,0:r2,0:r2)
        complex*16 m02,m12,m22,m32,m42,eta4(0:4),dety4
        real*8 tz4(4,4)

        id0 = id

        if (r2.gt.4) then
          if (cout_on.and.(cout.le.coutmax)) then
            write(outchannel,*) 'Ey_dd not working up to rank ',r2
            if (cout.eq.coutmax) call DDlastmessage()
            cout = cout+1
          endif
          stopflag = min(-10,stopflag)
        endif

        call D0_dd(D0,Duv0,   p2, p3, p4,p15,p23,p34,m12,m22,m32,m42,
     &            r2,nid(0))
        call D_dd(D_1,Duv_1,p12, p3, p4, p5,p45,p34,m02,m22,m32,m42,
     &            r2,nid(1))
        call D_dd(D_2,Duv_2, p1,p23, p4, p5,p45,p15,m02,m12,m32,m42,
     &            r2,nid(2))
        call D_dd(D_3,Duv_3, p1, p2,p34, p5,p12,p15,m02,m12,m22,m42,
     &            r2,nid(3))
        call D_dd(D_4,Duv_4, p1, p2, p3,p45,p12,p23,m02,m12,m22,m32,
     &            r2,nid(4))

        do r=0,r2
        do i0=0,r,2
        do i1=0,r-i0
        do i2=0,r-i0-i1
        do i3=0,r-i0-i1-i2
        i4 = r-i0-i1-i2-i3
        i(1) = i1
        i(2) = i2
        i(3) = i3
        i(4) = i4
          E(i0/2,i1,i2,i3,i4) = -eta4(0)*D0(i0/2,i1,i2,i3,i4)
          if (i1.eq.0) E(i0/2,i1,i2,i3,i4) 
     &                = E(i0/2,i1,i2,i3,i4) - eta4(1)*D_1(i0/2,i2,i3,i4)
          if (i2.eq.0) E(i0/2,i1,i2,i3,i4) 
     &                = E(i0/2,i1,i2,i3,i4) - eta4(2)*D_2(i0/2,i1,i3,i4)
          if (i3.eq.0) E(i0/2,i1,i2,i3,i4) 
     &                = E(i0/2,i1,i2,i3,i4) - eta4(3)*D_3(i0/2,i1,i2,i4)
          if (i4.eq.0) E(i0/2,i1,i2,i3,i4) 
     &                = E(i0/2,i1,i2,i3,i4) - eta4(4)*D_4(i0/2,i1,i2,i3)
          do j=1,4
          l(1) = i1-del(j,1)
          l(2) = i2-del(j,2)
          l(3) = i3-del(j,3)
          l(4) = i4-del(j,4)
            if (i(j).ne.0) then
              E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) - 2d0*i(j)/dety4
     &          *(tz4(j,1)+tz4(j,2)+tz4(j,3)+tz4(j,4))
     &          *D0(i0/2+1,l(1),l(2),l(3),l(4)) 
              if (l(1).eq.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &          + 2d0*i(j)/dety4*tz4(j,1)*D_1(i0/2+1,l(2),l(3),l(4))
              if (l(2).eq.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &          + 2d0*i(j)/dety4*tz4(j,2)*D_2(i0/2+1,l(1),l(3),l(4))
              if (l(3).eq.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &          + 2d0*i(j)/dety4*tz4(j,3)*D_3(i0/2+1,l(1),l(2),l(4))
              if (l(4).eq.0) E(i0/2,i1,i2,i3,i4) = E(i0/2,i1,i2,i3,i4) 
     &          + 2d0*i(j)/dety4*tz4(j,4)*D_4(i0/2+1,l(1),l(2),l(3))
            endif
          enddo
        end do
        end do
        end do
        end do

        if (r.eq.4) E(2,0,0,0,0) = E(2,0,0,0,0) - detz4/dety4/48d0

        Eij_err(tid(id),r) = max(
     &    abs(eta4(0))*Dij_err(tid(nid(0)),r),
     &    abs(eta4(1))*Dij_err(tid(nid(1)),r),
     &    abs(eta4(2))*Dij_err(tid(nid(2)),r),
     &    abs(eta4(3))*Dij_err(tid(nid(3)),r),
     &    abs(eta4(4))*Dij_err(tid(nid(4)),r),
     &    maxtz_nj(id)*max(D00_err(tid(nid(0)),r),
     &                  D00_err(tid(nid(1)),r),D00_err(tid(nid(2)),r),
     &                  D00_err(tid(nid(3)),r),D00_err(tid(nid(4)),r))
     &      /abs(dety4) )
        Eij_err2(tid(id),r) = Eij_err(tid(id),r) 

        enddo

        end

**********************************************************************
        subroutine E0_dd(E0,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &                   m02,m12,m22,m32,m42,r2,id)
**********************************************************************
*       5-point coefficients E(0)_{...} with unshifted momentum
*---------------------------------------------------------------------
*       15.9.2006 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_5pt
        implicit real*8 (a-z)

c local variables
        integer r2,r,i0,i1,i2,i3,i4,i5,i12345,id
        complex*16 E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
        complex*16 E0(0:r2/2,0:r2,0:r2,0:r2,0:r2,0:r2)
        complex*16 m02,m12,m22,m32,m42

        call E_dd(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,
     &            m02,m12,m22,m32,m42,r2,id)

        do r=0,r2
          do i0=0,r,2
          i12345 = r-i0
          i1=0
          do i2=0,i12345-i1
          do i3=0,i12345-i1-i2
          do i4=0,i12345-i1-i2-i3
            i5 = i12345-i1-i2-i3-i4
            E0(i0/2,0,i2,i3,i4,i5) = E(i0/2,i2,i3,i4,i5)
          end do
          end do
          end do
          do i1=1,i12345
          do i2=0,i12345-i1
          do i3=0,i12345-i1-i2
          do i4=0,i12345-i1-i2-i3
            i5 = i12345-i1-i2-i3-i4
            E0(i0/2,i1,i2,i3,i4,i5) = -E0(i0/2,i1-1,i2,i3,i4,i5)
     &       - E0(i0/2,i1-1,i2+1,i3,i4,i5) - E0(i0/2,i1-1,i2,i3+1,i4,i5)
     &       - E0(i0/2,i1-1,i2,i3,i4+1,i5) - E0(i0/2,i1-1,i2,i3,i4,i5+1)
        end do
        end do
        end do
        end do
        end do
        end do

        end

**********************************************************************
        subroutine Etransmin(E,p1,p2,p3,p4,p5,p12,p23,p34,p45,p15,r2)
**********************************************************************
*       transform 5-point coefficients to minimal momentum basis
*---------------------------------------------------------------------
*       19.3.2007 Stefan Dittmaier
**********************************************************************
        use DD_global
        use DD_5pt
        implicit real*8 (a-z)

c local variables
        integer r2,i(4),j(4),h(4),ijh(4),k,l,m,n
        complex*16 E(0:r2/2,0:r2,0:r2,0:r2,0:r2)
        real*8 z4(4,4),z4i(4,4)

        if (r2.le.1) return

c algebraic quantities
c---------------------
        q1  = p1
        q2  = p12
        q3  = p45
        q4  = p5
        q12 = (p1+p12-p2)/2d0
        q13 = (p1+p45-p23)/2d0
        q14 = (p1+p5-p15)/2d0
        q23 = (p45+p12-p3)/2d0
        q24 = (p5+p12-p34)/2d0
        q34 = (p5+p45-p4)/2d0

c Gram and related matrices
        z4(1,1) = 2d0*q1
        z4(1,2) = 2d0*q12
        z4(1,3) = 2d0*q13
        z4(1,4) = 2d0*q14
        z4(2,1) = z4(1,2)
        z4(2,2) = 2d0*q2
        z4(2,3) = 2d0*q23
        z4(2,4) = 2d0*q24
        z4(3,1) = z4(1,3)
        z4(3,2) = z4(2,3)
        z4(3,3) = 2d0*q3
        z4(3,4) = 2d0*q34
        z4(4,1) = z4(1,4)
        z4(4,2) = z4(2,4)
        z4(4,3) = z4(3,4)
        z4(4,4) = 2d0*q4
        call inverse_dd(z4,z4i,detz4,4)

c rank 2
        do k=1,4
        do l=k,4
          do m=1,4
          i(m) = 0
          enddo
          i(k) = i(k) + 1
          i(l) = i(l) + 1
          E(0,i(1),i(2),i(3),i(4)) = E(0,i(1),i(2),i(3),i(4)) 
     &               + 2d0*z4i(k,l)*E(1,0,0,0,0)
        end do
        end do
        E(1,0,0,0,0) = 0d0

        if (r2.le.2) return

c rank 3
        do k=1,4
        do l=k,4
        do m=l,4
          do n=1,4
          i(n) = 0
          j(n) = 0
          h(n) = 0
          enddo
          i(k) = i(k) + 1
          j(l) = j(l) + 1
          h(m) = h(m) + 1
          do n=1,4
          ijh(n) = i(n) + j(n) + h(n)
          enddo
          E(0,ijh(1),ijh(2),ijh(3),ijh(4)) = 
     &          E(0,ijh(1),ijh(2),ijh(3),ijh(4)) 
     &        + 2d0*z4i(l,m)*E(1,i(1),i(2),i(3),i(4))
     &        + 2d0*z4i(k,m)*E(1,j(1),j(2),j(3),j(4))
     &        + 2d0*z4i(l,k)*E(1,h(1),h(2),h(3),h(4))
        end do
        end do
        end do
        do k=1,4
          do m=1,4
          i(m) = 0
          enddo
          i(k) = i(k) + 1
          E(1,i(1),i(2),i(3),i(4)) = 0d0
        enddo

        if (r2.le.3) return

        if (cout_on.and.(cout.le.coutmax)) then
          write(outchannel,*) 
     &       'subroutine Etransmin not yet working for rank = ',r2
          if (cout.eq.coutmax) call DDlastmessage()
          cout = cout+1
        endif
        stopflag = min(-10,stopflag)
        
        end

**********************************************************************
        subroutine E_dd_dummy(r2)
**********************************************************************
*       5-point coefficients  
*       E(i,j,k,l,m) = E_{0...01...12...23...34...4}(p1,...,m42)
*                         \___/\___/\___/\___/\___/
*                           i    j    k    l    m  indices
*       of rank r=i+j+k+l+m with r <= r2
*
*       DUMMY CALL for initialization
*---------------------------------------------------------------------
*       20.7.2015 Stefan Dittmaier
**********************************************************************
        use DD_global
        implicit real*8 (a-z)

c local variables
        integer r,r2,i,k,n,id,nid(0:nmax-1)

        id = 0

        if (id.eq.0) then
          do i=0,31
              r2_aux(i)     = -1
            r2_new_aux(i) = -1
            do r=0,r2
              resaccrel(i,r)  = 0d0
              resaccabs(i,r)  = 0d0
              resaccrel2(i,r) = 0d0
              resaccabs2(i,r) = 0d0
            enddo
          enddo
          nmaster   = 5
          r2master  = r2
          accflag   = 0
          errflag   = 0
          stopflag  = 0
        endif

c set identifiers for lower-point integrals
        n = 0
        do k=0,nmax-1
          if (mod(id,2**(k+1))/2**k.eq.0) then
            nid(n) = id + 2**k
            n=n+1
          endif
          if (n.eq.5) goto 205
        enddo
205     continue

        end
