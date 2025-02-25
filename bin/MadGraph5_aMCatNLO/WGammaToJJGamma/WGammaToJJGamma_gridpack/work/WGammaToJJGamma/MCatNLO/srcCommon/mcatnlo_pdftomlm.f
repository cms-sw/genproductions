c Interface to MLMPDF; the routines to be called are in PDFLIB format:
c
c  pdfset: sets the relevant parameters, calling pdfpar; the inputs are
c          in the same format as in pdflib (parm(20),value(20))
c  pftopdg: returns x*pdfs; it calls mlmpdf
c  structm: returns x*pdfs; it calls mlmpdf
c  structp: returns x*pdfs for photons; it calls structm
c
c On 16/7/2003, post-MRST99 and -CTEQ5 sets have been added. All the new
c set have not been included in PDFLIB, and their labelling scheme is
c defined in mcatnlo_mlmtopdf.f. Group=10 corresponds to Alekhin
c
c On 16/5/2002, common block and function names (not related to PDFLIB) 
c have been modified, inserting fk88 or ifk88 in front, to avoid conflicts 
c when this interface is used in large packages
c
c On 2/6/2002, condition on unrecognized parm(i) value removed, being
c machine-dependent (we cannot assume the user set all unused parm(i)
c to null string)
c***************************** WARNING ********************************
c There are notorious and nasty bugs in pdflib relevant to photon densities.
c When pftopdg is called, up and down are twice as large as upbar and downbar;
c this "feature" is kept here, to insure full compitibility with any program
c the uses pdflib, and fixes this bug. Since this bug is not considered as
c a bug, this seems to be a safe strategy with respect to future upgrades
c of pdflib. The same doesn't hold for the bug affecting AFG photon set
c (group,set)=(6,3); in this case, current pdflib version has all quarks
c multiplied by 4 and antiquarks multiplied by 2 (4/2 is the factor of 2
c mentioned before). This bug is not present here. Thus, be careful: a code
c that corrects this bug when linked to pdflib will get it wrong when
c using this interface 
c
c
c
c In all PDFLIB routines, the densities are defined as follows:
c   up=up_val+u_sea
c   down=down_val+d_sea
c   upbar=u_sea
c   downbar=d_sea
c This is rather misleading in the case of the pion
c
      subroutine pdfset(parm,value)
c Doesn't fill w505110, w505120, w505121, w505122 as the real pdfset
c is supposed to do. Does fill w50511, w50512 w50513
      implicit none
      character*20 parm(20)
      real*8 value(20)
      character*70 strin,strout
      integer ihmap(1:3),ndnsmap(1:3,1:10,1:142)
      common/fk88imaps/ihmap,ndnsmap
      integer i,ipdfih,ipdfgroup,ipdfndns,ih,ndns,iret,nlf
      common/fk88ipdfs/ih,ndns
      real*8 xpdflam4,xlam4,xpdflam5,xlam5,xmc,xmb,as,
     #       fk88alfa,fk88xlambd
      parameter (xmc=1.5d0)
      parameter (xmb=4.75d0)
      integer mode,lo
      real*8 tmas
      common/w50511/ipdfih,ipdfgroup,ipdfndns,mode,nlf,lo,tmas
      common/w50512/xpdflam4,xpdflam5
      real*8 xmin,xmax,q2min,q2max
      common/w50513/xmin,xmax,q2min,q2max
      character*2 scheme
      logical ini
      data ini/.true./
c
      if(ini)then
c        write(*,*)'This interface is based upon PDFLIB v8.04'
        call fk88init_of_map()
c mode is the old number set
        mode=0
        lo=2
        tmas=180.d0
c xmin,..,q2max information not available; set the parameters
c to zero -- the program will probably crash
        xmin=0.d0
        xmax=0.d0
        q2min=0.d0
        q2max=0.d0
        ini=.false.
      endif
c Force the user to choose a set (don't rely on defaults)
      ipdfih=-1
      ipdfgroup=-1
      ipdfndns=-1
      xpdflam4=0.d0
      xpdflam5=0.d0
c Convert parm(i) to uppercase
      do i=1,20
        strin=parm(i)
        call fk88low_to_upp(strin,strout)
        parm(i)=strout
      enddo
      do i=1,20
        if(parm(i).eq.'NPTYPE')then
          ipdfih=value(i)
        elseif(parm(i).eq.'NGROUP')then
          ipdfgroup=value(i)
        elseif(parm(i).eq.'NSET')then
          ipdfndns=value(i)
        elseif(parm(i).eq.'QCDL4')then
          xpdflam4=value(i)
        elseif(parm(i).eq.'QCDL5')then
          xpdflam5=value(i)
        elseif(parm(i).eq.'DO')then
          ipdfih=1
          ipdfgroup=1
          ipdfndns=value(i)
        elseif(parm(i).eq.'DFLM')then
          ipdfih=1
          ipdfgroup=2
          ipdfndns=value(i)
        elseif(parm(i).eq.'MRS')then
          ipdfih=1
          ipdfgroup=3
          ipdfndns=value(i)
        elseif(parm(i).eq.'CTEQ')then
          ipdfih=1
          ipdfgroup=4
          ipdfndns=value(i)
        elseif(parm(i).eq.'GRV')then
          ipdfih=1
          ipdfgroup=5
          ipdfndns=value(i)
        elseif(parm(i).eq.'ALEKHIN')then
          ipdfih=1
          ipdfgroup=10
          ipdfndns=value(i)
        elseif(parm(i).eq.'GRV-G')then
          ipdfih=3
          ipdfgroup=5
          ipdfndns=value(i)
        elseif(parm(i).eq.'AFG-G')then
          ipdfih=3
          ipdfgroup=6
          ipdfndns=value(i)
        endif
      enddo
      if(ipdfih.eq.-1.or.ipdfgroup.eq.-1.or.ipdfndns.eq.-1)then
        write(*,*)'You did not choose any valid set'
        stop
      endif
      if(xpdflam4*xpdflam5.ne.0.d0)then
        write(*,*)'Enter either Lambda_4 or Lambda_5'
        stop
      endif
      xlam4=0.d0
      xlam5=0.d0
      ih=ihmap(ipdfih)
      ndns=ndnsmap(ipdfih,ipdfgroup,ipdfndns)
      call pdfpar(ndns,ih,xlam5,scheme,iret)
      if(iret.ne.0)then
        write(*,*)'Set not available in MLMPDF'
        stop
      endif
      nlf=5
      as=fk88alfa(xmb,xlam5,nlf)
      xlam4=fk88xlambd(as,xmb,nlf-1)
      if((xpdflam4+xpdflam5).eq.0.d0)then
        xpdflam4=xlam4
        xpdflam5=xlam5
      else
        if(xpdflam5.ne.0.d0)then
          if(abs(xpdflam5-xlam5).gt.1.d-4)then
            write(*,*)'**** WARNING ****'
            write(*,*)'Your input for Lambda_5:          ',xpdflam5
            write(*,*)'Lambda_5 relevant to this pdf set:',xlam5
            write(*,*)'Your imput is will be used'
          endif
          nlf=5
          as=fk88alfa(xmb,xpdflam5,nlf)
          xpdflam4=fk88xlambd(as,xmb,nlf-1)
        elseif(xpdflam4.ne.0.d0)then
          if(abs(xpdflam4-xlam4).gt.1.d-4)then
            write(*,*)'**** WARNING ****'
            write(*,*)'Your input for Lambda_4:          ',xpdflam4
            write(*,*)'Lambda_4 relevant to this pdf set:',xlam4
            write(*,*)'Your imput is will be used'
          endif
          nlf=4
          as=fk88alfa(xmb,xpdflam4,nlf)
          xpdflam5=fk88xlambd(as,xmb,nlf+1)
        else
          write(*,*)'Fatal error in pdfset'
          stop
        endif
      endif
      return
      end


      subroutine pftopdg(x,scale,dxpdf)
c Returns x*pdf; pdg format
      implicit none
      real*8 x,scale,dxpdf(-6:6)
      real*8 q2
      integer i,nlf
      parameter (nlf=5)
      integer ih,ndns
      common/fk88ipdfs/ih,ndns
      real fx(-nlf:nlf),q2sngl,xsngl,tmp
      character * 1 char
      logical verbose
      data verbose/.true./
c
      q2=scale**2
      q2sngl=sngl(q2)
      xsngl=sngl(x)      
      call mlmpdf(ndns,ih,q2sngl,xsngl,fx,nlf)
      do i=-nlf,nlf
        fx(i)=xsngl*fx(i)
      enddo
c pdflib: 1=down, 2=up; mlm: 1=up, 2=down
      do i=-1,1,2
        tmp=fx(i)
        fx(i)=fx(2*i)
        fx(2*i)=tmp
      enddo
      dxpdf(6)=0.d0
      dxpdf(-6)=0.d0
      if(ih.eq.1)then
c proton
        continue
      elseif(ih.eq.3)then
c pion: pdflib sets upv=dnv, usea=dsea=str, and we assume a pi-: 
c thus we check what follows
        if(verbose)then
          if( abs(fx(1)-fx(-2)).gt.1.e-3 .or.
     #        abs(fx(2)-fx(-1)).gt.1.e-3 .or.
     #        abs(fx(1)-fx(3)).gt.1.e-3 )then
            write(*,*)
     # 'One of the assumptions on pion set does not hold'
            do i=-4,4
              write(6,*)'i, fx(i)',i,fx(i)
            enddo
            write(*,*)'Enter a character to continue'
            read(*,*)char
          endif
c here, down<-->downbar; in this way, the pdflib condition up=down is fulfilled
          tmp=fx(-1)
          fx(-1)=fx(1)
          fx(1)=tmp
        endif
c photon: instead of correcting bugs, they insist on having 
c dxpdf(1)=2*dxpdf(-1), dxpdf(2)=2*dxpdf(-2)
      elseif(ih.eq.4)then
        if(verbose)then
          if( abs(fx(1)-fx(-1)).gt.1.e-3 .or.
     #        abs(fx(2)-fx(-2)).gt.1.e-3 )then
            write(*,*)
     # 'One of the assumptions on photon set does not hold'
            do i=-4,4
              write(6,*)'i, fx(i)',i,fx(i)
            enddo
            write(*,*)'Enter a character to continue'
            read(*,*)char
          endif
        endif
      else
        write(*,*)'Fatal error in pftopdg: ih=',ih
        stop
      endif
      do i=-nlf,nlf
         dxpdf(i)=dble(fx(i))
      enddo
      if(ih.eq.4)then
c see comment abovw
        dxpdf(1)=2*dxpdf(1)
        dxpdf(2)=2*dxpdf(2)
      endif
      return
      end


      subroutine structp(x,q2,p2,ip2,upv,dnv,usea,dsea,str,chm,bot,
     #                   top,gl)
c New for photon pdfs
      implicit none
      real*8 x,q2,p2,ip2,upv,dnv,usea,dsea,str,chm,bot,top,gl
      real*8 scale
c
      if(p2.ne.0.d0)then
c p2 is the photon virtuality
        write(*,*)'We refuse to consider this german crap'
        stop
      endif
      scale=sqrt(q2)
      call structm(x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl)
      return
      end


      subroutine structm(x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl)
c Returns x*pdf
      implicit none
      real*8 x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl
      real*8 q2
      integer i,nlf
      parameter (nlf=5)
      integer ih,ndns
      common/fk88ipdfs/ih,ndns
      real fx(-nlf:nlf),q2sngl,xsngl
      character * 1 char
      logical verbose
      data verbose/.true./
c
      q2=scale**2
      q2sngl=sngl(q2)
      xsngl=sngl(x)      
      call mlmpdf(ndns,ih,q2sngl,xsngl,fx,nlf)
      do i=-nlf,nlf
        fx(i)=xsngl*fx(i)
      enddo
      top=0.d0
      gl=dble(fx(0))
      if(ih.eq.1)then
c proton: up=upv+usea, upbar=usea, dn=dnv+dsea, dnbar=dsea, str=strange
        usea=dble(fx(-1))
        upv=dble(fx(1))-usea
        dsea=dble(fx(-2))
        dnv=dble(fx(2))-dsea
        str=dble(fx(3))
        chm=dble(fx(4))
        bot=dble(fx(5))
      elseif(ih.eq.3)then
c pion: pdflib sets upv=dnv, usea=dsea=str, and we assume a pi+: 
c thus we check what follows
        if(verbose)then
          if( abs(fx(1)-fx(-2)).gt.1.e-3 .or.
     #        abs(fx(2)-fx(-1)).gt.1.e-3 .or.
     #        abs(fx(1)-fx(3)).gt.1.e-3 )then
            write(*,*)
     # 'One of the assumptions on pion set does not hold'
            do i=-4,4
              write(6,*)'i, fx(i)',i,fx(i)
            enddo
            write(*,*)'Enter a character to continue'
            read(*,*)char
          endif
        endif
        dsea=dble(fx(3))
        usea=dsea
        str=dsea
        dnv=dble(fx(2))-dsea
        upv=dnv
        chm=dble(fx(4))
        bot=dble(fx(5))
c photon
      elseif(ih.eq.4)then
        if(verbose)then
          if( abs(fx(1)-fx(-1)).gt.1.e-3 .or.
     #        abs(fx(2)-fx(-2)).gt.1.e-3 )then
            write(*,*)
     # 'One of the assumptions on photon set does not hold'
            do i=-4,4
              write(6,*)'i, fx(i)',i,fx(i)
            enddo
            write(*,*)'Enter a character to continue'
            read(*,*)char
          endif
        endif
        usea=dble(fx(-1))
        upv=dble(fx(1))
        dsea=dble(fx(-2))
        dnv=dble(fx(2))
        str=dble(fx(3))
        chm=dble(fx(4))
        bot=dble(fx(5))
      else
        write(*,*)'fatal error in structm: ih=',ih
        stop
      endif
      return
      end


      subroutine fk88init_of_map()
c This subroutines fills the arrays that map the particle type, group
c number and set number as given in pdflib convention into the corresponding
c quantities as given in mlmpdf conventions
      implicit none
      integer ihmap(1:3),ndnsmap(1:3,1:10,1:142)
      common/fk88imaps/ihmap,ndnsmap
      integer i,j,k
c
      do i=1,3
        do j=1,10
          do k=1,142
            ndnsmap(i,j,k)=0
          enddo
        enddo
      enddo
c ihmap: entry is particle type in pdflib, return value is particle
c type in mlmpdf
      ihmap(1)=1
      ihmap(2)=3
      ihmap(3)=4
c ndnsmap: the three entries are particle type, group number, and set number
c respectively, as in pdflib; the return value is set number in mlmpdf.
c A return value equal to zero means that the set is not present in mlmpdf
c
c
c ************************** proton sets *****************************
c
c DO-1 and 2
      ndnsmap(1,1,6)=1
      ndnsmap(1,1,7)=2
c EHLQ-1 and 2
      ndnsmap(1,1,8)=3
      ndnsmap(1,1,9)=4
c DFLM
      ndnsmap(1,2,7)=5
      ndnsmap(1,2,8)=6
      ndnsmap(1,2,9)=7
c MRS: follow the order in mlmpdf; skip MRSA modified
      ndnsmap(1,3,15)=11
      ndnsmap(1,3,18)=12
      ndnsmap(1,3,22)=13
      ndnsmap(1,3,23)=14
      ndnsmap(1,3,24)=15
      ndnsmap(1,3,25)=16
      ndnsmap(1,3,26)=17
      ndnsmap(1,3,27)=18
      ndnsmap(1,3,28)=19
      ndnsmap(1,3,37)=20
c
      ndnsmap(1,3,39)=71
      ndnsmap(1,3,41)=72
      ndnsmap(1,3,45)=73
      ndnsmap(1,3,46)=74
      ndnsmap(1,3,47)=75
      ndnsmap(1,3,48)=76
      ndnsmap(1,3,49)=77
      ndnsmap(1,3,50)=78
c
      ndnsmap(1,3,53)=91
      ndnsmap(1,3,54)=92
      ndnsmap(1,3,55)=93
      ndnsmap(1,3,56)=94
      ndnsmap(1,3,67)=95
      ndnsmap(1,3,68)=96
      ndnsmap(1,3,69)=97
      ndnsmap(1,3,70)=98
      ndnsmap(1,3,71)=99
c
      ndnsmap(1,3,89)=111
      ndnsmap(1,3,90)=112
      ndnsmap(1,3,91)=113
      ndnsmap(1,3,92)=114
      ndnsmap(1,3,93)=115
      ndnsmap(1,3,94)=116
      ndnsmap(1,3,95)=117
      ndnsmap(1,3,96)=118
      ndnsmap(1,3,97)=119
      ndnsmap(1,3,98)=120
      ndnsmap(1,3,99)=121
      ndnsmap(1,3,100)=122
c
      ndnsmap(1,3,101)=181
      ndnsmap(1,3,102)=182
      ndnsmap(1,3,103)=183
      ndnsmap(1,3,104)=184
      ndnsmap(1,3,105)=185
      ndnsmap(1,3,106)=186
      ndnsmap(1,3,107)=187
      ndnsmap(1,3,108)=188
      ndnsmap(1,3,109)=189
      ndnsmap(1,3,110)=191
      ndnsmap(1,3,111)=192
      do k=112,142
        ndnsmap(1,3,k)=k+88
      enddo
c CTEQ: follow the order in mlmpdf; includes MT
      ndnsmap(1,4,1)=21
      ndnsmap(1,4,2)=22
      ndnsmap(1,4,3)=23
      ndnsmap(1,4,4)=24
      ndnsmap(1,4,6)=25
      ndnsmap(1,4,5)=26
      ndnsmap(1,4,10)=27
      ndnsmap(1,4,11)=28
c
      ndnsmap(1,4,13)=61
      ndnsmap(1,4,14)=62
      ndnsmap(1,4,15)=63
      ndnsmap(1,4,16)=64
      ndnsmap(1,4,12)=65
      ndnsmap(1,4,30)=66
      ndnsmap(1,4,29)=67
      ndnsmap(1,4,31)=68
c
      ndnsmap(1,4,34)=81
      ndnsmap(1,4,33)=82
      ndnsmap(1,4,32)=83
      ndnsmap(1,4,35)=84
      ndnsmap(1,4,36)=85
      ndnsmap(1,4,38)=86
      ndnsmap(1,4,39)=87
      ndnsmap(1,4,40)=88
      ndnsmap(1,4,41)=89
c
      ndnsmap(1,4,48)=101
      ndnsmap(1,4,47)=102
      ndnsmap(1,4,46)=103
      ndnsmap(1,4,49)=104
      ndnsmap(1,4,50)=105
      ndnsmap(1,4,51)=106
      ndnsmap(1,4,52)=107
      ndnsmap(1,4,53)=108
      ndnsmap(1,4,54)=109
      ndnsmap(1,4,55)=110
c
      ndnsmap(1,4,56)=131
      ndnsmap(1,4,57)=132
      ndnsmap(1,4,58)=133
      do k=59,98
        ndnsmap(1,4,k)=k+75
      enddo
c Alekhin: follow the order in mlmpdf
      do k=1,20
        ndnsmap(1,10,k)=k+230
      enddo
c
c ************************** pion sets *****************************
c
c OW 1 and 2
      ndnsmap(2,1,1)=1
      ndnsmap(2,1,2)=2
c SMRS
      ndnsmap(2,3,1)=31
      ndnsmap(2,3,2)=32
      ndnsmap(2,3,3)=33
c
c ************************** photon sets *****************************
c
c DG
      ndnsmap(3,2,1)=40
c ACFGP and AFG 
      ndnsmap(3,6,2)=41
      ndnsmap(3,6,3)=42
c GRV
      ndnsmap(3,5,2)=43
c LAC1
      ndnsmap(3,3,1)=44
c
      return
      end


      function fk88alfa(q,xlam,nf)
      implicit real*8(a-h,o-z)
      data pi/3.1415926535897931d0/
c
      b  = (33-2*nf)/pi/12
      bp = (153 - 19*nf) / pi / 2 / (33 - 2*nf)
      t = 2 * log( q/xlam )
      xlt = log( t )
      fk88alfa = 1/(b * t) -  bp/(b * t)**2 * xlt
      return
      end


      function fk88xlambd(as,q,nf)
      implicit real*8(a-h,o-z)
      data pi/3.1415926535897931d0/
      b  = (33-2*nf)/pi/12
      bp = (153 - 19*nf) / pi / 2 / (33 - 2*nf)
      t  = 1/b/as
    1 xlt = log(t)
      ot = t
c-----------------------------------------------------------
c Value and Derivative of alfa with respect to t
      as0  = 1/b/t - bp*xlt/(b*t)**2
      as1  = - 1/b/t**2 -bp/b**2*(1-2*xlt)/t**3
      t  = (as-as0)/as1 + t
      if(abs(ot-t)/ot.gt.1.d-5)goto 1
      fk88xlambd = q/exp(t/2)
      return
      end
c
c
c Utilities: use them for consistency checks
c
c
      subroutine pftopdg2(x,scale,dxpdf)
c Same results as pftopdg; calls directly structm, which is more
c involved when mlmpdf are linked
      implicit none
      real*8 x,scale,dxpdf(-6:6)
      real*8 upv,dnv,usea,dsea,str,chm,bot,top,gl
      integer i
c
      call structm(x,scale,upv,dnv,usea,dsea,str,chm,bot,top,gl)
      dxpdf(-1)=dsea
      dxpdf(-2)=usea
      dxpdf(0)=gl
      dxpdf(1)=dnv+dsea
      dxpdf(2)=upv+usea
      dxpdf(3)=str
      dxpdf(4)=chm
      dxpdf(5)=bot
      dxpdf(6)=top
      do i=3,6
        dxpdf(-i)=dxpdf(i)
      enddo
      return
      end


      subroutine fk88test_pftopdg(x,scale)
      implicit none
      real*8 x,scale
      real*8 dxpdf(-6:6),dxpdf2(-6:6)
      integer i
c
      call pftopdg(x,scale,dxpdf)
      call pftopdg2(x,scale,dxpdf2)
      do i=-6,6
        if(abs(dxpdf(i)-dxpdf2(i)).gt.1.d-3)then
          write(6,*)'Error in fk88test_pftopdg:',i,dxpdf(i),dxpdf2(i)
        endif
      enddo
      return
      end


      subroutine pdftomlm(ipdfih,ipdfgroup,ipdfndns,itmpih,itmpndns)
c It should not be called when this file is linked
      write(*,*)'Call to pdftomlm must not occur'
      stop
      end


      subroutine setlhacblk(strin)
c It should not be called when this file is linked
      write(*,*)'Call to setlhacblk must not occur'
      stop
      end


      subroutine getlamlha(xlam,xlamlha)
c It should not be called when this file is linked
      write(*,*)'Call to getlamlha must not occur'
      stop
      end
