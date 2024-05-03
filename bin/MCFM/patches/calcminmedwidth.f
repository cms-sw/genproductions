

      subroutine calc_min_medwidth(rescale)
      implicit none 
      include 'constants.f' 
      include 'dm_params.f' 
      include 'qcdcouple.f' 
      include 'ewcouple.f' 
      include 'masses.f' 
      double precision pw_qqb,pw_xx 
      integer j 
      double precision nfq,rescale
      logical first 
      data first/.true./
      save first 
      logical forceminwidth ! variable which makes Gamma > min 
      logical dectotop,dectodm
      double precision pw_ttb,minwidth 
      double precision pw_bb
      logical dectobb 

!==== this should be false now, since mb is explicitly inlcuded
      forceminwidth=.false. 
      minwidth=4d-3

!===== number of light quark partial widths
      nfq=4d0
      pw_ttb=0d0 
      pw_xx=0d0 
      pw_qqb=0d0
      pw_bb=0d0 

!===== check ttb decay open 
      if(medmass.ge.2d0*mt) then 
         dectotop=.true.
      else
         dectotop=.false. 
      endif

!===== check xxb decay open 
      if(medmass.ge.2d0*xmass) then 
         dectodm=.true.
      else
         dectodm=.false. 
      endif

!===== check bbb decay open 
      if(medmass.ge.2d0*mb) then 
         dectobb=.true.
      else
         dectobb=.false. 
      endif


      if(dm_mediator.eq.'vector') then 
         pw_qqb=g_dmq**2*medmass/(12d0*pi) 
         if(dectodm) then 
         pw_xx= g_dmx**2*(medmass**2+2d0*xmass**2)/(12d0*pi*medmass)
     &        *dsqrt(one-4d0*xmass**2/medmass**2)
         endif
         if(dectotop) then 
            pw_ttb= g_dmq**2*(medmass**2+2d0*mt**2)/(12d0*pi*medmass)
     &        *dsqrt(one-4d0*mt**2/medmass**2)
         endif
         if(dectobb) then 
            pw_bb= g_dmq**2*(medmass**2+2d0*mb**2)/(12d0*pi*medmass)
     &        *dsqrt(one-4d0*mb**2/medmass**2)
         endif
      elseif(dm_mediator.eq.'axvect') then 
         pw_qqb=g_dmq**2*medmass/(12d0*pi) 
         if(dectodm) then 
         pw_xx= g_dmx**2*(medmass**2-4d0*xmass**2)/(12d0*pi*medmass)
     &        *dsqrt(one-4d0*xmass**2/medmass**2)
         endif
         if(dectotop) then 
            pw_ttb= g_dmq**2*(medmass**2-4d0*mt**2)/(12d0*pi*medmass)
     &           *dsqrt(one-4d0*mt**2/medmass**2)
         endif
         if(dectobb) then 
            pw_bb= g_dmq**2*(medmass**2-4d0*mb**2)/(12d0*pi*medmass)
     &           *dsqrt(one-4d0*mb**2/medmass**2)
         endif
      elseif(dm_mediator.eq.'scalmt') then 
         pw_qqb=0d0 
         if(dectodm) then 
         pw_xx=g_dmx**2*xmass**2*medmass/(8d0*pi*vevsq)
     &        *(one-4d0*xmass**2/medmass**2)**(3d0/2d0)
         endif
         if(dectotop) then 
            pw_ttb=g_dmq**2*mt**2*medmass/(8d0*pi*vevsq)
     &           *(one-4d0*mt**2/medmass**2)**(3d0/2d0) 
         endif
         if(dectobb) then 
            pw_bb=g_dmq**2*mb**2*medmass/(8d0*pi*vevsq)
     &           *(one-4d0*mb**2/medmass**2)**(3d0/2d0) 
         endif
         print *,"Check check",pw_bb,pw_xx,pw_ttb
      elseif(dm_mediator.eq.'psclmt') then 
         pw_qqb=0d0 
         if(dectodm) then 
         pw_xx=g_dmx**2*xmass**2*medmass/(8d0*pi*vevsq)
     &        *dsqrt(one-4d0*xmass**2/medmass**2)
         endif
         if(dectotop) then 
         pw_ttb=g_dmq**2*mt**2*medmass/(8d0*pi*vevsq)
     &        *dsqrt(one-4d0*mt**2/medmass**2)
         endif
         if(dectobb) then 
         pw_bb=g_dmq**2*mb**2*medmass/(8d0*pi*vevsq)
     &        *dsqrt(one-4d0*mb**2/medmass**2)
         endif
      endif
!------- if medwidth 

      medwidth=pw_xx+xn*nfq*pw_qqb+pw_ttb+pw_bb
      if(forceminwidth.and.(medwidth.lt.minwidth)) then 
         medwidth=minwidth
      endif

      if(first) then 
         first=.false. 
         write(6,*) '**** Minimal width calculated *****'
         write(6,33) '* Minimal width is ',medwidth,' * '
        if(dectodm) then 
            write(6,34) '* DM partial width included in decay *'
         endif
          if(dectotop) then 
            write(6,34) '* Top partial width included in decay *'
         endif
         write(6,33) '* Rescaled by      ',rescale, ' * '
         write(6,33) '* Total width  =  ',medwidth*rescale,' * '
         write(6,33) '* BR DM  =  ',pw_xx/medwidth,' * '
         write(6,*) '************************************'
      endif

 !     write(6,*) medwidth 
 !     pause
      medwidth=rescale*medwidth
 33        format(1x,a20,f8.3,a4)
 34        format(1x,a30,a4)
           return 
           end
