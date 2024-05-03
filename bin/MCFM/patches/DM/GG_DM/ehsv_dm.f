      double complex function ehsva4_dm(s,t,u,s34)
C     ehsv:EqnA.8
      implicit none
      double precision s,t,u,s34
      double complex ehsvb4_dm
      ehsva4_dm=ehsvb4_dm(s,t,u,s34)+ehsvb4_dm(u,s,t,s34)
     &     +ehsvb4_dm(t,u,s,s34)
      return 
      end

      double complex function ehsva2_dm(s,t,u,s34)
C     ehsv:EqnA.9
      implicit none
      double precision s,t,u,s34
      double complex ehsvb2_dm
      ehsva2_dm=ehsvb2_dm(s,t,u,s34)+ehsvb2_dm(s,u,t,s34)
      return 
      end

      double complex function ehsvb4_dm(s,t,u,s34)
      implicit none
C     ehsv:EqnA.10
      include 'masses.f'
      double precision hmass2,s,t,u,s34
      double complex w2,w3

      hmass2=s34
c--- The Fermilab preprint has w2(s), but it makes no difference due
c---  to symmetrization in ehsva4 above 
      ehsvb4_dm=mbsq/hmass2*(-2d0/3d0
     . +(mbsq/hmass2-0.25d0)*(w2(t)-w2(hmass2)+w3(s,t,u,hmass2)))
      return 
      end

      double complex function ehsvb2_dm(s,t,u,s34)
C     ehsv:EqnA.11
      implicit none
      include 'masses.f'
      double precision hmass2,s,t,u,s34
      double complex w1,w2,w3
      hmass2=s34
      ehsvb2_dm=mbsq/hmass2**2*(s*(u-s)/(s+u)
     . +2d0*u*t*(u+2d0*s)/(s+u)**2*(w1(t)-w1(hmass2))
     . +(mbsq-0.25d0*s)
     . *(0.5d0*w2(s)+0.5d0*w2(hmass2)-w2(t)+w3(s,t,u,hmass2))
     . +s**2*(2d0*mbsq/(s+u)**2-0.5d0/(s+u))*(w2(t)-w2(hmass2))
     . +0.5d0*u*t/s*(w2(hmass2)-2d0*w2(t))
     . +0.125d0*(s-12d0*mbsq-4d0*u*t/s)*w3(t,s,u,hmass2))
      return 
      end

      double complex function ehsva5_dm(s,t,u,s34)
C     ehsv:EqnA.14
      implicit none
      include 'masses.f'
      double precision hmass2,s,t,u,s34
      double complex w1,w2
      hmass2=s34
      ehsva5_dm=mbsq/hmass2*(4d0+4d0*s/(u+t)*(w1(s)-w1(hmass2))
     . +(1d0-4d0*mbsq/(u+t))*(w2(s)-w2(hmass2)))
      return 
      end




