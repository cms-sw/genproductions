      subroutine vvvsxx(ga,gb,gc,sc,g1,g2,vertex)
c
c- by RF - Mar. 2006
c
c This subroutine computes an amplitude of the coupling of three gauge bosons
c and a scalar particle
c
c input:
c       complex ga(6)          : first  incoming vector   (gluon)
c       complex gb(6)          : second incoming vector   (gluon)
c       complex gc(6)          : third  incoming vector   (gluon)
c       complex sc(3)          : incoming scalar particle (Higgs)
c       real    g1             : coupling constant        (QCD)
c       complex g2(2)          : coupling constant: gc(1) scalar
c                                                   gc(2) pseudo-scalar
c
c output:
c       complex vertex         : amplitude  
c

      implicit none

c--   dimension of the current set to arbitrary length
      INTEGER DIM
      PARAMETER(DIM=18)
c      include "dimension.inc"
      double complex ga(DIM),gb(DIM),gc(DIM),sc(DIM)

      double complex dvertx, vertex, vertex1, vertex2
      double complex vab, vbc, vca, v123, v124, v134, v234
      double complex pgagb, pgagc, pgbga, pgbgc, pgcga, pgcgb
      double precision pga(0:3),pgb(0:3),pgc(0:3),pabc(4)
      double precision g1
      double complex g2(2)

      pga(0) = dble( ga(5))
      pga(1) = dble( ga(6))
      pga(2) = dimag(ga(6))
      pga(3) = dimag(ga(5))

      pgb(0) = dble( gb(5))
      pgb(1) = dble( gb(6))
      pgb(2) = dimag(gb(6))
      pgb(3) = dimag(gb(5))

      pgc(0) = dble( gc(5))
      pgc(1) = dble( gc(6))
      pgc(2) = dimag(gc(6))
      pgc(3) = dimag(gc(5))

      vertex1 = (0D0,0D0)
      vertex2 = (0D0,0D0)

      if (g2(1).NE.(0D0,0D0)) then
      vab = ga(1)*gb(1)-ga(2)*gb(2)-ga(3)*gb(3)-ga(4)*gb(4)
      vbc = gb(1)*gc(1)-gb(2)*gc(2)-gb(3)*gc(3)-gb(4)*gc(4)
      vca = gc(1)*ga(1)-gc(2)*ga(2)-gc(3)*ga(3)-gc(4)*ga(4)

      pgagb = pga(0)*gb(1) - pga(1)*gb(2) - pga(2)*gb(3) - pga(3)*gb(4)
      pgagc = pga(0)*gc(1) - pga(1)*gc(2) - pga(2)*gc(3) - pga(3)*gc(4)
      pgbga = pgb(0)*ga(1) - pgb(1)*ga(2) - pgb(2)*ga(3) - pgb(3)*ga(4)
      pgbgc = pgb(0)*gc(1) - pgb(1)*gc(2) - pgb(2)*gc(3) - pgb(3)*gc(4)
      pgcga = pgc(0)*ga(1) - pgc(1)*ga(2) - pgc(2)*ga(3) - pgc(3)*ga(4)
      pgcgb = pgc(0)*gb(1) - pgc(1)*gb(2) - pgc(2)*gb(3) - pgc(3)*gb(4)

      dvertx = vab*(pgagc-pgbgc) + vbc*(pgbga-pgcga) + vca*(pgcgb-pgagb)
      vertex1= dvertx * g2(1)
      endif

      if (g2(2).NE.(0D0,0D0)) then
      pabc(1) = pga(0) + pgb(0) + pgc(0)
      pabc(2) = pga(1) + pgb(1) + pgc(1)
      pabc(3) = pga(2) + pgb(2) + pgc(2)
      pabc(4) = pga(3) + pgb(3) + pgc(3)

      v123 =   ga(1)*gb(2)*gc(3) - ga(1)*gb(3)*gc(2) - ga(2)*gb(1)*gc(3)
     &       + ga(2)*gb(3)*gc(1) + ga(3)*gb(1)*gc(2) - ga(3)*gb(2)*gc(1)
      v124 = - ga(1)*gb(2)*gc(4) + ga(1)*gb(4)*gc(2) + ga(2)*gb(1)*gc(4)
     &       - ga(2)*gb(4)*gc(1) - ga(4)*gb(1)*gc(2) + ga(4)*gb(2)*gc(1)
      v134 =   ga(1)*gb(3)*gc(4) - ga(1)*gb(4)*gc(3) - ga(3)*gb(1)*gc(4)
     &       + ga(3)*gb(4)*gc(1) + ga(4)*gb(1)*gc(3) - ga(4)*gb(3)*gc(1)
      v234 = - ga(2)*gb(3)*gc(4) + ga(2)*gb(4)*gc(3) + ga(3)*gb(2)*gc(4)
     &       - ga(3)*gb(4)*gc(2) - ga(4)*gb(2)*gc(3) + ga(4)*gb(3)*gc(2)


      vertex2= g2(2) * (  v123*pabc(4) + v124*pabc(3)
     &                  + v134*pabc(2) + v234*pabc(1) )
      endif

      vertex = g1*sc(1) * (vertex1 + vertex2)

      return
      end
