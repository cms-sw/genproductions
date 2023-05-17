// Definition of the constructors of the class Basis.


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ninja/momentum.hh>
#include <ninja/spinors.hh>

#include <basis.hh>
using namespace ninja;


namespace ninja {

  // returns x multiplied by the sign of the second argument
  inline Real sign(const Real & x, const Real & sign)
  {
    return sign < 0. ? -x : x;
  }

  Basis::Basis(const RealMomentum & k1, const RealMomentum & k2)
    : e1(k1), e2(k2), e3(), e4(), r1(0.), r2(0.), mpee12()
  {
    Real k1q = mp2(k1), k2q = mp2(k2), k1k2=mp(k1,k2);
    bool k1n = abs(k1q) < INFRARED_EPS;
    bool k2n = abs(k2q) < INFRARED_EPS;

    if (k1n && k2n) {
      mpee12 = k1k2;
    } else if (k1n) {
      r2 = 0.5*k2q/k1k2;
      e2 -= r2*k1;
      mpee12 = k1k2;
    } else if (k2n) {
      r1 = 0.5*k1q/k1k2;
      e1 -= r1*k2;
      mpee12 = k1k2;
    } else {
      //Real gamma = k1k2 * (ONE + sqrt(1 - (k1q/k1k2) * (k2q/k1k2)));
      Real gamma = k1k2 + sign(ONE,k1k2)*sqrt(k1k2*k1k2-k1q*k2q);
      r1 = k1q/gamma;
      r2 = k2q/gamma;
      Real den = ONE - r1*r2;
      e1 -= r1*k2;
      e1 /= den;
      e2 -= r2*k1;
      e2 /= den;
      mpee12 = k1k2/(ONE+r1*r2);
    }
    Spinor sp1(e1);
    Spinor sp2(e2);
    e3 = momentumFromSpinors(sp1,sp2);
    e4 = momentumFromSpinors(sp2,sp1);
  }

  Basis::Basis(const RealMomentum & k)
    : e1(k), e2(), e3(), e4(), r1(0.), r2(0.), mpee12()
  {
    RealMomentum k2(  sign(ONE,k[0])     , -sign(INVSQRT3,k[1]),
                     -sign(INVSQRT3,k[2]), -sign(INVSQRT3,k[3])   );
    Real kq = mp2(k), k1k2=mp(k,k2);
    bool kn = abs(kq) < INFRARED_EPS;
    if (kn) {
      e2 = k2;
      mpee12 = k1k2;
    } else {
      r1 = 0.5*kq/k1k2;
      e1 -= r1*k2;
      e2 = k2;
      //mpee12 = mp(e1,k2);
      mpee12 = k1k2;
    }
    Spinor sp1(e1);
    Spinor sp2(e2);
    e3 = momentumFromSpinors(sp1,sp2);
    e4 = momentumFromSpinors(sp2,sp1);
  }


  namespace {

    bool check_ref_vecs(const RealMomentum v[], unsigned icut, unsigned n,
                        const RealMomentum & ref1, const RealMomentum & ref2)
    {
      const Real eps = INFRARED_EPS;
      for (unsigned i=0; i<n; ++i)
        if (i != icut) {
          RealMomentum vdiff = v[i]-v[icut];
          Real norm = eucl_mp2(vdiff);
          if (norm2(mp(vdiff,ref1))/norm < eps ||
              norm2(mp(vdiff,ref2))/norm < eps)
            return false;
        }
      return true;
    }

  } // namespace

  Basis tadpole_basis(const RealMomentum v[], unsigned icut, unsigned n)
  {
    Basis e;
    RealMomentum ref1(TWO, INVSQRT3, -INVSQRT3, INVSQRT3);
    RealMomentum ref2(SQRT3, INVSQRT3, -INVSQRT2, INVSQRT2);
    e = Basis(ref1,ref2);
    if (check_ref_vecs(v,icut,n,e.e1,e.e2))
      return e;

    ref1 = RealMomentum (TWO, HALF*(1+INVSQRT3), HALF*(1-INVSQRT3), INVSQRT3);
    ref2 = RealMomentum(SQRT3, HALF*(SQRT3*INVSQRT2+INVSQRT3),
                        HALF*(1-INVSQRT2), INVSQRT2);
    e = Basis(ref1,ref2);
    if (check_ref_vecs(v,icut,n,e.e1,e.e2))
      return e;

    ref1 = RealMomentum (TWO, HALF*(1-INVSQRT3), -HALF*(1+INVSQRT3), INVSQRT3);
    ref2 = RealMomentum(SQRT3, HALF*(1-INVSQRT2),
                        -HALF*(SQRT3*INVSQRT2+INVSQRT3), INVSQRT2);
    e = Basis(ref1,ref2);
    return e;
  }


} // namespace ninja
