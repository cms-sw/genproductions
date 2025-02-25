// -*- C++ -*- implentation of rambo phase space generator.  Based of
// the Fortran90 version of GoSam.  The latter is in turn based on
//
// R. Kleiss, W.James Stirling, S.D. Ellis, "A New Monte Carlo
// Treatment of Multiparticle Phase Space at High-energies,"
// Comput.Phys.Commun. 40 (1986) 359


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ninja_scoped_array.hh>
#include <ninja/rambo.hh>

//quadninja//#define QUADNINJA_RAMBO_CC 1
#if defined(NINJA_QUADRUPLE) || defined(QUADNINJA_RAMBO_CC)
# ifndef HAVE_CXX11_FLOAT128_RANDOM
#  undef HAVE_CXX11_RANDOM
# endif
#endif

#if HAVE_CXX11_RANDOM

#include <random>

namespace ninja {
  namespace detail {

    class RandomGenerator {

    public:

      RandomGenerator() : gen_(), rnd_(0,1) {}

      void setSeed(int n)
      {
        gen_.seed(n);
      }

      Real operator() ()
      {
        return rnd_(gen_);
      }

    private:
      std::mt19937 gen_;
      std::uniform_real_distribution<ninja::Real> rnd_;
    };

  } // detail
} // namespace ninja


#else // HAVE_CXX11_RANDOM


#include <cstdlib>

namespace ninja {
  namespace detail {

    class RandomGenerator {

    public:

      RandomGenerator() {}

      void setSeed(int n)
      {
        std::srand(n);
      }

      Real operator() ()
      {
        return static_cast<Real>(std::rand())/static_cast<Real>(RAND_MAX);
      }
    };

  } // detail
} // namespace ninja


#endif  // ! HAVE_CXX11_RANDOM


namespace ninja {

  namespace detail {

    RandomGenerator * newRandomGenerator()
    {
      return new RandomGenerator;
    }

    void deleteRandomGenerator(RandomGenerator * gen)
    {
      delete gen;
    }

  } // namespace detail


  Rambo & Rambo::setSeed(int n)
  {
    (*rnd_).setSeed(n);
    return *this;
  }

  void Rambo::getIncomingMomenta_(RealMomentum vecs[])
  {
    Real m12 = 0;
    Real m22 = 0;
    if (m_) {
      m12 = m_[0]*m_[0];
      m22 = m_[1]*m_[1];
    }
    Real sqrts = 2 * sqrt(s_);
    Real A = (s_ + m12 - m22) / sqrts;
    Real B = (s_ + m22 - m12) / sqrts;

    vecs[0] = RealMomentum(A,0,0, sqrt(A*A - m12));
    vecs[1] = RealMomentum(B,0,0,-sqrt(B*B - m22));
  }

  Real Rambo::rambo0_(RealMomentum u[], RealMomentum vecs[])
  {
    const int n_out = n_-2;
    RealMomentum uu, v;
    Real cos_th, sin_th, phi;
    Real E;
    ScopedArray<RealMomentum> q(n_out);

    for (int i=0; i<n_out; ++i) {
      uu = u[i];

      cos_th = 2*uu[0]-Real(1);
      sin_th = Real(2)*sqrt(uu[0]*(Real(1)-uu[0]));
      phi = Real(2) * PI * uu[1];
      E = -log(uu[2]*uu[3]);
      q[i] = RealMomentum(Real(1), cos(phi)*sin_th,
                          sin(phi)*sin_th, cos_th);
      q[i] *= E;
      v += q[i];
    }

    Real v2 = mp2(v);
    RealMomentum b = -sqrt(Real(1)/v2) * v;  b[0] = Real();
    Real gamma = sqrt(Real(1) - mp2(b));
    Real a = Real(1)/(Real(1)+gamma);
    Real x = sqrt(s_/v2);
    Real bq;

    for (int i=0; i<n_out; ++i) {
      bq = -mp(b,q[i]);
      vecs[i] = b*(q[i][0]+a*bq) + q[i];
      vecs[i][0] = gamma*q[i][0]+bq;
      vecs[i] *= x;
    }

    Real w0 = 0.5*pow(4*PI,3-2*n_out)*pow(s_,n_out-2);
    for (int i=2; i<=n_out-2; ++i) {
      w0 = w0/i/i;
    }

    return w0/(n_out-1);
  }

  Real Rambo::newton_(RealMomentum * vecs)
  {
    const int n_out = n_-2;
    const Real eps = REAL_EPS;

    Real x = Real(0.5);
    Real sqs = sqrt(s_);
    Real fx = -sqs;

    Real x2, p0, p2, tmp, fpx;
    int limit = 1000;

    while ((abs(fx)>eps) && (limit > 0)) {
      Real m = 0;
      --limit;
      fx = -sqs;
      fpx = 0;
      x2 = x*x;
      for (int i=0; i<n_out; ++i) {
        p0 = vecs[i][0];
        p2 = p0*p0;
        if (m_)
          m = m_[i+2];
        tmp = sqrt(m*m + x2*p2);
        fx += tmp;
        fpx += p2/tmp;
      }
      fpx *= x;
      x -= fx/fpx;
    }

    if (limit == 0)
      flag_ = Rambo::NEWTON_METHOD_FAILED;
    else
      flag_ = Rambo::SUCCESS;

    return x;
  }

  int Rambo::getMomenta(RealMomentum * vecs, Real * weight)
  {
    // return status flag
    flag_ = SUCCESS;

    // Incoming particles
    getIncomingMomenta_(vecs);

    // Outgoing particles
    const int n_out = n_-2;

    if (n_out == 1) {
      vecs[2] = vecs[0]+vecs[1];
      return flag_;
    }

    ScopedArray<RealMomentum> u(n_out);

    for (int i=0; i<n_out; ++i)
      for (int j=0; j<4; ++j)
        u[i][j] = (*rnd_)();

    Real wgt = rambo0_(u.data(), vecs+2);
    Real x = newton_(vecs+2);
    Real x2 = x*x;

    for (int i=2; i<n_; ++i) {
      Real m = 0;
      if (m_)
        m = m_[i];
      vecs[i][0] = sqrt(m*m + x2*vecs[i][0]*vecs[i][0]);
      for (int j=1; j<4; ++j)
        vecs[i][j] *= x;
    }

    if (weight) {

      Real s1 = 0, s2 = 0;
      Real p1 = wgt * pow(s_,2-n_out);

      for (int i=2; i<n_; ++i) {
        Real k0 = vecs[i][0];
        Real k3 = vecs[i][1]*vecs[i][1] + vecs[i][2]*vecs[i][2]
          + vecs[i][3]*vecs[i][3];

        s1 += sqrt(k3);
        s2 += k3/k0;
        p1 *= sqrt(k3)/k0;
      }

      *weight = pow(s1,2*n_out-3) * p1 / s2;
    }

    return flag_;
  }

} // namespace ninja
