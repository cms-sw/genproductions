#ifndef NINJA_RAMBO_HH
#define NINJA_RAMBO_HH

#include <ninja/types.hh>
#include <ninja/momentum.hh>

namespace ninja {

  namespace detail {
    class RandomGenerator;    
    RandomGenerator * newRandomGenerator();
    void deleteRandomGenerator(RandomGenerator * gen);
  }


  class Rambo {

  public:
    enum {SUCCESS, NEWTON_METHOD_FAILED=1};

  public:

    Rambo()
      : m_(0), rnd_(detail::newRandomGenerator()), s_(0), n_(0), flag_(0) {}

    Rambo(Real energy, unsigned n, const Real masses[] = 0)
      : m_(masses), rnd_(detail::newRandomGenerator()),
        s_(energy), n_(n), flag_(SUCCESS)
    {}

    ~Rambo()
    {
      detail::deleteRandomGenerator(rnd_);
    }

    Rambo & setNParticles(unsigned n)
    {
      n_ = n;
      return *this;
    }

    Rambo & setEnergy(Real energy)
    {
      s_ = energy;
      return *this;
    }


    Rambo & setMasses(const Real * masses)
    {
      m_ = masses;
      return *this;
    }

    Rambo & setSeed(int seed);

    int getMomenta(RealMomentum vecs[], Real * weight = 0);

  private:

    void getIncomingMomenta_(RealMomentum vecs[]);
    Real rambo0_(RealMomentum u[], RealMomentum vecs[]);
    Real newton_(RealMomentum vecs[]);

  private:
    const Real * m_;
    detail::RandomGenerator * rnd_;
    Real s_;
    int n_, flag_;
  };

}

#endif // RAMBO_HH
