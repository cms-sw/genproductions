// This header defines the abstract class ninja::Numerator.  The
// numerator provided by the user should be a class inherited from
// this one and it should overload its methods `evaluate',
// `tExpansion' and if nedded `muExpansion'.


#ifndef NINJA_NUM_DEFS_HH
#define NINJA_NUM_DEFS_HH

#include <ninja/types.hh>
#include <ninja/momentum.hh>

namespace ninja {

  typedef unsigned char PartitionInt;

}


namespace samurai {

  class Numerator {

  public:

    virtual ninja::Complex evaluate(const ninja::ComplexMomentum & q,
                                    const ninja::Complex & muq,
                                    int cut,
                                    const ninja::PartitionInt part[]) = 0;

    virtual ~Numerator() {}

  };

} // namespace samurai


namespace ninja {

  class Numerator : public samurai::Numerator {

  public:

    virtual Complex evaluate(const ninja::ComplexMomentum & q,
                             const ninja::Complex & muq,
                             int cut, const ninja::PartitionInt part[]) = 0;

    virtual void muExpansion(const ninja::ComplexMomentum v_perp[],
                             const ninja::PartitionInt part[],
                             ninja::Complex c[])
    {
      (void)(v_perp); (void)(part); (void)(c);
    }

    virtual void t3Expansion(const ninja::ComplexMomentum & a,
                             const ninja::ComplexMomentum & e3,
                             const ninja::ComplexMomentum & e4,
                             const ninja::Complex & param,
                             int mindeg,
                             int cut, const ninja::PartitionInt part[],
                             ninja::Complex c[]) = 0;

    virtual void t2Expansion(const ninja::ComplexMomentum & a0,
                             const ninja::ComplexMomentum & a1,
                             const ninja::ComplexMomentum & e3,
                             const ninja::ComplexMomentum & e4,
                             const ninja::Complex param[],
                             int mindeg,
                             int cut, const ninja::PartitionInt part[],
                             ninja::Complex c[]) = 0;

    virtual ~Numerator() {}

  };

} // nammespace ninja

#endif // NINJA_NUM_DEFS_HH
