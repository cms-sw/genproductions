// This header defines the abstract class quadninja::Numerator.  The
// numerator provided by the user should be a class inherited from
// this one and it should overload its methods `evaluate',
// `tExpansion' and if nedded `muExpansion'.


#ifndef QUADNINJA_NUM_DEFS_HH
#define QUADNINJA_NUM_DEFS_HH

#include <quadninja/types.hh>
#include <quadninja/momentum.hh>

namespace quadninja {

  typedef unsigned char PartitionInt;

}


namespace quadsamurai {

  class Numerator {

  public:

    virtual quadninja::Complex evaluate(const quadninja::ComplexMomentum & q,
                                    const quadninja::Complex & muq,
                                    int cut,
                                    const quadninja::PartitionInt part[]) = 0;

    virtual ~Numerator() {}

  };

} // namespace quadsamurai


namespace quadninja {

  class Numerator : public quadsamurai::Numerator {

  public:

    virtual Complex evaluate(const quadninja::ComplexMomentum & q,
                             const quadninja::Complex & muq,
                             int cut, const quadninja::PartitionInt part[]) = 0;

    virtual void muExpansion(const quadninja::ComplexMomentum v_perp[],
                             const quadninja::PartitionInt part[],
                             quadninja::Complex c[])
    {
      (void)(v_perp); (void)(part); (void)(c);
    }

    virtual void t3Expansion(const quadninja::ComplexMomentum & a,
                             const quadninja::ComplexMomentum & e3,
                             const quadninja::ComplexMomentum & e4,
                             const quadninja::Complex & param,
                             int mindeg,
                             int cut, const quadninja::PartitionInt part[],
                             quadninja::Complex c[]) = 0;

    virtual void t2Expansion(const quadninja::ComplexMomentum & a0,
                             const quadninja::ComplexMomentum & a1,
                             const quadninja::ComplexMomentum & e3,
                             const quadninja::ComplexMomentum & e4,
                             const quadninja::Complex param[],
                             int mindeg,
                             int cut, const quadninja::PartitionInt part[],
                             quadninja::Complex c[]) = 0;

    virtual ~Numerator() {}

  };

} // nammespace ninja

#endif // QUADNINJA_NUM_DEFS_HH
