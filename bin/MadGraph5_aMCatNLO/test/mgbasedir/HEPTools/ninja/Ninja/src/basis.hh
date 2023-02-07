// A class for the construction of a Basis of (complex) massless
// four-momenta.


#ifndef NINJA_BASIS_HH
#define NINJA_BASIS_HH

#include <ninja/types.hh>
#include <ninja/momentum.hh>

namespace ninja{

  // A Basis has, as public members, the massless momenta e1, e2, e3,
  // e4, and the real numbers r1, r2.  It is defined form 2 real
  // momenta k1, k2 in order to satisfy the following:
  //
  //   k1 = e1 + r1*e2
  //   k2 = e2 + r2*e1,
  //
  // while e3 and e4 are orthogonal to e1 and e2 and they satisfy
  //
  //   e3.e4 = -e1.e2
  //
  // Note that, while e1 and e2 are real, e3 and e4 are complex.
  class Basis {
  public:

    // Default constructor
    Basis() : e1(), e2(), e3(), e4(), r1(0), r2(0), mpee12(0) {}

    // Copy constructor
    Basis(const Basis & e)
      : e1(e.e1), e2(e.e2), e3(e.e3), e4(e.e4),
        r1(e.r1), r2(e.r2), mpee12(e.mpee12) {}

    // Constructor from two external momenta
    Basis(const RealMomentum & k1, const RealMomentum & k2);

    // Constructor from one external momenta
    explicit Basis(const RealMomentum & k);

    // Returns the scalar poroduct e(1).e(2)
    Real mp12() const
    {
      return mpee12;
    }

    // Returns the scalar poroduct e(3).e(4)
    Real mp34() const
    {
      return -mpee12;
    }

    RealMomentum e1,e2;
    ComplexMomentum e3, e4; 
    Real r1,r2;

  private:

    Real mpee12;
  };


  // choose reference vectors for tadpoles
  Basis tadpole_basis(const RealMomentum v[], unsigned icut, unsigned n);

} // namespace ninja

#endif // NINJA_BASIS_HH
