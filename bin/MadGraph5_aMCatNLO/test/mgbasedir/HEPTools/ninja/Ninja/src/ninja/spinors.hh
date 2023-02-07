// A simple class for Spinor-related operations.  It allows the
// construction of spinors from massless momenta, and it can also be
// used for the construction of polarization vectors.


#ifndef NINJA_SPINORS_HH
#define NINJA_SPINORS_HH

#include <ninja/types.hh>
#include <ninja/momentum.hh>

namespace ninja {

  class Spinor;

  // Angle product <i,j>
  Complex spaa(const Spinor & i, const Spinor & j);

  // Bracket product [i,j]
  Complex spbb(const Spinor & i, const Spinor & j);

  // Complex momentum from two spinors.
  // Returns 0.5 * <i \gamma^\mu j ]
  ComplexMomentum momentumFromSpinors(const Spinor & i, const Spinor & j);

  // Right handed polarization vector from two spinors.
  // Returns <r | \gamma^\mu | k ]/sqrt2/<r,k>
  ComplexMomentum polarizationVectorR(const Spinor & r, const Spinor & k);

  // Right handed polarization vector from two momenta.
  // Returns <r | \gamma^\mu | k ]/sqrt2/<r,k>
  ComplexMomentum polarizationVectorR(const RealMomentum & r,
                                      const RealMomentum & k);

  // Left handed polarization vector from two spinors.
  // Returns <k | \gamma^/mu | r ]/sqrt2/[k,r]
  ComplexMomentum polarizationVectorL(const Spinor & r, const Spinor & k);

  // Left handed polarization vector from two momenta.
  // Returns <k | \gamma^/mu | r ]/sqrt2/[k,r]
  ComplexMomentum polarizationVectorL(const RealMomentum & r,
                                      const RealMomentum & k);



  class Spinor {
  public:

    // Default constructor
    Spinor(): ap(0.), ame(0.), bp(0.), bme(0.) {}

    // Copy constructor
    Spinor(const Spinor & s): ap(s.ap), ame(s.ame), bp(s.bp), bme(s.bme) {}

    // Independent construction of angle and square brackets
    Spinor(const Spinor & a, const Spinor & b)
      : ap(a.ap), ame(a.ame), bp(b.bp), bme(b.bme) {}

    // Constructors from Momentum types
    explicit Spinor(const RealMomentum & p);
    explicit Spinor(const ComplexMomentum & p);

    friend Complex spaa(const Spinor & i, const Spinor & j);
    friend Complex spbb(const Spinor & i, const Spinor & j);
    friend ComplexMomentum momentumFromSpinors(const Spinor & i,
                                               const Spinor & j);
    friend ComplexMomentum polarizationVectorR(const Spinor & r,
                                               const Spinor & k);
    friend ComplexMomentum polarizationVectorR(const RealMomentum & r,
                                               const RealMomentum & k);
    friend ComplexMomentum polarizationVectorL(const Spinor & r,
                                               const Spinor & k);
    friend ComplexMomentum polarizationVectorL(const RealMomentum & r,
                                               const RealMomentum & k);
    friend std::ostream & operator << (std::ostream & os,
                                       const Spinor & s);

  private:
    Complex ap, ame, bp, bme;
  
  };


  // Angle product <i,j>
  inline Complex spaa(const Spinor & i, const Spinor & j)
  {
    return i.ame*j.ap-i.ap*j.ame;
  }

  // Bracket product [i,j]
  inline Complex spbb(const Spinor & i, const Spinor & j)
  {
    return -i.bme*j.bp+i.bp*j.bme;
  }

  // Complex momentum from two spinors.
  // Returns 0.5 * <i \gamma^\mu j ]
  inline ComplexMomentum
  momentumFromSpinors(const Spinor & i, const Spinor & j)
  {
    return ComplexMomentum(HALF * (i.ame*j.bme + i.ap*j.bp),
                           HALF * (i.ap*j.bme + i.ame*j.bp),
                           HALF * I * (i.ap*j.bme - i.ame*j.bp),
                           HALF * (-i.ame*j.bme + i.ap*j.bp));
  }


  inline ComplexMomentum
  polarizationVectorR(const Spinor & r, const Spinor & k)
  {
    Complex spaark = spaa(r,k);
    return ComplexMomentum
      ( INVSQRT2 *    (  r.ame*k.bme + r.ap*k.bp   )/spaark,
        INVSQRT2 * (  r.ap*k.bme + r.ame*k.bp  )/spaark,
        INVSQRT2 * I * (  r.ap*k.bme - r.ame*k.bp  )/spaark,
        INVSQRT2 * (  -r.ame*k.bme + r.ap*k.bp   )/spaark  );
  }

  inline ComplexMomentum
  polarizationVectorL(const Spinor & r, const Spinor & k)
  {
    Complex spbbkr = spbb(k,r);
    return ComplexMomentum
      ( INVSQRT2 * (  k.ame*r.bme + k.ap*r.bp   )/spbbkr,
        INVSQRT2 * (  k.ap*r.bme + k.ame*r.bp  )/spbbkr,
        INVSQRT2 * I * (  k.ap*r.bme - k.ame*r.bp  )/spbbkr,
        INVSQRT2 * (  -k.ame*r.bme + k.ap*r.bp   )/spbbkr  );
  }

  inline ComplexMomentum polarizationVectorR(const RealMomentum & r,
                                             const RealMomentum & k)
  {
    return polarizationVectorR(Spinor(r), Spinor(k));
  }

  inline ComplexMomentum polarizationVectorL(const RealMomentum & r,
                                             const RealMomentum & k)
  {
    return polarizationVectorL(Spinor(r), Spinor(k));
  }
  
  // printing (mostly for debugging)
  inline std::ostream & operator << (std::ostream & os, const Spinor & s)
  {
    os << "spinor("
       << s.ap << "," << s.ame << "," << s.bp << "," << s.bme << ")";
    return os;
  }

} // namespace ninja

#endif // NINJA_SPINORS_HH
