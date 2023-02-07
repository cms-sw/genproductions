// Universal coefficient level subtractions.


#ifndef COEFFICIENT_LEVEL_SUBS_HH
#define COEFFICIENT_LEVEL_SUBS_HH

#if !defined(HAVE_CONFIG_H) || defined(NINJA_X1RANK)
# define NINJA_X1RANK_SUBTRACTIONS
#endif

#include <ninja/types.hh>
#include <ninja/momentum.hh>

namespace ninja {

  // normal rank
  namespace cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(ninja::Complex * divnum,
                          const ninja::ComplexMomentum & et3,
                          const ninja::ComplexMomentum & et4,
                          const ninja::Complex * c,
                          const ninja::ComplexMomentum & eb1,
                          const ninja::ComplexMomentum & eb2,
                          const ninja::ComplexMomentum & eb3,
                          const ninja::ComplexMomentum & eb4,
                          const ninja::Complex * param,
                          const ninja::RealMomentum & k,
                          const ninja::Complex & f,
                          int rmn,  // rank-n
                          bool allcoeffs);

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(ninja::Complex divnum[],
                          const ninja::ComplexMomentum & et3,
                          const ninja::ComplexMomentum & et4,
                          const ninja::Complex c[],
                          const ninja::ComplexMomentum & e3,
                          const ninja::RealMomentum & k0,
                          const ninja::Complex & f0,
                          const ninja::RealMomentum & k1,
                          const ninja::Complex & f1,
                          int rmn // rank-n
                          );

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(ninja::Complex divnum[],
                          const ninja::ComplexMomentum & eb2,
                          const ninja::ComplexMomentum & eb3,
                          const ninja::ComplexMomentum & eb4,
                          const ninja::RealMomentum & kbol,
                          const ninja::Complex b[],
                          const ninja::ComplexMomentum & e3,
                          const ninja::RealMomentum & k,
                          const ninja::Complex & f,
                          int rmn // rank-n
                          );

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(ninja::Complex divnum[],
                              const ninja::ComplexMomentum & et3,
                              const ninja::ComplexMomentum & et4,
                              const ninja::Complex c[],
                              const ninja::ComplexMomentum & e3,
                              const ninja::RealMomentum & k0,
                              const ninja::Complex & f0,
                              const ninja::RealMomentum & k1,
                              const ninja::Complex & f1,
                              int rmn // rank-n
                              );

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(ninja::Complex divnum[],
                              const ninja::ComplexMomentum & eb2,
                              const ninja::ComplexMomentum & eb3,
                              const ninja::ComplexMomentum & eb4,
                              const ninja::RealMomentum & kbol,
                              const ninja::Complex c[],
                              const ninja::ComplexMomentum & e3,
                              const ninja::RealMomentum & k,
                              const ninja::Complex & f,
                              int rmn // rank-n
                              );

  }  // namespace cuts


#ifdef NINJA_X1RANK_SUBTRACTIONS

  // higher rank
  namespace x1cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(ninja::Complex * divnum,
                          const ninja::ComplexMomentum & et3,
                          const ninja::ComplexMomentum & et4,
                          const ninja::Complex * c,
                          const ninja::ComplexMomentum & eb1,
                          const ninja::ComplexMomentum & eb2,
                          const ninja::ComplexMomentum & eb3,
                          const ninja::ComplexMomentum & eb4,
                          const ninja::Complex * param,
                          const ninja::RealMomentum & k,
                          const ninja::Complex & f,
                          int ,  // rank-n (unused for higher rank)
                          bool allcoeffs);

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(ninja::Complex divnum[],
                          const ninja::ComplexMomentum & et3,
                          const ninja::ComplexMomentum & et4,
                          const ninja::Complex c[],
                          const ninja::ComplexMomentum & e3,
                          const ninja::ComplexMomentum & e4,
                          const ninja::Complex & param,
                          const ninja::RealMomentum & k0,
                          const ninja::Complex & f0,
                          const ninja::RealMomentum & k1,
                          const ninja::Complex & f1,
                          bool);

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(ninja::Complex divnum[],
                          const ninja::ComplexMomentum & eb2,
                          const ninja::ComplexMomentum & eb3,
                          const ninja::ComplexMomentum & eb4,
                          const ninja::RealMomentum & kbol,
                          const ninja::Complex c[],
                          const ninja::ComplexMomentum & e3,
                          const ninja::ComplexMomentum & e4,
                          const ninja::Complex & param,
                          const ninja::RealMomentum & k,
                          const ninja::Complex & f,
                          bool);

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(ninja::Complex divnum[],
                              const ninja::ComplexMomentum & et3,
                              const ninja::ComplexMomentum & et4,
                              const ninja::Complex c[],
                              const ninja::ComplexMomentum & e3,
                              const ninja::ComplexMomentum & e4,
                              const ninja::Complex & param,
                              const ninja::RealMomentum & k0,
                              const ninja::Complex & f0,
                              const ninja::RealMomentum & k1,
                              const ninja::Complex & f1,
                              int rmn // rank-n
                              );

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(ninja::Complex divnum[],
                              const ninja::ComplexMomentum & eb2,
                              const ninja::ComplexMomentum & eb3,
                              const ninja::ComplexMomentum & eb4,
                              const ninja::RealMomentum & kbol,
                              const ninja::Complex c[],
                              const ninja::ComplexMomentum & e3,
                              const ninja::ComplexMomentum & e4,
                              const ninja::Complex & param,
                              const ninja::RealMomentum & k,
                              const ninja::Complex & f,
                              int rmn // rank-n
                              );

  } // namespace x1cuts

#endif // NINJA_X1RANK_SUBTRACTIONS

} // namespace ninja


#endif // COEFFICIENT_LEVEL_SUBS_HH
