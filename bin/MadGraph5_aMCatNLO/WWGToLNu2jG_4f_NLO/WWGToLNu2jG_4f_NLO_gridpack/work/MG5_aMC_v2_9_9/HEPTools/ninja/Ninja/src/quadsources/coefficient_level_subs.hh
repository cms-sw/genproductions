// Universal coefficient level subtractions.


#ifndef COEFFICIENT_LEVEL_SUBS_HH
#define COEFFICIENT_LEVEL_SUBS_HH

#if !defined(HAVE_CONFIG_H) || defined(NINJA_X1RANK)
# define QUADNINJA_X1RANK_SUBTRACTIONS
#endif

#include <quadninja/types.hh>
#include <quadninja/momentum.hh>

namespace quadninja {

  // normal rank
  namespace cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(quadninja::Complex * divnum,
                          const quadninja::ComplexMomentum & et3,
                          const quadninja::ComplexMomentum & et4,
                          const quadninja::Complex * c,
                          const quadninja::ComplexMomentum & eb1,
                          const quadninja::ComplexMomentum & eb2,
                          const quadninja::ComplexMomentum & eb3,
                          const quadninja::ComplexMomentum & eb4,
                          const quadninja::Complex * param,
                          const quadninja::RealMomentum & k,
                          const quadninja::Complex & f,
                          int rmn,  // rank-n
                          bool allcoeffs);

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(quadninja::Complex divnum[],
                          const quadninja::ComplexMomentum & et3,
                          const quadninja::ComplexMomentum & et4,
                          const quadninja::Complex c[],
                          const quadninja::ComplexMomentum & e3,
                          const quadninja::RealMomentum & k0,
                          const quadninja::Complex & f0,
                          const quadninja::RealMomentum & k1,
                          const quadninja::Complex & f1,
                          int rmn // rank-n
                          );

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(quadninja::Complex divnum[],
                          const quadninja::ComplexMomentum & eb2,
                          const quadninja::ComplexMomentum & eb3,
                          const quadninja::ComplexMomentum & eb4,
                          const quadninja::RealMomentum & kbol,
                          const quadninja::Complex b[],
                          const quadninja::ComplexMomentum & e3,
                          const quadninja::RealMomentum & k,
                          const quadninja::Complex & f,
                          int rmn // rank-n
                          );

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(quadninja::Complex divnum[],
                              const quadninja::ComplexMomentum & et3,
                              const quadninja::ComplexMomentum & et4,
                              const quadninja::Complex c[],
                              const quadninja::ComplexMomentum & e3,
                              const quadninja::RealMomentum & k0,
                              const quadninja::Complex & f0,
                              const quadninja::RealMomentum & k1,
                              const quadninja::Complex & f1,
                              int rmn // rank-n
                              );

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(quadninja::Complex divnum[],
                              const quadninja::ComplexMomentum & eb2,
                              const quadninja::ComplexMomentum & eb3,
                              const quadninja::ComplexMomentum & eb4,
                              const quadninja::RealMomentum & kbol,
                              const quadninja::Complex c[],
                              const quadninja::ComplexMomentum & e3,
                              const quadninja::RealMomentum & k,
                              const quadninja::Complex & f,
                              int rmn // rank-n
                              );

  }  // namespace cuts


#ifdef QUADNINJA_X1RANK_SUBTRACTIONS

  // higher rank
  namespace x1cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(quadninja::Complex * divnum,
                          const quadninja::ComplexMomentum & et3,
                          const quadninja::ComplexMomentum & et4,
                          const quadninja::Complex * c,
                          const quadninja::ComplexMomentum & eb1,
                          const quadninja::ComplexMomentum & eb2,
                          const quadninja::ComplexMomentum & eb3,
                          const quadninja::ComplexMomentum & eb4,
                          const quadninja::Complex * param,
                          const quadninja::RealMomentum & k,
                          const quadninja::Complex & f,
                          int ,  // rank-n (unused for higher rank)
                          bool allcoeffs);

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(quadninja::Complex divnum[],
                          const quadninja::ComplexMomentum & et3,
                          const quadninja::ComplexMomentum & et4,
                          const quadninja::Complex c[],
                          const quadninja::ComplexMomentum & e3,
                          const quadninja::ComplexMomentum & e4,
                          const quadninja::Complex & param,
                          const quadninja::RealMomentum & k0,
                          const quadninja::Complex & f0,
                          const quadninja::RealMomentum & k1,
                          const quadninja::Complex & f1,
                          bool);

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(quadninja::Complex divnum[],
                          const quadninja::ComplexMomentum & eb2,
                          const quadninja::ComplexMomentum & eb3,
                          const quadninja::ComplexMomentum & eb4,
                          const quadninja::RealMomentum & kbol,
                          const quadninja::Complex c[],
                          const quadninja::ComplexMomentum & e3,
                          const quadninja::ComplexMomentum & e4,
                          const quadninja::Complex & param,
                          const quadninja::RealMomentum & k,
                          const quadninja::Complex & f,
                          bool);

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(quadninja::Complex divnum[],
                              const quadninja::ComplexMomentum & et3,
                              const quadninja::ComplexMomentum & et4,
                              const quadninja::Complex c[],
                              const quadninja::ComplexMomentum & e3,
                              const quadninja::ComplexMomentum & e4,
                              const quadninja::Complex & param,
                              const quadninja::RealMomentum & k0,
                              const quadninja::Complex & f0,
                              const quadninja::RealMomentum & k1,
                              const quadninja::Complex & f1,
                              int rmn // rank-n
                              );

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(quadninja::Complex divnum[],
                              const quadninja::ComplexMomentum & eb2,
                              const quadninja::ComplexMomentum & eb3,
                              const quadninja::ComplexMomentum & eb4,
                              const quadninja::RealMomentum & kbol,
                              const quadninja::Complex c[],
                              const quadninja::ComplexMomentum & e3,
                              const quadninja::ComplexMomentum & e4,
                              const quadninja::Complex & param,
                              const quadninja::RealMomentum & k,
                              const quadninja::Complex & f,
                              int rmn // rank-n
                              );

  } // namespace x1cuts

#endif // QUADNINJA_X1RANK_SUBTRACTIONS

} // namespace quadninja


#endif // COEFFICIENT_LEVEL_SUBS_HH
