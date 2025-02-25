// 6photons.cc

#include <algorithm>
#include <ninja/ninja.hh>
#include <ninja/rambo.hh>

#ifdef NINJA_USE_ONELOOP
# include <ninja/avholo.hh>
#endif

#ifdef NINJA_USE_LOOPTOOLS
# include <ninja/looptools.hh>
#endif

#include "6photons_num.hh"
using namespace ninja;

namespace {

  // phase space point of arXiv:1006.0710 [hep-ph]
  void getPSP(RealMomentum k[])
  {
#  ifndef NINJA_QUADRUPLE
    k[0] = RealMomentum(0, 0.0, 0.0,0);
    k[1] = RealMomentum(0, 0.0, 0.0,0);
    k[2] = RealMomentum(0, 33.5, 15.9, 25.0);
    k[3] = RealMomentum(0,-12.5, 15.3,  0.3);
    k[4] = RealMomentum(0,-10.0,-18.0, -3.3);
    k[5] = RealMomentum(0,-11.0,-13.2,-22.0);
#  else
    k[0] = RealMomentum(0,0.0, 0.0,0);
    k[1] = RealMomentum(0, 0.0, 0.0,0);
    k[2] = RealMomentum(0, 33.5q, 15.9q, 25.0q);
    k[3] = RealMomentum(0,-12.5q, 15.3q,  0.3q);
    k[4] = RealMomentum(0,-10.0q,-18.0q, -3.3q);
    k[5] = RealMomentum(0,-11.0q,-13.2q,-22.0q);
#  endif
    Real energy = 0;
    for (int i=2; i<6; ++i) {
      k[i][0] = ninja::sqrt(-mp2(k[i]));
      energy += k[i][0];
    }
    energy /= 2;
    k[0][0] = k[0][3] = k[1][0] = -energy;
    k[1][3] = energy;
  }

}


int main()
{
  const int N_EVENTS = 1;
  const Real CM_ENERGY = 100;

  // helicities
  char helicities[] = "+--++-";

  // kinematics
  RealMomentum k[6];
  Rambo gen(CM_ENERGY*CM_ENERGY,6);

  // select different seed, for a change
  gen.setSeed(125);

  // define the integrand
  SixPhotons diagram;

  // helicity and kinematics of different diagrams
  RealMomentum k_in[6];
  char hel_in[6];
  int permutation[6] = {0,1,2,3,4,5};

  // We stop the reduction at the triangles
  Amplitude<Massless> amp;
  amp.setN(6).setRank(6).setCutStop(3);

  for (int i=0; i<N_EVENTS; ++i) {

    // set the result to zero
    amp.reset();

    // get phase space point (all incoming)
    gen.getMomenta(k);
    for (int i=2; i<6; ++i)
      k[i] *= -1;

    // ... or use point of arXiv:1006.0710 [hep-ph]
    if (false)
      getPSP(k);

    // loop over permutations
    do {

      // avoid double counting of diagrams where loop is reversed
      if (permutation[1]>permutation[5])
        continue;

      // fill momenta and helicities
      for (int i=0; i<6; ++i) {
        k_in[i] = k[permutation[i]];
        hel_in[i] = helicities[permutation[i]];
      }

      // compute the diagram (and discard rational part)
      diagram.init(k_in,hel_in);
      amp.setKinematics(diagram.getInternalMomenta());
      amp.setSMatrix(diagram.getSMatrix());
      amp.evaluate(diagram);
      amp.onlyCutConstructible();

    } while (std::next_permutation(permutation+1,permutation+6));

#ifdef NINJA_USE_LOOPTOOLS
    if (getIntegralLibrary() == & loop_tools)
      loop_tools.clearIntegralCache();
#endif

#ifdef NINJA_USE_ONELOOP
    if (getIntegralLibrary() == & avh_olo)
      avh_olo.clearIntegralCache();
#endif

  }

  // print the result
  std::cout << "Finite:      " << amp[0]
            << ",   Abs. val.:   " << abs(amp[0]) << std::endl
            << "Rational:    " << amp.getRationalPart()
            << ",   Abs. val.:   " << abs(amp.getRationalPart()) << std::endl
            << "CutConstr.:  " << amp.getCutConstructiblePart()
            << ",   Abs. val.:   " << abs(amp.getCutConstructiblePart()) << std::endl
            << "Single pole: " << amp[1] << std::endl
            << "Double pole: " << amp[2] << std::endl;
}
