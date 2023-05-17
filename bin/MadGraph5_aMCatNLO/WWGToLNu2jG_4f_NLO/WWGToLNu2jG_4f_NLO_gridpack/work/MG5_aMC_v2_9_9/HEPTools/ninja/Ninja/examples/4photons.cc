// 4photons.cc

#include <algorithm>
#include <ninja/ninja.hh>
#include <ninja/rambo.hh>

#ifdef NINJA_USE_ONELOOP
# include <ninja/avholo.hh>
#endif

#ifdef NINJA_USE_LOOPTOOLS
# include <ninja/looptools.hh>
#endif

#include "4photons_num.hh"
using namespace ninja;

int main()
{
  const int N_EVENTS = 1;
  const Real CM_ENERGY = 14;

  // fermion mass, with width
  Complex mass = Complex(10)-I;

  // alternatively, use Rambo
  RealMomentum k[4];
  Rambo gen(CM_ENERGY*CM_ENERGY,4);

  char helicities[] = "++++";

  // define the integrand
  FourPhotons diagram;
  
  // define an amplitude object
  Amplitude<ComplexMasses> amp(4,4,
                               diagram.getInternalMomenta(),
                               diagram.getInternalMasses());

  for (int i=0; i<N_EVENTS; ++i) {

    // get phase space point (all incoming)
    gen.getMomenta(k);
    k[2] *= -1;
    k[3] *= -1;
  
#if 1
    // ... or use point of arXiv:1006.0710 [hep-ph]
    k[0] = -RealMomentum(-7.0000000000000000 ,-0.0000000000000000,
                            -0.0000000000000000, -7.0000000000000000);
    k[1] = -RealMomentum(-7.0000000000000000, -0.0000000000000000,
                         -0.0000000000000000,  7.0000000000000000);
    k[2] = -RealMomentum(6.9999999999999964,  6.1126608202785198,
                         -0.8284979592001092,  3.3089226083172685);
    k[3] = -RealMomentum(7.0000000000000027, -6.1126608202785278,
                         0.8284979592001093, -3.3089226083172703);
#endif
    
    // set the result to zero
    amp.reset();

    // evaluate 1st diagram
    diagram.init(k,mass,helicities);
    amp.setSMatrix(diagram.getSMatrix());
    amp.evaluate(diagram);

    // other diagrams are just permutations of the first

    // 2nd diagram
    std::swap(k[0],k[1]);
    diagram.init(k,mass,helicities);
    amp.setSMatrix(diagram.getSMatrix());
    amp.evaluate(diagram);

    // 3rd diagram
    //std::swap(k[0],k[1]);
    std::swap(k[0],k[3]);
    diagram.init(k,mass,helicities);
    amp.setSMatrix(diagram.getSMatrix());
    amp.evaluate(diagram);

#ifdef NINJA_USE_LOOPTOOLS
    if (getIntegralLibrary() == & loop_tools)
      loop_tools.clearIntegralCache();
#endif

#ifdef NINJA_USE_ONELOOP
    if (getIntegralLibrary() == & avh_olo)
      avh_olo.clearIntegralCache();
#endif

  }

  // print the result (only for last iteration)
  std::cout << "Finite:      " << amp[0] << std::endl
            << "Abs. val.:   " << abs(amp[0]) << std::endl
            << "Single pole: " << amp[1] << std::endl
            << "Double pole: " << amp[2] << std::endl;

  return 0;
}
