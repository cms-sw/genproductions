// simple_test.cc

#include <iostream>
#include <ninja/ninja.hh>
#include <ninja/rambo.hh>
#include "mynum.hh"
using namespace ninja;

int main()
{
  // External legs of the loop
  const int N_LEGS = 4;

  // Center of mass energy
  const Real ENERGY_CM = 14;

  // Invariant s
  const Real S = ENERGY_CM*ENERGY_CM;

  // Rank of the numerator
  const int RANK = 4;

  // Declare an instance of the numerator
  Diagram num;

  // Assign numerical values to the reference vectors
  num.v1 = ninja::ComplexMomentum(1.0,1.1,1.2,1.3);
  num.v2 = ninja::ComplexMomentum(1.4,1.5,1.6,1.7);
  num.v3 = ninja::ComplexMomentum(1.8,1.9,2.0,2.1);
  num.v4 = ninja::ComplexMomentum(2.2,2.3,2.4,2.5);

  // Define external momenta
  RealMomentum k[N_LEGS];

  // Create an instance of Rambo phase-space generator
  Rambo phase_space(S,N_LEGS);

  // get a random phase-space point
  phase_space.getMomenta(k);

  // define the internal momenta of the loop
  ninja::RealMomentum pi[N_LEGS];
  pi[0] = ninja::RealMomentum(0,0,0,0);
  pi[1] = k[0];
  pi[2] = k[0]+k[1];
  pi[3] = k[3];

  // define the internal masses
  ninja::Real msq[N_LEGS] = {1.,2.,3.,4.};

  // create an amplitude object
  ninja::Amplitude<RealMasses> amp(N_LEGS,RANK,pi,msq);

#if 1 // define S-Matrix from array...

  Real s_mat[N_LEGS*N_LEGS] = {0, 0, 2*mp(k[0],k[1]), 0,
                               0, 0, 0, -2*mp(k[0],k[3]),
                               2*mp(k[0],k[1]), 0, 0, 0,
                               0, -2*mp(k[0],k[3]), 0, 0};
  amp.setSMatrix(s_mat);

#else // ...or from an SMatrix object

  SMatrix s_mat ;
  s_mat.allocate(N_LEGS); // allocate the matrix
  s_mat.fill (0); // fill the entries with zeros
  s_mat(0,2) = s_mat(2 ,0) = 2*mp(k[0],k[1]);
  s_mat(1,3) = s_mat(3 ,1) = -2*mp(k[0],k[3]);
  amp.setSMatrix(s_mat);

#endif

  // evaluate the integral
  amp.evaluate(num);

  // print the result
  std::cout << "Finite:      " << amp[0] // or amp.eps0(),  finite part
            << std::endl
            << "Single pole: " << amp[1] // or amp.epsm1(), single-pole
            << std::endl
            << "Double pole: " << amp[2] // or amp.epsm2(), double-pole
            << std::endl;

  return 0;
}
