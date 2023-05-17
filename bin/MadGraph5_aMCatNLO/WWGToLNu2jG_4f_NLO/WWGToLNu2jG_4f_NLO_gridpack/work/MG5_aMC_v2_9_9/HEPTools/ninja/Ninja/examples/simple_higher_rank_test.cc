// simple_higher_rank_test.cc

#include <iostream>
#include <ninja/ninja.hh>
#include <ninja/rambo.hh>
#include "mynumhr.hh"
using namespace ninja;

int main()
{
  // External legs of the loop
  const int N_LEGS = 5;

  // Center of mass energy
  const Real ENERGY_CM = 14;

  // Invariant s
  const Real S = ENERGY_CM*ENERGY_CM;

  // Rank of the numerator
  const int RANK = 6;

  // Declare an instance of the numerator
  Diagram num;

  // Assign numerical values to the reference vectors
  num.v0 = ComplexMomentum(0.9,0.8,0.7,0.6);
  num.v1 = ComplexMomentum(1.0,1.1,1.2,1.3);
  num.v2 = ComplexMomentum(1.4,1.5,1.6,1.7);
  num.v3 = ComplexMomentum(1.8,1.9,2.0,2.1);
  num.v4 = ComplexMomentum(2.2,2.3,2.4,2.5);
  num.v5 = ComplexMomentum(2.6,2.7,2.8,2.9);

  // Define external momenta
  RealMomentum k[N_LEGS];

  // Create an instance of Rambo phase-space generator
  Rambo phase_space(S,N_LEGS);

  // get a random phase-space point
  phase_space.getMomenta(k);

  // define the internal momenta of the loop
  RealMomentum pi[N_LEGS];
  pi[0] = RealMomentum(0,0,0,0);
  pi[1] = k[0];
  pi[2] = k[0]+k[1];
  pi[3] = k[3]+k[4];
  pi[4] = k[4];

  // define the internal masses
  Real msq[N_LEGS] = {1.,2.,3.,4.,5.};

  // create an amplitude object
  Amplitude<RealMasses> amp(N_LEGS,RANK,pi,msq);

  // S-matrix
  SMatrix s_mat(N_LEGS);
  const Real ir_threshold = 1.0e-8;
  s_mat.fillFromKinematics(pi,ir_threshold);
  amp.setSMatrix(s_mat);

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
