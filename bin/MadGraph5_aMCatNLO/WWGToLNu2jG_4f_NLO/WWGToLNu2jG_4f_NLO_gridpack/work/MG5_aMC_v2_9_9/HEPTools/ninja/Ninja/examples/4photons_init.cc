// 4photons_init.cc

#include <ninja/ninja.hh>
#include <ninja/spinors.hh>
#include "4photons_num.hh"
using namespace ninja;


// Define an init method for our numerator
void FourPhotons::init(const ninja::RealMomentum k[],
                       const ninja::Complex & fermion_mass,
                       const char * helicities)
{
  k1 = k[0];
  k4 = k[3];
  p2 = k[0]+k[1];
  mf2_ = fermion_mass*fermion_mass;

  // SMatrix
  if (s_mat_.isNull())
    s_mat_.allocate(4);
  s_mat_.fill (0);
  s_mat_(0,2) = s_mat_(2 ,0) = 2*mp(k[0],k[1]);
  s_mat_(1,3) = s_mat_(3 ,1) = 2*mp(k[0],k[3]);

  // squared masses of the loop
  m2_[0] = m2_[1] = m2_[2] = m2_[3] = mf2_;

  // internal momenta of the loop
  pi_[0] = RealMomentum();
  pi_[1] = k1;
  pi_[2] = p2;
  pi_[3] = -k4;

  // spinors for external momenta
  Spinor spk[4];
  for (int i=0; i<4; ++i)
    spk[i] = Spinor(k[i]);

  // Polarization vectors
  const RealMomentum reference_mom(1.,-INVSQRT3,INVSQRT3,-INVSQRT3);
  ComplexMomentum epsilon[4];
  for (int i=0; i<4; ++i) {
    if (helicities[i] == '+')
      epsilon[i] = polarizationVectorR(reference_mom,k[i]);
    else
      epsilon[i] = polarizationVectorL(reference_mom,k[i]);
  }
   
  // Spinors for polarization vectors 
  Spinor eps1 = Spinor(epsilon[0]);
  Spinor eps2 = Spinor(epsilon[1]);
  Spinor eps3 = Spinor(epsilon[2]);
  Spinor eps4 = Spinor(epsilon[3]);

  // Spinor products and momenta appearing in the numerator (these are
  // private data members)
  eps12 = momentumFromSpinors(eps1, eps2);
  eps21 = momentumFromSpinors(eps2, eps1);
  eps23 = momentumFromSpinors(eps2, eps3);
  eps32 = momentumFromSpinors(eps3, eps2);
  eps34 = momentumFromSpinors(eps3, eps4);
  eps43 = momentumFromSpinors(eps4, eps3);
  eps41 = momentumFromSpinors(eps4, eps1);
  eps14 = momentumFromSpinors(eps1, eps4);
  spae23 = spaa(eps2,eps3);
  spbe23 = spbb(eps2,eps3);
  spae34 = spaa(eps3,eps4);
  spbe34 = spbb(eps3,eps4);
  spae41 = spaa(eps4,eps1);
  spbe41 = spbb(eps4,eps1);
  spae21 = spaa(eps2,eps1);
  spbe21 = spbb(eps2,eps1);
}
