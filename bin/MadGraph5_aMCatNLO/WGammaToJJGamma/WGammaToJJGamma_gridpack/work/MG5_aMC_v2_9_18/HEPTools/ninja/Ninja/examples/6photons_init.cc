// 6photons_init.cc

#include <ninja/ninja.hh>
#include <ninja/spinors.hh>
#include "6photons_num.hh"
using namespace ninja;


// Define an init method for our numerator
void SixPhotons::init(const ninja::RealMomentum k[],
                      const char * helicities)
{
  k1 = k[0];
  k6 = k[5];
  p2 = k[0]+k[1];
  p3 = p2+k[2];
  p4 = p3+k[3];

  // internal momenta of the loop
  pi_[0] = RealMomentum();
  pi_[1] = k1;
  pi_[2] = p2;
  pi_[3] = p3;
  pi_[4] = p4;
  pi_[5] = -k6;

  // SMatrix
  const Real ir_threshold = 1.0e-10;
  if (s_mat_.isNull())
    s_mat_.allocate(6);
  s_mat_.fillFromKinematics(pi_,ir_threshold);

  // spinors for external momenta
  Spinor spk[6];
  for (int i=0; i<6; ++i)
    spk[i] = Spinor(k[i]);

  // Polarization vectors
  const RealMomentum reference_mom(1.,-INVSQRT3,INVSQRT3,-INVSQRT3);
  ComplexMomentum epsilon[6];
  for (int i=0; i<6; ++i) {
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
  Spinor eps5 = Spinor(epsilon[4]);
  Spinor eps6 = Spinor(epsilon[5]);

  // Spinor products and momenta appearing in the numerator (these are
  // private data members)
  eps12 = momentumFromSpinors(eps1, eps2);
  eps21 = momentumFromSpinors(eps2, eps1);
  eps23 = momentumFromSpinors(eps2, eps3);
  eps32 = momentumFromSpinors(eps3, eps2);
  eps34 = momentumFromSpinors(eps3, eps4);
  eps43 = momentumFromSpinors(eps4, eps3);
  eps45 = momentumFromSpinors(eps4, eps5);
  eps54 = momentumFromSpinors(eps5, eps4);
  eps56 = momentumFromSpinors(eps5, eps6);
  eps65 = momentumFromSpinors(eps6, eps5);
  eps61 = momentumFromSpinors(eps6, eps1);
  eps16 = momentumFromSpinors(eps1, eps6);
}
