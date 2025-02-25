#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <coefficient_level_subs.hh>
using namespace ninja;

namespace ninja {

  // normal rank
  namespace cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(Complex * divnum,
                          const ComplexMomentum & et3,
                          const ComplexMomentum & et4,
                          const Complex * c,
                          const ComplexMomentum & eb1,
                          const ComplexMomentum & eb2,
                          const ComplexMomentum & eb3,
                          const ComplexMomentum & eb4,
                          const Complex * param,
                          const RealMomentum & k,
                          const Complex & f,
                          int rmn,  // rank-n
                          bool allcoeffs)
    {

      const int jext2 = rmn;
      const int jext1 = rmn+1;
      const int jext1x1 = 2;
      const int jext0 = rmn+2 + (rmn>=0 ? 1 : 0);
      const int jext0mu2 = 4;
      const int jext0x1 = 2 + (rmn>=0 ? 3 : 0);
      const int jext0x2 = 6;

      if (allcoeffs) {

        Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
        Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_;
        Complex Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_;
        Complex Z34_;

        Z1_=Real(1)/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
        Z2_=mp(eb3,k);
        Z3_=mp(eb3,et3);
        Z4_=mp(eb4,et3);
        Z5_=mp(eb3,et4);
        Z6_=mp(eb4,et4);
        Z7_=mp(eb4,k);
        Z8_=mp(eb1,k);
        Z9_=mp(eb1,et3);
        Z10_=mp(eb1,et4);
        Z11_=mp(eb2,k);
        Z12_=mp(eb2,et3);
        Z13_=mp(eb2,et4);
        Z14_=(Z3_*Z3_);
        Z15_=Z14_*Z4_;
        Z16_=THREE*c[3];
        Z16_=Z16_*Z15_;
        Z17_=THREE*c[6];
        Z18_=(Z5_*Z5_);
        Z17_=Z17_*Z18_*Z6_;
        Z19_=Z3_*c[8];
        Z20_=c[9]*Z5_;
        Z16_=Z20_ + Z17_ + Z19_ + Z16_;
        Z16_=Z2_*Z16_;
        Z17_=c[3]*(Z3_*Z3_*Z3_);
        Z19_=c[6]*(Z5_*Z5_*Z5_);
        Z17_=Z17_ + Z19_;
        Z19_=Z7_*Z17_;
        Z16_=Z16_ - Z19_;
        Z20_=HALF*Z1_;
        Z21_=Z20_*Z2_;
        Z16_=Z16_*Z21_;
        Z22_=Z8_*Z17_;
        Z23_=HALF*f;
        Z24_=Z23_*Z17_;
        Z24_=Z22_ + Z24_;
        Z25_=Z10_*c[6];
        Z26_=c[5] + THREE*Z25_;
        Z27_=Z26_*Z18_;
        Z28_=Z9_*c[3];
        Z29_=c[2] + THREE*Z28_;
        Z30_=Z14_*Z29_;
        Z27_=Z27_ + Z30_;
        Z30_=Z2_*Z27_;
        Z30_=Z30_ - Z24_;
        Z30_=Z30_*Z21_;
        Z31_=Z17_*(Z2_*Z2_)*Z20_;
        Z32_= - Z11_*Z27_;
        Z15_=Z15_*c[3];
        Z18_=Z18_*c[6];
        Z33_=Z18_*Z6_;
        Z15_=Z15_ + Z33_;
        Z33_=THREE*Z2_;
        Z34_=Z15_*Z33_;
        Z34_=Z34_ - Z19_;
        Z34_=param[1]*Z34_;
        Z18_=Z18_*Z13_;
        Z14_=Z14_*Z12_*c[3];
        Z14_=Z18_ + Z14_;
        Z18_=Z23_ + Z8_;
        Z23_= - Z14_*Z18_;
        Z23_=Z34_ + THREE*Z23_ + Z32_;
        Z29_=Z12_*Z3_*Z29_;
        Z26_=Z5_*Z13_*Z26_;
        Z26_=Z26_ + Z29_;
        Z26_=Z2_*Z26_;
        Z23_=Z26_ + HALF*Z23_;
        Z23_=Z2_*Z23_;
        Z24_=Z11_*Z24_;
        Z23_=Z24_ + Z23_;
        Z23_=Z1_*Z23_;
        Z24_= - Z11_*Z17_;
        Z26_=Z14_*Z33_;
        Z24_=Z24_ + Z26_;
        Z21_=Z24_*Z21_;
        Z24_=param[2]*Z15_;
        Z26_=Z5_*c[6]*(Z13_*Z13_);
        Z29_=c[3]*Z3_*(Z12_*Z12_);
        Z24_=Z29_ + Z26_ + Z24_;
        Z24_=Z24_*Z33_;
        Z26_= - param[2]*Z19_;
        Z14_=Z11_*Z14_;
        Z14_=Z24_ - THREE*Z14_ + Z26_;
        Z14_=Z2_*Z14_;
        Z24_=Z17_*(Z11_*Z11_);
        Z14_=Z24_ + Z14_;
        Z14_=Z14_*Z20_;
        Z18_= - Z27_*Z18_;
        Z19_= - param[0]*Z19_;
        Z18_=Z19_ + Z18_;
        Z15_=param[0]*Z15_;
        Z19_=c[2] + Real(3.)/Real(2.)*Z28_;
        Z19_=Z9_*Z19_;
        Z19_=Z19_ + HALF*c[1];
        Z19_=Z3_*Z19_;
        Z20_=c[5] + Real(3.)/Real(2.)*Z25_;
        Z20_=Z10_*Z20_;
        Z20_=Z20_ + HALF*c[4];
        Z20_=Z5_*Z20_;
        Z15_=Real(3.)/Real(2.)*Z15_ + Z20_ + Z19_;
        Z15_=Z2_*Z15_;
        Z15_=HALF*Z18_ + Z15_;
        Z15_=Z2_*Z15_;
        Z18_=f*Z17_;
        Z18_=Real(1.)/Real(4.)*Z18_ + Z22_;
        Z18_=f*Z18_;
        Z17_=Z17_*(Z8_*Z8_);
        Z17_=Z18_ + Z17_;
        Z15_=HALF*Z17_ + Z15_;
        Z15_=Z1_*Z15_;

        if (rmn >= 0) divnum[jext2] -= Z31_;
        if (rmn >= 0) divnum[jext1x1] -= Z21_;
        if (rmn >= 0) divnum[jext0mu2] -= Z16_;
        if (rmn >= 0) divnum[jext0x2] -= Z14_;
        if (rmn >= -1) divnum[jext1] -= Z30_;
        if (rmn >= -1) divnum[jext0x1] -= Z23_;
        divnum[jext0] -= Z15_;

        /*
          Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
          Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_;
          Complex Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_;

          Z1_=mp(k,eb4);
          Z2_=mp(eb3,et3);
          Z3_=(ONE/mp(k,eb3));
          Z4_=mp(eb3,et4);
          Z5_=mp(eb4,et3);
          Z6_=mp(eb4,et4);
          Z7_=mp(k,eb1);
          Z8_=mp(eb1,et3);
          Z9_=mp(eb1,et4);
          Z10_=mp(k,eb2);
          Z11_=mp(eb2,et3);
          Z12_=mp(eb2,et4);
          Z13_=c[3]*(Z2_*Z2_*Z2_);
          Z14_=c[6]*(Z4_*Z4_*Z4_);
          Z13_=Z13_ + Z14_;
          Z14_=Z3_*Z1_*Z13_;
          Z15_=(Z2_*Z2_);
          Z16_=Z15_*c[3];
          Z17_=Z16_*Z5_;
          Z18_=(Z4_*Z4_);
          Z19_=Z18_*c[6];
          Z20_=Z19_*Z6_;
          Z17_=Z17_ + Z20_;
          Z20_= - Z14_ + THREE*Z17_;
          Z21_=c[9]*Z4_;
          Z21_=Z21_ + Z20_;
          Z21_=Z3_*Z21_;
          Z22_=Z3_*Z2_;
          Z23_=c[8]*Z22_;
          Z21_=Z23_ + Z21_;
          Z21_=ONE/TWO*Z21_;
          Z23_=THREE*c[6];
          Z24_=Z23_*Z18_;
          Z25_=Z24_*Z9_;
          Z26_=THREE*Z8_;
          Z26_=Z26_*Z16_;
          Z15_=Z15_*c[2];
          Z18_=Z18_*c[5];
          Z15_=Z15_ + Z18_ + Z25_ + Z26_;
          Z18_=Z3_*Z15_;
          Z25_=(Z3_*Z3_);
          Z26_=Z13_*Z25_;
          Z27_=ONE/TWO*f;
          Z28_=Z27_ + Z7_;
          Z29_= - Z28_*Z26_;
          Z18_=Z18_ + Z29_;
          Z18_=ONE/TWO*Z18_;
          Z29_=ONE/TWO*Z3_*Z13_;
          Z15_=Z15_*Z25_;
          Z30_=Z10_*Z15_;
          Z19_=Z12_*Z19_;
          Z31_=Z16_*Z11_;
          Z19_=Z31_ + Z19_;
          Z32_=THREE/TWO*Z25_;
          Z19_=Z32_*Z19_;
          Z13_=Z13_*(Z3_*Z3_*Z3_);
          Z32_=Z13_*Z10_;
          Z19_= - Z32_ + Z19_;
          Z19_= - Z19_*Z28_;
          Z20_=ONE/TWO*Z20_;
          Z28_=param[1]*Z20_;
          Z23_=Z23_*Z9_;
          Z23_=c[5] + Z23_;
          Z23_=Z12_*Z4_*Z23_;
          Z33_=THREE*c[3];
          Z33_=Z8_*Z33_;
          Z33_=c[2] + Z33_;
          Z33_=Z11_*Z2_*Z33_;
          Z23_=Z28_ + Z33_ + Z23_;
          Z23_=Z3_*Z23_;
          Z19_= - ONE/TWO*Z30_ + Z23_ + Z19_;
          Z23_=Z24_*Z12_;
          Z24_=THREE*Z31_ + Z23_;
          Z24_=Z3_*Z24_;
          Z26_= - Z10_*Z26_;
          Z24_=Z26_ + Z24_;
          Z24_=ONE/TWO*Z24_;Z17_=param[2]*Z17_;
          Z26_=Z4_*c[6]*(Z12_*Z12_);
          Z17_=Z26_ + Z17_;
          Z14_= - param[2]*Z14_;
          Z14_=THREE*Z17_ + Z14_;
          Z14_=Z3_*Z14_;
          Z17_= - Z25_*Z23_;
          Z17_=Z17_ + Z32_;
          Z17_=Z10_*Z17_;
          Z16_= - Z16_*Z25_*Z10_;
          Z22_=Z11_*c[3]*Z22_;
          Z16_=Z16_ + Z22_;
          Z16_=Z11_*Z16_;
          Z14_=THREE*Z16_ + Z14_ + Z17_;
          Z14_=ONE/TWO*Z14_;
          Z16_=Z13_*Z27_;
          Z16_= - Z15_ + Z16_;
          Z16_=f*Z16_;
          Z17_=Z7_ + f;
          Z13_=Z13_*Z17_;
          Z13_= - Z15_ + Z13_;
          Z13_=Z7_*Z13_;
          Z15_=param[0]*Z20_;
          Z17_=Z8_*c[2];
          Z20_=c[3]*(Z8_*Z8_);
          Z17_=THREE/TWO*Z20_ + Z17_ + ONE/TWO*c[1];
          Z17_=Z2_*Z17_;
          Z20_=Z9_*c[6];
          Z20_=c[5] + THREE/TWO*Z20_;
          Z20_=Z9_*Z20_;
          Z20_=ONE/TWO*c[4] + Z20_;
          Z20_=Z4_*Z20_;
          Z15_=Z15_ + Z17_ + Z20_;
          Z15_=Z3_*Z15_;
          Z13_=ONE/TWO*Z13_ + ONE/FOUR*Z16_ + Z15_;

          if (rmn >= 0) divnum[jext2] -= Z29_;
          if (rmn >= 0) divnum[jext1x1] -= Z24_;
          if (rmn >= 0) divnum[jext0mu2] -= Z21_;
          if (rmn >= 0) divnum[jext0x2] -= Z14_;
          if (rmn >= -1) divnum[jext1] -= Z18_;
          if (rmn >= -1) divnum[jext0x1] -= Z19_;
          divnum[jext0] -= Z13_;
        */

      } else {

        Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
        Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_;

        Z1_=mp(eb3,et3);
        Z2_=(ONE/mp(k,eb3));
        Z3_=mp(eb3,et4);
        Z4_=mp(k,eb2);
        Z5_=mp(eb2,et3);
        Z6_=mp(eb2,et4);
        Z7_=mp(k,eb1);
        Z8_=mp(eb1,et3);
        Z9_=mp(eb1,et4);
        Z10_=c[3]*(Z1_*Z1_*Z1_);
        Z11_=c[6]*(Z3_*Z3_*Z3_);
        Z10_=Z10_ + Z11_;
        Z11_=ONE/TWO*Z2_;
        Z12_=Z10_*Z11_;
        Z13_=(Z3_*Z3_);
        Z14_=Z13_*c[6];
        Z15_=Z6_*Z14_;
        Z16_=(Z1_*Z1_);
        Z17_=Z16_*c[3];
        Z18_=Z5_*Z17_;
        Z15_=Z15_ + Z18_;
        Z10_=Z10_*Z2_;
        Z18_= - Z4_*Z10_;
        Z15_=THREE*Z15_ + Z18_;
        Z15_=Z15_*Z11_;
        Z17_=Z8_*Z17_;
        Z14_=Z9_*Z14_;
        Z14_=Z17_ + Z14_;
        Z17_= - Z7_ - ONE/TWO*f;
        Z10_=Z17_*Z10_;
        Z16_=c[2]*Z16_;
        Z13_=c[5]*Z13_;
        Z10_=Z10_ + Z13_ + Z16_ + THREE*Z14_;
        Z10_=Z10_*Z11_;

        if (rmn >= 0) divnum[jext2] -= Z12_;
        if (rmn >= 0) divnum[jext1x1] -= Z15_;
        divnum[jext1] -= Z10_;

      }

    }

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(Complex divnum[],
                          const ComplexMomentum & et3,
                          const ComplexMomentum & et4,
                          const Complex c[],
                          const ComplexMomentum & e3,
                          const RealMomentum & k0, const Complex & f0,
                          const RealMomentum & k1, const Complex & f1,
                          int rmn // rank-n
                          )
    {
      Complex e3k0 = mp(e3, k0);
      Complex e3k1 = mp(e3, k1);
      Complex e3t3 = mp(e3,et3);
      Complex e3t4 = mp(e3,et4);

      Complex e3t3q = e3t3*e3t3;
      Complex e3t3c = e3t3q*e3t3;

      Complex e3t4q = e3t4*e3t4;
      Complex e3t4c = e3t4q*e3t4;

      divnum[rmn+1] -= INV8*(TWO*c[2]*e3k0*e3k1*e3t3q
                             + TWO*c[5]*e3k0*e3k1*e3t4q
                             - (c[3]*e3t3c + c[6]*e3t4c)*(e3k1*f0 + e3k0*f1)
                             )/(e3k0*e3k0*e3k1*e3k1);
    }

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(Complex divnum[],
                          const ComplexMomentum & eb2,
                          const ComplexMomentum & eb3,
                          const ComplexMomentum & eb4,
                          const RealMomentum & kbol,
                          const Complex b[],
                          const ComplexMomentum & e3,
                          const RealMomentum & k, const Complex & f,
                          int rmn // rank-n
                          )
    {
      Complex e3b2 = mp(e3,eb2);
      Complex e3b3 = mp(e3,eb3);
      Complex e3b4 = mp(e3,eb4);
      Complex e3k = mp(e3,k);
      Complex kbolb2 = mp(eb2,kbol);

      Complex abbr9 = b[2]*e3b2*e3b2 + b[7]*e3b2*e3b3
        + b[4]*e3b3*e3b3 + b[8]*e3b2*e3b4 + b[6]*e3b4*e3b4;
 
      divnum[rmn+1] -= (-(abbr9*f)
                        + TWO*e3k*(b[1]*e3b2 + b[3]*e3b3 + b[5]*e3b4
                                   + TWO*b[2]*e3b2*kbolb2 + b[7]*e3b3*kbolb2
                                   + b[8]*e3b4*kbolb2)
                        )/(FOUR*e3k*e3k);
    }

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(Complex divnum[],
                              const ComplexMomentum & et3,
                              const ComplexMomentum & et4,
                              const Complex c[],
                              const ComplexMomentum & e3,
                              const RealMomentum & k0,
                              const Complex & f0,
                              const RealMomentum & k1,
                              const Complex & f1,
                              int rmn // rank-n
                              )
    {
      Complex e3t3 = mp(e3,et3);
      Complex e3t4 = mp(e3,et4);
      Complex e3t3q = e3t3*e3t3;
      Complex e3t3c = e3t3q*e3t3;
      Complex e3t4q = e3t4*e3t4;
      Complex e3t4c = e3t4q*e3t4;

      Complex e3k0 = mp(e3, k0);
      Complex e3k1 = mp(e3, k1);

      Complex abbr5 = c[3]*e3t3c + c[6]*e3t4c;
      if(rmn>=0) divnum[rmn] -= abbr5/(FOUR*e3k0*e3k1);

      Complex e3k0q = e3k0*e3k0;
      Complex e3k1q = e3k1*e3k1;
      divnum[rmn+1] -= (TWO*c[2]*e3k0*e3k1*e3t3q + TWO*c[5]*e3k0*e3k1*e3t4q
                        - abbr5*(e3k1*f0 + e3k0*f1)
                        )/(EIGHT*e3k0q*e3k1q);
    }

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(Complex divnum[],
                              const ComplexMomentum & eb2,
                              const ComplexMomentum & eb3,
                              const ComplexMomentum & eb4,
                              const RealMomentum & kbol,
                              const Complex c[],
                              const ComplexMomentum & e3,
                              const RealMomentum & k, const Complex & f,
                              int rmn // rank-n
                              )
    {
      Complex e3b2 = mp(e3,eb2);
      Complex e3b3 = mp(e3,eb3);
      Complex e3b4 = mp(e3,eb4);
      Complex e3k = mp(e3,k);
      Complex kbolb2 = mp(eb2,kbol);

      Complex abbr9 = c[2]*e3b2*e3b2 + c[7]*e3b2*e3b3
        + c[4]*e3b3*e3b3 + c[8]*e3b2*e3b4 + c[6]*e3b4*e3b4;
      if(rmn>=0) divnum[rmn] -= abbr9/(TWO*e3k);
 
      divnum[rmn+1] -= (-(abbr9*f)
                        + TWO*e3k*(c[1]*e3b2 + c[3]*e3b3 + c[5]*e3b4
                                   + TWO*c[2]*e3b2*kbolb2 + c[7]*e3b3*kbolb2
                                   + c[8]*e3b4*kbolb2)
                        )/(FOUR*e3k*e3k);
    }

  }  // namespace cuts


#ifdef NINJA_X1RANK_SUBTRACTIONS

  // higher rank
  namespace x1cuts {

    // Subtraction of triangles from bubbles
    void correctbubcoeffs(Complex * divnum,
                          const ComplexMomentum & et3,
                          const ComplexMomentum & et4,
                          const Complex * c,
                          const ComplexMomentum & eb1,
                          const ComplexMomentum & eb2,
                          const ComplexMomentum & eb3,
                          const ComplexMomentum & eb4,
                          const Complex * param,
                          const RealMomentum & k,
                          const Complex & f,
                          int ,  // rank-n (unused for higher rank)
                          bool allcoeffs)
    {

      const int jext3 = 0;
      const int jext2 = 1;
      const int jext2x1 = 2;
      const int jext1 = 3;
      const int jext1mu2 = 4;
      const int jext1x1 = 5;
      const int jext1x2 = 6;
      const int jext0 = 7;
      const int jext0mu2 = 8;
      const int jext0x1 = 9;
      const int jext0x1mu2 = 10;
      const int jext0x2 = 11;
      const int jext0x3 = 12;

      if (allcoeffs) {

        Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_, Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_, Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_, Z34_, Z35_, Z36_, Z37_, Z38_, Z39_, Z40_, Z41_, Z42_, Z43_, Z44_, Z45_, Z46_, Z47_, Z48_, Z49_, Z50_, Z51_, Z52_, Z53_, Z54_, Z55_, Z56_, Z57_, Z58_, Z59_, Z60_, Z61_, Z62_, Z63_, Z64_, Z65_, Z66_, Z67_, Z68_, Z69_, Z70_, Z71_, Z72_, Z73_, Z74_, Z75_, Z76_, Z77_, Z78_, Z79_, Z80_, Z81_, Z82_;

        Z1_=Real(1)/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
        Z2_=mp(eb1,k);
        Z3_=mp(eb3,k);
        Z4_=mp(eb3,et3);
        Z5_=mp(eb4,et3);
        Z6_=mp(eb3,et4);
        Z7_=mp(eb4,et4);
        Z8_=mp(eb4,k);
        Z9_=mp(eb1,et3);
        Z10_=mp(eb1,et4);
        Z11_=mp(eb2,k);
        Z12_=mp(eb2,et3);
        Z13_=mp(eb2,et4);
        Z14_=Z9_*c[12];
        Z15_=Real(2)*Z14_ + Real(1.)/Real(2.)*c[3];
        Z16_=(Z4_*Z4_);
        Z17_=Z16_*Z5_;
        Z18_=Z17_*Z15_;
        Z19_=Real(1.)/Real(2.)*c[6];
        Z20_=(Z6_*Z6_);
        Z21_=Z19_*Z20_;
        Z22_=Z21_*Z7_;
        Z18_=Z18_ + Z22_;
        Z22_=Real(3)*Z3_;
        Z18_=Z18_*Z22_;
        Z23_=(Z4_*Z4_*Z4_);
        Z24_=Z15_*Z23_;
        Z25_=(Z6_*Z6_*Z6_);
        Z26_=Z19_*Z25_;
        Z24_=Z24_ + Z26_;
        Z26_=Z24_*Z8_;
        Z18_=Z18_ - Z26_;
        Z26_=(Z3_*Z3_);
        Z18_=Z18_*Z26_;
        Z27_=Z7_*c[13];
        Z28_=Z27_*Z25_;
        Z29_=Z23_*c[12];
        Z30_=Z29_*Z5_;
        Z28_=Z28_ + Z30_;
        Z30_=Real(2)*Z3_;
        Z31_=Z28_*Z30_;
        Z32_=c[12]*(Z16_*Z16_);
        Z33_=c[13]*(Z6_*Z6_*Z6_*Z6_);
        Z32_=Z32_ + Z33_;
        Z33_=Z32_*Z8_;
        Z33_=Z31_ - Z33_;
        Z34_=Z2_*Z3_;
        Z35_=Z33_*Z34_;
        Z28_=Z28_*Z3_;
        Z36_=Real(1.)/Real(2.)*Z32_;
        Z37_=Z36_*Z8_;
        Z28_=Z28_ - Z37_;
        Z38_=f*Z3_;
        Z39_=Z28_*Z38_;
        Z27_=Z27_*Z20_;
        Z40_=Z27_*Z22_;
        Z41_=Z25_*c[13];
        Z42_=Z41_*Z8_;
        Z40_=Z42_ - Z40_;
        Z42_=Real(2)*Z26_;
        Z40_=Z40_*Z42_;
        Z43_=Z40_*Z10_;
        Z35_=Z39_ - Z18_ + Z35_ + Z43_;
        Z39_=Real(1.)/Real(2.)*Z16_;
        Z43_=Z26_*Z2_;
        Z44_= - Z43_*Z39_;
        Z45_=Z26_*f;
        Z46_=Z16_*Z45_;
        Z47_=(Z3_*Z3_*Z3_);
        Z48_=Z47_*Z4_;
        Z49_=Z9_*Z48_;
        Z44_= - Real(1.)/Real(4.)*Z46_ + Z49_ + Z44_;
        Z44_=c[10]*Z44_;
        Z46_=Z20_*Z26_;
        Z49_=Real(1.)/Real(2.)*f;
        Z50_=Z49_ + Z2_;
        Z50_=Z46_*Z50_;
        Z51_=Z47_*Z6_;
        Z52_=Z10_*Z51_;
        Z52_= - Real(1.)/Real(2.)*Z50_ + Z52_;
        Z52_=c[11]*Z52_;
        Z53_=HALF*Z48_;
        Z54_=c[8]*Z53_;
        Z55_=HALF*Z51_;
        Z56_=c[9]*Z55_;
        Z44_=Z56_ + Z44_ + Z52_ + Z54_ - Z35_;
        Z44_=Z1_*Z44_;
        Z52_=Z36_*Z34_;
        Z54_=Z14_ + Real(1.)/Real(4.)*c[3];
        Z56_=Z54_*Z23_;
        Z25_=Z25_*c[6];
        Z56_=Z56_ + Real(1.)/Real(4.)*Z25_;
        Z57_= - Z56_*Z26_;
        Z58_=Z32_*Z38_;
        Z57_=Real(1.)/Real(8.)*Z58_ + Z57_ + Z52_;
        Z57_=f*Z57_;
        Z58_=Z42_*Z2_;
        Z45_=Z58_ + Z45_;
        Z58_= - Z41_*Z45_;
        Z59_=Z20_*c[6];
        Z60_=Z47_*Z59_;
        Z61_=Real(3)*Z47_;
        Z62_=Z61_*Z20_;
        Z63_=Z10_*c[13]*Z62_;
        Z58_=Z63_ + Real(3.)/Real(2.)*Z60_ + Z58_;
        Z58_=Z10_*Z58_;
        Z31_=Z37_ - Z31_;
        Z31_=Z31_*Z26_;
        Z37_= - param[0]*Z31_;
        Z60_=Z24_*Z26_;
        Z52_= - Z60_ + Z52_;
        Z52_=Z2_*Z52_;
        Z63_=(Z9_*Z9_);
        Z64_=Z63_*c[12];
        Z65_=c[3]*Z9_;
        Z64_=Real(3)*Z64_ + Real(3.)/Real(2.)*Z65_ + HALF*c[2];
        Z64_=Z64_*Z16_;
        Z66_=HALF*Z20_;
        Z67_=Z66_*c[5];
        Z64_=Z64_ + Z67_;
        Z67_=Z64_*Z47_;
        Z37_=Z58_ + Z57_ + Z52_ + Z67_ + Z37_;
        Z37_=Z1_*Z37_;
        Z52_=c[11]*Z66_;
        Z57_=c[10]*Z39_;
        Z52_=Z57_ + Z52_;
        Z52_=Z47_*Z52_;
        Z52_= - Z31_ + Z52_;
        Z52_=Z1_*Z52_;
        Z57_=Real(2)*Z10_;
        Z58_=Z41_*Z57_;
        Z58_=Z58_ + Z24_;
        Z58_=Z47_*Z58_;
        Z66_= - Z36_*Z43_;
        Z67_=Z32_*f;
        Z68_=Z26_*Z67_;
        Z58_= - Real(1.)/Real(4.)*Z68_ + Z66_ + Z58_;
        Z58_=Z1_*Z58_;
        Z66_=Z1_*Z47_*Z36_;
        Z68_=param[0]*Z3_;
        Z69_=Z68_*Z33_;
        Z64_=Z64_*Z26_;
        Z69_=Z64_ + Z69_;
        Z70_=Z32_*Z2_;
        Z71_=Real(3.)/Real(2.)*Z70_;
        Z14_=Real(4)*Z14_ + c[3];
        Z14_=Z14_*Z23_;
        Z14_=Z25_ + Z14_;
        Z14_=Z3_*Z14_;
        Z14_=Z14_ - Z71_;
        Z14_=Z2_*Z14_;
        Z23_=Z24_*Z3_;
        Z24_=Z71_ - Z23_;
        Z25_= - Real(3.)/Real(8.)*Z67_ - Z24_;
        Z25_=f*Z25_;
        Z14_=Z25_ + Z14_ - Z69_;
        Z14_=Z11_*Z14_;
        Z25_= - param[1]*Z35_;
        Z29_=Z29_*Z12_;
        Z71_=Z41_*Z13_;
        Z29_=Z29_ + Z71_;
        Z71_=Z29_*Z30_;
        Z72_=Z71_*Z2_;
        Z73_=Z16_*Z12_;
        Z54_= - Z54_*Z73_;
        Z59_=Z13_*Z59_;
        Z54_= - Real(1.)/Real(4.)*Z59_ + Z54_;
        Z59_=Real(3)*Z26_;
        Z54_=Z54_*Z59_;
        Z74_=HALF*Z38_;
        Z75_=Z29_*Z74_;
        Z54_=Z75_ + Z54_ + Z72_;
        Z54_=f*Z54_;
        Z75_=Z13_*c[13];
        Z76_=Z75_*Z51_;
        Z77_=Z46_*Z11_;
        Z78_= - c[13]*Z77_;
        Z76_=Real(2)*Z76_ + Z78_;
        Z76_=Z10_*Z76_;
        Z78_=Z46_*Z75_;
        Z79_= - f - Real(2)*Z2_;
        Z79_=Z78_*Z79_;
        Z80_=Z51_*Z13_;
        Z81_=c[6]*Z80_;
        Z76_=Z76_ + Z81_ + Z79_;
        Z46_=Z46_*c[6];
        Z79_=Real(3.)/Real(2.)*Z46_;
        Z81_=Real(4)*Z34_;
        Z82_=f*Z30_;
        Z82_=Z82_ + Z81_;
        Z82_=Z41_*Z82_;
        Z82_= - Z79_ + Z82_;
        Z82_=Z11_*Z82_;
        Z76_=Z82_ + Real(3)*Z76_;
        Z76_=Z10_*Z76_;
        Z27_=Z27_*Z13_;
        Z82_=c[12]*Z12_;
        Z17_=Z17_*Z82_;
        Z17_=Z27_ + Z17_;
        Z17_=Z17_*Z22_;
        Z22_=Z29_*Z8_;
        Z17_=Z17_ - Z22_;
        Z17_=Z17_*Z42_;
        Z22_=param[0]*Z17_;
        Z27_=Z15_*Z73_;
        Z21_=Z21_*Z13_;
        Z21_=Z27_ + Z21_;
        Z27_=Z21_*Z59_;
        Z72_= - Z27_ + Z72_;
        Z72_=Z2_*Z72_;
        Z73_=Z63_*Z82_;
        Z65_=Real(3)*Z65_ + c[2];
        Z65_=Z12_*Z65_;
        Z65_=Real(6)*Z73_ + Z65_;
        Z65_=Z4_*Z65_;
        Z73_=c[5]*Z6_*Z13_;
        Z65_=Z73_ + Z65_;
        Z65_=Z65_*Z47_;
        Z14_=Z25_ + Z76_ + Z14_ + Z54_ + Z72_ + Z65_ + Z22_;
        Z14_=Z1_*Z14_;
        Z22_=Z11_*Z3_;
        Z25_=Z22_*Z33_;
        Z17_=Z17_ - Z25_;
        Z25_=Z80_ - HALF*Z77_;
        Z25_=c[11]*Z25_;
        Z33_=Z26_*Z11_;
        Z39_= - Z33_*Z39_;
        Z54_=Z12_*Z48_;
        Z39_=Z54_ + Z39_;
        Z39_=c[10]*Z39_;
        Z25_=Z39_ + Z25_ + Z17_;
        Z25_=Z1_*Z25_;
        Z32_=Z32_*Z34_;
        Z34_=Z36_*Z38_;
        Z32_=Z34_ - Z60_ + Z32_;
        Z32_=Z11_*Z32_;
        Z34_= - Z29_*Z45_;
        Z21_=Z21_*Z61_;
        Z38_=Z75_*Z62_;
        Z39_= - Z41_*Z33_;
        Z38_=Z38_ + Z39_;
        Z38_=Z38_*Z57_;
        Z39_= - param[1]*Z31_;
        Z21_=Z39_ + Z38_ + Z32_ + Z21_ + Z34_;
        Z21_=Z1_*Z21_;
        Z32_=Real(2)*Z47_;
        Z34_=Z29_*Z32_;
        Z33_= - Z36_*Z33_;
        Z33_=Z34_ + Z33_;
        Z33_=Z1_*Z33_;
        Z24_= - Real(3.)/Real(4.)*Z67_ - Z24_;
        Z24_=Z11_*Z24_;
        Z34_=Z29_*Z81_;
        Z38_=f*Z71_;
        Z24_=Z24_ + Z38_ - Z27_ + Z34_;
        Z24_=Z11_*Z24_;
        Z27_= - param[2]*Z35_;
        Z34_=(Z12_*Z12_);
        Z16_=c[12]*Z34_*Z16_;
        Z35_=(Z13_*Z13_);
        Z38_=Z35_*c[13];
        Z20_=Z38_*Z20_;
        Z16_=Z16_ + Z20_;
        Z20_= - Z26_*Z49_;
        Z20_=Z20_ - Z43_;
        Z20_=Z16_*Z20_;
        Z26_=Z6_*Z35_*Z19_;
        Z15_=Z4_*Z34_*Z15_;
        Z15_=Z26_ + Z15_;
        Z15_=Z15_*Z47_;
        Z15_=Z15_ + Z20_;
        Z20_=param[1]*Z17_;
        Z26_=Z41_*Z22_;
        Z26_= - Real(3)*Z78_ + Z26_;
        Z26_=Z11_*Z26_;
        Z34_=Z51_*Z38_;
        Z26_=Real(3)*Z34_ + Z26_;
        Z26_=Z26_*Z57_;
        Z15_=Z27_ + Z20_ + Z26_ + Real(3)*Z15_ + Z24_;
        Z15_=Z1_*Z15_;
        Z20_=Z16_*Z61_;
        Z24_= - Z29_*Z42_;
        Z22_=Z36_*Z22_;
        Z22_=Z24_ + Z22_;
        Z22_=Z11_*Z22_;
        Z24_= - param[2]*Z31_;
        Z20_=Z24_ + Z20_ + Z22_;
        Z20_=Z1_*Z20_;
        Z16_= - Z16_*Z59_;
        Z22_= - Z11_*Z36_;
        Z22_=Z71_ + Z22_;
        Z22_=Z11_*Z22_;
        Z16_=Z16_ + Z22_;
        Z16_=Z11_*Z16_;
        Z17_=param[2]*Z17_;
        Z22_=Z6_*c[13]*(Z13_*Z13_*Z13_);
        Z24_=Z4_*c[12]*(Z12_*Z12_*Z12_);
        Z22_=Z22_ + Z24_;
        Z22_=Z22_*Z32_;
        Z16_=Z17_ + Z22_ + Z16_;
        Z16_=Z1_*Z16_;
        Z17_=Real(3.)/Real(4.)*Z70_;
        Z22_=Z3_*Z56_;
        Z22_= - Real(1.)/Real(8.)*Z67_ + Z22_ - Z17_;
        Z22_=Z22_*Z49_;
        Z24_= - Z28_*Z68_;
        Z17_=Z23_ - Z17_;
        Z17_=Z2_*Z17_;
        Z17_=Z22_ + Z17_ - HALF*Z64_ + Z24_;
        Z17_=f*Z17_;
        Z22_=Z30_*Z2_;
        Z24_=Z41_*Z22_;
        Z24_= - Z79_ + Z24_;
        Z24_=Z2_*Z24_;
        Z22_=Z74_ + Z22_;
        Z22_=Z41_*Z22_;
        Z22_= - Real(3.)/Real(4.)*Z46_ + Z22_;
        Z22_=f*Z22_;
        Z26_= - c[13]*Z50_;
        Z19_=Z19_*Z51_;
        Z19_=Z19_ + Z26_;
        Z26_=c[13]*Z51_*Z57_;
        Z19_=Real(3)*Z19_ + Z26_;
        Z19_=Z10_*Z19_;
        Z26_= - param[0]*Z40_;
        Z27_=c[5]*Z51_;
        Z19_=Z19_ + Z22_ + Z24_ + Z27_ + Z26_;
        Z19_=Z10_*Z19_;
        Z18_=param[0]*Z18_;
        Z22_= - Z2_*Z36_;
        Z22_=Z23_ + Z22_;
        Z22_=Z2_*Z22_;
        Z22_=Z22_ - Z69_;
        Z22_=Z2_*Z22_;
        Z23_=Real(2)*c[12];
        Z23_=Z63_*Z23_;
        Z23_=c[2] + Z23_;
        Z23_=Z9_*Z23_;
        Z24_=Real(3.)/Real(2.)*c[3];
        Z24_=Z63_*Z24_;
        Z23_=Z23_ + Z24_;
        Z23_=Z23_*Z48_;
        Z24_=c[4]*Z55_;
        Z26_=c[1]*Z53_;
        Z17_=Z26_ + Z19_ + Z24_ + Z17_ + Z22_ + Z23_ + Z18_;
        Z17_=Z1_*Z17_;

        divnum[jext0] -= Z17_;
        divnum[jext0mu2] -= Z44_;
        divnum[jext1] -= Z37_;
        divnum[jext1mu2] -= Z52_;
        divnum[jext2] -= Z58_;
        divnum[jext3] -= Z66_;
        divnum[jext0x1] -= Z14_;
        divnum[jext0x1mu2] -= Z25_;
        divnum[jext1x1] -= Z21_;
        divnum[jext2x1] -= Z33_;
        divnum[jext0x2] -= Z15_;
        divnum[jext1x2] -= Z20_;
        divnum[jext0x3] -= Z16_;

        /*

            Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_, Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_, Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_, Z34_, Z35_, Z36_, Z37_, Z38_, Z39_, Z40_, Z41_, Z42_, Z43_, Z44_, Z45_, Z46_, Z47_, Z48_, Z49_, Z50_, Z51_, Z52_, Z53_, Z54_, Z55_, Z56_, Z57_, Z58_, Z59_, Z60_, Z61_, Z62_, Z63_, Z64_, Z65_, Z66_, Z67_, Z68_, Z69_, Z70_, Z71_, Z72_, Z73_, Z74_, Z75_, Z76_, Z77_, Z78_, Z79_, Z80_, Z81_, Z82_, Z83_, Z84_, Z85_, Z86_, Z87_, Z88_, Z89_, Z90_, Z91_, Z92_, Z93_, Z94_, Z95_, Z96_, Z97_, Z98_, Z99_, Z100_, Z101_, Z102_, Z103_;

          Z1_=ONE/(mp(eb3,k));
          Z2_=mp(eb1,et3);
          Z3_=mp(eb3,et3);
          Z4_=mp(eb4,et3);
          Z5_=mp(eb1,et4);
          Z6_=mp(eb3,et4);
          Z7_=mp(eb4,et4);
          Z8_=ONE/((mp(eb3,k)*mp(eb3,k)));
          Z9_=mp(eb1,k);
          Z10_=ONE/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
          Z11_=mp(eb3,k);
          Z12_=mp(eb4,k);
          Z13_=ONE/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
          Z14_=ONE/(mp(eb3,et3));
          Z15_=ONE/(mp(eb3,et4));
          Z16_=ONE/((mp(eb3,et3)*mp(eb3,et3)));
          Z17_=ONE/((mp(eb3,et4)*mp(eb3,et4)));
          Z18_=mp(eb2,et3);
          Z19_=mp(eb2,et4);
          Z20_=mp(eb2,k);
          Z21_=ONE/TWO*c[6];
          Z22_=TWO*c[13];
          Z23_=Z22_*Z5_;
          Z24_=Z23_ + Z21_;
          Z25_=(Z6_*Z6_*Z6_);
          Z26_=Z25_*Z10_;
          Z24_=Z26_*Z24_;
          Z27_=TWO*c[12];
          Z28_=(Z3_*Z3_*Z3_);
          Z29_=Z27_*Z28_;
          Z30_=Z29_*Z2_;
          Z31_=Z28_*c[3];
          Z32_=ONE/TWO*Z31_;
          Z30_=Z30_ + Z32_;
          Z33_=Z30_*Z10_;
          Z24_=Z33_ + Z24_;
          Z33_=Z11_*Z24_;
          Z34_=(Z6_*Z6_);
          Z35_=(Z34_*Z34_);
          Z36_=Z35_*c[13];
          Z37_=(Z3_*Z3_);
          Z38_=(Z37_*Z37_);
          Z39_=Z38_*c[12];
          Z40_=Z36_ + Z39_;
          Z41_=Z40_*Z13_;
          Z42_=Z11_*Z41_;
          Z43_=ONE/TWO*f;
          Z44_= - Z43_ - Z9_;
          Z44_=Z42_*Z44_;
          Z33_=Z33_ + Z44_;
          Z44_=Z33_*Z12_;
          Z45_=Z23_*Z34_;
          Z46_=Z25_*c[6];
          Z47_=ONE/TWO*Z46_;
          Z48_=Z47_*Z15_;
          Z45_=Z45_ + Z48_;
          Z48_=THREE*Z1_;
          Z45_=Z45_*Z48_;
          Z49_=Z15_*Z22_*Z35_;
          Z50_=Z9_*Z8_;
          Z51_=Z49_*Z50_;
          Z52_=Z36_*Z8_;
          Z53_=f*Z15_*Z52_;
          Z45_= - Z53_ + Z45_ - Z51_;
          Z51_=Z45_*Z7_;
          Z53_=Z4_*Z1_;
          Z54_=Z53_*Z37_;
          Z54_=SIX*Z54_;
          Z55_=Z2_*c[12];
          Z56_=Z54_*Z55_;
          Z44_= - Z56_ + Z44_ - Z51_;
          Z51_=Z27_*Z38_;
          Z56_=Z50_*Z51_;
          Z57_=Z39_*Z8_;
          Z58_=Z57_*f;
          Z59_=THREE/TWO*Z31_;
          Z60_=Z59_*Z1_;
          Z56_=Z58_ + Z56_ - Z60_;
          Z58_=Z14_*Z4_;
          Z60_=Z56_*Z58_;
          Z60_=Z60_ + Z44_;
          Z61_=ONE/TWO*Z37_;
          Z62_=Z61_*Z50_;
          Z63_=f*Z8_;
          Z63_=ONE/FOUR*Z63_;
          Z64_= - Z37_*Z63_;
          Z64_=Z64_ - Z62_;
          Z64_=c[10]*Z64_;
          Z65_=Z6_*c[9];
          Z66_=Z2_*Z3_;
          Z67_=c[10]*Z66_;
          Z68_=ONE/TWO*Z3_;
          Z69_=c[8]*Z68_;
          Z65_=Z69_ + ONE/TWO*Z65_ + Z67_;
          Z65_=Z1_*Z65_;
          Z67_=ONE/TWO*Z34_;
          Z69_= - Z50_*Z67_;
          Z70_= - Z34_*Z63_;
          Z71_=Z1_*Z6_;
          Z72_=Z5_*Z71_;
          Z69_=Z70_ + Z72_ + Z69_;
          Z69_=c[11]*Z69_;
          Z64_=Z69_ + Z65_ + Z64_ - Z60_;
          Z38_=Z16_*Z55_*Z38_;
          Z65_=Z61_*c[3];
          Z69_=Z38_ + Z65_;
          Z70_=THREE*Z2_;
          Z69_=Z69_*Z70_;
          Z70_=Z36_*Z17_;
          Z72_=THREE*Z70_;
          Z73_=(Z5_*Z5_);
          Z74_=Z72_*Z73_;
          Z75_=Z67_*c[5];
          Z76_=Z34_*c[6];
          Z77_=Z76_*Z5_;
          Z69_=Z69_ + THREE/TWO*Z77_ + Z75_ + Z74_;
          Z74_=c[2]*Z61_;
          Z74_=Z74_ + Z69_;
          Z74_=Z1_*Z74_;
          Z75_=Z55_*Z28_;
          Z77_=Z75_ + ONE/FOUR*Z31_;
          Z78_=Z25_*c[13];
          Z79_= - Z5_*Z78_;
          Z79_=Z79_ - ONE/FOUR*Z46_ - Z77_;
          Z79_=Z8_*Z79_;
          Z80_=c[13]*Z10_;
          Z81_=Z80_*Z35_;
          Z82_=Z39_*Z10_;
          Z81_=Z81_ + Z82_;
          Z82_=ONE/TWO*Z81_;
          Z83_=Z82_*Z9_;
          Z84_=ONE/EIGHT*f;
          Z85_=Z81_*Z84_;
          Z79_=Z85_ + Z79_ + Z83_;
          Z79_=f*Z79_;
          Z49_=Z1_*Z49_*Z7_;
          Z85_=Z51_*Z14_;
          Z53_=Z85_*Z53_;
          Z49_=Z49_ + Z53_;
          Z53_=Z12_*Z11_*Z81_;
          Z86_=ONE/TWO*Z53_ - Z49_;
          Z87_= - param[0]*Z86_;
          Z88_=Z22_*Z25_;
          Z89_=Z88_*Z5_;
          Z30_=Z30_ + Z89_ + Z47_;
          Z89_= - Z8_*Z30_;
          Z83_=Z89_ + Z83_;
          Z83_=Z9_*Z83_;
          Z74_=Z79_ + Z83_ + Z87_ + Z74_;
          Z79_=Z37_*Z1_;
          Z83_=c[10]*Z79_;
          Z53_=Z83_ - Z53_;
          Z83_=c[11]*Z1_*Z67_;
          Z49_=Z83_ + ONE/TWO*Z53_ + Z49_;
          Z53_=Z1_*Z30_;
          Z83_=ONE/TWO*Z40_;
          Z87_= - Z50_*Z83_;
          Z40_= - Z40_*Z63_;
          Z40_=Z40_ + Z53_ + Z87_;
          Z53_=Z1_*Z83_;
          Z63_=Z41_*Z20_;
          Z87_=THREE/FOUR*Z63_;
          Z89_=Z18_*c[12];
          Z28_=Z89_*Z28_;
          Z90_=Z28_*Z10_;
          Z25_=Z80_*Z25_;
          Z80_=Z19_*Z25_;
          Z80_=Z80_ + Z90_ - Z87_;
          Z80_=Z80_*Z43_;
          Z91_=THREE/TWO*Z63_;
          Z29_=Z29_*Z18_;
          Z92_=Z29_*Z10_;
          Z93_=Z91_ - Z92_;
          Z93_=Z93_*Z9_;
          Z94_=Z24_*Z20_;
          Z95_=Z26_*Z22_;
          Z96_=Z95_*Z9_;
          Z97_= - Z5_*Z70_;
          Z76_= - ONE/FOUR*Z76_ + Z97_;
          Z97_=THREE*Z8_;
          Z76_=Z76_*Z97_;
          Z76_=Z76_ + Z96_;
          Z76_=Z19_*Z76_;
          Z37_=ONE/FOUR*Z37_;
          Z98_= - c[3]*Z37_;
          Z38_=Z98_ - Z38_;
          Z98_=Z97_*Z18_;
          Z38_=Z38_*Z98_;
          Z38_=Z80_ + Z76_ - Z93_ + Z38_ + Z94_;
          Z38_=f*Z38_;
          Z44_= - param[1]*Z44_;
          Z76_=Z20_*Z8_;
          Z80_= - Z69_*Z76_;
          Z99_=Z25_*Z5_;
          Z75_=Z31_ + FOUR*Z75_;
          Z75_=Z10_*Z75_;
          Z26_=Z26_*c[6];
          Z75_=FOUR*Z99_ + Z75_ + Z26_;
          Z75_=Z20_*Z75_;
          Z100_=Z16_*Z51_*Z2_;
          Z65_=Z100_ + Z65_;
          Z98_=Z65_*Z98_;
          Z75_= - Z93_ - Z98_ + Z75_;
          Z75_=Z9_*Z75_;
          Z36_=Z76_*Z36_*Z15_;
          Z93_=Z48_*Z19_;
          Z100_=c[13]*Z93_*Z34_;
          Z36_=Z36_ - Z100_;
          Z100_=TWO*Z7_;
          Z100_=Z36_*Z100_;
          Z101_=Z95_*Z19_;
          Z101_=Z92_ + Z101_;
          Z101_=Z11_*Z101_;
          Z42_=Z42_*Z20_;
          Z42_= - Z42_ + Z101_;
          Z101_=Z42_*Z12_;
          Z54_=Z54_*Z89_;
          Z54_=Z100_ + Z101_ - Z54_;
          Z100_= - param[0]*Z54_;
          Z35_=Z17_*Z23_*Z35_;
          Z21_=Z21_*Z34_;
          Z21_=Z35_ + Z21_;
          Z34_= - Z21_*Z97_;
          Z34_=Z34_ + Z96_;
          Z34_=Z9_*Z34_;
          Z35_=c[13]*Z73_;
          Z35_=SIX*Z35_ + c[5];
          Z35_=Z6_*Z35_;
          Z46_=Z46_*Z17_;
          Z96_=Z5_*Z46_;
          Z35_=THREE*Z96_ + Z35_;
          Z35_=Z1_*Z35_;
          Z34_=Z35_ + Z34_;
          Z34_=Z19_*Z34_;
          Z35_= - Z4_*param[1]*Z56_;
          Z96_=Z76_*Z4_;
          Z97_= - param[0]*Z51_*Z96_;
          Z35_=Z35_ + Z97_;
          Z35_=Z14_*Z35_;
          Z97_=Z66_*Z27_;
          Z31_=Z16_*Z31_;
          Z31_=Z31_ + Z97_;
          Z101_=Z48_*Z18_;
          Z31_=Z2_*Z31_*Z101_;
          Z61_=Z61_*Z76_;
          Z102_=Z1_*Z3_;
          Z103_=Z102_*Z18_;
          Z61_=Z61_ - Z103_;
          Z103_= - c[2]*Z61_;
          Z31_=Z35_ + Z100_ + Z38_ + Z103_ + Z34_ + Z75_ + Z31_ + Z80_ + Z44_;
          Z34_=Z96_*Z85_;
          Z34_=Z54_ + Z34_;
          Z35_= - c[10]*Z61_;
          Z38_= - Z76_*Z67_;
          Z44_=Z71_*Z19_;
          Z38_=Z38_ + Z44_;
          Z38_=c[11]*Z38_;
          Z35_=Z38_ + Z35_ - Z34_;
          Z38_= - param[1]*Z86_;
          Z30_= - Z30_*Z76_;
          Z54_=Z29_*Z8_;
          Z61_=Z20_*Z81_;
          Z61_= - Z54_ + Z61_;
          Z61_=Z9_*Z61_;
          Z67_=Z21_*Z48_;
          Z75_= - Z88_*Z50_;
          Z67_=Z67_ + Z75_;
          Z67_=Z19_*Z67_;
          Z75_=Z82_*Z20_;
          Z78_= - Z19_*Z78_;
          Z28_=Z78_ - Z28_;
          Z28_=Z8_*Z28_;
          Z28_=Z75_ + Z28_;
          Z28_=f*Z28_;
          Z65_=Z65_*Z101_;
          Z28_=Z28_ + Z67_ + Z61_ + Z65_ + Z30_ + Z38_;
          Z30_=Z19_*Z88_;
          Z29_=Z30_ + Z29_;
          Z29_=Z1_*Z29_;
          Z30_= - Z76_*Z83_;
          Z29_=Z30_ + Z29_;
          Z21_=Z21_*Z76_;
          Z30_=Z17_*Z47_;
          Z23_=Z6_*Z23_;
          Z23_=Z30_ + Z23_;
          Z23_=Z1_*Z23_;
          Z30_= - Z50_*Z70_;
          Z23_=Z23_ + Z30_;
          Z23_=Z19_*Z23_;
          Z21_=Z21_ - Z23_;
          Z23_=Z9_*Z20_*Z25_;
          Z21_=FOUR*Z23_ - THREE*Z21_;
          Z21_=Z19_*Z21_;
          Z23_=Z92_ - Z87_;
          Z23_=Z20_*Z23_;
          Z25_=(Z18_*Z18_);
          Z30_=Z25_*Z16_;
          Z38_=Z30_*Z57_;
          Z47_=Z20_*Z95_;
          Z50_=Z19_*Z17_*Z52_;
          Z47_=Z47_ - THREE/TWO*Z50_;
          Z47_=Z19_*Z47_;
          Z23_=Z47_ - THREE/TWO*Z38_ + Z23_;
          Z23_=f*Z23_;
          Z33_= - param[2]*Z33_;
          Z42_= - param[1]*Z42_;
          Z33_=Z42_ + Z33_;
          Z33_=Z12_*Z33_;
          Z42_=param[2]*Z45_;
          Z36_= - param[1]*Z36_;
          Z36_=TWO*Z36_ + Z42_;
          Z36_=Z7_*Z36_;
          Z32_=Z16_*Z32_;
          Z32_=Z32_ + Z97_;
          Z25_=Z32_*Z25_*Z48_;
          Z32_= - Z98_ + Z94_;
          Z32_=Z20_*Z32_;
          Z42_=FOUR*Z90_ - Z91_;
          Z42_=Z20_*Z42_;
          Z38_=THREE*Z38_;
          Z42_= - Z38_ + Z42_;
          Z42_=Z9_*Z42_;
          Z45_= - param[2]*Z56_;
          Z47_= - param[1]*Z76_*Z51_;
          Z45_=Z47_ + Z45_;
          Z45_=Z45_*Z58_;
          Z47_=param[1]*Z89_;
          Z50_=param[2]*Z55_;
          Z47_=Z47_ + Z50_;
          Z47_=Z4_*Z79_*Z47_;
          Z21_=Z45_ + SIX*Z47_ + Z36_ + Z33_ + Z23_ + Z21_ + Z42_ + Z25_ + Z32_;
          Z23_= - param[2]*Z86_;
          Z25_=Z39_*Z48_*Z30_;
          Z30_= - Z54_ + Z75_;
          Z30_=Z20_*Z30_;
          Z32_= - Z88_*Z76_;
          Z33_=Z70_*Z93_;
          Z32_=Z32_ + Z33_;
          Z32_=Z19_*Z32_;
          Z23_=Z32_ + Z25_ + Z30_ + Z23_;
          Z25_= - param[2]*Z34_;
          Z30_=Z92_ - ONE/TWO*Z63_;
          Z30_=Z20_*Z30_;
          Z30_= - Z38_ + Z30_;
          Z30_=Z20_*Z30_;
          Z32_= - Z76_*Z72_;
          Z33_=Z22_*Z44_;
          Z32_=Z32_ + Z33_;
          Z32_=Z19_*Z32_;
          Z33_=(Z20_*Z20_)*Z95_;
          Z32_=Z33_ + Z32_;
          Z32_=Z19_*Z32_;
          Z27_=(Z18_*Z18_*Z18_)*Z27_*Z102_;
          Z25_=Z32_ + Z27_ + Z30_ + Z25_;
          Z27_=Z41_*Z9_;
          Z30_=THREE/FOUR*Z27_;
          Z32_=Z10_*Z77_;
          Z33_= - Z41_*Z84_;
          Z26_=Z33_ - Z30_ + Z99_ + Z32_ + ONE/FOUR*Z26_;
          Z26_=Z26_*Z43_;
          Z32_=Z69_*Z8_;
          Z30_= - Z30_ + Z24_;
          Z30_=Z9_*Z30_;
          Z33_= - c[2]*Z8_*Z37_;
          Z26_=Z26_ + Z33_ - ONE/TWO*Z32_ + Z30_;
          Z26_=f*Z26_;
          Z30_= - param[0]*Z60_;
          Z33_=Z5_*c[5];
          Z22_=(Z5_*Z5_*Z5_)*Z22_;
          Z22_=Z22_ + Z33_;
          Z22_=Z6_*Z22_;
          Z33_=Z16_*Z59_;
          Z33_=Z33_ + Z97_;
          Z33_=Z33_*(Z2_*Z2_);
          Z34_=Z73_*Z46_;
          Z36_=c[1]*Z68_;
          Z22_=THREE/TWO*Z34_ + Z33_ + Z36_ + Z22_;
          Z22_=Z1_*Z22_;
          Z24_= - ONE/TWO*Z27_ + Z24_;
          Z24_=Z9_*Z24_;
          Z24_= - Z32_ + Z24_;
          Z24_=Z9_*Z24_;
          Z27_=Z1_*Z66_;
          Z27_=Z27_ - Z62_;
          Z27_=c[2]*Z27_;
          Z32_=c[4]*Z71_;
          Z22_=Z26_ + Z27_ + Z24_ + Z22_ + ONE/TWO*Z32_ + Z30_;

          divnum[jext0] -= Z22_;
          divnum[jext0mu2] -= Z64_;
          divnum[jext1] -= Z74_;
          divnum[jext1mu2] -= Z49_;
          divnum[jext2] -= Z40_;
          divnum[jext3] -= Z53_;
          divnum[jext0x1] -= Z31_;
          divnum[jext0x1mu2] -= Z35_;
          divnum[jext1x1] -= Z28_;
          divnum[jext2x1] -= Z29_;
          divnum[jext3] -= Z53_;
          divnum[jext0x2] -= Z21_;
          divnum[jext1x2] -= Z23_;
          divnum[jext0x3] -= Z25_;
        */
      } else {

      
        Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_, Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_, Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_, Z34_, Z35_;

        Z1_=Real(1)/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
        Z2_=mp(eb1,k);
        Z3_=mp(eb3,k);
        Z4_=mp(eb3,et3);
        Z5_=mp(eb3,et4);
        Z6_=mp(eb1,et3);
        Z7_=mp(eb1,et4);
        Z8_=mp(eb4,et3);
        Z9_=mp(eb4,et4);
        Z10_=mp(eb4,k);
        Z11_=mp(eb2,k);
        Z12_=mp(eb2,et3);
        Z13_=mp(eb2,et4);
        Z14_=(Z5_*Z5_);
        Z15_=c[13]*Z1_;
        Z16_=Z14_*Z15_;
        Z17_=Z7_*Z16_;
        Z18_=Real(1.)/Real(2.)*Z1_;
        Z19_=Z14_*Z18_;
        Z20_=c[6]*Z19_;
        Z20_=Z20_ + Z17_;
        Z20_=Z7_*Z20_;
        Z21_=(Z4_*Z4_);
        Z22_=Z21_*Z18_;
        Z23_=Z6_*c[3]*Z22_;
        Z24_=c[12]*Z1_;
        Z25_=Z21_*Z24_;
        Z26_=(Z6_*Z6_)*Z25_;
        Z20_=Z20_ + Z23_ + Z26_;
        Z23_=(Z4_*Z4_*Z4_);
        Z26_=Z23_*Z24_;
        Z27_=Z26_*Z8_;
        Z28_=(Z5_*Z5_*Z5_);
        Z29_=Z28_*Z15_;
        Z30_=Z29_*Z9_;
        Z27_=Z27_ + Z30_;
        Z30_=Real(2)*Z27_;
        Z31_=param[0]*Z30_;
        Z32_=c[2]*Z22_;
        Z20_=Z32_ + Real(3)*Z20_ + Z31_;
        Z20_=Z3_*Z20_;
        Z31_=Z26_*Z6_;
        Z32_=Z29_*Z7_;
        Z31_=Z31_ + Z32_;
        Z28_=Z28_*c[6];
        Z23_=Z23_*c[3];
        Z23_=Z28_ + Z23_;
        Z28_=Z1_*Z23_;
        Z28_= - Real(1.)/Real(4.)*Z28_ - Z31_;
        Z28_=f*Z28_;
        Z15_=Z15_*(Z14_*Z14_);
        Z32_=Z24_*(Z4_*Z4_*Z4_*Z4_);
        Z15_=Z15_ + Z32_;
        Z32_=Z10_*Z15_;
        Z32_=Real(1.)/Real(2.)*Z32_;
        Z33_= - param[0]*Z32_;
        Z20_=Z20_ + Z33_ + Z28_;
        Z20_=Z3_*Z20_;
        Z23_=Z23_*Z18_;
        Z23_=Z23_ + Real(2)*Z31_;
        Z28_=Z23_*Z3_;
        Z31_=Real(1.)/Real(2.)*Z15_;
        Z33_=Z31_*Z2_;
        Z28_=Z28_ - Z33_;
        Z33_=Z31_*f;
        Z34_=Z33_ - Z28_;
        Z34_=Z2_*Z34_;
        Z35_=Z15_*(f*f);
        Z20_=Z34_ + Real(1.)/Real(8.)*Z35_ + Z20_;
        Z20_=Z3_*Z20_;
        Z34_=(Z3_*Z3_*Z3_);
        Z35_=c[5]*Z34_*Z19_;
        Z20_=Z35_ + Z20_;
        Z19_=c[11]*Z19_;
        Z22_=c[10]*Z22_;
        Z19_=Z22_ + Z19_ + Z30_;
        Z19_=Z3_*Z19_;
        Z19_= - Z32_ + Z19_;
        Z22_=(Z3_*Z3_);
        Z19_=Z19_*Z22_;
        Z35_=Z15_*f;
        Z28_= - Real(1.)/Real(4.)*Z35_ + Z28_;
        Z28_=Z22_*Z28_;
        Z34_=Z34_*Z31_;
        Z21_=Z21_*Z12_;
        Z24_=Z21_*Z24_*Z6_;
        Z17_=Z13_*Z17_;
        Z17_=Z24_ + Z17_;
        Z14_=c[6]*Z14_*Z13_;
        Z21_=c[3]*Z21_;
        Z14_=Z14_ + Z21_;
        Z14_=Z14_*Z18_;
        Z14_=Z14_ + Real(2)*Z17_;
        Z17_=param[1]*Z30_;
        Z14_=Real(3)*Z14_ + Z17_;
        Z14_=Z3_*Z14_;
        Z17_= - Z11_*Z23_;
        Z18_=Z26_*Z12_;
        Z21_=Z29_*Z13_;
        Z18_=Z18_ + Z21_;
        Z21_= - f*Z18_;
        Z23_= - param[1]*Z32_;
        Z14_=Z14_ + Z21_ + Z17_ + Z23_;
        Z14_=Z3_*Z14_;
        Z17_=Real(2)*Z18_;
        Z18_=Z17_*Z3_;
        Z15_=Z15_*Z11_;
        Z15_=Z15_ - Z18_;
        Z15_=Z2_*Z15_;
        Z21_=Z11_*Z33_;
        Z14_=Z15_ + Z21_ + Z14_;
        Z14_=Z3_*Z14_;
        Z15_= - Z11_*Z31_;
        Z15_=Z15_ + Z18_;
        Z15_=Z15_*Z22_;
        Z17_= - Z11_*Z17_;
        Z18_=(Z12_*Z12_)*Z25_;
        Z16_=(Z13_*Z13_)*Z16_;
        Z16_=Z18_ + Z16_;
        Z16_=Z3_*Z16_;
        Z16_=Z17_ + Real(3)*Z16_;
        Z16_=Z3_*Z16_;
        Z17_=(Z11_*Z11_)*Z31_;
        Z16_=Z17_ + Z16_;
        Z16_=Z3_*Z16_;
        Z17_=Real(2)*Z3_;
        Z17_=Z27_*Z17_;
        Z17_= - Z32_ + Z17_;
        Z17_=param[2]*Z17_*Z22_;
        Z16_=Z16_ + Z17_;

        divnum[jext1] -= Z20_;
        divnum[jext1mu2] -= Z19_;
        divnum[jext2] -= Z28_;
        divnum[jext3] -= Z34_;
        divnum[jext1x1] -= Z14_;
        divnum[jext2x1] -= Z15_;
        divnum[jext1x2] -= Z16_;
        /*

          Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_, Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_, Z23_, Z24_, Z25_, Z26_, Z27_, Z28_, Z29_, Z30_, Z31_, Z32_, Z33_, Z34_, Z35_, Z36_, Z37_, Z38_, Z39_, Z40_, Z41_, Z42_, Z43_, Z44_, Z45_;

          Z1_=ONE/(mp(eb3,k));
          Z2_=mp(eb3,et3);
          Z3_=mp(eb3,et4);
          Z4_=ONE/((mp(eb3,k)*mp(eb3,k)*mp(eb3,k)));
          Z5_=mp(eb3,k);
          Z6_=mp(eb4,k);
          Z7_=ONE/(mp(eb3,et3));
          Z8_=mp(eb4,et3);
          Z9_=ONE/(mp(eb3,et4));
          Z10_=mp(eb4,et4);
          Z11_=mp(eb1,et3);
          Z12_=mp(eb1,et4);
          Z13_=ONE/((mp(eb3,k)*mp(eb3,k)));
          Z14_=mp(eb1,k);
          Z15_=mp(eb2,et3);
          Z16_=mp(eb2,et4);
          Z17_=mp(eb2,k);
          Z18_=ONE/((mp(eb3,et3)*mp(eb3,et3)));
          Z19_=ONE/((mp(eb3,et4)*mp(eb3,et4)));
          Z20_=Z1_*c[13];
          Z21_=(Z3_*Z3_*Z3_*Z3_);
          Z22_=Z20_*Z21_;
          Z23_=TWO*Z22_;
          Z24_=Z10_*Z23_*Z9_;
          Z25_=c[12]*Z1_;
          Z26_=(Z2_*Z2_*Z2_*Z2_);
          Z27_=Z25_*Z26_;
          Z28_=TWO*Z27_;
          Z29_=Z8_*Z28_*Z7_;
          Z24_=Z24_ + Z29_;
          Z29_=Z26_*c[12];
          Z30_=Z21_*c[13];
          Z29_=Z29_ + Z30_;
          Z30_=Z6_*Z29_;
          Z31_= - Z5_*Z4_*Z30_;
          Z32_=Z1_*(Z3_*Z3_);
          Z33_=c[11]*Z32_;
          Z34_=Z1_*(Z2_*Z2_);
          Z35_=c[10]*Z34_;
          Z31_=Z31_ + Z33_ + Z35_;
          Z31_=ONE/TWO*Z31_ + Z24_;
          Z33_=c[12]*Z13_;
          Z26_=Z33_*Z26_;
          Z35_=Z13_*c[13];
          Z21_=Z35_*Z21_;
          Z21_=Z26_ + Z21_;
          Z26_= - ONE/FOUR*f - ONE/TWO*Z14_;
          Z26_=Z21_*Z26_;
          Z36_=(Z3_*Z3_*Z3_);
          Z20_=Z20_*Z36_;
          Z37_=Z12_*Z20_;
          Z38_=(Z2_*Z2_*Z2_);
          Z25_=Z25_*Z38_;
          Z39_=Z11_*Z25_;
          Z37_=Z37_ + Z39_;
          Z39_=Z38_*c[3];
          Z40_=Z36_*c[6];
          Z39_=Z39_ + Z40_;
          Z40_=ONE/TWO*Z39_;
          Z40_=Z1_*Z40_;
          Z26_=TWO*Z37_ + Z40_ + Z26_;
          Z37_=Z22_ + Z27_;
          Z37_=ONE/TWO*Z37_;
          Z35_=Z35_*Z36_;
          Z36_=Z35_*Z12_;
          Z33_=Z33_*Z38_;
          Z38_=Z33_*Z11_;
          Z36_=Z36_ + Z38_;
          Z38_=ONE/TWO*Z13_;
          Z38_=Z38_*Z39_;
          Z38_=TWO*Z36_ + Z38_;
          Z40_=Z29_*Z4_;
          Z41_=Z14_*Z40_;
          Z42_=ONE/TWO*Z4_;
          Z29_=Z29_*Z42_;
          Z43_=f*Z29_;
          Z41_=Z43_ + Z41_ - Z38_;
          Z41_=Z17_*Z41_;
          Z30_=Z30_*Z42_*Z5_;
          Z24_=Z30_ - Z24_;
          Z30_= - param[1]*Z24_;
          Z28_=Z11_*Z18_*Z28_;
          Z42_=Z34_*c[3];
          Z28_=Z28_ + ONE/TWO*Z42_;
          Z43_=TWO*Z14_;
          Z44_= - Z33_*Z43_;
          Z28_=THREE*Z28_ + Z44_;
          Z28_=Z15_*Z28_;
          Z33_=Z33_*Z15_;
          Z35_=Z35_*Z16_;
          Z33_=Z33_ + Z35_;
          Z44_= - f*Z33_;
          Z23_=Z12_*Z19_*Z23_;
          Z45_=Z32_*c[6];
          Z23_=Z23_ + ONE/TWO*Z45_;
          Z23_=Z16_*Z23_;
          Z35_= - Z35_*Z43_;
          Z23_=Z41_ + Z44_ + Z28_ + Z35_ + THREE*Z23_ + Z30_;
          Z20_=Z16_*Z20_;
          Z25_=Z15_*Z25_;
          Z20_=Z20_ + Z25_;
          Z21_=Z17_*Z21_;
          Z20_=TWO*Z20_ - ONE/TWO*Z21_;
          Z21_= - param[2]*Z24_;
          Z25_=Z17_*Z29_;
          Z25_= - TWO*Z33_ + Z25_;
          Z25_=Z17_*Z25_;
          Z22_=Z22_*Z19_;
          Z22_=THREE*Z22_;
          Z28_=(Z16_*Z16_)*Z22_;
          Z27_=Z27_*Z18_;
          Z27_=THREE*Z27_;
          Z30_=(Z15_*Z15_)*Z27_;
          Z21_=Z25_ + Z30_ + Z28_ + Z21_;
          Z25_=Z13_*Z39_;
          Z28_=Z29_*Z14_;
          Z29_=f*Z40_;
          Z25_=ONE/EIGHT*Z29_ + Z28_ - Z36_ - ONE/FOUR*Z25_;
          Z25_=f*Z25_;
          Z24_= - param[0]*Z24_;
          Z28_=Z28_ - Z38_;
          Z28_=Z14_*Z28_;
          Z29_=Z12_*Z45_;
          Z30_=Z11_*Z42_;
          Z29_=Z29_ + Z30_;
          Z30_=c[5]*Z32_;
          Z32_=c[2]*Z34_;
          Z30_=Z30_ + Z32_;
          Z22_=(Z12_*Z12_)*Z22_;
          Z27_=(Z11_*Z11_)*Z27_;
          Z22_=Z25_ + Z28_ + Z27_ + ONE/TWO*Z30_ + Z22_ + THREE/TWO*Z29_ + Z24_;

          divnum[jext1] -= Z22_;
          divnum[jext1mu2] -= Z31_;
          divnum[jext2] -= Z26_;
          divnum[jext3] -= Z37_;
          divnum[jext1x1] -= Z23_;
          divnum[jext2x1] -= Z20_;
          divnum[jext1x2] -= Z21_;
        */
      }

    }

    // Subtraction of triangles from tadpoles
    void correcttadcoeffs(Complex divnum[],
                          const ComplexMomentum & et3,
                          const ComplexMomentum & et4,
                          const Complex c[],
                          const ComplexMomentum & e3,
                          const ComplexMomentum & e4,
                          const Complex & param,
                          const RealMomentum & k0, const Complex & f0,
                          const RealMomentum & k1, const Complex & f1,
                          bool)
    {
      const int jext0 = 2;
      const int jext0mu2 = 3;
      Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
      Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_;

      Z1_=Real(1)/((mp(e3,k0)*mp(e3,k0)*mp(e3,k0))*(mp(e3,k1)*mp(e3,k1)*mp(e3,k1)));
      Z2_=mp(e3,et3);
      Z3_=mp(e3,k0);
      Z4_=mp(e3,k1);
      Z5_=mp(e4,k1);
      Z6_=mp(e4,k0);
      Z7_=mp(e4,et3);
      Z8_=mp(e3,et4);
      Z9_=mp(e4,et4);
      Z10_=(Z3_*Z3_);
      Z11_=Z10_*Z4_;
      Z12_=Z5_*Z11_;
      Z13_=(Z4_*Z4_);
      Z14_=Z13_*Z6_*Z3_;
      Z12_=Z12_ + Z14_;
      Z14_=Real(1.)/Real(4.)*Z8_;
      Z14_=Z14_*Z12_;
      Z15_=Z4_*Z3_;
      Z16_=(Z15_*Z15_);
      Z17_=Z16_*Z9_;
      Z14_=Z14_ - Z17_;
      Z14_=c[13]*Z14_;
      Z17_= - Z8_*Z14_;
      Z18_=Real(1.)/Real(4.)*Z16_;
      Z19_=c[11]*Z18_;
      Z17_=Z19_ + Z17_;
      Z19_=(Z8_*Z8_);
      Z17_=Z17_*Z19_;
      Z20_=Z2_*c[12];
      Z12_=Z12_*Z20_;
      Z21_=Z7_*Z16_*c[12];
      Z12_= - Z21_ + Real(1.)/Real(4.)*Z12_;
      Z21_= - Z2_*Z12_;
      Z18_=c[10]*Z18_;
      Z18_=Z18_ + Z21_;
      Z21_=(Z2_*Z2_);
      Z18_=Z18_*Z21_;
      Z17_=Z17_ + Z18_;
      Z17_=Z1_*Z17_;
      Z11_=f1*Z11_;
      Z13_=Z13_*f0;
      Z18_=Z13_*Z3_;
      Z11_=Z11_ + Z18_;
      Z18_= - c[3]*Z11_;
      Z15_=Z15_*f1;
      Z13_=Z15_ + Z13_;
      Z13_=Z13_*f0;
      Z10_=Z10_*(f1*f1);
      Z10_=Z10_ + Z13_;
      Z13_=Z10_*Z20_;
      Z13_=Real(1.)/Real(2.)*Z13_ + Z18_;
      Z13_=Z2_*Z13_;
      Z15_=c[2]*Z16_;
      Z13_=Z15_ + Real(1.)/Real(2.)*Z13_;
      Z13_=Z13_*Z21_;
      Z10_=Z19_*c[13]*Z10_;
      Z15_=c[5]*Z16_;
      Z10_=Z15_ + Real(1.)/Real(4.)*Z10_;
      Z10_=Z10_*Z19_;
      Z15_=(Z8_*Z8_*Z8_);
      Z11_=c[6]*Z11_*Z15_;
      Z10_=Z13_ + Z10_ - Real(1.)/Real(2.)*Z11_;
      Z11_= - Z14_*Z15_;
      Z12_= - Z12_*(Z2_*Z2_*Z2_);
      Z11_=Z11_ + Z12_;
      Z11_=param*Z11_;
      Z10_=Real(1.)/Real(4.)*Z10_ + Z11_;
      Z10_=Z1_*Z10_;

      divnum[jext0mu2] -= Z17_;
      divnum[jext0] -= Z10_;
    }

    // Subtraction of bubbles from tadpoles
    void correcttadcoeffs(Complex divnum[],
                          const ComplexMomentum & eb2,
                          const ComplexMomentum & eb3,
                          const ComplexMomentum & eb4,
                          const RealMomentum & kbol,
                          const Complex c[],
                          const ComplexMomentum & e3,
                          const ComplexMomentum & e4,
                          const Complex & param,
                          const RealMomentum & k, const Complex & f,
                          bool)
    {
      const int jext0 = 2;
      const int jext0mu2 = 3;
      Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
      Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_;

      Z1_=Real(1)/((mp(e3,k)*mp(e3,k)*mp(e3,k)));
      Z2_=mp(e3,k);
      Z3_=mp(e3,eb2);
      Z4_=mp(e4,eb2);
      Z5_=mp(e4,eb3);
      Z6_=mp(e4,eb4);
      Z7_=mp(e3,eb3);
      Z8_=mp(e3,eb4);
      Z9_=mp(e4,k);
      Z10_=mp(eb2,kbol);
      Z11_=Z4_*Z8_;
      Z12_=Real(1.)/Real(2.)*Z3_;
      Z13_=Z12_*Z6_;
      Z11_=Z11_ + Z13_;
      Z11_=Z11_*c[17];
      Z13_=Z4_*Z7_;
      Z12_=Z12_*Z5_;
      Z12_=Z13_ + Z12_;
      Z12_=Z12_*c[16];
      Z13_=c[19]*Z6_*Z8_;
      Z14_=Z5_*c[18]*Z7_;
      Z15_=c[13]*Z3_*Z4_;
      Z11_=Z14_ + Z13_ + Real(3.)/Real(2.)*Z15_ + Z11_ + Z12_;
      Z12_=Real(1.)/Real(2.)*c[10] + Z11_;
      Z12_=Z3_*Z12_;
      Z13_=(Z7_*Z7_);
      Z14_=Z5_*Z13_*c[14];
      Z15_=(Z8_*Z8_);
      Z16_=c[15]*Z15_*Z6_;
      Z14_=Z14_ + Z16_;
      Z16_=Z13_*c[18];
      Z17_=Z15_*c[19];
      Z16_=Z16_ + Z17_;
      Z17_=Z16_*Z4_;
      Z14_=Z17_ + Real(3)*Z14_;
      Z17_=Z7_*c[11];
      Z18_=c[12]*Z8_;
      Z17_=Z18_ + Z17_ + Z14_;
      Z12_=Z12_ + Real(1.)/Real(2.)*Z17_;
      Z12_=Z2_*Z12_;
      Z17_=(Z3_*Z3_);
      Z18_=Z17_*c[13];
      Z18_=Z16_ + Z18_;
      Z18_=Z18_*Z3_;
      Z19_=c[17]*Z8_;
      Z20_=c[16]*Z7_;
      Z19_=Z19_ + Z20_;
      Z17_=Z17_*Z19_;
      Z20_=c[14]*(Z7_*Z7_*Z7_);
      Z21_=c[15]*(Z8_*Z8_*Z8_);
      Z17_=Z18_ + Z21_ + Z17_ + Z20_;
      Z18_=Z17_*Z9_;
      Z12_= - Real(1.)/Real(2.)*Z18_ + Z12_;
      Z20_=Z2_*Z1_;
      Z12_=Z12_*Z20_;
      Z16_= - Z16_*Z10_;
      Z13_= - c[4]*Z13_;
      Z15_= - c[6]*Z15_;
      Z13_=Z16_ + Z15_ + Z13_;
      Z13_=f*Z13_;
      Z15_= - c[8]*Z8_;
      Z16_= - c[7]*Z7_;
      Z15_=Z15_ + Z16_;
      Z15_=f*Z15_;
      Z16_=Z3_*f;
      Z21_=Real(3)*c[13];
      Z22_= - Z10_*Z21_;
      Z22_=Z22_ - c[2];
      Z22_=Z22_*Z16_;
      Z15_=Z22_ + Z15_;
      Z15_=Z3_*Z15_;
      Z13_=Z15_ + Z13_;
      Z15_= - param*Z18_;
      Z16_= - Z19_*Z16_*Z10_;
      Z13_=Z15_ + Real(1.)/Real(2.)*Z13_ + Z16_;
      Z11_=Z3_*Z11_;
      Z11_=Real(1.)/Real(2.)*Z14_ + Z11_;
      Z11_=param*Z11_;
      Z14_=Real(1.)/Real(2.)*Z19_;
      Z15_=(Z10_*Z10_);
      Z14_=Z15_*Z14_;
      Z16_=c[8]*Z10_;
      Z16_=Z16_ + c[5];
      Z16_=Z8_*Z16_;
      Z18_=c[7]*Z10_;
      Z18_=Z18_ + c[3];
      Z18_=Z7_*Z18_;
      Z16_=Z18_ + Z16_;
      Z15_=Z15_*Z21_;
      Z15_=c[1] + Z15_;
      Z18_=c[2]*Z10_;
      Z15_=Real(1.)/Real(2.)*Z15_ + Z18_;
      Z15_=Z3_*Z15_;
      Z11_=Z11_ + Real(1.)/Real(2.)*Z16_ + Z15_ + Z14_;
      Z11_=Z2_*Z11_;
      Z11_=Real(1.)/Real(2.)*Z13_ + Z11_;
      Z11_=Z11_*Z20_;
      Z13_=Z1_*Z17_*(f*f);
      Z11_=Real(1.)/Real(8.)*Z13_ + Z11_;

      divnum[jext0mu2] -= Z12_;
      divnum[jext0] -= Z11_;
    }

    // Subtraction of triangles from full tadpoles
    void correcttadcoeffsfull(Complex divnum[],
                              const ComplexMomentum & et3,
                              const ComplexMomentum & et4,
                              const Complex c[],
                              const ComplexMomentum & e3,
                              const ComplexMomentum & e4,
                              const Complex & param,
                              const RealMomentum & k0,
                              const Complex & f0,
                              const RealMomentum & k1,
                              const Complex & f1,
                              int // rank-n (unused)
                              )
    {
      const int jext2 = 0;
      const int jext1 = 1;
      const int jext0 = 2;
      const int jext0mu2 = 3;

      Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_;
      Complex Z11_, Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_;
      Complex Z21_, Z22_, Z23_, Z24_, Z25_;

      Z1_=Real(1)/((mp(e3,k0)*mp(e3,k0)*mp(e3,k0))*(mp(e3,k1)*mp(e3,k1)*mp(e3,k1)));
      Z2_=mp(e3,et3);
      Z3_=mp(e3,k0);
      Z4_=mp(e3,k1);
      Z5_=mp(e4,k1);
      Z6_=mp(e4,k0);
      Z7_=mp(e4,et3);
      Z8_=mp(e3,et4);
      Z9_=mp(e4,et4);
      Z10_=Real(1.)/Real(4.)*Z1_;
      Z11_=(Z4_*Z4_);
      Z12_=Z3_*Z6_*Z10_*Z11_;
      Z13_=(Z3_*Z3_);
      Z14_=Z10_*Z13_;
      Z15_=Z4_*Z14_*Z5_;
      Z12_=Z12_ + Z15_;
      Z15_=c[13]*(Z8_*Z8_*Z8_*Z8_);
      Z16_=c[12]*(Z2_*Z2_*Z2_*Z2_);
      Z15_=Z15_ + Z16_;
      Z12_=Z12_*Z15_;
      Z16_=(Z2_*Z2_);
      Z17_=c[10]*Z16_;
      Z18_=(Z8_*Z8_);
      Z19_=c[11]*Z18_;
      Z17_=Z17_ + Z19_;
      Z19_=(Z8_*Z8_*Z8_);
      Z20_=c[13]*Z19_*Z9_;
      Z21_=(Z2_*Z2_*Z2_);
      Z22_=Z7_*Z21_*c[12];
      Z20_=Z20_ + Z22_;
      Z17_=Z20_ + Real(1.)/Real(4.)*Z17_;
      Z22_=Z3_*Z4_;
      Z23_=(Z22_*Z22_);
      Z24_=Z23_*Z1_;
      Z17_=Z17_*Z24_;
      Z17_=Z17_ - Z12_;
      Z25_=Z15_*f1;
      Z19_=Z19_*c[6];
      Z21_=Z21_*c[3];
      Z19_=Z19_ + Z21_;
      Z21_=Z19_*Z4_;
      Z21_= - Z21_ + Real(1.)/Real(2.)*Z25_;
      Z13_= - Z21_*Z13_*Z4_;
      Z25_=Real(1.)/Real(2.)*f0;
      Z11_=Z25_*Z15_*Z11_;
      Z25_= - Z11_*Z3_;
      Z13_=Z13_ + Z25_;
      Z13_=Real(1.)/Real(4.)*Z1_*Z13_;
      Z10_=Z15_*Z23_*Z10_;
      Z21_=Z21_*Z22_;
      Z11_=Z21_ + Z11_;
      Z11_=f0*Z1_*Z11_;
      Z19_= - f1*Z19_;
      Z18_=c[5]*Z18_;
      Z16_=c[2]*Z16_;
      Z16_=Z18_ + Z16_;
      Z16_=Z4_*Z16_;
      Z16_=Real(1.)/Real(2.)*Z19_ + Z16_;
      Z16_=Z4_*Z16_;
      Z15_=Z15_*(f1*f1);
      Z15_=Real(1.)/Real(4.)*Z15_ + Z16_;
      Z14_=Z15_*Z14_;
      Z15_=Z20_*Z24_;
      Z12_=Z15_ - Z12_;
      Z12_=param*Z12_;
      Z11_=Real(1.)/Real(8.)*Z11_ + Z14_ + Z12_;

      divnum[jext2] -= Z10_;
      divnum[jext1] -= Z13_;
      divnum[jext0mu2] -= Z17_;
      divnum[jext0] -= Z11_;
    }

    // Subtraction of bubbles from full tadpoles
    void correcttadcoeffsfull(Complex divnum[],
                              const ComplexMomentum & eb2,
                              const ComplexMomentum & eb3,
                              const ComplexMomentum & eb4,
                              const RealMomentum & kbol,
                              const Complex c[],
                              const ComplexMomentum & e3,
                              const ComplexMomentum & e4,
                              const Complex & param,
                              const RealMomentum & k,
                              const Complex & f,
                              int // rank-n (unused)
                              )
    {
      const int jext2 = 0;
      const int jext1 = 1;
      const int jext0 = 2;
      const int jext0mu2 = 3;

      Complex Z1_, Z2_, Z3_, Z4_, Z5_, Z6_, Z7_, Z8_, Z9_, Z10_, Z11_;
      Complex Z12_, Z13_, Z14_, Z15_, Z16_, Z17_, Z18_, Z19_, Z20_, Z21_, Z22_;
      Complex Z23_, Z24_, Z25_, Z26_, Z27_;

      Z1_=Real(1)/((mp(e3,k)*mp(e3,k)*mp(e3,k)));
      Z2_=mp(e3,k);
      Z3_=mp(e3,eb2);
      Z4_=mp(e4,eb2);
      Z5_=mp(e4,eb3);
      Z6_=mp(e4,eb4);
      Z7_=mp(e3,eb3);
      Z8_=mp(e3,eb4);
      Z9_=mp(e4,k);
      Z10_=mp(eb2,kbol);
      Z11_=c[17]*Z3_;
      Z12_=Real(1.)/Real(2.)*Z8_;
      Z13_=Z12_*c[19];
      Z11_=Z11_ + Z13_;
      Z11_=Z11_*Z8_;
      Z13_=c[16]*Z3_;
      Z14_=Real(1.)/Real(2.)*Z7_;
      Z15_=Z14_*c[18];
      Z13_=Z13_ + Z15_;
      Z13_=Z13_*Z7_;
      Z15_=(Z3_*Z3_);
      Z16_=Z15_*c[13];
      Z11_=Z11_ + Z13_ + Real(3.)/Real(2.)*Z16_;
      Z13_=Z4_*Z11_;
      Z16_=Z3_*c[19];
      Z17_=Z8_*c[15];
      Z18_=Z16_ + Real(3.)/Real(2.)*Z17_;
      Z18_=Z18_*Z8_;
      Z19_=Z15_*c[17];
      Z18_=Z18_ + Real(1.)/Real(2.)*Z19_;
      Z18_=Z18_*Z6_;
      Z20_=c[18]*Z3_;
      Z21_=Z7_*c[14];
      Z22_=Z20_ + Real(3.)/Real(2.)*Z21_;
      Z22_=Z22_*Z7_;
      Z23_=Z15_*c[16];
      Z22_=Z22_ + Real(1.)/Real(2.)*Z23_;
      Z22_=Z22_*Z5_;
      Z13_=Z18_ + Z22_ + Z13_;
      Z14_=c[11]*Z14_;
      Z12_=c[12]*Z12_;
      Z12_=Z12_ + Z14_ + Z13_;
      Z14_=(Z2_*Z2_);
      Z12_=Z14_*Z12_;
      Z16_=Z16_ + Z17_;
      Z16_=Z16_*Z8_;
      Z16_=Z16_ + Z19_;
      Z16_=Z16_*Z8_;
      Z17_=Z20_ + Z21_;
      Z17_=Z17_*Z7_;
      Z17_=Z17_ + Z23_;
      Z17_=Z17_*Z7_;
      Z18_=c[13]*(Z3_*Z3_*Z3_);
      Z16_=Z18_ + Z16_ + Z17_;
      Z17_=Real(1.)/Real(2.)*Z2_;
      Z18_=Z16_*Z17_*Z9_;
      Z12_= - Z18_ + Z12_;
      Z12_=Z1_*Z12_;
      Z19_=Real(1.)/Real(2.)*Z14_;
      Z20_=Z19_*Z1_;
      Z21_=c[10]*Z3_*Z20_;
      Z12_=Z12_ + Z21_;
      Z21_=Z3_*c[7];
      Z22_=Z7_*c[4];
      Z21_=Z21_ + Z22_;
      Z21_=Z21_*Z7_;
      Z22_=c[8]*Z3_;
      Z23_=Z8_*c[6];
      Z22_=Z22_ + Z23_;
      Z22_=Z22_*Z8_;
      Z21_=Z21_ + Z22_;
      Z22_=c[2]*Z15_;
      Z22_=Z22_ + Z21_;
      Z22_=Z19_*Z22_;
      Z23_=Z14_*Z10_;
      Z24_=Z11_*Z23_;
      Z25_=Real(1.)/Real(4.)*f;
      Z26_=Z25_*Z2_;
      Z27_= - Z16_*Z26_;
      Z22_=Z27_ + Z24_ + Z22_;
      Z22_=Z1_*Z22_;
      Z20_=Z16_*Z20_;
      Z17_= - Z21_*Z17_;
      Z11_= - Z10_*Z2_*Z11_;
      Z16_=Z16_*Z25_;
      Z11_=Z16_ + Z17_ + Z11_;
      Z11_=f*Z11_;
      Z16_=c[13]*Z3_;
      Z17_=Z7_*c[16];
      Z21_=Z8_*c[17];
      Z16_=Z21_ + Real(3)*Z16_ + Z17_;
      Z16_=Z16_*Z23_;
      Z17_=Z7_*c[7];
      Z21_=Z8_*c[8];
      Z17_=Z17_ + Z21_;
      Z17_=Z17_*Z14_;
      Z16_=Z17_ + Z16_;
      Z16_=Z10_*Z16_;
      Z11_=Z16_ + Z11_;
      Z13_=Z14_*Z13_;
      Z13_= - Z18_ + Z13_;
      Z13_=param*Z13_;
      Z14_=c[1]*Z3_;
      Z16_=Z7_*c[3];
      Z17_=Z8_*c[5];
      Z14_=Z17_ + Z14_ + Z16_;
      Z14_=Z14_*Z19_;
      Z15_= - Z15_*Z26_;
      Z16_=Z3_*Z23_;
      Z15_=Z16_ + Z15_;
      Z15_=c[2]*Z15_;
      Z11_=Z15_ + Z14_ + Z13_ + Real(1.)/Real(2.)*Z11_;
      Z11_=Z1_*Z11_;

      divnum[jext2] -= Z20_;
      divnum[jext1] -= Z22_;
      divnum[jext0mu2] -= Z12_;
      divnum[jext0] -= Z11_;      
    }

  } // namespace x1cuts

#endif // NINJA_X1RANK_SUBTRACTIONS

} // namespace ninja
