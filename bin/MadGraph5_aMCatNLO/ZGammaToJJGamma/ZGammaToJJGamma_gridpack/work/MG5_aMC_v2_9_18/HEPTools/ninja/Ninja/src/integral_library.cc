
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ninja/integral_library.hh>
#include <ninja/ninjanumgen.hh>

namespace ninja {

  void
  IntegralLibrary::getBoxIntegralNM(Complex rslt[3],
                                    Real s21, Real s32, Real s43,
                                    Real s14, Real s31, Real s42)
  {
    getBoxIntegralRM(rslt, s21, s32, s43, s14, s31, s42,
                     Real(), Real(), Real(), Real());
  }

  void
  IntegralLibrary::getTriangleIntegralNM(Complex rslt[3],
                                         Real s21, Real s32, Real s13)
  {
    getTriangleIntegralRM(rslt, s21, s32, s13, Real(), Real(), Real());
  }

  void
  IntegralLibrary::getBubbleIntegralNM(Complex rslt[3],
                                       Real s21)
  {
    getBubbleIntegralRM(rslt, s21, Real(), Real());
  }

  void
  IntegralLibrary::getRank2BubbleIntegralNM(Complex b11[3],
                                            Complex b1[3], Complex b0[3],
                                            Real s21)
  {
    getRank2BubbleIntegralRM(b11, b1, b0, s21, Real(), Real());
  }

  void
  IntegralLibrary::getTadpoleIntegralNM(Complex rslt[3])
  {
    rslt[0] = rslt[1] = rslt[2] = Real();
  }

  void
  IntegralLibrary::getRank3BubbleIntegralCM(Complex b111[3], Complex b11[3],
                                            Complex b1[3], Complex b0[3],
                                            Real p2,
                                            const Complex & m1q,
                                            const Complex & m2q)
  {
    getRank2BubbleIntegralCM(b11, b1, b0, p2, m1q, m2q);
    if (taxicab_norm(p2)>Real(1.0e-8)) {
      Complex b00[3];
      Complex A0d1[3];
      Complex A0d2[3];
      getTadpoleIntegralCM(A0d1, m1q);
      getTadpoleIntegralCM(A0d2, m2q);
      {
        // compute b00
        Complex I2q2[3];
        I2q2[0] = A0d2[0] + m1q*b0[0] -(p2-THREE*(m1q+m2q))/SIX;
        I2q2[1] = A0d2[1] + m1q*b0[1];
        I2q2[2] = A0d2[2] + m1q*b0[2];
        b00[0] = (I2q2[0] - p2*b11[0]) * Real(0.25);
        b00[1] = (I2q2[1] - p2*b11[1]) * Real(0.25);
        b00[2] = (I2q2[2] - p2*b11[2]) * Real(0.25);
      }
      Complex rat = - p2*p2*(p2 - TWO*m1q - FOUR*m2q)/Real(3)
        + Real(0.5) * p2 * (m1q*m1q - m2q*m2q);
      Complex c1 = -Real(4) * (p2*p2*m1q);
      Complex c2 = Real(4) * (m2q - m1q - p2);
      b111[0] = c1*b1[0] + c2*p2*(b00[0] + p2*b11[0])
        + p2*(m1q*A0d1[0] - m2q*A0d2[0]) + rat;
      b111[1] = c1*b1[1] + c2*p2*(b00[1] + p2*b11[1])
        + p2*(m1q*A0d1[1] - m2q*A0d2[1]);
      b111[2] = c1*b1[2] + c2*p2*(b00[2] + p2*b11[2])
        + p2*(m1q*A0d1[2] - m2q*A0d2[2]);
      b111[0] /= Real(4) * p2*p2*p2;
      b111[1] /= Real(4) * p2*p2*p2;
      b111[2] /= Real(4) * p2*p2*p2;
    } else {
      Complex B0z11[3];
      getBubbleIntegralCM(B0z11, Real(), m1q, m1q);
      if (m1q != m2q) {
        Complex B0z22[3];
        getBubbleIntegralCM(B0z22, Real(), m2q, m2q);
        Complex abbr3 = m1q - m2q;
        Complex abbr3q = abbr3*abbr3;
        Complex abbr3c = abbr3q*abbr3;
        Complex abbr4 = ONE/(abbr3c*abbr3);
        Complex m1qq = m1q*m1q;
        Complex m1qc = m1qq*m1q;
        Complex m1qf = m1qq*m1qq;
        Complex m2qq = m2q*m2q;
        Complex m2qc = m2qq*m2q;
        Complex B1 = -(m1qf*abbr4)/FOUR;
        Complex B2 = (-ONE + m1qf*abbr4)/Real(4);
        Complex B3 = (-Real(25)*m1qc + Real(23)*m1qq*m2q
                      - Real(13)*m1q*m2qq + Real(3)*m2qc)/(Real(48)*abbr3c);
        b111[0] = B1*B0z11[0] + B2*B0z22[0] + B3; 
        b111[1] = B1*B0z11[1] + B2*B0z22[1]; 
        b111[2] = B1*B0z11[2] + B2*B0z22[2]; 
      } else {
        b111[0] = -INV4*B0z11[0];
        b111[1] = -INV4*B0z11[1];
        b111[2] = -INV4*B0z11[2];
      }
    }
  }

  void
  IntegralLibrary::getRank3BubbleIntegralRM(Complex b111[3], Complex b11[3],
                                            Complex b1[3], Complex b0[3],
                                            Real p2,
                                            Real m1q, Real m2q)
  {
    getRank2BubbleIntegralRM(b11, b1, b0, p2, m1q, m2q);
    if (taxicab_norm(p2)>Real(1.0e-8)) {
      Complex b00[3];
      Complex A0d1[3];
      Complex A0d2[3];
      getTadpoleIntegralRM(A0d1, m1q);
      getTadpoleIntegralRM(A0d2, m2q);
      {
        // compute b00
        Complex I2q2[3];
        I2q2[0] = A0d2[0] + m1q*b0[0] -(p2-THREE*(m1q+m2q))/SIX;
        I2q2[1] = A0d2[1] + m1q*b0[1];
        I2q2[2] = A0d2[2] + m1q*b0[2];
        b00[0] = (I2q2[0] - p2*b11[0]) * Real(0.25);
        b00[1] = (I2q2[1] - p2*b11[1]) * Real(0.25);
        b00[2] = (I2q2[2] - p2*b11[2]) * Real(0.25);
      }
      Complex rat = - p2*p2*(p2 - TWO*m1q - FOUR*m2q)/Real(3)
        + Real(0.5) * p2 * (m1q*m1q - m2q*m2q);
      Complex c1 = -Real(4) * (p2*p2*m1q);
      Complex c2 = Real(4) * (m2q - m1q - p2);
      b111[0] = c1*b1[0] + c2*p2*(b00[0] + p2*b11[0])
        + p2*(m1q*A0d1[0] - m2q*A0d2[0]) + rat;
      b111[1] = c1*b1[1] + c2*p2*(b00[1] + p2*b11[1])
        + p2*(m1q*A0d1[1] - m2q*A0d2[1]);
      b111[2] = c1*b1[2] + c2*p2*(b00[2] + p2*b11[2])
        + p2*(m1q*A0d1[2] - m2q*A0d2[2]);
      b111[0] /= Real(4) * p2*p2*p2;
      b111[1] /= Real(4) * p2*p2*p2;
      b111[2] /= Real(4) * p2*p2*p2;
    } else {
      Complex B0z11[3];
      getBubbleIntegralRM(B0z11, Real(), m1q, m1q);
      if (m1q != m2q) {
        Complex B0z22[3];
        getBubbleIntegralRM(B0z22, Real(), m2q, m2q);
        Complex abbr3 = m1q - m2q;
        Complex abbr3q = abbr3*abbr3;
        Complex abbr3c = abbr3q*abbr3;
        Complex abbr4 = ONE/(abbr3c*abbr3);
        Complex m1qq = m1q*m1q;
        Complex m1qc = m1qq*m1q;
        Complex m1qf = m1qq*m1qq;
        Complex m2qq = m2q*m2q;
        Complex m2qc = m2qq*m2q;
        Complex B1 = -(m1qf*abbr4)/FOUR;
        Complex B2 = (-ONE + m1qf*abbr4)/Real(4);
        Complex B3 = (-Real(25)*m1qc + Real(23)*m1qq*m2q
                      - Real(13)*m1q*m2qq + Real(3)*m2qc)/(Real(48)*abbr3c);
        b111[0] = B1*B0z11[0] + B2*B0z22[0] + B3; 
        b111[1] = B1*B0z11[1] + B2*B0z22[1]; 
        b111[2] = B1*B0z11[2] + B2*B0z22[2]; 
      } else {
        b111[0] = -INV4*B0z11[0];
        b111[1] = -INV4*B0z11[1];
        b111[2] = -INV4*B0z11[2];
      }
    }
  }

  void
  IntegralLibrary::getRank3BubbleIntegralNM(Complex b111[3], Complex b11[3],
                                            Complex b1[3], Complex b0[3],
                                            Real K11)
  {
    getRank2BubbleIntegralNM(b11, b1, b0, K11);
    if (taxicab_norm(K11)>Real(1.0e-8)) {
      Real K11q = K11*K11;
      Real K11c = K11q*K11;
      Real abbr1 = ONE/K11c;
      Complex A1 = -(abbr1*(K11)*(K11q + TWO*K11))/FOUR;
      Complex A4 = (abbr1*(-TWO*K11c))/Real(24); 
      b111[0] = A1*b0[0] + A4;
      b111[1] = A1*b0[1];
      b111[2] = A1*b0[2];
    } else {
      b111[0] = b111[1] = b111[2] = 0;
    }
  }

} // namespace ninja
