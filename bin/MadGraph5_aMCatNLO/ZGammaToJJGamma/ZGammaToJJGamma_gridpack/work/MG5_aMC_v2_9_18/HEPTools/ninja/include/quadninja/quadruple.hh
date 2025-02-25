#ifndef QUADNINJA_QUADRUPLE_HH
#define QUADNINJA_QUADRUPLE_HH

#include <iostream>

extern "C" {
#include <quadmath.h>
}

namespace quadninja {

  typedef __float128 Quadruple;

  class ComplexQuadruple {
  public:
    ComplexQuadruple()
      : m_(0) {}
    ComplexQuadruple(const __complex128 & oth)
      : m_(oth) {}
    ComplexQuadruple(const ComplexQuadruple & oth)
      : m_(oth.m_) {}
    ComplexQuadruple(const Quadruple & r, const Quadruple & i = 0)
      : m_(r+(__extension__ 1.0iQ)*i) {}
    operator __complex128() const
    {
      return m_;
    }
    ComplexQuadruple & operator + ()
    {
      return *this;
    }
    ComplexQuadruple operator - ()
    {
      return -m_;
    }
    ComplexQuadruple & operator += (const ComplexQuadruple & z)
    {
      m_ += z.m_;
      return *this;
    }
    ComplexQuadruple & operator -= (const ComplexQuadruple & z)
    {
      m_ -= z.m_;
      return *this;
    }
    ComplexQuadruple & operator *= (const ComplexQuadruple & z)
    {
      m_ *= z.m_;
      return *this;
    }
    ComplexQuadruple & operator /= (const ComplexQuadruple & z)
    {
      m_ /= z.m_;
      return *this;
    }

  private :
    __complex128 m_;
  };

  // Real functions

  inline quadninja::Quadruple acos (const quadninja::Quadruple & x)
  {
    return acosq (x);
  }

  inline quadninja::Quadruple acosh (const quadninja::Quadruple & x)
  {
    return acoshq(x);
  }

  inline quadninja::Quadruple asin (const quadninja::Quadruple & x)
  {
    return asinq(x);
  }

  inline quadninja::Quadruple asinh (const quadninja::Quadruple & x)
  {
    return asinhq(x);
  }

  inline quadninja::Quadruple atan (const quadninja::Quadruple & x)
  {
    return atanq(x);
  }

  inline quadninja::Quadruple atanh (const quadninja::Quadruple & x)
  {
    return atanhq(x);
  }

  inline quadninja::Quadruple atan2 (const quadninja::Quadruple & x,
                                 const quadninja::Quadruple & y)
  {
    return atan2q(x,y);
  }

  inline quadninja::Quadruple cbrt (const quadninja::Quadruple & x)
  {
    return cbrtq(x);
  }

  inline quadninja::Quadruple ceil (const quadninja::Quadruple & x)
  {
    return ceilq(x);
  }

  inline quadninja::Quadruple copysign (const quadninja::Quadruple & x,
                                    const quadninja::Quadruple & y)
  {
    return copysignq(x,y);
  }

  inline quadninja::Quadruple cosh (const quadninja::Quadruple & x)
  {
    return coshq(x);
  }

  inline quadninja::Quadruple cos (const quadninja::Quadruple & x)
  {
    return cosq(x);
  }

  inline quadninja::Quadruple erf (const quadninja::Quadruple & x)
  {
    return erfq(x);
  }

  inline quadninja::Quadruple erfc (const quadninja::Quadruple & x)
  {
    return erfcq(x);
  }

  inline quadninja::Quadruple exp (const quadninja::Quadruple & x)
  {
    return expq(x);
  }

  inline quadninja::Quadruple expm1 (const quadninja::Quadruple & x)
  {
    return expm1q(x);
  }

  inline quadninja::Quadruple abs(const quadninja::Quadruple & r)
  {
    return fabsq (r);
  }

  inline quadninja::Quadruple fabs (const quadninja::Quadruple & x)
  {
    return fabsq(x);
  }

  inline quadninja::Quadruple floor (const quadninja::Quadruple & x)
  {
    return floorq(x);
  }


  inline quadninja::Quadruple hypot (const quadninja::Quadruple & x,
                                 const quadninja::Quadruple & y)
  {
    return hypotq(x,y);
  }

  inline quadninja::Quadruple ldexp (const quadninja::Quadruple & x, int n)
  {
    return ldexpq(x,n);
  }

  inline quadninja::Quadruple lgamma (const quadninja::Quadruple & x)
  {
    return lgammaq(x);
  }

  inline quadninja::Quadruple log (const quadninja::Quadruple & x)
  {
    return logq(x);
  }

  inline quadninja::Quadruple log10 (const quadninja::Quadruple & x)
  {
    return log10q(x);
  }

  inline quadninja::Quadruple log2 (const quadninja::Quadruple & x)
  {
    return log2q(x);
  }

  inline quadninja::Quadruple log1p (const quadninja::Quadruple & x)
  {
    return log1pq(x);
  }

  inline quadninja::Quadruple nearbyint (const quadninja::Quadruple & x)
  {
    return nearbyintq(x);
  }

  inline quadninja::Quadruple nextafter (const quadninja::Quadruple & x,
                                     const quadninja::Quadruple & y)
  {
    return nextafterq(x,y);
  }

  inline quadninja::Quadruple pow (const quadninja::Quadruple & x,
                               const quadninja::Quadruple & y)
  {
    return powq(x,y);
  }

  inline quadninja::Quadruple remainder (const quadninja::Quadruple & x,
                                     const quadninja::Quadruple & y)
  {
    return remainderq(x,y);
  }

  inline quadninja::Quadruple rint (const quadninja::Quadruple & x)
  {
    return rintq(x);
  }

  inline quadninja::Quadruple round (const quadninja::Quadruple & x)
  {
    return roundq(x);
  }

  inline quadninja::Quadruple sinh (const quadninja::Quadruple & x)
  {
    return sinhq(x);
  }

  inline quadninja::Quadruple sin (const quadninja::Quadruple & x)
  {
    return sinq(x);
  }

  inline quadninja::Quadruple sqrt (const quadninja::Quadruple & x)
  {
    return sqrtq(x);
  }

  inline quadninja::Quadruple tan (const quadninja::Quadruple & x)
  {
    return tanq(x);
  }

  inline quadninja::Quadruple tanh (const quadninja::Quadruple & x)
  {
    return tanhq(x);
  }

  inline quadninja::Quadruple tgamma (const quadninja::Quadruple & x)
  {
    return tgammaq(x);
  }

  inline quadninja::Quadruple trunc (const quadninja::Quadruple & x)
  {
    return truncq(x);
  }


  // ComplexFunctions

  inline quadninja::Quadruple abs(const quadninja::ComplexQuadruple & z)
  {
    return cabsq(z);
  }

  inline quadninja::Quadruple real(const quadninja::ComplexQuadruple & z)
  {
    return crealq(z);
  }

  inline quadninja::Quadruple imag(const quadninja::ComplexQuadruple & z)
  {
    return cimagq(z);
  }

  inline quadninja::Quadruple arg(const quadninja::ComplexQuadruple & z)
  {
    return cargq(z);
  }

  inline quadninja::ComplexQuadruple conj(const quadninja::ComplexQuadruple & z)
  {
    return conjq(z);
  }

  inline quadninja::ComplexQuadruple acos(const quadninja::ComplexQuadruple & z)
  {
    return cacosq(z);
  }

  inline quadninja::ComplexQuadruple acosh(const quadninja::ComplexQuadruple & z)
  {
    return cacoshq(z);
  }

  inline quadninja::ComplexQuadruple asin(const quadninja::ComplexQuadruple & z)
  {
    return casinq(z);
  }

  inline quadninja::ComplexQuadruple asinh(const quadninja::ComplexQuadruple & z)
  {
    return casinhq(z);
  }

  inline quadninja::ComplexQuadruple atan(const quadninja::ComplexQuadruple & z)
  {
    return catanq(z);
  }

  inline quadninja::ComplexQuadruple atanh(const quadninja::ComplexQuadruple & z)
  {
    return catanhq(z);
  }

  inline quadninja::ComplexQuadruple cos(const quadninja::ComplexQuadruple & z)
  {
    return ccosq(z);
  }

  inline quadninja::ComplexQuadruple cosh(const quadninja::ComplexQuadruple & z)
  {
    return ccoshq(z);
  }

  inline quadninja::ComplexQuadruple exp(const quadninja::ComplexQuadruple & z)
  {
    return cexpq(z);
  }

  inline quadninja::ComplexQuadruple log(const quadninja::ComplexQuadruple & z)
  {
    return clogq(z);
  }

  inline quadninja::ComplexQuadruple log10(const quadninja::ComplexQuadruple & z)
  {
    return clog10q(z);
  }

  inline quadninja::ComplexQuadruple pow(const quadninja::ComplexQuadruple & base,
                                     const quadninja::ComplexQuadruple & exponent)
  {
    return cpowq(base,exponent);
  }

  inline quadninja::ComplexQuadruple sin(const quadninja::ComplexQuadruple & z)
  {
    return csinq(z);
  }

  inline quadninja::ComplexQuadruple sinh(const quadninja::ComplexQuadruple & z)
  {
    return csinhq(z);
  }

  inline quadninja::ComplexQuadruple sqrt(const quadninja::ComplexQuadruple & z)
  {
    return csqrtq(z);
  }

  inline quadninja::ComplexQuadruple tan(const quadninja::ComplexQuadruple & z)
  {
    return ctanq(z);
  }

  inline quadninja::ComplexQuadruple tanh(const quadninja::ComplexQuadruple & z)
  {
    return ctanhq(z);
  }



  // stream operators
  inline std::ostream & operator << (std::ostream & os,
                                     const quadninja::Quadruple & a)
  {
    int width = 30;
    char buf[128];
    quadmath_snprintf (buf, sizeof buf, "%-#*.30Qe", width, a);
    return os << buf;
  }

  // stream operators
  inline std::ostream & operator << (std::ostream & os,
                                     const __complex128 & a)
  {
    return os << "(" << real(a) << "," << imag(a) << ")";
  }

} // namespace quadninja

#endif // QUADNINJA_QUADRUPLE_HH
