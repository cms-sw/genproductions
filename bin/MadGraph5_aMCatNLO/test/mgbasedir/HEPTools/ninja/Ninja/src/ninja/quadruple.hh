#ifndef NINJA_QUADRUPLE_HH
#define NINJA_QUADRUPLE_HH

#include <iostream>

extern "C" {
#include <quadmath.h>
}

namespace ninja {

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

  inline ninja::Quadruple acos (const ninja::Quadruple & x)
  {
    return acosq (x);
  }

  inline ninja::Quadruple acosh (const ninja::Quadruple & x)
  {
    return acoshq(x);
  }

  inline ninja::Quadruple asin (const ninja::Quadruple & x)
  {
    return asinq(x);
  }

  inline ninja::Quadruple asinh (const ninja::Quadruple & x)
  {
    return asinhq(x);
  }

  inline ninja::Quadruple atan (const ninja::Quadruple & x)
  {
    return atanq(x);
  }

  inline ninja::Quadruple atanh (const ninja::Quadruple & x)
  {
    return atanhq(x);
  }

  inline ninja::Quadruple atan2 (const ninja::Quadruple & x,
                                 const ninja::Quadruple & y)
  {
    return atan2q(x,y);
  }

  inline ninja::Quadruple cbrt (const ninja::Quadruple & x)
  {
    return cbrtq(x);
  }

  inline ninja::Quadruple ceil (const ninja::Quadruple & x)
  {
    return ceilq(x);
  }

  inline ninja::Quadruple copysign (const ninja::Quadruple & x,
                                    const ninja::Quadruple & y)
  {
    return copysignq(x,y);
  }

  inline ninja::Quadruple cosh (const ninja::Quadruple & x)
  {
    return coshq(x);
  }

  inline ninja::Quadruple cos (const ninja::Quadruple & x)
  {
    return cosq(x);
  }

  inline ninja::Quadruple erf (const ninja::Quadruple & x)
  {
    return erfq(x);
  }

  inline ninja::Quadruple erfc (const ninja::Quadruple & x)
  {
    return erfcq(x);
  }

  inline ninja::Quadruple exp (const ninja::Quadruple & x)
  {
    return expq(x);
  }

  inline ninja::Quadruple expm1 (const ninja::Quadruple & x)
  {
    return expm1q(x);
  }

  inline ninja::Quadruple abs(const ninja::Quadruple & r)
  {
    return fabsq (r);
  }

  inline ninja::Quadruple fabs (const ninja::Quadruple & x)
  {
    return fabsq(x);
  }

  inline ninja::Quadruple floor (const ninja::Quadruple & x)
  {
    return floorq(x);
  }


  inline ninja::Quadruple hypot (const ninja::Quadruple & x,
                                 const ninja::Quadruple & y)
  {
    return hypotq(x,y);
  }

  inline ninja::Quadruple ldexp (const ninja::Quadruple & x, int n)
  {
    return ldexpq(x,n);
  }

  inline ninja::Quadruple lgamma (const ninja::Quadruple & x)
  {
    return lgammaq(x);
  }

  inline ninja::Quadruple log (const ninja::Quadruple & x)
  {
    return logq(x);
  }

  inline ninja::Quadruple log10 (const ninja::Quadruple & x)
  {
    return log10q(x);
  }

  inline ninja::Quadruple log2 (const ninja::Quadruple & x)
  {
    return log2q(x);
  }

  inline ninja::Quadruple log1p (const ninja::Quadruple & x)
  {
    return log1pq(x);
  }

  inline ninja::Quadruple nearbyint (const ninja::Quadruple & x)
  {
    return nearbyintq(x);
  }

  inline ninja::Quadruple nextafter (const ninja::Quadruple & x,
                                     const ninja::Quadruple & y)
  {
    return nextafterq(x,y);
  }

  inline ninja::Quadruple pow (const ninja::Quadruple & x,
                               const ninja::Quadruple & y)
  {
    return powq(x,y);
  }

  inline ninja::Quadruple remainder (const ninja::Quadruple & x,
                                     const ninja::Quadruple & y)
  {
    return remainderq(x,y);
  }

  inline ninja::Quadruple rint (const ninja::Quadruple & x)
  {
    return rintq(x);
  }

  inline ninja::Quadruple round (const ninja::Quadruple & x)
  {
    return roundq(x);
  }

  inline ninja::Quadruple sinh (const ninja::Quadruple & x)
  {
    return sinhq(x);
  }

  inline ninja::Quadruple sin (const ninja::Quadruple & x)
  {
    return sinq(x);
  }

  inline ninja::Quadruple sqrt (const ninja::Quadruple & x)
  {
    return sqrtq(x);
  }

  inline ninja::Quadruple tan (const ninja::Quadruple & x)
  {
    return tanq(x);
  }

  inline ninja::Quadruple tanh (const ninja::Quadruple & x)
  {
    return tanhq(x);
  }

  inline ninja::Quadruple tgamma (const ninja::Quadruple & x)
  {
    return tgammaq(x);
  }

  inline ninja::Quadruple trunc (const ninja::Quadruple & x)
  {
    return truncq(x);
  }


  // ComplexFunctions

  inline ninja::Quadruple abs(const ninja::ComplexQuadruple & z)
  {
    return cabsq(z);
  }

  inline ninja::Quadruple real(const ninja::ComplexQuadruple & z)
  {
    return crealq(z);
  }

  inline ninja::Quadruple imag(const ninja::ComplexQuadruple & z)
  {
    return cimagq(z);
  }

  inline ninja::Quadruple arg(const ninja::ComplexQuadruple & z)
  {
    return cargq(z);
  }

  inline ninja::ComplexQuadruple conj(const ninja::ComplexQuadruple & z)
  {
    return conjq(z);
  }

  inline ninja::ComplexQuadruple acos(const ninja::ComplexQuadruple & z)
  {
    return cacosq(z);
  }

  inline ninja::ComplexQuadruple acosh(const ninja::ComplexQuadruple & z)
  {
    return cacoshq(z);
  }

  inline ninja::ComplexQuadruple asin(const ninja::ComplexQuadruple & z)
  {
    return casinq(z);
  }

  inline ninja::ComplexQuadruple asinh(const ninja::ComplexQuadruple & z)
  {
    return casinhq(z);
  }

  inline ninja::ComplexQuadruple atan(const ninja::ComplexQuadruple & z)
  {
    return catanq(z);
  }

  inline ninja::ComplexQuadruple atanh(const ninja::ComplexQuadruple & z)
  {
    return catanhq(z);
  }

  inline ninja::ComplexQuadruple cos(const ninja::ComplexQuadruple & z)
  {
    return ccosq(z);
  }

  inline ninja::ComplexQuadruple cosh(const ninja::ComplexQuadruple & z)
  {
    return ccoshq(z);
  }

  inline ninja::ComplexQuadruple exp(const ninja::ComplexQuadruple & z)
  {
    return cexpq(z);
  }

  inline ninja::ComplexQuadruple log(const ninja::ComplexQuadruple & z)
  {
    return clogq(z);
  }

  inline ninja::ComplexQuadruple log10(const ninja::ComplexQuadruple & z)
  {
    return clog10q(z);
  }

  inline ninja::ComplexQuadruple pow(const ninja::ComplexQuadruple & base,
                                     const ninja::ComplexQuadruple & exponent)
  {
    return cpowq(base,exponent);
  }

  inline ninja::ComplexQuadruple sin(const ninja::ComplexQuadruple & z)
  {
    return csinq(z);
  }

  inline ninja::ComplexQuadruple sinh(const ninja::ComplexQuadruple & z)
  {
    return csinhq(z);
  }

  inline ninja::ComplexQuadruple sqrt(const ninja::ComplexQuadruple & z)
  {
    return csqrtq(z);
  }

  inline ninja::ComplexQuadruple tan(const ninja::ComplexQuadruple & z)
  {
    return ctanq(z);
  }

  inline ninja::ComplexQuadruple tanh(const ninja::ComplexQuadruple & z)
  {
    return ctanhq(z);
  }



  // stream operators
  inline std::ostream & operator << (std::ostream & os,
                                     const ninja::Quadruple & a)
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

} // namespace ninja

#endif // NINJA_QUADRUPLE_HH
