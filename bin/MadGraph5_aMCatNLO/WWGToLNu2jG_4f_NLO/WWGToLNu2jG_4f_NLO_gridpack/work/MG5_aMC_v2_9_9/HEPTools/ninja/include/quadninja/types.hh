// This -*- C++ -*- header file contains typedefs and wrappers used in
// the Ninja library.


#ifndef QUADNINJA_TYPES_HH
#define QUADNINJA_TYPES_HH

#include <quadninja/ninja_config.h>

#define QUADNINJA_TYPES_HH_INSIDE 1

#if defined(NINJA_QUADRUPLE) || defined(QUADNINJA_TYPES_HH_INSIDE)
#include <quadninja/quadruple.hh>
#else
# include <cmath>
# include <complex>
#endif

#include <iostream>
#include <limits>

#include <quadninja/static_arrays.hh>
#include <quadninja/zero_float.hh>

#if defined(NINJA_NO_EXCEPTIONS)
# define QUADNINJA_THROW(exception) (std::terminate())
#else
# define QUADNINJA_THROW(exception) throw exception
#endif

#define QUADNINJA_REAL(x) (quadninja::Real(x))

#if !defined(NINJA_QUADRUPLE) && !defined(QUADNINJA_TYPES_HH_INSIDE)
# define QUADNINJA_REAL_DEF(x) (quadninja::Real(x))
# define QUADNINJA_COMPLEX_DEF(r,i) (quadninja::Complex(r,i))
#else
# define QUADNINJA_TOKEN_QPASTE0(x,y) x##y
# define QUADNINJA_REAL_DEF(x) (quadninja::Real(NINJA_TOKEN_QPASTE0(x,q)))
# define QUADNINJA_COMPLEX_DEF(r,i) (quadninja::Complex(NINJA_REAL_DEF(r)),quadninja::Complex(NINJA_REAL_DEF(i)))
#endif


namespace quadninja {

  // typedefs for Real and Complex floating-point types
#if !defined(NINJA_QUADRUPLE) && !defined(QUADNINJA_TYPES_HH_INSIDE)
  typedef double Real;
  typedef std::complex<Real> Complex;
  const Real INFRARED_EPS = 1.0e-09;
  const Real REAL_EPS = Real(1.0e+3)*std::numeric_limits<Real>::epsilon();
  const Real REAL_MIN = Real(1.0e+20)*std::numeric_limits<Real>::min();
#else
  typedef quadninja::Quadruple Real;
  typedef quadninja::ComplexQuadruple Complex;
  const Real INFRARED_EPS = 1.0e+15*FLT128_EPSILON;
  const Real REAL_EPS = Real(1.0e+3)*FLT128_EPSILON;
  const Real REAL_MIN = Real(1.0e+20)*FLT128_MIN;
#endif

  // typedef for zero, real and complex masses
  typedef Real RealMasses;
  typedef Complex ComplexMasses;
  typedef ZeroFloat Massless;


  // Imaginary unit
  const Complex I(Real(0.),Real(1.));


#if !defined(NINJA_QUADRUPLE) && !defined(QUADNINJA_TYPES_HH_INSIDE)
  // Put real in ninja-namespace
  inline Real real(Real z)
  {
    return z;
  }
  inline Real real(const Complex & z)
  {
    return std::real(z);
  }
  // Put imag in ninja-namespace
  inline Real imag(Real)
  {
    return 0;
  }
  inline Real imag(const Complex & z)
  {
    return std::imag(z);
  }
  // Put conj in ninja-namespace
  inline Real conj(Real z)
  {
    return z;
  }
  inline Complex conj(const Complex & z)
  {
    return std::conj(z);
  }
  // Put abs in ninja-namespace
  inline Real abs(Real z)
  {
    return std::abs(z);
  }
  inline Real abs(const Complex & z)
  {
    return std::abs(z);
  }
  // Put real pow in ninja-namespace
  inline Real pow(Real z, unsigned n)
  {
    return std::pow(z, n);
  }
#define QUADNINJA_IMPORT_STD_FUN(fun) \
  inline Real fun(Real z)         \
  {                               \
    return std::fun(z);           \
  }                                     \
  inline Complex fun(const Complex & z) \
  {                                     \
    return std::fun(z);                 \
  }
  QUADNINJA_IMPORT_STD_FUN(sqrt)
  QUADNINJA_IMPORT_STD_FUN(log)
  QUADNINJA_IMPORT_STD_FUN(cos)
  QUADNINJA_IMPORT_STD_FUN(sin)
#undef QUADNINJA_IMPORT_STD_FUN
#endif

  inline Real norm2(Real x)
  {
    return x*x;
  }

  // The taxicab norm (or Manhattan norm) in the complex plane
  //
  //    ||z|| = |real(z)| + |imag(z)|
  //
  // Its computation should be faster than abs(z)
  inline Real taxicab_norm (const Complex & z)
  {
    return abs(real(z))+abs(imag(z));
  }

  // overrides taxicab_norm for real types
  inline Real taxicab_norm(const Real & x)
  {
    return abs(x);
  }


  // const pointer type
  template<typename X> struct const_pointer
  {
    typedef const X* type;
  };

  // Specialize quadninja::const_pointer<ZeroFloat>
  template<> struct const_pointer<ZeroFloat> {
    typedef ZeroFloatArray type;
  };


  // Convert Zero-Floats to reals by asking for real part
  inline Real real (ZeroFloat)
  {
    return Real();
  }

  // Converting ZeroFloats to Complex
  inline Complex toCmplx (const Complex & z)
  {
    return z;
  }
  inline Complex toCmplx (const ZeroFloat &)
  {
    return Complex();
  }


  // some constants

  const Real ZERO = Real(0.0);
  const Real INV8 = Real(0.125);
  const Real INV4 = Real(0.25);
  const Real HALF = Real(0.5);
  const Real ONE = Real(1.);
  const Real ONEDOTFIVE = Real(1.5);
  const Real TWO = Real(2.);
  const Real THREE = Real(3.);
  const Real FOUR = Real(4.);
  const Real FIVE = Real(5.);
  const Real SIX = Real(6.);
  const Real EIGHT = Real(8.);
  const Real TWELVE = Real(12.);
  const Real SIXTEEN = Real(16.);
  const Real SQRT2 = sqrt(TWO);
  const Real SQRT3 = sqrt(THREE);

  const Real INVSQRT2 = HALF*SQRT2;
  const Real INVSQRT3 = 1./sqrt(THREE);
  const Real INVSQRT6 = 1./sqrt(Real(6.));

#if !defined(NINJA_QUADRUPLE) && !defined(QUADNINJA_TYPES_HH_INSIDE)
  const quadninja::Real PI = M_PI;
#else
  const quadninja::Real PI = M_PIq;
#endif

} // namespace quadninja

#undef QUADNINJA_TYPES_HH_INSIDE

#endif // QUADNINJA_TYPES_HH
