// -* C++ -*- header file which defines classes for Real and Complex
// fourmomenta.


#ifndef NINJA_MOMENTUM_HH
#define NINJA_MOMENTUM_HH

#include <iostream>

#include <ninja/types.hh>
#include <ninja/static_arrays.hh>

#if 1
# define NINJA_MOM_EXPR(mu,expr) do { \
    unsigned mu = 0;  expr;              \
    mu = 1;  expr;                      \
    mu = 2;  expr;                      \
    mu = 3;  expr;                      \
  } while(0)
#endif
#if 0
# define NINJA_MOM_EXPR(mu,expr) do { \
    for (unsigned mu=0; mu<4; ++mu) { \
      expr;                              \
    }                                  \
  } while(0)
#endif

namespace ninja {

  // Forward declarations
  class RealMomentum;
  class ComplexMomentum;


  // Definition of a class for Real four-momenta
  class RealMomentum {
    
  public:

    // Default constructor
    RealMomentum(): data(Real(), Real(), Real(), Real()) {}

    // Constructor from four real numbers
    RealMomentum(const Real & p0, const Real & p1,
                 const Real & p2, const Real & p3)
      : data(p0, p1, p2, p3)  {}

    // Constructor from an array of four real numbers
    explicit RealMomentum(const Real p[4])
      : data(p[0], p[1], p[2], p[3])  {}

    // Copy-constructor
    RealMomentum(const RealMomentum & p)
      : data( p.data[0], p.data[1], p.data[2], p.data[3])  {}

    // Acces to components with: p(0), p(1), p(2), p(3)
    Real operator() (unsigned i) const
    {
      return data[i];
    }
    Real & operator() (unsigned i)
    {
      return data[i];
    }

    // Access to components with: p[0], p[1], p[2], p[3]
    Real operator[] (unsigned i) const
    {
      return data[i];
    }
    Real & operator[] (unsigned i)
    {
      return data[i];
    }

    // Assignment operator =
    RealMomentum & operator = (const RealMomentum & p)
    {
      data[0] = p.data[0];
      data[1] = p.data[1];
      data[2] = p.data[2];
      data[3] = p.data[3];
      return *this;
    }

    // Operator +=
    RealMomentum & operator += (const RealMomentum & p)
    {
      data[0] += p.data[0];
      data[1] += p.data[1];
      data[2] += p.data[2];
      data[3] += p.data[3];
      return *this;
    }

    // Operator -=
    RealMomentum & operator -= (const RealMomentum & p)
    {
      data[0] -= p.data[0];
      data[1] -= p.data[1];
      data[2] -= p.data[2];
      data[3] -= p.data[3];
      return *this;
    }

    // Operator *=
    RealMomentum & operator *= (const Real s)
    {
      data[0] *= s;
      data[1] *= s;
      data[2] *= s;
      data[3] *= s;
      return *this;
    }

    // Operator /=
    RealMomentum & operator /= (const Real s)
    {
      data[0] /= s;
      data[1] /= s;
      data[2] /= s;
      data[3] /= s;
      return *this;
    }

    // friend declarations
    friend std::ostream & operator << (std::ostream & os,
                                       const RealMomentum & p);
    friend const RealMomentum & operator +(const RealMomentum & p1);
    friend RealMomentum operator -(const RealMomentum & p1);
    friend RealMomentum operator+(const RealMomentum & p1,
                                  const RealMomentum & p2);
    friend RealMomentum operator-(const RealMomentum & p1,
                                  const RealMomentum & p2);
    friend RealMomentum operator *(const RealMomentum & p, const Real & s );
    friend RealMomentum operator *(const Real & s, const RealMomentum & p);
    friend RealMomentum operator /(const RealMomentum & p, const Real & s );
    friend Real mp(const RealMomentum & p1, const RealMomentum & p2);
    friend Real mp2(const RealMomentum & p);
    friend Real eucl_mp2(const RealMomentum & p);
    friend class ComplexMomentum;
    friend ComplexMomentum operator+(const RealMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator+(const ComplexMomentum & p1,
                                     const RealMomentum & p2);
    friend ComplexMomentum operator-(const RealMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator-(const ComplexMomentum & p1,
                                     const RealMomentum & p2);
    friend ComplexMomentum operator *(const RealMomentum & p,
                                      const Complex & s );
    friend ComplexMomentum operator *(const Complex & s,
                                      const RealMomentum & p);
    friend Complex mp(const ComplexMomentum & p1, const RealMomentum & p2);
    friend Complex mp(const RealMomentum & p1, const ComplexMomentum & p2);

  private:
    ninja::details::Array4D<Real> data;

  }; // class RealMomentum



  // Definition of a class for complex four-momenta
  class ComplexMomentum
  {

  public:

    // Default constructor
    ComplexMomentum(): data(Complex(), Complex(),
                            Complex(), Complex()) {}

    // Constructor from four complex numbers
    ComplexMomentum(const Complex & p0, const Complex & p1,
                    const Complex & p2, const Complex & p3)
      : data(p0, p1, p2, p3)  {}

    // Constructor from an array of four complex numbers
    explicit ComplexMomentum(const Complex p[4])
      : data(p[0], p[1], p[2], p[3])  {}

    // Copy-constructor
    ComplexMomentum(const ComplexMomentum & p)
      : data(p.data[0], p.data[1], p.data[2], p.data[3])  {}

    // Conversion from real-momentum
    ComplexMomentum(const RealMomentum & p)
      : data(p.data[0], p.data[1], p.data[2], p.data[3])  {}

    // Access to components with: p(0), p(1), p(2), p(3)
    const Complex operator() (unsigned i) const
    {
      return data[i];
    }
    Complex & operator() (unsigned i)
    {
      return data[i];
    }

    // Access to components with: p[0], p[1], p[2], p[3]
    const Complex operator[] (unsigned i) const
    {
      return data[i];
    }
    Complex & operator[] (unsigned i)
    {
      return data[i];
    }

    // Assignment operator =
    ComplexMomentum & operator = (const ComplexMomentum & p)
    {
      data[0] = p.data[0];
      data[1] = p.data[1];
      data[2] = p.data[2];
      data[3] = p.data[3];
      return *this;
    }
    ComplexMomentum & operator = (const RealMomentum & p)
    {
      data[0] = p[0];
      data[1] = p[1];
      data[2] = p[2];
      data[3] = p[3];
      return *this;
    }

    // Operator +=
    ComplexMomentum & operator += (const ComplexMomentum & p)
    {
      data[0] += p.data[0];
      data[1] += p.data[1];
      data[2] += p.data[2];
      data[3] += p.data[3];
      return *this;
    }
    ComplexMomentum & operator += (const RealMomentum & p)
    {
      data[0] += p[0];
      data[1] += p[1];
      data[2] += p[2];
      data[3] += p[3];
      return *this;
    }

    // Operator -=
    ComplexMomentum & operator -= (const ComplexMomentum & p)
    {
      data[0] -= p.data[0];
      data[1] -= p.data[1];
      data[2] -= p.data[2];
      data[3] -= p.data[3];
      return *this;
    }
    ComplexMomentum & operator -= (const RealMomentum & p)
    {
      data[0] -= p[0];
      data[1] -= p[1];
      data[2] -= p[2];
      data[3] -= p[3];
      return *this;
    }

    // Multiplication operator *=
    ComplexMomentum & operator *= (const Complex s)
    {
      data[0] *= s;
      data[1] *= s;
      data[2] *= s;
      data[3] *= s;
      return *this;
    }
    ComplexMomentum & operator *= (const Real s)
    {
      data[0] *= s;
      data[1] *= s;
      data[2] *= s;
      data[3] *= s;
      return *this;
    }

    // Operator /=
    ComplexMomentum & operator /= (const Complex s)
    {
      data[0] /= s;
      data[1] /= s;
      data[2] /= s;
      data[3] /= s;
      return *this;
    }
    // Operator /=
    ComplexMomentum & operator /= (const Real s)
    {
      data[0] /= s;
      data[1] /= s;
      data[2] /= s;
      data[3] /= s;
      return *this;
    }

    // friend declarations
    friend std::ostream & operator << (std::ostream & os,
                                       const ComplexMomentum & p);
    friend const ComplexMomentum & operator +(const ComplexMomentum & p1);
    friend ComplexMomentum operator -(const ComplexMomentum & p1);
    friend ComplexMomentum operator+(const ComplexMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator+(const RealMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator+(const ComplexMomentum & p1,
                                     const RealMomentum & p2);
    friend ComplexMomentum operator-(const ComplexMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator-(const RealMomentum & p1,
                                     const ComplexMomentum & p2);
    friend ComplexMomentum operator-(const ComplexMomentum & p1,
                                     const RealMomentum & p2);
    friend ComplexMomentum operator *(const ComplexMomentum & p,
                                      const Complex & s );
    friend ComplexMomentum operator *(const Complex & s,
                                      const ComplexMomentum & p);
    friend ComplexMomentum operator *(const ComplexMomentum & p,
                                      const Real & s );
    friend ComplexMomentum operator *(const Real & s,
                                      const ComplexMomentum & p);
    friend ComplexMomentum operator *(const RealMomentum & p,
                                      const Complex & s );
    friend ComplexMomentum operator *(const Complex & s,
                                      const RealMomentum & p);
    friend ComplexMomentum operator /(const ComplexMomentum & p,
                                      const Complex & s );
    friend ComplexMomentum operator /(const ComplexMomentum & p,
                                      const Real & s );
    friend Complex mp(const ComplexMomentum & p1, const ComplexMomentum & p2);
    friend Complex mp(const RealMomentum & p1, const ComplexMomentum & p2);
    friend Complex mp(const ComplexMomentum & p1, const RealMomentum & p2);
    friend Complex mp2(const ComplexMomentum & p);

  private:
    ninja::details::Array4D<Complex> data;

  }; // class ComplexMomentum


  // Functions and operators for Real Momenta

  // Standard output stream
  inline std::ostream & operator << (std::ostream & os,
                                     const RealMomentum & p)
  {
    return os << "{"<< p.data[0]<<", " << p.data[1]
              << ", " << p.data[2] << ", " << p.data[3] << "}";
  }

  // Unary operators + and -
  inline const RealMomentum & operator +(const RealMomentum & p1)
  {
    return p1;
  }
  inline RealMomentum operator -(const RealMomentum & p1)
  {
    return RealMomentum(-p1.data[0], -p1.data[1], -p1.data[2], -p1.data[3]);
  }


  // Binary operator +
  inline RealMomentum operator+(const RealMomentum & p1,
                                const RealMomentum & p2)
  {
    return RealMomentum(p1.data[0] + p2.data[0],
                        p1.data[1] + p2.data[1],
                        p1.data[2] + p2.data[2],
                        p1.data[3] + p2.data[3]);
  }

  // Binary operator -
  inline RealMomentum operator-(const RealMomentum & p1,
                                const RealMomentum & p2)
  {
    return RealMomentum(p1.data[0] - p2.data[0],
                        p1.data[1] - p2.data[1],
                        p1.data[2] - p2.data[2],
                        p1.data[3] - p2.data[3]);
  }

  // Binary operator *
  inline RealMomentum operator *(const RealMomentum & p, const Real & s )
  {
    return RealMomentum(s*p.data[0], s*p.data[1],
                        s*p.data[2], s*p.data[3]);
  }
  inline RealMomentum operator *(const Real & s, const RealMomentum & p)
  {
    return RealMomentum(s*p.data[0], s*p.data[1],
                        s*p.data[2], s*p.data[3]);
  }

  // Binary operator /
  inline RealMomentum operator /(const RealMomentum & p, const Real & s )
  {
    return RealMomentum(p.data[0]/s, p.data[1]/s,
                        p.data[2]/s, p.data[3]/s);
  }

  // Scalar Minkowski-product p1.p2
  inline Real mp(const RealMomentum & p1, const RealMomentum & p2)
  {
    return p1.data[0]*p2.data[0]
      -p1.data[1]*p2.data[1] - p1.data[2]*p2.data[2] - p1.data[3]*p2.data[3];
  }

  // Square p^2 = p.p (with Minkowski metric)
  inline Real mp2(const RealMomentum & p)
  {
    // consider (p0+p3)*(p0-p3)-p1*p1-p2*p2 --> same n. of operations
    // but 1 less multiplication
    return p.data[0]*p.data[0]
      -p.data[1]*p.data[1] - p.data[2]*p.data[2] - p.data[3]*p.data[3];
  }

  // euclidean p^2
  inline Real eucl_mp2(const RealMomentum & p)
  {
    return p.data[0]*p.data[0]
      +p.data[1]*p.data[1] + p.data[2]*p.data[2] + p.data[3]*p.data[3];
  }


  // Functions and operators for Complex Momenta

  // Standard output stream
  inline std::ostream & operator << (std::ostream & os,
                                     const ComplexMomentum & p)
  {
    return os << "{"<< p.data[0]<<", " << p.data[1]
              << ", " << p.data[2] << ", " << p.data[3] << "}";
  }

  // Unary operators + and -
  inline const ComplexMomentum & operator +(const ComplexMomentum & p1)
  {
    return p1;
  }
  inline ComplexMomentum operator -(const ComplexMomentum & p1)
  {
    return ComplexMomentum(-p1.data[0],-p1.data[1],-p1.data[2],-p1.data[3]);
  }

  // Binary operator +
  inline ComplexMomentum operator+(const ComplexMomentum & p1,
                                   const ComplexMomentum & p2)
  {
    return ComplexMomentum(p1.data[0] + p2.data[0],
                           p1.data[1] + p2.data[1],
                           p1.data[2] + p2.data[2],
                           p1.data[3] + p2.data[3]);
  }
  inline ComplexMomentum operator+(const RealMomentum & p1,
                                   const ComplexMomentum & p2)
  {
    return ComplexMomentum(p1.data[0] + p2.data[0],
                           p1.data[1] + p2.data[1],
                           p1.data[2] + p2.data[2],
                           p1.data[3] + p2.data[3]);
  }
  inline ComplexMomentum operator+(const ComplexMomentum & p1,
                                   const RealMomentum & p2)
  {
    return ComplexMomentum(p1.data[0] + p2.data[0],
                           p1.data[1] + p2.data[1],
                           p1.data[2] + p2.data[2],
                           p1.data[3] + p2.data[3]);
  }

  // Binary operator -
  inline ComplexMomentum operator-(const ComplexMomentum & p1,
                                   const ComplexMomentum & p2)
  {
    return ComplexMomentum( p1.data[0] - p2.data[0],
                            p1.data[1] - p2.data[1],
                            p1.data[2] - p2.data[2],
                            p1.data[3] - p2.data[3]);
  }
  inline ComplexMomentum operator-(const RealMomentum & p1,
                                   const ComplexMomentum & p2)
  {
    return ComplexMomentum(p1.data[0] - p2.data[0],
                           p1.data[1] - p2.data[1],
                           p1.data[2] - p2.data[2],
                           p1.data[3] - p2.data[3]);
  }
  inline ComplexMomentum operator-(const ComplexMomentum & p1,
                                   const RealMomentum & p2)
  {
    return ComplexMomentum(p1.data[0] - p2.data[0],
                           p1.data[1] - p2.data[1],
                           p1.data[2] - p2.data[2],
                           p1.data[3] - p2.data[3]);
  }

  // Binary operator *
  inline ComplexMomentum operator *(const ComplexMomentum & p,
                                    const Complex & s )
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }
  inline ComplexMomentum operator *(const Complex & s,
                                    const ComplexMomentum & p)
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }
  inline ComplexMomentum operator *(const ComplexMomentum & p,
                                    const Real & s )
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }
  inline ComplexMomentum operator *(const Real & s,
                                    const ComplexMomentum & p)
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }
  inline ComplexMomentum operator *(const RealMomentum & p,
                                    const Complex & s )
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }
  inline ComplexMomentum operator *(const Complex & s,
                                    const RealMomentum & p)
  {
    return ComplexMomentum(s*p.data[0], s*p.data[1],
                           s*p.data[2], s*p.data[3]);
  }

  // Binary operator /
  inline ComplexMomentum operator /(const ComplexMomentum & p,
                                    const Complex & s )
  {
    return ComplexMomentum( p.data[0]/s, p.data[1]/s,
                            p.data[2]/s, p.data[3]/s);
  }
  inline ComplexMomentum operator /(const ComplexMomentum & p,
                                    const Real & s )
  {
    return ComplexMomentum(p.data[0]/s, p.data[1]/s,
                           p.data[2]/s, p.data[3]/s);
  }

  // Scalar Minkowski-product p1.p2
  inline Complex mp(const ComplexMomentum & p1, const ComplexMomentum & p2)
  {
    return p1.data[0]*p2.data[0]
      -p1.data[1]*p2.data[1] - p1.data[2]*p2.data[2] - p1.data[3]*p2.data[3];
  }
  // Scalar Minkowski-product p1.p2
  inline Complex mp(const RealMomentum & p1, const ComplexMomentum & p2)
  {
    return p1.data[0]*p2.data[0]
      -p1.data[1]*p2.data[1] - p1.data[2]*p2.data[2] - p1.data[3]*p2.data[3];
  }
  // Scalar Minkowski-product p1.p2
  inline Complex mp(const ComplexMomentum & p1, const RealMomentum & p2)
  {
    return p1.data[0]*p2.data[0]
      -p1.data[1]*p2.data[1] - p1.data[2]*p2.data[2] - p1.data[3]*p2.data[3];
  }

  // Square p^2 = p.p (with Minkowski metric)
  inline Complex mp2(const ComplexMomentum & p){
    // consider (p0+p3)*(p0-p3)-p1*p1-p2*p2 --> same n. of operations
    // but 1 less multiplication
    return p.data[0]*p.data[0]
      -p.data[1]*p.data[1] - p.data[2]*p.data[2] - p.data[3]*p.data[3];
  }

  // Real part of a complex momentum
  inline RealMomentum real(const ComplexMomentum & p)
  {
    return RealMomentum(ninja::real(p[0]), ninja::real(p[1]),
                        ninja::real(p[2]), ninja::real(p[3]));
  }
  
  // Immaginary part of a complex momentum
  inline RealMomentum imag(const ComplexMomentum & p)
  {
    return RealMomentum(ninja::imag(p[0]), ninja::imag(p[1]),
                        ninja::imag(p[2]), ninja::imag(p[3]));
  }
  
  // Complex conjugate of a complex momentum
  inline ComplexMomentum conj(const ComplexMomentum & p)
  {
    return ComplexMomentum(ninja::conj(p[0]), ninja::conj(p[1]),
                           ninja::conj(p[2]), ninja::conj(p[3]));
  }

} // namespace ninja

#endif // NINJA_MOMENTUM_HH
