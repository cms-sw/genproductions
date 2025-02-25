// This -*- C++ -*- header file contains the definition of the classes
// ZeroFloat and ZeroFloatArray.
//
// ZeroFloat is an empty class which behaves like the floating point
// numer 0.0.  The compiler should be able to automatically set to
// zero some (sub)expressions containing the ZeroFloat type at compile
// time.  It is used for the Masslesss type instantation of the
// Amplitde methods.
//
// ZeroFloatArray is an empty class which behaves like an array of
// ZeroFloat objects.


#ifndef NINJA_ZERO_FLOAT_HH
#define NINJA_ZERO_FLOAT_HH

#include <iostream>

namespace ninja {

  class ZeroFloat {
  public:
    ZeroFloat() {}

    // +0 = 0
    ZeroFloat operator + () const
    {
      return ZeroFloat();
    }
    
    // -0 = 0
    ZeroFloat operator - () const
    {
      return ZeroFloat();
    }
    
  private:
    // no-data
  };


  // Comparisons with 0
  template<typename T>
  inline bool operator == (ZeroFloat, const T & x)
  {
    return (x==T(0));
  }
  template<typename T>
  inline bool operator == (const T & x, ZeroFloat)
  {
    return (x==T(0));
  }
  template<typename T>
  inline bool operator != (ZeroFloat, const T & x)
  {
    return (x!=T(0));
  }
  template<typename T>
  inline bool operator != (const T & x, ZeroFloat)
  {
    return (x!=T(0));
  }
  template<typename T>
  inline bool operator < (ZeroFloat, const T & x)
  {
    return (x>T(0));
  }
  template<typename T>
  inline bool operator < (const T & x, ZeroFloat)
  {
    return (x<T(0));
  }


  // Standard output stream
  inline std::ostream & operator << (std::ostream & os, ZeroFloat)
  {
    return os << "(zero)";
  }


  // The following class can be used in order to mimic an array of zeroes
  class ZeroFloatArray {

  public:

    ZeroFloatArray() {}

    template<typename T> ZeroFloatArray(const T *) {}

    ZeroFloat operator [] (int)
    {
      return ZeroFloat();
    }

  private:
    // no-data
  };


  namespace details {

    // compile-time check: if a type is massless
    template <typename MassType>
    struct MasslessTypeError {};
    template <>
    struct MasslessTypeError<ZeroFloat> {
      static void MassType_must_be_massless() {}
    };

  } // namespace details

} // namespace ninja

#endif // NINJA_ZERO_FLOAT_HH
