// Some utils and wrappers used in the ninja-library.


#ifndef NINJA_TMP_UTILS_HH
#define NINJA_TMP_UTILS_HH

#include <ninja/types.hh>


#if HAVE_DECLTYPE

// If decltype() is available, use the C++11 std::common_type to
// choose return type in mixed types operations.

#include <type_traits>

namespace ninja{
  namespace details{

    template<typename T, typename U>
    struct common_type {
      typedef typename std::common_type<T,U>::type type;
    };

  }
}

#else  // HAVE_DECLTYPE

// If decltype() is not available, use some template structs in order
// to choose return type in mixed types operations.

namespace ninja {
  namespace details {

    // Choose return type in mixed floating point operations
    template<typename T, typename U>
    struct common_type;
    template<> struct common_type<ninja::Complex, ninja::Real> {
      typedef ninja::Complex type;
    };
    template<>
    struct common_type<ninja::Real,ninja::Complex> {
      typedef ninja::Complex type;
    };
    template<>
    struct common_type<ninja::Complex,ninja::Complex> {
      typedef ninja::Complex type;
    };
    template<typename T>
    struct common_type<T,T> {
      typedef T type;
    };

  } // namespace details
} // namespace ninja

#endif // ! HAVE_DECLTYPE


namespace ninja {


  // Defining ZeroFloat arithmetic operations


  // 0+0 = 0
  // x+0 = 0
  // 0+x = 0
  inline ZeroFloat operator + (ZeroFloat, ZeroFloat)
  {
    return ZeroFloat();
  }
  template<typename T>
  inline T operator + (const T & x, ZeroFloat)
  {
    return x;
  }
  template<typename T>
  inline T operator + (ZeroFloat, const T & x)
  {
    return x;
  }

  // 0-0 = 0
  // x-0 = x
  // 0-x = -x
  inline ZeroFloat operator - (ZeroFloat, ZeroFloat)
  {
    return ZeroFloat();
  }
  template<typename T>
  inline T operator - (const T & x, ZeroFloat)
  {
    return x;
  }
  template<typename T>
  inline T operator - (ZeroFloat, const T & x)
  {
    return -x;
  }

  // 0*0 = 0
  // 0*x = 0
  // x*0 = 0
  inline ZeroFloat operator * (ZeroFloat, ZeroFloat)
  {
    return ZeroFloat();
  }
  template<typename T>
  inline ZeroFloat operator * (const T &, ZeroFloat)
  {
    return ZeroFloat();
  }
  template<typename T>
  inline ZeroFloat operator * (ZeroFloat, const T &)
  {
    return ZeroFloat();
  }

  // 0/x = 0
  template<typename T>
  inline ZeroFloat operator / (ZeroFloat, const T &)
  {
    return ZeroFloat();
  }


  namespace details {

    // Specialize ninja::details::common_type<...>
    template<typename T>
    struct common_type<ZeroFloat, T> {
      typedef T type;
    };
    template<typename T>
    struct common_type<T,ZeroFloat> {
      typedef T type;
    };
    template<>
    struct common_type<ZeroFloat,ZeroFloat> {
      typedef ZeroFloat type;
    };

  } // namespace details

}

#endif // NINJA_TMP_UTILS_HH
