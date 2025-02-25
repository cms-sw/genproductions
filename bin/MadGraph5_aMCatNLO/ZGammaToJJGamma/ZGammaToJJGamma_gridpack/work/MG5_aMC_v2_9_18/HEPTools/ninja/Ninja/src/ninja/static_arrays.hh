// -*- C++ -*- wrapper for static arrays.


#ifndef NINJA_STATIC_ARRAYS_HH
#define NINJA_STATIC_ARRAYS_HH


#if !defined(HAVE_INIT_LIST) && !defined(NINJA_CONFIG_H_INTERNAL)
#  if defined(__clang__)
#    if __has_feature(cxx_generalized_initializers)
#      define HAVE_INIT_LIST 1
#    endif
#  elif (defined __INTEL_COMPILER && __INTEL_COMPILER >= 1400) || (defined __ICC && __ICC >= 1400)
#      define HAVE_INIT_LIST 1
#  elif defined(__GNUC__) && defined(__GNUC_MINOR__)
#    if (__GNUC__ > 4 ||  (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)) && defined(__GXX_EXPERIMENTAL_CXX0X__)
#      define HAVE_INIT_LIST 1
#    endif
#  endif
#endif


#if HAVE_INIT_LIST

// Definition of a wrapper around 3 and 4-dimensional static arrays,
// initialized with a C++11 initializer list.

namespace ninja {
  namespace details {

    template<typename T>
    struct Array3D {

      Array3D(const T & v0, const T & v1, const T & v2)
        : data{v0,v1,v2} {}

      const T operator[] (unsigned i) const
      {
        return data[i];
      }

      T & operator[] (unsigned i)
      {
        return data[i];
      }

      T data[3];
    };


    template<typename T>
    struct Array4D {

      Array4D(const T & v0, const T & v1, const T & v2, const T & v3)
        : data{v0,v1,v2,v3} {}

      const T & operator[] (unsigned i) const
      {
        return data[i];
      }

      T & operator[] (unsigned i)
      {
        return data[i];
      }

      T data[4];
    };

  } // namespace details
} // namespace ninja

#else //  HAVE_INIT_LIST

// Definition of a wrapper around 3 and 4-dimensional static arrays,
// initialized in the body of the constructor (should only be used
// when C++11 initialization lists are not available).

namespace ninja {
  namespace details {

    template<typename T>
    struct Array3D {

      Array3D(const T & v0, const T & v1, const T & v2)
      {
        data[0]=v0; data[1]=v1; data[2]=v2;
      }

      const T operator[] (unsigned i) const
      {
        return data[i];
      }

      T & operator[] (unsigned i)
      {
        return data[i];
      }

      T data[3];
    };


    template<typename T>
    struct Array4D {

      Array4D(const T & v0)
      {
        data[0]=v0;
      }

      Array4D(const T & v0, const T & v1)
      {
        data[0]=v0; data[1]=v1;
      }

      Array4D(const T & v0, const T & v1, const T & v2, const T & v3)
      {
        data[0]=v0; data[1]=v1; data[2]=v2; data[3]=v3;
      }

      const T & operator[] (unsigned i) const
      {
        return data[i];
      }

      T & operator[] (unsigned i)
      {
        return data[i];
      }

      T data[4];
    };

  } // namespace details
} // namespace ninja

#endif // ! HAVE_INIT_LIST

#endif // NINJA_STATIC_ARRAYS_HH
