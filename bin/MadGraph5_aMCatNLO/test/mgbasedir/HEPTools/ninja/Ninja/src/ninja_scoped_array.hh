// A dynamic scoped array, which automatically deletes the allocated
// memory when it goes out of scope.


#ifndef NINJA_SCOPED_ARRAY_HH
#define NINJA_SCOPED_ARRAY_HH

#include <new>

namespace ninja {

  template<typename T>
  class ScopedArray {

  public:

    explicit ScopedArray(std::size_t n) : data_(new T[n]) {}

    ~ScopedArray()
    {
      delete [] data_;
    }

    const T & operator[]( std::size_t i) const
    {
      return data_[i];
    }

    T & operator[]( std::size_t i)
    {
      return data_[i];
    }

    const T * data() const
    {
      return data_;
    }

    T * data()
    {
      return data_;
    }

  private:

    // disable copy and assignment
    ScopedArray(ScopedArray const &);
    ScopedArray & operator=(ScopedArray const &);

  private:

    T * data_;

  }; // template <typename T> class ScopedArray

} // namespace ninja


#endif // NINJA_SCOPED_ARRAY_HH
