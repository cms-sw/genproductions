// This class is used as container for cut objects defined in cuts.hh
// (and xcuts.hh) instead of std::vector, since the latter doesn't
// work with non-copiable objects and we don't need all its
// functionalities.


#ifndef CUTS_VECTOR
#define CUTS_VECTOR

#include <new>

#undef NINJA_NON_COPIABLE
#define NINJA_NON_COPIABLE(MyClass)      \
  MyClass(const MyClass &);                 \
  MyClass & operator= (const MyClass &)


namespace ninja {

  template <typename T>
  class CutsVector {

  private:
    NINJA_NON_COPIABLE(CutsVector);

  public :

    typedef const T * const_iterator;
    typedef T * iterator;

    CutsVector(std::size_t n, int nlegs): base_(n)
    {
      if ((n != 0) && (nlegs > T::legs))
        for (unsigned int i=0; i<n; ++i)
          base_.data_[i].initcp(nlegs);
    }

    ~CutsVector() {}

    iterator begin()
    {
      return base_.data_;
    }

    const_iterator begin() const
    {
      return base_.data_;
    }

    const_iterator end() const
    {
      return base_.data_ + base_.size_;
    }

    std::size_t size() const
    {
      return base_.size_;
    }

    T & operator[] (std::size_t n) const
    {
      return base_.data_[n];
    }

  private :

    struct CutsVectorBase_ {

      CutsVectorBase_(std::size_t n) : data_(0), size_(n)
      {
        if (n)
          data_ = new T[n];
      }

      ~CutsVectorBase_()
      {
        delete [] data_;
      }

      T * data_;
      std::size_t size_;

    };

    CutsVectorBase_ base_;

  };

} // namespace ninja

#endif
