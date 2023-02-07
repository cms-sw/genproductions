// -*- C++ -*- defining S-matrix class for Ninja.  It is used to store
// invariants s[i,j] defined as:
//
//   s[i,j] = (vi[i]-vi[j])^2
//
// where vi[i] is the internal momentum of the i-th loop denominator
//
//   Denom[i] = (q+vi[i])^2 - m[i]^2.
//
// If an S-matrix is provided by the user, the computation of Master
// Integrals by the integral libraries might become more accurate.


#ifndef NINJA_S_MAT_H
#define NINJA_S_MAT_H

#include <ninja/types.hh>

namespace ninja {

  class SMatrix {
  public:

    // Default constructor
    SMatrix() : data_(0), n_rows_(0), owns_data_(false) {}

    // Constructor from number of rows.  This is the only constructor
    // which allocates data.  The destructor will free the allocated
    // memory only if either this constructor or the allocate method
    // was called.
    explicit SMatrix(int n)
      : data_(new Real[n*n]), n_rows_(n), owns_data_(true) {}

    // Constructor from number of rows and pointer to data
    SMatrix(int n, Real * data_ptr)
      : data_(data_ptr), n_rows_(data_ptr ? n : 0), owns_data_(false) {}

    // Copy constructor
    SMatrix(const SMatrix & s_mat)
      : data_(s_mat.data_), n_rows_(s_mat.n_rows_), owns_data_(false) {}

    // Destructor.  It deallocates data only if it was allocated in
    // this instance.  Once this is called, if the instance had been
    // newly-allocated before, all its copies are invalidated.  It is
    // up to the user making sure no such copies survive after this
    // happens.  On the other hand, if the instance was itself a copy
    // no action is performed on the data upon destruction of this
    // instance.
    ~SMatrix()
    {
      if (owns_data_)
        delete [] data_;
    }

    // Assign (equivalent to copy constructor)
    SMatrix & operator= (const SMatrix & s_mat)
    {
      if (owns_data_)
        delete [] data_;
      data_ = s_mat.data_;
      n_rows_ = data_ ? s_mat.n_rows_ : 0;
      owns_data_ = false;
      return *this;
    }

    // Allocate memory for the matrix.  The destructor will later free
    // the allocated memory automatically.
    SMatrix & allocate(int n)
    {
      if (owns_data_)
        delete [] data_;
      n_rows_ = n;
      data_ = new Real[n*n];
      owns_data_ = true;
      return *this;
    }

    //  Does same thing as the destructor
    void clear()
    {
      if (owns_data_)
        delete [] data_;
      data_ = 0;
      owns_data_ = false;
    }

    // copy data from another matrix
    void copy(const SMatrix & s_mat) {
      if (s_mat.data_) {
        allocate(s_mat.n_rows_);
        for (int i=0; i<n_rows_*n_rows_; ++i)
          data_[i] = s_mat.data_[i];
      } else {
        clear();
      }
    }

    // Pointer to data_
    Real * data()
    {
      return data_;
    }

    // Check wheter there's no data_
    bool isNull()
    {
      return (! data_);
    }

    // Fill the whole matrix with the same value
    SMatrix & fill(Real value)
    {
      for (int i=0; i<n_rows_*n_rows_; ++i)
        data_[i] = value;
      return *this;
    }

    // Fill the matrix from the internal kinematics and set to zero
    // elememnts smaller than ir_threshold.
    SMatrix & fillFromKinematics(const RealMomentum pi[],
                                 Real ir_threshold = 0)
    {
      Real temp;
      for (int i=0; i<n_rows_; ++i) {
        (*this)(i,i) = Real();
        for (int j=i+1; j<n_rows_; ++j) {
          temp = mp2(pi[i]-pi[j]);
          (*this)(i,j) = (*this)(j,i) =  abs(temp) < ir_threshold ? 0
                                                     : temp;
        }
      }
      return *this;
    }

    // Element access via s_mat(i,j)
    Real operator() (unsigned i, unsigned j) const
    {
      return data_[i*n_rows_+j];
    }
    Real & operator() (unsigned i, unsigned j)
    {
      return data_[i*n_rows_+j];
    }

  private:

    Real * data_;
    int n_rows_;
    bool owns_data_;

  }; // SMatrix

} // namespace ninja

#endif // NINJA_S_MAT_H
