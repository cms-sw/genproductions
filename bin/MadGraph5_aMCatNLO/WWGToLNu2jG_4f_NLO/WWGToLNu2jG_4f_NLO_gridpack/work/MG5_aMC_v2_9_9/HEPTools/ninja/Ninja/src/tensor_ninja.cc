#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

// The coefficients of a tensor numerator are stored in a contigous
// array of complex numbers.  For generic rannk-r functions (with
// terms of rank=0,...,r) we use the representation of
// http://arxiv.org/abs/1405.0301, see in particular Eq. (C.15).  For
// homogeneous tensors (i.e. with rank-r terms only) we use a similar
// representation but removing the lower rank coefficients.  This way,
// a tensor numerator can also be seen as a contigous array of
// homogeneous tensors.

#include <ninja/tensor_ninja.hh>

namespace ninja {

  namespace {

    // ten_coeff(r) is the number of coefficients of a rank-r
    // numerator.  We have: ten_coeff(r) = binomial(r+4,r), and for
    // conveniency we also define ten_coeff(-1) = 0.  This way, the
    // homogeneous terms of rank=r of a tensor t start at
    // t[ten_ncoeff(r-1)] and end at t[ten_ncoeff(r)-1];
    const unsigned ten_ncoeff_array_[] = {0, 1, 5, 15, 35, 70, 126, 210,
                                          330, 495, 715, 1001, 1365, 1820,
                                          2380, 3060, 3876, 4845, 5985,
                                          7315, 8855, 10626};
    inline unsigned ten_ncoeff(unsigned rank)
    {
      return ten_ncoeff_array_[rank+1];
    }

    // ten_coeff_r(r) is the number of coefficient of a homogeneous
    // rank-r tensor.  We have: ten_coeff_r(r) = binomoal(r+3,r).
    const unsigned ten_ncoeff_r_array_[] = {1, 4, 10, 20, 35, 56, 84, 120,
                                            165, 220, 286, 364, 455, 560, 680,
                                            816, 969, 1140, 1330, 1540, 1771};
    inline unsigned ten_ncoeff_r(unsigned rank)
    {
      return ten_ncoeff_r_array_[rank];
    }

    // The following routines are supposed to build higher-rank
    // tensors by multiplying lower-rank tensors with 4-vectors.

    // ten_ncoeff_comp`i'(r) are the number of lower-rank tensor
    // coefficients which should be contracted with the component `i'
    // of the vector when building a rank-r tensor from a rank-(r-1)
    // tensor.  ten_ncoeff_comp0(r) = 1, so it is not defined.

    // ten_ncoeff_comp1(r) = r
    inline unsigned ten_ncoeff_comp1(unsigned rank)
    {
      return rank;
    }

    // ten_ncoeff_comp2(r) = 1/2 r (1 + r)
    const unsigned ten_ncoeff_comp2_array_[] = {0, 1, 3, 6, 10, 15, 21, 28, 36,
                                                45, 55, 66, 78, 91, 105, 120,
                                                136, 153, 171, 190, 210};
    inline unsigned ten_ncoeff_comp2(unsigned rank)
    {
      return ten_ncoeff_comp2_array_[rank];
    }

    // ten_ncoeff_comp3(r) = 1/6 r (1 + r) (2 + r)
    const unsigned ten_ncoeff_comp3_array_[] = {0, 1, 4, 10, 20, 35, 56, 84,
                                                120, 165, 220, 286, 364, 455,
                                                560, 680, 816, 969, 1140, 1330,
                                                1540};
    inline unsigned ten_ncoeff_comp3(unsigned rank)
    {
      return ten_ncoeff_comp3_array_[rank];
    }

    // Build the components of a homogeneous rank-r tensor t0, from
    // t1, which is a rank-(r-1) tensor, and the momentum v
    inline void ten_build(unsigned rank,
                          const Complex * t1, const ComplexMomentum & v,
                          Complex * t0)
    {
      unsigned j=1;
      const unsigned r1 = ten_ncoeff_comp1(rank);
      const unsigned r2 = ten_ncoeff_comp2(rank);
      const unsigned r3 = ten_ncoeff_comp3(rank);

      t0[0] = t1[0]*v[0];

      for (unsigned i = 0; i<r1; ++i, ++j)
        t0[j] = v[1]*t1[i];

      for (unsigned i = 0; i<r2; ++i, ++j)
        t0[j] = v[2]*t1[i];

      for (unsigned i = 0; i<r3; ++i, ++j)
        t0[j] = v[3]*t1[i];
    }

    // Same as before, but the result is added to the current value of
    // t0.
    inline void ten_build_add(unsigned rank,
                              const Complex * t1, const ComplexMomentum & v,
                              Complex * t0)
    {
      unsigned j=1;
      const unsigned r1 = ten_ncoeff_comp1(rank);
      const unsigned r2 = ten_ncoeff_comp2(rank);
      const unsigned r3 = ten_ncoeff_comp3(rank);

      t0[0] += t1[0]*v[0];

      for (unsigned i = 0; i<r1; ++i, ++j)
        t0[j] += v[1]*t1[i];

      for (unsigned i = 0; i<r2; ++i, ++j)
        t0[j] += v[2]*t1[i];

      for (unsigned i = 0; i<r3; ++i, ++j)
        t0[j] += v[3]*t1[i];
    }


    // Build a tensor from a momentum v rank times, i.e. t = v^r
    inline void ten_from_mom(unsigned rank, const ComplexMomentum & v,
                             Complex * t)
    {
      t[0] = 1;
      const Complex * tlower = t;
      ++t;
      for (unsigned r=1; r<=rank; ++r) {
        ten_build(r,tlower,v,t);
        tlower = t;
        t += ten_ncoeff_r(r);
      }
    }

    
    // build n^klr (next^k-to-leading rank) tensor t1 = v0^(r-k) v1^k,
    // from leading tensor t0 = v0^(r-k+1) v1^(k-1), for r=0,...,r_.
    // Assumes r_,k>=1.
    void nlr_ten(unsigned r_, unsigned k,
                 const Complex * t0_,
                 const ComplexMomentum & v0,
                 const ComplexMomentum & v1,
                 Complex * t1_)
    {
      // for r<k, t1_ = 0
      for (unsigned i=0; i<ten_ncoeff(k-1); ++i)
        t1_[i] = 0;

      // for r=k, t1_ is t0_*v1
      Complex * t1 = t1_+ten_ncoeff(k-1);
      const Complex * tlower0 = t0_+ten_ncoeff(k-2);
      ten_build(k,tlower0,v1,t1);
      t1 += ten_ncoeff_r(k);
      tlower0 += ten_ncoeff_r(k-1);

      // for r>k, t1_ = t0_*v1 + t1_*v0
      const Complex * tlower1 = t1_+ten_ncoeff(k-1);
      for (unsigned r=k+1; r<=r_; ++r) {
        ten_build(r,tlower0,v1,t1);
        ten_build_add(r,tlower1,v0,t1);
        tlower0 += ten_ncoeff_r(r-1);
        tlower1 = t1;
        t1 += ten_ncoeff_r(r);
      }
    }

    
    // build a tensor t3 = v0^(r-k-1) v1^k v2, from tensors
    // t1=v0^(r-k-1)v1^k and t2=v0^(r-k-1)v2, for r=0,...,r_.  Assumes
    // r_>=k+1,k>=1
    void n2lr_ten(unsigned r_, unsigned k,
                  const Complex * t1_,
                  const Complex * t2_,
                  const ComplexMomentum & v0,
                  const ComplexMomentum & v1,
                  const ComplexMomentum & v2,
                  Complex * t3_)
    {
      // for r<=k, t3_ = 0
      for (unsigned i=0; i<ten_ncoeff(k); ++i)
        t3_[i] = 0;

      // for r=k+1, t3_ = v1^k * v2 + (v2*v1^(k-1)) * v1
      Complex * t3 = t3_ + ten_ncoeff(k);
      const Complex * t1low = t1_+ten_ncoeff(k-1);
      const Complex * t2low = t2_+ten_ncoeff(k-1);
      ten_build(k+1,t1low,v2,t3);
      ten_build_add(k+1,t2low,v1,t3);
      t1low += ten_ncoeff_r(k);
      t2low += ten_ncoeff_r(k);
      t3 += ten_ncoeff_r(k+1);

      // for r>k+1, t1_ = t1*v2 + t2*v1 + t3*v0
      const Complex * t3low = t3_+ten_ncoeff(k);
      for (unsigned r=k+2; r<=r_; ++r) {
        ten_build(r,t1low,v2,t3);
        ten_build_add(r,t2low,v1,t3);
        ten_build_add(r,t3low,v0,t3);
        t1low += ten_ncoeff_r(r-1);
        t2low += ten_ncoeff_r(r-1);
        t3low = t3;
        t3 += ten_ncoeff_r(r);
      }
    }


    // contract all the components of two tensors from rank=rmin up to
    // rank=rmax
    inline Complex ten_contract(unsigned rmin, unsigned rmax,
                                const Complex * t1, const Complex * t2)
    {
      Complex res = 0;
      const unsigned imin = ten_ncoeff(rmin-1);
      const unsigned imax = ten_ncoeff(rmax);
      for (unsigned i=imin; i<imax; ++i)
        res += t1[i]*t2[i];
      return res;
    }

    // same as above, but with rmin=0
    inline Complex ten_contract(unsigned rmax,
                                const Complex * t1, const Complex * t2)
    {
      return ten_contract(0,rmax,t1,t2);
    }
    
  } // namespace



  TensorNumerator::TensorNumerator(unsigned n, unsigned r, const Complex * num)
    : num_(num),
      t0_(new Complex[ten_ncoeff(r)]),
      t1_(0),
      t2_(0),
      t3_(0),
      t4_(0),
      t5_(0),
      t6_(0),
      t7_(0),
      n_(n), r_(r)
  {
    if (r_ >= n_-2)
      t1_ = new Complex[ten_ncoeff(r)];
    if (r_ >= n_-1) {
      t2_ = new Complex[ten_ncoeff(r)];
      t3_ = new Complex[ten_ncoeff(r)];
    }
    if (r_ >= n_) {
      t4_ = new Complex[ten_ncoeff(r)];
      t5_ = new Complex[ten_ncoeff(r)];
      t6_ = new Complex[ten_ncoeff(r)];
    }
    if (r_ >= n_+1) {
      t7_ = new Complex[ten_ncoeff(r)];
    }
  }
  
  
  Complex TensorNumerator::evaluate(const ninja::ComplexMomentum & q,
                                    const ninja::Complex & muq,
                                    int cut, const ninja::PartitionInt part[])
  {
    (void)(muq); (void)(cut); (void)(part);

    ten_from_mom(r_,q,t0_);
    return ten_contract(r_,num_,t0_);
  }

  
  void TensorNumerator::muExpansion(const ninja::ComplexMomentum v[],
                                    const ninja::PartitionInt part[],
                                    ninja::Complex c[])
  {
    (void)(part);

    ten_from_mom(r_,v[0],t0_);
    c[0] = ten_contract(r_,r_,num_,t0_);

    if (r_ >= n_+1) {

      nlr_ten(r_,1,t0_,v[0],v[1],t1_);

      c[1] = ten_contract(r_,r_,num_,t1_);
      c[1] += ten_contract(r_-1,r_-1,num_,t0_);
      
    }
  }

  
  void TensorNumerator::t3Expansion(const ninja::ComplexMomentum & a,
                                    const ninja::ComplexMomentum & e3,
                                    const ninja::ComplexMomentum & e4,
                                    const ninja::Complex & param,
                                    int mindeg,
                                    int cut, const ninja::PartitionInt part[],
                                    ninja::Complex c[])
  {
    (void)(cut); (void)(part);
    
    // lr terms
    ten_from_mom(r_,e3,t0_);
    c[0] = ten_contract(r_,r_,num_,t0_);

    
    // nlr terms
    if (mindeg > 0) {

      // t1_ = e3^(r-1) a
      nlr_ten(r_,1,t0_,e3,a,t1_);

      // t^(r-1)
      c[1] = ten_contract(r_,r_,num_,t1_);
      c[1] += ten_contract(r_-1,r_-1,num_,t0_);

      
      // nnlr terms
      if (mindeg > 1) {

        // t2_ = e3^(r-2) a^2
        nlr_ten(r_,2,t1_,e3,a,t2_);
        // t3_ = e3^(r-1) e4
        nlr_ten(r_,1,t0_,e3,e4,t3_);

        Complex term_t2_e4 = ten_contract(r_,r_,num_,t3_);

        // t^(r-2)
        c[2] = ten_contract(r_-2,r_-2,num_,t0_)
          + ten_contract(r_-1,r_-1,num_,t1_)
          + ten_contract(r_,r_,num_,t2_)
          + param * term_t2_e4;

        // t^(r-2)*mu2
        c[3] = term_t2_e4;

        
        // nnnlr terms
        if (mindeg > 2) {
          
          // t4_ = e3^(r-3) a^3
          nlr_ten(r_,3,t2_,e3,a,t4_);
          // t5_ = e3^(r-2) a e4
          n2lr_ten(r_,1,t1_,t3_,e3,a,e4,t5_);

          Complex term_t3_e4 = ten_contract(r_-1,r_-1,num_,t3_)
            + ten_contract(r_,r_,num_,t5_);

          // t^(r-3)
          c[4] = ten_contract(r_-3,r_-3,num_,t0_)
            + ten_contract(r_-2,r_-2,num_,t1_)
            + ten_contract(r_-1,r_-1,num_,t2_)
            + param * term_t3_e4
            + ten_contract(r_,r_,num_,t4_);

          // t^(r-3)*mu2
          c[5] = term_t3_e4;

          
          // nnnnlr terms
          if (mindeg > 3) {

            // note: this is the last epansion term, hence no more
            // bookkeeping is needed.  Therefore we just use t6_ for
            // everything

            // t6_ = e3^(r-4) a^4
            nlr_ten(r_,4,t4_,e3,a,t6_);

            // t^(r-4), parts in e3^(r-k) a^k
            c[6] = ten_contract(r_-4,r_-4,num_,t0_)
              + ten_contract(r_-3,r_-3,num_,t1_)
              + ten_contract(r_-2,r_-2,num_,t2_)
              + ten_contract(r_-1,r_-1,num_,t4_)
              + ten_contract(r_,r_,num_,t6_);

            // t6_ = e3^(r-3) a^2 e4
            n2lr_ten(r_,2,t2_,t5_,e3,a,e4,t6_);

            // t^(r-4) and t^(r-4)*mu2 parts in e4
            Complex term_t4_e4 = ten_contract(r_-2,r_-2,num_,t3_)
              + ten_contract(r_-1,r_-1,num_,t5_)
              + ten_contract(r_,r_,num_,t6_);
            c[6] += param*term_t4_e4;
            c[7] = term_t4_e4;

            // t6_ = e3^(r-2) e4^2
            nlr_ten(r_,2,t3_,e3,e4,t6_);

            // t^(r-4)*mu2^i parts in e4^2
            Complex term_t4_e42 = ten_contract(r_,r_,num_,t6_);
            c[6] += param*param*term_t4_e42;
            c[7] += Real(2)*param*term_t4_e42;
            c[8] = term_t4_e42;
            
          }
          
        }

      }
      
    }
  }

  
  void TensorNumerator::t2Expansion(const ninja::ComplexMomentum & a0,
                                    const ninja::ComplexMomentum & a1,
                                    const ninja::ComplexMomentum & e3,
                                    const ninja::ComplexMomentum & e4,
                                    const ninja::Complex param[],
                                    int mindeg,
                                    int cut, const ninja::PartitionInt part[],
                                    ninja::Complex c[])
  {
    (void)(cut); (void)(part);

    // lr terms
    ten_from_mom(r_,e3,t0_);
    c[0] = ten_contract(r_,r_,num_,t0_);

    // nlr terms
    if (mindeg > 0) {

      // t1_ = e3^(r-1) a0
      nlr_ten(r_,1,t0_,e3,a0,t1_);
      // t2_ = e3^(r-1) a1
      nlr_ten(r_,1,t0_,e3,a1,t2_);

      // t^(r-1)
      c[1] = ten_contract(r_,r_,num_,t1_)
        + ten_contract(r_-1,r_-1,num_,t0_);

      // t^(r-1)*x
      c[2] = ten_contract(r_,r_,num_,t2_);


      // nnlr terms
      if (mindeg > 1) {

        // t3_ = e3^(r-2) a0^2
        nlr_ten(r_,2,t1_,e3,a0,t3_);
        // t4_ = e3^(r-2) a0 a1
        n2lr_ten(r_,1,t1_,t2_,e3,a0,a1,t4_);
        // t5_ = e3^(r-2) a1^2
        nlr_ten(r_,2,t2_,e3,a1,t5_);

        // t6_ = e3^(r-1) e4^1
        nlr_ten(r_,1,t0_,e3,e4,t6_);

        Complex term_t2_e4 = ten_contract(r_,r_,num_,t6_);

        // t^(r-2)
        c[3] = ten_contract(r_-2,r_-2,num_,t0_)
          + ten_contract(r_-1,r_-1,num_,t1_)
          + ten_contract(r_,r_,num_,t3_)
          + param[0] * term_t2_e4;

        // t^(r-2)*mu2
        c[4] = term_t2_e4;

        // t^(r-2)*x
        c[5] = ten_contract(r_-1,r_-1,num_,t2_)
          + ten_contract(r_,r_,num_,t4_)
          + param[1]*term_t2_e4;

        // t^(r-2)*x^2
        c[6] = ten_contract(r_,r_,num_,t5_)
          + param[2]*term_t2_e4;


        // nnnlr terms
        if (mindeg > 2) {

          // note: last term in expansion, hence using t7_ everywhere

          // t7_ = e3^(r-3) a0^3
          nlr_ten(r_,3,t3_,e3,a0,t7_);

          // t^(r-3), parts in e3^(r-k) a0^k
          c[7] = ten_contract(r_-3,r_-3,num_,t0_)
            + ten_contract(r_-2,r_-2,num_,t1_)
            + ten_contract(r_-1,r_-1,num_,t3_)
            + ten_contract(r_,r_,num_,t7_);

          // t7_ = e3^(r-2) a0 e4
          n2lr_ten(r_,1,t1_,t6_,e3,a0,e4,t7_);

          // e4 terms for c[7,8]
          Complex term_t3_e4 = ten_contract(r_,r_,num_,t7_)
            + ten_contract(r_-1,r_-1,num_,t6_);
          c[7] += param[0]*term_t3_e4;
          c[8] = term_t3_e4;

          // t7_ = e3^(r-2) a1 e4
          n2lr_ten(r_,1,t2_,t6_,e3,a1,e4,t7_);

          Complex term_t3_e4x1 = ten_contract(r_,r_,num_,t7_);
          c[9] = ten_contract(r_-2,r_-2,num_,t2_)
            + ten_contract(r_-1,r_-1,num_,t4_)
            + param[0]*term_t3_e4x1 + param[1]*term_t3_e4;
          c[10] = term_t3_e4x1;

          // t7_ = e3^(r-3) a0^2 a1
          n2lr_ten(r_,2,t3_,t4_,e3,a0,a1,t7_);
          c[9] += ten_contract(r_,r_,num_,t7_);

          // t7_ = e3^(r-3) a1^2 a0
          n2lr_ten(r_,2,t5_,t4_,e3,a1,a0,t7_);
          c[11] = ten_contract(r_,r_,num_,t7_)
            + ten_contract(r_-1,r_-1,num_,t5_)
            + param[1]*term_t3_e4x1
            + param[2]*term_t3_e4;

          // t7_ = e3^(r-3) a1^3
          nlr_ten(r_,3,t5_,e3,a1,t7_);
          c[12] = ten_contract(r_,r_,num_,t7_)
            + param[2]*term_t3_e4x1;          

          
        }

      }

    }

    
  }
  
  
} // namespace ninja
