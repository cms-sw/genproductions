#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cuts_utils.hh>
#include <basis.hh>
#include <tmp_utils.hh>
using namespace ninja;

namespace ninja {
  namespace cuts_utils {


    ////////////////
    // Partitions //
    ////////////////

    // This functions writes the Partition cp complementary to the
    // Partition p of dim dimp with respect to the set {0,...,n-1}
    void complementaryPartition (const PartitionInt p [],
                                 const int dimp, const int n,
                                 PartitionInt * cp)
    {
      int ncp = 0;
      for(int i =0;i<p[0];++i){ cp[ncp] = i; ++ncp;}
      for(int j =0;j<dimp-1;++j)
           for(int i =p[j]+1;i<p[j+1];++i){ cp[ncp] = i; ++ncp;}
      for(int i =p[dimp-1]+1;i<n;++i){ cp[ncp] = i; ++ncp;}
      //
      // TODO: consider replacing with:
      //
      // for (int i=0; i < n; ++i) {
      //     if (i != *p) {
      //       (*cp) = i;
      //       ++cp;
      //     }
      //     else ++p;
      // }
    }


    // This function checks whether a Partition 'a' of dimension 4 is a
    // subset of another Partition 'b' of dimension 5
    bool isSubPartition4of5(const PartitionInt a[], const PartitionInt b[])
    {
      if(a[0]==b[0])
        if(a[1]==b[1])
          if(a[2]==b[2])
            if(a[3]==b[3] || a[3]==b[4]) return true;
            else return false;
          else if (a[2]==b[3] && a[3]==b[4]) return true;
          else return false;
        else if (a[1]==b[2] && a[2]==b[3] && a[3]==b[4]) return true;
        else return false;
      else if (a[0]==b[1] && a[1]==b[2] && a[2]==b[3] && a[3]==b[4]) return true;
      else return false;
    }

    // This function checks whether a Partition 'a' of dimension 2 is a
    // subset of another Partition 'b' of dimension 3 and in that case
    // returns b-a in (the first entry of) c
    bool isSubPartition2of3(const PartitionInt a[], const PartitionInt b[],
                            PartitionInt c[])
    {
      if(a[0]==b[0]){
        if(a[1]==b[1]){
          c[0] = b[2];
          return true;
        }
        else if(a[1]==b[2]){
          c[0] = b[1];
          return true;
        }
        else return false;
      }
      else if(a[0]==b[1] && a[1]==b[2]){
        c[0] = b[0];
        return true;
      }
      else return false;
    }

    // This function checks whether a Partition 'a' of dimension 1 is a
    // subset of another Partition 'b' of dimension 3 and in that case
    // returns b-a in c
    bool isSubPartition1of3(const PartitionInt a[], const PartitionInt b[],
                            PartitionInt c[])
    {
      if(a[0]==b[0]){
        c[0] = b[1];
        c[1] = b[2];
        return true;
      }
      else if(a[0]==b[1]){
        c[0] = b[0];
        c[1] = b[2];
        return true;
      }
      else if(a[0]==b[2]){
        c[0] = b[0];
        c[1] = b[1];
        return true;
      }
      else return false;
    }

    // This function checks whether a Partition 'a' of dimension 1 is a
    // subset of another Partition 'b' of dimension 2 and in that case
    // returns b-a in (the first entry of) c
    bool isSubPartition1of2(const PartitionInt a[], const PartitionInt b[],
                            PartitionInt c[])
    {
      if(a[0]==b[0]){
        c[0] = b[1];
        return true;
      }
      else if(a[0]==b[1]){
        c[0] = b[0];
        return true;
      }
      else return false;
    }


    namespace detail {

      template <unsigned CUT> struct SizeHelper;

      template <>
      struct SizeHelper<3> {
        std::size_t operator()(std::size_t i)
        {
          return 1 + i/2;
        }
      };

      template <>
      struct SizeHelper<2> {
        std::size_t operator()(std::size_t i)
        {
          return 1 + i + i/2 + i/3;
        }
      };

      // Object which iterates through the terms of a Laurent
      // expansion in t (each term is an array of coefficients which
      // multiply the same power of t but different powers of the
      // other free variables).  It is used as an hepler class for the
      // polynomial division algorithm
      template <unsigned CUT>
      class NumExpansionIter {

      public:
        NumExpansionIter () : data_(0), i_(0) {}

        //NumExpansionIter (const NumExpansionIter & it)
        //  : data_(it.data_), i_(it.i_) {}

        explicit NumExpansionIter (Complex * ptr) : data_(ptr), i_(0) {}

        std::size_t size() const
        {
          return SizeHelper<CUT>()(i_);
        }

        NumExpansionIter next() const
        {
          return NumExpansionIter(data_ + size(), i_+1);
        }

        // go to next power of t
        NumExpansionIter & operator ++()
        {
          data_ += size();
          ++i_;
          return *this;
        }

        void divide(const Complex & z)
        {
          for (unsigned int j=0; j < size(); ++j)
            data_[j] /= z;
        }

        void subtractD1 (const DenExp<3> & z, const NumExpansionIter & oth);
        void subtractD2 (const DenExp<3> & z, const NumExpansionIter & oth);

        void subtractD1 (const DenExp<2> & z, const NumExpansionIter & oth);
        void subtractD2 (const DenExp<2> & z, const NumExpansionIter & oth);
 
      private:

        NumExpansionIter (Complex * ptr, std::size_t el) : data_(ptr), i_(el) {}

        Complex * data_;
        std::size_t i_;
      };

      // For triangles the numExpansions at fixed t are just
      // expansions in mu^2.  den.d1 = {t^0}
      template <unsigned NCUT> inline void
      NumExpansionIter<NCUT>::subtractD1 (const DenExp<3> & den,
                                          const NumExpansionIter & oth)
      {
        const Complex z = den.d1;
        const Complex * vec = oth.data_;
        for (unsigned int j=0; j < oth.size(); ++j)
          data_[j] -= z*vec[j];
      }

      // For triangles the numExpansions at fixed t are just
      // expansions in mu^2.  den.d2 = {t^-1, mu^2*t^-1}
      template <unsigned NCUT> inline void
      NumExpansionIter<NCUT>::subtractD2 (const DenExp<3> & den,
                                          const NumExpansionIter & oth)
      {
        const Complex * z = den.d2;
        const Complex * vec = oth.data_;
        for (unsigned int j=0; j < oth.size(); ++j) {
          data_[j] -= z[0]*vec[j];
          data_[j+1] -= z[1]*vec[j];
        }
      }


      // This is called when: oth.i_ == i_-2.  Based on the following
      // table:
      //
      // i_ = 0 --> {t^r}
      // i_ = 1 --> {t^(r-1), t^(r-1)*x}
      // i_ = 2 --> {t^(r-2), t^(r-2)*mu^2, t^(r-2)*x, t^(r-2)*x^2}
      // i_ = 3 --> {t^(r-3), t^(r-3)*mu^2, t^(r-3)*x, t^(r-3)*x*mu^2,
      //             t^(r-3)*x^2, t^(r-3)*x^3}
      //
      // while den.d1 = {t^0, t^0*x}
      template <unsigned NCUT> inline void
      NumExpansionIter<NCUT>::subtractD1 (const DenExp<2> & den,
                                          const NumExpansionIter & oth)
      {
        const Complex * z = den.d1;
        const Complex * vec = oth.data_;
        //const std::size_t s = oth.size();
        
        data_[0] -= z[0]*vec[0];
        switch (i_) {
        case 1:
          data_[1] -= z[1]*vec[0];
          break;
        case 2:
          data_[2] -= z[0]*vec[1];
          data_[2] -= z[1]*vec[0];
          data_[3] -= z[1]*vec[1];
          break;
#ifdef NINJA_X1RANK
        case 3:
          data_[1] -= z[0]*vec[1];
          data_[2] -= z[0]*vec[2];
          data_[4] -= z[0]*vec[3];
          data_[2] -= z[1]*vec[0];
          data_[3] -= z[1]*vec[1];
          data_[4] -= z[1]*vec[2];
          data_[5] -= z[1]*vec[3];
          break;
#endif // NINJA_X1RANK
        }
      }


      // This is called when: oth.i_ == i_-2.  Based on the following
      // table:
      //
      // i_ = 0 --> {t^r}
      // i_ = 1 --> {t^(r-1), t^(r-1)*x}
      // i_ = 2 --> {t^(r-2), t^(r-2)*mu^2, t^(r-2)*x, t^(r-2)*x^2}
      // i_ = 3 --> {t^(r-3), t^(r-3)*mu^2, t^(r-3)*x, t^(r-3)*x*mu^2,
      //             t^(r-3)*x^2, t^(r-3)*x^3}
      //
      // while den.d2 = {t^-1, mu^2*t^-1, x*t^-1, x^2*t^-1}
      template <unsigned NCUT> inline void
      NumExpansionIter<NCUT>::subtractD2 (const DenExp<2> & den,
                                          const NumExpansionIter & oth)
      {
        const Complex * z = den.d2;
        const Complex * vec = oth.data_;
        //const std::size_t s = oth.size();
        
        data_[0] -= z[0]*vec[0];
#ifndef NINJA_X1RANK
        data_[1] -= z[1]*vec[0];
        data_[2] -= z[2]*vec[0];
        data_[3] -= z[3]*vec[0];
#else // NINJA_X1RANK
        // higher rank
        if (i_==2) {
          data_[1] -= z[1]*vec[0];
          data_[2] -= z[2]*vec[0];
          data_[3] -= z[3]*vec[0];
        } else {
          data_[1] -= z[1]*vec[0];
          data_[2] -= z[2]*vec[0];
          data_[4] -= z[3]*vec[0];
          data_[2] -= z[0]*vec[1];
          data_[3] -= z[1]*vec[1];
          data_[4] -= z[2]*vec[1];
          data_[5] -= z[3]*vec[1];
        }
#endif // NINJA_X1RANK
      }
      
    } // namespace detail

    using namespace detail;


    ////////////////////////
    // Polynomial-related //
    ////////////////////////
    template <unsigned CUT>
    void divpolyby (Complex num_in[], int nterms, const DenExp<CUT> & den)
    {
      NumExpansionIter<CUT> num(num_in);
      NumExpansionIter<CUT> next, next2;
      for (int i =0; i<nterms; ++i, ++num) {
        num.divide(den.d0);
        if (i+1<nterms) {
          next = num.next();
          next.subtractD1(den,num);
          if (i+2<nterms) {
            next2 = next.next();
            next2.subtractD2(den,num);
          }
        }
      }
    }

    template void
    divpolyby<2> (Complex num_in[], int nterms, const DenExp<2> & den);
    template void
    divpolyby<3> (Complex num_in[], int nterms, const DenExp<3> & den);


    //////////
    // Cuts //
    //////////

    // pentagons
    template<typename MassType>
    void CutPentagon<MassType>::getLoopMomentum(ComplexMomentum & l,
                                                Complex & muq)
    {
      typedef typename details::common_type<MassType,Real>::type AbbrType;

      // find x1 and x2
      // Real e12 = TWO*e.mp12();
      // Real k1q = mp2(k1), k2q = mp2(k2), k3q = mp2(k3), k5q = mp2(k5);
      // AbbrType m51 = m5q-m1q-k1q, m21 = m2q-m1q-k2q;
      // Complex x1 = (m21-e.beta()*m51)/e12;
      // Complex x2 = (e.alpha()*m21-m51)/e12;

      Real k1q = mp2(k1), k2q = mp2(k2), k3q = mp2(k3), k5q = mp2(k5);
      //Real e12 = TWO*e.mp12();
      AbbrType abbr2 = m1q - m5q + k1q;
      AbbrType abbr4 = m1q - m2q + k2q;
      Complex mpee1k1 = mp(e.e1,k1), mpee1k2 = mp(e.e1,k2),
          mpee2k1 = mp(e.e2,k1), mpee2k2 = mp(e.e2,k2);
      // x1 and x2
      Complex x1 = -HALF*(mpee2k2*abbr2 +mpee2k1*abbr4)
          /(mpee1k2*mpee2k1 - mpee1k1*mpee2k2);
      Complex x2 = HALF*(mpee1k2*abbr2 + mpee1k1*abbr4)
          /(mpee1k2*mpee2k1 - mpee1k1*mpee2k2);

      // find x3 and x4 (i.e. x[2] and x[3])
      Complex Y3 = m3q-m2q-k3q - TWO*mp(k2,k3) - TWO*mp(e.e2,k3)*x2 - TWO*mp(e.e1,k3)*x1;
      Complex Y4 = m4q-m5q-k5q - TWO*mp(k1,k5) + TWO*mp(e.e2,k5)*x2 + TWO*mp(e.e1,k5)*x1;
      Complex ek33 = TWO*mp(e.e3,k3), ek43 = TWO*mp(e.e4,k3);
      Complex ek35 = TWO*mp(e.e3,k5), ek45 = TWO*mp(e.e4,k5);
      Complex det = ek35*ek43-ek33*ek45;
      Complex x3 = (-ek45*Y3-ek43*Y4)/det;
      Complex x4 = (ek35*Y3+ek33*Y4)/det;

      // solution
      l = x1*e.e1 + x2*e.e2 + x3*e.e3 + x4*e.e4;
      muq = mp2(l)-m1q;

    }

    // boxes
    template<typename MassType>
    void CutBox<MassType>::getLoopMomentum(ComplexMomentum & lp,
                                           ComplexMomentum & lm)
    {
      typedef typename details::common_type<MassType,Real>::type AbbrType;

      Real MK11 = mp2(k1);
      Real MK22 = mp2(k2);
      Real beta = ONE/(ONE-e.r1*e.r2);
      AbbrType A1 = (m1q-m4q+MK11)/TWO/e.mp12();
      AbbrType A2 = (m2q-m1q-MK22)/TWO/e.mp12();
      AbbrType x1 = beta*(A2-e.r2*A1);
      AbbrType x2 = beta*(A1-e.r1*A2);

      RealMomentum k23 = k2+k3;
      Real MK33 = mp2(k23);
      Real ME31 = mp(k23,e.e1);
      Real ME32 = mp(k23,e.e2);
      Complex ME33 = mp(k23,e.e3);
      Complex ME34 = mp(k23,e.e4);
      Complex A3 = (m3q-m1q-MK33-TWO*x1*ME31-TWO*x2*ME32)/TWO/ME33;
      Complex A4 = -ME34/ME33;
      Complex B1 = -TWO*e.mp12()*A3;
      Complex B2 = -TWO*e.mp12()*A4;
      AbbrType B0 = (TWO*x1*x2*e.mp12()-m1q);
      Complex rtdel = sqrt(B1*B1-FOUR*B0*B2);
      Complex x41 = (-B1+rtdel)/TWO/B2;
      Complex x42 = (-B1-rtdel)/TWO/B2;
      Complex x31 = x41*A4+A3;
      Complex x32 = x42*A4+A3;
      lp = x1*e.e1 + x2*e.e2 + x31*e.e3 + x41*e.e4;
      lm = x1*e.e1 + x2*e.e2 + x32*e.e3 + x42*e.e4;
    }


    // triangles
    template<typename MassType>
    void CutTriangle<MassType>::getLoopMomentum(ComplexMomentum & amu,
                                                Complex & param)
    {
      typedef typename details::common_type<MassType,Real>::type AbbrType;

      Real MK11 = mp2(k1);
      Real MK22 = mp2(k2);
      Real beta = ONE/(ONE-e.r1*e.r2);
      AbbrType A1 = (m1q-m3q+MK11)/TWO/e.mp12();
      AbbrType A2 = (m2q-m1q-MK22)/TWO/e.mp12();
      AbbrType x1 = beta*(A2-e.r2*A1);
      AbbrType x2 = beta*(A1-e.r1*A2);
      amu = x1*e.e1 + x2*e.e2;

      // q = a + e3*t + (param + muq)/t * (0.5*e4/e3.e4);
      param = (m1q-TWO*e.mp12()*x1*x2);
    }


    // bubbles
    template<typename MassType>
    void CutBubble<MassType>::getLoopMomentum(ComplexMomentum & a0,
                                              ComplexMomentum & a1,
                                              Complex param[3])
    {
      // useful products
      Real kq = mp2(k);
      Real e12 = TWO*e.mp12();
      Real e1k = TWO*mp(e.e1,k);
      Real e2k = TWO*mp(e.e2,k);
      // solutions
      // x2 = b0 + b1*x 
      Complex b0 = (-m2q+m1q+kq)/e2k;
      Real b1 = -e1k/e2k;
      a0 = b0*e.e2;
      a1 = e.e1+b1*e.e2;
      param[0] = toCmplx(m1q);
      param[1] = -b0*e12;
      param[2] = -b1*e12;
    }


    // template<typename MassType> void CutTadpole<MassType>::getLoopMomentum(...)
    // is inlined in cuts_utils-inl.hh


    // template instantiations

    template class CutPentagon<RealMasses>;
    template class CutPentagon<ComplexMasses>;

    template class CutBox<RealMasses>;
    template class CutBox<ComplexMasses>;

    template class CutTriangle<RealMasses>;
    template class CutTriangle<ComplexMasses>;

    template class CutBubble<RealMasses>;
    template class CutBubble<ComplexMasses>;

    template class CutTadpole<RealMasses>;
    template class CutTadpole<ComplexMasses>;

#ifdef NINJA_MASSLESS
    template class CutPentagon<Massless>;
    template class CutBox<Massless>;
    template class CutTriangle<Massless>;
    template class CutBubble<Massless>;
    template class CutTadpole<Massless>;
#endif

  } // namespace cuts_utils
} // namespace ninja
