#ifndef NINJA_CUTS_UTILS_HH
#define NINJA_CUTS_UTILS_HH

#include <basis.hh>
#include <ninja/num_defs.hh>

namespace ninja {
  namespace cuts_utils {

    // Denominator expansions
    template <unsigned CUT> struct DenExp;
    
    template <>
    struct DenExp<3> {
      // D = d0*t +d1 + (d2[0]+d2[1]*mu2)/t
      Complex d0;
      Complex d1;
      Complex d2[2];
    };

    template <>
    struct DenExp<2> {
      // D = d0*t + (d1[0]+d1[1]*x)
      //     + (d2[0]+d2[1]*mu2+d2[2]*x+d2[3]*x^2)/t
      Complex d0;
      Complex d1[2];
      Complex d2[4];
    };


    inline Real gram_det(const RealMomentum & k1, const RealMomentum & k2)
    {
      Real k12 = mp(k1,k2);
      return mp2(k1)*mp2(k2) - k12*k12;
    }


    ////////////////
    // Partitions //
    ////////////////

    // This function writes the Partition cp complementary to the
    // Partition p of dim dimp with respect to the set {0,...,n-1}
    void complementaryPartition (const ninja::PartitionInt p [],
                                 const int dimp, const int n,
                                 ninja::PartitionInt * cp);

    // This function checks whether a Partition 'a' of dimension 4 is a
    // subset of another Partition 'b' of dimension 5
    bool isSubPartition4of5(const ninja::PartitionInt a[],
                            const ninja::PartitionInt b[]);

    // This function checks whether a Partition 'a' of dimension 2 is a
    // subset of another Partition 'b' of dimension 3 and in that case
    // returns b-a in (the first entry of) c
    bool isSubPartition2of3(const ninja::PartitionInt a[],
                            const ninja::PartitionInt b[],
                            ninja::PartitionInt c[]);

    // This function checks whether a Partition 'a' of dimension 1 is a
    // subset of another Partition 'b' of dimension 3 and in that case
    // returns b-a in c
    bool isSubPartition1of3(const ninja::PartitionInt a[],
                            const ninja::PartitionInt b[],
                            ninja::PartitionInt c[]);

    // This function checks whether a Partition 'a' of dimension 1 is a
    // subset of another Partition 'b' of dimension 2 and in that case
    // returns b-a in (the first entry of) c
    bool isSubPartition1of2(const ninja::PartitionInt a[],
                            const ninja::PartitionInt b[],
                            ninja::PartitionInt c[]);



    ////////////////////////
    // Polynomial-related //
    ////////////////////////
    template <unsigned CUT>
    void divpolyby (Complex num_in[], int nterms, const DenExp<CUT> & den);


    //////////
    // Cuts //
    //////////

    // The following classes correspond to cuts. They all have a
    // method getLoopMomentum which is used to determine the solutions
    // of the corresponding multiple cuts.  (A better description of
    // the solutions and their parametrization can be found in
    // ninja_implem.cxx, where this method is called.)

    // pentagons
    template<typename MassType>
    class CutPentagon {
    public:
      CutPentagon(const RealMomentum & k1in, const RealMomentum & k2in,
                  const RealMomentum & k3in, const RealMomentum & k5in,
                  const ninja::Basis & ein,
                  const MassType & m1qin, const MassType & m2qin,
                  const MassType & m3qin, const MassType & m4qin,
                  const MassType & m5qin)
        : k1(k1in), k2(k2in), k3(k3in), k5(k5in), e(ein),
          m1q(m1qin), m2q(m2qin), m3q(m3qin), m4q(m4qin), m5q(m5qin) {}
      void getLoopMomentum(ComplexMomentum & l, Complex & muq);
    private:
      const RealMomentum & k1;
      const RealMomentum & k2;
      const RealMomentum & k3;
      const RealMomentum & k5;
      const ninja::Basis & e;
      const MassType & m1q;
      const MassType & m2q;
      const MassType & m3q;
      const MassType & m4q;
      const MassType & m5q;
    };

    // boxes
    template<typename MassType>
    class CutBox {
    public:
      CutBox(const RealMomentum & k1in, const RealMomentum & k2in,
             const RealMomentum & k3in,
             const ninja::Basis & ein,
             const MassType & m1qin, const MassType & m2qin,
             const MassType & m3qin, const MassType & m4qin)
        : k1(k1in), k2(k2in), k3(k3in), e(ein),
          m1q(m1qin), m2q(m2qin), m3q(m3qin), m4q(m4qin) {}
      void getLoopMomentum(ComplexMomentum & lp, ComplexMomentum & lm);
    private:
      const RealMomentum & k1;
      const RealMomentum & k2;
      const RealMomentum & k3;
      const ninja::Basis & e;
      const MassType & m1q;
      const MassType & m2q;
      const MassType & m3q;
      const MassType & m4q;
    };

    // triangles
    template<typename MassType>
    class CutTriangle {
    public:
      CutTriangle(const RealMomentum & k1in, const RealMomentum & k2in,
                  const ninja::Basis & ein,
                  const MassType & m1qin, const MassType & m2qin,
                  const MassType & m3qin)
        : k1(k1in), k2(k2in), e(ein), m1q(m1qin), m2q(m2qin), m3q(m3qin) {}
      void getLoopMomentum(ComplexMomentum & amu, Complex & param);
    private:
      const RealMomentum & k1;
      const RealMomentum & k2;
      const ninja::Basis & e;
      const MassType & m1q;
      const MassType & m2q;
      const MassType & m3q;
    };

    // bubbles
    template<typename MassType>
    class CutBubble {
    public:
      CutBubble(const RealMomentum & kin,
                const ninja::Basis & ein,
                const MassType & m1qin, const MassType & m2qin)
        : k(kin), e(ein), m1q(m1qin), m2q(m2qin) {}
      void getLoopMomentum(ComplexMomentum & a0, ComplexMomentum & a1,
                           Complex params[]);
    private:
      const RealMomentum & k;
      const ninja::Basis & e;
      const MassType & m1q;
      const MassType & m2q;
    };

    // tadpoles
    template<typename MassType>
    class CutTadpole {
    public:
      CutTadpole(const ninja::Basis & ein,
                 const MassType & mqin)
        : e(ein), mq(mqin) {}
      void getLoopMomentum(const Real & x1, const Real & x2,
                           Complex & X);
    private:
      const ninja::Basis & e;
      const MassType & mq;
    };


  } // namespace cuts_utils
} // namespace ninja

#include <cuts_utils-inl.hh>

#endif //  NINJA_CUTS_UTILS_HH
