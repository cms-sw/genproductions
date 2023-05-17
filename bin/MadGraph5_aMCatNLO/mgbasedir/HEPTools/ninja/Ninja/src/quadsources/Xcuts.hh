// This classes store the coefficients and the relevant information of
// each multiple cut, namely its defining partition, the complementary
// partition and the vectors appearing in the polynomial form of its
// residue.  This is the higher-rank version of cuts.hh


#ifndef QUADNINJA_XCUTS_HH
#define QUADNINJA_XCUTS_HH

#include <quadninja/types.hh>
#include <quadninja/num_defs.hh>
#include <quadsources/cuts_vector.hh>


namespace quadninja{
  namespace x1cuts{

    class Pentagon {

    private:

      QUADNINJA_NON_COPIABLE(Pentagon);

    public:

      enum { csize  = 1 };
      enum { legs  = 5 };

      quadninja::PartitionInt p[5];
      quadninja::PartitionInt * cp;
      Complex c[csize];

      Pentagon(): p(), cp(0), c() {}

      ~Pentagon()
      {
        delete [] cp;
      }

      void initcp (int nlegs)
      {
        cp = new PartitionInt[nlegs - legs];
      }

      // Returns the polynomial residue
      Complex poly(const ComplexMomentum &, const Complex & muq) const
      {
        return c[0]*muq;
      }

    };

    typedef CutsVector<Pentagon>::const_iterator PentagonsCIter;
    typedef CutsVector<Pentagon>::iterator PentagonsIter;

    // Prints the coefficients.  If they are smaller than chop_tol
    // they are printed as zeros.
    void print(const CutsVector<Pentagon> & pentagon);



    class Box {

    private:

      QUADNINJA_NON_COPIABLE(Box);

    public:

      enum { csize  = 6 };
      enum { legs  = 4 };

      quadninja::PartitionInt p[4];
      quadninja::PartitionInt * cp;
  
      Complex c[csize];
      RealMomentum V0;
      ComplexMomentum Vort;

      Box(): p(), cp(0), c(), V0(), Vort() {}

      ~Box()
      {
        delete [] cp;
      }

      void initcp (int nlegs)
      {
        cp = new PartitionInt[nlegs - legs];
      }

      // Returns the polynomial residue
      Complex poly(const ComplexMomentum & q, const Complex & muq) const;

    };

    typedef CutsVector<Box>::const_iterator BoxesCIter;
    typedef CutsVector<Box>::iterator BoxesIter;

    // Prints the coefficients.  If they are smaller than chop_tol
    // they are printed as zeros.
    void print(const CutsVector<Box> & box);



    class Triangle {

    private:

      QUADNINJA_NON_COPIABLE(Triangle);

    public:

      enum { csize  = 15 };
      enum { legs  = 3 };

      quadninja::PartitionInt p[3];
      quadninja::PartitionInt * cp;
  
      Complex c[csize];
      RealMomentum V0;
      ComplexMomentum e3, e4;

      Triangle(): p(), cp(0), c(), V0(), e3(), e4() {}

      ~Triangle()
      {
        delete [] cp;
      }

      void initcp (int nlegs)
      {
        cp = new PartitionInt[nlegs - legs];
      }

      // Returns the polynomial residue
      Complex poly(const ComplexMomentum & q, const Complex & muq) const;

    };

    typedef CutsVector<Triangle>::const_iterator TrianglesCIter;
    typedef CutsVector<Triangle>::iterator TrianglesIter;

    // Prints the coefficients.  If they are smaller than chop_tol
    // they are printed as zeros.
    void print(const CutsVector<Triangle> & triangle);



    class Bubble {

    private:

      QUADNINJA_NON_COPIABLE(Bubble);

    public:

      enum { csize  = 20 };
      enum { legs  = 2 };

      quadninja::PartitionInt p[2];
      quadninja::PartitionInt * cp;
  
      Complex c[csize];
      RealMomentum V0;
      ComplexMomentum e2, e3, e4;

      Bubble(): p(), cp(0), c(), V0(), e2(), e3(), e4() {}

      ~Bubble()
      {
        delete [] cp;
      }

      void initcp (int nlegs)
      {
        cp = new PartitionInt[nlegs - legs];
      }

      // Returns the polynomial residue
      Complex poly(const ComplexMomentum & q, const Complex & muq) const;

    };

    typedef CutsVector<Bubble>::const_iterator BubblesCIter;
    typedef CutsVector<Bubble>::iterator BubblesIter;

    // Prints the coefficients.  If they are smaller than chop_tol
    // they are printed as zeros.
    void print(const CutsVector<Bubble> & bubble);



    class Tadpole {

    private:

      QUADNINJA_NON_COPIABLE(Tadpole);

    public:

      enum { csize  = 16 };
      enum { legs  = 1 };

      quadninja::PartitionInt p[1];
      quadninja::PartitionInt * cp;
  
      Complex c[csize];
      RealMomentum V0;
      ComplexMomentum e1, e2, e3, e4;

      Tadpole() : p(), cp(0), c(), V0(), e1(), e2(), e3(), e4() {}

      ~Tadpole()
      {
        delete [] cp;
      }

      void initcp (int nlegs)
      {
        cp = new PartitionInt[nlegs - legs];
      }

      // Returns the polynomial residue
      Complex poly(const ComplexMomentum & q, const Complex & muq) const;

    };

    typedef CutsVector<Tadpole>::const_iterator TadpolesCIter;
    typedef CutsVector<Tadpole>::iterator TadpolesIter;

    // Prints the coefficients.  If they are smaller than chop_tol
    // they are printed as zeros.
    void print(const CutsVector<Tadpole> & tadpole);

    // NOTE: there could be an extra coeff. outside-the-cuts for n = 2

  } // namespace x1cuts
} // namespace quadninja

#endif // QUADNINJA_XCUTS_HH
