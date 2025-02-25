// Main -*- C++ -*- header file of the Ninja library.


#ifndef NINJA_NINJA_HH
#define NINJA_NINJA_HH

#define NINJA_NINJA_HH_INSIDE

#include <iostream>
#include <stdexcept>

#include <ninja/types.hh>
#include <ninja/momentum.hh>
#include <ninja/s_mat.hh>
#include <ninja/num_defs.hh>
#include <ninja/ninja_in.hh>


namespace ninja {

  // Flags which control the internal tests performed by Ninja (NONE
  // by default).
  struct Test {
    enum {
      NONE = 0,
      ALL = ~(0),
      GLOBAL = 1,
      LOCAL_4 = 1 << 4,
      LOCAL_3 = 1 << 3,
      LOCAL_2 = 1 << 2,
      LOCAL_1 = 1 << 1,
      LOCAL = LOCAL_4 | LOCAL_3 | LOCAL_2 | LOCAL_1
    };
  };


  // Flags which control the output of ninja (NONE by default).
  struct Verbose {
    enum {

      NONE = 0,
      ALL = ~(0),

      // Output tests
      GLOBAL_TEST = 1,
      LOCAL_TEST_4 = 1 << 4,
      LOCAL_TEST_3 = 1 << 3,
      LOCAL_TEST_2 = 1 << 2,
      LOCAL_TEST_1 = 1 << 1,
      LOCAL_TESTS = LOCAL_TEST_1 | LOCAL_TEST_2 | LOCAL_TEST_3 | LOCAL_TEST_4,
      TESTS = LOCAL_TESTS | GLOBAL_TEST,

      // Output coefficients
      C5 = 1 << (4+5),
      C4 = 1 << (4+4),
      C3 = 1 << (4+3),
      C2 = 1 << (4+2),
      C1 = 1 << (4+1),
      COEFFS = C5 | C4 | C3 | C2 | C1,

      // Result
      RESULT = 1 << (4+5+1),

      // Output integrals
      INTEGRALS = 1 << (4+5+2)
    };
  };

  // Choose which tests to perform (NONE by default).  Example:
  // setTest(Test::LOCAL) performs local tests.
  inline void setTest(unsigned flag)
  {
    Options::test = flag;
  }

  // Set the verbosity flag.  Example:
  // setVerbosity(Verbose::GLOBAL_TEST|Verbose::COEFFS) prints the
  // result of the global test and the value of all the computed
  // coefficients
  inline void setVerbosity(unsigned flag)
  {
    Options::verb = flag;
  }


  // set the integral library to be used by ninja
  inline void setDefaultIntegralLibrary(IntegralLibrary & mis)
  {
    Options::mis = & mis;
  }

  // get a pointer to the integral library ninja is using
  inline IntegralLibrary * getIntegralLibrary()
  {
    return Options::mis;
  }

  // Print the banner.  If force_print is set to false, the banner
  // will be printed only if never printed before.
  void printBanner(std::ostream & banner_out = std::cout,
                   bool force_print=true);

  // Set the chop_tolerance.  If the verbosity is set to a non-zero
  // value, only the coefficients greater than chop_tolerance will be
  // printed as non-zeros.  Its default value is 1.0e-5.
  inline void setChopTolerance(Real chop_tolerance)
  {
    Options::chop_tol = chop_tolerance;
  }

  // Set the chop_tolerance.  If a test is performed and the relative
  // error is greater than the specified tolerance, the
  // Amplitude::evaluate method will return Amplitude::TEST_FAILDED.
  // Its default value is 1.0e-5.
  inline void setTestTolerance(Real test_tolerance)
  {
    Options::test_tol = test_tolerance;
  }

  // Set the default floating-point threshold used to detect unstable
  // kinematics to be used when not specified for an Amplitude object.
  inline void setDefaultFloatingPointThreshold(Real threshold)
  {
    Options::fp_threshold = threshold;
  }

  // Set the output stream, used when the verbosity is true.
  inline void setOutputStream(std::ostream & outs)
  {
    Options::out = & outs;
  }


  // ninja::Amplitude is the main class of the ninja library.  The
  // method Amplitude::evaluate computes the integrals.
  template<typename MassType>
  class Amplitude {
  public:

    enum {SUCCESS = 0, TEST_FAILED=1, UNSTABLE_KINEMATICS=1 << 1};

    typedef typename const_pointer<MassType>::type MassConstPtr;

  public:

    // default
    Amplitude()
      : result(0.,0.,0.),
        s_mat(),
        V(0),
        mis(Options::mis),
        m2(static_cast<MassType*>(0)),
        cut_constr(),
        scale(1.),
        fp_threshold(Options::fp_threshold),
        n(0), rank(0), 
        min_cut(0),
        return_val(SUCCESS),
        use_mu_exp(true) {}

    // copy
    Amplitude(Amplitude & amp)
      : result(amp.result[0],amp.result[1],amp.result[2]),
        s_mat(),
        V(amp.V),
        mis(amp.mis),
        m2(amp.m2),
        cut_constr(amp.cut_constr),
        scale(amp.scale),
        fp_threshold(amp.fp_threshold),
        n(amp.n), rank(amp.rank), 
        min_cut(amp.min_cut),
        return_val(SUCCESS),
        use_mu_exp(amp.use_mu_exp)
    {
      if (!amp.s_mat.isNull())
        s_mat.copy(amp.s_mat);
    }

    // Constructor.  nn = number-of-loop-propagators,
    // rr=rank-of-numerator, p=pointer-to-array-ofinternal-momenta,
    // mass_sq=pointer-to-array-of-square-masses (which will be
    // ignored in the massless case))
    Amplitude(int nn, int rr, const RealMomentum p[], MassConstPtr mass_sq)
      : result(0.,0.,0.),
        s_mat(0,0),
        V(p),
        mis(Options::mis),
        m2(mass_sq),
        cut_constr(),
        scale(1.),
        fp_threshold(Options::fp_threshold),
        n(nn), rank(rr), 
        min_cut(0),
        return_val(SUCCESS),
        use_mu_exp(true) {}

    // this can only be used for MassType == Massless
    Amplitude(int nn, int rr, const RealMomentum p[]);
    
    // Set number of loop propagators
    Amplitude & setN(int nn)
    {
      n = nn;
      return *this;
    }

    // Set the rank of the numerator
    Amplitude & setRank(int rr)
    {
      rank = rr;
      return *this;
    }

    // Set pointer to array of internal momenta
    Amplitude & setKinematics(const RealMomentum p[])
    {
      V = p;
      return *this;
    }

    // Set pointer to array of internal squared masses
    Amplitude & setInternalMasses(MassConstPtr mass_sq)
    {
      m2 = mass_sq;
      return *this;
    }

    // Set S-matrix from another
    Amplitude & setSMatrix(const SMatrix & s_matrix)
    {
      s_mat = s_matrix;
      return *this;
    }

    // Set S-matrix from pointer to data
    Amplitude & setSMatrix(Real * data_ptr)
    {
      s_mat = SMatrix(data_ptr ? n : 0, data_ptr);
      return *this;
    }

    // Set the renormalization scale (it only affects the computation
    // of the Master Integrals).  If not set, its default value is 1.
    Amplitude & setRenormalizationScale(Real renorm_scale)
    {
      scale = renorm_scale;
      return *this;
    }

    // Stop the reduction right after the specified cut (e.g. if val=3
    // the reduction will stop after the determination of the triangle
    // residues and assumes that bubbles and tadpoles give no
    // contribution to the amplitude; if val<=1 instead the reduction
    // will be complete)
    Amplitude & setCutStop(int val)
    {
      min_cut = val;
      return *this;
    }
    
    // Set the mu-expansion flag.  If set to true (default), use the
    // mu-expansion for the determination of the rational part coming
    // from the box contributions.  If set to false use the sampling
    // on mu^2 instead.
    Amplitude & useMuExpansion(bool val = true)
    {
      use_mu_exp = val;
      return *this;
    }

    // set the integral library to be used by this Amplitude object,
    // if different from the default one
    Amplitude & setIntegralLibrary(IntegralLibrary & library)
    {
      mis = & library;
      return *this;
    }

    // Reset the total result to zero
    Amplitude & reset()
    {
      result[0] = result[1] = result[2] = ninja::Complex();
      cut_constr = ninja::Complex();
      return *this;
    }

    // Evaluate the Amplitude an add the result to the total.  Returns
    // Amplitude::SUCCESS unless one of the tests performed failed, in
    // which case it returns Amplitude::TEST_FAILED.
    int evaluate(Numerator & num);

    // Acces to components eps^0, eps^-1, eps^-2 of the result
    const Complex eps0() const { return result[0];}
    const Complex epsm1() const { return result[1];}
    const Complex epsm2() const { return result[2];}

    // rational part of the result
    const Complex getRationalPart() const
    {
      return result[0] - cut_constr;
    }

    // cut constructible part of the result (finite-term)
    const Complex getCutConstructiblePart() const
    {
      return cut_constr;
    }

    // throw away rational part
    void onlyCutConstructible()
    {
      result[0] = cut_constr;
    }

    // operator []: operator [i] returns the coefficients of eps^(-i)
    const Complex operator[] (unsigned i) const 
    {
      return result[i];
    }

    // set floating-point threshold used to detect unstable
    // kinematics.  If an unstable kinematics is detected in the
    // 'evaluate' method, UNSTABLE_KINEMATICS is returned and the
    // result is left unchanged.
    void setFloatingPointThreshold(Real threshold)
    {
      fp_threshold = threshold;
    }

  private:

    void evaluatePentagons(Numerator & num,
                           CutsVector<cuts::Pentagon> & pentagon);
    void evaluatePentagon(Numerator & num,
                          cuts::Pentagon & pentagon);

    void evaluateBoxes(Numerator & num,
                       const CutsVector<cuts::Pentagon> & pentagon,
                       CutsVector<cuts::Box> & box);
    void evaluateBox(Numerator & num,
                     const CutsVector<cuts::Pentagon> & pentagon,
                     cuts::Box & box);

    void evaluateFullBoxes(Numerator & num,
                           const CutsVector<cuts::Pentagon> & pentagon,
                           CutsVector<cuts::Box> & box);
    void evaluateFullBox(Numerator & num,
                         const CutsVector<cuts::Pentagon> & pentagon,
                         cuts::Box & box);

    void evaluateTriangles(Numerator & num,
                           CutsVector<cuts::Triangle> & triangle);
    void evaluateTriangle(Numerator & num,
                          cuts::Triangle & triangle);

    void evaluateBubbles(Numerator & num,
                         const CutsVector<cuts::Triangle> & triangle,
                         CutsVector<cuts::Bubble> & bubble);
    void evaluateBubble(Numerator & num,
                        const CutsVector<cuts::Triangle> & triangle,
                        cuts::Bubble & bubble);

    void evaluateTadpoles(Numerator & num,
                          const CutsVector<cuts::Triangle> & triangle,
                          const CutsVector<cuts::Bubble> & bubble,
                          CutsVector<cuts::Tadpole> & tadpole);
    void evaluateTadpole(Numerator & num,
                         const CutsVector<cuts::Triangle> & triangle,
                         const CutsVector<cuts::Bubble> & bubble,
                         cuts::Tadpole & tadpole);

    void evaluateFullTadpoles(Numerator & num,
                              const CutsVector<cuts::Triangle> & triangle,
                              const CutsVector<cuts::Bubble> & bubble,
                              CutsVector<cuts::Tadpole> & tadpole);
    void evaluateFullTadpole(Numerator & num,
                             const CutsVector<cuts::Triangle> & triangle,
                             const CutsVector<cuts::Bubble> & bubble,
                             cuts::Tadpole & tadpole);

    // Global N = N test
    void NeqNtest(Numerator & num,
                 const CutsVector<cuts::Pentagon> & pentagon,
                 const CutsVector<cuts::Box> & box,
                 const CutsVector<cuts::Triangle> & triangle,
                 const CutsVector<cuts::Bubble> & bubble,
                 const CutsVector<cuts::Tadpole> & tadpole,
                 const ComplexMomentum & q, const Complex & muq);
    // Local N = N tests
    void local4NeqNtests(Numerator & num,
                        const CutsVector<cuts::Pentagon> & pentagon,
                        const CutsVector<cuts::Box> & box);
    void local3NeqNtests(Numerator & num,
                        const CutsVector<cuts::Pentagon> & pentagon,
                        const CutsVector<cuts::Box> & box,
                        const CutsVector<cuts::Triangle> & triangle);
    void local2NeqNtests(Numerator & num,
                        const CutsVector<cuts::Pentagon> & pentagon,
                        const CutsVector<cuts::Box> & box,
                        const CutsVector<cuts::Triangle> & triangle,
                        const CutsVector<cuts::Bubble> & bubble);
    void local1NeqNtests(Numerator & num,
                        const CutsVector<cuts::Pentagon> & pentagon,
                        const CutsVector<cuts::Box> & box,
                        const CutsVector<cuts::Triangle> & triangle,
                        const CutsVector<cuts::Bubble> & bubble,
                        const CutsVector<cuts::Tadpole> & tadpole);

    
    // Higher rank methods

    int higherRankEvaluate(Numerator & num);

    void evaluatePentagons(Numerator & num,
                           CutsVector<x1cuts::Pentagon> & pentagon);
    void evaluatePentagon(Numerator & num,
                          x1cuts::Pentagon & pentagon);

    void evaluateBoxes(Numerator & num,
                       const CutsVector<x1cuts::Pentagon> & pentagon,
                       CutsVector<x1cuts::Box> & box);
    void evaluateBox(Numerator & num,
                     const CutsVector<x1cuts::Pentagon> & pentagon,
                     x1cuts::Box & box);

    void evaluateFullBoxes(Numerator & num,
                           const CutsVector<x1cuts::Pentagon> & pentagon,
                           CutsVector<x1cuts::Box> & box);
    void evaluateFullBox(Numerator & num,
                         const CutsVector<x1cuts::Pentagon> & pentagon,
                         x1cuts::Box & box);

    void evaluateTriangles(Numerator & num,
                           CutsVector<x1cuts::Triangle> & triangle);
    void evaluateTriangle(Numerator & num,
                          x1cuts::Triangle & triangle);

    void evaluateBubbles(Numerator & num,
                         const CutsVector<x1cuts::Triangle> & triangle,
                         CutsVector<x1cuts::Bubble> & bubble);
    void evaluateBubble(Numerator & num,
                        const CutsVector<x1cuts::Triangle> & triangle,
                        x1cuts::Bubble & bubble);

    void evaluateTadpoles(Numerator & num,
                          const CutsVector<x1cuts::Triangle> & triangle,
                          const CutsVector<x1cuts::Bubble> & bubble,
                          CutsVector<x1cuts::Tadpole> & tadpole);
    void evaluateTadpole(Numerator & num,
                         const CutsVector<x1cuts::Triangle> & triangle,
                         const CutsVector<x1cuts::Bubble> & bubble,
                         x1cuts::Tadpole & tadpole);

    void evaluateFullTadpoles(Numerator & num,
                              const CutsVector<x1cuts::Triangle> & triangle,
                              const CutsVector<x1cuts::Bubble> & bubble,
                              CutsVector<x1cuts::Tadpole> & tadpole);
    void evaluateFullTadpole(Numerator & num,
                             const CutsVector<x1cuts::Triangle> & triangle,
                             const CutsVector<x1cuts::Bubble> & bubble,
                             x1cuts::Tadpole & tadpole);

    // If the argument is too small, set the kinematics as unstable
    // and returns false.  Returns true otherwise
    template <typename T>
    bool stability_check(const T & z)
    {
      if (taxicab_norm(z) < fp_threshold) {
        return_val = return_val | UNSTABLE_KINEMATICS;
        return false;
      }
      return true;
    }

    // check if the kinematics has been detected as unstable
    bool unstable_kinematics() const
    {
      if (return_val & UNSTABLE_KINEMATICS) {
        if (Options::verb) {
          (*Options::out) << std::endl
                          << "ninja::Amplitude is returning "
                          << "UNSTABLE_KINEMATICS" << std::endl;
        }
        return true;
      }
      return false;
    }
    
    // Global N = N test
    void NeqNtest(Numerator & num,
                 const CutsVector<x1cuts::Pentagon> & pentagon,
                 const CutsVector<x1cuts::Box> & box,
                 const CutsVector<x1cuts::Triangle> & triangle,
                 const CutsVector<x1cuts::Bubble> & bubble,
                 const CutsVector<x1cuts::Tadpole> & tadpole,
                 const ComplexMomentum & q, const Complex & muq);
    // Local N = N tests
    void local4NeqNtests(Numerator & num,
                        const CutsVector<x1cuts::Pentagon> & pentagon,
                        const CutsVector<x1cuts::Box> & box);
    void local3NeqNtests(Numerator & num,
                        const CutsVector<x1cuts::Pentagon> & pentagon,
                        const CutsVector<x1cuts::Box> & box,
                        const CutsVector<x1cuts::Triangle> & triangle);
    void local2NeqNtests(Numerator & num,
                        const CutsVector<x1cuts::Pentagon> & pentagon,
                        const CutsVector<x1cuts::Box> & box,
                        const CutsVector<x1cuts::Triangle> & triangle,
                        const CutsVector<x1cuts::Bubble> & bubble);
    void local1NeqNtests(Numerator & num,
                        const CutsVector<x1cuts::Pentagon> & pentagon,
                        const CutsVector<x1cuts::Box> & box,
                        const CutsVector<x1cuts::Triangle> & triangle,
                        const CutsVector<x1cuts::Bubble> & bubble,
                        const CutsVector<x1cuts::Tadpole> & tadpole);

    // data
    ninja::details::Array3D<Complex> result;
    SMatrix s_mat;
    const RealMomentum * V;
    IntegralLibrary * mis;
    MassConstPtr m2;
    Complex cut_constr;
    Real scale, fp_threshold;
    int n, rank;
    int min_cut;
    int return_val;
    bool use_mu_exp;

  }; // template<typename MassType> class Amplitude


  template <typename MassType>
  inline Amplitude<MassType>::Amplitude(int nn, int rr, const RealMomentum p[])
    : result(0.,0.,0.),
      s_mat(0,0),
      V(p),
      mis(Options::mis),
      m2(static_cast<const MassType*>(0)),
      cut_constr(),
      scale(1.),
      fp_threshold(Options::fp_threshold),
      n(nn), rank(rr), 
      min_cut(0),
      return_val(SUCCESS),
      use_mu_exp(true)
  {
    // this makes sure it is only called for Massless types
    details::MasslessTypeError<MassType>::MassType_must_be_massless();
  }

} // namespace ninja

#undef NINJA_NINJA_HH_INSIDE

#endif // NINJA_NINJA_HH
