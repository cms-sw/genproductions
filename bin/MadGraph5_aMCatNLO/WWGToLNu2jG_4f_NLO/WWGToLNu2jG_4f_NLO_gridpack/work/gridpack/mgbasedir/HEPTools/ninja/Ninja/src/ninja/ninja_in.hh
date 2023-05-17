// This file is included by ninja/ninja.hh.  It contains some forward
// declarations for private methods and some options.  It is neither
// supposed to be used nor included directly by the user.


#ifndef NINJA_NINJA_IN_HH
#define NINJA_NINJA_IN_HH

#ifndef NINJA_NINJA_HH_INSIDE
# error "Do not include <ninja/ninja_in.hh> directly.  Include <ninja/ninja.h> instead."
#endif

namespace ninja {

  // Forward declarations

  template <typename T> class CutsVector;
  class IntegralLibrary;

  void printBanner(std::ostream & banner_out, bool force_print);
  void setDefaultIntegralLibrary(ninja::IntegralLibrary & lib);
  ninja::IntegralLibrary * getIntegralLibrary();
  void setTest(unsigned flag);
  void setTestTolerance(Real test_tolerance);
  void setOutputStream(std::ostream & outs);
  void setVerbosity(unsigned flag);
  void setChopTolerance(Real chop_tolerance);

  namespace cuts {
    class Pentagon;
    class Box;
    class Triangle;
    class Bubble;
    class Tadpole;
    void print (const CutsVector<Pentagon> & pentagon);
    void print (const CutsVector<Box> & box);
    void print (const CutsVector<Triangle> & triangle);
    void print (const CutsVector<Bubble> & bubble);
    void print (const CutsVector<Tadpole> & tadpole);
  } // namespace cuts

  //higher rank
  namespace x1cuts {
    class Pentagon;
    class Box;
    class Triangle;
    class Bubble;
    class Tadpole;
    void print (const CutsVector<Pentagon> & pentagon);
    void print (const CutsVector<Box> & box);
    void print (const CutsVector<Triangle> & triangle);
    void print (const CutsVector<Bubble> & bubble);
    void print (const CutsVector<Tadpole> & tadpole);
  } // namespace x1cuts


  class Options {
  private:

    // data options
    static std::ostream * out;
    static IntegralLibrary * mis;
    static Real chop_tol, test_tol, fp_threshold;
    static unsigned verb, test;
    static bool quiet;

    // private methods using options
    static Real chop(Real x);
    static Complex chop(const Complex & z);

    // friends
    friend void setDefaultIntegralLibrary(IntegralLibrary & lib);
    friend ninja::IntegralLibrary * getIntegralLibrary();
    friend void printBanner(std::ostream & banner_out, bool force_print);
    friend void setTest(unsigned flag);
    friend void setChopTolerance(Real chop_tolerance);
    friend void setTestTolerance(Real test_tolerance);
    friend void setOutputStream(std::ostream & outs);
    friend void setVerbosity(unsigned flag);
    friend void setDefaultFloatingPointThreshold(Real threshold);
    template<typename MassType> friend class Amplitude;
    friend void cuts::print (const CutsVector<cuts::Pentagon> & pentagon);
    friend void cuts::print (const CutsVector<cuts::Box> & box);
    friend void cuts::print (const CutsVector<cuts::Triangle> & triangle);
    friend void cuts::print (const CutsVector<cuts::Bubble> & bubble);
    friend void cuts::print (const CutsVector<cuts::Tadpole> & tadpole);
    friend void x1cuts::print (const CutsVector<x1cuts::Pentagon> & pentagon);
    friend void x1cuts::print (const CutsVector<x1cuts::Box> & box);
    friend void x1cuts::print (const CutsVector<x1cuts::Triangle> & triangle);
    friend void x1cuts::print (const CutsVector<x1cuts::Bubble> & bubble);
    friend void x1cuts::print (const CutsVector<x1cuts::Tadpole> & tadpole);

    // do not instantiate
    Options();
  }; // class Options

} // namespace ninja

#endif // NINJA_NINJA_IN_HH
