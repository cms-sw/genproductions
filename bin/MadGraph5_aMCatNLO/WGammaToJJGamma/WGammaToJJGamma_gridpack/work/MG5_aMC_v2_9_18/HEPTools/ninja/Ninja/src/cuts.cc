#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cuts.hh>
#include <basis.hh>
using namespace ninja;
using namespace cuts;
using namespace std;

#include <ninja/ninja.hh>

// don't implement extra-rank here
// (this shouldn't be necessary, it is just a precaution)
#undef NINJA_IMPLEMENTING_X1RANK

namespace ninja {

  inline Real Options::chop(Real x)
  {
    return taxicab_norm(x) > chop_tol ? x : 0.;
  }

  inline Complex Options::chop(const Complex & z)
  {
    double x, y;
    if(taxicab_norm(real(z))<chop_tol) x=0.; else x=real(z);
    if(taxicab_norm(imag(z))<chop_tol) y=0.; else y=imag(z);
    return Complex(x,y);
  }

  namespace cuts {

#include <polys_implem.cxx>   // the implementation is in polys_implem.cxx

  } // namespace cuts

} // namespace ninja
