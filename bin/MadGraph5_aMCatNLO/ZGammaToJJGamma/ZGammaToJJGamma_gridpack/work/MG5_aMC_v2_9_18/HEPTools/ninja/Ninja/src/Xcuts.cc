#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef NINJA_X1RANK
#include <Xcuts.hh>
#include <basis.hh>
using namespace ninja;
using namespace x1cuts;
using namespace std;

#include <cuts_utils.hh>
using namespace cuts_utils;

#include <ninja/ninja.hh>

// implement extra-rank here
#define NINJA_IMPLEMENTING_X1RANK

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

  namespace x1cuts {

#include <polys_implem.cxx>   // the implementation is in polys_implem.cxx

  } // namespace x1cuts

} // namespace ninja

// extra-rank implementation ends here
#undef NINJA_IMPLEMENTING_X1RANK

#endif // NINJA_X1RANK
