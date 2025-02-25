#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef QUADNINJA_X1RANK
#include <quadsources/Xcuts.hh>
#include <quadsources/basis.hh>
using namespace quadninja;
using namespace x1cuts;
using namespace std;

#include <quadsources/cuts_utils.hh>
using namespace cuts_utils;

#include <quadninja/ninja.hh>

// implement extra-rank here
#define QUADNINJA_IMPLEMENTING_X1RANK

namespace quadninja {

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

#include <quadsources/polys_implem.cxx>   // the implementation is in polys_implem.cxx

  } // namespace x1cuts

} // namespace quadninja

// extra-rank implementation ends here
#undef QUADNINJA_IMPLEMENTING_X1RANK

#endif // QUADNINJA_X1RANK
