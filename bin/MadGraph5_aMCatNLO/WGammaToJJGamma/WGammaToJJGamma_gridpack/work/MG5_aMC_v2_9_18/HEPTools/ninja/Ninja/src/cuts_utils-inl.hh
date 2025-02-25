#ifndef NINJA_CUTS_UTILS_INL_HH
#define NINJA_CUTS_UTILS_INL_HH

namespace ninja {
  namespace cuts_utils {

    template<typename MassType>
    inline void CutTadpole<MassType>::getLoopMomentum(const Real & x1,
                                                      const Real & x2,
                                                      Complex & X)
    {
      // useful products
      Complex e12 = TWO*e.mp12();
      Complex e34 = -e12;
      // result
      X = (mq-e12*x1*x2)/e34;
    }

  } // namespace cuts_utils
} // namespace ninja

#endif // #define NINJA_CUTS_UTILS_INL_HH
