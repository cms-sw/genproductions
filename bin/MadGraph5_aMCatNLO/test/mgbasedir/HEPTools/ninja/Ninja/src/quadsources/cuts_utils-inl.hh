#ifndef QUADNINJA_CUTS_UTILS_INL_HH
#define QUADNINJA_CUTS_UTILS_INL_HH

namespace quadninja {
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
} // namespace quadninja

#endif // #define QUADNINJA_CUTS_UTILS_INL_HH
