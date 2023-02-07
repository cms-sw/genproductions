// -*- C++ -*- header file which declares the interface between Ninja
// and the OneLoop integral library.  A cache of computed integrals is
// added on top of the OneLoop library, in order to speed up multiple
// evaluations of the same functions.


#ifndef NINJA_AVHOLO_INTERFACE_HH
#define NINJA_AVHOLO_INTERFACE_HH

#include <ninja/integral_library.hh>

#if !defined(NINJA_USE_ONELOOP) && !defined(NINJA_CONFIG_H_INTERNAL)
# error "Ninja was not configured with OneLoop"
#endif

namespace ninja {

  struct IntegralCache;

  class AvHOneLoop : public IntegralLibrary {
  public:
    AvHOneLoop(): cache_(0), mur_(0), mur2_(0) {}

    // This method sets the numerical infrared threshold (default =
    // 1.0e-10)
    static void setInfraredThreshold(Real threshold);

    // This method clears the computed MIs but keeps the allocated
    // buckets in memory.  This avoids rehashing in future calls of
    // MIs.  It is suggested to be called after every phase-space
    // point, especially for amplitudes with many external legs.
    void clearIntegralCache();

    // This method completely frees the memory allocated by the cache
    // of MIs.  There is no obvious case when this should be called.
    // The method clearIntegralCache() should in general be preferred.
    void freeIntegralCache();

    virtual void init(Real muRsq);

    // 4-point MIs
    // - real masses
    virtual void getBoxIntegralRM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42,
                                  Real m1, Real m2, Real m3, Real m4);
    // - complex masses
    virtual void getBoxIntegralCM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42,
                                  const Complex & m1, const Complex & m2,
                                  const Complex & m3, const Complex & m4);
    // - massless
    virtual void getBoxIntegralNM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42);
  
    // 3-point MIs
    // - real massses
    virtual void getTriangleIntegralRM(Complex rslt[3],
                                       Real s21, Real s32, Real s13,
                                       Real m1, Real m2, Real m3);
    // - complex massses
    virtual void getTriangleIntegralCM(Complex rslt[3],
                                       Real s21, Real s32, Real s13,
                                       const Complex & m1, const Complex & m2,
                                       const Complex & m3);
    // - massless
    virtual void getTriangleIntegralNM(Complex rslt[3],
                                       Real s21, Real s32, Real s13);
  
    // scalar 2-point MIs.  Not cached
    // - real masses
    virtual void getBubbleIntegralRM(Complex rslt[3],
                                     Real s21, Real m1, Real m2);
    // - complex massses
    virtual void getBubbleIntegralCM(Complex rslt[3],
                                     Real s21,
                                     const Complex & m1, const Complex & m2);

    // rank-2 2-point MIs
    // - real masses
    virtual void getRank2BubbleIntegralRM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21, Real m1, Real m2);
    // - complex massses
    virtual void getRank2BubbleIntegralCM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          const Complex & m1,
                                          const Complex & m2);
    // - massless
    virtual void getRank2BubbleIntegralNM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21);


    // 1-point MIs
    virtual void getTadpoleIntegralRM(Complex rslt[3], Real m0);
    virtual void getTadpoleIntegralCM(Complex rslt[3], const Complex & m0);


    // destructor
    virtual ~AvHOneLoop();

  private:
    IntegralCache * cache_;
    Real mur_, mur2_;
    static Real ir_threshold_;
    static bool initialized_;

  }; // class AvHOneLoop

  extern AvHOneLoop avh_olo;

} // namespace ninja

#endif // NINJA_AVHOLO_INTERFACE_HH
