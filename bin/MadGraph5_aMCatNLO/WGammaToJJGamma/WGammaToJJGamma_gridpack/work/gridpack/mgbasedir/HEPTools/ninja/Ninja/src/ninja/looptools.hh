// -*- C++ -*- header file which declares the interface between Ninja
// and the LoopTools integral library.


#ifndef NINJA_LOOPTOOLS_INTERFACE_HH
#define NINJA_LOOPTOOLS_INTERFACE_HH

#include <ninja/integral_library.hh>

#if !defined(NINJA_USE_LOOPTOOLS)
# error "Ninja was not configured with LoopTools"
#endif

namespace ninja {

  class LoopTools : public IntegralLibrary {
  public:

    LoopTools() {}

    // LoopTools specific methods
    static void clearIntegralCache();
    static void markIntegralCache();
    static void restoreIntegralCache();
    static void callLTini();
    static void callLTexi();
    static void setMinMass(Real minmass);
    static void setDebugKey(int debugkey);
    static int getDebugKey();


    // Overloaded Virtual methods

    // Init
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
  
    // scalar 2-point MIs
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

    // 1-point MIs
    virtual void getTadpoleIntegralRM(Complex rslt[3], Real m0);
    virtual void getTadpoleIntegralCM(Complex rslt[3], const Complex & m0);

    // 2-point integrals of rank 3.
    // - real masses
    virtual void getRank3BubbleIntegralRM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          Real m0, Real m1);
    // - complex masses
    virtual void getRank3BubbleIntegralCM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          const Complex & m0,
                                          const Complex & m1);
    // - complex masses
    virtual void getRank3BubbleIntegralNM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21);

  private:
    static bool initialized_;

  }; // class LoopTools

  extern LoopTools loop_tools;

} // namespace ninja

#endif // NINJA_LOOPTOOLS_INTERFACE_HH
