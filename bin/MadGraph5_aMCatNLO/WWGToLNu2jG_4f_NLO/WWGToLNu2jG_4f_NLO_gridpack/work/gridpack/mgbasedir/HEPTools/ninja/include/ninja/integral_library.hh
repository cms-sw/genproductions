// This -*- C++ -*- header file defines the generic interface between
// Ninja and libraries of Master Integrals.  Ninja already provides an
// interface to the OneLoop library (used by default, adding an
// internal cache of computed integrals) and the LoopTools library.
// The abstract class IntegralLibrary allows to interface other
// libraries defined by the user.


#ifndef NINJA_INTEGRAL_LIBRARY_HH
#define NINJA_INTEGRAL_LIBRARY_HH

#include <ninja/types.hh>

namespace ninja {

  // IntegralLibrary is an abstract base class which provides a
  // generic interface to Intergral Libraries.  In order to implement
  // this interface, the user must create a derived class which
  // implements all the methods bewteen the two comments starting with
  // '***'.  Other methods can also be overloaded but they are not
  // mandatory, since default definitions are provided.
  //
  // The arguments of each method are the internal masses m1, m2, ...
  // and the invariants s[i,j] defined as:
  //
  //   s[i,j] = (vi[i]-vi[j])^2
  //
  // where vi[i] is the internal momentum of the i-th loop denominator
  //
  //   Denom[i] = (q+vi[i])^2 - m[i]^2.
  //
  // Note that s[i,j] coincide with the definition of the S-matrix
  // defined in <ninja/s_math.hh>.
  class IntegralLibrary {
  public:

    // *** BEGIN of declarations of methods which _must_ be overloaded

    // This method is called by the method Amplitude::evaluate for
    // every integrand, before computing the first needed Master
    // Integral.  It is supposed to set the renormalization scale
    // mu_R^2 to be used in subsequent calls of the integral library,
    // and perform any other intialization that the library might need
    // (e.g. setting IR thresholds, etc...).
    virtual void init(Real muRsq) = 0;

    // 4-point MIs
    // - real masses
    virtual void getBoxIntegralRM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42,
                                  Real m1sq, Real m2sq,
                                  Real m3sq, Real m4sq) = 0;
    // - complex masses
    virtual void getBoxIntegralCM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42,
                                  const Complex & m1sq, const Complex & m2sq,
                                  const Complex & m3sq, const Complex & m4sq)
    = 0;
  
    // 3-point MIs
    // - real massses
    virtual void getTriangleIntegralRM(Complex rslt[3],
                                       Real s21, Real s32, Real s13,
                                       Real m1sq, Real m2sq, Real m3sq) = 0;
    // - complex massses
    virtual void getTriangleIntegralCM(Complex rslt[3],
                                       Real s21, Real s32, Real s13,
                                       const Complex & m1sq,
                                       const Complex & m2sq,
                                       const Complex & m3sq) = 0;
  
    // scalar 2-point MIs
    // - real masses
    virtual void getBubbleIntegralRM(Complex rslt[3],
                                     Real s21, Real m1sq, Real m2sq) = 0;
    // - complex massses
    virtual void getBubbleIntegralCM(Complex rslt[3],
                                     Real s21,
                                     const Complex & m1sq,
                                     const Complex & m2sq) = 0;

    // rank-2 2-point MIs
    // - real masses
    virtual void getRank2BubbleIntegralRM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21, Real m1sq, Real m2sq) = 0;
    // - complex massses
    virtual void getRank2BubbleIntegralCM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          const Complex & m1sq,
                                          const Complex & m2sq) = 0;


    // 1-point MIs
    virtual void getTadpoleIntegralRM(Complex rslt[3], Real m0sq) = 0;
    virtual void getTadpoleIntegralCM(Complex rslt[3],
                                      const Complex & m0sq) = 0;

    // *** END of declarations of methods which _must_ be overloaded


    // The following methods can -- but do not need to -- be
    // overloaded by an implementation of the IntegralLibrary.


    // This method is called by the method Amplitude::evaluate after
    // every Master Integral is computed for the given integrand.  The
    // default implementation is trivial, but a non-trivial
    // implementation might be needed in some cases.
    virtual void exit() {}


    // 2-point integrals of rank 3.  Ninja provides a default
    // implementation of these in terms of lower rank integrals.
    // - real masses
    virtual void getRank3BubbleIntegralRM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          Real m1sq, Real m2sq);
    // - complex masses
    virtual void getRank3BubbleIntegralCM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21,
                                          const Complex & m1sq,
                                          const Complex & m2sq);


    // MIs with massless loop propagators.  Ninja provides a default
    // implementation of these in terms of Master Integrals with real
    // internal masses (set numerically to zero).
    virtual void getBoxIntegralNM(Complex rslt[3],
                                  Real s21, Real s32, Real s43,
                                  Real s14, Real s31, Real s42);
    virtual void getTriangleIntegralNM(Complex rslt[3],
                                       Real s21, Real s32, Real s13);
    virtual void getBubbleIntegralNM(Complex rslt[3],
                                     Real s21);
    virtual void getRank2BubbleIntegralNM(Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21);
    virtual void getRank3BubbleIntegralNM(Complex b111[3], Complex b11[3],
                                          Complex b1[3], Complex b0[3],
                                          Real s21);
    virtual void getTadpoleIntegralNM(Complex rslt[3]);


    // this is not used at the moment
    virtual bool requiresOrderedCalls()
    {
      return false;
    }


    virtual ~IntegralLibrary() {}

  }; // class IntegralLibrary

} // namespace ninja

#endif // NINJA_MASTER_INTEGRALS_HH
