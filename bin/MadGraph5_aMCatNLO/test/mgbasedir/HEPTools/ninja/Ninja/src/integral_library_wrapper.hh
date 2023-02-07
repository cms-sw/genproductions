// Wrapper class for integral libraries, used internally by Ninja.  It
// calls the init() method in the constructor and the exit() method in
// the descrutor.  It also provides a more template-friendly
// interface, which looks the same for massive and massless calls.


#ifndef INTEGRAL_LIBRARY_WRAPPER_HH
#define INTEGRAL_LIBRARY_WRAPPER_HH

#include <ninja/integral_library.hh>

namespace ninja {
  namespace integral_library_wrapper {

    class WrapIntegralLibrary {

    public:

      WrapIntegralLibrary(IntegralLibrary * lib, Real muRsq): lib_(lib)
      {
        lib_->init(muRsq);
      }

      ~WrapIntegralLibrary()
      {
        lib_->exit();
      }

      // ComplexMasses

      void getBoxIntegral(Complex rslt[3],
                          Real s21, Real s32, Real s43,
                          Real s14, Real s31, Real s42,
                          const Complex & m1, const Complex & m2,
                          const Complex & m3, const Complex & m4)
      {
        lib_->getBoxIntegralCM(rslt,s21,s32,s43,s14,s31,s42,m1,m2,m3,m4);
      }
    
      void getTriangleIntegral(Complex rslt[3],
                               Real s21, Real s32, Real s13,
                               const Complex & m1, const Complex & m2,
                               const Complex & m3)
      {
        lib_->getTriangleIntegralCM(rslt,s21,s32,s13,m1,m2,m3);
      }

      void getRank2BubbleIntegral(Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  const Complex & m1,
                                  const Complex & m2)
      {
        lib_->getRank2BubbleIntegralCM(b11,b1,b0,s21,m1,m2);
      }

      void getRank3BubbleIntegral(Complex b111[3], Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  const Complex & m1,
                                  const Complex & m2)
      {
        lib_->getRank3BubbleIntegralCM(b111,b11,b1,b0,s21,m1,m2);
      }

      void getTadpoleIntegral(Complex rslt[3], const Complex & m0)
      {
        lib_->getTadpoleIntegralCM(rslt,m0);
      }


      // RealMasses

      void getBoxIntegral(Complex rslt[3],
                          Real s21, Real s32, Real s43,
                          Real s14, Real s31, Real s42,
                          Real m1, Real m2,
                          Real m3, Real m4)
      {
        lib_->getBoxIntegralRM(rslt,s21,s32,s43,s14,s31,s42,m1,m2,m3,m4);
      }
    
      void getTriangleIntegral(Complex rslt[3],
                               Real s21, Real s32, Real s13,
                               Real m1, Real m2,
                               Real m3)
      {
        lib_->getTriangleIntegralRM(rslt,s21,s32,s13,m1,m2,m3);
      }

      void getRank2BubbleIntegral(Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  Real m1,
                                  Real m2)
      {
        lib_->getRank2BubbleIntegralRM(b11,b1,b0,s21,m1,m2);
      }

      void getRank3BubbleIntegral(Complex b111[3], Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  Real m1,
                                  Real m2)
      {
        lib_->getRank3BubbleIntegralRM(b111,b11,b1,b0,s21,m1,m2);
      }

      void getTadpoleIntegral(Complex rslt[3], Real m0)
      {
        lib_->getTadpoleIntegralRM(rslt,m0);
      }

    
      // Massless

      void getBoxIntegral(Complex rslt[3],
                          Real s21, Real s32, Real s43,
                          Real s14, Real s31, Real s42,
                          ZeroFloat, ZeroFloat,
                          ZeroFloat, ZeroFloat)
      {
        lib_->getBoxIntegralNM(rslt,s21,s32,s43,s14,s31,s42);
      }
    
      void getTriangleIntegral(Complex rslt[3],
                               Real s21, Real s32, Real s13,
                               ZeroFloat, ZeroFloat,
                               ZeroFloat)
      {
        lib_->getTriangleIntegralNM(rslt,s21,s32,s13);
      }

      void getRank2BubbleIntegral(Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  ZeroFloat,
                                  ZeroFloat)
      {
        lib_->getRank2BubbleIntegralNM(b11,b1,b0,s21);
      }

      void getRank3BubbleIntegral(Complex b111[3], Complex b11[3],
                                  Complex b1[3], Complex b0[3],
                                  Real s21,
                                  ZeroFloat,
                                  ZeroFloat)
      {
        lib_->getRank3BubbleIntegralNM(b111,b11,b1,b0,s21);
      }

      void getTadpoleIntegral(Complex rslt[3], ZeroFloat)
      {
        lib_->getTadpoleIntegralNM(rslt);
      }

    private:
      IntegralLibrary * lib_;

    };

  }
}

#endif
