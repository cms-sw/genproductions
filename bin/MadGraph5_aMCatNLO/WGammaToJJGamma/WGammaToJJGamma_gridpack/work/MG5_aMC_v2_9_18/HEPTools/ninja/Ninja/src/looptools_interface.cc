#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef NINJA_USE_LOOPTOOLS

#include <ninja/looptools.hh>
#include <clooptools.h>

#define NINJA_LT_WRITE_RESULT(rslt,func,label,idx)  \
  do {                                              \
    rslt[0] = func(label,idx);                      \
    rslt[1] = func(label,idx+1);                    \
    rslt[2] = func(label,idx+2);                    \
  } while(0)


namespace ninja {

  LoopTools loop_tools;

  bool LoopTools::initialized_ = false;

  void LoopTools::clearIntegralCache()
  {
    clearcache();
  }

  void LoopTools::markIntegralCache()
  {
    markcache();
  }
  
  void LoopTools::restoreIntegralCache()
  {
    restorecache();
  }

  void LoopTools::callLTini()
  {
    ltini();
    initialized_ = true;
  }

  void LoopTools::callLTexi()
  {
    ltexi();
    initialized_ = false;
  }

  void LoopTools::setMinMass(Real minmass)
  {
    setminmass(minmass);
  }

  void LoopTools::setDebugKey(int debugkey)
  {
    setdebugkey(debugkey);
  }
  
  int LoopTools::getDebugKey()
  {
    return getdebugkey();
  }

  void LoopTools::init(Real muRsq)
  {
    if (!initialized_) {
      ltini();
      initialized_ = true;
    }
    setlambda(0);
    setmudim(muRsq);
  }

  void LoopTools::getBoxIntegralRM(Complex rslt[3],
                                   Real s21, Real s32, Real s43,
                                   Real s14, Real s31, Real s42,
                                   Real m1, Real m2, Real m3, Real m4)
  {
    memindex i = Dget(s21,s32,s43,s14,s31,s42,m1,m2,m3,m4);
    NINJA_LT_WRITE_RESULT(rslt,Dval,dd0,i);
  }

  void LoopTools::getBoxIntegralCM(Complex rslt[3],
                                   Real s21, Real s32, Real s43,
                                   Real s14, Real s31, Real s42,
                                   const Complex & m1, const Complex & m2,
                                   const Complex & m3, const Complex & m4)
  {
    memindex i = DgetC(s21,s32,s43,s14,s31,s42,m1,m2,m3,m4);
    NINJA_LT_WRITE_RESULT(rslt,DvalC,dd0,i);
  }

  void LoopTools::getTriangleIntegralRM(Complex rslt[3],
                                        Real s21, Real s32, Real s13,
                                        Real m1, Real m2, Real m3)
  {
    memindex i = Cget(s21,s32,s13,m1,m2,m3);
    NINJA_LT_WRITE_RESULT(rslt,Cval,cc0,i);
  }

  void LoopTools::getTriangleIntegralCM(Complex rslt[3],
                                        Real s21, Real s32, Real s13,
                                        const Complex & m1, const Complex & m2,
                                        const Complex & m3)
  {
    memindex i = CgetC(s21,s32,s13,m1,m2,m3);
    NINJA_LT_WRITE_RESULT(rslt,CvalC,cc0,i);
  }

  void LoopTools::getBubbleIntegralRM(Complex rslt[3],
                                      Real s21, Real m1, Real m2)
  {
    memindex i = Bget(s21,m1,m2);
    NINJA_LT_WRITE_RESULT(rslt,Bval,bb0,i);
  }

  void LoopTools::getBubbleIntegralCM(Complex rslt[3],
                                      Real s21,
                                      const Complex & m1, const Complex & m2)
  {
    memindex i = BgetC(s21,m1,m2);
    NINJA_LT_WRITE_RESULT(rslt,BvalC,bb0,i);
  }

  void LoopTools::getRank2BubbleIntegralRM(Complex b11[3],
                                           Complex b1[3], Complex b0[3],
                                           Real s21, Real m1, Real m2)
  {
    memindex i = Bget(s21, m1, m2);
    NINJA_LT_WRITE_RESULT(b0,Bval,bb0,i);
    NINJA_LT_WRITE_RESULT(b1,Bval,bb1,i);
    NINJA_LT_WRITE_RESULT(b11,Bval,bb11,i);
  }

  void LoopTools::getRank2BubbleIntegralCM(Complex b11[3],
                                           Complex b1[3], Complex b0[3],
                                           Real s21,
                                           const Complex & m1,
                                           const Complex & m2)
  {
    memindex i = BgetC(s21, m1, m2);
    NINJA_LT_WRITE_RESULT(b0,BvalC,bb0,i);
    NINJA_LT_WRITE_RESULT(b1,BvalC,bb1,i);
    NINJA_LT_WRITE_RESULT(b11,BvalC,bb11,i);
  }

  void LoopTools::getTadpoleIntegralRM(Complex rslt[3], Real m0)
  {
    memindex i = Aget(m0);
    NINJA_LT_WRITE_RESULT(rslt,Aval,aa0,i);
  }

  void LoopTools::getTadpoleIntegralCM(Complex rslt[3], const Complex & m0)
  {
    memindex i = AgetC(m0);
    NINJA_LT_WRITE_RESULT(rslt,AvalC,aa0,i);
  }

  void LoopTools::getRank3BubbleIntegralRM(Complex b111[3], Complex b11[3],
                                           Complex b1[3], Complex b0[3],
                                           Real s21,
                                           Real m1, Real m2)
  {
    memindex i = Bget(s21, m1, m2);
    NINJA_LT_WRITE_RESULT(b0,Bval,bb0,i);
    NINJA_LT_WRITE_RESULT(b1,Bval,bb1,i);
    NINJA_LT_WRITE_RESULT(b11,Bval,bb11,i);
    NINJA_LT_WRITE_RESULT(b111,Bval,bb111,i);
  }

  void LoopTools::getRank3BubbleIntegralNM(Complex b111[3], Complex b11[3],
                                           Complex b1[3], Complex b0[3],
                                           Real s21)
  {
    memindex i = Bget(s21, 0, 0);
    NINJA_LT_WRITE_RESULT(b0,Bval,bb0,i);
    NINJA_LT_WRITE_RESULT(b1,Bval,bb1,i);
    NINJA_LT_WRITE_RESULT(b11,Bval,bb11,i);
    NINJA_LT_WRITE_RESULT(b111,Bval,bb111,i);
  }

  void LoopTools::getRank3BubbleIntegralCM(Complex b111[3], Complex b11[3],
                                           Complex b1[3], Complex b0[3],
                                           Real s21,
                                           const Complex & m1,
                                           const Complex & m2)
  {
    memindex i = BgetC(s21, m1, m2);
    NINJA_LT_WRITE_RESULT(b0,BvalC,bb0,i);
    NINJA_LT_WRITE_RESULT(b1,BvalC,bb1,i);
    NINJA_LT_WRITE_RESULT(b11,BvalC,bb11,i);
    NINJA_LT_WRITE_RESULT(b111,BvalC,bb111,i);
  }

} // namespace ninja

#endif  // NINJA_USE_LOOPTOOLS
