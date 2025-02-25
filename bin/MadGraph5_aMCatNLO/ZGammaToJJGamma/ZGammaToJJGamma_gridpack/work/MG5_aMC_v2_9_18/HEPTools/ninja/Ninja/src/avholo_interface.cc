#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ninja/avholo.hh>

namespace ninja {

  AvHOneLoop avh_olo;
  Real AvHOneLoop::ir_threshold_ = INFRARED_EPS;
  bool AvHOneLoop::initialized_ = false;

}


#ifdef NINJA_USE_ONELOOP
# include <avholo_decls.hh>

#ifdef NINJA_USE_ONELOOP_WITH_CACHE
# include <integral_cache.hh>
#endif

using namespace ninja;
#ifdef NINJA_USE_ONELOOP_WITH_CACHE
using namespace detail;
#endif

// Common methods of cached and uncached OneLoop

namespace ninja {

  void AvHOneLoop::setInfraredThreshold(Real threshold)
  {
    ir_threshold_ = threshold;
    ninjavholo_onshell(ir_threshold_);
  }

  void AvHOneLoop::init(Real muRsq)
  {
    if (!initialized_) {
      ninjavholo_onshell(ir_threshold_);
      initialized_ = true;
    }
#ifdef NINJA_USE_ONELOOP_WITH_CACHE
    if (!cache_)
      cache_ = new IntegralCache();
#endif
    if (mur2_ != muRsq) {
      clearIntegralCache();
      mur2_ = muRsq;
      mur_ = sqrt(muRsq);
    }
  }

  AvHOneLoop::~AvHOneLoop()
  {
    freeIntegralCache();
  }

  void
  AvHOneLoop::getBubbleIntegralCM(Complex result[3],
                                  Real p1,
                                  const Complex & m1, const Complex & m2)
  {
    ninjavholo_b0_cm(result,p1,m1,m2,mur_);
  }

  void
  AvHOneLoop::getBubbleIntegralRM(Complex result[3],
                                  Real p1,
                                  Real m1, Real m2)
  {
    ninjavholo_b0_rm(result,p1,m1,m2,mur_);
  }

}  // namespace ninja


#ifdef NINJA_USE_ONELOOP_WITH_CACHE

// methods for AvHOlo with cache

namespace {

  // Initial number of buckets.  As long as the cache is cleared
  // (hence maintaining the buckets in memory) and not freed, changing
  // these numbers shouldn't make much difference.
  const std::size_t N_BOXES = 30; // 750
  const std::size_t N_TRIANGLES = 30; // 350
  const std::size_t N_BUBBLES = 30; // 75
  const std::size_t N_TADPOLES = 1; // 2

}

namespace ninja {

  void
  AvHOneLoop::getBoxIntegralCM(Complex result[3],
                               Real p1, Real p2,
                               Real p3, Real p4,
                               Real p12, Real p23,
                               const Complex & m1, const Complex & m2,
                               const Complex & m3, const Complex & m4)
  {
    BoxArgsCM args = {{p1,p2,p3,p12,p23},{m1,m2,m3,m4}};
    MIsResult *val;

    if (cache_->ht_4cm.empty())
      cache_->ht_4cm.resize(N_BOXES);

    if (cache_->ht_4cm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_d0_cm(result,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }

  void
  AvHOneLoop::getBoxIntegralRM(Complex result[3],
                               Real p1, Real p2,
                               Real p3, Real p4,
                               Real p12, Real p23,
                               Real m1, Real m2,
                               Real m3, Real m4)
  {
    BoxArgsRM args = {{p1,p2,p3,p12,p23},{m1,m2,m3,m4}};
    MIsResult *val;

    if (cache_->ht_4rm.empty())
      cache_->ht_4rm.resize(N_BOXES);

    if (cache_->ht_4rm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_d0_rm(result,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }

  void
  AvHOneLoop::getBoxIntegralNM(Complex result[3],
                               Real p1, Real p2, Real p3, Real p4,
                               Real p12, Real p23)
  {
    const Real msq(0);
    BoxArgsNM args = {{p1,p2,p3,p12,p23}};
    MIsResult *val;

    if (cache_->ht_4nm.empty())
      cache_->ht_4nm.resize(N_BOXES);

    if (cache_->ht_4nm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_d0_rm(result,p1,p2,p3,p4,p12,p23,msq,msq,msq,msq,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }


  void
  AvHOneLoop::getTriangleIntegralCM(Complex result[3],
                                    Real p1, Real p2,
                                    Real p3,
                                    const Complex & m1, const Complex & m2,
                                    const Complex & m3)
  {
    TriangleArgsCM args = {{p1,p2,p3},{m1,m2,m3}};
    MIsResult *val;

    if (cache_->ht_3cm.empty())
      cache_->ht_3cm.resize(N_TRIANGLES);

    if (cache_->ht_3cm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_c0_cm(result,p1,p2,p3,m1,m2,m3,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }

  void
  AvHOneLoop::getTriangleIntegralRM(Complex result[3],
                                    Real p1, Real p2,
                                    Real p3,
                                    Real m1, Real m2,
                                    Real m3)
  {
    TriangleArgsRM args = {{p1,p2,p3},{m1,m2,m3}};
    MIsResult *val;

    if (cache_->ht_3rm.empty())
      cache_->ht_3rm.resize(N_TRIANGLES);

    if (cache_->ht_3rm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_c0_rm(result,p1,p2,p3,m1,m2,m3,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }

  void
  AvHOneLoop::getTriangleIntegralNM(Complex result[3],
                                    Real p1, Real p2,
                                    Real p3)
  {
    const Real msq(0);
    TriangleArgsNM args = {{p1,p2,p3}};
    MIsResult *val;

    if (cache_->ht_3nm.empty())
      cache_->ht_3nm.resize(N_TRIANGLES);

    if (cache_->ht_3nm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_c0_rm(result,p1,p2,p3,msq,msq,msq,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }


  void
  AvHOneLoop::getRank2BubbleIntegralCM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real p1,
                                       const Complex & m1, const Complex & m2)
  {
    BubbleArgsCM args = {p1,{m1,m2}};
    MIsRank2BubbleResult *val;

    if (cache_->ht_2cm.empty())
      cache_->ht_2cm.resize(N_BUBBLES);

    if (cache_->ht_2cm.find(args,val)) {
      b11[0] = val->data11[0];
      b11[1] = val->data11[1];
      b11[2] = val->data11[2];
      b1[0] = val->data1[0];
      b1[1] = val->data1[1];
      b1[2] = val->data1[2];
      b0[0] = val->data0[0];
      b0[1] = val->data0[1];
      b0[2] = val->data0[2];
    } else {
      Complex avh_res00[3];
      ninjavholo_b11_cm(b11,avh_res00,b1,b0,p1,m1,m2,mur_);
      val->data11[0] = b11[0];
      val->data11[1] = b11[1];
      val->data11[2] = b11[2];
      val->data1[0] = b1[0];
      val->data1[1] = b1[1];
      val->data1[2] = b1[2];
      val->data0[0] = b0[0];
      val->data0[1] = b0[1];
      val->data0[2] = b0[2];
    }
  }

  void
  AvHOneLoop::getRank2BubbleIntegralRM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real p1,
                                       Real m1, Real m2)
  {
    BubbleArgsRM args = {p1,{m1,m2}};
    MIsRank2BubbleResult *val;

    if (cache_->ht_2rm.empty())
      cache_->ht_2rm.resize(N_BUBBLES);

    if (cache_->ht_2rm.find(args,val)) {
      b11[0] = val->data11[0];
      b11[1] = val->data11[1];
      b11[2] = val->data11[2];
      b1[0] = val->data1[0];
      b1[1] = val->data1[1];
      b1[2] = val->data1[2];
      b0[0] = val->data0[0];
      b0[1] = val->data0[1];
      b0[2] = val->data0[2];
    } else {
      Complex avh_res00[3];
      ninjavholo_b11_rm(b11,avh_res00,b1,b0,p1,m1,m2,mur_);
      val->data11[0] = b11[0];
      val->data11[1] = b11[1];
      val->data11[2] = b11[2];
      val->data1[0] = b1[0];
      val->data1[1] = b1[1];
      val->data1[2] = b1[2];
      val->data0[0] = b0[0];
      val->data0[1] = b0[1];
      val->data0[2] = b0[2];
    }
  }

  void
  AvHOneLoop::getRank2BubbleIntegralNM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real p1)
  {
    const Real msq(0);
    BubbleArgsNM args = {p1};
    MIsRank2BubbleResult *val;

    if (cache_->ht_2nm.empty())
      cache_->ht_2nm.resize(N_BUBBLES);

    if (cache_->ht_2nm.find(args,val)) {
      b11[0] = val->data11[0];
      b11[1] = val->data11[1];
      b11[2] = val->data11[2];
      b1[0] = val->data1[0];
      b1[1] = val->data1[1];
      b1[2] = val->data1[2];
      b0[0] = val->data0[0];
      b0[1] = val->data0[1];
      b0[2] = val->data0[2];
    } else {
      Complex avh_res00[3];
      ninjavholo_b11_rm(b11,avh_res00,b1,b0,p1,msq,msq,mur_);
      val->data11[0] = b11[0];
      val->data11[1] = b11[1];
      val->data11[2] = b11[2];
      val->data1[0] = b1[0];
      val->data1[1] = b1[1];
      val->data1[2] = b1[2];
      val->data0[0] = b0[0];
      val->data0[1] = b0[1];
      val->data0[2] = b0[2];
    }
  }


  void
  AvHOneLoop::getTadpoleIntegralCM(Complex result[3], const Complex & m0)
  {
    TadpoleArgsCM args = {m0};
    MIsResult *val;

    if (cache_->ht_1cm.empty())
      cache_->ht_1cm.resize(N_TADPOLES);

    if (cache_->ht_1cm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_a0_cm(result,m0,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }

  void
  AvHOneLoop::getTadpoleIntegralRM(Complex result[3], Real m0)
  {
    TadpoleArgsRM args = {m0};
    MIsResult *val;

    if (cache_->ht_1rm.empty())
      cache_->ht_1rm.resize(N_TADPOLES);

    if (cache_->ht_1rm.find(args,val)) {
      result[0] = val->data[0];
      result[1] = val->data[1];
      result[2] = val->data[2];
    } else {
      ninjavholo_a0_rm(result,m0,mur_);
      val->data[0] = result[0];
      val->data[1] = result[1];
      val->data[2] = result[2];
    }
  }


  void AvHOneLoop::clearIntegralCache()
  {
    if (!cache_)
      return;

    if (!cache_->ht_4cm.empty())
      cache_->ht_4cm.clear();
    if (!cache_->ht_3cm.empty())
      cache_->ht_3cm.clear();
    if (!cache_->ht_2cm.empty())
      cache_->ht_2cm.clear();
    if (!cache_->ht_1cm.empty())
      cache_->ht_1cm.clear();

    if (!cache_->ht_4rm.empty())
      cache_->ht_4rm.clear();
    if (!cache_->ht_3rm.empty())
      cache_->ht_3rm.clear();
    if (!cache_->ht_2rm.empty())
      cache_->ht_2rm.clear();
    if (!cache_->ht_1rm.empty())
      cache_->ht_1rm.clear();

    if (!cache_->ht_4nm.empty())
      cache_->ht_4nm.clear();
    if (!cache_->ht_3nm.empty())
      cache_->ht_3nm.clear();
    if (!cache_->ht_2nm.empty())
      cache_->ht_2nm.clear();
  }


  void AvHOneLoop::freeIntegralCache()
  {
#if 0 // not needed, we delete the pointer (which calls the destructor)
    if (!cache_)
      return;

    if (!cache_->ht_4cm.empty())
      cache_->ht_4cm.free();
    if (!cache_->ht_3cm.empty())
      cache_->ht_3cm.free();
    if (!cache_->ht_2cm.empty())
      cache_->ht_2cm.free();
    if (!cache_->ht_1cm.empty())
      cache_->ht_1cm.free();

    if (!cache_->ht_4rm.empty())
      cache_->ht_4rm.free();
    if (!cache_->ht_3rm.empty())
      cache_->ht_3rm.free();
    if (!cache_->ht_2rm.empty())
      cache_->ht_2rm.free();
    if (!cache_->ht_1rm.empty())
      cache_->ht_1rm.free();

    if (!cache_->ht_4nm.empty())
      cache_->ht_4nm.free();
    if (!cache_->ht_3nm.empty())
      cache_->ht_3nm.free();
    if (!cache_->ht_2nm.empty())
      cache_->ht_2nm.free();
#endif

    delete cache_;
    cache_ = 0;
  }

} // namespace ninja

#else // NINJA_USE_ONELOOP_WITH_CACHE

// OneLoop without cache

namespace ninja {

  void
  AvHOneLoop::getBoxIntegralCM(Complex result[3],
                               Real p1, Real p2,
                               Real p3, Real p4,
                               Real p12, Real p23,
                               const Complex & m1, const Complex & m2,
                               const Complex & m3, const Complex & m4)
  {
    ninjavholo_d0_cm(result,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,mur_);
  }

  void
  AvHOneLoop::getBoxIntegralRM(Complex result[3],
                               Real p1, Real p2,
                               Real p3, Real p4,
                               Real p12, Real p23,
                               Real m1, Real m2,
                               Real m3, Real m4)
  {
    ninjavholo_d0_rm(result,p1,p2,p3,p4,p12,p23,m1,m2,m3,m4,mur_);
  }

  void
  AvHOneLoop::getTriangleIntegralCM(Complex result[3],
                                    Real p1, Real p2,
                                    Real p3,
                                    const Complex & m1, const Complex & m2,
                                    const Complex & m3)
  {
    ninjavholo_c0_cm(result,p1,p2,p3,m1,m2,m3,mur_);
  }

  void
  AvHOneLoop::getTriangleIntegralRM(Complex result[3],
                                    Real p1, Real p2,
                                    Real p3,
                                    Real m1, Real m2,
                                    Real m3)
  {
    ninjavholo_c0_rm(result,p1,p2,p3,m1,m2,m3,mur_);
  }

  void
  AvHOneLoop::getRank2BubbleIntegralCM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real p1,
                                       const Complex & m1, const Complex & m2)
  {
    Complex avh_res00[3];
    ninjavholo_b11_cm(b11,avh_res00,b1,b0,p1,m1,m2,mur_);
  }

  void
  AvHOneLoop::getRank2BubbleIntegralRM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real p1,
                                       Real m1, Real m2)
  {
    Complex avh_res00[3];
    ninjavholo_b11_rm(b11,avh_res00,b1,b0,p1,m1,m2,mur_);
  }

  void
  AvHOneLoop::getTadpoleIntegralCM(Complex result[3], const Complex & m0)
  {
    ninjavholo_a0_cm(result,m0,mur_);
  }

  void
  AvHOneLoop::getTadpoleIntegralRM(Complex result[3], Real m0)
  {
    ninjavholo_a0_rm(result,m0,mur_);
  }

  void AvHOneLoop::clearIntegralCache() {}
  void AvHOneLoop::freeIntegralCache() {}

} // namespace ninja

#endif // ! NINJA_USE_ONELOOP_WITH_CACHE
#else // NINJA_USE_ONELOOP

// No OneLoop library --> dummy implementation

#include <iostream>
#include <stdexcept>
using namespace std;

namespace {

  void avh_olo_disabled()
  {
    cerr << "ERROR IN NINJA: "
         << "No intergral library has been specified.\n"
         << "ERROR IN NINJA: Pleas enable OneLoop and/or LoopTools"
         << " during configuration.\n"
         << "ERROR IN NINJA: Alternatively, specify another custom library."
         << endl;
    NINJA_THROW (logic_error("No integral library."));
  }

}

namespace ninja {

  void AvHOneLoop::init(Real)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getBubbleIntegralCM(Complex *,
                                  Real,
                                  const Complex &, const Complex &)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getBubbleIntegralRM(Complex *,
                                  Real, Real, Real)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getBoxIntegralCM(Complex *,
                               Real, Real, Real, Real, Real, Real,
                               const Complex &, const Complex &,
                               const Complex &, const Complex &)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getBoxIntegralRM(Complex *,
                               Real, Real, Real, Real, Real, Real,
                               Real, Real, Real, Real)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getTriangleIntegralCM(Complex *,
                                    Real, Real, Real,
                                    const Complex &, const Complex &,
                                    const Complex &)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getTriangleIntegralRM(Complex *,
                                    Real, Real, Real,
                                    Real, Real, Real)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getRank2BubbleIntegralCM(Complex *, Complex *,
                                       Complex *,
                                       Real,
                                       const Complex &, const Complex &)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getRank2BubbleIntegralRM(Complex *, Complex *,
                                       Complex *,
                                       Real, Real, Real)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getTadpoleIntegralCM(Complex *, const Complex &)
  {
    avh_olo_disabled();
  }

  void
  AvHOneLoop::getTadpoleIntegralRM(Complex *, Real)
  {
    avh_olo_disabled();
  }

  void AvHOneLoop::clearIntegralCache()
  {
    avh_olo_disabled();
  }

  void AvHOneLoop::freeIntegralCache()
  {
    avh_olo_disabled();
  }

  AvHOneLoop::~AvHOneLoop() {}

} // namespace ninja

#endif // ! NINJA_USE_ONELOOP


#if !defined(NINJA_USE_ONELOOP_WITH_CACHE) || !defined(NINJA_USE_ONELOOP)

namespace ninja {

  void
  AvHOneLoop::getBoxIntegralNM(Complex rslt[3],
                               Real s21, Real s32, Real s43,
                               Real s14, Real s31, Real s42)
  {
    getBoxIntegralRM(rslt, s21, s32, s43, s14, s31, s42,
                     Real(), Real(), Real(), Real());
  }

  void
  AvHOneLoop::getTriangleIntegralNM(Complex rslt[3],
                                    Real s21, Real s32, Real s13)
  {
    getTriangleIntegralRM(rslt, s21, s32, s13, Real(), Real(), Real());
  }

  void
  AvHOneLoop::getRank2BubbleIntegralNM(Complex b11[3],
                                       Complex b1[3], Complex b0[3],
                                       Real s21)
  {
    getRank2BubbleIntegralRM(b11, b1, b0, s21, Real(), Real());
  }

} // namespace ninja

#endif // ! NINJA_USE_ONELOOP_WITH_CACHE
