// Wrappers of Ninja routines useful for interfacing to other
// languages (e.g. Fortran90 or Fortran77)

// note: at the moment only the interface for tensor numerators is
// exposed

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

//quadninja//#define QUADNINJA_WRAPS 1

#include <ctime>
#include <iostream>
#include <fstream>
#include <exception>

#include <ninja/ninja.hh>
#include <ninja/tensor_ninja.hh>

#if defined(QUADNINJA) && !defined(QUADNINJA_WRAPS)
#include <quadninja/ninja.hh>
#include <quadninja/tensor_ninja.hh>
#endif

#ifdef NINJA_USE_ONELOOP
# include <ninja/avholo.hh>
#if defined(QUADNINJA) && !defined(QUADNINJA_WRAPS)
# include <quadninja/avholo.hh>
#endif
#endif

#ifdef NINJA_USE_LOOPTOOLS
# include <ninja/looptools.hh>
#endif

using namespace ninja;


namespace  {

#ifndef QUADNINJA_WRAPS
  std::ofstream ninja_out;
#endif

  template <typename MassType>
  inline
  void ninja_tensor_evaluate(const Complex * tensor,
                             int n, int rank,
                             const RealMomentum p[],
                             typename const_pointer<MassType>::type m2,
                             Real * s_mat,
                             Real mur2,
                             Complex tot[3], Complex & totr,
                             int & return_status)
  {
    TensorNumerator num(n,rank,tensor);
    Amplitude<MassType> amp(n,rank,p,m2);

    amp.setRenormalizationScale(mur2);
    if (s_mat)
      amp.setSMatrix(s_mat);
    return_status = amp.evaluate(num);

    tot[0] = amp[0];
    tot[1] = amp[1];
    tot[2] = amp[2];
    totr = amp.getRationalPart();
  }
  
} // namespace 



extern "C" {

  void ninja_tensor_evaluate_cm_(const Complex * tensor,
                                 const int & n, const int & rank,
                                 const RealMomentum p[],
                                 const Complex m2[],
                                 const Real & mur2,
                                 Complex tot[3], Complex & totr,
                                 int & return_status)
  {
    ninja_tensor_evaluate<ComplexMasses>(tensor,n,rank,p,m2,0,mur2,
                                         tot,totr,return_status);
  }

  void ninja_tensor_evaluate_rm_(const Complex * tensor,
                                 const int & n, const int & rank,
                                 const RealMomentum p[],
                                 const Real m2[],
                                 const Real & mur2,
                                 Complex tot[3], Complex & totr,
                                 int & return_status)
  {
    ninja_tensor_evaluate<RealMasses>(tensor,n,rank,p,m2,0,mur2,
                                      tot,totr,return_status);
  }

#ifdef NINJA_MASSLESS
  void ninja_tensor_evaluate_nm_(const Complex * tensor,
                                 const int & n, const int & rank,
                                 const RealMomentum p[],
                                 const Real & mur2,
                                 Complex tot[3], Complex & totr,
                                 int & return_status)
  {
    ninja_tensor_evaluate<Massless>(tensor,n,rank,p,ZeroFloatArray(),0,mur2,
                                    tot,totr,return_status);
  }
#endif


  void ninja_tensor_evaluate_smat_cm_(const Complex * tensor,
                                      const int & n, const int & rank,
                                      Real * s_mat,
                                      const RealMomentum p[],
                                      const Complex m2[],
                                      const Real & mur2,
                                      Complex tot[3], Complex & totr,
                                      int & return_status)
  {
    ninja_tensor_evaluate<ComplexMasses>(tensor,n,rank,p,m2,s_mat,mur2,
                                         tot,totr,return_status);
  }

  void ninja_tensor_evaluate_smat_rm_(const Complex * tensor,
                                      const int & n, const int & rank,
                                      Real * s_mat,
                                      const RealMomentum p[],
                                      const Real m2[],
                                      const Real & mur2,
                                      Complex tot[3], Complex & totr,
                                      int & return_status)
  {
    ninja_tensor_evaluate<RealMasses>(tensor,n,rank,p,m2,s_mat,mur2,
                                      tot,totr,return_status);
  }

#ifdef NINJA_MASSLESS
  void ninja_tensor_evaluate_smat_nm_(const Complex * tensor,
                                      const int & n, const int & rank,
                                      const RealMomentum p[],
                                      const Real & mur2,
                                      Complex tot[3], Complex & totr,
                                      int & return_status)
  {
    ninja_tensor_evaluate<Massless>(tensor,n,rank,p,ZeroFloatArray(),0,mur2,
                                    tot,totr,return_status);
  }
#endif


#ifndef QUADNINJA_WRAPS

  void ninja_clear_integral_cache_()
  {
#if defined(NINJA_USE_ONELOOP) && defined(NINJA_USE_ONELOOP_WITH_CACHE)
    if (ninja::getIntegralLibrary() == (& ninja::avh_olo))
      avh_olo.clearIntegralCache();
#endif
#ifdef NINJA_USE_LOOPTOOLS
    if (ninja::getIntegralLibrary() == (& ninja::loop_tools))
      loop_tools.clearIntegralCache();
#endif

#if QUADNINJA
#if defined(NINJA_USE_ONELOOP) && defined(NINJA_USE_ONELOOP_WITH_CACHE)
    if (quadninja::getIntegralLibrary() == (& quadninja::avh_olo))
      quadninja::avh_olo.clearIntegralCache();
#endif
//#ifdef NINJA_USE_LOOPTOOLS
//    if (quadninja::getIntegralLibrary() == (& quadninja::loop_tools))
//      quadninja::loop_tools.clearIntegralCache();
//#endif
#endif
  }

  void ninja_free_integral_cache_()
  {
#if defined(NINJA_USE_ONELOOP) && defined(NINJA_USE_ONELOOP_WITH_CACHE)
    avh_olo.freeIntegralCache();
#if QUADNINJA
    quadninja::avh_olo.freeIntegralCache();
#endif
#endif
  }

  void ninja_set_verbosity_(const int & val)
  {
    setVerbosity(val);
    if (val && !ninja_out.is_open()) {
      ninja_out.open("ninja.out", std::ofstream::app);
      if (ninja_out.fail()) {
        std::cerr << "ERROR IN NINJA: "
                  << "ninja.out could not be opened for appending."
                  << std::endl;
        std::terminate();
      }
      using namespace std;
      time_t rawtime;
      tm * timeinfo;
      time (&rawtime);
      timeinfo = localtime (&rawtime);
      ninja_out << "----------------------------"
                << "----------------------------\n\n"
                << "Ninja called on: "
                << asctime(timeinfo) << "\n" << endl;
    }
    setOutputStream(ninja_out);
#if QUADNINJA
    quadninja::setOutputStream(ninja_out);
    quadninja::setVerbosity(val);
#endif
  }

  void ninja_set_test_(const int & val)
  {
    setTest(val);
#if QUADNINJA
    quadninja::setTest(val);
#endif
  }

  void ninja_set_test_tolerance_(const Real & val)
  {
    ninja::setTestTolerance(val);
#if QUADNINJA
    quadninja::setTestTolerance(val);
#endif
  }

  void ninja_set_output_precision_(const int & val)
  {
    ninja_out.precision(val);
  }

  void ninja_set_integral_library_(const int & flag)
  {
    switch (flag) {
#ifdef NINJA_USE_ONELOOP
    case 1:
      ninja::setDefaultIntegralLibrary(ninja::avh_olo);
#if QUADNINJA
      quadninja::setDefaultIntegralLibrary(quadninja::avh_olo);
#endif
      break;
#endif
#ifdef NINJA_USE_LOOPTOOLS
    case 2:
      ninja::setDefaultIntegralLibrary(ninja::loop_tools);
//#if QUADNINJA
//      quadninja::setDefaultIntegralLibrary(ninja::loop_tools);
//#endif
      break;
#endif
    default:
      std::cerr << "ERROR IN NINJA: "
                << "The flag specified in ninja_set_integral_library"
                <<" does not correpond to a valid Integral Library."
                << std::endl;
      std::terminate();      
    }
  }

  void ninja_set_floating_point_threshold_(const Real & thr)
  {
    ninja::setDefaultFloatingPointThreshold(thr);
  }

#if defined(QUADNINJA)
  void quadninja_set_floating_point_threshold_(const Real & thr)
  {
    quadninja::setDefaultFloatingPointThreshold(thr);
  }
#endif

#endif // !QUADNINJA_WRAPS

} // extern "C"
