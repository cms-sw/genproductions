#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ctime>
#include <iostream>
#include <fstream>
#include <exception>

#include <quadsources/ninja_scoped_array.hh>
#include <quadsources/gosam_interface.hh>

#ifdef QUADNINJA_USE_ONELOOP
#include <quadninja/avholo.hh>
#endif
#ifdef QUADNINJA_USE_LOOPTOOLS
#include <quadninja/looptools.hh>
#endif

using namespace quadninja;

namespace {

  // Numerator class for the interface with GoSam
  class GoSamNumerator : public Numerator
  {

  private:

    gosam_interface::Numerator numerator;
    gosam_interface::Numerator_t3 numerator_t3;
    gosam_interface::Numerator_t2 numerator_t2;
    gosam_interface::Numerator_d numerator_d;

  public:

    GoSamNumerator(gosam_interface::Numerator gosam_numerator,
                   gosam_interface::Numerator_t3 gosam_numerator_t3,
                   gosam_interface::Numerator_t2 gosam_numerator_t2,
                   gosam_interface::Numerator_d gosam_numerator_d)
      : numerator(gosam_numerator),
        numerator_t3(gosam_numerator_t3),
        numerator_t2(gosam_numerator_t2),
        numerator_d(gosam_numerator_d) {}
    

    Complex evaluate(const quadninja::ComplexMomentum & q,
                     const quadninja::Complex & muq,
                     int cut, const quadninja::PartitionInt part[])
    {
      // Format cut-number (not used by gosam)
      // int ncut = cut ? 0: -1;
      // for (int i = cut-1; i >= 0; i--) {
      //     ncut += part[i]*pow(10,i);
      // }
      (void)(cut); (void)(part);
      // Call numerator
      Complex result;
      (*numerator)(-1, q, muq, result) ;
      return result;
    }

    void muExpansion(const quadninja::ComplexMomentum Vort[],
                     const quadninja::PartitionInt part[],
                     quadninja::Complex c[])
    {
      // Format cut-number (not used by gosam)
      // int ncut = 0;
      // for (int i = 4-1; i >= 0; i--) {
      //     ncut += part[i]*pow(10,i);
      // }
      (void)(part);
      // Call numerator
      (*numerator_d)(-1, Vort, c) ;
    }

    void t3Expansion(const quadninja::ComplexMomentum & a,
                     const quadninja::ComplexMomentum & e3,
                     const quadninja::ComplexMomentum & e4,
                     const quadninja::Complex & param,
                     int mindeg, int cut, const quadninja::PartitionInt part[],
                     quadninja::Complex c[])
    {
      // Format cut-number (not used by gosam)
      // int ncut = 0;
      // for (int i = cut-1; i >= 0; i--) {
      //     ncut += part[i]*pow(10,i);
      // }
      (void)(part);
      // Just to distinguish 3-ple and 1-gle cuts...
      int ncut = (cut == 3) ? 999 : 9;
      // Call numerator
      (*numerator_t3)(ncut, a, e3, e4, param, mindeg, c) ;
    }

    void t2Expansion(const quadninja::ComplexMomentum & a0,
                     const quadninja::ComplexMomentum & a1,
                     const quadninja::ComplexMomentum & e3,
                     const quadninja::ComplexMomentum & e4,
                     const quadninja::Complex param[],
                     int mindeg, int cut, const quadninja::PartitionInt part[],
                     quadninja::Complex c[])
    {
      // Format cut-number
      // int ncut = 0;
      // for (int i = cut-1; i >= 0; i--) {
      //     ncut += part[i]*pow(10,i);
      // }
      (void)(cut); (void)(part);
      // Call numerator
      (*numerator_t2)(-1, a0, a1, e3, e4, param, mindeg, c) ;
    }

  }; // class GoSamNumerator

  std::ofstream quadninjago_out;

  // other flags
  bool quadninjago_first = true;
  inline void quadninjago_init_impl()
  {
    if (quadninjago_first == true) {
      setDefaultFloatingPointThreshold(0);
      quadninjago_first = false;
    }
  }

} // namespace


extern "C" {  

  // Definition of the routines to be called by the Fortran code in GoSam

  void quadninjago_init()
  {
    quadninjago_init_impl();
  }

  void quadninjago_diag_rm(gosam_interface::Numerator numerator,
                       gosam_interface::Numerator_t3 numerator_t3,
                       gosam_interface::Numerator_t2 numerator_t2,
                       gosam_interface::Numerator_d numerator_d,
                       const int & nprops_group,
                       const int & nprops, const int & rk,
                       const int * indices,
                       const RealMomentum * Vi, const Real * msq,
                       Real * g_mat_data,
                       const Real & scale2,
                       const int & istop,
                       Complex tot[3], Complex & totr,
                       int & return_status)
  {
    quadninjago_init_impl();

    // define the numerator
    GoSamNumerator gosam_numerator(numerator,numerator_t3,numerator_t2,
                                   numerator_d);

    // define s_mat
    SMatrix g_mat(nprops_group,g_mat_data), s_mat;

    // define subset of internal vectors and masses needed
    ScopedArray<RealMomentum> pi(nprops);
    ScopedArray<Real> mi(nprops);
    for (int i =0; i<nprops; ++i) {
      pi[i] = Vi[indices[i]-1];
      mi[i] = msq[indices[i]-1];
    }
    // s-mat
    if (!g_mat.isNull()) {
      s_mat.allocate(nprops);
      for (int i =0; i<nprops; ++i) {
        int im = indices[i]-1;
        s_mat(i,i) = Real();
        for (int j =i+1; j<nprops; ++j) {
          int jm = indices[j]-1;
          s_mat(i,j) = g_mat(im,jm) + msq[im] + msq[jm];
          s_mat(j,i) = s_mat(i,j);
        }
      }
    }

    // create a amplitude object and pass the parameters
    Amplitude<RealMasses> amplitude(nprops,rk,pi.data(),mi.data());
    amplitude
      .setSMatrix(s_mat)
      .setRenormalizationScale(scale2)
      .setCutStop(istop);

    // evaluate the amplitude
    return_status = amplitude.evaluate(gosam_numerator);

    // write the result (the order is reversed)
    tot[2] = amplitude.eps0();
    tot[1] = amplitude.epsm1();
    tot[0] = amplitude.epsm2();
    totr = amplitude.getRationalPart();
  }

  void quadninjago_diag_cm(gosam_interface::Numerator numerator,
                       gosam_interface::Numerator_t3 numerator_t3,
                       gosam_interface::Numerator_t2 numerator_t2,
                       gosam_interface::Numerator_d numerator_d,
                       const int & nprops_group,
                       const int & nprops, const int & rk,
                        const int * indices,
                       const RealMomentum * Vi, const Complex * msq,
                       Real * g_mat_data,
                       const Real & scale2,
                       const int & istop,
                       Complex tot[3], Complex & totr,
                       int & return_status)
  {
    quadninjago_init_impl();

    // define the numerator
    GoSamNumerator gosam_numerator(numerator,numerator_t3,numerator_t2,
                                   numerator_d);

    // define s_mat
    SMatrix g_mat(nprops_group,g_mat_data), s_mat;

    // define subset of interal vectors and masses needed
    ScopedArray<RealMomentum> pi(nprops);
    ScopedArray<Complex> mi(nprops);
    for (int i =0; i<nprops; ++i) {
      pi[i] = Vi[ indices[i]-1 ];
      mi[i] = msq[ indices[i]-1 ];
    }
    // s-mat
    if (!g_mat.isNull()) {
      s_mat.allocate(nprops);
      for (int i =0; i<nprops; ++i) {
        int im = indices[i]-1;
        s_mat(i,i) = Real();
        for (int j =i+1; j<nprops; ++j) {
          int jm = indices[j]-1;
          s_mat(i,j) = g_mat(im,jm)
            + quadninja::real(msq[im]) + quadninja::real(msq[jm]);
          s_mat(j,i) = s_mat(i,j);
        }
      }
    }

    // create a quadninja::amplitude object and pass the parameters
    Amplitude<ComplexMasses> amplitude(nprops,rk,pi.data(),mi.data());
    amplitude
      .setSMatrix(s_mat)
      .setRenormalizationScale(scale2)
      .setCutStop(istop);

    // evaluate the amplitude
    return_status = amplitude.evaluate(gosam_numerator);

    // write the result (the order is reversed)
    tot[2] = amplitude.eps0();
    tot[1] = amplitude.epsm1();
    tot[0] = amplitude.epsm2();
    totr = amplitude.getRationalPart();
  }


#ifdef QUADNINJA_MASSLESS

  void quadninjago_diag_nm(gosam_interface::Numerator numerator,
                       gosam_interface::Numerator_t3 numerator_t3,
                       gosam_interface::Numerator_t2 numerator_t2,
                       gosam_interface::Numerator_d numerator_d,
                       const int & nprops_group,
                       const int & nprops, const int & rk,
                        const int * indices,
                       const RealMomentum * Vi,
                       Real * g_mat_data,
                       const Real & scale2,
                       const int & istop,
                       Complex tot[3], Complex & totr,
                       int & return_status)
  {
    quadninjago_init_impl();
    
    // define the numerator
    GoSamNumerator gosam_numerator(numerator,numerator_t3,numerator_t2,
                                   numerator_d);

    // define s_mat
    SMatrix g_mat(nprops_group,g_mat_data), s_mat;

    // define subset of interal vectors and masses needed
    ScopedArray<RealMomentum> pi(nprops);
    for (int i =0; i<nprops; ++i)
      pi[i] = Vi[ indices[i]-1 ];
    // s-mat
    if (!g_mat.isNull()) {
      s_mat.allocate(nprops);
      for (int i =0; i<nprops; ++i) {
        int im = indices[i]-1;
        s_mat(i,i) = Real();
        for (int j =i+1; j<nprops; ++j) {
          int jm = indices[j]-1;
          s_mat(i,j) = g_mat(im,jm);
          s_mat(j,i) = s_mat(i,j);
        }
      }
    }

    // create a amplitude object and pass the parameters
    Amplitude<Massless> amplitude(nprops,rk,pi.data());
    amplitude
      .setSMatrix(s_mat)
      .setRenormalizationScale(scale2)
      .setCutStop(istop);

    // evaluate the amplitude
    return_status = amplitude.evaluate(gosam_numerator);

    // write the result (the order is reversed)
    tot[2] = amplitude.eps0();
    tot[1] = amplitude.epsm1();
    tot[0] = amplitude.epsm2();
    totr = amplitude.getRationalPart();
  }

#endif // QUADNINJA_MASSLESS

  void quadninjago_clear_integral_cache()
  {
#if defined(NINJA_USE_ONELOOP) && defined(NINJA_USE_ONELOOP_WITH_CACHE)
    if (quadninja::getIntegralLibrary() == (& quadninja::avh_olo))
      avh_olo.clearIntegralCache();
#endif
#ifdef QUADNINJA_USE_LOOPTOOLS
    if (quadninja::getIntegralLibrary() == (& quadninja::loop_tools))
      loop_tools.clearIntegralCache();
#endif
  }

  void quadninjago_free_integral_cache()
  {
#if defined(NINJA_USE_ONELOOP) && defined(NINJA_USE_ONELOOP_WITH_CACHE)
    avh_olo.freeIntegralCache();
#endif
  }

  void quadninjago_set_verbosity(int val)
  {
    setVerbosity(val);
    if (val && !quadninjago_out.is_open()) {
      quadninjago_out.open("ninja_gosam.out", std::ofstream::app);
      if (quadninjago_out.fail()) {
        std::cerr << "ERROR IN NINJA: "
                  << "ninja_gosam.out could not be opened for appending."
                  << std::endl;
        std::terminate();
      }
      using namespace std;
      time_t rawtime;
      tm * timeinfo;
      time (&rawtime);
      timeinfo = localtime (&rawtime);
      quadninjago_out << "----------------------------"
                  << "----------------------------\n\n"
                  << "Ninja called by Gosam on: "
                  << asctime(timeinfo) << "\n" << endl;
    }
    setOutputStream(quadninjago_out);
  }

  void quadninjago_set_test(int val)
  {
    setTest(val);
  }

  void quadninjago_set_test_tolerance(const Real & val)
  {
    quadninja::setTestTolerance(val);
  }

  void quadninjago_set_output_precision(int val)
  {
    quadninjago_out.precision(val);
  }

  void quadninjago_set_integral_library(int flag)
  {
    switch (flag) {
#ifdef QUADNINJA_USE_ONELOOP
    case 1:
      quadninja::setDefaultIntegralLibrary(quadninja::avh_olo);
      break;
#endif
#ifdef QUADNINJA_USE_LOOPTOOLS
    case 2:
      quadninja::setDefaultIntegralLibrary(quadninja::loop_tools);
      break;
#endif
    default:
      std::cerr << "ERROR IN NINJA: "
                << "The flag specified in quadninjago_set_integral_library"
                <<" does not correpond to a valid Integral Library."
                << std::endl;
      std::terminate();      
    }
  }

  void quadninjago_fp_check_default_threshold()
  {
    quadninjago_init_impl();
    quadninja::setDefaultFloatingPointThreshold(REAL_MIN);
  }

  void quadninjago_fp_check_threshold(const Real & thr)
  {
    quadninjago_init_impl();
    quadninja::setDefaultFloatingPointThreshold(thr);
  }

} // extern "C"
