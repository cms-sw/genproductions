// thread_4photons.cc

#include <algorithm>
#include <pthread.h>
#include <cassert>

#include <ninja/ninja.hh>
#include <ninja/rambo.hh>

#include <ninja/thread_safe_integral_library.hh>

#ifdef NINJA_USE_ONELOOP
# include <ninja/avholo.hh>
#endif

#ifdef NINJA_USE_LOOPTOOLS
# include <ninja/looptools.hh>
#endif

// Choose an integral library to use as the base one
#if defined(NINJA_USE_ONELOOP)
typedef ninja::AvHOneLoop BaseIntegralLibrary;
#elif defined(NINJA_USE_LOOPTOOLS)
typedef ninja::LoopTools BaseIntegralLibrary;
#else
# error "Sorry, this example only works with OneLoop or LoopTools enabled."
#endif

#include "4photons_num.hh"
using namespace ninja;


namespace {

  struct ThreadData {
    Complex mass;
    const char * helicities;
    const RealMomentum * momenta;
    Amplitude<ComplexMasses> * amp;
    Complex result;
  };


  // this is the function passed to each thread
  void * getAmplitude(void * thread_data_in)
  {
    ThreadData * thread_data = static_cast<ThreadData *>(thread_data_in);
    const RealMomentum * k = thread_data->momenta;
    const char * helicities = thread_data->helicities;
    const Complex mass = thread_data->mass;
    Amplitude<ComplexMasses> & amp = *(thread_data->amp);
    thread_data->result = 0;

    // here we do the same as in the non-threaded version, for one
    // phase space point
    RealMomentum k_in[4];
    char hel_in[4];
    int permutation[4] = {0,1,2,3};
    FourPhotons diagram;

    amp.reset();

    do {

      if (permutation[1]>permutation[3])
        continue;
    
      for (int i=0; i<4; ++i) {
        k_in[i] = k[permutation[i]];
        hel_in[i] = helicities[permutation[i]];
      }

      diagram.init(k_in,mass,hel_in);
      amp.setKinematics(diagram.getInternalMomenta());
      amp.setInternalMasses(diagram.getInternalMasses());
      amp.setSMatrix(diagram.getSMatrix());
      amp.evaluate(diagram);

    } while (std::next_permutation(permutation+1,permutation+4));

    thread_data->result = amp[0];

    return NULL;
  }

} // namespace


int main()
{
  const int N_THREADS = 4;
  const int EVENTS_PER_THREAD = 100;
  const Real CM_ENERGY = 14;

  // fermion mass, with width
  Complex mass = Complex(10)-I;

  // either use a thread-safe integral library per thread
#ifdef NINJA_USE_ONELOOP

  AvHOneLoop my_integral_lib[N_THREADS];
  avh_olo.init(1);

#else   // ... or make a gloabally thread-safe one

  ThreadSafeIntegralLibrary<BaseIntegralLibrary> my_integral_lib;
  my_integral_lib.init(1);
  setDefaultIntegralLibrary(my_integral_lib);

#endif

  // thread data: kinematics and helicities
  ThreadData thread_data[N_THREADS];
  const char helicities[] = "++++";
  RealMomentum k[N_THREADS][4];

  // phase space generation
  Rambo gen(CM_ENERGY*CM_ENERGY,4);

  // print the banner and avoid race conditions
  printBanner();

  // initialize N_THREADS amplitude objects
  Amplitude<ComplexMasses> amp[N_THREADS];

  for (int it=0; it<N_THREADS; ++it) {
    amp[it].setN(4).setRank(4);
#ifdef NINJA_USE_ONELOOP
    amp[it].setIntegralLibrary(my_integral_lib[it]);
#endif
  }


  // note: One might want to invert the following loops or use a
  // thread pool, in order to be more efficient (this also depends on
  // the thread-safety of the phase space generator), but here we only
  // want to check that multithreaded applications can work with calls
  // of Amplitude methods.

  for (int i=0; i<EVENTS_PER_THREAD; ++i) {

    pthread_t threads[N_THREADS];

    for (int it=0; it<N_THREADS; ++it) {

      gen.getMomenta(k[it]);
      for (int i=2; i<4; ++i)
        k[it][i] *= -1;

      thread_data[it].mass = mass;
      thread_data[it].momenta = k[it];
      thread_data[it].helicities = helicities;
      thread_data[it].amp = & amp[it];

      int thread_creation = pthread_create(&threads[it], NULL,
                                           getAmplitude,
                                           static_cast<void*>(&thread_data[it]));
      assert(thread_creation == 0);

    }

    for (int it=0; it<N_THREADS; ++it) {

      int thread_status = pthread_join(threads[it], NULL);
      assert(thread_status == 0);

#ifdef NINJA_USE_ONELOOP
      my_integral_lib[it].clearIntegralCache();
#endif

      // here thread_data[it].result gives the finite part of the
      // amplitude in the computed phase space point

    }

#if !defined(NINJA_USE_ONELOOP)
    my_integral_lib.clearIntegralCache();
#endif
  }

  // print the result of the last computed p.s.p.
  std::cout << "Finite part:  "
            << thread_data[N_THREADS-1].result << std::endl
            << "Abs. val.:   " << abs(thread_data[N_THREADS-1].result)
            << std::endl;

}
