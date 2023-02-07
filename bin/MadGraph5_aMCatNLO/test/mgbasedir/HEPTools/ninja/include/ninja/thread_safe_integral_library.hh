// This is a template class which defines a thread-safe integral
// library, from a possibly thread-unsafe one.  This class is derived
// from the one defined by its template parameter (which in turn
// should be derived from IntegralLibrary).  We use mutexes (defined
// by POSIX threads) to control access to the integral library
// routines.


#ifndef NINJA_THREAD_SAFE_INTEGRAL_LIBRARY
#define NINJA_THREAD_SAFE_INTEGRAL_LIBRARY

#include <pthread.h>
#include <ninja/integral_library.hh>


namespace ninja {

  // simple class for a mutex
  class SimpleMutex {
  public:

    SimpleMutex()
    {
      pthread_mutex_init(& mutex_, NULL);
    }

    ~SimpleMutex()
    {
      pthread_mutex_destroy(& mutex_);
    }

    void lock()
    {
      pthread_mutex_lock(& mutex_);
    }

    void unlock()
    {
      pthread_mutex_unlock(& mutex_);
    }

  private:
    pthread_mutex_t mutex_;    
  };


  // this class unlocks a mutex when goes out of scope (but, unlike a
  // lock guard, it doesn't lock it in the constructor)
  class SimpleMutexUnlocker {
  public:

    SimpleMutexUnlocker(SimpleMutex & mutex): mutex_(mutex) {}

    ~SimpleMutexUnlocker()
    {
      mutex_.unlock();
    }

  private:
    SimpleMutex & mutex_;
  };


  template <typename BaseLib_>
  class ThreadSafeIntegralLibrary : public BaseLib_ {
  public:

    ThreadSafeIntegralLibrary(): BaseLib_(), mutex_() {}

    virtual void init(Real muRsq)
    {
      mutex_.lock();
      BaseLib_::init(muRsq);
    }

    virtual void exit()
    {
      SimpleMutexUnlocker unlocker(mutex_);
      BaseLib_::exit();
    }

  private:
    SimpleMutex mutex_;
  };

} // namespace ninja

#endif // NINJA_THREAD_SAFE_INTEGRAL_LIBRARY
