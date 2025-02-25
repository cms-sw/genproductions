// Implements a C wrapper of the AvHOneLoop IntegralLibrary interface.
// This is exported in the ninjavholo_module defined in
// ninjavholo.F90.  They may also be called directly from fortran-77
// when using either gfortran or ifort.  (note: Since it uses the
// global instance ninja::avh_olo, it is not thread-safe).

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <ninja/avholo.hh>
using namespace ninja;

// declaration of the interface
extern "C" {

  void ninjavholo_set_ir_threshold_(const Real & threshold);

  void ninjavholo_clear_cache_();
  void ninjavholo_free_cache_();

  void ninjavholo_init_(const Real & mur2);

  void ninjavholo_get_mi4_rm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s43, const Real & s14,
                              const Real & s31, const Real & s42,
                              const Real & m1, const Real & m2,
                              const Real & m3, const Real & m4);
  void ninjavholo_get_mi4_cm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s43, const Real & s14,
                              const Real & s31, const Real & s42,
                              const Complex & m1, const Complex & m2,
                              const Complex & m3, const Complex & m4);

  void ninjavholo_get_mi3_rm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s13,
                              const Real & m1, const Real & m2,
                              const Real & m3);
  void ninjavholo_get_mi3_cm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s13,
                              const Complex & m1, const Complex & m2,
                              const Complex & m3);

  void ninjavholo_get_mi2_rank2_rm_(Complex b11[3],
                                    Complex b1[3], Complex b0[3],
                                    const Real & s21, const Real & m1,
                                    const Real & m2);
  void ninjavholo_get_mi2_rank2_cm_(Complex b11[3],
                                    Complex b1[3], Complex b0[3],
                                    const Real & s21,
                                    const Complex & m1,
                                    const Complex & m2);

  void ninjavholo_get_mi1_rm_(Complex rslt[3], const Real & m0);
  void ninjavholo_get_mi1_cm_(Complex rslt[3], const Complex & m0);

} // extern "C"


// Implementation
extern "C" {

  void ninjavholo_set_ir_threshold_(const Real & threshold)
  {
    AvHOneLoop::setInfraredThreshold(threshold);
  }

  void ninjavholo_clear_cache_()
  {
    avh_olo.clearIntegralCache();
  }

  void ninjavholo_free_cache_()
  {
    avh_olo.freeIntegralCache();
  }

  void ninjavholo_init_(const Real & mur2)
  {
    avh_olo.init(mur2);
  }

  void ninjavholo_get_mi4_rm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s43, const Real & s14,
                              const Real & s31, const Real & s42,
                              const Real & m1, const Real & m2,
                              const Real & m3, const Real & m4)
  {
    avh_olo.getBoxIntegralRM(rslt, s21, s32, s43, s14, s31, s42,
                             m1, m2, m3, m4);
  }

  void ninjavholo_get_mi4_cm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s43, const Real & s14,
                              const Real & s31, const Real & s42,
                              const Complex & m1, const Complex & m2,
                              const Complex & m3, const Complex & m4)
  {
    avh_olo.getBoxIntegralCM(rslt, s21, s32, s43, s14, s31, s42,
                             m1, m2, m3, m4);
  }

  void ninjavholo_get_mi3_rm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s13,
                              const Real & m1, const Real & m2,
                              const Real & m3)
  {
    avh_olo.getTriangleIntegralRM(rslt, s21, s32, s13, m1, m2, m3);
  }

  void ninjavholo_get_mi3_cm_(Complex rslt[3],
                              const Real & s21, const Real & s32,
                              const Real & s13,
                              const Complex & m1, const Complex & m2,
                              const Complex & m3)
  {
    avh_olo.getTriangleIntegralCM(rslt, s21, s32, s13, m1, m2, m3);
  }

  void ninjavholo_get_mi2_rank2_rm_(Complex b11[3],
                                    Complex b1[3], Complex b0[3],
                                    const Real & s21, const Real & m1,
                                    const Real & m2)
  {
    avh_olo.getRank2BubbleIntegralRM(b11, b1, b0, s21, m1, m2);
  }

  void ninjavholo_get_mi2_rank2_cm_(Complex b11[3],
                                    Complex b1[3], Complex b0[3],
                                    const Real & s21,
                                    const Complex & m1,
                                    const Complex & m2)
  {
    avh_olo.getRank2BubbleIntegralCM(b11, b1, b0, s21, m1, m2);
  }

  void ninjavholo_get_mi1_rm_(Complex rslt[3], const Real & m0)
  {
    avh_olo.getTadpoleIntegralRM(rslt,m0);
  }

  void ninjavholo_get_mi1_cm_(Complex rslt[3], const Complex & m0)
  {
    avh_olo.getTadpoleIntegralCM(rslt,m0);
  }

} // extern "C"
