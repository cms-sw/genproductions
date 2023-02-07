#ifndef AVHOLO_DECLS_HH
#define AVHOLO_DECLS_HH

#include <quadsources/tmp_utils.hh>
#include <quadninja/types.hh>


// Declarations of functions in OneLoop

extern "C" {
  
  // set the threshold to distinguish between IR-divergent and
  // IR-finite cases
  void quadninjavholo_onshell(const quadninja::Real & thrs);

  // 4-point MI
  void quadninjavholo_d0_rm(quadninja::Complex rslt[3],
                        const quadninja::Real & p1, const quadninja::Real & p2,
                        const quadninja::Real & p3, const quadninja::Real & p4,
                        const quadninja::Real & p12, const quadninja::Real & p23,
                        const quadninja::Real & m1, const quadninja::Real & m2,
                        const quadninja::Real & m3, const quadninja::Real & m4,
                        const quadninja::Real & mur);
  void quadninjavholo_d0_cm(quadninja::Complex rslt[3],
                        const quadninja::Complex & p1, const quadninja::Complex & p2,
                        const quadninja::Complex & p3, const quadninja::Complex & p4,
                        const quadninja::Complex & p12, const quadninja::Complex & p23,
                        const quadninja::Complex & m1, const quadninja::Complex & m2,
                        const quadninja::Complex & m3, const quadninja::Complex & m4,
                        const quadninja::Real & mur);

  // 3-point MI
  void quadninjavholo_c0_rm(quadninja::Complex rslt[3],
                        const quadninja::Real & p1,const quadninja::Real & p2,
                        const quadninja::Real & p3,
                        const quadninja::Real & m1, const quadninja::Real & m2,
                        const quadninja::Real & m3, const quadninja::Real & mur);
  void quadninjavholo_c0_cm(quadninja::Complex rslt[3],
                        const quadninja::Complex & p1,const quadninja::Complex & p2,
                        const quadninja::Complex & p3,
                        const quadninja::Complex & m1, const quadninja::Complex & m2,
                        const quadninja::Complex & m3, const quadninja::Real & mur);

  // 2-point MIs
  void quadninjavholo_b0_rm(quadninja::Complex rslt[3], const quadninja::Real & p1,
                        const quadninja::Real & m1, const quadninja::Real & m2,
                        const quadninja::Real & mur);
  void quadninjavholo_b11_rm(quadninja::Complex b11[3], quadninja::Complex b00[3],
                         quadninja::Complex b1[3], quadninja::Complex b0[3],
                         const quadninja::Real & p1, const quadninja::Real & m1,
                         const quadninja::Real & m2, const quadninja::Real & mur);
  void quadninjavholo_b0_cm(quadninja::Complex rslt[3], const quadninja::Complex & p1,
                        const quadninja::Complex & m1, const quadninja::Complex & m2,
                        const quadninja::Real & mur);
  void quadninjavholo_b11_cm(quadninja::Complex b11[3], quadninja::Complex b00p[3],
                         quadninja::Complex b1[3], quadninja::Complex b0[3],
                         const quadninja::Complex & p1, const quadninja::Complex & m1,
                         const quadninja::Complex & m2, const quadninja::Real & mur);
  
  // 1-point MI
  void quadninjavholo_a0_rm(quadninja::Complex rslt[3],
                        const quadninja::Real & m0, const quadninja::Real & mur);
  void quadninjavholo_a0_cm(quadninja::Complex rslt[3],
                        const quadninja::Complex & m0, const quadninja::Real & mur);

} // extern "C"

#endif // AVHOLO_DECLS_HH
