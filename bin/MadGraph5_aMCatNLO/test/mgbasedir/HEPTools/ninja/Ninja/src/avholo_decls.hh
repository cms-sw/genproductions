#ifndef AVHOLO_DECLS_HH
#define AVHOLO_DECLS_HH

#include <tmp_utils.hh>
#include <ninja/types.hh>


// Declarations of functions in OneLoop

extern "C" {
  
  // set the threshold to distinguish between IR-divergent and
  // IR-finite cases
  void ninjavholo_onshell(const ninja::Real & thrs);

  // 4-point MI
  void ninjavholo_d0_rm(ninja::Complex rslt[3],
                        const ninja::Real & p1, const ninja::Real & p2,
                        const ninja::Real & p3, const ninja::Real & p4,
                        const ninja::Real & p12, const ninja::Real & p23,
                        const ninja::Real & m1, const ninja::Real & m2,
                        const ninja::Real & m3, const ninja::Real & m4,
                        const ninja::Real & mur);
  void ninjavholo_d0_cm(ninja::Complex rslt[3],
                        const ninja::Complex & p1, const ninja::Complex & p2,
                        const ninja::Complex & p3, const ninja::Complex & p4,
                        const ninja::Complex & p12, const ninja::Complex & p23,
                        const ninja::Complex & m1, const ninja::Complex & m2,
                        const ninja::Complex & m3, const ninja::Complex & m4,
                        const ninja::Real & mur);

  // 3-point MI
  void ninjavholo_c0_rm(ninja::Complex rslt[3],
                        const ninja::Real & p1,const ninja::Real & p2,
                        const ninja::Real & p3,
                        const ninja::Real & m1, const ninja::Real & m2,
                        const ninja::Real & m3, const ninja::Real & mur);
  void ninjavholo_c0_cm(ninja::Complex rslt[3],
                        const ninja::Complex & p1,const ninja::Complex & p2,
                        const ninja::Complex & p3,
                        const ninja::Complex & m1, const ninja::Complex & m2,
                        const ninja::Complex & m3, const ninja::Real & mur);

  // 2-point MIs
  void ninjavholo_b0_rm(ninja::Complex rslt[3], const ninja::Real & p1,
                        const ninja::Real & m1, const ninja::Real & m2,
                        const ninja::Real & mur);
  void ninjavholo_b11_rm(ninja::Complex b11[3], ninja::Complex b00[3],
                         ninja::Complex b1[3], ninja::Complex b0[3],
                         const ninja::Real & p1, const ninja::Real & m1,
                         const ninja::Real & m2, const ninja::Real & mur);
  void ninjavholo_b0_cm(ninja::Complex rslt[3], const ninja::Complex & p1,
                        const ninja::Complex & m1, const ninja::Complex & m2,
                        const ninja::Real & mur);
  void ninjavholo_b11_cm(ninja::Complex b11[3], ninja::Complex b00p[3],
                         ninja::Complex b1[3], ninja::Complex b0[3],
                         const ninja::Complex & p1, const ninja::Complex & m1,
                         const ninja::Complex & m2, const ninja::Real & mur);
  
  // 1-point MI
  void ninjavholo_a0_rm(ninja::Complex rslt[3],
                        const ninja::Real & m0, const ninja::Real & mur);
  void ninjavholo_a0_cm(ninja::Complex rslt[3],
                        const ninja::Complex & m0, const ninja::Real & mur);

} // extern "C"

#endif // AVHOLO_DECLS_HH
