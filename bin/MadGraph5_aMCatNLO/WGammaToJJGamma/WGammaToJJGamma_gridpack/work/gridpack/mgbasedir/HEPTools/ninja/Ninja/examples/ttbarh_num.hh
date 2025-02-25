#ifndef TTBARH_NUM_HH
#define TTBARH_NUM_HH

#include <ninja/num_defs.hh>
#include <ninja/s_mat.hh>

class TTbarHDiagram : public ninja::Numerator {
public:

  virtual ninja::Complex evaluate(const ninja::ComplexMomentum & q,
                                  const ninja::Complex & muq,
                                  int cut, const ninja::PartitionInt part[]);

  virtual void muExpansion(const ninja::ComplexMomentum v_perp[],
                           const ninja::PartitionInt part[],
                           ninja::Complex c[]);

  virtual void t3Expansion(const ninja::ComplexMomentum & a,
                           const ninja::ComplexMomentum & e3,
                           const ninja::ComplexMomentum & e4,
                           const ninja::Complex & param,
                           int mindeg,
                           int cut, const ninja::PartitionInt part[],
                           ninja::Complex c[]);

  virtual void t2Expansion(const ninja::ComplexMomentum & a0,
                           const ninja::ComplexMomentum & a1,
                           const ninja::ComplexMomentum & e3,
                           const ninja::ComplexMomentum & e4,
                           const ninja::Complex param[],
                           int mindeg,
                           int cut, const ninja::PartitionInt part[],
                           ninja::Complex c[]);

public:

  void init(const ninja::RealMomentum k[]);

  ninja::SMatrix & getSMatrix()
  {
    return s_mat;
  }

  ninja::Real * getInternalMasses()
  {
    return msq;
  }

  ninja::RealMomentum * getInternalMomenta()
  {
    return pi;
  }

  static ninja::Real mH;
  static ninja::Real mT;

private:

  void init_global_abbreviations_();

  // internal momenta and masses
  ninja::RealMomentum pi[5];
  ninja::Real msq[5];

  // S-matrix
  ninja::SMatrix s_mat;

  // stucture which stores "global" (i.e. q-independent) abbreviations
  // created by GoSam
  struct Abbrev {
    ninja::Complex * data_;
    Abbrev(): data_(0) {}
    ~Abbrev() { delete [] data_; }
    void allocate(unsigned n) { data_ = new ninja::Complex[n]; }
    bool allocated() const { return data_; }
    ninja::Complex operator ()(unsigned i) const { return data_[i-1]; }
    ninja::Complex & operator ()(unsigned i) { return data_[i-1]; }
  } globabbr;

  // Here we store the kinematic invariants

   ninja::Real es45;
   ninja::Real es12;
   ninja::Real es3 ;
   ninja::Real es51;
   ninja::Real es23;
   ninja::Real es5 ;
   ninja::Real es4 ;
   ninja::Real es34;
   
   ninja::Complex spak1k2, spbk2k1;
   ninja::Complex spak1l3, spbl3k1;
   ninja::Complex spak1l4, spbl4k1;
   ninja::Complex spak1l5, spbl5k1;
   ninja::Complex spak2l3, spbl3k2;
   ninja::Complex spak2l4, spbl4k2;
   ninja::Complex spak2l5, spbl5k2;
   ninja::Complex spal3l4, spbl4l3;
   ninja::Complex spal3l5, spbl5l3;
   ninja::Complex spal4l5, spbl5l4;
   ninja::ComplexMomentum spvak1k2;
   ninja::ComplexMomentum spvak1l3;
   ninja::ComplexMomentum spvak1l4;
   ninja::ComplexMomentum spvak1l5;
   ninja::ComplexMomentum spvak2k1;
   ninja::ComplexMomentum spvak2l3;
   ninja::ComplexMomentum spvak2l4;
   ninja::ComplexMomentum spvak2l5;
   ninja::ComplexMomentum spval3k1;
   ninja::ComplexMomentum spval3k2;
   ninja::ComplexMomentum spval3l4;
   ninja::ComplexMomentum spval3l5;
   ninja::ComplexMomentum spval4k1;
   ninja::ComplexMomentum spval4k2;
   ninja::ComplexMomentum spval4l3;
   ninja::ComplexMomentum spval4l5;
   ninja::ComplexMomentum spval5k1;
   ninja::ComplexMomentum spval5k2;
   ninja::ComplexMomentum spval5l3;
   ninja::ComplexMomentum spval5l4;
   ninja::RealMomentum k1;
   ninja::RealMomentum k2;
   ninja::RealMomentum k3;
   ninja::RealMomentum k4;
   ninja::RealMomentum k5;
   ninja::RealMomentum l3;
   ninja::RealMomentum l4;
   ninja::RealMomentum l5;

  // Polarisation vectors and related symbols
   ninja::ComplexMomentum e1;
   ninja::ComplexMomentum e2;
   ninja::Complex spak1e1, spbe1k1;
   ninja::Complex spae1k1, spbk1e1;
   ninja::Complex spak1e2, spbe2k1;
   ninja::Complex spae1k2, spbk2e1;
   ninja::Complex spae1l3, spbl3e1;
   ninja::Complex spae1l4, spbl4e1;
   ninja::Complex spae1l5, spbl5e1;
   ninja::Complex spak2e2, spbe2k2;
   ninja::Complex spae2k2, spbk2e2;
   ninja::Complex spae2l3, spbl3e2;
   ninja::Complex spae2l4, spbl4e2;
   ninja::Complex spae2l5, spbl5e2;
   ninja::Complex spae1e2, spbe2e1;
   ninja::ComplexMomentum spvak1e1, spvae1k1;
   ninja::ComplexMomentum spvak1e2, spvae2k1;
   ninja::ComplexMomentum spvak2e1, spvae1k2;
   ninja::ComplexMomentum spvak2e2, spvae2k2;
   ninja::ComplexMomentum spval3e1, spvae1l3;
   ninja::ComplexMomentum spval3e2, spvae2l3;
   ninja::ComplexMomentum spval4e1, spvae1l4;
   ninja::ComplexMomentum spval4e2, spvae2l4;
   ninja::ComplexMomentum spval5e1, spvae1l5;
   ninja::ComplexMomentum spval5e2, spvae2l5;
   ninja::ComplexMomentum spvae1e2, spvae2e1;

};

#endif // TTBARH_NUM_HH
