#define _USE_MATH_DEFINES
#include <cmath> 
#include <math.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>

double lambda(double xh, double xf);
double calcWidth(double m_bstar);

void writeCards( double m_bstar, double m_h, std::string coupling, std::string path) {

  std::string filename;
  std::stringstream stream;
  std::stringstream stream2;

  stream << std::fixed << std::setprecision(0) << m_bstar;
  stream2 << std::fixed << std::setprecision(0) << m_h;

  float l, r;
  if (coupling=="LH")
    {
      l = 1.;
      r = 0;

    }
  else
    {
      l = 0;
      r = 1.;
    }


  filename = path + "PairVLQ_x53x53_tHtH_narrow_" + coupling +"_MX" + stream.str() + "_MH"+ stream2.str() + "_customizecards.dat";
  ofstream f;
  f.open(filename);

  f << "set param_card KXSPuL 1 0 " << std::endl;
  f << "set param_card KXSPuL 2 0 " << std::endl;
  f << "set param_card KXSPuL 3 " << l << std::endl;
  f << "set param_card KXSPuR 1 0 " << std::endl;
  f << "set param_card KXSPuR 2 0 " << std::endl;
  f << "set param_card KXSPuR 3 " << r << std::endl;
  f << "set param_card KSPEN 1 0 " << std::endl;
  f << "set param_card KSPEN 2 0 " << std::endl; 
  f << "set param_card KSPEN 3 0 " << std::endl;
  f << "set param_card KSPEN 4 0 " << std::endl;
  f << "set param_card KSPEN 5 0 " << std::endl;
  f << "set param_card KSPEN 6 0 " << std::endl;
  f << "set param_card KSPEN 7 0 " << std::endl;
  f << "set param_card KSPEN 8 0 " << std::endl;
  f << "set param_card KSPEN 9 0 " << std::endl;
  f << "set param_card KSPUDL 1 0 " << std::endl; 
  f << "set param_card KSPUDL 2 0 " << std::endl;
  f << "set param_card KSPUDL 3 0 " << std::endl;
  f << "set param_card KSPUDL 4 0 " << std::endl;
  f << "set param_card KSPUDL 5 0 " << std::endl;
  f << "set param_card KSPUDL 6 0 " << std::endl;
  f << "set param_card KSPUDL 7 0 " << std::endl;
  f << "set param_card KSPUDL 8 0 " << std::endl;
  f << "set param_card KSPUDL 9 1 " << std::endl;
  f << "set param_card KSPUDR 1 0 " << std::endl;
  f << "set param_card KSPUDR 2 0 " << std::endl;
  f << "set param_card KSPUDR 3 0 " << std::endl;
  f << "set param_card KSPUDR 4 0 " << std::endl;
  f << "set param_card KSPUDR 5 0 " << std::endl;
  f << "set param_card KSPUDR 6 0 " << std::endl;
  f << "set param_card KSPUDR 7 0 " << std::endl;
  f << "set param_card KSPUDR 8 0 " << std::endl;
  f << "set param_card KSPUDR 9 1 " << std::endl;
  f << "set param_card mass 6000007 " << stream.str() << std::endl;
  f << "set param_card mass 6100027 " << stream2.str() << std::endl;
  f << "set param_card mass 6200027 500" << std::endl;
  f << "set param_card mass 71000027 500" << std::endl;
  f << "set param_card mass 72000027 500" << std::endl;
  f << "set param_card decay 6000007 auto" << std::endl;
  f << "set param_card decay 6100027 auto" << std::endl;
  f.close();
}

 
double calcWidth(double m_bstar)
{
  double g2 = 0.65189214;
  double gs = 1.2177158;

  double m_w     = 80.267; // W mass in GeV
  double m_z     = 91.545; // Z mass in GeV
  double m_t     = 172.5; // top mass in GeV
  double m_h     = 125.09; // Higgs mass in GeV
  double m_b     = 4.2;  // b quark mass in GeV

  double vev     = 2 * m_w / g2; // Higgs vacuum expectation value in GeV
  double k_l     = 0.5;
  double k_r     = 0.;
  double c_w = m_w / m_z;

  double f_l     = vev / m_bstar;
  double f_r     = 0.;
  double x_t = m_t / m_bstar;
  double x_z = m_z / m_bstar;
  double x_h = m_h / m_bstar;
  double x_w = m_w / m_bstar;
  double x_b = m_b / m_bstar;


      double width_bz = pow(g2,2.)/(64. * M_PI * 2. * pow(c_w,2.)) * pow(m_bstar,3.) / pow(m_z,2.)
	* sqrt(lambda(x_b, x_z)) * (pow(f_l,2.) + pow(f_r,2.)) 
	* (1. + pow(x_z,2.) - 2. * pow(x_b,2.) - 2. * pow(x_z,4.) + pow(x_z,2.) * pow(x_b,2.) + pow(x_b,4.)) - 12.*f_l*f_r*pow(x_z,2.)*x_b;

      double width_tw = pow(g2,2.)/(64. * M_PI) * pow(m_bstar,3.) / pow(m_w,2.)
	* sqrt(lambda(x_t, x_w)) * (pow(f_l,2.) + pow(f_r,2.)) 
	* (1. + pow(x_w,2.) - 2. * pow(x_t,2.) - 2. * pow(x_w,4.) + pow(x_w,2.) * pow(x_t,2.) + pow(x_t,4.)) - 12.*f_l*f_r*pow(x_w,2.)*x_t;

      double width_bh = pow(g2,2.) / (2 * 64. * M_PI) * pow(m_bstar,3.) / pow(m_w,2.)
	* sqrt(lambda(x_b,x_h)) * (pow(f_l,2.) + pow(f_r,2.)) 
	*(1. + pow(x_b,2.) - pow(x_h,2.)) - 4. * f_l * f_r * x_b;

      double width_bg = 4./3. * pow(gs,2.) / (32. * M_PI) * m_bstar
	* sqrt(lambda(x_b,0)) * (pow(k_l,2.) + pow(k_r,2.)) 
	*(2. - pow(0.,2.) - 4. * pow(x_b,2.) - pow(0.,4.) -pow(0.,2.) * pow(x_b,2.) + 2. * pow(x_b,4.)) - 12.*k_l*k_r*pow(0.,2.)*x_b;
      double width = width_bz + width_tw + width_bh+ width_bg;

  return width;
}

double lambda(double xh, double xf) {
  return ( 1.0 + pow(xh,4.) + pow(xf,4.) - 2.0 * pow(xh,2.) - 2.0 * pow(xf,2.) - 2.0 * pow(xh,2.) * pow(xf,2.) );
}
