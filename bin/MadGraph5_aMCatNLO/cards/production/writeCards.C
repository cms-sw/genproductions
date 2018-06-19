#define _USE_MATH_DEFINES
#include <cmath> 
#include <math.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>

double lambda(double xh, double xf);
double calcWidth(double m_bstar);

void writeCards(double m_bstar, std::string coupling, std::string path) {

  std::string filename;
  std::stringstream stream;
  stream << std::fixed << std::setprecision(0) << m_bstar;
  filename = path + "bstar_tW_" + coupling +"_M" + stream.str() + "_customizecards.dat";
  int l, r;
  if (coupling=="Left")
    {
      l = 1;
      r = 0;
    }
  else
    {
      l = 0;
      r = 1;
    }

  ofstream f;
  f.open(filename);
  f << "set param_card frblock 1 " << l << std::endl;
  f << "set param_card frblock 2 " << r << std::endl;
  f << "set param_card mass 1005 " << stream.str() << std::endl;
  f << "set param_card DECAY 1005 " << calcWidth(m_bstar) << std::endl;
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
