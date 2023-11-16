#define _USE_MATH_DEFINES
#include <cmath> 
#include <math.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>

void writeCards_wide(double masspoint, double pct, std::string path, std::string dirname) {

  double width = masspoint*pct;
  std::string filename;
  std::stringstream stream;
  std::stringstream stream_width;
  stream << std::fixed << std::setprecision(0) << masspoint;
  stream_width << std::fixed << std::setprecision(0) << width;
  filename = path + dirname + "_M" + stream.str() + "_customizecards.dat";
  ofstream f;
  f.open(filename);
  f << "set param_card mass 6000007 " << stream.str() << std::endl;
  f << "set param_card decay 6000007 " << stream_width.str() << std::endl;
  f.close();
}
