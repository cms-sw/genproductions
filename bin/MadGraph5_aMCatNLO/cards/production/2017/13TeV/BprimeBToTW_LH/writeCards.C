#define _USE_MATH_DEFINES
#include <cmath> 
#include <math.h>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>

void writeCards(double masspoint, std::string path) {

  std::string filename;
  std::stringstream stream;
  stream << std::fixed << std::setprecision(0) << masspoint;
  filename = path + "BprimeBToTW_LH_narrowLO_M" + stream.str() + "_customizecards.dat";
  ofstream f;
  f.open(filename);
  f << "set param_card mass 6000007 " << stream.str() << std::endl;
  f.close();
}
