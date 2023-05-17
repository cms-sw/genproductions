#include "HepMC/GenEvent.h"

int main( ){
  HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();
  hepmcevt->weights().push_back(1.0,"00");
  delete hepmcevt;
  return 0;
}
