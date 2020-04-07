// main01.cc is a part of the PYTHIA event generator.
// Copyright (C) 2012 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// This is a simple test program. It fits on one slide in a talk. 
// It studies the charged multiplicity distribution at the LHC.

#include "Pythia8/Pythia.h"
#include "Pythia8Plugins/LHAFortran.h"
#include <sstream>

// RIVET
// uncomment the following is want to use pipe to analyze events with
// Rivet.
//#include "Pythia8Plugins/Pythia8Rivet.h"

// NOT NEEDED ANYMORE WITH NEWER PY8 VERSION
//#include "Pythia8/Pythia8ToHepMC.h"
//#include "HepMC/GenEvent.h"
//#include "HepMC/IO_GenEvent.h"
//#include "HepMC/Units.h"

using namespace Pythia8; 

extern "C" {
  extern struct {
    int py8tune;
    int nohad;
  } cpy8tune_;
}

class myLHAupFortran : public LHAupFortran {

protected:
  // User-written routine that does the intialization and fills heprup.
  virtual bool fillHepRup() {return true;}

  // User-written routine that does the event generation and fills hepeup.
  virtual bool fillHepEup() {return true;}

};


class MyUserHooks : public UserHooks {

public:

  MyUserHooks() {cout << "Setting up Hook";}

  // Destructor deletes anti-kT jet finder.
  ~MyUserHooks() {;}

  // Allow process cross section to be modified..
private:

};

Pythia pythia;
myLHAupFortran* LHAinstance=new myLHAupFortran();
//MyUserHooks* MyHook=new MyUserHooks();

// RIVET
//Pythia8Rivet rivet(pythia, "outputfile.yoda"); 


extern "C" {
  // F77 interface to pythia8
  void pythia_option0_(char *string) {
    pythia.readString(string);
  }

  void pythia_init_() {
    // Generator. Process selection. LHC initialization. Histogram.
    //  pythia.readString("Beams:eCM = 8000.");    
    //  pythia.readString("HardQCD:all = on");    
    //  pythia.readString("PhaseSpace:pTHatMin = 20.");
    
    // disable the listing of the changed setting (too long)
    pythia.readString("Init:showChangedParticleData = off");

    // shower vetoing
    pythia.readString("SpaceShower:pTmaxMatch = 1");
    pythia.readString("TimeShower:pTmaxMatch = 1");

    pythia.readString("SpaceShower:QEDshowerByQ = off"); // From quarks.        
    pythia.readString("SpaceShower:QEDshowerByL = off"); // From Leptons.       
    pythia.readString("TimeShower:QEDshowerByQ = off"); // From quarks.         
    pythia.readString("TimeShower:QEDshowerByL = off"); // From Leptons.

    // GZ 26/4/2018 (checking effect of photon radiation, not our default setting) 
    //pythia.readString("SpaceShower:QEDshowerByL = on"); // From Leptons
    //pythia.readString("TimeShower:QEDshowerByL = on"); // From Leptons..       

    // tune
    if(cpy8tune_.py8tune == 14) {
      pythia.readString("Tune:pp = 14"); // Monash2013 tune
      cout << "pythia8F77: setting pythia tune 14";
      //      pythia.readString("Tune:pp = 5"); // Default
    }

    pythia.readString("PartonLevel:MPI = off");
    if(cpy8tune_.nohad == 1) {
      pythia.readString("HadronLevel:All = off");
    }


    pythia.readString("25:mayDecay = off"); 

    pythia.readString("111:mayDecay = off"); 
    pythia.readString("521:mayDecay = off"); 
    pythia.readString("-521:mayDecay = off"); 
    pythia.readString("511:mayDecay = off"); 
    pythia.readString("-511:mayDecay = off"); 
    pythia.readString("531:mayDecay = off"); 
    pythia.readString("-531:mayDecay = off"); 
    pythia.readString("5222:mayDecay = off"); 
    pythia.readString("5112:mayDecay = off"); 
    pythia.readString("5232:mayDecay = off"); 
    pythia.readString("-5132:mayDecay = off"); 
    pythia.readString("5132:mayDecay = off"); 
    pythia.readString("541:mayDecay = off"); 
    pythia.readString("-541:mayDecay = off"); 
    pythia.readString("553:mayDecay = off"); 
    pythia.readString("-5112:mayDecay = off"); 
    pythia.readString("-5222:mayDecay = off"); 
    pythia.readString("-5122:mayDecay = off"); 
    pythia.readString("5332:mayDecay = off"); 
    pythia.readString("-5232:mayDecay = off"); 
    pythia.readString("-5332:mayDecay = off"); 
    pythia.readString("5122:mayDecay = off"); 

    pythia.readString("Beams:frameType = 5");
    //    pythia.setUserHooksPtr(MyHook);
    
    pythia.setLHAupPtr(LHAinstance); 
    LHAinstance->setInit();  
    pythia.init();

    // RIVET
    //    rivet.addAnalysis("MC_JETS"); 
    //rivet.addAnalysis("LH2017_ZHJETS"); 
    //rivet.addAnalysis("CMS_RAD");
    //rivet.addAnalysis("CMS_RAD_lead");
    //rivet.addAnalysis("CMS_RAD_2lead");
    
  }

  void pythia_next_(int & iret){
  // Begin event loop. Generate event. Skip if error. List first one.
    iret = pythia.next();

    // RIVET
    //rivet(); 


    
    // // uncomment the following is want to use pipe to analyze events with Rivet.
    // // Construct new empty HepMC event and fill it.
    // // Units will be as chosen for HepMC build, but can be changed by arguments, e.g. GenEvt( HepMC::Units::GEV, HepMC::Units::MM)
    // HepMC::GenEvent* hepmcevt = new HepMC::GenEvent();
    // ToHepMC.fill_next_event( pythia , hepmcevt );
    // // Write the HepMC event to file. Done with it.
    // ascii_io << hepmcevt;
    // delete hepmcevt;             
  }

  void pythia_to_hepevt_(const int &nmxhep, int & nhep, int * isthep,
			 int * idhep,
			 int  (*jmohep)[2], int (*jdahep)[2],
			 double (*phep)[5], double (*vhep)[4]) {
    nhep = pythia.event.size();
    if(nhep>nmxhep) {cout << "too many particles!" ; exit(-1); }
    for (int i = 0; i < pythia.event.size(); ++i) {
      //WWarn
      //      *(isthep+i) = pythia.event[i].statusHepMC(); !ER: !!!!!!
      *(isthep+i) = pythia.event[i].status();
      *(idhep+i) = pythia.event[i].id();
      // All pointers should be increased by 1, since here we follow
      // the c/c++ convention of indeces from 0 to n-1, but i fortran
      // they are from 1 to n.
      (*(jmohep+i))[0] = pythia.event[i].mother1() + 1;
      (*(jmohep+i))[1] = pythia.event[i].mother2() + 1;
      (*(jdahep+i))[0] = pythia.event[i].daughter1() + 1;
      (*(jdahep+i))[1] = pythia.event[i].daughter2() + 1;
      (*(phep+i))[0] = pythia.event[i].px();
      (*(phep+i))[1] = pythia.event[i].py();
      (*(phep+i))[2] = pythia.event[i].pz();
      (*(phep+i))[3] = pythia.event[i].e();
      (*(phep+i))[4] = pythia.event[i].m();
    }
    // override mother of very first event, set to 0
    *(jmohep)[0] = 0 ;
    *(jmohep)[1] = 0 ;

    // print every event 
    //pythia.event.list(); 
  }

  void pythia_stat_() {
    pythia.stat();
    // RIVET
    //rivet.done(); 
  }

}

