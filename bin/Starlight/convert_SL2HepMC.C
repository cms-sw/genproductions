/* Routine for the conversion of starlight data format
 * into something readable by CMSSW_1_4_5
 *
 * Modification by X. Rouby on a routine kindly offered by J. Hollar
 * Sept. 28, 2007

https://twiki.cern.ch/twiki/pub/CMS/UpsilonPhotoproduction/convert_starlight.C
https://twiki.cern.ch/twiki/bin/view/CMSPublic/SWGuideHeavyIonExclusiveUpsilonDileptonsAnalysis
http://cmssw.cvs.cern.ch/cgi-bin/cmssw.cgi/UserCode/kumarv/Input/StarToHepmc.C?view=markup

* Modifications by IK:
* -- N_particles derived from starlight EVENT record
* -- (-evt_n) written as barcode for HepMC V record
* -- TParticle used to derive masses from PDG codes

*/ 

#include <TStyle.h>
#include <TROOT.h>
#include <TApplication.h>

#include "TParticle.h"

#include "TFile.h"
#include "TTree.h"
#include "TRandom3.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cmath>
#include <stdio.h>

using namespace std;

//void convert_starlight(int num=0, ifstream &infile = 0, string outfilename = "slight");
void convert_SL2HepMC(int number_of_files=1, string filename = "slight.out", string outfilename = "starlight_hepMCtest" );

void convert_starlightRandFlip(int num, ifstream &infile, string outfilename) //makeEventsFile
{
  char ofName[100];
  sprintf(ofName,"%s%i.hepmc",outfilename.c_str(),num);
   //input ///tmp/katkov/starlight_dpmjet_pPb.out //slight.out //starlight_pythia_pPb.out
  ofstream output(ofName);

  string temp_string, temp;
  istringstream curstring;
  int N = 0;
  int N2 = 0; // N_particles (set for every even independently in the code)
  int M = 4000; // N_events
  int K = num*M+1;  // first event  //num*M+1
  //const double MU = 0.105658369; // muon mass [GeV] 
  //const double MU = 0.51099892811e-3; //electron mass [GeV]
  double MU;
  int evt_n=0; // event_number, read from the input-file
  int nn=0; // event_counter, in the output file
  int signFlip=1;
  TRandom3 random;

  // GENER: STL  1.0            1           1           1           1  200.00     999.999     CMS
  //getline(infile,temp_string); // The very first line is useless --> and does not seem to exist now !
  

  output <<  "HepMC::IO_Ascii-START_EVENT_LISTING" << endl;

  while (getline(infile,temp_string)) {
	
	curstring.clear(); // needed when using several tims istringstream::str(string)
	curstring.str(temp_string);

	if(strstr(temp_string.c_str(),"EVENT"))	{
		curstring >> temp >> evt_n >> N; //assuming that EVENT always preceeds VERTEX/TRACK so that N is set correctly
		// EVENT:          1       2       1
      		if(evt_n >=K && evt_n < K+M)  {
			output << "E " << evt_n << " -1.0000000000000000e+00 -1.0000000000000000e+00 -1.0000000000000000e+00 20 " << evt_n << " 1 0 0" << endl;
			nn++;
		}
		N2=0;
                if(random.Rndm()>.5) signFlip=-1;
		else signFlip=1;

	} else if(strstr(temp_string.c_str(),"VERTEX"))	{
		float x,y,z,t;
		curstring >> temp >> x >> y >> z >> t; 
		// VERTEX:   0.0000       0.0000       0.0000       0.0000          1      0      0      2
      		if(evt_n >=K && evt_n < K+M) output << "V " << -evt_n << " 0 " << x << " " << y << " " << z << " " << t << " 0 " << N <<  " 0" << endl;

	} else if(strstr(temp_string.c_str(),"TRACK"))	{
                N2++;
		int useless, part_n, pdg_id;
		float px, py, pz;
		//TRACK:      6   2.9797       3.1399       84.461          1      1      0    -13
		curstring >> temp >> useless >> px >> py >> pz >> part_n >> useless >> useless >> pdg_id;
 		//P 5 2212 -2.0 1.2 8.1 5.2 1 0 0 0 0   
                TParticle p1 (pdg_id, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
                MU = p1.GetMass();
                //std::cout << MU << std::endl;
                //
 		if(evt_n >=K && evt_n < K+M) output << "P " << part_n+(evt_n-1)*N << " " << pdg_id << " " << signFlip*px << " " << py << " " << signFlip*pz << " " << sqrt(MU*MU + px*px + py*py + pz*pz) << " 1 0 0 0 0\n";
                if(N==N2 && evt_n == K+M-1){
		    output << "HepMC::IO_Ascii-END_EVENT_LISTING" << endl;
		    output.close();
                    cout << "closing time" << endl;
		    cout << nn << " events written in " << ofName << endl;
		    return;
                }
			
	}

  } // reading loop of the input file
  output << "HepMC::IO_Ascii-END_EVENT_LISTING" << endl;
  output.close();
  cout << nn << " events written in " << ofName << endl;
  return;
}

void convert_SL2HepMC(int number_of_files, string filename, string outfilename) {
  ifstream infile(filename.c_str());
  if (! infile.is_open()) { cout << "\t convert_starlight ERROR: I can not open \"" << filename << "\"" << endl; return; }
  for(int i=0; i<number_of_files; i++) convert_starlightRandFlip(i, infile, outfilename);
  infile.close();
}

/* Explaination of the format :
 * +++ Event +++
 * E 1 -1.0000000000000000e+00 -1.0000000000000000e+00 -1.0000000000000000e+00 20 0 1 0 0
 *   1 : event number  			<-------
 *   -1 : event scale
 *   -1 : alpha_QCD
 *   -1 : alpha_QED
 *   20 : signal process ID
 *   0 : signal process vertex barcode
 *   1 : number of vertices 		<-------
 *   0 : list of vertices
 *   0 : ?
 *
 * +++ Vertex +++
 * V -1 0 0 0 0 0 0 4 0
 *   -1 : vertex barcode (unique)       <-------
 *    0 : vertex id
 *    0 0 0 0 : vertex x,y,z,t
 *    0 : number of orphans
 *    4 : number of out particles       <-------
 *    0 : weights
 *
 * +++ Particle +++
 * P 5 2212 -2.0 1.2 8.1 5.2 1 0 0 0 0   
 *    5 : barcode			<-------
 *    0 : pdg_id			<-------
 *   -2.0 : px				<-------
 *    1.2 : py				<-------
 *    8.1 : pz				<-------
 *    5.2 : e				<-------
 *    1 : status			<-------
 *    0 0  : polarization eta , phi
 *    0 0  : vertex and ?
 */
