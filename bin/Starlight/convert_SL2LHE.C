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
void convert_SL2LHE(int number_of_files=1, string filename = "slight.out", string outfilename = "starlight_LHEtest", double beamE1=2560, double beamE2=2560);

void convert_starlight(int num, ifstream &infile, string outfilename, double beamE1, double beamE2) //makeEventsFile
{
  char ofName[100];
  sprintf(ofName,"%s.lhe",outfilename.c_str());
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
  //int signFlip=1;
  TRandom3 random;

  // GENER: STL  1.0            1           1           1           1  200.00     999.999     CMS
  //getline(infile,temp_string); // The very first line is useless --> and does not seem to exist now !
  
  //do the header
  output << "<LesHouchesEvents version=\"1.0\">" << endl;
  output << "<!-- " << endl << " #Converted from STARLIGHT generator output " << endl << "-->" << endl;
  output << "<header>" << endl << "</header>" << endl;
  
  //put generic initialization-level info since STARLIGHT doesn't save this
  output << "<init>" << endl;
  //beam particle 1, 2, beam energy 1, 2, author group, beam 1, 2, PDFSET beam 1, 2,
  output << "22 " << "22 " << beamE1 << " " << beamE2 << " 0 " << "0 " << "0 " << "0 " << "3 " << "1" << endl;
  output << "1.0 " << "0.0 " << "3.0 " << "81" << endl;
  output << "</init>" << endl;

  while (getline(infile,temp_string)) {
	
	curstring.clear(); // needed when using several tims istringstream::str(string)
	curstring.str(temp_string);
	
	//for each event, write Nparticles, process ID, event weight, event scale, and alpha_EM alpha_s
	if(strstr(temp_string.c_str(),"EVENT"))	{
		if(nn) output << "</event>" << endl;
		output << "<event>" << endl;
		curstring >> temp >> evt_n >> N; //assuming that EVENT always preceeds VERTEX/TRACK so that N is set correctly
		// EVENT:          1       2       1
      		if(evt_n >=K && evt_n < K+M)  {
			output << N << " 81" << " 1.0 -1.0 -1.0 -1.0" << endl;
			nn++;
		}
		N2=0;

	}
	else if(strstr(temp_string.c_str(),"TRACK"))	{
                N2++;
		int useless, part_n, pdg_id;
		float px, py, pz;
		//TRACK:      6   2.9797       3.1399       84.461          1      1      0    -13
		curstring >> temp >> useless >> px >> py >> pz >> part_n >> useless >> useless >> pdg_id;
 		//P 5 2212 -2.0 1.2 8.1 5.2 1 0 0 0 0   
                TParticle p1 (pdg_id, 0, 0, 0, 0, 0, px, py, pz, 0.0, 0.0, 0.0, 0.0, 0.0);
                MU = p1.GetMass();
		float E = TMath::Sqrt(pow(MU,2)+pow(p1.P(),2));
                //std::cout << MU << std::endl;
                //
 		if(evt_n >=K && evt_n < K+M){
			output << pdg_id << " -1" << " 0 0 0 0 " << px << " " << py << " " << pz << " " << E << " " << MU << " 0.0 9.0" << endl; 
		}
                if(N==N2 && evt_n == K+M-1){
		    output << "</event>" << endl;
		    output << "</LesHouchesEvents>" << endl; 
		    output.close();
		    cout << nn << " events written in " << ofName << endl;
		    return;
                }
			
	}

  } // reading loop of the input file
  output << "</event>" << endl;
  output << "</LesHouchesEvents>" << endl;
  output.close();
  cout << nn << " events written in " << ofName << endl;
  return;
}

void convert_SL2LHE(int number_of_files, string filename, string outfilename, double beamE1, double beamE2) {
  ifstream infile(filename.c_str());
  if (! infile.is_open()) { cout << "\t convert_starlight ERROR: I can not open \"" << filename << "\"" << endl; return; }
  for(int i=0; i<number_of_files; i++) convert_starlight(i, infile, outfilename, beamE1, beamE2);
  infile.close();
}

/* Explanation of the format :
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
