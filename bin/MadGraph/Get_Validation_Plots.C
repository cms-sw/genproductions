//Script to extract all the plots from the DQM after step3 harvesting 
// Written by Alexis Kalogeropoulos 22/12/2012

#include <iostream.h>
#include <string.h>
#include <cmath>
#include <sstream>
#include <iomanip>
#include "TChain.h"
#include "TH1.h"
#include "Riostream.h"
#include "TCanvas.h"
#include "TH2.h"
#include "TLegend.h"
#include "TROOT.h"
#include "TStyle.h"
#include <vector>
#include <iostream>
#include <algorithm>
#include "TList.h"
#include <string>
#include "TObject.h"
#include "TBranch.h"
#include <functional>
#include "TObject.h"
#include "TDirectory.h"
#include "TKey.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"


using namespace std;


void Parton_Plots()
{
  TList *FileList;

  
  TFile *Target;


  string file_old = "plots_7TeV_W2jets_1.1.root";
  string file_new = "plots_7TeV_W2jets_1.3.root";
  FileList = new TList ();
  FileList->Add (TFile::Open (file_old.c_str ()));
  FileList->Add (TFile::Open (file_new.c_str ()));
vector <string> categories;
categories.clear();
categories.push_back("DrellYanElectrons");
categories.push_back("DrellYanMuons");
categories.push_back("Particles");
categories.push_back("WMuons");
categories.push_back("WAntiMuons");
categories.push_back("WElectrons");
categories.push_back("WPositrons");
categories.push_back("DuplicationCheck");
categories.push_back("GenParticles");
categories.push_back("MBUEandQCD");
categories.push_back("Tau");
  
string new_title = "mg5_1.3_pdfoff";
for (unsigned int i=0;i<categories.size();i++){
TString dir = categories[i];

  Target = TFile::Open ("W2jets_"+dir+".root", "RECREATE");
  Impose (Target, FileList, new_title,dir);
}
  delete FileList;
  delete Target;
}


void Impose (TDirectory * target, TList * sourcelist, string & new_title_ , TString & Dir)
{
  cout << "	" << "========================================================" << endl;
  cout << "	" << "This is a macro to superimpose plots of different root files." << endl;
  cout << "	" << "Only TH1F objects are superimposed." << endl;
  cout << "	" << "========================================================" << endl;

//  TString path ((char *) strstr (target->GetPath (), ":"));
  Dir = "DQMData/Run 1/Generator/Run summary/"+Dir;
  //path.Remove (0, 2);
    char *theDir = Dir;
  TFile *first_source = (TFile *) sourcelist->First ();
  first_source->cd (theDir);
  TDirectory *current_sourcedir = gDirectory;
  //gain time, do not add the objects in the list in memory
  Bool_t status = TH1::AddDirectoryStatus ();
  TH1::AddDirectory (kFALSE);

  // loop over all keys in this directory
  TChain *globChain = 0;
  TIter nextkey (current_sourcedir->GetListOfKeys ());
  TKey *key, *oldkey = 0;
  while ((key = (TKey *) nextkey ())) {

    //keep only the highest cycle number for each key
    if (oldkey && !strcmp (oldkey->GetName (), key->GetName ()))
      continue;

    // read object from first source file and create a canvas
    first_source->cd (theDir);
    TObject *obj = key->ReadObj ();
    TCanvas *c1 = new TCanvas ("c1", obj->GetName (), 500, 500);
    TLegend *legend_c1 = new TLegend (0.65, 0.80, 0.89, 0.70);

    if (obj->IsA ()->InheritsFrom ("TH1F")) {
      // descendant of TH1F -> prepare the histograms to be superimposed

      //      cout << "Modifying histogram " << obj->GetName() << endl;      
      TH1F *h1 = (TH1F *) obj;
      ModifyHist (h1, kRed);

      // loop over all source files and modify the correspondant
      // histogram to the one pointed to by "h1"
      TFile *nextsource = (TFile *) sourcelist->After (first_source);
      while (nextsource) {

	// make sure we are at the correct directory level by cd'ing to path
	nextsource->cd (theDir);
	TKey *key2 = (TKey *) gDirectory->GetListOfKeys ()->FindObject (h1->GetName ());
	if (key2) {
	  TH1F *h2 = (TH1F *) key2->ReadObj ();
	  ModifyHist (h2, kBlue);



	  double maxh1;
	  double maxh2;
	  TH1F* htest;
	  if (h1->GetNbinsX()>h2->GetNbinsX()) htest=h1;
	  else htest=h2;
	
	  maxh1 = h1->GetMaximum (10000.);
	  maxh2 = h2->GetMaximum (10000.);
	  if (maxh1 > maxh2) {
	    h1->Draw ();
	    h2->Draw ("SAME");

	  }
	  else {
	    h2->Draw ();
	    h1->Draw ("SAME");

	  }



	  legend_c1->SetTextFont (70);
	  legend_c1->SetTextSize (0.03);
	  legend_c1->AddEntry (h1, new_title_.c_str (), "L");	// change this aBranch_Bkg_ording to first source file
	  legend_c1->AddEntry (h2, "mg5_1.1", "L");	// change this aBranch_Bkg_ording to second source file
	  legend_c1->Draw ("SAME");
	}

	nextsource = (TFile *) sourcelist->After (nextsource);
      }				// while ( nextsource )
    }
    else if (obj->IsA ()->InheritsFrom ("TTree")) {	// not tested

      // loop over all source files create a chain of Trees "globChain"
      const char *obj_name = obj->GetName ();

      globChain = new TChain (obj_name);
      globChain->Add (first_source->GetName ());
      TFile *nextsource = (TFile *) sourcelist->After (first_source);
      //      const char* file_name = nextsource->GetName();
      // cout << "file name  " << file_name << endl;
      while (nextsource) {

	globChain->Add (nextsource->GetName ());
	nextsource = (TFile *) sourcelist->After (nextsource);
      }

    }
    else if (obj->IsA ()->InheritsFrom ("TDirectory")) {	// not tested
      // it's a subdirectory

      cout << "Found subdirectory " << obj->GetName () << endl;

      // create a new subdir of same name and title in the target file
      target->cd ();
      ////     TDirectory *newdir = target->mkdir (obj->GetName (), obj->GetTitle ());


    }
    else {

      // object is of no type that we know or can handle
      cout << "Unknown object type, name: " << obj->GetName () << ", object type: " << obj->ClassName () << endl;
    }

    if (obj) {
      target->cd ();

      //!!if the object is a tree, it is stored in globChain...     
      if (obj->IsA ()->InheritsFrom ("TTree"))	// not tested
	globChain->Merge (target->GetFile (), 0, "keep");
      else
	c1->Write (key->GetName ());
    }


    delete obj;
    delete c1;
    delete legend_c1;
  }				// while ( ( TKey *key = (TKey*)nextkey() ) )

  // save modifications to target file
  target->SaveSelf (kTRUE);
  TH1F::AddDirectory (status);
  cout << "	" << "========================================================" << endl;
  cout<< " Ended SuperImpose of files.... " <<endl;



}



void
ModifyHist (TH1F * &h, Color_t lcolor)
{
  double temp_integral;

  h->SetLineColor (lcolor);
  temp_integral = h->Integral ();
  if (temp_integral != 0)
    h->Scale (pow (temp_integral, -1));
}


