//Script to read all TCanvas and save them in png format
//Written by Alex Kalogeropoulos  22/12/2012


void ExtractPlots() {

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

TFile * Target;
string new_title = "mg5_1.3_pdfoff";
for (unsigned int i=0;i<categories.size();i++){
 TString dir = categories[i];

  Target = TFile::Open ("W2jets_"+dir+".root", "READ");
  WritePlots (Target, dir);
}


}


void WritePlots(TFile * f1, TString & Dir)
 { 
   TIter next(f1->GetListOfKeys());
   TKey *key;
   while ((key = (TKey*)next())) {
      TClass *cl = gROOT->GetClass(key->GetClassName());
      if (!cl->InheritsFrom("TCanvas")) continue;
      TCanvas *h = (TCanvas*)key->ReadObj();
 	h->cd();      
	h->Draw();
	TString title=h->GetTitle();
       h.SaveAs(title+Dir+".png");
   }
}


