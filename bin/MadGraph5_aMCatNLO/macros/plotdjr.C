#include "TFile.h"
#include "TTree.h"
#include "TH1D.h"
#include "TCanvas.h"
#include "TCut.h"
#include "TROOT.h"
#include "TChain.h"
#include "TLegend.h"
#include <vector>


void setcanvas(TCanvas *c1, TPad **pad){

  c1->SetLeftMargin(0.0);
  c1->SetTopMargin(0.00);
  c1->SetRightMargin(0.00);
  c1->SetBottomMargin(0.0);

  pad[0]  =new TPad("pad0","pad",0,0.5,0.5,1.0);
  pad[1]  =new TPad("pad1","pad",0.5,0.5,1.0,1.0);
  pad[2]  =new TPad("pad2","pad",0,0,0.5,0.5);
  pad[3]  =new TPad("pad3","pad",0.5,0.0,1.0,0.5);
  for(int k=0;k<4;k++){
    pad[k]->Draw();
  }
}

void setlegend(TLegend *legend, TH1D *hall, TH1D *hmult0, TH1D *hmult1, TH1D *hmult2, TH1D *hmult3){

  legend->SetTextSize(0.050);
  legend->SetBorderSize(0);
  legend->SetTextFont(62);
  legend->SetLineColor(0);
  legend->SetLineStyle(1);
  legend->SetLineWidth(1);
  legend->SetFillColor(0);
  legend->SetFillStyle(1001);

  legend->AddEntry(hall,"all partons");
  legend->AddEntry(hmult0,"0 partons");
  legend->AddEntry(hmult1,"1 parton");
  legend->AddEntry(hmult2,"2 partons");
  legend->AddEntry(hmult3,"3 partons");

}

void makeplot(const char *name, TTree *tree, TCut weight, const char *drawstring, const char *xlabel, int nbins, double xlow, double xhigh) {
  
  //this is for NLO with FXFX merging
//   TCut mult0 = "LHEEvent.npNLO()==0";
//   TCut mult1 = "LHEEvent.npNLO()==1";
//   TCut mult2 = "LHEEvent.npNLO()==2";
//   TCut mult3 = "LHEEvent.npNLO()==3";  

  //this is for LO with MLM
//  TCut mult0 = "GenEvent.nMEPartons()==0";
//  TCut mult1 = "GenEvent.nMEPartons()==1";
//  TCut mult2 = "GenEvent.nMEPartons()==2";
//  TCut mult3 = "GenEvent.nMEPartons()==3";

  //this is for LO with MLM (plotting partons after excluding non-matched partons in wbb/vbf type processes)
  TCut mult0 = "GenEvent.nMEPartonsFiltered()==0";
  TCut mult1 = "GenEvent.nMEPartonsFiltered()==1";
  TCut mult2 = "GenEvent.nMEPartonsFiltered()==2";
  TCut mult3 = "GenEvent.nMEPartonsFiltered()==3";
  
  TH1D *hall = new TH1D(TString::Format("hall_%s",name),"",nbins,xlow,xhigh);
  TH1D *hmult0 = new TH1D(TString::Format("hmult0_%s",name),"",nbins,xlow,xhigh);
  TH1D *hmult1 = new TH1D(TString::Format("hmult1_%s",name),"",nbins,xlow,xhigh);
  TH1D *hmult2 = new TH1D(TString::Format("hmult2_%s",name),"",nbins,xlow,xhigh);
  TH1D *hmult3 = new TH1D(TString::Format("hmult3_%s",name),"",nbins,xlow,xhigh);

  hall->SetLineColor(921);  
  hmult0->SetLineColor(600);
  hmult1->SetLineColor(629);
  hmult2->SetLineColor(419);
  hmult3->SetLineColor(810);

  hall->SetLineWidth(2);
  hmult0->SetLineStyle(2);
  hmult1->SetLineStyle(2);  
  hmult2->SetLineStyle(2);
  hmult3->SetLineStyle(2);
  
  tree->Draw(TString::Format("%s>>%s",drawstring,hall->GetName()),weight,"goff");
  tree->Draw(TString::Format("%s>>%s",drawstring,hmult0->GetName()),weight*mult0,"goff");
  tree->Draw(TString::Format("%s>>%s",drawstring,hmult1->GetName()),weight*mult1,"goff");
  tree->Draw(TString::Format("%s>>%s",drawstring,hmult2->GetName()),weight*mult2,"goff");
  tree->Draw(TString::Format("%s>>%s",drawstring,hmult3->GetName()),weight*mult3,"goff");  

  hall->GetXaxis()->SetTitle(xlabel);

  TLegend *legend=new TLegend(0.67,0.87-4*0.06,0.87,0.87);
  setlegend(legend, hall, hmult0, hmult1, hmult2, hmult3);
  
  hall->Draw("EHIST");
  hmult0->Draw("EHISTSAME");
  hmult1->Draw("EHISTSAME");
  hmult2->Draw("EHISTSAME");
  hmult3->Draw("EHISTSAME");

  gStyle->SetOptStat(0);
  legend->Draw();
}

void plotdjr(const TString & infile, const TString & outfile) {
 
  TH1::SetDefaultSumw2();
  
  TChain *tree = new TChain("Events");
  tree->Add(infile);
  
  tree->SetAlias("GenEvent","GenEventInfoProduct_generator__GEN.obj");
  
  TCut weight = "GenEvent.weight()";
  int nbins = 50.;
  double djrmin = -0.5;
  double djrmax = 3.;
  
  
  TCanvas *c1 = new TCanvas("c1", "c1", 800, 600);
  TPad *pad[4];
  setcanvas(c1,pad);

  pad[0]->cd();
  makeplot("djr0",tree,weight,"log10(GenEvent.DJRValues_[0])","DJR 0->1",nbins,djrmin,djrmax);
  pad[1]->cd();
  makeplot("djr1",tree,weight,"log10(GenEvent.DJRValues_[1])","DJR 1->2",nbins,djrmin,djrmax);
  pad[2]->cd();
  makeplot("djr2",tree,weight,"log10(GenEvent.DJRValues_[2])","DJR 2->3",nbins,djrmin,djrmax);
  pad[3]->cd();
  makeplot("djr3",tree,weight,"log10(GenEvent.DJRValues_[3])","DJR 3->4",nbins,djrmin,djrmax);

  c1->Print(outfile);
  return;  
}

