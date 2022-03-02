import os
import sys
import argparse
import ROOT
import math

from ROOT import TFile, TH1F, TCanvas, TPad, TLine, TLegend, kRed, kBlack, kFALSE, kTRUE
from optparse import OptionParser

keys = ['lep1pt_LHE', 'lep1eta_LHE', 'lep1phi_LHE', 'lep2pt_LHE', 'lep2eta_LHE', 'lep2phi_LHE', 'Vpt_LHE', 'HT_LHE',
        'ngenjet', 'j1pt_GEN', 'j1eta_GEN', 'j1phi_GEN', 'j1mass_GEN', 'j2pt_GEN', 'j2eta_GEN', 'j2phi_GEN', 'j2mass_GEN', 'j3pt_GEN', 'j3eta_GEN', 'j3phi_GEN', 'j3mass_GEN', 'HT_GEN',
        'Dressedlep1pt_GEN', 'Dressedlep1eta_GEN', 'Dressedlep1phi_GEN', 'Dressedlep2pt_GEN', 'Dressedlep2eta_GEN', 'Dressedlep2phi_GEN',
        'Zmass_GEN', 'Zpt_GEN', 'Zeta_GEN', 'Zphi_GEN', 'Wpt_GEN', 'Weta_GEN', 'Wphi_GEN',
        'drlep1j1_GEN', 'drlep1j2_GEN', 'drlep1j3_GEN', 'drlep2j1_GEN', 'drlep2j2_GEN', 'drlep2j3_GEN',
        'drl1l2_GEN','drj1j2_GEN','drj1j3_GEN','drj2j3_GEN','Vistaupt_GEN','Vistaueta_GEN','Vistauphi_GEN','METpt_GEN','METphi_GEN']

def plot1(key, xsection1, xsection2, histo1, histo2, name1, name2, LogY, weight1, weight2, outputDIR):
  c1=TCanvas(key, key, 800, 600)
  pad1 = TPad("pad1", "", 0.00, 0.20, 0.99, 0.99)
  pad2 = TPad("pad2", "", 0.00, 0.00, 0.99, 0.20)
  pad1.SetGridx()
  pad1.SetGridy()
  pad2.SetGridy()
  pad1.SetFillColor(0)
  pad1.SetLineColor(0)
  pad2.SetFillColor(0)
  pad2.SetLineColor(0)
  pad1.SetBottomMargin(0.03)
  pad2.SetTopMargin(0.15)
  pad2.SetBottomMargin(0.3)
  pad1.Draw()
  pad2.Draw()

  leg1 = TLegend(0.65,0.75,0.86,0.86)

  histo1.Scale(xsection1/weight1)
  histo1.SetMaximum(1.2*histo1.GetMaximum())
  histo1.SetMinimum(0.0001)
  histo1.SetLineColor(kRed)
  histo1.SetLineWidth(2)
  histo1.GetXaxis().SetLabelSize(0)
  histo2.Scale(xsection2/weight2)
  histo2.SetLineColor(kBlack)
  histo2.SetLineWidth(2)
  
  pad1.cd()
  if LogY:
    pad1.SetLogy()

  histo_ratio=histo2.Clone()
  histo_ratio.SetTitle('')
  histo_ratio.Divide(histo1)
  histo_ratio.SetMaximum(1.5)
  histo_ratio.SetMinimum(0.5)
  histo_ratio.GetYaxis().SetNdivisions(4,kFALSE)
  histo_ratio.GetXaxis().SetTitle(key)
  histo_ratio.GetXaxis().SetTitleSize(0.15)
  histo_ratio.GetXaxis().SetTitleOffset(0.75)
  histo_ratio.GetXaxis().SetLabelSize(0.13)
  histo_ratio.GetYaxis().SetTitle(name1+'/'+name2)
  histo_ratio.GetYaxis().SetTitleSize(0.1)
  histo_ratio.GetYaxis().SetTitleOffset(0.3)
  histo_ratio.GetYaxis().SetLabelSize(0.13)
  histo_ratio.SetLineWidth(2)

  histo1.Draw('pe')
  histo2.Draw('same pe')
  leg1.AddEntry(histo1, name1)
  leg1.AddEntry(histo2, name2)
  leg1.SetFillStyle(0)
  leg1.SetBorderSize(0)
  leg1.Draw()
  c1.Update()
 
  pad2.cd()
  histo_ratio.Draw()
  if LogY:
    c1.SaveAs(outputDIR + '/' + key + '_log.png')
    c1.SaveAs(outputDIR + '/' + key + '_log.pdf')
  else:
    c1.SaveAs(outputDIR + '/' + key + '.png')
    c1.SaveAs(outputDIR + '/' + key + '.pdf')

  # renew 
  histo1.Scale(weight1/xsection1)
  histo2.Scale(weight2/xsection2)
  ####################################
def plot2(key, xsections, histos, names, LogY, weights, outputDIR):
  c1=TCanvas(key, key, 800, 600)
  pad1 = TPad("pad1", "", 0.00, 0.20, 0.99, 0.99)
  pad2 = TPad("pad2", "", 0.00, 0.00, 0.99, 0.20)
  pad1.SetGridx()
  pad1.SetGridy()
  pad2.SetGridy()
  pad1.SetFillColor(0)
  pad1.SetLineColor(0)
  pad2.SetFillColor(0)
  pad2.SetLineColor(0)
  pad1.SetBottomMargin(0.03)
  pad2.SetTopMargin(0.15)
  pad2.SetBottomMargin(0.3)
  pad1.Draw()
  pad2.Draw()

  leg1 = TLegend(0.68,0.55,0.86,0.86)
  histos[0].GetXaxis().SetLabelSize(0)
  histos[0].SetMarkerStyle(20)

  for i in range(0,len(histos)):
    #if histos[i].Integral()==0: continue
    histos[i].Scale(xsections[i]/weights[i])
    histos[i].SetLineColor(i+1)
    histos[i].SetLineWidth(2)

  histo_ratio=histos[1].Clone()
  for i in range(2,len(histos)):
    histo_ratio.Add(histos[i])
  histo_ratio.SetLineWidth(2)

  histo_sum=histo_ratio.Clone()
  histo_sum.SetLineColor(49)
  histo_sum.SetLineWidth(2)

  histo_ratio.SetTitle('')
  histo_ratio.Divide(histos[0])
  histo_ratio.SetLineColor(49)
  histo_ratio.SetMaximum(1.5)
  histo_ratio.SetMinimum(0.5)
  histo_ratio.GetYaxis().SetNdivisions(4,kFALSE)
  histo_ratio.GetXaxis().SetTitle(key)
  histo_ratio.GetXaxis().SetTitleSize(0.15)
  histo_ratio.GetXaxis().SetTitleOffset(0.75)
  histo_ratio.GetXaxis().SetLabelSize(0.13)
  histo_ratio.GetYaxis().SetTitle('summed/inc')
  histo_ratio.GetYaxis().SetTitleSize(0.1)
  histo_ratio.GetYaxis().SetTitleOffset(0.3)
  histo_ratio.GetYaxis().SetLabelSize(0.13)

  pad1.cd()
  if LogY:
    pad1.SetLogy()
  histos[0].SetMaximum(1.2*histos[0].GetMaximum())
  histos[0].SetMinimum(0.0001)
  histos[0].Draw('pe')
  for i in range(1,len(histos)):
    histos[i].Draw('same h')
  histo_sum.Draw('same h')  
  
  leg1.AddEntry(histos[0], names[0])
  leg1.AddEntry(histo_sum, 'MGsummed')
  for i in range(1,len(histos)):
    leg1.AddEntry(histos[i], names[i])
  leg1.SetFillStyle(0)
  leg1.SetBorderSize(0)
  leg1.Draw()
  c1.Update()

  pad2.cd()
  histo_ratio.Draw()
  if LogY:
    c1.SaveAs(outputDIR + '/' + key + '_log.png')
    c1.SaveAs(outputDIR + '/' + key + '_log.pdf')
  else:
    c1.SaveAs(outputDIR + '/' + key + '.png')
    c1.SaveAs(outputDIR + '/' + key + '.pdf')
  for i in range(0,len(histos)):
    histos[i].Scale(weights[i]/xsections[i])
###################################################################
def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('--xs', dest='xs', nargs='+', help='cross sections of samples', default=None, type=float)
  parser.add_argument('--histo', dest='histo', nargs='+', help='histograms of samples', default=None)
  parser.add_argument('--name', dest='name', nargs='+', help='names of samples', default=None)
  parser.add_argument('--inputdir', dest='inputdir', help='directory of input files', default=None)
  parser.add_argument('--outputdir', dest='outputdir', help='directory of output files', default=None)
  args=parser.parse_args()

  Xs=args.xs
  histograms=args.histo
  inputDIR = args.inputdir
  outputDIR = args.outputdir
  names = args.name
  

  histo1=TH1F()
  histo2=TH1F()
  if len(Xs)==2:
    print 'two version MG comparison mode--->>>'
    histos1=TFile.Open(inputDIR+histograms[0])
    histos2=TFile.Open(inputDIR+histograms[1])
    name1=args.name[0]
    name2=args.name[1]
    hist1_weight=TH1F()
    hist2_weight=TH1F()
    histos1.GetObject('weight',hist1_weight)
    histos2.GetObject('weight',hist2_weight)
    final_weight1=hist1_weight.GetBinContent(2)-hist1_weight.GetBinContent(1)
    final_weight2=hist2_weight.GetBinContent(2)-hist2_weight.GetBinContent(1)
    for key in keys:
      if key=='weight':continue
      histos1.GetObject(key,histo1)
      histos2.GetObject(key,histo2)
      if histo1.Integral()<=0 or histo2.Integral()<=0:continue
      plot1(key,Xs[0], Xs[1], histo1, histo2, name1, name2, kFALSE,final_weight1,final_weight2, outputDIR)
      plot1(key,Xs[0], Xs[1], histo1, histo2, name1, name2, kTRUE, final_weight1, final_weight2, outputDIR)
    
  else:
    print 'inclusive vs stitched mode--->>>'
    tfiles=[]
    Xss=[]
    Names=[]
    histo_temp=TH1F()
    histo_weight=[]
    final_weights=[]    
    for i in range(0,len(histograms)):
      tfiles.append(TFile.Open(inputDIR+histograms[i]))
      Xss.append(Xs[i])
      Names.append(names[i])

    for i in range(0,len(histograms)):
      tfiles[i].GetObject('weight',histo_temp)
      histo_weight.append(histo_temp.Clone("weight_"+str(i)))
    for i in range(0,len(histograms)):
      final_weights.append(histo_weight[i].GetBinContent(2)-histo_weight[i].GetBinContent(1))
    for key in keys:
      if key=='weight':continue
      histos=[]
      pass_ornot = 0
      for i in range(0,len(histograms)):
        tfiles[i].GetObject(key,histo_temp)
        histos.append(histo_temp.Clone(key+str(i)))
      for i in range(0,len(histograms)):
        if histos[i].Integral()<=0:pass_ornot=1  
      if pass_ornot: continue
      plot2(key, Xss, histos, Names, kFALSE, final_weights, outputDIR)
      plot2(key, Xss, histos, Names, kTRUE, final_weights, outputDIR)


if __name__ == "__main__":
  sys.exit(main())  
