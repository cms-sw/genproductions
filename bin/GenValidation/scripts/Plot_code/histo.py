import os
import sys
import optparse
import ROOT
import math

from ROOT import TH1F, TFile, TTree, TLorentzVector, TVectorD
from optparse import OptionParser
from math import cos, sin, sinh, hypot, pi
MW_=80.379

class extend_p4:
  p4_vector=ROOT.TLorentzVector()
  pdgid=11
  def __init__(self, ltvector, pid):
    self.p4_vector=ltvector
    self.pdgid=pid

def deltaPhi(phi1, phi2):
  dphi = (phi1 - phi2)
  while dphi > pi:
    dphi -= 2 * pi
  while dphi < -pi:
    dphi += 2 * pi
  return dphi

def deltaR(eta1, phi1, eta2, phi2):
    return hypot(eta1 - eta2, deltaPhi(phi1, phi2))

def wreco(lepp4, metpt, metphi):
  tmpsol1=1.0
  tmpsol2=1.0
  nu4=TLorentzVector()
  px=metpt*cos(metphi)
  py=metpt*sin(metphi)
  pz=0.
  pxl=lepp4.Px()
  pyl=lepp4.Py()
  pzl=lepp4.Pz()
  El=lepp4.E()
  a=MW_*MW_ + 2.*pxl*px + 2.*pyl*py
  A=4.*(El*El - pzl*pzl)
  B=-4.*a*pzl
  C=4.*El*El*(px*px+py*py)-a*a
  tmproot=B*B - 4.*A*C

  if tmproot<0:
    pz=-1.*B/(2*A)
  else:
    tmpsol1 = (-B + math.sqrt(tmproot))/(2.0*A)
    tmpsol2 = (-B - math.sqrt(tmproot))/(2.0*A)
    if abs(tmpsol2-pzl)<abs(tmpsol1-pzl):
      pz=tmpsol2
    else:
      pz=tmpsol1
  if abs(pz)>300:
    if abs(tmpsol1)<abs(tmpsol2):
      pz=tmpsol1
    else:
      pz=tmpsol2
  nu4.SetPxPyPzE(px,py,pz,math.sqrt(px*px+py*py+pz*pz))
  wp4=lepp4+nu4
  return wp4

def main():
  usage = 'usage: %prog [options]'
  parser = optparse.OptionParser(usage)
  parser.add_option('-i', '--in', dest='inputfiles', help='name of input files', default=None, type='string')
  parser.add_option('-o', '--out', dest='outputfiles', help='name output files', default=None, type='string')
  (opt, args) = parser.parse_args()

  histo_array = {}
  
  h_weight = TH1F('weight','weight',2,-1,1)
  #LHE histos
  h_lep1pt_LHE = TH1F('lep1pt_LHE', 'lep1pt_LHE', 75, 0, 150)
  h_lep1eta_LHE = TH1F('lep1eta_LHE', 'lep1eta_LHE', 60, -3, 3)
  h_lep1phi_LHE = TH1F('lep1phi_LHE', 'lep1phi_LHE', 80, -4, 4)
  h_lep2pt_LHE = TH1F('lep2pt_LHE', 'lep1pt_LHE', 75, 0, 150)
  h_lep2eta_LHE = TH1F('lep2eta_LHE', 'lep2eta_LHE', 60, -3, 3)
  h_lep2phi_LHE = TH1F('lep2phi_LHE', 'lep2phi_LHE', 80, -4, 4)
  h_Vpt_LHE = TH1F('Vpt_LHE', 'Vpt_LHE', 75, 0, 150)
  h_HT_LHE = TH1F('HT_LHE', 'HT_LHE', 60, 0, 300)
  
  #GEN histos
  h_ngenjet_GEN = TH1F('ngenjet', 'ngenjet', 10, 0, 10)
  h_j1pt_GEN = TH1F('j1pt_GEN', 'j1pt_GEN', 100, 0, 200)
  h_j1eta_GEN = TH1F('j1eta_GEN', 'j1eta_GEN', 100, -5, 5)
  h_j1phi_GEN = TH1F('j1phi_GEN', 'j1phi_GEN', 80, -4, 4)
  h_j1mass_GEN = TH1F('j1mass_GEN', 'j1mass_GEN', 40, 0, 40)
  h_j2pt_GEN = TH1F('j2pt_GEN', 'j2pt_GEN', 100, 0, 200)
  h_j2eta_GEN = TH1F('j2eta_GEN', 'j2eta_GEN', 100, -5, 5)
  h_j2phi_GEN = TH1F('j2phi_GEN', 'j2phi_GEN', 80, -4, 4)
  h_j2mass_GEN = TH1F('j2mass_GEN', 'j2mass_GEN', 40, 0, 40)
  h_j3pt_GEN = TH1F('j3pt_GEN', 'j3pt_GEN', 100, 0, 200)
  h_j3eta_GEN = TH1F('j3eta_GEN', 'j3eta_GEN', 100, -5, 5)
  h_j3phi_GEN = TH1F('j3phi_GEN', 'j3phi_GEN', 80, -4, 4)
  h_j3mass_GEN = TH1F('j3mass_GEN', 'j3mass_GEN', 40, 0, 40)
  h_HT_GEN = TH1F('HT_GEN', 'HT_GEN', 80, 0, 400)
  
  h_Dressedlep1pt_GEN = TH1F('Dressedlep1pt_GEN', 'Dressedlep1pt_GEN', 75, 0, 150)
  h_Dressedlep1eta_GEN = TH1F('Dressedlep1eta_GEN', 'Dressedlep1eta_GEN', 60, -3, 3)
  h_Dressedlep1phi_GEN = TH1F('Dressedlep1phi_GEN', 'Dressedlep1phi_GEN', 80, -4, 4)
  h_Dressedlep2pt_GEN = TH1F('Dressedlep2pt_GEN', 'Dressedlep2pt_GEN', 75, 0, 150)
  h_Dressedlep2eta_GEN = TH1F('Dressedlep2eta_GEN', 'Dressedlep2eta_GEN', 60, -3, 3)
  h_Dressedlep2phi_GEN = TH1F('Dressedlep2phi_GEN', 'Dressedlep2phi_GEN', 80, -4, 4)
  
  h_Zmass_GEN = TH1F('Zmass_GEN', 'Zmass_GEN', 80, 50, 130)
  h_Zpt_GEN = TH1F('Zpt_GEN', 'Zpt_GEN', 100, 0, 200)
  h_Zeta_GEN = TH1F('Zeta_GEN', 'Zeta_GEN', 100, -5, 5)
  h_Zphi_GEN = TH1F('Zphi_GEN', 'Zphi_GEN', 80, -4, 4)
  h_Wpt_GEN = TH1F('Wpt_GEN', 'Wpt_GEN', 400, 0, 800)
  h_Weta_GEN = TH1F('Weta_GEN', 'Weta_GEN', 100, -5, 5)
  h_Wphi_GEN = TH1F('Wphi_GEN', 'Wphi_GEN', 80, -4, 4)
  
  h_drlep1j1_GEN = TH1F('drlep1j1_GEN', 'drlep1j1_GEN', 60, 0, 3)
  h_drlep1j2_GEN = TH1F('drlep1j2_GEN', 'drlep1j2_GEN', 60, 0, 3)
  h_drlep1j3_GEN = TH1F('drlep1j3_GEN', 'drlep1j3_GEN', 60, 0, 3)
  h_drlep2j1_GEN = TH1F('drlep2j1_GEN', 'drlep2j1_GEN', 60, 0, 3)
  h_drlep2j2_GEN = TH1F('drlep2j2_GEN', 'drlep2j2_GEN', 60, 0, 3)
  h_drlep2j3_GEN = TH1F('drlep2j3_GEN', 'drlep2j3_GEN', 60, 0, 3)
  
  h_drl1l2_GEN = TH1F('drl1l2_GEN', 'drl1l2_GEN', 60, 0, 3)
  h_drj1j2_GEN = TH1F('drj1j2_GEN', 'drj1j2_GEN', 60, 0, 3)
  h_drj1j3_GEN = TH1F('drj1j3_GEN', 'drj1j3_GEN', 60, 0, 3)
  h_drj2j3_GEN = TH1F('drj2j3_GEN', 'drj2j3_GEN', 60, 0, 3)
  
  h_Vistaupt_GEN = TH1F('Vistaupt_GEN', 'Vistaupt_GEN', 100, 0, 200)
  h_Vistaueta_GEN = TH1F('Vistaueta_GEN', 'Vistaueta_GEN', 60, -3, 3)
  h_Vistauphi_GEN = TH1F('Vistauphi_GEN', 'Vistauphi_GEN', 80, -4, 4)
  
  h_METpt_GEN = TH1F('METpt_GEN', 'METpt_GEN', 50, 0, 100)
  h_METphi_GEN = TH1F('METphi_GEN', 'METphi_GEN', 80, -4, 4)
  
  #add histos
  histo_array['h_weight']=h_weight
  histo_array['h_lep1pt_LHE']=h_lep1pt_LHE
  histo_array['h_lep1eta_LHE']=h_lep1eta_LHE
  histo_array['h_lep1phi_LHE']=h_lep1phi_LHE
  histo_array['h_lep2pt_LHE']=h_lep2pt_LHE
  histo_array['h_lep2eta_LHE']=h_lep2eta_LHE
  histo_array['h_lep2phi_LHE']=h_lep2phi_LHE
  histo_array['h_Vpt_LHE']=h_Vpt_LHE
  histo_array['h_HT_LHE']=h_HT_LHE
  histo_array['h_ngenjet_GEN']=h_ngenjet_GEN
  histo_array['h_j1pt_GEN']=h_j1pt_GEN
  histo_array['h_j1eta_GEN']=h_j1eta_GEN
  histo_array['h_j1phi_GEN']=h_j1phi_GEN
  histo_array['h_j1mass_GEN']=h_j1mass_GEN
  histo_array['h_j2pt_GEN']=h_j2pt_GEN
  histo_array['h_j2eta_GEN']=h_j2eta_GEN
  histo_array['h_j2phi_GEN']=h_j2phi_GEN
  histo_array['h_j2mass_GEN']=h_j2mass_GEN
  histo_array['h_j3pt_GEN']=h_j3pt_GEN
  histo_array['h_j3eta_GEN']=h_j3eta_GEN
  histo_array['h_j3phi_GEN']=h_j3phi_GEN
  histo_array['h_j3mass_GEN']=h_j3mass_GEN
  histo_array['h_HT_GEN']=h_HT_GEN
  histo_array['h_Dressedlep1pt_GEN']=h_Dressedlep1pt_GEN
  histo_array['h_Dressedlep1eta_GEN']=h_Dressedlep1eta_GEN
  histo_array['h_Dressedlep1phi_GEN']=h_Dressedlep1phi_GEN
  histo_array['h_Dressedlep2pt_GEN']=h_Dressedlep2pt_GEN
  histo_array['h_Dressedlep2eta_GEN']=h_Dressedlep2eta_GEN
  histo_array['h_Dressedlep2phi_GEN']=h_Dressedlep2phi_GEN
  histo_array['h_Zmass_GEN']=h_Zmass_GEN
  histo_array['h_Zpt_GEN']=h_Zpt_GEN
  histo_array['h_Zeta_GEN']=h_Zeta_GEN
  histo_array['h_Zphi_GEN']=h_Zphi_GEN
  histo_array['h_Wpt_GEN']=h_Wpt_GEN
  histo_array['h_Weta_GEN']=h_Weta_GEN
  histo_array['h_Wphi_GEN']=h_Wphi_GEN
  histo_array['h_drlep1j1_GEN']=h_drlep1j1_GEN
  histo_array['h_drlep1j2_GEN']=h_drlep1j2_GEN
  histo_array['h_drlep1j3_GEN']=h_drlep1j3_GEN
  histo_array['h_drlep2j1_GEN']=h_drlep2j1_GEN
  histo_array['h_drlep2j2_GEN']=h_drlep2j2_GEN
  histo_array['h_drlep2j3_GEN']=h_drlep2j3_GEN
  histo_array['h_drl1l2_GEN']=h_drl1l2_GEN
  histo_array['h_drj1j2_GEN']=h_drj1j2_GEN
  histo_array['h_drj1j3_GEN']=h_drj1j3_GEN
  histo_array['h_drj2j3_GEN']=h_drj2j3_GEN
  histo_array['h_Vistaupt_GEN']=h_Vistaupt_GEN
  histo_array['h_Vistaueta_GEN']=h_Vistaueta_GEN
  histo_array['h_Vistauphi_GEN']=h_Vistauphi_GEN
  histo_array['h_METpt_GEN']=h_METpt_GEN
  histo_array['h_METphi_GEN']=h_METphi_GEN
  
  for key in histo_array:
    histo_array[key].SetStats(0)
    histo_array[key].Sumw2()
    histo_array[key].GetYaxis().SetTitle("a.u.")
    histo_array[key].GetYaxis().SetTitleSize(0.05);
    histo_array[key].GetYaxis().SetTitleOffset(0.75);
    histo_array[key].GetXaxis().SetTitle(key)
    histo_array[key].SetMinimum(0)

  if not os.path.isfile(opt.inputfiles): 
    print 'inputfile does not exist!!'
  filein=TFile.Open(opt.inputfiles)
  treein=filein.Get('Events')
  npos=treein.GetEntries('genWeight>0')
  nneg=treein.GetEntries('genWeight<0')
  h_weight.SetBinContent(1,nneg)
  h_weight.SetBinContent(2,npos)
  
#  for entry in range(0,5):
  for entry in range(0,treein.GetEntries()):
    p4temp = TLorentzVector()
    wp4temp=TLorentzVector()
    zp4 = TLorentzVector()
    LHElep = []
    GENjet = []
    GENDressLep = []
    HT_GEN = 0.
    treein.GetEntry(entry)
    weight=(treein.genWeight)/(abs(treein.genWeight))

    #LHE info
    for iLHE in range(2,treein.nLHEPart):
      if not (abs(treein.LHEPart_pdgId[iLHE])==11 or abs(treein.LHEPart_pdgId[iLHE])==13): continue
      p4temp.SetPtEtaPhiM(treein.LHEPart_pt[iLHE], treein.LHEPart_eta[iLHE], treein.LHEPart_phi[iLHE], treein.LHEPart_mass[iLHE])
      LHElep.append(p4temp.Clone())
    if len(LHElep)>0: LHElep.sort(key=lambda x: x.Pt(), reverse=True)
    if len(LHElep)==1: 
      histo_array['h_lep1pt_LHE'].Fill(LHElep[0].Pt(), weight)
      histo_array['h_lep1eta_LHE'].Fill(LHElep[0].Eta(), weight)
      histo_array['h_lep1phi_LHE'].Fill(LHElep[0].Phi(), weight)
    if len(LHElep)==2: 
      histo_array['h_lep1pt_LHE'].Fill(LHElep[0].Pt(), weight)
      histo_array['h_lep1eta_LHE'].Fill(LHElep[0].Eta(), weight)
      histo_array['h_lep1phi_LHE'].Fill(LHElep[0].Phi(), weight)
      histo_array['h_lep2pt_LHE'].Fill(LHElep[1].Pt(), weight)
      histo_array['h_lep2eta_LHE'].Fill(LHElep[1].Eta(), weight)
      histo_array['h_lep2phi_LHE'].Fill(LHElep[1].Phi(), weight)
    histo_array['h_Vpt_LHE'].Fill(treein.LHE_Vpt, weight)
    histo_array['h_HT_LHE'].Fill(treein.LHE_HT, weight)

    #GEN info
    cleanJet_ref=[]
    for ijet in range(0, treein.nGenJet):
      pass_lepclean=True
      for ilep in range(0, treein.nGenDressedLepton):
        if deltaR(treein.GenJet_eta[ijet], treein.GenJet_phi[ijet], treein.GenDressedLepton_eta[ilep], treein.GenDressedLepton_phi[ilep])<0.3:
          pass_lepclean=False
      if pass_lepclean:cleanJet_ref.append(ijet)

    histo_array['h_ngenjet_GEN'].Fill(treein.nGenJet, weight)
    for ijet in range(0, treein.nGenJet):
      if ijet not in cleanJet_ref:
        continue
      HT_GEN = HT_GEN+treein.GenJet_pt[ijet]
      p4temp.SetPtEtaPhiM(treein.GenJet_pt[ijet], treein.GenJet_eta[ijet], treein.GenJet_phi[ijet],treein.GenJet_mass[ijet])
      GENjet.append(p4temp.Clone())#no need to sort
    if len(GENjet)>2:
      histo_array['h_j1pt_GEN'].Fill(GENjet[0].Pt(), weight)
      histo_array['h_j1eta_GEN'].Fill(GENjet[0].Eta(), weight)
      histo_array['h_j1phi_GEN'].Fill(GENjet[0].Phi(), weight)
      histo_array['h_j1mass_GEN'].Fill(GENjet[0].M(), weight)
      histo_array['h_j2pt_GEN'].Fill(GENjet[1].Pt(), weight)
      histo_array['h_j2eta_GEN'].Fill(GENjet[1].Eta(), weight)
      histo_array['h_j2phi_GEN'].Fill(GENjet[1].Phi(), weight)
      histo_array['h_j2mass_GEN'].Fill(GENjet[1].M(), weight)
      histo_array['h_j3pt_GEN'].Fill(GENjet[2].Pt(), weight)
      histo_array['h_j3eta_GEN'].Fill(GENjet[2].Eta(), weight)
      histo_array['h_j3phi_GEN'].Fill(GENjet[2].Phi(), weight)
      histo_array['h_j3mass_GEN'].Fill(GENjet[2].M(), weight)
      histo_array['h_drj1j2_GEN'].Fill(GENjet[0].DeltaR(GENjet[1]), weight)
      histo_array['h_drj1j3_GEN'].Fill(GENjet[0].DeltaR(GENjet[2]), weight)
      histo_array['h_drj2j3_GEN'].Fill(GENjet[1].DeltaR(GENjet[2]), weight)
      
    elif len(GENjet)>1:
      histo_array['h_j1pt_GEN'].Fill(GENjet[0].Pt(), weight)
      histo_array['h_j1eta_GEN'].Fill(GENjet[0].Eta(), weight)
      histo_array['h_j1phi_GEN'].Fill(GENjet[0].Phi(), weight)
      histo_array['h_j1mass_GEN'].Fill(GENjet[0].M(), weight)
      histo_array['h_j2pt_GEN'].Fill(GENjet[1].Pt(), weight)
      histo_array['h_j2eta_GEN'].Fill(GENjet[1].Eta(), weight)
      histo_array['h_j2phi_GEN'].Fill(GENjet[1].Phi(), weight)
      histo_array['h_j2mass_GEN'].Fill(GENjet[1].M(), weight)
      histo_array['h_drj1j2_GEN'].Fill(GENjet[0].DeltaR(GENjet[1]), weight)
    elif len(GENjet)>0:
      histo_array['h_j1pt_GEN'].Fill(GENjet[0].Pt(), weight)
      histo_array['h_j1eta_GEN'].Fill(GENjet[0].Eta(), weight)
      histo_array['h_j1phi_GEN'].Fill(GENjet[0].Phi(), weight)
      histo_array['h_j1mass_GEN'].Fill(GENjet[0].M(), weight)
    else: 
      pass
    histo_array['h_HT_GEN'].Fill(HT_GEN, weight)

    if treein.nGenDressedLepton>0:
      for idressedlep in range(0, treein.nGenDressedLepton):
        if treein.GenDressedLepton_hasTauAnc[idressedlep]: continue
        p4temp.SetPtEtaPhiM(treein.GenDressedLepton_pt[idressedlep],treein.GenDressedLepton_eta[idressedlep],treein.GenDressedLepton_phi[idressedlep],treein.GenDressedLepton_mass[idressedlep])
        GENDressLep.append(extend_p4(p4temp.Clone(), treein.GenDressedLepton_pdgId[idressedlep]))
      GENDressLep.sort(key=lambda x: x.p4_vector.Pt(), reverse=True)
      if len(GENDressLep)==1:
        wp4temp=wreco(GENDressLep[0].p4_vector, treein.GenMET_pt, treein.GenMET_phi)
        histo_array['h_Weta_GEN'].Fill(wp4temp.Eta(), weight)
        histo_array['h_Wpt_GEN'].Fill(wp4temp.Pt(), weight)
        histo_array['h_Wphi_GEN'].Fill(wp4temp.Phi(), weight)
        histo_array['h_Dressedlep1pt_GEN'].Fill(GENDressLep[0].p4_vector.Pt(), weight)
        histo_array['h_Dressedlep1eta_GEN'].Fill(GENDressLep[0].p4_vector.Eta(), weight)
        histo_array['h_Dressedlep1phi_GEN'].Fill(GENDressLep[0].p4_vector.Phi(), weight)
        if len(GENjet)>2:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep1j2_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[1]), weight)
          histo_array['h_drlep1j3_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[2]), weight)
        elif len(GENjet)>1:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep1j2_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[1]), weight)
        elif len(GENjet)>0:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
        else:
          pass
      if len(GENDressLep)==2:
        histo_array['h_Dressedlep1pt_GEN'].Fill(GENDressLep[0].p4_vector.Pt(), weight)
        histo_array['h_Dressedlep1eta_GEN'].Fill(GENDressLep[0].p4_vector.Eta(), weight)
        histo_array['h_Dressedlep1phi_GEN'].Fill(GENDressLep[0].p4_vector.Phi(), weight)
        histo_array['h_Dressedlep2pt_GEN'].Fill(GENDressLep[1].p4_vector.Pt(), weight)
        histo_array['h_Dressedlep2eta_GEN'].Fill(GENDressLep[1].p4_vector.Eta(), weight)
        histo_array['h_Dressedlep2phi_GEN'].Fill(GENDressLep[1].p4_vector.Phi(), weight)
        histo_array['h_drl1l2_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENDressLep[1].p4_vector),weight)
        if len(GENjet)>2:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep1j2_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[1]), weight)
          histo_array['h_drlep1j3_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[2]), weight)
          histo_array['h_drlep2j1_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep2j2_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[1]), weight)
          histo_array['h_drlep2j3_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[2]), weight)
        elif len(GENjet)>1:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep1j2_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[1]), weight)
          histo_array['h_drlep2j1_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep2j2_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[1]), weight)
        elif len(GENjet)>0:
          histo_array['h_drlep1j1_GEN'].Fill(GENDressLep[0].p4_vector.DeltaR(GENjet[0]), weight)
          histo_array['h_drlep2j1_GEN'].Fill(GENDressLep[1].p4_vector.DeltaR(GENjet[0]), weight)
        else:
          pass

        if (GENDressLep[0].pdgid+GENDressLep[1].pdgid)==0:
          zp4=GENDressLep[0].p4_vector+GENDressLep[1].p4_vector
          histo_array['h_Zmass_GEN'].Fill(zp4.M(), weight)
          histo_array['h_Zeta_GEN'].Fill(zp4.Eta(), weight)
          histo_array['h_Zpt_GEN'].Fill(zp4.Pt(), weight)
          histo_array['h_Zphi_GEN'].Fill(zp4.Phi(), weight)

    if treein.nGenVisTau>0:
      histo_array['h_Vistaupt_GEN'].Fill(treein.GenVisTau_pt[0], weight)
      histo_array['h_Vistaueta_GEN'].Fill(treein.GenVisTau_eta[0], weight)
      histo_array['h_Vistauphi_GEN'].Fill(treein.GenVisTau_phi[0], weight)

    histo_array['h_METpt_GEN'].Fill(treein.GenMET_pt, weight)
    histo_array['h_METphi_GEN'].Fill(treein.GenMET_phi, weight)


  fileout=TFile.Open(opt.outputfiles+'.root','RECREATE')
  fileout.cd()
  for key in histo_array:
    histo_array[key].Write()
  fileout.Close()

if __name__ == "__main__":
  sys.exit(main())
