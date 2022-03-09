# import ROOT
# from math import pi, sqrt, copysign
# from ROOT import TH1F, TFile, TTree, TString, gSystem, gROOT, AddressOf, TLorentzVector, TVector, TMath
import sys
import os
# import numpy as np

verbose = False
#cardsPath = sys.argv[1]
# model = sys.argv[1] # either 2hdm or zpb

cardsPath = 'cards/examples'
# gridpoint = 'MH3_600_MH4_XX_MChi_YY'
prefix    = 'bbDM_DMSimp_pseudo_NLO_MPhi_YY_MChi_XX'

d_cardspath = os.path.join(cardsPath, prefix)

if verbose: print (d_cardspath)

d_run_card       = os.path.join(d_cardspath,prefix+'_run_card.dat')
d_proc_card      = os.path.join(d_cardspath,prefix+'_proc_card.dat')
d_extramodels    = os.path.join(d_cardspath,prefix+'_extramodels.dat')
d_customizecards = os.path.join(d_cardspath,prefix+'_customizecards.dat')
d_cutcards       = os.path.join(d_cardspath,prefix+'_cuts.f')

# print ()

#
if verbose: print (d_run_card, d_proc_card, d_extramodels, d_customizecards)

def change_cards(d_cardname, cardname, MPhi, MChi):
    f    = open(d_cardname, 'r')
    fout = open (cardname, 'w')
    for line in f:
        line = line.replace('YY', str(MPhi))
        line = line.replace('XX', str(MChi))
        fout.write(line)
    fout.close()
    print ("Cardname",cardname)

def submitgrid(MPhi, MChi):
    cardspath = d_cardspath.replace("YY",str(MPhi)).replace("XX",str(MChi))
    print ("cardpath",cardspath)
    os.system('mkdir '+cardspath)

    run_card       = d_run_card.replace("YY",str(MPhi)).replace("XX",str(MChi))
    proc_card      = d_proc_card.replace("YY",str(MPhi)).replace("XX",str(MChi))
    extramodels    = d_extramodels.replace("YY",str(MPhi)).replace("XX",str(MChi))
    customizecards = d_customizecards.replace("YY",str(MPhi)).replace("XX",str(MChi))
    cutcards = d_cutcards.replace("YY",str(MPhi)).replace("XX",str(MChi))

    change_cards(d_run_card, run_card, MPhi, MChi)
    change_cards(d_proc_card, proc_card, MPhi, MChi)
    change_cards(d_extramodels, extramodels, MPhi, MChi)
    change_cards(d_customizecards, customizecards, MPhi, MChi)
    change_cards(d_cutcards, cutcards, MPhi, MChi)

    outdir = prefix.replace("YY",str(MPhi)).replace("XX",str(MChi))
    print ("output dir",outdir)
    # os.system('nohup ./submit_cmsconnect_gridpack_generation.sh  '+ outdir +' '+ cardspath +'  4 "4 Gb" > mysubmit_'+outdir+'.debug 2>&1 &')
    #os.system('./submit_gridpack_generation.sh 30000 30000 2nw '+ outdir +' '+ cardspath +'  2nw')
    #os.system('nohup ./submit_cmsconnect_gridpack_generation.sh  '+ outdir +' '+ cardspath +'  4 "4 Gb" > mysubmit_'+outdir+'.debug 2>&1 &')

MPhi=[100,300,500,700,900,1100]
MChi=[10]
for i in MPhi:
    for j in MChi:
        if i > j:
            mphi = i
            mchi = j
            print ("MChi= ", mchi, "MPhi= ",mphi)
            submitgrid(mphi, mchi)
