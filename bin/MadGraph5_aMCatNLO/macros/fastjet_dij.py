''' 
The script calculates the DJR differentially in the number of additional jets (taken as additional particles in the LHEpart)
It uses the scikit-hep package of fastjet to calculate the clustering 
'''
import argparse
import os
import warnings
import coffea
import numpy as np
import matplotlib.pyplot as plt
import mplhep as hep
from tqdm import tqdm
#from coffea.nanoevents.methods import vector  
from pathlib import Path
from coffea.nanoevents import NanoEventsFactory, NanoAODSchema
warnings.filterwarnings("ignore", message="Missing cross-reference index")
import fastjet
import uproot
import vector 
import awkward as ak


def retrieve_4arr(dict_dij,add_jet):
    '''
    function to extract the dij from the list into arrays
    '''
    dij = dict_dij[add_jet]
    return dij[0], dij[1], dij[2], dij[3]


def plot(dijs, w, add_part):
    '''
    plotting the dijs in 4 plots, separated by the DJR (0to1, 1to2, ...)
    '''
    #for dijx_y: x: number of additional jets, y:dij at merge for yth jet 
    dij0_1, dij0_2, dij0_3, dij0_4 = retrieve_4arr(dijs, 0)
    dij1_1, dij1_2, dij1_3, dij1_4 = retrieve_4arr(dijs, 1)
    dij2_1, dij2_2, dij2_3, dij2_4 = retrieve_4arr(dijs, 2)
    dij3_1, dij3_2, dij3_3, dij3_4 = retrieve_4arr(dijs, 3)
    hep.style.use(hep.style.CMS) 
    hep.cms.text('Simulation')
    binsi = np.logspace(1,7,70)
    efficiencies = [len(dij0_1)/len(w), len(dij0_2/len(w)), len(dij0_3/len(w)), len(dij0_4/len(w)),
                    len(dij1_1/len(w)), len(dij1_2/len(w)),len(dij1_3/len(w)),len(dij1_4/len(w)),
                    len(dij2_1/len(w)), len(dij2_2/len(w)), len(dij2_3/len(w)), len(dij2_4/len(w)),
                    len(dij3_1/len(w)),len( dij3_2/len(w)), len(dij3_3/len(w)), len(dij3_4/len(w))]
    efficiencies = np.array(efficiencies) 
    plt.hist(dij0_1, weights=w[add_part==0], bins=binsi, histtype="step", label=r"$0 \rightarrow 1$, 0 additional jets, eff: "+str(round(efficiencies[0],3)*100))
    plt.hist(dij1_1, weights=w[add_part==1], bins=binsi, histtype="step", label=r"$0 \rightarrow 1$, 1 additional jet, eff: "+str(round(efficiencies[4],3)*100))
    plt.hist(dij2_1, weights=w[add_part==2], bins=binsi, histtype="step", label=r"$0 \rightarrow 1$, 2 additional jets, eff: "+str(round(efficiencies[8],3)*100))
    plt.hist(dij3_1, weights=w[add_part==3], bins=binsi, histtype="step", label=r"$0 \rightarrow 1$, 3 additional jets, eff: "+str(round(efficiencies[12],3)*100))
    plt.legend()
    plt.xlabel(r'dij [GeV]')
    plt.yscale("log")
    plt.xscale("log")
    plt.savefig('0to1.pdf',dpi=200)
    plt.clf()
    hep.style.use(hep.style.CMS) # For now ROOT defaults to CMS
    hep.cms.text('Simulation')
    plt.hist(dij0_2, weights=w[add_part==0], bins=binsi, histtype="step", label=r"$1 \rightarrow 2$, 0 additional jets")
    plt.hist(dij1_2, weights=w[add_part==1], bins=binsi, histtype="step", label=r"$1 \rightarrow 2$, 1 additional jet")
    plt.hist(dij2_2, weights=w[add_part==2], bins=binsi, histtype="step", label=r"$1 \rightarrow 2$, 2 additional jet")
    plt.hist(dij3_2, weights=w[add_part==3], bins=binsi, histtype="step", label=r"$1 \rightarrow 2$, 3 additional jet")
    plt.legend()
    plt.xlabel(r'dij [GeV]')
    plt.yscale("log")
    plt.xscale("log")
    plt.savefig('1to2.pdf',dpi=200)
    plt.clf()
    hep.style.use(hep.style.CMS) # For now ROOT defaults to CMS
    hep.cms.text('Simulation')
    plt.hist(dij0_3, weights=w[add_part==0], bins=binsi, histtype="step", label=r"$2 \rightarrow 3$, 0 additional jets")
    plt.hist(dij1_3, weights=w[add_part==1], bins=binsi, histtype="step", label=r"$2 \rightarrow 3$, 1 additional jet")
    plt.hist(dij2_3, weights=w[add_part==2], bins=binsi, histtype="step", label=r"$2 \rightarrow 3$, 2 additional jet")
    plt.hist(dij3_3, weights=w[add_part==3], bins=binsi, histtype="step", label=r"$2 \rightarrow 3$, 3 additional jet")
    plt.legend()
    plt.xlabel(r'dij [GeV]')
    plt.yscale("log")
    plt.xscale("log")
    plt.savefig('2to3.pdf',dpi=200)
    plt.clf()
    hep.style.use(hep.style.CMS) # For now ROOT defaults to CMS
    hep.cms.text('Simulation')
    plt.hist(dij0_4, weights=w[add_part==0], bins=binsi, histtype="step", label=r"$3 \rightarrow 4$, 0 additional jets")
    plt.hist(dij1_4, weights=w[add_part==1], bins=binsi, histtype="step", label=r"$3 \rightarrow 4$, 1 additional jet")
    plt.hist(dij2_4, weights=w[add_part==2], bins=binsi, histtype="step", label=r"$3 \rightarrow 4$, 2 additional jet")
    plt.hist(dij3_4, weights=w[add_part==3], bins=binsi, histtype="step", label=r"$3 \rightarrow 4$, 3 additional jet")
    plt.legend()
    plt.xlabel(r'dij [GeV]')
    plt.yscale("log")
    plt.xscale("log")
    plt.savefig('3to4.pdf', dpi=200)

def main():
    vector.register_awkward()
    parser = argparse.ArgumentParser()
    #input file
    parser.add_argument("-i", "--Input", help= "Input File")
    parser.add_argument('-vtau', '--Veto_Tau', help='Veto tau events in the LHEpart?', default=False)
    parser.add_argument('-s', '--SaveResults', help='enables saving the files as npy files', default=True)
    args = parser.parse_args()
    fname = args.Input
    with uproot.open(fname) as file:
        events = file['Events;1'].arrays()
    #veto all tau events
    if args.Veto_Tau:
        events = events[np.all(np.abs(events.LHEPart_pdgId) != 15, axis=1)]
    w = events.Generator_weight
    #count how many additional jets each event has 
    #add_part = [len(ev[ev.status==1])-2 for ev in events['LHEPart']]
    add_part = events.LHE_Njets  
    add_part = np.array(add_part)
    dijs = dict()
    #loop over the cases of extra jets 
    for i in range(4):
        ev_per_add_jet = events[add_part==i]
        #ev_per_add_jet = ev_per_add_jet[((ev_per_add_jet.GenPart_status==1)& (ev_per_add_jet.GenPart_pt>0.01)) & (np.abs(ev_per_add_jet.GenPart_pdgId) != 11)&
        #                                 (np.abs(ev_per_add_jet.GenPart_pdgId) != 13)& (np.abs(ev_per_add_jet.GenPart_pdgId) != 15)]
        jetdef = fastjet.JetDefinition(fastjet.kt_algorithm, 1)
        data = ak.zip({'pt':ev_per_add_jet.GenPart_pt, 'eta': ev_per_add_jet.GenPart_eta, 'phi': ev_per_add_jet.GenPart_phi, 'M': ev_per_add_jet.GenPart_mass},with_name="Momentum4D",)
        cl_alg = fastjet.ClusterSequence(data, jetdef)
        dijs[i] =  [cl_alg.exclusive_dmerge_max( 0), cl_alg.exclusive_dmerge_max( 1),cl_alg.exclusive_dmerge_max( 2),
                    cl_alg.exclusive_dmerge_max( 4)]
    plot(dijs, w, add_part)
    if args.SaveResults:
        np.save('dijs.npy', dijs) 
        we_np = ak.to_numpy(w)
        np.save('weights.npy', we_np)
        np.save('addpart.npy', add_part)





if __name__== '__main__':
        main()
