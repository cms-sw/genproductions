'''
script to calculate the DRJ differentially in the number of
 additional LHE Particles (based on DY + x Particles)
'''

import awkward as ak
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


def retrieve_4arr(dict_dij,add_jet):
    dij1 = [item[0] for item in dict_dij[add_jet]]
    dij2 = [item[1] for item in dict_dij[add_jet]]
    dij3 = [item[2] for item in dict_dij[add_jet]]
    dij4 = [item[3] for item in dict_dij[add_jet]]
    return dij1, dij2, dij3, dij4

def main():
    parser = argparse.ArgumentParser()
    #
    parser.add_argument("-i", "--Input", help= "Input File")
    parser.add_argument('-vtau', '--VetoTau', help='Veto events with taus in LHE', default=False)
    parser.add_argument('-s', '--SaveResults', help='enables saving the files as npy files', default=True)
    args = parser.parse_args()
    fname = args.Input
    events = NanoEventsFactory.from_root(
        fname,schemaclass=NanoAODSchema.v7,
        metadata={"dataset": "DYJets"},
    ).events()
    #veto all tau events
    if args.VetoTau:
        events = events[np.all(np.abs(events["LHEPart"].pdgId) != 15, axis=1)]
    w = events["genWeight"]
    add_part = list()
    #count how many additional jets each event has 
    for ev in events["LHEPart"]:
        add_part.append(len(ev[ev.status==1])-2)
    add_part = np.array(add_part)
    dijs = dict()
    for i in range(4):
        mindijs = list()
        for ev  in tqdm(events["GenPart"][np.where(add_part==i)]):
                #select (pseudo)particles with status stable, pt>0.01 GeV and rule out the leptons from the DY directly
                ev = ev[((ev.status==1)& (ev.pt>0.01)) & (np.abs(ev.pdgId) != 11)& (np.abs(ev.pdgId) != 13)& (np.abs(ev.pdgId) != 15)]
                #print(f'length of event:{(len(ev_prompt))}')
                fjet_in = [fastjet.PseudoJet(part.px, part.py, part.pz, part.E) for part in ev]
                #kt, antikt and Radius definition
                jetdef = fastjet.JetDefinition(fastjet.kt_algorithm, 0.4)
                cl_alg = fastjet.ClusterSequence(fjet_in, jetdef)
                mindijs.append([cl_alg.exclusive_dmerge( 0),cl_alg.exclusive_dmerge( 1),cl_alg.exclusive_dmerge( 2),cl_alg.exclusive_dmerge( 3)])
                dijs[i] = mindijs

    if args.SaveResults:
        np.save('dijs.npy', dijs) 
        we_np = ak.to_numpy(w)
        np.save('weights.npy', we_np)
        np.save('addpart.npy', add_part)
    #for dijx_y: x: number of additional jets, y:dij at merge for yth jet 
    dij0_1, dij0_2, dij0_3, dij0_4 = retrieve_4arr(dijs, 0)
    dij1_1, dij1_2, dij1_3, dij1_4 = retrieve_4arr(dijs, 1)
    dij2_1, dij2_2, dij2_3, dij2_4 = retrieve_4arr(dijs, 2)
    dij3_1, dij3_2, dij3_3, dij3_4 = retrieve_4arr(dijs, 3)


    hep.style.use(hep.style.CMS) 
    hep.cms.text('Simulation')
    binsi = np.logspace(1e-2,5,70)
    efficiencies = [len(dij0_1), len(dij0_2), len(dij0_3), len(dij0_4),
                    len(dij1_1), len(dij1_2),len(dij1_3),len(dij1_4),
                    len(dij2_1), len(dij2_2), len(dij2_3), len(dij2_4),
                    len(dij3_1),len( dij3_2), len(dij3_3), len(dij3_4)]
    efficiencies = np.array(efficiencies) / ev_per_job
    #print(efficiencies)


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
    plt.savefig('3to4.pdf',dpi=200)

if __name__== '__main__':
        main()
