import FWCore.ParameterSet.Config as cms
import os
from Configuration.GenProduction.ThirteenTeV.SemiVisibleJets.svjHelper import svjHelper

def createFragment(mZprime, mDark, rinv, alpha):
    helper = svjHelper()
    helper.setModel(mZprime, mDark, rinv, alpha)
    outname = helper.getOutName(outpre="SVJ",outsuff="TuneCP2_13TeV_pythia8")
    # make the name safe for python
    outname = outname.replace(".","p")

    # put parameters into template
    path = os.path.join(os.getenv("CMSSW_BASE"),"src/Configuration/GenProduction/python/ThirteenTeV/SemiVisibleJets")
    with open(os.path.join(path,"SVJTemplate_cff.py"),"r") as infile, open(os.path.join(path,outname+"_cff.py"),"w") as outfile:
        for line in infile:
            if "crossSection = " in line:
                outfile.write("    crossSection = cms.untracked.double("+str(helper.xsec)+"),\n")
            elif "processParameters = cms.vstring()," in line:
                outfile.write("        processParameters = cms.vstring(\n")
                outfile.write("".join(["            '"+setting+"',\n" for setting in helper.getPythiaSettings()]))
                outfile.write("        ),\n")
            else:
                outfile.write(line)

if __name__ == '__main__':
    createFragment(3000,20,0.3,0.2)
    createFragment(3000,20,0.3,"peak")
