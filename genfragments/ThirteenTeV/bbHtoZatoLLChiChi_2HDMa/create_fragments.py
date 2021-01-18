#!/usr/bin/env python3

##############################################
def fragment(mH,ma):
##############################################
	
	fragment_str = f"""import FWCore.ParameterSet.Config as cms

externalLHEProducer = cms.EDProducer("ExternalLHEProducer",
    args = cms.vstring('/eos/user/d/dperezad/Store/Gridpacks/bbHtoZatoLLChiChi_2HDMa/bbH_Za_LLChiChi_MH{mH}_Ma{ma}_MChi45_4f_slc7_amd64_gcc700_CMSSW_10_6_19_tarball.tar.xz'),
    nEvents = cms.untracked.uint32(5000),
    numberOfParameters = cms.uint32(1),
    outputFile = cms.string('cmsgrid_final.lhe'),
    scriptName = cms.FileInPath('GeneratorInterface/LHEInterface/data/run_generic_tarball_cvmfs.sh')
)

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings'
                                    )
    )
)

ProductionFilterSequence = cms.Sequence(generator)
"""
				
	return fragment_str
	
##############################################
def main():
##############################################
	
	for MH in ['400','500','600','800','1000']:
		
		for Ma in ['100','200','300','400','600','800']:
			
			if int(Ma)+2*90>int(MH):
				continue
				
			############# creating fragment ###############
			fin = open(f"bbH_Za_LLChiChi_MH{MH}_Ma{Ma}_MChi45_4f-TuneCP5_13TeV-fragment.py", "wt")
			fragment_card = fragment(MH,Ma)
			fin.write(fragment_card)
			fin.close()
			
if __name__ == "__main__":
	main()
