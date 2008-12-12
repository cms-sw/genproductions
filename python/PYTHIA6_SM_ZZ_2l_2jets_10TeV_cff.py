import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",					       
    pythiaHepMCVerbosity = cms.untracked.bool(False),			       
    maxEventsToPrint = cms.untracked.int32(0),				       
    pythiaPylistVerbosity = cms.untracked.int32(0),			       
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),				       
    PythiaParameters = cms.PSet(					       
	pythiaUESettingsBlock,
	processParameters = cms.vstring(
	    'MSEL=0              ! 0=full user control (then use MSUB)',
            'MSUB(22)=1          ! ffbar->Z/gamma Z/gamma',
	    'PMAS(23,1)=91.188   ! Z mass',
	    'PMAS(24,1)=80.450   ! W mass',

# Z boson decays: Z->qq
            'MDME(174,1)=4           ! Z decay into d dbar', 		       
            'MDME(175,1)=4           ! Z decay into u ubar', 		       
            'MDME(176,1)=4           ! Z decay into s sbar', 		       
            'MDME(177,1)=4           ! Z decay into c cbar', 		       
            'MDME(178,1)=4           ! Z decay into b bbar', 		       
            'MDME(179,1)=0           ! Z decay into t tbar',
# Z boson decays: Z->ee      
            'MDME(182,1)=5           ! Z decay into e- e+', 		       
            'MDME(183,1)=0           ! Z decay into nu_e nu_ebar',
# Z boson decays: Z->mumu                  
            'MDME(184,1)=5           ! Z decay into mu- mu+', 		       
            'MDME(185,1)=0           ! Z decay into nu_mu nu_mubar', 	       
            'MDME(186,1)=0           ! Z decay into tau- tau+', 	       
            'MDME(187,1)=0           ! Z decay into nu_tau nu_taubar'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 'processParameters')
    )									       
)									       
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_SM_H_ZZ_2l_2jets_mH140_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6 SM ZZ->2l2jets at 10TeV')
)
