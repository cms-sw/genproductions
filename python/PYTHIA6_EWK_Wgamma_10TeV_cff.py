import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(11960.),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
        processParameters = cms.vstring('MSEL        = 0    !User defined processes',
                'MSUB(20)=1 !Wgamma production',
                'CKIN(3)=10.        !minimum pt hat for hard interactions',
		'MDME(190,1)=0      !W decay into dbar u',
		'MDME(191,1)=0      !W decay into dbar c',
		'MDME(192,1)=0      !W decay into dbar t',
		'MDME(194,1)=0      !W decay into sbar u',
		'MDME(195,1)=0      !W decay into sbar c',
		'MDME(196,1)=0      !W decay into sbar t',
		'MDME(198,1)=0      !W decay into bbar u',
		'MDME(199,1)=0      !W decay into bbar c',
		'MDME(200,1)=0      !W decay into bbar t',
		'MDME(206,1)=1      !W decay into e+ nu_e',
		'MDME(207,1)=1      !W decay into mu+ nu_mu',
		'MDME(208,1)=1      !W decay into tau+ nu_tau'),
        # This is a vector of ParameterSet names to be read, in this order
        parameterSets = cms.vstring('pythiaUESettings', 
            'processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_EWK_Wgamma_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA6-EWK Wgamma at 10TeV')
)

