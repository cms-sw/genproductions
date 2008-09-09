

import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(0.0003),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(58700.0),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
	    'MSEL=0       ', 
            'MSUB(14)=0   ', 
            'MSUB(18)=0   ', 
            'MSUB(19)=0   ', 
            'MSUB(20)=0   ', 
            'MSUB(29)=0   ', 
            'MSUB(11)=1   ', 
            'MSUB(12)=1   ', 
            'MSUB(13)=1   ', 
            'MSUB(15)=1   ', 
            'MSUB(16)=1   ', 
            'MSUB(28)=1   ', 
            'MSUB(30)=1   ', 
            'MSUB(31)=1   ', 
            'MSUB(53)=1   ', 
            'MSUB(68)=1   ', 
	
	'CKIN(3)=170  ! minimum pt hat for hard interactions',
	'CKIN(4)=300  ! maximum pt hat for hard interactions'),
	# This is a vector of ParameterSet names to be read, in this order
	parameterSets = cms.vstring('pythiaUESettings', 
	'processParameters')
	
   

    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string
('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDEnrichedpt_170_300_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('QCDEnrichedpt-170-300 at 10TeV')
)



gj_filter = cms.EDFilter("PythiaFilterGammaJetWithBg",
    MaxEvents = cms.untracked.int32(2),
    MaxPhotonEta = cms.untracked.double(2.8),
    MaxPhotonPt = cms.untracked.double(22.0),
    MinPhotonEtaForwardJet = cms.untracked.double(1.3),
    moduleLabel = cms.untracked.string('source'),
    MinDeltaPhi = cms.untracked.double(170.0),
    MinPhotonPt = cms.untracked.double(18.0),
    MaxDeltaEta = cms.untracked.double(1.3),
    PhotonSeedPt = cms.untracked.double(5.0)
)
ProductionFilterSequence = cms.Sequence(gj_filter)
