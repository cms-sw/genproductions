

import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.double(7000.0),
    crossSection = cms.untracked.double(2.757e+00),
    PythiaParameters = cms.PSet(
        pythiaUESettingsBlock,
	processParameters = cms.vstring(
	
	'MSEL=0       ',
	'MSUB(15)=1   ',
	'MSUB(30)=1   ',
	
	'MDME(174,1)=0  !dd~',
	'MDME(175,1)=0  !uu~ ',
	'MDME(176,1)=0  !ss~',
	'MDME(177,1)=0  !cc~ ',
	'MDME(178,1)=0  !bb~ ', 
	'MDME(179,1)=0  !tt~',
	'MDME(182,1)=0  !ee',
	'MDME(183,1)=0  !nunu', 
	'MDME(184,1)=1  !mumu',
	'MDME(185,1)=0  !nunu',
	'MDME(186,1)=0  !tautau', 
	'MDME(187,1)=0  !nunu ',
	
	
	'CKIN(3)=120  ! minimum pt hat for hard interactions',
	'CKIN(4)=170  ! maximum pt hat for hard interactions'),
	# This is a vector of ParameterSet names to be read, in this order
	parameterSets = cms.vstring('pythiaUESettings', 
	'processParameters')
	
   

    )
)
configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string
('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_ZmumuJetpt_120_170_7TeV_cff.py,v $'),
    annotation = cms.untracked.string('ZmumuJetpt-120-170 at 7TeV')
)

ProductionFilterSequence = cms.Sequence(generator)
