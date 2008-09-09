import FWCore.ParameterSet.Config as cms

from Configuration.GenProduction.PythiaUESettings_cfi import *
source = cms.Source("PythiaSource",
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1.0),
    comEnergy = cms.untracked.double(10000.0),
    crossSection = cms.untracked.double(5.4),
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
	'MDME(182,1)=1  !ee',
	'MDME(183,1)=0  !nunu', 
	'MDME(184,1)=0  !mumu',
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
    version = cms.untracked.string('$Revision: 1.2 $'),
    name = cms.untracked.string
('$Source: /local/projects/CMSSW/rep/CMSSW/Configuration/GenProduction/python/PYTHIA6_ZeeJetpt_120_170_10TeV_cff.py,v $'),
    annotation = cms.untracked.string('ZeeJetpt-120-170 at 10TeV')
)

