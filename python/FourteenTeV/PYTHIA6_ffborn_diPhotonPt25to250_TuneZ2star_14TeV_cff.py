import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(14000.0),
	crossSection = cms.untracked.double(236.4),
	filterEfficiency = cms.untracked.double(1.0),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
    'MSEL=0 ',
    'MSUB(18)=1       ',
    'CKIN(3)=25.          ! minimum pt hat for hard interactions', 
    'CKIN(4)=250.          ! maximum pt hat for hard interactions'),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)
