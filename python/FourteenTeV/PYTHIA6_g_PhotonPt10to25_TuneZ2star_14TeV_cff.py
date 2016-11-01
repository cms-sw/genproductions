import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(14000.0),
	crossSection = cms.untracked.double(1.216e+06),
	filterEfficiency = cms.untracked.double(1.0),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
                'MSEL=10 ',
                #'MSUB(114)=1       ',
                'CKIN(3)=10.          ! minimum pt hat for hard interactions', 
                'CKIN(4)=25.          ! maximum pt hat for hard interactions'),                
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)
