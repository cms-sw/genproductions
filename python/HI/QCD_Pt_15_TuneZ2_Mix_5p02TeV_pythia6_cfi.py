import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

hiSignal = cms.EDFilter("Pythia6GeneratorFilter",
		comEnergy = cms.double(5020.0),
		filterEfficiency = cms.untracked.double(1.0000),
		maxEventsToPrint = cms.untracked.int32(0),
		pythiaHepMCVerbosity = cms.untracked.bool(False),
		pythiaPylistVerbosity = cms.untracked.int32(0),

		PythiaParameters = cms.PSet(
			pythiaUESettingsBlock,
			processParameters = cms.vstring(
				'MSEL=1   ! QCD hight pT processes',
				'CKIN(3)=15  ! minimum pt hat for hard interactions',
				),
			parameterSets = cms.vstring(
				'pythiaUESettings',
				'processParameters',
				)
			)
		)

ProductionFilterSequence = cms.Sequence(hiSignal)
