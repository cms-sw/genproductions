import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(0),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 10     ! minimum pt hat for hard interactions',
			'CKIN(4) = 15    ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

pthat_filter = cms.EDFilter('MCProcessFilter',
	MaxPthat = cms.untracked.vdouble(15., 15.0, 15.0, 15.0, 15.0, 15.0),
	ProcessID = cms.untracked.vint32(11, 12, 13, 68, 28, 53),
	MinPthat = cms.untracked.vdouble(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
)

ProductionFilterSequence = cms.Sequence(generator * pthat_filter)
