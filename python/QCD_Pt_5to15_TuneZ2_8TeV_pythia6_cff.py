import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(4.262793e+10),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 5     ! minimum pt hat for hard interactions',
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

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision$'),
	name = cms.untracked.string('\$Source$'),
	annotation = cms.untracked.string('Spring2011 sample with PYTHIA6: QCD dijet production, pThat = 5 .. 15 GeV, TuneZ2')
)
