import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.3 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCDpt_0_15_7TeV_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of QCD events, 7TeV, D6T tune, pthat = 0 .. 15 GeV')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(4.84450e+10),
	filterEfficiency = cms.untracked.double(0.981),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=1   ! QCD hight pT processes',
			'CKIN(3)=0  ! minimum pt hat for hard interactions',
			'CKIN(4)=15  ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

pthat_filter = cms.EDFilter("MCProcessFilter",
	MaxPthat = cms.untracked.vdouble(15., 15.0, 15.0, 15.0, 15.0, 15.0),
	ProcessID = cms.untracked.vint32(11, 12, 13, 68, 28, 53),
	MinPthat = cms.untracked.vdouble(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
)

ProductionFilterSequence = cms.Sequence(generator*pthat_filter)
