import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string(': 1.1 $'),
	name = cms.untracked.string(': /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Pythia6_QCD_pthat.template,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of QCD events, 10TeV, D6T tune, pthat > 170 GeV')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(10000.0),
	crossSection = cms.untracked.double(6.256287713e+04),
	filterEfficiency = cms.untracked.double(1.0000),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=1   ! QCD hight pT processes',
			'CKIN(3)=170  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ProductionFilterSequence = cms.Sequence(generator)
