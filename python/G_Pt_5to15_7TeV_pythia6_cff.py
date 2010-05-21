import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision$'),
	name = cms.untracked.string('$Source$'),
	annotation = cms.untracked.string('Spring10: Pythia6 generation of Photon+Jet events, 7TeV, pthat = 5 .. 15 GeV')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *
generator = cms.EDFilter("Pythia6GeneratorFilter",
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	filterEfficiency = cms.untracked.double(1.0),
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(4.030e+06),
	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=10',
			'CKIN(3)=5 ! minimum pt hat for hard interactions',
			'CKIN(4)=15 ! maximum pt hat for hard interactions'
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ProductionFilterSequence = cms.Sequence(generator)
