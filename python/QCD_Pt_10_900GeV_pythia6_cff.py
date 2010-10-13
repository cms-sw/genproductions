import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.1 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Attic/PYTHIA6_QCD_Pthat_10_900GeV_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of QCD events, 900 GeV, D6T tune, pthat > 10 GeV')
)

from Configuration.Generator.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(900.0),
	crossSection = cms.untracked.double(8.76215e+08),
	filterEfficiency = cms.untracked.double(1.0000),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=1   ! QCD hight pT processes',
			'CKIN(3)=10  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ProductionFilterSequence = cms.Sequence(generator)
