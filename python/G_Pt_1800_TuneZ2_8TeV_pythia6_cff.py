import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(1.865e-06),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 10       ! Photon+Jets production',
			'CKIN(3) = 1800  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision:$'),
	name = cms.untracked.string('\$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/G_Pt_1800_TuneZ2_8TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('Spring2011 sample with PYTHIA6: Prompt photon production, pThat > 1800 GeV, TuneZ2')
)
