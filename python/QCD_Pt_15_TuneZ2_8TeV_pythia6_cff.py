import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(1.063e+09),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 15.    ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.0 $'),
	name = cms.untracked.string('\$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/QCD_Pt_15_TuneZ2_8TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('Winter2010 sample with PYTHIA6: QCD dijet production @ 8TeV, pThat > 15 GeV, TuneZ2')
)
