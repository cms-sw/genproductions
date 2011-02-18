import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(2760.0),
	crossSection = cms.untracked.double(1.087214e-02),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 80  ! minimum pt hat for hard interactions'
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.2 $'),
	name = cms.untracked.string('\$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/QCD_Pt_80_TuneZ2_2760GeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('Sprint2011 sample with PYTHIA6: QCD dijet production, pThat > 80 GeV, TuneZ2')
)

ProductionFilterSequence = cms.Sequence(generator)
