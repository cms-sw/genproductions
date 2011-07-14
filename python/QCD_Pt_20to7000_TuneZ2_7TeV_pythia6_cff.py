import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(5.312237e+07),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 20    ! minimum pt hat for hard interactions',
			'CKIN(4) = 7000  ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.9 $'),
	name = cms.untracked.string('\$Source: /cvs/CMSSW/CMSSW/Configuration/GenProduction/python/QCD_Pt_30to50_TuneZ2_7TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('Summer2011 sample with PYTHIA6: QCD dijet production, pThat = 30 .. 50 GeV, TuneZ2')
)
