import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *


generator = cms.EDFilter('Pythia6GeneratorFilter',
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(50000000000),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL = 1        ! QCD hight pT processes',
			'CKIN(3) = 0  ! minimum pt hat for hard interactions',
			'CKIN(4) = 15  ! maximum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/QCD_Pt_0to15_TuneZ2star_8TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('PYTHIA6: QCD dijet production at 8TeV, pThat = 0 .. 15 GeV, TuneZ2star')
)


