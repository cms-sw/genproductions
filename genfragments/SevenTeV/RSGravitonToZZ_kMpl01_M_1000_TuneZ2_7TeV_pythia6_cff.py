import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2Settings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	filterEfficiency = cms.untracked.double(1),
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(0.128),
	
	PythiaParameters = cms.PSet(
	        pythiaUESettingsBlock,
                processParameters = cms.vstring(
		        'MSEL = 0',
		        'MSUB(391) = 1',
		        'MSUB(392) = 1',
		        'PMAS(347,1) = 1000',
		        'PARP(50) = 0.54', #0.54
		        '5000039:ALLOFF',
		        '5000039:ONIFANY 23',
        ),
		parameterSets = cms.vstring(
		        'pythiaUESettings',
			'processParameters')
	)
)

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('\$Revision: 1.1 $'),
	name = cms.untracked.string('\$Source: /cvs/CMSSW/UserCode/hinzmann/production/RSGravitonToZZ_kMpl01_M_1000_TuneZ2_7TeV_pythia6_cff.py,v $'),
	annotation = cms.untracked.string('Fall2011 sample with PYTHIA6: RSG -> ZZ, TuneZ2')
)

ProductionFilterSequence = cms.Sequence(generator)
