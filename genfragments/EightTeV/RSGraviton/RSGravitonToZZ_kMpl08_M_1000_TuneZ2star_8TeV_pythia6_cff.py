import FWCore.ParameterSet.Config as cms

from Configuration.Generator.PythiaUEZ2starSettings_cfi import *

generator = cms.EDFilter("Pythia6GeneratorFilter",
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaPylistVerbosity = cms.untracked.int32(0),
	filterEfficiency = cms.untracked.double(1),
	comEnergy = cms.double(8000.0),
	crossSection = cms.untracked.double(8.775),
	
	PythiaParameters = cms.PSet(
	        pythiaUESettingsBlock,
                processParameters = cms.vstring(
		        'MSEL = 0',
		        'MSUB(391) = 1',
		        'MSUB(392) = 1',
		        'PMAS(347,1) = 1000',
		        'PARP(50) = 4.32', 
		        '5000039:ALLOFF',
		        '5000039:ONIFANY 23',
        ),
		parameterSets = cms.vstring(
		        'pythiaUESettings',
			'processParameters')
	)
)


ProductionFilterSequence = cms.Sequence(generator)

