import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.1 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_estar_7TeV_M600gev_Lambda10tev_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of e*,Mass=600Gev, Lambda=10TeV')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(1.194E-3),
	filterEfficiency = cms.untracked.double(1.0000),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring('MSEL=0',
                 'MSUB(169)=1', 
                 'PMAS(345,1) = 600', 
                 'RTCM(41) = 10000', 
                 'MDME( 4079,1) = 1    !e* decay into e+gamma', 
                 'MDME( 4080,1) = 0    !e* decay into z+gamma', 
                 'MDME( 4081,1) = 0    !e* decay into w+nu'
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)

ProductionFilterSequence = cms.Sequence(generator)
