import FWCore.ParameterSet.Config as cms

configurationMetadata = cms.untracked.PSet(
	version = cms.untracked.string('$Revision: 1.5 $'),
	name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/PYTHIA6_QCD_Pthat_30_7TeV_cff.py,v $'),
	annotation = cms.untracked.string('Summer09: Pythia6 generation of QCD events, 7TeV, D6T tune, pthat > 30 GeV, b-quark filter')
)

from Configuration.GenProduction.PythiaUESettings_cfi import *

source = cms.Source("EmptySource")
generator = cms.EDFilter("Pythia6GeneratorFilter",
	comEnergy = cms.double(7000.0),
	crossSection = cms.untracked.double(6.04110e+07),
	filterEfficiency = cms.untracked.double(1.0000),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		pythiaUESettingsBlock,
		processParameters = cms.vstring(
			'MSEL=1   ! QCD ',
			'CKIN(3)=30  ! minimum pt hat for hard interactions',
		),
		parameterSets = cms.vstring(
			'pythiaUESettings',
			'processParameters',
		)
	)
)


bbFilter = cms.EDFilter("MCSingleParticleFilter",
                        ParticleID = cms.untracked.vint32(5,-5),
                        Status = cms.untracked.vint32(2,2)

                       )  


ProductionFilterSequence = cms.Sequence(generator*bbFilter)
