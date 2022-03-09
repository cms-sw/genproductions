import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(1.0),
	filterEfficiency = cms.untracked.double(0.132),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
	        pythia8CommonSettingsBlock,
		pythia8CUEP8M1SettingsBlock,
		processParameters = cms.vstring(
			'ExtraDimensionsG*:all = on',
			'ExtraDimensionsG*:kappaMG = 0.54',
			'5100039:m0 = 1800',
			'5100039:onMode = off',
			'5100039:onIfAny = 24',

		),
		parameterSets = cms.vstring('pythia8CommonSettings',
		                            'pythia8CUEP8M1Settings',
		                            'processParameters')
	)
)

llgenfilter = cms.EDFilter("MCMultiParticleFilter",
                           Status = cms.vint32(23, 23),
                           src = cms.InputTag('generator'),
                           ParticleID = cms.vint32(11, 13),
                           PtMin = cms.vdouble(0, 0),
                           NumRequired = cms.int32(1),
                           EtaMax = cms.vdouble(9999, 9999),
                           AcceptMore = cms.bool(True)
                           )

qqgenfilter = cms.EDFilter("MCMultiParticleFilter",
                           Status = cms.vint32(23, 23, 23, 23, 23),
                           src = cms.InputTag('generator'),
                           ParticleID = cms.vint32(1,2,3,4,5),
                           PtMin = cms.vdouble(0,0,0,0,0),
                           NumRequired = cms.int32(1),
                           EtaMax = cms.vdouble(9999, 9999, 9999, 9999, 9999),
                           AcceptMore = cms.bool(True)
                           )

ProductionFilterSequence = cms.Sequence(generator + llgenfilter + qqgenfilter)
