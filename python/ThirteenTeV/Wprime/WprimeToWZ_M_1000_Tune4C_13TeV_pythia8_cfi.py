
import FWCore.ParameterSet.Config as cms

source = cms.Source("EmptySource")

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(1e10),
	filterEfficiency = cms.untracked.double(1),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(1),
	PythiaParameters = cms.PSet(
		processParameters = cms.vstring(
			'Main:timesAllowErrors    = 10000',
			'ParticleDecays:limitTau0 = on',
			'ParticleDecays:tauMax = 10',
			'Tune:pp 5',
			'Tune:ee 3',

			'NewGaugeBoson:ffbar2Wprime = on',
			'Wprime:coup2WZ = 1',
			'34:m0 = 1000',
			'34:onMode = off',
			'34:onIfAny = 23,24',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

ProductionFilterSequence = cms.Sequence(generator)
