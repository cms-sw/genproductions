
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

			'ExcitedFermion:dg2dStar = on',
			'ExcitedFermion:ug2uStar = on',
			'ExcitedFermion:Lambda = 2500',
			'4000001:m0 = 2500',
			'4000001:onMode = off',
			'4000001:onIfMatch = 1 23',
			'4000002:m0 = 2500',
			'4000002:onMode = off',
			'4000002:onIfMatch = 2 23',

		),
		parameterSets = cms.vstring('processParameters')
	)
)

ProductionFilterSequence = cms.Sequence(generator)
