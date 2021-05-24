import FWCore.ParameterSet.Config as cms



generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(0.00649),
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

                        'ExtraDimensionsG*:ffbar2G* = on', 
			'ExtraDimensionsG*:kappaMG = 0.54',
			'5100039:m0 = 3000',
			'5100039:onMode = off',
			'5100039:onIfAny = 1 2 3 4 5'
		),
		parameterSets = cms.vstring('processParameters')
	)
)

ProductionFilterSequence = cms.Sequence(generator)
