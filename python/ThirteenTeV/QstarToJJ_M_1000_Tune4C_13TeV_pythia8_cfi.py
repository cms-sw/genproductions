import FWCore.ParameterSet.Config as cms



generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(13000.0),
	crossSection = cms.untracked.double(807.7),
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

                        'ExcitedFermion:ug2uStar = on', 
                        'ExcitedFermion:dg2dStar = on', 
                        '4000001:m0 = 1000.', 
                        '4000001:onMode = off', 
                        '4000001:onIfMatch = 21 1', 
                        '4000002:m0 = 1000.', 
                        '4000002:onMode = off', 
                        '4000002:onIfMatch = 21 2', 
                        'ExcitedFermion:Lambda = 1000.', 
                        'ExcitedFermion:coupFprime = 1.', 
                        'ExcitedFermion:coupF = 1.', 
                        'ExcitedFermion:coupFcol = 1.'
		),
		parameterSets = cms.vstring('processParameters')
	)
)

ProductionFilterSequence = cms.Sequence(generator)
