import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
	comEnergy = cms.double(8000.0),
        crossSection = cms.untracked.double(17250),
	filterEfficiency = cms.untracked.double(1.0),
	maxEventsToPrint = cms.untracked.int32(0),
	pythiaHepMCVerbosity = cms.untracked.bool(False),
	pythiaPylistVerbosity = cms.untracked.int32(0),

	PythiaParameters = cms.PSet(
		processParameters = cms.vstring(
                'Main:timesAllowErrors    = 10000',
	        'ParticleDecays:limitTau0 = on',
		'ParticleDecays:tauMax = 10',
                'ExcitedFermion:dg2dStar = on',
                'ExcitedFermion:ug2uStar = on',
                '4000001:m0 = 400.',
                '4000001:onMode = off',
                '4000001:onIfMatch = 21 1',
                '4000002:m0 = 400.',
                '4000002:onMode = off',
                '4000002:onIfMatch = 21 2', 
                'ExcitedFermion:Lambda = 400.',
                'ExcitedFermion:coupFprime = 1.',
                'ExcitedFermion:coupF = 1.',
                'ExcitedFermion:coupFcol = 1.',
                'Tune:pp 5',
		'Tune:ee 3',
		),

		parameterSets = cms.vstring('processParameters')
	)
)
