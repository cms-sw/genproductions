import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(132.9e-03),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
        'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tauMax = 10',
        'HiggsSM:ffbar2HW=on',
        '25:m0 = 115',
		'24:onMode = off',
		'24:onIfMatch = 11 12 ',
		'24:onIfMatch = 13 14 ',
		'24:onIfMatch = 15 16 ',
		'3000006:all = WHD2 void 1 0 0 15 .00001 0.0',
		'3000006:addChannel = 1 1.0 101  3000005 3000005',
		'3000005:all = WHD1 void 1 0 0 6 .00001 0.0',
		'3000005:addChannel = 1 0.198500 100  3000004 3000004',
		'3000005:addChannel = 1 0.801500 100  3000001 3000001',										
		'3000004:all = WHD0  void 1 0 0 0.1 0.0 0.0',
		'3000001:all = ZDWID void 1 0 0 0.1 0.000001 0.0',										
		'3000001:addChannel = 1 0.1 101  11 -11',
		'25:onMode = off',
        '25:addChannel = 1  1.00   101   3000006   3000006',
        'Tune:pp 5',                      
        'Tune:ee 3'),
        parameterSets = cms.vstring('processParameters')
    )
)
