import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",

    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    SLHAFileForPythia8 = cms.string('Configuration/Generator/data/GMSB_Lambda180_CTau6000.slha'),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	'Main:timesAllowErrors    = 10000', 
        'ParticleDecays:limitTau0 = on',
	'ParticleDecays:tauMax = 10',
        'Tune:ee 3',
        'Tune:pp 5',
	'SUSY:all on',
        ),
        parameterSets = cms.vstring('processParameters')
 
   )
)

ProductionFilterSequence = cms.Sequence(generator)
