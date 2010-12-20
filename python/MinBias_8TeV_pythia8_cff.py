import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(72850000000.),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
	    'Main:timesAllowErrors    = 10000', 
            'ParticleDecays:limitTau0 = on',
	    'ParticleDecays:tauMax = 10',
            'SoftQCD:minBias = on',           
            'SoftQCD:singleDiffractive = on', 
            'SoftQCD:doubleDiffractive = on', 
            'Tune:pp 2',                      
            'Tune:ee 3'),
        parameterSets = cms.vstring('processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision:$'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/MinBias_8TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8-MinBias at 8TeV')
)


ProductionFilterSequence = cms.Sequence(generator)
