import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(72850000000),
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
            'Tune:pp 5',                      
            'Tune:ee 3'),
        parameterSets = cms.vstring('processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.1 $'),
    name = cms.untracked.string('$Source: /afs/cern.ch/project/cvs/reps/CMSSW/CMSSW/Configuration/GenProduction/python/EightTeV/MinBias_Tune4C_8TeV_pythia8_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8-MinBias Tune 4C at 8TeV')
)


ProductionFilterSequence = cms.Sequence(generator)
