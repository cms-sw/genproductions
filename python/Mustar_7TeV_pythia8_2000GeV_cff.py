import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
    crossSection = cms.untracked.double(1.838e-05),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(7000.0),
    PythiaParameters = cms.PSet(
        processParameters = cms.vstring(
            'ExcitedFermion:qqbar2muStarmu = on',
            'ExcitedFermion:Lambda= 10000',
            '4000013:onMode = off', 
            '4000013:onIfMatch = 13 22',
            '4000013:m0 = 2000'
            ),
        parameterSets = cms.vstring('processParameters')
    )
)

configurationMetadata = cms.untracked.PSet(
    version = cms.untracked.string('$Revision: 1.3 $'),
    name = cms.untracked.string('$Source: /cvs_server/repositories/CMSSW/CMSSW/Configuration/GenProduction/python/Mustar_7TeV_pythia8_2000GeV_cff.py,v $'),
    annotation = cms.untracked.string('PYTHIA8-MX at 7TeV')
)


ProductionFilterSequence = cms.Sequence(generator)
