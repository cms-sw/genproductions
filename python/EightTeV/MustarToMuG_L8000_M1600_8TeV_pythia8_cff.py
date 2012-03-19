import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         filterEfficiency = cms.untracked.double(1.),
                         comEnergy = cms.double(8000.0),
                         crossSection = cms.untracked.double(0.3473),
                         PythiaParameters = cms.PSet(
    processParameters = cms.vstring(
    'ExcitedFermion:qqbar2muStarmu = on',
    'ExcitedFermion:Lambda= 8000',
    '4000013:onMode = off',
    '4000013:onIfMatch = 13 22',
    '4000013:m0 = 1600'
    ),
    parameterSets = cms.vstring('processParameters')
    )
                         )


