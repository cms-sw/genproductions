import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         filterEfficiency = cms.untracked.double(1.),
                         comEnergy = cms.double(8000.0),
                         crossSection = cms.untracked.double(0.7411),
                         PythiaParameters = cms.PSet(
    processParameters = cms.vstring(
    'Tune:pp 5',
    'ExcitedFermion:qqbar2eStare = on',
    'ExcitedFermion:Lambda= 10000',
    '4000011:onMode = off',
    '4000011:onIfMatch = 11 22',
    '4000011:m0 = 1500'
    ),
    parameterSets = cms.vstring('processParameters')
    )
                         )

