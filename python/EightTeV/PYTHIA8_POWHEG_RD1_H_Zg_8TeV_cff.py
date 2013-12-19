import FWCore.ParameterSet.Config as cms

generator = cms.EDFilter("Pythia8175HadronizerFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(8000),
    PythiaParameters = cms.PSet(
      processParameters = cms.vstring(
        'Tune:pp = 5',
        'PDF:pSet = 7',
        'HiggsSM:gg2H = on',
        '25:m0 = 125.0',
        '25:onMode = off',
        '25:onIfMatch = 23 22',
        '23:mMin = 50.0',
        '23:onMode = off',
        '23:onIfAny = 11 13 15',
        ),
      parameterSets = cms.vstring('processParameters')
      )
    )
