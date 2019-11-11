import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP2Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    comEnergy = cms.double(13000.0),
    crossSection = cms.untracked.double(1),
    filterEfficiency = cms.untracked.double(1),
    maxEventsToPrint = cms.untracked.int32(0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP2SettingsBlock,
        processParameters = cms.vstring(
            'ExcitedFermion:dg2dStar = on',
            'ExcitedFermion:ug2uStar = on',
            'ExcitedFermion:Lambda = 1000',
            '4000001:m0 = 1000',
            '4000001:onMode = off',
            '4000001:onIfMatch = 1 23',
            '4000002:m0 = 1000',
            '4000002:onMode = off',
            '4000002:onIfMatch = 2 23',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP2Settings',
                                    'processParameters')
    )
)

ProductionFilterSequence = cms.Sequence(generator)
