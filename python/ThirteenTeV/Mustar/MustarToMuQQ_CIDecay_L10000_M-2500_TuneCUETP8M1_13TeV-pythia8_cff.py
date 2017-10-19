import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.0),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(13000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'ExcitedFermion:Lambda = 10000',
            'ExcitedFermion:qqbar2muStarmu = on',
            '4000013:onMode = off',
            '4000013:m0 = 2500',
            '4000013:onIfMatch = 13 1 1',
            '4000013:onIfMatch = 13 2 2',
            '4000013:onIfMatch = 13 3 3',
            '4000013:onIfMatch = 13 4 4',
            '4000013:onIfMatch = 13 5 5',
            '4000013:onIfMatch = 13 6 6',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    )
)
