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
            'ExcitedFermion:qqbar2eStare = on',
            '4000011:onMode = off',
            '4000011:m0 = 4500',
            '4000011:onIfMatch = 12 24',
            '24:onMode = off',
            '24:onIfMatch = 1 2',
            '24:onIfMatch = 1 4',
            '24:onIfMatch = 3 2',
            '24:onIfMatch = 3 4',
            '24:onIfMatch = 5 2',
            '24:onIfMatch = 5 4',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    )
)
