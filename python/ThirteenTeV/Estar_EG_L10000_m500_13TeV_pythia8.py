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
            'ExcitedFermion:qqbar2eStare = on',
            'ExcitedFermion:Lambda= 10000',
            '4000011:onMode = off',
            '4000011:onIfMatch = 11 22',
            '4000011:m0 = 500'),
        ),
                         parameterSets = cms.vstring('pythia8CommonSettings',
                                                     'pythia8CUEP8M1Settings',
                                                     'processParameters',
                                                     )
                         )
)
