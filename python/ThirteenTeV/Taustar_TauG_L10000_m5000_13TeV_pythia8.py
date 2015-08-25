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
            'ExcitedFermion:qqbar2tauStartau = on',
            'ExcitedFermion:Lambda= 10000',
            '4000015:onMode = off',
            '4000015:onIfMatch = 15 22',
            '4000015:m0 = 5000'),
        ),
                         parameterSets = cms.vstring('pythia8CommonSettings',
                                                     'pythia8CUEP8M1Settings',
                                                     'processParameters',
                                                     )
                         )
