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
    processParameters = cms.vstring( ## see details on http://home.thep.lu.se/~torbjorn/php8135/ExtraDimensionalProcesses.php?filepath=files/
    'ExtraDimensionsLED:ffbar2GZ = on',
    #'ExtraDimensionsLED:CutOffmode = 1',
    'ExtraDimensionsLED:t = 0.5',
    'ExtraDimensionsLED:n = 5',         ####
    'ExtraDimensionsLED:MD = 3000.',    ####
    'ExtraDimensionsLED:LambdaT = 1000.',
    '5000039:m0 = 1200.',
    '5000039:mWidth = 1000.',
    '5000039:mMin = 1.',
    '5000039:mMax = 13990.',
    '23:onMode=off',
    '23:mMin=50',
    '23:onIfAny=11 13',
    #'PhaseSpace:pTHatMin = 130.'
    ),
    parameterSets = cms.vstring('pythia8CommonSettings',
                                'pythia8CUEP8M1Settings',
                                'processParameters',)
    )
                         )




