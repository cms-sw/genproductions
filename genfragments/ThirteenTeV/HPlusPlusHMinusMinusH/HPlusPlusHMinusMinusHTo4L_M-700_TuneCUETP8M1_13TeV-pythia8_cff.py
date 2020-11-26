import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    comEnergy = cms.double(13000.0),
    crossSection = cms.untracked.double(1.0),
    filterEfficiency = cms.untracked.double(1),
    maxeventsToPrint = cms.untracked.int32(0),
    pyhiaHepMCVerbosity = cms.untracked.bool(False),
    pythiaPylistVerbosity = cms.untracked.int32(0),
    PythiaParameters = cms.PSet(
      pythia8CommonSettingsBlock,
      pythia8CUEP8M1SettingsBlock,
      processParameters = cms.vstring(
            'LeftRightSymmmetry:ffbar2HLHL = on',
            '9900041:m0 = 700', # H_L++/-- mass
            '9900041:onMode = off', # switch off all decays
            '9900041:onIfAny = 11 13 15 -11 -13 -15', # turn on decays with e, mu, tau
            'LeftRightSymmmetry:coupHee = 0.01',
            'LeftRightSymmmetry:coupHmue = 0.007',
            'LeftRightSymmmetry:coupHmumu = 0.01',
            'LeftRightSymmmetry:coupHtaue = 0.007',
            'LeftRightSymmmetry:coupHtaumu = 0.007',
            'LeftRightSymmmetry:coupHtautau = 0.01',
            'LeftRightSymmmetry:vL = 0.000000001'
            ),
      parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters',
            )
    )
)
