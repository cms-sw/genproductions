import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
        comEnergy = cms.double(13000.0),
        crossSection = cms.untracked.double(0.00000136),
        filterEfficiency = cms.untracked.double(1),
        maxEventsToPrint = cms.untracked.int32(0),
        pythiaHepMCVerbosity = cms.untracked.bool(False),
        pythiaPylistVerbosity = cms.untracked.int32(1),
        PythiaParameters = cms.PSet(
                pythia8CommonSettingsBlock,
                pythia8CUEP8M1SettingsBlock,
                processParameters = cms.vstring(
                        'ExtraDimensionsG*:gg2G* = on',
                        'ExtraDimensionsG*:kappaMG = 1.439532822',
                        '5100039:m0 = 9000',
                        '5100039:onMode = off',
                        '5100039:onIfAny = 21'
                ),
                parameterSets = cms.vstring('pythia8CommonSettings',
                                            'pythia8CUEP8M1Settings',
                                            'processParameters',
                                            )
        )
)

ProductionFilterSequence = cms.Sequence(generator)


