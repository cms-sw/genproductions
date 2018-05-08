import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         comEnergy = cms.double(13000.0),
                         crossSection = cms.untracked.double(1.0),
                         filterEfficiency = cms.untracked.double(1.0),
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         PythiaParameters = cms.PSet(pythia8CommonSettingsBlock,
                                                     pythia8CUEP8M1SettingsBlock,
                                                     processParameters = cms.vstring(
                                                         'NewGaugeBoson:ffbar2Wprime = on',
                                                         'Wprime:coup2WZ = 1.0',
                                                         '34:m0 = 4500',
                                                         '34:onMode = off',
                                                         '34:onIfAny = 23,24',
                                                         '23:onMode = off',
                                                         '23:onIfAny = 1,2,3,4,5',
                                                         '24:onMode = off',
                                                         '24:onIfAny = 11,13,15',
                                                         ),
                                                     parameterSets = cms.vstring(
                                                         'pythia8CommonSettings',
                                                         'pythia8CUEP8M1Settings',
                                                         'processParameters')
                                                     )
                         )
