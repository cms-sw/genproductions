import FWCore.ParameterSet.Config as cms
# link to cards:
# https://github.com/OlivierBondu/genproductions/tree/ed5057598b9fbf246741829603162f1bc6700f7d/bin/MadGraph5_aMCatNLO/cards/production/13TeV/exo_diboson/Spin-0/Radion_GF_HH
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                                                     pythia8CommonSettingsBlock,
                                                     pythia8CP5SettingsBlock,
                                                     processParameters = cms.vstring(
                                                                                     '25:onMode = off',
                                                                                     '25:onIfMatch = 5 -5',
                                                                                     'ResonanceDecayFilter:filter = on'
                                                                                     ),
                                                     parameterSets = cms.vstring('pythia8CommonSettings',
                                                                                 'pythia8CP5Settings',
                                                                                 'processParameters'
                                                                                 )
                                                     )
                         )
ProductionFilterSequence = cms.Sequence(generator)
