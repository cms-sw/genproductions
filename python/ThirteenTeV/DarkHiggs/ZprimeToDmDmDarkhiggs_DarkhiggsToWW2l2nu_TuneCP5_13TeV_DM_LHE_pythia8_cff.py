import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from GeneratorInterface.ExternalDecays.TauolaSettings_cff import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         ExternalDecays = cms.PSet(
                             Tauola = cms.untracked.PSet(
                                 TauolaPolar,
                                 TauolaDefaultInputCards
                             ),
                             parameterSets = cms.vstring('Tauola')
                         ),
                         UseExternalGenerators = cms.untracked.bool(True),
                    
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
                             pythia8CommonSettingsBlock,
                             pythia8CP5SettingsBlock,
                             pythia8PSweightsSettingsBlock,
                             
                             processParameters = cms.vstring(
                                 'Main:timesAllowErrors    = 10000', 
                                 'ParticleDecays:limitTau0 = on',
                                 'ParticleDecays:tauMax = 10'),

                             parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    'pythia8PSweightsSettings'
                                                     )
                        )
)

ProductionFilterSequence = cms.Sequence(generator)
