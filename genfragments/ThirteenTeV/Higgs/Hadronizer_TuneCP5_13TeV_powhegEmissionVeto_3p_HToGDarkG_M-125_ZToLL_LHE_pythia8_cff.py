import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8HadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
              pythia8CommonSettingsBlock,
              pythia8CP5SettingsBlock,
              pythia8PowhegEmissionVetoSettingsBlock,
              processParameters = cms.vstring(
                 'POWHEG:nFinal = 3', ## Number of final state particles
                                      ## (BEFORE THE DECAYS) in the LHE
                                      ## other than emitted extra parton
                 '53:all = DarkPhoton void 3 0 0 0. 0. 0. 0. 0.',
                 '53:isVisible = 0',
                 '25:m0 = 125.0',
                 '25:oneChannel = 1 1 100 22 53',
                 '23:onMode = off',
                 '23:onIfAny = 11 13 15',
               ),
             parameterSets = cms.vstring('pythia8CommonSettings',
                                         'pythia8CP5Settings',
                                         'pythia8PowhegEmissionVetoSettings',
                                         'processParameters'
                                          )
      )
                        )
