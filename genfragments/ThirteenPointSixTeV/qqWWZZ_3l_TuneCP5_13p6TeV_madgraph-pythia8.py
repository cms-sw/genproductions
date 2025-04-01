import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *

generator = cms.EDFilter(
    "Pythia8ConcurrentHadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13600.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters = cms.vstring(
              '23:mMin = 0.05',
              '24:mMin = 0.05',
              '23:onMode = off', # disable all Z decay modes
              '23:onIfAny = 1 2 3 4 5 6 7 8 11 13 15', # enable only Z->ll and Z->qq
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on', #off: require at least the specified number of daughters, on: require exactly the specified number of daughters
             'ResonanceDecayFilter:mothers = 24,23', #? TBC! list of mothers not specified -> count all particles in hard process+resonance decays 
            'ResonanceDecayFilter:eMuAsEquivalent = on', #on: treat electrons and muons as equivalent
            'ResonanceDecayFilter:daughters = 11,11,11',
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    'processParameters'
                                    )
        )
                         )


ProductionFilterSequence = cms.Sequence(generator)
