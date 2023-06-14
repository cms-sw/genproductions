#Link to datacards:
#https://github.com/cms-sw/genproductions/tree/d8108bb9d9db0b61a755c3625e73ee53c7d900dc/bin/Powheg/production/2017/13TeV/ggHH_EWChL

import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentHadronizerFilter",
                         maxEventsToPrint = cms.untracked.int32(1),
                         pythiaPylistVerbosity = cms.untracked.int32(1),
                         filterEfficiency = cms.untracked.double(1.0),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         comEnergy = cms.double(13000.),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters = cms.vstring(
            'POWHEG:nFinal = 2',
            '25:m0 = 125.0',
            '25:onMode = off',
            '25:onIfMatch = 5 -5',
            '25:onIfMatch = 13 -13',
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on',
            'ResonanceDecayFilter:eMuTauAsEquivalent = false',
            'ResonanceDecayFilter:allNuAsEquivalent = false',
            'ResonanceDecayFilter:udscAsEquivalent = false',
            'ResonanceDecayFilter:mothers = 25',
            'ResonanceDecayFilter:daughters = 5,5,13,13'
            ),

        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'pythia8PSweightsSettings',
                                    'pythia8PowhegEmissionVetoSettings',
                                    'processParameters'
                                )
                         )
                    )

ProductionFilterSequence = cms.Sequence(generator)
