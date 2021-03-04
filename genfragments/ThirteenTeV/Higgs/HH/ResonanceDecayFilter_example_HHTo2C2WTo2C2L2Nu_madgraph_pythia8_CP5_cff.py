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
            '15:onMode = on',
            '24:mMin = 0.05',
            '24:onMode = off',
            '24:onIfAny = 11 13 15', # only leptonic W decays
            '25:m0 = 125.0',
            '25:onMode = off',
            '25:onIfMatch = 4 -4',
            '25:onIfMatch = 24 -24',
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on', #off: require at least the specified number of daughters, on: require exactly the specified number of daughters
            'ResonanceDecayFilter:eMuAsEquivalent = off', #on: treat electrons and muons as equivalent
            'ResonanceDecayFilter:eMuTauAsEquivalent = on', #on: treat electrons, muons , and taus as equivalent
            'ResonanceDecayFilter:allNuAsEquivalent = on', #on: treat all three neutrino flavours as equivalent
            'ResonanceDecayFilter:mothers = 25,24', #list of mothers not specified -> count all particles in hard process+resonance decays (better to avoid specifying mothers when including leptons from the lhe in counting, since intermediate resonances are not gauranteed to appear in general
            'ResonanceDecayFilter:daughters = 4,4,11,11,12,12',
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters'
                                    )
        )
                         )


ProductionFilterSequence = cms.Sequence(generator)
