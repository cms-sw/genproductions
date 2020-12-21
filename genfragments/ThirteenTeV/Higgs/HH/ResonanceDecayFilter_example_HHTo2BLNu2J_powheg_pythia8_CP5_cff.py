# -*- coding: utf-8 -*-


import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *


generator = cms.EDFilter(
    "Pythia8HadronizerFilter",
    maxEventsToPrint=cms.untracked.int32(1),
    pythiaPylistVerbosity=cms.untracked.int32(1),
    filterEfficiency=cms.untracked.double(1.0),
    pythiaHepMCVerbosity=cms.untracked.bool(False),
    comEnergy=cms.double(13000.),
    PythiaParameters=cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters=cms.vstring(
            'POWHEG:nFinal = 2',   # Number of final state particles (BEFORE THE DECAYS) in the LHE other than emitted extra parton
            '24:mMin = 0.05',
            '24:onMode = on',
            '25:m0 = 125.0',
            '25:onMode = off',
            '25:onIfMatch = 5 -5',
            '25:onIfMatch = 24 -24',
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on', #on: require exactly the specified number of daughters
            'ResonanceDecayFilter:eMuTauAsEquivalent = on', #on: treat electrons, muons , and taus as equivalent
            'ResonanceDecayFilter:allNuAsEquivalent = on', #on: treat all three neutrino flavours as equivalent
            'ResonanceDecayFilter:udscAsEquivalent = on', #on: treat udsc quarks as equivalent
            'ResonanceDecayFilter:mothers = 24,25',
            'ResonanceDecayFilter:daughters = 5,5,1,1,11,12',
        ),
        parameterSets=cms.vstring(
            'pythia8CommonSettings',
            'pythia8CP5Settings',
            'pythia8PowhegEmissionVetoSettings',
            'processParameters',
        ),
    ),
)

ProductionFilterSequence = cms.Sequence(generator)
