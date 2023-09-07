import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunesRun3ECM13p6TeV.PythiaCP5Settings_cfi import *
from Configuration.Generator.PSweightsPythia.PythiaPSweightsSettings_cfi import *
from Configuration.Generator.Pythia8PowhegEmissionVetoSettings_cfi import *

generator = cms.EDFilter(
    "Pythia8ConcurrentHadronizerFilter",
    maxEventsToPrint=cms.untracked.int32(1),
    pythiaPylistVerbosity=cms.untracked.int32(1),
    filterEfficiency=cms.untracked.double(1.0),
    pythiaHepMCVerbosity=cms.untracked.bool(False),
    comEnergy=cms.double(13600.0),
    PythiaParameters=cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        pythia8PSweightsSettingsBlock,
        pythia8PowhegEmissionVetoSettingsBlock,
        processParameters=cms.vstring(
            "POWHEG:nFinal = 2",  # Number of final state particles (BEFORE THE DECAYS) in the LHE other than emitted extra parton
            "23:mMin = 0.05",
            "23:onMode = on",
            "24:mMin = 0.05",
            "24:onMode = on",
            "25:m0 = 125.0",
            "25:onMode = off",
            "25:onIfMatch = 5 -5",  # Add H->bb decay
            "25:onIfMatch = 24 -24",  # Add H->WW decay
            "25:onIfMatch = 23 23",  # Add H->ZZ decay
            "ResonanceDecayFilter:filter = on",
            "ResonanceDecayFilter:exclusive = on",  # off: require at least the specified number of daughters, on: require exactly the specified number of daughters
            "ResonanceDecayFilter:mothers = 25",  # list of mothers not specified -> count all particles in hard process+resonance decays (better to avoid specifying mothers when including leptons from the lhe in counting, since intermediate resonances are not gauranteed to appear in general
            "ResonanceDecayFilter:wzAsEquivalent = on",
            "ResonanceDecayFilter:daughters = 5,5,23,23",
        ),
        parameterSets=cms.vstring(
            "pythia8CommonSettings",
            "pythia8CP5Settings",
            "pythia8PSweightsSettings",
            "pythia8PowhegEmissionVetoSettings",
            "processParameters",
        ),
    ),
)

ProductionFilterSequence = cms.Sequence(generator)
