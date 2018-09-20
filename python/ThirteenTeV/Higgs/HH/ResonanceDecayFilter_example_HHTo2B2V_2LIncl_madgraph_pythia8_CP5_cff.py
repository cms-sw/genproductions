import FWCore.ParameterSet.Config as cms

# Link to cards:
# https://github.com/cms-sw/genproductions/tree/aa244debe3894ae9c80c846342db3366f7cacb45/bin/MadGraph5_aMCatNLO/cards/production/13TeV/NonRes_hh/tatabb_node_sm

import FWCore.ParameterSet.Config as cms
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
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            '23:mMin = 0.05',
            '24:mMin = 0.05',
            '25:m0 = 125.0',
            '25:onMode = off',
            '25:onIfMatch = 5 -5',
            '25:onIfMatch = 24 -24',
            '25:onIfMatch = 23 23',
            'ResonanceDecayFilter:filter = on',
            'ResonanceDecayFilter:exclusive = on', #off: require at least the specified number of daughters, on: require exactly the specified number of daughters
            'ResonanceDecayFilter:mothers = 25', #list of mothers not specified -> count all particles in hard process+resonance decays (better to avoid specifying mothers when including leptons from the lhe in counting, since intermediate resonances are not gauranteed to appear in general
            'ResonanceDecayFilter:daughters = 5,5,23,23',
            'ResonanceDecayFilter:wzAsEquivalent = on',  #on: treat W and Z as equivalent
          ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters'
                                    )
    )
)

llFilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(8., 8.),
    MaxEta = cms.untracked.vdouble(2.6, 2.6),
    MinEta = cms.untracked.vdouble(-2.6, -2.6),
    ParticleCharge = cms.untracked.int32(-1),
#    MaxInvMass = cms.untracked.double(110.0),
    MinInvMass = cms.untracked.double(70.0),
    ParticleID1 = cms.untracked.vint32(11, 13),
    ParticleID2 = cms.untracked.vint32(11, 13)
)

ProductionFilterSequence = cms.Sequence(generator*llFilter)


# link to fragment:
# https://raw.githubusercontent.com/cms-sw/genproductions/071218017779916161e47643e07a49ea18433425/python/ThirteenTeV/Hadronizer_TuneCUETP8M1_13TeV_generic_LHE_pythia8_cff.py
