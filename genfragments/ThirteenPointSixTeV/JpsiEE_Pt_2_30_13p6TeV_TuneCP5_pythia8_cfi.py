import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.MCTunes2017.PythiaCP5Settings_cfi import *

generator = cms.EDFilter("Pythia8ConcurrentGeneratorFilter",
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         #filterEfficiency = cms.untracked.double(0.003),
                         pythiaHepMCVerbosity = cms.untracked.bool(False),
                         crossSection = cms.untracked.double(1742000.0),
                         comEnergy = cms.double(13600.0),
                         maxEventsToPrint = cms.untracked.int32(10),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CP5SettingsBlock,
        processParameters = cms.vstring(
            'Charmonium:all = on',                       # Quarkonia, MSEL=61, including feed-down as well
            '443:onMode = off',                          # Turn off J/psi decays
            '443:onIfMatch = 11 -11',                    # just let J/psi -> e+ e-
            'PhaseSpace:pTHatMin = 2.'                   # be aware of this ckin(3) equivalent
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CP5Settings',
                                    'processParameters',
                                    )
        )
)

# Next are the two electron filters
elelgenfilter = cms.EDFilter("MCParticlePairFilter",
    Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(2., 2.),
    MaxPt = cms.untracked.vdouble(30., 30.),
    MinP = cms.untracked.vdouble(0., 0.),
    MaxEta = cms.untracked.vdouble(2.5, 2.5),
    MinEta = cms.untracked.vdouble(-2.5, -2.5),
    MinInvMass = cms.untracked.double(1.0),
    MaxInvMass = cms.untracked.double(5.0),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1 = cms.untracked.vint32(11),
    ParticleID2 = cms.untracked.vint32(11)
)



oniafilter = cms.EDFilter("PythiaFilter",
    Status = cms.untracked.int32(2),
    MaxEta = cms.untracked.double(2.5),
    MinEta = cms.untracked.double(-2.5),
    MinPt = cms.untracked.double(0.0),
    ParticleID = cms.untracked.int32(443)
)

ProductionFilterSequence = cms.Sequence(generator*oniafilter*elelgenfilter)
