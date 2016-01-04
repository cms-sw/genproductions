import FWCore.ParameterSet.Config as cms
from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
                         maxEventsToPrint = cms.untracked.int32(0),
                         pythiaPylistVerbosity = cms.untracked.int32(0),
                         filterEfficiency = cms.untracked.double(1.0),
                         crossSection = cms.untracked.double(425.6),
                         comEnergy = cms.double(5020.0),
                         PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'WeakSingleBoson:ffbar2gmZ = on',
            '23:onMode = off',
            '23:onIfAny = 11',
            ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
        )
                         )

eegenfilter = cms.EDFilter("MCParticlePairFilter",
    Status         = cms.untracked.vint32(1, 1),
    MinPt          = cms.untracked.vdouble(10, 10),
    MaxEta         = cms.untracked.vdouble(2.5, 2.5),
    MinEta         = cms.untracked.vdouble(-2.5, -2.5),
    MinInvMass     = cms.untracked.double(60.0),
    MaxInvMass     = cms.untracked.double(120.0),
    ParticleCharge = cms.untracked.int32(-1),
    ParticleID1    = cms.untracked.vint32(11),
    ParticleID2    = cms.untracked.vint32(11)
)

ProductionFilterSequence = cms.Sequence(generator*eegenfilter)
