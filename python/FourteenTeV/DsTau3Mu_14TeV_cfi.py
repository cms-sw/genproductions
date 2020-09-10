import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    maxEventsToPrint = cms.untracked.int32(1),
    pythiaPylistVerbosity = cms.untracked.int32(1),
    filterEfficiency = cms.untracked.double(1.),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(14000.),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SoftQCD:nonDiffractive = on',
            'SoftQCD:singleDiffractive = on',
            'SoftQCD:doubleDiffractive = on',
            '431:onMode = off',
            '431:onIfAny = 15',
            '15:addChannel = on .01 0 13 13 -13',
            '15:onMode = off',
            '15:onIfMatch = 13 13 13',
        ),
        parameterSets = cms.vstring('pythia8CommonSettings',
                                    'pythia8CUEP8M1Settings',
                                    'processParameters',
                                    )
    )
)

DsFilter = cms.EDFilter("PythiaFilter",
    ParticleID = cms.untracked.int32(431)  #D_s 
)

MuFilter = cms.EDFilter("MCParticlePairFilter",
    MinP = cms.untracked.vdouble(2.5, 2.5),
    MaxEta = cms.untracked.vdouble(3.0, 3.0),
    MinEta = cms.untracked.vdouble(-3.0, -3.0),
    ParticleID1 = cms.untracked.vint32(13),
    ParticleID2 = cms.untracked.vint32(13)
)

ProductionFilterSequence = cms.Sequence(generator * DsFilter * MuFilter)
