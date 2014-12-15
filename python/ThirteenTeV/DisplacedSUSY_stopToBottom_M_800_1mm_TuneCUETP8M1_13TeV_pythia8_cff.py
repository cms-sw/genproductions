FLAVOR = 'stop'
COM_ENERGY = 13000. # GeV
MASS_POINT = 800   # GeV
CROSS_SECTION = 0.0283338 # pb
CTAU_POINT = 1. # mm
SLHA_FILE ='Configuration/Generator/data/DisplacedSUSY_%sToBottom_%d_%.1fmm_SLHA.spc' % (FLAVOR, MASS_POINT, CTAU_POINT)

import FWCore.ParameterSet.Config as cms

from Configuration.Generator.Pythia8CommonSettings_cfi import *
from Configuration.Generator.Pythia8CUEP8M1Settings_cfi import *

generator = cms.EDFilter("Pythia8GeneratorFilter",
    pythiaPylistVerbosity = cms.untracked.int32(0),
    filterEfficiency = cms.untracked.double(1),
    pythiaHepMCVerbosity = cms.untracked.bool(False),
    comEnergy = cms.double(COM_ENERGY),
    crossSection = cms.untracked.double(CROSS_SECTION),
    maxEventsToPrint = cms.untracked.int32(0),
    SLHAFileForPythia8 = cms.string('%s' % SLHA_FILE),
    PythiaParameters = cms.PSet(
        pythia8CommonSettingsBlock,
        pythia8CUEP8M1SettingsBlock,
        processParameters = cms.vstring(
            'SUSY:all = off',
            'SUSY:gg2squarkantisquark  = on',
            'SUSY:qqbar2squarkantisquark= on',
            'RHadrons:allow  = on',
            'RHadrons:allowDecay = on',
            'RHadrons:setMasses = on',
            '1000006:tau0 = %.1f' % CTAU_POINT,
        ),
        parameterSets = cms.vstring(
            'pythia8CommonSettings',
            'pythia8CUEP8M1Settings',
            'processParameters'
        )
    )
)

dirhadrongenfilter = cms.EDFilter("MCParticlePairFilter",
    #Status = cms.untracked.vint32(1, 1),
    MinPt = cms.untracked.vdouble(0., 0.),
    MinP = cms.untracked.vdouble(0., 0.),
    MaxEta = cms.untracked.vdouble(100., 100.),
    MinEta = cms.untracked.vdouble(-100, -100),
    ParticleCharge = cms.untracked.int32(0),
    ParticleID1 = cms.untracked.vint32(1000612,1000622,1000632,1000642,1000652,1006113,1006211,1006213,1006223,1006311,1006313,1006321,1006323,1006333),
    ParticleID2 = cms.untracked.vint32(1000612,1000622,1000632,1000642,1000652,1006113,1006211,1006213,1006223,1006311,1006313,1006321,1006323,1006333)
)

ProductionFilterSequence = cms.Sequence(generator*dirhadrongenfilter)
